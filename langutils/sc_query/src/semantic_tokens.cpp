// Semantic tokens: the lexer's tokens of the current text, with the names of the last compile laid over them.

#include "sc_query/queries.hpp"
#include "internal.hpp"

#include "sc_lexer/lexer.hpp"
#include "sc_source/text_info.hpp"

#include <algorithm>

namespace sc::query {

namespace {

using lex::TokenType;

// A token as byte offsets into the normalised text.
struct Span {
    std::uint32_t begin;
    std::uint32_t end;
    TokenKind kind;
    std::uint8_t modifiers;
};

struct KeepAll {
    struct Output {
        TokenType type;
        src::SourceCodeRange range;
    };
    template <TokenType T> [[nodiscard]] std::optional<Output> process(src::SourceCodeRange r) {
        return Output { T, r };
    }
};

bool is_keyword_name(std::string_view s) { return s == "this" || s == "super"; }

bool is_builtin_name(std::string_view s) {
    return s == "thisProcess" || s == "thisThread" || s == "thisMethod" || s == "thisFunction" || s == "thisFunctionDef"
        || s == "currentEnvironment" || s == "topEnvironment";
}

std::optional<TokenKind> lexical_kind(TokenType t, std::string_view text) {
    if (lex::is_comment(t) || t == TokenType::ErMultilineCommentUnclosed)
        return TokenKind::Comment;
    if (lex::matches(t, TokenType::StringLine, TokenType::ErStringUnclosed))
        return TokenKind::String;
    if (lex::matches(t, TokenType::SymbolSlash, TokenType::SymbolQuote, TokenType::ErSymbolQuoteUnclosed))
        return TokenKind::Symbol;
    if (lex::matches(t, TokenType::Ascii))
        return TokenKind::Character;
    if (lex::matches(t, TokenType::True, TokenType::False, TokenType::Nil, TokenType::Inf))
        return TokenKind::Builtin;
    if (lex::is_literal(t))
        return TokenKind::Number;
    if (lex::is_keyword(t))
        return TokenKind::Keyword;
    if (t == TokenType::PrimitiveName)
        return TokenKind::Primitive;
    if (t == TokenType::ClassName)
        return TokenKind::Class;
    if (t == TokenType::Name) {
        if (is_keyword_name(text))
            return TokenKind::Keyword;
        if (is_builtin_name(text))
            return TokenKind::Builtin;
    }
    return std::nullopt;
}

std::vector<Span> lexical_spans(const src::TextInfo& text, const Library* library) {
    std::vector<Span> out;
    const std::string_view source = text.source.as_string();
    lex::CodePointStream stream { text.source, text.source_start_in_file };
    KeepAll action;
    bool after_tilde = false;
    std::uint32_t tilde = 0;
    while (true) {
        const auto t = lex::lexer(stream, action);
        if (t.type == TokenType::EndOfFile)
            return out;
        const auto begin = t.range.begin.absolute;
        const auto end = t.range.end.absolute;
        const auto word = source.substr(begin, end - begin);
        if (after_tilde && t.type == TokenType::Name)
            out.push_back({ tilde, end, TokenKind::Environment, 0 });
        else if (const auto kind = lexical_kind(t.type, word)) {
            std::uint8_t modifiers = 0;
            if (*kind == TokenKind::Class && library && !library->find_class(word))
                modifiers |= TokenModifier::Unknown;
            out.push_back({ begin, end, *kind, modifiers });
        }
        after_tilde = t.type == TokenType::Tilde;
        tilde = begin;
    }
}

std::optional<Span> name_span(const Occurrence& o) {
    Span s { 0, 0, TokenKind::Variable, 0 };
    if (o.declaration)
        s.modifiers |= TokenModifier::Declaration;
    switch (o.kind) {
    case Occurrence::Kind::Local:
        s.kind = o.argument ? TokenKind::Parameter : TokenKind::Variable;
        break;
    case Occurrence::Kind::Member:
        s.kind = TokenKind::Property;
        if (o.member_kind != MemberKind::Instance)
            s.modifiers |= TokenModifier::Static;
        if (o.member_kind == MemberKind::Constant)
            s.modifiers |= TokenModifier::Readonly;
        break;
    case Occurrence::Kind::Environment:
        s.kind = TokenKind::Environment;
        break;
    case Occurrence::Kind::Send:
        s.kind = TokenKind::Method;
        break;
    case Occurrence::Kind::Method:
        s.kind = TokenKind::Method;
        if (o.class_side)
            s.modifiers |= TokenModifier::Static;
        break;
    case Occurrence::Kind::Keyword:
        s.kind = TokenKind::KeywordArgument;
        break;
    case Occurrence::Kind::Class:
    case Occurrence::Kind::Symbol:
        return std::nullopt; // the lexer finds these
    }
    return s;
}

bool word_char(char c) {
    return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9') || c == '_';
}

// The names of a compile, moved into the current text. When it was compiled from another text, only the names before
// the first change, or after the last, are kept.
std::vector<Span> name_spans(const Analysis& analysis, const src::TextInfo* old_text, const src::TextInfo& current) {
    std::vector<Span> out;
    if (!old_text)
        return out;
    const std::string_view now = current.source.as_string();
    const std::string_view then = old_text->source.as_string();
    auto prefix = static_cast<std::uint32_t>(
        std::mismatch(now.begin(), now.begin() + std::min(now.size(), then.size()), then.begin()).first - now.begin());
    std::uint32_t suffix = 0;
    if (now != then) {
        const auto most = std::min(now.size(), then.size()) - prefix;
        while (suffix < most && now[now.size() - 1 - suffix] == then[then.size() - 1 - suffix])
            ++suffix;
        // Not in the middle of a word, so `foo` typed into `foox` isn't still a name.
        const auto at = [](std::string_view s, std::size_t i) { return i < s.size() && word_char(s[i]); };
        if (at(now, prefix) || at(then, prefix))
            while (prefix > 0 && word_char(now[prefix - 1]))
                --prefix;
        if (suffix > 0 && (at(now, now.size() - suffix - 1) || at(then, then.size() - suffix - 1)))
            while (suffix > 0 && word_char(now[now.size() - suffix]))
                --suffix;
    } else {
        suffix = static_cast<std::uint32_t>(now.size()) - prefix;
    }
    const auto old_tail = static_cast<std::uint32_t>(then.size()) - suffix;
    const auto new_tail = static_cast<std::uint32_t>(now.size()) - suffix;

    const LineIndex old_lines { then };
    for (const auto& o : analysis.occurrences) {
        auto span = name_span(o);
        if (!span)
            continue;
        const auto b = static_cast<std::uint32_t>(old_lines.offset(o.range.begin));
        const auto e = static_cast<std::uint32_t>(old_lines.offset(o.range.end));
        if (e <= b)
            continue;
        if (e <= prefix) {
            span->begin = b;
            span->end = e;
        } else if (b >= old_tail) {
            span->begin = b - old_tail + new_tail;
            span->end = e - old_tail + new_tail;
        } else {
            continue;
        }
        // Only the name, an argument's declaration also has its default value, `a = 1`.
        if (word_char(now[span->begin])) {
            auto word_end = span->begin;
            while (word_end < span->end && word_char(now[word_end]))
                ++word_end;
            if (span->kind != TokenKind::KeywordArgument)
                span->end = word_end;
        }
        // The name of an environment variable is after its `~`.
        if (span->kind == TokenKind::Environment && span->begin > 0 && now[span->begin - 1] == '~')
            --span->begin;
        if (span->kind == TokenKind::KeywordArgument && span->end > span->begin + 1 && now[span->end - 1] == ':')
            --span->end;
        out.push_back(*span);
    }
    return out;
}

}

std::vector<SemanticToken> semantic_tokens(const Document& document) {
    std::vector<SemanticToken> out;
    const auto& current = document.text_info();
    if (!current)
        return out;
    const auto* library = document.library() ? document.library().get() : nullptr;

    auto spans = lexical_spans(*current, library);
    std::vector<Span> names;
    if (document.kind() == DocumentKind::ClassFile) {
        const auto text = library ? library->text_of(document.path()) : nullptr;
        names = name_spans(analyse_names(document), text.get(), *current);
    } else {
        names = name_spans(analyse_names(document), document.graph_text().get(), *current);
        // Code that doesn't parse loses most of its names, the last code that parsed fills them in.
        const auto& parsed = document.parsed();
        if (parsed.graph && parsed.graph != document.graph()) {
            const auto older = name_spans(parsed_names(document), parsed.text.get(), *current);
            names.insert(names.end(), older.begin(), older.end());
        }
    }
    // Sorted by where they begin, so the first of overlapping names, the newest, is kept.
    std::stable_sort(names.begin(), names.end(), [](const Span& l, const Span& r) { return l.begin < r.begin; });
    // The same name can be found twice, e.g., an assignment's target.
    names.erase(std::unique(names.begin(), names.end(),
                            [](const Span& kept, const Span& next) { return next.begin < kept.end; }),
                names.end());
    // A name replaces the lexer's tokens it overlaps, e.g., `this` is a keyword, but `~a` is an environment variable.
    std::vector<Span> merged;
    merged.reserve(spans.size() + names.size());
    auto name = names.begin();
    for (const auto& s : spans) {
        while (name != names.end() && name->end <= s.begin)
            merged.push_back(*name++);
        const bool covered = name != names.end() && name->begin < s.end;
        if (!covered)
            merged.push_back(s);
    }
    merged.insert(merged.end(), name, names.end());

    // Split at line breaks, columns in codepoints.
    const std::string_view text = current->source.as_string();
    const LineIndex lines { text };
    for (const auto& s : merged) {
        auto begin = s.begin;
        while (begin < s.end) {
            const auto line_end = text.find('\n', begin);
            const auto end = static_cast<std::uint32_t>(std::min<std::size_t>(s.end, line_end));
            if (end > begin)
                out.push_back(
                    { lines.position(begin), count_codepoints(text.substr(begin, end - begin)), s.kind, s.modifiers });
            begin = end + 1;
        }
    }
    return out;
}

}
