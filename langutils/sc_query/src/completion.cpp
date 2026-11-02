// Completion and signature help. Both lex the text before the cursor and ask the parser what it can take there, as the
// code being typed rarely parses as a whole. They use the last compile only for the locals in scope.

#include "sc_query/queries.hpp"
#include "internal.hpp"

#include "sc_lexer/lexer.hpp"
#include "sc_parser/ast.hpp"
#include "sc_parser/details/ast_erased.hpp"
#include "sc_parser/sc_parser.hpp"
#include "sc_source/normalise_source.hpp"
#include "sc_source/text_info.hpp"
#include "sc_util/indexed_vector.hpp"
#include "sc_util/strong_index.hpp"

#include <algorithm>
#include <cctype>
#include <limits>
#include <set>

namespace sc::query {

namespace {

using lex::TokenType;

////////////////////////////////////////////////////////////////////////////////
// Tokens
////////////////////////////////////////////////////////////////////////////////

struct Token {
    TokenType type;
    src::SourceCodeRange range;
    [[nodiscard]] std::uint32_t begin() const { return range.begin.absolute; }
    [[nodiscard]] std::uint32_t end() const { return range.end.absolute; }
};

struct TokenTag;
using TokenIndex = util::StrongIndex<std::uint32_t, TokenTag>;
using Tokens = util::IndexedVector<TokenIndex, Token>;

// Keeps every token, whitespace and comments too.
struct KeepAll {
    using Output = Token;
    template <TokenType T> [[nodiscard]] std::optional<Output> process(src::SourceCodeRange r) {
        return Token { T, r };
    }
};

bool trivia(TokenType t) { return lex::is_whitespace(t) || lex::is_comment(t); }

// Normalised as the compiler reads it, so offsets match the parser's. Null when too large.
std::shared_ptr<const src::TextInfo> make_text(std::string text, DocumentKind kind) {
    try {
        return std::make_shared<const src::TextInfo>(
            src::NormalisedSource { std::move(text) }, src::FileCodeLocation {}, std::nullopt,
            kind == DocumentKind::ClassFile ? src::TextKind::ClassFile : src::TextKind::ScdFile);
    } catch (const std::length_error&) { return nullptr; }
}

Tokens lex_all(const src::TextInfo& text) {
    Tokens out;
    lex::CodePointStream stream { text.source, text.source_start_in_file };
    KeepAll action;
    while (true) {
        const auto t = lex::lexer(stream, action);
        if (t.type == TokenType::EndOfFile)
            return out;
        out.push_back(t);
    }
}

// The location of the end of @p text.
src::SourceCodeLocation end_of(std::string_view text) {
    src::SourceCodeLocation l { static_cast<std::uint32_t>(text.size()), 0, 0 };
    for (const char c : text) {
        if (c == '\n') {
            ++l.line_number;
            l.codepoint_in_line = 0;
        } else if ((static_cast<unsigned char>(c) & 0xC0) != 0x80) {
            ++l.codepoint_in_line;
        }
    }
    return l;
}

////////////////////////////////////////////////////////////////////////////////
// The text before the cursor
////////////////////////////////////////////////////////////////////////////////

// The text before the cursor and its tokens.
struct Before {
    std::shared_ptr<const src::TextInfo> info;
    Tokens tokens;

    [[nodiscard]] std::string_view text() const { return info->source.as_string(); }
    [[nodiscard]] std::string_view read(std::uint32_t b, std::uint32_t e) const { return text().substr(b, e - b); }
    [[nodiscard]] std::string_view read(const Token& t) const { return read(t.begin(), t.end()); }
    // The first token starting at or after @p offset.
    [[nodiscard]] TokenIndex first_from(std::uint32_t offset) const {
        const auto& raw = tokens.raw();
        const auto it =
            std::partition_point(raw.begin(), raw.end(), [&](const Token& t) { return t.begin() < offset; });
        return TokenIndex { static_cast<std::uint32_t>(it - raw.begin()) };
    }
    // The last token before @p i that isn't whitespace or a comment.
    [[nodiscard]] std::optional<TokenIndex> significant_before(TokenIndex i) const {
        while (*i > 0) {
            i = util::retreat(i);
            if (!trivia(tokens[i].type))
                return i;
        }
        return std::nullopt;
    }
    // The innermost bracket left open by the tokens before @p end.
    [[nodiscard]] std::optional<TokenIndex> open_bracket(TokenIndex end) const {
        int depth = 0;
        for (auto i = end; *i > 0;) {
            i = util::retreat(i);
            const auto t = tokens[i].type;
            if (lex::is_close_bracket(t))
                ++depth;
            else if (lex::is_open_bracket(t) && depth-- == 0)
                return i;
        }
        return std::nullopt;
    }
};

// Where the cursor is.
struct Cursor {
    enum struct In : std::uint8_t { Code, Comment, String, Symbol, QuotedSymbol, Other };
    Before before;
    In in { In::Code };
    std::string prefix; // the word typed so far, for symbols without the `\` or `'`
    std::uint32_t start { 0 }; // the offset of the token being typed, or the cursor when there isn't one
    std::uint32_t prefix_start { 0 }; // the offset the prefix starts at
    std::optional<TokenIndex> previous; // the token before the one being typed, ignoring whitespace and comments
};

bool is_word(TokenType t) {
    return lex::matches(t, TokenType::Name, TokenType::ClassName, TokenType::While, TokenType::Var, TokenType::Arg,
                        TokenType::ClassVar, TokenType::Const, TokenType::True, TokenType::False, TokenType::Nil,
                        TokenType::Pi, TokenType::Inf);
}

std::optional<Cursor> cursor_at(const Document& document, Position p) {
    Cursor c;
    c.before.info = make_text(document.text().substr(0, document.lines().offset(p)), document.kind());
    if (!c.before.info)
        return std::nullopt;
    c.before.tokens = lex_all(*c.before.info);
    const auto& b = c.before;
    const auto end = static_cast<std::uint32_t>(b.text().size());
    c.start = c.prefix_start = end;
    auto typed = b.tokens.next_index();
    if (!b.tokens.empty() && b.tokens.back().end() == end) {
        const auto& last = b.tokens.back();
        const auto t = last.type;
        const auto text = b.read(last);
        if (lex::matches(t, TokenType::Comment, TokenType::VersionComment, TokenType::ErMultilineCommentUnclosed))
            c.in = Cursor::In::Comment;
        else if (t == TokenType::ErStringUnclosed)
            c.in = Cursor::In::String;
        else if (t == TokenType::SymbolSlash)
            c.in = Cursor::In::Symbol;
        else if (t == TokenType::ErSymbolQuoteUnclosed)
            c.in = Cursor::In::QuotedSymbol;
        else if (!is_word(t) || std::isdigit(static_cast<unsigned char>(text.front())))
            c.in = lex::is_error(t) || lex::is_literal(t) ? Cursor::In::Other : Cursor::In::Code;
        if (c.in != Cursor::In::Code || is_word(t)) {
            typed = util::retreat(typed);
            c.start = last.begin();
            const bool marked = c.in == Cursor::In::Symbol || c.in == Cursor::In::QuotedSymbol;
            c.prefix_start = c.start + (marked ? 1 : 0);
            c.prefix = std::string { b.read(c.prefix_start, end) };
        }
    }
    c.previous = b.significant_before(typed);
    return c;
}

// The start of the statement or argument the code up to @p upto is in: after the innermost bracket left open, a
// function's argument list, the last `;` or `,` in that bracket, and a keyword argument's keyword.
std::uint32_t statement_start(const Before& b, std::uint32_t upto) {
    const auto n = b.first_from(upto);
    const auto open = b.open_bracket(n);
    auto i = open ? util::advance(*open) : TokenIndex { 0 };
    std::uint32_t start = open ? b.tokens[*open].end() : 0;
    const auto significant = [&](TokenIndex j) {
        while (*j < *n && trivia(b.tokens[j].type))
            j = util::advance(j);
        return j;
    };
    int depth = 0;
    // `{ |a, b|`
    if (open && lex::matches(b.tokens[*open].type, TokenType::OpenCurly, TokenType::BeginClosedFunction)) {
        auto j = significant(i);
        if (*j < *n && b.tokens[j].type == TokenType::Pipe) {
            for (j = util::advance(j); *j < *n; j = util::advance(j)) {
                const auto t = b.tokens[j].type;
                if (lex::is_open_bracket(t))
                    ++depth;
                else if (lex::is_close_bracket(t))
                    --depth;
                else if (depth == 0 && t == TokenType::Pipe)
                    break;
            }
            if (*j < *n) {
                i = util::advance(j);
                start = b.tokens[j].end();
            }
            depth = 0;
        }
    }
    for (; *i < *n; i = util::advance(i)) {
        const auto t = b.tokens[i].type;
        if (lex::is_open_bracket(t))
            ++depth;
        else if (lex::is_close_bracket(t))
            --depth;
        else if (depth == 0 && lex::matches(t, TokenType::SemiColon, TokenType::Comma))
            start = b.tokens[i].end();
    }
    // `key: value`
    const auto first = significant(b.first_from(start));
    if (*first < *n && b.tokens[first].type == TokenType::KeywordBinaryOperator)
        start = b.tokens[first].end();
    return start;
}

// What the parser can take at @p offset, from a parse of the text before it, or of the statement it is in when an error
// before it hides that. Nothing when neither knows.
std::optional<std::vector<TokenType>> expected_at(const Before& b, std::uint32_t offset) {
    if (auto e = ast::expected_tokens(b.info, { {}, end_of(b.read(0, offset)) }))
        return e;
    const auto start = statement_start(b, offset);
    const auto snippet = make_text(std::string { b.read(start, offset) }, DocumentKind::Interpreted);
    if (!snippet)
        return std::nullopt;
    return ast::expected_tokens(snippet, { {}, end_of(snippet->source.as_string()) });
}

bool has(const std::optional<std::vector<TokenType>>& expected, TokenType t) {
    // Not knowing allows anything.
    return !expected || std::find(expected->begin(), expected->end(), t) != expected->end();
}

////////////////////////////////////////////////////////////////////////////////
// Receivers
////////////////////////////////////////////////////////////////////////////////

// What the expression before a message is known to be: a class, or an instance of one.
struct Receiver {
    std::string class_name; // empty when not known
    bool class_side { false };
};

// The text of the receiver of the message whose selector starts at @p selector, from a parse of its statement with
// @p tail written from the selector on, e.g., `x` for `a.`, or `x: nil` for the keyword binary operator in `a ke`.
std::optional<std::string> receiver_text(const Before& b, std::uint32_t selector, std::string_view tail, bool binary) {
    const auto start = statement_start(b, selector);
    auto snippet_text = std::string { b.read(start, selector) };
    snippet_text += tail;
    const auto snippet = make_text(std::move(snippet_text), DocumentKind::Interpreted);
    if (!snippet)
        return std::nullopt;
    const auto [graph, accepted] = ast::parse(snippet);
    (void)accepted;
    if (!graph)
        return std::nullopt;
    const auto& g = *graph;
    const auto at = selector - start;
    std::optional<std::string> out;
    g.flat_walk([&](const auto&, const auto&, const src::SourceCodeRange&, ast::Index index) {
        if (out)
            return;
        const auto found = [&](ast::OptionalIndex sel, ast::OptionalIndex receiver) {
            if (ast::get_location(g, sel).begin.absolute == at)
                out = std::string { ast::get_string_view(g, receiver) };
        };
        if (binary) {
            if (const auto m = g.as_a<ast::BinaryMessageIndex>(index)) {
                const auto [sel, lhs, rhs] = g.children(*m);
                found(sel, lhs);
            }
        } else if (const auto m = g.as_a<ast::PostfixMessageIndex>(index)) {
            const auto [receiver, sel, args, blocks] = g.children(*m);
            found(sel, receiver);
        }
    });
    return out;
}

// The class of a receiver written as @p code, compiled when it isn't a pseudo variable.
Receiver receiver_class(Library* library, std::string_view code) {
    static const std::pair<const char*, const char*> pseudo_variables[] = {
        { "thisProcess", "Main" },
        { "thisThread", "Thread" },
        { "thisFunction", "Function" },
        { "thisMethod", "Method" },
        { "currentEnvironment", "Environment" },
        { "topEnvironment", "Environment" },
    };
    for (const auto& [name, cls] : pseudo_variables)
        if (code == name)
            return { cls, false };
    constexpr std::size_t largest { 20000 };
    if (!library || code.empty() || code.size() > largest)
        return {};
    const auto [c, class_side] = compiled_class(*library, code);
    if (!c)
        return {};
    return { library->get(*c).name, class_side };
}

Receiver receiver_of(const Before& b, Library* library, std::uint32_t selector, std::string_view tail, bool binary) {
    const auto text = receiver_text(b, selector, tail, binary);
    return text ? receiver_class(library, *text) : Receiver {};
}

////////////////////////////////////////////////////////////////////////////////
// Calls
////////////////////////////////////////////////////////////////////////////////

std::string_view trim(std::string_view s) {
    while (!s.empty() && std::isspace(static_cast<unsigned char>(s.front())))
        s.remove_prefix(1);
    while (!s.empty() && std::isspace(static_cast<unsigned char>(s.back())))
        s.remove_suffix(1);
    return s;
}

// The call whose argument list the code up to @p upto is in.
struct Call {
    bool found { false };
    Receiver receiver;
    std::string selector; // empty for a bracket that isn't a call, e.g., an event
    std::int32_t argument { 0 };
    std::string previous; // the argument before this one
    std::string keyword; // when written `key: ...`
    bool function_call { false }; // `foo(a, b)`, which is `a.foo(b)`
};

Call call_at(const Before& b, std::uint32_t upto, Library* library) {
    Call call;
    const auto n = b.first_from(upto);
    const auto open = b.open_bracket(n);
    if (!open || b.tokens[*open].type != TokenType::OpenParen)
        return call;
    call.found = true;
    std::vector<TokenIndex> commas;
    int depth = 0;
    for (auto i = util::advance(*open); *i < *n; i = util::advance(i)) {
        const auto t = b.tokens[i].type;
        if (lex::is_open_bracket(t))
            ++depth;
        else if (lex::is_close_bracket(t))
            --depth;
        else if (depth == 0 && t == TokenType::Comma)
            commas.push_back(i);
    }
    call.argument = static_cast<std::int32_t>(commas.size());
    const auto argument_start = util::advance(commas.empty() ? *open : commas.back());
    for (auto i = argument_start; *i < *n; i = util::advance(i)) {
        if (trivia(b.tokens[i].type))
            continue;
        const auto keyword = b.read(b.tokens[i]);
        if (b.tokens[i].type == TokenType::KeywordBinaryOperator
            && std::islower(static_cast<unsigned char>(keyword.front())))
            call.keyword = std::string { keyword.substr(0, keyword.size() - 1) };
        break;
    }
    if (!commas.empty()) {
        const auto from = commas.size() > 1 ? b.tokens[commas[commas.size() - 2]].end() : b.tokens[*open].end();
        call.previous = std::string { trim(b.read(from, b.tokens[commas.back()].begin())) };
    }

    const auto name = b.significant_before(*open);
    if (!name)
        return call;
    const auto& token = b.tokens[*name];
    if (token.type == TokenType::ClassName) {
        call.selector = "new";
        call.receiver = { std::string { b.read(token) }, true };
        return call;
    }
    if (token.type != TokenType::Name)
        return call;
    const auto before = b.significant_before(*name);
    const auto before_type = before ? b.tokens[*before].type : TokenType::EndOfFile;
    if (before_type == TokenType::Tilde)
        return call; // `~f.(...)` is written with a dot, `~f(...)` isn't a call of f
    call.selector = std::string { b.read(token) };
    if (before_type == TokenType::Dot)
        call.receiver = receiver_of(b, library, token.begin(), call.selector, false);
    else
        call.function_call = true;
    return call;
}

////////////////////////////////////////////////////////////////////////////////
// Candidates
////////////////////////////////////////////////////////////////////////////////

bool starts_with(std::string_view s, std::string_view prefix) { return s.substr(0, prefix.size()) == prefix; }

bool starts_with_ignoring_case(std::string_view s, std::string_view prefix) {
    if (s.size() < prefix.size())
        return false;
    for (std::size_t i = 0; i < prefix.size(); ++i)
        if (std::tolower(static_cast<unsigned char>(s[i])) != std::tolower(static_cast<unsigned char>(prefix[i])))
            return false;
    return true;
}

// Names written in the document: symbols, `\name` and `'name'`, and environment variables, `~name`. Not the one
// being typed, which starts at @p skip.
struct Written {
    std::set<std::string> symbols;
    std::set<std::string> environment;
};

Written written_in(const Document& document, std::uint32_t skip) {
    Written out;
    const auto info = make_text(document.text(), document.kind());
    if (!info)
        return out;
    const auto tokens = lex_all(*info);
    const std::string_view text = info->source.as_string();
    for (const auto i : tokens.indices()) {
        const auto& t = tokens[i];
        const auto s = text.substr(t.begin(), t.end() - t.begin());
        if (t.begin() == skip)
            continue;
        if (t.type == TokenType::SymbolSlash && s.size() > 1)
            out.symbols.insert(std::string { s.substr(1) });
        else if (t.type == TokenType::SymbolQuote && s.size() > 2 && s.find('\\') == std::string_view::npos)
            out.symbols.insert(std::string { s.substr(1, s.size() - 2) });
        else if (t.type == TokenType::Tilde) {
            auto j = util::advance(i);
            while (tokens.contains(j) && trivia(tokens[j].type))
                j = util::advance(j);
            if (tokens.contains(j) && tokens[j].type == TokenType::Name && tokens[j].begin() != skip)
                out.environment.insert(
                    std::string { text.substr(tokens[j].begin(), tokens[j].end() - tokens[j].begin()) });
        }
    }
    return out;
}

constexpr std::size_t max_items = 400;

// With the token each is written as, so only those the parser can take there are offered.
const std::pair<const char*, TokenType> keywords[] = {
    { "var", TokenType::Var },
    { "arg", TokenType::Arg },
    { "this", TokenType::Name },
    { "super", TokenType::Name },
    { "nil", TokenType::Nil },
    { "true", TokenType::True },
    { "false", TokenType::False },
    { "inf", TokenType::Inf },
    { "pi", TokenType::Pi },
    { "thisProcess", TokenType::Name },
    { "thisThread", TokenType::Name },
    { "thisFunction", TokenType::Name },
    { "thisMethod", TokenType::Name },
    { "currentEnvironment", TokenType::Name },
    { "topEnvironment", TokenType::Name },
    { "classvar", TokenType::ClassVar },
    { "const", TokenType::Const },
};

std::vector<MethodId> methods_for(const Library& library, const Receiver& r, std::string_view selector) {
    if (!r.class_name.empty())
        if (const auto c = library.find_class(r.class_name))
            if (const auto m = library.lookup(*c, r.class_side, selector))
                return { *m };
    return library.implementors(selector);
}

bool takes_an_argument(const Library& library, MethodId m) {
    const auto& info = library.get(m);
    return !info.arguments.empty() || !info.variadic.empty();
}

// How far up the hierarchy from the receiver a method is defined, the order Library::responds_to walks it in, so
// the many methods of Object sort last.
class Distance {
public:
    Distance(const Library& library, ClassId receiver, bool class_side):
        m_library(library),
        m_class_side(class_side),
        m_chain(library.superclasses(receiver)) {
        if (class_side)
            if (const auto klass = library.find_class("Class"))
                m_class_chain = library.superclasses(*klass);
    }

    [[nodiscard]] std::uint8_t operator()(MethodId m) const {
        const auto& info = m_library.get(m);
        std::size_t d = m_chain.size() + m_class_chain.size();
        if (info.class_method == m_class_side)
            d = steps_to(m_chain, info.owner);
        else if (const auto steps = steps_to(m_class_chain, info.owner); steps < m_class_chain.size())
            d = m_chain.size() + steps;
        return static_cast<std::uint8_t>(std::min<std::size_t>(d, std::numeric_limits<std::uint8_t>::max()));
    }

private:
    // How many classes up the chain @p c is, the chain's size when it isn't in it.
    static std::size_t steps_to(const std::vector<ClassId>& chain, ClassId c) {
        return static_cast<std::size_t>(std::find(chain.begin(), chain.end(), c) - chain.begin());
    }

    const Library& m_library;
    bool m_class_side;
    std::vector<ClassId> m_chain;
    std::vector<ClassId> m_class_chain;
};
}

////////////////////////////////////////////////////////////////////////////////
// Completion
////////////////////////////////////////////////////////////////////////////////

Completions complete(const Document& document, Position p, HookHost* hooks) {
    Completions out;
    const auto& analysis = analyse_names(document);
    const auto library = analysis.library ? analysis.library : document.library();
    if (!library)
        return out;
    const auto cursor = cursor_at(document, p);
    if (!cursor)
        return out;
    const auto& b = cursor->before;
    const auto& prefix = cursor->prefix;
    const auto replace = end_of(b.read(0, cursor->prefix_start));
    out.replace = { { replace.line_number, replace.codepoint_in_line }, p };
    const auto add = [&](CompletionItem item) {
        if (out.items.size() >= max_items) {
            out.incomplete = true;
            return;
        }
        out.items.push_back(std::move(item));
    };
    using In = Cursor::In;
    if (cursor->in == In::Comment || cursor->in == In::String || cursor->in == In::Other)
        return out;

    // Symbols: `\na` or `'na`.
    if (cursor->in == In::Symbol || cursor->in == In::QuotedSymbol) {
        std::set<std::string> seen;
        const auto symbol = [&](const std::string& name, std::string detail, std::uint8_t rank) {
            if (!starts_with(name, prefix) || !seen.insert(name).second)
                return;
            add({ name, CompletionKind::Symbol, std::move(detail), {}, {}, rank });
        };
        bool hooks_suceeded = false;
        if (hooks) {
            const auto call = call_at(b, cursor->start, library.get());
            SymbolContext context;
            context.receiver = call.receiver.class_name;
            context.selector = call.selector;
            context.argument = call.found ? call.argument : -1;
            context.previous = call.previous;
            context.keyword = call.keyword;
            context.prefix = prefix;
            for (const auto& c : hooks->complete_symbol(*library, context)) {
                symbol(c.label, c.detail.empty() ? c.hook : c.detail, 0);
                hooks_suceeded = true;
            }
        }
        if (!hooks_suceeded) {
            for (const auto& name : written_in(document, cursor->start).symbols)
                symbol(name, "in this document", 1);
            for (const auto& name : library->symbols())
                symbol(name, {}, 2);
        }
        return out;
    }

    const auto previous = cursor->previous ? b.tokens[*cursor->previous].type : TokenType::EndOfFile;

    // Environment variables: `~na`.
    if (previous == TokenType::Tilde) {
        for (const auto& name : written_in(document, cursor->start).environment)
            if (starts_with(name, prefix))
                add({ name, CompletionKind::Environment, "environment variable", {}, {}, 1 });
        return out;
    }

    // Messages: `x.na`.
    if (previous == TokenType::Dot) {
        const auto receiver = receiver_of(b, library.get(), cursor->start, "x", false);
        std::optional<ClassId> c;
        if (!receiver.class_name.empty())
            c = library->find_class(receiver.class_name);
        if (c) {
            const Distance distance(*library, *c, receiver.class_side);
            // A binary operator replaces the `.`, as `1.+ 2` doesn't parse. Editors only take a range on one line.
            const auto dot = b.tokens[*cursor->previous].begin();
            const auto dot_position = end_of(b.read(0, dot));
            const Range from_dot { { dot_position.line_number, dot_position.codepoint_in_line }, p };
            const auto spaced = dot > 0 && std::isspace(static_cast<unsigned char>(b.text()[dot - 1])) != 0;
            const std::string typed { b.text().substr(dot) };
            for (const auto m : library->responds_to(*c, receiver.class_side)) {
                const auto& info = library->get(m);
                if (!starts_with_ignoring_case(info.selector, prefix) || info.selector.back() == '_')
                    continue;
                CompletionItem item {
                    info.selector, CompletionKind::Method, library->signature(m), Library::where(info.location), {},
                    distance(m)
                };
                const auto first = static_cast<unsigned char>(info.selector.front());
                if (std::isalpha(first) == 0 && first != '_' && from_dot.begin.line == p.line) {
                    item.insert = (spaced ? "" : " ") + info.selector + " ";
                    item.replace = from_dot;
                    item.filter = typed + info.selector;
                }
                add(std::move(item));
            }
            return out;
        }
        if (prefix.empty()) {
            out.incomplete = true; // too many to list until something is typed
            return out;
        }
        for (const auto& selector : library->selectors()) {
            if (!starts_with_ignoring_case(selector, prefix) || selector.back() == '_'
                || std::isalpha(static_cast<unsigned char>(selector.front())) == 0)
                continue;
            const auto& implementors = library->implementors(selector);
            const auto detail = implementors.size() == 1 ? library->signature(implementors.front())
                                                         : std::to_string(implementors.size()) + " implementations";
            add({ selector, CompletionKind::Method, detail, {}, {}, 2 });
        }
        return out;
    }

    const auto expected = expected_at(b, cursor->start);
    const auto keyword_arguments = [&](std::uint8_t rank) {
        if (!has(expected, TokenType::KeywordBinaryOperator))
            return;
        const auto call = call_at(b, cursor->start, library.get());
        if (!call.found || call.selector.empty() || !call.keyword.empty())
            return;
        for (const auto m : methods_for(*library, call.receiver, call.selector)) {
            for (const auto& a : library->get(m).arguments)
                if (starts_with(a.name, prefix))
                    add({ a.name + ":", CompletionKind::Argument, library->signature(m), {}, {}, rank });
            break;
        }
    };

    // Only keyword arguments are offered before anything is typed.
    if (prefix.empty()) {
        keyword_arguments(0);
        return out;
    }

    // Classes.
    if (std::isupper(static_cast<unsigned char>(prefix.front()))) {
        if (!has(expected, TokenType::ClassName))
            return out;
        for (const auto c : library->classes().indices()) {
            const auto& info = library->get(c);
            if (!starts_with_ignoring_case(info.name, prefix) || info.location.path.empty())
                continue;
            std::string detail = info.superclass ? "class : " + library->get(*info.superclass).name : "class";
            add({ info.name,
                  CompletionKind::Class,
                  detail,
                  Library::where(info.location),
                  {},
                  static_cast<std::uint8_t>(starts_with(info.name, prefix) ? 0 : 1) });
        }
        return out;
    }

    // Keyword binary operators: two expressions can't be next to each other, so `x na` is only the start of
    // `x name: y`.
    if (!has(expected, TokenType::Name) && has(expected, TokenType::KeywordBinaryOperator)) {
        const auto receiver = receiver_of(b, library.get(), cursor->start, "x: nil", true);
        std::optional<ClassId> c;
        if (!receiver.class_name.empty())
            c = library->find_class(receiver.class_name);
        const auto keyword = [&](const std::string& selector, std::string detail, std::string where,
                                 std::uint8_t rank) {
            add({ selector + ":", CompletionKind::Method, std::move(detail), std::move(where), selector + ": ", rank });
        };
        if (c) {
            const Distance distance(*library, *c, receiver.class_side);
            for (const auto m : library->responds_to(*c, receiver.class_side)) {
                const auto& info = library->get(m);
                if (!starts_with_ignoring_case(info.selector, prefix) || info.selector.back() == '_'
                    || !takes_an_argument(*library, m))
                    continue;
                keyword(info.selector, library->signature(m), Library::where(info.location), distance(m));
            }
            return out;
        }
        for (const auto& selector : library->selectors()) {
            if (!starts_with_ignoring_case(selector, prefix) || selector.back() == '_'
                || std::isalpha(static_cast<unsigned char>(selector.front())) == 0)
                continue;
            const auto& implementors = library->implementors(selector);
            if (std::none_of(implementors.begin(), implementors.end(),
                             [&](MethodId m) { return takes_an_argument(*library, m); }))
                continue;
            const auto detail = implementors.size() == 1 ? library->signature(implementors.front())
                                                         : std::to_string(implementors.size()) + " implementations";
            keyword(selector, detail, {}, 2);
        }
        return out;
    }

    // Names: keyword arguments of the call, locals in scope, the class's variables, keywords.
    keyword_arguments(0);
    std::set<std::string> seen;
    if (has(expected, TokenType::Name)) {
        for (const auto& l : analysis.locals) {
            if (!starts_with(l.name, prefix) || l.name == prefix || !l.scope.touches(p) || p < l.declared)
                continue;
            if (seen.insert(l.name).second)
                add({ l.name, CompletionKind::Variable, l.argument ? "argument" : "variable", {}, {}, 1 });
        }
        if (document.kind() == DocumentKind::ClassFile) {
            for (const auto& c : library->classes().raw()) {
                if (c.location.path != document.path())
                    continue;
                for (const auto ancestor : library->superclasses(*library->find_class(c.name)))
                    for (const auto& m : library->get(ancestor).members)
                        if (starts_with(m.name, prefix) && seen.insert(m.name).second)
                            add({ m.name,
                                  CompletionKind::Member,
                                  std::string { m.kind == MemberKind::Instance    ? "var"
                                                    : m.kind == MemberKind::Class ? "classvar"
                                                                                  : "const" }
                                      + " in " + library->get(ancestor).name,
                                  {},
                                  {},
                                  2 });
            }
        }
    }
    for (const auto& [k, token] : keywords)
        if (starts_with(k, prefix) && prefix != k && has(expected, token))
            add({ k, CompletionKind::Keyword, "keyword", {}, {}, 3 });
    return out;
}

////////////////////////////////////////////////////////////////////////////////
// Signature help
////////////////////////////////////////////////////////////////////////////////

std::optional<SignatureHelp> signature_help(const Document& document, Position p) {
    const auto& library = document.library();
    if (!library)
        return std::nullopt;
    const auto cursor = cursor_at(document, p);
    using In = Cursor::In;
    if (!cursor || cursor->in == In::Comment || cursor->in == In::String || cursor->in == In::QuotedSymbol)
        return std::nullopt;
    const auto& b = cursor->before;
    const auto call = call_at(b, static_cast<std::uint32_t>(b.text().size()), library.get());
    if (!call.found || call.selector.empty())
        return std::nullopt;
    auto methods = methods_for(*library, call.receiver, call.selector);
    if (methods.empty())
        return std::nullopt;
    constexpr std::size_t shown = 6;
    if (methods.size() > shown)
        methods.erase(methods.begin() + shown, methods.end());
    SignatureHelp help;
    for (const auto m : methods) {
        const auto& info = library->get(m);
        Signature sig;
        sig.label = library->get(info.owner).name + (info.class_method ? ".*" : ":") + info.selector + "(";
        const auto param = [&](const std::string& t) {
            const auto begin = count_codepoints(sig.label);
            sig.label += t;
            sig.parameters.emplace_back(begin, count_codepoints(sig.label));
        };
        bool first = true;
        for (const auto& a : info.arguments) {
            if (!first)
                sig.label += ", ";
            first = false;
            param(a.default_value.empty() ? a.name : a.name + " = " + a.default_value);
        }
        if (!info.variadic.empty()) {
            if (!first)
                sig.label += ", ";
            param("..." + info.variadic);
        }
        sig.label += ")";
        sig.documentation = Library::where(info.location);
        help.signatures.push_back(std::move(sig));
    }
    // A function call's receiver is its first argument, `foo(a, b)` is `a.foo(b)`.
    std::int32_t active = call.argument - (call.function_call ? 1 : 0);
    if (!call.keyword.empty()) {
        const auto& info = library->get(methods.front());
        for (std::size_t i = 0; i < info.arguments.size(); ++i)
            if (info.arguments[i].name == call.keyword)
                active = static_cast<std::int32_t>(i);
    }
    help.active_parameter = static_cast<std::uint32_t>(std::max(0, active));
    return help;
}

}
