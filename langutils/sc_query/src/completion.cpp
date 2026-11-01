// Completion and signature help. Both read the text before the cursor, as the code being typed rarely parses, and use
// the last compile only for the locals in scope.

#include "sc_query/queries.hpp"

#include <algorithm>
#include <cctype>
#include <set>

namespace sc::query {

namespace {

////////////////////////////////////////////////////////////////////////////////
// Scanning the text before the cursor
////////////////////////////////////////////////////////////////////////////////

bool word_char(char c) { return std::isalnum(static_cast<unsigned char>(c)) || c == '_'; }

// The brackets open at the cursor, and whether it is in a string, symbol or comment.
struct Scan {
    enum struct State : std::uint8_t { Code, String, QuotedSymbol, LineComment, BlockComment };
    struct Frame {
        char open;
        std::size_t at; // the bracket
        std::int32_t commas { 0 };
        std::size_t last_comma; // the bracket, before the first comma
        std::size_t previous_comma;
    };
    State state { State::Code };
    std::size_t quote { 0 }; // the opening quote of a quoted symbol
    std::vector<Frame> frames;
};

Scan scan(std::string_view text, std::size_t end) {
    Scan s;
    int comment_depth = 0;
    for (std::size_t i = 0; i < end && i < text.size(); ++i) {
        const char c = text[i];
        const char next = i + 1 < text.size() ? text[i + 1] : '\0';
        switch (s.state) {
        case Scan::State::Code:
            if (c == '/' && next == '/') {
                s.state = Scan::State::LineComment;
                ++i;
            } else if (c == '/' && next == '*') {
                s.state = Scan::State::BlockComment;
                comment_depth = 1;
                ++i;
            } else if (c == '"') {
                s.state = Scan::State::String;
            } else if (c == '\'') {
                s.state = Scan::State::QuotedSymbol;
                s.quote = i;
            } else if (c == '$') {
                i += next == '\\' ? 2 : 1;
            } else if (c == '(' || c == '[' || c == '{') {
                s.frames.push_back({ c, i, 0, i, i });
            } else if (c == ')' || c == ']' || c == '}') {
                if (!s.frames.empty())
                    s.frames.pop_back();
            } else if (c == ',' && !s.frames.empty()) {
                auto& f = s.frames.back();
                f.previous_comma = f.last_comma;
                f.last_comma = i;
                ++f.commas;
            }
            break;
        case Scan::State::String:
            if (c == '\\')
                ++i;
            else if (c == '"')
                s.state = Scan::State::Code;
            break;
        case Scan::State::QuotedSymbol:
            if (c == '\\')
                ++i;
            else if (c == '\'')
                s.state = Scan::State::Code;
            break;
        case Scan::State::LineComment:
            if (c == '\n' || c == '\r')
                s.state = Scan::State::Code;
            break;
        case Scan::State::BlockComment:
            if (c == '/' && next == '*') {
                ++comment_depth;
                ++i;
            } else if (c == '*' && next == '/') {
                ++i;
                if (--comment_depth == 0)
                    s.state = Scan::State::Code;
            }
            break;
        }
    }
    return s;
}

std::string_view trim(std::string_view s) {
    while (!s.empty() && std::isspace(static_cast<unsigned char>(s.front())))
        s.remove_prefix(1);
    while (!s.empty() && std::isspace(static_cast<unsigned char>(s.back())))
        s.remove_suffix(1);
    return s;
}

std::size_t skip_space_back(std::string_view text, std::size_t end) {
    while (end > 0 && std::isspace(static_cast<unsigned char>(text[end - 1])))
        --end;
    return end;
}

std::size_t word_start(std::string_view text, std::size_t end) {
    while (end > 0 && word_char(text[end - 1]))
        --end;
    return end;
}

bool is_class_name(std::string_view s) { return !s.empty() && std::isupper(static_cast<unsigned char>(s.front())); }

// What the expression ending at @p end is known to be: a class, or a literal of a known class.
struct Receiver {
    std::string class_name; // empty when not known
    bool class_side { false };
};

Receiver receiver_before(std::string_view text, std::size_t end, int depth = 0);

// `Foo.new` or `Foo.new(...)`, and the other `new` methods, ending at @p end, guessed to make an instance of Foo.
Receiver instance_made_before(std::string_view text, std::size_t end, int depth) {
    if (depth > 4)
        return {};
    bool called = false;
    if (end > 0 && text[end - 1] == ')') {
        called = true;
        int nesting = 0;
        std::size_t i = end;
        while (i > 0) {
            const char c = text[--i];
            if (c == ')')
                ++nesting;
            else if (c == '(' && --nesting == 0)
                break;
        }
        if (nesting != 0)
            return {};
        end = skip_space_back(text, i);
    }
    const auto start = word_start(text, end);
    const auto word = text.substr(start, end - start);
    // `Foo(...)` is `Foo.new(...)`.
    if (called && is_class_name(word) && (start == 0 || text[start - 1] != '.'))
        return { std::string { word }, false };
    if (word.substr(0, 3) != "new" || start == 0 || text[start - 1] != '.')
        return {};
    const auto inner = receiver_before(text, start - 1, depth + 1);
    if (inner.class_side)
        return { inner.class_name, false };
    return {};
}

Receiver receiver_before(std::string_view text, std::size_t end, int depth) {
    end = skip_space_back(text, end);
    if (end == 0)
        return {};
    if (const auto made = instance_made_before(text, end, depth); !made.class_name.empty())
        return made;
    const char last = text[end - 1];
    if (last == '"')
        return { "String" };
    if (last == '\'')
        return { "Symbol" };
    if (last == '}')
        return { "Function" };
    if (!word_char(last))
        return {};
    const auto start = word_start(text, end);
    const auto word = text.substr(start, end - start);
    if (std::isdigit(static_cast<unsigned char>(word.front()))) {
        // 1.5 is a Float, the dot after 1 starts the message.
        const bool fraction =
            start >= 2 && text[start - 1] == '.' && std::isdigit(static_cast<unsigned char>(text[start - 2]));
        return { fraction ? "Float" : "Integer" };
    }
    if (start > 0 && text[start - 1] == '\\')
        return { "Symbol" };
    if (start > 0 && text[start - 1] == '$')
        return { "Char" };
    if (start > 0 && text[start - 1] == '~')
        return {};
    if (is_class_name(word))
        return { std::string { word }, true };
    if (word == "nil")
        return { "Nil" };
    if (word == "true")
        return { "True" };
    if (word == "false")
        return { "False" };
    if (word == "inf" || word == "pi")
        return { "Float" };
    if (word == "thisProcess")
        return { "Main" };
    if (word == "thisThread")
        return { "Thread" };
    if (word == "thisFunction")
        return { "Function" };
    if (word == "thisMethod")
        return { "Method" };
    if (word == "currentEnvironment" || word == "topEnvironment")
        return { "Environment" };
    return {};
}

// The call the cursor is an argument of, from the innermost open bracket.
struct Call {
    bool found { false };
    Receiver receiver;
    std::string selector; // empty for a bare `(`, e.g., an event
    std::int32_t argument { 0 };
    std::string previous; // the argument before this one
    std::string keyword; // when written `key: ...`
    std::size_t open { 0 };
    bool function_call { false }; // `foo(a, b)`, which is `a.foo(b)`
};

Call call_at(std::string_view text, const Scan& s, std::size_t cursor) {
    Call call;
    if (s.frames.empty() || s.frames.back().open != '(')
        return call;
    const auto& f = s.frames.back();
    call.found = true;
    call.open = f.at;
    call.argument = f.commas;
    const auto current = trim(text.substr(f.last_comma + 1, cursor - f.last_comma - 1));
    // `key: value`
    std::size_t k = 0;
    while (k < current.size() && word_char(current[k]))
        ++k;
    if (k > 0 && k < current.size() && current[k] == ':' && std::islower(static_cast<unsigned char>(current[0])))
        call.keyword = std::string { current.substr(0, k) };
    if (f.commas > 0)
        call.previous = std::string { trim(text.substr(f.previous_comma + 1, f.last_comma - f.previous_comma - 1)) };

    const auto before = skip_space_back(text, f.at);
    const auto start = word_start(text, before);
    const auto word = text.substr(start, before - start);
    if (word.empty() || std::isdigit(static_cast<unsigned char>(word.front())))
        return call;
    if (start > 0 && text[start - 1] == '.') {
        call.selector = std::string { word };
        call.receiver = receiver_before(text, start - 1);
    } else if (is_class_name(word)) {
        call.selector = "new";
        call.receiver = { std::string { word }, true };
    } else if (!(start > 0 && (text[start - 1] == '\\' || text[start - 1] == '~'))) {
        // `foo(x)`, the receiver is the first argument.
        call.selector = std::string { word };
        call.function_call = true;
    }
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

// Words written after a marker in the text, e.g., `\name` and `'name'` symbols, or `~name`.
std::set<std::string> words_after(std::string_view text, char marker, std::size_t skip_at) {
    std::set<std::string> out;
    for (std::size_t i = 0; i < text.size(); ++i) {
        if (text[i] != marker || i == skip_at)
            continue;
        std::size_t j = i + 1;
        while (j < text.size() && word_char(text[j]))
            ++j;
        if (j > i + 1)
            out.insert(std::string { text.substr(i + 1, j - i - 1) });
        if (marker == '\'' && j < text.size() && text[j] == '\'')
            i = j;
    }
    return out;
}

constexpr std::size_t max_items = 400;

const char* const keywords[] = { "var",
                                 "arg",
                                 "this",
                                 "super",
                                 "nil",
                                 "true",
                                 "false",
                                 "inf",
                                 "pi",
                                 "thisProcess",
                                 "thisThread",
                                 "thisFunction",
                                 "thisMethod",
                                 "currentEnvironment",
                                 "topEnvironment",
                                 "classvar",
                                 "const" };

std::vector<MethodId> methods_for(const Library& library, const Receiver& r, std::string_view selector) {
    if (!r.class_name.empty())
        if (const auto c = library.find_class(r.class_name))
            if (const auto m = library.lookup(*c, r.class_side, selector))
                return { *m };
    return library.implementors(selector);
}

}

////////////////////////////////////////////////////////////////////////////////
// Completion
////////////////////////////////////////////////////////////////////////////////

Completions complete(const Document& document, Position p, HookHost* hooks) {
    Completions out;
    const auto& text = document.text();
    const auto cursor = document.lines().offset(p);
    const auto s = scan(text, cursor);
    const auto analysis = analyse_names(document);
    const auto library = analysis.library ? analysis.library : document.library();
    if (!library)
        return out;

    const auto start = word_start(text, cursor);
    const auto prefix = std::string { text.substr(start, cursor - start) };
    out.replace = { document.lines().position(start), p };
    const char marker = start > 0 ? text[start - 1] : '\0';
    const auto add = [&](CompletionItem item) {
        if (out.items.size() >= max_items) {
            out.incomplete = true;
            return;
        }
        out.items.push_back(std::move(item));
    };

    if (s.state == Scan::State::String || s.state == Scan::State::LineComment || s.state == Scan::State::BlockComment)
        return out;

    // Symbols: `\na` or `'na`.
    const bool quoted = s.state == Scan::State::QuotedSymbol;
    if (marker == '\\' || quoted) {
        const auto symbol_prefix = quoted ? std::string { text.substr(s.quote + 1, cursor - s.quote - 1) } : prefix;
        if (quoted)
            out.replace = { document.lines().position(s.quote + 1), p };
        std::set<std::string> seen;
        const auto symbol = [&](const std::string& name, std::string detail, std::uint8_t rank) {
            if (!starts_with(name, symbol_prefix) || !seen.insert(name).second)
                return;
            add({ name, CompletionKind::Symbol, std::move(detail), {}, {}, rank });
        };
        if (hooks) {
            const auto call = call_at(text, s, quoted ? s.quote : start - 1);
            SymbolContext context;
            context.receiver = call.receiver.class_name;
            context.selector = call.selector;
            context.argument = call.found ? call.argument : -1;
            context.previous = call.previous;
            context.keyword = call.keyword;
            context.prefix = symbol_prefix;
            for (const auto& c : hooks->complete_symbol(*library, context))
                symbol(c.label, c.detail.empty() ? c.hook : c.detail, 0);
        }
        for (const auto& name : words_after(text, '\\', start - 1))
            symbol(name, "in this document", 1);
        for (const auto& name : words_after(text, '\'', quoted ? s.quote : std::string::npos))
            symbol(name, "in this document", 1);
        for (const auto& name : library->symbols())
            symbol(name, {}, 2);
        return out;
    }

    // Environment variables: `~na`.
    if (marker == '~') {
        for (const auto& name : words_after(text, '~', start - 1))
            if (starts_with(name, prefix))
                add({ name, CompletionKind::Environment, "environment variable", {}, {}, 1 });
        return out;
    }

    // Messages: `x.na`.
    if (marker == '.') {
        const auto receiver = receiver_before(text, start - 1);
        std::optional<ClassId> c;
        if (!receiver.class_name.empty())
            c = library->find_class(receiver.class_name);
        if (c) {
            for (const auto m : library->responds_to(*c, receiver.class_side)) {
                const auto& info = library->get(m);
                if (!starts_with_ignoring_case(info.selector, prefix) || info.selector.back() == '_')
                    continue;
                const bool own = info.owner == *c;
                add({ info.selector,
                      CompletionKind::Method,
                      library->signature(m),
                      Library::where(info.location),
                      {},
                      static_cast<std::uint8_t>(own ? 0 : 1) });
            }
            return out;
        }
        if (prefix.empty() && receiver.class_name.empty()) {
            out.incomplete = true; // too many to list until something is typed
            return out;
        }
        for (const auto& selector : library->selectors()) {
            if (!starts_with_ignoring_case(selector, prefix) || selector.back() == '_')
                continue;
            const auto& implementors = library->implementors(selector);
            if (std::isalpha(static_cast<unsigned char>(selector.front())) == 0)
                continue;
            const auto detail = implementors.size() == 1 ? library->signature(implementors.front())
                                                         : std::to_string(implementors.size()) + " implementations";
            add({ selector, CompletionKind::Method, detail, {}, {}, 2 });
        }
        return out;
    }

    if (prefix.empty() && !quoted) {
        // Only keyword arguments are offered before anything is typed.
        const auto call = call_at(text, s, cursor);
        if (call.found && !call.selector.empty() && call.keyword.empty())
            for (const auto m : methods_for(*library, call.receiver, call.selector)) {
                for (const auto& a : library->get(m).arguments)
                    add({ a.name + ":", CompletionKind::Argument, library->signature(m), {}, {}, 0 });
                break;
            }
        return out;
    }
    if (std::isdigit(static_cast<unsigned char>(prefix.front())))
        return out;

    // Classes.
    if (is_class_name(prefix)) {
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

    // Names: keyword arguments of the call, locals in scope, the class's variables, keywords.
    const auto call = call_at(text, s, start);
    if (call.found && !call.selector.empty() && call.keyword.empty()) {
        for (const auto m : methods_for(*library, call.receiver, call.selector)) {
            for (const auto& a : library->get(m).arguments)
                if (starts_with(a.name, prefix))
                    add({ a.name + ":", CompletionKind::Argument, library->signature(m), {}, {}, 0 });
            break;
        }
    }
    std::set<std::string> seen;
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
    for (const auto* k : keywords)
        if (starts_with(k, prefix) && prefix != k)
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
    const auto& text = document.text();
    const auto cursor = document.lines().offset(p);
    const auto s = scan(text, cursor);
    if (s.state != Scan::State::Code)
        return std::nullopt;
    const auto call = call_at(text, s, cursor);
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
