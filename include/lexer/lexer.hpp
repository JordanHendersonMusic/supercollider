#pragma once

#include <cassert>
#include <limits>
#include <cstddef>
#include <cstdint>
#include <optional>
#include <tuple>
#include <array>
#include <utility>
#include <vector>

namespace sc::lex {

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Main interface
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

/*

To lex SuperCollider source code, first you must setup a CodePointStream, then you must define and create an Action.
These are then both passed by reference to lexer.

The Action is in charge of creating semantic values, responding to warnings, and errors.
The TypeAndLocationAction provided only returns the TokenType and the SourceCodeRange

In the SuperCollider lang there is an action the mutates global state to communicate with the Bison generated parser.
You can also use this for code formatting and syntax highlighting.

The lexer here does not do any processing of the semantic values, meaning for example, the strings are not escaped.
This must happen elsewhere.
Likewise, we do not join string lines together into one string.

*/

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Here is the public api and documentation. Everything in the 'details' namespace is private, do not use.

// Unicode code point
using CodePoint = std::uint32_t;
inline constexpr CodePoint ascii_to_codepoint(char c);

// Location and range in source code and file.
struct SourceCodeLocation;
struct SourceCodeRange;
struct FileCodeLocation;
struct FileCodeRange;

// The type of the token produced by the lexer.
enum struct TokenType;

// Helpers to identify tokens.
constexpr inline bool is_error(TokenType t);
constexpr inline bool is_whitespace(TokenType t);
constexpr inline bool is_comment(TokenType t);
constexpr inline bool is_identifier(TokenType t);
constexpr inline bool is_integer(TokenType t);
constexpr inline bool is_float(TokenType t);
constexpr inline bool is_accidental(TokenType t);
constexpr inline bool is_symbol(TokenType t);
constexpr inline bool is_boolean(TokenType t);

// Used to iterate through the source code.
class CodePointStream;

// Main lexer function.
template <typename Action> auto lexer(CodePointStream& stream, Action& action) noexcept -> typename Action::Output;

/*

Actions take in a Token and SourceCodeRange and produce an optional.
If the optional is empty, the lexer function skips this token, otherwise, it returns the output unwrapped.

See TypeAndLocationAction for more documentation.

struct ActionExample {
    using Output = ...;  //

    template <TokenType type> std::optional<Output> process(SourceCodeRange loc);
};
*/

// Used to produce output. Serves as an example action, you can define your own.
struct TypeAndLocationAction;

namespace errors {
struct SymbolQuoteContainsNewLine;
struct SymbolQuoteUnclosed;
struct MissingExponent;
struct StringUnclose;
struct MultilineCommentUnclosed;
struct InvalidUTF8;
struct UnexpectedCodePoint;
}


////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// details
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

namespace details {

template <typename CRTP> struct TextLocationLocation {
    TextLocationLocation() = default;
    constexpr TextLocationLocation(std::size_t absolute, std::size_t lineNumber, std::size_t column) noexcept:
        absolute(absolute),
        lineNumber(lineNumber),
        column(column) {};
    constexpr TextLocationLocation(TextLocationLocation&&) noexcept = default;
    constexpr TextLocationLocation(const TextLocationLocation&) noexcept = default;
    constexpr TextLocationLocation& operator=(TextLocationLocation&&) noexcept = default;
    constexpr TextLocationLocation& operator=(const TextLocationLocation&) noexcept = default;

    [[nodiscard]] constexpr bool operator==(const CRTP& o) const noexcept { return tuple() == o.tuple(); }
    [[nodiscard]] constexpr auto tuple() const noexcept -> std::tuple<std::size_t, std::size_t, std::size_t> {
        return { absolute, lineNumber, column };
    }

    std::size_t absolute { 0 }; // Offset as a byte index into the text.
    std::size_t lineNumber { 0 }; // Zero indexed, first line is zero.
    std::size_t column {
        0
    }; // Codepoint count, note, this is currently broken and doesn't work with multicodepoint graphemes.
};

// A range of points in some text.
template <typename POINT> struct TextLocationRange {
    using Point = POINT;
    TextLocationRange() noexcept = default;
    constexpr TextLocationRange(Point begin, Point end) noexcept: begin(begin), end(end) {}
    constexpr TextLocationRange(TextLocationRange&&) noexcept = default;
    constexpr TextLocationRange(const TextLocationRange&) noexcept = default;
    constexpr TextLocationRange& operator=(TextLocationRange&&) noexcept = default;
    constexpr TextLocationRange& operator=(const TextLocationRange&) noexcept = default;

    [[nodiscard]] constexpr auto size() const { return end.absolute - begin.absolute; }
    [[nodiscard]] constexpr auto line_count() const { return (end.lineNumber - begin.lineNumber) + 1; }
    [[nodiscard]] constexpr static TextLocationRange range(TextLocationRange left, TextLocationRange right) {
        return { left.begin, right.end };
    }

    Point begin, end;
};

} // details


////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Locations in source code snippets or files. Some C++ rigmarole is required to make them strong types.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
struct SourceCodeLocation : public details::TextLocationLocation<SourceCodeLocation> {
    using TextLocationLocation::TextLocationLocation;
};

struct FileCodeLocation : public details::TextLocationLocation<FileCodeLocation> {
    using TextLocationLocation::TextLocationLocation;
};

struct SourceCodeRange : details::TextLocationRange<SourceCodeLocation> {
    using TextLocationRange<SourceCodeLocation>::TextLocationRange;
    using TextLocationRange<SourceCodeLocation>::operator=;
};
struct FileCodeRange : details::TextLocationRange<FileCodeLocation> {
    using TextLocationRange<FileCodeLocation>::TextLocationRange;
    using TextLocationRange<FileCodeLocation>::operator=;
};


////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Codepoint --- represents a unicode code point. T
// Node: we do not work with graphemes, so all multicodepoint unicode must be handled by the logic in the lexer.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
static constexpr CodePoint invalid_utf8_flag = std::numeric_limits<CodePoint>::max();

// Returns code point and number of utf8 bytes consumed
inline std::tuple<CodePoint, std::uint8_t> char_sequence_to_codepoint(const char* source, size_t pos,
                                                                      size_t len) noexcept {
    const auto* c = reinterpret_cast<const unsigned char*>(source);
    assert(pos < len);

    const auto u = static_cast<CodePoint>(c[pos]);

    if (u < 0b1000'0000)
        return { static_cast<CodePoint>(u), 1 };

    if (u < 0b1100'0000) {
        return { invalid_utf8_flag, 1 };
    }

    if (u < 0b1110'0000) { // two bytes
        const auto chigh = static_cast<CodePoint>(c[pos]);
        if (pos + 1 >= len)
            return { invalid_utf8_flag, len - pos };

        const auto clow = static_cast<CodePoint>(c[pos + 1]);
        return { ((chigh & 0b00111111) << 6) | (clow & 0b00111111), 2 };
    }
    if (u < 0b1111'0000) { // three bytes
        if (pos + 2 >= len)
            return { invalid_utf8_flag, len - pos };

        const auto chigh = static_cast<CodePoint>(c[pos]);
        const auto cmid = static_cast<CodePoint>(c[pos + 1]);
        const auto clow = static_cast<CodePoint>(c[pos + 2]);
        return { ((chigh & 0b00001111) << 12) | ((cmid & 0b00111111) << 6) | (clow & 0b00111111), 3 };
    }
    if (u < 0b1111'1000) { // four bytes
        if (pos + 3 >= len)
            return { invalid_utf8_flag, len - pos };
        const auto chigh = static_cast<CodePoint>(c[pos]);
        const auto cmid = static_cast<CodePoint>(c[pos + 1]);
        const auto cmid2 = static_cast<CodePoint>(c[pos + 2]);
        const auto clow = static_cast<CodePoint>(c[pos + 3]);
        return { ((chigh & 0b00001111) << 18) | ((cmid & 0b00111111) << 12) | ((cmid2 & 0b00111111) << 6)
                     | (clow & 0b00111111),
                 4 };
    }

    return { invalid_utf8_flag, 1 };
}


inline constexpr CodePoint ascii_to_codepoint(char c) {
    assert(c >= 0 && c <= 127);
    return static_cast<CodePoint>(c);
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// CodePointStream - Walks through source code.
// Its job is to return information about where we are in the source code, allow conversions to locations in the file
//      and to return the 'character'.
// It does not discard anything.
// It cannot undo.
// Annoyingly, it has state, because when in a command line context, the first token must be Interpret,
//    call should_leave_cmd_initial to see if this should be returned.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
class CodePointStream {
    // The first time we enter cmd mode, we must return Interpret.
    enum struct Mode { ClassLibrary, CMDInitial, CMDContinue };

public:
    template <size_t N> struct Peek {
        template <size_t M> [[nodiscard]] constexpr Peek<M> shrink_to() const noexcept;
        [[nodiscard]] constexpr CodePoint operator[](size_t n) const noexcept;
        template <size_t M> [[nodiscard]] constexpr bool operator==(std::array<CodePoint, M> c) const noexcept;
        template <size_t M> [[nodiscard]] constexpr bool operator!=(std::array<CodePoint, M> c) const noexcept;
        std::array<CodePoint, N> characters;
    };

    CodePointStream(bool in_class_library, const char* source, size_t source_length,
                    FileCodeLocation source_start_in_file);

    CodePointStream() = delete;
    CodePointStream(CodePointStream&&) noexcept = default;
    CodePointStream& operator=(CodePointStream&&) noexcept = default;
    CodePointStream(const CodePointStream&) = delete;
    CodePointStream& operator=(const CodePointStream&) = delete;


    // SourceCodeRange manipulation

    [[nodiscard]] FileCodeRange source_to_file(const SourceCodeRange& source) const;
    [[nodiscard]] FileCodeLocation source_to_file(const SourceCodeLocation& source) const;

    // returns begin and size
    [[nodiscard]] std::tuple<const char*, std::size_t> source_range(const SourceCodeRange& range) const;

    struct LineIter {
        const CodePointStream& cps;
        std::size_t start_line, end_line;
        std::size_t current_line { start_line };
        [[nodiscard]] inline std::optional<std::tuple<const char*, std::size_t, std::size_t>> operator()();
    };
    friend struct LineIter;

    [[nodiscard]] LineIter line_iter(const SourceCodeRange& range, std::size_t lines_before = 0,
                                     std::size_t lines_after = 0) const;

    // Given a location in a line, return the start of that line.
    [[nodiscard]] SourceCodeLocation line_start(SourceCodeLocation p) const;


    // Advancing through stream.

    [[nodiscard]] std::tuple<SourceCodeLocation, CodePoint> start_token();

    [[nodiscard]] SourceCodeLocation end_token() const;

    // Must call this first. If true, then lexer should emit the Interpret token.
    // This mutates states, so repeated calls will return false.
    [[nodiscard]] bool should_leave_cmd_initial() noexcept;

    // Returns next character. Mutates state.
    CodePoint advance();

    // This is used to force the skipping of bytes.
    // Should not be used in normal use, but is useful when testing.
    void force_skip(size_t amount = 1) { next.absolute += amount; };

    // Peeking is the same as advance, but does not advance the state.
    [[nodiscard]] CodePoint peek() const;
    template <size_t N> [[nodiscard]] Peek<N> peek_n() const;

    // Commits peek to the current token.
    template <size_t N> SourceCodeLocation advance_by_peek(Peek<N> peek);

    // Peeks, if equal to one of c, advances.
    // Usage: if (stream.peek_advance_if('.')) { ... consumed '.' ... };
    // Usage: if (stream.peek_advance_if('+', '-')) { ... consumed '-' or '+' ... };
    template <typename... C> bool peek_advance_if(C... cs);
    // Advance followed by peek
    [[nodiscard]] CodePoint advance_and_peek();

    // Null terminator is NEVER accepted here.
    template <typename Predicate> SourceCodeLocation advance_while(Predicate&& predicate);
    template <typename Predicate> size_t advance_while_count(Predicate&& predicate);

    // Does not clear the new line cache. Goes back to the beginning.
    void reset();


private:
    FileCodeLocation source_start_in_file; // Allows us to get the location in the file from the source code location.
    SourceCodeLocation next; // Iterator through code, points to the next not the current.
    const char* source; // Text, may not be null terminated. Only access this inside read.
    size_t source_length; // Text length.
    // Cache of new line locations, in source code, not file.
    mutable std::vector<std::uint32_t> abs_new_line_locations { 0 };
    Mode mode;

    // Also returns the character size in bytes.
    [[nodiscard]] std::tuple<CodePoint, std::uint8_t> read(size_t pos) const noexcept;
    void increment_source_code_point(SourceCodeLocation& p, CodePoint c, std::uint8_t sz) const;
    void increment_source_code_point(SourceCodeLocation& p, CodePoint c) const;
};

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// TokenType
// An enum covering all the types of things the lexer sees.
// A conversion function should be provided to yytokentype if using the bison parser.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

enum struct TokenType : int {
    EndOfFile = 0,
    // All the ascii characters are here, useful for literals, does not support unicode literals.

    // Identifiers
    Name = 1024,
    ClassName,
    PrimitiveName,

    // Literals - ints
    Integer,
    IntegerRadix,
    Hexidecimal,

    // Literals - floats
    Float,
    FloatRadix,
    FloatExponent,
    Pi,
    Inf,

    // Literals - accidentals
    AccidentalSteps,
    AccidentalCents,

    // Literals - symbols
    SymbolSlash, //   \symbol
    SymbolQuote, // 'symbol literal'

    // Literals - chars
    Ascii, // character

    // Literals - booleans
    True,
    False,

    // Literals - nil
    Nil,

    // Literals - string
    StringLine,

    // Keywords
    While,
    Var,
    Arg,
    ClassVar,
    Const,

    // Punctuation
    LeftArrow,
    ReadWriteVar,
    Ellipsis,
    DotDot,
    CurryArg,

    // Operators
    BinaryOperator,
    KeywordBinaryOperator,

    // Special
    BeginClosedFunction,
    Interpret,

    // white space
    Space = 2048,
    NewLine,
    Tab,

    // comments
    Comment = 3072,
    DocumentationComment,
    MultiLineComment,

    // errors
    ErUnexpected = 4096,
    ErSymbolQuoteUnclosed,
    ErMultilineCommentUnclosed,
    ErStringUnclosed,
    ErMissingExponent,
    ErInvalidToken,
    ErInvalidUTF8,

    ErFirstUserDefinedError = 5119
};

template <typename... ARGS> constexpr inline bool matches(TokenType t, ARGS... args) {
    return ((t == static_cast<TokenType>(args)) || ...);
}

constexpr inline bool is_ascii_literal(TokenType t) {
    const auto v = static_cast<int>(t);
    return 0 < v && v < 128;
}

constexpr inline bool is_error(TokenType t) {
    const auto v { static_cast<int>(t) };
    return static_cast<int>(TokenType::ErUnexpected) <= v;
}

constexpr inline bool is_whitespace(TokenType t) {
    const auto v { static_cast<int>(t) };
    return 2048 <= v && v < 3072;
}

constexpr inline bool is_comment(TokenType t) {
    const auto v { static_cast<int>(t) };
    return 3072 <= v && v < 4096;
}

constexpr inline bool is_identifier(TokenType t) {
    return matches(t, TokenType::Name, TokenType::PrimitiveName, TokenType::ClassName);
}

constexpr inline bool is_keyword(TokenType t) {
    return matches(t, TokenType::Arg, TokenType::Var, TokenType::Const, TokenType::While, TokenType::ClassVar);
}

constexpr inline bool is_integer(TokenType t) {
    return matches(t, TokenType::Integer, TokenType::IntegerRadix, TokenType::Hexidecimal);
}

// Also includes constants pi and inf
constexpr inline bool is_float(TokenType t) {
    return matches(t, TokenType::Float, TokenType::FloatRadix, TokenType::FloatExponent, TokenType::Pi, TokenType::Inf);
}

constexpr inline bool is_constant(TokenType t) {
    return matches(t, TokenType::Pi, TokenType::Nil, TokenType::Inf, TokenType::True, TokenType::False);
}

constexpr inline bool is_accidental(TokenType t) {
    return matches(t, TokenType::AccidentalCents, TokenType::AccidentalSteps);
}

constexpr inline bool is_symbol(TokenType t) { return matches(t, TokenType::SymbolQuote, TokenType::SymbolSlash); }

// Constants true and false
constexpr inline bool is_boolean(TokenType t) { return matches(t, TokenType::True, TokenType::False); }

namespace literals {
inline constexpr TokenType operator""_tokentype(char c) { return static_cast<TokenType>(c); }
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Semantic Action, used to do stuff.
// The main lexer function takes in a template argument call Action which is in charge of emitting or discarding
// tokens. `typename Action::Output` becomes the return type of lexer. You cannot discard the EndOfFile token,
// ensure this returns a filled optional.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
struct TypeAndLocationAction {
    struct Output {
        TokenType type {};
        SourceCodeRange range {};
    };

    // Return std::nullopt to discard the token type.
    // Don't use template speicialisation as this isn't valid c++, instead use if constexpr.
    template <TokenType type> std::optional<Output> process(SourceCodeRange loc) { return { { type, loc } }; }
};


////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Helpers used in main lexer function
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
namespace details {

constexpr inline bool is_newline(CodePoint c) { return c == '\n' || c == '\r'; }

constexpr inline bool is_space(CodePoint c) {
    return c == ' ' || c == '\v' || c == '\f'
        || c == 0x00a0 // This is the Ux00a0, no-break-space, often occurs when copying code from the web
        ;
}

constexpr inline bool is_whitespace(CodePoint c) { return is_newline(c) || is_space(c); }

constexpr inline bool is_control_code(CodePoint c) { return (1 <= c && c <= 8) || (14 <= c && c <= 31) || c == 127; }
constexpr inline bool is_lower(CodePoint c) { return 'a' <= c && c <= 'z'; }
constexpr inline bool is_upper(CodePoint c) { return 'A' <= c && c <= 'Z'; }
constexpr inline bool is_numeric(CodePoint c) { return '0' <= c && c <= '9'; }
constexpr inline bool is_start_of_class(CodePoint c) { return is_upper(c); }

constexpr inline bool is_starting_identifier(CodePoint c) { return is_lower(c) || is_upper(c); }

constexpr inline bool is_continuing_identifier(CodePoint c) {
    return is_lower(c) || is_upper(c) || is_numeric(c) || c == '_';
}

constexpr inline bool is_binary_operator_character(CodePoint c) {
    constexpr std::array<CodePoint, 13> binary_operator_characters { '!', '@', '%', '&', '*', '-', '+',
                                                                     '=', '|', '<', '>', '?', '/' };
    for (CodePoint b : binary_operator_characters)
        if (b == c)
            return true;
    return false;
}

constexpr inline int str_to_int_for_radix(const char* str, size_t n, int base) {
    // TODO: in future it would be nice to remove this from the lexer, it means changing the language to accept
    // invalid radixs at the lexing stage, but makes things context dependant.
    int out { 0 };
    for (size_t i { 0 }; i < n; ++i) {
        const char c = *str++;
        if (c >= '0' && c <= '0' + std::min(10, base) - 1)
            out = out * base + c - '0';
        else if (c >= 'a' && c <= 'a' + std::min(36, base) - 11)
            out = out * base + c - 'a' + 10;
        else if (c >= 'A' && c <= 'A' + std::min(36, base) - 11)
            out = out * base + c - 'A' + 10;
    }
    return out;
}


template <size_t N> using CodePointArray = std::array<CodePoint, N>;

// Because we don't have std::string_view until 2020.
// Used to match on keywords
struct StringMatcher {
    const char* start;
    template <size_t N> bool match(const char array[N]) const {
        for (size_t i { 0 }; i < N; ++i) {
            if (start[i] != array[i])
                return false;
        }
        return true;
    }
};

template <typename Action>
decltype(auto) lexer_binary_operator(CodePointStream& stream, Action& action, SourceCodeLocation token_start) {
    const auto end = stream.advance_while(is_binary_operator_character);
    const auto [str_b, sz] = stream.source_range({ token_start, end });
    assert(sz > 0);
    if (sz == 1) {
        const auto c = str_b[0];
        switch (c) {
        case '<':
            return action.template process<static_cast<TokenType>('<')>({ token_start, end });
        case '>':
            return action.template process<static_cast<TokenType>('>')>({ token_start, end });
        case '.':
            return action.template process<static_cast<TokenType>('.')>({ token_start, end });
        case '-':
            return action.template process<static_cast<TokenType>('-')>({ token_start, end });
        case '*':
            return action.template process<static_cast<TokenType>('*')>({ token_start, end });
        case '+':
            return action.template process<static_cast<TokenType>('+')>({ token_start, end });
        case '|':
            return action.template process<static_cast<TokenType>('|')>({ token_start, end });
        case '=':
            return action.template process<static_cast<TokenType>('=')>({ token_start, end });
        }
    } else if (sz == 2) {
        const auto c1 = str_b[0];
        const auto c2 = str_b[1];
        if (c1 == '<' && c2 == '-')
            return action.template process<TokenType::LeftArrow>({ token_start, end });
        if (c1 == '<' && c2 == '>')
            return action.template process<TokenType::ReadWriteVar>({ token_start, end });
    }
    return action.template process<TokenType::BinaryOperator>({ token_start, end });
}


template <typename Action>
decltype(auto) lexer_identifier_keybinop_curry_kw_etc(CodePointStream& stream, Action& action,
                                                      SourceCodeLocation token_start) {
    const auto end = stream.advance_while([](auto c) { return is_continuing_identifier(c); });

    // Note: this logic is a little odd, as it mean '_:' and '_asdf:' are keybinops, as is 'Foo:'.
    // This explains why you need a space between child and parent class in class definitions.
    // Also means keywords can be keybinops, 'var:', 'pi:'.
    // The only place these can be used is in Event: `( _: {|self, other| other } ) _: 1`. This makes no sense for
    // '_', but 'true:' and 'pi:' could be used somwhere.
    if (stream.peek_advance_if(':'))
        return action.template process<TokenType::KeywordBinaryOperator>({ token_start, stream.end_token() });

    const SourceCodeRange range { token_start, end };
    const auto [t_b, sz] = stream.source_range(range);

    if (t_b[0] == '_' && range.size() == 1)
        return action.template process<TokenType::CurryArg>(range);

    if (t_b[0] == '_')
        return action.template process<TokenType::PrimitiveName>(range);

    if (is_start_of_class(ascii_to_codepoint(*t_b)))
        return action.template process<TokenType::ClassName>(range);

    const auto txt = StringMatcher { t_b };
    if (sz == 2) {
        if (txt.match<2>("pi"))
            return action.template process<TokenType::Pi>(range);
    } else if (sz == 3) {
        if (txt.match<3>("var"))
            return action.template process<TokenType::Var>(range);
        else if (txt.match<3>("arg"))
            return action.template process<TokenType::Arg>(range);
        else if (txt.match<3>("nil"))
            return action.template process<TokenType::Nil>(range);
        else if (txt.match<3>("inf"))
            return action.template process<TokenType::Float>(range);
    } else if (sz == 4) {
        if (txt.match<4>("true"))
            return action.template process<TokenType::True>(range);
    } else if (sz == 5) {
        if (txt.match<5>("const"))
            return action.template process<TokenType::Const>(range);
        else if (txt.match<5>("while"))
            return action.template process<TokenType::While>(range);
        else if (txt.match<5>("false"))
            return action.template process<TokenType::False>(range);
    } else if (sz == 8) {
        if (txt.match<8>("classvar"))
            return action.template process<TokenType::ClassVar>(range);
    }

    return action.template process<TokenType::Name>(range);
}

template <typename Action>
decltype(auto) lexer_digits(CodePointStream& stream, Action& action, SourceCodeLocation token_start) {
    const auto end_of_pre = stream.advance_while([](auto c) { return is_numeric(c); });

    const auto peek = stream.peek_n<2>();

    switch (peek[0]) {
    case 'r': {
        const auto [radix_str_b, sz] = stream.source_range({ token_start, end_of_pre });
        stream.advance(); // drop 'r'
        const int radix = str_to_int_for_radix(radix_str_b, sz, 10);
        const auto offset10 = std::max<int>(0, std::min<int>(10, radix)) - 1;
        const auto offset36 = std::max<int>(0, std::min<int>(36, radix)) - 11;

        // TODO: I'd like to change this part of the language.
        // We are only accepting valid radixs, that means what happens after can become another token..
        //
        //
        //      |*| ---- integer '345'
        // 2r012345
        // |^^^|
        //   |------------- floatradix '2r012'
        //
        //      |**| ---- keybinop def:
        // 2rabcdef:
        // |^^^|
        //   |------------- floatradix '2rabc'
        stream.advance_while([=](auto c) {
            return ('0' <= c && c <= '0' + offset10) || ('a' <= c && c <= 'a' + offset36)
                || ('A' <= c && c <= 'A' + offset36);
        });
        if (stream.peek_advance_if('.')) {
            stream.advance_while(
                [=](auto c) { return ('0' <= c && c <= '0' + offset10) || ('A' <= c && c <= 'A' + offset36); });
            return action.template process<TokenType::FloatRadix>({ token_start, stream.end_token() });
        } else {
            return action.template process<TokenType::IntegerRadix>({ token_start, stream.end_token() });
        }
    }

    case 'e':
        [[fallthrough]];
    case 'E':
    exponent : { // This is the label for a goto, only used for floating point exponents.
        stream.advance(); // drop e
        const auto has_sign = stream.peek_advance_if('+', '-');
        const auto count = stream.advance_while_count([](auto c) { return is_numeric(c); });
        if (count == 0)
            return action.template process<TokenType::ErMissingExponent>({ token_start, stream.end_token() });

        return action.template process<TokenType::FloatExponent>({ token_start, stream.end_token() });
    }

    case '.': {
        if (!is_numeric(peek[1]))
            return action.template process<TokenType::Integer>({ token_start, stream.end_token() });
        stream.advance(); // drop '.'
        stream.advance_while(is_numeric);
        if (const auto e = stream.peek(); e == 'e' || e == 'E')
            goto exponent; // floating point exponent.
        return action.template process<TokenType::Float>({ token_start, stream.end_token() });
    }

    case 'b':
        [[fallthrough]];
    case 's': {
        const auto num_acc = stream.advance_while_count([acc = peek[0]](auto c) { return c == acc; });
        assert(num_acc > 0);
        if (num_acc == 1) {
            // Lets you specify a cent value after a *single* accidental.
            // 123s40
            const auto num_cent_chars = stream.advance_while_count(is_numeric);
            if (num_cent_chars == 0)
                return action.template process<TokenType::AccidentalSteps>({ token_start, stream.end_token() });
            else
                return action.template process<TokenType::AccidentalCents>({ token_start, stream.end_token() });
        }
        return action.template process<TokenType::AccidentalSteps>({ token_start, stream.end_token() });
    }

    case 'x': {
        stream.advance();
        // Ignores the preceding digits and the 'x'.
        // BUG: this means 89702347890234589xAA == 0xAA. Probably not intended.
        const auto end = stream.advance_while(
            [](auto c) { return is_numeric(c) || ('a' <= c && c <= 'f') || ('A' <= c && c <= 'F'); });
        return action.template process<TokenType::Hexidecimal>({ token_start, end });
    }

    default:
        return action.template process<TokenType::Integer>({ token_start, stream.end_token() });
    }
}

} // details

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Main lexer function
// Takes in state and mutates it.
// Looks to the action to deduce its return type.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

template <typename Action> auto lexer(CodePointStream& stream, Action& action) noexcept -> typename Action::Output {
    using namespace details;
    // Must return the Interpret token the first time we are called if in interpret mode.
    if (stream.should_leave_cmd_initial()) {
        if (auto r = action.template process<TokenType::Interpret>({ stream.end_token(), stream.end_token() }))
            return *r;
    }


// A simple loop, I've used a label rather than a continue so it is clear exactly where we are continuing to.
discard_token : {
    const auto [token_start, c] = stream.start_token();

    // Will repeatedly return this as this doesn't mutate state.
    if (c == 0) {
        const auto r = action.template process<TokenType::EndOfFile>({ token_start, stream.end_token() });
        return *r;
    }

    if (is_newline(c)) {
        const auto end = stream.advance_while([](auto c) { return is_newline(c); });
        if (auto r = action.template process<TokenType::NewLine>({ token_start, end }))
            return *r;
        else
            goto discard_token;
    }


    if (c == '\t') {
        const auto end = stream.advance_while([](auto c) { return c == '\t'; });
        if (auto r = action.template process<TokenType::Tab>({ token_start, stream.end_token() }))
            return *r;
        else
            goto discard_token;
    }

    if (is_space(c)) {
        const auto end = stream.advance_while(is_space);
        if (auto r = action.template process<TokenType::Space>({ token_start, stream.end_token() }))
            return *r;
        else
            goto discard_token;
    }

#define literal_case(c)                                                                                                \
    case (c):                                                                                                          \
        if (auto r = action.template process<static_cast<TokenType>((c))>({ token_start, stream.end_token() }))        \
            return *r;                                                                                                 \
        else                                                                                                           \
            goto discard_token

    // Simple character literals
    switch (c) {
        literal_case('^');
        literal_case('~');
        literal_case(';');
        literal_case(':');
        literal_case('`');
        literal_case(',');
        literal_case('(');
        literal_case('[');
        literal_case('{');
        literal_case(')');
        literal_case(']');
        literal_case('}');
    }
#undef literal_case

    if (c == '#') {
        if (stream.peek_advance_if('{')) {
            if (auto r = action.template process<TokenType::BeginClosedFunction>({ token_start, stream.end_token() }))
                return *r;
            else
                goto discard_token;
        } else if (auto r = action.template process<static_cast<TokenType>('#')>({ token_start, stream.end_token() })) {
            return *r;
        } else
            goto discard_token;
    }

    // Comments or binary op
    if (c == '/') {
        const auto p = stream.peek_n<2>();
        // '///' documentation comments
        if (p == CodePointArray<2> { '/', '/' }) {
            const auto end = stream.advance_while([](auto c) { return !is_newline(c); });
            if (auto r = action.template process<TokenType::DocumentationComment>({ token_start, stream.end_token() }))
                return *r;
            else
                goto discard_token;
        }

        // '/*'
        else if (p == CodePointArray<1> { '*' }) {
            stream.advance();
            CodePoint it_1 = 0, it = 0;
            size_t level = 1;
            const auto end = stream.advance_while([&](auto c) {
                it_1 = it;
                it = c;
                if (it_1 == '/' && it == '*') {
                    level += 1;
                    it = 0; // consume
                    return true;
                }
                if (it_1 == '*' && it == '/') {
                    level -= 1;
                    it = 0; // consume
                    return level != 0;
                }
                return true;
            });
            const auto delimit = stream.advance();
            if (delimit == 0) {
                if (auto r = action.template process<TokenType::ErMultilineCommentUnclosed>(
                        { token_start, stream.end_token() }))
                    return *r;
                else
                    goto discard_token;
            }

            if (auto r = action.template process<TokenType::MultiLineComment>({ token_start, stream.end_token() }))
                return *r;
            else
                goto discard_token;
        }

        // '//'
        else if (p == CodePointArray<1> { '/' }) {
            const auto end = stream.advance_while([](auto c) { return !is_newline(c); });
            if (auto r = action.template process<TokenType::Comment>({ token_start, stream.end_token() }))
                return *r;
            else
                goto discard_token;
        }

        if (auto r = lexer_binary_operator(stream, action, token_start))
            return *r;
        else
            goto discard_token;
    }

    if (c == '.') {
        const auto p = stream.peek_n<2>();
        if (p == CodePointArray<2> { '.', '.' }) {
            if (auto r = action.template process<TokenType::Ellipsis>({ token_start, stream.advance_by_peek(p) }))
                return *r;
            else
                goto discard_token;
        } else if (p == CodePointArray<1> { '.' }) {
            stream.advance(); // drop '.;
            if (auto r = action.template process<TokenType::DotDot>({ token_start, stream.end_token() }))
                return *r;
            else
                goto discard_token;
        }
        if (auto r = action.template process<static_cast<TokenType>('.')>({ token_start, stream.end_token() }))
            return *r;
        else
            goto discard_token;
    }

    if (c == '$') {
        if (stream.peek() == '\\') {
            stream.advance(); // consume '\'
            stream.advance(); // consume whatever happens after it.
            if (auto r = action.template process<TokenType::Ascii>({ token_start, stream.end_token() }))
                return *r;
            else
                goto discard_token;
        }

        stream.advance(); // consume whatever character comes after the '$'

        if (auto r = action.template process<TokenType::Ascii>({ token_start, stream.end_token() }))
            return *r;
        else
            goto discard_token;
    }

    if (is_starting_identifier(c) || c == '_') {
        if (auto r = lexer_identifier_keybinop_curry_kw_etc(stream, action, token_start))
            return *r;
        else
            goto discard_token;
    }

    if (is_binary_operator_character(c)) {
        if (auto r = lexer_binary_operator(stream, action, token_start))
            return *r;
        else
            goto discard_token;
    }

    if (is_numeric(c)) {
        if (auto r = lexer_digits(stream, action, token_start))
            return *r;
        else
            goto discard_token;
    }

    // Strings
    if (c == '"') {
        const auto end = stream.advance_while([escaped = false](auto c) mutable {
            if (c == '\\' && !escaped) {
                escaped = true;
                return true;
            }
            if (c == '"' && !escaped)
                return false;
            escaped = false;
            return true;
        });

        if (stream.advance() != '"') {
            if (auto r = action.template process<TokenType::ErStringUnclosed>({ token_start, end }))
                return *r;
            else
                goto discard_token;
        }

        if (auto r = action.template process<TokenType::StringLine>({ token_start, stream.end_token() }))
            return *r;
        else
            goto discard_token;
    }

    // Symbol that begin with a '\'. Note: first character is used to alter what is acceptable in the following.
    if (c == '\\') {
        const auto first = stream.peek();
        const SourceCodeLocation end = [&]() {
            if (is_lower(first) || is_upper(first) || first == '_')
                // Symbol began with alpha or underscore, therefore, can contain alpha, underscore, and numbers
                return stream.advance_while(
                    [](auto c) { return is_lower(c) || is_upper(c) || is_numeric(c) || c == '_'; });
            if (is_numeric(first))
                // Symbol began with a digit, therefore can only contain digits... why??
                return stream.advance_while(is_numeric);

            // This is weird, if it isn't either of the above, emit an empty symbol.
            return stream.end_token();
        }();
        if (auto r = action.template process<TokenType::SymbolSlash>({ token_start, end }))
            return *r;
        else
            goto discard_token;
    }

    // Symbol quotes 'asdf'
    if (c == '\'') {
        const auto content_end = stream.advance_while([escape = false](auto c) mutable {
            if (is_newline(c) && !escape) // you can escape the new line characters
                return false;
            if (c == '\'' && !escape)
                return false;
            if (c == '\\' && !escape) {
                escape = true;
                return true;
            }
            escape = false;
            return true;
        });
        const auto next = stream.advance();
        if (next != '\'') {
            if (auto r = action.template process<TokenType::ErSymbolQuoteUnclosed>({ token_start, content_end }))
                return *r;
            else
                goto discard_token;
        }

        if (auto r = action.template process<TokenType::SymbolQuote>({ token_start, stream.end_token() }))
            return *r;
        else
            goto discard_token;
    }

    // These are the control codes, throw them away!
    // We might consider generating an error here, as this really ought not to occur.
    if (is_control_code(c)) {
        stream.advance_while(is_control_code);
        goto discard_token;
    }

    if (c == invalid_utf8_flag) {
        if (auto r = action.template process<TokenType::ErInvalidUTF8>({ token_start, stream.end_token() }))
            return *r;
        else
            goto discard_token;
    }


    if (auto r = action.template process<TokenType::ErUnexpected>({ token_start, stream.end_token() }))
        return *r;
    else
        goto discard_token;
}
}


////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// impls
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

template <size_t N> inline constexpr CodePoint CodePointStream::Peek<N>::operator[](size_t n) const noexcept {
    assert(n < N);
    return characters[n];
}

template <size_t N>
template <size_t M>
inline constexpr bool CodePointStream::Peek<N>::operator!=(std::array<CodePoint, M> c) const noexcept {
    return !(*this == c);
}

template <size_t N>
template <size_t M>
inline constexpr bool CodePointStream::Peek<N>::operator==(std::array<CodePoint, M> c) const noexcept {
    static_assert(M <= N);
    for (size_t i { 0 }; i < M; ++i)
        if (this->operator[](i) != c[i])
            return false;
    return true;
};

inline CodePointStream::CodePointStream(bool in_class_library, const char* source, size_t source_length,
                                        FileCodeLocation source_start_in_file):
    source_start_in_file(source_start_in_file),
    next({}),
    source(source),
    source_length(source_length),
    mode(in_class_library ? Mode::ClassLibrary : Mode::CMDInitial) {}

template <size_t N>
template <size_t M>
inline constexpr CodePointStream::Peek<M> CodePointStream::Peek<N>::shrink_to() const noexcept {
    static_assert(M <= N);
    std::array<CodePoint, M> out;
    for (size_t i { 0 }; i < M; ++i)
        out[i] = this->operator[](i);
    return { out };
};

template <size_t N> [[nodiscard]] CodePointStream::Peek<N> CodePointStream::peek_n() const {
    static_assert(N > 0);
    std::array<CodePoint, N> out;
    const auto sz = source_length;
    size_t byte_offset = next.absolute;
    const auto get_next = [&]() {
        const auto [c, codepoint_size] = read(byte_offset);
        byte_offset += codepoint_size;
        return c;
    };
    for (size_t char_count { 0 }; char_count < N; ++char_count)
        out[char_count] = (byte_offset >= sz) ? 0 : get_next();
    return { out };
};

template <size_t N> inline SourceCodeLocation CodePointStream::advance_by_peek(CodePointStream::Peek<N> peek) {
    for (size_t i { 0 }; i < N; ++i)
        increment_source_code_point(next, peek[i]);
    return end_token();
}

template <typename... C> inline bool CodePointStream::peek_advance_if(C... cs) {
    if (const auto p = peek(); ((p == cs) || ...)) {
        advance();
        return true;
    }
    return false;
};

template <typename Predicate> inline size_t CodePointStream::advance_while_count(Predicate&& predicate) {
    auto discard_null_then_predicate = [&](auto c) { return (c == 0) ? false : predicate(c); };
    size_t i { 0 };
    for (auto c = peek(); discard_null_then_predicate(c); c = advance_and_peek(), ++i) {}
    return i;
};

template <typename Predicate> inline SourceCodeLocation CodePointStream::advance_while(Predicate&& predicate) {
    advance_while_count(std::forward<Predicate>(predicate));
    return end_token();
}

[[nodiscard]] inline FileCodeRange CodePointStream::source_to_file(const SourceCodeRange& source) const {
    return FileCodeRange { source_to_file(source.begin), source_to_file(source.end) };
}

[[nodiscard]] inline FileCodeLocation CodePointStream::source_to_file(const SourceCodeLocation& source) const {
    const auto file_begin_abs = source.absolute - source_start_in_file.absolute;
    const auto file_begin_line_number = [&]() -> int {
        for (int l { 0 }; l < abs_new_line_locations.size(); ++l) {
            if (abs_new_line_locations[l] <= file_begin_abs) {
                return l;
            }
        }
        return static_cast<int>(abs_new_line_locations.size()) - 1;
    }();
    const auto abs_start_of_line = abs_new_line_locations[file_begin_line_number];
    const auto offset_in_line = file_begin_abs - abs_start_of_line;
    return { file_begin_abs, abs_start_of_line, offset_in_line };
}

[[nodiscard]] inline std::tuple<SourceCodeLocation, CodePoint> CodePointStream::start_token() {
    const auto loc = next;
    return { loc, advance() };
}

inline SourceCodeLocation CodePointStream::line_start(SourceCodeLocation p) const {
    // Should have seen this before, so should be here.
    assert(p.lineNumber < abs_new_line_locations.size());
    return { abs_new_line_locations[p.lineNumber], p.lineNumber, 0 };
}

[[nodiscard]] inline SourceCodeLocation CodePointStream::end_token() const { return next; }

[[nodiscard]] inline bool CodePointStream::should_leave_cmd_initial() noexcept {
    if (mode == Mode::CMDInitial) {
        mode = Mode::CMDContinue;
        return true;
    }
    return false;
}

inline CodePoint CodePointStream::advance() {
    if (next.absolute >= source_length)
        return 0;

    const auto [c, sz] = read(next.absolute);
    increment_source_code_point(next, c, sz);
    return c;
}


[[nodiscard]] inline std::tuple<CodePoint, std::uint8_t> CodePointStream::read(size_t pos) const noexcept {
    const auto [cp, sz] = char_sequence_to_codepoint(source, pos, source_length);
    if (details::is_newline(cp)) {
        const auto line_start = pos + sz;
        if (abs_new_line_locations.empty() || abs_new_line_locations.back() < line_start) {
            abs_new_line_locations.push_back(line_start);
        }
    }
    return { cp, sz };
}

[[nodiscard]] inline CodePoint CodePointStream::advance_and_peek() {
    advance();
    return peek();
}


[[nodiscard]] inline std::uint8_t character_size(CodePoint c) {
    if (c < (1ULL << 8ULL))
        return 1;
    else if (c < (1ULL << 16ULL))
        return 2;
    else if (c < (1ULL << 24ULL))
        return 3;
    else
        return 4;
}

inline void CodePointStream::increment_source_code_point(SourceCodeLocation& p, CodePoint c, std::uint8_t sz) const {
    p.absolute += sz;
    if (details::is_newline(c)) {
        p.lineNumber += 1;
        p.column = 0;
    } else {
        p.column += 1;
    }
}

inline void CodePointStream::increment_source_code_point(SourceCodeLocation& p, CodePoint c) const {
    return increment_source_code_point(p, c, character_size(c));
}


[[nodiscard]] inline std::tuple<const char*, std::size_t>
CodePointStream::source_range(const SourceCodeRange& range) const {
    assert(range.begin.absolute <= range.end.absolute);
    return { source + range.begin.absolute, range.end.absolute - range.begin.absolute };
}

inline void CodePointStream::reset() {
    next = SourceCodeLocation {};
    mode = (mode == Mode::ClassLibrary) ? Mode::ClassLibrary : Mode::CMDInitial;
}
[[nodiscard]] inline CodePoint CodePointStream::peek() const { return peek_n<1>()[0]; }


[[nodiscard]] inline CodePointStream::LineIter
CodePointStream::line_iter(const SourceCodeRange& range, std::size_t lines_before, std::size_t lines_after) const {
    const auto start_line = range.begin.lineNumber >= lines_before ? range.begin.lineNumber - lines_before : 0;
    const auto end_line = range.end.lineNumber + lines_after;

    auto pos = range.end.absolute;
    // ensures we have seen the entirety of the lines we are about to return.
    // This means we can always access the next line.
    while (pos < source_length && abs_new_line_locations.size() < (end_line + 2)) {
        const auto [cp, sz] = read(pos);
        pos += sz;
    }
    return { *this, start_line, std::min(end_line, abs_new_line_locations.size() - 1) };
}

[[nodiscard]] inline std::optional<std::tuple<const char*, std::size_t, std::size_t>>
CodePointStream::LineIter::operator()() {
    if (current_line > end_line)
        return std::nullopt;

    if (current_line >= cps.abs_new_line_locations.size())
        return std::nullopt;

    const auto line_start = cps.abs_new_line_locations[current_line];
    current_line += 1;
    if (current_line < cps.abs_new_line_locations.size()) {
        const auto line_end = cps.abs_new_line_locations[current_line];
        return { { cps.source + line_start, line_end - line_start, current_line - 1 } };
    } else {
        return { { cps.source + line_start, cps.source_length - line_start, current_line - 1 } };
    }
}

template <typename T> T& operator<<(T& stream, const TokenType& t) {
    const auto i = static_cast<int>(t);
    if (i < 128) {
        return stream << static_cast<char>(t);
    }
    switch (t) {
    case TokenType::Name:
        return stream << "Name";
    case TokenType::Integer:
        return stream << "Integer";
    case TokenType::Float:
        return stream << "Float";
    case TokenType::AccidentalSteps:
        return stream << "AccidentalSteps";
    case TokenType::AccidentalCents:
        return stream << "AccidentalCents";
    case TokenType::Hexidecimal:
        return stream << "Hexidecimal";
    case TokenType::SymbolSlash:
        return stream << "SymbolSlash";
    case TokenType::SymbolQuote:
        return stream << "SymbolQuote";
    case TokenType::Ascii:
        return stream << "Ascii";
    case TokenType::ClassName:
        return stream << "ClassName";
    case TokenType::Pi:
        return stream << "Pi";
    case TokenType::Inf:
        return stream << "Inf";
    case TokenType::While:
        return stream << "While";
    case TokenType::PrimitiveName:
        return stream << "PrimitiveName";
    case TokenType::LeftArrow:
        return stream << "LeftArrow";
    case TokenType::True:
        return stream << "True";
    case TokenType::False:
        return stream << "False";
    case TokenType::Nil:
        return stream << "Nil";
    case TokenType::Var:
        return stream << "Var";
    case TokenType::Arg:
        return stream << "Arg";
    case TokenType::ClassVar:
        return stream << "ClassVar";
    case TokenType::Const:
        return stream << "Const";
    case TokenType::Ellipsis:
        return stream << "Ellipsis";
    case TokenType::DotDot:
        return stream << "DotDot";
    case TokenType::BeginClosedFunction:
        return stream << "BeginClosedFunction";
    case TokenType::ErUnexpected:
        return stream << "BadToken";
    case TokenType::Interpret:
        return stream << "Interpret";
    case TokenType::CurryArg:
        return stream << "CurryArg";
    case TokenType::BinaryOperator:
        return stream << "BinaryOperator";
    case TokenType::KeywordBinaryOperator:
        return stream << "KeywordBinaryOperator";
    case TokenType::ReadWriteVar:
        return stream << "ReadWriteVar";
    case TokenType::StringLine:
        return stream << "StringLine";
    case TokenType::Space:
        return stream << "Space";
    case TokenType::NewLine:
        return stream << "NewLine";
    case TokenType::Tab:
        return stream << "Tab";
    case TokenType::Comment:
        return stream << "Comment";
    case TokenType::DocumentationComment:
        return stream << "DocumentationComment";
    case TokenType::MultiLineComment:
        return stream << "MultiLineComment";
    default:
        return stream << "unknown[" << static_cast<int>(t) << "]";
    }
}

} // sc::lex
