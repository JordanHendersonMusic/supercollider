// A Bison parser, made by GNU Bison 3.8.2.

// Skeleton interface for Bison LALR(1) parsers in C++

// Copyright (C) 2002-2015, 2018-2021 Free Software Foundation, Inc.

// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.

// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <https://www.gnu.org/licenses/>.

// As a special exception, you may create a larger work that contains
// part or all of the Bison parser skeleton and distribute that work
// under terms of your choice, so long as that work isn't itself a
// parser generator using the skeleton or a modified version thereof
// as a parser skeleton.  Alternatively, if you modify or redistribute
// the parser skeleton itself, you may (at your option) remove this
// special exception, which will cause the skeleton and the resulting
// Bison output files to be licensed under the GNU General Public
// License without this special exception.

// This special exception was added by the Free Software Foundation in
// version 2.2 of Bison.


/**
 ** \file langutils/sc_parser/src/sc_grammar_parser.hpp
 ** Define the sc::parser::parser class.
 */

// C++ LALR(1) parser skeleton written by Akim Demaille.

// DO NOT RELY ON FEATURES THAT ARE NOT DOCUMENTED in the manual,
// especially those whose name start with YY_ or yy_.  They are
// private implementation details that can be changed or removed.

#ifndef YY_YY_LANGUTILS_SC_PARSER_SRC_SC_GRAMMAR_PARSER_HPP_INCLUDED
#define YY_YY_LANGUTILS_SC_PARSER_SRC_SC_GRAMMAR_PARSER_HPP_INCLUDED
// "%code requires" blocks.
#line 15 "langutils/sc_parser/src/sc_grammar.y"


#include "parser_context.hpp"

namespace sc::parser {

struct NoValue {}; // This is used to indicate that token rules don't create anything in the resulting graph.

enum struct ReadWriteAccessor : std::uint8_t { Private, PublicRead, PublicWrite, PublicReadAndWrite };

}


#line 63 "langutils/sc_parser/src/sc_grammar_parser.hpp"


#include <cstdlib> // std::abort
#include <iostream>
#include <stdexcept>
#include <string>
#include <vector>

#if defined __cplusplus
#    define YY_CPLUSPLUS __cplusplus
#else
#    define YY_CPLUSPLUS 199711L
#endif

// Support move semantics when possible.
#if 201103L <= YY_CPLUSPLUS
#    define YY_MOVE std::move
#    define YY_MOVE_OR_COPY move
#    define YY_MOVE_REF(Type) Type&&
#    define YY_RVREF(Type) Type&&
#    define YY_COPY(Type) Type
#else
#    define YY_MOVE
#    define YY_MOVE_OR_COPY copy
#    define YY_MOVE_REF(Type) Type&
#    define YY_RVREF(Type) const Type&
#    define YY_COPY(Type) const Type&
#endif

// Support noexcept when possible.
#if 201103L <= YY_CPLUSPLUS
#    define YY_NOEXCEPT noexcept
#    define YY_NOTHROW
#else
#    define YY_NOEXCEPT
#    define YY_NOTHROW throw()
#endif

// Support constexpr when possible.
#if 201703 <= YY_CPLUSPLUS
#    define YY_CONSTEXPR constexpr
#else
#    define YY_CONSTEXPR
#endif


#ifndef YY_ATTRIBUTE_PURE
#    if defined __GNUC__ && 2 < __GNUC__ + (96 <= __GNUC_MINOR__)
#        define YY_ATTRIBUTE_PURE __attribute__((__pure__))
#    else
#        define YY_ATTRIBUTE_PURE
#    endif
#endif

#ifndef YY_ATTRIBUTE_UNUSED
#    if defined __GNUC__ && 2 < __GNUC__ + (7 <= __GNUC_MINOR__)
#        define YY_ATTRIBUTE_UNUSED __attribute__((__unused__))
#    else
#        define YY_ATTRIBUTE_UNUSED
#    endif
#endif

/* Suppress unused-variable warnings by "using" E.  */
#if !defined lint || defined __GNUC__
#    define YY_USE(E) ((void)(E))
#else
#    define YY_USE(E) /* empty */
#endif

/* Suppress an incorrect diagnostic about yylval being uninitialized.  */
#if defined __GNUC__ && !defined __ICC && 406 <= __GNUC__ * 100 + __GNUC_MINOR__
#    if __GNUC__ * 100 + __GNUC_MINOR__ < 407
#        define YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN                                                                    \
            _Pragma("GCC diagnostic push") _Pragma("GCC diagnostic ignored \"-Wuninitialized\"")
#    else
#        define YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN                                                                    \
            _Pragma("GCC diagnostic push") _Pragma("GCC diagnostic ignored \"-Wuninitialized\"")                       \
                _Pragma("GCC diagnostic ignored \"-Wmaybe-uninitialized\"")
#    endif
#    define YY_IGNORE_MAYBE_UNINITIALIZED_END _Pragma("GCC diagnostic pop")
#else
#    define YY_INITIAL_VALUE(Value) Value
#endif
#ifndef YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
#    define YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
#    define YY_IGNORE_MAYBE_UNINITIALIZED_END
#endif
#ifndef YY_INITIAL_VALUE
#    define YY_INITIAL_VALUE(Value) /* Nothing. */
#endif

#if defined __cplusplus && defined __GNUC__ && !defined __ICC && 6 <= __GNUC__
#    define YY_IGNORE_USELESS_CAST_BEGIN                                                                               \
        _Pragma("GCC diagnostic push") _Pragma("GCC diagnostic ignored \"-Wuseless-cast\"")
#    define YY_IGNORE_USELESS_CAST_END _Pragma("GCC diagnostic pop")
#endif
#ifndef YY_IGNORE_USELESS_CAST_BEGIN
#    define YY_IGNORE_USELESS_CAST_BEGIN
#    define YY_IGNORE_USELESS_CAST_END
#endif

#ifndef YY_CAST
#    ifdef __cplusplus
#        define YY_CAST(Type, Val) static_cast<Type>(Val)
#        define YY_REINTERPRET_CAST(Type, Val) reinterpret_cast<Type>(Val)
#    else
#        define YY_CAST(Type, Val) ((Type)(Val))
#        define YY_REINTERPRET_CAST(Type, Val) ((Type)(Val))
#    endif
#endif
#ifndef YY_NULLPTR
#    if defined __cplusplus
#        if 201103L <= __cplusplus
#            define YY_NULLPTR nullptr
#        else
#            define YY_NULLPTR 0
#        endif
#    else
#        define YY_NULLPTR ((void*)0)
#    endif
#endif

/* Debug traces.  */
#ifndef YYDEBUG
#    define YYDEBUG 0
#endif

#line 6 "langutils/sc_parser/src/sc_grammar.y"
namespace sc { namespace parser {
#line 199 "langutils/sc_parser/src/sc_grammar_parser.hpp"


/// A Bison parser.
class parser {
public:
#ifdef YYSTYPE
#    ifdef __GNUC__
#        pragma GCC message "bison: do not #define YYSTYPE in C++, use %define api.value.type"
#    endif
    typedef YYSTYPE value_type;
#else
    /// A buffer to store and retrieve objects.
    ///
    /// Sort of a variant, but does not keep track of the nature
    /// of the stored data, since that knowledge is available
    /// via the current parser state.
    class value_type {
    public:
        /// Type of *this.
        typedef value_type self_type;

        /// Empty construction.
        value_type() YY_NOEXCEPT : yyraw_() {}

        /// Construct and fill.
        template <typename T> value_type(YY_RVREF(T) t) { new (yyas_<T>()) T(YY_MOVE(t)); }

#    if 201103L <= YY_CPLUSPLUS
        /// Non copyable.
        value_type(const self_type&) = delete;
        /// Non copyable.
        self_type& operator=(const self_type&) = delete;
#    endif

        /// Destruction, allowed only if empty.
        ~value_type() YY_NOEXCEPT {}

#    if 201103L <= YY_CPLUSPLUS
        /// Instantiate a \a T in here from \a t.
        template <typename T, typename... U> T& emplace(U&&... u) { return *new (yyas_<T>()) T(std::forward<U>(u)...); }
#    else
        /// Instantiate an empty \a T in here.
        template <typename T> T& emplace() { return *new (yyas_<T>()) T(); }

        /// Instantiate a \a T in here from \a t.
        template <typename T> T& emplace(const T& t) { return *new (yyas_<T>()) T(t); }
#    endif

        /// Instantiate an empty \a T in here.
        /// Obsolete, use emplace.
        template <typename T> T& build() { return emplace<T>(); }

        /// Instantiate a \a T in here from \a t.
        /// Obsolete, use emplace.
        template <typename T> T& build(const T& t) { return emplace<T>(t); }

        /// Accessor to a built \a T.
        template <typename T> T& as() YY_NOEXCEPT { return *yyas_<T>(); }

        /// Const accessor to a built \a T (for %printer).
        template <typename T> const T& as() const YY_NOEXCEPT { return *yyas_<T>(); }

        /// Swap the content with \a that, of same type.
        ///
        /// Both variants must be built beforehand, because swapping the actual
        /// data requires reading it (with as()), and this is not possible on
        /// unconstructed variants: it would require some dynamic testing, which
        /// should not be the variant's responsibility.
        /// Swapping between built and (possibly) non-built is done with
        /// self_type::move ().
        template <typename T> void swap(self_type& that) YY_NOEXCEPT { std::swap(as<T>(), that.as<T>()); }

        /// Move the content of \a that to this.
        ///
        /// Destroys \a that.
        template <typename T> void move(self_type& that) {
#    if 201103L <= YY_CPLUSPLUS
            emplace<T>(std::move(that.as<T>()));
#    else
            emplace<T>();
            swap<T>(that);
#    endif
            that.destroy<T>();
        }

#    if 201103L <= YY_CPLUSPLUS
        /// Move the content of \a that to this.
        template <typename T> void move(self_type&& that) {
            emplace<T>(std::move(that.as<T>()));
            that.destroy<T>();
        }
#    endif

        /// Copy the content of \a that to this.
        template <typename T> void copy(const self_type& that) { emplace<T>(that.as<T>()); }

        /// Destroy the stored \a T.
        template <typename T> void destroy() { as<T>().~T(); }

    private:
#    if YY_CPLUSPLUS < 201103L
        /// Non copyable.
        value_type(const self_type&);
        /// Non copyable.
        self_type& operator=(const self_type&);
#    endif

        /// Accessor to raw memory as \a T.
        template <typename T> T* yyas_() YY_NOEXCEPT {
            void* yyp = yyraw_;
            return static_cast<T*>(yyp);
        }

        /// Const accessor to raw memory as \a T.
        template <typename T> const T* yyas_() const YY_NOEXCEPT {
            const void* yyp = yyraw_;
            return static_cast<const T*>(yyp);
        }

        /// An auxiliary type to compute the largest semantic type.
        union union_type {
            // accidental.unsigned
            // accidental
            char dummy1[sizeof(AccidentalLitIndex)];

            // literal
            char dummy2[sizeof(AnyLiteralIndex)];

            // boolean
            char dummy3[sizeof(BooleanLitIndex)];

            // float.raw_unsigned
            // float.raw
            char dummy4[sizeof(FloatLitIndex)];

            // float
            char dummy5[sizeof(FloatProducingIndex)];

            // integer
            char dummy6[sizeof(IntLitIndex)];

            // commandLine
            // basicBinOp
            char dummy7[sizeof(MessageIndex)];

            // nil
            char dummy8[sizeof(NilLitIndex)];

            // OPENCURLY
            // CLOSECURLY
            // OPENSQUARE
            // CLOSESQUARE
            // OPENPAREN
            // CLOSEPAREN
            // SEMICOLON
            // NONLOCALRETURN
            // COMMA
            // HASH
            // TILDE
            // NAME
            // INTEGER
            // INTEGER_RADIX
            // HEXADECIMAL
            // FLOAT
            // FLOAT_RADIX
            // FLOAT_EXPONENT
            // FLOAT_INF
            // ACCIDENTAL_STEPS
            // ACCIDENTAL_CENTS
            // SYMBOL_QUOTE
            // SYMBOL_SLASH
            // STRINGLINE
            // ASCII
            // PRIMITIVENAME
            // CLASSNAME
            // CURRYARG
            // VAR
            // ARG
            // CLASSVAR
            // SC_CONST
            // NILOBJ
            // TRUEOBJ
            // FALSEOBJ
            // PI
            // ELLIPSIS
            // DOTDOT
            // BEGINCLOSEDFUNC
            // BADTOKEN
            // INTERPRET
            // LEFTARROW
            // WHILE
            // COLON
            // EQUALSSIGN
            // BINOP
            // KEYBINOP
            // MINUS
            // LESSTHAN
            // GREATERTHAN
            // MULTIPLY
            // ADD
            // PIPE
            // READWRITEVAR
            // DOT
            // BACKTICK
            // UMINUS
            char dummy9[sizeof(NoValue)];

            // binary_op.raw
            // binary_op
            char dummy10[sizeof(SelectorIndex)];

            // string
            char dummy11[sizeof(StringLitIndex)];

            // symbol
            char dummy12[sizeof(SymbolLitIndex)];
        };

        /// The size of the largest semantic type.
        enum { size = sizeof(union_type) };

        /// A buffer to store semantic values.
        union {
            /// Strongest alignment constraints.
            long double yyalign_me_;
            /// A buffer large enough to store any of the semantic values.
            char yyraw_[size];
        };
    };

#endif
    /// Backward compatibility (Bison 3.8).
    typedef value_type semantic_type;

    /// Symbol locations.
    typedef sc::lex::SourceCodeRange location_type;

    /// Syntax errors thrown from user actions.
    struct syntax_error : std::runtime_error {
        syntax_error(const location_type& l, const std::string& m): std::runtime_error(m), location(l) {}

        syntax_error(const syntax_error& s): std::runtime_error(s.what()), location(s.location) {}

        ~syntax_error() YY_NOEXCEPT YY_NOTHROW;

        location_type location;
    };

    /// Token kinds.
    struct token {
        enum token_kind_type {
            TOKEN_YYEMPTY = -2,
            TOKEN_YYEOF = 0, // "end of file"
            TOKEN_YYerror = 256, // error
            TOKEN_YYUNDEF = 257, // "invalid token"
            TOKEN_OPENCURLY = 258, // OPENCURLY
            TOKEN_CLOSECURLY = 259, // CLOSECURLY
            TOKEN_OPENSQUARE = 260, // OPENSQUARE
            TOKEN_CLOSESQUARE = 261, // CLOSESQUARE
            TOKEN_OPENPAREN = 262, // OPENPAREN
            TOKEN_CLOSEPAREN = 263, // CLOSEPAREN
            TOKEN_SEMICOLON = 264, // SEMICOLON
            TOKEN_NONLOCALRETURN = 265, // NONLOCALRETURN
            TOKEN_COMMA = 266, // COMMA
            TOKEN_HASH = 267, // HASH
            TOKEN_TILDE = 268, // TILDE
            TOKEN_NAME = 269, // NAME
            TOKEN_INTEGER = 270, // INTEGER
            TOKEN_INTEGER_RADIX = 271, // INTEGER_RADIX
            TOKEN_HEXADECIMAL = 272, // HEXADECIMAL
            TOKEN_FLOAT = 273, // FLOAT
            TOKEN_FLOAT_RADIX = 274, // FLOAT_RADIX
            TOKEN_FLOAT_EXPONENT = 275, // FLOAT_EXPONENT
            TOKEN_FLOAT_INF = 276, // FLOAT_INF
            TOKEN_ACCIDENTAL_STEPS = 277, // ACCIDENTAL_STEPS
            TOKEN_ACCIDENTAL_CENTS = 278, // ACCIDENTAL_CENTS
            TOKEN_SYMBOL_QUOTE = 279, // SYMBOL_QUOTE
            TOKEN_SYMBOL_SLASH = 280, // SYMBOL_SLASH
            TOKEN_STRINGLINE = 281, // STRINGLINE
            TOKEN_ASCII = 282, // ASCII
            TOKEN_PRIMITIVENAME = 283, // PRIMITIVENAME
            TOKEN_CLASSNAME = 284, // CLASSNAME
            TOKEN_CURRYARG = 285, // CURRYARG
            TOKEN_VAR = 286, // VAR
            TOKEN_ARG = 287, // ARG
            TOKEN_CLASSVAR = 288, // CLASSVAR
            TOKEN_SC_CONST = 289, // SC_CONST
            TOKEN_NILOBJ = 290, // NILOBJ
            TOKEN_TRUEOBJ = 291, // TRUEOBJ
            TOKEN_FALSEOBJ = 292, // FALSEOBJ
            TOKEN_PI = 293, // PI
            TOKEN_ELLIPSIS = 294, // ELLIPSIS
            TOKEN_DOTDOT = 295, // DOTDOT
            TOKEN_BEGINCLOSEDFUNC = 296, // BEGINCLOSEDFUNC
            TOKEN_BADTOKEN = 297, // BADTOKEN
            TOKEN_INTERPRET = 298, // INTERPRET
            TOKEN_LEFTARROW = 299, // LEFTARROW
            TOKEN_WHILE = 300, // WHILE
            TOKEN_COLON = 301, // COLON
            TOKEN_EQUALSSIGN = 302, // EQUALSSIGN
            TOKEN_BINOP = 303, // BINOP
            TOKEN_KEYBINOP = 304, // KEYBINOP
            TOKEN_MINUS = 305, // MINUS
            TOKEN_LESSTHAN = 306, // LESSTHAN
            TOKEN_GREATERTHAN = 307, // GREATERTHAN
            TOKEN_MULTIPLY = 308, // MULTIPLY
            TOKEN_ADD = 309, // ADD
            TOKEN_PIPE = 310, // PIPE
            TOKEN_READWRITEVAR = 311, // READWRITEVAR
            TOKEN_DOT = 312, // DOT
            TOKEN_BACKTICK = 313, // BACKTICK
            TOKEN_UMINUS = 314 // UMINUS
        };
        /// Backward compatibility alias (Bison 3.6).
        typedef token_kind_type yytokentype;
    };

    /// Token kind, as returned by yylex.
    typedef token::token_kind_type token_kind_type;

    /// Backward compatibility alias (Bison 3.6).
    typedef token_kind_type token_type;

    /// Symbol kinds.
    struct symbol_kind {
        enum symbol_kind_type {
            YYNTOKENS = 60, ///< Number of tokens.
            S_YYEMPTY = -2,
            S_YYEOF = 0, // "end of file"
            S_YYerror = 1, // error
            S_YYUNDEF = 2, // "invalid token"
            S_OPENCURLY = 3, // OPENCURLY
            S_CLOSECURLY = 4, // CLOSECURLY
            S_OPENSQUARE = 5, // OPENSQUARE
            S_CLOSESQUARE = 6, // CLOSESQUARE
            S_OPENPAREN = 7, // OPENPAREN
            S_CLOSEPAREN = 8, // CLOSEPAREN
            S_SEMICOLON = 9, // SEMICOLON
            S_NONLOCALRETURN = 10, // NONLOCALRETURN
            S_COMMA = 11, // COMMA
            S_HASH = 12, // HASH
            S_TILDE = 13, // TILDE
            S_NAME = 14, // NAME
            S_INTEGER = 15, // INTEGER
            S_INTEGER_RADIX = 16, // INTEGER_RADIX
            S_HEXADECIMAL = 17, // HEXADECIMAL
            S_FLOAT = 18, // FLOAT
            S_FLOAT_RADIX = 19, // FLOAT_RADIX
            S_FLOAT_EXPONENT = 20, // FLOAT_EXPONENT
            S_FLOAT_INF = 21, // FLOAT_INF
            S_ACCIDENTAL_STEPS = 22, // ACCIDENTAL_STEPS
            S_ACCIDENTAL_CENTS = 23, // ACCIDENTAL_CENTS
            S_SYMBOL_QUOTE = 24, // SYMBOL_QUOTE
            S_SYMBOL_SLASH = 25, // SYMBOL_SLASH
            S_STRINGLINE = 26, // STRINGLINE
            S_ASCII = 27, // ASCII
            S_PRIMITIVENAME = 28, // PRIMITIVENAME
            S_CLASSNAME = 29, // CLASSNAME
            S_CURRYARG = 30, // CURRYARG
            S_VAR = 31, // VAR
            S_ARG = 32, // ARG
            S_CLASSVAR = 33, // CLASSVAR
            S_SC_CONST = 34, // SC_CONST
            S_NILOBJ = 35, // NILOBJ
            S_TRUEOBJ = 36, // TRUEOBJ
            S_FALSEOBJ = 37, // FALSEOBJ
            S_PI = 38, // PI
            S_ELLIPSIS = 39, // ELLIPSIS
            S_DOTDOT = 40, // DOTDOT
            S_BEGINCLOSEDFUNC = 41, // BEGINCLOSEDFUNC
            S_BADTOKEN = 42, // BADTOKEN
            S_INTERPRET = 43, // INTERPRET
            S_LEFTARROW = 44, // LEFTARROW
            S_WHILE = 45, // WHILE
            S_COLON = 46, // COLON
            S_EQUALSSIGN = 47, // EQUALSSIGN
            S_BINOP = 48, // BINOP
            S_KEYBINOP = 49, // KEYBINOP
            S_MINUS = 50, // MINUS
            S_LESSTHAN = 51, // LESSTHAN
            S_GREATERTHAN = 52, // GREATERTHAN
            S_MULTIPLY = 53, // MULTIPLY
            S_ADD = 54, // ADD
            S_PIPE = 55, // PIPE
            S_READWRITEVAR = 56, // READWRITEVAR
            S_DOT = 57, // DOT
            S_BACKTICK = 58, // BACKTICK
            S_UMINUS = 59, // UMINUS
            S_YYACCEPT = 60, // $accept
            S_commandLine = 61, // commandLine
            S_basicBinOp = 62, // basicBinOp
            S_literal = 63, // literal
            S_64_binary_op_raw = 64, // binary_op.raw
            S_binary_op = 65, // binary_op
            S_nil = 66, // nil
            S_boolean = 67, // boolean
            S_symbol = 68, // symbol
            S_string = 69, // string
            S_integer = 70, // integer
            S_71_float_raw_unsigned = 71, // float.raw_unsigned
            S_72_float_raw = 72, // float.raw
            S_73_accidental_unsigned = 73, // accidental.unsigned
            S_accidental = 74, // accidental
            S_float = 75 // float
        };
    };

    /// (Internal) symbol kind.
    typedef symbol_kind::symbol_kind_type symbol_kind_type;

    /// The number of tokens.
    static const symbol_kind_type YYNTOKENS = symbol_kind::YYNTOKENS;

    /// A complete symbol.
    ///
    /// Expects its Base type to provide access to the symbol kind
    /// via kind ().
    ///
    /// Provide access to semantic value and location.
    template <typename Base> struct basic_symbol : Base {
        /// Alias to Base.
        typedef Base super_type;

        /// Default constructor.
        basic_symbol() YY_NOEXCEPT : value(), location() {}

#if 201103L <= YY_CPLUSPLUS
        /// Move constructor.
        basic_symbol(basic_symbol&& that): Base(std::move(that)), value(), location(std::move(that.location)) {
            switch (this->kind()) {
            case symbol_kind::S_73_accidental_unsigned: // accidental.unsigned
            case symbol_kind::S_accidental: // accidental
                value.move<AccidentalLitIndex>(std::move(that.value));
                break;

            case symbol_kind::S_literal: // literal
                value.move<AnyLiteralIndex>(std::move(that.value));
                break;

            case symbol_kind::S_boolean: // boolean
                value.move<BooleanLitIndex>(std::move(that.value));
                break;

            case symbol_kind::S_71_float_raw_unsigned: // float.raw_unsigned
            case symbol_kind::S_72_float_raw: // float.raw
                value.move<FloatLitIndex>(std::move(that.value));
                break;

            case symbol_kind::S_float: // float
                value.move<FloatProducingIndex>(std::move(that.value));
                break;

            case symbol_kind::S_integer: // integer
                value.move<IntLitIndex>(std::move(that.value));
                break;

            case symbol_kind::S_commandLine: // commandLine
            case symbol_kind::S_basicBinOp: // basicBinOp
                value.move<MessageIndex>(std::move(that.value));
                break;

            case symbol_kind::S_nil: // nil
                value.move<NilLitIndex>(std::move(that.value));
                break;

            case symbol_kind::S_OPENCURLY: // OPENCURLY
            case symbol_kind::S_CLOSECURLY: // CLOSECURLY
            case symbol_kind::S_OPENSQUARE: // OPENSQUARE
            case symbol_kind::S_CLOSESQUARE: // CLOSESQUARE
            case symbol_kind::S_OPENPAREN: // OPENPAREN
            case symbol_kind::S_CLOSEPAREN: // CLOSEPAREN
            case symbol_kind::S_SEMICOLON: // SEMICOLON
            case symbol_kind::S_NONLOCALRETURN: // NONLOCALRETURN
            case symbol_kind::S_COMMA: // COMMA
            case symbol_kind::S_HASH: // HASH
            case symbol_kind::S_TILDE: // TILDE
            case symbol_kind::S_NAME: // NAME
            case symbol_kind::S_INTEGER: // INTEGER
            case symbol_kind::S_INTEGER_RADIX: // INTEGER_RADIX
            case symbol_kind::S_HEXADECIMAL: // HEXADECIMAL
            case symbol_kind::S_FLOAT: // FLOAT
            case symbol_kind::S_FLOAT_RADIX: // FLOAT_RADIX
            case symbol_kind::S_FLOAT_EXPONENT: // FLOAT_EXPONENT
            case symbol_kind::S_FLOAT_INF: // FLOAT_INF
            case symbol_kind::S_ACCIDENTAL_STEPS: // ACCIDENTAL_STEPS
            case symbol_kind::S_ACCIDENTAL_CENTS: // ACCIDENTAL_CENTS
            case symbol_kind::S_SYMBOL_QUOTE: // SYMBOL_QUOTE
            case symbol_kind::S_SYMBOL_SLASH: // SYMBOL_SLASH
            case symbol_kind::S_STRINGLINE: // STRINGLINE
            case symbol_kind::S_ASCII: // ASCII
            case symbol_kind::S_PRIMITIVENAME: // PRIMITIVENAME
            case symbol_kind::S_CLASSNAME: // CLASSNAME
            case symbol_kind::S_CURRYARG: // CURRYARG
            case symbol_kind::S_VAR: // VAR
            case symbol_kind::S_ARG: // ARG
            case symbol_kind::S_CLASSVAR: // CLASSVAR
            case symbol_kind::S_SC_CONST: // SC_CONST
            case symbol_kind::S_NILOBJ: // NILOBJ
            case symbol_kind::S_TRUEOBJ: // TRUEOBJ
            case symbol_kind::S_FALSEOBJ: // FALSEOBJ
            case symbol_kind::S_PI: // PI
            case symbol_kind::S_ELLIPSIS: // ELLIPSIS
            case symbol_kind::S_DOTDOT: // DOTDOT
            case symbol_kind::S_BEGINCLOSEDFUNC: // BEGINCLOSEDFUNC
            case symbol_kind::S_BADTOKEN: // BADTOKEN
            case symbol_kind::S_INTERPRET: // INTERPRET
            case symbol_kind::S_LEFTARROW: // LEFTARROW
            case symbol_kind::S_WHILE: // WHILE
            case symbol_kind::S_COLON: // COLON
            case symbol_kind::S_EQUALSSIGN: // EQUALSSIGN
            case symbol_kind::S_BINOP: // BINOP
            case symbol_kind::S_KEYBINOP: // KEYBINOP
            case symbol_kind::S_MINUS: // MINUS
            case symbol_kind::S_LESSTHAN: // LESSTHAN
            case symbol_kind::S_GREATERTHAN: // GREATERTHAN
            case symbol_kind::S_MULTIPLY: // MULTIPLY
            case symbol_kind::S_ADD: // ADD
            case symbol_kind::S_PIPE: // PIPE
            case symbol_kind::S_READWRITEVAR: // READWRITEVAR
            case symbol_kind::S_DOT: // DOT
            case symbol_kind::S_BACKTICK: // BACKTICK
            case symbol_kind::S_UMINUS: // UMINUS
                value.move<NoValue>(std::move(that.value));
                break;

            case symbol_kind::S_64_binary_op_raw: // binary_op.raw
            case symbol_kind::S_binary_op: // binary_op
                value.move<SelectorIndex>(std::move(that.value));
                break;

            case symbol_kind::S_string: // string
                value.move<StringLitIndex>(std::move(that.value));
                break;

            case symbol_kind::S_symbol: // symbol
                value.move<SymbolLitIndex>(std::move(that.value));
                break;

            default:
                break;
            }
        }
#endif

        /// Copy constructor.
        basic_symbol(const basic_symbol& that);

        /// Constructors for typed symbols.
#if 201103L <= YY_CPLUSPLUS
        basic_symbol(typename Base::kind_type t, location_type&& l): Base(t), location(std::move(l)) {}
#else
        basic_symbol(typename Base::kind_type t, const location_type& l): Base(t), location(l) {}
#endif

#if 201103L <= YY_CPLUSPLUS
        basic_symbol(typename Base::kind_type t, AccidentalLitIndex&& v, location_type&& l):
            Base(t),
            value(std::move(v)),
            location(std::move(l)) {}
#else
        basic_symbol(typename Base::kind_type t, const AccidentalLitIndex& v, const location_type& l):
            Base(t),
            value(v),
            location(l) {}
#endif

#if 201103L <= YY_CPLUSPLUS
        basic_symbol(typename Base::kind_type t, AnyLiteralIndex&& v, location_type&& l):
            Base(t),
            value(std::move(v)),
            location(std::move(l)) {}
#else
        basic_symbol(typename Base::kind_type t, const AnyLiteralIndex& v, const location_type& l):
            Base(t),
            value(v),
            location(l) {}
#endif

#if 201103L <= YY_CPLUSPLUS
        basic_symbol(typename Base::kind_type t, BooleanLitIndex&& v, location_type&& l):
            Base(t),
            value(std::move(v)),
            location(std::move(l)) {}
#else
        basic_symbol(typename Base::kind_type t, const BooleanLitIndex& v, const location_type& l):
            Base(t),
            value(v),
            location(l) {}
#endif

#if 201103L <= YY_CPLUSPLUS
        basic_symbol(typename Base::kind_type t, FloatLitIndex&& v, location_type&& l):
            Base(t),
            value(std::move(v)),
            location(std::move(l)) {}
#else
        basic_symbol(typename Base::kind_type t, const FloatLitIndex& v, const location_type& l):
            Base(t),
            value(v),
            location(l) {}
#endif

#if 201103L <= YY_CPLUSPLUS
        basic_symbol(typename Base::kind_type t, FloatProducingIndex&& v, location_type&& l):
            Base(t),
            value(std::move(v)),
            location(std::move(l)) {}
#else
        basic_symbol(typename Base::kind_type t, const FloatProducingIndex& v, const location_type& l):
            Base(t),
            value(v),
            location(l) {}
#endif

#if 201103L <= YY_CPLUSPLUS
        basic_symbol(typename Base::kind_type t, IntLitIndex&& v, location_type&& l):
            Base(t),
            value(std::move(v)),
            location(std::move(l)) {}
#else
        basic_symbol(typename Base::kind_type t, const IntLitIndex& v, const location_type& l):
            Base(t),
            value(v),
            location(l) {}
#endif

#if 201103L <= YY_CPLUSPLUS
        basic_symbol(typename Base::kind_type t, MessageIndex&& v, location_type&& l):
            Base(t),
            value(std::move(v)),
            location(std::move(l)) {}
#else
        basic_symbol(typename Base::kind_type t, const MessageIndex& v, const location_type& l):
            Base(t),
            value(v),
            location(l) {}
#endif

#if 201103L <= YY_CPLUSPLUS
        basic_symbol(typename Base::kind_type t, NilLitIndex&& v, location_type&& l):
            Base(t),
            value(std::move(v)),
            location(std::move(l)) {}
#else
        basic_symbol(typename Base::kind_type t, const NilLitIndex& v, const location_type& l):
            Base(t),
            value(v),
            location(l) {}
#endif

#if 201103L <= YY_CPLUSPLUS
        basic_symbol(typename Base::kind_type t, NoValue&& v, location_type&& l):
            Base(t),
            value(std::move(v)),
            location(std::move(l)) {}
#else
        basic_symbol(typename Base::kind_type t, const NoValue& v, const location_type& l):
            Base(t),
            value(v),
            location(l) {}
#endif

#if 201103L <= YY_CPLUSPLUS
        basic_symbol(typename Base::kind_type t, SelectorIndex&& v, location_type&& l):
            Base(t),
            value(std::move(v)),
            location(std::move(l)) {}
#else
        basic_symbol(typename Base::kind_type t, const SelectorIndex& v, const location_type& l):
            Base(t),
            value(v),
            location(l) {}
#endif

#if 201103L <= YY_CPLUSPLUS
        basic_symbol(typename Base::kind_type t, StringLitIndex&& v, location_type&& l):
            Base(t),
            value(std::move(v)),
            location(std::move(l)) {}
#else
        basic_symbol(typename Base::kind_type t, const StringLitIndex& v, const location_type& l):
            Base(t),
            value(v),
            location(l) {}
#endif

#if 201103L <= YY_CPLUSPLUS
        basic_symbol(typename Base::kind_type t, SymbolLitIndex&& v, location_type&& l):
            Base(t),
            value(std::move(v)),
            location(std::move(l)) {}
#else
        basic_symbol(typename Base::kind_type t, const SymbolLitIndex& v, const location_type& l):
            Base(t),
            value(v),
            location(l) {}
#endif

        /// Destroy the symbol.
        ~basic_symbol() { clear(); }


        /// Destroy contents, and record that is empty.
        void clear() YY_NOEXCEPT {
            // User destructor.
            symbol_kind_type yykind = this->kind();
            basic_symbol<Base>& yysym = *this;
            (void)yysym;
            switch (yykind) {
            default:
                break;
            }

            // Value type destructor.
            switch (yykind) {
            case symbol_kind::S_73_accidental_unsigned: // accidental.unsigned
            case symbol_kind::S_accidental: // accidental
                value.template destroy<AccidentalLitIndex>();
                break;

            case symbol_kind::S_literal: // literal
                value.template destroy<AnyLiteralIndex>();
                break;

            case symbol_kind::S_boolean: // boolean
                value.template destroy<BooleanLitIndex>();
                break;

            case symbol_kind::S_71_float_raw_unsigned: // float.raw_unsigned
            case symbol_kind::S_72_float_raw: // float.raw
                value.template destroy<FloatLitIndex>();
                break;

            case symbol_kind::S_float: // float
                value.template destroy<FloatProducingIndex>();
                break;

            case symbol_kind::S_integer: // integer
                value.template destroy<IntLitIndex>();
                break;

            case symbol_kind::S_commandLine: // commandLine
            case symbol_kind::S_basicBinOp: // basicBinOp
                value.template destroy<MessageIndex>();
                break;

            case symbol_kind::S_nil: // nil
                value.template destroy<NilLitIndex>();
                break;

            case symbol_kind::S_OPENCURLY: // OPENCURLY
            case symbol_kind::S_CLOSECURLY: // CLOSECURLY
            case symbol_kind::S_OPENSQUARE: // OPENSQUARE
            case symbol_kind::S_CLOSESQUARE: // CLOSESQUARE
            case symbol_kind::S_OPENPAREN: // OPENPAREN
            case symbol_kind::S_CLOSEPAREN: // CLOSEPAREN
            case symbol_kind::S_SEMICOLON: // SEMICOLON
            case symbol_kind::S_NONLOCALRETURN: // NONLOCALRETURN
            case symbol_kind::S_COMMA: // COMMA
            case symbol_kind::S_HASH: // HASH
            case symbol_kind::S_TILDE: // TILDE
            case symbol_kind::S_NAME: // NAME
            case symbol_kind::S_INTEGER: // INTEGER
            case symbol_kind::S_INTEGER_RADIX: // INTEGER_RADIX
            case symbol_kind::S_HEXADECIMAL: // HEXADECIMAL
            case symbol_kind::S_FLOAT: // FLOAT
            case symbol_kind::S_FLOAT_RADIX: // FLOAT_RADIX
            case symbol_kind::S_FLOAT_EXPONENT: // FLOAT_EXPONENT
            case symbol_kind::S_FLOAT_INF: // FLOAT_INF
            case symbol_kind::S_ACCIDENTAL_STEPS: // ACCIDENTAL_STEPS
            case symbol_kind::S_ACCIDENTAL_CENTS: // ACCIDENTAL_CENTS
            case symbol_kind::S_SYMBOL_QUOTE: // SYMBOL_QUOTE
            case symbol_kind::S_SYMBOL_SLASH: // SYMBOL_SLASH
            case symbol_kind::S_STRINGLINE: // STRINGLINE
            case symbol_kind::S_ASCII: // ASCII
            case symbol_kind::S_PRIMITIVENAME: // PRIMITIVENAME
            case symbol_kind::S_CLASSNAME: // CLASSNAME
            case symbol_kind::S_CURRYARG: // CURRYARG
            case symbol_kind::S_VAR: // VAR
            case symbol_kind::S_ARG: // ARG
            case symbol_kind::S_CLASSVAR: // CLASSVAR
            case symbol_kind::S_SC_CONST: // SC_CONST
            case symbol_kind::S_NILOBJ: // NILOBJ
            case symbol_kind::S_TRUEOBJ: // TRUEOBJ
            case symbol_kind::S_FALSEOBJ: // FALSEOBJ
            case symbol_kind::S_PI: // PI
            case symbol_kind::S_ELLIPSIS: // ELLIPSIS
            case symbol_kind::S_DOTDOT: // DOTDOT
            case symbol_kind::S_BEGINCLOSEDFUNC: // BEGINCLOSEDFUNC
            case symbol_kind::S_BADTOKEN: // BADTOKEN
            case symbol_kind::S_INTERPRET: // INTERPRET
            case symbol_kind::S_LEFTARROW: // LEFTARROW
            case symbol_kind::S_WHILE: // WHILE
            case symbol_kind::S_COLON: // COLON
            case symbol_kind::S_EQUALSSIGN: // EQUALSSIGN
            case symbol_kind::S_BINOP: // BINOP
            case symbol_kind::S_KEYBINOP: // KEYBINOP
            case symbol_kind::S_MINUS: // MINUS
            case symbol_kind::S_LESSTHAN: // LESSTHAN
            case symbol_kind::S_GREATERTHAN: // GREATERTHAN
            case symbol_kind::S_MULTIPLY: // MULTIPLY
            case symbol_kind::S_ADD: // ADD
            case symbol_kind::S_PIPE: // PIPE
            case symbol_kind::S_READWRITEVAR: // READWRITEVAR
            case symbol_kind::S_DOT: // DOT
            case symbol_kind::S_BACKTICK: // BACKTICK
            case symbol_kind::S_UMINUS: // UMINUS
                value.template destroy<NoValue>();
                break;

            case symbol_kind::S_64_binary_op_raw: // binary_op.raw
            case symbol_kind::S_binary_op: // binary_op
                value.template destroy<SelectorIndex>();
                break;

            case symbol_kind::S_string: // string
                value.template destroy<StringLitIndex>();
                break;

            case symbol_kind::S_symbol: // symbol
                value.template destroy<SymbolLitIndex>();
                break;

            default:
                break;
            }

            Base::clear();
        }

        /// The user-facing name of this symbol.
        const char* name() const YY_NOEXCEPT { return parser::symbol_name(this->kind()); }

        /// Backward compatibility (Bison 3.6).
        symbol_kind_type type_get() const YY_NOEXCEPT;

        /// Whether empty.
        bool empty() const YY_NOEXCEPT;

        /// Destructive move, \a s is emptied into this.
        void move(basic_symbol& s);

        /// The semantic value.
        value_type value;

        /// The location.
        location_type location;

    private:
#if YY_CPLUSPLUS < 201103L
        /// Assignment operator.
        basic_symbol& operator=(const basic_symbol& that);
#endif
    };

    /// Type access provider for token (enum) based symbols.
    struct by_kind {
        /// The symbol kind as needed by the constructor.
        typedef token_kind_type kind_type;

        /// Default constructor.
        by_kind() YY_NOEXCEPT;

#if 201103L <= YY_CPLUSPLUS
        /// Move constructor.
        by_kind(by_kind&& that) YY_NOEXCEPT;
#endif

        /// Copy constructor.
        by_kind(const by_kind& that) YY_NOEXCEPT;

        /// Constructor from (external) token numbers.
        by_kind(kind_type t) YY_NOEXCEPT;


        /// Record that this symbol is empty.
        void clear() YY_NOEXCEPT;

        /// Steal the symbol kind from \a that.
        void move(by_kind& that);

        /// The (internal) type number (corresponding to \a type).
        /// \a empty when empty.
        symbol_kind_type kind() const YY_NOEXCEPT;

        /// Backward compatibility (Bison 3.6).
        symbol_kind_type type_get() const YY_NOEXCEPT;

        /// The symbol kind.
        /// \a S_YYEMPTY when empty.
        symbol_kind_type kind_;
    };

    /// Backward compatibility for a private implementation detail (Bison 3.6).
    typedef by_kind by_type;

    /// "External" symbols: returned by the scanner.
    struct symbol_type : basic_symbol<by_kind> {
        /// Superclass.
        typedef basic_symbol<by_kind> super_type;

        /// Empty symbol.
        symbol_type() YY_NOEXCEPT {}

        /// Constructor for valueless symbols, and symbols from each type.
#if 201103L <= YY_CPLUSPLUS
        symbol_type(int tok, location_type l):
            super_type(token_kind_type(tok), std::move(l))
#else
        symbol_type(int tok, const location_type& l):
            super_type(token_kind_type(tok), l)
#endif
        {
        }
#if 201103L <= YY_CPLUSPLUS
        symbol_type(int tok, NoValue v, location_type l):
            super_type(token_kind_type(tok), std::move(v), std::move(l))
#else
        symbol_type(int tok, const NoValue& v, const location_type& l):
            super_type(token_kind_type(tok), v, l)
#endif
        {
        }
    };

    /// Build a parser object.
    parser(ParserContext& cxt_yyarg);
    virtual ~parser();

#if 201103L <= YY_CPLUSPLUS
    /// Non copyable.
    parser(const parser&) = delete;
    /// Non copyable.
    parser& operator=(const parser&) = delete;
#endif

    /// Parse.  An alias for parse ().
    /// \returns  0 iff parsing succeeded.
    int operator()();

    /// Parse.
    /// \returns  0 iff parsing succeeded.
    virtual int parse();

#if YYDEBUG
    /// The current debugging stream.
    std::ostream& debug_stream() const YY_ATTRIBUTE_PURE;
    /// Set the current debugging stream.
    void set_debug_stream(std::ostream&);

    /// Type for debugging levels.
    typedef int debug_level_type;
    /// The current debugging level.
    debug_level_type debug_level() const YY_ATTRIBUTE_PURE;
    /// Set the current debugging level.
    void set_debug_level(debug_level_type l);
#endif

    /// Report a syntax error.
    /// \param loc    where the syntax error is found.
    /// \param msg    a description of the syntax error.
    virtual void error(const location_type& loc, const std::string& msg);

    /// Report a syntax error.
    void error(const syntax_error& err);

    /// The user-facing name of the symbol whose (internal) number is
    /// YYSYMBOL.  No bounds checking.
    static const char* symbol_name(symbol_kind_type yysymbol);

    // Implementation of make_symbol for each token kind.
#if 201103L <= YY_CPLUSPLUS
    static symbol_type make_YYEOF(location_type l) { return symbol_type(token::TOKEN_YYEOF, std::move(l)); }
#else
    static symbol_type make_YYEOF(const location_type& l) { return symbol_type(token::TOKEN_YYEOF, l); }
#endif
#if 201103L <= YY_CPLUSPLUS
    static symbol_type make_YYerror(location_type l) { return symbol_type(token::TOKEN_YYerror, std::move(l)); }
#else
    static symbol_type make_YYerror(const location_type& l) { return symbol_type(token::TOKEN_YYerror, l); }
#endif
#if 201103L <= YY_CPLUSPLUS
    static symbol_type make_YYUNDEF(location_type l) { return symbol_type(token::TOKEN_YYUNDEF, std::move(l)); }
#else
    static symbol_type make_YYUNDEF(const location_type& l) { return symbol_type(token::TOKEN_YYUNDEF, l); }
#endif
#if 201103L <= YY_CPLUSPLUS
    static symbol_type make_OPENCURLY(NoValue v, location_type l) {
        return symbol_type(token::TOKEN_OPENCURLY, std::move(v), std::move(l));
    }
#else
    static symbol_type make_OPENCURLY(const NoValue& v, const location_type& l) {
        return symbol_type(token::TOKEN_OPENCURLY, v, l);
    }
#endif
#if 201103L <= YY_CPLUSPLUS
    static symbol_type make_CLOSECURLY(NoValue v, location_type l) {
        return symbol_type(token::TOKEN_CLOSECURLY, std::move(v), std::move(l));
    }
#else
    static symbol_type make_CLOSECURLY(const NoValue& v, const location_type& l) {
        return symbol_type(token::TOKEN_CLOSECURLY, v, l);
    }
#endif
#if 201103L <= YY_CPLUSPLUS
    static symbol_type make_OPENSQUARE(NoValue v, location_type l) {
        return symbol_type(token::TOKEN_OPENSQUARE, std::move(v), std::move(l));
    }
#else
    static symbol_type make_OPENSQUARE(const NoValue& v, const location_type& l) {
        return symbol_type(token::TOKEN_OPENSQUARE, v, l);
    }
#endif
#if 201103L <= YY_CPLUSPLUS
    static symbol_type make_CLOSESQUARE(NoValue v, location_type l) {
        return symbol_type(token::TOKEN_CLOSESQUARE, std::move(v), std::move(l));
    }
#else
    static symbol_type make_CLOSESQUARE(const NoValue& v, const location_type& l) {
        return symbol_type(token::TOKEN_CLOSESQUARE, v, l);
    }
#endif
#if 201103L <= YY_CPLUSPLUS
    static symbol_type make_OPENPAREN(NoValue v, location_type l) {
        return symbol_type(token::TOKEN_OPENPAREN, std::move(v), std::move(l));
    }
#else
    static symbol_type make_OPENPAREN(const NoValue& v, const location_type& l) {
        return symbol_type(token::TOKEN_OPENPAREN, v, l);
    }
#endif
#if 201103L <= YY_CPLUSPLUS
    static symbol_type make_CLOSEPAREN(NoValue v, location_type l) {
        return symbol_type(token::TOKEN_CLOSEPAREN, std::move(v), std::move(l));
    }
#else
    static symbol_type make_CLOSEPAREN(const NoValue& v, const location_type& l) {
        return symbol_type(token::TOKEN_CLOSEPAREN, v, l);
    }
#endif
#if 201103L <= YY_CPLUSPLUS
    static symbol_type make_SEMICOLON(NoValue v, location_type l) {
        return symbol_type(token::TOKEN_SEMICOLON, std::move(v), std::move(l));
    }
#else
    static symbol_type make_SEMICOLON(const NoValue& v, const location_type& l) {
        return symbol_type(token::TOKEN_SEMICOLON, v, l);
    }
#endif
#if 201103L <= YY_CPLUSPLUS
    static symbol_type make_NONLOCALRETURN(NoValue v, location_type l) {
        return symbol_type(token::TOKEN_NONLOCALRETURN, std::move(v), std::move(l));
    }
#else
    static symbol_type make_NONLOCALRETURN(const NoValue& v, const location_type& l) {
        return symbol_type(token::TOKEN_NONLOCALRETURN, v, l);
    }
#endif
#if 201103L <= YY_CPLUSPLUS
    static symbol_type make_COMMA(NoValue v, location_type l) {
        return symbol_type(token::TOKEN_COMMA, std::move(v), std::move(l));
    }
#else
    static symbol_type make_COMMA(const NoValue& v, const location_type& l) {
        return symbol_type(token::TOKEN_COMMA, v, l);
    }
#endif
#if 201103L <= YY_CPLUSPLUS
    static symbol_type make_HASH(NoValue v, location_type l) {
        return symbol_type(token::TOKEN_HASH, std::move(v), std::move(l));
    }
#else
    static symbol_type make_HASH(const NoValue& v, const location_type& l) {
        return symbol_type(token::TOKEN_HASH, v, l);
    }
#endif
#if 201103L <= YY_CPLUSPLUS
    static symbol_type make_TILDE(NoValue v, location_type l) {
        return symbol_type(token::TOKEN_TILDE, std::move(v), std::move(l));
    }
#else
    static symbol_type make_TILDE(const NoValue& v, const location_type& l) {
        return symbol_type(token::TOKEN_TILDE, v, l);
    }
#endif
#if 201103L <= YY_CPLUSPLUS
    static symbol_type make_NAME(NoValue v, location_type l) {
        return symbol_type(token::TOKEN_NAME, std::move(v), std::move(l));
    }
#else
    static symbol_type make_NAME(const NoValue& v, const location_type& l) {
        return symbol_type(token::TOKEN_NAME, v, l);
    }
#endif
#if 201103L <= YY_CPLUSPLUS
    static symbol_type make_INTEGER(NoValue v, location_type l) {
        return symbol_type(token::TOKEN_INTEGER, std::move(v), std::move(l));
    }
#else
    static symbol_type make_INTEGER(const NoValue& v, const location_type& l) {
        return symbol_type(token::TOKEN_INTEGER, v, l);
    }
#endif
#if 201103L <= YY_CPLUSPLUS
    static symbol_type make_INTEGER_RADIX(NoValue v, location_type l) {
        return symbol_type(token::TOKEN_INTEGER_RADIX, std::move(v), std::move(l));
    }
#else
    static symbol_type make_INTEGER_RADIX(const NoValue& v, const location_type& l) {
        return symbol_type(token::TOKEN_INTEGER_RADIX, v, l);
    }
#endif
#if 201103L <= YY_CPLUSPLUS
    static symbol_type make_HEXADECIMAL(NoValue v, location_type l) {
        return symbol_type(token::TOKEN_HEXADECIMAL, std::move(v), std::move(l));
    }
#else
    static symbol_type make_HEXADECIMAL(const NoValue& v, const location_type& l) {
        return symbol_type(token::TOKEN_HEXADECIMAL, v, l);
    }
#endif
#if 201103L <= YY_CPLUSPLUS
    static symbol_type make_FLOAT(NoValue v, location_type l) {
        return symbol_type(token::TOKEN_FLOAT, std::move(v), std::move(l));
    }
#else
    static symbol_type make_FLOAT(const NoValue& v, const location_type& l) {
        return symbol_type(token::TOKEN_FLOAT, v, l);
    }
#endif
#if 201103L <= YY_CPLUSPLUS
    static symbol_type make_FLOAT_RADIX(NoValue v, location_type l) {
        return symbol_type(token::TOKEN_FLOAT_RADIX, std::move(v), std::move(l));
    }
#else
    static symbol_type make_FLOAT_RADIX(const NoValue& v, const location_type& l) {
        return symbol_type(token::TOKEN_FLOAT_RADIX, v, l);
    }
#endif
#if 201103L <= YY_CPLUSPLUS
    static symbol_type make_FLOAT_EXPONENT(NoValue v, location_type l) {
        return symbol_type(token::TOKEN_FLOAT_EXPONENT, std::move(v), std::move(l));
    }
#else
    static symbol_type make_FLOAT_EXPONENT(const NoValue& v, const location_type& l) {
        return symbol_type(token::TOKEN_FLOAT_EXPONENT, v, l);
    }
#endif
#if 201103L <= YY_CPLUSPLUS
    static symbol_type make_FLOAT_INF(NoValue v, location_type l) {
        return symbol_type(token::TOKEN_FLOAT_INF, std::move(v), std::move(l));
    }
#else
    static symbol_type make_FLOAT_INF(const NoValue& v, const location_type& l) {
        return symbol_type(token::TOKEN_FLOAT_INF, v, l);
    }
#endif
#if 201103L <= YY_CPLUSPLUS
    static symbol_type make_ACCIDENTAL_STEPS(NoValue v, location_type l) {
        return symbol_type(token::TOKEN_ACCIDENTAL_STEPS, std::move(v), std::move(l));
    }
#else
    static symbol_type make_ACCIDENTAL_STEPS(const NoValue& v, const location_type& l) {
        return symbol_type(token::TOKEN_ACCIDENTAL_STEPS, v, l);
    }
#endif
#if 201103L <= YY_CPLUSPLUS
    static symbol_type make_ACCIDENTAL_CENTS(NoValue v, location_type l) {
        return symbol_type(token::TOKEN_ACCIDENTAL_CENTS, std::move(v), std::move(l));
    }
#else
    static symbol_type make_ACCIDENTAL_CENTS(const NoValue& v, const location_type& l) {
        return symbol_type(token::TOKEN_ACCIDENTAL_CENTS, v, l);
    }
#endif
#if 201103L <= YY_CPLUSPLUS
    static symbol_type make_SYMBOL_QUOTE(NoValue v, location_type l) {
        return symbol_type(token::TOKEN_SYMBOL_QUOTE, std::move(v), std::move(l));
    }
#else
    static symbol_type make_SYMBOL_QUOTE(const NoValue& v, const location_type& l) {
        return symbol_type(token::TOKEN_SYMBOL_QUOTE, v, l);
    }
#endif
#if 201103L <= YY_CPLUSPLUS
    static symbol_type make_SYMBOL_SLASH(NoValue v, location_type l) {
        return symbol_type(token::TOKEN_SYMBOL_SLASH, std::move(v), std::move(l));
    }
#else
    static symbol_type make_SYMBOL_SLASH(const NoValue& v, const location_type& l) {
        return symbol_type(token::TOKEN_SYMBOL_SLASH, v, l);
    }
#endif
#if 201103L <= YY_CPLUSPLUS
    static symbol_type make_STRINGLINE(NoValue v, location_type l) {
        return symbol_type(token::TOKEN_STRINGLINE, std::move(v), std::move(l));
    }
#else
    static symbol_type make_STRINGLINE(const NoValue& v, const location_type& l) {
        return symbol_type(token::TOKEN_STRINGLINE, v, l);
    }
#endif
#if 201103L <= YY_CPLUSPLUS
    static symbol_type make_ASCII(NoValue v, location_type l) {
        return symbol_type(token::TOKEN_ASCII, std::move(v), std::move(l));
    }
#else
    static symbol_type make_ASCII(const NoValue& v, const location_type& l) {
        return symbol_type(token::TOKEN_ASCII, v, l);
    }
#endif
#if 201103L <= YY_CPLUSPLUS
    static symbol_type make_PRIMITIVENAME(NoValue v, location_type l) {
        return symbol_type(token::TOKEN_PRIMITIVENAME, std::move(v), std::move(l));
    }
#else
    static symbol_type make_PRIMITIVENAME(const NoValue& v, const location_type& l) {
        return symbol_type(token::TOKEN_PRIMITIVENAME, v, l);
    }
#endif
#if 201103L <= YY_CPLUSPLUS
    static symbol_type make_CLASSNAME(NoValue v, location_type l) {
        return symbol_type(token::TOKEN_CLASSNAME, std::move(v), std::move(l));
    }
#else
    static symbol_type make_CLASSNAME(const NoValue& v, const location_type& l) {
        return symbol_type(token::TOKEN_CLASSNAME, v, l);
    }
#endif
#if 201103L <= YY_CPLUSPLUS
    static symbol_type make_CURRYARG(NoValue v, location_type l) {
        return symbol_type(token::TOKEN_CURRYARG, std::move(v), std::move(l));
    }
#else
    static symbol_type make_CURRYARG(const NoValue& v, const location_type& l) {
        return symbol_type(token::TOKEN_CURRYARG, v, l);
    }
#endif
#if 201103L <= YY_CPLUSPLUS
    static symbol_type make_VAR(NoValue v, location_type l) {
        return symbol_type(token::TOKEN_VAR, std::move(v), std::move(l));
    }
#else
    static symbol_type make_VAR(const NoValue& v, const location_type& l) {
        return symbol_type(token::TOKEN_VAR, v, l);
    }
#endif
#if 201103L <= YY_CPLUSPLUS
    static symbol_type make_ARG(NoValue v, location_type l) {
        return symbol_type(token::TOKEN_ARG, std::move(v), std::move(l));
    }
#else
    static symbol_type make_ARG(const NoValue& v, const location_type& l) {
        return symbol_type(token::TOKEN_ARG, v, l);
    }
#endif
#if 201103L <= YY_CPLUSPLUS
    static symbol_type make_CLASSVAR(NoValue v, location_type l) {
        return symbol_type(token::TOKEN_CLASSVAR, std::move(v), std::move(l));
    }
#else
    static symbol_type make_CLASSVAR(const NoValue& v, const location_type& l) {
        return symbol_type(token::TOKEN_CLASSVAR, v, l);
    }
#endif
#if 201103L <= YY_CPLUSPLUS
    static symbol_type make_SC_CONST(NoValue v, location_type l) {
        return symbol_type(token::TOKEN_SC_CONST, std::move(v), std::move(l));
    }
#else
    static symbol_type make_SC_CONST(const NoValue& v, const location_type& l) {
        return symbol_type(token::TOKEN_SC_CONST, v, l);
    }
#endif
#if 201103L <= YY_CPLUSPLUS
    static symbol_type make_NILOBJ(NoValue v, location_type l) {
        return symbol_type(token::TOKEN_NILOBJ, std::move(v), std::move(l));
    }
#else
    static symbol_type make_NILOBJ(const NoValue& v, const location_type& l) {
        return symbol_type(token::TOKEN_NILOBJ, v, l);
    }
#endif
#if 201103L <= YY_CPLUSPLUS
    static symbol_type make_TRUEOBJ(NoValue v, location_type l) {
        return symbol_type(token::TOKEN_TRUEOBJ, std::move(v), std::move(l));
    }
#else
    static symbol_type make_TRUEOBJ(const NoValue& v, const location_type& l) {
        return symbol_type(token::TOKEN_TRUEOBJ, v, l);
    }
#endif
#if 201103L <= YY_CPLUSPLUS
    static symbol_type make_FALSEOBJ(NoValue v, location_type l) {
        return symbol_type(token::TOKEN_FALSEOBJ, std::move(v), std::move(l));
    }
#else
    static symbol_type make_FALSEOBJ(const NoValue& v, const location_type& l) {
        return symbol_type(token::TOKEN_FALSEOBJ, v, l);
    }
#endif
#if 201103L <= YY_CPLUSPLUS
    static symbol_type make_PI(NoValue v, location_type l) {
        return symbol_type(token::TOKEN_PI, std::move(v), std::move(l));
    }
#else
    static symbol_type make_PI(const NoValue& v, const location_type& l) { return symbol_type(token::TOKEN_PI, v, l); }
#endif
#if 201103L <= YY_CPLUSPLUS
    static symbol_type make_ELLIPSIS(NoValue v, location_type l) {
        return symbol_type(token::TOKEN_ELLIPSIS, std::move(v), std::move(l));
    }
#else
    static symbol_type make_ELLIPSIS(const NoValue& v, const location_type& l) {
        return symbol_type(token::TOKEN_ELLIPSIS, v, l);
    }
#endif
#if 201103L <= YY_CPLUSPLUS
    static symbol_type make_DOTDOT(NoValue v, location_type l) {
        return symbol_type(token::TOKEN_DOTDOT, std::move(v), std::move(l));
    }
#else
    static symbol_type make_DOTDOT(const NoValue& v, const location_type& l) {
        return symbol_type(token::TOKEN_DOTDOT, v, l);
    }
#endif
#if 201103L <= YY_CPLUSPLUS
    static symbol_type make_BEGINCLOSEDFUNC(NoValue v, location_type l) {
        return symbol_type(token::TOKEN_BEGINCLOSEDFUNC, std::move(v), std::move(l));
    }
#else
    static symbol_type make_BEGINCLOSEDFUNC(const NoValue& v, const location_type& l) {
        return symbol_type(token::TOKEN_BEGINCLOSEDFUNC, v, l);
    }
#endif
#if 201103L <= YY_CPLUSPLUS
    static symbol_type make_BADTOKEN(NoValue v, location_type l) {
        return symbol_type(token::TOKEN_BADTOKEN, std::move(v), std::move(l));
    }
#else
    static symbol_type make_BADTOKEN(const NoValue& v, const location_type& l) {
        return symbol_type(token::TOKEN_BADTOKEN, v, l);
    }
#endif
#if 201103L <= YY_CPLUSPLUS
    static symbol_type make_INTERPRET(NoValue v, location_type l) {
        return symbol_type(token::TOKEN_INTERPRET, std::move(v), std::move(l));
    }
#else
    static symbol_type make_INTERPRET(const NoValue& v, const location_type& l) {
        return symbol_type(token::TOKEN_INTERPRET, v, l);
    }
#endif
#if 201103L <= YY_CPLUSPLUS
    static symbol_type make_LEFTARROW(NoValue v, location_type l) {
        return symbol_type(token::TOKEN_LEFTARROW, std::move(v), std::move(l));
    }
#else
    static symbol_type make_LEFTARROW(const NoValue& v, const location_type& l) {
        return symbol_type(token::TOKEN_LEFTARROW, v, l);
    }
#endif
#if 201103L <= YY_CPLUSPLUS
    static symbol_type make_WHILE(NoValue v, location_type l) {
        return symbol_type(token::TOKEN_WHILE, std::move(v), std::move(l));
    }
#else
    static symbol_type make_WHILE(const NoValue& v, const location_type& l) {
        return symbol_type(token::TOKEN_WHILE, v, l);
    }
#endif
#if 201103L <= YY_CPLUSPLUS
    static symbol_type make_COLON(NoValue v, location_type l) {
        return symbol_type(token::TOKEN_COLON, std::move(v), std::move(l));
    }
#else
    static symbol_type make_COLON(const NoValue& v, const location_type& l) {
        return symbol_type(token::TOKEN_COLON, v, l);
    }
#endif
#if 201103L <= YY_CPLUSPLUS
    static symbol_type make_EQUALSSIGN(NoValue v, location_type l) {
        return symbol_type(token::TOKEN_EQUALSSIGN, std::move(v), std::move(l));
    }
#else
    static symbol_type make_EQUALSSIGN(const NoValue& v, const location_type& l) {
        return symbol_type(token::TOKEN_EQUALSSIGN, v, l);
    }
#endif
#if 201103L <= YY_CPLUSPLUS
    static symbol_type make_BINOP(NoValue v, location_type l) {
        return symbol_type(token::TOKEN_BINOP, std::move(v), std::move(l));
    }
#else
    static symbol_type make_BINOP(const NoValue& v, const location_type& l) {
        return symbol_type(token::TOKEN_BINOP, v, l);
    }
#endif
#if 201103L <= YY_CPLUSPLUS
    static symbol_type make_KEYBINOP(NoValue v, location_type l) {
        return symbol_type(token::TOKEN_KEYBINOP, std::move(v), std::move(l));
    }
#else
    static symbol_type make_KEYBINOP(const NoValue& v, const location_type& l) {
        return symbol_type(token::TOKEN_KEYBINOP, v, l);
    }
#endif
#if 201103L <= YY_CPLUSPLUS
    static symbol_type make_MINUS(NoValue v, location_type l) {
        return symbol_type(token::TOKEN_MINUS, std::move(v), std::move(l));
    }
#else
    static symbol_type make_MINUS(const NoValue& v, const location_type& l) {
        return symbol_type(token::TOKEN_MINUS, v, l);
    }
#endif
#if 201103L <= YY_CPLUSPLUS
    static symbol_type make_LESSTHAN(NoValue v, location_type l) {
        return symbol_type(token::TOKEN_LESSTHAN, std::move(v), std::move(l));
    }
#else
    static symbol_type make_LESSTHAN(const NoValue& v, const location_type& l) {
        return symbol_type(token::TOKEN_LESSTHAN, v, l);
    }
#endif
#if 201103L <= YY_CPLUSPLUS
    static symbol_type make_GREATERTHAN(NoValue v, location_type l) {
        return symbol_type(token::TOKEN_GREATERTHAN, std::move(v), std::move(l));
    }
#else
    static symbol_type make_GREATERTHAN(const NoValue& v, const location_type& l) {
        return symbol_type(token::TOKEN_GREATERTHAN, v, l);
    }
#endif
#if 201103L <= YY_CPLUSPLUS
    static symbol_type make_MULTIPLY(NoValue v, location_type l) {
        return symbol_type(token::TOKEN_MULTIPLY, std::move(v), std::move(l));
    }
#else
    static symbol_type make_MULTIPLY(const NoValue& v, const location_type& l) {
        return symbol_type(token::TOKEN_MULTIPLY, v, l);
    }
#endif
#if 201103L <= YY_CPLUSPLUS
    static symbol_type make_ADD(NoValue v, location_type l) {
        return symbol_type(token::TOKEN_ADD, std::move(v), std::move(l));
    }
#else
    static symbol_type make_ADD(const NoValue& v, const location_type& l) {
        return symbol_type(token::TOKEN_ADD, v, l);
    }
#endif
#if 201103L <= YY_CPLUSPLUS
    static symbol_type make_PIPE(NoValue v, location_type l) {
        return symbol_type(token::TOKEN_PIPE, std::move(v), std::move(l));
    }
#else
    static symbol_type make_PIPE(const NoValue& v, const location_type& l) {
        return symbol_type(token::TOKEN_PIPE, v, l);
    }
#endif
#if 201103L <= YY_CPLUSPLUS
    static symbol_type make_READWRITEVAR(NoValue v, location_type l) {
        return symbol_type(token::TOKEN_READWRITEVAR, std::move(v), std::move(l));
    }
#else
    static symbol_type make_READWRITEVAR(const NoValue& v, const location_type& l) {
        return symbol_type(token::TOKEN_READWRITEVAR, v, l);
    }
#endif
#if 201103L <= YY_CPLUSPLUS
    static symbol_type make_DOT(NoValue v, location_type l) {
        return symbol_type(token::TOKEN_DOT, std::move(v), std::move(l));
    }
#else
    static symbol_type make_DOT(const NoValue& v, const location_type& l) {
        return symbol_type(token::TOKEN_DOT, v, l);
    }
#endif
#if 201103L <= YY_CPLUSPLUS
    static symbol_type make_BACKTICK(NoValue v, location_type l) {
        return symbol_type(token::TOKEN_BACKTICK, std::move(v), std::move(l));
    }
#else
    static symbol_type make_BACKTICK(const NoValue& v, const location_type& l) {
        return symbol_type(token::TOKEN_BACKTICK, v, l);
    }
#endif
#if 201103L <= YY_CPLUSPLUS
    static symbol_type make_UMINUS(NoValue v, location_type l) {
        return symbol_type(token::TOKEN_UMINUS, std::move(v), std::move(l));
    }
#else
    static symbol_type make_UMINUS(const NoValue& v, const location_type& l) {
        return symbol_type(token::TOKEN_UMINUS, v, l);
    }
#endif


    class context {
    public:
        context(const parser& yyparser, const symbol_type& yyla);
        const symbol_type& lookahead() const YY_NOEXCEPT { return yyla_; }
        symbol_kind_type token() const YY_NOEXCEPT { return yyla_.kind(); }
        const location_type& location() const YY_NOEXCEPT { return yyla_.location; }

        /// Put in YYARG at most YYARGN of the expected tokens, and return the
        /// number of tokens stored in YYARG.  If YYARG is null, return the
        /// number of expected tokens (guaranteed to be less than YYNTOKENS).
        int expected_tokens(symbol_kind_type yyarg[], int yyargn) const;

    private:
        const parser& yyparser_;
        const symbol_type& yyla_;
    };

private:
#if YY_CPLUSPLUS < 201103L
    /// Non copyable.
    parser(const parser&);
    /// Non copyable.
    parser& operator=(const parser&);
#endif


    /// Stored state numbers (used for stacks).
    typedef signed char state_type;

    /// Report a syntax error
    /// \param yyctx     the context in which the error occurred.
    void report_syntax_error(const context& yyctx) const;
    /// Compute post-reduction state.
    /// \param yystate   the current state
    /// \param yysym     the nonterminal to push on the stack
    static state_type yy_lr_goto_state_(state_type yystate, int yysym);

    /// Whether the given \c yypact_ value indicates a defaulted state.
    /// \param yyvalue   the value to check
    static bool yy_pact_value_is_default_(int yyvalue) YY_NOEXCEPT;

    /// Whether the given \c yytable_ value indicates a syntax error.
    /// \param yyvalue   the value to check
    static bool yy_table_value_is_error_(int yyvalue) YY_NOEXCEPT;

    static const signed char yypact_ninf_;
    static const signed char yytable_ninf_;

    /// Convert a scanner token kind \a t to a symbol kind.
    /// In theory \a t should be a token_kind_type, but character literals
    /// are valid, yet not members of the token_kind_type enum.
    static symbol_kind_type yytranslate_(int t) YY_NOEXCEPT;


    // Tables.
    // YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
    // STATE-NUM.
    static const signed char yypact_[];

    // YYDEFACT[STATE-NUM] -- Default reduction number in state STATE-NUM.
    // Performed when YYTABLE does not specify something else to do.  Zero
    // means the default is an error.
    static const signed char yydefact_[];

    // YYPGOTO[NTERM-NUM].
    static const signed char yypgoto_[];

    // YYDEFGOTO[NTERM-NUM].
    static const signed char yydefgoto_[];

    // YYTABLE[YYPACT[STATE-NUM]] -- What to do in state STATE-NUM.  If
    // positive, shift that token.  If negative, reduce the rule whose
    // number is the opposite.  If YYTABLE_NINF, syntax error.
    static const signed char yytable_[];

    static const signed char yycheck_[];

    // YYSTOS[STATE-NUM] -- The symbol kind of the accessing symbol of
    // state STATE-NUM.
    static const signed char yystos_[];

    // YYR1[RULE-NUM] -- Symbol kind of the left-hand side of rule RULE-NUM.
    static const signed char yyr1_[];

    // YYR2[RULE-NUM] -- Number of symbols on the right-hand side of rule RULE-NUM.
    static const signed char yyr2_[];


#if YYDEBUG
    // YYRLINE[YYN] -- Source line where rule number YYN was defined.
    static const unsigned char yyrline_[];
    /// Report on the debug stream that the rule \a r is going to be reduced.
    virtual void yy_reduce_print_(int r) const;
    /// Print the state stack on the debug stream.
    virtual void yy_stack_print_() const;

    /// Debugging level.
    int yydebug_;
    /// Debug stream.
    std::ostream* yycdebug_;

    /// \brief Display a symbol kind, value and location.
    /// \param yyo    The output stream.
    /// \param yysym  The symbol.
    template <typename Base> void yy_print_(std::ostream& yyo, const basic_symbol<Base>& yysym) const;
#endif

    /// \brief Reclaim the memory associated to a symbol.
    /// \param yymsg     Why this token is reclaimed.
    ///                  If null, print nothing.
    /// \param yysym     The symbol.
    template <typename Base> void yy_destroy_(const char* yymsg, basic_symbol<Base>& yysym) const;

private:
    /// Type access provider for state based symbols.
    struct by_state {
        /// Default constructor.
        by_state() YY_NOEXCEPT;

        /// The symbol kind as needed by the constructor.
        typedef state_type kind_type;

        /// Constructor.
        by_state(kind_type s) YY_NOEXCEPT;

        /// Copy constructor.
        by_state(const by_state& that) YY_NOEXCEPT;

        /// Record that this symbol is empty.
        void clear() YY_NOEXCEPT;

        /// Steal the symbol kind from \a that.
        void move(by_state& that);

        /// The symbol kind (corresponding to \a state).
        /// \a symbol_kind::S_YYEMPTY when empty.
        symbol_kind_type kind() const YY_NOEXCEPT;

        /// The state number used to denote an empty symbol.
        /// We use the initial state, as it does not have a value.
        enum { empty_state = 0 };

        /// The state.
        /// \a empty when empty.
        state_type state;
    };

    /// "Internal" symbol: element of the stack.
    struct stack_symbol_type : basic_symbol<by_state> {
        /// Superclass.
        typedef basic_symbol<by_state> super_type;
        /// Construct an empty symbol.
        stack_symbol_type();
        /// Move or copy construction.
        stack_symbol_type(YY_RVREF(stack_symbol_type) that);
        /// Steal the contents from \a sym to build this.
        stack_symbol_type(state_type s, YY_MOVE_REF(symbol_type) sym);
#if YY_CPLUSPLUS < 201103L
        /// Assignment, needed by push_back by some old implementations.
        /// Moves the contents of that.
        stack_symbol_type& operator=(stack_symbol_type& that);

        /// Assignment, needed by push_back by other implementations.
        /// Needed by some other old implementations.
        stack_symbol_type& operator=(const stack_symbol_type& that);
#endif
    };

    /// A stack with random access from its top.
    template <typename T, typename S = std::vector<T>> class stack {
    public:
        // Hide our reversed order.
        typedef typename S::iterator iterator;
        typedef typename S::const_iterator const_iterator;
        typedef typename S::size_type size_type;
        typedef typename std::ptrdiff_t index_type;

        stack(size_type n = 200) YY_NOEXCEPT : seq_(n) {}

#if 201103L <= YY_CPLUSPLUS
        /// Non copyable.
        stack(const stack&) = delete;
        /// Non copyable.
        stack& operator=(const stack&) = delete;
#endif

        /// Random access.
        ///
        /// Index 0 returns the topmost element.
        const T& operator[](index_type i) const { return seq_[size_type(size() - 1 - i)]; }

        /// Random access.
        ///
        /// Index 0 returns the topmost element.
        T& operator[](index_type i) { return seq_[size_type(size() - 1 - i)]; }

        /// Steal the contents of \a t.
        ///
        /// Close to move-semantics.
        void push(YY_MOVE_REF(T) t) {
            seq_.push_back(T());
            operator[](0).move(t);
        }

        /// Pop elements from the stack.
        void pop(std::ptrdiff_t n = 1) YY_NOEXCEPT {
            for (; 0 < n; --n)
                seq_.pop_back();
        }

        /// Pop all elements from the stack.
        void clear() YY_NOEXCEPT { seq_.clear(); }

        /// Number of elements on the stack.
        index_type size() const YY_NOEXCEPT { return index_type(seq_.size()); }

        /// Iterator on top of the stack (going downwards).
        const_iterator begin() const YY_NOEXCEPT { return seq_.begin(); }

        /// Bottom of the stack.
        const_iterator end() const YY_NOEXCEPT { return seq_.end(); }

        /// Present a slice of the top of a stack.
        class slice {
        public:
            slice(const stack& stack, index_type range) YY_NOEXCEPT : stack_(stack), range_(range) {}

            const T& operator[](index_type i) const { return stack_[range_ - i]; }

        private:
            const stack& stack_;
            index_type range_;
        };

    private:
#if YY_CPLUSPLUS < 201103L
        /// Non copyable.
        stack(const stack&);
        /// Non copyable.
        stack& operator=(const stack&);
#endif
        /// The wrapped container.
        S seq_;
    };


    /// Stack type.
    typedef stack<stack_symbol_type> stack_type;

    /// The stack.
    stack_type yystack_;

    /// Push a new state on the stack.
    /// \param m    a debug message to display
    ///             if null, no trace is output.
    /// \param sym  the symbol
    /// \warning the contents of \a s.value is stolen.
    void yypush_(const char* m, YY_MOVE_REF(stack_symbol_type) sym);

    /// Push a new look ahead token on the state on the stack.
    /// \param m    a debug message to display
    ///             if null, no trace is output.
    /// \param s    the state
    /// \param sym  the symbol (for its value and location).
    /// \warning the contents of \a sym.value is stolen.
    void yypush_(const char* m, state_type s, YY_MOVE_REF(symbol_type) sym);

    /// Pop \a n symbols from the stack.
    void yypop_(int n = 1) YY_NOEXCEPT;

    /// Constants.
    enum {
        yylast_ = 59, ///< Last index in yytable_.
        yynnts_ = 16, ///< Number of nonterminal symbols.
        yyfinal_ = 32 ///< Termination state number.
    };


    // User arguments.
    ParserContext& cxt;
};


#line 6 "langutils/sc_parser/src/sc_grammar.y"
}} // sc::parser
#line 2544 "langutils/sc_parser/src/sc_grammar_parser.hpp"


#endif // !YY_YY_LANGUTILS_SC_PARSER_SRC_SC_GRAMMAR_PARSER_HPP_INCLUDED
