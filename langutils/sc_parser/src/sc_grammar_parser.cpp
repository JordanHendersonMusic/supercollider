// A Bison parser, made by GNU Bison 3.8.2.

// Skeleton implementation for Bison LALR(1) parsers in C++

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

// DO NOT RELY ON FEATURES THAT ARE NOT DOCUMENTED in the manual,
// especially those whose name start with YY_ or yy_.  They are
// private implementation details that can be changed or removed.

// "%code top" blocks.
#line 37 "langutils/sc_parser/src/sc_grammar.y"


#include "sc_grammar_parser.hpp"
#include "indexes_typed.hpp"
#include "nodes.hpp"
#include "lexer.hpp"

namespace sc::parser {
class parser;
}

static int yylex(sc::parser::parser::value_type* v, sc::lex::SourceCodeRange* loc, sc::parser::ParserContext& cxt);


using namespace sc::parser::nodes;


#line 58 "langutils/sc_parser/src/sc_grammar_parser.cpp"


#include "sc_grammar_parser.hpp"


#ifndef YY_
#    if defined YYENABLE_NLS && YYENABLE_NLS
#        if ENABLE_NLS
#            include <libintl.h> // FIXME: INFRINGES ON USER NAME SPACE.
#            define YY_(msgid) dgettext("bison-runtime", msgid)
#        endif
#    endif
#    ifndef YY_
#        define YY_(msgid) msgid
#    endif
#endif


// Whether we are compiled with exception support.
#ifndef YY_EXCEPTIONS
#    if defined __GNUC__ && !defined __EXCEPTIONS
#        define YY_EXCEPTIONS 0
#    else
#        define YY_EXCEPTIONS 1
#    endif
#endif

#define YYRHSLOC(Rhs, K) ((Rhs)[K].location)
/* YYLLOC_DEFAULT -- Set CURRENT to span from RHS[1] to RHS[N].
   If N is 0, then set CURRENT to the empty location which ends
   the previous symbol: RHS[0] (always defined).  */

#ifndef YYLLOC_DEFAULT
#    define YYLLOC_DEFAULT(Current, Rhs, N)                                                                            \
        do                                                                                                             \
            if (N) {                                                                                                   \
                (Current).begin = YYRHSLOC(Rhs, 1).begin;                                                              \
                (Current).end = YYRHSLOC(Rhs, N).end;                                                                  \
            } else {                                                                                                   \
                (Current).begin = (Current).end = YYRHSLOC(Rhs, 0).end;                                                \
            }                                                                                                          \
        while (false)
#endif


// Enable debugging if requested.
#if YYDEBUG

// A pseudo ostream that takes yydebug_ into account.
#    define YYCDEBUG                                                                                                   \
        if (yydebug_)                                                                                                  \
        (*yycdebug_)

#    define YY_SYMBOL_PRINT(Title, Symbol)                                                                             \
        do {                                                                                                           \
            if (yydebug_) {                                                                                            \
                *yycdebug_ << Title << ' ';                                                                            \
                yy_print_(*yycdebug_, Symbol);                                                                         \
                *yycdebug_ << '\n';                                                                                    \
            }                                                                                                          \
        } while (false)

#    define YY_REDUCE_PRINT(Rule)                                                                                      \
        do {                                                                                                           \
            if (yydebug_)                                                                                              \
                yy_reduce_print_(Rule);                                                                                \
        } while (false)

#    define YY_STACK_PRINT()                                                                                           \
        do {                                                                                                           \
            if (yydebug_)                                                                                              \
                yy_stack_print_();                                                                                     \
        } while (false)

#else // !YYDEBUG

#    define YYCDEBUG                                                                                                   \
        if (false)                                                                                                     \
        std::cerr
#    define YY_SYMBOL_PRINT(Title, Symbol) YY_USE(Symbol)
#    define YY_REDUCE_PRINT(Rule) static_cast<void>(0)
#    define YY_STACK_PRINT() static_cast<void>(0)

#endif // !YYDEBUG

#define yyerrok (yyerrstatus_ = 0)
#define yyclearin (yyla.clear())

#define YYACCEPT goto yyacceptlab
#define YYABORT goto yyabortlab
#define YYERROR goto yyerrorlab
#define YYRECOVERING() (!!yyerrstatus_)

#line 6 "langutils/sc_parser/src/sc_grammar.y"
namespace sc { namespace parser {
#line 158 "langutils/sc_parser/src/sc_grammar_parser.cpp"

/// Build a parser object.
parser::parser(ParserContext& cxt_yyarg)
#if YYDEBUG
    :
    yydebug_(false),
    yycdebug_(&std::cerr),
#else
    :
#endif
    cxt(cxt_yyarg) {
}

parser::~parser() {}

parser::syntax_error::~syntax_error() YY_NOEXCEPT YY_NOTHROW {}

/*---------.
| symbol.  |
`---------*/

// basic_symbol.
template <typename Base>
parser::basic_symbol<Base>::basic_symbol(const basic_symbol& that): Base(that), value(), location(that.location) {
    switch (this->kind()) {
    case symbol_kind::S_73_accidental_unsigned: // accidental.unsigned
    case symbol_kind::S_accidental: // accidental
        value.copy<AccidentalLitIndex>(YY_MOVE(that.value));
        break;

    case symbol_kind::S_literal: // literal
        value.copy<AnyLiteralIndex>(YY_MOVE(that.value));
        break;

    case symbol_kind::S_boolean: // boolean
        value.copy<BooleanLitIndex>(YY_MOVE(that.value));
        break;

    case symbol_kind::S_71_float_raw_unsigned: // float.raw_unsigned
    case symbol_kind::S_72_float_raw: // float.raw
        value.copy<FloatLitIndex>(YY_MOVE(that.value));
        break;

    case symbol_kind::S_float: // float
        value.copy<FloatProducingIndex>(YY_MOVE(that.value));
        break;

    case symbol_kind::S_integer: // integer
        value.copy<IntLitIndex>(YY_MOVE(that.value));
        break;

    case symbol_kind::S_commandLine: // commandLine
    case symbol_kind::S_basicBinOp: // basicBinOp
        value.copy<MessageIndex>(YY_MOVE(that.value));
        break;

    case symbol_kind::S_nil: // nil
        value.copy<NilLitIndex>(YY_MOVE(that.value));
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
        value.copy<NoValue>(YY_MOVE(that.value));
        break;

    case symbol_kind::S_64_binary_op_raw: // binary_op.raw
    case symbol_kind::S_binary_op: // binary_op
        value.copy<SelectorIndex>(YY_MOVE(that.value));
        break;

    case symbol_kind::S_string: // string
        value.copy<StringLitIndex>(YY_MOVE(that.value));
        break;

    case symbol_kind::S_symbol: // symbol
        value.copy<SymbolLitIndex>(YY_MOVE(that.value));
        break;

    default:
        break;
    }
}


template <typename Base> parser::symbol_kind_type parser::basic_symbol<Base>::type_get() const YY_NOEXCEPT {
    return this->kind();
}


template <typename Base> bool parser::basic_symbol<Base>::empty() const YY_NOEXCEPT {
    return this->kind() == symbol_kind::S_YYEMPTY;
}

template <typename Base> void parser::basic_symbol<Base>::move(basic_symbol& s) {
    super_type::move(s);
    switch (this->kind()) {
    case symbol_kind::S_73_accidental_unsigned: // accidental.unsigned
    case symbol_kind::S_accidental: // accidental
        value.move<AccidentalLitIndex>(YY_MOVE(s.value));
        break;

    case symbol_kind::S_literal: // literal
        value.move<AnyLiteralIndex>(YY_MOVE(s.value));
        break;

    case symbol_kind::S_boolean: // boolean
        value.move<BooleanLitIndex>(YY_MOVE(s.value));
        break;

    case symbol_kind::S_71_float_raw_unsigned: // float.raw_unsigned
    case symbol_kind::S_72_float_raw: // float.raw
        value.move<FloatLitIndex>(YY_MOVE(s.value));
        break;

    case symbol_kind::S_float: // float
        value.move<FloatProducingIndex>(YY_MOVE(s.value));
        break;

    case symbol_kind::S_integer: // integer
        value.move<IntLitIndex>(YY_MOVE(s.value));
        break;

    case symbol_kind::S_commandLine: // commandLine
    case symbol_kind::S_basicBinOp: // basicBinOp
        value.move<MessageIndex>(YY_MOVE(s.value));
        break;

    case symbol_kind::S_nil: // nil
        value.move<NilLitIndex>(YY_MOVE(s.value));
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
        value.move<NoValue>(YY_MOVE(s.value));
        break;

    case symbol_kind::S_64_binary_op_raw: // binary_op.raw
    case symbol_kind::S_binary_op: // binary_op
        value.move<SelectorIndex>(YY_MOVE(s.value));
        break;

    case symbol_kind::S_string: // string
        value.move<StringLitIndex>(YY_MOVE(s.value));
        break;

    case symbol_kind::S_symbol: // symbol
        value.move<SymbolLitIndex>(YY_MOVE(s.value));
        break;

    default:
        break;
    }

    location = YY_MOVE(s.location);
}

// by_kind.
parser::by_kind::by_kind() YY_NOEXCEPT : kind_(symbol_kind::S_YYEMPTY) {}

#if 201103L <= YY_CPLUSPLUS
parser::by_kind::by_kind(by_kind&& that) YY_NOEXCEPT : kind_(that.kind_) { that.clear(); }
#endif

parser::by_kind::by_kind(const by_kind& that) YY_NOEXCEPT : kind_(that.kind_) {}

parser::by_kind::by_kind(token_kind_type t) YY_NOEXCEPT : kind_(yytranslate_(t)) {}


void parser::by_kind::clear() YY_NOEXCEPT { kind_ = symbol_kind::S_YYEMPTY; }

void parser::by_kind::move(by_kind& that) {
    kind_ = that.kind_;
    that.clear();
}

parser::symbol_kind_type parser::by_kind::kind() const YY_NOEXCEPT { return kind_; }


parser::symbol_kind_type parser::by_kind::type_get() const YY_NOEXCEPT { return this->kind(); }


// by_state.
parser::by_state::by_state() YY_NOEXCEPT : state(empty_state) {}

parser::by_state::by_state(const by_state& that) YY_NOEXCEPT : state(that.state) {}

void parser::by_state::clear() YY_NOEXCEPT { state = empty_state; }

void parser::by_state::move(by_state& that) {
    state = that.state;
    that.clear();
}

parser::by_state::by_state(state_type s) YY_NOEXCEPT : state(s) {}

parser::symbol_kind_type parser::by_state::kind() const YY_NOEXCEPT {
    if (state == empty_state)
        return symbol_kind::S_YYEMPTY;
    else
        return YY_CAST(symbol_kind_type, yystos_[+state]);
}

parser::stack_symbol_type::stack_symbol_type() {}

parser::stack_symbol_type::stack_symbol_type(YY_RVREF(stack_symbol_type) that):
    super_type(YY_MOVE(that.state), YY_MOVE(that.location)) {
    switch (that.kind()) {
    case symbol_kind::S_73_accidental_unsigned: // accidental.unsigned
    case symbol_kind::S_accidental: // accidental
        value.YY_MOVE_OR_COPY<AccidentalLitIndex>(YY_MOVE(that.value));
        break;

    case symbol_kind::S_literal: // literal
        value.YY_MOVE_OR_COPY<AnyLiteralIndex>(YY_MOVE(that.value));
        break;

    case symbol_kind::S_boolean: // boolean
        value.YY_MOVE_OR_COPY<BooleanLitIndex>(YY_MOVE(that.value));
        break;

    case symbol_kind::S_71_float_raw_unsigned: // float.raw_unsigned
    case symbol_kind::S_72_float_raw: // float.raw
        value.YY_MOVE_OR_COPY<FloatLitIndex>(YY_MOVE(that.value));
        break;

    case symbol_kind::S_float: // float
        value.YY_MOVE_OR_COPY<FloatProducingIndex>(YY_MOVE(that.value));
        break;

    case symbol_kind::S_integer: // integer
        value.YY_MOVE_OR_COPY<IntLitIndex>(YY_MOVE(that.value));
        break;

    case symbol_kind::S_commandLine: // commandLine
    case symbol_kind::S_basicBinOp: // basicBinOp
        value.YY_MOVE_OR_COPY<MessageIndex>(YY_MOVE(that.value));
        break;

    case symbol_kind::S_nil: // nil
        value.YY_MOVE_OR_COPY<NilLitIndex>(YY_MOVE(that.value));
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
        value.YY_MOVE_OR_COPY<NoValue>(YY_MOVE(that.value));
        break;

    case symbol_kind::S_64_binary_op_raw: // binary_op.raw
    case symbol_kind::S_binary_op: // binary_op
        value.YY_MOVE_OR_COPY<SelectorIndex>(YY_MOVE(that.value));
        break;

    case symbol_kind::S_string: // string
        value.YY_MOVE_OR_COPY<StringLitIndex>(YY_MOVE(that.value));
        break;

    case symbol_kind::S_symbol: // symbol
        value.YY_MOVE_OR_COPY<SymbolLitIndex>(YY_MOVE(that.value));
        break;

    default:
        break;
    }

#if 201103L <= YY_CPLUSPLUS
    // that is emptied.
    that.state = empty_state;
#endif
}

parser::stack_symbol_type::stack_symbol_type(state_type s, YY_MOVE_REF(symbol_type) that):
    super_type(s, YY_MOVE(that.location)) {
    switch (that.kind()) {
    case symbol_kind::S_73_accidental_unsigned: // accidental.unsigned
    case symbol_kind::S_accidental: // accidental
        value.move<AccidentalLitIndex>(YY_MOVE(that.value));
        break;

    case symbol_kind::S_literal: // literal
        value.move<AnyLiteralIndex>(YY_MOVE(that.value));
        break;

    case symbol_kind::S_boolean: // boolean
        value.move<BooleanLitIndex>(YY_MOVE(that.value));
        break;

    case symbol_kind::S_71_float_raw_unsigned: // float.raw_unsigned
    case symbol_kind::S_72_float_raw: // float.raw
        value.move<FloatLitIndex>(YY_MOVE(that.value));
        break;

    case symbol_kind::S_float: // float
        value.move<FloatProducingIndex>(YY_MOVE(that.value));
        break;

    case symbol_kind::S_integer: // integer
        value.move<IntLitIndex>(YY_MOVE(that.value));
        break;

    case symbol_kind::S_commandLine: // commandLine
    case symbol_kind::S_basicBinOp: // basicBinOp
        value.move<MessageIndex>(YY_MOVE(that.value));
        break;

    case symbol_kind::S_nil: // nil
        value.move<NilLitIndex>(YY_MOVE(that.value));
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
        value.move<NoValue>(YY_MOVE(that.value));
        break;

    case symbol_kind::S_64_binary_op_raw: // binary_op.raw
    case symbol_kind::S_binary_op: // binary_op
        value.move<SelectorIndex>(YY_MOVE(that.value));
        break;

    case symbol_kind::S_string: // string
        value.move<StringLitIndex>(YY_MOVE(that.value));
        break;

    case symbol_kind::S_symbol: // symbol
        value.move<SymbolLitIndex>(YY_MOVE(that.value));
        break;

    default:
        break;
    }

    // that is emptied.
    that.kind_ = symbol_kind::S_YYEMPTY;
}

#if YY_CPLUSPLUS < 201103L
parser::stack_symbol_type& parser::stack_symbol_type::operator=(const stack_symbol_type& that) {
    state = that.state;
    switch (that.kind()) {
    case symbol_kind::S_73_accidental_unsigned: // accidental.unsigned
    case symbol_kind::S_accidental: // accidental
        value.copy<AccidentalLitIndex>(that.value);
        break;

    case symbol_kind::S_literal: // literal
        value.copy<AnyLiteralIndex>(that.value);
        break;

    case symbol_kind::S_boolean: // boolean
        value.copy<BooleanLitIndex>(that.value);
        break;

    case symbol_kind::S_71_float_raw_unsigned: // float.raw_unsigned
    case symbol_kind::S_72_float_raw: // float.raw
        value.copy<FloatLitIndex>(that.value);
        break;

    case symbol_kind::S_float: // float
        value.copy<FloatProducingIndex>(that.value);
        break;

    case symbol_kind::S_integer: // integer
        value.copy<IntLitIndex>(that.value);
        break;

    case symbol_kind::S_commandLine: // commandLine
    case symbol_kind::S_basicBinOp: // basicBinOp
        value.copy<MessageIndex>(that.value);
        break;

    case symbol_kind::S_nil: // nil
        value.copy<NilLitIndex>(that.value);
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
        value.copy<NoValue>(that.value);
        break;

    case symbol_kind::S_64_binary_op_raw: // binary_op.raw
    case symbol_kind::S_binary_op: // binary_op
        value.copy<SelectorIndex>(that.value);
        break;

    case symbol_kind::S_string: // string
        value.copy<StringLitIndex>(that.value);
        break;

    case symbol_kind::S_symbol: // symbol
        value.copy<SymbolLitIndex>(that.value);
        break;

    default:
        break;
    }

    location = that.location;
    return *this;
}

parser::stack_symbol_type& parser::stack_symbol_type::operator=(stack_symbol_type& that) {
    state = that.state;
    switch (that.kind()) {
    case symbol_kind::S_73_accidental_unsigned: // accidental.unsigned
    case symbol_kind::S_accidental: // accidental
        value.move<AccidentalLitIndex>(that.value);
        break;

    case symbol_kind::S_literal: // literal
        value.move<AnyLiteralIndex>(that.value);
        break;

    case symbol_kind::S_boolean: // boolean
        value.move<BooleanLitIndex>(that.value);
        break;

    case symbol_kind::S_71_float_raw_unsigned: // float.raw_unsigned
    case symbol_kind::S_72_float_raw: // float.raw
        value.move<FloatLitIndex>(that.value);
        break;

    case symbol_kind::S_float: // float
        value.move<FloatProducingIndex>(that.value);
        break;

    case symbol_kind::S_integer: // integer
        value.move<IntLitIndex>(that.value);
        break;

    case symbol_kind::S_commandLine: // commandLine
    case symbol_kind::S_basicBinOp: // basicBinOp
        value.move<MessageIndex>(that.value);
        break;

    case symbol_kind::S_nil: // nil
        value.move<NilLitIndex>(that.value);
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
        value.move<NoValue>(that.value);
        break;

    case symbol_kind::S_64_binary_op_raw: // binary_op.raw
    case symbol_kind::S_binary_op: // binary_op
        value.move<SelectorIndex>(that.value);
        break;

    case symbol_kind::S_string: // string
        value.move<StringLitIndex>(that.value);
        break;

    case symbol_kind::S_symbol: // symbol
        value.move<SymbolLitIndex>(that.value);
        break;

    default:
        break;
    }

    location = that.location;
    // that is emptied.
    that.state = empty_state;
    return *this;
}
#endif

template <typename Base> void parser::yy_destroy_(const char* yymsg, basic_symbol<Base>& yysym) const {
    if (yymsg)
        YY_SYMBOL_PRINT(yymsg, yysym);
}

#if YYDEBUG
template <typename Base> void parser::yy_print_(std::ostream& yyo, const basic_symbol<Base>& yysym) const {
    std::ostream& yyoutput = yyo;
    YY_USE(yyoutput);
    if (yysym.empty())
        yyo << "empty symbol";
    else {
        symbol_kind_type yykind = yysym.kind();
        yyo << (yykind < YYNTOKENS ? "token" : "nterm") << ' ' << yysym.name() << " (" << yysym.location << ": ";
        YY_USE(yykind);
        yyo << ')';
    }
}
#endif

void parser::yypush_(const char* m, YY_MOVE_REF(stack_symbol_type) sym) {
    if (m)
        YY_SYMBOL_PRINT(m, sym);
    yystack_.push(YY_MOVE(sym));
}

void parser::yypush_(const char* m, state_type s, YY_MOVE_REF(symbol_type) sym) {
#if 201103L <= YY_CPLUSPLUS
    yypush_(m, stack_symbol_type(s, std::move(sym)));
#else
    stack_symbol_type ss(s, sym);
    yypush_(m, ss);
#endif
}

void parser::yypop_(int n) YY_NOEXCEPT { yystack_.pop(n); }

#if YYDEBUG
std::ostream& parser::debug_stream() const { return *yycdebug_; }

void parser::set_debug_stream(std::ostream& o) { yycdebug_ = &o; }


parser::debug_level_type parser::debug_level() const { return yydebug_; }

void parser::set_debug_level(debug_level_type l) { yydebug_ = l; }
#endif // YYDEBUG

parser::state_type parser::yy_lr_goto_state_(state_type yystate, int yysym) {
    int yyr = yypgoto_[yysym - YYNTOKENS] + yystate;
    if (0 <= yyr && yyr <= yylast_ && yycheck_[yyr] == yystate)
        return yytable_[yyr];
    else
        return yydefgoto_[yysym - YYNTOKENS];
}

bool parser::yy_pact_value_is_default_(int yyvalue) YY_NOEXCEPT { return yyvalue == yypact_ninf_; }

bool parser::yy_table_value_is_error_(int yyvalue) YY_NOEXCEPT { return yyvalue == yytable_ninf_; }

int parser::operator()() { return parse(); }

int parser::parse() {
    int yyn;
    /// Length of the RHS of the rule being reduced.
    int yylen = 0;

    // Error handling.
    int yynerrs_ = 0;
    int yyerrstatus_ = 0;

    /// The lookahead symbol.
    symbol_type yyla;

    /// The locations where the error started and ended.
    stack_symbol_type yyerror_range[3];

    /// The return value of parse ().
    int yyresult;

#if YY_EXCEPTIONS
    try
#endif // YY_EXCEPTIONS
    {
        YYCDEBUG << "Starting parse\n";


        /* Initialize the stack.  The initial state will be set in
           yynewstate, since the latter expects the semantical and the
           location values to have been already stored, initialize these
           stacks with a primary value.  */
        yystack_.clear();
        yypush_(YY_NULLPTR, 0, YY_MOVE(yyla));

    /*-----------------------------------------------.
    | yynewstate -- push a new symbol on the stack.  |
    `-----------------------------------------------*/
    yynewstate:
        YYCDEBUG << "Entering state " << int(yystack_[0].state) << '\n';
        YY_STACK_PRINT();

        // Accept?
        if (yystack_[0].state == yyfinal_)
            YYACCEPT;

        goto yybackup;


    /*-----------.
    | yybackup.  |
    `-----------*/
    yybackup:
        // Try to take a decision without lookahead.
        yyn = yypact_[+yystack_[0].state];
        if (yy_pact_value_is_default_(yyn))
            goto yydefault;

        // Read a lookahead token.
        if (yyla.empty()) {
            YYCDEBUG << "Reading a token\n";
#if YY_EXCEPTIONS
            try
#endif // YY_EXCEPTIONS
            {
                yyla.kind_ = yytranslate_(yylex(&yyla.value, &yyla.location, cxt));
            }
#if YY_EXCEPTIONS
            catch (const syntax_error& yyexc) {
                YYCDEBUG << "Caught exception: " << yyexc.what() << '\n';
                error(yyexc);
                goto yyerrlab1;
            }
#endif // YY_EXCEPTIONS
        }
        YY_SYMBOL_PRINT("Next token is", yyla);

        if (yyla.kind() == symbol_kind::S_YYerror) {
            // The scanner already issued an error message, process directly
            // to error recovery.  But do not keep the error token as
            // lookahead, it is too special and may lead us to an endless
            // loop in error recovery. */
            yyla.kind_ = symbol_kind::S_YYUNDEF;
            goto yyerrlab1;
        }

        /* If the proper action on seeing token YYLA.TYPE is to reduce or
           to detect an error, take that action.  */
        yyn += yyla.kind();
        if (yyn < 0 || yylast_ < yyn || yycheck_[yyn] != yyla.kind()) {
            goto yydefault;
        }

        // Reduce or error.
        yyn = yytable_[yyn];
        if (yyn <= 0) {
            if (yy_table_value_is_error_(yyn))
                goto yyerrlab;
            yyn = -yyn;
            goto yyreduce;
        }

        // Count tokens shifted since error; after three, turn off error status.
        if (yyerrstatus_)
            --yyerrstatus_;

        // Shift the lookahead token.
        yypush_("Shifting", state_type(yyn), YY_MOVE(yyla));
        goto yynewstate;


    /*-----------------------------------------------------------.
    | yydefault -- do the default action for the current state.  |
    `-----------------------------------------------------------*/
    yydefault:
        yyn = yydefact_[+yystack_[0].state];
        if (yyn == 0)
            goto yyerrlab;
        goto yyreduce;


    /*-----------------------------.
    | yyreduce -- do a reduction.  |
    `-----------------------------*/
    yyreduce:
        yylen = yyr2_[yyn];
        {
            stack_symbol_type yylhs;
            yylhs.state = yy_lr_goto_state_(yystack_[yylen].state, yyr1_[yyn]);
            /* Variants are always initialized to an empty instance of the
               correct type. The default '$$ = $1' action is NOT applied
               when using variants.  */
            switch (yyr1_[yyn]) {
            case symbol_kind::S_73_accidental_unsigned: // accidental.unsigned
            case symbol_kind::S_accidental: // accidental
                yylhs.value.emplace<AccidentalLitIndex>();
                break;

            case symbol_kind::S_literal: // literal
                yylhs.value.emplace<AnyLiteralIndex>();
                break;

            case symbol_kind::S_boolean: // boolean
                yylhs.value.emplace<BooleanLitIndex>();
                break;

            case symbol_kind::S_71_float_raw_unsigned: // float.raw_unsigned
            case symbol_kind::S_72_float_raw: // float.raw
                yylhs.value.emplace<FloatLitIndex>();
                break;

            case symbol_kind::S_float: // float
                yylhs.value.emplace<FloatProducingIndex>();
                break;

            case symbol_kind::S_integer: // integer
                yylhs.value.emplace<IntLitIndex>();
                break;

            case symbol_kind::S_commandLine: // commandLine
            case symbol_kind::S_basicBinOp: // basicBinOp
                yylhs.value.emplace<MessageIndex>();
                break;

            case symbol_kind::S_nil: // nil
                yylhs.value.emplace<NilLitIndex>();
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
                yylhs.value.emplace<NoValue>();
                break;

            case symbol_kind::S_64_binary_op_raw: // binary_op.raw
            case symbol_kind::S_binary_op: // binary_op
                yylhs.value.emplace<SelectorIndex>();
                break;

            case symbol_kind::S_string: // string
                yylhs.value.emplace<StringLitIndex>();
                break;

            case symbol_kind::S_symbol: // symbol
                yylhs.value.emplace<SymbolLitIndex>();
                break;

            default:
                break;
            }


            // Default location.
            {
                stack_type::slice range(yystack_, yylen);
                YYLLOC_DEFAULT(yylhs.location, range, yylen);
                yyerror_range[1].location = yylhs.location;
            }

            // Perform the reduction.
            YY_REDUCE_PRINT(yyn);
#if YY_EXCEPTIONS
            try
#endif // YY_EXCEPTIONS
            {
                switch (yyn) {
                case 2: // commandLine: INTERPRET basicBinOp
#line 120 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<MessageIndex>() = yystack_[0].value.as<MessageIndex>();
                }
#line 1403 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 3: // basicBinOp: literal binary_op literal
#line 123 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<MessageIndex>() = cxt.create(MessageNode {}, yylhs.location,
                                                                yystack_[2].value.as<AnyLiteralIndex>(), // receiver
                                                                yystack_[1].value.as<SelectorIndex>(), // selector
                                                                yystack_[0].value.as<AnyLiteralIndex>());
                }
#line 1417 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 4: // literal: symbol
#line 134 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<AnyLiteralIndex>() = yystack_[0].value.as<SymbolLitIndex>();
                }
#line 1423 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 5: // literal: string
#line 135 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<AnyLiteralIndex>() = yystack_[0].value.as<StringLitIndex>();
                }
#line 1429 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 6: // literal: integer
#line 136 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<AnyLiteralIndex>() = yystack_[0].value.as<IntLitIndex>();
                }
#line 1435 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 7: // literal: float
#line 137 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<AnyLiteralIndex>() = yystack_[0].value.as<FloatProducingIndex>();
                }
#line 1441 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 8: // literal: boolean
#line 138 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<AnyLiteralIndex>() = yystack_[0].value.as<BooleanLitIndex>();
                }
#line 1447 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 9: // literal: nil
#line 139 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<AnyLiteralIndex>() = yystack_[0].value.as<NilLitIndex>();
                }
#line 1453 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 10: // binary_op.raw: binary_op
#line 147 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<SelectorIndex>() = cxt.create(SelectorNode { false }, yylhs.location);
                }
#line 1459 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 11: // binary_op.raw: READWRITEVAR
#line 148 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<SelectorIndex>() = cxt.create(SelectorNode { false }, yylhs.location);
                }
#line 1465 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 12: // binary_op.raw: LESSTHAN
#line 149 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<SelectorIndex>() = cxt.create(SelectorNode { false }, yylhs.location);
                }
#line 1471 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 13: // binary_op.raw: GREATERTHAN
#line 150 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<SelectorIndex>() = cxt.create(SelectorNode { false }, yylhs.location);
                }
#line 1477 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 14: // binary_op.raw: MINUS
#line 151 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<SelectorIndex>() = cxt.create(SelectorNode { false }, yylhs.location);
                }
#line 1483 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 15: // binary_op.raw: MULTIPLY
#line 152 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<SelectorIndex>() = cxt.create(SelectorNode { false }, yylhs.location);
                }
#line 1489 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 16: // binary_op.raw: ADD
#line 153 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<SelectorIndex>() = cxt.create(SelectorNode { false }, yylhs.location);
                }
#line 1495 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 17: // binary_op.raw: PIPE
#line 154 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<SelectorIndex>() = cxt.create(SelectorNode { false }, yylhs.location);
                }
#line 1501 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 18: // binary_op: binary_op.raw
#line 157 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<SelectorIndex>() = yystack_[0].value.as<SelectorIndex>();
                }
#line 1507 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 19: // binary_op: KEYBINOP
#line 158 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<SelectorIndex>() = cxt.create(SelectorNode { true }, yylhs.location);
                }
#line 1513 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 20: // nil: NILOBJ
#line 161 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<NilLitIndex>() = cxt.create(NilNode {}, yylhs.location);
                }
#line 1519 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 21: // boolean: TRUEOBJ
#line 163 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<BooleanLitIndex>() = cxt.create(BooleanNode { true }, yylhs.location);
                }
#line 1525 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 22: // boolean: FALSEOBJ
#line 164 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<BooleanLitIndex>() = cxt.create(BooleanNode { false }, yylhs.location);
                }
#line 1531 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 23: // symbol: SYMBOL_QUOTE
#line 167 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<SymbolLitIndex>() =
                        cxt.create(SymbolNode { SymbolNode::Kind::Quote }, yylhs.location);
                }
#line 1537 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 24: // symbol: SYMBOL_SLASH
#line 168 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<SymbolLitIndex>() =
                        cxt.create(SymbolNode { SymbolNode::Kind::Slash }, yylhs.location);
                }
#line 1543 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 25: // string: STRINGLINE
#line 172 "langutils/sc_parser/src/sc_grammar.y"
                {
                    auto line = cxt.create(StringLineNode {}, yylhs.location);
                    auto list = cxt.create(StringLineList {}, yylhs.location);
                    yylhs.value.as<StringLitIndex>() = cxt.graph.append_to_list(list, line);
                }
#line 1553 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 26: // string: string STRINGLINE
#line 178 "langutils/sc_parser/src/sc_grammar.y"
                {
                    auto list = yystack_[1].value.as<StringLitIndex>();
                    auto line = cxt.create(StringLineNode {}, yystack_[0].location);
                    yylhs.value.as<StringLitIndex>() = cxt.graph.append_to_list(list, line);
                }
#line 1563 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 27: // integer: INTEGER
#line 185 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<IntLitIndex>() = cxt.create(IntNode {}, yylhs.location);
                }
#line 1569 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 28: // integer: INTEGER_RADIX
#line 186 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<IntLitIndex>() = cxt.create(IntNode { IntNode::Kind::Radix }, yylhs.location);
                }
#line 1575 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 29: // integer: HEXADECIMAL
#line 187 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<IntLitIndex>() = cxt.create(IntNode { IntNode::Kind::Hexadecimal }, yylhs.location);
                }
#line 1581 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 30: // integer: MINUS integer
#line 189 "langutils/sc_parser/src/sc_grammar.y"
                {
                    // Reaches into the previous integer and changes its sign.
                    cxt.graph.get_payload(yystack_[0].value.as<IntLitIndex>()).sign = IntNode::Sign::Negative;
                    yylhs.value.as<IntLitIndex>() = yystack_[0].value.as<IntLitIndex>();
                }
#line 1591 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 31: // float.raw_unsigned: FLOAT
#line 196 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<FloatLitIndex>() = cxt.create(FloatNode {}, yylhs.location);
                }
#line 1597 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 32: // float.raw_unsigned: FLOAT_RADIX
#line 197 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<FloatLitIndex>() = cxt.create(FloatNode { FloatNode::Kind::Radix }, yylhs.location);
                }
#line 1603 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 33: // float.raw_unsigned: FLOAT_EXPONENT
#line 198 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<FloatLitIndex>() =
                        cxt.create(FloatNode { FloatNode::Kind::Exponent }, yylhs.location);
                }
#line 1609 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 34: // float.raw_unsigned: FLOAT_INF
#line 199 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<FloatLitIndex>() = cxt.create(FloatNode { FloatNode::Kind::Inf }, yylhs.location);
                }
#line 1615 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 35: // float.raw: float.raw_unsigned
#line 202 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<FloatLitIndex>() = yystack_[0].value.as<FloatLitIndex>();
                }
#line 1621 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 36: // float.raw: MINUS float.raw_unsigned
#line 204 "langutils/sc_parser/src/sc_grammar.y"
                {
                    cxt.graph.get_payload(yystack_[0].value.as<FloatLitIndex>()).sign = FloatNode::Sign::Negative;
                    yylhs.value.as<FloatLitIndex>() = yystack_[0].value.as<FloatLitIndex>();
                }
#line 1630 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 37: // accidental.unsigned: ACCIDENTAL_STEPS
#line 210 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<AccidentalLitIndex>() =
                        cxt.create(AccidentalNode { AccidentalNode::Kind::Steps }, yylhs.location);
                }
#line 1636 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 38: // accidental.unsigned: ACCIDENTAL_CENTS
#line 211 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<AccidentalLitIndex>() =
                        cxt.create(AccidentalNode { AccidentalNode::Kind::Cents }, yylhs.location);
                }
#line 1642 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 39: // accidental: accidental.unsigned
#line 214 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<AccidentalLitIndex>() = yystack_[0].value.as<AccidentalLitIndex>();
                }
#line 1648 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 40: // accidental: MINUS accidental.unsigned
#line 216 "langutils/sc_parser/src/sc_grammar.y"
                {
                    cxt.graph.get_payload(yystack_[0].value.as<AccidentalLitIndex>()).sign =
                        AccidentalNode::Sign::Negative;
                    yylhs.value.as<AccidentalLitIndex>() = yystack_[0].value.as<AccidentalLitIndex>();
                }
#line 1657 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 41: // float: float.raw
#line 222 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<FloatProducingIndex>() = yystack_[0].value.as<FloatLitIndex>();
                }
#line 1663 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 42: // float: accidental
#line 223 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<FloatProducingIndex>() = yystack_[0].value.as<AccidentalLitIndex>();
                }
#line 1669 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 43: // float: float.raw PI
#line 224 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<FloatProducingIndex>() =
                        cxt.create(PiNode {}, yylhs.location, yystack_[1].value.as<FloatLitIndex>());
                }
#line 1675 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 44: // float: integer PI
#line 225 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<FloatProducingIndex>() =
                        cxt.create(PiNode {}, yylhs.location, yystack_[1].value.as<IntLitIndex>());
                }
#line 1681 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 45: // float: PI
#line 226 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<FloatProducingIndex>() =
                        cxt.create(PiNode {}, yylhs.location, cxt.create(Missing {}, yylhs.location));
                }
#line 1687 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 46: // float: MINUS PI
#line 227 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<FloatProducingIndex>() = cxt.create(
                        PiNode { PiNode::Sign::Negative }, yylhs.location, cxt.create(Missing {}, yylhs.location));
                }
#line 1693 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;


#line 1697 "langutils/sc_parser/src/sc_grammar_parser.cpp"

                default:
                    break;
                }
            }
#if YY_EXCEPTIONS
            catch (const syntax_error& yyexc) {
                YYCDEBUG << "Caught exception: " << yyexc.what() << '\n';
                error(yyexc);
                YYERROR;
            }
#endif // YY_EXCEPTIONS
            YY_SYMBOL_PRINT("-> $$ =", yylhs);
            yypop_(yylen);
            yylen = 0;

            // Shift the result of the reduction.
            yypush_(YY_NULLPTR, YY_MOVE(yylhs));
        }
        goto yynewstate;


    /*--------------------------------------.
    | yyerrlab -- here on detecting error.  |
    `--------------------------------------*/
    yyerrlab:
        // If not already recovering from an error, report this error.
        if (!yyerrstatus_) {
            ++yynerrs_;
            context yyctx(*this, yyla);
            report_syntax_error(yyctx);
        }


        yyerror_range[1].location = yyla.location;
        if (yyerrstatus_ == 3) {
            /* If just tried and failed to reuse lookahead token after an
               error, discard it.  */

            // Return failure if at end of input.
            if (yyla.kind() == symbol_kind::S_YYEOF)
                YYABORT;
            else if (!yyla.empty()) {
                yy_destroy_("Error: discarding", yyla);
                yyla.clear();
            }
        }

        // Else will try to reuse lookahead token after shifting the error token.
        goto yyerrlab1;


    /*---------------------------------------------------.
    | yyerrorlab -- error raised explicitly by YYERROR.  |
    `---------------------------------------------------*/
    yyerrorlab:
        /* Pacify compilers when the user code never invokes YYERROR and
           the label yyerrorlab therefore never appears in user code.  */
        if (false)
            YYERROR;

        /* Do not reclaim the symbols of the rule whose action triggered
           this YYERROR.  */
        yypop_(yylen);
        yylen = 0;
        YY_STACK_PRINT();
        goto yyerrlab1;


    /*-------------------------------------------------------------.
    | yyerrlab1 -- common code for both syntax error and YYERROR.  |
    `-------------------------------------------------------------*/
    yyerrlab1:
        yyerrstatus_ = 3; // Each real token shifted decrements this.
        // Pop stack until we find a state that shifts the error token.
        for (;;) {
            yyn = yypact_[+yystack_[0].state];
            if (!yy_pact_value_is_default_(yyn)) {
                yyn += symbol_kind::S_YYerror;
                if (0 <= yyn && yyn <= yylast_ && yycheck_[yyn] == symbol_kind::S_YYerror) {
                    yyn = yytable_[yyn];
                    if (0 < yyn)
                        break;
                }
            }

            // Pop the current state because it cannot handle the error token.
            if (yystack_.size() == 1)
                YYABORT;

            yyerror_range[1].location = yystack_[0].location;
            yy_destroy_("Error: popping", yystack_[0]);
            yypop_();
            YY_STACK_PRINT();
        }
        {
            stack_symbol_type error_token;

            yyerror_range[2].location = yyla.location;
            YYLLOC_DEFAULT(error_token.location, yyerror_range, 2);

            // Shift the error token.
            error_token.state = state_type(yyn);
            yypush_("Shifting", YY_MOVE(error_token));
        }
        goto yynewstate;


    /*-------------------------------------.
    | yyacceptlab -- YYACCEPT comes here.  |
    `-------------------------------------*/
    yyacceptlab:
        yyresult = 0;
        goto yyreturn;


    /*-----------------------------------.
    | yyabortlab -- YYABORT comes here.  |
    `-----------------------------------*/
    yyabortlab:
        yyresult = 1;
        goto yyreturn;


    /*-----------------------------------------------------.
    | yyreturn -- parsing is finished, return the result.  |
    `-----------------------------------------------------*/
    yyreturn:
        if (!yyla.empty())
            yy_destroy_("Cleanup: discarding lookahead", yyla);

        /* Do not reclaim the symbols of the rule whose action triggered
           this YYABORT or YYACCEPT.  */
        yypop_(yylen);
        YY_STACK_PRINT();
        while (1 < yystack_.size()) {
            yy_destroy_("Cleanup: popping", yystack_[0]);
            yypop_();
        }

        return yyresult;
    }
#if YY_EXCEPTIONS
    catch (...) {
        YYCDEBUG << "Exception caught: cleaning lookahead and stack\n";
        // Do not try to display the values of the reclaimed symbols,
        // as their printers might throw an exception.
        if (!yyla.empty())
            yy_destroy_(YY_NULLPTR, yyla);

        while (1 < yystack_.size()) {
            yy_destroy_(YY_NULLPTR, yystack_[0]);
            yypop_();
        }
        throw;
    }
#endif // YY_EXCEPTIONS
}

void parser::error(const syntax_error& yyexc) { error(yyexc.location, yyexc.what()); }

const char* parser::symbol_name(symbol_kind_type yysymbol) {
    static const char* const yy_sname[] = { "end of file",
                                            "error",
                                            "invalid token",
                                            "OPENCURLY",
                                            "CLOSECURLY",
                                            "OPENSQUARE",
                                            "CLOSESQUARE",
                                            "OPENPAREN",
                                            "CLOSEPAREN",
                                            "SEMICOLON",
                                            "NONLOCALRETURN",
                                            "COMMA",
                                            "HASH",
                                            "TILDE",
                                            "NAME",
                                            "INTEGER",
                                            "INTEGER_RADIX",
                                            "HEXADECIMAL",
                                            "FLOAT",
                                            "FLOAT_RADIX",
                                            "FLOAT_EXPONENT",
                                            "FLOAT_INF",
                                            "ACCIDENTAL_STEPS",
                                            "ACCIDENTAL_CENTS",
                                            "SYMBOL_QUOTE",
                                            "SYMBOL_SLASH",
                                            "STRINGLINE",
                                            "ASCII",
                                            "PRIMITIVENAME",
                                            "CLASSNAME",
                                            "CURRYARG",
                                            "VAR",
                                            "ARG",
                                            "CLASSVAR",
                                            "SC_CONST",
                                            "NILOBJ",
                                            "TRUEOBJ",
                                            "FALSEOBJ",
                                            "PI",
                                            "ELLIPSIS",
                                            "DOTDOT",
                                            "BEGINCLOSEDFUNC",
                                            "BADTOKEN",
                                            "INTERPRET",
                                            "LEFTARROW",
                                            "WHILE",
                                            "COLON",
                                            "EQUALSSIGN",
                                            "BINOP",
                                            "KEYBINOP",
                                            "MINUS",
                                            "LESSTHAN",
                                            "GREATERTHAN",
                                            "MULTIPLY",
                                            "ADD",
                                            "PIPE",
                                            "READWRITEVAR",
                                            "DOT",
                                            "BACKTICK",
                                            "UMINUS",
                                            "$accept",
                                            "commandLine",
                                            "basicBinOp",
                                            "literal",
                                            "binary_op.raw",
                                            "binary_op",
                                            "nil",
                                            "boolean",
                                            "symbol",
                                            "string",
                                            "integer",
                                            "float.raw_unsigned",
                                            "float.raw",
                                            "accidental.unsigned",
                                            "accidental",
                                            "float",
                                            YY_NULLPTR };
    return yy_sname[yysymbol];
}


// parser::context.
parser::context::context(const parser& yyparser, const symbol_type& yyla): yyparser_(yyparser), yyla_(yyla) {}

int parser::context::expected_tokens(symbol_kind_type yyarg[], int yyargn) const {
    // Actual number of expected tokens
    int yycount = 0;

    const int yyn = yypact_[+yyparser_.yystack_[0].state];
    if (!yy_pact_value_is_default_(yyn)) {
        /* Start YYX at -YYN if negative to avoid negative indexes in
           YYCHECK.  In other words, skip the first -YYN actions for
           this state because they are default actions.  */
        const int yyxbegin = yyn < 0 ? -yyn : 0;
        // Stay within bounds of both yycheck and yytname.
        const int yychecklim = yylast_ - yyn + 1;
        const int yyxend = yychecklim < YYNTOKENS ? yychecklim : YYNTOKENS;
        for (int yyx = yyxbegin; yyx < yyxend; ++yyx)
            if (yycheck_[yyx + yyn] == yyx && yyx != symbol_kind::S_YYerror
                && !yy_table_value_is_error_(yytable_[yyx + yyn])) {
                if (!yyarg)
                    ++yycount;
                else if (yycount == yyargn)
                    return 0;
                else
                    yyarg[yycount++] = YY_CAST(symbol_kind_type, yyx);
            }
    }

    if (yyarg && yycount == 0 && 0 < yyargn)
        yyarg[0] = symbol_kind::S_YYEMPTY;
    return yycount;
}


const signed char parser::yypact_ninf_ = -32;

const signed char parser::yytable_ninf_ = -1;

const signed char parser::yypact_[] = { -31, -15, 16,  -32, -32, -32, -32, -32, -32, -32, -32, -32, -32,
                                        -32, -32, -32, -32, -32, -32, 9,   -32, -13, -32, -32, -32, -9,
                                        -19, -32, -4,  -32, -32, -32, -32, -32, -2,  -32, -32, -32, -32,
                                        -32, -32, -32, -32, -32, -32, -32, -32, -15, -32, -32, -32, -32 };

const signed char parser::yydefact_[] = { 0,  0,  0,  27, 28, 29, 31, 32, 33, 34, 37, 38, 23, 24, 25, 20, 21, 22,
                                          45, 0,  2,  0,  9,  8,  4,  5,  6,  35, 41, 39, 42, 7,  1,  46, 0,  30,
                                          36, 40, 19, 14, 12, 13, 15, 16, 17, 11, 18, 0,  26, 44, 43, 3 };

const signed char parser::yypgoto_[] = { -32, -32, -32, -3, -32, -32, -32, -32, -32, -32, -1, 26, -32, 27, -32, -32 };

const signed char parser::yydefgoto_[] = { 0, 2, 20, 21, 46, 47, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31 };

const signed char parser::yytable_[] = {
    3, 4,  5,  6,  7,  8,  9,  10, 11, 12, 13, 14, 1,  3,  4,  5,  32, 48, 35, 49, 15, 16, 17, 18, 3, 4, 5, 6, 7, 8,
    9, 10, 11, 35, 50, 19, 38, 39, 40, 41, 42, 43, 44, 45, 51, 36, 37, 33, 34, 0,  0,  0,  0,  0,  0, 0, 0, 0, 0, 34
};

const signed char parser::yycheck_[] = {
    15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 43, 15, 16, 17, 0,  26, 19, 38,
    35, 36, 37, 38, 15, 16, 17, 18, 19, 20, 21, 22, 23, 34, 38, 50, 49, 50, 51, 52,
    53, 54, 55, 56, 47, 19, 19, 38, 50, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 50
};

const signed char parser::yystos_[] = { 0,  43, 61, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 35, 36, 37,
                                        38, 50, 62, 63, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 0,  38, 50, 70,
                                        71, 73, 49, 50, 51, 52, 53, 54, 55, 56, 64, 65, 26, 38, 38, 63 };

const signed char parser::yyr1_[] = { 0,  60, 61, 62, 63, 63, 63, 63, 63, 63, 64, 64, 64, 64, 64, 64,
                                      64, 64, 65, 65, 66, 67, 67, 68, 68, 69, 69, 70, 70, 70, 70, 71,
                                      71, 71, 71, 72, 72, 73, 73, 74, 74, 75, 75, 75, 75, 75, 75 };

const signed char parser::yyr2_[] = { 0, 2, 2, 3, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
                                      1, 1, 2, 1, 1, 1, 2, 1, 1, 1, 1, 1, 2, 1, 1, 1, 2, 1, 1, 2, 2, 1, 2 };


#if YYDEBUG
const unsigned char parser::yyrline_[] = { 0,   120, 120, 122, 134, 135, 136, 137, 138, 139, 147, 148,
                                           149, 150, 151, 152, 153, 154, 157, 158, 161, 163, 164, 167,
                                           168, 171, 177, 185, 186, 187, 188, 196, 197, 198, 199, 202,
                                           203, 210, 211, 214, 215, 222, 223, 224, 225, 226, 227 };

void parser::yy_stack_print_() const {
    *yycdebug_ << "Stack now";
    for (stack_type::const_iterator i = yystack_.begin(), i_end = yystack_.end(); i != i_end; ++i)
        *yycdebug_ << ' ' << int(i->state);
    *yycdebug_ << '\n';
}

void parser::yy_reduce_print_(int yyrule) const {
    int yylno = yyrline_[yyrule];
    int yynrhs = yyr2_[yyrule];
    // Print the symbols being reduced, and their result.
    *yycdebug_ << "Reducing stack by rule " << yyrule - 1 << " (line " << yylno << "):\n";
    // The symbols being reduced.
    for (int yyi = 0; yyi < yynrhs; yyi++)
        YY_SYMBOL_PRINT("   $" << yyi + 1 << " =", yystack_[(yynrhs) - (yyi + 1)]);
}
#endif // YYDEBUG

parser::symbol_kind_type parser::yytranslate_(int t) YY_NOEXCEPT {
    // YYTRANSLATE[TOKEN-NUM] -- Symbol number corresponding to
    // TOKEN-NUM as returned by yylex.
    static const signed char translate_table[] = {
        0,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,
        2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,
        2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,
        2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,
        2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,
        2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,
        2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,
        2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,
        2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,
        2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  1,  2,  3,  4,  5,  6,  7,  8,  9,  10, 11, 12, 13, 14,
        15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41,
        42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59
    };
    // Last valid token kind.
    const int code_max = 314;

    if (t <= 0)
        return symbol_kind::S_YYEOF;
    else if (t <= code_max)
        return static_cast<symbol_kind_type>(translate_table[t]);
    else
        return symbol_kind::S_YYUNDEF;
}

#line 6 "langutils/sc_parser/src/sc_grammar.y"
}} // sc::parser
#line 2135 "langutils/sc_parser/src/sc_grammar_parser.cpp"

#line 236 "langutils/sc_parser/src/sc_grammar.y"


inline sc::parser::parser::token_kind_type to_parser_token(sc::lex::TokenType t) {
    using T = sc::parser::parser::token_kind_type;
    using TokenType = sc::lex::TokenType;
    switch (t) {
    case TokenType::EndOfFile:
        return T::TOKEN_YYEOF;
    case TokenType::Name:
        return T::TOKEN_NAME;
    case TokenType::ClassName:
        return T::TOKEN_CLASSNAME;
    case TokenType::PrimitiveName:
        return T::TOKEN_YYUNDEF;
    case TokenType::Integer:
        return T::TOKEN_INTEGER;
    case TokenType::IntegerRadix:
        return T::TOKEN_INTEGER_RADIX;
    case TokenType::Hexadecimal:
        return T::TOKEN_HEXADECIMAL;
    case TokenType::Float:
        return T::TOKEN_FLOAT;
    case TokenType::FloatRadix:
        return T::TOKEN_FLOAT_RADIX;
    case TokenType::FloatExponent:
        return T::TOKEN_FLOAT_EXPONENT;
    case TokenType::Pi:
        return T::TOKEN_PI;
    case TokenType::Inf:
        return T::TOKEN_FLOAT_INF;
    case TokenType::AccidentalSteps:
        return T::TOKEN_YYUNDEF;
    case TokenType::AccidentalCents:
        return T::TOKEN_YYUNDEF;
    case TokenType::SymbolSlash:
        return T::TOKEN_SYMBOL_SLASH;
    case TokenType::SymbolQuote:
        return T::TOKEN_SYMBOL_QUOTE;
    case TokenType::Ascii:
        return T::TOKEN_YYUNDEF;
    case TokenType::True:
        return T::TOKEN_YYUNDEF;
    case TokenType::False:
        return T::TOKEN_YYUNDEF;
    case TokenType::Nil:
        return T::TOKEN_YYUNDEF;
    case TokenType::StringLine:
        return T::TOKEN_YYUNDEF;
    case TokenType::While:
        return T::TOKEN_YYUNDEF;
    case TokenType::Var:
        return T::TOKEN_YYUNDEF;
    case TokenType::Arg:
        return T::TOKEN_YYUNDEF;
    case TokenType::ClassVar:
        return T::TOKEN_YYUNDEF;
    case TokenType::Const:
        return T::TOKEN_YYUNDEF;
    case TokenType::OpenParen:
        return T::TOKEN_YYUNDEF;
    case TokenType::OpenSquare:
        return T::TOKEN_YYUNDEF;
    case TokenType::OpenCurly:
        return T::TOKEN_YYUNDEF;
    case TokenType::BeginClosedFunction:
        return T::TOKEN_YYUNDEF;
    case TokenType::CloseParen:
        return T::TOKEN_YYUNDEF;
    case TokenType::CloseSquare:
        return T::TOKEN_YYUNDEF;
    case TokenType::CloseCurly:
        return T::TOKEN_YYUNDEF;
    case TokenType::SemiColon:
        return T::TOKEN_YYUNDEF;
    case TokenType::Colon:
        return T::TOKEN_YYUNDEF;
    case TokenType::Comma:
        return T::TOKEN_YYUNDEF;
    case TokenType::EqualsSign:
        return T::TOKEN_YYUNDEF;
    case TokenType::NonLocalReturn:
        return T::TOKEN_YYUNDEF;
    case TokenType::BackTick:
        return T::TOKEN_YYUNDEF;
    case TokenType::Tilde:
        return T::TOKEN_YYUNDEF;
    case TokenType::Hash:
        return T::TOKEN_YYUNDEF;
    case TokenType::LeftArrow:
        return T::TOKEN_YYUNDEF;
    case TokenType::Ellipsis:
        return T::TOKEN_YYUNDEF;
    case TokenType::Dot:
        return T::TOKEN_YYUNDEF;
    case TokenType::DotDot:
        return T::TOKEN_YYUNDEF;
    case TokenType::CurryArg:
        return T::TOKEN_YYUNDEF;
    case TokenType::Pipe:
        return T::TOKEN_YYUNDEF;
    case TokenType::ReadWriteVar:
        return T::TOKEN_YYUNDEF;
    case TokenType::Minus:
        return T::TOKEN_YYUNDEF;
    case TokenType::Multiply:
        return T::TOKEN_YYUNDEF;
    case TokenType::Add:
        return T::TOKEN_ADD;
    case TokenType::LessThan:
        return T::TOKEN_YYUNDEF;
    case TokenType::GreaterThan:
        return T::TOKEN_YYUNDEF;
    case TokenType::BinaryOperator:
        return T::TOKEN_YYUNDEF;
    case TokenType::KeywordBinaryOperator:
        return T::TOKEN_YYUNDEF;
    case TokenType::Space:
        return T::TOKEN_YYUNDEF;
    case TokenType::NewLine:
        return T::TOKEN_YYUNDEF;
    case TokenType::Tab:
        return T::TOKEN_YYUNDEF;
    case TokenType::Comment:
        return T::TOKEN_YYUNDEF;
    case TokenType::MultilineComment:
        return T::TOKEN_YYUNDEF;
    default:
        return T::TOKEN_YYUNDEF;
    }
}


static int yylex(sc::parser::parser::value_type* v, sc::lex::SourceCodeRange* loc, sc::parser::ParserContext& cxt) {
    using T = sc::parser::parser::token_kind_type;

    if (cxt.mode == sc::parser::ParserContext::Mode::CommandInitial) {
        cxt.mode = sc::parser::ParserContext::Mode::CommandContinue;
        v->emplace<sc::parser::NoValue>();
        *loc = {};
        return T::TOKEN_INTERPRET;
    }
    const auto [lex_token, location, extra_location] = sc::lex::lexer(cxt.cps, cxt.action);
    *loc = location;
    v->emplace<sc::parser::NoValue>();

    if (sc::lex::is_error(lex_token)) {
        cxt.state = sc::parser::ParserContext::State::Failure;
        cxt.error_handler->operator()(cxt.text_info, lex_token, location, extra_location);
        return T::TOKEN_YYerror;
    }

    return static_cast<int>(to_parser_token(lex_token));
}

void sc::parser::parser::report_syntax_error(const context& symbol_cxt) const {
    std::vector<symbol_kind_type> expected(5);
    expected.resize(symbol_cxt.expected_tokens(expected.data(), 5));

    std::vector<const char*> expected_names;
    expected_names.reserve(expected.size());
    for (const auto s : expected)
        expected_names.push_back(sc::parser::parser::symbol_name(s));

    const auto got = symbol_cxt.token();
    const char* got_name = parser::symbol_name(got);
    cxt.error_handler->operator()(cxt.text_info, std::move(expected_names), symbol_cxt.location(), got_name);
}

void sc::parser::parser::error(const sc::lex::SourceCodeRange& loc, const std::string& message) {
    cxt.error_handler->operator()(cxt.text_info, loc, message);
}
