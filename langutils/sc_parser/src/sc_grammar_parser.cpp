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
#line 31 "langutils/sc_parser/src/sc_grammar.y"


#include "sc_grammar_parser.hpp"
#include "indexes_typed.hpp"
#include "nodes.hpp"
#include "lexer.hpp"
#include "sc_grammar_impl.hpp"
#include "parser_context.hpp"

#include <iostream>

namespace sc::parser {
class parser;
} // forward declare the parser

// static int yylex(sc::parser::parser::value_type* v, sc::lex::SourceCodeRange* loc, sc::parser::ParserContext& cxt);

using namespace sc::parser::nodes;

template <typename... REJECTS>
auto create_error(sc::parser::ParserContext& cxt, sc::lex::SourceCodeRange loc, REJECTS... rejects) {
    const auto orphans = cxt.graph.orphans();
    auto er = cxt.create(Error {}, loc);
    for (auto o : orphans) {
        if (!((*o == *rejects) || ...))
            cxt.graph.append_to_list(er, sc::parser::AnyIndex { *o });
    }
    return er;
}


#line 69 "langutils/sc_parser/src/sc_grammar_parser.cpp"


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

#line 7 "langutils/sc_parser/src/sc_grammar.y"
namespace sc { namespace parser {
#line 169 "langutils/sc_parser/src/sc_grammar_parser.cpp"

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
    case symbol_kind::S_ascii: // ascii
        value.copy<ASCIIIndex>(YY_MOVE(that.value));
        break;

    case symbol_kind::S_126_accidental_unsigned: // accidental.unsigned
    case symbol_kind::S_accidental: // accidental
        value.copy<AccidentalLitIndex>(YY_MOVE(that.value));
        break;

    case symbol_kind::S_adverb: // adverb
        value.copy<AdverbIndex>(YY_MOVE(that.value));
        break;

    case symbol_kind::S_105_literal_terminal: // literal.terminal
    case symbol_kind::S_literal: // literal
        value.copy<AnyLiteralIndex>(YY_MOVE(that.value));
        break;

    case symbol_kind::S_method: // method
        value.copy<AnyMethodIndex>(YY_MOVE(that.value));
        break;

    case symbol_kind::S_100_arguments_entries: // arguments.entries
        value.copy<ArgumentEntryIndex>(YY_MOVE(that.value));
        break;

    case symbol_kind::S_101_arguments_no_trailing: // arguments.no_trailing
    case symbol_kind::S_arguments: // arguments
    case symbol_kind::S_103_arguments_paren: // arguments.paren
    case symbol_kind::S_104_arguments_maybe_paren: // arguments.maybe_paren
        value.copy<ArgumentListIndex>(YY_MOVE(that.value));
        break;

    case symbol_kind::S_106_literal_array_contents: // literal.array.contents
    case symbol_kind::S_110_literal_array: // literal.array
        value.copy<ArrayIndex>(YY_MOVE(that.value));
        break;

    case symbol_kind::S_85_block_contents: // block.contents
        value.copy<BlockContentsListIndex>(YY_MOVE(that.value));
        break;

    case symbol_kind::S_block: // block
        value.copy<BlockIndex>(YY_MOVE(that.value));
        break;

    case symbol_kind::S_86_block_contents_item: // block.contents.item
        value.copy<BlockItemIndex>(YY_MOVE(that.value));
        break;

    case symbol_kind::S_83_block_opt_list: // block.opt_list
    case symbol_kind::S_84_block_list: // block.list
        value.copy<BlockListIndex>(YY_MOVE(that.value));
        break;

    case symbol_kind::S_boolean: // boolean
        value.copy<BooleanLitIndex>(YY_MOVE(that.value));
        break;

    case symbol_kind::S_70_class_extension: // class.extension
        value.copy<ClassExtensionIndex>(YY_MOVE(that.value));
        break;

    case symbol_kind::S_class: // class
        value.copy<ClassIndex>(YY_MOVE(that.value));
        break;

    case symbol_kind::S_go: // go
        value.copy<ClassListOrExprListIndex>(YY_MOVE(that.value));
        break;

    case symbol_kind::S_66_classOrExtList_item: // classOrExtList.item
        value.copy<ClassOrExtensionIndex>(YY_MOVE(that.value));
        break;

    case symbol_kind::S_65_classOrExtList_list: // classOrExtList.list
        value.copy<ClassOrExtensionListIndex>(YY_MOVE(that.value));
        break;

    case symbol_kind::S_73_class_vars_entry: // class.vars.entry
        value.copy<DeclareAnyList>(YY_MOVE(that.value));
        break;

    case symbol_kind::S_97_variable_declarations_list_item: // variable_declarations.list.item
        value.copy<DeclareAnyVariableIndex>(YY_MOVE(that.value));
        break;

    case symbol_kind::S_93_argument_declarations_list: // argument_declarations.list
    case symbol_kind::S_94_argument_declarations_pipelist: // argument_declarations.pipelist
    case symbol_kind::S_argument_declarations: // argument_declarations
    case symbol_kind::S_96_argument_declarations_opt: // argument_declarations.opt
        value.copy<DeclareArgumentListIndex>(YY_MOVE(that.value));
        break;

    case symbol_kind::S_74_class_vars: // class.vars
    case symbol_kind::S_75_class_vars_opt: // class.vars.opt
        value.copy<DeclareClassAnyVarListIndex>(YY_MOVE(that.value));
        break;

    case symbol_kind::S_71_class_vars_entry_item: // class.vars.entry.item
        value.copy<DeclareClassVarIndex>(YY_MOVE(that.value));
        break;

    case symbol_kind::S_72_class_vars_entry_list: // class.vars.entry.list
        value.copy<DeclareMemberListIndex>(YY_MOVE(that.value));
        break;

    case symbol_kind::S_98_variable_declarations_list: // variable_declarations.list
    case symbol_kind::S_variable_declarations: // variable_declarations
        value.copy<DeclareVariableListIndex>(YY_MOVE(that.value));
        break;

    case symbol_kind::S_107_literal_dictionary_entry: // literal.dictionary.entry
        value.copy<DictionaryEntryIndex>(YY_MOVE(that.value));
        break;

    case symbol_kind::S_108_literal_dictionary_entries: // literal.dictionary.entries
    case symbol_kind::S_109_literal_dictionary: // literal.dictionary
        value.copy<DictionaryIndex>(YY_MOVE(that.value));
        break;

    case symbol_kind::S_msgsend: // msgsend
    case symbol_kind::S_88_expr_base: // expr.base
    case symbol_kind::S_expr: // expr
    case symbol_kind::S_90_expr_seq_base: // expr.seq.base
    case symbol_kind::S_91_expr_seq: // expr.seq
        value.copy<ExprSeqIndex>(YY_MOVE(that.value));
        break;

    case symbol_kind::S_124_float_raw_unsigned: // float.raw_unsigned
    case symbol_kind::S_125_float_raw: // float.raw
        value.copy<FloatLitIndex>(YY_MOVE(that.value));
        break;

    case symbol_kind::S_float: // float
        value.copy<FloatProducingIndex>(YY_MOVE(that.value));
        break;

    case symbol_kind::S_integer: // integer
        value.copy<IntLitIndex>(YY_MOVE(that.value));
        break;

    case symbol_kind::S_REGION_SEPARATOR: // REGION_SEPARATOR
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
    case symbol_kind::S_CONST: // CONST
    case symbol_kind::S_NIL: // NIL
    case symbol_kind::S_TRUE: // TRUE
    case symbol_kind::S_FALSE: // FALSE
    case symbol_kind::S_PI: // PI
    case symbol_kind::S_ELLIPSIS: // ELLIPSIS
    case symbol_kind::S_DOTDOT: // DOTDOT
    case symbol_kind::S_BEGINCLOSEDFUNC: // BEGINCLOSEDFUNC
    case symbol_kind::S_BADTOKEN: // BADTOKEN
    case symbol_kind::S_INTERPRET: // INTERPRET
    case symbol_kind::S_LEFTARROW: // LEFTARROW
    case symbol_kind::S_LEXER_ERROR: // LEXER_ERROR
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
        value.copy<LexerToken>(YY_MOVE(that.value));
        break;

    case symbol_kind::S_77_method_base: // method.base
        value.copy<MethodIndex>(YY_MOVE(that.value));
        break;

    case symbol_kind::S_79_method_list: // method.list
    case symbol_kind::S_80_method_list_opt: // method.list.opt
        value.copy<MethodListIndex>(YY_MOVE(that.value));
        break;

    case symbol_kind::S_76_method_name: // method.name
        value.copy<MethodNameIndex>(YY_MOVE(that.value));
        break;

    case symbol_kind::S_name: // name
        value.copy<NamedIdentifierIndex>(YY_MOVE(that.value));
        break;

    case symbol_kind::S_nil: // nil
        value.copy<NilLitIndex>(YY_MOVE(that.value));
        break;

    case symbol_kind::S_accessor: // accessor
        value.copy<ReadWriteAccessor>(YY_MOVE(that.value));
        break;

    case symbol_kind::S_region: // region
        value.copy<RegionListIndex>(YY_MOVE(that.value));
        break;

    case symbol_kind::S_113_binary_op_raw: // binary_op.raw
    case symbol_kind::S_114_binary_op_no_adverb: // binary_op.no_adverb
        value.copy<SelectorIndex>(YY_MOVE(that.value));
        break;

    case symbol_kind::S_binary_op: // binary_op
        value.copy<SelectorMaybeAdverbIndex>(YY_MOVE(that.value));
        break;

    case symbol_kind::S_string: // string
        value.copy<StringLitIndex>(YY_MOVE(that.value));
        break;

    case symbol_kind::S_symbol: // symbol
        value.copy<SymbolLitIndex>(YY_MOVE(that.value));
        break;

    case symbol_kind::S_64_expr_error: // expr.error
        value.copy<error_index<ExprSeqIndex>>(YY_MOVE(that.value));
        break;

    case symbol_kind::S_68_class_super_opt: // class.super.opt
        value.copy<maybe<ClassNameIdentifierIndex>>(YY_MOVE(that.value));
        break;

    case symbol_kind::S_69_class_slot_opt: // class.slot.opt
        value.copy<maybe<NamedIdentifierIndex>>(YY_MOVE(that.value));
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
    case symbol_kind::S_ascii: // ascii
        value.move<ASCIIIndex>(YY_MOVE(s.value));
        break;

    case symbol_kind::S_126_accidental_unsigned: // accidental.unsigned
    case symbol_kind::S_accidental: // accidental
        value.move<AccidentalLitIndex>(YY_MOVE(s.value));
        break;

    case symbol_kind::S_adverb: // adverb
        value.move<AdverbIndex>(YY_MOVE(s.value));
        break;

    case symbol_kind::S_105_literal_terminal: // literal.terminal
    case symbol_kind::S_literal: // literal
        value.move<AnyLiteralIndex>(YY_MOVE(s.value));
        break;

    case symbol_kind::S_method: // method
        value.move<AnyMethodIndex>(YY_MOVE(s.value));
        break;

    case symbol_kind::S_100_arguments_entries: // arguments.entries
        value.move<ArgumentEntryIndex>(YY_MOVE(s.value));
        break;

    case symbol_kind::S_101_arguments_no_trailing: // arguments.no_trailing
    case symbol_kind::S_arguments: // arguments
    case symbol_kind::S_103_arguments_paren: // arguments.paren
    case symbol_kind::S_104_arguments_maybe_paren: // arguments.maybe_paren
        value.move<ArgumentListIndex>(YY_MOVE(s.value));
        break;

    case symbol_kind::S_106_literal_array_contents: // literal.array.contents
    case symbol_kind::S_110_literal_array: // literal.array
        value.move<ArrayIndex>(YY_MOVE(s.value));
        break;

    case symbol_kind::S_85_block_contents: // block.contents
        value.move<BlockContentsListIndex>(YY_MOVE(s.value));
        break;

    case symbol_kind::S_block: // block
        value.move<BlockIndex>(YY_MOVE(s.value));
        break;

    case symbol_kind::S_86_block_contents_item: // block.contents.item
        value.move<BlockItemIndex>(YY_MOVE(s.value));
        break;

    case symbol_kind::S_83_block_opt_list: // block.opt_list
    case symbol_kind::S_84_block_list: // block.list
        value.move<BlockListIndex>(YY_MOVE(s.value));
        break;

    case symbol_kind::S_boolean: // boolean
        value.move<BooleanLitIndex>(YY_MOVE(s.value));
        break;

    case symbol_kind::S_70_class_extension: // class.extension
        value.move<ClassExtensionIndex>(YY_MOVE(s.value));
        break;

    case symbol_kind::S_class: // class
        value.move<ClassIndex>(YY_MOVE(s.value));
        break;

    case symbol_kind::S_go: // go
        value.move<ClassListOrExprListIndex>(YY_MOVE(s.value));
        break;

    case symbol_kind::S_66_classOrExtList_item: // classOrExtList.item
        value.move<ClassOrExtensionIndex>(YY_MOVE(s.value));
        break;

    case symbol_kind::S_65_classOrExtList_list: // classOrExtList.list
        value.move<ClassOrExtensionListIndex>(YY_MOVE(s.value));
        break;

    case symbol_kind::S_73_class_vars_entry: // class.vars.entry
        value.move<DeclareAnyList>(YY_MOVE(s.value));
        break;

    case symbol_kind::S_97_variable_declarations_list_item: // variable_declarations.list.item
        value.move<DeclareAnyVariableIndex>(YY_MOVE(s.value));
        break;

    case symbol_kind::S_93_argument_declarations_list: // argument_declarations.list
    case symbol_kind::S_94_argument_declarations_pipelist: // argument_declarations.pipelist
    case symbol_kind::S_argument_declarations: // argument_declarations
    case symbol_kind::S_96_argument_declarations_opt: // argument_declarations.opt
        value.move<DeclareArgumentListIndex>(YY_MOVE(s.value));
        break;

    case symbol_kind::S_74_class_vars: // class.vars
    case symbol_kind::S_75_class_vars_opt: // class.vars.opt
        value.move<DeclareClassAnyVarListIndex>(YY_MOVE(s.value));
        break;

    case symbol_kind::S_71_class_vars_entry_item: // class.vars.entry.item
        value.move<DeclareClassVarIndex>(YY_MOVE(s.value));
        break;

    case symbol_kind::S_72_class_vars_entry_list: // class.vars.entry.list
        value.move<DeclareMemberListIndex>(YY_MOVE(s.value));
        break;

    case symbol_kind::S_98_variable_declarations_list: // variable_declarations.list
    case symbol_kind::S_variable_declarations: // variable_declarations
        value.move<DeclareVariableListIndex>(YY_MOVE(s.value));
        break;

    case symbol_kind::S_107_literal_dictionary_entry: // literal.dictionary.entry
        value.move<DictionaryEntryIndex>(YY_MOVE(s.value));
        break;

    case symbol_kind::S_108_literal_dictionary_entries: // literal.dictionary.entries
    case symbol_kind::S_109_literal_dictionary: // literal.dictionary
        value.move<DictionaryIndex>(YY_MOVE(s.value));
        break;

    case symbol_kind::S_msgsend: // msgsend
    case symbol_kind::S_88_expr_base: // expr.base
    case symbol_kind::S_expr: // expr
    case symbol_kind::S_90_expr_seq_base: // expr.seq.base
    case symbol_kind::S_91_expr_seq: // expr.seq
        value.move<ExprSeqIndex>(YY_MOVE(s.value));
        break;

    case symbol_kind::S_124_float_raw_unsigned: // float.raw_unsigned
    case symbol_kind::S_125_float_raw: // float.raw
        value.move<FloatLitIndex>(YY_MOVE(s.value));
        break;

    case symbol_kind::S_float: // float
        value.move<FloatProducingIndex>(YY_MOVE(s.value));
        break;

    case symbol_kind::S_integer: // integer
        value.move<IntLitIndex>(YY_MOVE(s.value));
        break;

    case symbol_kind::S_REGION_SEPARATOR: // REGION_SEPARATOR
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
    case symbol_kind::S_CONST: // CONST
    case symbol_kind::S_NIL: // NIL
    case symbol_kind::S_TRUE: // TRUE
    case symbol_kind::S_FALSE: // FALSE
    case symbol_kind::S_PI: // PI
    case symbol_kind::S_ELLIPSIS: // ELLIPSIS
    case symbol_kind::S_DOTDOT: // DOTDOT
    case symbol_kind::S_BEGINCLOSEDFUNC: // BEGINCLOSEDFUNC
    case symbol_kind::S_BADTOKEN: // BADTOKEN
    case symbol_kind::S_INTERPRET: // INTERPRET
    case symbol_kind::S_LEFTARROW: // LEFTARROW
    case symbol_kind::S_LEXER_ERROR: // LEXER_ERROR
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
        value.move<LexerToken>(YY_MOVE(s.value));
        break;

    case symbol_kind::S_77_method_base: // method.base
        value.move<MethodIndex>(YY_MOVE(s.value));
        break;

    case symbol_kind::S_79_method_list: // method.list
    case symbol_kind::S_80_method_list_opt: // method.list.opt
        value.move<MethodListIndex>(YY_MOVE(s.value));
        break;

    case symbol_kind::S_76_method_name: // method.name
        value.move<MethodNameIndex>(YY_MOVE(s.value));
        break;

    case symbol_kind::S_name: // name
        value.move<NamedIdentifierIndex>(YY_MOVE(s.value));
        break;

    case symbol_kind::S_nil: // nil
        value.move<NilLitIndex>(YY_MOVE(s.value));
        break;

    case symbol_kind::S_accessor: // accessor
        value.move<ReadWriteAccessor>(YY_MOVE(s.value));
        break;

    case symbol_kind::S_region: // region
        value.move<RegionListIndex>(YY_MOVE(s.value));
        break;

    case symbol_kind::S_113_binary_op_raw: // binary_op.raw
    case symbol_kind::S_114_binary_op_no_adverb: // binary_op.no_adverb
        value.move<SelectorIndex>(YY_MOVE(s.value));
        break;

    case symbol_kind::S_binary_op: // binary_op
        value.move<SelectorMaybeAdverbIndex>(YY_MOVE(s.value));
        break;

    case symbol_kind::S_string: // string
        value.move<StringLitIndex>(YY_MOVE(s.value));
        break;

    case symbol_kind::S_symbol: // symbol
        value.move<SymbolLitIndex>(YY_MOVE(s.value));
        break;

    case symbol_kind::S_64_expr_error: // expr.error
        value.move<error_index<ExprSeqIndex>>(YY_MOVE(s.value));
        break;

    case symbol_kind::S_68_class_super_opt: // class.super.opt
        value.move<maybe<ClassNameIdentifierIndex>>(YY_MOVE(s.value));
        break;

    case symbol_kind::S_69_class_slot_opt: // class.slot.opt
        value.move<maybe<NamedIdentifierIndex>>(YY_MOVE(s.value));
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
    case symbol_kind::S_ascii: // ascii
        value.YY_MOVE_OR_COPY<ASCIIIndex>(YY_MOVE(that.value));
        break;

    case symbol_kind::S_126_accidental_unsigned: // accidental.unsigned
    case symbol_kind::S_accidental: // accidental
        value.YY_MOVE_OR_COPY<AccidentalLitIndex>(YY_MOVE(that.value));
        break;

    case symbol_kind::S_adverb: // adverb
        value.YY_MOVE_OR_COPY<AdverbIndex>(YY_MOVE(that.value));
        break;

    case symbol_kind::S_105_literal_terminal: // literal.terminal
    case symbol_kind::S_literal: // literal
        value.YY_MOVE_OR_COPY<AnyLiteralIndex>(YY_MOVE(that.value));
        break;

    case symbol_kind::S_method: // method
        value.YY_MOVE_OR_COPY<AnyMethodIndex>(YY_MOVE(that.value));
        break;

    case symbol_kind::S_100_arguments_entries: // arguments.entries
        value.YY_MOVE_OR_COPY<ArgumentEntryIndex>(YY_MOVE(that.value));
        break;

    case symbol_kind::S_101_arguments_no_trailing: // arguments.no_trailing
    case symbol_kind::S_arguments: // arguments
    case symbol_kind::S_103_arguments_paren: // arguments.paren
    case symbol_kind::S_104_arguments_maybe_paren: // arguments.maybe_paren
        value.YY_MOVE_OR_COPY<ArgumentListIndex>(YY_MOVE(that.value));
        break;

    case symbol_kind::S_106_literal_array_contents: // literal.array.contents
    case symbol_kind::S_110_literal_array: // literal.array
        value.YY_MOVE_OR_COPY<ArrayIndex>(YY_MOVE(that.value));
        break;

    case symbol_kind::S_85_block_contents: // block.contents
        value.YY_MOVE_OR_COPY<BlockContentsListIndex>(YY_MOVE(that.value));
        break;

    case symbol_kind::S_block: // block
        value.YY_MOVE_OR_COPY<BlockIndex>(YY_MOVE(that.value));
        break;

    case symbol_kind::S_86_block_contents_item: // block.contents.item
        value.YY_MOVE_OR_COPY<BlockItemIndex>(YY_MOVE(that.value));
        break;

    case symbol_kind::S_83_block_opt_list: // block.opt_list
    case symbol_kind::S_84_block_list: // block.list
        value.YY_MOVE_OR_COPY<BlockListIndex>(YY_MOVE(that.value));
        break;

    case symbol_kind::S_boolean: // boolean
        value.YY_MOVE_OR_COPY<BooleanLitIndex>(YY_MOVE(that.value));
        break;

    case symbol_kind::S_70_class_extension: // class.extension
        value.YY_MOVE_OR_COPY<ClassExtensionIndex>(YY_MOVE(that.value));
        break;

    case symbol_kind::S_class: // class
        value.YY_MOVE_OR_COPY<ClassIndex>(YY_MOVE(that.value));
        break;

    case symbol_kind::S_go: // go
        value.YY_MOVE_OR_COPY<ClassListOrExprListIndex>(YY_MOVE(that.value));
        break;

    case symbol_kind::S_66_classOrExtList_item: // classOrExtList.item
        value.YY_MOVE_OR_COPY<ClassOrExtensionIndex>(YY_MOVE(that.value));
        break;

    case symbol_kind::S_65_classOrExtList_list: // classOrExtList.list
        value.YY_MOVE_OR_COPY<ClassOrExtensionListIndex>(YY_MOVE(that.value));
        break;

    case symbol_kind::S_73_class_vars_entry: // class.vars.entry
        value.YY_MOVE_OR_COPY<DeclareAnyList>(YY_MOVE(that.value));
        break;

    case symbol_kind::S_97_variable_declarations_list_item: // variable_declarations.list.item
        value.YY_MOVE_OR_COPY<DeclareAnyVariableIndex>(YY_MOVE(that.value));
        break;

    case symbol_kind::S_93_argument_declarations_list: // argument_declarations.list
    case symbol_kind::S_94_argument_declarations_pipelist: // argument_declarations.pipelist
    case symbol_kind::S_argument_declarations: // argument_declarations
    case symbol_kind::S_96_argument_declarations_opt: // argument_declarations.opt
        value.YY_MOVE_OR_COPY<DeclareArgumentListIndex>(YY_MOVE(that.value));
        break;

    case symbol_kind::S_74_class_vars: // class.vars
    case symbol_kind::S_75_class_vars_opt: // class.vars.opt
        value.YY_MOVE_OR_COPY<DeclareClassAnyVarListIndex>(YY_MOVE(that.value));
        break;

    case symbol_kind::S_71_class_vars_entry_item: // class.vars.entry.item
        value.YY_MOVE_OR_COPY<DeclareClassVarIndex>(YY_MOVE(that.value));
        break;

    case symbol_kind::S_72_class_vars_entry_list: // class.vars.entry.list
        value.YY_MOVE_OR_COPY<DeclareMemberListIndex>(YY_MOVE(that.value));
        break;

    case symbol_kind::S_98_variable_declarations_list: // variable_declarations.list
    case symbol_kind::S_variable_declarations: // variable_declarations
        value.YY_MOVE_OR_COPY<DeclareVariableListIndex>(YY_MOVE(that.value));
        break;

    case symbol_kind::S_107_literal_dictionary_entry: // literal.dictionary.entry
        value.YY_MOVE_OR_COPY<DictionaryEntryIndex>(YY_MOVE(that.value));
        break;

    case symbol_kind::S_108_literal_dictionary_entries: // literal.dictionary.entries
    case symbol_kind::S_109_literal_dictionary: // literal.dictionary
        value.YY_MOVE_OR_COPY<DictionaryIndex>(YY_MOVE(that.value));
        break;

    case symbol_kind::S_msgsend: // msgsend
    case symbol_kind::S_88_expr_base: // expr.base
    case symbol_kind::S_expr: // expr
    case symbol_kind::S_90_expr_seq_base: // expr.seq.base
    case symbol_kind::S_91_expr_seq: // expr.seq
        value.YY_MOVE_OR_COPY<ExprSeqIndex>(YY_MOVE(that.value));
        break;

    case symbol_kind::S_124_float_raw_unsigned: // float.raw_unsigned
    case symbol_kind::S_125_float_raw: // float.raw
        value.YY_MOVE_OR_COPY<FloatLitIndex>(YY_MOVE(that.value));
        break;

    case symbol_kind::S_float: // float
        value.YY_MOVE_OR_COPY<FloatProducingIndex>(YY_MOVE(that.value));
        break;

    case symbol_kind::S_integer: // integer
        value.YY_MOVE_OR_COPY<IntLitIndex>(YY_MOVE(that.value));
        break;

    case symbol_kind::S_REGION_SEPARATOR: // REGION_SEPARATOR
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
    case symbol_kind::S_CONST: // CONST
    case symbol_kind::S_NIL: // NIL
    case symbol_kind::S_TRUE: // TRUE
    case symbol_kind::S_FALSE: // FALSE
    case symbol_kind::S_PI: // PI
    case symbol_kind::S_ELLIPSIS: // ELLIPSIS
    case symbol_kind::S_DOTDOT: // DOTDOT
    case symbol_kind::S_BEGINCLOSEDFUNC: // BEGINCLOSEDFUNC
    case symbol_kind::S_BADTOKEN: // BADTOKEN
    case symbol_kind::S_INTERPRET: // INTERPRET
    case symbol_kind::S_LEFTARROW: // LEFTARROW
    case symbol_kind::S_LEXER_ERROR: // LEXER_ERROR
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
        value.YY_MOVE_OR_COPY<LexerToken>(YY_MOVE(that.value));
        break;

    case symbol_kind::S_77_method_base: // method.base
        value.YY_MOVE_OR_COPY<MethodIndex>(YY_MOVE(that.value));
        break;

    case symbol_kind::S_79_method_list: // method.list
    case symbol_kind::S_80_method_list_opt: // method.list.opt
        value.YY_MOVE_OR_COPY<MethodListIndex>(YY_MOVE(that.value));
        break;

    case symbol_kind::S_76_method_name: // method.name
        value.YY_MOVE_OR_COPY<MethodNameIndex>(YY_MOVE(that.value));
        break;

    case symbol_kind::S_name: // name
        value.YY_MOVE_OR_COPY<NamedIdentifierIndex>(YY_MOVE(that.value));
        break;

    case symbol_kind::S_nil: // nil
        value.YY_MOVE_OR_COPY<NilLitIndex>(YY_MOVE(that.value));
        break;

    case symbol_kind::S_accessor: // accessor
        value.YY_MOVE_OR_COPY<ReadWriteAccessor>(YY_MOVE(that.value));
        break;

    case symbol_kind::S_region: // region
        value.YY_MOVE_OR_COPY<RegionListIndex>(YY_MOVE(that.value));
        break;

    case symbol_kind::S_113_binary_op_raw: // binary_op.raw
    case symbol_kind::S_114_binary_op_no_adverb: // binary_op.no_adverb
        value.YY_MOVE_OR_COPY<SelectorIndex>(YY_MOVE(that.value));
        break;

    case symbol_kind::S_binary_op: // binary_op
        value.YY_MOVE_OR_COPY<SelectorMaybeAdverbIndex>(YY_MOVE(that.value));
        break;

    case symbol_kind::S_string: // string
        value.YY_MOVE_OR_COPY<StringLitIndex>(YY_MOVE(that.value));
        break;

    case symbol_kind::S_symbol: // symbol
        value.YY_MOVE_OR_COPY<SymbolLitIndex>(YY_MOVE(that.value));
        break;

    case symbol_kind::S_64_expr_error: // expr.error
        value.YY_MOVE_OR_COPY<error_index<ExprSeqIndex>>(YY_MOVE(that.value));
        break;

    case symbol_kind::S_68_class_super_opt: // class.super.opt
        value.YY_MOVE_OR_COPY<maybe<ClassNameIdentifierIndex>>(YY_MOVE(that.value));
        break;

    case symbol_kind::S_69_class_slot_opt: // class.slot.opt
        value.YY_MOVE_OR_COPY<maybe<NamedIdentifierIndex>>(YY_MOVE(that.value));
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
    case symbol_kind::S_ascii: // ascii
        value.move<ASCIIIndex>(YY_MOVE(that.value));
        break;

    case symbol_kind::S_126_accidental_unsigned: // accidental.unsigned
    case symbol_kind::S_accidental: // accidental
        value.move<AccidentalLitIndex>(YY_MOVE(that.value));
        break;

    case symbol_kind::S_adverb: // adverb
        value.move<AdverbIndex>(YY_MOVE(that.value));
        break;

    case symbol_kind::S_105_literal_terminal: // literal.terminal
    case symbol_kind::S_literal: // literal
        value.move<AnyLiteralIndex>(YY_MOVE(that.value));
        break;

    case symbol_kind::S_method: // method
        value.move<AnyMethodIndex>(YY_MOVE(that.value));
        break;

    case symbol_kind::S_100_arguments_entries: // arguments.entries
        value.move<ArgumentEntryIndex>(YY_MOVE(that.value));
        break;

    case symbol_kind::S_101_arguments_no_trailing: // arguments.no_trailing
    case symbol_kind::S_arguments: // arguments
    case symbol_kind::S_103_arguments_paren: // arguments.paren
    case symbol_kind::S_104_arguments_maybe_paren: // arguments.maybe_paren
        value.move<ArgumentListIndex>(YY_MOVE(that.value));
        break;

    case symbol_kind::S_106_literal_array_contents: // literal.array.contents
    case symbol_kind::S_110_literal_array: // literal.array
        value.move<ArrayIndex>(YY_MOVE(that.value));
        break;

    case symbol_kind::S_85_block_contents: // block.contents
        value.move<BlockContentsListIndex>(YY_MOVE(that.value));
        break;

    case symbol_kind::S_block: // block
        value.move<BlockIndex>(YY_MOVE(that.value));
        break;

    case symbol_kind::S_86_block_contents_item: // block.contents.item
        value.move<BlockItemIndex>(YY_MOVE(that.value));
        break;

    case symbol_kind::S_83_block_opt_list: // block.opt_list
    case symbol_kind::S_84_block_list: // block.list
        value.move<BlockListIndex>(YY_MOVE(that.value));
        break;

    case symbol_kind::S_boolean: // boolean
        value.move<BooleanLitIndex>(YY_MOVE(that.value));
        break;

    case symbol_kind::S_70_class_extension: // class.extension
        value.move<ClassExtensionIndex>(YY_MOVE(that.value));
        break;

    case symbol_kind::S_class: // class
        value.move<ClassIndex>(YY_MOVE(that.value));
        break;

    case symbol_kind::S_go: // go
        value.move<ClassListOrExprListIndex>(YY_MOVE(that.value));
        break;

    case symbol_kind::S_66_classOrExtList_item: // classOrExtList.item
        value.move<ClassOrExtensionIndex>(YY_MOVE(that.value));
        break;

    case symbol_kind::S_65_classOrExtList_list: // classOrExtList.list
        value.move<ClassOrExtensionListIndex>(YY_MOVE(that.value));
        break;

    case symbol_kind::S_73_class_vars_entry: // class.vars.entry
        value.move<DeclareAnyList>(YY_MOVE(that.value));
        break;

    case symbol_kind::S_97_variable_declarations_list_item: // variable_declarations.list.item
        value.move<DeclareAnyVariableIndex>(YY_MOVE(that.value));
        break;

    case symbol_kind::S_93_argument_declarations_list: // argument_declarations.list
    case symbol_kind::S_94_argument_declarations_pipelist: // argument_declarations.pipelist
    case symbol_kind::S_argument_declarations: // argument_declarations
    case symbol_kind::S_96_argument_declarations_opt: // argument_declarations.opt
        value.move<DeclareArgumentListIndex>(YY_MOVE(that.value));
        break;

    case symbol_kind::S_74_class_vars: // class.vars
    case symbol_kind::S_75_class_vars_opt: // class.vars.opt
        value.move<DeclareClassAnyVarListIndex>(YY_MOVE(that.value));
        break;

    case symbol_kind::S_71_class_vars_entry_item: // class.vars.entry.item
        value.move<DeclareClassVarIndex>(YY_MOVE(that.value));
        break;

    case symbol_kind::S_72_class_vars_entry_list: // class.vars.entry.list
        value.move<DeclareMemberListIndex>(YY_MOVE(that.value));
        break;

    case symbol_kind::S_98_variable_declarations_list: // variable_declarations.list
    case symbol_kind::S_variable_declarations: // variable_declarations
        value.move<DeclareVariableListIndex>(YY_MOVE(that.value));
        break;

    case symbol_kind::S_107_literal_dictionary_entry: // literal.dictionary.entry
        value.move<DictionaryEntryIndex>(YY_MOVE(that.value));
        break;

    case symbol_kind::S_108_literal_dictionary_entries: // literal.dictionary.entries
    case symbol_kind::S_109_literal_dictionary: // literal.dictionary
        value.move<DictionaryIndex>(YY_MOVE(that.value));
        break;

    case symbol_kind::S_msgsend: // msgsend
    case symbol_kind::S_88_expr_base: // expr.base
    case symbol_kind::S_expr: // expr
    case symbol_kind::S_90_expr_seq_base: // expr.seq.base
    case symbol_kind::S_91_expr_seq: // expr.seq
        value.move<ExprSeqIndex>(YY_MOVE(that.value));
        break;

    case symbol_kind::S_124_float_raw_unsigned: // float.raw_unsigned
    case symbol_kind::S_125_float_raw: // float.raw
        value.move<FloatLitIndex>(YY_MOVE(that.value));
        break;

    case symbol_kind::S_float: // float
        value.move<FloatProducingIndex>(YY_MOVE(that.value));
        break;

    case symbol_kind::S_integer: // integer
        value.move<IntLitIndex>(YY_MOVE(that.value));
        break;

    case symbol_kind::S_REGION_SEPARATOR: // REGION_SEPARATOR
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
    case symbol_kind::S_CONST: // CONST
    case symbol_kind::S_NIL: // NIL
    case symbol_kind::S_TRUE: // TRUE
    case symbol_kind::S_FALSE: // FALSE
    case symbol_kind::S_PI: // PI
    case symbol_kind::S_ELLIPSIS: // ELLIPSIS
    case symbol_kind::S_DOTDOT: // DOTDOT
    case symbol_kind::S_BEGINCLOSEDFUNC: // BEGINCLOSEDFUNC
    case symbol_kind::S_BADTOKEN: // BADTOKEN
    case symbol_kind::S_INTERPRET: // INTERPRET
    case symbol_kind::S_LEFTARROW: // LEFTARROW
    case symbol_kind::S_LEXER_ERROR: // LEXER_ERROR
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
        value.move<LexerToken>(YY_MOVE(that.value));
        break;

    case symbol_kind::S_77_method_base: // method.base
        value.move<MethodIndex>(YY_MOVE(that.value));
        break;

    case symbol_kind::S_79_method_list: // method.list
    case symbol_kind::S_80_method_list_opt: // method.list.opt
        value.move<MethodListIndex>(YY_MOVE(that.value));
        break;

    case symbol_kind::S_76_method_name: // method.name
        value.move<MethodNameIndex>(YY_MOVE(that.value));
        break;

    case symbol_kind::S_name: // name
        value.move<NamedIdentifierIndex>(YY_MOVE(that.value));
        break;

    case symbol_kind::S_nil: // nil
        value.move<NilLitIndex>(YY_MOVE(that.value));
        break;

    case symbol_kind::S_accessor: // accessor
        value.move<ReadWriteAccessor>(YY_MOVE(that.value));
        break;

    case symbol_kind::S_region: // region
        value.move<RegionListIndex>(YY_MOVE(that.value));
        break;

    case symbol_kind::S_113_binary_op_raw: // binary_op.raw
    case symbol_kind::S_114_binary_op_no_adverb: // binary_op.no_adverb
        value.move<SelectorIndex>(YY_MOVE(that.value));
        break;

    case symbol_kind::S_binary_op: // binary_op
        value.move<SelectorMaybeAdverbIndex>(YY_MOVE(that.value));
        break;

    case symbol_kind::S_string: // string
        value.move<StringLitIndex>(YY_MOVE(that.value));
        break;

    case symbol_kind::S_symbol: // symbol
        value.move<SymbolLitIndex>(YY_MOVE(that.value));
        break;

    case symbol_kind::S_64_expr_error: // expr.error
        value.move<error_index<ExprSeqIndex>>(YY_MOVE(that.value));
        break;

    case symbol_kind::S_68_class_super_opt: // class.super.opt
        value.move<maybe<ClassNameIdentifierIndex>>(YY_MOVE(that.value));
        break;

    case symbol_kind::S_69_class_slot_opt: // class.slot.opt
        value.move<maybe<NamedIdentifierIndex>>(YY_MOVE(that.value));
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
    case symbol_kind::S_ascii: // ascii
        value.copy<ASCIIIndex>(that.value);
        break;

    case symbol_kind::S_126_accidental_unsigned: // accidental.unsigned
    case symbol_kind::S_accidental: // accidental
        value.copy<AccidentalLitIndex>(that.value);
        break;

    case symbol_kind::S_adverb: // adverb
        value.copy<AdverbIndex>(that.value);
        break;

    case symbol_kind::S_105_literal_terminal: // literal.terminal
    case symbol_kind::S_literal: // literal
        value.copy<AnyLiteralIndex>(that.value);
        break;

    case symbol_kind::S_method: // method
        value.copy<AnyMethodIndex>(that.value);
        break;

    case symbol_kind::S_100_arguments_entries: // arguments.entries
        value.copy<ArgumentEntryIndex>(that.value);
        break;

    case symbol_kind::S_101_arguments_no_trailing: // arguments.no_trailing
    case symbol_kind::S_arguments: // arguments
    case symbol_kind::S_103_arguments_paren: // arguments.paren
    case symbol_kind::S_104_arguments_maybe_paren: // arguments.maybe_paren
        value.copy<ArgumentListIndex>(that.value);
        break;

    case symbol_kind::S_106_literal_array_contents: // literal.array.contents
    case symbol_kind::S_110_literal_array: // literal.array
        value.copy<ArrayIndex>(that.value);
        break;

    case symbol_kind::S_85_block_contents: // block.contents
        value.copy<BlockContentsListIndex>(that.value);
        break;

    case symbol_kind::S_block: // block
        value.copy<BlockIndex>(that.value);
        break;

    case symbol_kind::S_86_block_contents_item: // block.contents.item
        value.copy<BlockItemIndex>(that.value);
        break;

    case symbol_kind::S_83_block_opt_list: // block.opt_list
    case symbol_kind::S_84_block_list: // block.list
        value.copy<BlockListIndex>(that.value);
        break;

    case symbol_kind::S_boolean: // boolean
        value.copy<BooleanLitIndex>(that.value);
        break;

    case symbol_kind::S_70_class_extension: // class.extension
        value.copy<ClassExtensionIndex>(that.value);
        break;

    case symbol_kind::S_class: // class
        value.copy<ClassIndex>(that.value);
        break;

    case symbol_kind::S_go: // go
        value.copy<ClassListOrExprListIndex>(that.value);
        break;

    case symbol_kind::S_66_classOrExtList_item: // classOrExtList.item
        value.copy<ClassOrExtensionIndex>(that.value);
        break;

    case symbol_kind::S_65_classOrExtList_list: // classOrExtList.list
        value.copy<ClassOrExtensionListIndex>(that.value);
        break;

    case symbol_kind::S_73_class_vars_entry: // class.vars.entry
        value.copy<DeclareAnyList>(that.value);
        break;

    case symbol_kind::S_97_variable_declarations_list_item: // variable_declarations.list.item
        value.copy<DeclareAnyVariableIndex>(that.value);
        break;

    case symbol_kind::S_93_argument_declarations_list: // argument_declarations.list
    case symbol_kind::S_94_argument_declarations_pipelist: // argument_declarations.pipelist
    case symbol_kind::S_argument_declarations: // argument_declarations
    case symbol_kind::S_96_argument_declarations_opt: // argument_declarations.opt
        value.copy<DeclareArgumentListIndex>(that.value);
        break;

    case symbol_kind::S_74_class_vars: // class.vars
    case symbol_kind::S_75_class_vars_opt: // class.vars.opt
        value.copy<DeclareClassAnyVarListIndex>(that.value);
        break;

    case symbol_kind::S_71_class_vars_entry_item: // class.vars.entry.item
        value.copy<DeclareClassVarIndex>(that.value);
        break;

    case symbol_kind::S_72_class_vars_entry_list: // class.vars.entry.list
        value.copy<DeclareMemberListIndex>(that.value);
        break;

    case symbol_kind::S_98_variable_declarations_list: // variable_declarations.list
    case symbol_kind::S_variable_declarations: // variable_declarations
        value.copy<DeclareVariableListIndex>(that.value);
        break;

    case symbol_kind::S_107_literal_dictionary_entry: // literal.dictionary.entry
        value.copy<DictionaryEntryIndex>(that.value);
        break;

    case symbol_kind::S_108_literal_dictionary_entries: // literal.dictionary.entries
    case symbol_kind::S_109_literal_dictionary: // literal.dictionary
        value.copy<DictionaryIndex>(that.value);
        break;

    case symbol_kind::S_msgsend: // msgsend
    case symbol_kind::S_88_expr_base: // expr.base
    case symbol_kind::S_expr: // expr
    case symbol_kind::S_90_expr_seq_base: // expr.seq.base
    case symbol_kind::S_91_expr_seq: // expr.seq
        value.copy<ExprSeqIndex>(that.value);
        break;

    case symbol_kind::S_124_float_raw_unsigned: // float.raw_unsigned
    case symbol_kind::S_125_float_raw: // float.raw
        value.copy<FloatLitIndex>(that.value);
        break;

    case symbol_kind::S_float: // float
        value.copy<FloatProducingIndex>(that.value);
        break;

    case symbol_kind::S_integer: // integer
        value.copy<IntLitIndex>(that.value);
        break;

    case symbol_kind::S_REGION_SEPARATOR: // REGION_SEPARATOR
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
    case symbol_kind::S_CONST: // CONST
    case symbol_kind::S_NIL: // NIL
    case symbol_kind::S_TRUE: // TRUE
    case symbol_kind::S_FALSE: // FALSE
    case symbol_kind::S_PI: // PI
    case symbol_kind::S_ELLIPSIS: // ELLIPSIS
    case symbol_kind::S_DOTDOT: // DOTDOT
    case symbol_kind::S_BEGINCLOSEDFUNC: // BEGINCLOSEDFUNC
    case symbol_kind::S_BADTOKEN: // BADTOKEN
    case symbol_kind::S_INTERPRET: // INTERPRET
    case symbol_kind::S_LEFTARROW: // LEFTARROW
    case symbol_kind::S_LEXER_ERROR: // LEXER_ERROR
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
        value.copy<LexerToken>(that.value);
        break;

    case symbol_kind::S_77_method_base: // method.base
        value.copy<MethodIndex>(that.value);
        break;

    case symbol_kind::S_79_method_list: // method.list
    case symbol_kind::S_80_method_list_opt: // method.list.opt
        value.copy<MethodListIndex>(that.value);
        break;

    case symbol_kind::S_76_method_name: // method.name
        value.copy<MethodNameIndex>(that.value);
        break;

    case symbol_kind::S_name: // name
        value.copy<NamedIdentifierIndex>(that.value);
        break;

    case symbol_kind::S_nil: // nil
        value.copy<NilLitIndex>(that.value);
        break;

    case symbol_kind::S_accessor: // accessor
        value.copy<ReadWriteAccessor>(that.value);
        break;

    case symbol_kind::S_region: // region
        value.copy<RegionListIndex>(that.value);
        break;

    case symbol_kind::S_113_binary_op_raw: // binary_op.raw
    case symbol_kind::S_114_binary_op_no_adverb: // binary_op.no_adverb
        value.copy<SelectorIndex>(that.value);
        break;

    case symbol_kind::S_binary_op: // binary_op
        value.copy<SelectorMaybeAdverbIndex>(that.value);
        break;

    case symbol_kind::S_string: // string
        value.copy<StringLitIndex>(that.value);
        break;

    case symbol_kind::S_symbol: // symbol
        value.copy<SymbolLitIndex>(that.value);
        break;

    case symbol_kind::S_64_expr_error: // expr.error
        value.copy<error_index<ExprSeqIndex>>(that.value);
        break;

    case symbol_kind::S_68_class_super_opt: // class.super.opt
        value.copy<maybe<ClassNameIdentifierIndex>>(that.value);
        break;

    case symbol_kind::S_69_class_slot_opt: // class.slot.opt
        value.copy<maybe<NamedIdentifierIndex>>(that.value);
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
    case symbol_kind::S_ascii: // ascii
        value.move<ASCIIIndex>(that.value);
        break;

    case symbol_kind::S_126_accidental_unsigned: // accidental.unsigned
    case symbol_kind::S_accidental: // accidental
        value.move<AccidentalLitIndex>(that.value);
        break;

    case symbol_kind::S_adverb: // adverb
        value.move<AdverbIndex>(that.value);
        break;

    case symbol_kind::S_105_literal_terminal: // literal.terminal
    case symbol_kind::S_literal: // literal
        value.move<AnyLiteralIndex>(that.value);
        break;

    case symbol_kind::S_method: // method
        value.move<AnyMethodIndex>(that.value);
        break;

    case symbol_kind::S_100_arguments_entries: // arguments.entries
        value.move<ArgumentEntryIndex>(that.value);
        break;

    case symbol_kind::S_101_arguments_no_trailing: // arguments.no_trailing
    case symbol_kind::S_arguments: // arguments
    case symbol_kind::S_103_arguments_paren: // arguments.paren
    case symbol_kind::S_104_arguments_maybe_paren: // arguments.maybe_paren
        value.move<ArgumentListIndex>(that.value);
        break;

    case symbol_kind::S_106_literal_array_contents: // literal.array.contents
    case symbol_kind::S_110_literal_array: // literal.array
        value.move<ArrayIndex>(that.value);
        break;

    case symbol_kind::S_85_block_contents: // block.contents
        value.move<BlockContentsListIndex>(that.value);
        break;

    case symbol_kind::S_block: // block
        value.move<BlockIndex>(that.value);
        break;

    case symbol_kind::S_86_block_contents_item: // block.contents.item
        value.move<BlockItemIndex>(that.value);
        break;

    case symbol_kind::S_83_block_opt_list: // block.opt_list
    case symbol_kind::S_84_block_list: // block.list
        value.move<BlockListIndex>(that.value);
        break;

    case symbol_kind::S_boolean: // boolean
        value.move<BooleanLitIndex>(that.value);
        break;

    case symbol_kind::S_70_class_extension: // class.extension
        value.move<ClassExtensionIndex>(that.value);
        break;

    case symbol_kind::S_class: // class
        value.move<ClassIndex>(that.value);
        break;

    case symbol_kind::S_go: // go
        value.move<ClassListOrExprListIndex>(that.value);
        break;

    case symbol_kind::S_66_classOrExtList_item: // classOrExtList.item
        value.move<ClassOrExtensionIndex>(that.value);
        break;

    case symbol_kind::S_65_classOrExtList_list: // classOrExtList.list
        value.move<ClassOrExtensionListIndex>(that.value);
        break;

    case symbol_kind::S_73_class_vars_entry: // class.vars.entry
        value.move<DeclareAnyList>(that.value);
        break;

    case symbol_kind::S_97_variable_declarations_list_item: // variable_declarations.list.item
        value.move<DeclareAnyVariableIndex>(that.value);
        break;

    case symbol_kind::S_93_argument_declarations_list: // argument_declarations.list
    case symbol_kind::S_94_argument_declarations_pipelist: // argument_declarations.pipelist
    case symbol_kind::S_argument_declarations: // argument_declarations
    case symbol_kind::S_96_argument_declarations_opt: // argument_declarations.opt
        value.move<DeclareArgumentListIndex>(that.value);
        break;

    case symbol_kind::S_74_class_vars: // class.vars
    case symbol_kind::S_75_class_vars_opt: // class.vars.opt
        value.move<DeclareClassAnyVarListIndex>(that.value);
        break;

    case symbol_kind::S_71_class_vars_entry_item: // class.vars.entry.item
        value.move<DeclareClassVarIndex>(that.value);
        break;

    case symbol_kind::S_72_class_vars_entry_list: // class.vars.entry.list
        value.move<DeclareMemberListIndex>(that.value);
        break;

    case symbol_kind::S_98_variable_declarations_list: // variable_declarations.list
    case symbol_kind::S_variable_declarations: // variable_declarations
        value.move<DeclareVariableListIndex>(that.value);
        break;

    case symbol_kind::S_107_literal_dictionary_entry: // literal.dictionary.entry
        value.move<DictionaryEntryIndex>(that.value);
        break;

    case symbol_kind::S_108_literal_dictionary_entries: // literal.dictionary.entries
    case symbol_kind::S_109_literal_dictionary: // literal.dictionary
        value.move<DictionaryIndex>(that.value);
        break;

    case symbol_kind::S_msgsend: // msgsend
    case symbol_kind::S_88_expr_base: // expr.base
    case symbol_kind::S_expr: // expr
    case symbol_kind::S_90_expr_seq_base: // expr.seq.base
    case symbol_kind::S_91_expr_seq: // expr.seq
        value.move<ExprSeqIndex>(that.value);
        break;

    case symbol_kind::S_124_float_raw_unsigned: // float.raw_unsigned
    case symbol_kind::S_125_float_raw: // float.raw
        value.move<FloatLitIndex>(that.value);
        break;

    case symbol_kind::S_float: // float
        value.move<FloatProducingIndex>(that.value);
        break;

    case symbol_kind::S_integer: // integer
        value.move<IntLitIndex>(that.value);
        break;

    case symbol_kind::S_REGION_SEPARATOR: // REGION_SEPARATOR
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
    case symbol_kind::S_CONST: // CONST
    case symbol_kind::S_NIL: // NIL
    case symbol_kind::S_TRUE: // TRUE
    case symbol_kind::S_FALSE: // FALSE
    case symbol_kind::S_PI: // PI
    case symbol_kind::S_ELLIPSIS: // ELLIPSIS
    case symbol_kind::S_DOTDOT: // DOTDOT
    case symbol_kind::S_BEGINCLOSEDFUNC: // BEGINCLOSEDFUNC
    case symbol_kind::S_BADTOKEN: // BADTOKEN
    case symbol_kind::S_INTERPRET: // INTERPRET
    case symbol_kind::S_LEFTARROW: // LEFTARROW
    case symbol_kind::S_LEXER_ERROR: // LEXER_ERROR
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
        value.move<LexerToken>(that.value);
        break;

    case symbol_kind::S_77_method_base: // method.base
        value.move<MethodIndex>(that.value);
        break;

    case symbol_kind::S_79_method_list: // method.list
    case symbol_kind::S_80_method_list_opt: // method.list.opt
        value.move<MethodListIndex>(that.value);
        break;

    case symbol_kind::S_76_method_name: // method.name
        value.move<MethodNameIndex>(that.value);
        break;

    case symbol_kind::S_name: // name
        value.move<NamedIdentifierIndex>(that.value);
        break;

    case symbol_kind::S_nil: // nil
        value.move<NilLitIndex>(that.value);
        break;

    case symbol_kind::S_accessor: // accessor
        value.move<ReadWriteAccessor>(that.value);
        break;

    case symbol_kind::S_region: // region
        value.move<RegionListIndex>(that.value);
        break;

    case symbol_kind::S_113_binary_op_raw: // binary_op.raw
    case symbol_kind::S_114_binary_op_no_adverb: // binary_op.no_adverb
        value.move<SelectorIndex>(that.value);
        break;

    case symbol_kind::S_binary_op: // binary_op
        value.move<SelectorMaybeAdverbIndex>(that.value);
        break;

    case symbol_kind::S_string: // string
        value.move<StringLitIndex>(that.value);
        break;

    case symbol_kind::S_symbol: // symbol
        value.move<SymbolLitIndex>(that.value);
        break;

    case symbol_kind::S_64_expr_error: // expr.error
        value.move<error_index<ExprSeqIndex>>(that.value);
        break;

    case symbol_kind::S_68_class_super_opt: // class.super.opt
        value.move<maybe<ClassNameIdentifierIndex>>(that.value);
        break;

    case symbol_kind::S_69_class_slot_opt: // class.slot.opt
        value.move<maybe<NamedIdentifierIndex>>(that.value);
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
            case symbol_kind::S_ascii: // ascii
                yylhs.value.emplace<ASCIIIndex>();
                break;

            case symbol_kind::S_126_accidental_unsigned: // accidental.unsigned
            case symbol_kind::S_accidental: // accidental
                yylhs.value.emplace<AccidentalLitIndex>();
                break;

            case symbol_kind::S_adverb: // adverb
                yylhs.value.emplace<AdverbIndex>();
                break;

            case symbol_kind::S_105_literal_terminal: // literal.terminal
            case symbol_kind::S_literal: // literal
                yylhs.value.emplace<AnyLiteralIndex>();
                break;

            case symbol_kind::S_method: // method
                yylhs.value.emplace<AnyMethodIndex>();
                break;

            case symbol_kind::S_100_arguments_entries: // arguments.entries
                yylhs.value.emplace<ArgumentEntryIndex>();
                break;

            case symbol_kind::S_101_arguments_no_trailing: // arguments.no_trailing
            case symbol_kind::S_arguments: // arguments
            case symbol_kind::S_103_arguments_paren: // arguments.paren
            case symbol_kind::S_104_arguments_maybe_paren: // arguments.maybe_paren
                yylhs.value.emplace<ArgumentListIndex>();
                break;

            case symbol_kind::S_106_literal_array_contents: // literal.array.contents
            case symbol_kind::S_110_literal_array: // literal.array
                yylhs.value.emplace<ArrayIndex>();
                break;

            case symbol_kind::S_85_block_contents: // block.contents
                yylhs.value.emplace<BlockContentsListIndex>();
                break;

            case symbol_kind::S_block: // block
                yylhs.value.emplace<BlockIndex>();
                break;

            case symbol_kind::S_86_block_contents_item: // block.contents.item
                yylhs.value.emplace<BlockItemIndex>();
                break;

            case symbol_kind::S_83_block_opt_list: // block.opt_list
            case symbol_kind::S_84_block_list: // block.list
                yylhs.value.emplace<BlockListIndex>();
                break;

            case symbol_kind::S_boolean: // boolean
                yylhs.value.emplace<BooleanLitIndex>();
                break;

            case symbol_kind::S_70_class_extension: // class.extension
                yylhs.value.emplace<ClassExtensionIndex>();
                break;

            case symbol_kind::S_class: // class
                yylhs.value.emplace<ClassIndex>();
                break;

            case symbol_kind::S_go: // go
                yylhs.value.emplace<ClassListOrExprListIndex>();
                break;

            case symbol_kind::S_66_classOrExtList_item: // classOrExtList.item
                yylhs.value.emplace<ClassOrExtensionIndex>();
                break;

            case symbol_kind::S_65_classOrExtList_list: // classOrExtList.list
                yylhs.value.emplace<ClassOrExtensionListIndex>();
                break;

            case symbol_kind::S_73_class_vars_entry: // class.vars.entry
                yylhs.value.emplace<DeclareAnyList>();
                break;

            case symbol_kind::S_97_variable_declarations_list_item: // variable_declarations.list.item
                yylhs.value.emplace<DeclareAnyVariableIndex>();
                break;

            case symbol_kind::S_93_argument_declarations_list: // argument_declarations.list
            case symbol_kind::S_94_argument_declarations_pipelist: // argument_declarations.pipelist
            case symbol_kind::S_argument_declarations: // argument_declarations
            case symbol_kind::S_96_argument_declarations_opt: // argument_declarations.opt
                yylhs.value.emplace<DeclareArgumentListIndex>();
                break;

            case symbol_kind::S_74_class_vars: // class.vars
            case symbol_kind::S_75_class_vars_opt: // class.vars.opt
                yylhs.value.emplace<DeclareClassAnyVarListIndex>();
                break;

            case symbol_kind::S_71_class_vars_entry_item: // class.vars.entry.item
                yylhs.value.emplace<DeclareClassVarIndex>();
                break;

            case symbol_kind::S_72_class_vars_entry_list: // class.vars.entry.list
                yylhs.value.emplace<DeclareMemberListIndex>();
                break;

            case symbol_kind::S_98_variable_declarations_list: // variable_declarations.list
            case symbol_kind::S_variable_declarations: // variable_declarations
                yylhs.value.emplace<DeclareVariableListIndex>();
                break;

            case symbol_kind::S_107_literal_dictionary_entry: // literal.dictionary.entry
                yylhs.value.emplace<DictionaryEntryIndex>();
                break;

            case symbol_kind::S_108_literal_dictionary_entries: // literal.dictionary.entries
            case symbol_kind::S_109_literal_dictionary: // literal.dictionary
                yylhs.value.emplace<DictionaryIndex>();
                break;

            case symbol_kind::S_msgsend: // msgsend
            case symbol_kind::S_88_expr_base: // expr.base
            case symbol_kind::S_expr: // expr
            case symbol_kind::S_90_expr_seq_base: // expr.seq.base
            case symbol_kind::S_91_expr_seq: // expr.seq
                yylhs.value.emplace<ExprSeqIndex>();
                break;

            case symbol_kind::S_124_float_raw_unsigned: // float.raw_unsigned
            case symbol_kind::S_125_float_raw: // float.raw
                yylhs.value.emplace<FloatLitIndex>();
                break;

            case symbol_kind::S_float: // float
                yylhs.value.emplace<FloatProducingIndex>();
                break;

            case symbol_kind::S_integer: // integer
                yylhs.value.emplace<IntLitIndex>();
                break;

            case symbol_kind::S_REGION_SEPARATOR: // REGION_SEPARATOR
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
            case symbol_kind::S_CONST: // CONST
            case symbol_kind::S_NIL: // NIL
            case symbol_kind::S_TRUE: // TRUE
            case symbol_kind::S_FALSE: // FALSE
            case symbol_kind::S_PI: // PI
            case symbol_kind::S_ELLIPSIS: // ELLIPSIS
            case symbol_kind::S_DOTDOT: // DOTDOT
            case symbol_kind::S_BEGINCLOSEDFUNC: // BEGINCLOSEDFUNC
            case symbol_kind::S_BADTOKEN: // BADTOKEN
            case symbol_kind::S_INTERPRET: // INTERPRET
            case symbol_kind::S_LEFTARROW: // LEFTARROW
            case symbol_kind::S_LEXER_ERROR: // LEXER_ERROR
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
                yylhs.value.emplace<LexerToken>();
                break;

            case symbol_kind::S_77_method_base: // method.base
                yylhs.value.emplace<MethodIndex>();
                break;

            case symbol_kind::S_79_method_list: // method.list
            case symbol_kind::S_80_method_list_opt: // method.list.opt
                yylhs.value.emplace<MethodListIndex>();
                break;

            case symbol_kind::S_76_method_name: // method.name
                yylhs.value.emplace<MethodNameIndex>();
                break;

            case symbol_kind::S_name: // name
                yylhs.value.emplace<NamedIdentifierIndex>();
                break;

            case symbol_kind::S_nil: // nil
                yylhs.value.emplace<NilLitIndex>();
                break;

            case symbol_kind::S_accessor: // accessor
                yylhs.value.emplace<ReadWriteAccessor>();
                break;

            case symbol_kind::S_region: // region
                yylhs.value.emplace<RegionListIndex>();
                break;

            case symbol_kind::S_113_binary_op_raw: // binary_op.raw
            case symbol_kind::S_114_binary_op_no_adverb: // binary_op.no_adverb
                yylhs.value.emplace<SelectorIndex>();
                break;

            case symbol_kind::S_binary_op: // binary_op
                yylhs.value.emplace<SelectorMaybeAdverbIndex>();
                break;

            case symbol_kind::S_string: // string
                yylhs.value.emplace<StringLitIndex>();
                break;

            case symbol_kind::S_symbol: // symbol
                yylhs.value.emplace<SymbolLitIndex>();
                break;

            case symbol_kind::S_64_expr_error: // expr.error
                yylhs.value.emplace<error_index<ExprSeqIndex>>();
                break;

            case symbol_kind::S_68_class_super_opt: // class.super.opt
                yylhs.value.emplace<maybe<ClassNameIdentifierIndex>>();
                break;

            case symbol_kind::S_69_class_slot_opt: // class.slot.opt
                yylhs.value.emplace<maybe<NamedIdentifierIndex>>();
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
                case 2: // go: INTERPRET region semicolon.opt
#line 174 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<ClassListOrExprListIndex>() =
                        cxt.graph.assign_root(yystack_[1].value.as<RegionListIndex>());
                }
#line 2485 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 3: // go: classOrExtList.list
#line 175 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<ClassListOrExprListIndex>() =
                        cxt.graph.assign_root(yystack_[0].value.as<ClassOrExtensionListIndex>());
                }
#line 2491 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 4: // region: expr.error
#line 182 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<RegionListIndex>() =
                        cxt.create(RegionList {}, yylhs.location, yystack_[0].value.as<error_index<ExprSeqIndex>>());
                }
#line 2497 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 5: // region: region SEMICOLON expr.error
#line 184 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<RegionListIndex>() =
                        cxt.graph.append_to_list(yystack_[2].value.as<RegionListIndex>(), yylhs.location,
                                                 yystack_[0].value.as<error_index<ExprSeqIndex>>());
                }
#line 2503 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 6: // region: region error
#line 186 "langutils/sc_parser/src/sc_grammar.y"
                {
                    if (yystack_[1].location.end.line_number != yystack_[0].location.begin.line_number) {
                        error_recovery::region_separator(cxt, yystack_[1].location);
                        cxt.region_recovery = sc::parser::ParserContext::RegionRecovery::EmitRegionSeparator;
                        static_assert(std::is_same_v<decltype(yyerrstatus_), int>);
                        yyerrstatus_ = 0; // this is NOT in the api, but the only way to get errors to re-emit.
                        yyclearin;
                        yylhs.value.as<RegionListIndex>() = yystack_[1].value.as<RegionListIndex>();
                    } else {
                        error_recovery::expr(cxt);
                        yylhs.value.as<RegionListIndex>() = cxt.graph.append_to_list(
                            yystack_[1].value.as<RegionListIndex>(),
                            create_error(cxt, yystack_[0].location, yystack_[1].value.as<RegionListIndex>()));
                        // cxt.region_recovery = sc::parser::ParserContext::RegionRecovery::EmitRegionSeparator;
                        yyclearin;
                    }
                }
#line 2523 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 7: // region: region REGION_SEPARATOR expr.error
#line 203 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<RegionListIndex>() =
                        cxt.graph.append_to_list(yystack_[2].value.as<RegionListIndex>(), yylhs.location,
                                                 yystack_[0].value.as<error_index<ExprSeqIndex>>());
                }
#line 2529 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 8: // expr.error: expr
#line 207 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<error_index<ExprSeqIndex>>() = yystack_[0].value.as<ExprSeqIndex>();
                }
#line 2535 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 9: // expr.error: error
#line 208 "langutils/sc_parser/src/sc_grammar.y"
                {
                    std::cout << "EXPR ERROR" << std::endl;
                    auto unexpected = cxt.consume_error();
                    yylhs.value.as<error_index<ExprSeqIndex>>() = create_error(cxt, yylhs.location);
                    yyclearin;
                }
#line 2546 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 10: // classOrExtList.list: classOrExtList.item
#line 218 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<ClassOrExtensionListIndex>() = cxt.create(
                        ClassOrExtensionList {}, yylhs.location, yystack_[0].value.as<ClassOrExtensionIndex>());
                }
#line 2552 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 11: // classOrExtList.list: classOrExtList.list classOrExtList.item
#line 220 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<ClassOrExtensionListIndex>() =
                        cxt.graph.append_to_list(yystack_[1].value.as<ClassOrExtensionListIndex>(),
                                                 yystack_[0].value.as<ClassOrExtensionIndex>());
                }
#line 2558 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 12: // classOrExtList.item: class
#line 224 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<ClassOrExtensionIndex>() = yystack_[0].value.as<ClassIndex>();
                }
#line 2564 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 13: // classOrExtList.item: class.extension
#line 225 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<ClassOrExtensionIndex>() = yystack_[0].value.as<ClassExtensionIndex>();
                }
#line 2570 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 14: // class: CLASSNAME class.slot.opt class.super.opt OPENCURLY class.vars.opt method.list.opt
                         // CLOSECURLY
#line 230 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<ClassIndex>() = cxt.create(
                        Class {}, yylhs.location, cxt.create(ClassNameIdentifier {}, yystack_[6].location),
                        yystack_[5].value.as<maybe<NamedIdentifierIndex>>(),
                        yystack_[4].value.as<maybe<ClassNameIdentifierIndex>>(),
                        yystack_[2].value.as<DeclareClassAnyVarListIndex>(), yystack_[1].value.as<MethodListIndex>());
                }
#line 2576 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 15: // class.super.opt: %empty
#line 234 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<maybe<ClassNameIdentifierIndex>>() = cxt.create(Missing {}, yylhs.location);
                }
#line 2582 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 16: // class.super.opt: COLON CLASSNAME
#line 235 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<maybe<ClassNameIdentifierIndex>>() =
                        cxt.create(ClassNameIdentifier {}, yystack_[0].location);
                }
#line 2588 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 17: // class.slot.opt: %empty
#line 239 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<maybe<NamedIdentifierIndex>>() = cxt.create(Missing {}, yylhs.location);
                }
#line 2594 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 18: // class.slot.opt: OPENSQUARE name CLOSESQUARE
#line 240 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<maybe<NamedIdentifierIndex>>() = yystack_[1].value.as<NamedIdentifierIndex>();
                }
#line 2600 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 19: // class.extension: ADD CLASSNAME OPENCURLY method.list.opt CLOSECURLY
#line 245 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<ClassExtensionIndex>() = cxt.create(
                        ClassExtension {}, yylhs.location, cxt.create(ClassNameIdentifier {}, yystack_[3].location),
                        yystack_[1].value.as<MethodListIndex>());
                }
#line 2606 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 20: // class.vars.entry.item: accessor variable_declarations.list.item
#line 250 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<DeclareClassVarIndex>() =
                        cxt.create(DeclareClassVar { yystack_[1].value.as<ReadWriteAccessor>() }, yylhs.location,
                                   yystack_[0].value.as<DeclareAnyVariableIndex>());
                }
#line 2612 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 21: // class.vars.entry.list: class.vars.entry.item
#line 255 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<DeclareMemberListIndex>() =
                        cxt.create(DeclareMemberList {}, yylhs.location, yystack_[0].value.as<DeclareClassVarIndex>());
                }
#line 2618 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 22: // class.vars.entry.list: class.vars.entry.list COMMA class.vars.entry.item
#line 257 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<DeclareMemberListIndex>() = cxt.graph.append_to_list(
                        yystack_[2].value.as<DeclareMemberListIndex>(), yystack_[0].value.as<DeclareClassVarIndex>());
                }
#line 2624 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 23: // class.vars.entry: CLASSVAR class.vars.entry.list
#line 262 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<DeclareAnyList>() =
                        cxt.graph.cast<DeclareClassMemberList>(yystack_[0].value.as<DeclareMemberListIndex>());
                }
#line 2630 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 24: // class.vars.entry: VAR class.vars.entry.list
#line 264 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<DeclareAnyList>() =
                        cxt.graph.cast<DeclareMemberList>(yystack_[0].value.as<DeclareMemberListIndex>());
                }
#line 2636 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 25: // class.vars.entry: CONST class.vars.entry.list
#line 266 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<DeclareAnyList>() =
                        cxt.graph.cast<DeclareConstList>(yystack_[0].value.as<DeclareMemberListIndex>());
                }
#line 2642 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 26: // class.vars: class.vars.entry
#line 271 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<DeclareClassAnyVarListIndex>() =
                        cxt.create(ClassAnyVarList {}, yylhs.location, yystack_[0].value.as<DeclareAnyList>());
                }
#line 2648 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 27: // class.vars: class.vars SEMICOLON class.vars.entry
#line 273 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<DeclareClassAnyVarListIndex>() = cxt.graph.append_to_list(
                        yystack_[2].value.as<DeclareClassAnyVarListIndex>(), yystack_[0].value.as<DeclareAnyList>());
                }
#line 2654 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 28: // class.vars.opt: %empty
#line 277 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<DeclareClassAnyVarListIndex>() = cxt.create(ClassAnyVarList {}, yylhs.location);
                }
#line 2660 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 29: // class.vars.opt: class.vars semicolon.opt
#line 278 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<DeclareClassAnyVarListIndex>() = yystack_[1].value.as<DeclareClassAnyVarListIndex>();
                }
#line 2666 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 30: // method.name: name
#line 282 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<MethodNameIndex>() = yystack_[0].value.as<NamedIdentifierIndex>();
                }
#line 2672 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 31: // method.name: binary_op.raw
#line 283 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<MethodNameIndex>() = yystack_[0].value.as<SelectorIndex>();
                }
#line 2678 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 32: // method.base: method.name OPENCURLY argument_declarations.opt block.contents semicolon.opt
                         // CLOSECURLY
#line 288 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<MethodIndex>() = cxt.create(
                        Method {}, yylhs.location, yystack_[5].value.as<MethodNameIndex>(),
                        yystack_[3].value.as<DeclareArgumentListIndex>(), cxt.create(Missing {}, yystack_[4].location),
                        yystack_[2].value.as<BlockContentsListIndex>());
                }
#line 2684 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 33: // method.base: method.name OPENCURLY argument_declarations.opt CLOSECURLY
#line 290 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<MethodIndex>() = cxt.create(
                        Method {}, yylhs.location, yystack_[3].value.as<MethodNameIndex>(),
                        yystack_[1].value.as<DeclareArgumentListIndex>(), cxt.create(Missing {}, yystack_[2].location),
                        cxt.create(BlockList {}, yystack_[0].location));
                }
#line 2690 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 34: // method.base: method.name OPENCURLY argument_declarations.opt PRIMITIVENAME block.contents
                         // semicolon.opt CLOSECURLY
#line 292 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<MethodIndex>() =
                        cxt.create(Method {}, yylhs.location, yystack_[6].value.as<MethodNameIndex>(),
                                   yystack_[4].value.as<DeclareArgumentListIndex>(),
                                   cxt.create(PrimitiveIdentifier {}, yystack_[3].location),
                                   yystack_[2].value.as<BlockContentsListIndex>());
                }
#line 2696 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 35: // method.base: method.name OPENCURLY argument_declarations.opt PRIMITIVENAME CLOSECURLY
#line 294 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<MethodIndex>() =
                        cxt.create(Method {}, yylhs.location, yystack_[4].value.as<MethodNameIndex>(),
                                   yystack_[2].value.as<DeclareArgumentListIndex>(),
                                   cxt.create(PrimitiveIdentifier {}, yystack_[1].location),
                                   cxt.create(BlockList {}, yystack_[0].location));
                }
#line 2702 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 36: // method: method.base
#line 298 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<AnyMethodIndex>() = yystack_[0].value.as<MethodIndex>();
                }
#line 2708 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 37: // method: MULTIPLY method.base
#line 300 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<AnyMethodIndex>() = cxt.graph.cast<ClassMethod>(yystack_[0].value.as<MethodIndex>());
                }
#line 2714 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 38: // method.list: method
#line 304 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<MethodListIndex>() =
                        cxt.create(MethodList {}, yylhs.location, yystack_[0].value.as<AnyMethodIndex>());
                }
#line 2720 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 39: // method.list: method.list method
#line 305 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<MethodListIndex>() =
                        cxt.graph.append_to_list(yystack_[1].value.as<MethodListIndex>(), yylhs.location,
                                                 yystack_[0].value.as<AnyMethodIndex>());
                }
#line 2726 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 40: // method.list.opt: %empty
#line 309 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<MethodListIndex>() = cxt.create(MethodList {}, yylhs.location);
                }
#line 2732 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 41: // method.list.opt: method.list
#line 310 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<MethodListIndex>() = yystack_[0].value.as<MethodListIndex>();
                }
#line 2738 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 44: // block: block.open argument_declarations.opt block.contents semicolon.opt CLOSECURLY
#line 317 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<BlockIndex>() =
                        cxt.create(BlockNode {}, yylhs.location, yystack_[3].value.as<DeclareArgumentListIndex>(),
                                   yystack_[2].value.as<BlockContentsListIndex>());
                }
#line 2744 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 45: // block: block.open argument_declarations.opt CLOSECURLY
#line 319 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<BlockIndex>() =
                        cxt.create(BlockNode {}, yylhs.location, yystack_[1].value.as<DeclareArgumentListIndex>(),
                                   cxt.create(BlockContentsList {}, yylhs.location));
                }
#line 2750 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 46: // block.opt_list: %empty
#line 323 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<BlockListIndex>() = {};
                }
#line 2756 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 47: // block.opt_list: block.list
#line 324 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<BlockListIndex>() = yystack_[0].value.as<BlockListIndex>();
                }
#line 2762 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 48: // block.list: block
#line 328 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<BlockListIndex>() =
                        cxt.create(BlockList {}, yylhs.location, yystack_[0].value.as<BlockIndex>());
                }
#line 2768 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 49: // block.list: block.list block
#line 329 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<BlockListIndex>() = cxt.graph.append_to_list(
                        yystack_[1].value.as<BlockListIndex>(), yylhs.location, yystack_[0].value.as<BlockIndex>());
                }
#line 2774 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 50: // block.contents: block.contents.item
#line 333 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<BlockContentsListIndex>() =
                        cxt.create(BlockContentsList {}, yylhs.location, yystack_[0].value.as<BlockItemIndex>());
                }
#line 2780 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 51: // block.contents: block.contents SEMICOLON block.contents.item
#line 334 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<BlockContentsListIndex>() =
                        cxt.graph.append_to_list(yystack_[2].value.as<BlockContentsListIndex>(), yylhs.location,
                                                 yystack_[0].value.as<BlockItemIndex>());
                }
#line 2786 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 52: // block.contents.item: expr
#line 338 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<BlockItemIndex>() = yystack_[0].value.as<ExprSeqIndex>();
                }
#line 2792 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 53: // block.contents.item: variable_declarations
#line 339 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<BlockItemIndex>() = yystack_[0].value.as<DeclareVariableListIndex>();
                }
#line 2798 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 54: // block.contents.item: NONLOCALRETURN expr
#line 340 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<BlockItemIndex>() =
                        cxt.create(NonLocalReturnExpr {}, yylhs.location, yystack_[0].value.as<ExprSeqIndex>());
                }
#line 2804 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 55: // msgsend: OPENPAREN binary_op.no_adverb CLOSEPAREN OPENPAREN arguments CLOSEPAREN
                         // block.opt_list
#line 345 "langutils/sc_parser/src/sc_grammar.y"
                {
                    if (yystack_[0].value.as<BlockListIndex>()) {
                        cxt.graph.merge_list(yystack_[2].value.as<ArgumentListIndex>(),
                                             yystack_[0].value.as<BlockListIndex>());
                        cxt.graph.get_location(*yystack_[2].value.as<ArgumentListIndex>()) = {
                            yystack_[2].location.begin, yystack_[0].location.end
                        }; // spans arguments and block list
                    }
                    yylhs.value.as<ExprSeqIndex>() =
                        cxt.create(MessageNode {}, yylhs.location, yystack_[5].value.as<SelectorIndex>(),
                                   yystack_[2].value.as<ArgumentListIndex>());
                }
#line 2816 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 56: // msgsend: OPENPAREN binary_op.no_adverb CLOSEPAREN OPENPAREN CLOSEPAREN block.list
#line 354 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<ExprSeqIndex>() =
                        cxt.create(MessageNode {}, yylhs.location, yystack_[4].value.as<SelectorIndex>(),
                                   yystack_[0].value.as<BlockListIndex>());
                }
#line 2822 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 57: // msgsend: OPENPAREN binary_op.no_adverb CLOSEPAREN block.list
#line 357 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<ExprSeqIndex>() =
                        cxt.create(MessageNode {}, yylhs.location, yystack_[2].value.as<SelectorIndex>(),
                                   cxt.create(ArgumentList {}, yylhs.location, yystack_[0].value.as<BlockListIndex>()));
                }
#line 2828 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 58: // msgsend: name OPENPAREN arguments CLOSEPAREN block.opt_list
#line 361 "langutils/sc_parser/src/sc_grammar.y"
                {
                    if (yystack_[0].value.as<BlockListIndex>()) {
                        cxt.graph.merge_list(yystack_[2].value.as<ArgumentListIndex>(),
                                             yystack_[0].value.as<BlockListIndex>());
                        cxt.graph.get_location(*yystack_[2].value.as<ArgumentListIndex>()) = {
                            yystack_[2].location.begin, yystack_[0].location.end
                        }; // spans arguments and block list
                    }
                    yylhs.value.as<ExprSeqIndex>() =
                        cxt.create(MessageNode {}, yylhs.location, yystack_[4].value.as<NamedIdentifierIndex>(),
                                   yystack_[2].value.as<ArgumentListIndex>());
                }
#line 2840 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 59: // msgsend: name OPENPAREN CLOSEPAREN block.list
#line 370 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<ExprSeqIndex>() =
                        cxt.create(MessageNode {}, yylhs.location, yystack_[3].value.as<NamedIdentifierIndex>(),
                                   yystack_[0].value.as<BlockListIndex>());
                }
#line 2846 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 60: // msgsend: name block.list
#line 373 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<ExprSeqIndex>() = cxt.create(
                        MessageNode {}, yylhs.location, yystack_[1].value.as<NamedIdentifierIndex>(),
                        cxt.create(ArgumentList {}, yystack_[0].location, yystack_[0].value.as<BlockListIndex>()));
                }
#line 2852 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 61: // msgsend: expr DOT name arguments.maybe_paren block.opt_list
#line 376 "langutils/sc_parser/src/sc_grammar.y"
                {
                    if (yystack_[0].value.as<BlockListIndex>()) {
                        cxt.graph.merge_list(yystack_[1].value.as<ArgumentListIndex>(),
                                             yystack_[0].value.as<BlockListIndex>());
                        cxt.graph.get_location(*yystack_[1].value.as<ArgumentListIndex>()) = {
                            yystack_[1].location.begin, yystack_[0].location.end
                        }; // spans arguments and block list
                    }
                    cxt.graph.prepend_to_list(yystack_[1].value.as<ArgumentListIndex>(),
                                              yystack_[4].value.as<ExprSeqIndex>()); // put the receiver in place
                    cxt.graph.get_location(*yystack_[1].value.as<ArgumentListIndex>()) = {
                        yystack_[4].location.begin, yystack_[1].location.end
                    }; // spans arguments and block list
                    yylhs.value.as<ExprSeqIndex>() =
                        cxt.create(MessageNode {}, yylhs.location, yystack_[2].value.as<NamedIdentifierIndex>(),
                                   yystack_[1].value.as<ArgumentListIndex>());
                }
#line 2866 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 62: // msgsend: expr DOT arguments.paren block.opt_list
#line 387 "langutils/sc_parser/src/sc_grammar.y"
                {
                    if (yystack_[0].value.as<BlockListIndex>()) {
                        cxt.graph.merge_list(yystack_[1].value.as<ArgumentListIndex>(),
                                             yystack_[0].value.as<BlockListIndex>());
                        cxt.graph.get_location(*yystack_[1].value.as<ArgumentListIndex>()) = {
                            yystack_[1].location.begin, yystack_[0].location.end
                        }; // spans arguments and block list
                    }
                    cxt.graph.prepend_to_list(yystack_[1].value.as<ArgumentListIndex>(),
                                              yystack_[3].value.as<ExprSeqIndex>()); // put the receiver in place
                    cxt.graph.get_location(*yystack_[1].value.as<ArgumentListIndex>()) = {
                        yystack_[3].location.begin, yystack_[1].location.end
                    }; // spans arguments and block list
                    yylhs.value.as<ExprSeqIndex>() = cxt.create(
                        MessageNode { MessageNode::SelectorMode::Value }, yylhs.location,
                        cxt.create(Missing {}, yystack_[2].location), yystack_[1].value.as<ArgumentListIndex>());
                }
#line 2880 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 63: // msgsend: expr DOT OPENPAREN CLOSEPAREN block.opt_list
#line 397 "langutils/sc_parser/src/sc_grammar.y"
                {
                    auto args = cxt.create(ArgumentList {}, yylhs.location, yystack_[4].value.as<ExprSeqIndex>());
                    if (yystack_[0].value.as<BlockListIndex>())
                        cxt.graph.merge_list(args, yystack_[0].value.as<BlockListIndex>());
                    yylhs.value.as<ExprSeqIndex>() =
                        cxt.create(MessageNode { MessageNode::SelectorMode::Value }, yylhs.location,
                                   cxt.create(Missing {}, yystack_[3].location), args);
                }
#line 2890 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 64: // msgsend: expr DOT error
#line 405 "langutils/sc_parser/src/sc_grammar.y"
                {
                    auto unexpected = cxt.consume_error();
                    std::cout << "GOT AN ERROR WITH A DOT" << std::endl;
                    yylhs.value.as<ExprSeqIndex>() = yystack_[2].value.as<ExprSeqIndex>();
                }
#line 2900 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 65: // msgsend: CLASSNAME OPENSQUARE literal.array.contents CLOSESQUARE
#line 412 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<ExprSeqIndex>() = cxt.create(
                        CollectionNode {}, yylhs.location, cxt.create(ClassNameIdentifier {}, yystack_[3].location),
                        yystack_[1].value.as<ArrayIndex>());
                }
#line 2906 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 66: // msgsend: CLASSNAME block.list
#line 415 "langutils/sc_parser/src/sc_grammar.y"
                {
                    auto args = cxt.create(ArgumentList {}, yylhs.location,
                                           cxt.create(NamedIdentifier {}, yystack_[1].location));
                    cxt.graph.merge_list(args, yystack_[0].value.as<BlockListIndex>());
                    yylhs.value.as<ExprSeqIndex>() =
                        cxt.create(MessageNode {}, yylhs.location, cxt.create(Missing {}, yystack_[1].location), args);
                }
#line 2916 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 67: // msgsend: CLASSNAME OPENPAREN arguments CLOSEPAREN block.opt_list
#line 421 "langutils/sc_parser/src/sc_grammar.y"
                {
                    if (yystack_[0].value.as<BlockListIndex>()) {
                        cxt.graph.merge_list(yystack_[2].value.as<ArgumentListIndex>(),
                                             yystack_[0].value.as<BlockListIndex>());
                        cxt.graph.get_location(*yystack_[2].value.as<ArgumentListIndex>()) = {
                            yystack_[2].location.begin, yystack_[0].location.end
                        }; // spans arguments and block list
                    }
                    cxt.graph.prepend_to_list(
                        yystack_[2].value.as<ArgumentListIndex>(),
                        cxt.create(ClassNameIdentifier {}, yystack_[4].location)); // put the receiver in place
                    cxt.graph.get_location(*yystack_[2].value.as<ArgumentListIndex>()) = {
                        yystack_[2].location.begin, yystack_[0].location.end
                    }; // spans arguments and block list
                    yylhs.value.as<ExprSeqIndex>() = cxt.create(
                        MessageNode { MessageNode::SelectorMode::New }, yylhs.location,
                        cxt.create(Missing {}, yystack_[4].location), yystack_[2].value.as<ArgumentListIndex>());
                }
#line 2930 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 68: // msgsend: CLASSNAME OPENPAREN CLOSEPAREN block.opt_list
#line 432 "langutils/sc_parser/src/sc_grammar.y"
                {
                    auto args = cxt.create(ArgumentList {}, yylhs.location, yystack_[0].value.as<BlockListIndex>());
                    cxt.graph.prepend_to_list(
                        args, cxt.create(ClassNameIdentifier {}, yystack_[3].location)); // put the receiver in place
                    yylhs.value.as<ExprSeqIndex>() =
                        cxt.create(MessageNode { MessageNode::SelectorMode::New }, yylhs.location,
                                   cxt.create(Missing {}, yystack_[3].location), args);
                }
#line 2940 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 69: // expr.base: literal
#line 441 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<ExprSeqIndex>() = yystack_[0].value.as<AnyLiteralIndex>();
                }
#line 2946 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 70: // expr.base: name
#line 443 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<ExprSeqIndex>() = yystack_[0].value.as<NamedIdentifierIndex>();
                }
#line 2952 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 71: // expr.base: msgsend
#line 445 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<ExprSeqIndex>() = yystack_[0].value.as<ExprSeqIndex>();
                }
#line 2958 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 72: // expr.base: OPENPAREN expr.seq CLOSEPAREN
#line 447 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<ExprSeqIndex>() = yystack_[1].value.as<ExprSeqIndex>();
                }
#line 2964 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 73: // expr.base: TILDE name
#line 448 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<ExprSeqIndex>() =
                        cxt.create(EnvIdentifierNode {}, yylhs.location, yystack_[0].value.as<NamedIdentifierIndex>());
                }
#line 2970 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 74: // expr.base: expr.base OPENSQUARE arguments CLOSESQUARE
#line 454 "langutils/sc_parser/src/sc_grammar.y"
                {
                    cxt.graph.prepend_to_list(yystack_[1].value.as<ArgumentListIndex>(),
                                              yystack_[3].value.as<ExprSeqIndex>()); // put receiver in place.
                    cxt.graph.get_location(*yystack_[1].value.as<ArgumentListIndex>()) = yylhs.location;
                    yylhs.value.as<ExprSeqIndex>() = cxt.create(
                        MessageNode { MessageNode::SelectorMode::At }, yylhs.location,
                        cxt.create(Missing {}, yystack_[2].location), yystack_[1].value.as<ArgumentListIndex>());
                }
#line 2980 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 75: // expr.base: expr.base OPENSQUARE CLOSESQUARE
#line 461 "langutils/sc_parser/src/sc_grammar.y"
                {
                    auto args = cxt.create(ArgumentList {}, yylhs.location, yystack_[2].value.as<ExprSeqIndex>());
                    yylhs.value.as<ExprSeqIndex>() =
                        cxt.create(MessageNode { MessageNode::SelectorMode::At }, yylhs.location,
                                   cxt.create(Missing {}, yystack_[1].location), args);
                }
#line 2989 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 76: // expr: expr.base
#line 470 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<ExprSeqIndex>() = yystack_[0].value.as<ExprSeqIndex>();
                }
#line 2995 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 77: // expr: CLASSNAME
#line 474 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<ExprSeqIndex>() = cxt.create(ClassNameIdentifier {}, yylhs.location);
                }
#line 3001 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 78: // expr: expr DOT OPENSQUARE arguments CLOSESQUARE
#line 477 "langutils/sc_parser/src/sc_grammar.y"
                {
                    cxt.graph.prepend_to_list(yystack_[1].value.as<ArgumentListIndex>(),
                                              yystack_[4].value.as<ExprSeqIndex>()); // put receiver in place
                    yylhs.value.as<ExprSeqIndex>() =
                        cxt.create(MessageNode { MessageNode::SelectorMode::At }, yylhs.location,
                                   cxt.create(Missing {}, yylhs.location), yystack_[1].value.as<ArgumentListIndex>());
                }
#line 3010 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 79: // expr: expr DOT OPENSQUARE CLOSESQUARE
#line 482 "langutils/sc_parser/src/sc_grammar.y"
                {
                    auto args = cxt.create(ArgumentList {}, yylhs.location, yystack_[3].value.as<ExprSeqIndex>());
                    yylhs.value.as<ExprSeqIndex>() =
                        cxt.create(MessageNode { MessageNode::SelectorMode::At }, yylhs.location,
                                   cxt.create(Missing {}, yystack_[1].location), args);
                }
#line 3019 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 80: // expr: BACKTICK expr
#line 487 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<ExprSeqIndex>() =
                        cxt.create(ReferenceNode {}, yylhs.location, yystack_[0].value.as<ExprSeqIndex>());
                }
#line 3025 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 81: // expr: expr binary_op expr
#line 490 "langutils/sc_parser/src/sc_grammar.y"
                {
                    auto args = cxt.create(ArgumentList {}, yylhs.location, yystack_[2].value.as<ExprSeqIndex>(),
                                           yystack_[0].value.as<ExprSeqIndex>());
                    yylhs.value.as<ExprSeqIndex>() = cxt.create(MessageNode {}, yylhs.location,
                                                                yystack_[1].value.as<SelectorMaybeAdverbIndex>(), args);
                }
#line 3034 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 82: // expr: name EQUALSSIGN expr
#line 496 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<ExprSeqIndex>() =
                        cxt.create(AssignmentNode {}, yylhs.location, yystack_[2].value.as<NamedIdentifierIndex>(),
                                   yystack_[0].value.as<ExprSeqIndex>());
                }
#line 3040 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 83: // expr: TILDE name EQUALSSIGN expr
#line 499 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<ExprSeqIndex>() =
                        cxt.create(AssignmentNode { AssignmentNode::Target::Environment }, yylhs.location,
                                   yystack_[2].value.as<NamedIdentifierIndex>(), yystack_[0].value.as<ExprSeqIndex>());
                }
#line 3046 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 84: // expr: expr DOT name EQUALSSIGN expr
#line 502 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<ExprSeqIndex>() =
                        cxt.create(SetterNode {}, yylhs.location, yystack_[4].value.as<ExprSeqIndex>(),
                                   yystack_[2].value.as<NamedIdentifierIndex>(), yystack_[0].value.as<ExprSeqIndex>());
                }
#line 3052 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 85: // expr: name OPENPAREN arguments CLOSEPAREN EQUALSSIGN expr
#line 505 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<ExprSeqIndex>() =
                        cxt.create(SetterNode {}, yylhs.location, yystack_[3].value.as<ArgumentListIndex>(),
                                   yystack_[5].value.as<NamedIdentifierIndex>(), yystack_[0].value.as<ExprSeqIndex>());
                }
#line 3058 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 86: // expr: expr.base OPENSQUARE arguments CLOSESQUARE EQUALSSIGN expr
#line 513 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<ExprSeqIndex>() =
                        cxt.create(AssignmentAtNode {}, yylhs.location, yystack_[5].value.as<ExprSeqIndex>(),
                                   yystack_[3].value.as<ArgumentListIndex>(), yystack_[0].value.as<ExprSeqIndex>());
                }
#line 3064 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 87: // expr: expr.base OPENSQUARE CLOSESQUARE EQUALSSIGN expr
#line 515 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<ExprSeqIndex>() = cxt.create(
                        AssignmentAtNode {}, yylhs.location, yystack_[4].value.as<ExprSeqIndex>(),
                        cxt.create(ArgumentList {}, yystack_[3].location), yystack_[0].value.as<ExprSeqIndex>());
                }
#line 3070 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 88: // expr: expr DOT OPENSQUARE arguments CLOSESQUARE EQUALSSIGN expr
#line 518 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<ExprSeqIndex>() =
                        cxt.create(AssignmentAtNode {}, yylhs.location, yystack_[6].value.as<ExprSeqIndex>(),
                                   yystack_[3].value.as<ArgumentListIndex>(), yystack_[0].value.as<ExprSeqIndex>());
                }
#line 3076 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 89: // expr: expr DOT OPENSQUARE CLOSESQUARE EQUALSSIGN expr
#line 521 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<ExprSeqIndex>() = cxt.create(
                        AssignmentAtNode {}, yylhs.location, yystack_[5].value.as<ExprSeqIndex>(),
                        cxt.create(ArgumentList {}, yystack_[3].location), yystack_[0].value.as<ExprSeqIndex>());
                }
#line 3082 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 90: // expr.seq.base: expr
#line 525 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<ExprSeqIndex>() = yystack_[0].value.as<ExprSeqIndex>();
                }
#line 3088 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 91: // expr.seq.base: expr.seq.base SEMICOLON expr
#line 527 "langutils/sc_parser/src/sc_grammar.y"
                {
                    // This piece of logic is here because exprs can contain expr.seq, so we avoid creating the list
                    // node if we can.
                    if (cxt.graph.is_a<ExprSeqIndex>(*yystack_[2].value.as<ExprSeqIndex>())) {
                        cxt.graph.get_location(*yystack_[2].value.as<ExprSeqIndex>()) =
                            yylhs.location; // updates the location of the list
                        cxt.graph.append_to_list(yystack_[2].value.as<ExprSeqIndex>(),
                                                 yystack_[0].value.as<ExprSeqIndex>()); // appends to the list
                        yylhs.value.as<ExprSeqIndex>() = yystack_[2].value.as<ExprSeqIndex>();
                    } else {
                        yylhs.value.as<ExprSeqIndex>() =
                            cxt.create(ExprSeq {}, yylhs.location, yystack_[2].value.as<ExprSeqIndex>(),
                                       yystack_[0].value.as<ExprSeqIndex>());
                    }
                }
#line 3103 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 92: // expr.seq: expr.seq.base semicolon.opt
#line 539 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<ExprSeqIndex>() = yystack_[1].value.as<ExprSeqIndex>();
                }
#line 3109 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 93: // adverb: DOT name
#line 542 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<AdverbIndex>() = yystack_[0].value.as<NamedIdentifierIndex>();
                }
#line 3115 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 94: // adverb: DOT integer
#line 543 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<AdverbIndex>() = yystack_[0].value.as<IntLitIndex>();
                }
#line 3121 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 95: // adverb: DOT OPENPAREN expr.seq CLOSEPAREN
#line 544 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<AdverbIndex>() =
                        cxt.create(AdverbExprNode {}, yylhs.location, yystack_[1].value.as<ExprSeqIndex>());
                }
#line 3127 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 96: // argument_declarations.list: name
#line 550 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<DeclareArgumentListIndex>() = cxt.create(
                        DeclareArgumentList {}, yylhs.location, yystack_[0].value.as<NamedIdentifierIndex>());
                }
#line 3133 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 97: // argument_declarations.list: name EQUALSSIGN literal
#line 552 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<DeclareArgumentListIndex>() =
                        cxt.create(DeclareArgumentList {}, yylhs.location,
                                   cxt.create(DeclareArgumentWithDefaultNode { true }, yylhs.location,
                                              yystack_[2].value.as<NamedIdentifierIndex>(),
                                              yystack_[0].value.as<AnyLiteralIndex>()));
                }
#line 3139 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 98: // argument_declarations.list: name OPENPAREN expr.seq CLOSEPAREN
#line 554 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<DeclareArgumentListIndex>() = cxt.create(
                        DeclareArgumentList {}, yylhs.location,
                        cxt.create(DeclareArgumentWithDefaultNode { false }, yylhs.location,
                                   yystack_[3].value.as<NamedIdentifierIndex>(), yystack_[1].value.as<ExprSeqIndex>()));
                }
#line 3145 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 99: // argument_declarations.list: name EQUALSSIGN OPENPAREN expr.seq CLOSEPAREN
#line 556 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<DeclareArgumentListIndex>() = cxt.create(
                        DeclareArgumentList {}, yylhs.location,
                        cxt.create(DeclareArgumentWithDefaultNode { false }, yylhs.location,
                                   yystack_[4].value.as<NamedIdentifierIndex>(), yystack_[1].value.as<ExprSeqIndex>()));
                }
#line 3151 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 100: // argument_declarations.list: argument_declarations.list COMMA name
#line 558 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<DeclareArgumentListIndex>() =
                        cxt.graph.append_to_list(yystack_[2].value.as<DeclareArgumentListIndex>(), yylhs.location,
                                                 yystack_[0].value.as<NamedIdentifierIndex>());
                }
#line 3157 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 101: // argument_declarations.list: argument_declarations.list COMMA name EQUALSSIGN literal
#line 560 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<DeclareArgumentListIndex>() =
                        cxt.graph.append_to_list(yystack_[4].value.as<DeclareArgumentListIndex>(), yylhs.location,
                                                 cxt.create(DeclareArgumentWithDefaultNode { true }, yylhs.location,
                                                            yystack_[2].value.as<NamedIdentifierIndex>(),
                                                            yystack_[0].value.as<AnyLiteralIndex>()));
                }
#line 3163 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 102: // argument_declarations.list: argument_declarations.list COMMA name OPENPAREN expr.seq
                          // CLOSEPAREN
#line 562 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<DeclareArgumentListIndex>() = cxt.graph.append_to_list(
                        yystack_[5].value.as<DeclareArgumentListIndex>(), yylhs.location,
                        cxt.create(DeclareArgumentWithDefaultNode { false }, yylhs.location,
                                   yystack_[3].value.as<NamedIdentifierIndex>(), yystack_[1].value.as<ExprSeqIndex>()));
                }
#line 3169 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 103: // argument_declarations.list: argument_declarations.list COMMA name EQUALSSIGN OPENPAREN
                          // expr.seq CLOSEPAREN
#line 564 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<DeclareArgumentListIndex>() = cxt.graph.append_to_list(
                        yystack_[6].value.as<DeclareArgumentListIndex>(), yylhs.location,
                        cxt.create(DeclareArgumentWithDefaultNode { false }, yylhs.location,
                                   yystack_[4].value.as<NamedIdentifierIndex>(), yystack_[1].value.as<ExprSeqIndex>()));
                }
#line 3175 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 104: // argument_declarations.pipelist: name literal
#line 569 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<DeclareArgumentListIndex>() =
                        cxt.create(DeclareArgumentList {}, yylhs.location,
                                   cxt.create(DeclareArgumentWithDefaultNode { true }, yylhs.location,
                                              yystack_[1].value.as<NamedIdentifierIndex>(),
                                              yystack_[0].value.as<AnyLiteralIndex>()));
                }
#line 3181 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 105: // argument_declarations.pipelist: name
#line 571 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<DeclareArgumentListIndex>() = cxt.create(
                        DeclareArgumentList {}, yylhs.location, yystack_[0].value.as<NamedIdentifierIndex>());
                }
#line 3187 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 106: // argument_declarations.pipelist: name EQUALSSIGN literal
#line 573 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<DeclareArgumentListIndex>() =
                        cxt.create(DeclareArgumentList {}, yylhs.location,
                                   cxt.create(DeclareArgumentWithDefaultNode { true }, yylhs.location,
                                              yystack_[2].value.as<NamedIdentifierIndex>(),
                                              yystack_[0].value.as<AnyLiteralIndex>()));
                }
#line 3193 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 107: // argument_declarations.pipelist: name OPENPAREN expr.seq CLOSEPAREN
#line 575 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<DeclareArgumentListIndex>() = cxt.create(
                        DeclareArgumentList {}, yylhs.location,
                        cxt.create(DeclareArgumentWithDefaultNode { false }, yylhs.location,
                                   yystack_[3].value.as<NamedIdentifierIndex>(), yystack_[1].value.as<ExprSeqIndex>()));
                }
#line 3199 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 108: // argument_declarations.pipelist: name EQUALSSIGN OPENPAREN expr.seq CLOSEPAREN
#line 577 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<DeclareArgumentListIndex>() = cxt.create(
                        DeclareArgumentList {}, yylhs.location,
                        cxt.create(DeclareArgumentWithDefaultNode { false }, yylhs.location,
                                   yystack_[4].value.as<NamedIdentifierIndex>(), yystack_[1].value.as<ExprSeqIndex>()));
                }
#line 3205 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 109: // argument_declarations.pipelist: argument_declarations.pipelist comma.opt name literal
#line 579 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<DeclareArgumentListIndex>() =
                        cxt.graph.append_to_list(yystack_[3].value.as<DeclareArgumentListIndex>(), yylhs.location,
                                                 cxt.create(DeclareArgumentWithDefaultNode { true }, yylhs.location,
                                                            yystack_[1].value.as<NamedIdentifierIndex>(),
                                                            yystack_[0].value.as<AnyLiteralIndex>()));
                }
#line 3211 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 110: // argument_declarations.pipelist: argument_declarations.pipelist comma.opt name
#line 581 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<DeclareArgumentListIndex>() =
                        cxt.graph.append_to_list(yystack_[2].value.as<DeclareArgumentListIndex>(), yylhs.location,
                                                 yystack_[0].value.as<NamedIdentifierIndex>());
                }
#line 3217 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 111: // argument_declarations.pipelist: argument_declarations.pipelist comma.opt name EQUALSSIGN
                          // literal
#line 583 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<DeclareArgumentListIndex>() =
                        cxt.graph.append_to_list(yystack_[4].value.as<DeclareArgumentListIndex>(), yylhs.location,
                                                 cxt.create(DeclareArgumentWithDefaultNode { true }, yylhs.location,
                                                            yystack_[2].value.as<NamedIdentifierIndex>(),
                                                            yystack_[0].value.as<AnyLiteralIndex>()));
                }
#line 3223 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 112: // argument_declarations.pipelist: argument_declarations.pipelist comma.opt name OPENPAREN
                          // expr.seq CLOSEPAREN
#line 585 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<DeclareArgumentListIndex>() = cxt.graph.append_to_list(
                        yystack_[5].value.as<DeclareArgumentListIndex>(), yylhs.location,
                        cxt.create(DeclareArgumentWithDefaultNode { false }, yylhs.location,
                                   yystack_[3].value.as<NamedIdentifierIndex>(), yystack_[1].value.as<ExprSeqIndex>()));
                }
#line 3229 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 113: // argument_declarations.pipelist: argument_declarations.pipelist comma.opt name EQUALSSIGN
                          // OPENPAREN expr.seq CLOSEPAREN
#line 587 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<DeclareArgumentListIndex>() = cxt.graph.append_to_list(
                        yystack_[6].value.as<DeclareArgumentListIndex>(), yylhs.location,
                        cxt.create(DeclareArgumentWithDefaultNode { false }, yylhs.location,
                                   yystack_[4].value.as<NamedIdentifierIndex>(), yystack_[1].value.as<ExprSeqIndex>()));
                }
#line 3235 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 114: // argument_declarations: ARG SEMICOLON
#line 591 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<DeclareArgumentListIndex>() = cxt.create(DeclareArgumentList {}, yylhs.location);
                }
#line 3241 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 115: // argument_declarations: ARG argument_declarations.list comma.opt SEMICOLON
#line 592 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<DeclareArgumentListIndex>() = yystack_[2].value.as<DeclareArgumentListIndex>();
                }
#line 3247 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 116: // argument_declarations: ARG argument_declarations.list ELLIPSIS name SEMICOLON
#line 594 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<DeclareArgumentListIndex>() =
                        cxt.graph.append_to_list(yystack_[3].value.as<DeclareArgumentListIndex>(), yylhs.location,
                                                 cxt.create(DeclareArgumentVariadicNode {}, yystack_[1].location,
                                                            yystack_[1].value.as<NamedIdentifierIndex>()));
                }
#line 3253 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 117: // argument_declarations: ARG argument_declarations.list ELLIPSIS name COMMA name SEMICOLON
#line 596 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<DeclareArgumentListIndex>() =
                        cxt.graph.append_to_list(yystack_[5].value.as<DeclareArgumentListIndex>(), yylhs.location,
                                                 cxt.create(DeclareArgumentVariadicNode {}, yystack_[3].location,
                                                            yystack_[3].value.as<NamedIdentifierIndex>()),
                                                 cxt.create(DeclareArgumentVariadicNode {}, yystack_[1].location,
                                                            yystack_[1].value.as<NamedIdentifierIndex>()));
                }
#line 3259 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 118: // argument_declarations: PIPE PIPE
#line 598 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<DeclareArgumentListIndex>() = cxt.create(DeclareArgumentList {}, yylhs.location);
                }
#line 3265 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 119: // argument_declarations: PIPE argument_declarations.pipelist comma.opt PIPE
#line 600 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<DeclareArgumentListIndex>() = yystack_[2].value.as<DeclareArgumentListIndex>();
                }
#line 3271 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 120: // argument_declarations: PIPE argument_declarations.pipelist ELLIPSIS name PIPE
#line 602 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<DeclareArgumentListIndex>() =
                        cxt.graph.append_to_list(yystack_[3].value.as<DeclareArgumentListIndex>(), yystack_[3].location,
                                                 cxt.create(DeclareArgumentVariadicNode {}, yystack_[1].location,
                                                            yystack_[1].value.as<NamedIdentifierIndex>()));
                }
#line 3277 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 121: // argument_declarations: PIPE argument_declarations.pipelist ELLIPSIS name COMMA name PIPE
#line 604 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<DeclareArgumentListIndex>() =
                        cxt.graph.append_to_list(yystack_[5].value.as<DeclareArgumentListIndex>(), yylhs.location,
                                                 cxt.create(DeclareArgumentVariadicNode {}, yystack_[3].location,
                                                            yystack_[3].value.as<NamedIdentifierIndex>()),
                                                 cxt.create(DeclareArgumentVariadicNode {}, yystack_[1].location,
                                                            yystack_[1].value.as<NamedIdentifierIndex>()));
                }
#line 3283 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 122: // argument_declarations.opt: %empty
#line 609 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<DeclareArgumentListIndex>() = cxt.create(DeclareArgumentList {}, yylhs.location);
                }
#line 3289 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 123: // argument_declarations.opt: argument_declarations
#line 610 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<DeclareArgumentListIndex>() = yystack_[0].value.as<DeclareArgumentListIndex>();
                }
#line 3295 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 124: // variable_declarations.list.item: name
#line 615 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<DeclareAnyVariableIndex>() = yystack_[0].value.as<NamedIdentifierIndex>();
                }
#line 3301 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 125: // variable_declarations.list.item: name EQUALSSIGN expr
#line 617 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<DeclareAnyVariableIndex>() =
                        cxt.create(DeclareVariableWithDefaultNode {}, yylhs.location,
                                   yystack_[2].value.as<NamedIdentifierIndex>(), yystack_[0].value.as<ExprSeqIndex>());
                }
#line 3307 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 126: // variable_declarations.list.item: name OPENPAREN expr.seq CLOSEPAREN
#line 619 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<DeclareAnyVariableIndex>() =
                        cxt.create(DeclareVariableWithDefaultNode {}, yylhs.location,
                                   yystack_[3].value.as<NamedIdentifierIndex>(), yystack_[1].value.as<ExprSeqIndex>());
                }
#line 3313 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 127: // variable_declarations.list: variable_declarations.list.item
#line 628 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<DeclareVariableListIndex>() = cxt.create(
                        DeclareVariableList {}, yylhs.location, yystack_[0].value.as<DeclareAnyVariableIndex>());
                }
#line 3319 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 128: // variable_declarations.list: variable_declarations.list COMMA
                          // variable_declarations.list.item
#line 630 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<DeclareVariableListIndex>() =
                        cxt.graph.append_to_list(yystack_[2].value.as<DeclareVariableListIndex>(),
                                                 yystack_[0].value.as<DeclareAnyVariableIndex>());
                }
#line 3325 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 129: // variable_declarations: VAR variable_declarations.list comma.opt
#line 633 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<DeclareVariableListIndex>() = yystack_[1].value.as<DeclareVariableListIndex>();
                }
#line 3331 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 130: // arguments.entries: KEYBINOP expr.seq
#line 637 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<ArgumentEntryIndex>() =
                        cxt.create(KwArgNode {}, yylhs.location,
                                   cxt.create(SymbolNode { SymbolNode::Kind::KeyBinOp }, yystack_[1].location),
                                   yystack_[0].value.as<ExprSeqIndex>());
                }
#line 3337 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 131: // arguments.entries: MULTIPLY expr.seq
#line 638 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<ArgumentEntryIndex>() =
                        cxt.create(VariadicArgNode {}, yylhs.location, yystack_[0].value.as<ExprSeqIndex>());
                }
#line 3343 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 132: // arguments.entries: expr.seq
#line 639 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<ArgumentEntryIndex>() = yystack_[0].value.as<ExprSeqIndex>();
                }
#line 3349 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 133: // arguments.no_trailing: arguments.entries
#line 643 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<ArgumentListIndex>() =
                        cxt.create(ArgumentList {}, yylhs.location, yystack_[0].value.as<ArgumentEntryIndex>());
                }
#line 3355 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 134: // arguments.no_trailing: arguments.no_trailing COMMA arguments.entries
#line 644 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<ArgumentListIndex>() =
                        cxt.graph.append_to_list(yystack_[2].value.as<ArgumentListIndex>(), yylhs.location,
                                                 yystack_[0].value.as<ArgumentEntryIndex>());
                }
#line 3361 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 135: // arguments: arguments.no_trailing comma.opt
#line 647 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<ArgumentListIndex>() = yystack_[1].value.as<ArgumentListIndex>();
                }
#line 3367 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 136: // arguments.paren: OPENPAREN arguments CLOSEPAREN
#line 650 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<ArgumentListIndex>() = yystack_[1].value.as<ArgumentListIndex>();
                }
#line 3373 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 137: // arguments.maybe_paren: %empty
#line 653 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<ArgumentListIndex>() = cxt.create(ArgumentList {}, yylhs.location);
                }
#line 3379 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 138: // arguments.maybe_paren: arguments.paren
#line 654 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<ArgumentListIndex>() = yystack_[0].value.as<ArgumentListIndex>();
                }
#line 3385 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 139: // literal.terminal: symbol
#line 658 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<AnyLiteralIndex>() = yystack_[0].value.as<SymbolLitIndex>();
                }
#line 3391 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 140: // literal.terminal: string
#line 659 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<AnyLiteralIndex>() = yystack_[0].value.as<StringLitIndex>();
                }
#line 3397 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 141: // literal.terminal: integer
#line 660 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<AnyLiteralIndex>() = yystack_[0].value.as<IntLitIndex>();
                }
#line 3403 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 142: // literal.terminal: float
#line 661 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<AnyLiteralIndex>() = yystack_[0].value.as<FloatProducingIndex>();
                }
#line 3409 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 143: // literal.terminal: boolean
#line 662 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<AnyLiteralIndex>() = yystack_[0].value.as<BooleanLitIndex>();
                }
#line 3415 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 144: // literal.terminal: nil
#line 663 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<AnyLiteralIndex>() = yystack_[0].value.as<NilLitIndex>();
                }
#line 3421 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 145: // literal.terminal: ascii
#line 664 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<AnyLiteralIndex>() = yystack_[0].value.as<ASCIIIndex>();
                }
#line 3427 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 146: // literal.terminal: block
#line 665 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<AnyLiteralIndex>() = yystack_[0].value.as<BlockIndex>();
                }
#line 3433 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 147: // literal.array.contents: %empty
#line 670 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<ArrayIndex>() = cxt.create(ArrayNode {}, yylhs.location);
                }
#line 3439 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 148: // literal.array.contents: expr.seq
#line 672 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<ArrayIndex>() =
                        cxt.create(ArrayNode {}, yylhs.location, yystack_[0].value.as<ExprSeqIndex>());
                }
#line 3445 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 149: // literal.array.contents: expr.seq COLON expr.seq
#line 674 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<ArrayIndex>() =
                        cxt.create(ArrayNode {}, yylhs.location, yystack_[2].value.as<ExprSeqIndex>(),
                                   yystack_[0].value.as<ExprSeqIndex>());
                }
#line 3451 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 150: // literal.array.contents: KEYBINOP expr.seq
#line 676 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<ArrayIndex>() =
                        cxt.create(ArrayNode {}, yylhs.location,
                                   cxt.create(SymbolNode { SymbolNode::Kind::KeyBinOp }, yystack_[1].location),
                                   yystack_[0].value.as<ExprSeqIndex>());
                }
#line 3457 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 151: // literal.array.contents: literal.array.contents COMMA expr.seq
#line 678 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<ArrayIndex>() = cxt.graph.append_to_list(
                        yystack_[2].value.as<ArrayIndex>(), yylhs.location, yystack_[0].value.as<ExprSeqIndex>());
                }
#line 3463 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 152: // literal.array.contents: literal.array.contents COMMA expr.seq COLON expr.seq
#line 680 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<ArrayIndex>() = cxt.graph.append_to_list(
                        yystack_[4].value.as<ArrayIndex>(), yylhs.location, yystack_[2].value.as<ExprSeqIndex>(),
                        yystack_[0].value.as<ExprSeqIndex>());
                }
#line 3469 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 153: // literal.array.contents: literal.array.contents COMMA KEYBINOP expr.seq
#line 682 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<ArrayIndex>() = cxt.graph.append_to_list(
                        yystack_[3].value.as<ArrayIndex>(), yylhs.location,
                        cxt.create(SymbolNode { SymbolNode::Kind::KeyBinOp }, yystack_[1].location),
                        yystack_[0].value.as<ExprSeqIndex>());
                }
#line 3475 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 154: // literal.dictionary.entry: expr.seq COLON expr.seq
#line 687 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<DictionaryEntryIndex>() =
                        cxt.create(DictionaryEntryNode {}, yylhs.location, yystack_[2].value.as<ExprSeqIndex>(),
                                   yystack_[0].value.as<ExprSeqIndex>());
                }
#line 3481 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 155: // literal.dictionary.entry: KEYBINOP expr.seq
#line 689 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<DictionaryEntryIndex>() =
                        cxt.create(DictionaryEntryNode {}, yylhs.location,
                                   cxt.create(SymbolNode { SymbolNode::Kind::KeyBinOp }, yystack_[1].location),
                                   yystack_[0].value.as<ExprSeqIndex>());
                }
#line 3487 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 156: // literal.dictionary.entries: %empty
#line 694 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<DictionaryIndex>() = cxt.create(DictionaryNode {}, yylhs.location);
                }
#line 3493 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 157: // literal.dictionary.entries: literal.dictionary.entry
#line 696 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<DictionaryIndex>() =
                        cxt.create(DictionaryNode {}, yylhs.location, yystack_[0].value.as<DictionaryEntryIndex>());
                }
#line 3499 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 158: // literal.dictionary.entries: literal.dictionary.entries COMMA literal.dictionary.entry
#line 698 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<DictionaryIndex>() =
                        cxt.graph.append_to_list(yystack_[2].value.as<DictionaryIndex>(), yylhs.location,
                                                 yystack_[0].value.as<DictionaryEntryIndex>());
                }
#line 3505 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 159: // literal.dictionary: OPENPAREN literal.dictionary.entries comma.opt CLOSEPAREN
#line 703 "langutils/sc_parser/src/sc_grammar.y"
                {
                    cxt.graph.get_location(*yystack_[2].value.as<DictionaryIndex>()) = yylhs.location;
                    yylhs.value.as<DictionaryIndex>() = yystack_[2].value.as<DictionaryIndex>();
                }
#line 3511 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 160: // literal.array: OPENSQUARE literal.array.contents comma.opt CLOSESQUARE
#line 708 "langutils/sc_parser/src/sc_grammar.y"
                {
                    cxt.graph.get_location(*yystack_[2].value.as<ArrayIndex>()) = yylhs.location;
                    yylhs.value.as<ArrayIndex>() = yystack_[2].value.as<ArrayIndex>();
                }
#line 3517 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 161: // literal.array: HASH OPENSQUARE literal.array.contents comma.opt CLOSESQUARE
#line 710 "langutils/sc_parser/src/sc_grammar.y"
                {
                    cxt.graph.get_payload(yystack_[2].value.as<ArrayIndex>()).is_immutable = true;
                    cxt.graph.get_location(*yystack_[2].value.as<ArrayIndex>()) = yylhs.location;
                    yylhs.value.as<ArrayIndex>() = yystack_[2].value.as<ArrayIndex>();
                }
#line 3527 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 162: // literal: literal.terminal
#line 718 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<AnyLiteralIndex>() = yystack_[0].value.as<AnyLiteralIndex>();
                }
#line 3533 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 163: // literal: literal.array
#line 719 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<AnyLiteralIndex>() = yystack_[0].value.as<ArrayIndex>();
                }
#line 3539 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 164: // literal: literal.dictionary
#line 720 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<AnyLiteralIndex>() = yystack_[0].value.as<DictionaryIndex>();
                }
#line 3545 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 165: // name: NAME
#line 723 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<NamedIdentifierIndex>() = cxt.create(NamedIdentifier {}, yylhs.location);
                }
#line 3551 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 166: // binary_op.raw: BINOP
#line 732 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<SelectorIndex>() = cxt.create(SelectorNode { false }, yylhs.location);
                }
#line 3557 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 167: // binary_op.raw: READWRITEVAR
#line 733 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<SelectorIndex>() = cxt.create(SelectorNode { false }, yylhs.location);
                }
#line 3563 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 168: // binary_op.raw: LESSTHAN
#line 734 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<SelectorIndex>() = cxt.create(SelectorNode { false }, yylhs.location);
                }
#line 3569 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 169: // binary_op.raw: GREATERTHAN
#line 735 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<SelectorIndex>() = cxt.create(SelectorNode { false }, yylhs.location);
                }
#line 3575 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 170: // binary_op.raw: MINUS
#line 736 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<SelectorIndex>() = cxt.create(SelectorNode { false }, yylhs.location);
                }
#line 3581 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 171: // binary_op.raw: MULTIPLY
#line 737 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<SelectorIndex>() = cxt.create(SelectorNode { false }, yylhs.location);
                }
#line 3587 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 172: // binary_op.raw: ADD
#line 738 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<SelectorIndex>() = cxt.create(SelectorNode { false }, yylhs.location);
                }
#line 3593 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 173: // binary_op.raw: PIPE
#line 739 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<SelectorIndex>() = cxt.create(SelectorNode { false }, yylhs.location);
                }
#line 3599 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 174: // binary_op.no_adverb: binary_op.raw
#line 743 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<SelectorIndex>() = yystack_[0].value.as<SelectorIndex>();
                }
#line 3605 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 175: // binary_op.no_adverb: KEYBINOP
#line 744 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<SelectorIndex>() = cxt.create(SelectorNode { true }, yylhs.location);
                }
#line 3611 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 176: // binary_op: binary_op.no_adverb adverb
#line 749 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<SelectorMaybeAdverbIndex>() =
                        cxt.create(SelectorWAdverb {}, yylhs.location, yystack_[1].value.as<SelectorIndex>(),
                                   yystack_[0].value.as<AdverbIndex>());
                }
#line 3617 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 177: // binary_op: binary_op.no_adverb
#line 751 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<SelectorMaybeAdverbIndex>() = yystack_[0].value.as<SelectorIndex>();
                }
#line 3623 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 182: // ascii: ASCII
#line 757 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<ASCIIIndex>() = cxt.create(ASCIINode {}, yylhs.location);
                }
#line 3629 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 183: // nil: NIL
#line 759 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<NilLitIndex>() = cxt.create(NilNode {}, yylhs.location);
                }
#line 3635 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 184: // boolean: TRUE
#line 762 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<BooleanLitIndex>() = cxt.create(BooleanNode { true }, yylhs.location);
                }
#line 3641 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 185: // boolean: FALSE
#line 763 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<BooleanLitIndex>() = cxt.create(BooleanNode { false }, yylhs.location);
                }
#line 3647 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 186: // symbol: SYMBOL_QUOTE
#line 767 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<SymbolLitIndex>() =
                        cxt.create(SymbolNode { SymbolNode::Kind::Quote }, yylhs.location);
                }
#line 3653 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 187: // symbol: SYMBOL_SLASH
#line 768 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<SymbolLitIndex>() =
                        cxt.create(SymbolNode { SymbolNode::Kind::Slash }, yylhs.location);
                }
#line 3659 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 188: // string: STRINGLINE
#line 772 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<StringLitIndex>() =
                        cxt.create(StringLineList {}, yylhs.location, cxt.create(StringLineNode {}, yylhs.location));
                }
#line 3665 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 189: // string: string STRINGLINE
#line 773 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<StringLitIndex>() = cxt.graph.append_to_list(
                        yystack_[1].value.as<StringLitIndex>(), cxt.create(StringLineNode {}, yystack_[0].location));
                }
#line 3671 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 190: // integer: INTEGER
#line 777 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<IntLitIndex>() = cxt.create(IntNode {}, yylhs.location);
                }
#line 3677 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 191: // integer: INTEGER_RADIX
#line 778 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<IntLitIndex>() = cxt.create(IntNode { IntNode::Kind::Radix }, yylhs.location);
                }
#line 3683 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 192: // integer: HEXADECIMAL
#line 779 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<IntLitIndex>() = cxt.create(IntNode { IntNode::Kind::Hexadecimal }, yylhs.location);
                }
#line 3689 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 193: // integer: MINUS integer
#line 781 "langutils/sc_parser/src/sc_grammar.y"
                {
                    // Reaches into the previous integer and changes its sign.
                    cxt.graph.get_payload(yystack_[0].value.as<IntLitIndex>()).sign = IntNode::Sign::Negative;
                    yylhs.value.as<IntLitIndex>() = yystack_[0].value.as<IntLitIndex>();
                }
#line 3699 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 194: // float.raw_unsigned: FLOAT
#line 789 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<FloatLitIndex>() = cxt.create(FloatNode {}, yylhs.location);
                }
#line 3705 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 195: // float.raw_unsigned: FLOAT_RADIX
#line 790 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<FloatLitIndex>() = cxt.create(FloatNode { FloatNode::Kind::Radix }, yylhs.location);
                }
#line 3711 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 196: // float.raw_unsigned: FLOAT_EXPONENT
#line 791 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<FloatLitIndex>() =
                        cxt.create(FloatNode { FloatNode::Kind::Exponent }, yylhs.location);
                }
#line 3717 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 197: // float.raw_unsigned: FLOAT_INF
#line 792 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<FloatLitIndex>() = cxt.create(FloatNode { FloatNode::Kind::Inf }, yylhs.location);
                }
#line 3723 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 198: // float.raw: float.raw_unsigned
#line 797 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<FloatLitIndex>() = yystack_[0].value.as<FloatLitIndex>();
                }
#line 3729 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 199: // float.raw: MINUS float.raw_unsigned
#line 799 "langutils/sc_parser/src/sc_grammar.y"
                {
                    cxt.graph.get_payload(yystack_[0].value.as<FloatLitIndex>()).sign = FloatNode::Sign::Negative;
                    yylhs.value.as<FloatLitIndex>() = yystack_[0].value.as<FloatLitIndex>();
                }
#line 3735 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 200: // accidental.unsigned: ACCIDENTAL_STEPS
#line 804 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<AccidentalLitIndex>() =
                        cxt.create(AccidentalNode { AccidentalNode::Kind::Steps }, yylhs.location);
                }
#line 3741 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 201: // accidental.unsigned: ACCIDENTAL_CENTS
#line 806 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<AccidentalLitIndex>() =
                        cxt.create(AccidentalNode { AccidentalNode::Kind::Cents }, yylhs.location);
                }
#line 3747 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 202: // accidental: accidental.unsigned
#line 811 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<AccidentalLitIndex>() = yystack_[0].value.as<AccidentalLitIndex>();
                }
#line 3753 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 203: // accidental: MINUS accidental.unsigned
#line 813 "langutils/sc_parser/src/sc_grammar.y"
                {
                    cxt.graph.get_payload(yystack_[0].value.as<AccidentalLitIndex>()).sign =
                        AccidentalNode::Sign::Negative;
                    yylhs.value.as<AccidentalLitIndex>() = yystack_[0].value.as<AccidentalLitIndex>();
                }
#line 3759 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 204: // float: float.raw
#line 817 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<FloatProducingIndex>() = yystack_[0].value.as<FloatLitIndex>();
                }
#line 3765 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 205: // float: accidental
#line 818 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<FloatProducingIndex>() = yystack_[0].value.as<AccidentalLitIndex>();
                }
#line 3771 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 206: // float: float.raw PI
#line 819 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<FloatProducingIndex>() =
                        cxt.create(PiNode {}, yylhs.location, yystack_[1].value.as<FloatLitIndex>());
                }
#line 3777 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 207: // float: integer PI
#line 820 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<FloatProducingIndex>() =
                        cxt.create(PiNode {}, yylhs.location, yystack_[1].value.as<IntLitIndex>());
                }
#line 3783 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 208: // float: PI
#line 821 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<FloatProducingIndex>() =
                        cxt.create(PiNode {}, yylhs.location, cxt.create(Missing {}, yylhs.location));
                }
#line 3789 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 209: // float: MINUS PI
#line 822 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<FloatProducingIndex>() = cxt.create(
                        PiNode { PiNode::Sign::Negative }, yylhs.location, cxt.create(Missing {}, yylhs.location));
                }
#line 3795 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 210: // accessor: %empty
#line 826 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<ReadWriteAccessor>() = ReadWriteAccessor::Private;
                }
#line 3801 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 211: // accessor: LESSTHAN
#line 827 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<ReadWriteAccessor>() = ReadWriteAccessor::PublicRead;
                }
#line 3807 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 212: // accessor: READWRITEVAR
#line 828 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<ReadWriteAccessor>() = ReadWriteAccessor::PublicReadAndWrite;
                }
#line 3813 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 213: // accessor: GREATERTHAN
#line 829 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<ReadWriteAccessor>() = ReadWriteAccessor::PublicWrite;
                }
#line 3819 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;


#line 3823 "langutils/sc_parser/src/sc_grammar_parser.cpp"

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
                                            "REGION_SEPARATOR",
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
                                            "CONST",
                                            "NIL",
                                            "TRUE",
                                            "FALSE",
                                            "PI",
                                            "ELLIPSIS",
                                            "DOTDOT",
                                            "BEGINCLOSEDFUNC",
                                            "BADTOKEN",
                                            "INTERPRET",
                                            "LEFTARROW",
                                            "LEXER_ERROR",
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
                                            "go",
                                            "region",
                                            "expr.error",
                                            "classOrExtList.list",
                                            "classOrExtList.item",
                                            "class",
                                            "class.super.opt",
                                            "class.slot.opt",
                                            "class.extension",
                                            "class.vars.entry.item",
                                            "class.vars.entry.list",
                                            "class.vars.entry",
                                            "class.vars",
                                            "class.vars.opt",
                                            "method.name",
                                            "method.base",
                                            "method",
                                            "method.list",
                                            "method.list.opt",
                                            "block.open",
                                            "block",
                                            "block.opt_list",
                                            "block.list",
                                            "block.contents",
                                            "block.contents.item",
                                            "msgsend",
                                            "expr.base",
                                            "expr",
                                            "expr.seq.base",
                                            "expr.seq",
                                            "adverb",
                                            "argument_declarations.list",
                                            "argument_declarations.pipelist",
                                            "argument_declarations",
                                            "argument_declarations.opt",
                                            "variable_declarations.list.item",
                                            "variable_declarations.list",
                                            "variable_declarations",
                                            "arguments.entries",
                                            "arguments.no_trailing",
                                            "arguments",
                                            "arguments.paren",
                                            "arguments.maybe_paren",
                                            "literal.terminal",
                                            "literal.array.contents",
                                            "literal.dictionary.entry",
                                            "literal.dictionary.entries",
                                            "literal.dictionary",
                                            "literal.array",
                                            "literal",
                                            "name",
                                            "binary_op.raw",
                                            "binary_op.no_adverb",
                                            "binary_op",
                                            "semicolon.opt",
                                            "comma.opt",
                                            "ascii",
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
                                            "accessor",
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


const short parser::yypact_ninf_ = -285;

const short parser::yytable_ninf_ = -180;

const short parser::yypact_[] = {
    105,  18,   431,  7,    67,   85,   -285, -285, -285, 89,   97,   -285, -285, 1076, 479,  126,  89,   -285, -285,
    -285, -285, -285, -285, -285, -285, -285, -285, -285, -285, -285, -285, 125,  -285, -285, -285, -285, -285, 262,
    1220, 33,   -285, 118,  -285, -285, 146,  290,  -285, -285, -285, -285, 12,   -285, -285, -285, -285, 129,  123,
    -285, 134,  -285, -285, -285, 160,  -285, -285, 172,  162,  201,  1220, 290,  197,  161,  200,  -285, 1220, 262,
    -285, -285, -285, -285, -285, -285, 59,   -285, 202,  -285, 208,  1076, 170,  1076, 583,  -285, 46,   -285, 153,
    -285, -285, -285, -285, -285, 431,  377,  -285, 25,   -5,   -285, 635,  683,  -285, -285, 174,  163,  1220, 732,
    1220, 46,   -285, -285, -285, 42,   -285, -285, 168,  -285, 1220, -285, 1220, 1124, 212,  -285, -285, 1220, 1172,
    213,  41,   200,  1220, 136,  46,   1220, 1220, -285, -285, 211,  215,  -285, -285, -285, -285, 19,   17,   -285,
    44,   1264, -285, 1220, 89,   217,  -285, 290,  -285, 177,  221,  -285, 781,  830,  46,   22,   11,   -285, 171,
    46,   222,  290,  142,  226,  -285, -285, 42,   228,  -285, -285, 131,  131,  131,  -285, 224,  42,   290,  -285,
    1220, 188,  -285, -285, 1220, 190,  -285, -285, 879,  46,   232,  290,  -285, 1124, -285, 46,   -285, -285, 979,
    -285, 46,   89,   89,   230,  1220, 1338, -285, 89,   2,    1172, 1375, -285, 290,  -285, 229,  31,   1028, 237,
    1220, 196,  204,  238,  46,   239,  -285, 979,  1220, -285, 46,   1220, -285, -285, 46,   4,    -285, 118,  -285,
    -285, -285, -285, -285, -285, 241,  89,   241,  241,  168,  -285, 244,  -285, 1220, 46,   245,  -285, -285, -285,
    34,   75,   -285, 246,  1172, -285, -3,   -285, 1301, 81,   1172, -285, 89,   -285, 1220, 1220, -285, -285, 290,
    1220, 1220, 210,  -285, -285, 290,  -285, 247,  1220, -285, 535,  131,  -285, -285, -285, -285, 46,   46,   1220,
    1412, -285, 89,   -285, 91,   89,   -285, 1172, 1449, -285, -285, 94,   -285, 250,  290,  290,  290,  1220, -285,
    290,  -285, 931,  217,  -285, -285, 251,  1172, -285, 240,  -285, 209,  98,   1172, -285, -285, -285, 290,  -285,
    217,  259,  -285, 100,  -285, -285, -285, 103,  263,  -285, -285, -285, -285
};

const unsigned char parser::yydefact_[] = {
    0,   17,  0,   0,   0,   3,   10,  12,  13,  0,   15,  9,   42,  147, 156, 0,   0,   165, 190, 191, 192, 194, 195,
    196, 197, 200, 201, 186, 187, 188, 182, 77,  183, 184, 185, 208, 43,  0,   0,   0,   4,   122, 146, 71,  76,  8,
    162, 164, 163, 69,  70,  145, 144, 143, 139, 140, 141, 198, 204, 202, 205, 142, 0,   1,   11,  0,   0,   0,   0,
    90,  178, 148, 180, 166, 175, 170, 168, 169, 171, 172, 173, 167, 0,   157, 180, 174, 0,   147, 73,  147, 0,   48,
    66,  209, 0,   193, 199, 203, 80,  6,   0,   0,   2,   0,   0,   123, 0,   0,   175, 170, 0,   177, 0,   0,   0,
    60,  189, 207, 206, 40,  18,  16,  28,  150, 179, 92,  0,   181, 0,   155, 72,  0,   181, 0,   0,   180, 0,   0,
    46,  0,   0,   132, 133, 180, 0,   49,  7,   5,   114, 180, 96,  118, 180, 105, 45,  0,   0,   178, 50,  52,  53,
    75,  0,   64,  0,   0,   46,  137, 0,   176, 81,  0,   0,   82,  171, 0,   36,  38,  41,  0,   30,  31,  210, 210,
    210, 26,  178, 40,  91,  149, 0,   151, 160, 154, 0,   0,   158, 159, 0,   57,  0,   83,  65,  0,   68,  47,  130,
    131, 181, 135, 46,  181, 0,   0,   0,   0,   181, 0,   0,   156, 0,   104, 54,  127, 180, 124, 179, 0,   0,   74,
    79,  0,   46,  0,   62,  0,   0,   138, 46,  0,   93,  94,  59,  46,  37,  122, 39,  19,  211, 213, 212, 21,  24,
    0,   23,  25,  179, 29,  0,   153, 0,   0,   0,   161, 134, 67,  100, 0,   115, 0,   156, 97,  0,   119, 110, 0,
    156, 106, 181, 129, 0,   0,   51,  44,  87,  0,   0,   78,  63,  136, 84,  61,  0,   0,   58,  0,   210, 20,  27,
    14,  152, 56,  46,  0,   0,   116, 0,   98,  0,   0,   120, 156, 0,   109, 107, 0,   128, 0,   125, 86,  89,  0,
    95,  85,  33,  0,   178, 22,  55,  0,   156, 101, 0,   99,  0,   0,   156, 111, 108, 126, 88,  35,  178, 0,   102,
    0,   117, 121, 112, 0,   0,   32,  103, 113, 34
};

const short parser::yypgoto_[] = { -285, -285, -285, -78,  -285, 264,  -285, -285, -285, -285, -26,  3,    15,   -285,
                                   -285, -285, 99,   96,   -285, 88,   -285, 128,  -163, -29,  -284, 61,   -285, -285,
                                   10,   -285, -13,  -285, -285, -285, -285, 27,   -117, -285, -285, 80,   -285, -75,
                                   122,  -285, -285, 124,  165,  -285, -285, -285, -149, -2,   -101, 278,  -285, -65,
                                   -71,  -285, -285, -285, -285, -285, -31,  83,   -285, 93,   -285, -285, -285 };

const unsigned char parser::yydefgoto_[] = { 0,   4,   39,  40,  5,   6,   7,   67,  10,  8,   251, 252, 185, 186,
                                             187, 175, 176, 177, 178, 179, 41,  42,  204, 205, 157, 158, 43,  44,
                                             69,  70,  141, 169, 149, 152, 105, 106, 223, 224, 160, 142, 143, 233,
                                             166, 238, 46,  72,  83,  84,  47,  48,  49,  50,  85,  111, 112, 102,
                                             128, 51,  52,  53,  54,  55,  56,  57,  58,  59,  60,  61,  253 };

const short parser::yytable_[] = {
    71,  82,  92,  234, 221, 125, 95,  65,  12,  309,  17,   326, 45,  133, 88,  144, 12,  17,  181, 239, 113, 115, 146,
    147, 9,   214, 17,  18,  19,  20,  235, 211, 162,  -178, 99,  148, 100, 62,  172, 280, 17,  342, 303, 101, 95,  12,
    36,  265, 98,  198, 12,  151, 293, 310, 36,  123,  216,  17,  273, 212, 114, 129, 94,  95,  200, 215, 271, 63,  130,
    288, 236, 277, 209, 181, 71,  291, 71,  181, 213,  281,  294, 218, 304, 36,  217, 305, 181, 306, 36,  231, 314, 73,
    227, 109, 76,  77,  174, 79,  80,  81,  333, 150,  153,  338, 17,  199, 131, 348, 167, 352, 45,  45,  353, 189, 191,
    1,   159, 180, 193, 195, 96,  257, 170, 262, 173,  313,  206, 207, 131, 12,  97,  89,  87,  90,  188, 1,   297, 241,
    131, 328, 3,   131, 242, 202, 66,  131, 201, 131,  203,  2,   131, 103, 107, 279, 225, 331, 116, 17,  96,  91,  3,
    316, 117, 337, 119, 222, 240, 36,  97,  18,  19,   20,   180, 118, 104, 163, 180, 259, 91,  120, 164, 129, 165, 248,
    249, 180, 254, 255, 250, 17,  191, 73,  121, 109,  76,   77,  78,  79,  80,  81,  182, 269, 183, 184, 94,  122, 275,
    124, 126, 266, 267, 135, 127, 137, 132, 272, 274,  134,  136, 192, 145, 168, 197, 208, 210, 228, 292, 226, 229, 110,
    245, 243, 301, 247, 256, 260, 159, 131, 284, 263,  268,  278, 283, 145, 285, 287, 290, 300, 289, 299, 346, 225, 286,
    296, 302, 307, 322, 308, 321, 339, 344, 343, 91,   315,  351, 347, 91,  317, 354, 64,  327, 298, 295, 244, 246, 258,
    225, 350, 18,  19,  20,  21,  22,  23,  24,  25,   26,   282, 264, 237, 329, 318, 86,  0,   91,  319, 320, 196, 335,
    91,  0,   93,  0,   323, 332, 159, 0,   334, 0,    0,    0,   0,   0,   94,  0,   0,   0,   345, 0,   0,   0,   0,
    0,   349, 0,   0,   0,   145, 0,   0,   0,   340,  0,    145, 0,   159, 0,   0,   91,  73,  108, 109, 76,  77,  78,
    79,  80,  81,  110, 0,   0,   0,   0,   0,   0,    0,    0,   0,   0,   0,   91,  0,   0,   0,   0,   0,   91,  0,
    0,   0,   145, 91,  0,   0,   0,   0,   0,   -179, 11,   0,   0,   12,  0,   13,  0,   14,  0,   0,   0,   91,  15,
    16,  17,  18,  19,  20,  21,  22,  23,  24,  25,   26,   27,  28,  29,  30,  0,   31,  0,   0,   0,   0,   0,   32,
    33,  34,  35,  0,   0,   36,  0,   0,   0,   0,    0,    0,   0,   0,   37,  145, 91,  0,   11,  0,   0,   12,  38,
    13,  0,   14,  0,   0,   0,   0,   15,  16,  17,   18,   19,  20,  21,  22,  23,  24,  25,  26,  27,  28,  29,  30,
    0,   31,  0,   0,   0,   0,   0,   32,  33,  34,   35,   0,   0,   36,  0,   0,   0,   0,   0,   0,   0,   0,   37,
    12,  0,   13,  0,   14,  0,   0,   38,  0,   15,   16,   17,  18,  19,  20,  21,  22,  23,  24,  25,  26,  27,  28,
    29,  30,  0,   31,  0,   0,   0,   0,   0,   32,   33,   34,  35,  0,   0,   36,  0,   0,   0,   0,   0,   0,   73,
    74,  75,  76,  77,  78,  79,  80,  81,  0,   38,   12,   324, 13,  0,   14,  0,   0,   155, 0,   15,  16,  17,  18,
    19,  20,  21,  22,  23,  24,  25,  26,  27,  28,   29,   30,  325, 31,  0,   156, 0,   0,   0,   32,  33,  34,  35,
    0,   0,   36,  0,   0,   0,   0,   0,   0,   0,    0,    37,  12,  0,   13,  0,   14,  138, 0,   38,  0,   15,  16,
    17,  18,  19,  20,  21,  22,  23,  24,  25,  26,   27,   28,  29,  30,  0,   31,  0,   0,   0,   0,   0,   32,  33,
    34,  35,  0,   0,   36,  0,   0,   0,   0,   0,    0,    0,   139, 37,  0,   0,   140, 0,   12,  154, 13,  38,  14,
    0,   0,   155, 0,   15,  16,  17,  18,  19,  20,   21,   22,  23,  24,  25,  26,  27,  28,  29,  30,  0,   31,  0,
    156, 0,   0,   0,   32,  33,  34,  35,  0,   0,    36,   0,   0,   0,   0,   0,   0,   0,   0,   37,  12,  0,   13,
    161, 14,  0,   0,   38,  0,   15,  16,  17,  18,   19,   20,  21,  22,  23,  24,  25,  26,  27,  28,  29,  30,  0,
    31,  0,   0,   0,   0,   0,   32,  33,  34,  35,   0,    0,   36,  0,   0,   0,   0,   0,   0,   0,   139, 37,  0,
    12,  140, 13,  0,   14,  171, 38,  0,   0,   15,   16,   17,  18,  19,  20,  21,  22,  23,  24,  25,  26,  27,  28,
    29,  30,  0,   31,  0,   0,   0,   0,   0,   32,   33,   34,  35,  0,   0,   36,  0,   0,   0,   0,   0,   0,   0,
    139, 37,  0,   12,  140, 13,  230, 14,  0,   38,   0,    0,   15,  16,  17,  18,  19,  20,  21,  22,  23,  24,  25,
    26,  27,  28,  29,  30,  0,   31,  0,   0,   0,    0,    0,   32,  33,  34,  35,  0,   0,   36,  0,   0,   0,   0,
    0,   0,   0,   139, 37,  0,   12,  140, 13,  0,    14,   232, 38,  0,   0,   15,  16,  17,  18,  19,  20,  21,  22,
    23,  24,  25,  26,  27,  28,  29,  30,  0,   31,   0,    0,   0,   0,   0,   32,  33,  34,  35,  0,   0,   36,  0,
    0,   0,   0,   0,   0,   0,   139, 37,  0,   12,   140,  13,  0,   14,  261, 38,  0,   0,   15,  16,  17,  18,  19,
    20,  21,  22,  23,  24,  25,  26,  27,  28,  29,   30,   0,   31,  0,   0,   0,   0,   0,   32,  33,  34,  35,  0,
    0,   36,  0,   0,   0,   0,   0,   0,   0,   139,  37,   0,   0,   140, 0,   12,  341, 13,  38,  14,  0,   0,   155,
    0,   15,  16,  17,  18,  19,  20,  21,  22,  23,   24,   25,  26,  27,  28,  29,  30,  0,   31,  0,   156, 0,   0,
    0,   32,  33,  34,  35,  0,   0,   36,  0,   0,    0,    0,   0,   0,   0,   0,   37,  12,  0,   13,  0,   14,  0,
    0,   38,  0,   15,  16,  17,  18,  19,  20,  21,   22,   23,  24,  25,  26,  27,  28,  29,  30,  0,   31,  0,   0,
    0,   0,   0,   32,  33,  34,  35,  0,   0,   36,   0,    0,   0,   0,   0,   0,   0,   139, 37,  0,   12,  140, 13,
    0,   14,  0,   38,  155, 0,   15,  16,  17,  18,   19,   20,  21,  22,  23,  24,  25,  26,  27,  28,  29,  30,  0,
    31,  0,   156, 0,   0,   0,   32,  33,  34,  35,   0,    0,   36,  0,   0,   0,   0,   0,   0,   0,   0,   37,  12,
    0,   13,  0,   14,  0,   0,   38,  0,   15,  16,   17,   18,  19,  20,  21,  22,  23,  24,  25,  26,  27,  28,  29,
    30,  0,   31,  0,   0,   0,   0,   0,   32,  33,   34,   35,  0,   0,   36,  0,   0,   0,   0,   0,   0,   0,   68,
    37,  12,  0,   13,  0,   14,  0,   0,   38,  0,    15,   16,  17,  18,  19,  20,  21,  22,  23,  24,  25,  26,  27,
    28,  29,  30,  0,   31,  0,   0,   0,   0,   0,    32,   33,  34,  35,  0,   0,   36,  0,   0,   0,   0,   0,   0,
    0,   190, 37,  12,  0,   13,  0,   14,  0,   0,    38,   0,   15,  16,  17,  18,  19,  20,  21,  22,  23,  24,  25,
    26,  27,  28,  29,  30,  0,   31,  0,   0,   0,    0,    0,   32,  33,  34,  35,  0,   0,   36,  0,   0,   0,   0,
    0,   0,   0,   194, 37,  12,  0,   13,  0,   14,   0,    0,   38,  0,   15,  16,  17,  18,  19,  20,  21,  22,  23,
    24,  25,  26,  27,  28,  29,  30,  0,   31,  0,    0,    0,   0,   0,   32,  33,  34,  35,  0,   0,   36,  0,   0,
    0,   0,   0,   12,  0,   13,  37,  219, 0,   0,    0,    0,   15,  0,   38,  18,  19,  20,  21,  22,  23,  24,  25,
    26,  27,  28,  29,  30,  0,   0,   0,   0,   0,    0,    0,   32,  33,  34,  35,  0,   12,  36,  13,  0,   311, 0,
    0,   220, 0,   15,  37,  0,   18,  19,  20,  21,   22,   23,  24,  25,  26,  27,  28,  29,  30,  0,   0,   0,   0,
    0,   0,   0,   32,  33,  34,  35,  0,   12,  36,   13,   0,   270, 0,   0,   312, 0,   15,  37,  0,   18,  19,  20,
    21,  22,  23,  24,  25,  26,  27,  28,  29,  30,   0,    0,   0,   0,   0,   0,   0,   32,  33,  34,  35,  0,   12,
    36,  13,  0,   276, 0,   0,   0,   0,   15,  37,   0,    18,  19,  20,  21,  22,  23,  24,  25,  26,  27,  28,  29,
    30,  0,   0,   0,   0,   0,   0,   0,   32,  33,   34,   35,  0,   12,  36,  13,  0,   330, 0,   0,   0,   0,   15,
    37,  0,   18,  19,  20,  21,  22,  23,  24,  25,   26,   27,  28,  29,  30,  0,   0,   0,   0,   0,   0,   0,   32,
    33,  34,  35,  0,   12,  36,  13,  0,   336, 0,    0,    0,   0,   15,  37,  0,   18,  19,  20,  21,  22,  23,  24,
    25,  26,  27,  28,  29,  30,  0,   0,   0,   0,    0,    0,   0,   32,  33,  34,  35,  0,   0,   36,  0,   0,   0,
    0,   0,   0,   0,   0,   37
};

const short parser::yycheck_[] = {
    13,  14,  31,  166, 153, 70,  37,  9,   4,   12,  15,  295, 2,   84,  16,  90,  4,   15,  119, 8,   8,   50,  100,
    101, 6,   8,   15,  16,  17,  18,  8,   12,  107, 0,   1,   10,  3,   30,  113, 8,   15,  325, 8,   10,  75,  4,
    42,  210, 38,  8,   4,   56,  48,  56,  42,  68,  12,  15,  56,  40,  48,  74,  51,  94,  135, 48,  215, 0,   9,
    232, 48,  220, 143, 174, 87,  238, 89,  178, 149, 48,  243, 152, 48,  42,  40,  10,  187, 12,  42,  164, 9,   49,
    157, 51,  52,  53,  54,  55,  56,  57,  9,   103, 104, 9,   15,  134, 47,  9,   110, 9,   100, 101, 9,   126, 127,
    30,  106, 119, 131, 132, 37,  186, 112, 198, 114, 274, 139, 140, 47,  4,   37,  6,   6,   8,   124, 30,  253, 168,
    47,  302, 55,  47,  171, 7,   47,  47,  136, 47,  12,  44,  47,  33,  6,   224, 156, 304, 27,  15,  75,  31,  55,
    278, 39,  312, 4,   155, 168, 42,  75,  16,  17,  18,  174, 39,  56,  1,   178, 190, 50,  7,   6,   194, 8,   52,
    53,  187, 183, 184, 57,  15,  203, 49,  30,  51,  52,  53,  54,  55,  56,  57,  32,  214, 34,  35,  51,  4,   219,
    10,  47,  211, 212, 87,  12,  89,  12,  217, 218, 9,   48,  7,   92,  58,  9,   12,  9,   48,  239, 10,  7,   58,
    4,   9,   261, 5,   10,  47,  226, 47,  228, 7,   10,  12,  5,   115, 48,  7,   236, 260, 9,   5,   10,  253, 48,
    12,  9,   9,   9,   270, 48,  9,   9,   326, 134, 276, 5,   56,  138, 280, 5,   5,   296, 256, 245, 174, 178, 187,
    278, 342, 16,  17,  18,  19,  20,  21,  22,  23,  24,  226, 208, 167, 303, 281, 14,  -1,  166, 285, 286, 132, 311,
    171, -1,  39,  -1,  293, 306, 295, -1,  309, -1,  -1,  -1,  -1,  -1,  51,  -1,  -1,  -1,  330, -1,  -1,  -1,  -1,
    -1,  336, -1,  -1,  -1,  199, -1,  -1,  -1,  321, -1,  205, -1,  325, -1,  -1,  210, 49,  50,  51,  52,  53,  54,
    55,  56,  57,  58,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  232, -1,  -1,  -1,  -1,  -1,  238, -1,
    -1,  -1,  242, 243, -1,  -1,  -1,  -1,  -1,  0,   1,   -1,  -1,  4,   -1,  6,   -1,  8,   -1,  -1,  -1,  261, 13,
    14,  15,  16,  17,  18,  19,  20,  21,  22,  23,  24,  25,  26,  27,  28,  -1,  30,  -1,  -1,  -1,  -1,  -1,  36,
    37,  38,  39,  -1,  -1,  42,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  51,  301, 302, -1,  1,   -1,  -1,  4,   59,
    6,   -1,  8,   -1,  -1,  -1,  -1,  13,  14,  15,  16,  17,  18,  19,  20,  21,  22,  23,  24,  25,  26,  27,  28,
    -1,  30,  -1,  -1,  -1,  -1,  -1,  36,  37,  38,  39,  -1,  -1,  42,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  51,
    4,   -1,  6,   -1,  8,   -1,  -1,  59,  -1,  13,  14,  15,  16,  17,  18,  19,  20,  21,  22,  23,  24,  25,  26,
    27,  28,  -1,  30,  -1,  -1,  -1,  -1,  -1,  36,  37,  38,  39,  -1,  -1,  42,  -1,  -1,  -1,  -1,  -1,  -1,  49,
    50,  51,  52,  53,  54,  55,  56,  57,  -1,  59,  4,   5,   6,   -1,  8,   -1,  -1,  11,  -1,  13,  14,  15,  16,
    17,  18,  19,  20,  21,  22,  23,  24,  25,  26,  27,  28,  29,  30,  -1,  32,  -1,  -1,  -1,  36,  37,  38,  39,
    -1,  -1,  42,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  51,  4,   -1,  6,   -1,  8,   9,   -1,  59,  -1,  13,  14,
    15,  16,  17,  18,  19,  20,  21,  22,  23,  24,  25,  26,  27,  28,  -1,  30,  -1,  -1,  -1,  -1,  -1,  36,  37,
    38,  39,  -1,  -1,  42,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  50,  51,  -1,  -1,  54,  -1,  4,   5,   6,   59,  8,
    -1,  -1,  11,  -1,  13,  14,  15,  16,  17,  18,  19,  20,  21,  22,  23,  24,  25,  26,  27,  28,  -1,  30,  -1,
    32,  -1,  -1,  -1,  36,  37,  38,  39,  -1,  -1,  42,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  51,  4,   -1,  6,
    7,   8,   -1,  -1,  59,  -1,  13,  14,  15,  16,  17,  18,  19,  20,  21,  22,  23,  24,  25,  26,  27,  28,  -1,
    30,  -1,  -1,  -1,  -1,  -1,  36,  37,  38,  39,  -1,  -1,  42,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  50,  51,  -1,
    4,   54,  6,   -1,  8,   9,   59,  -1,  -1,  13,  14,  15,  16,  17,  18,  19,  20,  21,  22,  23,  24,  25,  26,
    27,  28,  -1,  30,  -1,  -1,  -1,  -1,  -1,  36,  37,  38,  39,  -1,  -1,  42,  -1,  -1,  -1,  -1,  -1,  -1,  -1,
    50,  51,  -1,  4,   54,  6,   7,   8,   -1,  59,  -1,  -1,  13,  14,  15,  16,  17,  18,  19,  20,  21,  22,  23,
    24,  25,  26,  27,  28,  -1,  30,  -1,  -1,  -1,  -1,  -1,  36,  37,  38,  39,  -1,  -1,  42,  -1,  -1,  -1,  -1,
    -1,  -1,  -1,  50,  51,  -1,  4,   54,  6,   -1,  8,   9,   59,  -1,  -1,  13,  14,  15,  16,  17,  18,  19,  20,
    21,  22,  23,  24,  25,  26,  27,  28,  -1,  30,  -1,  -1,  -1,  -1,  -1,  36,  37,  38,  39,  -1,  -1,  42,  -1,
    -1,  -1,  -1,  -1,  -1,  -1,  50,  51,  -1,  4,   54,  6,   -1,  8,   9,   59,  -1,  -1,  13,  14,  15,  16,  17,
    18,  19,  20,  21,  22,  23,  24,  25,  26,  27,  28,  -1,  30,  -1,  -1,  -1,  -1,  -1,  36,  37,  38,  39,  -1,
    -1,  42,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  50,  51,  -1,  -1,  54,  -1,  4,   5,   6,   59,  8,   -1,  -1,  11,
    -1,  13,  14,  15,  16,  17,  18,  19,  20,  21,  22,  23,  24,  25,  26,  27,  28,  -1,  30,  -1,  32,  -1,  -1,
    -1,  36,  37,  38,  39,  -1,  -1,  42,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  51,  4,   -1,  6,   -1,  8,   -1,
    -1,  59,  -1,  13,  14,  15,  16,  17,  18,  19,  20,  21,  22,  23,  24,  25,  26,  27,  28,  -1,  30,  -1,  -1,
    -1,  -1,  -1,  36,  37,  38,  39,  -1,  -1,  42,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  50,  51,  -1,  4,   54,  6,
    -1,  8,   -1,  59,  11,  -1,  13,  14,  15,  16,  17,  18,  19,  20,  21,  22,  23,  24,  25,  26,  27,  28,  -1,
    30,  -1,  32,  -1,  -1,  -1,  36,  37,  38,  39,  -1,  -1,  42,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  51,  4,
    -1,  6,   -1,  8,   -1,  -1,  59,  -1,  13,  14,  15,  16,  17,  18,  19,  20,  21,  22,  23,  24,  25,  26,  27,
    28,  -1,  30,  -1,  -1,  -1,  -1,  -1,  36,  37,  38,  39,  -1,  -1,  42,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  50,
    51,  4,   -1,  6,   -1,  8,   -1,  -1,  59,  -1,  13,  14,  15,  16,  17,  18,  19,  20,  21,  22,  23,  24,  25,
    26,  27,  28,  -1,  30,  -1,  -1,  -1,  -1,  -1,  36,  37,  38,  39,  -1,  -1,  42,  -1,  -1,  -1,  -1,  -1,  -1,
    -1,  50,  51,  4,   -1,  6,   -1,  8,   -1,  -1,  59,  -1,  13,  14,  15,  16,  17,  18,  19,  20,  21,  22,  23,
    24,  25,  26,  27,  28,  -1,  30,  -1,  -1,  -1,  -1,  -1,  36,  37,  38,  39,  -1,  -1,  42,  -1,  -1,  -1,  -1,
    -1,  -1,  -1,  50,  51,  4,   -1,  6,   -1,  8,   -1,  -1,  59,  -1,  13,  14,  15,  16,  17,  18,  19,  20,  21,
    22,  23,  24,  25,  26,  27,  28,  -1,  30,  -1,  -1,  -1,  -1,  -1,  36,  37,  38,  39,  -1,  -1,  42,  -1,  -1,
    -1,  -1,  -1,  4,   -1,  6,   51,  8,   -1,  -1,  -1,  -1,  13,  -1,  59,  16,  17,  18,  19,  20,  21,  22,  23,
    24,  25,  26,  27,  28,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  36,  37,  38,  39,  -1,  4,   42,  6,   -1,  8,   -1,
    -1,  48,  -1,  13,  51,  -1,  16,  17,  18,  19,  20,  21,  22,  23,  24,  25,  26,  27,  28,  -1,  -1,  -1,  -1,
    -1,  -1,  -1,  36,  37,  38,  39,  -1,  4,   42,  6,   -1,  8,   -1,  -1,  48,  -1,  13,  51,  -1,  16,  17,  18,
    19,  20,  21,  22,  23,  24,  25,  26,  27,  28,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  36,  37,  38,  39,  -1,  4,
    42,  6,   -1,  8,   -1,  -1,  -1,  -1,  13,  51,  -1,  16,  17,  18,  19,  20,  21,  22,  23,  24,  25,  26,  27,
    28,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  36,  37,  38,  39,  -1,  4,   42,  6,   -1,  8,   -1,  -1,  -1,  -1,  13,
    51,  -1,  16,  17,  18,  19,  20,  21,  22,  23,  24,  25,  26,  27,  28,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  36,
    37,  38,  39,  -1,  4,   42,  6,   -1,  8,   -1,  -1,  -1,  -1,  13,  51,  -1,  16,  17,  18,  19,  20,  21,  22,
    23,  24,  25,  26,  27,  28,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  36,  37,  38,  39,  -1,  -1,  42,  -1,  -1,  -1,
    -1,  -1,  -1,  -1,  -1,  51
};

const unsigned char parser::yystos_[] = {
    0,   30,  44,  55,  62,  65,  66,  67,  70,  6,   69,  1,   4,   6,   8,   13,  14,  15,  16,  17,  18,  19,  20,
    21,  22,  23,  24,  25,  26,  27,  28,  30,  36,  37,  38,  39,  42,  51,  59,  63,  64,  81,  82,  87,  88,  89,
    105, 109, 110, 111, 112, 118, 119, 120, 121, 122, 123, 124, 125, 126, 127, 128, 30,  0,   66,  112, 47,  68,  50,
    89,  90,  91,  106, 49,  50,  51,  52,  53,  54,  55,  56,  57,  91,  107, 108, 113, 114, 6,   112, 6,   8,   82,
    84,  39,  51,  123, 124, 126, 89,  1,   3,   10,  116, 33,  56,  95,  96,  6,   50,  51,  58,  114, 115, 8,   48,
    84,  27,  39,  39,  4,   7,   30,  4,   91,  10,  116, 47,  12,  117, 91,  9,   47,  12,  117, 9,   106, 48,  106,
    9,   50,  54,  91,  100, 101, 102, 82,  64,  64,  10,  93,  112, 56,  94,  112, 5,   11,  32,  85,  86,  89,  99,
    7,   102, 1,   6,   8,   103, 112, 58,  92,  89,  9,   102, 89,  54,  76,  77,  78,  79,  80,  112, 113, 32,  34,
    35,  73,  74,  75,  89,  91,  50,  91,  7,   91,  50,  91,  107, 9,   8,   84,  117, 89,  7,   12,  83,  84,  91,
    91,  12,  117, 9,   12,  40,  117, 8,   48,  12,  40,  117, 8,   48,  111, 89,  97,  98,  112, 10,  116, 48,  7,
    7,   102, 9,   102, 83,  8,   48,  103, 104, 8,   112, 123, 84,  9,   77,  4,   78,  5,   52,  53,  57,  71,  72,
    129, 72,  72,  10,  116, 80,  91,  47,  9,   102, 7,   100, 83,  112, 112, 10,  91,  8,   111, 112, 56,  112, 91,
    8,   111, 12,  117, 8,   48,  86,  5,   89,  48,  48,  7,   83,  9,   89,  83,  91,  48,  83,  96,  12,  97,  73,
    5,   91,  84,  9,   8,   48,  10,  12,  9,   91,  12,  56,  8,   48,  111, 9,   91,  97,  91,  89,  89,  89,  48,
    9,   89,  5,   29,  85,  71,  83,  91,  8,   111, 112, 9,   112, 91,  8,   111, 9,   9,   89,  5,   85,  116, 9,
    91,  10,  56,  9,   91,  116, 5,   9,   9,   5
};

const unsigned char parser::yyr1_[] = {
    0,   61,  62,  62,  63,  63,  63,  63,  64,  64,  65,  65,  66,  66,  67,  68,  68,  69,  69,  70,  71,  72,
    72,  73,  73,  73,  74,  74,  75,  75,  76,  76,  77,  77,  77,  77,  78,  78,  79,  79,  80,  80,  81,  81,
    82,  82,  83,  83,  84,  84,  85,  85,  86,  86,  86,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,  87,
    87,  87,  87,  88,  88,  88,  88,  88,  88,  88,  89,  89,  89,  89,  89,  89,  89,  89,  89,  89,  89,  89,
    89,  89,  90,  90,  91,  92,  92,  92,  93,  93,  93,  93,  93,  93,  93,  93,  94,  94,  94,  94,  94,  94,
    94,  94,  94,  94,  95,  95,  95,  95,  95,  95,  95,  95,  96,  96,  97,  97,  97,  98,  98,  99,  100, 100,
    100, 101, 101, 102, 103, 104, 104, 105, 105, 105, 105, 105, 105, 105, 105, 106, 106, 106, 106, 106, 106, 106,
    107, 107, 108, 108, 108, 109, 110, 110, 111, 111, 111, 112, 113, 113, 113, 113, 113, 113, 113, 113, 114, 114,
    115, 115, 116, 116, 117, 117, 118, 119, 120, 120, 121, 121, 122, 122, 123, 123, 123, 123, 124, 124, 124, 124,
    125, 125, 126, 126, 127, 127, 128, 128, 128, 128, 128, 128, 129, 129, 129, 129
};

const signed char parser::yyr2_[] = { 0, 2, 3, 1, 1, 3, 2, 3, 1, 1, 1, 2, 1, 1, 7, 0, 2, 0, 3, 5, 2, 1, 3, 2, 2, 2, 1,
                                      3, 0, 2, 1, 1, 6, 4, 7, 5, 1, 2, 1, 2, 0, 1, 1, 1, 5, 3, 0, 1, 1, 2, 1, 3, 1, 1,
                                      2, 7, 6, 4, 5, 4, 2, 5, 4, 5, 3, 4, 2, 5, 4, 1, 1, 1, 3, 2, 4, 3, 1, 1, 5, 4, 2,
                                      3, 3, 4, 5, 6, 6, 5, 7, 6, 1, 3, 2, 2, 2, 4, 1, 3, 4, 5, 3, 5, 6, 7, 2, 1, 3, 4,
                                      5, 4, 3, 5, 6, 7, 2, 4, 5, 7, 2, 4, 5, 7, 0, 1, 1, 3, 4, 1, 3, 3, 2, 2, 1, 1, 3,
                                      2, 3, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 3, 2, 3, 5, 4, 3, 2, 0, 1, 3, 4, 4, 5,
                                      1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 1, 0, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1,
                                      2, 1, 1, 1, 2, 1, 1, 1, 1, 1, 2, 1, 1, 1, 2, 1, 1, 2, 2, 1, 2, 0, 1, 1, 1 };


#if YYDEBUG
const short parser::yyrline_[] = {
    0,   174, 174, 175, 181, 183, 185, 202, 207, 208, 217, 219, 224, 225, 229, 234, 235, 239, 240, 244, 249, 254,
    256, 261, 263, 265, 270, 272, 277, 278, 282, 283, 287, 289, 291, 293, 298, 299, 304, 305, 309, 310, 313, 313,
    316, 318, 323, 324, 328, 329, 333, 334, 338, 339, 340, 344, 353, 356, 360, 369, 372, 375, 386, 396, 404, 411,
    414, 420, 431, 441, 443, 445, 446, 448, 453, 460, 470, 474, 476, 481, 487, 489, 495, 498, 501, 504, 512, 514,
    517, 520, 525, 526, 539, 542, 543, 544, 549, 551, 553, 555, 557, 559, 561, 563, 568, 570, 572, 574, 576, 578,
    580, 582, 584, 586, 591, 592, 593, 595, 597, 599, 601, 603, 609, 610, 614, 616, 618, 627, 629, 633, 637, 638,
    639, 643, 644, 647, 650, 653, 654, 658, 659, 660, 661, 662, 663, 664, 665, 669, 671, 673, 675, 677, 679, 681,
    686, 688, 693, 695, 697, 702, 707, 709, 718, 719, 720, 723, 732, 733, 734, 735, 736, 737, 738, 739, 743, 744,
    748, 750, 754, 754, 755, 755, 757, 759, 762, 763, 767, 768, 772, 773, 777, 778, 779, 780, 789, 790, 791, 792,
    796, 798, 803, 805, 810, 812, 817, 818, 819, 820, 821, 822, 826, 827, 828, 829
};

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
        42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60
    };
    // Last valid token kind.
    const int code_max = 315;

    if (t <= 0)
        return symbol_kind::S_YYEOF;
    else if (t <= code_max)
        return static_cast<symbol_kind_type>(translate_table[t]);
    else
        return symbol_kind::S_YYUNDEF;
}

#line 7 "langutils/sc_parser/src/sc_grammar.y"
}} // sc::parser
#line 4718 "langutils/sc_parser/src/sc_grammar_parser.cpp"

#line 832 "langutils/sc_parser/src/sc_grammar.y"
