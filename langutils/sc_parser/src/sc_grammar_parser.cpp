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

    case symbol_kind::S_63_region_item: // region.item
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

    case symbol_kind::S_63_region_item: // region.item
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

    case symbol_kind::S_63_region_item: // region.item
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

    case symbol_kind::S_63_region_item: // region.item
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

    case symbol_kind::S_63_region_item: // region.item
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

    case symbol_kind::S_63_region_item: // region.item
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

            case symbol_kind::S_63_region_item: // region.item
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
                case 2: // go: region $end
#line 175 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<ClassListOrExprListIndex>() =
                        cxt.graph.assign_root(yystack_[1].value.as<RegionListIndex>());
                }
#line 2485 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 3: // go: classOrExtList.list $end
#line 176 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<ClassListOrExprListIndex>() =
                        cxt.graph.assign_root(yystack_[1].value.as<ClassOrExtensionListIndex>());
                }
#line 2491 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 4: // region.item: expr
#line 182 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<error_index<ExprSeqIndex>>() = yystack_[0].value.as<ExprSeqIndex>();
                }
#line 2497 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 5: // region.item: OPENPAREN argument_declarations block.contents CLOSEPAREN
#line 184 "langutils/sc_parser/src/sc_grammar.y"
                {
                    auto block =
                        cxt.create(BlockNode {}, yylhs.location, yystack_[2].value.as<DeclareArgumentListIndex>(),
                                   yystack_[1].value.as<BlockContentsListIndex>());

                    yylhs.value.as<error_index<ExprSeqIndex>>() = cxt.create(
                        MessageNode { MessageNode::SelectorMode::Value }, yylhs.location,
                        cxt.create(Missing {}, yylhs.location), cxt.create(ArgumentList {}, yylhs.location, block));
                }
#line 2512 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 6: // region.item: OPENPAREN argument_declarations CLOSEPAREN
#line 195 "langutils/sc_parser/src/sc_grammar.y"
                {
                    auto block =
                        cxt.create(BlockNode {}, yylhs.location, yystack_[1].value.as<DeclareArgumentListIndex>(),
                                   cxt.create(BlockContentsList {}, yylhs.location));

                    yylhs.value.as<error_index<ExprSeqIndex>>() = cxt.create(
                        MessageNode { MessageNode::SelectorMode::Value }, yylhs.location,
                        cxt.create(Missing {}, yylhs.location), cxt.create(ArgumentList {}, yylhs.location, block));
                }
#line 2527 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 7: // region: INTERPRET expr
#line 209 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<RegionListIndex>() =
                        cxt.create(RegionList {}, yylhs.location, yystack_[0].value.as<ExprSeqIndex>());
                }
#line 2535 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 8: // region: INTERPRET error
#line 213 "langutils/sc_parser/src/sc_grammar.y"
                {
                    error_recovery::expr(cxt);
                    yyclearin;
                    yylhs.value.as<RegionListIndex>() =
                        cxt.create(RegionList {}, yylhs.location, create_error(cxt, yystack_[0].location));
                }
#line 2545 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 9: // region: INTERPRET OPENPAREN argument_declarations block.contents semicolon.opt CLOSEPAREN
#line 219 "langutils/sc_parser/src/sc_grammar.y"
                {
                    auto block =
                        cxt.create(BlockNode {}, yylhs.location, yystack_[3].value.as<DeclareArgumentListIndex>(),
                                   yystack_[2].value.as<BlockContentsListIndex>());

                    auto msg = cxt.create(MessageNode { MessageNode::SelectorMode::Value }, yylhs.location,
                                          cxt.create(Missing {}, yylhs.location),
                                          cxt.create(ArgumentList {}, yylhs.location, block));
                    yylhs.value.as<RegionListIndex>() = cxt.create(RegionList {}, yylhs.location, msg);
                }
#line 2561 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 10: // region: INTERPRET OPENPAREN argument_declarations CLOSEPAREN
#line 231 "langutils/sc_parser/src/sc_grammar.y"
                {
                    auto block =
                        cxt.create(BlockNode {}, yylhs.location, yystack_[1].value.as<DeclareArgumentListIndex>(),
                                   cxt.create(BlockContentsList {}, yylhs.location));

                    auto msg = cxt.create(MessageNode { MessageNode::SelectorMode::Value }, yylhs.location,
                                          cxt.create(Missing {}, yylhs.location),
                                          cxt.create(ArgumentList {}, yylhs.location, block));

                    yylhs.value.as<RegionListIndex>() = cxt.create(RegionList {}, yylhs.location, msg);
                }
#line 2578 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 11: // region: region SEMICOLON region.item
#line 245 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<RegionListIndex>() =
                        cxt.graph.append_to_list(yystack_[2].value.as<RegionListIndex>(), yylhs.location,
                                                 yystack_[0].value.as<error_index<ExprSeqIndex>>());
                }
#line 2584 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 12: // region: region REGION_SEPARATOR region.item
#line 248 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<RegionListIndex>() =
                        cxt.graph.append_to_list(yystack_[2].value.as<RegionListIndex>(), yylhs.location,
                                                 yystack_[0].value.as<error_index<ExprSeqIndex>>());
                }
#line 2590 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 13: // region: region error
#line 251 "langutils/sc_parser/src/sc_grammar.y"
                {
                    if (yystack_[1].location.end.line_number != yystack_[0].location.begin.line_number) {
                        auto first_child =
                            cxt.graph.edges(*yystack_[1].value.as<RegionListIndex>()).first_child.value();
                        auto last_child = cxt.graph.edges(Index { first_child }).last_sibling;
                        auto loc = cxt.graph.location(last_child ? Index { *last_child } : Index { first_child });
                        error_recovery::region_separator(cxt, loc);
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
                        yyclearin;
                    }
                }
#line 2612 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 14: // classOrExtList.list: classOrExtList.item
#line 277 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<ClassOrExtensionListIndex>() = cxt.create(
                        ClassOrExtensionList {}, yylhs.location, yystack_[0].value.as<ClassOrExtensionIndex>());
                }
#line 2618 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 15: // classOrExtList.list: classOrExtList.list classOrExtList.item
#line 279 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<ClassOrExtensionListIndex>() =
                        cxt.graph.append_to_list(yystack_[1].value.as<ClassOrExtensionListIndex>(),
                                                 yystack_[0].value.as<ClassOrExtensionIndex>());
                }
#line 2624 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 16: // classOrExtList.item: class
#line 283 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<ClassOrExtensionIndex>() = yystack_[0].value.as<ClassIndex>();
                }
#line 2630 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 17: // classOrExtList.item: class.extension
#line 284 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<ClassOrExtensionIndex>() = yystack_[0].value.as<ClassExtensionIndex>();
                }
#line 2636 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 18: // class: CLASSNAME class.slot.opt class.super.opt OPENCURLY class.vars.opt method.list.opt
                         // CLOSECURLY
#line 289 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<ClassIndex>() = cxt.create(
                        Class {}, yylhs.location, cxt.create(ClassNameIdentifier {}, yystack_[6].location),
                        yystack_[5].value.as<maybe<NamedIdentifierIndex>>(),
                        yystack_[4].value.as<maybe<ClassNameIdentifierIndex>>(),
                        yystack_[2].value.as<DeclareClassAnyVarListIndex>(), yystack_[1].value.as<MethodListIndex>());
                }
#line 2642 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 19: // class.super.opt: %empty
#line 293 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<maybe<ClassNameIdentifierIndex>>() = cxt.create(Missing {}, yylhs.location);
                }
#line 2648 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 20: // class.super.opt: COLON CLASSNAME
#line 294 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<maybe<ClassNameIdentifierIndex>>() =
                        cxt.create(ClassNameIdentifier {}, yystack_[0].location);
                }
#line 2654 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 21: // class.slot.opt: %empty
#line 298 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<maybe<NamedIdentifierIndex>>() = cxt.create(Missing {}, yylhs.location);
                }
#line 2660 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 22: // class.slot.opt: OPENSQUARE name CLOSESQUARE
#line 299 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<maybe<NamedIdentifierIndex>>() = yystack_[1].value.as<NamedIdentifierIndex>();
                }
#line 2666 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 23: // class.extension: ADD CLASSNAME OPENCURLY method.list.opt CLOSECURLY
#line 304 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<ClassExtensionIndex>() = cxt.create(
                        ClassExtension {}, yylhs.location, cxt.create(ClassNameIdentifier {}, yystack_[3].location),
                        yystack_[1].value.as<MethodListIndex>());
                }
#line 2672 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 24: // class.vars.entry.item: accessor variable_declarations.list.item
#line 309 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<DeclareClassVarIndex>() =
                        cxt.create(DeclareClassVar { yystack_[1].value.as<ReadWriteAccessor>() }, yylhs.location,
                                   yystack_[0].value.as<DeclareAnyVariableIndex>());
                }
#line 2678 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 25: // class.vars.entry.list: class.vars.entry.item
#line 314 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<DeclareMemberListIndex>() =
                        cxt.create(DeclareMemberList {}, yylhs.location, yystack_[0].value.as<DeclareClassVarIndex>());
                }
#line 2684 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 26: // class.vars.entry.list: class.vars.entry.list COMMA class.vars.entry.item
#line 316 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<DeclareMemberListIndex>() = cxt.graph.append_to_list(
                        yystack_[2].value.as<DeclareMemberListIndex>(), yystack_[0].value.as<DeclareClassVarIndex>());
                }
#line 2690 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 27: // class.vars.entry: CLASSVAR class.vars.entry.list
#line 321 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<DeclareAnyList>() =
                        cxt.graph.cast<DeclareClassMemberList>(yystack_[0].value.as<DeclareMemberListIndex>());
                }
#line 2696 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 28: // class.vars.entry: VAR class.vars.entry.list
#line 323 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<DeclareAnyList>() =
                        cxt.graph.cast<DeclareMemberList>(yystack_[0].value.as<DeclareMemberListIndex>());
                }
#line 2702 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 29: // class.vars.entry: CONST class.vars.entry.list
#line 325 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<DeclareAnyList>() =
                        cxt.graph.cast<DeclareConstList>(yystack_[0].value.as<DeclareMemberListIndex>());
                }
#line 2708 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 30: // class.vars: class.vars.entry
#line 330 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<DeclareClassAnyVarListIndex>() =
                        cxt.create(ClassAnyVarList {}, yylhs.location, yystack_[0].value.as<DeclareAnyList>());
                }
#line 2714 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 31: // class.vars: class.vars SEMICOLON class.vars.entry
#line 332 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<DeclareClassAnyVarListIndex>() = cxt.graph.append_to_list(
                        yystack_[2].value.as<DeclareClassAnyVarListIndex>(), yystack_[0].value.as<DeclareAnyList>());
                }
#line 2720 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 32: // class.vars.opt: %empty
#line 336 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<DeclareClassAnyVarListIndex>() = cxt.create(ClassAnyVarList {}, yylhs.location);
                }
#line 2726 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 33: // class.vars.opt: class.vars semicolon.opt
#line 337 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<DeclareClassAnyVarListIndex>() = yystack_[1].value.as<DeclareClassAnyVarListIndex>();
                }
#line 2732 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 34: // method.name: name
#line 341 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<MethodNameIndex>() = yystack_[0].value.as<NamedIdentifierIndex>();
                }
#line 2738 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 35: // method.name: binary_op.raw
#line 342 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<MethodNameIndex>() = yystack_[0].value.as<SelectorIndex>();
                }
#line 2744 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 36: // method.base: method.name OPENCURLY argument_declarations.opt block.contents semicolon.opt
                         // CLOSECURLY
#line 347 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<MethodIndex>() = cxt.create(
                        Method {}, yylhs.location, yystack_[5].value.as<MethodNameIndex>(),
                        yystack_[3].value.as<DeclareArgumentListIndex>(), cxt.create(Missing {}, yystack_[4].location),
                        yystack_[2].value.as<BlockContentsListIndex>());
                }
#line 2750 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 37: // method.base: method.name OPENCURLY argument_declarations.opt CLOSECURLY
#line 349 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<MethodIndex>() = cxt.create(
                        Method {}, yylhs.location, yystack_[3].value.as<MethodNameIndex>(),
                        yystack_[1].value.as<DeclareArgumentListIndex>(), cxt.create(Missing {}, yystack_[2].location),
                        cxt.create(BlockList {}, yystack_[0].location));
                }
#line 2756 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 38: // method.base: method.name OPENCURLY argument_declarations.opt PRIMITIVENAME block.contents
                         // semicolon.opt CLOSECURLY
#line 351 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<MethodIndex>() =
                        cxt.create(Method {}, yylhs.location, yystack_[6].value.as<MethodNameIndex>(),
                                   yystack_[4].value.as<DeclareArgumentListIndex>(),
                                   cxt.create(PrimitiveIdentifier {}, yystack_[3].location),
                                   yystack_[2].value.as<BlockContentsListIndex>());
                }
#line 2762 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 39: // method.base: method.name OPENCURLY argument_declarations.opt PRIMITIVENAME CLOSECURLY
#line 353 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<MethodIndex>() =
                        cxt.create(Method {}, yylhs.location, yystack_[4].value.as<MethodNameIndex>(),
                                   yystack_[2].value.as<DeclareArgumentListIndex>(),
                                   cxt.create(PrimitiveIdentifier {}, yystack_[1].location),
                                   cxt.create(BlockList {}, yystack_[0].location));
                }
#line 2768 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 40: // method: method.base
#line 357 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<AnyMethodIndex>() = yystack_[0].value.as<MethodIndex>();
                }
#line 2774 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 41: // method: MULTIPLY method.base
#line 359 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<AnyMethodIndex>() = cxt.graph.cast<ClassMethod>(yystack_[0].value.as<MethodIndex>());
                }
#line 2780 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 42: // method.list: method
#line 363 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<MethodListIndex>() =
                        cxt.create(MethodList {}, yylhs.location, yystack_[0].value.as<AnyMethodIndex>());
                }
#line 2786 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 43: // method.list: method.list method
#line 364 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<MethodListIndex>() =
                        cxt.graph.append_to_list(yystack_[1].value.as<MethodListIndex>(), yylhs.location,
                                                 yystack_[0].value.as<AnyMethodIndex>());
                }
#line 2792 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 44: // method.list.opt: %empty
#line 368 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<MethodListIndex>() = cxt.create(MethodList {}, yylhs.location);
                }
#line 2798 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 45: // method.list.opt: method.list
#line 369 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<MethodListIndex>() = yystack_[0].value.as<MethodListIndex>();
                }
#line 2804 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 48: // block: block.open argument_declarations.opt block.contents semicolon.opt CLOSECURLY
#line 376 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<BlockIndex>() =
                        cxt.create(BlockNode {}, yylhs.location, yystack_[3].value.as<DeclareArgumentListIndex>(),
                                   yystack_[2].value.as<BlockContentsListIndex>());
                }
#line 2810 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 49: // block: block.open argument_declarations.opt CLOSECURLY
#line 378 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<BlockIndex>() =
                        cxt.create(BlockNode {}, yylhs.location, yystack_[1].value.as<DeclareArgumentListIndex>(),
                                   cxt.create(BlockContentsList {}, yylhs.location));
                }
#line 2816 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 50: // block.opt_list: %empty
#line 382 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<BlockListIndex>() = {};
                }
#line 2822 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 51: // block.opt_list: block.list
#line 383 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<BlockListIndex>() = yystack_[0].value.as<BlockListIndex>();
                }
#line 2828 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 52: // block.list: block
#line 387 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<BlockListIndex>() =
                        cxt.create(BlockList {}, yylhs.location, yystack_[0].value.as<BlockIndex>());
                }
#line 2834 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 53: // block.list: block.list block
#line 388 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<BlockListIndex>() = cxt.graph.append_to_list(
                        yystack_[1].value.as<BlockListIndex>(), yylhs.location, yystack_[0].value.as<BlockIndex>());
                }
#line 2840 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 54: // block.contents: block.contents.item
#line 392 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<BlockContentsListIndex>() =
                        cxt.create(BlockContentsList {}, yylhs.location, yystack_[0].value.as<BlockItemIndex>());
                }
#line 2846 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 55: // block.contents: block.contents SEMICOLON block.contents.item
#line 393 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<BlockContentsListIndex>() =
                        cxt.graph.append_to_list(yystack_[2].value.as<BlockContentsListIndex>(), yylhs.location,
                                                 yystack_[0].value.as<BlockItemIndex>());
                }
#line 2852 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 56: // block.contents.item: expr
#line 397 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<BlockItemIndex>() = yystack_[0].value.as<ExprSeqIndex>();
                }
#line 2858 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 57: // block.contents.item: variable_declarations
#line 398 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<BlockItemIndex>() = yystack_[0].value.as<DeclareVariableListIndex>();
                }
#line 2864 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 58: // block.contents.item: NONLOCALRETURN expr
#line 399 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<BlockItemIndex>() =
                        cxt.create(NonLocalReturnExpr {}, yylhs.location, yystack_[0].value.as<ExprSeqIndex>());
                }
#line 2870 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 59: // msgsend: OPENPAREN binary_op.no_adverb CLOSEPAREN OPENPAREN arguments CLOSEPAREN
                         // block.opt_list
#line 404 "langutils/sc_parser/src/sc_grammar.y"
                {
                    if (yystack_[0].value.as<BlockListIndex>()) {
                        cxt.graph.merge_list(yystack_[2].value.as<ArgumentListIndex>(),
                                             yystack_[0].value.as<BlockListIndex>());
                        cxt.graph.location(*yystack_[2].value.as<ArgumentListIndex>()) = {
                            yystack_[2].location.begin, yystack_[0].location.end
                        }; // spans arguments and block list
                    }
                    yylhs.value.as<ExprSeqIndex>() =
                        cxt.create(MessageNode {}, yylhs.location, yystack_[5].value.as<SelectorIndex>(),
                                   yystack_[2].value.as<ArgumentListIndex>());
                }
#line 2882 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 60: // msgsend: OPENPAREN binary_op.no_adverb CLOSEPAREN OPENPAREN CLOSEPAREN block.list
#line 413 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<ExprSeqIndex>() =
                        cxt.create(MessageNode {}, yylhs.location, yystack_[4].value.as<SelectorIndex>(),
                                   yystack_[0].value.as<BlockListIndex>());
                }
#line 2888 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 61: // msgsend: OPENPAREN binary_op.no_adverb CLOSEPAREN block.list
#line 416 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<ExprSeqIndex>() =
                        cxt.create(MessageNode {}, yylhs.location, yystack_[2].value.as<SelectorIndex>(),
                                   cxt.create(ArgumentList {}, yylhs.location, yystack_[0].value.as<BlockListIndex>()));
                }
#line 2894 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 62: // msgsend: name OPENPAREN arguments CLOSEPAREN block.opt_list
#line 420 "langutils/sc_parser/src/sc_grammar.y"
                {
                    if (yystack_[0].value.as<BlockListIndex>()) {
                        cxt.graph.merge_list(yystack_[2].value.as<ArgumentListIndex>(),
                                             yystack_[0].value.as<BlockListIndex>());
                        cxt.graph.location(*yystack_[2].value.as<ArgumentListIndex>()) = {
                            yystack_[2].location.begin, yystack_[0].location.end
                        }; // spans arguments and block list
                    }
                    yylhs.value.as<ExprSeqIndex>() =
                        cxt.create(MessageNode {}, yylhs.location, yystack_[4].value.as<NamedIdentifierIndex>(),
                                   yystack_[2].value.as<ArgumentListIndex>());
                }
#line 2906 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 63: // msgsend: name OPENPAREN CLOSEPAREN block.list
#line 429 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<ExprSeqIndex>() =
                        cxt.create(MessageNode {}, yylhs.location, yystack_[3].value.as<NamedIdentifierIndex>(),
                                   yystack_[0].value.as<BlockListIndex>());
                }
#line 2912 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 64: // msgsend: name block.list
#line 432 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<ExprSeqIndex>() = cxt.create(
                        MessageNode {}, yylhs.location, yystack_[1].value.as<NamedIdentifierIndex>(),
                        cxt.create(ArgumentList {}, yystack_[0].location, yystack_[0].value.as<BlockListIndex>()));
                }
#line 2918 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 65: // msgsend: expr DOT name arguments.maybe_paren block.opt_list
#line 435 "langutils/sc_parser/src/sc_grammar.y"
                {
                    if (yystack_[0].value.as<BlockListIndex>()) {
                        cxt.graph.merge_list(yystack_[1].value.as<ArgumentListIndex>(),
                                             yystack_[0].value.as<BlockListIndex>());
                        cxt.graph.location(*yystack_[1].value.as<ArgumentListIndex>()) = {
                            yystack_[1].location.begin, yystack_[0].location.end
                        }; // spans arguments and block list
                    }
                    cxt.graph.prepend_to_list(yystack_[1].value.as<ArgumentListIndex>(),
                                              yystack_[4].value.as<ExprSeqIndex>()); // put the receiver in place
                    cxt.graph.location(*yystack_[1].value.as<ArgumentListIndex>()) = {
                        yystack_[4].location.begin, yystack_[1].location.end
                    }; // spans arguments and block list
                    yylhs.value.as<ExprSeqIndex>() =
                        cxt.create(MessageNode {}, yylhs.location, yystack_[2].value.as<NamedIdentifierIndex>(),
                                   yystack_[1].value.as<ArgumentListIndex>());
                }
#line 2932 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 66: // msgsend: expr DOT arguments.paren block.opt_list
#line 446 "langutils/sc_parser/src/sc_grammar.y"
                {
                    if (yystack_[0].value.as<BlockListIndex>()) {
                        cxt.graph.merge_list(yystack_[1].value.as<ArgumentListIndex>(),
                                             yystack_[0].value.as<BlockListIndex>());
                        cxt.graph.location(*yystack_[1].value.as<ArgumentListIndex>()) = {
                            yystack_[1].location.begin, yystack_[0].location.end
                        }; // spans arguments and block list
                    }
                    cxt.graph.prepend_to_list(yystack_[1].value.as<ArgumentListIndex>(),
                                              yystack_[3].value.as<ExprSeqIndex>()); // put the receiver in place
                    cxt.graph.location(*yystack_[1].value.as<ArgumentListIndex>()) = {
                        yystack_[3].location.begin, yystack_[1].location.end
                    }; // spans arguments and block list
                    yylhs.value.as<ExprSeqIndex>() = cxt.create(
                        MessageNode { MessageNode::SelectorMode::Value }, yylhs.location,
                        cxt.create(Missing {}, yystack_[2].location), yystack_[1].value.as<ArgumentListIndex>());
                }
#line 2946 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 67: // msgsend: expr DOT OPENPAREN CLOSEPAREN block.opt_list
#line 456 "langutils/sc_parser/src/sc_grammar.y"
                {
                    auto args = cxt.create(ArgumentList {}, yylhs.location, yystack_[4].value.as<ExprSeqIndex>());
                    if (yystack_[0].value.as<BlockListIndex>())
                        cxt.graph.merge_list(args, yystack_[0].value.as<BlockListIndex>());
                    yylhs.value.as<ExprSeqIndex>() =
                        cxt.create(MessageNode { MessageNode::SelectorMode::Value }, yylhs.location,
                                   cxt.create(Missing {}, yystack_[3].location), args);
                }
#line 2956 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 68: // msgsend: expr DOT error
#line 464 "langutils/sc_parser/src/sc_grammar.y"
                {
                    auto unexpected = cxt.consume_error();
                    std::cout << "GOT AN ERROR WITH A DOT" << std::endl;
                    yylhs.value.as<ExprSeqIndex>() = yystack_[2].value.as<ExprSeqIndex>();
                }
#line 2966 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 69: // msgsend: CLASSNAME OPENSQUARE literal.array.contents CLOSESQUARE
#line 471 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<ExprSeqIndex>() = cxt.create(
                        CollectionNode {}, yylhs.location, cxt.create(ClassNameIdentifier {}, yystack_[3].location),
                        yystack_[1].value.as<ArrayIndex>());
                }
#line 2972 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 70: // msgsend: CLASSNAME block.list
#line 474 "langutils/sc_parser/src/sc_grammar.y"
                {
                    auto args = cxt.create(ArgumentList {}, yylhs.location,
                                           cxt.create(NamedIdentifier {}, yystack_[1].location));
                    cxt.graph.merge_list(args, yystack_[0].value.as<BlockListIndex>());
                    yylhs.value.as<ExprSeqIndex>() =
                        cxt.create(MessageNode {}, yylhs.location, cxt.create(Missing {}, yystack_[1].location), args);
                }
#line 2982 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 71: // msgsend: CLASSNAME OPENPAREN arguments CLOSEPAREN block.opt_list
#line 480 "langutils/sc_parser/src/sc_grammar.y"
                {
                    if (yystack_[0].value.as<BlockListIndex>()) {
                        cxt.graph.merge_list(yystack_[2].value.as<ArgumentListIndex>(),
                                             yystack_[0].value.as<BlockListIndex>());
                        cxt.graph.location(*yystack_[2].value.as<ArgumentListIndex>()) = {
                            yystack_[2].location.begin, yystack_[0].location.end
                        }; // spans arguments and block list
                    }
                    cxt.graph.prepend_to_list(
                        yystack_[2].value.as<ArgumentListIndex>(),
                        cxt.create(ClassNameIdentifier {}, yystack_[4].location)); // put the receiver in place
                    cxt.graph.location(*yystack_[2].value.as<ArgumentListIndex>()) = {
                        yystack_[2].location.begin, yystack_[0].location.end
                    }; // spans arguments and block list
                    yylhs.value.as<ExprSeqIndex>() = cxt.create(
                        MessageNode { MessageNode::SelectorMode::New }, yylhs.location,
                        cxt.create(Missing {}, yystack_[4].location), yystack_[2].value.as<ArgumentListIndex>());
                }
#line 2996 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 72: // msgsend: CLASSNAME OPENPAREN CLOSEPAREN block.opt_list
#line 491 "langutils/sc_parser/src/sc_grammar.y"
                {
                    auto args = cxt.create(ArgumentList {}, yylhs.location, yystack_[0].value.as<BlockListIndex>());
                    cxt.graph.prepend_to_list(
                        args, cxt.create(ClassNameIdentifier {}, yystack_[3].location)); // put the receiver in place
                    yylhs.value.as<ExprSeqIndex>() =
                        cxt.create(MessageNode { MessageNode::SelectorMode::New }, yylhs.location,
                                   cxt.create(Missing {}, yystack_[3].location), args);
                }
#line 3006 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 73: // expr.base: literal
#line 500 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<ExprSeqIndex>() = yystack_[0].value.as<AnyLiteralIndex>();
                }
#line 3012 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 74: // expr.base: name
#line 502 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<ExprSeqIndex>() = yystack_[0].value.as<NamedIdentifierIndex>();
                }
#line 3018 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 75: // expr.base: msgsend
#line 504 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<ExprSeqIndex>() = yystack_[0].value.as<ExprSeqIndex>();
                }
#line 3024 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 76: // expr.base: OPENPAREN block.contents semicolon.opt CLOSEPAREN
#line 506 "langutils/sc_parser/src/sc_grammar.y"
                {
                    auto blk =
                        cxt.create(BlockNode {}, yylhs.location, cxt.create(DeclareArgumentList {}, yylhs.location),
                                   yystack_[2].value.as<BlockContentsListIndex>());
                    yylhs.value.as<ExprSeqIndex>() =
                        cxt.create(MessageNode { MessageNode::SelectorMode::Value }, yylhs.location.flatten(),
                                   cxt.create(Missing {}, yylhs.location.flatten()),
                                   cxt.create(ArgumentList {}, yylhs.location, blk));
                }
#line 3033 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 77: // expr.base: TILDE name
#line 510 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<ExprSeqIndex>() =
                        cxt.create(EnvIdentifierNode {}, yylhs.location, yystack_[0].value.as<NamedIdentifierIndex>());
                }
#line 3039 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 78: // expr.base: expr.base OPENSQUARE arguments CLOSESQUARE
#line 516 "langutils/sc_parser/src/sc_grammar.y"
                {
                    cxt.graph.prepend_to_list(yystack_[1].value.as<ArgumentListIndex>(),
                                              yystack_[3].value.as<ExprSeqIndex>()); // put receiver in place.
                    cxt.graph.location(*yystack_[1].value.as<ArgumentListIndex>()) = yylhs.location;
                    yylhs.value.as<ExprSeqIndex>() = cxt.create(
                        MessageNode { MessageNode::SelectorMode::At }, yylhs.location,
                        cxt.create(Missing {}, yystack_[2].location), yystack_[1].value.as<ArgumentListIndex>());
                }
#line 3049 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 79: // expr.base: expr.base OPENSQUARE CLOSESQUARE
#line 523 "langutils/sc_parser/src/sc_grammar.y"
                {
                    auto args = cxt.create(ArgumentList {}, yylhs.location, yystack_[2].value.as<ExprSeqIndex>());
                    yylhs.value.as<ExprSeqIndex>() =
                        cxt.create(MessageNode { MessageNode::SelectorMode::At }, yylhs.location,
                                   cxt.create(Missing {}, yystack_[1].location), args);
                }
#line 3058 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 80: // expr: expr.base
#line 532 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<ExprSeqIndex>() = yystack_[0].value.as<ExprSeqIndex>();
                }
#line 3064 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 81: // expr: CLASSNAME
#line 536 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<ExprSeqIndex>() = cxt.create(ClassNameIdentifier {}, yylhs.location);
                }
#line 3070 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 82: // expr: expr DOT OPENSQUARE arguments CLOSESQUARE
#line 539 "langutils/sc_parser/src/sc_grammar.y"
                {
                    cxt.graph.prepend_to_list(yystack_[1].value.as<ArgumentListIndex>(),
                                              yystack_[4].value.as<ExprSeqIndex>()); // put receiver in place
                    yylhs.value.as<ExprSeqIndex>() =
                        cxt.create(MessageNode { MessageNode::SelectorMode::At }, yylhs.location,
                                   cxt.create(Missing {}, yylhs.location), yystack_[1].value.as<ArgumentListIndex>());
                }
#line 3079 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 83: // expr: expr DOT OPENSQUARE CLOSESQUARE
#line 544 "langutils/sc_parser/src/sc_grammar.y"
                {
                    auto args = cxt.create(ArgumentList {}, yylhs.location, yystack_[3].value.as<ExprSeqIndex>());
                    yylhs.value.as<ExprSeqIndex>() =
                        cxt.create(MessageNode { MessageNode::SelectorMode::At }, yylhs.location,
                                   cxt.create(Missing {}, yystack_[1].location), args);
                }
#line 3088 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 84: // expr: BACKTICK expr
#line 549 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<ExprSeqIndex>() =
                        cxt.create(ReferenceNode {}, yylhs.location, yystack_[0].value.as<ExprSeqIndex>());
                }
#line 3094 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 85: // expr: expr binary_op expr
#line 552 "langutils/sc_parser/src/sc_grammar.y"
                {
                    auto args = cxt.create(ArgumentList {}, yylhs.location, yystack_[2].value.as<ExprSeqIndex>(),
                                           yystack_[0].value.as<ExprSeqIndex>());
                    yylhs.value.as<ExprSeqIndex>() = cxt.create(MessageNode {}, yylhs.location,
                                                                yystack_[1].value.as<SelectorMaybeAdverbIndex>(), args);
                }
#line 3103 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 86: // expr: name EQUALSSIGN expr
#line 558 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<ExprSeqIndex>() =
                        cxt.create(AssignmentNode {}, yylhs.location, yystack_[2].value.as<NamedIdentifierIndex>(),
                                   yystack_[0].value.as<ExprSeqIndex>());
                }
#line 3109 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 87: // expr: TILDE name EQUALSSIGN expr
#line 561 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<ExprSeqIndex>() =
                        cxt.create(AssignmentNode { AssignmentNode::Target::Environment }, yylhs.location,
                                   yystack_[2].value.as<NamedIdentifierIndex>(), yystack_[0].value.as<ExprSeqIndex>());
                }
#line 3115 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 88: // expr: expr DOT name EQUALSSIGN expr
#line 564 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<ExprSeqIndex>() =
                        cxt.create(SetterNode {}, yylhs.location, yystack_[4].value.as<ExprSeqIndex>(),
                                   yystack_[2].value.as<NamedIdentifierIndex>(), yystack_[0].value.as<ExprSeqIndex>());
                }
#line 3121 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 89: // expr: name OPENPAREN arguments CLOSEPAREN EQUALSSIGN expr
#line 567 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<ExprSeqIndex>() =
                        cxt.create(SetterNode {}, yylhs.location, yystack_[3].value.as<ArgumentListIndex>(),
                                   yystack_[5].value.as<NamedIdentifierIndex>(), yystack_[0].value.as<ExprSeqIndex>());
                }
#line 3127 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 90: // expr: expr.base OPENSQUARE arguments CLOSESQUARE EQUALSSIGN expr
#line 575 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<ExprSeqIndex>() =
                        cxt.create(AssignmentAtNode {}, yylhs.location, yystack_[5].value.as<ExprSeqIndex>(),
                                   yystack_[3].value.as<ArgumentListIndex>(), yystack_[0].value.as<ExprSeqIndex>());
                }
#line 3133 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 91: // expr: expr.base OPENSQUARE CLOSESQUARE EQUALSSIGN expr
#line 577 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<ExprSeqIndex>() = cxt.create(
                        AssignmentAtNode {}, yylhs.location, yystack_[4].value.as<ExprSeqIndex>(),
                        cxt.create(ArgumentList {}, yystack_[3].location), yystack_[0].value.as<ExprSeqIndex>());
                }
#line 3139 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 92: // expr: expr DOT OPENSQUARE arguments CLOSESQUARE EQUALSSIGN expr
#line 580 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<ExprSeqIndex>() =
                        cxt.create(AssignmentAtNode {}, yylhs.location, yystack_[6].value.as<ExprSeqIndex>(),
                                   yystack_[3].value.as<ArgumentListIndex>(), yystack_[0].value.as<ExprSeqIndex>());
                }
#line 3145 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 93: // expr: expr DOT OPENSQUARE CLOSESQUARE EQUALSSIGN expr
#line 583 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<ExprSeqIndex>() = cxt.create(
                        AssignmentAtNode {}, yylhs.location, yystack_[5].value.as<ExprSeqIndex>(),
                        cxt.create(ArgumentList {}, yystack_[3].location), yystack_[0].value.as<ExprSeqIndex>());
                }
#line 3151 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 94: // expr.seq.base: expr
#line 587 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<ExprSeqIndex>() = yystack_[0].value.as<ExprSeqIndex>();
                }
#line 3157 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 95: // expr.seq.base: expr.seq.base SEMICOLON expr
#line 589 "langutils/sc_parser/src/sc_grammar.y"
                {
                    // This piece of logic is here because exprs can contain expr.seq, so we avoid creating the list
                    // node if we can.
                    if (cxt.graph.is_a<ExprSeqIndex>(*yystack_[2].value.as<ExprSeqIndex>())) {
                        cxt.graph.location(*yystack_[2].value.as<ExprSeqIndex>()) =
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
#line 3172 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 96: // expr.seq: expr.seq.base semicolon.opt
#line 601 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<ExprSeqIndex>() = yystack_[1].value.as<ExprSeqIndex>();
                }
#line 3178 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 97: // adverb: DOT name
#line 604 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<AdverbIndex>() = yystack_[0].value.as<NamedIdentifierIndex>();
                }
#line 3184 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 98: // adverb: DOT integer
#line 605 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<AdverbIndex>() = yystack_[0].value.as<IntLitIndex>();
                }
#line 3190 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 99: // adverb: DOT OPENPAREN expr.seq CLOSEPAREN
#line 606 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<AdverbIndex>() =
                        cxt.create(AdverbExprNode {}, yylhs.location, yystack_[1].value.as<ExprSeqIndex>());
                }
#line 3196 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 100: // argument_declarations.list: name
#line 612 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<DeclareArgumentListIndex>() = cxt.create(
                        DeclareArgumentList {}, yylhs.location, yystack_[0].value.as<NamedIdentifierIndex>());
                }
#line 3202 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 101: // argument_declarations.list: name EQUALSSIGN literal
#line 614 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<DeclareArgumentListIndex>() =
                        cxt.create(DeclareArgumentList {}, yylhs.location,
                                   cxt.create(DeclareArgumentWithDefaultNode { true }, yylhs.location,
                                              yystack_[2].value.as<NamedIdentifierIndex>(),
                                              yystack_[0].value.as<AnyLiteralIndex>()));
                }
#line 3208 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 102: // argument_declarations.list: name OPENPAREN expr.seq CLOSEPAREN
#line 616 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<DeclareArgumentListIndex>() = cxt.create(
                        DeclareArgumentList {}, yylhs.location,
                        cxt.create(DeclareArgumentWithDefaultNode { false }, yylhs.location,
                                   yystack_[3].value.as<NamedIdentifierIndex>(), yystack_[1].value.as<ExprSeqIndex>()));
                }
#line 3214 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 103: // argument_declarations.list: name EQUALSSIGN OPENPAREN expr.seq CLOSEPAREN
#line 618 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<DeclareArgumentListIndex>() = cxt.create(
                        DeclareArgumentList {}, yylhs.location,
                        cxt.create(DeclareArgumentWithDefaultNode { false }, yylhs.location,
                                   yystack_[4].value.as<NamedIdentifierIndex>(), yystack_[1].value.as<ExprSeqIndex>()));
                }
#line 3220 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 104: // argument_declarations.list: argument_declarations.list COMMA name
#line 620 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<DeclareArgumentListIndex>() =
                        cxt.graph.append_to_list(yystack_[2].value.as<DeclareArgumentListIndex>(), yylhs.location,
                                                 yystack_[0].value.as<NamedIdentifierIndex>());
                }
#line 3226 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 105: // argument_declarations.list: argument_declarations.list COMMA name EQUALSSIGN literal
#line 622 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<DeclareArgumentListIndex>() =
                        cxt.graph.append_to_list(yystack_[4].value.as<DeclareArgumentListIndex>(), yylhs.location,
                                                 cxt.create(DeclareArgumentWithDefaultNode { true }, yylhs.location,
                                                            yystack_[2].value.as<NamedIdentifierIndex>(),
                                                            yystack_[0].value.as<AnyLiteralIndex>()));
                }
#line 3232 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 106: // argument_declarations.list: argument_declarations.list COMMA name OPENPAREN expr.seq
                          // CLOSEPAREN
#line 624 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<DeclareArgumentListIndex>() = cxt.graph.append_to_list(
                        yystack_[5].value.as<DeclareArgumentListIndex>(), yylhs.location,
                        cxt.create(DeclareArgumentWithDefaultNode { false }, yylhs.location,
                                   yystack_[3].value.as<NamedIdentifierIndex>(), yystack_[1].value.as<ExprSeqIndex>()));
                }
#line 3238 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 107: // argument_declarations.list: argument_declarations.list COMMA name EQUALSSIGN OPENPAREN
                          // expr.seq CLOSEPAREN
#line 626 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<DeclareArgumentListIndex>() = cxt.graph.append_to_list(
                        yystack_[6].value.as<DeclareArgumentListIndex>(), yylhs.location,
                        cxt.create(DeclareArgumentWithDefaultNode { false }, yylhs.location,
                                   yystack_[4].value.as<NamedIdentifierIndex>(), yystack_[1].value.as<ExprSeqIndex>()));
                }
#line 3244 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 108: // argument_declarations.pipelist: name literal
#line 631 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<DeclareArgumentListIndex>() =
                        cxt.create(DeclareArgumentList {}, yylhs.location,
                                   cxt.create(DeclareArgumentWithDefaultNode { true }, yylhs.location,
                                              yystack_[1].value.as<NamedIdentifierIndex>(),
                                              yystack_[0].value.as<AnyLiteralIndex>()));
                }
#line 3250 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 109: // argument_declarations.pipelist: name
#line 633 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<DeclareArgumentListIndex>() = cxt.create(
                        DeclareArgumentList {}, yylhs.location, yystack_[0].value.as<NamedIdentifierIndex>());
                }
#line 3256 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 110: // argument_declarations.pipelist: name EQUALSSIGN literal
#line 635 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<DeclareArgumentListIndex>() =
                        cxt.create(DeclareArgumentList {}, yylhs.location,
                                   cxt.create(DeclareArgumentWithDefaultNode { true }, yylhs.location,
                                              yystack_[2].value.as<NamedIdentifierIndex>(),
                                              yystack_[0].value.as<AnyLiteralIndex>()));
                }
#line 3262 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 111: // argument_declarations.pipelist: name OPENPAREN expr.seq CLOSEPAREN
#line 637 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<DeclareArgumentListIndex>() = cxt.create(
                        DeclareArgumentList {}, yylhs.location,
                        cxt.create(DeclareArgumentWithDefaultNode { false }, yylhs.location,
                                   yystack_[3].value.as<NamedIdentifierIndex>(), yystack_[1].value.as<ExprSeqIndex>()));
                }
#line 3268 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 112: // argument_declarations.pipelist: name EQUALSSIGN OPENPAREN expr.seq CLOSEPAREN
#line 639 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<DeclareArgumentListIndex>() = cxt.create(
                        DeclareArgumentList {}, yylhs.location,
                        cxt.create(DeclareArgumentWithDefaultNode { false }, yylhs.location,
                                   yystack_[4].value.as<NamedIdentifierIndex>(), yystack_[1].value.as<ExprSeqIndex>()));
                }
#line 3274 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 113: // argument_declarations.pipelist: argument_declarations.pipelist comma.opt name literal
#line 641 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<DeclareArgumentListIndex>() =
                        cxt.graph.append_to_list(yystack_[3].value.as<DeclareArgumentListIndex>(), yylhs.location,
                                                 cxt.create(DeclareArgumentWithDefaultNode { true }, yylhs.location,
                                                            yystack_[1].value.as<NamedIdentifierIndex>(),
                                                            yystack_[0].value.as<AnyLiteralIndex>()));
                }
#line 3280 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 114: // argument_declarations.pipelist: argument_declarations.pipelist comma.opt name
#line 643 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<DeclareArgumentListIndex>() =
                        cxt.graph.append_to_list(yystack_[2].value.as<DeclareArgumentListIndex>(), yylhs.location,
                                                 yystack_[0].value.as<NamedIdentifierIndex>());
                }
#line 3286 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 115: // argument_declarations.pipelist: argument_declarations.pipelist comma.opt name EQUALSSIGN
                          // literal
#line 645 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<DeclareArgumentListIndex>() =
                        cxt.graph.append_to_list(yystack_[4].value.as<DeclareArgumentListIndex>(), yylhs.location,
                                                 cxt.create(DeclareArgumentWithDefaultNode { true }, yylhs.location,
                                                            yystack_[2].value.as<NamedIdentifierIndex>(),
                                                            yystack_[0].value.as<AnyLiteralIndex>()));
                }
#line 3292 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 116: // argument_declarations.pipelist: argument_declarations.pipelist comma.opt name OPENPAREN
                          // expr.seq CLOSEPAREN
#line 647 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<DeclareArgumentListIndex>() = cxt.graph.append_to_list(
                        yystack_[5].value.as<DeclareArgumentListIndex>(), yylhs.location,
                        cxt.create(DeclareArgumentWithDefaultNode { false }, yylhs.location,
                                   yystack_[3].value.as<NamedIdentifierIndex>(), yystack_[1].value.as<ExprSeqIndex>()));
                }
#line 3298 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 117: // argument_declarations.pipelist: argument_declarations.pipelist comma.opt name EQUALSSIGN
                          // OPENPAREN expr.seq CLOSEPAREN
#line 649 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<DeclareArgumentListIndex>() = cxt.graph.append_to_list(
                        yystack_[6].value.as<DeclareArgumentListIndex>(), yylhs.location,
                        cxt.create(DeclareArgumentWithDefaultNode { false }, yylhs.location,
                                   yystack_[4].value.as<NamedIdentifierIndex>(), yystack_[1].value.as<ExprSeqIndex>()));
                }
#line 3304 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 118: // argument_declarations: ARG SEMICOLON
#line 653 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<DeclareArgumentListIndex>() = cxt.create(DeclareArgumentList {}, yylhs.location);
                }
#line 3310 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 119: // argument_declarations: ARG argument_declarations.list comma.opt SEMICOLON
#line 654 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<DeclareArgumentListIndex>() = yystack_[2].value.as<DeclareArgumentListIndex>();
                }
#line 3316 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 120: // argument_declarations: ARG argument_declarations.list ELLIPSIS name SEMICOLON
#line 656 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<DeclareArgumentListIndex>() =
                        cxt.graph.append_to_list(yystack_[3].value.as<DeclareArgumentListIndex>(), yylhs.location,
                                                 cxt.create(DeclareArgumentVariadicNode {}, yystack_[1].location,
                                                            yystack_[1].value.as<NamedIdentifierIndex>()));
                }
#line 3322 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 121: // argument_declarations: ARG argument_declarations.list ELLIPSIS name COMMA name SEMICOLON
#line 658 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<DeclareArgumentListIndex>() =
                        cxt.graph.append_to_list(yystack_[5].value.as<DeclareArgumentListIndex>(), yylhs.location,
                                                 cxt.create(DeclareArgumentVariadicNode {}, yystack_[3].location,
                                                            yystack_[3].value.as<NamedIdentifierIndex>()),
                                                 cxt.create(DeclareArgumentVariadicNode {}, yystack_[1].location,
                                                            yystack_[1].value.as<NamedIdentifierIndex>()));
                }
#line 3328 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 122: // argument_declarations: PIPE PIPE
#line 660 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<DeclareArgumentListIndex>() = cxt.create(DeclareArgumentList {}, yylhs.location);
                }
#line 3334 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 123: // argument_declarations: PIPE argument_declarations.pipelist comma.opt PIPE
#line 662 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<DeclareArgumentListIndex>() = yystack_[2].value.as<DeclareArgumentListIndex>();
                }
#line 3340 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 124: // argument_declarations: PIPE argument_declarations.pipelist ELLIPSIS name PIPE
#line 664 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<DeclareArgumentListIndex>() =
                        cxt.graph.append_to_list(yystack_[3].value.as<DeclareArgumentListIndex>(), yystack_[3].location,
                                                 cxt.create(DeclareArgumentVariadicNode {}, yystack_[1].location,
                                                            yystack_[1].value.as<NamedIdentifierIndex>()));
                }
#line 3346 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 125: // argument_declarations: PIPE argument_declarations.pipelist ELLIPSIS name COMMA name PIPE
#line 666 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<DeclareArgumentListIndex>() =
                        cxt.graph.append_to_list(yystack_[5].value.as<DeclareArgumentListIndex>(), yylhs.location,
                                                 cxt.create(DeclareArgumentVariadicNode {}, yystack_[3].location,
                                                            yystack_[3].value.as<NamedIdentifierIndex>()),
                                                 cxt.create(DeclareArgumentVariadicNode {}, yystack_[1].location,
                                                            yystack_[1].value.as<NamedIdentifierIndex>()));
                }
#line 3352 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 126: // argument_declarations.opt: %empty
#line 671 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<DeclareArgumentListIndex>() = cxt.create(DeclareArgumentList {}, yylhs.location);
                }
#line 3358 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 127: // argument_declarations.opt: argument_declarations
#line 672 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<DeclareArgumentListIndex>() = yystack_[0].value.as<DeclareArgumentListIndex>();
                }
#line 3364 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 128: // variable_declarations.list.item: name
#line 677 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<DeclareAnyVariableIndex>() = yystack_[0].value.as<NamedIdentifierIndex>();
                }
#line 3370 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 129: // variable_declarations.list.item: name EQUALSSIGN expr
#line 679 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<DeclareAnyVariableIndex>() =
                        cxt.create(DeclareVariableWithDefaultNode {}, yylhs.location,
                                   yystack_[2].value.as<NamedIdentifierIndex>(), yystack_[0].value.as<ExprSeqIndex>());
                }
#line 3376 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 130: // variable_declarations.list.item: name OPENPAREN expr.seq CLOSEPAREN
#line 681 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<DeclareAnyVariableIndex>() =
                        cxt.create(DeclareVariableWithDefaultNode {}, yylhs.location,
                                   yystack_[3].value.as<NamedIdentifierIndex>(), yystack_[1].value.as<ExprSeqIndex>());
                }
#line 3382 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 131: // variable_declarations.list: variable_declarations.list.item
#line 690 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<DeclareVariableListIndex>() = cxt.create(
                        DeclareVariableList {}, yylhs.location, yystack_[0].value.as<DeclareAnyVariableIndex>());
                }
#line 3388 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 132: // variable_declarations.list: variable_declarations.list COMMA
                          // variable_declarations.list.item
#line 692 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<DeclareVariableListIndex>() =
                        cxt.graph.append_to_list(yystack_[2].value.as<DeclareVariableListIndex>(),
                                                 yystack_[0].value.as<DeclareAnyVariableIndex>());
                }
#line 3394 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 133: // variable_declarations: VAR variable_declarations.list
#line 695 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<DeclareVariableListIndex>() = yystack_[0].value.as<DeclareVariableListIndex>();
                }
#line 3400 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 134: // arguments.entries: KEYBINOP expr.seq
#line 699 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<ArgumentEntryIndex>() =
                        cxt.create(KwArgNode {}, yylhs.location,
                                   cxt.create(SymbolNode { SymbolNode::Kind::KeyBinOp }, yystack_[1].location),
                                   yystack_[0].value.as<ExprSeqIndex>());
                }
#line 3406 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 135: // arguments.entries: MULTIPLY expr.seq
#line 700 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<ArgumentEntryIndex>() =
                        cxt.create(VariadicArgNode {}, yylhs.location, yystack_[0].value.as<ExprSeqIndex>());
                }
#line 3412 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 136: // arguments.entries: expr.seq
#line 701 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<ArgumentEntryIndex>() = yystack_[0].value.as<ExprSeqIndex>();
                }
#line 3418 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 137: // arguments.no_trailing: arguments.entries
#line 705 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<ArgumentListIndex>() =
                        cxt.create(ArgumentList {}, yylhs.location, yystack_[0].value.as<ArgumentEntryIndex>());
                }
#line 3424 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 138: // arguments.no_trailing: arguments.no_trailing COMMA arguments.entries
#line 706 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<ArgumentListIndex>() =
                        cxt.graph.append_to_list(yystack_[2].value.as<ArgumentListIndex>(), yylhs.location,
                                                 yystack_[0].value.as<ArgumentEntryIndex>());
                }
#line 3430 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 139: // arguments: arguments.no_trailing comma.opt
#line 709 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<ArgumentListIndex>() = yystack_[1].value.as<ArgumentListIndex>();
                }
#line 3436 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 140: // arguments.paren: OPENPAREN arguments CLOSEPAREN
#line 712 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<ArgumentListIndex>() = yystack_[1].value.as<ArgumentListIndex>();
                }
#line 3442 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 141: // arguments.maybe_paren: %empty
#line 715 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<ArgumentListIndex>() = cxt.create(ArgumentList {}, yylhs.location);
                }
#line 3448 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 142: // arguments.maybe_paren: arguments.paren
#line 716 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<ArgumentListIndex>() = yystack_[0].value.as<ArgumentListIndex>();
                }
#line 3454 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 143: // literal.terminal: symbol
#line 720 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<AnyLiteralIndex>() = yystack_[0].value.as<SymbolLitIndex>();
                }
#line 3460 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 144: // literal.terminal: string
#line 721 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<AnyLiteralIndex>() = yystack_[0].value.as<StringLitIndex>();
                }
#line 3466 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 145: // literal.terminal: integer
#line 722 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<AnyLiteralIndex>() = yystack_[0].value.as<IntLitIndex>();
                }
#line 3472 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 146: // literal.terminal: float
#line 723 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<AnyLiteralIndex>() = yystack_[0].value.as<FloatProducingIndex>();
                }
#line 3478 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 147: // literal.terminal: boolean
#line 724 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<AnyLiteralIndex>() = yystack_[0].value.as<BooleanLitIndex>();
                }
#line 3484 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 148: // literal.terminal: nil
#line 725 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<AnyLiteralIndex>() = yystack_[0].value.as<NilLitIndex>();
                }
#line 3490 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 149: // literal.terminal: ascii
#line 726 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<AnyLiteralIndex>() = yystack_[0].value.as<ASCIIIndex>();
                }
#line 3496 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 150: // literal.terminal: block
#line 727 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<AnyLiteralIndex>() = yystack_[0].value.as<BlockIndex>();
                }
#line 3502 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 151: // literal.array.contents: %empty
#line 732 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<ArrayIndex>() = cxt.create(ArrayNode {}, yylhs.location);
                }
#line 3508 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 152: // literal.array.contents: expr.seq
#line 734 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<ArrayIndex>() =
                        cxt.create(ArrayNode {}, yylhs.location, yystack_[0].value.as<ExprSeqIndex>());
                }
#line 3514 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 153: // literal.array.contents: expr.seq COLON expr.seq
#line 736 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<ArrayIndex>() =
                        cxt.create(ArrayNode {}, yylhs.location, yystack_[2].value.as<ExprSeqIndex>(),
                                   yystack_[0].value.as<ExprSeqIndex>());
                }
#line 3520 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 154: // literal.array.contents: KEYBINOP expr.seq
#line 738 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<ArrayIndex>() =
                        cxt.create(ArrayNode {}, yylhs.location,
                                   cxt.create(SymbolNode { SymbolNode::Kind::KeyBinOp }, yystack_[1].location),
                                   yystack_[0].value.as<ExprSeqIndex>());
                }
#line 3526 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 155: // literal.array.contents: literal.array.contents COMMA expr.seq
#line 740 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<ArrayIndex>() = cxt.graph.append_to_list(
                        yystack_[2].value.as<ArrayIndex>(), yylhs.location, yystack_[0].value.as<ExprSeqIndex>());
                }
#line 3532 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 156: // literal.array.contents: literal.array.contents COMMA expr.seq COLON expr.seq
#line 742 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<ArrayIndex>() = cxt.graph.append_to_list(
                        yystack_[4].value.as<ArrayIndex>(), yylhs.location, yystack_[2].value.as<ExprSeqIndex>(),
                        yystack_[0].value.as<ExprSeqIndex>());
                }
#line 3538 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 157: // literal.array.contents: literal.array.contents COMMA KEYBINOP expr.seq
#line 744 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<ArrayIndex>() = cxt.graph.append_to_list(
                        yystack_[3].value.as<ArrayIndex>(), yylhs.location,
                        cxt.create(SymbolNode { SymbolNode::Kind::KeyBinOp }, yystack_[1].location),
                        yystack_[0].value.as<ExprSeqIndex>());
                }
#line 3544 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 158: // literal.dictionary.entry: expr COLON expr.seq
#line 752 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<DictionaryEntryIndex>() =
                        cxt.create(DictionaryEntryNode {}, yylhs.location, yystack_[2].value.as<ExprSeqIndex>(),
                                   yystack_[0].value.as<ExprSeqIndex>());
                }
#line 3550 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 159: // literal.dictionary.entry: KEYBINOP expr.seq
#line 754 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<DictionaryEntryIndex>() =
                        cxt.create(DictionaryEntryNode {}, yylhs.location,
                                   cxt.create(SymbolNode { SymbolNode::Kind::KeyBinOp }, yystack_[1].location),
                                   yystack_[0].value.as<ExprSeqIndex>());
                }
#line 3556 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 160: // literal.dictionary.entries: %empty
#line 759 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<DictionaryIndex>() = cxt.create(DictionaryNode {}, yylhs.location);
                }
#line 3562 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 161: // literal.dictionary.entries: literal.dictionary.entry
#line 761 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<DictionaryIndex>() =
                        cxt.create(DictionaryNode {}, yylhs.location, yystack_[0].value.as<DictionaryEntryIndex>());
                }
#line 3568 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 162: // literal.dictionary.entries: literal.dictionary.entries COMMA literal.dictionary.entry
#line 763 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<DictionaryIndex>() =
                        cxt.graph.append_to_list(yystack_[2].value.as<DictionaryIndex>(), yylhs.location,
                                                 yystack_[0].value.as<DictionaryEntryIndex>());
                }
#line 3574 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 163: // literal.dictionary: OPENPAREN literal.dictionary.entries comma.opt CLOSEPAREN
#line 768 "langutils/sc_parser/src/sc_grammar.y"
                {
                    cxt.graph.location(*yystack_[2].value.as<DictionaryIndex>()) = yylhs.location;
                    yylhs.value.as<DictionaryIndex>() = yystack_[2].value.as<DictionaryIndex>();
                }
#line 3580 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 164: // literal.array: OPENSQUARE literal.array.contents comma.opt CLOSESQUARE
#line 773 "langutils/sc_parser/src/sc_grammar.y"
                {
                    cxt.graph.location(*yystack_[2].value.as<ArrayIndex>()) = yylhs.location;
                    yylhs.value.as<ArrayIndex>() = yystack_[2].value.as<ArrayIndex>();
                }
#line 3586 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 165: // literal.array: HASH OPENSQUARE literal.array.contents comma.opt CLOSESQUARE
#line 775 "langutils/sc_parser/src/sc_grammar.y"
                {
                    cxt.graph.payload(yystack_[2].value.as<ArrayIndex>()).is_immutable = true;
                    cxt.graph.location(*yystack_[2].value.as<ArrayIndex>()) = yylhs.location;
                    yylhs.value.as<ArrayIndex>() = yystack_[2].value.as<ArrayIndex>();
                }
#line 3596 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 166: // literal: literal.terminal
#line 783 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<AnyLiteralIndex>() = yystack_[0].value.as<AnyLiteralIndex>();
                }
#line 3602 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 167: // literal: literal.array
#line 784 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<AnyLiteralIndex>() = yystack_[0].value.as<ArrayIndex>();
                }
#line 3608 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 168: // literal: literal.dictionary
#line 785 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<AnyLiteralIndex>() = yystack_[0].value.as<DictionaryIndex>();
                }
#line 3614 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 169: // name: NAME
#line 788 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<NamedIdentifierIndex>() = cxt.create(NamedIdentifier {}, yylhs.location);
                }
#line 3620 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 170: // binary_op.raw: BINOP
#line 797 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<SelectorIndex>() = cxt.create(SelectorNode { false }, yylhs.location);
                }
#line 3626 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 171: // binary_op.raw: READWRITEVAR
#line 798 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<SelectorIndex>() = cxt.create(SelectorNode { false }, yylhs.location);
                }
#line 3632 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 172: // binary_op.raw: LESSTHAN
#line 799 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<SelectorIndex>() = cxt.create(SelectorNode { false }, yylhs.location);
                }
#line 3638 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 173: // binary_op.raw: GREATERTHAN
#line 800 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<SelectorIndex>() = cxt.create(SelectorNode { false }, yylhs.location);
                }
#line 3644 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 174: // binary_op.raw: MINUS
#line 801 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<SelectorIndex>() = cxt.create(SelectorNode { false }, yylhs.location);
                }
#line 3650 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 175: // binary_op.raw: MULTIPLY
#line 802 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<SelectorIndex>() = cxt.create(SelectorNode { false }, yylhs.location);
                }
#line 3656 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 176: // binary_op.raw: ADD
#line 803 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<SelectorIndex>() = cxt.create(SelectorNode { false }, yylhs.location);
                }
#line 3662 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 177: // binary_op.raw: PIPE
#line 804 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<SelectorIndex>() = cxt.create(SelectorNode { false }, yylhs.location);
                }
#line 3668 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 178: // binary_op.no_adverb: binary_op.raw
#line 808 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<SelectorIndex>() = yystack_[0].value.as<SelectorIndex>();
                }
#line 3674 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 179: // binary_op.no_adverb: KEYBINOP
#line 809 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<SelectorIndex>() = cxt.create(SelectorNode { true }, yylhs.location);
                }
#line 3680 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 180: // binary_op: binary_op.no_adverb adverb
#line 814 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<SelectorMaybeAdverbIndex>() =
                        cxt.create(SelectorWAdverb {}, yylhs.location, yystack_[1].value.as<SelectorIndex>(),
                                   yystack_[0].value.as<AdverbIndex>());
                }
#line 3686 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 181: // binary_op: binary_op.no_adverb
#line 816 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<SelectorMaybeAdverbIndex>() = yystack_[0].value.as<SelectorIndex>();
                }
#line 3692 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 186: // ascii: ASCII
#line 822 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<ASCIIIndex>() = cxt.create(ASCIINode {}, yylhs.location);
                }
#line 3698 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 187: // nil: NIL
#line 824 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<NilLitIndex>() = cxt.create(NilNode {}, yylhs.location);
                }
#line 3704 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 188: // boolean: TRUE
#line 827 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<BooleanLitIndex>() = cxt.create(BooleanNode { true }, yylhs.location);
                }
#line 3710 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 189: // boolean: FALSE
#line 828 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<BooleanLitIndex>() = cxt.create(BooleanNode { false }, yylhs.location);
                }
#line 3716 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 190: // symbol: SYMBOL_QUOTE
#line 832 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<SymbolLitIndex>() =
                        cxt.create(SymbolNode { SymbolNode::Kind::Quote }, yylhs.location);
                }
#line 3722 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 191: // symbol: SYMBOL_SLASH
#line 833 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<SymbolLitIndex>() =
                        cxt.create(SymbolNode { SymbolNode::Kind::Slash }, yylhs.location);
                }
#line 3728 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 192: // string: STRINGLINE
#line 837 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<StringLitIndex>() =
                        cxt.create(StringLineList {}, yylhs.location, cxt.create(StringLineNode {}, yylhs.location));
                }
#line 3734 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 193: // string: string STRINGLINE
#line 838 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<StringLitIndex>() = cxt.graph.append_to_list(
                        yystack_[1].value.as<StringLitIndex>(), cxt.create(StringLineNode {}, yystack_[0].location));
                }
#line 3740 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 194: // integer: INTEGER
#line 842 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<IntLitIndex>() = cxt.create(IntNode {}, yylhs.location);
                }
#line 3746 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 195: // integer: INTEGER_RADIX
#line 843 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<IntLitIndex>() = cxt.create(IntNode { IntNode::Kind::Radix }, yylhs.location);
                }
#line 3752 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 196: // integer: HEXADECIMAL
#line 844 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<IntLitIndex>() = cxt.create(IntNode { IntNode::Kind::Hexadecimal }, yylhs.location);
                }
#line 3758 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 197: // integer: MINUS integer
#line 846 "langutils/sc_parser/src/sc_grammar.y"
                {
                    // Reaches into the previous integer and changes its sign.
                    cxt.graph.payload(yystack_[0].value.as<IntLitIndex>()).sign = IntNode::Sign::Negative;
                    yylhs.value.as<IntLitIndex>() = yystack_[0].value.as<IntLitIndex>();
                }
#line 3768 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 198: // float.raw_unsigned: FLOAT
#line 854 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<FloatLitIndex>() = cxt.create(FloatNode {}, yylhs.location);
                }
#line 3774 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 199: // float.raw_unsigned: FLOAT_RADIX
#line 855 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<FloatLitIndex>() = cxt.create(FloatNode { FloatNode::Kind::Radix }, yylhs.location);
                }
#line 3780 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 200: // float.raw_unsigned: FLOAT_EXPONENT
#line 856 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<FloatLitIndex>() =
                        cxt.create(FloatNode { FloatNode::Kind::Exponent }, yylhs.location);
                }
#line 3786 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 201: // float.raw_unsigned: FLOAT_INF
#line 857 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<FloatLitIndex>() = cxt.create(FloatNode { FloatNode::Kind::Inf }, yylhs.location);
                }
#line 3792 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 202: // float.raw: float.raw_unsigned
#line 862 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<FloatLitIndex>() = yystack_[0].value.as<FloatLitIndex>();
                }
#line 3798 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 203: // float.raw: MINUS float.raw_unsigned
#line 864 "langutils/sc_parser/src/sc_grammar.y"
                {
                    cxt.graph.payload(yystack_[0].value.as<FloatLitIndex>()).sign = FloatNode::Sign::Negative;
                    yylhs.value.as<FloatLitIndex>() = yystack_[0].value.as<FloatLitIndex>();
                }
#line 3804 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 204: // accidental.unsigned: ACCIDENTAL_STEPS
#line 869 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<AccidentalLitIndex>() =
                        cxt.create(AccidentalNode { AccidentalNode::Kind::Steps }, yylhs.location);
                }
#line 3810 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 205: // accidental.unsigned: ACCIDENTAL_CENTS
#line 871 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<AccidentalLitIndex>() =
                        cxt.create(AccidentalNode { AccidentalNode::Kind::Cents }, yylhs.location);
                }
#line 3816 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 206: // accidental: accidental.unsigned
#line 876 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<AccidentalLitIndex>() = yystack_[0].value.as<AccidentalLitIndex>();
                }
#line 3822 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 207: // accidental: MINUS accidental.unsigned
#line 878 "langutils/sc_parser/src/sc_grammar.y"
                {
                    cxt.graph.payload(yystack_[0].value.as<AccidentalLitIndex>()).sign = AccidentalNode::Sign::Negative;
                    yylhs.value.as<AccidentalLitIndex>() = yystack_[0].value.as<AccidentalLitIndex>();
                }
#line 3828 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 208: // float: float.raw
#line 882 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<FloatProducingIndex>() = yystack_[0].value.as<FloatLitIndex>();
                }
#line 3834 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 209: // float: accidental
#line 883 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<FloatProducingIndex>() = yystack_[0].value.as<AccidentalLitIndex>();
                }
#line 3840 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 210: // float: float.raw PI
#line 884 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<FloatProducingIndex>() =
                        cxt.create(PiNode {}, yylhs.location, yystack_[1].value.as<FloatLitIndex>());
                }
#line 3846 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 211: // float: integer PI
#line 885 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<FloatProducingIndex>() =
                        cxt.create(PiNode {}, yylhs.location, yystack_[1].value.as<IntLitIndex>());
                }
#line 3852 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 212: // float: PI
#line 886 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<FloatProducingIndex>() =
                        cxt.create(PiNode {}, yylhs.location, cxt.create(Missing {}, yylhs.location));
                }
#line 3858 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 213: // float: MINUS PI
#line 887 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<FloatProducingIndex>() = cxt.create(
                        PiNode { PiNode::Sign::Negative }, yylhs.location, cxt.create(Missing {}, yylhs.location));
                }
#line 3864 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 214: // accessor: %empty
#line 891 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<ReadWriteAccessor>() = ReadWriteAccessor::Private;
                }
#line 3870 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 215: // accessor: LESSTHAN
#line 892 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<ReadWriteAccessor>() = ReadWriteAccessor::PublicRead;
                }
#line 3876 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 216: // accessor: READWRITEVAR
#line 893 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<ReadWriteAccessor>() = ReadWriteAccessor::PublicReadAndWrite;
                }
#line 3882 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;

                case 217: // accessor: GREATERTHAN
#line 894 "langutils/sc_parser/src/sc_grammar.y"
                {
                    yylhs.value.as<ReadWriteAccessor>() = ReadWriteAccessor::PublicWrite;
                }
#line 3888 "langutils/sc_parser/src/sc_grammar_parser.cpp"
                break;


#line 3892 "langutils/sc_parser/src/sc_grammar_parser.cpp"

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
                                            "region.item",
                                            "region",
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


const short parser::yypact_ninf_ = -193;

const signed char parser::yytable_ninf_ = -1;

const short parser::yypact_[] = {
    122,  20,   351,  6,    38,   181,  19,   -193, -193, -193, 27,   43,   -193, -193, 1149, 400,  93,   27,   -193,
    -193, -193, -193, -193, -193, -193, -193, -193, -193, -193, -193, -193, -193, 99,   -193, -193, -193, -193, -193,
    1606, 1293, 1,    -193, -193, 100,  1597, -193, -193, -193, -193, 12,   -193, -193, -193, -193, 82,   80,   -193,
    83,   -193, -193, -193, 121,  -193, -193, -193, 1341, 1341, -193, -193, 123,  97,   142,  454,  1293, 1597, 141,
    107,  145,  1293, 27,   37,   -193, 1293, 1606, -193, -193, -193, -193, 26,   -193, 164,  -193, 1584, 559,  -193,
    -193, 167,  -193, 171,  1149, 139,  1149, 607,  -193, 58,   -193, 127,  -193, -193, -193, -193, 26,   -193, 659,
    707,  -193, -193, -193, 161,  130,  1293, 756,  1293, 58,   -193, -193, -193, 502,  400,  -193, 1597, -193, -193,
    -193, 129,  -193, 1293, -193, 1293, 1197, 185,  1597, -193, 182,  17,   -193, 11,   32,   -193, -193, 18,   1385,
    1052, 186,  1293, -193, 164,  1597, 1245, 187,  113,  145,  1293, 21,   58,   1293, 1293, -193, -193, 188,  189,
    -193, -193, 164,  151,  195,  -193, 805,  854,  58,   36,   120,  -193, 146,  58,   196,  1597, 551,  204,  -193,
    -193, 502,  205,  -193, -193, 903,  133,  133,  133,  -193, 202,  502,  1597, -193, 1293, 168,  -193, 27,   1293,
    1293, 27,   27,   207,  1293, 1459, -193, 27,   33,   1245, 1496, -193, -193, -193, -193, 210,  1293, 1584, -193,
    -193, 951,  58,   213,  1597, -193, 1197, -193, 58,   -193, -193, 1100, -193, 58,   209,  1293, 174,  176,  218,
    58,   219,  -193, 1100, 1293, -193, 58,   1293, -193, -193, 58,   13,   -193, 1,    -193, -193, -193, 163,  -193,
    -193, -193, -193, 222,  27,   222,  222,  129,  -193, 226,  -193, 1293, -193, 227,  1597, 48,   102,  -193, 228,
    1245, -193, 23,   -193, 1422, 1584, 231,  1245, -193, -193, 58,   233,  -193, -193, -193, -193, 1597, 1293, 1293,
    198,  -193, -193, 1597, -193, 234,  1293, -193, 510,  -193, 1052, 133,  -193, -193, -193, -193, -193, 1293, 1533,
    -193, 27,   -193, 235,  27,   -193, 1245, 1570, -193, -193, 239,  58,   58,   1597, 1597, 1293, -193, 1597, -193,
    1003, 164,  -193, 243,  1245, -193, 225,  -193, 197,  245,  1245, -193, -193, -193, 1597, -193, 164,  250,  -193,
    248,  -193, -193, -193, 249,  254,  -193, -193, -193, -193
};

const unsigned char parser::yydefact_[] = {
    0,   21,  0,   0,   0,   0,   0,   14,  16,  17,  0,   19,  8,   46,  151, 160, 0,   0,   169, 194, 195, 196,
    198, 199, 200, 201, 204, 205, 190, 191, 192, 186, 81,  187, 188, 189, 212, 47,  0,   0,   126, 150, 75,  80,
    7,   166, 168, 167, 73,  74,  149, 148, 147, 143, 144, 145, 202, 208, 206, 209, 146, 0,   1,   2,   13,  0,
    0,   3,   15,  0,   0,   0,   160, 0,   94,  182, 152, 184, 0,   0,   0,   170, 179, 174, 172, 173, 175, 176,
    177, 171, 182, 54,  56,  0,   57,  161, 184, 178, 0,   151, 77,  151, 0,   52,  70,  213, 0,   197, 203, 207,
    84,  0,   127, 0,   0,   179, 174, 177, 0,   181, 0,   0,   0,   64,  193, 211, 210, 44,  160, 12,  4,   11,
    22,  20,  32,  154, 183, 96,  0,   185, 0,   58,  131, 133, 128, 118, 184, 100, 159, 122, 184, 109, 183, 0,
    0,   10,  182, 56,  185, 0,   0,   184, 0,   0,   50,  0,   0,   136, 137, 184, 0,   53,  49,  182, 79,  0,
    68,  0,   0,   50,  141, 0,   180, 85,  0,   0,   86,  175, 0,   40,  42,  45,  0,   34,  35,  0,   214, 214,
    214, 30,  182, 44,  95,  153, 0,   155, 164, 0,   0,   0,   185, 0,   0,   0,   0,   185, 0,   0,   160, 0,
    108, 55,  76,  158, 0,   0,   0,   162, 163, 0,   61,  0,   87,  69,  0,   72,  51,  134, 135, 185, 139, 50,
    0,   0,   78,  83,  0,   50,  0,   66,  0,   0,   142, 50,  0,   97,  98,  63,  50,  41,  126, 43,  23,  6,
    0,   215, 217, 216, 25,  28,  0,   27,  29,  183, 33,  0,   157, 0,   132, 0,   129, 104, 0,   119, 0,   160,
    101, 0,   123, 114, 94,  0,   160, 110, 9,   0,   0,   165, 138, 71,  48,  91,  0,   0,   82,  67,  140, 88,
    65,  0,   0,   62,  0,   5,   0,   214, 24,  31,  18,  156, 130, 0,   0,   120, 0,   102, 0,   0,   124, 160,
    0,   113, 111, 0,   60,  50,  90,  93,  0,   99,  89,  37,  0,   182, 26,  0,   160, 105, 0,   103, 0,   0,
    160, 115, 112, 59,  92,  39,  182, 0,   106, 0,   121, 125, 116, 0,   0,   36,  107, 117, 38
};

const short parser::yypgoto_[] = { -193, -193, 194,  -193, -193, 255,  -193, -193, -193, -193, -52,  -152, -11,  -193,
                                   -193, -193, 78,   75,   -193, 67,   -193, 66,   -170, -31,  -91,  -146, -193, -193,
                                   -2,   -193, -7,   -193, -193, -193, -5,   7,    -192, -193, -193, 30,   -193, -90,
                                   92,   -193, -193, -72,  115,  -193, -193, -193, -147, 22,   -106, -4,   -193, -87,
                                   -53,  -193, -193, -193, -193, -193, -33,  -30,  -193, -24,  -193, -193, -193 };

const short parser::yydefgoto_[] = { 0,   4,   129, 5,   6,   7,   8,   71,  11,  9,   268, 269, 199, 200,
                                     201, 188, 189, 190, 191, 192, 40,  41,  235, 236, 90,  91,  42,  43,
                                     74,  75,  167, 182, 146, 150, 112, 113, 142, 143, 94,  168, 169, 248,
                                     179, 253, 45,  77,  95,  96,  46,  47,  48,  49,  97,  119, 120, 137,
                                     140, 50,  51,  52,  53,  54,  55,  56,  57,  58,  59,  60,  270 };

const short parser::yytable_[] = {
    44,  104, 156, 153, 220, 107, 221, 76,  108, 249, 93,  98,  170, 92,  109, 278, 13,  13,  123, 67,  121, 194, 173,
    210, 175, 208, 10,  161, 233, 163, 215, 185, 69,  234, 80,  327, 61,  110, 62,  100, 213, 18,  18,  159, 250, 271,
    272, 145, 18,  1,   107, 211, 18,  108, 37,  37,  321, 111, 216, 109, 122, 310, 13,  130, 130, 209, 135, 286, 98,
    224, 92,  299, 293, 107, 3,   148, 141, 305, 316, 328, 214, 194, 149, 308, 251, 194, 242, 246, 311, 288, 70,  157,
    76,  212, 76,  194, 322, 217, 103, 99,  37,  144, 147, 13,  264, 101, 114, 102, 231, 124, 151, 157, 323, 274, 324,
    103, 240, 13,  183, 125, 186, 229, 126, 195, 98,  127, 92,  133, 254, 230, 132, 203, 205, 151, 202, 18,  19,  20,
    21,  296, 180, 37,  331, 19,  20,  21,  134, 223, 256, 193, 157, 136, 1,   257, 138, 37,  226, 139, 237, 238, 232,
    196, 176, 197, 198, 355, 2,   177, 221, 178, 171, 106, 313, 314, 152, 347, 18,  3,   106, 158, 160, 63,  64,  353,
    65,  265, 266, 162, 181, 171, 267, 66,  206, 157, 207, 222, 228, 276, 241, 243, 239, 279, 244, 255, 118, 258, 284,
    280, 260, 193, 262, 291, 273, 193, 300, 277, 290, 283, 148, 294, 297, 343, 302, 193, 303, 304, 103, 205, 306, 144,
    103, 318, 281, 282, 315, 362, 320, 325, 287, 289, 332, 301, 335, 339, 349, 103, 338, 309, 354, 307, 103, 358, 360,
    363, 364, 367, 359, 368, 369, 370, 131, 68,  317, 344, 334, 259, 261, 312, 275, 298, 319, 366, 252, 227, 0,   0,
    0,   0,   326, 0,   0,   0,   0,   290, 0,   333, 0,   0,   0,   0,   290, 0,   144, 0,   0,   0,   171, 0,   0,
    0,   336, 337, 171, 0,   0,   0,   0,   103, 340, 0,   157, 0,   157, 103, 345, 0,   0,   0,   0,   103, 0,   0,
    351, 171, 103, 0,   0,   290, 0,   0,   0,   0,   0,   0,   0,   0,   356, 0,   0,   361, 157, 0,   0,   0,   290,
    365, 348, 0,   0,   350, 290, 0,   12,  0,   0,   13,  0,   14,  0,   15,  0,   103, 0,   0,   16,  17,  18,  19,
    20,  21,  22,  23,  24,  25,  26,  27,  28,  29,  30,  31,  0,   32,  0,   0,   0,   0,   0,   33,  34,  35,  36,
    0,   0,   37,  0,   0,   0,   0,   0,   0,   171, 103, 38,  0,   13,  0,   14,  0,   72,  0,   39,  78,  0,   16,
    17,  18,  19,  20,  21,  22,  23,  24,  25,  26,  27,  28,  29,  30,  31,  0,   32,  0,   79,  80,  0,   0,   33,
    34,  35,  36,  0,   0,   37,  0,   0,   0,   0,   0,   0,   81,  82,  83,  84,  85,  86,  87,  88,  89,  13,  39,
    14,  0,   72,  0,   0,   78,  0,   16,  17,  18,  19,  20,  21,  22,  23,  24,  25,  26,  27,  28,  29,  30,  31,
    0,   32,  0,   79,  0,   0,   0,   33,  34,  35,  36,  0,   0,   37,  0,   0,   0,   0,   0,   0,   81,  82,  83,
    84,  85,  86,  87,  117, 89,  0,   39,  13,  341, 14,  18,  72,  0,   0,   78,  0,   16,  17,  18,  19,  20,  21,
    22,  23,  24,  25,  26,  27,  28,  29,  30,  31,  342, 32,  0,   79,  0,   0,   0,   33,  34,  35,  36,  0,   81,
    37,  116, 84,  85,  187, 87,  117, 89,  0,   38,  0,   13,  0,   14,  18,  72,  155, 39,  78,  0,   16,  17,  18,
    19,  20,  21,  22,  23,  24,  25,  26,  27,  28,  29,  30,  31,  0,   32,  0,   79,  0,   0,   0,   33,  34,  35,
    36,  0,   81,  37,  116, 84,  85,  86,  87,  117, 89,  0,   38,  13,  0,   14,  0,   72,  164, 0,   39,  0,   16,
    17,  18,  19,  20,  21,  22,  23,  24,  25,  26,  27,  28,  29,  30,  31,  0,   32,  0,   0,   0,   0,   0,   33,
    34,  35,  36,  0,   0,   37,  0,   0,   0,   0,   0,   0,   0,   165, 38,  0,   0,   166, 0,   13,  172, 14,  39,
    72,  0,   0,   78,  0,   16,  17,  18,  19,  20,  21,  22,  23,  24,  25,  26,  27,  28,  29,  30,  31,  0,   32,
    0,   79,  0,   0,   0,   33,  34,  35,  36,  0,   0,   37,  0,   0,   0,   0,   0,   0,   0,   0,   38,  13,  0,
    14,  174, 72,  0,   0,   39,  0,   16,  17,  18,  19,  20,  21,  22,  23,  24,  25,  26,  27,  28,  29,  30,  31,
    0,   32,  0,   0,   0,   0,   0,   33,  34,  35,  36,  0,   0,   37,  0,   0,   0,   0,   0,   0,   0,   165, 38,
    0,   13,  166, 14,  0,   72,  184, 39,  0,   0,   16,  17,  18,  19,  20,  21,  22,  23,  24,  25,  26,  27,  28,
    29,  30,  31,  0,   32,  0,   0,   0,   0,   0,   33,  34,  35,  36,  0,   0,   37,  0,   0,   0,   0,   0,   0,
    0,   165, 38,  0,   13,  166, 14,  245, 72,  0,   39,  0,   0,   16,  17,  18,  19,  20,  21,  22,  23,  24,  25,
    26,  27,  28,  29,  30,  31,  0,   32,  0,   0,   0,   0,   0,   33,  34,  35,  36,  0,   0,   37,  0,   0,   0,
    0,   0,   0,   0,   165, 38,  0,   13,  166, 14,  0,   72,  247, 39,  0,   0,   16,  17,  18,  19,  20,  21,  22,
    23,  24,  25,  26,  27,  28,  29,  30,  31,  0,   32,  0,   0,   0,   0,   0,   33,  34,  35,  36,  0,   0,   37,
    0,   0,   0,   0,   0,   0,   0,   165, 38,  0,   13,  166, 14,  0,   72,  263, 39,  78,  0,   16,  17,  18,  19,
    20,  21,  22,  23,  24,  25,  26,  27,  28,  29,  30,  31,  0,   32,  0,   79,  0,   0,   0,   33,  34,  35,  36,
    0,   0,   37,  0,   0,   0,   0,   0,   0,   0,   0,   38,  13,  0,   14,  0,   72,  295, 0,   39,  0,   16,  17,
    18,  19,  20,  21,  22,  23,  24,  25,  26,  27,  28,  29,  30,  31,  0,   32,  0,   0,   0,   0,   0,   33,  34,
    35,  36,  0,   0,   37,  0,   0,   0,   0,   0,   0,   0,   165, 38,  0,   0,   166, 0,   13,  357, 14,  39,  72,
    0,   0,   78,  0,   16,  17,  18,  19,  20,  21,  22,  23,  24,  25,  26,  27,  28,  29,  30,  31,  0,   32,  0,
    79,  0,   0,   0,   33,  34,  35,  36,  0,   0,   37,  0,   0,   0,   0,   0,   0,   0,   0,   38,  0,   13,  0,
    14,  0,   72,  0,   39,  78,  0,   16,  17,  18,  19,  20,  21,  22,  23,  24,  25,  26,  27,  28,  29,  30,  31,
    0,   32,  0,   79,  0,   0,   0,   33,  34,  35,  36,  0,   0,   37,  0,   0,   0,   0,   0,   0,   0,   0,   38,
    13,  0,   14,  0,   72,  0,   0,   39,  0,   16,  17,  18,  19,  20,  21,  22,  23,  24,  25,  26,  27,  28,  29,
    30,  31,  0,   32,  0,   0,   0,   0,   0,   33,  34,  35,  36,  0,   0,   37,  0,   0,   0,   0,   0,   0,   0,
    165, 38,  0,   13,  166, 14,  0,   72,  0,   39,  0,   0,   16,  17,  18,  19,  20,  21,  22,  23,  24,  25,  26,
    27,  28,  29,  30,  31,  0,   32,  0,   0,   0,   0,   0,   33,  34,  35,  36,  0,   0,   37,  0,   0,   0,   0,
    0,   0,   0,   73,  38,  13,  0,   14,  0,   72,  0,   0,   39,  0,   16,  17,  18,  19,  20,  21,  22,  23,  24,
    25,  26,  27,  28,  29,  30,  31,  0,   32,  0,   0,   0,   0,   0,   33,  34,  35,  36,  0,   0,   37,  0,   0,
    0,   0,   0,   0,   0,   204, 38,  13,  0,   14,  0,   72,  0,   0,   39,  0,   16,  17,  18,  19,  20,  21,  22,
    23,  24,  25,  26,  27,  28,  29,  30,  31,  0,   32,  0,   0,   0,   0,   0,   33,  34,  35,  36,  0,   0,   37,
    0,   0,   0,   0,   0,   0,   0,   225, 38,  13,  0,   14,  0,   72,  0,   0,   39,  0,   16,  17,  18,  19,  20,
    21,  22,  23,  24,  25,  26,  27,  28,  29,  30,  31,  0,   32,  0,   0,   0,   0,   0,   33,  34,  35,  36,  0,
    0,   37,  0,   0,   0,   0,   0,   0,   0,   0,   38,  13,  0,   14,  0,   128, 0,   0,   39,  0,   16,  17,  18,
    19,  20,  21,  22,  23,  24,  25,  26,  27,  28,  29,  30,  31,  0,   32,  0,   0,   0,   0,   0,   33,  34,  35,
    36,  0,   0,   37,  0,   0,   0,   0,   0,   13,  0,   14,  38,  218, 0,   0,   0,   0,   16,  0,   39,  19,  20,
    21,  22,  23,  24,  25,  26,  27,  28,  29,  30,  31,  0,   0,   0,   0,   0,   0,   0,   33,  34,  35,  36,  0,
    13,  37,  14,  0,   329, 0,   0,   219, 0,   16,  38,  0,   19,  20,  21,  22,  23,  24,  25,  26,  27,  28,  29,
    30,  31,  0,   0,   0,   0,   0,   0,   0,   33,  34,  35,  36,  0,   13,  37,  14,  0,   285, 0,   0,   330, 0,
    16,  38,  0,   19,  20,  21,  22,  23,  24,  25,  26,  27,  28,  29,  30,  31,  0,   0,   0,   0,   0,   0,   0,
    33,  34,  35,  36,  0,   13,  37,  14,  0,   292, 0,   0,   0,   0,   16,  38,  0,   19,  20,  21,  22,  23,  24,
    25,  26,  27,  28,  29,  30,  31,  0,   0,   0,   0,   0,   0,   0,   33,  34,  35,  36,  0,   13,  37,  14,  0,
    346, 0,   0,   0,   0,   16,  38,  0,   19,  20,  21,  22,  23,  24,  25,  26,  27,  28,  29,  30,  31,  0,   0,
    0,   0,   0,   0,   0,   33,  34,  35,  36,  0,   13,  37,  14,  0,   352, 0,   0,   0,   0,   16,  38,  0,   19,
    20,  21,  22,  23,  24,  25,  26,  27,  28,  29,  30,  31,  0,   0,   0,   0,   0,   0,   0,   33,  34,  35,  36,
    0,   0,   37,  0,   0,   0,   0,   0,   0,   0,   0,   38,  19,  20,  21,  22,  23,  24,  25,  26,  27,  154, 0,
    81,  115, 116, 84,  85,  86,  87,  117, 89,  118, 0,   0,   105, 81,  115, 116, 84,  85,  86,  87,  117, 89,  118,
    0,   106
};

const short parser::yycheck_[] = {
    2,   32,  93,  90,  151, 38,  152, 14,  38,  179, 15,  15,  102, 15,  38,  207, 4,   4,   49,  0,   8,   127, 113,
    12,  114, 8,   6,   99,  7,   101, 12,  121, 10,  12,  33,  12,  30,  39,  0,   17,  8,   15,  15,  96,  8,   197,
    198, 10,  15,  30,  83,  40,  15,  83,  42,  42,  8,   56,  40,  83,  48,  48,  4,   65,  66,  48,  73,  214, 72,
    156, 72,  241, 219, 106, 55,  82,  78,  247, 270, 56,  48,  187, 56,  253, 48,  191, 173, 177, 258, 56,  47,  93,
    99,  146, 101, 201, 48,  150, 32,  6,   42,  79,  80,  4,   195, 6,   6,   8,   161, 27,  88,  113, 10,  200, 12,
    49,  169, 4,   120, 39,  122, 8,   39,  128, 128, 4,   128, 30,  8,   160, 7,   138, 139, 111, 136, 15,  16,  17,
    18,  229, 118, 42,  289, 16,  17,  18,  4,   154, 181, 127, 152, 10,  30,  184, 47,  42,  158, 12,  165, 166, 162,
    32,  1,   34,  35,  335, 44,  6,   314, 8,   104, 51,  9,   10,  10,  322, 15,  55,  51,  12,  9,   0,   1,   330,
    3,   52,  53,  48,  58,  123, 57,  10,  7,   195, 12,  9,   9,   204, 9,   48,  12,  208, 7,   181, 58,  9,   213,
    209, 4,   187, 5,   218, 10,  191, 5,   47,  218, 10,  225, 9,   7,   312, 48,  201, 48,  7,   160, 234, 9,   207,
    164, 5,   210, 211, 12,  10,  9,   9,   216, 217, 9,   243, 9,   9,   9,   179, 48,  254, 9,   251, 184, 342, 9,
    56,  9,   5,   343, 9,   9,   5,   66,  6,   273, 315, 295, 187, 191, 260, 201, 239, 277, 358, 180, 158, -1,  -1,
    -1,  -1,  285, -1,  -1,  -1,  -1,  285, -1,  292, -1,  -1,  -1,  -1,  292, -1,  270, -1,  -1,  -1,  230, -1,  -1,
    -1,  302, 303, 236, -1,  -1,  -1,  -1,  241, 310, -1,  312, -1,  314, 247, 321, -1,  -1,  -1,  -1,  253, -1,  -1,
    329, 257, 258, -1,  -1,  329, -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  338, -1,  -1,  346, 342, -1,  -1,  -1,  346,
    352, 324, -1,  -1,  327, 352, -1,  1,   -1,  -1,  4,   -1,  6,   -1,  8,   -1,  295, -1,  -1,  13,  14,  15,  16,
    17,  18,  19,  20,  21,  22,  23,  24,  25,  26,  27,  28,  -1,  30,  -1,  -1,  -1,  -1,  -1,  36,  37,  38,  39,
    -1,  -1,  42,  -1,  -1,  -1,  -1,  -1,  -1,  334, 335, 51,  -1,  4,   -1,  6,   -1,  8,   -1,  59,  11,  -1,  13,
    14,  15,  16,  17,  18,  19,  20,  21,  22,  23,  24,  25,  26,  27,  28,  -1,  30,  -1,  32,  33,  -1,  -1,  36,
    37,  38,  39,  -1,  -1,  42,  -1,  -1,  -1,  -1,  -1,  -1,  49,  50,  51,  52,  53,  54,  55,  56,  57,  4,   59,
    6,   -1,  8,   -1,  -1,  11,  -1,  13,  14,  15,  16,  17,  18,  19,  20,  21,  22,  23,  24,  25,  26,  27,  28,
    -1,  30,  -1,  32,  -1,  -1,  -1,  36,  37,  38,  39,  -1,  -1,  42,  -1,  -1,  -1,  -1,  -1,  -1,  49,  50,  51,
    52,  53,  54,  55,  56,  57,  -1,  59,  4,   5,   6,   15,  8,   -1,  -1,  11,  -1,  13,  14,  15,  16,  17,  18,
    19,  20,  21,  22,  23,  24,  25,  26,  27,  28,  29,  30,  -1,  32,  -1,  -1,  -1,  36,  37,  38,  39,  -1,  49,
    42,  51,  52,  53,  54,  55,  56,  57,  -1,  51,  -1,  4,   -1,  6,   15,  8,   9,   59,  11,  -1,  13,  14,  15,
    16,  17,  18,  19,  20,  21,  22,  23,  24,  25,  26,  27,  28,  -1,  30,  -1,  32,  -1,  -1,  -1,  36,  37,  38,
    39,  -1,  49,  42,  51,  52,  53,  54,  55,  56,  57,  -1,  51,  4,   -1,  6,   -1,  8,   9,   -1,  59,  -1,  13,
    14,  15,  16,  17,  18,  19,  20,  21,  22,  23,  24,  25,  26,  27,  28,  -1,  30,  -1,  -1,  -1,  -1,  -1,  36,
    37,  38,  39,  -1,  -1,  42,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  50,  51,  -1,  -1,  54,  -1,  4,   5,   6,   59,
    8,   -1,  -1,  11,  -1,  13,  14,  15,  16,  17,  18,  19,  20,  21,  22,  23,  24,  25,  26,  27,  28,  -1,  30,
    -1,  32,  -1,  -1,  -1,  36,  37,  38,  39,  -1,  -1,  42,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  51,  4,   -1,
    6,   7,   8,   -1,  -1,  59,  -1,  13,  14,  15,  16,  17,  18,  19,  20,  21,  22,  23,  24,  25,  26,  27,  28,
    -1,  30,  -1,  -1,  -1,  -1,  -1,  36,  37,  38,  39,  -1,  -1,  42,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  50,  51,
    -1,  4,   54,  6,   -1,  8,   9,   59,  -1,  -1,  13,  14,  15,  16,  17,  18,  19,  20,  21,  22,  23,  24,  25,
    26,  27,  28,  -1,  30,  -1,  -1,  -1,  -1,  -1,  36,  37,  38,  39,  -1,  -1,  42,  -1,  -1,  -1,  -1,  -1,  -1,
    -1,  50,  51,  -1,  4,   54,  6,   7,   8,   -1,  59,  -1,  -1,  13,  14,  15,  16,  17,  18,  19,  20,  21,  22,
    23,  24,  25,  26,  27,  28,  -1,  30,  -1,  -1,  -1,  -1,  -1,  36,  37,  38,  39,  -1,  -1,  42,  -1,  -1,  -1,
    -1,  -1,  -1,  -1,  50,  51,  -1,  4,   54,  6,   -1,  8,   9,   59,  -1,  -1,  13,  14,  15,  16,  17,  18,  19,
    20,  21,  22,  23,  24,  25,  26,  27,  28,  -1,  30,  -1,  -1,  -1,  -1,  -1,  36,  37,  38,  39,  -1,  -1,  42,
    -1,  -1,  -1,  -1,  -1,  -1,  -1,  50,  51,  -1,  4,   54,  6,   -1,  8,   9,   59,  11,  -1,  13,  14,  15,  16,
    17,  18,  19,  20,  21,  22,  23,  24,  25,  26,  27,  28,  -1,  30,  -1,  32,  -1,  -1,  -1,  36,  37,  38,  39,
    -1,  -1,  42,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  51,  4,   -1,  6,   -1,  8,   9,   -1,  59,  -1,  13,  14,
    15,  16,  17,  18,  19,  20,  21,  22,  23,  24,  25,  26,  27,  28,  -1,  30,  -1,  -1,  -1,  -1,  -1,  36,  37,
    38,  39,  -1,  -1,  42,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  50,  51,  -1,  -1,  54,  -1,  4,   5,   6,   59,  8,
    -1,  -1,  11,  -1,  13,  14,  15,  16,  17,  18,  19,  20,  21,  22,  23,  24,  25,  26,  27,  28,  -1,  30,  -1,
    32,  -1,  -1,  -1,  36,  37,  38,  39,  -1,  -1,  42,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  51,  -1,  4,   -1,
    6,   -1,  8,   -1,  59,  11,  -1,  13,  14,  15,  16,  17,  18,  19,  20,  21,  22,  23,  24,  25,  26,  27,  28,
    -1,  30,  -1,  32,  -1,  -1,  -1,  36,  37,  38,  39,  -1,  -1,  42,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  51,
    4,   -1,  6,   -1,  8,   -1,  -1,  59,  -1,  13,  14,  15,  16,  17,  18,  19,  20,  21,  22,  23,  24,  25,  26,
    27,  28,  -1,  30,  -1,  -1,  -1,  -1,  -1,  36,  37,  38,  39,  -1,  -1,  42,  -1,  -1,  -1,  -1,  -1,  -1,  -1,
    50,  51,  -1,  4,   54,  6,   -1,  8,   -1,  59,  -1,  -1,  13,  14,  15,  16,  17,  18,  19,  20,  21,  22,  23,
    24,  25,  26,  27,  28,  -1,  30,  -1,  -1,  -1,  -1,  -1,  36,  37,  38,  39,  -1,  -1,  42,  -1,  -1,  -1,  -1,
    -1,  -1,  -1,  50,  51,  4,   -1,  6,   -1,  8,   -1,  -1,  59,  -1,  13,  14,  15,  16,  17,  18,  19,  20,  21,
    22,  23,  24,  25,  26,  27,  28,  -1,  30,  -1,  -1,  -1,  -1,  -1,  36,  37,  38,  39,  -1,  -1,  42,  -1,  -1,
    -1,  -1,  -1,  -1,  -1,  50,  51,  4,   -1,  6,   -1,  8,   -1,  -1,  59,  -1,  13,  14,  15,  16,  17,  18,  19,
    20,  21,  22,  23,  24,  25,  26,  27,  28,  -1,  30,  -1,  -1,  -1,  -1,  -1,  36,  37,  38,  39,  -1,  -1,  42,
    -1,  -1,  -1,  -1,  -1,  -1,  -1,  50,  51,  4,   -1,  6,   -1,  8,   -1,  -1,  59,  -1,  13,  14,  15,  16,  17,
    18,  19,  20,  21,  22,  23,  24,  25,  26,  27,  28,  -1,  30,  -1,  -1,  -1,  -1,  -1,  36,  37,  38,  39,  -1,
    -1,  42,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  51,  4,   -1,  6,   -1,  8,   -1,  -1,  59,  -1,  13,  14,  15,
    16,  17,  18,  19,  20,  21,  22,  23,  24,  25,  26,  27,  28,  -1,  30,  -1,  -1,  -1,  -1,  -1,  36,  37,  38,
    39,  -1,  -1,  42,  -1,  -1,  -1,  -1,  -1,  4,   -1,  6,   51,  8,   -1,  -1,  -1,  -1,  13,  -1,  59,  16,  17,
    18,  19,  20,  21,  22,  23,  24,  25,  26,  27,  28,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  36,  37,  38,  39,  -1,
    4,   42,  6,   -1,  8,   -1,  -1,  48,  -1,  13,  51,  -1,  16,  17,  18,  19,  20,  21,  22,  23,  24,  25,  26,
    27,  28,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  36,  37,  38,  39,  -1,  4,   42,  6,   -1,  8,   -1,  -1,  48,  -1,
    13,  51,  -1,  16,  17,  18,  19,  20,  21,  22,  23,  24,  25,  26,  27,  28,  -1,  -1,  -1,  -1,  -1,  -1,  -1,
    36,  37,  38,  39,  -1,  4,   42,  6,   -1,  8,   -1,  -1,  -1,  -1,  13,  51,  -1,  16,  17,  18,  19,  20,  21,
    22,  23,  24,  25,  26,  27,  28,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  36,  37,  38,  39,  -1,  4,   42,  6,   -1,
    8,   -1,  -1,  -1,  -1,  13,  51,  -1,  16,  17,  18,  19,  20,  21,  22,  23,  24,  25,  26,  27,  28,  -1,  -1,
    -1,  -1,  -1,  -1,  -1,  36,  37,  38,  39,  -1,  4,   42,  6,   -1,  8,   -1,  -1,  -1,  -1,  13,  51,  -1,  16,
    17,  18,  19,  20,  21,  22,  23,  24,  25,  26,  27,  28,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  36,  37,  38,  39,
    -1,  -1,  42,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  51,  16,  17,  18,  19,  20,  21,  22,  23,  24,  47,  -1,
    49,  50,  51,  52,  53,  54,  55,  56,  57,  58,  -1,  -1,  39,  49,  50,  51,  52,  53,  54,  55,  56,  57,  58,
    -1,  51
};

const unsigned char parser::yystos_[] = {
    0,   30,  44,  55,  62,  64,  65,  66,  67,  70,  6,   69,  1,   4,   6,   8,   13,  14,  15,  16,  17,  18,
    19,  20,  21,  22,  23,  24,  25,  26,  27,  28,  30,  36,  37,  38,  39,  42,  51,  59,  81,  82,  87,  88,
    89,  105, 109, 110, 111, 112, 118, 119, 120, 121, 122, 123, 124, 125, 126, 127, 128, 30,  0,   0,   1,   3,
    10,  0,   66,  112, 47,  68,  8,   50,  89,  90,  91,  106, 11,  32,  33,  49,  50,  51,  52,  53,  54,  55,
    56,  57,  85,  86,  89,  95,  99,  107, 108, 113, 114, 6,   112, 6,   8,   82,  84,  39,  51,  123, 124, 126,
    89,  56,  95,  96,  6,   50,  51,  56,  58,  114, 115, 8,   48,  84,  27,  39,  39,  4,   8,   63,  89,  63,
    7,   30,  4,   91,  10,  116, 47,  12,  117, 89,  97,  98,  112, 10,  93,  112, 91,  56,  94,  112, 10,  116,
    47,  9,   85,  89,  12,  117, 9,   106, 48,  106, 9,   50,  54,  91,  100, 101, 102, 82,  5,   85,  7,   102,
    1,   6,   8,   103, 112, 58,  92,  89,  9,   102, 89,  54,  76,  77,  78,  79,  80,  112, 113, 95,  32,  34,
    35,  73,  74,  75,  89,  91,  50,  91,  7,   12,  8,   48,  12,  40,  117, 8,   48,  12,  40,  117, 8,   48,
    111, 86,  9,   91,  116, 50,  89,  107, 9,   8,   84,  117, 89,  7,   12,  83,  84,  91,  91,  12,  117, 9,
    116, 48,  7,   7,   102, 9,   102, 83,  8,   48,  103, 104, 8,   112, 123, 84,  9,   77,  4,   78,  5,   9,
    85,  52,  53,  57,  71,  72,  129, 72,  72,  10,  116, 80,  91,  47,  97,  91,  89,  112, 112, 10,  91,  8,
    111, 112, 56,  112, 89,  91,  8,   111, 9,   9,   102, 7,   100, 83,  5,   89,  48,  48,  7,   83,  9,   89,
    83,  91,  48,  83,  96,  9,   10,  12,  97,  73,  5,   91,  9,   8,   48,  10,  12,  9,   91,  12,  56,  8,
    48,  111, 9,   91,  84,  9,   89,  89,  48,  9,   89,  5,   29,  85,  71,  91,  8,   111, 112, 9,   112, 91,
    8,   111, 9,   83,  89,  5,   85,  116, 9,   91,  10,  56,  9,   91,  116, 5,   9,   9,   5
};

const unsigned char parser::yyr1_[] = {
    0,   61,  62,  62,  63,  63,  63,  64,  64,  64,  64,  64,  64,  64,  65,  65,  66,  66,  67,  68,  68,  69,
    69,  70,  71,  72,  72,  73,  73,  73,  74,  74,  75,  75,  76,  76,  77,  77,  77,  77,  78,  78,  79,  79,
    80,  80,  81,  81,  82,  82,  83,  83,  84,  84,  85,  85,  86,  86,  86,  87,  87,  87,  87,  87,  87,  87,
    87,  87,  87,  87,  87,  87,  87,  88,  88,  88,  88,  88,  88,  88,  89,  89,  89,  89,  89,  89,  89,  89,
    89,  89,  89,  89,  89,  89,  90,  90,  91,  92,  92,  92,  93,  93,  93,  93,  93,  93,  93,  93,  94,  94,
    94,  94,  94,  94,  94,  94,  94,  94,  95,  95,  95,  95,  95,  95,  95,  95,  96,  96,  97,  97,  97,  98,
    98,  99,  100, 100, 100, 101, 101, 102, 103, 104, 104, 105, 105, 105, 105, 105, 105, 105, 105, 106, 106, 106,
    106, 106, 106, 106, 107, 107, 108, 108, 108, 109, 110, 110, 111, 111, 111, 112, 113, 113, 113, 113, 113, 113,
    113, 113, 114, 114, 115, 115, 116, 116, 117, 117, 118, 119, 120, 120, 121, 121, 122, 122, 123, 123, 123, 123,
    124, 124, 124, 124, 125, 125, 126, 126, 127, 127, 128, 128, 128, 128, 128, 128, 129, 129, 129, 129
};

const signed char parser::yyr2_[] = {
    0, 2, 2, 2, 1, 4, 3, 2, 2, 6, 4, 3, 3, 2, 1, 2, 1, 1, 7, 0, 2, 0, 3, 5, 2, 1, 3, 2, 2, 2, 1, 3, 0, 2, 1, 1, 6,
    4, 7, 5, 1, 2, 1, 2, 0, 1, 1, 1, 5, 3, 0, 1, 1, 2, 1, 3, 1, 1, 2, 7, 6, 4, 5, 4, 2, 5, 4, 5, 3, 4, 2, 5, 4, 1,
    1, 1, 4, 2, 4, 3, 1, 1, 5, 4, 2, 3, 3, 4, 5, 6, 6, 5, 7, 6, 1, 3, 2, 2, 2, 4, 1, 3, 4, 5, 3, 5, 6, 7, 2, 1, 3,
    4, 5, 4, 3, 5, 6, 7, 2, 4, 5, 7, 2, 4, 5, 7, 0, 1, 1, 3, 4, 1, 3, 2, 2, 2, 1, 1, 3, 2, 3, 0, 1, 1, 1, 1, 1, 1,
    1, 1, 1, 0, 1, 3, 2, 3, 5, 4, 3, 2, 0, 1, 3, 4, 4, 5, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 1, 0, 1, 0,
    1, 1, 1, 1, 1, 1, 1, 1, 2, 1, 1, 1, 2, 1, 1, 1, 1, 1, 2, 1, 1, 1, 2, 1, 1, 2, 2, 1, 2, 0, 1, 1, 1
};


#if YYDEBUG
const short parser::yyrline_[] = {
    0,   175, 175, 176, 181, 183, 194, 208, 212, 218, 230, 244, 247, 250, 276, 278, 283, 284, 288, 293, 294, 298,
    299, 303, 308, 313, 315, 320, 322, 324, 329, 331, 336, 337, 341, 342, 346, 348, 350, 352, 357, 358, 363, 364,
    368, 369, 372, 372, 375, 377, 382, 383, 387, 388, 392, 393, 397, 398, 399, 403, 412, 415, 419, 428, 431, 434,
    445, 455, 463, 470, 473, 479, 490, 500, 502, 504, 505, 510, 515, 522, 532, 536, 538, 543, 549, 551, 557, 560,
    563, 566, 574, 576, 579, 582, 587, 588, 601, 604, 605, 606, 611, 613, 615, 617, 619, 621, 623, 625, 630, 632,
    634, 636, 638, 640, 642, 644, 646, 648, 653, 654, 655, 657, 659, 661, 663, 665, 671, 672, 676, 678, 680, 689,
    691, 695, 699, 700, 701, 705, 706, 709, 712, 715, 716, 720, 721, 722, 723, 724, 725, 726, 727, 731, 733, 735,
    737, 739, 741, 743, 751, 753, 758, 760, 762, 767, 772, 774, 783, 784, 785, 788, 797, 798, 799, 800, 801, 802,
    803, 804, 808, 809, 813, 815, 819, 819, 820, 820, 822, 824, 827, 828, 832, 833, 837, 838, 842, 843, 844, 845,
    854, 855, 856, 857, 861, 863, 868, 870, 875, 877, 882, 883, 884, 885, 886, 887, 891, 892, 893, 894
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
#line 4823 "langutils/sc_parser/src/sc_grammar_parser.cpp"

#line 897 "langutils/sc_parser/src/sc_grammar.y"
