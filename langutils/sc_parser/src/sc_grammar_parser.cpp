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


#include "sc_lexer/lexer.hpp"
#include "sc_parser/indexes_typed.hpp"
#include "sc_parser/nodes.hpp"

#include "sc_grammar_parser.hpp"
#include "sc_grammar_impl.hpp"
#include "parser_context.hpp"

namespace sc::ast::parser{ class parser; } // forward declare the parser

using namespace sc::ast;

template<typename... REJECTS>
auto create_error(sc::ast::parser::ParserContext& cxt, sc::lex::SourceCodeRange loc, REJECTS...rejects) {
	const auto orphans = cxt.graph.orphans();
	auto er = cxt.create(Error{}, loc);
	for(auto o : orphans){
		if (!((*o == *rejects) || ...))
			cxt.graph.append(er, sc::ast::AnyIndex{*o});
	}
	return er;
}


#line 66 "langutils/sc_parser/src/sc_grammar_parser.cpp"


#include "sc_grammar_parser.hpp"




#ifndef YY_
# if defined YYENABLE_NLS && YYENABLE_NLS
#  if ENABLE_NLS
#   include <libintl.h> // FIXME: INFRINGES ON USER NAME SPACE.
#   define YY_(msgid) dgettext ("bison-runtime", msgid)
#  endif
# endif
# ifndef YY_
#  define YY_(msgid) msgid
# endif
#endif


// Whether we are compiled with exception support.
#ifndef YY_EXCEPTIONS
# if defined __GNUC__ && !defined __EXCEPTIONS
#  define YY_EXCEPTIONS 0
# else
#  define YY_EXCEPTIONS 1
# endif
#endif

#define YYRHSLOC(Rhs, K) ((Rhs)[K].location)
/* YYLLOC_DEFAULT -- Set CURRENT to span from RHS[1] to RHS[N].
   If N is 0, then set CURRENT to the empty location which ends
   the previous symbol: RHS[0] (always defined).  */

# ifndef YYLLOC_DEFAULT
#  define YYLLOC_DEFAULT(Current, Rhs, N)                               \
    do                                                                  \
      if (N)                                                            \
        {                                                               \
          (Current).begin  = YYRHSLOC (Rhs, 1).begin;                   \
          (Current).end    = YYRHSLOC (Rhs, N).end;                     \
        }                                                               \
      else                                                              \
        {                                                               \
          (Current).begin = (Current).end = YYRHSLOC (Rhs, 0).end;      \
        }                                                               \
    while (false)
# endif


// Enable debugging if requested.
#if YYDEBUG

// A pseudo ostream that takes yydebug_ into account.
# define YYCDEBUG if (yydebug_) (*yycdebug_)

# define YY_SYMBOL_PRINT(Title, Symbol)         \
  do {                                          \
    if (yydebug_)                               \
    {                                           \
      *yycdebug_ << Title << ' ';               \
      yy_print_ (*yycdebug_, Symbol);           \
      *yycdebug_ << '\n';                       \
    }                                           \
  } while (false)

# define YY_REDUCE_PRINT(Rule)          \
  do {                                  \
    if (yydebug_)                       \
      yy_reduce_print_ (Rule);          \
  } while (false)

# define YY_STACK_PRINT()               \
  do {                                  \
    if (yydebug_)                       \
      yy_stack_print_ ();                \
  } while (false)

#else // !YYDEBUG

# define YYCDEBUG if (false) std::cerr
# define YY_SYMBOL_PRINT(Title, Symbol)  YY_USE (Symbol)
# define YY_REDUCE_PRINT(Rule)           static_cast<void> (0)
# define YY_STACK_PRINT()                static_cast<void> (0)

#endif // !YYDEBUG

#define yyerrok         (yyerrstatus_ = 0)
#define yyclearin       (yyla.clear ())

#define YYACCEPT        goto yyacceptlab
#define YYABORT         goto yyabortlab
#define YYERROR         goto yyerrorlab
#define YYRECOVERING()  (!!yyerrstatus_)

#line 7 "langutils/sc_parser/src/sc_grammar.y"
namespace sc { namespace ast { namespace parser {
#line 166 "langutils/sc_parser/src/sc_grammar_parser.cpp"

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

  parser::~parser ()
  {}

  parser::syntax_error::~syntax_error () YY_NOEXCEPT YY_NOTHROW
  {}

  /*---------.
  | symbol.  |
  `---------*/

  // basic_symbol.
  template <typename Base>
  parser::basic_symbol<Base>::basic_symbol (const basic_symbol& that)
    : Base (that)
    , value ()
    , location (that.location)
  {
    switch (this->kind ())
    {
      case symbol_kind::S_ascii: // ascii
        value.copy< ASCIIIndex > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_126_accidental_unsigned: // accidental.unsigned
      case symbol_kind::S_accidental: // accidental
        value.copy< AccidentalIndex > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_adverb: // adverb
        value.copy< AdverbIndex > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_msgsend: // msgsend
      case symbol_kind::S_88_expr_base: // expr.base
      case symbol_kind::S_expr: // expr
      case symbol_kind::S_90_expr_seq_base: // expr.seq.base
      case symbol_kind::S_91_expr_seq: // expr.seq
          value.copy<AnyExprIndex>(YY_MOVE(that.value));
          break;

      case symbol_kind::S_method: // method
        value.copy< AnyMethodIndex > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_105_literal_terminal: // literal.terminal
      case symbol_kind::S_literal: // literal
          value.copy<AnyPossiblyLiteralIndex>(YY_MOVE(that.value));
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
        value.copy< BlockIndex > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_86_block_contents_item: // block.contents.item
          value.copy<BlockItemIndex>(YY_MOVE(that.value));
          break;

      case symbol_kind::S_83_block_opt_list: // block.opt_list
      case symbol_kind::S_84_block_list: // block.list
        value.copy< BlockListIndex > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_boolean: // boolean
        value.copy< BooleanLitIndex > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_70_class_extension: // class.extension
        value.copy< ClassExtensionIndex > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_class: // class
        value.copy< ClassIndex > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_go: // go
        value.copy< ClassListOrExprListIndex > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_66_classOrExtList_item: // classOrExtList.item
        value.copy< ClassOrExtensionIndex > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_65_classOrExtList_list: // classOrExtList.list
        value.copy< ClassOrExtensionListIndex > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_73_class_vars_entry: // class.vars.entry
        value.copy< DeclareAnyList > (YY_MOVE (that.value));
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
        value.copy< DeclareClassAnyVarListIndex > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_71_class_vars_entry_item: // class.vars.entry.item
        value.copy< DeclareClassVarIndex > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_72_class_vars_entry_list: // class.vars.entry.list
        value.copy< DeclareMemberListIndex > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_98_variable_declarations_list: // variable_declarations.list
      case symbol_kind::S_variable_declarations: // variable_declarations
        value.copy< DeclareVariableListIndex > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_107_literal_dictionary_entry: // literal.dictionary.entry
          value.copy<DictionaryEntryIndex>(YY_MOVE(that.value));
          break;

      case symbol_kind::S_108_literal_dictionary_entries: // literal.dictionary.entries
      case symbol_kind::S_109_literal_dictionary: // literal.dictionary
          value.copy<DictionaryIndex>(YY_MOVE(that.value));
          break;

      case symbol_kind::S_124_float_raw_unsigned: // float.raw_unsigned
      case symbol_kind::S_125_float_raw: // float.raw
          value.copy<FloatLitIndex>(YY_MOVE(that.value));
          break;

      case symbol_kind::S_float: // float
        value.copy< FloatProducingIndex > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_integer: // integer
        value.copy< IntLitIndex > (YY_MOVE (that.value));
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
        value.copy< LexerToken > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_77_method_base: // method.base
        value.copy< MethodIndex > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_79_method_list: // method.list
      case symbol_kind::S_80_method_list_opt: // method.list.opt
        value.copy< MethodListIndex > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_name: // name
        value.copy< NamedIdentifierIndex > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_nil: // nil
        value.copy< NilLitIndex > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_accessor: // accessor
        value.copy< ReadWriteAccessor > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_region: // region
        value.copy< RegionListIndex > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_76_method_name: // method.name
      case symbol_kind::S_113_binary_op_raw: // binary_op.raw
      case symbol_kind::S_114_binary_op_no_adverb: // binary_op.no_adverb
          value.copy<SelectorIndex>(YY_MOVE(that.value));
          break;

      case symbol_kind::S_binary_op: // binary_op
        value.copy< SelectorMaybeAdverbIndex > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_string: // string
        value.copy< StringLitIndex > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_symbol: // symbol
        value.copy< SymbolLitIndex > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_63_region_item: // region.item
        value.copy< error_index<AnyExprIndex> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_68_class_super_opt: // class.super.opt
        value.copy< maybe<ClassNameIdentifierIndex> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_69_class_slot_opt: // class.slot.opt
        value.copy< maybe<NamedIdentifierIndex> > (YY_MOVE (that.value));
        break;

      default:
        break;
    }

  }




  template <typename Base>
  parser::symbol_kind_type
  parser::basic_symbol<Base>::type_get () const YY_NOEXCEPT
  {
    return this->kind ();
  }


  template <typename Base>
  bool
  parser::basic_symbol<Base>::empty () const YY_NOEXCEPT
  {
    return this->kind () == symbol_kind::S_YYEMPTY;
  }

  template <typename Base>
  void
  parser::basic_symbol<Base>::move (basic_symbol& s)
  {
    super_type::move (s);
    switch (this->kind ())
    {
      case symbol_kind::S_ascii: // ascii
        value.move< ASCIIIndex > (YY_MOVE (s.value));
        break;

      case symbol_kind::S_126_accidental_unsigned: // accidental.unsigned
      case symbol_kind::S_accidental: // accidental
        value.move< AccidentalIndex > (YY_MOVE (s.value));
        break;

      case symbol_kind::S_adverb: // adverb
        value.move< AdverbIndex > (YY_MOVE (s.value));
        break;

      case symbol_kind::S_msgsend: // msgsend
      case symbol_kind::S_88_expr_base: // expr.base
      case symbol_kind::S_expr: // expr
      case symbol_kind::S_90_expr_seq_base: // expr.seq.base
      case symbol_kind::S_91_expr_seq: // expr.seq
          value.move<AnyExprIndex>(YY_MOVE(s.value));
          break;

      case symbol_kind::S_method: // method
        value.move< AnyMethodIndex > (YY_MOVE (s.value));
        break;

      case symbol_kind::S_105_literal_terminal: // literal.terminal
      case symbol_kind::S_literal: // literal
          value.move<AnyPossiblyLiteralIndex>(YY_MOVE(s.value));
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
        value.move< BlockIndex > (YY_MOVE (s.value));
        break;

      case symbol_kind::S_86_block_contents_item: // block.contents.item
          value.move<BlockItemIndex>(YY_MOVE(s.value));
          break;

      case symbol_kind::S_83_block_opt_list: // block.opt_list
      case symbol_kind::S_84_block_list: // block.list
        value.move< BlockListIndex > (YY_MOVE (s.value));
        break;

      case symbol_kind::S_boolean: // boolean
        value.move< BooleanLitIndex > (YY_MOVE (s.value));
        break;

      case symbol_kind::S_70_class_extension: // class.extension
        value.move< ClassExtensionIndex > (YY_MOVE (s.value));
        break;

      case symbol_kind::S_class: // class
        value.move< ClassIndex > (YY_MOVE (s.value));
        break;

      case symbol_kind::S_go: // go
        value.move< ClassListOrExprListIndex > (YY_MOVE (s.value));
        break;

      case symbol_kind::S_66_classOrExtList_item: // classOrExtList.item
        value.move< ClassOrExtensionIndex > (YY_MOVE (s.value));
        break;

      case symbol_kind::S_65_classOrExtList_list: // classOrExtList.list
        value.move< ClassOrExtensionListIndex > (YY_MOVE (s.value));
        break;

      case symbol_kind::S_73_class_vars_entry: // class.vars.entry
        value.move< DeclareAnyList > (YY_MOVE (s.value));
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
        value.move< DeclareClassAnyVarListIndex > (YY_MOVE (s.value));
        break;

      case symbol_kind::S_71_class_vars_entry_item: // class.vars.entry.item
        value.move< DeclareClassVarIndex > (YY_MOVE (s.value));
        break;

      case symbol_kind::S_72_class_vars_entry_list: // class.vars.entry.list
        value.move< DeclareMemberListIndex > (YY_MOVE (s.value));
        break;

      case symbol_kind::S_98_variable_declarations_list: // variable_declarations.list
      case symbol_kind::S_variable_declarations: // variable_declarations
        value.move< DeclareVariableListIndex > (YY_MOVE (s.value));
        break;

      case symbol_kind::S_107_literal_dictionary_entry: // literal.dictionary.entry
          value.move<DictionaryEntryIndex>(YY_MOVE(s.value));
          break;

      case symbol_kind::S_108_literal_dictionary_entries: // literal.dictionary.entries
      case symbol_kind::S_109_literal_dictionary: // literal.dictionary
          value.move<DictionaryIndex>(YY_MOVE(s.value));
          break;

      case symbol_kind::S_124_float_raw_unsigned: // float.raw_unsigned
      case symbol_kind::S_125_float_raw: // float.raw
          value.move<FloatLitIndex>(YY_MOVE(s.value));
          break;

      case symbol_kind::S_float: // float
        value.move< FloatProducingIndex > (YY_MOVE (s.value));
        break;

      case symbol_kind::S_integer: // integer
        value.move< IntLitIndex > (YY_MOVE (s.value));
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
        value.move< LexerToken > (YY_MOVE (s.value));
        break;

      case symbol_kind::S_77_method_base: // method.base
        value.move< MethodIndex > (YY_MOVE (s.value));
        break;

      case symbol_kind::S_79_method_list: // method.list
      case symbol_kind::S_80_method_list_opt: // method.list.opt
        value.move< MethodListIndex > (YY_MOVE (s.value));
        break;

      case symbol_kind::S_name: // name
        value.move< NamedIdentifierIndex > (YY_MOVE (s.value));
        break;

      case symbol_kind::S_nil: // nil
        value.move< NilLitIndex > (YY_MOVE (s.value));
        break;

      case symbol_kind::S_accessor: // accessor
        value.move< ReadWriteAccessor > (YY_MOVE (s.value));
        break;

      case symbol_kind::S_region: // region
        value.move< RegionListIndex > (YY_MOVE (s.value));
        break;

      case symbol_kind::S_76_method_name: // method.name
      case symbol_kind::S_113_binary_op_raw: // binary_op.raw
      case symbol_kind::S_114_binary_op_no_adverb: // binary_op.no_adverb
          value.move<SelectorIndex>(YY_MOVE(s.value));
          break;

      case symbol_kind::S_binary_op: // binary_op
        value.move< SelectorMaybeAdverbIndex > (YY_MOVE (s.value));
        break;

      case symbol_kind::S_string: // string
        value.move< StringLitIndex > (YY_MOVE (s.value));
        break;

      case symbol_kind::S_symbol: // symbol
        value.move< SymbolLitIndex > (YY_MOVE (s.value));
        break;

      case symbol_kind::S_63_region_item: // region.item
        value.move< error_index<AnyExprIndex> > (YY_MOVE (s.value));
        break;

      case symbol_kind::S_68_class_super_opt: // class.super.opt
        value.move< maybe<ClassNameIdentifierIndex> > (YY_MOVE (s.value));
        break;

      case symbol_kind::S_69_class_slot_opt: // class.slot.opt
        value.move< maybe<NamedIdentifierIndex> > (YY_MOVE (s.value));
        break;

      default:
        break;
    }

    location = YY_MOVE (s.location);
  }

  // by_kind.
  parser::by_kind::by_kind () YY_NOEXCEPT
    : kind_ (symbol_kind::S_YYEMPTY)
  {}

#if 201103L <= YY_CPLUSPLUS
  parser::by_kind::by_kind (by_kind&& that) YY_NOEXCEPT
    : kind_ (that.kind_)
  {
    that.clear ();
  }
#endif

  parser::by_kind::by_kind (const by_kind& that) YY_NOEXCEPT
    : kind_ (that.kind_)
  {}

  parser::by_kind::by_kind (token_kind_type t) YY_NOEXCEPT
    : kind_ (yytranslate_ (t))
  {}



  void
  parser::by_kind::clear () YY_NOEXCEPT
  {
    kind_ = symbol_kind::S_YYEMPTY;
  }

  void
  parser::by_kind::move (by_kind& that)
  {
    kind_ = that.kind_;
    that.clear ();
  }

  parser::symbol_kind_type
  parser::by_kind::kind () const YY_NOEXCEPT
  {
    return kind_;
  }


  parser::symbol_kind_type
  parser::by_kind::type_get () const YY_NOEXCEPT
  {
    return this->kind ();
  }



  // by_state.
  parser::by_state::by_state () YY_NOEXCEPT
    : state (empty_state)
  {}

  parser::by_state::by_state (const by_state& that) YY_NOEXCEPT
    : state (that.state)
  {}

  void
  parser::by_state::clear () YY_NOEXCEPT
  {
    state = empty_state;
  }

  void
  parser::by_state::move (by_state& that)
  {
    state = that.state;
    that.clear ();
  }

  parser::by_state::by_state (state_type s) YY_NOEXCEPT
    : state (s)
  {}

  parser::symbol_kind_type
  parser::by_state::kind () const YY_NOEXCEPT
  {
    if (state == empty_state)
      return symbol_kind::S_YYEMPTY;
    else
      return YY_CAST (symbol_kind_type, yystos_[+state]);
  }

  parser::stack_symbol_type::stack_symbol_type ()
  {}

  parser::stack_symbol_type::stack_symbol_type (YY_RVREF (stack_symbol_type) that)
    : super_type (YY_MOVE (that.state), YY_MOVE (that.location))
  {
    switch (that.kind ())
    {
      case symbol_kind::S_ascii: // ascii
        value.YY_MOVE_OR_COPY< ASCIIIndex > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_126_accidental_unsigned: // accidental.unsigned
      case symbol_kind::S_accidental: // accidental
        value.YY_MOVE_OR_COPY< AccidentalIndex > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_adverb: // adverb
        value.YY_MOVE_OR_COPY< AdverbIndex > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_msgsend: // msgsend
      case symbol_kind::S_88_expr_base: // expr.base
      case symbol_kind::S_expr: // expr
      case symbol_kind::S_90_expr_seq_base: // expr.seq.base
      case symbol_kind::S_91_expr_seq: // expr.seq
          value.YY_MOVE_OR_COPY<AnyExprIndex>(YY_MOVE(that.value));
          break;

      case symbol_kind::S_method: // method
        value.YY_MOVE_OR_COPY< AnyMethodIndex > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_105_literal_terminal: // literal.terminal
      case symbol_kind::S_literal: // literal
          value.YY_MOVE_OR_COPY<AnyPossiblyLiteralIndex>(YY_MOVE(that.value));
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
        value.YY_MOVE_OR_COPY< BlockIndex > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_86_block_contents_item: // block.contents.item
          value.YY_MOVE_OR_COPY<BlockItemIndex>(YY_MOVE(that.value));
          break;

      case symbol_kind::S_83_block_opt_list: // block.opt_list
      case symbol_kind::S_84_block_list: // block.list
        value.YY_MOVE_OR_COPY< BlockListIndex > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_boolean: // boolean
        value.YY_MOVE_OR_COPY< BooleanLitIndex > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_70_class_extension: // class.extension
        value.YY_MOVE_OR_COPY< ClassExtensionIndex > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_class: // class
        value.YY_MOVE_OR_COPY< ClassIndex > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_go: // go
        value.YY_MOVE_OR_COPY< ClassListOrExprListIndex > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_66_classOrExtList_item: // classOrExtList.item
        value.YY_MOVE_OR_COPY< ClassOrExtensionIndex > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_65_classOrExtList_list: // classOrExtList.list
        value.YY_MOVE_OR_COPY< ClassOrExtensionListIndex > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_73_class_vars_entry: // class.vars.entry
        value.YY_MOVE_OR_COPY< DeclareAnyList > (YY_MOVE (that.value));
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
        value.YY_MOVE_OR_COPY< DeclareClassAnyVarListIndex > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_71_class_vars_entry_item: // class.vars.entry.item
        value.YY_MOVE_OR_COPY< DeclareClassVarIndex > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_72_class_vars_entry_list: // class.vars.entry.list
        value.YY_MOVE_OR_COPY< DeclareMemberListIndex > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_98_variable_declarations_list: // variable_declarations.list
      case symbol_kind::S_variable_declarations: // variable_declarations
        value.YY_MOVE_OR_COPY< DeclareVariableListIndex > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_107_literal_dictionary_entry: // literal.dictionary.entry
          value.YY_MOVE_OR_COPY<DictionaryEntryIndex>(YY_MOVE(that.value));
          break;

      case symbol_kind::S_108_literal_dictionary_entries: // literal.dictionary.entries
      case symbol_kind::S_109_literal_dictionary: // literal.dictionary
          value.YY_MOVE_OR_COPY<DictionaryIndex>(YY_MOVE(that.value));
          break;

      case symbol_kind::S_124_float_raw_unsigned: // float.raw_unsigned
      case symbol_kind::S_125_float_raw: // float.raw
          value.YY_MOVE_OR_COPY<FloatLitIndex>(YY_MOVE(that.value));
          break;

      case symbol_kind::S_float: // float
        value.YY_MOVE_OR_COPY< FloatProducingIndex > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_integer: // integer
        value.YY_MOVE_OR_COPY< IntLitIndex > (YY_MOVE (that.value));
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
        value.YY_MOVE_OR_COPY< LexerToken > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_77_method_base: // method.base
        value.YY_MOVE_OR_COPY< MethodIndex > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_79_method_list: // method.list
      case symbol_kind::S_80_method_list_opt: // method.list.opt
        value.YY_MOVE_OR_COPY< MethodListIndex > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_name: // name
        value.YY_MOVE_OR_COPY< NamedIdentifierIndex > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_nil: // nil
        value.YY_MOVE_OR_COPY< NilLitIndex > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_accessor: // accessor
        value.YY_MOVE_OR_COPY< ReadWriteAccessor > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_region: // region
        value.YY_MOVE_OR_COPY< RegionListIndex > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_76_method_name: // method.name
      case symbol_kind::S_113_binary_op_raw: // binary_op.raw
      case symbol_kind::S_114_binary_op_no_adverb: // binary_op.no_adverb
          value.YY_MOVE_OR_COPY<SelectorIndex>(YY_MOVE(that.value));
          break;

      case symbol_kind::S_binary_op: // binary_op
        value.YY_MOVE_OR_COPY< SelectorMaybeAdverbIndex > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_string: // string
        value.YY_MOVE_OR_COPY< StringLitIndex > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_symbol: // symbol
        value.YY_MOVE_OR_COPY< SymbolLitIndex > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_63_region_item: // region.item
        value.YY_MOVE_OR_COPY< error_index<AnyExprIndex> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_68_class_super_opt: // class.super.opt
        value.YY_MOVE_OR_COPY< maybe<ClassNameIdentifierIndex> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_69_class_slot_opt: // class.slot.opt
        value.YY_MOVE_OR_COPY< maybe<NamedIdentifierIndex> > (YY_MOVE (that.value));
        break;

      default:
        break;
    }

#if 201103L <= YY_CPLUSPLUS
    // that is emptied.
    that.state = empty_state;
#endif
  }

  parser::stack_symbol_type::stack_symbol_type (state_type s, YY_MOVE_REF (symbol_type) that)
    : super_type (s, YY_MOVE (that.location))
  {
    switch (that.kind ())
    {
      case symbol_kind::S_ascii: // ascii
        value.move< ASCIIIndex > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_126_accidental_unsigned: // accidental.unsigned
      case symbol_kind::S_accidental: // accidental
        value.move< AccidentalIndex > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_adverb: // adverb
        value.move< AdverbIndex > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_msgsend: // msgsend
      case symbol_kind::S_88_expr_base: // expr.base
      case symbol_kind::S_expr: // expr
      case symbol_kind::S_90_expr_seq_base: // expr.seq.base
      case symbol_kind::S_91_expr_seq: // expr.seq
          value.move<AnyExprIndex>(YY_MOVE(that.value));
          break;

      case symbol_kind::S_method: // method
        value.move< AnyMethodIndex > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_105_literal_terminal: // literal.terminal
      case symbol_kind::S_literal: // literal
          value.move<AnyPossiblyLiteralIndex>(YY_MOVE(that.value));
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
        value.move< BlockIndex > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_86_block_contents_item: // block.contents.item
          value.move<BlockItemIndex>(YY_MOVE(that.value));
          break;

      case symbol_kind::S_83_block_opt_list: // block.opt_list
      case symbol_kind::S_84_block_list: // block.list
        value.move< BlockListIndex > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_boolean: // boolean
        value.move< BooleanLitIndex > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_70_class_extension: // class.extension
        value.move< ClassExtensionIndex > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_class: // class
        value.move< ClassIndex > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_go: // go
        value.move< ClassListOrExprListIndex > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_66_classOrExtList_item: // classOrExtList.item
        value.move< ClassOrExtensionIndex > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_65_classOrExtList_list: // classOrExtList.list
        value.move< ClassOrExtensionListIndex > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_73_class_vars_entry: // class.vars.entry
        value.move< DeclareAnyList > (YY_MOVE (that.value));
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
        value.move< DeclareClassAnyVarListIndex > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_71_class_vars_entry_item: // class.vars.entry.item
        value.move< DeclareClassVarIndex > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_72_class_vars_entry_list: // class.vars.entry.list
        value.move< DeclareMemberListIndex > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_98_variable_declarations_list: // variable_declarations.list
      case symbol_kind::S_variable_declarations: // variable_declarations
        value.move< DeclareVariableListIndex > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_107_literal_dictionary_entry: // literal.dictionary.entry
          value.move<DictionaryEntryIndex>(YY_MOVE(that.value));
          break;

      case symbol_kind::S_108_literal_dictionary_entries: // literal.dictionary.entries
      case symbol_kind::S_109_literal_dictionary: // literal.dictionary
          value.move<DictionaryIndex>(YY_MOVE(that.value));
          break;

      case symbol_kind::S_124_float_raw_unsigned: // float.raw_unsigned
      case symbol_kind::S_125_float_raw: // float.raw
          value.move<FloatLitIndex>(YY_MOVE(that.value));
          break;

      case symbol_kind::S_float: // float
        value.move< FloatProducingIndex > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_integer: // integer
        value.move< IntLitIndex > (YY_MOVE (that.value));
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
        value.move< LexerToken > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_77_method_base: // method.base
        value.move< MethodIndex > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_79_method_list: // method.list
      case symbol_kind::S_80_method_list_opt: // method.list.opt
        value.move< MethodListIndex > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_name: // name
        value.move< NamedIdentifierIndex > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_nil: // nil
        value.move< NilLitIndex > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_accessor: // accessor
        value.move< ReadWriteAccessor > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_region: // region
        value.move< RegionListIndex > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_76_method_name: // method.name
      case symbol_kind::S_113_binary_op_raw: // binary_op.raw
      case symbol_kind::S_114_binary_op_no_adverb: // binary_op.no_adverb
          value.move<SelectorIndex>(YY_MOVE(that.value));
          break;

      case symbol_kind::S_binary_op: // binary_op
        value.move< SelectorMaybeAdverbIndex > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_string: // string
        value.move< StringLitIndex > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_symbol: // symbol
        value.move< SymbolLitIndex > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_63_region_item: // region.item
        value.move< error_index<AnyExprIndex> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_68_class_super_opt: // class.super.opt
        value.move< maybe<ClassNameIdentifierIndex> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_69_class_slot_opt: // class.slot.opt
        value.move< maybe<NamedIdentifierIndex> > (YY_MOVE (that.value));
        break;

      default:
        break;
    }

    // that is emptied.
    that.kind_ = symbol_kind::S_YYEMPTY;
  }

#if YY_CPLUSPLUS < 201103L
  parser::stack_symbol_type&
  parser::stack_symbol_type::operator= (const stack_symbol_type& that)
  {
    state = that.state;
    switch (that.kind ())
    {
      case symbol_kind::S_ascii: // ascii
        value.copy< ASCIIIndex > (that.value);
        break;

      case symbol_kind::S_126_accidental_unsigned: // accidental.unsigned
      case symbol_kind::S_accidental: // accidental
        value.copy< AccidentalIndex > (that.value);
        break;

      case symbol_kind::S_adverb: // adverb
        value.copy< AdverbIndex > (that.value);
        break;

      case symbol_kind::S_msgsend: // msgsend
      case symbol_kind::S_88_expr_base: // expr.base
      case symbol_kind::S_expr: // expr
      case symbol_kind::S_90_expr_seq_base: // expr.seq.base
      case symbol_kind::S_91_expr_seq: // expr.seq
          value.copy<AnyExprIndex>(that.value);
          break;

      case symbol_kind::S_method: // method
        value.copy< AnyMethodIndex > (that.value);
        break;

      case symbol_kind::S_105_literal_terminal: // literal.terminal
      case symbol_kind::S_literal: // literal
          value.copy<AnyPossiblyLiteralIndex>(that.value);
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
        value.copy< BlockIndex > (that.value);
        break;

      case symbol_kind::S_86_block_contents_item: // block.contents.item
          value.copy<BlockItemIndex>(that.value);
          break;

      case symbol_kind::S_83_block_opt_list: // block.opt_list
      case symbol_kind::S_84_block_list: // block.list
        value.copy< BlockListIndex > (that.value);
        break;

      case symbol_kind::S_boolean: // boolean
        value.copy< BooleanLitIndex > (that.value);
        break;

      case symbol_kind::S_70_class_extension: // class.extension
        value.copy< ClassExtensionIndex > (that.value);
        break;

      case symbol_kind::S_class: // class
        value.copy< ClassIndex > (that.value);
        break;

      case symbol_kind::S_go: // go
        value.copy< ClassListOrExprListIndex > (that.value);
        break;

      case symbol_kind::S_66_classOrExtList_item: // classOrExtList.item
        value.copy< ClassOrExtensionIndex > (that.value);
        break;

      case symbol_kind::S_65_classOrExtList_list: // classOrExtList.list
        value.copy< ClassOrExtensionListIndex > (that.value);
        break;

      case symbol_kind::S_73_class_vars_entry: // class.vars.entry
        value.copy< DeclareAnyList > (that.value);
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
        value.copy< DeclareClassAnyVarListIndex > (that.value);
        break;

      case symbol_kind::S_71_class_vars_entry_item: // class.vars.entry.item
        value.copy< DeclareClassVarIndex > (that.value);
        break;

      case symbol_kind::S_72_class_vars_entry_list: // class.vars.entry.list
        value.copy< DeclareMemberListIndex > (that.value);
        break;

      case symbol_kind::S_98_variable_declarations_list: // variable_declarations.list
      case symbol_kind::S_variable_declarations: // variable_declarations
        value.copy< DeclareVariableListIndex > (that.value);
        break;

      case symbol_kind::S_107_literal_dictionary_entry: // literal.dictionary.entry
          value.copy<DictionaryEntryIndex>(that.value);
          break;

      case symbol_kind::S_108_literal_dictionary_entries: // literal.dictionary.entries
      case symbol_kind::S_109_literal_dictionary: // literal.dictionary
          value.copy<DictionaryIndex>(that.value);
          break;

      case symbol_kind::S_124_float_raw_unsigned: // float.raw_unsigned
      case symbol_kind::S_125_float_raw: // float.raw
          value.copy<FloatLitIndex>(that.value);
          break;

      case symbol_kind::S_float: // float
        value.copy< FloatProducingIndex > (that.value);
        break;

      case symbol_kind::S_integer: // integer
        value.copy< IntLitIndex > (that.value);
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
        value.copy< LexerToken > (that.value);
        break;

      case symbol_kind::S_77_method_base: // method.base
        value.copy< MethodIndex > (that.value);
        break;

      case symbol_kind::S_79_method_list: // method.list
      case symbol_kind::S_80_method_list_opt: // method.list.opt
        value.copy< MethodListIndex > (that.value);
        break;

      case symbol_kind::S_name: // name
        value.copy< NamedIdentifierIndex > (that.value);
        break;

      case symbol_kind::S_nil: // nil
        value.copy< NilLitIndex > (that.value);
        break;

      case symbol_kind::S_accessor: // accessor
        value.copy< ReadWriteAccessor > (that.value);
        break;

      case symbol_kind::S_region: // region
        value.copy< RegionListIndex > (that.value);
        break;

      case symbol_kind::S_76_method_name: // method.name
      case symbol_kind::S_113_binary_op_raw: // binary_op.raw
      case symbol_kind::S_114_binary_op_no_adverb: // binary_op.no_adverb
          value.copy<SelectorIndex>(that.value);
          break;

      case symbol_kind::S_binary_op: // binary_op
        value.copy< SelectorMaybeAdverbIndex > (that.value);
        break;

      case symbol_kind::S_string: // string
        value.copy< StringLitIndex > (that.value);
        break;

      case symbol_kind::S_symbol: // symbol
        value.copy< SymbolLitIndex > (that.value);
        break;

      case symbol_kind::S_63_region_item: // region.item
        value.copy< error_index<AnyExprIndex> > (that.value);
        break;

      case symbol_kind::S_68_class_super_opt: // class.super.opt
        value.copy< maybe<ClassNameIdentifierIndex> > (that.value);
        break;

      case symbol_kind::S_69_class_slot_opt: // class.slot.opt
        value.copy< maybe<NamedIdentifierIndex> > (that.value);
        break;

      default:
        break;
    }

    location = that.location;
    return *this;
  }

  parser::stack_symbol_type&
  parser::stack_symbol_type::operator= (stack_symbol_type& that)
  {
    state = that.state;
    switch (that.kind ())
    {
      case symbol_kind::S_ascii: // ascii
        value.move< ASCIIIndex > (that.value);
        break;

      case symbol_kind::S_126_accidental_unsigned: // accidental.unsigned
      case symbol_kind::S_accidental: // accidental
        value.move< AccidentalIndex > (that.value);
        break;

      case symbol_kind::S_adverb: // adverb
        value.move< AdverbIndex > (that.value);
        break;

      case symbol_kind::S_msgsend: // msgsend
      case symbol_kind::S_88_expr_base: // expr.base
      case symbol_kind::S_expr: // expr
      case symbol_kind::S_90_expr_seq_base: // expr.seq.base
      case symbol_kind::S_91_expr_seq: // expr.seq
          value.move<AnyExprIndex>(that.value);
          break;

      case symbol_kind::S_method: // method
        value.move< AnyMethodIndex > (that.value);
        break;

      case symbol_kind::S_105_literal_terminal: // literal.terminal
      case symbol_kind::S_literal: // literal
          value.move<AnyPossiblyLiteralIndex>(that.value);
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
        value.move< BlockIndex > (that.value);
        break;

      case symbol_kind::S_86_block_contents_item: // block.contents.item
          value.move<BlockItemIndex>(that.value);
          break;

      case symbol_kind::S_83_block_opt_list: // block.opt_list
      case symbol_kind::S_84_block_list: // block.list
        value.move< BlockListIndex > (that.value);
        break;

      case symbol_kind::S_boolean: // boolean
        value.move< BooleanLitIndex > (that.value);
        break;

      case symbol_kind::S_70_class_extension: // class.extension
        value.move< ClassExtensionIndex > (that.value);
        break;

      case symbol_kind::S_class: // class
        value.move< ClassIndex > (that.value);
        break;

      case symbol_kind::S_go: // go
        value.move< ClassListOrExprListIndex > (that.value);
        break;

      case symbol_kind::S_66_classOrExtList_item: // classOrExtList.item
        value.move< ClassOrExtensionIndex > (that.value);
        break;

      case symbol_kind::S_65_classOrExtList_list: // classOrExtList.list
        value.move< ClassOrExtensionListIndex > (that.value);
        break;

      case symbol_kind::S_73_class_vars_entry: // class.vars.entry
        value.move< DeclareAnyList > (that.value);
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
        value.move< DeclareClassAnyVarListIndex > (that.value);
        break;

      case symbol_kind::S_71_class_vars_entry_item: // class.vars.entry.item
        value.move< DeclareClassVarIndex > (that.value);
        break;

      case symbol_kind::S_72_class_vars_entry_list: // class.vars.entry.list
        value.move< DeclareMemberListIndex > (that.value);
        break;

      case symbol_kind::S_98_variable_declarations_list: // variable_declarations.list
      case symbol_kind::S_variable_declarations: // variable_declarations
        value.move< DeclareVariableListIndex > (that.value);
        break;

      case symbol_kind::S_107_literal_dictionary_entry: // literal.dictionary.entry
          value.move<DictionaryEntryIndex>(that.value);
          break;

      case symbol_kind::S_108_literal_dictionary_entries: // literal.dictionary.entries
      case symbol_kind::S_109_literal_dictionary: // literal.dictionary
          value.move<DictionaryIndex>(that.value);
          break;

      case symbol_kind::S_124_float_raw_unsigned: // float.raw_unsigned
      case symbol_kind::S_125_float_raw: // float.raw
          value.move<FloatLitIndex>(that.value);
          break;

      case symbol_kind::S_float: // float
        value.move< FloatProducingIndex > (that.value);
        break;

      case symbol_kind::S_integer: // integer
        value.move< IntLitIndex > (that.value);
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
        value.move< LexerToken > (that.value);
        break;

      case symbol_kind::S_77_method_base: // method.base
        value.move< MethodIndex > (that.value);
        break;

      case symbol_kind::S_79_method_list: // method.list
      case symbol_kind::S_80_method_list_opt: // method.list.opt
        value.move< MethodListIndex > (that.value);
        break;

      case symbol_kind::S_name: // name
        value.move< NamedIdentifierIndex > (that.value);
        break;

      case symbol_kind::S_nil: // nil
        value.move< NilLitIndex > (that.value);
        break;

      case symbol_kind::S_accessor: // accessor
        value.move< ReadWriteAccessor > (that.value);
        break;

      case symbol_kind::S_region: // region
        value.move< RegionListIndex > (that.value);
        break;

      case symbol_kind::S_76_method_name: // method.name
      case symbol_kind::S_113_binary_op_raw: // binary_op.raw
      case symbol_kind::S_114_binary_op_no_adverb: // binary_op.no_adverb
          value.move<SelectorIndex>(that.value);
          break;

      case symbol_kind::S_binary_op: // binary_op
        value.move< SelectorMaybeAdverbIndex > (that.value);
        break;

      case symbol_kind::S_string: // string
        value.move< StringLitIndex > (that.value);
        break;

      case symbol_kind::S_symbol: // symbol
        value.move< SymbolLitIndex > (that.value);
        break;

      case symbol_kind::S_63_region_item: // region.item
        value.move< error_index<AnyExprIndex> > (that.value);
        break;

      case symbol_kind::S_68_class_super_opt: // class.super.opt
        value.move< maybe<ClassNameIdentifierIndex> > (that.value);
        break;

      case symbol_kind::S_69_class_slot_opt: // class.slot.opt
        value.move< maybe<NamedIdentifierIndex> > (that.value);
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

  template <typename Base>
  void
  parser::yy_destroy_ (const char* yymsg, basic_symbol<Base>& yysym) const
  {
    if (yymsg)
      YY_SYMBOL_PRINT (yymsg, yysym);
  }

#if YYDEBUG
  template <typename Base>
  void
  parser::yy_print_ (std::ostream& yyo, const basic_symbol<Base>& yysym) const
  {
    std::ostream& yyoutput = yyo;
    YY_USE (yyoutput);
    if (yysym.empty ())
      yyo << "empty symbol";
    else
      {
        symbol_kind_type yykind = yysym.kind ();
        yyo << (yykind < YYNTOKENS ? "token" : "nterm")
            << ' ' << yysym.name () << " ("
            << yysym.location << ": ";
        YY_USE (yykind);
        yyo << ')';
      }
  }
#endif

  void
  parser::yypush_ (const char* m, YY_MOVE_REF (stack_symbol_type) sym)
  {
    if (m)
      YY_SYMBOL_PRINT (m, sym);
    yystack_.push (YY_MOVE (sym));
  }

  void
  parser::yypush_ (const char* m, state_type s, YY_MOVE_REF (symbol_type) sym)
  {
#if 201103L <= YY_CPLUSPLUS
    yypush_ (m, stack_symbol_type (s, std::move (sym)));
#else
    stack_symbol_type ss (s, sym);
    yypush_ (m, ss);
#endif
  }

  void
  parser::yypop_ (int n) YY_NOEXCEPT
  {
    yystack_.pop (n);
  }

#if YYDEBUG
  std::ostream&
  parser::debug_stream () const
  {
    return *yycdebug_;
  }

  void
  parser::set_debug_stream (std::ostream& o)
  {
    yycdebug_ = &o;
  }


  parser::debug_level_type
  parser::debug_level () const
  {
    return yydebug_;
  }

  void
  parser::set_debug_level (debug_level_type l)
  {
    yydebug_ = l;
  }
#endif // YYDEBUG

  parser::state_type
  parser::yy_lr_goto_state_ (state_type yystate, int yysym)
  {
    int yyr = yypgoto_[yysym - YYNTOKENS] + yystate;
    if (0 <= yyr && yyr <= yylast_ && yycheck_[yyr] == yystate)
      return yytable_[yyr];
    else
      return yydefgoto_[yysym - YYNTOKENS];
  }

  bool
  parser::yy_pact_value_is_default_ (int yyvalue) YY_NOEXCEPT
  {
    return yyvalue == yypact_ninf_;
  }

  bool
  parser::yy_table_value_is_error_ (int yyvalue) YY_NOEXCEPT
  {
    return yyvalue == yytable_ninf_;
  }

  int
  parser::operator() ()
  {
    return parse ();
  }

  int
  parser::parse ()
  {
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
    yystack_.clear ();
    yypush_ (YY_NULLPTR, 0, YY_MOVE (yyla));

  /*-----------------------------------------------.
  | yynewstate -- push a new symbol on the stack.  |
  `-----------------------------------------------*/
  yynewstate:
    YYCDEBUG << "Entering state " << int (yystack_[0].state) << '\n';
    YY_STACK_PRINT ();

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
    if (yy_pact_value_is_default_ (yyn))
      goto yydefault;

    // Read a lookahead token.
    if (yyla.empty ())
      {
        YYCDEBUG << "Reading a token\n";
#if YY_EXCEPTIONS
        try
#endif // YY_EXCEPTIONS
          {
            yyla.kind_ = yytranslate_ (yylex (&yyla.value, &yyla.location, cxt));
          }
#if YY_EXCEPTIONS
        catch (const syntax_error& yyexc)
          {
            YYCDEBUG << "Caught exception: " << yyexc.what() << '\n';
            error (yyexc);
            goto yyerrlab1;
          }
#endif // YY_EXCEPTIONS
      }
    YY_SYMBOL_PRINT ("Next token is", yyla);

    if (yyla.kind () == symbol_kind::S_YYerror)
    {
      // The scanner already issued an error message, process directly
      // to error recovery.  But do not keep the error token as
      // lookahead, it is too special and may lead us to an endless
      // loop in error recovery. */
      yyla.kind_ = symbol_kind::S_YYUNDEF;
      goto yyerrlab1;
    }

    /* If the proper action on seeing token YYLA.TYPE is to reduce or
       to detect an error, take that action.  */
    yyn += yyla.kind ();
    if (yyn < 0 || yylast_ < yyn || yycheck_[yyn] != yyla.kind ())
      {
        goto yydefault;
      }

    // Reduce or error.
    yyn = yytable_[yyn];
    if (yyn <= 0)
      {
        if (yy_table_value_is_error_ (yyn))
          goto yyerrlab;
        yyn = -yyn;
        goto yyreduce;
      }

    // Count tokens shifted since error; after three, turn off error status.
    if (yyerrstatus_)
      --yyerrstatus_;

    // Shift the lookahead token.
    yypush_ ("Shifting", state_type (yyn), YY_MOVE (yyla));
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
      yylhs.state = yy_lr_goto_state_ (yystack_[yylen].state, yyr1_[yyn]);
      /* Variants are always initialized to an empty instance of the
         correct type. The default '$$ = $1' action is NOT applied
         when using variants.  */
      switch (yyr1_[yyn])
    {
      case symbol_kind::S_ascii: // ascii
        yylhs.value.emplace< ASCIIIndex > ();
        break;

      case symbol_kind::S_126_accidental_unsigned: // accidental.unsigned
      case symbol_kind::S_accidental: // accidental
        yylhs.value.emplace< AccidentalIndex > ();
        break;

      case symbol_kind::S_adverb: // adverb
        yylhs.value.emplace< AdverbIndex > ();
        break;

      case symbol_kind::S_msgsend: // msgsend
      case symbol_kind::S_88_expr_base: // expr.base
      case symbol_kind::S_expr: // expr
      case symbol_kind::S_90_expr_seq_base: // expr.seq.base
      case symbol_kind::S_91_expr_seq: // expr.seq
          yylhs.value.emplace<AnyExprIndex>();
          break;

      case symbol_kind::S_method: // method
        yylhs.value.emplace< AnyMethodIndex > ();
        break;

      case symbol_kind::S_105_literal_terminal: // literal.terminal
      case symbol_kind::S_literal: // literal
          yylhs.value.emplace<AnyPossiblyLiteralIndex>();
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
        yylhs.value.emplace< BlockIndex > ();
        break;

      case symbol_kind::S_86_block_contents_item: // block.contents.item
          yylhs.value.emplace<BlockItemIndex>();
          break;

      case symbol_kind::S_83_block_opt_list: // block.opt_list
      case symbol_kind::S_84_block_list: // block.list
        yylhs.value.emplace< BlockListIndex > ();
        break;

      case symbol_kind::S_boolean: // boolean
        yylhs.value.emplace< BooleanLitIndex > ();
        break;

      case symbol_kind::S_70_class_extension: // class.extension
        yylhs.value.emplace< ClassExtensionIndex > ();
        break;

      case symbol_kind::S_class: // class
        yylhs.value.emplace< ClassIndex > ();
        break;

      case symbol_kind::S_go: // go
        yylhs.value.emplace< ClassListOrExprListIndex > ();
        break;

      case symbol_kind::S_66_classOrExtList_item: // classOrExtList.item
        yylhs.value.emplace< ClassOrExtensionIndex > ();
        break;

      case symbol_kind::S_65_classOrExtList_list: // classOrExtList.list
        yylhs.value.emplace< ClassOrExtensionListIndex > ();
        break;

      case symbol_kind::S_73_class_vars_entry: // class.vars.entry
        yylhs.value.emplace< DeclareAnyList > ();
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
        yylhs.value.emplace< DeclareClassAnyVarListIndex > ();
        break;

      case symbol_kind::S_71_class_vars_entry_item: // class.vars.entry.item
        yylhs.value.emplace< DeclareClassVarIndex > ();
        break;

      case symbol_kind::S_72_class_vars_entry_list: // class.vars.entry.list
        yylhs.value.emplace< DeclareMemberListIndex > ();
        break;

      case symbol_kind::S_98_variable_declarations_list: // variable_declarations.list
      case symbol_kind::S_variable_declarations: // variable_declarations
        yylhs.value.emplace< DeclareVariableListIndex > ();
        break;

      case symbol_kind::S_107_literal_dictionary_entry: // literal.dictionary.entry
          yylhs.value.emplace<DictionaryEntryIndex>();
          break;

      case symbol_kind::S_108_literal_dictionary_entries: // literal.dictionary.entries
      case symbol_kind::S_109_literal_dictionary: // literal.dictionary
          yylhs.value.emplace<DictionaryIndex>();
          break;

      case symbol_kind::S_124_float_raw_unsigned: // float.raw_unsigned
      case symbol_kind::S_125_float_raw: // float.raw
          yylhs.value.emplace<FloatLitIndex>();
          break;

      case symbol_kind::S_float: // float
        yylhs.value.emplace< FloatProducingIndex > ();
        break;

      case symbol_kind::S_integer: // integer
        yylhs.value.emplace< IntLitIndex > ();
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
        yylhs.value.emplace< LexerToken > ();
        break;

      case symbol_kind::S_77_method_base: // method.base
        yylhs.value.emplace< MethodIndex > ();
        break;

      case symbol_kind::S_79_method_list: // method.list
      case symbol_kind::S_80_method_list_opt: // method.list.opt
        yylhs.value.emplace< MethodListIndex > ();
        break;

      case symbol_kind::S_name: // name
        yylhs.value.emplace< NamedIdentifierIndex > ();
        break;

      case symbol_kind::S_nil: // nil
        yylhs.value.emplace< NilLitIndex > ();
        break;

      case symbol_kind::S_accessor: // accessor
        yylhs.value.emplace< ReadWriteAccessor > ();
        break;

      case symbol_kind::S_region: // region
        yylhs.value.emplace< RegionListIndex > ();
        break;

      case symbol_kind::S_76_method_name: // method.name
      case symbol_kind::S_113_binary_op_raw: // binary_op.raw
      case symbol_kind::S_114_binary_op_no_adverb: // binary_op.no_adverb
          yylhs.value.emplace<SelectorIndex>();
          break;

      case symbol_kind::S_binary_op: // binary_op
        yylhs.value.emplace< SelectorMaybeAdverbIndex > ();
        break;

      case symbol_kind::S_string: // string
        yylhs.value.emplace< StringLitIndex > ();
        break;

      case symbol_kind::S_symbol: // symbol
        yylhs.value.emplace< SymbolLitIndex > ();
        break;

      case symbol_kind::S_63_region_item: // region.item
        yylhs.value.emplace< error_index<AnyExprIndex> > ();
        break;

      case symbol_kind::S_68_class_super_opt: // class.super.opt
        yylhs.value.emplace< maybe<ClassNameIdentifierIndex> > ();
        break;

      case symbol_kind::S_69_class_slot_opt: // class.slot.opt
        yylhs.value.emplace< maybe<NamedIdentifierIndex> > ();
        break;

      default:
        break;
    }


      // Default location.
      {
        stack_type::slice range (yystack_, yylen);
        YYLLOC_DEFAULT (yylhs.location, range, yylen);
        yyerror_range[1].location = yylhs.location;
      }

      // Perform the reduction.
      YY_REDUCE_PRINT (yyn);
#if YY_EXCEPTIONS
      try
#endif // YY_EXCEPTIONS
        {
          switch (yyn)
            {
          case 2: // go: region $end
#line 172 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<ClassListOrExprListIndex>() =
                  cxt.graph.assign_root(yystack_[1].value.as<RegionListIndex>());
          }
#line 2461 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 3: // go: classOrExtList.list $end
#line 173 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<ClassListOrExprListIndex>() =
                  cxt.graph.assign_root(yystack_[1].value.as<ClassOrExtensionListIndex>());
          }
#line 2467 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 4: // go: %empty
#line 174 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<ClassListOrExprListIndex>() =
                  cxt.graph.assign_root(cxt.create(ClassOrExtensionList {}, yylhs.location));
          }
#line 2473 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 5: // region.item: expr
#line 180 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<error_index<AnyExprIndex>>() = yystack_[0].value.as<AnyExprIndex>();
          }
#line 2479 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 6: // region.item: OPENPAREN argument_declarations block.contents CLOSEPAREN
#line 182 "langutils/sc_parser/src/sc_grammar.y"
          {
              auto block = cxt.create(BlockNode {}, yylhs.location, yystack_[2].value.as<DeclareArgumentListIndex>(),
                                      yystack_[1].value.as<BlockContentsListIndex>());

              yylhs.value.as<error_index<AnyExprIndex>>() = cxt.create(
                  MessageNode { MessageNode::SelectorMode::Value }, yylhs.location,
                  cxt.create(Missing {}, yylhs.location), cxt.create(ArgumentList {}, yylhs.location, block));
          }
#line 2494 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 7: // region.item: OPENPAREN argument_declarations CLOSEPAREN
#line 193 "langutils/sc_parser/src/sc_grammar.y"
          {
              auto block = cxt.create(BlockNode {}, yylhs.location, yystack_[1].value.as<DeclareArgumentListIndex>(),
                                      cxt.create(BlockContentsList {}, yylhs.location));

              yylhs.value.as<error_index<AnyExprIndex>>() = cxt.create(
                  MessageNode { MessageNode::SelectorMode::Value }, yylhs.location,
                  cxt.create(Missing {}, yylhs.location), cxt.create(ArgumentList {}, yylhs.location, block));
          }
#line 2509 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 8: // region: INTERPRET expr
#line 207 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<RegionListIndex>() =
                  cxt.create(RegionList {}, yylhs.location, yystack_[0].value.as<AnyExprIndex>());
          }
#line 2517 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 9: // region: INTERPRET error
#line 211 "langutils/sc_parser/src/sc_grammar.y"
          {
              error_recovery::expr(cxt);
              yyclearin;
              yylhs.value.as<RegionListIndex>() =
                  cxt.create(RegionList {}, yylhs.location, create_error(cxt, yystack_[0].location));
          }
#line 2527 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 10: // region: INTERPRET OPENPAREN argument_declarations block.contents semicolon.opt CLOSEPAREN
#line 217 "langutils/sc_parser/src/sc_grammar.y"
          {
              auto block = cxt.create(BlockNode {}, yylhs.location, yystack_[3].value.as<DeclareArgumentListIndex>(),
                                      yystack_[2].value.as<BlockContentsListIndex>());

              auto msg = cxt.create(MessageNode { MessageNode::SelectorMode::Value }, yylhs.location,
                                    cxt.create(Missing {}, yylhs.location),
                                    cxt.create(ArgumentList {}, yylhs.location, block));
              yylhs.value.as<RegionListIndex>() = cxt.create(RegionList {}, yylhs.location, msg);
          }
#line 2543 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 11: // region: INTERPRET OPENPAREN argument_declarations CLOSEPAREN
#line 229 "langutils/sc_parser/src/sc_grammar.y"
          {
              auto block = cxt.create(BlockNode {}, yylhs.location, yystack_[1].value.as<DeclareArgumentListIndex>(),
                                      cxt.create(BlockContentsList {}, yylhs.location));

              auto msg = cxt.create(MessageNode { MessageNode::SelectorMode::Value }, yylhs.location,
                                    cxt.create(Missing {}, yylhs.location),
                                    cxt.create(ArgumentList {}, yylhs.location, block));

              yylhs.value.as<RegionListIndex>() = cxt.create(RegionList {}, yylhs.location, msg);
          }
#line 2560 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 12: // region: region SEMICOLON region.item
#line 243 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<RegionListIndex>() =
                  cxt.graph.append(yystack_[2].value.as<RegionListIndex>(), yylhs.location,
                                   yystack_[0].value.as<error_index<AnyExprIndex>>());
          }
#line 2566 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 13: // region: region REGION_SEPARATOR region.item
#line 246 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<RegionListIndex>() =
                  cxt.graph.append(yystack_[2].value.as<RegionListIndex>(), yylhs.location,
                                   yystack_[0].value.as<error_index<AnyExprIndex>>());
          }
#line 2572 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 14: // region: region error
#line 249 "langutils/sc_parser/src/sc_grammar.y"
          {
              if (yystack_[1].location.end.line_number != yystack_[0].location.begin.line_number) {
                  auto first_child = cxt.graph.edges(*yystack_[1].value.as<RegionListIndex>()).first_child.value();
                  auto last_child = cxt.graph.last_child(*yystack_[1].value.as<RegionListIndex>());
                  auto loc = cxt.graph.location(last_child ? Index { *last_child } : Index { first_child });
                  error_recovery::region_separator(cxt, loc);
                  cxt.region_recovery = sc::ast::parser::ParserContext::RegionRecovery::EmitRegionSeparator;
                  static_assert(std::is_same_v<decltype(yyerrstatus_), int>);
                  yyerrstatus_ = 0; // this is NOT in the api, but the only way to get errors to re-emit.
                  yyclearin;
                  yylhs.value.as<RegionListIndex>() = yystack_[1].value.as<RegionListIndex>();
              } else {
                  error_recovery::expr(cxt);
                  yylhs.value.as<RegionListIndex>() = cxt.graph.append(
                      yystack_[1].value.as<RegionListIndex>(),
                      create_error(cxt, yystack_[0].location, yystack_[1].value.as<RegionListIndex>()));
                  yyclearin;
              }
          }
#line 2594 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 15: // classOrExtList.list: classOrExtList.item
#line 275 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<ClassOrExtensionListIndex>() =
                  cxt.create(ClassOrExtensionList {}, yylhs.location, yystack_[0].value.as<ClassOrExtensionIndex>());
          }
#line 2600 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 16: // classOrExtList.list: classOrExtList.list classOrExtList.item
#line 277 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<ClassOrExtensionListIndex>() = cxt.graph.append(
                  yystack_[1].value.as<ClassOrExtensionListIndex>(), yystack_[0].value.as<ClassOrExtensionIndex>());
          }
#line 2606 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 17: // classOrExtList.item: class
#line 281 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<ClassOrExtensionIndex>() = yystack_[0].value.as<ClassIndex>();
          }
#line 2612 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 18: // classOrExtList.item: class.extension
#line 282 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<ClassOrExtensionIndex>() = yystack_[0].value.as<ClassExtensionIndex>();
          }
#line 2618 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 19: // class: CLASSNAME class.slot.opt class.super.opt OPENCURLY class.vars.opt method.list.opt
                   // CLOSECURLY
#line 287 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<ClassIndex>() = cxt.create(
                  Class {}, yylhs.location, cxt.create(ClassNameIdentifier {}, yystack_[6].location),
                  yystack_[5].value.as<maybe<NamedIdentifierIndex>>(),
                  yystack_[4].value.as<maybe<ClassNameIdentifierIndex>>(),
                  yystack_[2].value.as<DeclareClassAnyVarListIndex>(), yystack_[1].value.as<MethodListIndex>());
          }
#line 2624 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 20: // class.super.opt: %empty
#line 291 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<maybe<ClassNameIdentifierIndex>>() = cxt.create(Missing {}, yylhs.location);
          }
#line 2630 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 21: // class.super.opt: COLON CLASSNAME
#line 292 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<maybe<ClassNameIdentifierIndex>>() =
                  cxt.create(ClassNameIdentifier {}, yystack_[0].location);
          }
#line 2636 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 22: // class.slot.opt: %empty
#line 296 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<maybe<NamedIdentifierIndex>>() = cxt.create(Missing {}, yylhs.location);
          }
#line 2642 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 23: // class.slot.opt: OPENSQUARE name CLOSESQUARE
#line 297 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<maybe<NamedIdentifierIndex>>() = yystack_[1].value.as<NamedIdentifierIndex>();
          }
#line 2648 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 24: // class.extension: ADD CLASSNAME OPENCURLY method.list.opt CLOSECURLY
#line 302 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<ClassExtensionIndex>() = cxt.create(
                  ClassExtension {}, yylhs.location, cxt.create(ClassNameIdentifier {}, yystack_[3].location),
                  yystack_[1].value.as<MethodListIndex>());
          }
#line 2654 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 25: // class.vars.entry.item: accessor variable_declarations.list.item
#line 307 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<DeclareClassVarIndex>() =
                  cxt.create(DeclareClassVar { yystack_[1].value.as<ReadWriteAccessor>() }, yylhs.location,
                             yystack_[0].value.as<DeclareAnyVariableIndex>());
          }
#line 2660 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 26: // class.vars.entry.list: class.vars.entry.item
#line 312 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<DeclareMemberListIndex>() =
                  cxt.create(DeclareMemberList {}, yylhs.location, yystack_[0].value.as<DeclareClassVarIndex>());
          }
#line 2666 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 27: // class.vars.entry.list: class.vars.entry.list COMMA class.vars.entry.item
#line 314 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<DeclareMemberListIndex>() = cxt.graph.append(
                  yystack_[2].value.as<DeclareMemberListIndex>(), yystack_[0].value.as<DeclareClassVarIndex>());
          }
#line 2672 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 28: // class.vars.entry: CLASSVAR class.vars.entry.list
#line 319 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<DeclareAnyList>() =
                  cxt.graph.cast<DeclareClassMemberList>(yystack_[0].value.as<DeclareMemberListIndex>());
          }
#line 2678 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 29: // class.vars.entry: VAR class.vars.entry.list
#line 321 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<DeclareAnyList>() =
                  cxt.graph.cast<DeclareMemberList>(yystack_[0].value.as<DeclareMemberListIndex>());
          }
#line 2684 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 30: // class.vars.entry: CONST class.vars.entry.list
#line 323 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<DeclareAnyList>() =
                  cxt.graph.cast<DeclareConstList>(yystack_[0].value.as<DeclareMemberListIndex>());
          }
#line 2690 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 31: // class.vars: class.vars.entry
#line 328 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<DeclareClassAnyVarListIndex>() =
                  cxt.create(ClassAnyVarList {}, yylhs.location, yystack_[0].value.as<DeclareAnyList>());
          }
#line 2696 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 32: // class.vars: class.vars SEMICOLON class.vars.entry
#line 330 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<DeclareClassAnyVarListIndex>() = cxt.graph.append(
                  yystack_[2].value.as<DeclareClassAnyVarListIndex>(), yystack_[0].value.as<DeclareAnyList>());
          }
#line 2702 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 33: // class.vars.opt: %empty
#line 334 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<DeclareClassAnyVarListIndex>() = cxt.create(ClassAnyVarList {}, yylhs.location);
          }
#line 2708 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 34: // class.vars.opt: class.vars semicolon.opt
#line 335 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<DeclareClassAnyVarListIndex>() = yystack_[1].value.as<DeclareClassAnyVarListIndex>();
          }
#line 2714 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 35: // method.name: name
#line 339 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<SelectorIndex>() = yystack_[0].value.as<NamedIdentifierIndex>();
          }
#line 2720 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 36: // method.name: binary_op.raw
#line 340 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<SelectorIndex>() = yystack_[0].value.as<SelectorIndex>();
          }
#line 2726 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 37: // method.base: method.name OPENCURLY argument_declarations.opt block.contents semicolon.opt
                   // CLOSECURLY
#line 345 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<MethodIndex>() = cxt.create(
                  Method {}, yylhs.location, yystack_[5].value.as<SelectorIndex>(),
                  yystack_[3].value.as<DeclareArgumentListIndex>(), cxt.create(Missing {}, yystack_[4].location),
                  yystack_[2].value.as<BlockContentsListIndex>());
          }
#line 2732 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 38: // method.base: method.name OPENCURLY argument_declarations.opt CLOSECURLY
#line 347 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<MethodIndex>() = cxt.create(
                  Method {}, yylhs.location, yystack_[3].value.as<SelectorIndex>(),
                  yystack_[1].value.as<DeclareArgumentListIndex>(), cxt.create(Missing {}, yystack_[2].location),
                  cxt.create(BlockList {}, yystack_[0].location));
          }
#line 2738 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 39: // method.base: method.name OPENCURLY argument_declarations.opt PRIMITIVENAME semicolon.opt
                   // block.contents semicolon.opt CLOSECURLY
#line 349 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<MethodIndex>() =
                  cxt.create(Method {}, yylhs.location, yystack_[7].value.as<SelectorIndex>(),
                             yystack_[5].value.as<DeclareArgumentListIndex>(),
                             cxt.create(PrimitiveIdentifier {}, yystack_[4].location),
                             yystack_[2].value.as<BlockContentsListIndex>());
          }
#line 2744 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 40: // method.base: method.name OPENCURLY argument_declarations.opt PRIMITIVENAME semicolon.opt
                   // CLOSECURLY
#line 351 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<MethodIndex>() =
                  cxt.create(Method {}, yylhs.location, yystack_[5].value.as<SelectorIndex>(),
                             yystack_[3].value.as<DeclareArgumentListIndex>(),
                             cxt.create(PrimitiveIdentifier {}, yystack_[2].location),
                             cxt.create(BlockList {}, yystack_[1].location));
          }
#line 2750 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 41: // method: method.base
#line 355 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<AnyMethodIndex>() = yystack_[0].value.as<MethodIndex>();
          }
#line 2756 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 42: // method: MULTIPLY method.base
#line 357 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<AnyMethodIndex>() = cxt.graph.cast<ClassMethod>(yystack_[0].value.as<MethodIndex>());
          }
#line 2762 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 43: // method.list: method
#line 361 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<MethodListIndex>() =
                  cxt.create(MethodList {}, yylhs.location, yystack_[0].value.as<AnyMethodIndex>());
          }
#line 2768 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 44: // method.list: method.list method
#line 362 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<MethodListIndex>() = cxt.graph.append(
                  yystack_[1].value.as<MethodListIndex>(), yylhs.location, yystack_[0].value.as<AnyMethodIndex>());
          }
#line 2774 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 45: // method.list.opt: %empty
#line 366 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<MethodListIndex>() = cxt.create(MethodList {}, yylhs.location);
          }
#line 2780 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 46: // method.list.opt: method.list
#line 367 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<MethodListIndex>() = yystack_[0].value.as<MethodListIndex>();
          }
#line 2786 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 49: // block: block.open argument_declarations.opt block.contents semicolon.opt CLOSECURLY
#line 374 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<BlockIndex>() =
                  cxt.create(BlockNode {}, yylhs.location, yystack_[3].value.as<DeclareArgumentListIndex>(),
                             yystack_[2].value.as<BlockContentsListIndex>());
          }
#line 2792 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 50: // block: block.open argument_declarations.opt CLOSECURLY
#line 376 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<BlockIndex>() =
                  cxt.create(BlockNode {}, yylhs.location, yystack_[1].value.as<DeclareArgumentListIndex>(),
                             cxt.create(BlockContentsList {}, yylhs.location));
          }
#line 2798 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 51: // block.opt_list: %empty
#line 380 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<BlockListIndex>() = {};
          }
#line 2804 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 52: // block.opt_list: block.list
#line 381 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<BlockListIndex>() = yystack_[0].value.as<BlockListIndex>();
          }
#line 2810 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 53: // block.list: block
#line 385 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<BlockListIndex>() =
                  cxt.create(BlockList {}, yylhs.location, yystack_[0].value.as<BlockIndex>());
          }
#line 2816 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 54: // block.list: block.list block
#line 386 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<BlockListIndex>() = cxt.graph.append(yystack_[1].value.as<BlockListIndex>(),
                                                                  yylhs.location, yystack_[0].value.as<BlockIndex>());
          }
#line 2822 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 55: // block.contents: block.contents.item
#line 390 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<BlockContentsListIndex>() =
                  cxt.create(BlockContentsList {}, yylhs.location, yystack_[0].value.as<BlockItemIndex>());
          }
#line 2828 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 56: // block.contents: block.contents SEMICOLON block.contents.item
#line 391 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<BlockContentsListIndex>() =
                  cxt.graph.append(yystack_[2].value.as<BlockContentsListIndex>(), yylhs.location,
                                   yystack_[0].value.as<BlockItemIndex>());
          }
#line 2834 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 57: // block.contents: block.contents NONLOCALRETURN expr
#line 393 "langutils/sc_parser/src/sc_grammar.y"
          {
              auto non_local =
                  cxt.create(NonLocalReturnExpr {}, yystack_[0].location, yystack_[0].value.as<AnyExprIndex>());
              yylhs.value.as<BlockContentsListIndex>() =
                  cxt.graph.append(yystack_[2].value.as<BlockContentsListIndex>(), yylhs.location, non_local);
          }
#line 2843 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 58: // block.contents: block.contents SEMICOLON NONLOCALRETURN expr
#line 398 "langutils/sc_parser/src/sc_grammar.y"
          {
              auto non_local =
                  cxt.create(NonLocalReturnExpr {}, yystack_[0].location, yystack_[0].value.as<AnyExprIndex>());
              yylhs.value.as<BlockContentsListIndex>() =
                  cxt.graph.append(yystack_[3].value.as<BlockContentsListIndex>(), yylhs.location, non_local);
          }
#line 2852 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 59: // block.contents: NONLOCALRETURN expr
#line 403 "langutils/sc_parser/src/sc_grammar.y"
          {
              auto non_local =
                  cxt.create(NonLocalReturnExpr {}, yystack_[0].location, yystack_[0].value.as<AnyExprIndex>());
              yylhs.value.as<BlockContentsListIndex>() = cxt.create(BlockContentsList {}, yylhs.location, non_local);
          }
#line 2861 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 60: // block.contents.item: expr
#line 410 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<BlockItemIndex>() = yystack_[0].value.as<AnyExprIndex>();
          }
#line 2867 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 61: // block.contents.item: variable_declarations
#line 411 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<BlockItemIndex>() = yystack_[0].value.as<DeclareVariableListIndex>();
          }
#line 2873 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 62: // msgsend: OPENPAREN binary_op.no_adverb CLOSEPAREN OPENPAREN arguments CLOSEPAREN block.opt_list
#line 416 "langutils/sc_parser/src/sc_grammar.y"
          {
              if (yystack_[0].value.as<BlockListIndex>()) {
                  cxt.graph.merge(yystack_[2].value.as<ArgumentListIndex>(), yystack_[0].value.as<BlockListIndex>());
                  cxt.graph.location(*yystack_[2].value.as<ArgumentListIndex>()) =
                      yylhs.location; // spans arguments and block list
              }
              yylhs.value.as<AnyExprIndex>() =
                  cxt.create(MessageNode {}, yylhs.location, yystack_[5].value.as<SelectorIndex>(),
                             yystack_[2].value.as<ArgumentListIndex>());
          }
#line 2885 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 63: // msgsend: OPENPAREN binary_op.no_adverb CLOSEPAREN OPENPAREN CLOSEPAREN block.list
#line 425 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<AnyExprIndex>() =
                  cxt.create(MessageNode {}, yylhs.location, yystack_[4].value.as<SelectorIndex>(),
                             yystack_[0].value.as<BlockListIndex>());
          }
#line 2891 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 64: // msgsend: OPENPAREN binary_op.no_adverb CLOSEPAREN block.list
#line 428 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<AnyExprIndex>() =
                  cxt.create(MessageNode {}, yylhs.location, yystack_[2].value.as<SelectorIndex>(),
                             cxt.create(ArgumentList {}, yylhs.location, yystack_[0].value.as<BlockListIndex>()));
          }
#line 2897 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 65: // msgsend: name OPENPAREN arguments CLOSEPAREN block.opt_list
#line 432 "langutils/sc_parser/src/sc_grammar.y"
          {
              if (yystack_[0].value.as<BlockListIndex>()) {
                  cxt.graph.merge(yystack_[2].value.as<ArgumentListIndex>(), yystack_[0].value.as<BlockListIndex>());
                  cxt.graph.location(*yystack_[2].value.as<ArgumentListIndex>()) = {
                      yystack_[2].location.begin, yystack_[0].location.end
                  }; // spans arguments and block list
              }
              yylhs.value.as<AnyExprIndex>() =
                  cxt.create(MessageNode {}, yylhs.location, yystack_[4].value.as<NamedIdentifierIndex>(),
                             yystack_[2].value.as<ArgumentListIndex>());
          }
#line 2909 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 66: // msgsend: name OPENPAREN CLOSEPAREN block.list
#line 441 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<AnyExprIndex>() =
                  cxt.create(MessageNode {}, yylhs.location, yystack_[3].value.as<NamedIdentifierIndex>(),
                             yystack_[0].value.as<BlockListIndex>());
          }
#line 2915 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 67: // msgsend: name block.list
#line 444 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<AnyExprIndex>() =
                  cxt.create(MessageNode {}, yylhs.location, yystack_[1].value.as<NamedIdentifierIndex>(),
                             cxt.create(ArgumentList {}, yystack_[0].location, yystack_[0].value.as<BlockListIndex>()));
          }
#line 2921 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 68: // msgsend: expr DOT name arguments.maybe_paren block.opt_list
#line 447 "langutils/sc_parser/src/sc_grammar.y"
          {
              if (yystack_[0].value.as<BlockListIndex>()) {
                  cxt.graph.merge(yystack_[1].value.as<ArgumentListIndex>(), yystack_[0].value.as<BlockListIndex>());
                  cxt.graph.location(*yystack_[1].value.as<ArgumentListIndex>()) = {
                      yystack_[1].location.begin, yystack_[0].location.end
                  }; // spans arguments and block list
              }
              cxt.graph.prepend(yystack_[1].value.as<ArgumentListIndex>(),
                                yystack_[4].value.as<AnyExprIndex>()); // put the receiver in place
              cxt.graph.location(*yystack_[1].value.as<ArgumentListIndex>()) = {
                  yystack_[4].location.begin, yystack_[1].location.end
              }; // spans arguments and block list
              yylhs.value.as<AnyExprIndex>() =
                  cxt.create(MessageNode {}, yylhs.location, yystack_[2].value.as<NamedIdentifierIndex>(),
                             yystack_[1].value.as<ArgumentListIndex>());
          }
#line 2935 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 69: // msgsend: expr DOT arguments.paren block.opt_list
#line 458 "langutils/sc_parser/src/sc_grammar.y"
          {
              if (yystack_[0].value.as<BlockListIndex>()) {
                  cxt.graph.merge(yystack_[1].value.as<ArgumentListIndex>(), yystack_[0].value.as<BlockListIndex>());
                  cxt.graph.location(*yystack_[1].value.as<ArgumentListIndex>()) = {
                      yystack_[1].location.begin, yystack_[0].location.end
                  }; // spans arguments and block list
              }
              cxt.graph.prepend(yystack_[1].value.as<ArgumentListIndex>(),
                                yystack_[3].value.as<AnyExprIndex>()); // put the receiver in place
              cxt.graph.location(*yystack_[1].value.as<ArgumentListIndex>()) = {
                  yystack_[3].location.begin, yystack_[1].location.end
              }; // spans arguments and block list
              yylhs.value.as<AnyExprIndex>() =
                  cxt.create(MessageNode { MessageNode::SelectorMode::Value }, yylhs.location,
                             cxt.create(Missing {}, yystack_[2].location), yystack_[1].value.as<ArgumentListIndex>());
          }
#line 2949 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 70: // msgsend: expr DOT OPENPAREN CLOSEPAREN block.opt_list
#line 468 "langutils/sc_parser/src/sc_grammar.y"
          {
              auto args = cxt.create(ArgumentList {}, yylhs.location, yystack_[4].value.as<AnyExprIndex>());
              if (yystack_[0].value.as<BlockListIndex>())
                  cxt.graph.merge(args, yystack_[0].value.as<BlockListIndex>());
              yylhs.value.as<AnyExprIndex>() =
                  cxt.create(MessageNode { MessageNode::SelectorMode::Value }, yylhs.location,
                             cxt.create(Missing {}, yystack_[3].location), args);
          }
#line 2959 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 71: // msgsend: CLASSNAME OPENSQUARE literal.array.contents CLOSESQUARE
#line 483 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<AnyExprIndex>() = cxt.create(CollectionNode {}, yylhs.location,
                                                          cxt.create(ClassNameIdentifier {}, yystack_[3].location),
                                                          yystack_[1].value.as<ArrayIndex>());
          }
#line 2965 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 72: // msgsend: CLASSNAME block.list
#line 486 "langutils/sc_parser/src/sc_grammar.y"
          {
              auto args =
                  cxt.create(ArgumentList {}, yylhs.location, cxt.create(NamedIdentifier {}, yystack_[1].location));
              cxt.graph.merge(args, yystack_[0].value.as<BlockListIndex>());
              yylhs.value.as<AnyExprIndex>() =
                  cxt.create(MessageNode {}, yylhs.location, cxt.create(Missing {}, yystack_[1].location), args);
          }
#line 2975 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 73: // msgsend: CLASSNAME OPENPAREN arguments CLOSEPAREN block.opt_list
#line 492 "langutils/sc_parser/src/sc_grammar.y"
          {
              if (yystack_[0].value.as<BlockListIndex>()) {
                  cxt.graph.merge(yystack_[2].value.as<ArgumentListIndex>(), yystack_[0].value.as<BlockListIndex>());
                  cxt.graph.location(*yystack_[2].value.as<ArgumentListIndex>()) = {
                      yystack_[2].location.begin, yystack_[0].location.end
                  }; // spans arguments and block list
              }
              cxt.graph.prepend(yystack_[2].value.as<ArgumentListIndex>(),
                                cxt.create(ClassNameIdentifier {}, yystack_[4].location)); // put the receiver in place
              cxt.graph.location(*yystack_[2].value.as<ArgumentListIndex>()) = {
                  yystack_[2].location.begin, yystack_[0].location.end
              }; // spans arguments and block list
              yylhs.value.as<AnyExprIndex>() =
                  cxt.create(MessageNode { MessageNode::SelectorMode::New }, yylhs.location,
                             cxt.create(Missing {}, yystack_[4].location), yystack_[2].value.as<ArgumentListIndex>());
          }
#line 2989 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 74: // msgsend: CLASSNAME OPENPAREN CLOSEPAREN block.opt_list
#line 503 "langutils/sc_parser/src/sc_grammar.y"
          {
              auto args = cxt.create(ArgumentList {}, yylhs.location, yystack_[0].value.as<BlockListIndex>());
              cxt.graph.prepend(args,
                                cxt.create(ClassNameIdentifier {}, yystack_[3].location)); // put the receiver in place
              yylhs.value.as<AnyExprIndex>() =
                  cxt.create(MessageNode { MessageNode::SelectorMode::New }, yylhs.location,
                             cxt.create(Missing {}, yystack_[3].location), args);
          }
#line 2999 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 75: // expr.base: literal
#line 512 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<AnyExprIndex>() = yystack_[0].value.as<AnyPossiblyLiteralIndex>();
          }
#line 3005 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 76: // expr.base: name
#line 514 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<AnyExprIndex>() = yystack_[0].value.as<NamedIdentifierIndex>();
          }
#line 3011 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 77: // expr.base: msgsend
#line 516 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<AnyExprIndex>() = yystack_[0].value.as<AnyExprIndex>();
          }
#line 3017 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 78: // expr.base: OPENPAREN block.contents semicolon.opt CLOSEPAREN
#line 518 "langutils/sc_parser/src/sc_grammar.y"
          {
              auto blk = cxt.create(BlockNode {}, yylhs.location, cxt.create(DeclareArgumentList {}, yylhs.location),
                                    yystack_[2].value.as<BlockContentsListIndex>());
              yylhs.value.as<AnyExprIndex>() = cxt.create(
                  MessageNode { MessageNode::SelectorMode::Value }, yylhs.location.flatten(),
                  cxt.create(Missing {}, yylhs.location.flatten()), cxt.create(ArgumentList {}, yylhs.location, blk));
          }
#line 3026 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 79: // expr.base: TILDE name
#line 522 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<AnyExprIndex>() =
                  cxt.create(EnvIdentifierNode {}, yylhs.location, yystack_[0].value.as<NamedIdentifierIndex>());
          }
#line 3032 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 80: // expr.base: expr.base OPENSQUARE arguments CLOSESQUARE
#line 528 "langutils/sc_parser/src/sc_grammar.y"
          {
              cxt.graph.prepend(yystack_[1].value.as<ArgumentListIndex>(),
                                yystack_[3].value.as<AnyExprIndex>()); // put receiver in place.
              cxt.graph.location(*yystack_[1].value.as<ArgumentListIndex>()) = yylhs.location;
              yylhs.value.as<AnyExprIndex>() =
                  cxt.create(MessageNode { MessageNode::SelectorMode::At }, yylhs.location,
                             cxt.create(Missing {}, yystack_[2].location), yystack_[1].value.as<ArgumentListIndex>());
          }
#line 3042 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 81: // expr.base: expr.base OPENSQUARE CLOSESQUARE
#line 535 "langutils/sc_parser/src/sc_grammar.y"
          {
              auto args = cxt.create(ArgumentList {}, yylhs.location, yystack_[2].value.as<AnyExprIndex>());
              yylhs.value.as<AnyExprIndex>() = cxt.create(MessageNode { MessageNode::SelectorMode::At }, yylhs.location,
                                                          cxt.create(Missing {}, yystack_[1].location), args);
          }
#line 3051 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 82: // expr: expr.base
#line 544 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<AnyExprIndex>() = yystack_[0].value.as<AnyExprIndex>();
          }
#line 3057 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 83: // expr: CLASSNAME
#line 548 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<AnyExprIndex>() = cxt.create(ClassNameIdentifier {}, yylhs.location);
          }
#line 3063 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 84: // expr: expr DOT OPENSQUARE arguments CLOSESQUARE
#line 551 "langutils/sc_parser/src/sc_grammar.y"
          {
              cxt.graph.prepend(yystack_[1].value.as<ArgumentListIndex>(),
                                yystack_[4].value.as<AnyExprIndex>()); // put receiver in place
              yylhs.value.as<AnyExprIndex>() =
                  cxt.create(MessageNode { MessageNode::SelectorMode::At }, yylhs.location,
                             cxt.create(Missing {}, yylhs.location), yystack_[1].value.as<ArgumentListIndex>());
          }
#line 3072 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 85: // expr: expr DOT OPENSQUARE CLOSESQUARE
#line 556 "langutils/sc_parser/src/sc_grammar.y"
          {
              auto args = cxt.create(ArgumentList {}, yylhs.location, yystack_[3].value.as<AnyExprIndex>());
              yylhs.value.as<AnyExprIndex>() = cxt.create(MessageNode { MessageNode::SelectorMode::At }, yylhs.location,
                                                          cxt.create(Missing {}, yystack_[1].location), args);
          }
#line 3081 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 86: // expr: BACKTICK expr
#line 561 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<AnyExprIndex>() =
                  cxt.create(ReferenceNode {}, yylhs.location, yystack_[0].value.as<AnyExprIndex>());
          }
#line 3087 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 87: // expr: expr binary_op expr
#line 564 "langutils/sc_parser/src/sc_grammar.y"
          {
              auto args = cxt.create(ArgumentList {}, yylhs.location, yystack_[2].value.as<AnyExprIndex>(),
                                     yystack_[0].value.as<AnyExprIndex>());
              yylhs.value.as<AnyExprIndex>() =
                  cxt.create(MessageNode {}, yylhs.location, yystack_[1].value.as<SelectorMaybeAdverbIndex>(), args);
          }
#line 3096 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 88: // expr: name EQUALSSIGN expr
#line 570 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<AnyExprIndex>() =
                  cxt.create(AssignmentNode {}, yylhs.location, yystack_[2].value.as<NamedIdentifierIndex>(),
                             yystack_[0].value.as<AnyExprIndex>());
          }
#line 3102 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 89: // expr: TILDE name EQUALSSIGN expr
#line 573 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<AnyExprIndex>() =
                  cxt.create(AssignmentNode { AssignmentNode::Target::Environment }, yylhs.location,
                             yystack_[2].value.as<NamedIdentifierIndex>(), yystack_[0].value.as<AnyExprIndex>());
          }
#line 3108 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 90: // expr: expr DOT name EQUALSSIGN expr
#line 576 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<AnyExprIndex>() =
                  cxt.create(SetterNode {}, yylhs.location, yystack_[4].value.as<AnyExprIndex>(),
                             yystack_[2].value.as<NamedIdentifierIndex>(), yystack_[0].value.as<AnyExprIndex>());
          }
#line 3114 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 91: // expr: name OPENPAREN arguments CLOSEPAREN EQUALSSIGN expr
#line 579 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<AnyExprIndex>() =
                  cxt.create(SetterNode {}, yylhs.location, yystack_[3].value.as<ArgumentListIndex>(),
                             yystack_[5].value.as<NamedIdentifierIndex>(), yystack_[0].value.as<AnyExprIndex>());
          }
#line 3120 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 92: // expr: expr.base OPENSQUARE arguments CLOSESQUARE EQUALSSIGN expr
#line 587 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<AnyExprIndex>() =
                  cxt.create(AssignmentAtNode {}, yylhs.location, yystack_[5].value.as<AnyExprIndex>(),
                             yystack_[3].value.as<ArgumentListIndex>(), yystack_[0].value.as<AnyExprIndex>());
          }
#line 3126 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 93: // expr: expr.base OPENSQUARE CLOSESQUARE EQUALSSIGN expr
#line 589 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<AnyExprIndex>() =
                  cxt.create(AssignmentAtNode {}, yylhs.location, yystack_[4].value.as<AnyExprIndex>(),
                             cxt.create(ArgumentList {}, yystack_[3].location), yystack_[0].value.as<AnyExprIndex>());
          }
#line 3132 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 94: // expr: expr DOT OPENSQUARE arguments CLOSESQUARE EQUALSSIGN expr
#line 592 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<AnyExprIndex>() =
                  cxt.create(AssignmentAtNode {}, yylhs.location, yystack_[6].value.as<AnyExprIndex>(),
                             yystack_[3].value.as<ArgumentListIndex>(), yystack_[0].value.as<AnyExprIndex>());
          }
#line 3138 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 95: // expr: expr DOT OPENSQUARE CLOSESQUARE EQUALSSIGN expr
#line 595 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<AnyExprIndex>() =
                  cxt.create(AssignmentAtNode {}, yylhs.location, yystack_[5].value.as<AnyExprIndex>(),
                             cxt.create(ArgumentList {}, yystack_[3].location), yystack_[0].value.as<AnyExprIndex>());
          }
#line 3144 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 96: // expr.seq.base: expr
#line 599 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<AnyExprIndex>() = yystack_[0].value.as<AnyExprIndex>();
          }
#line 3150 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 97: // expr.seq.base: expr.seq.base SEMICOLON expr
#line 601 "langutils/sc_parser/src/sc_grammar.y"
          {
              // This piece of logic is here because exprs can contain expr.seq, so we avoid creating the list node if
              // we can.
              if (auto expr_seq = cxt.graph.as_a<ExprSeqIndex>(*yystack_[2].value.as<AnyExprIndex>())) {
                  cxt.graph.append(*expr_seq, yylhs.location, yystack_[0].value.as<AnyExprIndex>());
                  yylhs.value.as<AnyExprIndex>() = yystack_[2].value.as<AnyExprIndex>();
              } else {
                  yylhs.value.as<AnyExprIndex>() =
                      cxt.create(ExprSeq {}, yylhs.location, yystack_[2].value.as<AnyExprIndex>(),
                                 yystack_[0].value.as<AnyExprIndex>());
              }
          }
#line 3164 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 98: // expr.seq: expr.seq.base semicolon.opt
#line 612 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<AnyExprIndex>() = yystack_[1].value.as<AnyExprIndex>();
          }
#line 3170 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 99: // adverb: DOT name
#line 615 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<AdverbIndex>() = yystack_[0].value.as<NamedIdentifierIndex>();
          }
#line 3176 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 100: // adverb: DOT integer
#line 616 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<AdverbIndex>() = yystack_[0].value.as<IntLitIndex>();
          }
#line 3182 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 101: // adverb: DOT OPENPAREN expr.seq CLOSEPAREN
#line 617 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<AdverbIndex>() =
                  cxt.create(AdverbExprNode {}, yylhs.location, yystack_[1].value.as<AnyExprIndex>());
          }
#line 3188 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 102: // argument_declarations.list: name
#line 623 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<DeclareArgumentListIndex>() =
                  cxt.create(DeclareArgumentList {}, yylhs.location, yystack_[0].value.as<NamedIdentifierIndex>());
          }
#line 3194 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 103: // argument_declarations.list: name EQUALSSIGN literal
#line 625 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<DeclareArgumentListIndex>() =
                  cxt.create(DeclareArgumentList {}, yylhs.location,
                             cxt.create(DeclareArgumentWithDefaultNode { true }, yylhs.location,
                                        yystack_[2].value.as<NamedIdentifierIndex>(),
                                        yystack_[0].value.as<AnyPossiblyLiteralIndex>()));
          }
#line 3200 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 104: // argument_declarations.list: name OPENPAREN expr.seq CLOSEPAREN
#line 627 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<DeclareArgumentListIndex>() = cxt.create(
                  DeclareArgumentList {}, yylhs.location,
                  cxt.create(DeclareArgumentWithDefaultNode { false }, yylhs.location,
                             yystack_[3].value.as<NamedIdentifierIndex>(), yystack_[1].value.as<AnyExprIndex>()));
          }
#line 3206 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 105: // argument_declarations.list: name EQUALSSIGN OPENPAREN expr.seq CLOSEPAREN
#line 629 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<DeclareArgumentListIndex>() = cxt.create(
                  DeclareArgumentList {}, yylhs.location,
                  cxt.create(DeclareArgumentWithDefaultNode { false }, yylhs.location,
                             yystack_[4].value.as<NamedIdentifierIndex>(), yystack_[1].value.as<AnyExprIndex>()));
          }
#line 3212 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 106: // argument_declarations.list: argument_declarations.list COMMA name
#line 631 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<DeclareArgumentListIndex>() =
                  cxt.graph.append(yystack_[2].value.as<DeclareArgumentListIndex>(), yylhs.location,
                                   yystack_[0].value.as<NamedIdentifierIndex>());
          }
#line 3218 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 107: // argument_declarations.list: argument_declarations.list COMMA name EQUALSSIGN literal
#line 633 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<DeclareArgumentListIndex>() =
                  cxt.graph.append(yystack_[4].value.as<DeclareArgumentListIndex>(), yylhs.location,
                                   cxt.create(DeclareArgumentWithDefaultNode { true }, yylhs.location,
                                              yystack_[2].value.as<NamedIdentifierIndex>(),
                                              yystack_[0].value.as<AnyPossiblyLiteralIndex>()));
          }
#line 3224 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 108: // argument_declarations.list: argument_declarations.list COMMA name OPENPAREN expr.seq CLOSEPAREN
#line 635 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<DeclareArgumentListIndex>() = cxt.graph.append(
                  yystack_[5].value.as<DeclareArgumentListIndex>(), yylhs.location,
                  cxt.create(DeclareArgumentWithDefaultNode { false }, yylhs.location,
                             yystack_[3].value.as<NamedIdentifierIndex>(), yystack_[1].value.as<AnyExprIndex>()));
          }
#line 3230 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 109: // argument_declarations.list: argument_declarations.list COMMA name EQUALSSIGN OPENPAREN expr.seq
                    // CLOSEPAREN
#line 637 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<DeclareArgumentListIndex>() = cxt.graph.append(
                  yystack_[6].value.as<DeclareArgumentListIndex>(), yylhs.location,
                  cxt.create(DeclareArgumentWithDefaultNode { false }, yylhs.location,
                             yystack_[4].value.as<NamedIdentifierIndex>(), yystack_[1].value.as<AnyExprIndex>()));
          }
#line 3236 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 110: // argument_declarations.pipelist: name literal
#line 642 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<DeclareArgumentListIndex>() =
                  cxt.create(DeclareArgumentList {}, yylhs.location,
                             cxt.create(DeclareArgumentWithDefaultNode { true }, yylhs.location,
                                        yystack_[1].value.as<NamedIdentifierIndex>(),
                                        yystack_[0].value.as<AnyPossiblyLiteralIndex>()));
          }
#line 3242 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 111: // argument_declarations.pipelist: name
#line 644 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<DeclareArgumentListIndex>() =
                  cxt.create(DeclareArgumentList {}, yylhs.location, yystack_[0].value.as<NamedIdentifierIndex>());
          }
#line 3248 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 112: // argument_declarations.pipelist: name EQUALSSIGN literal
#line 646 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<DeclareArgumentListIndex>() =
                  cxt.create(DeclareArgumentList {}, yylhs.location,
                             cxt.create(DeclareArgumentWithDefaultNode { true }, yylhs.location,
                                        yystack_[2].value.as<NamedIdentifierIndex>(),
                                        yystack_[0].value.as<AnyPossiblyLiteralIndex>()));
          }
#line 3254 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 113: // argument_declarations.pipelist: name OPENPAREN expr.seq CLOSEPAREN
#line 648 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<DeclareArgumentListIndex>() = cxt.create(
                  DeclareArgumentList {}, yylhs.location,
                  cxt.create(DeclareArgumentWithDefaultNode { false }, yylhs.location,
                             yystack_[3].value.as<NamedIdentifierIndex>(), yystack_[1].value.as<AnyExprIndex>()));
          }
#line 3260 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 114: // argument_declarations.pipelist: name EQUALSSIGN OPENPAREN expr.seq CLOSEPAREN
#line 650 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<DeclareArgumentListIndex>() = cxt.create(
                  DeclareArgumentList {}, yylhs.location,
                  cxt.create(DeclareArgumentWithDefaultNode { false }, yylhs.location,
                             yystack_[4].value.as<NamedIdentifierIndex>(), yystack_[1].value.as<AnyExprIndex>()));
          }
#line 3266 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 115: // argument_declarations.pipelist: argument_declarations.pipelist comma.opt name literal
#line 652 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<DeclareArgumentListIndex>() =
                  cxt.graph.append(yystack_[3].value.as<DeclareArgumentListIndex>(), yylhs.location,
                                   cxt.create(DeclareArgumentWithDefaultNode { true }, yylhs.location,
                                              yystack_[1].value.as<NamedIdentifierIndex>(),
                                              yystack_[0].value.as<AnyPossiblyLiteralIndex>()));
          }
#line 3272 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 116: // argument_declarations.pipelist: argument_declarations.pipelist comma.opt name
#line 654 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<DeclareArgumentListIndex>() =
                  cxt.graph.append(yystack_[2].value.as<DeclareArgumentListIndex>(), yylhs.location,
                                   yystack_[0].value.as<NamedIdentifierIndex>());
          }
#line 3278 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 117: // argument_declarations.pipelist: argument_declarations.pipelist comma.opt name EQUALSSIGN literal
#line 656 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<DeclareArgumentListIndex>() =
                  cxt.graph.append(yystack_[4].value.as<DeclareArgumentListIndex>(), yylhs.location,
                                   cxt.create(DeclareArgumentWithDefaultNode { true }, yylhs.location,
                                              yystack_[2].value.as<NamedIdentifierIndex>(),
                                              yystack_[0].value.as<AnyPossiblyLiteralIndex>()));
          }
#line 3284 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 118: // argument_declarations.pipelist: argument_declarations.pipelist comma.opt name OPENPAREN expr.seq
                    // CLOSEPAREN
#line 658 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<DeclareArgumentListIndex>() = cxt.graph.append(
                  yystack_[5].value.as<DeclareArgumentListIndex>(), yylhs.location,
                  cxt.create(DeclareArgumentWithDefaultNode { false }, yylhs.location,
                             yystack_[3].value.as<NamedIdentifierIndex>(), yystack_[1].value.as<AnyExprIndex>()));
          }
#line 3290 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 119: // argument_declarations.pipelist: argument_declarations.pipelist comma.opt name EQUALSSIGN
                    // OPENPAREN expr.seq CLOSEPAREN
#line 660 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<DeclareArgumentListIndex>() = cxt.graph.append(
                  yystack_[6].value.as<DeclareArgumentListIndex>(), yylhs.location,
                  cxt.create(DeclareArgumentWithDefaultNode { false }, yylhs.location,
                             yystack_[4].value.as<NamedIdentifierIndex>(), yystack_[1].value.as<AnyExprIndex>()));
          }
#line 3296 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 120: // argument_declarations: ARG SEMICOLON
#line 664 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<DeclareArgumentListIndex>() = cxt.create(DeclareArgumentList {}, yylhs.location);
          }
#line 3302 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 121: // argument_declarations: ARG argument_declarations.list comma.opt SEMICOLON
#line 665 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<DeclareArgumentListIndex>() = yystack_[2].value.as<DeclareArgumentListIndex>();
          }
#line 3308 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 122: // argument_declarations: ARG argument_declarations.list ELLIPSIS name SEMICOLON
#line 667 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<DeclareArgumentListIndex>() =
                  cxt.graph.append(yystack_[3].value.as<DeclareArgumentListIndex>(), yylhs.location,
                                   cxt.create(DeclareArgumentVariadicNode {}, yystack_[1].location,
                                              yystack_[1].value.as<NamedIdentifierIndex>()));
          }
#line 3314 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 123: // argument_declarations: ARG argument_declarations.list ELLIPSIS name COMMA name SEMICOLON
#line 669 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<DeclareArgumentListIndex>() =
                  cxt.graph.append(yystack_[5].value.as<DeclareArgumentListIndex>(), yylhs.location,
                                   cxt.create(DeclareArgumentVariadicNode {}, yystack_[3].location,
                                              yystack_[3].value.as<NamedIdentifierIndex>()),
                                   cxt.create(DeclareArgumentVariadicNode {}, yystack_[1].location,
                                              yystack_[1].value.as<NamedIdentifierIndex>()));
          }
#line 3320 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 124: // argument_declarations: ARG ELLIPSIS name SEMICOLON
#line 671 "langutils/sc_parser/src/sc_grammar.y"
          {
              auto arg_list = cxt.create(DeclareArgumentList {}, yylhs.location);
              yylhs.value.as<DeclareArgumentListIndex>() =
                  cxt.graph.append(arg_list, yylhs.location,
                                   cxt.create(DeclareArgumentVariadicNode {}, yystack_[1].location,
                                              yystack_[1].value.as<NamedIdentifierIndex>()));
          }
#line 3329 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 125: // argument_declarations: ARG ELLIPSIS name COMMA name SEMICOLON
#line 676 "langutils/sc_parser/src/sc_grammar.y"
          {
              auto arg_list = cxt.create(DeclareArgumentList {}, yylhs.location);
              yylhs.value.as<DeclareArgumentListIndex>() =
                  cxt.graph.append(arg_list, yylhs.location,
                                   cxt.create(DeclareArgumentVariadicNode {}, yystack_[3].location,
                                              yystack_[3].value.as<NamedIdentifierIndex>()),
                                   cxt.create(DeclareArgumentVariadicNode {}, yystack_[1].location,
                                              yystack_[1].value.as<NamedIdentifierIndex>()));
          }
#line 3342 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 126: // argument_declarations: PIPE PIPE
#line 685 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<DeclareArgumentListIndex>() = cxt.create(DeclareArgumentList {}, yylhs.location);
          }
#line 3348 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 127: // argument_declarations: PIPE argument_declarations.pipelist comma.opt PIPE
#line 687 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<DeclareArgumentListIndex>() = yystack_[2].value.as<DeclareArgumentListIndex>();
          }
#line 3354 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 128: // argument_declarations: PIPE argument_declarations.pipelist ELLIPSIS name PIPE
#line 689 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<DeclareArgumentListIndex>() =
                  cxt.graph.append(yystack_[3].value.as<DeclareArgumentListIndex>(), yystack_[3].location,
                                   cxt.create(DeclareArgumentVariadicNode {}, yystack_[1].location,
                                              yystack_[1].value.as<NamedIdentifierIndex>()));
          }
#line 3360 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 129: // argument_declarations: PIPE argument_declarations.pipelist ELLIPSIS name COMMA name PIPE
#line 691 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<DeclareArgumentListIndex>() =
                  cxt.graph.append(yystack_[5].value.as<DeclareArgumentListIndex>(), yylhs.location,
                                   cxt.create(DeclareArgumentVariadicNode {}, yystack_[3].location,
                                              yystack_[3].value.as<NamedIdentifierIndex>()),
                                   cxt.create(DeclareArgumentVariadicNode {}, yystack_[1].location,
                                              yystack_[1].value.as<NamedIdentifierIndex>()));
          }
#line 3366 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 130: // argument_declarations: PIPE ELLIPSIS name PIPE
#line 693 "langutils/sc_parser/src/sc_grammar.y"
          {
              auto arg_list = cxt.create(DeclareArgumentList {}, yylhs.location);
              yylhs.value.as<DeclareArgumentListIndex>() =
                  cxt.graph.append(arg_list, yylhs.location,
                                   cxt.create(DeclareArgumentVariadicNode {}, yystack_[1].location,
                                              yystack_[1].value.as<NamedIdentifierIndex>()));
          }
#line 3375 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 131: // argument_declarations: PIPE ELLIPSIS name COMMA name PIPE
#line 698 "langutils/sc_parser/src/sc_grammar.y"
          {
              auto arg_list = cxt.create(DeclareArgumentList {}, yylhs.location);
              yylhs.value.as<DeclareArgumentListIndex>() =
                  cxt.graph.append(arg_list, yylhs.location,
                                   cxt.create(DeclareArgumentVariadicNode {}, yystack_[3].location,
                                              yystack_[3].value.as<NamedIdentifierIndex>()),
                                   cxt.create(DeclareArgumentVariadicNode {}, yystack_[1].location,
                                              yystack_[1].value.as<NamedIdentifierIndex>()));
          }
#line 3388 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 132: // argument_declarations.opt: %empty
#line 710 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<DeclareArgumentListIndex>() = cxt.create(DeclareArgumentList {}, yylhs.location);
          }
#line 3394 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 133: // argument_declarations.opt: argument_declarations
#line 711 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<DeclareArgumentListIndex>() = yystack_[0].value.as<DeclareArgumentListIndex>();
          }
#line 3400 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 134: // variable_declarations.list.item: name
#line 716 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<DeclareAnyVariableIndex>() = yystack_[0].value.as<NamedIdentifierIndex>();
          }
#line 3406 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 135: // variable_declarations.list.item: name EQUALSSIGN expr
#line 718 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<DeclareAnyVariableIndex>() =
                  cxt.create(DeclareVariableWithDefaultNode {}, yylhs.location,
                             yystack_[2].value.as<NamedIdentifierIndex>(), yystack_[0].value.as<AnyExprIndex>());
          }
#line 3412 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 136: // variable_declarations.list.item: name OPENPAREN expr.seq CLOSEPAREN
#line 720 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<DeclareAnyVariableIndex>() =
                  cxt.create(DeclareVariableWithDefaultNode {}, yylhs.location,
                             yystack_[3].value.as<NamedIdentifierIndex>(), yystack_[1].value.as<AnyExprIndex>());
          }
#line 3418 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 137: // variable_declarations.list: variable_declarations.list.item
#line 729 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<DeclareVariableListIndex>() =
                  cxt.create(DeclareVariableList {}, yylhs.location, yystack_[0].value.as<DeclareAnyVariableIndex>());
          }
#line 3424 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 138: // variable_declarations.list: variable_declarations.list COMMA variable_declarations.list.item
#line 731 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<DeclareVariableListIndex>() = cxt.graph.append(
                  yystack_[2].value.as<DeclareVariableListIndex>(), yystack_[0].value.as<DeclareAnyVariableIndex>());
          }
#line 3430 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 139: // variable_declarations: VAR variable_declarations.list
#line 734 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<DeclareVariableListIndex>() = yystack_[0].value.as<DeclareVariableListIndex>();
          }
#line 3436 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 140: // arguments.entries: KEYBINOP expr.seq
#line 738 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<ArgumentEntryIndex>() =
                  cxt.create(KwArgNode {}, yylhs.location,
                             cxt.create(SymbolNode { SymbolNode::Kind::KeyBinOp }, yystack_[1].location),
                             yystack_[0].value.as<AnyExprIndex>());
          }
#line 3442 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 141: // arguments.entries: MULTIPLY expr.seq
#line 739 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<ArgumentEntryIndex>() =
                  cxt.create(VariadicArgNode {}, yylhs.location, yystack_[0].value.as<AnyExprIndex>());
          }
#line 3448 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 142: // arguments.entries: expr.seq
#line 740 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<ArgumentEntryIndex>() = yystack_[0].value.as<AnyExprIndex>();
          }
#line 3454 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 143: // arguments.no_trailing: arguments.entries
#line 744 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<ArgumentListIndex>() =
                  cxt.create(ArgumentList {}, yylhs.location, yystack_[0].value.as<ArgumentEntryIndex>());
          }
#line 3460 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 144: // arguments.no_trailing: arguments.no_trailing COMMA arguments.entries
#line 745 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<ArgumentListIndex>() =
                  cxt.graph.append(yystack_[2].value.as<ArgumentListIndex>(), yylhs.location,
                                   yystack_[0].value.as<ArgumentEntryIndex>());
          }
#line 3466 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 145: // arguments: arguments.no_trailing comma.opt
#line 748 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<ArgumentListIndex>() = yystack_[1].value.as<ArgumentListIndex>();
          }
#line 3472 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 146: // arguments.paren: OPENPAREN arguments CLOSEPAREN
#line 751 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<ArgumentListIndex>() = yystack_[1].value.as<ArgumentListIndex>();
          }
#line 3478 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 147: // arguments.maybe_paren: %empty
#line 754 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<ArgumentListIndex>() = cxt.create(ArgumentList {}, yylhs.location);
          }
#line 3484 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 148: // arguments.maybe_paren: OPENPAREN CLOSEPAREN
#line 756 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<ArgumentListIndex>() = cxt.create(ArgumentList {}, yylhs.location);
          }
#line 3490 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 149: // arguments.maybe_paren: arguments.paren
#line 757 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<ArgumentListIndex>() = yystack_[0].value.as<ArgumentListIndex>();
          }
#line 3496 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 150: // literal.terminal: symbol
#line 761 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<AnyPossiblyLiteralIndex>() = yystack_[0].value.as<SymbolLitIndex>();
          }
#line 3502 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 151: // literal.terminal: string
#line 762 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<AnyPossiblyLiteralIndex>() = yystack_[0].value.as<StringLitIndex>();
          }
#line 3508 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 152: // literal.terminal: integer
#line 763 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<AnyPossiblyLiteralIndex>() = yystack_[0].value.as<IntLitIndex>();
          }
#line 3514 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 153: // literal.terminal: float
#line 764 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<AnyPossiblyLiteralIndex>() = yystack_[0].value.as<FloatProducingIndex>();
          }
#line 3520 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 154: // literal.terminal: boolean
#line 765 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<AnyPossiblyLiteralIndex>() = yystack_[0].value.as<BooleanLitIndex>();
          }
#line 3526 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 155: // literal.terminal: nil
#line 766 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<AnyPossiblyLiteralIndex>() = yystack_[0].value.as<NilLitIndex>();
          }
#line 3532 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 156: // literal.terminal: ascii
#line 767 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<AnyPossiblyLiteralIndex>() = yystack_[0].value.as<ASCIIIndex>();
          }
#line 3538 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 157: // literal.terminal: block
#line 768 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<AnyPossiblyLiteralIndex>() = yystack_[0].value.as<BlockIndex>();
          }
#line 3544 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 158: // literal.array.contents: %empty
#line 773 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<ArrayIndex>() = cxt.create(ArrayNode {}, yylhs.location);
          }
#line 3550 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 159: // literal.array.contents: expr.seq
#line 775 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<ArrayIndex>() =
                  cxt.create(ArrayNode {}, yylhs.location, yystack_[0].value.as<AnyExprIndex>());
          }
#line 3556 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 160: // literal.array.contents: expr.seq COLON expr.seq
#line 777 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<ArrayIndex>() =
                  cxt.create(ArrayNode {}, yylhs.location, yystack_[2].value.as<AnyExprIndex>(),
                             yystack_[0].value.as<AnyExprIndex>());
          }
#line 3562 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 161: // literal.array.contents: KEYBINOP expr.seq
#line 779 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<ArrayIndex>() =
                  cxt.create(ArrayNode {}, yylhs.location,
                             cxt.create(SymbolNode { SymbolNode::Kind::KeyBinOp }, yystack_[1].location),
                             yystack_[0].value.as<AnyExprIndex>());
          }
#line 3568 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 162: // literal.array.contents: literal.array.contents COMMA expr.seq
#line 781 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<ArrayIndex>() = cxt.graph.append(yystack_[2].value.as<ArrayIndex>(), yylhs.location,
                                                              yystack_[0].value.as<AnyExprIndex>());
          }
#line 3574 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 163: // literal.array.contents: literal.array.contents COMMA expr.seq COLON expr.seq
#line 783 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<ArrayIndex>() =
                  cxt.graph.append(yystack_[4].value.as<ArrayIndex>(), yylhs.location,
                                   yystack_[2].value.as<AnyExprIndex>(), yystack_[0].value.as<AnyExprIndex>());
          }
#line 3580 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 164: // literal.array.contents: literal.array.contents COMMA KEYBINOP expr.seq
#line 785 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<ArrayIndex>() =
                  cxt.graph.append(yystack_[3].value.as<ArrayIndex>(), yylhs.location,
                                   cxt.create(SymbolNode { SymbolNode::Kind::KeyBinOp }, yystack_[1].location),
                                   yystack_[0].value.as<AnyExprIndex>());
          }
#line 3586 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 165: // literal.dictionary.entry: expr COLON expr.seq
#line 793 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<DictionaryEntryIndex>() =
                  cxt.create(DictionaryEntryNode {}, yylhs.location, yystack_[2].value.as<AnyExprIndex>(),
                             yystack_[0].value.as<AnyExprIndex>());
          }
#line 3592 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 166: // literal.dictionary.entry: KEYBINOP expr.seq
#line 795 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<DictionaryEntryIndex>() =
                  cxt.create(DictionaryEntryNode {}, yylhs.location,
                             cxt.create(SymbolNode { SymbolNode::Kind::KeyBinOp }, yystack_[1].location),
                             yystack_[0].value.as<AnyExprIndex>());
          }
#line 3598 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 167: // literal.dictionary.entries: %empty
#line 800 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<DictionaryIndex>() = cxt.create(DictionaryNode {}, yylhs.location);
          }
#line 3604 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 168: // literal.dictionary.entries: literal.dictionary.entry
#line 802 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<DictionaryIndex>() =
                  cxt.create(DictionaryNode {}, yylhs.location, yystack_[0].value.as<DictionaryEntryIndex>());
          }
#line 3610 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 169: // literal.dictionary.entries: literal.dictionary.entries COMMA literal.dictionary.entry
#line 804 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<DictionaryIndex>() =
                  cxt.graph.append(yystack_[2].value.as<DictionaryIndex>(), yylhs.location,
                                   yystack_[0].value.as<DictionaryEntryIndex>());
          }
#line 3616 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 170: // literal.dictionary: OPENPAREN literal.dictionary.entries comma.opt CLOSEPAREN
#line 809 "langutils/sc_parser/src/sc_grammar.y"
          {
              cxt.graph.location(*yystack_[2].value.as<DictionaryIndex>()) = yylhs.location;
              yylhs.value.as<DictionaryIndex>() = yystack_[2].value.as<DictionaryIndex>();
          }
#line 3622 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 171: // literal.array: OPENSQUARE literal.array.contents comma.opt CLOSESQUARE
#line 814 "langutils/sc_parser/src/sc_grammar.y"
          {
              cxt.graph.location(*yystack_[2].value.as<ArrayIndex>()) = yylhs.location;
              yylhs.value.as<ArrayIndex>() = yystack_[2].value.as<ArrayIndex>();
          }
#line 3628 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 172: // literal.array: HASH OPENSQUARE literal.array.contents comma.opt CLOSESQUARE
#line 816 "langutils/sc_parser/src/sc_grammar.y"
          {
              cxt.graph.payload(yystack_[2].value.as<ArrayIndex>()).is_immutable = true;
              cxt.graph.location(*yystack_[2].value.as<ArrayIndex>()) = yylhs.location;
              yylhs.value.as<ArrayIndex>() = yystack_[2].value.as<ArrayIndex>();
          }
#line 3638 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 173: // literal: literal.terminal
#line 824 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<AnyPossiblyLiteralIndex>() = yystack_[0].value.as<AnyPossiblyLiteralIndex>();
          }
#line 3644 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 174: // literal: literal.array
#line 825 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<AnyPossiblyLiteralIndex>() = yystack_[0].value.as<ArrayIndex>();
          }
#line 3650 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 175: // literal: literal.dictionary
#line 826 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<AnyPossiblyLiteralIndex>() = yystack_[0].value.as<DictionaryIndex>();
          }
#line 3656 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 176: // name: NAME
#line 829 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<NamedIdentifierIndex>() = cxt.create(NamedIdentifier {}, yylhs.location);
          }
#line 3662 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 177: // binary_op.raw: BINOP
#line 838 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<SelectorIndex>() = cxt.create(SelectorNode { false }, yylhs.location);
          }
#line 3668 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 178: // binary_op.raw: READWRITEVAR
#line 839 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<SelectorIndex>() = cxt.create(SelectorNode { false }, yylhs.location);
          }
#line 3674 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 179: // binary_op.raw: LESSTHAN
#line 840 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<SelectorIndex>() = cxt.create(SelectorNode { false }, yylhs.location);
          }
#line 3680 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 180: // binary_op.raw: GREATERTHAN
#line 841 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<SelectorIndex>() = cxt.create(SelectorNode { false }, yylhs.location);
          }
#line 3686 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 181: // binary_op.raw: MINUS
#line 842 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<SelectorIndex>() = cxt.create(SelectorNode { false }, yylhs.location);
          }
#line 3692 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 182: // binary_op.raw: MULTIPLY
#line 843 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<SelectorIndex>() = cxt.create(SelectorNode { false }, yylhs.location);
          }
#line 3698 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 183: // binary_op.raw: ADD
#line 844 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<SelectorIndex>() = cxt.create(SelectorNode { false }, yylhs.location);
          }
#line 3704 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 184: // binary_op.raw: PIPE
#line 845 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<SelectorIndex>() = cxt.create(SelectorNode { false }, yylhs.location);
          }
#line 3710 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 185: // binary_op.no_adverb: binary_op.raw
#line 849 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<SelectorIndex>() = yystack_[0].value.as<SelectorIndex>();
          }
#line 3716 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 186: // binary_op.no_adverb: KEYBINOP
#line 850 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<SelectorIndex>() = cxt.create(SelectorNode { true }, yylhs.location);
          }
#line 3722 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 187: // binary_op: binary_op.no_adverb adverb
#line 855 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<SelectorMaybeAdverbIndex>() =
                  cxt.create(SelectorWAdverb {}, yylhs.location, yystack_[1].value.as<SelectorIndex>(),
                             yystack_[0].value.as<AdverbIndex>());
          }
#line 3728 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 188: // binary_op: binary_op.no_adverb
#line 857 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<SelectorMaybeAdverbIndex>() = yystack_[0].value.as<SelectorIndex>();
          }
#line 3734 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 193: // ascii: ASCII
#line 863 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<ASCIIIndex>() = cxt.create(ASCIINode {}, yylhs.location);
          }
#line 3740 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 194: // nil: NIL
#line 865 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<NilLitIndex>() = cxt.create(NilNode {}, yylhs.location);
          }
#line 3746 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 195: // boolean: TRUE
#line 868 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<BooleanLitIndex>() = cxt.create(BooleanNode { true }, yylhs.location);
          }
#line 3752 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 196: // boolean: FALSE
#line 869 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<BooleanLitIndex>() = cxt.create(BooleanNode { false }, yylhs.location);
          }
#line 3758 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 197: // symbol: SYMBOL_QUOTE
#line 873 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<SymbolLitIndex>() = cxt.create(SymbolNode { SymbolNode::Kind::Quote }, yylhs.location);
          }
#line 3764 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 198: // symbol: SYMBOL_SLASH
#line 874 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<SymbolLitIndex>() = cxt.create(SymbolNode { SymbolNode::Kind::Slash }, yylhs.location);
          }
#line 3770 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 199: // string: STRINGLINE
#line 878 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<StringLitIndex>() =
                  cxt.create(StringLineList {}, yylhs.location, cxt.create(StringLineNode {}, yylhs.location));
          }
#line 3776 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 200: // string: string STRINGLINE
#line 879 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<StringLitIndex>() = cxt.graph.append(yystack_[1].value.as<StringLitIndex>(),
                                                                  cxt.create(StringLineNode {}, yystack_[0].location));
          }
#line 3782 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 201: // integer: INTEGER
#line 883 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<IntLitIndex>() = cxt.create(IntNode {}, yylhs.location);
          }
#line 3788 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 202: // integer: INTEGER_RADIX
#line 884 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<IntLitIndex>() = cxt.create(IntNode { IntNode::Kind::Radix }, yylhs.location);
          }
#line 3794 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 203: // integer: HEXADECIMAL
#line 885 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<IntLitIndex>() = cxt.create(IntNode { IntNode::Kind::Hexadecimal }, yylhs.location);
          }
#line 3800 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 204: // integer: MINUS integer
#line 887 "langutils/sc_parser/src/sc_grammar.y"
          {
              // Reaches into the previous integer and changes its sign.
              cxt.graph.payload(yystack_[0].value.as<IntLitIndex>()).sign = IntNode::Sign::Negative;
              yylhs.value.as<IntLitIndex>() = yystack_[0].value.as<IntLitIndex>();
          }
#line 3810 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 205: // float.raw_unsigned: FLOAT
#line 895 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<FloatLitIndex>() = cxt.create(FloatNode {}, yylhs.location);
          }
#line 3816 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 206: // float.raw_unsigned: FLOAT_RADIX
#line 896 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<FloatLitIndex>() = cxt.create(FloatNode { FloatNode::Kind::Radix }, yylhs.location);
          }
#line 3822 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 207: // float.raw_unsigned: FLOAT_EXPONENT
#line 897 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<FloatLitIndex>() = cxt.create(FloatNode { FloatNode::Kind::Exponent }, yylhs.location);
          }
#line 3828 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 208: // float.raw_unsigned: FLOAT_INF
#line 898 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<FloatLitIndex>() = cxt.create(FloatNode { FloatNode::Kind::Inf }, yylhs.location);
          }
#line 3834 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 209: // float.raw: float.raw_unsigned
#line 903 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<FloatLitIndex>() = yystack_[0].value.as<FloatLitIndex>();
          }
#line 3840 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 210: // float.raw: MINUS float.raw_unsigned
#line 905 "langutils/sc_parser/src/sc_grammar.y"
          {
              cxt.graph.payload(yystack_[0].value.as<FloatLitIndex>()).sign = FloatNode::Sign::Negative;
              yylhs.value.as<FloatLitIndex>() = yystack_[0].value.as<FloatLitIndex>();
          }
#line 3846 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 211: // accidental.unsigned: ACCIDENTAL_STEPS
#line 910 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<AccidentalIndex>() =
                  cxt.create(AccidentalNode { AccidentalNode::Kind::Steps }, yylhs.location);
          }
#line 3852 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 212: // accidental.unsigned: ACCIDENTAL_CENTS
#line 912 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<AccidentalIndex>() =
                  cxt.create(AccidentalNode { AccidentalNode::Kind::Cents }, yylhs.location);
          }
#line 3858 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 213: // accidental: accidental.unsigned
#line 917 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<AccidentalIndex>() = yystack_[0].value.as<AccidentalIndex>();
          }
#line 3864 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 214: // accidental: MINUS accidental.unsigned
#line 919 "langutils/sc_parser/src/sc_grammar.y"
          {
              cxt.graph.payload(yystack_[0].value.as<AccidentalIndex>()).sign = AccidentalNode::Sign::Negative;
              yylhs.value.as<AccidentalIndex>() = yystack_[0].value.as<AccidentalIndex>();
          }
#line 3870 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 215: // float: float.raw
#line 923 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<FloatProducingIndex>() = yystack_[0].value.as<FloatLitIndex>();
          }
#line 3876 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 216: // float: accidental
#line 924 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<FloatProducingIndex>() = yystack_[0].value.as<AccidentalIndex>();
          }
#line 3882 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 217: // float: float.raw PI
#line 925 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<FloatProducingIndex>() =
                  cxt.create(PiNode {}, yylhs.location, yystack_[1].value.as<FloatLitIndex>());
          }
#line 3888 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 218: // float: integer PI
#line 926 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<FloatProducingIndex>() =
                  cxt.create(PiNode {}, yylhs.location, yystack_[1].value.as<IntLitIndex>());
          }
#line 3894 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 219: // float: PI
#line 927 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<FloatProducingIndex>() =
                  cxt.create(PiNode {}, yylhs.location, cxt.create(Missing {}, yylhs.location));
          }
#line 3900 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 220: // float: MINUS PI
#line 928 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<FloatProducingIndex>() =
                  cxt.create(PiNode { PiNode::Sign::Negative }, yylhs.location, cxt.create(Missing {}, yylhs.location));
          }
#line 3906 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 221: // accessor: %empty
#line 932 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<ReadWriteAccessor>() = ReadWriteAccessor::Private;
          }
#line 3912 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 222: // accessor: LESSTHAN
#line 933 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<ReadWriteAccessor>() = ReadWriteAccessor::ExternallyReadable;
          }
#line 3918 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 223: // accessor: READWRITEVAR
#line 934 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<ReadWriteAccessor>() =
                  combine<ReadWriteAccessor::ExternallyReadable, ReadWriteAccessor::ExternallyWriteable>();
          }
#line 3924 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 224: // accessor: GREATERTHAN
#line 935 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<ReadWriteAccessor>() = ReadWriteAccessor::ExternallyWriteable;
          }
#line 3930 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;


#line 3934 "langutils/sc_parser/src/sc_grammar_parser.cpp"

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
    if (!yyerrstatus_)
      {
        ++yynerrs_;
        context yyctx (*this, yyla);
        report_syntax_error (yyctx);
      }


    yyerror_range[1].location = yyla.location;
    if (yyerrstatus_ == 3)
      {
        /* If just tried and failed to reuse lookahead token after an
           error, discard it.  */

        // Return failure if at end of input.
        if (yyla.kind () == symbol_kind::S_YYEOF)
          YYABORT;
        else if (!yyla.empty ())
          {
            yy_destroy_ ("Error: discarding", yyla);
            yyla.clear ();
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
    yypop_ (yylen);
    yylen = 0;
    YY_STACK_PRINT ();
    goto yyerrlab1;


  /*-------------------------------------------------------------.
  | yyerrlab1 -- common code for both syntax error and YYERROR.  |
  `-------------------------------------------------------------*/
  yyerrlab1:
    yyerrstatus_ = 3;   // Each real token shifted decrements this.
    // Pop stack until we find a state that shifts the error token.
    for (;;)
      {
        yyn = yypact_[+yystack_[0].state];
        if (!yy_pact_value_is_default_ (yyn))
          {
            yyn += symbol_kind::S_YYerror;
            if (0 <= yyn && yyn <= yylast_
                && yycheck_[yyn] == symbol_kind::S_YYerror)
              {
                yyn = yytable_[yyn];
                if (0 < yyn)
                  break;
              }
          }

        // Pop the current state because it cannot handle the error token.
        if (yystack_.size () == 1)
          YYABORT;

        yyerror_range[1].location = yystack_[0].location;
        yy_destroy_ ("Error: popping", yystack_[0]);
        yypop_ ();
        YY_STACK_PRINT ();
      }
    {
      stack_symbol_type error_token;

      yyerror_range[2].location = yyla.location;
      YYLLOC_DEFAULT (error_token.location, yyerror_range, 2);

      // Shift the error token.
      error_token.state = state_type (yyn);
      yypush_ ("Shifting", YY_MOVE (error_token));
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
    if (!yyla.empty ())
      yy_destroy_ ("Cleanup: discarding lookahead", yyla);

    /* Do not reclaim the symbols of the rule whose action triggered
       this YYABORT or YYACCEPT.  */
    yypop_ (yylen);
    YY_STACK_PRINT ();
    while (1 < yystack_.size ())
      {
        yy_destroy_ ("Cleanup: popping", yystack_[0]);
        yypop_ ();
      }

    return yyresult;
  }
#if YY_EXCEPTIONS
    catch (...)
      {
        YYCDEBUG << "Exception caught: cleaning lookahead and stack\n";
        // Do not try to display the values of the reclaimed symbols,
        // as their printers might throw an exception.
        if (!yyla.empty ())
          yy_destroy_ (YY_NULLPTR, yyla);

        while (1 < yystack_.size ())
          {
            yy_destroy_ (YY_NULLPTR, yystack_[0]);
            yypop_ ();
          }
        throw;
      }
#endif // YY_EXCEPTIONS
  }

  void
  parser::error (const syntax_error& yyexc)
  {
    error (yyexc.location, yyexc.what ());
  }

  const char *
  parser::symbol_name (symbol_kind_type yysymbol)
  {
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
  parser::context::context (const parser& yyparser, const symbol_type& yyla)
    : yyparser_ (yyparser)
    , yyla_ (yyla)
  {}

  int
  parser::context::expected_tokens (symbol_kind_type yyarg[], int yyargn) const
  {
    // Actual number of expected tokens
    int yycount = 0;

    const int yyn = yypact_[+yyparser_.yystack_[0].state];
    if (!yy_pact_value_is_default_ (yyn))
      {
        /* Start YYX at -YYN if negative to avoid negative indexes in
           YYCHECK.  In other words, skip the first -YYN actions for
           this state because they are default actions.  */
        const int yyxbegin = yyn < 0 ? -yyn : 0;
        // Stay within bounds of both yycheck and yytname.
        const int yychecklim = yylast_ - yyn + 1;
        const int yyxend = yychecklim < YYNTOKENS ? yychecklim : YYNTOKENS;
        for (int yyx = yyxbegin; yyx < yyxend; ++yyx)
          if (yycheck_[yyx + yyn] == yyx && yyx != symbol_kind::S_YYerror
              && !yy_table_value_is_error_ (yytable_[yyx + yyn]))
            {
              if (!yyarg)
                ++yycount;
              else if (yycount == yyargn)
                return 0;
              else
                yyarg[yycount++] = YY_CAST (symbol_kind_type, yyx);
            }
      }

    if (yyarg && yycount == 0 && 0 < yyargn)
      yyarg[0] = symbol_kind::S_YYEMPTY;
    return yycount;
  }


  const short parser::yypact_ninf_ = -200;

  const signed char parser::yytable_ninf_ = -1;

  const short parser::yypact_[] = {
      73,   37,   377,  20,   40,   32,   25,   -200, -200, -200, 79,   60,   -200, -200, 1248, 450,  98,   79,   -200,
      -200, -200, -200, -200, -200, -200, -200, -200, -200, -200, -200, -200, -200, 158,  -200, -200, -200, -200, -200,
      1705, 1392, -18,  -200, -200, 104,  1696, -200, -200, -200, -200, 54,   -200, -200, -200, -200, 97,   80,   -200,
      94,   -200, -200, -200, 132,  -200, -200, -200, 1440, 1440, -200, -200, 135,  118,  161,  504,  1392, 1696, 157,
      138,  167,  1392, 79,   19,   -200, 1392, 1705, -200, -200, -200, -200, 53,   -200, 171,  -200, 1683, 609,  -200,
      -200, 180,  -200, 185,  1248, 149,  1248, 657,  -200, 15,   -200, 160,  -200, -200, -200, -200, 53,   -200, 709,
      757,  -200, -200, -200, 33,   140,  1392, 806,  1392, 15,   -200, -200, -200, 252,  450,  -200, 1696, -200, -200,
      -200, 12,   -200, 1392, -200, 1392, 1296, 192,  1696, -200, 189,  44,   -200, 79,   109,  57,   -200, 79,   -200,
      146,  1484, 1151, 1392, 193,  1392, -200, 171,  1696, 1344, 194,  108,  167,  1392, 24,   15,   1392, 1392, -200,
      -200, 195,  196,  -200, -200, 171,  156,  201,  855,  904,  15,   87,   129,  -200, 154,  15,   204,  1696, 391,
      210,  -200, -200, 252,  212,  -200, -200, 953,  131,  131,  131,  -200, 208,  252,  1696, -200, 1392, 172,  -200,
      79,   1392, 1392, 16,   79,   79,   211,  1392, 1558, 10,   -200, 79,   34,   1344, 1595, -200, 1392, -200, 1696,
      -200, -200, 215,  1392, 1683, -200, -200, 1001, 15,   218,  1696, -200, 1296, -200, 15,   -200, -200, 1199, -200,
      15,   222,  1392, 181,  182,  221,  15,   224,  -200, 1050, 1392, -200, 15,   1392, -200, -200, 15,   3,    -200,
      -18,  -200, -200, -200, 162,  -200, -200, -200, -200, 225,  79,   225,  225,  12,   -200, 233,  -200, 1392, -200,
      230,  1696, -200, 79,   92,   103,  -200, 232,  1344, -200, 79,   -200, 11,   -200, 1521, 1683, 234,  1344, -200,
      1696, -200, 15,   235,  -200, -200, -200, -200, 1696, 1392, 1392, 197,  -200, -200, -200, 1696, -200, 239,  1392,
      -200, 560,  -200, 1151, 131,  -200, -200, -200, -200, -200, 240,  1392, 1632, -200, 79,   -200, 242,  186,  79,
      -200, 1344, 1669, -200, -200, 244,  15,   15,   1696, 1696, 1392, -200, 1696, -200, 248,  171,  -200, -200, 245,
      1344, -200, 249,  -200, -200, 205,  253,  1344, -200, -200, -200, 1696, -200, 1102, 260,  -200, 257,  -200, -200,
      -200, 261,  -200, 171,  -200, -200, -200, 266,  -200
  };

  const unsigned char parser::yydefact_[] = {
      4,   22,  0,   0,   0,   0,   0,   15,  17,  18,  0,   20,  9,   47,  158, 167, 0,   0,   176, 201, 202, 203, 205,
      206, 207, 208, 211, 212, 197, 198, 199, 193, 83,  194, 195, 196, 219, 48,  0,   0,   132, 157, 77,  82,  8,   173,
      175, 174, 75,  76,  156, 155, 154, 150, 151, 152, 209, 215, 213, 216, 153, 0,   1,   2,   14,  0,   0,   3,   16,
      0,   0,   0,   167, 0,   96,  189, 159, 191, 0,   0,   0,   177, 186, 181, 179, 180, 182, 183, 184, 178, 189, 55,
      60,  0,   61,  168, 191, 185, 0,   158, 79,  158, 0,   53,  72,  220, 0,   204, 210, 214, 86,  0,   133, 0,   0,
      186, 181, 184, 0,   188, 0,   0,   0,   67,  200, 218, 217, 45,  167, 13,  5,   12,  23,  21,  33,  161, 190, 98,
      0,   192, 0,   59,  137, 139, 134, 120, 0,   191, 102, 166, 0,   126, 191, 111, 190, 0,   0,   0,   11,  189, 60,
      192, 0,   0,   191, 0,   0,   51,  0,   0,   142, 143, 191, 0,   54,  50,  189, 81,  0,   0,   0,   51,  147, 0,
      187, 87,  0,   0,   88,  182, 0,   41,  43,  46,  0,   35,  36,  0,   221, 221, 221, 31,  189, 45,  97,  160, 0,
      162, 171, 0,   0,   0,   0,   192, 0,   0,   0,   0,   0,   192, 0,   0,   167, 0,   110, 0,   56,  57,  78,  165,
      0,   0,   0,   169, 170, 0,   64,  0,   89,  71,  0,   74,  52,  140, 141, 192, 145, 51,  0,   0,   80,  85,  0,
      51,  0,   69,  0,   0,   149, 51,  0,   99,  100, 66,  51,  42,  132, 44,  24,  7,   0,   222, 224, 223, 26,  29,
      0,   28,  30,  190, 34,  0,   164, 0,   138, 0,   135, 124, 0,   106, 0,   121, 0,   167, 103, 0,   130, 0,   127,
      116, 96,  0,   167, 112, 58,  10,  0,   0,   172, 144, 73,  49,  93,  0,   0,   84,  70,  146, 148, 90,  68,  0,
      0,   65,  0,   6,   0,   221, 25,  32,  19,  163, 136, 0,   0,   0,   122, 0,   104, 0,   0,   0,   128, 167, 0,
      115, 113, 0,   63,  51,  92,  95,  0,   101, 91,  38,  189, 189, 27,  125, 0,   167, 107, 0,   105, 131, 0,   0,
      167, 117, 114, 62,  94,  190, 0,   0,   108, 0,   123, 129, 118, 0,   40,  189, 37,  109, 119, 0,   39
  };

  const short parser::yypgoto_[] = { -200, -200, 183,  -200, -200, 267,  -200, -200, -200, -200, -55,  -9,   1,    -200,
                                     -200, -200, 88,   83,   -200, 76,   -200, 111,  -175, -31,  -89,  -151, -200, -200,
                                     -2,   -200, 0,    -200, -200, -200, -3,   18,   -199, -200, -200, 36,   -200, -94,
                                     100,  -200, -200, 28,   125,  -200, -200, -200, -148, 43,   -106, 2,    -200, -88,
                                     -66,  -200, -200, -200, -200, -200, -27,  -29,  -200, -22,  -200, -200, -200 };

  const short parser::yydefgoto_[] = { 0,   4,   129, 5,   6,   7,   8,   71,  11,  9,   274, 275, 201, 202,
                                       203, 190, 191, 192, 193, 194, 40,  41,  241, 242, 90,  91,  42,  43,
                                       74,  75,  170, 184, 147, 152, 112, 113, 142, 143, 94,  171, 172, 254,
                                       181, 259, 45,  77,  95,  96,  46,  47,  48,  49,  97,  119, 120, 137,
                                       140, 50,  51,  52,  53,  54,  55,  56,  57,  58,  59,  60,  276 };

  const short parser::yytable_[] = {
      44,  104, 156, 226, 159, 224, 255, 13,  173, 108, 284, 107, 93,  92,  76,  80,  109, 98,  123, 13,  178, 196, 295,
      341, 176, 67,  287, 187, 288, 145, 162, 239, 63,  64,  18,  65,  240, 110, 111, 179, 62,  180, 66,  10,  198, 37,
      199, 200, 18,  18,  61,  322, 210, 69,  108, 1,   107, 37,  13,  146, 100, 109, 121, 130, 130, 216, 296, 342, 18,
      294, 92,  230, 310, 135, 98,  303, 141, 328, 316, 107, 3,   215, 149, 196, 320, 252, 221, 196, 248, 323, 298, 160,
      211, 150, 18,  256, 37,  196, 237, 76,  334, 76,  122, 1,   99,  217, 246, 70,  270, 151, 114, 160, 13,  336, 280,
      337, 235, 2,   185, 125, 188, 213, 144, 148, 124, 197, 92,  164, 3,   166, 98,  153, 236, 126, 204, 257, 127, 260,
      205, 207, 335, 307, 132, 103, 18,  19,  20,  21,  133, 214, 37,  345, 160, 227, 153, 263, 262, 229, 219, 232, 103,
      182, 13,  238, 101, 134, 102, 136, 243, 244, 195, 325, 326, 155, 371, 226, 19,  20,  21,  139, 106, 154, 155, 271,
      272, 138, 220, 362, 273, 212, 277, 278, 161, 218, 163, 160, 369, 165, 183, 208, 37,  209, 228, 234, 249, 247, 282,
      245, 250, 286, 285, 106, 118, 264, 266, 174, 292, 268, 279, 283, 300, 291, 301, 304, 305, 308, 261, 311, 315, 313,
      314, 149, 195, 317, 174, 357, 195, 327, 330, 332, 207, 338, 365, 346, 349, 352, 195, 312, 353, 131, 359, 364, 144,
      370, 376, 319, 289, 290, 373, 378, 321, 379, 380, 297, 299, 384, 385, 18,  374, 375, 386, 388, 358, 68,  103, 348,
      267, 265, 103, 281, 329, 309, 258, 331, 324, 383, 233, 0,   0,   0,   0,   300, 103, 339, 0,   387, 0,   103, 0,
      0,   300, 81,  347, 116, 84,  85,  189, 87,  117, 89,  0,   350, 351, 0,   0,   0,   0,   0,   0,   144, 354, 0,
      160, 0,   160, 0,   0,   0,   0,   0,   0,   333, 0,   0,   360, 0,   0,   0,   340, 0,   0,   300, 0,   367, 0,
      0,   0,   174, 0,   0,   372, 0,   0,   174, 0,   0,   0,   0,   103, 300, 0,   377, 0,   0,   103, 0,   300, 0,
      381, 0,   103, 0,   160, 0,   174, 103, 0,   0,   12,  0,   363, 13,  0,   14,  366, 15,  0,   0,   0,   0,   16,
      17,  18,  19,  20,  21,  22,  23,  24,  25,  26,  27,  28,  29,  30,  31,  18,  32,  0,   0,   0,   0,   0,   33,
      34,  35,  36,  103, 0,   37,  0,   0,   0,   0,   0,   0,   0,   0,   38,  0,   0,   0,   0,   0,   0,   0,   39,
      0,   0,   0,   81,  0,   116, 84,  85,  86,  87,  117, 89,  0,   0,   0,   0,   0,   13,  0,   14,  0,   72,  174,
      103, 78,  0,   16,  17,  18,  19,  20,  21,  22,  23,  24,  25,  26,  27,  28,  29,  30,  31,  0,   32,  0,   79,
      80,  0,   0,   33,  34,  35,  36,  0,   0,   37,  0,   0,   0,   0,   0,   0,   81,  82,  83,  84,  85,  86,  87,
      88,  89,  13,  39,  14,  0,   72,  0,   0,   78,  0,   16,  17,  18,  19,  20,  21,  22,  23,  24,  25,  26,  27,
      28,  29,  30,  31,  0,   32,  0,   79,  0,   0,   0,   33,  34,  35,  36,  0,   0,   37,  0,   0,   0,   0,   0,
      0,   81,  82,  83,  84,  85,  86,  87,  117, 89,  0,   39,  13,  355, 14,  0,   72,  0,   0,   78,  0,   16,  17,
      18,  19,  20,  21,  22,  23,  24,  25,  26,  27,  28,  29,  30,  31,  356, 32,  0,   79,  0,   0,   0,   33,  34,
      35,  36,  0,   0,   37,  0,   0,   0,   0,   0,   0,   0,   0,   38,  0,   13,  0,   14,  0,   72,  158, 39,  78,
      0,   16,  17,  18,  19,  20,  21,  22,  23,  24,  25,  26,  27,  28,  29,  30,  31,  0,   32,  0,   79,  0,   0,
      0,   33,  34,  35,  36,  0,   0,   37,  0,   0,   0,   0,   0,   0,   0,   0,   38,  13,  0,   14,  0,   72,  167,
      0,   39,  0,   16,  17,  18,  19,  20,  21,  22,  23,  24,  25,  26,  27,  28,  29,  30,  31,  0,   32,  0,   0,
      0,   0,   0,   33,  34,  35,  36,  0,   0,   37,  0,   0,   0,   0,   0,   0,   0,   168, 38,  0,   0,   169, 0,
      13,  175, 14,  39,  72,  0,   0,   78,  0,   16,  17,  18,  19,  20,  21,  22,  23,  24,  25,  26,  27,  28,  29,
      30,  31,  0,   32,  0,   79,  0,   0,   0,   33,  34,  35,  36,  0,   0,   37,  0,   0,   0,   0,   0,   0,   0,
      0,   38,  13,  0,   14,  177, 72,  0,   0,   39,  0,   16,  17,  18,  19,  20,  21,  22,  23,  24,  25,  26,  27,
      28,  29,  30,  31,  0,   32,  0,   0,   0,   0,   0,   33,  34,  35,  36,  0,   0,   37,  0,   0,   0,   0,   0,
      0,   0,   168, 38,  0,   13,  169, 14,  0,   72,  186, 39,  0,   0,   16,  17,  18,  19,  20,  21,  22,  23,  24,
      25,  26,  27,  28,  29,  30,  31,  0,   32,  0,   0,   0,   0,   0,   33,  34,  35,  36,  0,   0,   37,  0,   0,
      0,   0,   0,   0,   0,   168, 38,  0,   13,  169, 14,  251, 72,  0,   39,  0,   0,   16,  17,  18,  19,  20,  21,
      22,  23,  24,  25,  26,  27,  28,  29,  30,  31,  0,   32,  0,   0,   0,   0,   0,   33,  34,  35,  36,  0,   0,
      37,  0,   0,   0,   0,   0,   0,   0,   168, 38,  0,   13,  169, 14,  0,   72,  253, 39,  0,   0,   16,  17,  18,
      19,  20,  21,  22,  23,  24,  25,  26,  27,  28,  29,  30,  31,  0,   32,  0,   0,   0,   0,   0,   33,  34,  35,
      36,  0,   0,   37,  0,   0,   0,   0,   0,   0,   0,   168, 38,  0,   13,  169, 14,  0,   72,  269, 39,  78,  0,
      16,  17,  18,  19,  20,  21,  22,  23,  24,  25,  26,  27,  28,  29,  30,  31,  0,   32,  0,   79,  0,   0,   0,
      33,  34,  35,  36,  0,   0,   37,  0,   0,   0,   0,   0,   0,   0,   0,   38,  13,  0,   14,  0,   72,  306, 0,
      39,  0,   16,  17,  18,  19,  20,  21,  22,  23,  24,  25,  26,  27,  28,  29,  30,  31,  0,   32,  0,   0,   0,
      0,   0,   33,  34,  35,  36,  0,   0,   37,  0,   0,   0,   0,   0,   0,   0,   168, 38,  0,   13,  169, 14,  0,
      72,  318, 39,  0,   0,   16,  17,  18,  19,  20,  21,  22,  23,  24,  25,  26,  27,  28,  29,  30,  31,  0,   32,
      0,   0,   0,   0,   0,   33,  34,  35,  36,  0,   0,   37,  0,   0,   0,   0,   0,   0,   0,   168, 38,  0,   0,
      169, 0,   13,  382, 14,  39,  72,  0,   0,   78,  0,   16,  17,  18,  19,  20,  21,  22,  23,  24,  25,  26,  27,
      28,  29,  30,  31,  0,   32,  0,   79,  0,   0,   0,   33,  34,  35,  36,  0,   0,   37,  0,   0,   0,   0,   0,
      0,   0,   0,   38,  0,   13,  0,   14,  0,   72,  0,   39,  225, 0,   16,  17,  18,  19,  20,  21,  22,  23,  24,
      25,  26,  27,  28,  29,  30,  31,  0,   32,  0,   79,  0,   0,   0,   33,  34,  35,  36,  0,   0,   37,  0,   0,
      0,   0,   0,   0,   0,   0,   38,  13,  0,   14,  0,   72,  0,   0,   39,  0,   16,  17,  18,  19,  20,  21,  22,
      23,  24,  25,  26,  27,  28,  29,  30,  31,  0,   32,  0,   0,   0,   0,   0,   33,  34,  35,  36,  0,   0,   37,
      0,   0,   0,   0,   0,   0,   0,   168, 38,  0,   13,  169, 14,  0,   72,  0,   39,  0,   0,   16,  17,  18,  19,
      20,  21,  22,  23,  24,  25,  26,  27,  28,  29,  30,  31,  0,   32,  0,   0,   0,   0,   0,   33,  34,  35,  36,
      0,   0,   37,  0,   0,   0,   0,   0,   0,   0,   73,  38,  13,  0,   14,  0,   72,  0,   0,   39,  0,   16,  17,
      18,  19,  20,  21,  22,  23,  24,  25,  26,  27,  28,  29,  30,  31,  0,   32,  0,   0,   0,   0,   0,   33,  34,
      35,  36,  0,   0,   37,  0,   0,   0,   0,   0,   0,   0,   206, 38,  13,  0,   14,  0,   72,  0,   0,   39,  0,
      16,  17,  18,  19,  20,  21,  22,  23,  24,  25,  26,  27,  28,  29,  30,  31,  0,   32,  0,   0,   0,   0,   0,
      33,  34,  35,  36,  0,   0,   37,  0,   0,   0,   0,   0,   0,   0,   231, 38,  13,  0,   14,  0,   72,  0,   0,
      39,  0,   16,  17,  18,  19,  20,  21,  22,  23,  24,  25,  26,  27,  28,  29,  30,  31,  0,   32,  0,   0,   0,
      0,   0,   33,  34,  35,  36,  0,   0,   37,  0,   0,   0,   0,   0,   0,   0,   0,   38,  13,  0,   14,  0,   128,
      0,   0,   39,  0,   16,  17,  18,  19,  20,  21,  22,  23,  24,  25,  26,  27,  28,  29,  30,  31,  0,   32,  0,
      0,   0,   0,   0,   33,  34,  35,  36,  0,   0,   37,  0,   0,   0,   0,   0,   13,  0,   14,  38,  222, 0,   0,
      0,   0,   16,  0,   39,  19,  20,  21,  22,  23,  24,  25,  26,  27,  28,  29,  30,  31,  0,   0,   0,   0,   0,
      0,   0,   33,  34,  35,  36,  0,   13,  37,  14,  0,   343, 0,   0,   223, 0,   16,  38,  0,   19,  20,  21,  22,
      23,  24,  25,  26,  27,  28,  29,  30,  31,  0,   0,   0,   0,   0,   0,   0,   33,  34,  35,  36,  0,   13,  37,
      14,  0,   293, 0,   0,   344, 0,   16,  38,  0,   19,  20,  21,  22,  23,  24,  25,  26,  27,  28,  29,  30,  31,
      0,   0,   0,   0,   0,   0,   0,   33,  34,  35,  36,  0,   13,  37,  14,  0,   302, 0,   0,   0,   0,   16,  38,
      0,   19,  20,  21,  22,  23,  24,  25,  26,  27,  28,  29,  30,  31,  0,   0,   0,   0,   0,   0,   0,   33,  34,
      35,  36,  0,   13,  37,  14,  0,   361, 0,   0,   0,   0,   16,  38,  0,   19,  20,  21,  22,  23,  24,  25,  26,
      27,  28,  29,  30,  31,  0,   0,   0,   0,   0,   0,   0,   33,  34,  35,  36,  0,   13,  37,  14,  0,   368, 0,
      0,   0,   0,   16,  38,  0,   19,  20,  21,  22,  23,  24,  25,  26,  27,  28,  29,  30,  31,  0,   0,   0,   0,
      0,   0,   0,   33,  34,  35,  36,  0,   0,   37,  0,   0,   0,   0,   0,   0,   0,   0,   38,  19,  20,  21,  22,
      23,  24,  25,  26,  27,  157, 0,   81,  115, 116, 84,  85,  86,  87,  117, 89,  118, 0,   0,   105, 81,  115, 116,
      84,  85,  86,  87,  117, 89,  118, 0,   106
  };

  const short parser::yycheck_[] = {
      2,   32,  90,  154, 93,  153, 181, 4,   102, 38,  209, 38,  15,  15,  14,  33,  38,  15,  49,  4,   114, 127, 12,
      12,  113, 0,   10,  121, 12,  10,  96,  7,   0,   1,   15,  3,   12,  39,  56,  6,   0,   8,   10,  6,   32,  42,
      34,  35,  15,  15,  30,  48,  8,   10,  83,  30,  83,  42,  4,   40,  17,  83,  8,   65,  66,  8,   56,  56,  15,
      217, 72,  159, 247, 73,  72,  223, 78,  276, 253, 106, 55,  147, 82,  189, 259, 179, 152, 193, 176, 264, 56,  93,
      48,  40,  15,  8,   42,  203, 164, 99,  8,   101, 48,  30,  6,   48,  172, 47,  197, 56,  6,   113, 4,   10,  202,
      12,  8,   44,  120, 39,  122, 12,  79,  80,  27,  128, 128, 99,  55,  101, 128, 88,  163, 39,  136, 48,  4,   8,
      138, 139, 48,  235, 7,   32,  15,  16,  17,  18,  30,  40,  42,  299, 154, 155, 111, 186, 183, 157, 12,  161, 49,
      118, 4,   165, 6,   4,   8,   10,  168, 169, 127, 9,   10,  11,  349, 326, 16,  17,  18,  12,  51,  10,  11,  52,
      53,  47,  40,  335, 57,  146, 199, 200, 12,  150, 9,   197, 344, 48,  58,  7,   42,  12,  9,   9,   48,  9,   206,
      12,  7,   211, 210, 51,  58,  9,   4,   104, 216, 5,   10,  47,  222, 10,  222, 225, 9,   7,   183, 5,   7,   48,
      48,  231, 189, 9,   123, 324, 193, 12,  5,   9,   240, 9,   56,  9,   9,   48,  203, 249, 9,   66,  10,  9,   209,
      9,   9,   257, 213, 214, 10,  10,  260, 56,  9,   220, 221, 5,   9,   15,  356, 357, 9,   5,   327, 6,   163, 306,
      193, 189, 167, 203, 279, 245, 182, 283, 266, 374, 161, -1,  -1,  -1,  -1,  293, 181, 293, -1,  383, -1,  186, -1,
      -1,  302, 49,  302, 51,  52,  53,  54,  55,  56,  57,  -1,  313, 314, -1,  -1,  -1,  -1,  -1,  -1,  276, 322, -1,
      324, -1,  326, -1,  -1,  -1,  -1,  -1,  -1,  288, -1,  -1,  334, -1,  -1,  -1,  295, -1,  -1,  343, -1,  343, -1,
      -1,  -1,  236, -1,  -1,  352, -1,  -1,  242, -1,  -1,  -1,  -1,  247, 361, -1,  361, -1,  -1,  253, -1,  368, -1,
      368, -1,  259, -1,  374, -1,  263, 264, -1,  -1,  1,   -1,  337, 4,   -1,  6,   341, 8,   -1,  -1,  -1,  -1,  13,
      14,  15,  16,  17,  18,  19,  20,  21,  22,  23,  24,  25,  26,  27,  28,  15,  30,  -1,  -1,  -1,  -1,  -1,  36,
      37,  38,  39,  306, -1,  42,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  51,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  59,
      -1,  -1,  -1,  49,  -1,  51,  52,  53,  54,  55,  56,  57,  -1,  -1,  -1,  -1,  -1,  4,   -1,  6,   -1,  8,   348,
      349, 11,  -1,  13,  14,  15,  16,  17,  18,  19,  20,  21,  22,  23,  24,  25,  26,  27,  28,  -1,  30,  -1,  32,
      33,  -1,  -1,  36,  37,  38,  39,  -1,  -1,  42,  -1,  -1,  -1,  -1,  -1,  -1,  49,  50,  51,  52,  53,  54,  55,
      56,  57,  4,   59,  6,   -1,  8,   -1,  -1,  11,  -1,  13,  14,  15,  16,  17,  18,  19,  20,  21,  22,  23,  24,
      25,  26,  27,  28,  -1,  30,  -1,  32,  -1,  -1,  -1,  36,  37,  38,  39,  -1,  -1,  42,  -1,  -1,  -1,  -1,  -1,
      -1,  49,  50,  51,  52,  53,  54,  55,  56,  57,  -1,  59,  4,   5,   6,   -1,  8,   -1,  -1,  11,  -1,  13,  14,
      15,  16,  17,  18,  19,  20,  21,  22,  23,  24,  25,  26,  27,  28,  29,  30,  -1,  32,  -1,  -1,  -1,  36,  37,
      38,  39,  -1,  -1,  42,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  51,  -1,  4,   -1,  6,   -1,  8,   9,   59,  11,
      -1,  13,  14,  15,  16,  17,  18,  19,  20,  21,  22,  23,  24,  25,  26,  27,  28,  -1,  30,  -1,  32,  -1,  -1,
      -1,  36,  37,  38,  39,  -1,  -1,  42,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  51,  4,   -1,  6,   -1,  8,   9,
      -1,  59,  -1,  13,  14,  15,  16,  17,  18,  19,  20,  21,  22,  23,  24,  25,  26,  27,  28,  -1,  30,  -1,  -1,
      -1,  -1,  -1,  36,  37,  38,  39,  -1,  -1,  42,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  50,  51,  -1,  -1,  54,  -1,
      4,   5,   6,   59,  8,   -1,  -1,  11,  -1,  13,  14,  15,  16,  17,  18,  19,  20,  21,  22,  23,  24,  25,  26,
      27,  28,  -1,  30,  -1,  32,  -1,  -1,  -1,  36,  37,  38,  39,  -1,  -1,  42,  -1,  -1,  -1,  -1,  -1,  -1,  -1,
      -1,  51,  4,   -1,  6,   7,   8,   -1,  -1,  59,  -1,  13,  14,  15,  16,  17,  18,  19,  20,  21,  22,  23,  24,
      25,  26,  27,  28,  -1,  30,  -1,  -1,  -1,  -1,  -1,  36,  37,  38,  39,  -1,  -1,  42,  -1,  -1,  -1,  -1,  -1,
      -1,  -1,  50,  51,  -1,  4,   54,  6,   -1,  8,   9,   59,  -1,  -1,  13,  14,  15,  16,  17,  18,  19,  20,  21,
      22,  23,  24,  25,  26,  27,  28,  -1,  30,  -1,  -1,  -1,  -1,  -1,  36,  37,  38,  39,  -1,  -1,  42,  -1,  -1,
      -1,  -1,  -1,  -1,  -1,  50,  51,  -1,  4,   54,  6,   7,   8,   -1,  59,  -1,  -1,  13,  14,  15,  16,  17,  18,
      19,  20,  21,  22,  23,  24,  25,  26,  27,  28,  -1,  30,  -1,  -1,  -1,  -1,  -1,  36,  37,  38,  39,  -1,  -1,
      42,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  50,  51,  -1,  4,   54,  6,   -1,  8,   9,   59,  -1,  -1,  13,  14,  15,
      16,  17,  18,  19,  20,  21,  22,  23,  24,  25,  26,  27,  28,  -1,  30,  -1,  -1,  -1,  -1,  -1,  36,  37,  38,
      39,  -1,  -1,  42,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  50,  51,  -1,  4,   54,  6,   -1,  8,   9,   59,  11,  -1,
      13,  14,  15,  16,  17,  18,  19,  20,  21,  22,  23,  24,  25,  26,  27,  28,  -1,  30,  -1,  32,  -1,  -1,  -1,
      36,  37,  38,  39,  -1,  -1,  42,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  51,  4,   -1,  6,   -1,  8,   9,   -1,
      59,  -1,  13,  14,  15,  16,  17,  18,  19,  20,  21,  22,  23,  24,  25,  26,  27,  28,  -1,  30,  -1,  -1,  -1,
      -1,  -1,  36,  37,  38,  39,  -1,  -1,  42,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  50,  51,  -1,  4,   54,  6,   -1,
      8,   9,   59,  -1,  -1,  13,  14,  15,  16,  17,  18,  19,  20,  21,  22,  23,  24,  25,  26,  27,  28,  -1,  30,
      -1,  -1,  -1,  -1,  -1,  36,  37,  38,  39,  -1,  -1,  42,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  50,  51,  -1,  -1,
      54,  -1,  4,   5,   6,   59,  8,   -1,  -1,  11,  -1,  13,  14,  15,  16,  17,  18,  19,  20,  21,  22,  23,  24,
      25,  26,  27,  28,  -1,  30,  -1,  32,  -1,  -1,  -1,  36,  37,  38,  39,  -1,  -1,  42,  -1,  -1,  -1,  -1,  -1,
      -1,  -1,  -1,  51,  -1,  4,   -1,  6,   -1,  8,   -1,  59,  11,  -1,  13,  14,  15,  16,  17,  18,  19,  20,  21,
      22,  23,  24,  25,  26,  27,  28,  -1,  30,  -1,  32,  -1,  -1,  -1,  36,  37,  38,  39,  -1,  -1,  42,  -1,  -1,
      -1,  -1,  -1,  -1,  -1,  -1,  51,  4,   -1,  6,   -1,  8,   -1,  -1,  59,  -1,  13,  14,  15,  16,  17,  18,  19,
      20,  21,  22,  23,  24,  25,  26,  27,  28,  -1,  30,  -1,  -1,  -1,  -1,  -1,  36,  37,  38,  39,  -1,  -1,  42,
      -1,  -1,  -1,  -1,  -1,  -1,  -1,  50,  51,  -1,  4,   54,  6,   -1,  8,   -1,  59,  -1,  -1,  13,  14,  15,  16,
      17,  18,  19,  20,  21,  22,  23,  24,  25,  26,  27,  28,  -1,  30,  -1,  -1,  -1,  -1,  -1,  36,  37,  38,  39,
      -1,  -1,  42,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  50,  51,  4,   -1,  6,   -1,  8,   -1,  -1,  59,  -1,  13,  14,
      15,  16,  17,  18,  19,  20,  21,  22,  23,  24,  25,  26,  27,  28,  -1,  30,  -1,  -1,  -1,  -1,  -1,  36,  37,
      38,  39,  -1,  -1,  42,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  50,  51,  4,   -1,  6,   -1,  8,   -1,  -1,  59,  -1,
      13,  14,  15,  16,  17,  18,  19,  20,  21,  22,  23,  24,  25,  26,  27,  28,  -1,  30,  -1,  -1,  -1,  -1,  -1,
      36,  37,  38,  39,  -1,  -1,  42,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  50,  51,  4,   -1,  6,   -1,  8,   -1,  -1,
      59,  -1,  13,  14,  15,  16,  17,  18,  19,  20,  21,  22,  23,  24,  25,  26,  27,  28,  -1,  30,  -1,  -1,  -1,
      -1,  -1,  36,  37,  38,  39,  -1,  -1,  42,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  51,  4,   -1,  6,   -1,  8,
      -1,  -1,  59,  -1,  13,  14,  15,  16,  17,  18,  19,  20,  21,  22,  23,  24,  25,  26,  27,  28,  -1,  30,  -1,
      -1,  -1,  -1,  -1,  36,  37,  38,  39,  -1,  -1,  42,  -1,  -1,  -1,  -1,  -1,  4,   -1,  6,   51,  8,   -1,  -1,
      -1,  -1,  13,  -1,  59,  16,  17,  18,  19,  20,  21,  22,  23,  24,  25,  26,  27,  28,  -1,  -1,  -1,  -1,  -1,
      -1,  -1,  36,  37,  38,  39,  -1,  4,   42,  6,   -1,  8,   -1,  -1,  48,  -1,  13,  51,  -1,  16,  17,  18,  19,
      20,  21,  22,  23,  24,  25,  26,  27,  28,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  36,  37,  38,  39,  -1,  4,   42,
      6,   -1,  8,   -1,  -1,  48,  -1,  13,  51,  -1,  16,  17,  18,  19,  20,  21,  22,  23,  24,  25,  26,  27,  28,
      -1,  -1,  -1,  -1,  -1,  -1,  -1,  36,  37,  38,  39,  -1,  4,   42,  6,   -1,  8,   -1,  -1,  -1,  -1,  13,  51,
      -1,  16,  17,  18,  19,  20,  21,  22,  23,  24,  25,  26,  27,  28,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  36,  37,
      38,  39,  -1,  4,   42,  6,   -1,  8,   -1,  -1,  -1,  -1,  13,  51,  -1,  16,  17,  18,  19,  20,  21,  22,  23,
      24,  25,  26,  27,  28,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  36,  37,  38,  39,  -1,  4,   42,  6,   -1,  8,   -1,
      -1,  -1,  -1,  13,  51,  -1,  16,  17,  18,  19,  20,  21,  22,  23,  24,  25,  26,  27,  28,  -1,  -1,  -1,  -1,
      -1,  -1,  -1,  36,  37,  38,  39,  -1,  -1,  42,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  51,  16,  17,  18,  19,
      20,  21,  22,  23,  24,  47,  -1,  49,  50,  51,  52,  53,  54,  55,  56,  57,  58,  -1,  -1,  39,  49,  50,  51,
      52,  53,  54,  55,  56,  57,  58,  -1,  51
  };

  const unsigned char parser::yystos_[] = {
      0,   30,  44,  55,  62,  64,  65,  66,  67,  70,  6,   69,  1,   4,   6,   8,   13,  14,  15,  16,  17,  18,  19,
      20,  21,  22,  23,  24,  25,  26,  27,  28,  30,  36,  37,  38,  39,  42,  51,  59,  81,  82,  87,  88,  89,  105,
      109, 110, 111, 112, 118, 119, 120, 121, 122, 123, 124, 125, 126, 127, 128, 30,  0,   0,   1,   3,   10,  0,   66,
      112, 47,  68,  8,   50,  89,  90,  91,  106, 11,  32,  33,  49,  50,  51,  52,  53,  54,  55,  56,  57,  85,  86,
      89,  95,  99,  107, 108, 113, 114, 6,   112, 6,   8,   82,  84,  39,  51,  123, 124, 126, 89,  56,  95,  96,  6,
      50,  51,  56,  58,  114, 115, 8,   48,  84,  27,  39,  39,  4,   8,   63,  89,  63,  7,   30,  4,   91,  10,  116,
      47,  12,  117, 89,  97,  98,  112, 10,  40,  93,  112, 91,  40,  56,  94,  112, 10,  11,  116, 47,  9,   85,  89,
      12,  117, 9,   106, 48,  106, 9,   50,  54,  91,  100, 101, 102, 82,  5,   85,  7,   102, 6,   8,   103, 112, 58,
      92,  89,  9,   102, 89,  54,  76,  77,  78,  79,  80,  112, 113, 95,  32,  34,  35,  73,  74,  75,  89,  91,  50,
      91,  7,   12,  8,   48,  112, 12,  40,  117, 8,   48,  112, 12,  40,  117, 8,   48,  111, 11,  86,  89,  9,   91,
      116, 50,  89,  107, 9,   8,   84,  117, 89,  7,   12,  83,  84,  91,  91,  12,  117, 9,   116, 48,  7,   7,   102,
      9,   102, 83,  8,   48,  103, 104, 8,   112, 123, 84,  9,   77,  4,   78,  5,   9,   85,  52,  53,  57,  71,  72,
      129, 72,  72,  10,  116, 80,  91,  47,  97,  91,  89,  10,  12,  112, 112, 10,  91,  8,   111, 12,  56,  112, 56,
      112, 89,  91,  8,   111, 89,  9,   9,   102, 7,   100, 83,  5,   89,  48,  48,  7,   83,  9,   9,   89,  83,  91,
      48,  83,  96,  9,   10,  12,  97,  73,  5,   91,  9,   112, 8,   48,  10,  12,  9,   91,  112, 12,  56,  8,   48,
      111, 9,   91,  84,  9,   89,  89,  48,  9,   89,  5,   29,  85,  71,  10,  91,  8,   111, 112, 9,   56,  112, 91,
      8,   111, 9,   83,  89,  10,  116, 116, 9,   91,  10,  56,  9,   91,  5,   85,  5,   9,   9,   116, 5
  };

  const unsigned char parser::yyr1_[] = {
      0,   61,  62,  62,  62,  63,  63,  63,  64,  64,  64,  64,  64,  64,  64,  65,  65,  66,  66,  67,  68,  68,  69,
      69,  70,  71,  72,  72,  73,  73,  73,  74,  74,  75,  75,  76,  76,  77,  77,  77,  77,  78,  78,  79,  79,  80,
      80,  81,  81,  82,  82,  83,  83,  84,  84,  85,  85,  85,  85,  85,  86,  86,  87,  87,  87,  87,  87,  87,  87,
      87,  87,  87,  87,  87,  87,  88,  88,  88,  88,  88,  88,  88,  89,  89,  89,  89,  89,  89,  89,  89,  89,  89,
      89,  89,  89,  89,  90,  90,  91,  92,  92,  92,  93,  93,  93,  93,  93,  93,  93,  93,  94,  94,  94,  94,  94,
      94,  94,  94,  94,  94,  95,  95,  95,  95,  95,  95,  95,  95,  95,  95,  95,  95,  96,  96,  97,  97,  97,  98,
      98,  99,  100, 100, 100, 101, 101, 102, 103, 104, 104, 104, 105, 105, 105, 105, 105, 105, 105, 105, 106, 106, 106,
      106, 106, 106, 106, 107, 107, 108, 108, 108, 109, 110, 110, 111, 111, 111, 112, 113, 113, 113, 113, 113, 113, 113,
      113, 114, 114, 115, 115, 116, 116, 117, 117, 118, 119, 120, 120, 121, 121, 122, 122, 123, 123, 123, 123, 124, 124,
      124, 124, 125, 125, 126, 126, 127, 127, 128, 128, 128, 128, 128, 128, 129, 129, 129, 129
  };

  const signed char parser::yyr2_[] = {
      0, 2, 2, 2, 0, 1, 4, 3, 2, 2, 6, 4, 3, 3, 2, 1, 2, 1, 1, 7, 0, 2, 0, 3, 5, 2, 1, 3, 2, 2, 2, 1, 3, 0, 2, 1, 1, 6,
      4, 8, 6, 1, 2, 1, 2, 0, 1, 1, 1, 5, 3, 0, 1, 1, 2, 1, 3, 3, 4, 2, 1, 1, 7, 6, 4, 5, 4, 2, 5, 4, 5, 4, 2, 5, 4, 1,
      1, 1, 4, 2, 4, 3, 1, 1, 5, 4, 2, 3, 3, 4, 5, 6, 6, 5, 7, 6, 1, 3, 2, 2, 2, 4, 1, 3, 4, 5, 3, 5, 6, 7, 2, 1, 3, 4,
      5, 4, 3, 5, 6, 7, 2, 4, 5, 7, 4, 6, 2, 4, 5, 7, 4, 6, 0, 1, 1, 3, 4, 1, 3, 2, 2, 2, 1, 1, 3, 2, 3, 0, 2, 1, 1, 1,
      1, 1, 1, 1, 1, 1, 0, 1, 3, 2, 3, 5, 4, 3, 2, 0, 1, 3, 4, 4, 5, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 1, 0,
      1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 2, 1, 1, 1, 2, 1, 1, 1, 1, 1, 2, 1, 1, 1, 2, 1, 1, 2, 2, 1, 2, 0, 1, 1, 1
  };


#if YYDEBUG
  const short parser::yyrline_[] = {
      0,   172, 172, 173, 174, 179, 181, 192, 206, 210, 216, 228, 242, 245, 248, 274, 276, 281, 282, 286, 291, 292, 296,
      297, 301, 306, 311, 313, 318, 320, 322, 327, 329, 334, 335, 339, 340, 344, 346, 348, 350, 355, 356, 361, 362, 366,
      367, 370, 370, 373, 375, 380, 381, 385, 386, 390, 391, 392, 397, 402, 410, 411, 415, 424, 427, 431, 440, 443, 446,
      457, 467, 482, 485, 491, 502, 512, 514, 516, 517, 522, 527, 534, 544, 548, 550, 555, 561, 563, 569, 572, 575, 578,
      586, 588, 591, 594, 599, 600, 612, 615, 616, 617, 622, 624, 626, 628, 630, 632, 634, 636, 641, 643, 645, 647, 649,
      651, 653, 655, 657, 659, 664, 665, 666, 668, 670, 675, 684, 686, 688, 690, 692, 697, 710, 711, 715, 717, 719, 728,
      730, 734, 738, 739, 740, 744, 745, 748, 751, 754, 755, 757, 761, 762, 763, 764, 765, 766, 767, 768, 772, 774, 776,
      778, 780, 782, 784, 792, 794, 799, 801, 803, 808, 813, 815, 824, 825, 826, 829, 838, 839, 840, 841, 842, 843, 844,
      845, 849, 850, 854, 856, 860, 860, 861, 861, 863, 865, 868, 869, 873, 874, 878, 879, 883, 884, 885, 886, 895, 896,
      897, 898, 902, 904, 909, 911, 916, 918, 923, 924, 925, 926, 927, 928, 932, 933, 934, 935
  };

  void
  parser::yy_stack_print_ () const
  {
    *yycdebug_ << "Stack now";
    for (stack_type::const_iterator
           i = yystack_.begin (),
           i_end = yystack_.end ();
         i != i_end; ++i)
      *yycdebug_ << ' ' << int (i->state);
    *yycdebug_ << '\n';
  }

  void
  parser::yy_reduce_print_ (int yyrule) const
  {
    int yylno = yyrline_[yyrule];
    int yynrhs = yyr2_[yyrule];
    // Print the symbols being reduced, and their result.
    *yycdebug_ << "Reducing stack by rule " << yyrule - 1
               << " (line " << yylno << "):\n";
    // The symbols being reduced.
    for (int yyi = 0; yyi < yynrhs; yyi++)
      YY_SYMBOL_PRINT ("   $" << yyi + 1 << " =",
                       yystack_[(yynrhs) - (yyi + 1)]);
  }
#endif // YYDEBUG

  parser::symbol_kind_type
  parser::yytranslate_ (int t) YY_NOEXCEPT
  {
    // YYTRANSLATE[TOKEN-NUM] -- Symbol number corresponding to
    // TOKEN-NUM as returned by yylex.
    static
    const signed char
    translate_table[] =
    {
       0,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     1,     2,     3,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    19,    20,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    47,    48,    49,    50,    51,    52,    53,    54,
      55,    56,    57,    58,    59,    60
    };
    // Last valid token kind.
    const int code_max = 315;

    if (t <= 0)
      return symbol_kind::S_YYEOF;
    else if (t <= code_max)
      return static_cast <symbol_kind_type> (translate_table[t]);
    else
      return symbol_kind::S_YYUNDEF;
  }

#line 7 "langutils/sc_parser/src/sc_grammar.y"
} } } // sc::ast::parser
#line 4891 "langutils/sc_parser/src/sc_grammar_parser.cpp"

#line 938 "langutils/sc_parser/src/sc_grammar.y"
