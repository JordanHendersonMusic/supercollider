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

//static int yylex(sc::ast::parser::parser::value_type* v, sc::lex::SourceCodeRange* loc, sc::ast::ParserContext& cxt);

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


#line 68 "langutils/sc_parser/src/sc_grammar_parser.cpp"




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
#line 168 "langutils/sc_parser/src/sc_grammar_parser.cpp"

  /// Build a parser object.
  parser::parser (ParserContext& cxt_yyarg)
#if YYDEBUG
    : yydebug_ (false),
      yycdebug_ (&std::cerr),
#else
    :
#endif
      cxt (cxt_yyarg)
  {}

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
        value.copy< AnyExprIndex > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_105_literal_terminal: // literal.terminal
      case symbol_kind::S_literal: // literal
        value.copy< AnyPossiblyLiteralIndex > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_method: // method
        value.copy< AnyMethodIndex > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_100_arguments_entries: // arguments.entries
        value.copy< ArgumentEntryIndex > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_101_arguments_no_trailing: // arguments.no_trailing
      case symbol_kind::S_arguments: // arguments
      case symbol_kind::S_103_arguments_paren: // arguments.paren
      case symbol_kind::S_104_arguments_maybe_paren: // arguments.maybe_paren
        value.copy< ArgumentListIndex > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_106_literal_array_contents: // literal.array.contents
      case symbol_kind::S_110_literal_array: // literal.array
        value.copy< ArrayIndex > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_85_block_contents: // block.contents
        value.copy< BlockContentsListIndex > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_block: // block
        value.copy< BlockIndex > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_86_block_contents_item: // block.contents.item
        value.copy< BlockItemIndex > (YY_MOVE (that.value));
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
        value.copy< DeclareAnyVariableIndex > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_93_argument_declarations_list: // argument_declarations.list
      case symbol_kind::S_94_argument_declarations_pipelist: // argument_declarations.pipelist
      case symbol_kind::S_argument_declarations: // argument_declarations
      case symbol_kind::S_96_argument_declarations_opt: // argument_declarations.opt
        value.copy< DeclareArgumentListIndex > (YY_MOVE (that.value));
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
        value.copy< DictionaryEntryIndex > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_108_literal_dictionary_entries: // literal.dictionary.entries
      case symbol_kind::S_109_literal_dictionary: // literal.dictionary
        value.copy< DictionaryIndex > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_124_float_raw_unsigned: // float.raw_unsigned
      case symbol_kind::S_125_float_raw: // float.raw
        value.copy< FloatLitIndex > (YY_MOVE (that.value));
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
        value.copy< SelectorIndex > (YY_MOVE (that.value));
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
        value.move< AnyExprIndex > (YY_MOVE (s.value));
        break;

      case symbol_kind::S_105_literal_terminal: // literal.terminal
      case symbol_kind::S_literal: // literal
        value.move< AnyPossiblyLiteralIndex > (YY_MOVE (s.value));
        break;

      case symbol_kind::S_method: // method
        value.move< AnyMethodIndex > (YY_MOVE (s.value));
        break;

      case symbol_kind::S_100_arguments_entries: // arguments.entries
        value.move< ArgumentEntryIndex > (YY_MOVE (s.value));
        break;

      case symbol_kind::S_101_arguments_no_trailing: // arguments.no_trailing
      case symbol_kind::S_arguments: // arguments
      case symbol_kind::S_103_arguments_paren: // arguments.paren
      case symbol_kind::S_104_arguments_maybe_paren: // arguments.maybe_paren
        value.move< ArgumentListIndex > (YY_MOVE (s.value));
        break;

      case symbol_kind::S_106_literal_array_contents: // literal.array.contents
      case symbol_kind::S_110_literal_array: // literal.array
        value.move< ArrayIndex > (YY_MOVE (s.value));
        break;

      case symbol_kind::S_85_block_contents: // block.contents
        value.move< BlockContentsListIndex > (YY_MOVE (s.value));
        break;

      case symbol_kind::S_block: // block
        value.move< BlockIndex > (YY_MOVE (s.value));
        break;

      case symbol_kind::S_86_block_contents_item: // block.contents.item
        value.move< BlockItemIndex > (YY_MOVE (s.value));
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
        value.move< DeclareAnyVariableIndex > (YY_MOVE (s.value));
        break;

      case symbol_kind::S_93_argument_declarations_list: // argument_declarations.list
      case symbol_kind::S_94_argument_declarations_pipelist: // argument_declarations.pipelist
      case symbol_kind::S_argument_declarations: // argument_declarations
      case symbol_kind::S_96_argument_declarations_opt: // argument_declarations.opt
        value.move< DeclareArgumentListIndex > (YY_MOVE (s.value));
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
        value.move< DictionaryEntryIndex > (YY_MOVE (s.value));
        break;

      case symbol_kind::S_108_literal_dictionary_entries: // literal.dictionary.entries
      case symbol_kind::S_109_literal_dictionary: // literal.dictionary
        value.move< DictionaryIndex > (YY_MOVE (s.value));
        break;

      case symbol_kind::S_124_float_raw_unsigned: // float.raw_unsigned
      case symbol_kind::S_125_float_raw: // float.raw
        value.move< FloatLitIndex > (YY_MOVE (s.value));
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
        value.move< SelectorIndex > (YY_MOVE (s.value));
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
        value.YY_MOVE_OR_COPY< AnyExprIndex > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_105_literal_terminal: // literal.terminal
      case symbol_kind::S_literal: // literal
        value.YY_MOVE_OR_COPY< AnyPossiblyLiteralIndex > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_method: // method
        value.YY_MOVE_OR_COPY< AnyMethodIndex > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_100_arguments_entries: // arguments.entries
        value.YY_MOVE_OR_COPY< ArgumentEntryIndex > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_101_arguments_no_trailing: // arguments.no_trailing
      case symbol_kind::S_arguments: // arguments
      case symbol_kind::S_103_arguments_paren: // arguments.paren
      case symbol_kind::S_104_arguments_maybe_paren: // arguments.maybe_paren
        value.YY_MOVE_OR_COPY< ArgumentListIndex > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_106_literal_array_contents: // literal.array.contents
      case symbol_kind::S_110_literal_array: // literal.array
        value.YY_MOVE_OR_COPY< ArrayIndex > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_85_block_contents: // block.contents
        value.YY_MOVE_OR_COPY< BlockContentsListIndex > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_block: // block
        value.YY_MOVE_OR_COPY< BlockIndex > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_86_block_contents_item: // block.contents.item
        value.YY_MOVE_OR_COPY< BlockItemIndex > (YY_MOVE (that.value));
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
        value.YY_MOVE_OR_COPY< DeclareAnyVariableIndex > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_93_argument_declarations_list: // argument_declarations.list
      case symbol_kind::S_94_argument_declarations_pipelist: // argument_declarations.pipelist
      case symbol_kind::S_argument_declarations: // argument_declarations
      case symbol_kind::S_96_argument_declarations_opt: // argument_declarations.opt
        value.YY_MOVE_OR_COPY< DeclareArgumentListIndex > (YY_MOVE (that.value));
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
        value.YY_MOVE_OR_COPY< DictionaryEntryIndex > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_108_literal_dictionary_entries: // literal.dictionary.entries
      case symbol_kind::S_109_literal_dictionary: // literal.dictionary
        value.YY_MOVE_OR_COPY< DictionaryIndex > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_124_float_raw_unsigned: // float.raw_unsigned
      case symbol_kind::S_125_float_raw: // float.raw
        value.YY_MOVE_OR_COPY< FloatLitIndex > (YY_MOVE (that.value));
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
        value.YY_MOVE_OR_COPY< SelectorIndex > (YY_MOVE (that.value));
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
        value.move< AnyExprIndex > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_105_literal_terminal: // literal.terminal
      case symbol_kind::S_literal: // literal
        value.move< AnyPossiblyLiteralIndex > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_method: // method
        value.move< AnyMethodIndex > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_100_arguments_entries: // arguments.entries
        value.move< ArgumentEntryIndex > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_101_arguments_no_trailing: // arguments.no_trailing
      case symbol_kind::S_arguments: // arguments
      case symbol_kind::S_103_arguments_paren: // arguments.paren
      case symbol_kind::S_104_arguments_maybe_paren: // arguments.maybe_paren
        value.move< ArgumentListIndex > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_106_literal_array_contents: // literal.array.contents
      case symbol_kind::S_110_literal_array: // literal.array
        value.move< ArrayIndex > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_85_block_contents: // block.contents
        value.move< BlockContentsListIndex > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_block: // block
        value.move< BlockIndex > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_86_block_contents_item: // block.contents.item
        value.move< BlockItemIndex > (YY_MOVE (that.value));
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
        value.move< DeclareAnyVariableIndex > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_93_argument_declarations_list: // argument_declarations.list
      case symbol_kind::S_94_argument_declarations_pipelist: // argument_declarations.pipelist
      case symbol_kind::S_argument_declarations: // argument_declarations
      case symbol_kind::S_96_argument_declarations_opt: // argument_declarations.opt
        value.move< DeclareArgumentListIndex > (YY_MOVE (that.value));
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
        value.move< DictionaryEntryIndex > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_108_literal_dictionary_entries: // literal.dictionary.entries
      case symbol_kind::S_109_literal_dictionary: // literal.dictionary
        value.move< DictionaryIndex > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_124_float_raw_unsigned: // float.raw_unsigned
      case symbol_kind::S_125_float_raw: // float.raw
        value.move< FloatLitIndex > (YY_MOVE (that.value));
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
        value.move< SelectorIndex > (YY_MOVE (that.value));
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
        value.copy< AnyExprIndex > (that.value);
        break;

      case symbol_kind::S_105_literal_terminal: // literal.terminal
      case symbol_kind::S_literal: // literal
        value.copy< AnyLiteralIndex > (that.value);
        break;

      case symbol_kind::S_method: // method
        value.copy< AnyMethodIndex > (that.value);
        break;

      case symbol_kind::S_100_arguments_entries: // arguments.entries
        value.copy< ArgumentEntryIndex > (that.value);
        break;

      case symbol_kind::S_101_arguments_no_trailing: // arguments.no_trailing
      case symbol_kind::S_arguments: // arguments
      case symbol_kind::S_103_arguments_paren: // arguments.paren
      case symbol_kind::S_104_arguments_maybe_paren: // arguments.maybe_paren
        value.copy< ArgumentListIndex > (that.value);
        break;

      case symbol_kind::S_106_literal_array_contents: // literal.array.contents
      case symbol_kind::S_110_literal_array: // literal.array
        value.copy< ArrayIndex > (that.value);
        break;

      case symbol_kind::S_85_block_contents: // block.contents
        value.copy< BlockContentsListIndex > (that.value);
        break;

      case symbol_kind::S_block: // block
        value.copy< BlockIndex > (that.value);
        break;

      case symbol_kind::S_86_block_contents_item: // block.contents.item
        value.copy< BlockItemIndex > (that.value);
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
        value.copy< DeclareAnyVariableIndex > (that.value);
        break;

      case symbol_kind::S_93_argument_declarations_list: // argument_declarations.list
      case symbol_kind::S_94_argument_declarations_pipelist: // argument_declarations.pipelist
      case symbol_kind::S_argument_declarations: // argument_declarations
      case symbol_kind::S_96_argument_declarations_opt: // argument_declarations.opt
        value.copy< DeclareArgumentListIndex > (that.value);
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
        value.copy< DictionaryEntryIndex > (that.value);
        break;

      case symbol_kind::S_108_literal_dictionary_entries: // literal.dictionary.entries
      case symbol_kind::S_109_literal_dictionary: // literal.dictionary
        value.copy< DictionaryIndex > (that.value);
        break;

      case symbol_kind::S_124_float_raw_unsigned: // float.raw_unsigned
      case symbol_kind::S_125_float_raw: // float.raw
        value.copy< FloatLitIndex > (that.value);
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
        value.copy< SelectorIndex > (that.value);
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
        value.move< AnyExprIndex > (that.value);
        break;

      case symbol_kind::S_105_literal_terminal: // literal.terminal
      case symbol_kind::S_literal: // literal
        value.move< AnyLiteralIndex > (that.value);
        break;

      case symbol_kind::S_method: // method
        value.move< AnyMethodIndex > (that.value);
        break;

      case symbol_kind::S_100_arguments_entries: // arguments.entries
        value.move< ArgumentEntryIndex > (that.value);
        break;

      case symbol_kind::S_101_arguments_no_trailing: // arguments.no_trailing
      case symbol_kind::S_arguments: // arguments
      case symbol_kind::S_103_arguments_paren: // arguments.paren
      case symbol_kind::S_104_arguments_maybe_paren: // arguments.maybe_paren
        value.move< ArgumentListIndex > (that.value);
        break;

      case symbol_kind::S_106_literal_array_contents: // literal.array.contents
      case symbol_kind::S_110_literal_array: // literal.array
        value.move< ArrayIndex > (that.value);
        break;

      case symbol_kind::S_85_block_contents: // block.contents
        value.move< BlockContentsListIndex > (that.value);
        break;

      case symbol_kind::S_block: // block
        value.move< BlockIndex > (that.value);
        break;

      case symbol_kind::S_86_block_contents_item: // block.contents.item
        value.move< BlockItemIndex > (that.value);
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
        value.move< DeclareAnyVariableIndex > (that.value);
        break;

      case symbol_kind::S_93_argument_declarations_list: // argument_declarations.list
      case symbol_kind::S_94_argument_declarations_pipelist: // argument_declarations.pipelist
      case symbol_kind::S_argument_declarations: // argument_declarations
      case symbol_kind::S_96_argument_declarations_opt: // argument_declarations.opt
        value.move< DeclareArgumentListIndex > (that.value);
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
        value.move< DictionaryEntryIndex > (that.value);
        break;

      case symbol_kind::S_108_literal_dictionary_entries: // literal.dictionary.entries
      case symbol_kind::S_109_literal_dictionary: // literal.dictionary
        value.move< DictionaryIndex > (that.value);
        break;

      case symbol_kind::S_124_float_raw_unsigned: // float.raw_unsigned
      case symbol_kind::S_125_float_raw: // float.raw
        value.move< FloatLitIndex > (that.value);
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
        value.move< SelectorIndex > (that.value);
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
        yylhs.value.emplace< AnyExprIndex > ();
        break;

      case symbol_kind::S_105_literal_terminal: // literal.terminal
      case symbol_kind::S_literal: // literal
        yylhs.value.emplace< AnyPossiblyLiteralIndex > ();
        break;

      case symbol_kind::S_method: // method
        yylhs.value.emplace< AnyMethodIndex > ();
        break;

      case symbol_kind::S_100_arguments_entries: // arguments.entries
        yylhs.value.emplace< ArgumentEntryIndex > ();
        break;

      case symbol_kind::S_101_arguments_no_trailing: // arguments.no_trailing
      case symbol_kind::S_arguments: // arguments
      case symbol_kind::S_103_arguments_paren: // arguments.paren
      case symbol_kind::S_104_arguments_maybe_paren: // arguments.maybe_paren
        yylhs.value.emplace< ArgumentListIndex > ();
        break;

      case symbol_kind::S_106_literal_array_contents: // literal.array.contents
      case symbol_kind::S_110_literal_array: // literal.array
        yylhs.value.emplace< ArrayIndex > ();
        break;

      case symbol_kind::S_85_block_contents: // block.contents
        yylhs.value.emplace< BlockContentsListIndex > ();
        break;

      case symbol_kind::S_block: // block
        yylhs.value.emplace< BlockIndex > ();
        break;

      case symbol_kind::S_86_block_contents_item: // block.contents.item
        yylhs.value.emplace< BlockItemIndex > ();
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
        yylhs.value.emplace< DeclareAnyVariableIndex > ();
        break;

      case symbol_kind::S_93_argument_declarations_list: // argument_declarations.list
      case symbol_kind::S_94_argument_declarations_pipelist: // argument_declarations.pipelist
      case symbol_kind::S_argument_declarations: // argument_declarations
      case symbol_kind::S_96_argument_declarations_opt: // argument_declarations.opt
        yylhs.value.emplace< DeclareArgumentListIndex > ();
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
        yylhs.value.emplace< DictionaryEntryIndex > ();
        break;

      case symbol_kind::S_108_literal_dictionary_entries: // literal.dictionary.entries
      case symbol_kind::S_109_literal_dictionary: // literal.dictionary
        yylhs.value.emplace< DictionaryIndex > ();
        break;

      case symbol_kind::S_124_float_raw_unsigned: // float.raw_unsigned
      case symbol_kind::S_125_float_raw: // float.raw
        yylhs.value.emplace< FloatLitIndex > ();
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
        yylhs.value.emplace< SelectorIndex > ();
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
#line 174 "langutils/sc_parser/src/sc_grammar.y"
                       { yylhs.value.as < ClassListOrExprListIndex > () = cxt.graph.assign_root(yystack_[1].value.as < RegionListIndex > ()); }
#line 2463 "langutils/sc_parser/src/sc_grammar_parser.cpp"
    break;

  case 3: // go: classOrExtList.list $end
#line 175 "langutils/sc_parser/src/sc_grammar.y"
                                              { yylhs.value.as < ClassListOrExprListIndex > () = cxt.graph.assign_root(yystack_[1].value.as < ClassOrExtensionListIndex > ()); }
#line 2469 "langutils/sc_parser/src/sc_grammar_parser.cpp"
    break;

  case 4: // region.item: expr
#line 181 "langutils/sc_parser/src/sc_grammar.y"
                { yylhs.value.as < error_index<AnyExprIndex> > () = yystack_[0].value.as < AnyExprIndex > (); }
#line 2475 "langutils/sc_parser/src/sc_grammar_parser.cpp"
    break;

  case 5: // region.item: OPENPAREN argument_declarations block.contents CLOSEPAREN
#line 183 "langutils/sc_parser/src/sc_grammar.y"
                { 
			auto block = cxt.create(BlockNode{}, yylhs.location, yystack_[2].value.as < DeclareArgumentListIndex > (), yystack_[1].value.as < BlockContentsListIndex > ()); 

			yylhs.value.as < error_index<AnyExprIndex> > () = cxt.create(
				MessageNode{MessageNode::SelectorMode::Value}, 
				yylhs.location, 
				cxt.create(Missing{}, yylhs.location),
				cxt.create(ArgumentList{}, yylhs.location, block)
			);
		}
#line 2490 "langutils/sc_parser/src/sc_grammar_parser.cpp"
    break;

  case 6: // region.item: OPENPAREN argument_declarations CLOSEPAREN
#line 194 "langutils/sc_parser/src/sc_grammar.y"
                { 
			auto block = cxt.create(BlockNode{}, yylhs.location, yystack_[1].value.as < DeclareArgumentListIndex > (), cxt.create(BlockContentsList{}, yylhs.location));

			yylhs.value.as < error_index<AnyExprIndex> > () = cxt.create(
				MessageNode{MessageNode::SelectorMode::Value}, 
				yylhs.location, 
				cxt.create(Missing{}, yylhs.location),
				cxt.create(ArgumentList{}, yylhs.location, block)
			);
		}
#line 2505 "langutils/sc_parser/src/sc_grammar_parser.cpp"
    break;

  case 7: // region: INTERPRET expr
#line 208 "langutils/sc_parser/src/sc_grammar.y"
                { 
			yylhs.value.as < RegionListIndex > () = cxt.create(RegionList{}, yylhs.location, yystack_[0].value.as < AnyExprIndex > ());
		}
#line 2513 "langutils/sc_parser/src/sc_grammar_parser.cpp"
    break;

  case 8: // region: INTERPRET error
#line 212 "langutils/sc_parser/src/sc_grammar.y"
                {
			error_recovery::expr(cxt);
			yyclearin;
			yylhs.value.as < RegionListIndex > () = cxt.create(RegionList{}, yylhs.location, create_error(cxt, yystack_[0].location));
		}
#line 2523 "langutils/sc_parser/src/sc_grammar_parser.cpp"
    break;

  case 9: // region: INTERPRET OPENPAREN argument_declarations block.contents semicolon.opt CLOSEPAREN
#line 218 "langutils/sc_parser/src/sc_grammar.y"
                { 
			auto block = cxt.create(BlockNode{}, yylhs.location, yystack_[3].value.as < DeclareArgumentListIndex > (), yystack_[2].value.as < BlockContentsListIndex > ()); 

			auto msg = cxt.create(
				MessageNode{MessageNode::SelectorMode::Value}, 
				yylhs.location, 
				cxt.create(Missing{}, yylhs.location),
				cxt.create(ArgumentList{}, yylhs.location, block)
			);
			yylhs.value.as < RegionListIndex > () = cxt.create(RegionList{}, yylhs.location, msg);
		}
#line 2539 "langutils/sc_parser/src/sc_grammar_parser.cpp"
    break;

  case 10: // region: INTERPRET OPENPAREN argument_declarations CLOSEPAREN
#line 230 "langutils/sc_parser/src/sc_grammar.y"
                { 
			auto block = cxt.create(BlockNode{}, yylhs.location, yystack_[1].value.as < DeclareArgumentListIndex > (), cxt.create(BlockContentsList{}, yylhs.location));

			auto msg = cxt.create(
				MessageNode{MessageNode::SelectorMode::Value}, 
				yylhs.location, 
				cxt.create(Missing{}, yylhs.location),
				cxt.create(ArgumentList{}, yylhs.location, block)
			);

			yylhs.value.as < RegionListIndex > () = cxt.create(RegionList{}, yylhs.location, msg);
		}
#line 2556 "langutils/sc_parser/src/sc_grammar_parser.cpp"
    break;

  case 11: // region: region SEMICOLON region.item
#line 244 "langutils/sc_parser/src/sc_grammar.y"
                { yylhs.value.as < RegionListIndex > () = cxt.graph.append(yystack_[2].value.as < RegionListIndex > (), yylhs.location, yystack_[0].value.as < error_index<AnyExprIndex> > ()); }
#line 2562 "langutils/sc_parser/src/sc_grammar_parser.cpp"
    break;

  case 12: // region: region REGION_SEPARATOR region.item
#line 247 "langutils/sc_parser/src/sc_grammar.y"
                { yylhs.value.as < RegionListIndex > () = cxt.graph.append(yystack_[2].value.as < RegionListIndex > (), yylhs.location, yystack_[0].value.as < error_index<AnyExprIndex> > ()); }
#line 2568 "langutils/sc_parser/src/sc_grammar_parser.cpp"
    break;

  case 13: // region: region error
#line 250 "langutils/sc_parser/src/sc_grammar.y"
                {
			if (yystack_[1].location.end.line_number != yystack_[0].location.begin.line_number){
				auto first_child = cxt.graph.edges(*yystack_[1].value.as < RegionListIndex > ()).first_child.value();
				auto last_child = cxt.graph.last_child(*yystack_[1].value.as < RegionListIndex > ());
				auto loc = cxt.graph.location(last_child ? Index{*last_child} : Index{first_child});
				error_recovery::region_separator(cxt, loc);
				cxt.region_recovery = sc::ast::parser::ParserContext::RegionRecovery::EmitRegionSeparator;
				static_assert(std::is_same_v<decltype(yyerrstatus_), int>);
				yyerrstatus_ = 0; // this is NOT in the api, but the only way to get errors to re-emit.
				yyclearin;
				yylhs.value.as < RegionListIndex > () = yystack_[1].value.as < RegionListIndex > ();
			} else {
				error_recovery::expr(cxt);
				yylhs.value.as < RegionListIndex > () = cxt.graph.append(yystack_[1].value.as < RegionListIndex > (), create_error(cxt, yystack_[0].location, yystack_[1].value.as < RegionListIndex > ()));
				yyclearin;
			} 
		}
#line 2590 "langutils/sc_parser/src/sc_grammar_parser.cpp"
    break;

  case 14: // classOrExtList.list: classOrExtList.item
#line 276 "langutils/sc_parser/src/sc_grammar.y"
                { yylhs.value.as < ClassOrExtensionListIndex > () = cxt.create(ClassOrExtensionList{}, yylhs.location, yystack_[0].value.as < ClassOrExtensionIndex > ()); }
#line 2596 "langutils/sc_parser/src/sc_grammar_parser.cpp"
    break;

  case 15: // classOrExtList.list: classOrExtList.list classOrExtList.item
#line 278 "langutils/sc_parser/src/sc_grammar.y"
                { yylhs.value.as < ClassOrExtensionListIndex > () = cxt.graph.append(yystack_[1].value.as < ClassOrExtensionListIndex > (), yystack_[0].value.as < ClassOrExtensionIndex > ()); }
#line 2602 "langutils/sc_parser/src/sc_grammar_parser.cpp"
    break;

  case 16: // classOrExtList.item: class
#line 282 "langutils/sc_parser/src/sc_grammar.y"
                { yylhs.value.as < ClassOrExtensionIndex > () = yystack_[0].value.as < ClassIndex > (); }
#line 2608 "langutils/sc_parser/src/sc_grammar_parser.cpp"
    break;

  case 17: // classOrExtList.item: class.extension
#line 283 "langutils/sc_parser/src/sc_grammar.y"
                          { yylhs.value.as < ClassOrExtensionIndex > () = yystack_[0].value.as < ClassExtensionIndex > (); }
#line 2614 "langutils/sc_parser/src/sc_grammar_parser.cpp"
    break;

  case 18: // class: CLASSNAME class.slot.opt class.super.opt OPENCURLY class.vars.opt method.list.opt CLOSECURLY
#line 288 "langutils/sc_parser/src/sc_grammar.y"
                { yylhs.value.as < ClassIndex > () = cxt.create(Class{}, yylhs.location, cxt.create(ClassNameIdentifier{}, yystack_[6].location), yystack_[5].value.as < maybe<NamedIdentifierIndex> > (), yystack_[4].value.as < maybe<ClassNameIdentifierIndex> > (), yystack_[2].value.as < DeclareClassAnyVarListIndex > (), yystack_[1].value.as < MethodListIndex > ()); }
#line 2620 "langutils/sc_parser/src/sc_grammar_parser.cpp"
    break;

  case 19: // class.super.opt: %empty
#line 292 "langutils/sc_parser/src/sc_grammar.y"
                 { yylhs.value.as < maybe<ClassNameIdentifierIndex> > () = cxt.create(Missing{}, yylhs.location); }
#line 2626 "langutils/sc_parser/src/sc_grammar_parser.cpp"
    break;

  case 20: // class.super.opt: COLON CLASSNAME
#line 293 "langutils/sc_parser/src/sc_grammar.y"
                          { yylhs.value.as < maybe<ClassNameIdentifierIndex> > () = cxt.create(ClassNameIdentifier{}, yystack_[0].location); }
#line 2632 "langutils/sc_parser/src/sc_grammar_parser.cpp"
    break;

  case 21: // class.slot.opt: %empty
#line 297 "langutils/sc_parser/src/sc_grammar.y"
                 { yylhs.value.as < maybe<NamedIdentifierIndex> > () = cxt.create(Missing{}, yylhs.location); }
#line 2638 "langutils/sc_parser/src/sc_grammar_parser.cpp"
    break;

  case 22: // class.slot.opt: OPENSQUARE name CLOSESQUARE
#line 298 "langutils/sc_parser/src/sc_grammar.y"
                                      { yylhs.value.as < maybe<NamedIdentifierIndex> > () = yystack_[1].value.as < NamedIdentifierIndex > (); }
#line 2644 "langutils/sc_parser/src/sc_grammar_parser.cpp"
    break;

  case 23: // class.extension: ADD CLASSNAME OPENCURLY method.list.opt CLOSECURLY
#line 303 "langutils/sc_parser/src/sc_grammar.y"
                { yylhs.value.as < ClassExtensionIndex > () = cxt.create(ClassExtension{}, yylhs.location, cxt.create(ClassNameIdentifier{}, yystack_[3].location), yystack_[1].value.as < MethodListIndex > ()); }
#line 2650 "langutils/sc_parser/src/sc_grammar_parser.cpp"
    break;

  case 24: // class.vars.entry.item: accessor variable_declarations.list.item
#line 308 "langutils/sc_parser/src/sc_grammar.y"
                { yylhs.value.as < DeclareClassVarIndex > () = cxt.create(DeclareClassVar{yystack_[1].value.as < ReadWriteAccessor > ()}, yylhs.location, yystack_[0].value.as < DeclareAnyVariableIndex > ());}
#line 2656 "langutils/sc_parser/src/sc_grammar_parser.cpp"
    break;

  case 25: // class.vars.entry.list: class.vars.entry.item
#line 313 "langutils/sc_parser/src/sc_grammar.y"
                { yylhs.value.as < DeclareMemberListIndex > () = cxt.create(DeclareMemberList{}, yylhs.location, yystack_[0].value.as < DeclareClassVarIndex > ()); }
#line 2662 "langutils/sc_parser/src/sc_grammar_parser.cpp"
    break;

  case 26: // class.vars.entry.list: class.vars.entry.list COMMA class.vars.entry.item
#line 315 "langutils/sc_parser/src/sc_grammar.y"
                { yylhs.value.as < DeclareMemberListIndex > () = cxt.graph.append(yystack_[2].value.as < DeclareMemberListIndex > (), yystack_[0].value.as < DeclareClassVarIndex > ()); }
#line 2668 "langutils/sc_parser/src/sc_grammar_parser.cpp"
    break;

  case 27: // class.vars.entry: CLASSVAR class.vars.entry.list
#line 320 "langutils/sc_parser/src/sc_grammar.y"
                { yylhs.value.as < DeclareAnyList > () = cxt.graph.cast<DeclareClassMemberList>(yystack_[0].value.as < DeclareMemberListIndex > ()); }
#line 2674 "langutils/sc_parser/src/sc_grammar_parser.cpp"
    break;

  case 28: // class.vars.entry: VAR class.vars.entry.list
#line 322 "langutils/sc_parser/src/sc_grammar.y"
                { yylhs.value.as < DeclareAnyList > () = cxt.graph.cast<DeclareMemberList>(yystack_[0].value.as < DeclareMemberListIndex > ()); }
#line 2680 "langutils/sc_parser/src/sc_grammar_parser.cpp"
    break;

  case 29: // class.vars.entry: CONST class.vars.entry.list
#line 324 "langutils/sc_parser/src/sc_grammar.y"
                { yylhs.value.as < DeclareAnyList > () = cxt.graph.cast<DeclareConstList>(yystack_[0].value.as < DeclareMemberListIndex > ()); }
#line 2686 "langutils/sc_parser/src/sc_grammar_parser.cpp"
    break;

  case 30: // class.vars: class.vars.entry
#line 329 "langutils/sc_parser/src/sc_grammar.y"
                { yylhs.value.as < DeclareClassAnyVarListIndex > () = cxt.create(ClassAnyVarList{}, yylhs.location, yystack_[0].value.as < DeclareAnyList > ());}
#line 2692 "langutils/sc_parser/src/sc_grammar_parser.cpp"
    break;

  case 31: // class.vars: class.vars SEMICOLON class.vars.entry
#line 331 "langutils/sc_parser/src/sc_grammar.y"
                { yylhs.value.as < DeclareClassAnyVarListIndex > () = cxt.graph.append(yystack_[2].value.as < DeclareClassAnyVarListIndex > (), yystack_[0].value.as < DeclareAnyList > ()); }
#line 2698 "langutils/sc_parser/src/sc_grammar_parser.cpp"
    break;

  case 32: // class.vars.opt: %empty
#line 335 "langutils/sc_parser/src/sc_grammar.y"
                 { yylhs.value.as < DeclareClassAnyVarListIndex > () = cxt.create(ClassAnyVarList{}, yylhs.location); }
#line 2704 "langutils/sc_parser/src/sc_grammar_parser.cpp"
    break;

  case 33: // class.vars.opt: class.vars semicolon.opt
#line 336 "langutils/sc_parser/src/sc_grammar.y"
                                   { yylhs.value.as < DeclareClassAnyVarListIndex > () = yystack_[1].value.as < DeclareClassAnyVarListIndex > (); }
#line 2710 "langutils/sc_parser/src/sc_grammar_parser.cpp"
    break;

  case 34: // method.name: name
#line 340 "langutils/sc_parser/src/sc_grammar.y"
               { yylhs.value.as < SelectorIndex > () = yystack_[0].value.as < NamedIdentifierIndex > (); }
#line 2716 "langutils/sc_parser/src/sc_grammar_parser.cpp"
    break;

  case 35: // method.name: binary_op.raw
#line 341 "langutils/sc_parser/src/sc_grammar.y"
                        { yylhs.value.as < SelectorIndex > () = yystack_[0].value.as < SelectorIndex > (); }
#line 2722 "langutils/sc_parser/src/sc_grammar_parser.cpp"
    break;

  case 36: // method.base: method.name OPENCURLY argument_declarations.opt block.contents semicolon.opt CLOSECURLY
#line 346 "langutils/sc_parser/src/sc_grammar.y"
                { yylhs.value.as < MethodIndex > () = cxt.create(Method{}, yylhs.location, yystack_[5].value.as < SelectorIndex > (), yystack_[3].value.as < DeclareArgumentListIndex > (), cxt.create(Missing{},yystack_[4].location), yystack_[2].value.as < BlockContentsListIndex > ()); }
#line 2728 "langutils/sc_parser/src/sc_grammar_parser.cpp"
    break;

  case 37: // method.base: method.name OPENCURLY argument_declarations.opt CLOSECURLY
#line 348 "langutils/sc_parser/src/sc_grammar.y"
                { yylhs.value.as < MethodIndex > () = cxt.create(Method{}, yylhs.location, yystack_[3].value.as < SelectorIndex > (), yystack_[1].value.as < DeclareArgumentListIndex > (), cxt.create(Missing{},yystack_[2].location), cxt.create(BlockList{}, yystack_[0].location)); }
#line 2734 "langutils/sc_parser/src/sc_grammar_parser.cpp"
    break;

  case 38: // method.base: method.name OPENCURLY argument_declarations.opt PRIMITIVENAME block.contents semicolon.opt CLOSECURLY
#line 350 "langutils/sc_parser/src/sc_grammar.y"
                { yylhs.value.as < MethodIndex > () = cxt.create(Method{}, yylhs.location, yystack_[6].value.as < SelectorIndex > (), yystack_[4].value.as < DeclareArgumentListIndex > (), cxt.create(PrimitiveIdentifier{}, yystack_[3].location), yystack_[2].value.as < BlockContentsListIndex > ()); }
#line 2740 "langutils/sc_parser/src/sc_grammar_parser.cpp"
    break;

  case 39: // method.base: method.name OPENCURLY argument_declarations.opt PRIMITIVENAME CLOSECURLY
#line 352 "langutils/sc_parser/src/sc_grammar.y"
                { yylhs.value.as < MethodIndex > () = cxt.create(Method{}, yylhs.location, yystack_[4].value.as < SelectorIndex > (), yystack_[2].value.as < DeclareArgumentListIndex > (), cxt.create(PrimitiveIdentifier{}, yystack_[1].location), cxt.create(BlockList{}, yystack_[0].location)); }
#line 2746 "langutils/sc_parser/src/sc_grammar_parser.cpp"
    break;

  case 40: // method: method.base
#line 356 "langutils/sc_parser/src/sc_grammar.y"
                      { yylhs.value.as < AnyMethodIndex > () = yystack_[0].value.as < MethodIndex > (); }
#line 2752 "langutils/sc_parser/src/sc_grammar_parser.cpp"
    break;

  case 41: // method: MULTIPLY method.base
#line 358 "langutils/sc_parser/src/sc_grammar.y"
                { yylhs.value.as < AnyMethodIndex > () = cxt.graph.cast<ClassMethod>(yystack_[0].value.as < MethodIndex > ()); }
#line 2758 "langutils/sc_parser/src/sc_grammar_parser.cpp"
    break;

  case 42: // method.list: method
#line 362 "langutils/sc_parser/src/sc_grammar.y"
                 { yylhs.value.as < MethodListIndex > () = cxt.create(MethodList{}, yylhs.location, yystack_[0].value.as < AnyMethodIndex > ()); }
#line 2764 "langutils/sc_parser/src/sc_grammar_parser.cpp"
    break;

  case 43: // method.list: method.list method
#line 363 "langutils/sc_parser/src/sc_grammar.y"
                             { yylhs.value.as < MethodListIndex > () = cxt.graph.append(yystack_[1].value.as < MethodListIndex > (), yylhs.location, yystack_[0].value.as < AnyMethodIndex > ()); }
#line 2770 "langutils/sc_parser/src/sc_grammar_parser.cpp"
    break;

  case 44: // method.list.opt: %empty
#line 367 "langutils/sc_parser/src/sc_grammar.y"
                 { yylhs.value.as < MethodListIndex > () = cxt.create(MethodList{}, yylhs.location); }
#line 2776 "langutils/sc_parser/src/sc_grammar_parser.cpp"
    break;

  case 45: // method.list.opt: method.list
#line 368 "langutils/sc_parser/src/sc_grammar.y"
                      { yylhs.value.as < MethodListIndex > () = yystack_[0].value.as < MethodListIndex > (); }
#line 2782 "langutils/sc_parser/src/sc_grammar_parser.cpp"
    break;

  case 48: // block: block.open argument_declarations.opt block.contents semicolon.opt CLOSECURLY
#line 375 "langutils/sc_parser/src/sc_grammar.y"
                { yylhs.value.as < BlockIndex > () = cxt.create(BlockNode{}, yylhs.location, yystack_[3].value.as < DeclareArgumentListIndex > (), yystack_[2].value.as < BlockContentsListIndex > ()); }
#line 2788 "langutils/sc_parser/src/sc_grammar_parser.cpp"
    break;

  case 49: // block: block.open argument_declarations.opt CLOSECURLY
#line 377 "langutils/sc_parser/src/sc_grammar.y"
                { yylhs.value.as < BlockIndex > () = cxt.create(BlockNode{}, yylhs.location, yystack_[1].value.as < DeclareArgumentListIndex > (), cxt.create(BlockContentsList{}, yylhs.location)); }
#line 2794 "langutils/sc_parser/src/sc_grammar_parser.cpp"
    break;

  case 50: // block.opt_list: %empty
#line 381 "langutils/sc_parser/src/sc_grammar.y"
                 { yylhs.value.as < BlockListIndex > () = {}; }
#line 2800 "langutils/sc_parser/src/sc_grammar_parser.cpp"
    break;

  case 51: // block.opt_list: block.list
#line 382 "langutils/sc_parser/src/sc_grammar.y"
                     { yylhs.value.as < BlockListIndex > () = yystack_[0].value.as < BlockListIndex > (); }
#line 2806 "langutils/sc_parser/src/sc_grammar_parser.cpp"
    break;

  case 52: // block.list: block
#line 386 "langutils/sc_parser/src/sc_grammar.y"
                { yylhs.value.as < BlockListIndex > () = cxt.create(BlockList{}, yylhs.location, yystack_[0].value.as < BlockIndex > ()); }
#line 2812 "langutils/sc_parser/src/sc_grammar_parser.cpp"
    break;

  case 53: // block.list: block.list block
#line 387 "langutils/sc_parser/src/sc_grammar.y"
                           { yylhs.value.as < BlockListIndex > () = cxt.graph.append(yystack_[1].value.as < BlockListIndex > (), yylhs.location, yystack_[0].value.as < BlockIndex > ()); }
#line 2818 "langutils/sc_parser/src/sc_grammar_parser.cpp"
    break;

  case 54: // block.contents: block.contents.item
#line 391 "langutils/sc_parser/src/sc_grammar.y"
                              { yylhs.value.as < BlockContentsListIndex > () = cxt.create(BlockContentsList{}, yylhs.location, yystack_[0].value.as < BlockItemIndex > ()); }
#line 2824 "langutils/sc_parser/src/sc_grammar_parser.cpp"
    break;

  case 55: // block.contents: block.contents SEMICOLON block.contents.item
#line 392 "langutils/sc_parser/src/sc_grammar.y"
                                                       { yylhs.value.as < BlockContentsListIndex > () = cxt.graph.append(yystack_[2].value.as < BlockContentsListIndex > (), yylhs.location, yystack_[0].value.as < BlockItemIndex > ()); }
#line 2830 "langutils/sc_parser/src/sc_grammar_parser.cpp"
    break;

  case 56: // block.contents.item: expr
#line 396 "langutils/sc_parser/src/sc_grammar.y"
               { yylhs.value.as < BlockItemIndex > () = yystack_[0].value.as < AnyExprIndex > (); }
#line 2836 "langutils/sc_parser/src/sc_grammar_parser.cpp"
    break;

  case 57: // block.contents.item: variable_declarations
#line 397 "langutils/sc_parser/src/sc_grammar.y"
                                { yylhs.value.as < BlockItemIndex > () = yystack_[0].value.as < DeclareVariableListIndex > (); }
#line 2842 "langutils/sc_parser/src/sc_grammar_parser.cpp"
    break;

  case 58: // block.contents.item: NONLOCALRETURN expr
#line 398 "langutils/sc_parser/src/sc_grammar.y"
                              { yylhs.value.as < BlockItemIndex > () = cxt.create(NonLocalReturnExpr{}, yylhs.location, yystack_[0].value.as < AnyExprIndex > ()); }
#line 2848 "langutils/sc_parser/src/sc_grammar_parser.cpp"
    break;

  case 59: // msgsend: OPENPAREN binary_op.no_adverb CLOSEPAREN OPENPAREN arguments CLOSEPAREN block.opt_list
#line 403 "langutils/sc_parser/src/sc_grammar.y"
                {
			if (yystack_[0].value.as < BlockListIndex > ()) {
				cxt.graph.merge(yystack_[2].value.as < ArgumentListIndex > (), yystack_[0].value.as < BlockListIndex > ());
				cxt.graph.location(*yystack_[2].value.as < ArgumentListIndex > ()) = {yystack_[2].location.begin, yystack_[0].location.end}; // spans arguments and block list
			}
			yylhs.value.as < AnyExprIndex > () = cxt.create(MessageNode{}, yylhs.location, yystack_[5].value.as < SelectorIndex > (), yystack_[2].value.as < ArgumentListIndex > ());
		}
#line 2860 "langutils/sc_parser/src/sc_grammar_parser.cpp"
    break;

  case 60: // msgsend: OPENPAREN binary_op.no_adverb CLOSEPAREN OPENPAREN CLOSEPAREN block.list
#line 412 "langutils/sc_parser/src/sc_grammar.y"
                { yylhs.value.as < AnyExprIndex > () = cxt.create(MessageNode{}, yylhs.location, yystack_[4].value.as < SelectorIndex > (), yystack_[0].value.as < BlockListIndex > ()); }
#line 2866 "langutils/sc_parser/src/sc_grammar_parser.cpp"
    break;

  case 61: // msgsend: OPENPAREN binary_op.no_adverb CLOSEPAREN block.list
#line 415 "langutils/sc_parser/src/sc_grammar.y"
                { yylhs.value.as < AnyExprIndex > () = cxt.create(MessageNode{}, yylhs.location, yystack_[2].value.as < SelectorIndex > (), cxt.create(ArgumentList{}, yylhs.location, yystack_[0].value.as < BlockListIndex > ())); }
#line 2872 "langutils/sc_parser/src/sc_grammar_parser.cpp"
    break;

  case 62: // msgsend: name OPENPAREN arguments CLOSEPAREN block.opt_list
#line 419 "langutils/sc_parser/src/sc_grammar.y"
                { 
			if(yystack_[0].value.as < BlockListIndex > ()){
				cxt.graph.merge(yystack_[2].value.as < ArgumentListIndex > (), yystack_[0].value.as < BlockListIndex > ());
				cxt.graph.location(*yystack_[2].value.as < ArgumentListIndex > ()) = {yystack_[2].location.begin, yystack_[0].location.end}; // spans arguments and block list
			}
			yylhs.value.as < AnyExprIndex > () = cxt.create(MessageNode{}, yylhs.location, yystack_[4].value.as < NamedIdentifierIndex > (), yystack_[2].value.as < ArgumentListIndex > ()); 
		}
#line 2884 "langutils/sc_parser/src/sc_grammar_parser.cpp"
    break;

  case 63: // msgsend: name OPENPAREN CLOSEPAREN block.list
#line 428 "langutils/sc_parser/src/sc_grammar.y"
                { yylhs.value.as < AnyExprIndex > () = cxt.create(MessageNode{}, yylhs.location, yystack_[3].value.as < NamedIdentifierIndex > (), yystack_[0].value.as < BlockListIndex > ()); }
#line 2890 "langutils/sc_parser/src/sc_grammar_parser.cpp"
    break;

  case 64: // msgsend: name block.list
#line 431 "langutils/sc_parser/src/sc_grammar.y"
                { yylhs.value.as < AnyExprIndex > () = cxt.create(MessageNode{}, yylhs.location, yystack_[1].value.as < NamedIdentifierIndex > (), cxt.create(ArgumentList{}, yystack_[0].location, yystack_[0].value.as < BlockListIndex > ())); }
#line 2896 "langutils/sc_parser/src/sc_grammar_parser.cpp"
    break;

  case 65: // msgsend: expr DOT name arguments.maybe_paren block.opt_list
#line 434 "langutils/sc_parser/src/sc_grammar.y"
                { 
			if (yystack_[0].value.as < BlockListIndex > ()) {
				cxt.graph.merge(yystack_[1].value.as < ArgumentListIndex > (), yystack_[0].value.as < BlockListIndex > ());
				cxt.graph.location(*yystack_[1].value.as < ArgumentListIndex > ()) = {yystack_[1].location.begin, yystack_[0].location.end}; // spans arguments and block list
			}
			cxt.graph.prepend(yystack_[1].value.as < ArgumentListIndex > (), yystack_[4].value.as < AnyExprIndex > ()); // put the receiver in place
			cxt.graph.location(*yystack_[1].value.as < ArgumentListIndex > ()) = {yystack_[4].location.begin, yystack_[1].location.end}; // spans arguments and block list
			yylhs.value.as < AnyExprIndex > () = cxt.create(MessageNode{}, yylhs.location, yystack_[2].value.as < NamedIdentifierIndex > (), yystack_[1].value.as < ArgumentListIndex > ());
		}
#line 2910 "langutils/sc_parser/src/sc_grammar_parser.cpp"
    break;

  case 66: // msgsend: expr DOT arguments.paren block.opt_list
#line 445 "langutils/sc_parser/src/sc_grammar.y"
                { 
			if (yystack_[0].value.as < BlockListIndex > ()) {
				cxt.graph.merge(yystack_[1].value.as < ArgumentListIndex > (), yystack_[0].value.as < BlockListIndex > ());
				cxt.graph.location(*yystack_[1].value.as < ArgumentListIndex > ()) = {yystack_[1].location.begin, yystack_[0].location.end}; // spans arguments and block list
			}
			cxt.graph.prepend(yystack_[1].value.as < ArgumentListIndex > (), yystack_[3].value.as < AnyExprIndex > ()); // put the receiver in place
			cxt.graph.location(*yystack_[1].value.as < ArgumentListIndex > ()) = {yystack_[3].location.begin, yystack_[1].location.end}; // spans arguments and block list
			yylhs.value.as < AnyExprIndex > () = cxt.create(MessageNode{MessageNode::SelectorMode::Value}, yylhs.location, cxt.create(Missing{}, yystack_[2].location), yystack_[1].value.as < ArgumentListIndex > ());
		}
#line 2924 "langutils/sc_parser/src/sc_grammar_parser.cpp"
    break;

  case 67: // msgsend: expr DOT OPENPAREN CLOSEPAREN block.opt_list
#line 455 "langutils/sc_parser/src/sc_grammar.y"
                {
			auto args = cxt.create(ArgumentList{}, yylhs.location, yystack_[4].value.as < AnyExprIndex > ());
			if (yystack_[0].value.as < BlockListIndex > ()) cxt.graph.merge(args, yystack_[0].value.as < BlockListIndex > ());
			yylhs.value.as < AnyExprIndex > () = cxt.create(MessageNode{MessageNode::SelectorMode::Value}, yylhs.location, cxt.create(Missing{}, yystack_[3].location), args);
		}
#line 2934 "langutils/sc_parser/src/sc_grammar_parser.cpp"
    break;

  case 68: // msgsend: expr DOT error
#line 463 "langutils/sc_parser/src/sc_grammar.y"
                {
			auto unexpected = cxt.consume_error(); 
			std::cout << "GOT AN ERROR WITH A DOT"<< std::endl;
			yylhs.value.as < AnyExprIndex > () = yystack_[2].value.as < AnyExprIndex > ();
		}
#line 2944 "langutils/sc_parser/src/sc_grammar_parser.cpp"
    break;

  case 69: // msgsend: CLASSNAME OPENSQUARE literal.array.contents CLOSESQUARE
#line 470 "langutils/sc_parser/src/sc_grammar.y"
                {  yylhs.value.as < AnyExprIndex > () = cxt.create(CollectionNode{}, yylhs.location, cxt.create(ClassNameIdentifier{}, yystack_[3].location), yystack_[1].value.as < ArrayIndex > ()); }
#line 2950 "langutils/sc_parser/src/sc_grammar_parser.cpp"
    break;

  case 70: // msgsend: CLASSNAME block.list
#line 473 "langutils/sc_parser/src/sc_grammar.y"
                {
			auto args = cxt.create(ArgumentList{}, yylhs.location, cxt.create(NamedIdentifier{}, yystack_[1].location));
			cxt.graph.merge(args, yystack_[0].value.as < BlockListIndex > ());
			yylhs.value.as < AnyExprIndex > () = cxt.create(MessageNode{}, yylhs.location, cxt.create(Missing{}, yystack_[1].location), args);
		}
#line 2960 "langutils/sc_parser/src/sc_grammar_parser.cpp"
    break;

  case 71: // msgsend: CLASSNAME OPENPAREN arguments CLOSEPAREN block.opt_list
#line 479 "langutils/sc_parser/src/sc_grammar.y"
                {
			if (yystack_[0].value.as < BlockListIndex > ()) {
				cxt.graph.merge(yystack_[2].value.as < ArgumentListIndex > (), yystack_[0].value.as < BlockListIndex > ());
				cxt.graph.location(*yystack_[2].value.as < ArgumentListIndex > ()) = {yystack_[2].location.begin, yystack_[0].location.end}; // spans arguments and block list
			}
			cxt.graph.prepend(yystack_[2].value.as < ArgumentListIndex > (), cxt.create(ClassNameIdentifier{}, yystack_[4].location)); // put the receiver in place
			cxt.graph.location(*yystack_[2].value.as < ArgumentListIndex > ()) = {yystack_[2].location.begin, yystack_[0].location.end}; // spans arguments and block list
			yylhs.value.as < AnyExprIndex > () = cxt.create(MessageNode{MessageNode::SelectorMode::New}, yylhs.location, cxt.create(Missing{}, yystack_[4].location), yystack_[2].value.as < ArgumentListIndex > ());
		}
#line 2974 "langutils/sc_parser/src/sc_grammar_parser.cpp"
    break;

  case 72: // msgsend: CLASSNAME OPENPAREN CLOSEPAREN block.opt_list
#line 490 "langutils/sc_parser/src/sc_grammar.y"
                {
			auto args = cxt.create(ArgumentList{}, yylhs.location, yystack_[0].value.as < BlockListIndex > ());
			cxt.graph.prepend(args, cxt.create(ClassNameIdentifier{}, yystack_[3].location)); // put the receiver in place
			yylhs.value.as < AnyExprIndex > () = cxt.create(MessageNode{MessageNode::SelectorMode::New}, yylhs.location, cxt.create(Missing{}, yystack_[3].location), args);
		}
#line 2984 "langutils/sc_parser/src/sc_grammar_parser.cpp"
    break;

  case 73: // expr.base: literal
#line 499 "langutils/sc_parser/src/sc_grammar.y"
                  { yylhs.value.as < AnyExprIndex > () = yystack_[0].value.as < AnyPossiblyLiteralIndex > (); }
#line 2990 "langutils/sc_parser/src/sc_grammar_parser.cpp"
    break;

  case 74: // expr.base: name
#line 501 "langutils/sc_parser/src/sc_grammar.y"
               { yylhs.value.as < AnyExprIndex > () = yystack_[0].value.as < NamedIdentifierIndex > (); }
#line 2996 "langutils/sc_parser/src/sc_grammar_parser.cpp"
    break;

  case 75: // expr.base: msgsend
#line 503 "langutils/sc_parser/src/sc_grammar.y"
                  { yylhs.value.as < AnyExprIndex > () = yystack_[0].value.as < AnyExprIndex > (); }
#line 3002 "langutils/sc_parser/src/sc_grammar_parser.cpp"
    break;

  case 76: // expr.base: OPENPAREN block.contents semicolon.opt CLOSEPAREN
#line 505 "langutils/sc_parser/src/sc_grammar.y"
                { 
			auto blk = cxt.create(BlockNode{}, yylhs.location, cxt.create(DeclareArgumentList{}, yylhs.location), yystack_[2].value.as < BlockContentsListIndex > ());
			yylhs.value.as < AnyExprIndex > () = cxt.create(MessageNode{MessageNode::SelectorMode::Value}, yylhs.location.flatten(), cxt.create(Missing{}, yylhs.location.flatten()), cxt.create(ArgumentList{}, yylhs.location, blk));
		}
#line 3011 "langutils/sc_parser/src/sc_grammar_parser.cpp"
    break;

  case 77: // expr.base: TILDE name
#line 509 "langutils/sc_parser/src/sc_grammar.y"
                     { yylhs.value.as < AnyExprIndex > () = cxt.create(EnvIdentifierNode{}, yylhs.location, yystack_[0].value.as < NamedIdentifierIndex > ()); }
#line 3017 "langutils/sc_parser/src/sc_grammar_parser.cpp"
    break;

  case 78: // expr.base: expr.base OPENSQUARE arguments CLOSESQUARE
#line 515 "langutils/sc_parser/src/sc_grammar.y"
                { 
			cxt.graph.prepend(yystack_[1].value.as < ArgumentListIndex > (), yystack_[3].value.as < AnyExprIndex > ()); // put receiver in place.
			cxt.graph.location(*yystack_[1].value.as < ArgumentListIndex > ()) = yylhs.location;
			yylhs.value.as < AnyExprIndex > () = cxt.create(MessageNode{MessageNode::SelectorMode::At}, yylhs.location, cxt.create(Missing{}, yystack_[2].location), yystack_[1].value.as < ArgumentListIndex > ()); 
		}
#line 3027 "langutils/sc_parser/src/sc_grammar_parser.cpp"
    break;

  case 79: // expr.base: expr.base OPENSQUARE CLOSESQUARE
#line 522 "langutils/sc_parser/src/sc_grammar.y"
                { 
			auto args = cxt.create(ArgumentList{}, yylhs.location, yystack_[2].value.as < AnyExprIndex > ());
			yylhs.value.as < AnyExprIndex > () = cxt.create(MessageNode{MessageNode::SelectorMode::At}, yylhs.location, cxt.create(Missing{}, yystack_[1].location), args); 
		}
#line 3036 "langutils/sc_parser/src/sc_grammar_parser.cpp"
    break;

  case 80: // expr: expr.base
#line 531 "langutils/sc_parser/src/sc_grammar.y"
                    { yylhs.value.as < AnyExprIndex > () = yystack_[0].value.as < AnyExprIndex > (); }
#line 3042 "langutils/sc_parser/src/sc_grammar_parser.cpp"
    break;

  case 81: // expr: CLASSNAME
#line 535 "langutils/sc_parser/src/sc_grammar.y"
                    { yylhs.value.as < AnyExprIndex > () = cxt.create(ClassNameIdentifier{}, yylhs.location); }
#line 3048 "langutils/sc_parser/src/sc_grammar_parser.cpp"
    break;

  case 82: // expr: expr DOT OPENSQUARE arguments CLOSESQUARE
#line 538 "langutils/sc_parser/src/sc_grammar.y"
                { 
			cxt.graph.prepend(yystack_[1].value.as < ArgumentListIndex > (), yystack_[4].value.as < AnyExprIndex > ()); // put receiver in place
			yylhs.value.as < AnyExprIndex > () = cxt.create(MessageNode{MessageNode::SelectorMode::At}, yylhs.location, cxt.create(Missing{}, yylhs.location), yystack_[1].value.as < ArgumentListIndex > ());
		}
#line 3057 "langutils/sc_parser/src/sc_grammar_parser.cpp"
    break;

  case 83: // expr: expr DOT OPENSQUARE CLOSESQUARE
#line 543 "langutils/sc_parser/src/sc_grammar.y"
                { 
			auto args = cxt.create(ArgumentList{}, yylhs.location, yystack_[3].value.as < AnyExprIndex > ());
			yylhs.value.as < AnyExprIndex > () = cxt.create(MessageNode{MessageNode::SelectorMode::At}, yylhs.location, cxt.create(Missing{}, yystack_[1].location), args);
		}
#line 3066 "langutils/sc_parser/src/sc_grammar_parser.cpp"
    break;

  case 84: // expr: BACKTICK expr
#line 548 "langutils/sc_parser/src/sc_grammar.y"
                        { yylhs.value.as < AnyExprIndex > () = cxt.create(ReferenceNode{}, yylhs.location, yystack_[0].value.as < AnyExprIndex > ()); }
#line 3072 "langutils/sc_parser/src/sc_grammar_parser.cpp"
    break;

  case 85: // expr: expr binary_op expr
#line 551 "langutils/sc_parser/src/sc_grammar.y"
                { 
			auto args = cxt.create(ArgumentList{}, yylhs.location, yystack_[2].value.as < AnyExprIndex > (), yystack_[0].value.as < AnyExprIndex > ());
			yylhs.value.as < AnyExprIndex > () = cxt.create(MessageNode{}, yylhs.location, yystack_[1].value.as < SelectorMaybeAdverbIndex > (), args);
		}
#line 3081 "langutils/sc_parser/src/sc_grammar_parser.cpp"
    break;

  case 86: // expr: name EQUALSSIGN expr
#line 557 "langutils/sc_parser/src/sc_grammar.y"
                { yylhs.value.as < AnyExprIndex > () = cxt.create(AssignmentNode{}, yylhs.location, yystack_[2].value.as < NamedIdentifierIndex > (), yystack_[0].value.as < AnyExprIndex > ()); }
#line 3087 "langutils/sc_parser/src/sc_grammar_parser.cpp"
    break;

  case 87: // expr: TILDE name EQUALSSIGN expr
#line 560 "langutils/sc_parser/src/sc_grammar.y"
                { yylhs.value.as < AnyExprIndex > () = cxt.create(AssignmentNode{AssignmentNode::Target::Environment}, yylhs.location, yystack_[2].value.as < NamedIdentifierIndex > (), yystack_[0].value.as < AnyExprIndex > ()); }
#line 3093 "langutils/sc_parser/src/sc_grammar_parser.cpp"
    break;

  case 88: // expr: expr DOT name EQUALSSIGN expr
#line 563 "langutils/sc_parser/src/sc_grammar.y"
                { yylhs.value.as < AnyExprIndex > () = cxt.create(SetterNode{}, yylhs.location, yystack_[4].value.as < AnyExprIndex > (), yystack_[2].value.as < NamedIdentifierIndex > (), yystack_[0].value.as < AnyExprIndex > ()); }
#line 3099 "langutils/sc_parser/src/sc_grammar_parser.cpp"
    break;

  case 89: // expr: name OPENPAREN arguments CLOSEPAREN EQUALSSIGN expr
#line 566 "langutils/sc_parser/src/sc_grammar.y"
                { yylhs.value.as < AnyExprIndex > () = cxt.create(SetterNode{}, yylhs.location, yystack_[3].value.as < ArgumentListIndex > (), yystack_[5].value.as < NamedIdentifierIndex > (), yystack_[0].value.as < AnyExprIndex > ()); }
#line 3105 "langutils/sc_parser/src/sc_grammar_parser.cpp"
    break;

  case 90: // expr: expr.base OPENSQUARE arguments CLOSESQUARE EQUALSSIGN expr
#line 574 "langutils/sc_parser/src/sc_grammar.y"
                { yylhs.value.as < AnyExprIndex > () = cxt.create(AssignmentAtNode{}, yylhs.location, yystack_[5].value.as < AnyExprIndex > (), yystack_[3].value.as < ArgumentListIndex > (), yystack_[0].value.as < AnyExprIndex > ()); }
#line 3111 "langutils/sc_parser/src/sc_grammar_parser.cpp"
    break;

  case 91: // expr: expr.base OPENSQUARE CLOSESQUARE EQUALSSIGN expr
#line 576 "langutils/sc_parser/src/sc_grammar.y"
                { yylhs.value.as < AnyExprIndex > () = cxt.create(AssignmentAtNode{}, yylhs.location, yystack_[4].value.as < AnyExprIndex > (), cxt.create(ArgumentList{}, yystack_[3].location), yystack_[0].value.as < AnyExprIndex > ()); }
#line 3117 "langutils/sc_parser/src/sc_grammar_parser.cpp"
    break;

  case 92: // expr: expr DOT OPENSQUARE arguments CLOSESQUARE EQUALSSIGN expr
#line 579 "langutils/sc_parser/src/sc_grammar.y"
                { yylhs.value.as < AnyExprIndex > () = cxt.create(AssignmentAtNode{}, yylhs.location, yystack_[6].value.as < AnyExprIndex > (), yystack_[3].value.as < ArgumentListIndex > (), yystack_[0].value.as < AnyExprIndex > ()); }
#line 3123 "langutils/sc_parser/src/sc_grammar_parser.cpp"
    break;

  case 93: // expr: expr DOT OPENSQUARE CLOSESQUARE EQUALSSIGN expr
#line 582 "langutils/sc_parser/src/sc_grammar.y"
                { yylhs.value.as < AnyExprIndex > () = cxt.create(AssignmentAtNode{}, yylhs.location, yystack_[5].value.as < AnyExprIndex > (), cxt.create(ArgumentList{}, yystack_[3].location), yystack_[0].value.as < AnyExprIndex > ()); }
#line 3129 "langutils/sc_parser/src/sc_grammar_parser.cpp"
    break;

  case 94: // expr.seq.base: expr
#line 586 "langutils/sc_parser/src/sc_grammar.y"
               { yylhs.value.as < AnyExprIndex > () = yystack_[0].value.as < AnyExprIndex > (); }
#line 3135 "langutils/sc_parser/src/sc_grammar_parser.cpp"
    break;

  case 95: // expr.seq.base: expr.seq.base SEMICOLON expr
#line 588 "langutils/sc_parser/src/sc_grammar.y"
                {
			// This piece of logic is here because exprs can contain expr.seq, so we avoid creating the list node if we can.
			if(auto expr_seq = cxt.graph.as_a<ExprSeqIndex>(*yystack_[2].value.as < AnyExprIndex > ())) {
				cxt.graph.append(*expr_seq, yylhs.location, yystack_[0].value.as < AnyExprIndex > ());
				yylhs.value.as < AnyExprIndex > () = yystack_[2].value.as < AnyExprIndex > ();
			} else {
				yylhs.value.as < AnyExprIndex > () = cxt.create(ExprSeq{}, yylhs.location, yystack_[2].value.as < AnyExprIndex > (), yystack_[0].value.as < AnyExprIndex > ());
			}
		}
#line 3149 "langutils/sc_parser/src/sc_grammar_parser.cpp"
    break;

  case 96: // expr.seq: expr.seq.base semicolon.opt
#line 599 "langutils/sc_parser/src/sc_grammar.y"
                                       { yylhs.value.as < AnyExprIndex > () = yystack_[1].value.as < AnyExprIndex > (); }
#line 3155 "langutils/sc_parser/src/sc_grammar_parser.cpp"
    break;

  case 97: // adverb: DOT name
#line 602 "langutils/sc_parser/src/sc_grammar.y"
                   { yylhs.value.as < AdverbIndex > () = yystack_[0].value.as < NamedIdentifierIndex > (); }
#line 3161 "langutils/sc_parser/src/sc_grammar_parser.cpp"
    break;

  case 98: // adverb: DOT integer
#line 603 "langutils/sc_parser/src/sc_grammar.y"
                      { yylhs.value.as < AdverbIndex > () = yystack_[0].value.as < IntLitIndex > (); }
#line 3167 "langutils/sc_parser/src/sc_grammar_parser.cpp"
    break;

  case 99: // adverb: DOT OPENPAREN expr.seq CLOSEPAREN
#line 604 "langutils/sc_parser/src/sc_grammar.y"
                                            { yylhs.value.as < AdverbIndex > () = cxt.create(AdverbExprNode{}, yylhs.location, yystack_[1].value.as < AnyExprIndex > ());  }
#line 3173 "langutils/sc_parser/src/sc_grammar_parser.cpp"
    break;

  case 100: // argument_declarations.list: name
#line 610 "langutils/sc_parser/src/sc_grammar.y"
                { yylhs.value.as < DeclareArgumentListIndex > () = cxt.create(DeclareArgumentList{}, yylhs.location, yystack_[0].value.as < NamedIdentifierIndex > ()); }
#line 3179 "langutils/sc_parser/src/sc_grammar_parser.cpp"
    break;

  case 101: // argument_declarations.list: name EQUALSSIGN literal
#line 612 "langutils/sc_parser/src/sc_grammar.y"
                { yylhs.value.as < DeclareArgumentListIndex > () = cxt.create(DeclareArgumentList{}, yylhs.location, cxt.create(DeclareArgumentWithDefaultNode{true}, yylhs.location, yystack_[2].value.as < NamedIdentifierIndex > (), yystack_[0].value.as < AnyPossiblyLiteralIndex > ())); }
#line 3185 "langutils/sc_parser/src/sc_grammar_parser.cpp"
    break;

  case 102: // argument_declarations.list: name OPENPAREN expr.seq CLOSEPAREN
#line 614 "langutils/sc_parser/src/sc_grammar.y"
                { yylhs.value.as < DeclareArgumentListIndex > () = cxt.create(DeclareArgumentList{}, yylhs.location, cxt.create(DeclareArgumentWithDefaultNode{false}, yylhs.location, yystack_[3].value.as < NamedIdentifierIndex > (), yystack_[1].value.as < AnyExprIndex > ())); }
#line 3191 "langutils/sc_parser/src/sc_grammar_parser.cpp"
    break;

  case 103: // argument_declarations.list: name EQUALSSIGN OPENPAREN expr.seq CLOSEPAREN
#line 616 "langutils/sc_parser/src/sc_grammar.y"
                { yylhs.value.as < DeclareArgumentListIndex > () = cxt.create(DeclareArgumentList{}, yylhs.location, cxt.create(DeclareArgumentWithDefaultNode{false}, yylhs.location, yystack_[4].value.as < NamedIdentifierIndex > (), yystack_[1].value.as < AnyExprIndex > ())); }
#line 3197 "langutils/sc_parser/src/sc_grammar_parser.cpp"
    break;

  case 104: // argument_declarations.list: argument_declarations.list COMMA name
#line 618 "langutils/sc_parser/src/sc_grammar.y"
                { yylhs.value.as < DeclareArgumentListIndex > () = cxt.graph.append( yystack_[2].value.as < DeclareArgumentListIndex > (), yylhs.location, yystack_[0].value.as < NamedIdentifierIndex > ()); }
#line 3203 "langutils/sc_parser/src/sc_grammar_parser.cpp"
    break;

  case 105: // argument_declarations.list: argument_declarations.list COMMA name EQUALSSIGN literal
#line 620 "langutils/sc_parser/src/sc_grammar.y"
                { yylhs.value.as < DeclareArgumentListIndex > () = cxt.graph.append( yystack_[4].value.as < DeclareArgumentListIndex > (), yylhs.location, cxt.create(DeclareArgumentWithDefaultNode{true}, yylhs.location, yystack_[2].value.as < NamedIdentifierIndex > (), yystack_[0].value.as < AnyPossiblyLiteralIndex > ())); }
#line 3209 "langutils/sc_parser/src/sc_grammar_parser.cpp"
    break;

  case 106: // argument_declarations.list: argument_declarations.list COMMA name OPENPAREN expr.seq CLOSEPAREN
#line 622 "langutils/sc_parser/src/sc_grammar.y"
                { yylhs.value.as < DeclareArgumentListIndex > () = cxt.graph.append( yystack_[5].value.as < DeclareArgumentListIndex > (), yylhs.location, cxt.create(DeclareArgumentWithDefaultNode{false}, yylhs.location, yystack_[3].value.as < NamedIdentifierIndex > (), yystack_[1].value.as < AnyExprIndex > ())); }
#line 3215 "langutils/sc_parser/src/sc_grammar_parser.cpp"
    break;

  case 107: // argument_declarations.list: argument_declarations.list COMMA name EQUALSSIGN OPENPAREN expr.seq CLOSEPAREN
#line 624 "langutils/sc_parser/src/sc_grammar.y"
                { yylhs.value.as < DeclareArgumentListIndex > () = cxt.graph.append( yystack_[6].value.as < DeclareArgumentListIndex > (), yylhs.location, cxt.create(DeclareArgumentWithDefaultNode{false}, yylhs.location, yystack_[4].value.as < NamedIdentifierIndex > (), yystack_[1].value.as < AnyExprIndex > ())); }
#line 3221 "langutils/sc_parser/src/sc_grammar_parser.cpp"
    break;

  case 108: // argument_declarations.pipelist: name literal
#line 629 "langutils/sc_parser/src/sc_grammar.y"
                { yylhs.value.as < DeclareArgumentListIndex > () = cxt.create(DeclareArgumentList{}, yylhs.location, cxt.create(DeclareArgumentWithDefaultNode{true}, yylhs.location, yystack_[1].value.as < NamedIdentifierIndex > (), yystack_[0].value.as < AnyPossiblyLiteralIndex > ())); }
#line 3227 "langutils/sc_parser/src/sc_grammar_parser.cpp"
    break;

  case 109: // argument_declarations.pipelist: name
#line 631 "langutils/sc_parser/src/sc_grammar.y"
                { yylhs.value.as < DeclareArgumentListIndex > () = cxt.create(DeclareArgumentList{}, yylhs.location, yystack_[0].value.as < NamedIdentifierIndex > ()); }
#line 3233 "langutils/sc_parser/src/sc_grammar_parser.cpp"
    break;

  case 110: // argument_declarations.pipelist: name EQUALSSIGN literal
#line 633 "langutils/sc_parser/src/sc_grammar.y"
                { yylhs.value.as < DeclareArgumentListIndex > () = cxt.create(DeclareArgumentList{}, yylhs.location, cxt.create(DeclareArgumentWithDefaultNode{true}, yylhs.location, yystack_[2].value.as < NamedIdentifierIndex > (), yystack_[0].value.as < AnyPossiblyLiteralIndex > ())); }
#line 3239 "langutils/sc_parser/src/sc_grammar_parser.cpp"
    break;

  case 111: // argument_declarations.pipelist: name OPENPAREN expr.seq CLOSEPAREN
#line 635 "langutils/sc_parser/src/sc_grammar.y"
                { yylhs.value.as < DeclareArgumentListIndex > () = cxt.create(DeclareArgumentList{}, yylhs.location, cxt.create(DeclareArgumentWithDefaultNode{false}, yylhs.location, yystack_[3].value.as < NamedIdentifierIndex > (), yystack_[1].value.as < AnyExprIndex > ())); }
#line 3245 "langutils/sc_parser/src/sc_grammar_parser.cpp"
    break;

  case 112: // argument_declarations.pipelist: name EQUALSSIGN OPENPAREN expr.seq CLOSEPAREN
#line 637 "langutils/sc_parser/src/sc_grammar.y"
                { yylhs.value.as < DeclareArgumentListIndex > () = cxt.create(DeclareArgumentList{}, yylhs.location, cxt.create(DeclareArgumentWithDefaultNode{false}, yylhs.location, yystack_[4].value.as < NamedIdentifierIndex > (), yystack_[1].value.as < AnyExprIndex > ())); }
#line 3251 "langutils/sc_parser/src/sc_grammar_parser.cpp"
    break;

  case 113: // argument_declarations.pipelist: argument_declarations.pipelist comma.opt name literal
#line 639 "langutils/sc_parser/src/sc_grammar.y"
                { yylhs.value.as < DeclareArgumentListIndex > () = cxt.graph.append( yystack_[3].value.as < DeclareArgumentListIndex > (), yylhs.location, cxt.create(DeclareArgumentWithDefaultNode{true}, yylhs.location, yystack_[1].value.as < NamedIdentifierIndex > (), yystack_[0].value.as < AnyPossiblyLiteralIndex > ())); }
#line 3257 "langutils/sc_parser/src/sc_grammar_parser.cpp"
    break;

  case 114: // argument_declarations.pipelist: argument_declarations.pipelist comma.opt name
#line 641 "langutils/sc_parser/src/sc_grammar.y"
                { yylhs.value.as < DeclareArgumentListIndex > () = cxt.graph.append( yystack_[2].value.as < DeclareArgumentListIndex > (), yylhs.location, yystack_[0].value.as < NamedIdentifierIndex > ()); }
#line 3263 "langutils/sc_parser/src/sc_grammar_parser.cpp"
    break;

  case 115: // argument_declarations.pipelist: argument_declarations.pipelist comma.opt name EQUALSSIGN literal
#line 643 "langutils/sc_parser/src/sc_grammar.y"
                { yylhs.value.as < DeclareArgumentListIndex > () = cxt.graph.append( yystack_[4].value.as < DeclareArgumentListIndex > (), yylhs.location, cxt.create(DeclareArgumentWithDefaultNode{true}, yylhs.location, yystack_[2].value.as < NamedIdentifierIndex > (), yystack_[0].value.as < AnyPossiblyLiteralIndex > ())); }
#line 3269 "langutils/sc_parser/src/sc_grammar_parser.cpp"
    break;

  case 116: // argument_declarations.pipelist: argument_declarations.pipelist comma.opt name OPENPAREN expr.seq CLOSEPAREN
#line 645 "langutils/sc_parser/src/sc_grammar.y"
                { yylhs.value.as < DeclareArgumentListIndex > () = cxt.graph.append( yystack_[5].value.as < DeclareArgumentListIndex > (), yylhs.location, cxt.create(DeclareArgumentWithDefaultNode{false}, yylhs.location, yystack_[3].value.as < NamedIdentifierIndex > (), yystack_[1].value.as < AnyExprIndex > ())); }
#line 3275 "langutils/sc_parser/src/sc_grammar_parser.cpp"
    break;

  case 117: // argument_declarations.pipelist: argument_declarations.pipelist comma.opt name EQUALSSIGN OPENPAREN expr.seq CLOSEPAREN
#line 647 "langutils/sc_parser/src/sc_grammar.y"
                { yylhs.value.as < DeclareArgumentListIndex > () = cxt.graph.append( yystack_[6].value.as < DeclareArgumentListIndex > (), yylhs.location, cxt.create(DeclareArgumentWithDefaultNode{false}, yylhs.location, yystack_[4].value.as < NamedIdentifierIndex > (), yystack_[1].value.as < AnyExprIndex > ())); }
#line 3281 "langutils/sc_parser/src/sc_grammar_parser.cpp"
    break;

  case 118: // argument_declarations: ARG SEMICOLON
#line 651 "langutils/sc_parser/src/sc_grammar.y"
                        { yylhs.value.as < DeclareArgumentListIndex > () = cxt.create(DeclareArgumentList{}, yylhs.location); }
#line 3287 "langutils/sc_parser/src/sc_grammar_parser.cpp"
    break;

  case 119: // argument_declarations: ARG argument_declarations.list comma.opt SEMICOLON
#line 652 "langutils/sc_parser/src/sc_grammar.y"
                                                             { yylhs.value.as < DeclareArgumentListIndex > () = yystack_[2].value.as < DeclareArgumentListIndex > (); }
#line 3293 "langutils/sc_parser/src/sc_grammar_parser.cpp"
    break;

  case 120: // argument_declarations: ARG argument_declarations.list ELLIPSIS name SEMICOLON
#line 654 "langutils/sc_parser/src/sc_grammar.y"
                { yylhs.value.as < DeclareArgumentListIndex > () = cxt.graph.append( yystack_[3].value.as < DeclareArgumentListIndex > (), yylhs.location, cxt.create(DeclareArgumentVariadicNode{}, yystack_[1].location, yystack_[1].value.as < NamedIdentifierIndex > ())); }
#line 3299 "langutils/sc_parser/src/sc_grammar_parser.cpp"
    break;

  case 121: // argument_declarations: ARG argument_declarations.list ELLIPSIS name COMMA name SEMICOLON
#line 656 "langutils/sc_parser/src/sc_grammar.y"
                { yylhs.value.as < DeclareArgumentListIndex > () = cxt.graph.append( yystack_[5].value.as < DeclareArgumentListIndex > (), yylhs.location, cxt.create(DeclareArgumentVariadicNode{}, yystack_[3].location, yystack_[3].value.as < NamedIdentifierIndex > ()), cxt.create(DeclareArgumentVariadicNode{}, yystack_[1].location, yystack_[1].value.as < NamedIdentifierIndex > ())); }
#line 3305 "langutils/sc_parser/src/sc_grammar_parser.cpp"
    break;

  case 122: // argument_declarations: PIPE PIPE
#line 658 "langutils/sc_parser/src/sc_grammar.y"
                { yylhs.value.as < DeclareArgumentListIndex > () = cxt.create(DeclareArgumentList{}, yylhs.location); }
#line 3311 "langutils/sc_parser/src/sc_grammar_parser.cpp"
    break;

  case 123: // argument_declarations: PIPE argument_declarations.pipelist comma.opt PIPE
#line 660 "langutils/sc_parser/src/sc_grammar.y"
                { yylhs.value.as < DeclareArgumentListIndex > () = yystack_[2].value.as < DeclareArgumentListIndex > (); }
#line 3317 "langutils/sc_parser/src/sc_grammar_parser.cpp"
    break;

  case 124: // argument_declarations: PIPE argument_declarations.pipelist ELLIPSIS name PIPE
#line 662 "langutils/sc_parser/src/sc_grammar.y"
                { yylhs.value.as < DeclareArgumentListIndex > () = cxt.graph.append(yystack_[3].value.as < DeclareArgumentListIndex > (), yystack_[3].location, cxt.create(DeclareArgumentVariadicNode{}, yystack_[1].location, yystack_[1].value.as < NamedIdentifierIndex > ())); }
#line 3323 "langutils/sc_parser/src/sc_grammar_parser.cpp"
    break;

  case 125: // argument_declarations: PIPE argument_declarations.pipelist ELLIPSIS name COMMA name PIPE
#line 664 "langutils/sc_parser/src/sc_grammar.y"
                { yylhs.value.as < DeclareArgumentListIndex > () = cxt.graph.append(yystack_[5].value.as < DeclareArgumentListIndex > (), yylhs.location, cxt.create(DeclareArgumentVariadicNode{}, yystack_[3].location, yystack_[3].value.as < NamedIdentifierIndex > ()), cxt.create(DeclareArgumentVariadicNode{}, yystack_[1].location, yystack_[1].value.as < NamedIdentifierIndex > ())); }
#line 3329 "langutils/sc_parser/src/sc_grammar_parser.cpp"
    break;

  case 126: // argument_declarations.opt: %empty
#line 669 "langutils/sc_parser/src/sc_grammar.y"
                 { yylhs.value.as < DeclareArgumentListIndex > () = cxt.create(DeclareArgumentList{}, yylhs.location); }
#line 3335 "langutils/sc_parser/src/sc_grammar_parser.cpp"
    break;

  case 127: // argument_declarations.opt: argument_declarations
#line 670 "langutils/sc_parser/src/sc_grammar.y"
                                { yylhs.value.as < DeclareArgumentListIndex > () = yystack_[0].value.as < DeclareArgumentListIndex > (); }
#line 3341 "langutils/sc_parser/src/sc_grammar_parser.cpp"
    break;

  case 128: // variable_declarations.list.item: name
#line 675 "langutils/sc_parser/src/sc_grammar.y"
                { yylhs.value.as < DeclareAnyVariableIndex > () = yystack_[0].value.as < NamedIdentifierIndex > (); }
#line 3347 "langutils/sc_parser/src/sc_grammar_parser.cpp"
    break;

  case 129: // variable_declarations.list.item: name EQUALSSIGN expr
#line 677 "langutils/sc_parser/src/sc_grammar.y"
                { yylhs.value.as < DeclareAnyVariableIndex > () = cxt.create(DeclareVariableWithDefaultNode{}, yylhs.location, yystack_[2].value.as < NamedIdentifierIndex > (), yystack_[0].value.as < AnyExprIndex > ()); }
#line 3353 "langutils/sc_parser/src/sc_grammar_parser.cpp"
    break;

  case 130: // variable_declarations.list.item: name OPENPAREN expr.seq CLOSEPAREN
#line 679 "langutils/sc_parser/src/sc_grammar.y"
                { yylhs.value.as < DeclareAnyVariableIndex > () = cxt.create(DeclareVariableWithDefaultNode{}, yylhs.location, yystack_[3].value.as < NamedIdentifierIndex > (), yystack_[1].value.as < AnyExprIndex > ()); }
#line 3359 "langutils/sc_parser/src/sc_grammar_parser.cpp"
    break;

  case 131: // variable_declarations.list: variable_declarations.list.item
#line 688 "langutils/sc_parser/src/sc_grammar.y"
                { yylhs.value.as < DeclareVariableListIndex > () = cxt.create(DeclareVariableList{}, yylhs.location, yystack_[0].value.as < DeclareAnyVariableIndex > ()); }
#line 3365 "langutils/sc_parser/src/sc_grammar_parser.cpp"
    break;

  case 132: // variable_declarations.list: variable_declarations.list COMMA variable_declarations.list.item
#line 690 "langutils/sc_parser/src/sc_grammar.y"
                {yylhs.value.as < DeclareVariableListIndex > () = cxt.graph.append(yystack_[2].value.as < DeclareVariableListIndex > (), yystack_[0].value.as < DeclareAnyVariableIndex > ()); }
#line 3371 "langutils/sc_parser/src/sc_grammar_parser.cpp"
    break;

  case 133: // variable_declarations: VAR variable_declarations.list
#line 693 "langutils/sc_parser/src/sc_grammar.y"
                                                       { yylhs.value.as < DeclareVariableListIndex > () = yystack_[0].value.as < DeclareVariableListIndex > (); }
#line 3377 "langutils/sc_parser/src/sc_grammar_parser.cpp"
    break;

  case 134: // arguments.entries: KEYBINOP expr.seq
#line 697 "langutils/sc_parser/src/sc_grammar.y"
                            { yylhs.value.as < ArgumentEntryIndex > () = cxt.create(KwArgNode{}, yylhs.location, cxt.create(SymbolNode{SymbolNode::Kind::KeyBinOp}, yystack_[1].location), yystack_[0].value.as < AnyExprIndex > ()); }
#line 3383 "langutils/sc_parser/src/sc_grammar_parser.cpp"
    break;

  case 135: // arguments.entries: MULTIPLY expr.seq
#line 698 "langutils/sc_parser/src/sc_grammar.y"
                            { yylhs.value.as < ArgumentEntryIndex > () = cxt.create(VariadicArgNode{}, yylhs.location, yystack_[0].value.as < AnyExprIndex > ()); }
#line 3389 "langutils/sc_parser/src/sc_grammar_parser.cpp"
    break;

  case 136: // arguments.entries: expr.seq
#line 699 "langutils/sc_parser/src/sc_grammar.y"
                   { yylhs.value.as < ArgumentEntryIndex > () = yystack_[0].value.as < AnyExprIndex > (); }
#line 3395 "langutils/sc_parser/src/sc_grammar_parser.cpp"
    break;

  case 137: // arguments.no_trailing: arguments.entries
#line 703 "langutils/sc_parser/src/sc_grammar.y"
                            {  yylhs.value.as < ArgumentListIndex > () = cxt.create(ArgumentList{}, yylhs.location, yystack_[0].value.as < ArgumentEntryIndex > ()); }
#line 3401 "langutils/sc_parser/src/sc_grammar_parser.cpp"
    break;

  case 138: // arguments.no_trailing: arguments.no_trailing COMMA arguments.entries
#line 704 "langutils/sc_parser/src/sc_grammar.y"
                                                        { yylhs.value.as < ArgumentListIndex > () = cxt.graph.append(yystack_[2].value.as < ArgumentListIndex > (), yylhs.location, yystack_[0].value.as < ArgumentEntryIndex > ()); }
#line 3407 "langutils/sc_parser/src/sc_grammar_parser.cpp"
    break;

  case 139: // arguments: arguments.no_trailing comma.opt
#line 707 "langutils/sc_parser/src/sc_grammar.y"
                                            {yylhs.value.as < ArgumentListIndex > () = yystack_[1].value.as < ArgumentListIndex > (); }
#line 3413 "langutils/sc_parser/src/sc_grammar_parser.cpp"
    break;

  case 140: // arguments.paren: OPENPAREN arguments CLOSEPAREN
#line 710 "langutils/sc_parser/src/sc_grammar.y"
                                                 { yylhs.value.as < ArgumentListIndex > () = yystack_[1].value.as < ArgumentListIndex > (); }
#line 3419 "langutils/sc_parser/src/sc_grammar_parser.cpp"
    break;

  case 141: // arguments.maybe_paren: %empty
#line 713 "langutils/sc_parser/src/sc_grammar.y"
                 { yylhs.value.as < ArgumentListIndex > () = cxt.create(ArgumentList{}, yylhs.location); }
#line 3425 "langutils/sc_parser/src/sc_grammar_parser.cpp"
    break;

  case 142: // arguments.maybe_paren: arguments.paren
#line 714 "langutils/sc_parser/src/sc_grammar.y"
                          { yylhs.value.as < ArgumentListIndex > () = yystack_[0].value.as < ArgumentListIndex > (); }
#line 3431 "langutils/sc_parser/src/sc_grammar_parser.cpp"
    break;

  case 143: // literal.terminal: symbol
#line 718 "langutils/sc_parser/src/sc_grammar.y"
                 { yylhs.value.as < AnyPossiblyLiteralIndex > () = yystack_[0].value.as < SymbolLitIndex > (); }
#line 3437 "langutils/sc_parser/src/sc_grammar_parser.cpp"
    break;

  case 144: // literal.terminal: string
#line 719 "langutils/sc_parser/src/sc_grammar.y"
                 { yylhs.value.as < AnyPossiblyLiteralIndex > () = yystack_[0].value.as < StringLitIndex > (); }
#line 3443 "langutils/sc_parser/src/sc_grammar_parser.cpp"
    break;

  case 145: // literal.terminal: integer
#line 720 "langutils/sc_parser/src/sc_grammar.y"
                  { yylhs.value.as < AnyPossiblyLiteralIndex > () = yystack_[0].value.as < IntLitIndex > (); }
#line 3449 "langutils/sc_parser/src/sc_grammar_parser.cpp"
    break;

  case 146: // literal.terminal: float
#line 721 "langutils/sc_parser/src/sc_grammar.y"
                { yylhs.value.as < AnyPossiblyLiteralIndex > () = yystack_[0].value.as < FloatProducingIndex > (); }
#line 3455 "langutils/sc_parser/src/sc_grammar_parser.cpp"
    break;

  case 147: // literal.terminal: boolean
#line 722 "langutils/sc_parser/src/sc_grammar.y"
                  { yylhs.value.as < AnyPossiblyLiteralIndex > () = yystack_[0].value.as < BooleanLitIndex > (); }
#line 3461 "langutils/sc_parser/src/sc_grammar_parser.cpp"
    break;

  case 148: // literal.terminal: nil
#line 723 "langutils/sc_parser/src/sc_grammar.y"
              { yylhs.value.as < AnyPossiblyLiteralIndex > () = yystack_[0].value.as < NilLitIndex > (); }
#line 3467 "langutils/sc_parser/src/sc_grammar_parser.cpp"
    break;

  case 149: // literal.terminal: ascii
#line 724 "langutils/sc_parser/src/sc_grammar.y"
                { yylhs.value.as < AnyPossiblyLiteralIndex > () = yystack_[0].value.as < ASCIIIndex > (); }
#line 3473 "langutils/sc_parser/src/sc_grammar_parser.cpp"
    break;

  case 150: // literal.terminal: block
#line 725 "langutils/sc_parser/src/sc_grammar.y"
                { yylhs.value.as < AnyPossiblyLiteralIndex > () = yystack_[0].value.as < BlockIndex > (); }
#line 3479 "langutils/sc_parser/src/sc_grammar_parser.cpp"
    break;

  case 151: // literal.array.contents: %empty
#line 730 "langutils/sc_parser/src/sc_grammar.y"
                { yylhs.value.as < ArrayIndex > () = cxt.create(ArrayNode{}, yylhs.location); }
#line 3485 "langutils/sc_parser/src/sc_grammar_parser.cpp"
    break;

  case 152: // literal.array.contents: expr.seq
#line 732 "langutils/sc_parser/src/sc_grammar.y"
                { yylhs.value.as < ArrayIndex > () = cxt.create(ArrayNode{}, yylhs.location, yystack_[0].value.as < AnyExprIndex > ()); }
#line 3491 "langutils/sc_parser/src/sc_grammar_parser.cpp"
    break;

  case 153: // literal.array.contents: expr.seq COLON expr.seq
#line 734 "langutils/sc_parser/src/sc_grammar.y"
                { yylhs.value.as < ArrayIndex > () = cxt.create(ArrayNode{}, yylhs.location, yystack_[2].value.as < AnyExprIndex > (), yystack_[0].value.as < AnyExprIndex > ()); }
#line 3497 "langutils/sc_parser/src/sc_grammar_parser.cpp"
    break;

  case 154: // literal.array.contents: KEYBINOP expr.seq
#line 736 "langutils/sc_parser/src/sc_grammar.y"
                { yylhs.value.as < ArrayIndex > () = cxt.create(ArrayNode{}, yylhs.location, cxt.create(SymbolNode{SymbolNode::Kind::KeyBinOp}, yystack_[1].location), yystack_[0].value.as < AnyExprIndex > ()); }
#line 3503 "langutils/sc_parser/src/sc_grammar_parser.cpp"
    break;

  case 155: // literal.array.contents: literal.array.contents COMMA expr.seq
#line 738 "langutils/sc_parser/src/sc_grammar.y"
                { yylhs.value.as < ArrayIndex > () = cxt.graph.append(yystack_[2].value.as < ArrayIndex > (), yylhs.location, yystack_[0].value.as < AnyExprIndex > ()); }
#line 3509 "langutils/sc_parser/src/sc_grammar_parser.cpp"
    break;

  case 156: // literal.array.contents: literal.array.contents COMMA expr.seq COLON expr.seq
#line 740 "langutils/sc_parser/src/sc_grammar.y"
                { yylhs.value.as < ArrayIndex > () = cxt.graph.append(yystack_[4].value.as < ArrayIndex > (), yylhs.location, yystack_[2].value.as < AnyExprIndex > (), yystack_[0].value.as < AnyExprIndex > ()); }
#line 3515 "langutils/sc_parser/src/sc_grammar_parser.cpp"
    break;

  case 157: // literal.array.contents: literal.array.contents COMMA KEYBINOP expr.seq
#line 742 "langutils/sc_parser/src/sc_grammar.y"
                { yylhs.value.as < ArrayIndex > () = cxt.graph.append(yystack_[3].value.as < ArrayIndex > (), yylhs.location, cxt.create(SymbolNode{SymbolNode::Kind::KeyBinOp}, yystack_[1].location), yystack_[0].value.as < AnyExprIndex > ()); }
#line 3521 "langutils/sc_parser/src/sc_grammar_parser.cpp"
    break;

  case 158: // literal.dictionary.entry: expr COLON expr.seq
#line 750 "langutils/sc_parser/src/sc_grammar.y"
                { yylhs.value.as < DictionaryEntryIndex > () = cxt.create(DictionaryEntryNode{}, yylhs.location, yystack_[2].value.as < AnyExprIndex > (), yystack_[0].value.as < AnyExprIndex > ()); }
#line 3527 "langutils/sc_parser/src/sc_grammar_parser.cpp"
    break;

  case 159: // literal.dictionary.entry: KEYBINOP expr.seq
#line 752 "langutils/sc_parser/src/sc_grammar.y"
                { yylhs.value.as < DictionaryEntryIndex > () = cxt.create(DictionaryEntryNode{}, yylhs.location, cxt.create(SymbolNode{SymbolNode::Kind::KeyBinOp}, yystack_[1].location), yystack_[0].value.as < AnyExprIndex > ()); }
#line 3533 "langutils/sc_parser/src/sc_grammar_parser.cpp"
    break;

  case 160: // literal.dictionary.entries: %empty
#line 757 "langutils/sc_parser/src/sc_grammar.y"
                { yylhs.value.as < DictionaryIndex > () = cxt.create(DictionaryNode{}, yylhs.location); }
#line 3539 "langutils/sc_parser/src/sc_grammar_parser.cpp"
    break;

  case 161: // literal.dictionary.entries: literal.dictionary.entry
#line 759 "langutils/sc_parser/src/sc_grammar.y"
                { yylhs.value.as < DictionaryIndex > () = cxt.create(DictionaryNode{}, yylhs.location, yystack_[0].value.as < DictionaryEntryIndex > ()); }
#line 3545 "langutils/sc_parser/src/sc_grammar_parser.cpp"
    break;

  case 162: // literal.dictionary.entries: literal.dictionary.entries COMMA literal.dictionary.entry
#line 761 "langutils/sc_parser/src/sc_grammar.y"
                { yylhs.value.as < DictionaryIndex > () = cxt.graph.append(yystack_[2].value.as < DictionaryIndex > (), yylhs.location,  yystack_[0].value.as < DictionaryEntryIndex > ()); }
#line 3551 "langutils/sc_parser/src/sc_grammar_parser.cpp"
    break;

  case 163: // literal.dictionary: OPENPAREN literal.dictionary.entries comma.opt CLOSEPAREN
#line 766 "langutils/sc_parser/src/sc_grammar.y"
                { cxt.graph.location(*yystack_[2].value.as < DictionaryIndex > ()) = yylhs.location; yylhs.value.as < DictionaryIndex > () = yystack_[2].value.as < DictionaryIndex > (); }
#line 3557 "langutils/sc_parser/src/sc_grammar_parser.cpp"
    break;

  case 164: // literal.array: OPENSQUARE literal.array.contents comma.opt CLOSESQUARE
#line 771 "langutils/sc_parser/src/sc_grammar.y"
                {  cxt.graph.location(*yystack_[2].value.as < ArrayIndex > ()) = yylhs.location; yylhs.value.as < ArrayIndex > () = yystack_[2].value.as < ArrayIndex > (); }
#line 3563 "langutils/sc_parser/src/sc_grammar_parser.cpp"
    break;

  case 165: // literal.array: HASH OPENSQUARE literal.array.contents comma.opt CLOSESQUARE
#line 773 "langutils/sc_parser/src/sc_grammar.y"
                {  
			cxt.graph.payload(yystack_[2].value.as < ArrayIndex > ()).is_immutable = true;
			cxt.graph.location(*yystack_[2].value.as < ArrayIndex > ()) = yylhs.location;
			yylhs.value.as < ArrayIndex > () = yystack_[2].value.as < ArrayIndex > ();
		}
#line 3573 "langutils/sc_parser/src/sc_grammar_parser.cpp"
    break;

  case 166: // literal: literal.terminal
#line 781 "langutils/sc_parser/src/sc_grammar.y"
                           { yylhs.value.as < AnyPossiblyLiteralIndex > () = yystack_[0].value.as < AnyPossiblyLiteralIndex > (); }
#line 3579 "langutils/sc_parser/src/sc_grammar_parser.cpp"
    break;

  case 167: // literal: literal.array
#line 782 "langutils/sc_parser/src/sc_grammar.y"
                        { yylhs.value.as < AnyPossiblyLiteralIndex > () = yystack_[0].value.as < ArrayIndex > (); }
#line 3585 "langutils/sc_parser/src/sc_grammar_parser.cpp"
    break;

  case 168: // literal: literal.dictionary
#line 783 "langutils/sc_parser/src/sc_grammar.y"
                             { yylhs.value.as < AnyPossiblyLiteralIndex > () = yystack_[0].value.as < DictionaryIndex > (); }
#line 3591 "langutils/sc_parser/src/sc_grammar_parser.cpp"
    break;

  case 169: // name: NAME
#line 786 "langutils/sc_parser/src/sc_grammar.y"
            { yylhs.value.as < NamedIdentifierIndex > () = cxt.create(NamedIdentifier{}, yylhs.location); }
#line 3597 "langutils/sc_parser/src/sc_grammar_parser.cpp"
    break;

  case 170: // binary_op.raw: BINOP
#line 795 "langutils/sc_parser/src/sc_grammar.y"
                { yylhs.value.as < SelectorIndex > () = cxt.create(SelectorNode{ false }, yylhs.location); }
#line 3603 "langutils/sc_parser/src/sc_grammar_parser.cpp"
    break;

  case 171: // binary_op.raw: READWRITEVAR
#line 796 "langutils/sc_parser/src/sc_grammar.y"
                       { yylhs.value.as < SelectorIndex > () = cxt.create(SelectorNode{ false }, yylhs.location); }
#line 3609 "langutils/sc_parser/src/sc_grammar_parser.cpp"
    break;

  case 172: // binary_op.raw: LESSTHAN
#line 797 "langutils/sc_parser/src/sc_grammar.y"
                   { yylhs.value.as < SelectorIndex > () = cxt.create(SelectorNode{ false }, yylhs.location); }
#line 3615 "langutils/sc_parser/src/sc_grammar_parser.cpp"
    break;

  case 173: // binary_op.raw: GREATERTHAN
#line 798 "langutils/sc_parser/src/sc_grammar.y"
                      { yylhs.value.as < SelectorIndex > () = cxt.create(SelectorNode{ false }, yylhs.location); }
#line 3621 "langutils/sc_parser/src/sc_grammar_parser.cpp"
    break;

  case 174: // binary_op.raw: MINUS
#line 799 "langutils/sc_parser/src/sc_grammar.y"
                { yylhs.value.as < SelectorIndex > () = cxt.create(SelectorNode{ false }, yylhs.location); }
#line 3627 "langutils/sc_parser/src/sc_grammar_parser.cpp"
    break;

  case 175: // binary_op.raw: MULTIPLY
#line 800 "langutils/sc_parser/src/sc_grammar.y"
                   { yylhs.value.as < SelectorIndex > () = cxt.create(SelectorNode{ false }, yylhs.location); }
#line 3633 "langutils/sc_parser/src/sc_grammar_parser.cpp"
    break;

  case 176: // binary_op.raw: ADD
#line 801 "langutils/sc_parser/src/sc_grammar.y"
              { yylhs.value.as < SelectorIndex > () = cxt.create(SelectorNode{ false }, yylhs.location); }
#line 3639 "langutils/sc_parser/src/sc_grammar_parser.cpp"
    break;

  case 177: // binary_op.raw: PIPE
#line 802 "langutils/sc_parser/src/sc_grammar.y"
               { yylhs.value.as < SelectorIndex > () = cxt.create(SelectorNode{ false }, yylhs.location); }
#line 3645 "langutils/sc_parser/src/sc_grammar_parser.cpp"
    break;

  case 178: // binary_op.no_adverb: binary_op.raw
#line 806 "langutils/sc_parser/src/sc_grammar.y"
                          { yylhs.value.as < SelectorIndex > () = yystack_[0].value.as < SelectorIndex > (); }
#line 3651 "langutils/sc_parser/src/sc_grammar_parser.cpp"
    break;

  case 179: // binary_op.no_adverb: KEYBINOP
#line 807 "langutils/sc_parser/src/sc_grammar.y"
                   { yylhs.value.as < SelectorIndex > () = cxt.create(SelectorNode{ true }, yylhs.location); }
#line 3657 "langutils/sc_parser/src/sc_grammar_parser.cpp"
    break;

  case 180: // binary_op: binary_op.no_adverb adverb
#line 812 "langutils/sc_parser/src/sc_grammar.y"
                { yylhs.value.as < SelectorMaybeAdverbIndex > () = cxt.create(SelectorWAdverb{}, yylhs.location, yystack_[1].value.as < SelectorIndex > (), yystack_[0].value.as < AdverbIndex > ()); }
#line 3663 "langutils/sc_parser/src/sc_grammar_parser.cpp"
    break;

  case 181: // binary_op: binary_op.no_adverb
#line 814 "langutils/sc_parser/src/sc_grammar.y"
                { yylhs.value.as < SelectorMaybeAdverbIndex > () = yystack_[0].value.as < SelectorIndex > (); }
#line 3669 "langutils/sc_parser/src/sc_grammar_parser.cpp"
    break;

  case 186: // ascii: ASCII
#line 820 "langutils/sc_parser/src/sc_grammar.y"
              { yylhs.value.as < ASCIIIndex > () = cxt.create(ASCIINode{}, yylhs.location); }
#line 3675 "langutils/sc_parser/src/sc_grammar_parser.cpp"
    break;

  case 187: // nil: NIL
#line 822 "langutils/sc_parser/src/sc_grammar.y"
          { yylhs.value.as < NilLitIndex > () = cxt.create(NilNode{}, yylhs.location); }
#line 3681 "langutils/sc_parser/src/sc_grammar_parser.cpp"
    break;

  case 188: // boolean: TRUE
#line 825 "langutils/sc_parser/src/sc_grammar.y"
               { yylhs.value.as < BooleanLitIndex > () = cxt.create(BooleanNode{true}, yylhs.location); }
#line 3687 "langutils/sc_parser/src/sc_grammar_parser.cpp"
    break;

  case 189: // boolean: FALSE
#line 826 "langutils/sc_parser/src/sc_grammar.y"
                { yylhs.value.as < BooleanLitIndex > () = cxt.create(BooleanNode{false}, yylhs.location); }
#line 3693 "langutils/sc_parser/src/sc_grammar_parser.cpp"
    break;

  case 190: // symbol: SYMBOL_QUOTE
#line 830 "langutils/sc_parser/src/sc_grammar.y"
                       { yylhs.value.as < SymbolLitIndex > () = cxt.create(SymbolNode{SymbolNode::Kind::Quote}, yylhs.location); }
#line 3699 "langutils/sc_parser/src/sc_grammar_parser.cpp"
    break;

  case 191: // symbol: SYMBOL_SLASH
#line 831 "langutils/sc_parser/src/sc_grammar.y"
                       { yylhs.value.as < SymbolLitIndex > () = cxt.create(SymbolNode{SymbolNode::Kind::Slash}, yylhs.location); }
#line 3705 "langutils/sc_parser/src/sc_grammar_parser.cpp"
    break;

  case 192: // string: STRINGLINE
#line 835 "langutils/sc_parser/src/sc_grammar.y"
                     { yylhs.value.as < StringLitIndex > () = cxt.create(StringLineList{}, yylhs.location, cxt.create(StringLineNode{}, yylhs.location)); }
#line 3711 "langutils/sc_parser/src/sc_grammar_parser.cpp"
    break;

  case 193: // string: string STRINGLINE
#line 836 "langutils/sc_parser/src/sc_grammar.y"
                            { yylhs.value.as < StringLitIndex > () = cxt.graph.append(yystack_[1].value.as < StringLitIndex > (), cxt.create(StringLineNode{}, yystack_[0].location)); }
#line 3717 "langutils/sc_parser/src/sc_grammar_parser.cpp"
    break;

  case 194: // integer: INTEGER
#line 840 "langutils/sc_parser/src/sc_grammar.y"
                  { yylhs.value.as < IntLitIndex > () = cxt.create(IntNode{}, yylhs.location); }
#line 3723 "langutils/sc_parser/src/sc_grammar_parser.cpp"
    break;

  case 195: // integer: INTEGER_RADIX
#line 841 "langutils/sc_parser/src/sc_grammar.y"
                        { yylhs.value.as < IntLitIndex > () = cxt.create(IntNode{IntNode::Kind::Radix}, yylhs.location); }
#line 3729 "langutils/sc_parser/src/sc_grammar_parser.cpp"
    break;

  case 196: // integer: HEXADECIMAL
#line 842 "langutils/sc_parser/src/sc_grammar.y"
                      { yylhs.value.as < IntLitIndex > () = cxt.create(IntNode{IntNode::Kind::Hexadecimal}, yylhs.location); }
#line 3735 "langutils/sc_parser/src/sc_grammar_parser.cpp"
    break;

  case 197: // integer: MINUS integer
#line 844 "langutils/sc_parser/src/sc_grammar.y"
                {
			// Reaches into the previous integer and changes its sign.
			cxt.graph.payload(yystack_[0].value.as < IntLitIndex > ()).sign = IntNode::Sign::Negative;
			yylhs.value.as < IntLitIndex > () = yystack_[0].value.as < IntLitIndex > ();
		}
#line 3745 "langutils/sc_parser/src/sc_grammar_parser.cpp"
    break;

  case 198: // float.raw_unsigned: FLOAT
#line 852 "langutils/sc_parser/src/sc_grammar.y"
                { yylhs.value.as < FloatLitIndex > () = cxt.create(FloatNode{}, yylhs.location); }
#line 3751 "langutils/sc_parser/src/sc_grammar_parser.cpp"
    break;

  case 199: // float.raw_unsigned: FLOAT_RADIX
#line 853 "langutils/sc_parser/src/sc_grammar.y"
                      { yylhs.value.as < FloatLitIndex > () = cxt.create(FloatNode{FloatNode::Kind::Radix}, yylhs.location); }
#line 3757 "langutils/sc_parser/src/sc_grammar_parser.cpp"
    break;

  case 200: // float.raw_unsigned: FLOAT_EXPONENT
#line 854 "langutils/sc_parser/src/sc_grammar.y"
                         { yylhs.value.as < FloatLitIndex > () = cxt.create(FloatNode{FloatNode::Kind::Exponent}, yylhs.location); }
#line 3763 "langutils/sc_parser/src/sc_grammar_parser.cpp"
    break;

  case 201: // float.raw_unsigned: FLOAT_INF
#line 855 "langutils/sc_parser/src/sc_grammar.y"
                    { yylhs.value.as < FloatLitIndex > () = cxt.create(FloatNode{FloatNode::Kind::Inf}, yylhs.location); }
#line 3769 "langutils/sc_parser/src/sc_grammar_parser.cpp"
    break;

  case 202: // float.raw: float.raw_unsigned
#line 860 "langutils/sc_parser/src/sc_grammar.y"
                { yylhs.value.as < FloatLitIndex > () = yystack_[0].value.as < FloatLitIndex > (); }
#line 3775 "langutils/sc_parser/src/sc_grammar_parser.cpp"
    break;

  case 203: // float.raw: MINUS float.raw_unsigned
#line 862 "langutils/sc_parser/src/sc_grammar.y"
                { cxt.graph.payload(yystack_[0].value.as < FloatLitIndex > ()).sign = FloatNode::Sign::Negative; yylhs.value.as < FloatLitIndex > () = yystack_[0].value.as < FloatLitIndex > (); }
#line 3781 "langutils/sc_parser/src/sc_grammar_parser.cpp"
    break;

  case 204: // accidental.unsigned: ACCIDENTAL_STEPS
#line 867 "langutils/sc_parser/src/sc_grammar.y"
                { yylhs.value.as < AccidentalIndex > () = cxt.create(AccidentalNode{AccidentalNode::Kind::Steps}, yylhs.location); }
#line 3787 "langutils/sc_parser/src/sc_grammar_parser.cpp"
    break;

  case 205: // accidental.unsigned: ACCIDENTAL_CENTS
#line 869 "langutils/sc_parser/src/sc_grammar.y"
                { yylhs.value.as < AccidentalIndex > () = cxt.create(AccidentalNode{AccidentalNode::Kind::Cents}, yylhs.location); }
#line 3793 "langutils/sc_parser/src/sc_grammar_parser.cpp"
    break;

  case 206: // accidental: accidental.unsigned
#line 874 "langutils/sc_parser/src/sc_grammar.y"
                { yylhs.value.as < AccidentalIndex > () = yystack_[0].value.as < AccidentalIndex > (); }
#line 3799 "langutils/sc_parser/src/sc_grammar_parser.cpp"
    break;

  case 207: // accidental: MINUS accidental.unsigned
#line 876 "langutils/sc_parser/src/sc_grammar.y"
                { cxt.graph.payload(yystack_[0].value.as < AccidentalIndex > ()).sign = AccidentalNode::Sign::Negative; yylhs.value.as < AccidentalIndex > () = yystack_[0].value.as < AccidentalIndex > (); }
#line 3805 "langutils/sc_parser/src/sc_grammar_parser.cpp"
    break;

  case 208: // float: float.raw
#line 880 "langutils/sc_parser/src/sc_grammar.y"
                    { yylhs.value.as < FloatProducingIndex > () = yystack_[0].value.as < FloatLitIndex > (); }
#line 3811 "langutils/sc_parser/src/sc_grammar_parser.cpp"
    break;

  case 209: // float: accidental
#line 881 "langutils/sc_parser/src/sc_grammar.y"
                     { yylhs.value.as < FloatProducingIndex > () = yystack_[0].value.as < AccidentalIndex > (); }
#line 3817 "langutils/sc_parser/src/sc_grammar_parser.cpp"
    break;

  case 210: // float: float.raw PI
#line 882 "langutils/sc_parser/src/sc_grammar.y"
                       { yylhs.value.as < FloatProducingIndex > () = cxt.create(PiNode{}, yylhs.location, yystack_[1].value.as < FloatLitIndex > ()); }
#line 3823 "langutils/sc_parser/src/sc_grammar_parser.cpp"
    break;

  case 211: // float: integer PI
#line 883 "langutils/sc_parser/src/sc_grammar.y"
                     { yylhs.value.as < FloatProducingIndex > () = cxt.create(PiNode{}, yylhs.location, yystack_[1].value.as < IntLitIndex > ()); }
#line 3829 "langutils/sc_parser/src/sc_grammar_parser.cpp"
    break;

  case 212: // float: PI
#line 884 "langutils/sc_parser/src/sc_grammar.y"
              { yylhs.value.as < FloatProducingIndex > () = cxt.create(PiNode{}, yylhs.location, cxt.create(Missing{}, yylhs.location)); }
#line 3835 "langutils/sc_parser/src/sc_grammar_parser.cpp"
    break;

  case 213: // float: MINUS PI
#line 885 "langutils/sc_parser/src/sc_grammar.y"
                   { yylhs.value.as < FloatProducingIndex > () = cxt.create(PiNode{PiNode::Sign::Negative}, yylhs.location, cxt.create(Missing{}, yylhs.location)); }
#line 3841 "langutils/sc_parser/src/sc_grammar_parser.cpp"
    break;

  case 214: // accessor: %empty
#line 889 "langutils/sc_parser/src/sc_grammar.y"
                 { yylhs.value.as < ReadWriteAccessor > () = ReadWriteAccessor::Private; }
#line 3847 "langutils/sc_parser/src/sc_grammar_parser.cpp"
    break;

  case 215: // accessor: LESSTHAN
#line 890 "langutils/sc_parser/src/sc_grammar.y"
                   { yylhs.value.as < ReadWriteAccessor > () = ReadWriteAccessor::PublicRead; }
#line 3853 "langutils/sc_parser/src/sc_grammar_parser.cpp"
    break;

  case 216: // accessor: READWRITEVAR
#line 891 "langutils/sc_parser/src/sc_grammar.y"
                       { yylhs.value.as < ReadWriteAccessor > () = ReadWriteAccessor::PublicReadAndWrite; }
#line 3859 "langutils/sc_parser/src/sc_grammar_parser.cpp"
    break;

  case 217: // accessor: GREATERTHAN
#line 892 "langutils/sc_parser/src/sc_grammar.y"
                      { yylhs.value.as < ReadWriteAccessor > () = ReadWriteAccessor::PublicWrite; }
#line 3865 "langutils/sc_parser/src/sc_grammar_parser.cpp"
    break;


#line 3869 "langutils/sc_parser/src/sc_grammar_parser.cpp"

            default:
              break;
            }
        }
#if YY_EXCEPTIONS
      catch (const syntax_error& yyexc)
        {
          YYCDEBUG << "Caught exception: " << yyexc.what() << '\n';
          error (yyexc);
          YYERROR;
        }
#endif // YY_EXCEPTIONS
      YY_SYMBOL_PRINT ("-> $$ =", yylhs);
      yypop_ (yylen);
      yylen = 0;

      // Shift the result of the reduction.
      yypush_ (YY_NULLPTR, YY_MOVE (yylhs));
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
    static const char *const yy_sname[] =
    {
    "end of file", "error", "invalid token", "REGION_SEPARATOR",
  "OPENCURLY", "CLOSECURLY", "OPENSQUARE", "CLOSESQUARE", "OPENPAREN",
  "CLOSEPAREN", "SEMICOLON", "NONLOCALRETURN", "COMMA", "HASH", "TILDE",
  "NAME", "INTEGER", "INTEGER_RADIX", "HEXADECIMAL", "FLOAT",
  "FLOAT_RADIX", "FLOAT_EXPONENT", "FLOAT_INF", "ACCIDENTAL_STEPS",
  "ACCIDENTAL_CENTS", "SYMBOL_QUOTE", "SYMBOL_SLASH", "STRINGLINE",
  "ASCII", "PRIMITIVENAME", "CLASSNAME", "CURRYARG", "VAR", "ARG",
  "CLASSVAR", "CONST", "NIL", "TRUE", "FALSE", "PI", "ELLIPSIS", "DOTDOT",
  "BEGINCLOSEDFUNC", "BADTOKEN", "INTERPRET", "LEFTARROW", "LEXER_ERROR",
  "COLON", "EQUALSSIGN", "BINOP", "KEYBINOP", "MINUS", "LESSTHAN",
  "GREATERTHAN", "MULTIPLY", "ADD", "PIPE", "READWRITEVAR", "DOT",
  "BACKTICK", "UMINUS", "$accept", "go", "region.item", "region",
  "classOrExtList.list", "classOrExtList.item", "class", "class.super.opt",
  "class.slot.opt", "class.extension", "class.vars.entry.item",
  "class.vars.entry.list", "class.vars.entry", "class.vars",
  "class.vars.opt", "method.name", "method.base", "method", "method.list",
  "method.list.opt", "block.open", "block", "block.opt_list", "block.list",
  "block.contents", "block.contents.item", "msgsend", "expr.base", "expr",
  "expr.seq.base", "expr.seq", "adverb", "argument_declarations.list",
  "argument_declarations.pipelist", "argument_declarations",
  "argument_declarations.opt", "variable_declarations.list.item",
  "variable_declarations.list", "variable_declarations",
  "arguments.entries", "arguments.no_trailing", "arguments",
  "arguments.paren", "arguments.maybe_paren", "literal.terminal",
  "literal.array.contents", "literal.dictionary.entry",
  "literal.dictionary.entries", "literal.dictionary", "literal.array",
  "literal", "name", "binary_op.raw", "binary_op.no_adverb", "binary_op",
  "semicolon.opt", "comma.opt", "ascii", "nil", "boolean", "symbol",
  "string", "integer", "float.raw_unsigned", "float.raw",
  "accidental.unsigned", "accidental", "float", "accessor", YY_NULLPTR
    };
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








  const short parser::yypact_ninf_ = -193;

  const signed char parser::yytable_ninf_ = -1;

  const short
  parser::yypact_[] =
  {
     122,    20,   351,     6,    38,   181,    19,  -193,  -193,  -193,
      27,    43,  -193,  -193,  1149,   400,    93,    27,  -193,  -193,
    -193,  -193,  -193,  -193,  -193,  -193,  -193,  -193,  -193,  -193,
    -193,  -193,    99,  -193,  -193,  -193,  -193,  -193,  1606,  1293,
       1,  -193,  -193,   100,  1597,  -193,  -193,  -193,  -193,    12,
    -193,  -193,  -193,  -193,    82,    80,  -193,    83,  -193,  -193,
    -193,   121,  -193,  -193,  -193,  1341,  1341,  -193,  -193,   123,
      97,   142,   454,  1293,  1597,   141,   107,   145,  1293,    27,
      37,  -193,  1293,  1606,  -193,  -193,  -193,  -193,    26,  -193,
     164,  -193,  1584,   559,  -193,  -193,   167,  -193,   171,  1149,
     139,  1149,   607,  -193,    58,  -193,   127,  -193,  -193,  -193,
    -193,    26,  -193,   659,   707,  -193,  -193,  -193,   161,   130,
    1293,   756,  1293,    58,  -193,  -193,  -193,   502,   400,  -193,
    1597,  -193,  -193,  -193,   129,  -193,  1293,  -193,  1293,  1197,
     185,  1597,  -193,   182,    17,  -193,    11,    32,  -193,  -193,
      18,  1385,  1052,   186,  1293,  -193,   164,  1597,  1245,   187,
     113,   145,  1293,    21,    58,  1293,  1293,  -193,  -193,   188,
     189,  -193,  -193,   164,   151,   195,  -193,   805,   854,    58,
      36,   120,  -193,   146,    58,   196,  1597,   551,   204,  -193,
    -193,   502,   205,  -193,  -193,   903,   133,   133,   133,  -193,
     202,   502,  1597,  -193,  1293,   168,  -193,    27,  1293,  1293,
      27,    27,   207,  1293,  1459,  -193,    27,    33,  1245,  1496,
    -193,  -193,  -193,  -193,   210,  1293,  1584,  -193,  -193,   951,
      58,   213,  1597,  -193,  1197,  -193,    58,  -193,  -193,  1100,
    -193,    58,   209,  1293,   174,   176,   218,    58,   219,  -193,
    1100,  1293,  -193,    58,  1293,  -193,  -193,    58,    13,  -193,
       1,  -193,  -193,  -193,   163,  -193,  -193,  -193,  -193,   222,
      27,   222,   222,   129,  -193,   226,  -193,  1293,  -193,   227,
    1597,    48,   102,  -193,   228,  1245,  -193,    23,  -193,  1422,
    1584,   231,  1245,  -193,  -193,    58,   233,  -193,  -193,  -193,
    -193,  1597,  1293,  1293,   198,  -193,  -193,  1597,  -193,   234,
    1293,  -193,   510,  -193,  1052,   133,  -193,  -193,  -193,  -193,
    -193,  1293,  1533,  -193,    27,  -193,   235,    27,  -193,  1245,
    1570,  -193,  -193,   239,    58,    58,  1597,  1597,  1293,  -193,
    1597,  -193,  1003,   164,  -193,   243,  1245,  -193,   225,  -193,
     197,   245,  1245,  -193,  -193,  -193,  1597,  -193,   164,   250,
    -193,   248,  -193,  -193,  -193,   249,   254,  -193,  -193,  -193,
    -193
  };

  const unsigned char
  parser::yydefact_[] =
  {
       0,    21,     0,     0,     0,     0,     0,    14,    16,    17,
       0,    19,     8,    46,   151,   160,     0,     0,   169,   194,
     195,   196,   198,   199,   200,   201,   204,   205,   190,   191,
     192,   186,    81,   187,   188,   189,   212,    47,     0,     0,
     126,   150,    75,    80,     7,   166,   168,   167,    73,    74,
     149,   148,   147,   143,   144,   145,   202,   208,   206,   209,
     146,     0,     1,     2,    13,     0,     0,     3,    15,     0,
       0,     0,   160,     0,    94,   182,   152,   184,     0,     0,
       0,   170,   179,   174,   172,   173,   175,   176,   177,   171,
     182,    54,    56,     0,    57,   161,   184,   178,     0,   151,
      77,   151,     0,    52,    70,   213,     0,   197,   203,   207,
      84,     0,   127,     0,     0,   179,   174,   177,     0,   181,
       0,     0,     0,    64,   193,   211,   210,    44,   160,    12,
       4,    11,    22,    20,    32,   154,   183,    96,     0,   185,
       0,    58,   131,   133,   128,   118,   184,   100,   159,   122,
     184,   109,   183,     0,     0,    10,   182,    56,   185,     0,
       0,   184,     0,     0,    50,     0,     0,   136,   137,   184,
       0,    53,    49,   182,    79,     0,    68,     0,     0,    50,
     141,     0,   180,    85,     0,     0,    86,   175,     0,    40,
      42,    45,     0,    34,    35,     0,   214,   214,   214,    30,
     182,    44,    95,   153,     0,   155,   164,     0,     0,     0,
     185,     0,     0,     0,     0,   185,     0,     0,   160,     0,
     108,    55,    76,   158,     0,     0,     0,   162,   163,     0,
      61,     0,    87,    69,     0,    72,    51,   134,   135,   185,
     139,    50,     0,     0,    78,    83,     0,    50,     0,    66,
       0,     0,   142,    50,     0,    97,    98,    63,    50,    41,
     126,    43,    23,     6,     0,   215,   217,   216,    25,    28,
       0,    27,    29,   183,    33,     0,   157,     0,   132,     0,
     129,   104,     0,   119,     0,   160,   101,     0,   123,   114,
      94,     0,   160,   110,     9,     0,     0,   165,   138,    71,
      48,    91,     0,     0,    82,    67,   140,    88,    65,     0,
       0,    62,     0,     5,     0,   214,    24,    31,    18,   156,
     130,     0,     0,   120,     0,   102,     0,     0,   124,   160,
       0,   113,   111,     0,    60,    50,    90,    93,     0,    99,
      89,    37,     0,   182,    26,     0,   160,   105,     0,   103,
       0,     0,   160,   115,   112,    59,    92,    39,   182,     0,
     106,     0,   121,   125,   116,     0,     0,    36,   107,   117,
      38
  };

  const short
  parser::yypgoto_[] =
  {
    -193,  -193,   194,  -193,  -193,   255,  -193,  -193,  -193,  -193,
     -52,  -152,   -11,  -193,  -193,  -193,    78,    75,  -193,    67,
    -193,    66,  -170,   -31,   -91,  -146,  -193,  -193,    -2,  -193,
      -7,  -193,  -193,  -193,    -5,     7,  -192,  -193,  -193,    30,
    -193,   -90,    92,  -193,  -193,   -72,   115,  -193,  -193,  -193,
    -147,    22,  -106,    -4,  -193,   -87,   -53,  -193,  -193,  -193,
    -193,  -193,   -33,   -30,  -193,   -24,  -193,  -193,  -193
  };

  const short
  parser::yydefgoto_[] =
  {
       0,     4,   129,     5,     6,     7,     8,    71,    11,     9,
     268,   269,   199,   200,   201,   188,   189,   190,   191,   192,
      40,    41,   235,   236,    90,    91,    42,    43,    74,    75,
     167,   182,   146,   150,   112,   113,   142,   143,    94,   168,
     169,   248,   179,   253,    45,    77,    95,    96,    46,    47,
      48,    49,    97,   119,   120,   137,   140,    50,    51,    52,
      53,    54,    55,    56,    57,    58,    59,    60,   270
  };

  const short
  parser::yytable_[] =
  {
      44,   104,   156,   153,   220,   107,   221,    76,   108,   249,
      93,    98,   170,    92,   109,   278,    13,    13,   123,    67,
     121,   194,   173,   210,   175,   208,    10,   161,   233,   163,
     215,   185,    69,   234,    80,   327,    61,   110,    62,   100,
     213,    18,    18,   159,   250,   271,   272,   145,    18,     1,
     107,   211,    18,   108,    37,    37,   321,   111,   216,   109,
     122,   310,    13,   130,   130,   209,   135,   286,    98,   224,
      92,   299,   293,   107,     3,   148,   141,   305,   316,   328,
     214,   194,   149,   308,   251,   194,   242,   246,   311,   288,
      70,   157,    76,   212,    76,   194,   322,   217,   103,    99,
      37,   144,   147,    13,   264,   101,   114,   102,   231,   124,
     151,   157,   323,   274,   324,   103,   240,    13,   183,   125,
     186,   229,   126,   195,    98,   127,    92,   133,   254,   230,
     132,   203,   205,   151,   202,    18,    19,    20,    21,   296,
     180,    37,   331,    19,    20,    21,   134,   223,   256,   193,
     157,   136,     1,   257,   138,    37,   226,   139,   237,   238,
     232,   196,   176,   197,   198,   355,     2,   177,   221,   178,
     171,   106,   313,   314,   152,   347,    18,     3,   106,   158,
     160,    63,    64,   353,    65,   265,   266,   162,   181,   171,
     267,    66,   206,   157,   207,   222,   228,   276,   241,   243,
     239,   279,   244,   255,   118,   258,   284,   280,   260,   193,
     262,   291,   273,   193,   300,   277,   290,   283,   148,   294,
     297,   343,   302,   193,   303,   304,   103,   205,   306,   144,
     103,   318,   281,   282,   315,   362,   320,   325,   287,   289,
     332,   301,   335,   339,   349,   103,   338,   309,   354,   307,
     103,   358,   360,   363,   364,   367,   359,   368,   369,   370,
     131,    68,   317,   344,   334,   259,   261,   312,   275,   298,
     319,   366,   252,   227,     0,     0,     0,     0,   326,     0,
       0,     0,     0,   290,     0,   333,     0,     0,     0,     0,
     290,     0,   144,     0,     0,     0,   171,     0,     0,     0,
     336,   337,   171,     0,     0,     0,     0,   103,   340,     0,
     157,     0,   157,   103,   345,     0,     0,     0,     0,   103,
       0,     0,   351,   171,   103,     0,     0,   290,     0,     0,
       0,     0,     0,     0,     0,     0,   356,     0,     0,   361,
     157,     0,     0,     0,   290,   365,   348,     0,     0,   350,
     290,     0,    12,     0,     0,    13,     0,    14,     0,    15,
       0,   103,     0,     0,    16,    17,    18,    19,    20,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
       0,    32,     0,     0,     0,     0,     0,    33,    34,    35,
      36,     0,     0,    37,     0,     0,     0,     0,     0,     0,
     171,   103,    38,     0,    13,     0,    14,     0,    72,     0,
      39,    78,     0,    16,    17,    18,    19,    20,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,     0,
      32,     0,    79,    80,     0,     0,    33,    34,    35,    36,
       0,     0,    37,     0,     0,     0,     0,     0,     0,    81,
      82,    83,    84,    85,    86,    87,    88,    89,    13,    39,
      14,     0,    72,     0,     0,    78,     0,    16,    17,    18,
      19,    20,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,     0,    32,     0,    79,     0,     0,     0,
      33,    34,    35,    36,     0,     0,    37,     0,     0,     0,
       0,     0,     0,    81,    82,    83,    84,    85,    86,    87,
     117,    89,     0,    39,    13,   341,    14,    18,    72,     0,
       0,    78,     0,    16,    17,    18,    19,    20,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,   342,
      32,     0,    79,     0,     0,     0,    33,    34,    35,    36,
       0,    81,    37,   116,    84,    85,   187,    87,   117,    89,
       0,    38,     0,    13,     0,    14,    18,    72,   155,    39,
      78,     0,    16,    17,    18,    19,    20,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,     0,    32,
       0,    79,     0,     0,     0,    33,    34,    35,    36,     0,
      81,    37,   116,    84,    85,    86,    87,   117,    89,     0,
      38,    13,     0,    14,     0,    72,   164,     0,    39,     0,
      16,    17,    18,    19,    20,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,     0,    32,     0,     0,
       0,     0,     0,    33,    34,    35,    36,     0,     0,    37,
       0,     0,     0,     0,     0,     0,     0,   165,    38,     0,
       0,   166,     0,    13,   172,    14,    39,    72,     0,     0,
      78,     0,    16,    17,    18,    19,    20,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,     0,    32,
       0,    79,     0,     0,     0,    33,    34,    35,    36,     0,
       0,    37,     0,     0,     0,     0,     0,     0,     0,     0,
      38,    13,     0,    14,   174,    72,     0,     0,    39,     0,
      16,    17,    18,    19,    20,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,     0,    32,     0,     0,
       0,     0,     0,    33,    34,    35,    36,     0,     0,    37,
       0,     0,     0,     0,     0,     0,     0,   165,    38,     0,
      13,   166,    14,     0,    72,   184,    39,     0,     0,    16,
      17,    18,    19,    20,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,     0,    32,     0,     0,     0,
       0,     0,    33,    34,    35,    36,     0,     0,    37,     0,
       0,     0,     0,     0,     0,     0,   165,    38,     0,    13,
     166,    14,   245,    72,     0,    39,     0,     0,    16,    17,
      18,    19,    20,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,     0,    32,     0,     0,     0,     0,
       0,    33,    34,    35,    36,     0,     0,    37,     0,     0,
       0,     0,     0,     0,     0,   165,    38,     0,    13,   166,
      14,     0,    72,   247,    39,     0,     0,    16,    17,    18,
      19,    20,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,     0,    32,     0,     0,     0,     0,     0,
      33,    34,    35,    36,     0,     0,    37,     0,     0,     0,
       0,     0,     0,     0,   165,    38,     0,    13,   166,    14,
       0,    72,   263,    39,    78,     0,    16,    17,    18,    19,
      20,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,     0,    32,     0,    79,     0,     0,     0,    33,
      34,    35,    36,     0,     0,    37,     0,     0,     0,     0,
       0,     0,     0,     0,    38,    13,     0,    14,     0,    72,
     295,     0,    39,     0,    16,    17,    18,    19,    20,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
       0,    32,     0,     0,     0,     0,     0,    33,    34,    35,
      36,     0,     0,    37,     0,     0,     0,     0,     0,     0,
       0,   165,    38,     0,     0,   166,     0,    13,   357,    14,
      39,    72,     0,     0,    78,     0,    16,    17,    18,    19,
      20,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,     0,    32,     0,    79,     0,     0,     0,    33,
      34,    35,    36,     0,     0,    37,     0,     0,     0,     0,
       0,     0,     0,     0,    38,     0,    13,     0,    14,     0,
      72,     0,    39,    78,     0,    16,    17,    18,    19,    20,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,     0,    32,     0,    79,     0,     0,     0,    33,    34,
      35,    36,     0,     0,    37,     0,     0,     0,     0,     0,
       0,     0,     0,    38,    13,     0,    14,     0,    72,     0,
       0,    39,     0,    16,    17,    18,    19,    20,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,     0,
      32,     0,     0,     0,     0,     0,    33,    34,    35,    36,
       0,     0,    37,     0,     0,     0,     0,     0,     0,     0,
     165,    38,     0,    13,   166,    14,     0,    72,     0,    39,
       0,     0,    16,    17,    18,    19,    20,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,     0,    32,
       0,     0,     0,     0,     0,    33,    34,    35,    36,     0,
       0,    37,     0,     0,     0,     0,     0,     0,     0,    73,
      38,    13,     0,    14,     0,    72,     0,     0,    39,     0,
      16,    17,    18,    19,    20,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,     0,    32,     0,     0,
       0,     0,     0,    33,    34,    35,    36,     0,     0,    37,
       0,     0,     0,     0,     0,     0,     0,   204,    38,    13,
       0,    14,     0,    72,     0,     0,    39,     0,    16,    17,
      18,    19,    20,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,     0,    32,     0,     0,     0,     0,
       0,    33,    34,    35,    36,     0,     0,    37,     0,     0,
       0,     0,     0,     0,     0,   225,    38,    13,     0,    14,
       0,    72,     0,     0,    39,     0,    16,    17,    18,    19,
      20,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,     0,    32,     0,     0,     0,     0,     0,    33,
      34,    35,    36,     0,     0,    37,     0,     0,     0,     0,
       0,     0,     0,     0,    38,    13,     0,    14,     0,   128,
       0,     0,    39,     0,    16,    17,    18,    19,    20,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
       0,    32,     0,     0,     0,     0,     0,    33,    34,    35,
      36,     0,     0,    37,     0,     0,     0,     0,     0,    13,
       0,    14,    38,   218,     0,     0,     0,     0,    16,     0,
      39,    19,    20,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,     0,     0,     0,     0,     0,     0,
       0,    33,    34,    35,    36,     0,    13,    37,    14,     0,
     329,     0,     0,   219,     0,    16,    38,     0,    19,    20,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,     0,     0,     0,     0,     0,     0,     0,    33,    34,
      35,    36,     0,    13,    37,    14,     0,   285,     0,     0,
     330,     0,    16,    38,     0,    19,    20,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,     0,     0,
       0,     0,     0,     0,     0,    33,    34,    35,    36,     0,
      13,    37,    14,     0,   292,     0,     0,     0,     0,    16,
      38,     0,    19,    20,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,     0,     0,     0,     0,     0,
       0,     0,    33,    34,    35,    36,     0,    13,    37,    14,
       0,   346,     0,     0,     0,     0,    16,    38,     0,    19,
      20,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,     0,     0,     0,     0,     0,     0,     0,    33,
      34,    35,    36,     0,    13,    37,    14,     0,   352,     0,
       0,     0,     0,    16,    38,     0,    19,    20,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,     0,
       0,     0,     0,     0,     0,     0,    33,    34,    35,    36,
       0,     0,    37,     0,     0,     0,     0,     0,     0,     0,
       0,    38,    19,    20,    21,    22,    23,    24,    25,    26,
      27,   154,     0,    81,   115,   116,    84,    85,    86,    87,
     117,    89,   118,     0,     0,   105,    81,   115,   116,    84,
      85,    86,    87,   117,    89,   118,     0,   106
  };

  const short
  parser::yycheck_[] =
  {
       2,    32,    93,    90,   151,    38,   152,    14,    38,   179,
      15,    15,   102,    15,    38,   207,     4,     4,    49,     0,
       8,   127,   113,    12,   114,     8,     6,    99,     7,   101,
      12,   121,    10,    12,    33,    12,    30,    39,     0,    17,
       8,    15,    15,    96,     8,   197,   198,    10,    15,    30,
      83,    40,    15,    83,    42,    42,     8,    56,    40,    83,
      48,    48,     4,    65,    66,    48,    73,   214,    72,   156,
      72,   241,   219,   106,    55,    82,    78,   247,   270,    56,
      48,   187,    56,   253,    48,   191,   173,   177,   258,    56,
      47,    93,    99,   146,   101,   201,    48,   150,    32,     6,
      42,    79,    80,     4,   195,     6,     6,     8,   161,    27,
      88,   113,    10,   200,    12,    49,   169,     4,   120,    39,
     122,     8,    39,   128,   128,     4,   128,    30,     8,   160,
       7,   138,   139,   111,   136,    15,    16,    17,    18,   229,
     118,    42,   289,    16,    17,    18,     4,   154,   181,   127,
     152,    10,    30,   184,    47,    42,   158,    12,   165,   166,
     162,    32,     1,    34,    35,   335,    44,     6,   314,     8,
     104,    51,     9,    10,    10,   322,    15,    55,    51,    12,
       9,     0,     1,   330,     3,    52,    53,    48,    58,   123,
      57,    10,     7,   195,    12,     9,     9,   204,     9,    48,
      12,   208,     7,   181,    58,     9,   213,   209,     4,   187,
       5,   218,    10,   191,     5,    47,   218,    10,   225,     9,
       7,   312,    48,   201,    48,     7,   160,   234,     9,   207,
     164,     5,   210,   211,    12,    10,     9,     9,   216,   217,
       9,   243,     9,     9,     9,   179,    48,   254,     9,   251,
     184,   342,     9,    56,     9,     5,   343,     9,     9,     5,
      66,     6,   273,   315,   295,   187,   191,   260,   201,   239,
     277,   358,   180,   158,    -1,    -1,    -1,    -1,   285,    -1,
      -1,    -1,    -1,   285,    -1,   292,    -1,    -1,    -1,    -1,
     292,    -1,   270,    -1,    -1,    -1,   230,    -1,    -1,    -1,
     302,   303,   236,    -1,    -1,    -1,    -1,   241,   310,    -1,
     312,    -1,   314,   247,   321,    -1,    -1,    -1,    -1,   253,
      -1,    -1,   329,   257,   258,    -1,    -1,   329,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   338,    -1,    -1,   346,
     342,    -1,    -1,    -1,   346,   352,   324,    -1,    -1,   327,
     352,    -1,     1,    -1,    -1,     4,    -1,     6,    -1,     8,
      -1,   295,    -1,    -1,    13,    14,    15,    16,    17,    18,
      19,    20,    21,    22,    23,    24,    25,    26,    27,    28,
      -1,    30,    -1,    -1,    -1,    -1,    -1,    36,    37,    38,
      39,    -1,    -1,    42,    -1,    -1,    -1,    -1,    -1,    -1,
     334,   335,    51,    -1,     4,    -1,     6,    -1,     8,    -1,
      59,    11,    -1,    13,    14,    15,    16,    17,    18,    19,
      20,    21,    22,    23,    24,    25,    26,    27,    28,    -1,
      30,    -1,    32,    33,    -1,    -1,    36,    37,    38,    39,
      -1,    -1,    42,    -1,    -1,    -1,    -1,    -1,    -1,    49,
      50,    51,    52,    53,    54,    55,    56,    57,     4,    59,
       6,    -1,     8,    -1,    -1,    11,    -1,    13,    14,    15,
      16,    17,    18,    19,    20,    21,    22,    23,    24,    25,
      26,    27,    28,    -1,    30,    -1,    32,    -1,    -1,    -1,
      36,    37,    38,    39,    -1,    -1,    42,    -1,    -1,    -1,
      -1,    -1,    -1,    49,    50,    51,    52,    53,    54,    55,
      56,    57,    -1,    59,     4,     5,     6,    15,     8,    -1,
      -1,    11,    -1,    13,    14,    15,    16,    17,    18,    19,
      20,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    -1,    32,    -1,    -1,    -1,    36,    37,    38,    39,
      -1,    49,    42,    51,    52,    53,    54,    55,    56,    57,
      -1,    51,    -1,     4,    -1,     6,    15,     8,     9,    59,
      11,    -1,    13,    14,    15,    16,    17,    18,    19,    20,
      21,    22,    23,    24,    25,    26,    27,    28,    -1,    30,
      -1,    32,    -1,    -1,    -1,    36,    37,    38,    39,    -1,
      49,    42,    51,    52,    53,    54,    55,    56,    57,    -1,
      51,     4,    -1,     6,    -1,     8,     9,    -1,    59,    -1,
      13,    14,    15,    16,    17,    18,    19,    20,    21,    22,
      23,    24,    25,    26,    27,    28,    -1,    30,    -1,    -1,
      -1,    -1,    -1,    36,    37,    38,    39,    -1,    -1,    42,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    50,    51,    -1,
      -1,    54,    -1,     4,     5,     6,    59,     8,    -1,    -1,
      11,    -1,    13,    14,    15,    16,    17,    18,    19,    20,
      21,    22,    23,    24,    25,    26,    27,    28,    -1,    30,
      -1,    32,    -1,    -1,    -1,    36,    37,    38,    39,    -1,
      -1,    42,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      51,     4,    -1,     6,     7,     8,    -1,    -1,    59,    -1,
      13,    14,    15,    16,    17,    18,    19,    20,    21,    22,
      23,    24,    25,    26,    27,    28,    -1,    30,    -1,    -1,
      -1,    -1,    -1,    36,    37,    38,    39,    -1,    -1,    42,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    50,    51,    -1,
       4,    54,     6,    -1,     8,     9,    59,    -1,    -1,    13,
      14,    15,    16,    17,    18,    19,    20,    21,    22,    23,
      24,    25,    26,    27,    28,    -1,    30,    -1,    -1,    -1,
      -1,    -1,    36,    37,    38,    39,    -1,    -1,    42,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    50,    51,    -1,     4,
      54,     6,     7,     8,    -1,    59,    -1,    -1,    13,    14,
      15,    16,    17,    18,    19,    20,    21,    22,    23,    24,
      25,    26,    27,    28,    -1,    30,    -1,    -1,    -1,    -1,
      -1,    36,    37,    38,    39,    -1,    -1,    42,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    50,    51,    -1,     4,    54,
       6,    -1,     8,     9,    59,    -1,    -1,    13,    14,    15,
      16,    17,    18,    19,    20,    21,    22,    23,    24,    25,
      26,    27,    28,    -1,    30,    -1,    -1,    -1,    -1,    -1,
      36,    37,    38,    39,    -1,    -1,    42,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    50,    51,    -1,     4,    54,     6,
      -1,     8,     9,    59,    11,    -1,    13,    14,    15,    16,
      17,    18,    19,    20,    21,    22,    23,    24,    25,    26,
      27,    28,    -1,    30,    -1,    32,    -1,    -1,    -1,    36,
      37,    38,    39,    -1,    -1,    42,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    51,     4,    -1,     6,    -1,     8,
       9,    -1,    59,    -1,    13,    14,    15,    16,    17,    18,
      19,    20,    21,    22,    23,    24,    25,    26,    27,    28,
      -1,    30,    -1,    -1,    -1,    -1,    -1,    36,    37,    38,
      39,    -1,    -1,    42,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    50,    51,    -1,    -1,    54,    -1,     4,     5,     6,
      59,     8,    -1,    -1,    11,    -1,    13,    14,    15,    16,
      17,    18,    19,    20,    21,    22,    23,    24,    25,    26,
      27,    28,    -1,    30,    -1,    32,    -1,    -1,    -1,    36,
      37,    38,    39,    -1,    -1,    42,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    51,    -1,     4,    -1,     6,    -1,
       8,    -1,    59,    11,    -1,    13,    14,    15,    16,    17,
      18,    19,    20,    21,    22,    23,    24,    25,    26,    27,
      28,    -1,    30,    -1,    32,    -1,    -1,    -1,    36,    37,
      38,    39,    -1,    -1,    42,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    51,     4,    -1,     6,    -1,     8,    -1,
      -1,    59,    -1,    13,    14,    15,    16,    17,    18,    19,
      20,    21,    22,    23,    24,    25,    26,    27,    28,    -1,
      30,    -1,    -1,    -1,    -1,    -1,    36,    37,    38,    39,
      -1,    -1,    42,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      50,    51,    -1,     4,    54,     6,    -1,     8,    -1,    59,
      -1,    -1,    13,    14,    15,    16,    17,    18,    19,    20,
      21,    22,    23,    24,    25,    26,    27,    28,    -1,    30,
      -1,    -1,    -1,    -1,    -1,    36,    37,    38,    39,    -1,
      -1,    42,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    50,
      51,     4,    -1,     6,    -1,     8,    -1,    -1,    59,    -1,
      13,    14,    15,    16,    17,    18,    19,    20,    21,    22,
      23,    24,    25,    26,    27,    28,    -1,    30,    -1,    -1,
      -1,    -1,    -1,    36,    37,    38,    39,    -1,    -1,    42,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    50,    51,     4,
      -1,     6,    -1,     8,    -1,    -1,    59,    -1,    13,    14,
      15,    16,    17,    18,    19,    20,    21,    22,    23,    24,
      25,    26,    27,    28,    -1,    30,    -1,    -1,    -1,    -1,
      -1,    36,    37,    38,    39,    -1,    -1,    42,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    50,    51,     4,    -1,     6,
      -1,     8,    -1,    -1,    59,    -1,    13,    14,    15,    16,
      17,    18,    19,    20,    21,    22,    23,    24,    25,    26,
      27,    28,    -1,    30,    -1,    -1,    -1,    -1,    -1,    36,
      37,    38,    39,    -1,    -1,    42,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    51,     4,    -1,     6,    -1,     8,
      -1,    -1,    59,    -1,    13,    14,    15,    16,    17,    18,
      19,    20,    21,    22,    23,    24,    25,    26,    27,    28,
      -1,    30,    -1,    -1,    -1,    -1,    -1,    36,    37,    38,
      39,    -1,    -1,    42,    -1,    -1,    -1,    -1,    -1,     4,
      -1,     6,    51,     8,    -1,    -1,    -1,    -1,    13,    -1,
      59,    16,    17,    18,    19,    20,    21,    22,    23,    24,
      25,    26,    27,    28,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    36,    37,    38,    39,    -1,     4,    42,     6,    -1,
       8,    -1,    -1,    48,    -1,    13,    51,    -1,    16,    17,
      18,    19,    20,    21,    22,    23,    24,    25,    26,    27,
      28,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    36,    37,
      38,    39,    -1,     4,    42,     6,    -1,     8,    -1,    -1,
      48,    -1,    13,    51,    -1,    16,    17,    18,    19,    20,
      21,    22,    23,    24,    25,    26,    27,    28,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    36,    37,    38,    39,    -1,
       4,    42,     6,    -1,     8,    -1,    -1,    -1,    -1,    13,
      51,    -1,    16,    17,    18,    19,    20,    21,    22,    23,
      24,    25,    26,    27,    28,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    36,    37,    38,    39,    -1,     4,    42,     6,
      -1,     8,    -1,    -1,    -1,    -1,    13,    51,    -1,    16,
      17,    18,    19,    20,    21,    22,    23,    24,    25,    26,
      27,    28,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    36,
      37,    38,    39,    -1,     4,    42,     6,    -1,     8,    -1,
      -1,    -1,    -1,    13,    51,    -1,    16,    17,    18,    19,
      20,    21,    22,    23,    24,    25,    26,    27,    28,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    36,    37,    38,    39,
      -1,    -1,    42,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    51,    16,    17,    18,    19,    20,    21,    22,    23,
      24,    47,    -1,    49,    50,    51,    52,    53,    54,    55,
      56,    57,    58,    -1,    -1,    39,    49,    50,    51,    52,
      53,    54,    55,    56,    57,    58,    -1,    51
  };

  const unsigned char
  parser::yystos_[] =
  {
       0,    30,    44,    55,    62,    64,    65,    66,    67,    70,
       6,    69,     1,     4,     6,     8,    13,    14,    15,    16,
      17,    18,    19,    20,    21,    22,    23,    24,    25,    26,
      27,    28,    30,    36,    37,    38,    39,    42,    51,    59,
      81,    82,    87,    88,    89,   105,   109,   110,   111,   112,
     118,   119,   120,   121,   122,   123,   124,   125,   126,   127,
     128,    30,     0,     0,     1,     3,    10,     0,    66,   112,
      47,    68,     8,    50,    89,    90,    91,   106,    11,    32,
      33,    49,    50,    51,    52,    53,    54,    55,    56,    57,
      85,    86,    89,    95,    99,   107,   108,   113,   114,     6,
     112,     6,     8,    82,    84,    39,    51,   123,   124,   126,
      89,    56,    95,    96,     6,    50,    51,    56,    58,   114,
     115,     8,    48,    84,    27,    39,    39,     4,     8,    63,
      89,    63,     7,    30,     4,    91,    10,   116,    47,    12,
     117,    89,    97,    98,   112,    10,    93,   112,    91,    56,
      94,   112,    10,   116,    47,     9,    85,    89,    12,   117,
       9,   106,    48,   106,     9,    50,    54,    91,   100,   101,
     102,    82,     5,    85,     7,   102,     1,     6,     8,   103,
     112,    58,    92,    89,     9,   102,    89,    54,    76,    77,
      78,    79,    80,   112,   113,    95,    32,    34,    35,    73,
      74,    75,    89,    91,    50,    91,     7,    12,     8,    48,
      12,    40,   117,     8,    48,    12,    40,   117,     8,    48,
     111,    86,     9,    91,   116,    50,    89,   107,     9,     8,
      84,   117,    89,     7,    12,    83,    84,    91,    91,    12,
     117,     9,   116,    48,     7,     7,   102,     9,   102,    83,
       8,    48,   103,   104,     8,   112,   123,    84,     9,    77,
       4,    78,     5,     9,    85,    52,    53,    57,    71,    72,
     129,    72,    72,    10,   116,    80,    91,    47,    97,    91,
      89,   112,   112,    10,    91,     8,   111,   112,    56,   112,
      89,    91,     8,   111,     9,     9,   102,     7,   100,    83,
       5,    89,    48,    48,     7,    83,     9,    89,    83,    91,
      48,    83,    96,     9,    10,    12,    97,    73,     5,    91,
       9,     8,    48,    10,    12,     9,    91,    12,    56,     8,
      48,   111,     9,    91,    84,     9,    89,    89,    48,     9,
      89,     5,    29,    85,    71,    91,     8,   111,   112,     9,
     112,    91,     8,   111,     9,    83,    89,     5,    85,   116,
       9,    91,    10,    56,     9,    91,   116,     5,     9,     9,
       5
  };

  const unsigned char
  parser::yyr1_[] =
  {
       0,    61,    62,    62,    63,    63,    63,    64,    64,    64,
      64,    64,    64,    64,    65,    65,    66,    66,    67,    68,
      68,    69,    69,    70,    71,    72,    72,    73,    73,    73,
      74,    74,    75,    75,    76,    76,    77,    77,    77,    77,
      78,    78,    79,    79,    80,    80,    81,    81,    82,    82,
      83,    83,    84,    84,    85,    85,    86,    86,    86,    87,
      87,    87,    87,    87,    87,    87,    87,    87,    87,    87,
      87,    87,    87,    88,    88,    88,    88,    88,    88,    88,
      89,    89,    89,    89,    89,    89,    89,    89,    89,    89,
      89,    89,    89,    89,    90,    90,    91,    92,    92,    92,
      93,    93,    93,    93,    93,    93,    93,    93,    94,    94,
      94,    94,    94,    94,    94,    94,    94,    94,    95,    95,
      95,    95,    95,    95,    95,    95,    96,    96,    97,    97,
      97,    98,    98,    99,   100,   100,   100,   101,   101,   102,
     103,   104,   104,   105,   105,   105,   105,   105,   105,   105,
     105,   106,   106,   106,   106,   106,   106,   106,   107,   107,
     108,   108,   108,   109,   110,   110,   111,   111,   111,   112,
     113,   113,   113,   113,   113,   113,   113,   113,   114,   114,
     115,   115,   116,   116,   117,   117,   118,   119,   120,   120,
     121,   121,   122,   122,   123,   123,   123,   123,   124,   124,
     124,   124,   125,   125,   126,   126,   127,   127,   128,   128,
     128,   128,   128,   128,   129,   129,   129,   129
  };

  const signed char
  parser::yyr2_[] =
  {
       0,     2,     2,     2,     1,     4,     3,     2,     2,     6,
       4,     3,     3,     2,     1,     2,     1,     1,     7,     0,
       2,     0,     3,     5,     2,     1,     3,     2,     2,     2,
       1,     3,     0,     2,     1,     1,     6,     4,     7,     5,
       1,     2,     1,     2,     0,     1,     1,     1,     5,     3,
       0,     1,     1,     2,     1,     3,     1,     1,     2,     7,
       6,     4,     5,     4,     2,     5,     4,     5,     3,     4,
       2,     5,     4,     1,     1,     1,     4,     2,     4,     3,
       1,     1,     5,     4,     2,     3,     3,     4,     5,     6,
       6,     5,     7,     6,     1,     3,     2,     2,     2,     4,
       1,     3,     4,     5,     3,     5,     6,     7,     2,     1,
       3,     4,     5,     4,     3,     5,     6,     7,     2,     4,
       5,     7,     2,     4,     5,     7,     0,     1,     1,     3,
       4,     1,     3,     2,     2,     2,     1,     1,     3,     2,
       3,     0,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     0,     1,     3,     2,     3,     5,     4,     3,     2,
       0,     1,     3,     4,     4,     5,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       2,     1,     0,     1,     0,     1,     1,     1,     1,     1,
       1,     1,     1,     2,     1,     1,     1,     2,     1,     1,
       1,     1,     1,     2,     1,     1,     1,     2,     1,     1,
       2,     2,     1,     2,     0,     1,     1,     1
  };




#if YYDEBUG
  const short
  parser::yyrline_[] =
  {
       0,   174,   174,   175,   180,   182,   193,   207,   211,   217,
     229,   243,   246,   249,   275,   277,   282,   283,   287,   292,
     293,   297,   298,   302,   307,   312,   314,   319,   321,   323,
     328,   330,   335,   336,   340,   341,   345,   347,   349,   351,
     356,   357,   362,   363,   367,   368,   371,   371,   374,   376,
     381,   382,   386,   387,   391,   392,   396,   397,   398,   402,
     411,   414,   418,   427,   430,   433,   444,   454,   462,   469,
     472,   478,   489,   499,   501,   503,   504,   509,   514,   521,
     531,   535,   537,   542,   548,   550,   556,   559,   562,   565,
     573,   575,   578,   581,   586,   587,   599,   602,   603,   604,
     609,   611,   613,   615,   617,   619,   621,   623,   628,   630,
     632,   634,   636,   638,   640,   642,   644,   646,   651,   652,
     653,   655,   657,   659,   661,   663,   669,   670,   674,   676,
     678,   687,   689,   693,   697,   698,   699,   703,   704,   707,
     710,   713,   714,   718,   719,   720,   721,   722,   723,   724,
     725,   729,   731,   733,   735,   737,   739,   741,   749,   751,
     756,   758,   760,   765,   770,   772,   781,   782,   783,   786,
     795,   796,   797,   798,   799,   800,   801,   802,   806,   807,
     811,   813,   817,   817,   818,   818,   820,   822,   825,   826,
     830,   831,   835,   836,   840,   841,   842,   843,   852,   853,
     854,   855,   859,   861,   866,   868,   873,   875,   880,   881,
     882,   883,   884,   885,   889,   890,   891,   892
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
#line 4800 "langutils/sc_parser/src/sc_grammar_parser.cpp"

#line 895 "langutils/sc_parser/src/sc_grammar.y"

