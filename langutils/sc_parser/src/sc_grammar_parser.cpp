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

      case symbol_kind::S_127_accidental_unsigned: // accidental.unsigned
      case symbol_kind::S_accidental: // accidental
        value.copy< AccidentalIndex > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_adverb: // adverb
        value.copy< AdverbIndex > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_msgsend: // msgsend
      case symbol_kind::S_89_expr_base: // expr.base
      case symbol_kind::S_expr: // expr
      case symbol_kind::S_91_expr_seq_base: // expr.seq.base
      case symbol_kind::S_92_expr_seq: // expr.seq
          value.copy<AnyExprIndex>(YY_MOVE(that.value));
          break;

      case symbol_kind::S_method: // method
        value.copy< AnyMethodIndex > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_106_literal_terminal: // literal.terminal
      case symbol_kind::S_literal: // literal
          value.copy<AnyPossiblyLiteralIndex>(YY_MOVE(that.value));
          break;

      case symbol_kind::S_101_arguments_entries: // arguments.entries
          value.copy<ArgumentEntryIndex>(YY_MOVE(that.value));
          break;

      case symbol_kind::S_102_arguments_no_trailing: // arguments.no_trailing
      case symbol_kind::S_arguments: // arguments
      case symbol_kind::S_104_arguments_paren: // arguments.paren
      case symbol_kind::S_105_arguments_maybe_paren: // arguments.maybe_paren
          value.copy<ArgumentListIndex>(YY_MOVE(that.value));
          break;

      case symbol_kind::S_107_literal_array_contents: // literal.array.contents
      case symbol_kind::S_111_literal_array: // literal.array
          value.copy<ArrayIndex>(YY_MOVE(that.value));
          break;

      case symbol_kind::S_85_block_contents_no_trailing_return: // block.contents.no_trailing_return
      case symbol_kind::S_86_block_contents: // block.contents
          value.copy<BlockContentsListIndex>(YY_MOVE(that.value));
          break;

      case symbol_kind::S_block: // block
        value.copy< BlockIndex > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_87_block_contents_item: // block.contents.item
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

      case symbol_kind::S_98_variable_declarations_list_item: // variable_declarations.list.item
          value.copy<DeclareAnyVariableIndex>(YY_MOVE(that.value));
          break;

      case symbol_kind::S_94_argument_declarations_list: // argument_declarations.list
      case symbol_kind::S_95_argument_declarations_pipelist: // argument_declarations.pipelist
      case symbol_kind::S_argument_declarations: // argument_declarations
      case symbol_kind::S_97_argument_declarations_opt: // argument_declarations.opt
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

      case symbol_kind::S_99_variable_declarations_list: // variable_declarations.list
      case symbol_kind::S_variable_declarations: // variable_declarations
        value.copy< DeclareVariableListIndex > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_108_literal_dictionary_entry: // literal.dictionary.entry
          value.copy<DictionaryEntryIndex>(YY_MOVE(that.value));
          break;

      case symbol_kind::S_109_literal_dictionary_entries: // literal.dictionary.entries
      case symbol_kind::S_110_literal_dictionary: // literal.dictionary
          value.copy<DictionaryIndex>(YY_MOVE(that.value));
          break;

      case symbol_kind::S_125_float_raw_unsigned: // float.raw_unsigned
      case symbol_kind::S_126_float_raw: // float.raw
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
      case symbol_kind::S_114_binary_op_raw: // binary_op.raw
      case symbol_kind::S_115_binary_op_no_adverb: // binary_op.no_adverb
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

      case symbol_kind::S_127_accidental_unsigned: // accidental.unsigned
      case symbol_kind::S_accidental: // accidental
        value.move< AccidentalIndex > (YY_MOVE (s.value));
        break;

      case symbol_kind::S_adverb: // adverb
        value.move< AdverbIndex > (YY_MOVE (s.value));
        break;

      case symbol_kind::S_msgsend: // msgsend
      case symbol_kind::S_89_expr_base: // expr.base
      case symbol_kind::S_expr: // expr
      case symbol_kind::S_91_expr_seq_base: // expr.seq.base
      case symbol_kind::S_92_expr_seq: // expr.seq
          value.move<AnyExprIndex>(YY_MOVE(s.value));
          break;

      case symbol_kind::S_method: // method
        value.move< AnyMethodIndex > (YY_MOVE (s.value));
        break;

      case symbol_kind::S_106_literal_terminal: // literal.terminal
      case symbol_kind::S_literal: // literal
          value.move<AnyPossiblyLiteralIndex>(YY_MOVE(s.value));
          break;

      case symbol_kind::S_101_arguments_entries: // arguments.entries
          value.move<ArgumentEntryIndex>(YY_MOVE(s.value));
          break;

      case symbol_kind::S_102_arguments_no_trailing: // arguments.no_trailing
      case symbol_kind::S_arguments: // arguments
      case symbol_kind::S_104_arguments_paren: // arguments.paren
      case symbol_kind::S_105_arguments_maybe_paren: // arguments.maybe_paren
          value.move<ArgumentListIndex>(YY_MOVE(s.value));
          break;

      case symbol_kind::S_107_literal_array_contents: // literal.array.contents
      case symbol_kind::S_111_literal_array: // literal.array
          value.move<ArrayIndex>(YY_MOVE(s.value));
          break;

      case symbol_kind::S_85_block_contents_no_trailing_return: // block.contents.no_trailing_return
      case symbol_kind::S_86_block_contents: // block.contents
          value.move<BlockContentsListIndex>(YY_MOVE(s.value));
          break;

      case symbol_kind::S_block: // block
        value.move< BlockIndex > (YY_MOVE (s.value));
        break;

      case symbol_kind::S_87_block_contents_item: // block.contents.item
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

      case symbol_kind::S_98_variable_declarations_list_item: // variable_declarations.list.item
          value.move<DeclareAnyVariableIndex>(YY_MOVE(s.value));
          break;

      case symbol_kind::S_94_argument_declarations_list: // argument_declarations.list
      case symbol_kind::S_95_argument_declarations_pipelist: // argument_declarations.pipelist
      case symbol_kind::S_argument_declarations: // argument_declarations
      case symbol_kind::S_97_argument_declarations_opt: // argument_declarations.opt
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

      case symbol_kind::S_99_variable_declarations_list: // variable_declarations.list
      case symbol_kind::S_variable_declarations: // variable_declarations
        value.move< DeclareVariableListIndex > (YY_MOVE (s.value));
        break;

      case symbol_kind::S_108_literal_dictionary_entry: // literal.dictionary.entry
          value.move<DictionaryEntryIndex>(YY_MOVE(s.value));
          break;

      case symbol_kind::S_109_literal_dictionary_entries: // literal.dictionary.entries
      case symbol_kind::S_110_literal_dictionary: // literal.dictionary
          value.move<DictionaryIndex>(YY_MOVE(s.value));
          break;

      case symbol_kind::S_125_float_raw_unsigned: // float.raw_unsigned
      case symbol_kind::S_126_float_raw: // float.raw
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
      case symbol_kind::S_114_binary_op_raw: // binary_op.raw
      case symbol_kind::S_115_binary_op_no_adverb: // binary_op.no_adverb
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

      case symbol_kind::S_127_accidental_unsigned: // accidental.unsigned
      case symbol_kind::S_accidental: // accidental
        value.YY_MOVE_OR_COPY< AccidentalIndex > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_adverb: // adverb
        value.YY_MOVE_OR_COPY< AdverbIndex > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_msgsend: // msgsend
      case symbol_kind::S_89_expr_base: // expr.base
      case symbol_kind::S_expr: // expr
      case symbol_kind::S_91_expr_seq_base: // expr.seq.base
      case symbol_kind::S_92_expr_seq: // expr.seq
          value.YY_MOVE_OR_COPY<AnyExprIndex>(YY_MOVE(that.value));
          break;

      case symbol_kind::S_method: // method
        value.YY_MOVE_OR_COPY< AnyMethodIndex > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_106_literal_terminal: // literal.terminal
      case symbol_kind::S_literal: // literal
          value.YY_MOVE_OR_COPY<AnyPossiblyLiteralIndex>(YY_MOVE(that.value));
          break;

      case symbol_kind::S_101_arguments_entries: // arguments.entries
          value.YY_MOVE_OR_COPY<ArgumentEntryIndex>(YY_MOVE(that.value));
          break;

      case symbol_kind::S_102_arguments_no_trailing: // arguments.no_trailing
      case symbol_kind::S_arguments: // arguments
      case symbol_kind::S_104_arguments_paren: // arguments.paren
      case symbol_kind::S_105_arguments_maybe_paren: // arguments.maybe_paren
          value.YY_MOVE_OR_COPY<ArgumentListIndex>(YY_MOVE(that.value));
          break;

      case symbol_kind::S_107_literal_array_contents: // literal.array.contents
      case symbol_kind::S_111_literal_array: // literal.array
          value.YY_MOVE_OR_COPY<ArrayIndex>(YY_MOVE(that.value));
          break;

      case symbol_kind::S_85_block_contents_no_trailing_return: // block.contents.no_trailing_return
      case symbol_kind::S_86_block_contents: // block.contents
          value.YY_MOVE_OR_COPY<BlockContentsListIndex>(YY_MOVE(that.value));
          break;

      case symbol_kind::S_block: // block
        value.YY_MOVE_OR_COPY< BlockIndex > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_87_block_contents_item: // block.contents.item
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

      case symbol_kind::S_98_variable_declarations_list_item: // variable_declarations.list.item
          value.YY_MOVE_OR_COPY<DeclareAnyVariableIndex>(YY_MOVE(that.value));
          break;

      case symbol_kind::S_94_argument_declarations_list: // argument_declarations.list
      case symbol_kind::S_95_argument_declarations_pipelist: // argument_declarations.pipelist
      case symbol_kind::S_argument_declarations: // argument_declarations
      case symbol_kind::S_97_argument_declarations_opt: // argument_declarations.opt
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

      case symbol_kind::S_99_variable_declarations_list: // variable_declarations.list
      case symbol_kind::S_variable_declarations: // variable_declarations
        value.YY_MOVE_OR_COPY< DeclareVariableListIndex > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_108_literal_dictionary_entry: // literal.dictionary.entry
          value.YY_MOVE_OR_COPY<DictionaryEntryIndex>(YY_MOVE(that.value));
          break;

      case symbol_kind::S_109_literal_dictionary_entries: // literal.dictionary.entries
      case symbol_kind::S_110_literal_dictionary: // literal.dictionary
          value.YY_MOVE_OR_COPY<DictionaryIndex>(YY_MOVE(that.value));
          break;

      case symbol_kind::S_125_float_raw_unsigned: // float.raw_unsigned
      case symbol_kind::S_126_float_raw: // float.raw
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
      case symbol_kind::S_114_binary_op_raw: // binary_op.raw
      case symbol_kind::S_115_binary_op_no_adverb: // binary_op.no_adverb
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

      case symbol_kind::S_127_accidental_unsigned: // accidental.unsigned
      case symbol_kind::S_accidental: // accidental
        value.move< AccidentalIndex > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_adverb: // adverb
        value.move< AdverbIndex > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_msgsend: // msgsend
      case symbol_kind::S_89_expr_base: // expr.base
      case symbol_kind::S_expr: // expr
      case symbol_kind::S_91_expr_seq_base: // expr.seq.base
      case symbol_kind::S_92_expr_seq: // expr.seq
          value.move<AnyExprIndex>(YY_MOVE(that.value));
          break;

      case symbol_kind::S_method: // method
        value.move< AnyMethodIndex > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_106_literal_terminal: // literal.terminal
      case symbol_kind::S_literal: // literal
          value.move<AnyPossiblyLiteralIndex>(YY_MOVE(that.value));
          break;

      case symbol_kind::S_101_arguments_entries: // arguments.entries
          value.move<ArgumentEntryIndex>(YY_MOVE(that.value));
          break;

      case symbol_kind::S_102_arguments_no_trailing: // arguments.no_trailing
      case symbol_kind::S_arguments: // arguments
      case symbol_kind::S_104_arguments_paren: // arguments.paren
      case symbol_kind::S_105_arguments_maybe_paren: // arguments.maybe_paren
          value.move<ArgumentListIndex>(YY_MOVE(that.value));
          break;

      case symbol_kind::S_107_literal_array_contents: // literal.array.contents
      case symbol_kind::S_111_literal_array: // literal.array
          value.move<ArrayIndex>(YY_MOVE(that.value));
          break;

      case symbol_kind::S_85_block_contents_no_trailing_return: // block.contents.no_trailing_return
      case symbol_kind::S_86_block_contents: // block.contents
          value.move<BlockContentsListIndex>(YY_MOVE(that.value));
          break;

      case symbol_kind::S_block: // block
        value.move< BlockIndex > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_87_block_contents_item: // block.contents.item
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

      case symbol_kind::S_98_variable_declarations_list_item: // variable_declarations.list.item
          value.move<DeclareAnyVariableIndex>(YY_MOVE(that.value));
          break;

      case symbol_kind::S_94_argument_declarations_list: // argument_declarations.list
      case symbol_kind::S_95_argument_declarations_pipelist: // argument_declarations.pipelist
      case symbol_kind::S_argument_declarations: // argument_declarations
      case symbol_kind::S_97_argument_declarations_opt: // argument_declarations.opt
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

      case symbol_kind::S_99_variable_declarations_list: // variable_declarations.list
      case symbol_kind::S_variable_declarations: // variable_declarations
        value.move< DeclareVariableListIndex > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_108_literal_dictionary_entry: // literal.dictionary.entry
          value.move<DictionaryEntryIndex>(YY_MOVE(that.value));
          break;

      case symbol_kind::S_109_literal_dictionary_entries: // literal.dictionary.entries
      case symbol_kind::S_110_literal_dictionary: // literal.dictionary
          value.move<DictionaryIndex>(YY_MOVE(that.value));
          break;

      case symbol_kind::S_125_float_raw_unsigned: // float.raw_unsigned
      case symbol_kind::S_126_float_raw: // float.raw
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
      case symbol_kind::S_114_binary_op_raw: // binary_op.raw
      case symbol_kind::S_115_binary_op_no_adverb: // binary_op.no_adverb
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

      case symbol_kind::S_127_accidental_unsigned: // accidental.unsigned
      case symbol_kind::S_accidental: // accidental
        value.copy< AccidentalIndex > (that.value);
        break;

      case symbol_kind::S_adverb: // adverb
        value.copy< AdverbIndex > (that.value);
        break;

      case symbol_kind::S_msgsend: // msgsend
      case symbol_kind::S_89_expr_base: // expr.base
      case symbol_kind::S_expr: // expr
      case symbol_kind::S_91_expr_seq_base: // expr.seq.base
      case symbol_kind::S_92_expr_seq: // expr.seq
          value.copy<AnyExprIndex>(that.value);
          break;

      case symbol_kind::S_method: // method
        value.copy< AnyMethodIndex > (that.value);
        break;

      case symbol_kind::S_106_literal_terminal: // literal.terminal
      case symbol_kind::S_literal: // literal
          value.copy<AnyPossiblyLiteralIndex>(that.value);
          break;

      case symbol_kind::S_101_arguments_entries: // arguments.entries
          value.copy<ArgumentEntryIndex>(that.value);
          break;

      case symbol_kind::S_102_arguments_no_trailing: // arguments.no_trailing
      case symbol_kind::S_arguments: // arguments
      case symbol_kind::S_104_arguments_paren: // arguments.paren
      case symbol_kind::S_105_arguments_maybe_paren: // arguments.maybe_paren
          value.copy<ArgumentListIndex>(that.value);
          break;

      case symbol_kind::S_107_literal_array_contents: // literal.array.contents
      case symbol_kind::S_111_literal_array: // literal.array
          value.copy<ArrayIndex>(that.value);
          break;

      case symbol_kind::S_85_block_contents_no_trailing_return: // block.contents.no_trailing_return
      case symbol_kind::S_86_block_contents: // block.contents
          value.copy<BlockContentsListIndex>(that.value);
          break;

      case symbol_kind::S_block: // block
        value.copy< BlockIndex > (that.value);
        break;

      case symbol_kind::S_87_block_contents_item: // block.contents.item
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

      case symbol_kind::S_98_variable_declarations_list_item: // variable_declarations.list.item
          value.copy<DeclareAnyVariableIndex>(that.value);
          break;

      case symbol_kind::S_94_argument_declarations_list: // argument_declarations.list
      case symbol_kind::S_95_argument_declarations_pipelist: // argument_declarations.pipelist
      case symbol_kind::S_argument_declarations: // argument_declarations
      case symbol_kind::S_97_argument_declarations_opt: // argument_declarations.opt
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

      case symbol_kind::S_99_variable_declarations_list: // variable_declarations.list
      case symbol_kind::S_variable_declarations: // variable_declarations
        value.copy< DeclareVariableListIndex > (that.value);
        break;

      case symbol_kind::S_108_literal_dictionary_entry: // literal.dictionary.entry
          value.copy<DictionaryEntryIndex>(that.value);
          break;

      case symbol_kind::S_109_literal_dictionary_entries: // literal.dictionary.entries
      case symbol_kind::S_110_literal_dictionary: // literal.dictionary
          value.copy<DictionaryIndex>(that.value);
          break;

      case symbol_kind::S_125_float_raw_unsigned: // float.raw_unsigned
      case symbol_kind::S_126_float_raw: // float.raw
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
      case symbol_kind::S_114_binary_op_raw: // binary_op.raw
      case symbol_kind::S_115_binary_op_no_adverb: // binary_op.no_adverb
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

      case symbol_kind::S_127_accidental_unsigned: // accidental.unsigned
      case symbol_kind::S_accidental: // accidental
        value.move< AccidentalIndex > (that.value);
        break;

      case symbol_kind::S_adverb: // adverb
        value.move< AdverbIndex > (that.value);
        break;

      case symbol_kind::S_msgsend: // msgsend
      case symbol_kind::S_89_expr_base: // expr.base
      case symbol_kind::S_expr: // expr
      case symbol_kind::S_91_expr_seq_base: // expr.seq.base
      case symbol_kind::S_92_expr_seq: // expr.seq
          value.move<AnyExprIndex>(that.value);
          break;

      case symbol_kind::S_method: // method
        value.move< AnyMethodIndex > (that.value);
        break;

      case symbol_kind::S_106_literal_terminal: // literal.terminal
      case symbol_kind::S_literal: // literal
          value.move<AnyPossiblyLiteralIndex>(that.value);
          break;

      case symbol_kind::S_101_arguments_entries: // arguments.entries
          value.move<ArgumentEntryIndex>(that.value);
          break;

      case symbol_kind::S_102_arguments_no_trailing: // arguments.no_trailing
      case symbol_kind::S_arguments: // arguments
      case symbol_kind::S_104_arguments_paren: // arguments.paren
      case symbol_kind::S_105_arguments_maybe_paren: // arguments.maybe_paren
          value.move<ArgumentListIndex>(that.value);
          break;

      case symbol_kind::S_107_literal_array_contents: // literal.array.contents
      case symbol_kind::S_111_literal_array: // literal.array
          value.move<ArrayIndex>(that.value);
          break;

      case symbol_kind::S_85_block_contents_no_trailing_return: // block.contents.no_trailing_return
      case symbol_kind::S_86_block_contents: // block.contents
          value.move<BlockContentsListIndex>(that.value);
          break;

      case symbol_kind::S_block: // block
        value.move< BlockIndex > (that.value);
        break;

      case symbol_kind::S_87_block_contents_item: // block.contents.item
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

      case symbol_kind::S_98_variable_declarations_list_item: // variable_declarations.list.item
          value.move<DeclareAnyVariableIndex>(that.value);
          break;

      case symbol_kind::S_94_argument_declarations_list: // argument_declarations.list
      case symbol_kind::S_95_argument_declarations_pipelist: // argument_declarations.pipelist
      case symbol_kind::S_argument_declarations: // argument_declarations
      case symbol_kind::S_97_argument_declarations_opt: // argument_declarations.opt
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

      case symbol_kind::S_99_variable_declarations_list: // variable_declarations.list
      case symbol_kind::S_variable_declarations: // variable_declarations
        value.move< DeclareVariableListIndex > (that.value);
        break;

      case symbol_kind::S_108_literal_dictionary_entry: // literal.dictionary.entry
          value.move<DictionaryEntryIndex>(that.value);
          break;

      case symbol_kind::S_109_literal_dictionary_entries: // literal.dictionary.entries
      case symbol_kind::S_110_literal_dictionary: // literal.dictionary
          value.move<DictionaryIndex>(that.value);
          break;

      case symbol_kind::S_125_float_raw_unsigned: // float.raw_unsigned
      case symbol_kind::S_126_float_raw: // float.raw
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
      case symbol_kind::S_114_binary_op_raw: // binary_op.raw
      case symbol_kind::S_115_binary_op_no_adverb: // binary_op.no_adverb
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

      case symbol_kind::S_127_accidental_unsigned: // accidental.unsigned
      case symbol_kind::S_accidental: // accidental
        yylhs.value.emplace< AccidentalIndex > ();
        break;

      case symbol_kind::S_adverb: // adverb
        yylhs.value.emplace< AdverbIndex > ();
        break;

      case symbol_kind::S_msgsend: // msgsend
      case symbol_kind::S_89_expr_base: // expr.base
      case symbol_kind::S_expr: // expr
      case symbol_kind::S_91_expr_seq_base: // expr.seq.base
      case symbol_kind::S_92_expr_seq: // expr.seq
          yylhs.value.emplace<AnyExprIndex>();
          break;

      case symbol_kind::S_method: // method
        yylhs.value.emplace< AnyMethodIndex > ();
        break;

      case symbol_kind::S_106_literal_terminal: // literal.terminal
      case symbol_kind::S_literal: // literal
          yylhs.value.emplace<AnyPossiblyLiteralIndex>();
          break;

      case symbol_kind::S_101_arguments_entries: // arguments.entries
          yylhs.value.emplace<ArgumentEntryIndex>();
          break;

      case symbol_kind::S_102_arguments_no_trailing: // arguments.no_trailing
      case symbol_kind::S_arguments: // arguments
      case symbol_kind::S_104_arguments_paren: // arguments.paren
      case symbol_kind::S_105_arguments_maybe_paren: // arguments.maybe_paren
          yylhs.value.emplace<ArgumentListIndex>();
          break;

      case symbol_kind::S_107_literal_array_contents: // literal.array.contents
      case symbol_kind::S_111_literal_array: // literal.array
          yylhs.value.emplace<ArrayIndex>();
          break;

      case symbol_kind::S_85_block_contents_no_trailing_return: // block.contents.no_trailing_return
      case symbol_kind::S_86_block_contents: // block.contents
          yylhs.value.emplace<BlockContentsListIndex>();
          break;

      case symbol_kind::S_block: // block
        yylhs.value.emplace< BlockIndex > ();
        break;

      case symbol_kind::S_87_block_contents_item: // block.contents.item
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

      case symbol_kind::S_98_variable_declarations_list_item: // variable_declarations.list.item
          yylhs.value.emplace<DeclareAnyVariableIndex>();
          break;

      case symbol_kind::S_94_argument_declarations_list: // argument_declarations.list
      case symbol_kind::S_95_argument_declarations_pipelist: // argument_declarations.pipelist
      case symbol_kind::S_argument_declarations: // argument_declarations
      case symbol_kind::S_97_argument_declarations_opt: // argument_declarations.opt
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

      case symbol_kind::S_99_variable_declarations_list: // variable_declarations.list
      case symbol_kind::S_variable_declarations: // variable_declarations
        yylhs.value.emplace< DeclareVariableListIndex > ();
        break;

      case symbol_kind::S_108_literal_dictionary_entry: // literal.dictionary.entry
          yylhs.value.emplace<DictionaryEntryIndex>();
          break;

      case symbol_kind::S_109_literal_dictionary_entries: // literal.dictionary.entries
      case symbol_kind::S_110_literal_dictionary: // literal.dictionary
          yylhs.value.emplace<DictionaryIndex>();
          break;

      case symbol_kind::S_125_float_raw_unsigned: // float.raw_unsigned
      case symbol_kind::S_126_float_raw: // float.raw
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
      case symbol_kind::S_114_binary_op_raw: // binary_op.raw
      case symbol_kind::S_115_binary_op_no_adverb: // binary_op.no_adverb
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
#line 2468 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 3: // go: classOrExtList.list $end
#line 173 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<ClassListOrExprListIndex>() =
                  cxt.graph.assign_root(yystack_[1].value.as<ClassOrExtensionListIndex>());
          }
#line 2474 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 4: // region.item: expr
#line 179 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<error_index<AnyExprIndex>>() = yystack_[0].value.as<AnyExprIndex>();
          }
#line 2480 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 5: // region.item: OPENPAREN argument_declarations block.contents CLOSEPAREN
#line 181 "langutils/sc_parser/src/sc_grammar.y"
          {
              auto block = cxt.create(BlockNode {}, yylhs.location, yystack_[2].value.as<DeclareArgumentListIndex>(),
                                      yystack_[1].value.as<BlockContentsListIndex>());

              yylhs.value.as<error_index<AnyExprIndex>>() = cxt.create(
                  MessageNode { MessageNode::SelectorMode::Value }, yylhs.location,
                  cxt.create(Missing {}, yylhs.location), cxt.create(ArgumentList {}, yylhs.location, block));
          }
#line 2495 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 6: // region.item: OPENPAREN argument_declarations CLOSEPAREN
#line 192 "langutils/sc_parser/src/sc_grammar.y"
          {
              auto block = cxt.create(BlockNode {}, yylhs.location, yystack_[1].value.as<DeclareArgumentListIndex>(),
                                      cxt.create(BlockContentsList {}, yylhs.location));

              yylhs.value.as<error_index<AnyExprIndex>>() = cxt.create(
                  MessageNode { MessageNode::SelectorMode::Value }, yylhs.location,
                  cxt.create(Missing {}, yylhs.location), cxt.create(ArgumentList {}, yylhs.location, block));
          }
#line 2510 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 7: // region: INTERPRET expr
#line 206 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<RegionListIndex>() =
                  cxt.create(RegionList {}, yylhs.location, yystack_[0].value.as<AnyExprIndex>());
          }
#line 2518 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 8: // region: INTERPRET error
#line 210 "langutils/sc_parser/src/sc_grammar.y"
          {
              error_recovery::expr(cxt);
              yyclearin;
              yylhs.value.as<RegionListIndex>() =
                  cxt.create(RegionList {}, yylhs.location, create_error(cxt, yystack_[0].location));
          }
#line 2528 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 9: // region: INTERPRET OPENPAREN argument_declarations block.contents semicolon.opt CLOSEPAREN
#line 216 "langutils/sc_parser/src/sc_grammar.y"
          {
              auto block = cxt.create(BlockNode {}, yylhs.location, yystack_[3].value.as<DeclareArgumentListIndex>(),
                                      yystack_[2].value.as<BlockContentsListIndex>());

              auto msg = cxt.create(MessageNode { MessageNode::SelectorMode::Value }, yylhs.location,
                                    cxt.create(Missing {}, yylhs.location),
                                    cxt.create(ArgumentList {}, yylhs.location, block));
              yylhs.value.as<RegionListIndex>() = cxt.create(RegionList {}, yylhs.location, msg);
          }
#line 2544 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 10: // region: INTERPRET OPENPAREN argument_declarations CLOSEPAREN
#line 228 "langutils/sc_parser/src/sc_grammar.y"
          {
              auto block = cxt.create(BlockNode {}, yylhs.location, yystack_[1].value.as<DeclareArgumentListIndex>(),
                                      cxt.create(BlockContentsList {}, yylhs.location));

              auto msg = cxt.create(MessageNode { MessageNode::SelectorMode::Value }, yylhs.location,
                                    cxt.create(Missing {}, yylhs.location),
                                    cxt.create(ArgumentList {}, yylhs.location, block));

              yylhs.value.as<RegionListIndex>() = cxt.create(RegionList {}, yylhs.location, msg);
          }
#line 2561 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 11: // region: region SEMICOLON region.item
#line 242 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<RegionListIndex>() =
                  cxt.graph.append(yystack_[2].value.as<RegionListIndex>(), yylhs.location,
                                   yystack_[0].value.as<error_index<AnyExprIndex>>());
          }
#line 2567 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 12: // region: region REGION_SEPARATOR region.item
#line 245 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<RegionListIndex>() =
                  cxt.graph.append(yystack_[2].value.as<RegionListIndex>(), yylhs.location,
                                   yystack_[0].value.as<error_index<AnyExprIndex>>());
          }
#line 2573 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 13: // region: region error
#line 248 "langutils/sc_parser/src/sc_grammar.y"
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
#line 2595 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 14: // classOrExtList.list: classOrExtList.item
#line 274 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<ClassOrExtensionListIndex>() =
                  cxt.create(ClassOrExtensionList {}, yylhs.location, yystack_[0].value.as<ClassOrExtensionIndex>());
          }
#line 2601 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 15: // classOrExtList.list: classOrExtList.list classOrExtList.item
#line 276 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<ClassOrExtensionListIndex>() = cxt.graph.append(
                  yystack_[1].value.as<ClassOrExtensionListIndex>(), yystack_[0].value.as<ClassOrExtensionIndex>());
          }
#line 2607 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 16: // classOrExtList.item: class
#line 280 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<ClassOrExtensionIndex>() = yystack_[0].value.as<ClassIndex>();
          }
#line 2613 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 17: // classOrExtList.item: class.extension
#line 281 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<ClassOrExtensionIndex>() = yystack_[0].value.as<ClassExtensionIndex>();
          }
#line 2619 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 18: // class: CLASSNAME class.slot.opt class.super.opt OPENCURLY class.vars.opt method.list.opt
                   // CLOSECURLY
#line 286 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<ClassIndex>() = cxt.create(
                  Class {}, yylhs.location, cxt.create(ClassNameIdentifier {}, yystack_[6].location),
                  yystack_[5].value.as<maybe<NamedIdentifierIndex>>(),
                  yystack_[4].value.as<maybe<ClassNameIdentifierIndex>>(),
                  yystack_[2].value.as<DeclareClassAnyVarListIndex>(), yystack_[1].value.as<MethodListIndex>());
          }
#line 2625 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 19: // class.super.opt: %empty
#line 290 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<maybe<ClassNameIdentifierIndex>>() = cxt.create(Missing {}, yylhs.location);
          }
#line 2631 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 20: // class.super.opt: COLON CLASSNAME
#line 291 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<maybe<ClassNameIdentifierIndex>>() =
                  cxt.create(ClassNameIdentifier {}, yystack_[0].location);
          }
#line 2637 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 21: // class.slot.opt: %empty
#line 295 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<maybe<NamedIdentifierIndex>>() = cxt.create(Missing {}, yylhs.location);
          }
#line 2643 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 22: // class.slot.opt: OPENSQUARE name CLOSESQUARE
#line 296 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<maybe<NamedIdentifierIndex>>() = yystack_[1].value.as<NamedIdentifierIndex>();
          }
#line 2649 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 23: // class.extension: ADD CLASSNAME OPENCURLY method.list.opt CLOSECURLY
#line 301 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<ClassExtensionIndex>() = cxt.create(
                  ClassExtension {}, yylhs.location, cxt.create(ClassNameIdentifier {}, yystack_[3].location),
                  yystack_[1].value.as<MethodListIndex>());
          }
#line 2655 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 24: // class.vars.entry.item: accessor variable_declarations.list.item
#line 306 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<DeclareClassVarIndex>() =
                  cxt.create(DeclareClassVar { yystack_[1].value.as<ReadWriteAccessor>() }, yylhs.location,
                             yystack_[0].value.as<DeclareAnyVariableIndex>());
          }
#line 2661 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 25: // class.vars.entry.list: class.vars.entry.item
#line 311 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<DeclareMemberListIndex>() =
                  cxt.create(DeclareMemberList {}, yylhs.location, yystack_[0].value.as<DeclareClassVarIndex>());
          }
#line 2667 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 26: // class.vars.entry.list: class.vars.entry.list COMMA class.vars.entry.item
#line 313 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<DeclareMemberListIndex>() = cxt.graph.append(
                  yystack_[2].value.as<DeclareMemberListIndex>(), yystack_[0].value.as<DeclareClassVarIndex>());
          }
#line 2673 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 27: // class.vars.entry: CLASSVAR class.vars.entry.list
#line 318 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<DeclareAnyList>() =
                  cxt.graph.cast<DeclareClassMemberList>(yystack_[0].value.as<DeclareMemberListIndex>());
          }
#line 2679 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 28: // class.vars.entry: VAR class.vars.entry.list
#line 320 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<DeclareAnyList>() =
                  cxt.graph.cast<DeclareMemberList>(yystack_[0].value.as<DeclareMemberListIndex>());
          }
#line 2685 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 29: // class.vars.entry: CONST class.vars.entry.list
#line 322 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<DeclareAnyList>() =
                  cxt.graph.cast<DeclareConstList>(yystack_[0].value.as<DeclareMemberListIndex>());
          }
#line 2691 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 30: // class.vars: class.vars.entry
#line 327 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<DeclareClassAnyVarListIndex>() =
                  cxt.create(ClassAnyVarList {}, yylhs.location, yystack_[0].value.as<DeclareAnyList>());
          }
#line 2697 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 31: // class.vars: class.vars SEMICOLON class.vars.entry
#line 329 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<DeclareClassAnyVarListIndex>() = cxt.graph.append(
                  yystack_[2].value.as<DeclareClassAnyVarListIndex>(), yystack_[0].value.as<DeclareAnyList>());
          }
#line 2703 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 32: // class.vars.opt: %empty
#line 333 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<DeclareClassAnyVarListIndex>() = cxt.create(ClassAnyVarList {}, yylhs.location);
          }
#line 2709 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 33: // class.vars.opt: class.vars semicolon.opt
#line 334 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<DeclareClassAnyVarListIndex>() = yystack_[1].value.as<DeclareClassAnyVarListIndex>();
          }
#line 2715 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 34: // method.name: name
#line 338 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<SelectorIndex>() = yystack_[0].value.as<NamedIdentifierIndex>();
          }
#line 2721 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 35: // method.name: binary_op.raw
#line 339 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<SelectorIndex>() = yystack_[0].value.as<SelectorIndex>();
          }
#line 2727 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 36: // method.base: method.name OPENCURLY argument_declarations.opt block.contents semicolon.opt
                   // CLOSECURLY
#line 344 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<MethodIndex>() = cxt.create(
                  Method {}, yylhs.location, yystack_[5].value.as<SelectorIndex>(),
                  yystack_[3].value.as<DeclareArgumentListIndex>(), cxt.create(Missing {}, yystack_[4].location),
                  yystack_[2].value.as<BlockContentsListIndex>());
          }
#line 2733 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 37: // method.base: method.name OPENCURLY argument_declarations.opt CLOSECURLY
#line 346 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<MethodIndex>() = cxt.create(
                  Method {}, yylhs.location, yystack_[3].value.as<SelectorIndex>(),
                  yystack_[1].value.as<DeclareArgumentListIndex>(), cxt.create(Missing {}, yystack_[2].location),
                  cxt.create(BlockList {}, yystack_[0].location));
          }
#line 2739 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 38: // method.base: method.name OPENCURLY argument_declarations.opt PRIMITIVENAME semicolon.opt
                   // block.contents semicolon.opt CLOSECURLY
#line 348 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<MethodIndex>() =
                  cxt.create(Method {}, yylhs.location, yystack_[7].value.as<SelectorIndex>(),
                             yystack_[5].value.as<DeclareArgumentListIndex>(),
                             cxt.create(PrimitiveIdentifier {}, yystack_[4].location),
                             yystack_[2].value.as<BlockContentsListIndex>());
          }
#line 2745 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 39: // method.base: method.name OPENCURLY argument_declarations.opt PRIMITIVENAME semicolon.opt
                   // CLOSECURLY
#line 350 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<MethodIndex>() =
                  cxt.create(Method {}, yylhs.location, yystack_[5].value.as<SelectorIndex>(),
                             yystack_[3].value.as<DeclareArgumentListIndex>(),
                             cxt.create(PrimitiveIdentifier {}, yystack_[2].location),
                             cxt.create(BlockList {}, yystack_[1].location));
          }
#line 2751 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 40: // method: method.base
#line 354 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<AnyMethodIndex>() = yystack_[0].value.as<MethodIndex>();
          }
#line 2757 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 41: // method: MULTIPLY method.base
#line 356 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<AnyMethodIndex>() = cxt.graph.cast<ClassMethod>(yystack_[0].value.as<MethodIndex>());
          }
#line 2763 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 42: // method.list: method
#line 360 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<MethodListIndex>() =
                  cxt.create(MethodList {}, yylhs.location, yystack_[0].value.as<AnyMethodIndex>());
          }
#line 2769 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 43: // method.list: method.list method
#line 361 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<MethodListIndex>() = cxt.graph.append(
                  yystack_[1].value.as<MethodListIndex>(), yylhs.location, yystack_[0].value.as<AnyMethodIndex>());
          }
#line 2775 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 44: // method.list.opt: %empty
#line 365 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<MethodListIndex>() = cxt.create(MethodList {}, yylhs.location);
          }
#line 2781 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 45: // method.list.opt: method.list
#line 366 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<MethodListIndex>() = yystack_[0].value.as<MethodListIndex>();
          }
#line 2787 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 48: // block: block.open argument_declarations.opt block.contents semicolon.opt CLOSECURLY
#line 373 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<BlockIndex>() =
                  cxt.create(BlockNode {}, yylhs.location, yystack_[3].value.as<DeclareArgumentListIndex>(),
                             yystack_[2].value.as<BlockContentsListIndex>());
          }
#line 2793 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 49: // block: block.open argument_declarations.opt CLOSECURLY
#line 375 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<BlockIndex>() =
                  cxt.create(BlockNode {}, yylhs.location, yystack_[1].value.as<DeclareArgumentListIndex>(),
                             cxt.create(BlockContentsList {}, yylhs.location));
          }
#line 2799 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 50: // block.opt_list: %empty
#line 379 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<BlockListIndex>() = {};
          }
#line 2805 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 51: // block.opt_list: block.list
#line 380 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<BlockListIndex>() = yystack_[0].value.as<BlockListIndex>();
          }
#line 2811 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 52: // block.list: block
#line 384 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<BlockListIndex>() =
                  cxt.create(BlockList {}, yylhs.location, yystack_[0].value.as<BlockIndex>());
          }
#line 2817 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 53: // block.list: block.list block
#line 385 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<BlockListIndex>() = cxt.graph.append(yystack_[1].value.as<BlockListIndex>(),
                                                                  yylhs.location, yystack_[0].value.as<BlockIndex>());
          }
#line 2823 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 54: // block.contents.no_trailing_return: block.contents.item
#line 389 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<BlockContentsListIndex>() =
                  cxt.create(BlockContentsList {}, yylhs.location, yystack_[0].value.as<BlockItemIndex>());
          }
#line 2829 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 55: // block.contents.no_trailing_return: block.contents SEMICOLON block.contents.item
#line 390 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<BlockContentsListIndex>() =
                  cxt.graph.append(yystack_[2].value.as<BlockContentsListIndex>(), yylhs.location,
                                   yystack_[0].value.as<BlockItemIndex>());
          }
#line 2835 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 56: // block.contents: block.contents.no_trailing_return
#line 394 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<BlockContentsListIndex>() = yystack_[0].value.as<BlockContentsListIndex>();
          }
#line 2841 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 57: // block.contents: block.contents.no_trailing_return NONLOCALRETURN expr
#line 397 "langutils/sc_parser/src/sc_grammar.y"
          {
              auto non_local =
                  cxt.create(NonLocalReturnExpr {}, yystack_[1].location, yystack_[0].value.as<AnyExprIndex>());
              yylhs.value.as<BlockContentsListIndex>() =
                  cxt.graph.append(yystack_[2].value.as<BlockContentsListIndex>(), yylhs.location, non_local);
          }
#line 2850 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 58: // block.contents.item: expr
#line 404 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<BlockItemIndex>() = yystack_[0].value.as<AnyExprIndex>();
          }
#line 2856 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 59: // block.contents.item: variable_declarations
#line 405 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<BlockItemIndex>() = yystack_[0].value.as<DeclareVariableListIndex>();
          }
#line 2862 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 60: // block.contents.item: NONLOCALRETURN expr
#line 406 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<BlockItemIndex>() =
                  cxt.create(NonLocalReturnExpr {}, yylhs.location, yystack_[0].value.as<AnyExprIndex>());
          }
#line 2868 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 61: // msgsend: OPENPAREN binary_op.no_adverb CLOSEPAREN OPENPAREN arguments CLOSEPAREN block.opt_list
#line 411 "langutils/sc_parser/src/sc_grammar.y"
          {
              if (yystack_[0].value.as<BlockListIndex>()) {
                  cxt.graph.merge(yystack_[2].value.as<ArgumentListIndex>(), yystack_[0].value.as<BlockListIndex>());
                  cxt.graph.location(*yystack_[2].value.as<ArgumentListIndex>()) = {
                      yystack_[2].location.begin, yystack_[0].location.end
                  }; // spans arguments and block list
              }
              yylhs.value.as<AnyExprIndex>() =
                  cxt.create(MessageNode {}, yylhs.location, yystack_[5].value.as<SelectorIndex>(),
                             yystack_[2].value.as<ArgumentListIndex>());
          }
#line 2880 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 62: // msgsend: OPENPAREN binary_op.no_adverb CLOSEPAREN OPENPAREN CLOSEPAREN block.list
#line 420 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<AnyExprIndex>() =
                  cxt.create(MessageNode {}, yylhs.location, yystack_[4].value.as<SelectorIndex>(),
                             yystack_[0].value.as<BlockListIndex>());
          }
#line 2886 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 63: // msgsend: OPENPAREN binary_op.no_adverb CLOSEPAREN block.list
#line 423 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<AnyExprIndex>() =
                  cxt.create(MessageNode {}, yylhs.location, yystack_[2].value.as<SelectorIndex>(),
                             cxt.create(ArgumentList {}, yylhs.location, yystack_[0].value.as<BlockListIndex>()));
          }
#line 2892 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 64: // msgsend: name OPENPAREN arguments CLOSEPAREN block.opt_list
#line 427 "langutils/sc_parser/src/sc_grammar.y"
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
#line 2904 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 65: // msgsend: name OPENPAREN CLOSEPAREN block.list
#line 436 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<AnyExprIndex>() =
                  cxt.create(MessageNode {}, yylhs.location, yystack_[3].value.as<NamedIdentifierIndex>(),
                             yystack_[0].value.as<BlockListIndex>());
          }
#line 2910 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 66: // msgsend: name block.list
#line 439 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<AnyExprIndex>() =
                  cxt.create(MessageNode {}, yylhs.location, yystack_[1].value.as<NamedIdentifierIndex>(),
                             cxt.create(ArgumentList {}, yystack_[0].location, yystack_[0].value.as<BlockListIndex>()));
          }
#line 2916 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 67: // msgsend: expr DOT name arguments.maybe_paren block.opt_list
#line 442 "langutils/sc_parser/src/sc_grammar.y"
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
#line 2930 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 68: // msgsend: expr DOT arguments.paren block.opt_list
#line 453 "langutils/sc_parser/src/sc_grammar.y"
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
#line 2944 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 69: // msgsend: expr DOT OPENPAREN CLOSEPAREN block.opt_list
#line 463 "langutils/sc_parser/src/sc_grammar.y"
          {
              auto args = cxt.create(ArgumentList {}, yylhs.location, yystack_[4].value.as<AnyExprIndex>());
              if (yystack_[0].value.as<BlockListIndex>())
                  cxt.graph.merge(args, yystack_[0].value.as<BlockListIndex>());
              yylhs.value.as<AnyExprIndex>() =
                  cxt.create(MessageNode { MessageNode::SelectorMode::Value }, yylhs.location,
                             cxt.create(Missing {}, yystack_[3].location), args);
          }
#line 2954 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 70: // msgsend: CLASSNAME OPENSQUARE literal.array.contents CLOSESQUARE
#line 478 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<AnyExprIndex>() = cxt.create(CollectionNode {}, yylhs.location,
                                                          cxt.create(ClassNameIdentifier {}, yystack_[3].location),
                                                          yystack_[1].value.as<ArrayIndex>());
          }
#line 2960 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 71: // msgsend: CLASSNAME block.list
#line 481 "langutils/sc_parser/src/sc_grammar.y"
          {
              auto args =
                  cxt.create(ArgumentList {}, yylhs.location, cxt.create(NamedIdentifier {}, yystack_[1].location));
              cxt.graph.merge(args, yystack_[0].value.as<BlockListIndex>());
              yylhs.value.as<AnyExprIndex>() =
                  cxt.create(MessageNode {}, yylhs.location, cxt.create(Missing {}, yystack_[1].location), args);
          }
#line 2970 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 72: // msgsend: CLASSNAME OPENPAREN arguments CLOSEPAREN block.opt_list
#line 487 "langutils/sc_parser/src/sc_grammar.y"
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
#line 2984 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 73: // msgsend: CLASSNAME OPENPAREN CLOSEPAREN block.opt_list
#line 498 "langutils/sc_parser/src/sc_grammar.y"
          {
              auto args = cxt.create(ArgumentList {}, yylhs.location, yystack_[0].value.as<BlockListIndex>());
              cxt.graph.prepend(args,
                                cxt.create(ClassNameIdentifier {}, yystack_[3].location)); // put the receiver in place
              yylhs.value.as<AnyExprIndex>() =
                  cxt.create(MessageNode { MessageNode::SelectorMode::New }, yylhs.location,
                             cxt.create(Missing {}, yystack_[3].location), args);
          }
#line 2994 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 74: // expr.base: literal
#line 507 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<AnyExprIndex>() = yystack_[0].value.as<AnyPossiblyLiteralIndex>();
          }
#line 3000 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 75: // expr.base: name
#line 509 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<AnyExprIndex>() = yystack_[0].value.as<NamedIdentifierIndex>();
          }
#line 3006 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 76: // expr.base: msgsend
#line 511 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<AnyExprIndex>() = yystack_[0].value.as<AnyExprIndex>();
          }
#line 3012 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 77: // expr.base: OPENPAREN block.contents semicolon.opt CLOSEPAREN
#line 513 "langutils/sc_parser/src/sc_grammar.y"
          {
              auto blk = cxt.create(BlockNode {}, yylhs.location, cxt.create(DeclareArgumentList {}, yylhs.location),
                                    yystack_[2].value.as<BlockContentsListIndex>());
              yylhs.value.as<AnyExprIndex>() = cxt.create(
                  MessageNode { MessageNode::SelectorMode::Value }, yylhs.location.flatten(),
                  cxt.create(Missing {}, yylhs.location.flatten()), cxt.create(ArgumentList {}, yylhs.location, blk));
          }
#line 3021 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 78: // expr.base: TILDE name
#line 517 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<AnyExprIndex>() =
                  cxt.create(EnvIdentifierNode {}, yylhs.location, yystack_[0].value.as<NamedIdentifierIndex>());
          }
#line 3027 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 79: // expr.base: expr.base OPENSQUARE arguments CLOSESQUARE
#line 523 "langutils/sc_parser/src/sc_grammar.y"
          {
              cxt.graph.prepend(yystack_[1].value.as<ArgumentListIndex>(),
                                yystack_[3].value.as<AnyExprIndex>()); // put receiver in place.
              cxt.graph.location(*yystack_[1].value.as<ArgumentListIndex>()) = yylhs.location;
              yylhs.value.as<AnyExprIndex>() =
                  cxt.create(MessageNode { MessageNode::SelectorMode::At }, yylhs.location,
                             cxt.create(Missing {}, yystack_[2].location), yystack_[1].value.as<ArgumentListIndex>());
          }
#line 3037 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 80: // expr.base: expr.base OPENSQUARE CLOSESQUARE
#line 530 "langutils/sc_parser/src/sc_grammar.y"
          {
              auto args = cxt.create(ArgumentList {}, yylhs.location, yystack_[2].value.as<AnyExprIndex>());
              yylhs.value.as<AnyExprIndex>() = cxt.create(MessageNode { MessageNode::SelectorMode::At }, yylhs.location,
                                                          cxt.create(Missing {}, yystack_[1].location), args);
          }
#line 3046 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 81: // expr: expr.base
#line 539 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<AnyExprIndex>() = yystack_[0].value.as<AnyExprIndex>();
          }
#line 3052 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 82: // expr: CLASSNAME
#line 543 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<AnyExprIndex>() = cxt.create(ClassNameIdentifier {}, yylhs.location);
          }
#line 3058 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 83: // expr: expr DOT OPENSQUARE arguments CLOSESQUARE
#line 546 "langutils/sc_parser/src/sc_grammar.y"
          {
              cxt.graph.prepend(yystack_[1].value.as<ArgumentListIndex>(),
                                yystack_[4].value.as<AnyExprIndex>()); // put receiver in place
              yylhs.value.as<AnyExprIndex>() =
                  cxt.create(MessageNode { MessageNode::SelectorMode::At }, yylhs.location,
                             cxt.create(Missing {}, yylhs.location), yystack_[1].value.as<ArgumentListIndex>());
          }
#line 3067 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 84: // expr: expr DOT OPENSQUARE CLOSESQUARE
#line 551 "langutils/sc_parser/src/sc_grammar.y"
          {
              auto args = cxt.create(ArgumentList {}, yylhs.location, yystack_[3].value.as<AnyExprIndex>());
              yylhs.value.as<AnyExprIndex>() = cxt.create(MessageNode { MessageNode::SelectorMode::At }, yylhs.location,
                                                          cxt.create(Missing {}, yystack_[1].location), args);
          }
#line 3076 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 85: // expr: BACKTICK expr
#line 556 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<AnyExprIndex>() =
                  cxt.create(ReferenceNode {}, yylhs.location, yystack_[0].value.as<AnyExprIndex>());
          }
#line 3082 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 86: // expr: expr binary_op expr
#line 559 "langutils/sc_parser/src/sc_grammar.y"
          {
              auto args = cxt.create(ArgumentList {}, yylhs.location, yystack_[2].value.as<AnyExprIndex>(),
                                     yystack_[0].value.as<AnyExprIndex>());
              yylhs.value.as<AnyExprIndex>() =
                  cxt.create(MessageNode {}, yylhs.location, yystack_[1].value.as<SelectorMaybeAdverbIndex>(), args);
          }
#line 3091 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 87: // expr: name EQUALSSIGN expr
#line 565 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<AnyExprIndex>() =
                  cxt.create(AssignmentNode {}, yylhs.location, yystack_[2].value.as<NamedIdentifierIndex>(),
                             yystack_[0].value.as<AnyExprIndex>());
          }
#line 3097 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 88: // expr: TILDE name EQUALSSIGN expr
#line 568 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<AnyExprIndex>() =
                  cxt.create(AssignmentNode { AssignmentNode::Target::Environment }, yylhs.location,
                             yystack_[2].value.as<NamedIdentifierIndex>(), yystack_[0].value.as<AnyExprIndex>());
          }
#line 3103 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 89: // expr: expr DOT name EQUALSSIGN expr
#line 571 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<AnyExprIndex>() =
                  cxt.create(SetterNode {}, yylhs.location, yystack_[4].value.as<AnyExprIndex>(),
                             yystack_[2].value.as<NamedIdentifierIndex>(), yystack_[0].value.as<AnyExprIndex>());
          }
#line 3109 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 90: // expr: name OPENPAREN arguments CLOSEPAREN EQUALSSIGN expr
#line 574 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<AnyExprIndex>() =
                  cxt.create(SetterNode {}, yylhs.location, yystack_[3].value.as<ArgumentListIndex>(),
                             yystack_[5].value.as<NamedIdentifierIndex>(), yystack_[0].value.as<AnyExprIndex>());
          }
#line 3115 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 91: // expr: expr.base OPENSQUARE arguments CLOSESQUARE EQUALSSIGN expr
#line 582 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<AnyExprIndex>() =
                  cxt.create(AssignmentAtNode {}, yylhs.location, yystack_[5].value.as<AnyExprIndex>(),
                             yystack_[3].value.as<ArgumentListIndex>(), yystack_[0].value.as<AnyExprIndex>());
          }
#line 3121 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 92: // expr: expr.base OPENSQUARE CLOSESQUARE EQUALSSIGN expr
#line 584 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<AnyExprIndex>() =
                  cxt.create(AssignmentAtNode {}, yylhs.location, yystack_[4].value.as<AnyExprIndex>(),
                             cxt.create(ArgumentList {}, yystack_[3].location), yystack_[0].value.as<AnyExprIndex>());
          }
#line 3127 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 93: // expr: expr DOT OPENSQUARE arguments CLOSESQUARE EQUALSSIGN expr
#line 587 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<AnyExprIndex>() =
                  cxt.create(AssignmentAtNode {}, yylhs.location, yystack_[6].value.as<AnyExprIndex>(),
                             yystack_[3].value.as<ArgumentListIndex>(), yystack_[0].value.as<AnyExprIndex>());
          }
#line 3133 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 94: // expr: expr DOT OPENSQUARE CLOSESQUARE EQUALSSIGN expr
#line 590 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<AnyExprIndex>() =
                  cxt.create(AssignmentAtNode {}, yylhs.location, yystack_[5].value.as<AnyExprIndex>(),
                             cxt.create(ArgumentList {}, yystack_[3].location), yystack_[0].value.as<AnyExprIndex>());
          }
#line 3139 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 95: // expr.seq.base: expr
#line 594 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<AnyExprIndex>() = yystack_[0].value.as<AnyExprIndex>();
          }
#line 3145 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 96: // expr.seq.base: expr.seq.base SEMICOLON expr
#line 596 "langutils/sc_parser/src/sc_grammar.y"
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
#line 3159 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 97: // expr.seq: expr.seq.base semicolon.opt
#line 607 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<AnyExprIndex>() = yystack_[1].value.as<AnyExprIndex>();
          }
#line 3165 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 98: // adverb: DOT name
#line 610 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<AdverbIndex>() = yystack_[0].value.as<NamedIdentifierIndex>();
          }
#line 3171 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 99: // adverb: DOT integer
#line 611 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<AdverbIndex>() = yystack_[0].value.as<IntLitIndex>();
          }
#line 3177 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 100: // adverb: DOT OPENPAREN expr.seq CLOSEPAREN
#line 612 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<AdverbIndex>() =
                  cxt.create(AdverbExprNode {}, yylhs.location, yystack_[1].value.as<AnyExprIndex>());
          }
#line 3183 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 101: // argument_declarations.list: name
#line 618 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<DeclareArgumentListIndex>() =
                  cxt.create(DeclareArgumentList {}, yylhs.location, yystack_[0].value.as<NamedIdentifierIndex>());
          }
#line 3189 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 102: // argument_declarations.list: name EQUALSSIGN literal
#line 620 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<DeclareArgumentListIndex>() =
                  cxt.create(DeclareArgumentList {}, yylhs.location,
                             cxt.create(DeclareArgumentWithDefaultNode { true }, yylhs.location,
                                        yystack_[2].value.as<NamedIdentifierIndex>(),
                                        yystack_[0].value.as<AnyPossiblyLiteralIndex>()));
          }
#line 3195 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 103: // argument_declarations.list: name OPENPAREN expr.seq CLOSEPAREN
#line 622 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<DeclareArgumentListIndex>() = cxt.create(
                  DeclareArgumentList {}, yylhs.location,
                  cxt.create(DeclareArgumentWithDefaultNode { false }, yylhs.location,
                             yystack_[3].value.as<NamedIdentifierIndex>(), yystack_[1].value.as<AnyExprIndex>()));
          }
#line 3201 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 104: // argument_declarations.list: name EQUALSSIGN OPENPAREN expr.seq CLOSEPAREN
#line 624 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<DeclareArgumentListIndex>() = cxt.create(
                  DeclareArgumentList {}, yylhs.location,
                  cxt.create(DeclareArgumentWithDefaultNode { false }, yylhs.location,
                             yystack_[4].value.as<NamedIdentifierIndex>(), yystack_[1].value.as<AnyExprIndex>()));
          }
#line 3207 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 105: // argument_declarations.list: argument_declarations.list COMMA name
#line 626 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<DeclareArgumentListIndex>() =
                  cxt.graph.append(yystack_[2].value.as<DeclareArgumentListIndex>(), yylhs.location,
                                   yystack_[0].value.as<NamedIdentifierIndex>());
          }
#line 3213 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 106: // argument_declarations.list: argument_declarations.list COMMA name EQUALSSIGN literal
#line 628 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<DeclareArgumentListIndex>() =
                  cxt.graph.append(yystack_[4].value.as<DeclareArgumentListIndex>(), yylhs.location,
                                   cxt.create(DeclareArgumentWithDefaultNode { true }, yylhs.location,
                                              yystack_[2].value.as<NamedIdentifierIndex>(),
                                              yystack_[0].value.as<AnyPossiblyLiteralIndex>()));
          }
#line 3219 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 107: // argument_declarations.list: argument_declarations.list COMMA name OPENPAREN expr.seq CLOSEPAREN
#line 630 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<DeclareArgumentListIndex>() = cxt.graph.append(
                  yystack_[5].value.as<DeclareArgumentListIndex>(), yylhs.location,
                  cxt.create(DeclareArgumentWithDefaultNode { false }, yylhs.location,
                             yystack_[3].value.as<NamedIdentifierIndex>(), yystack_[1].value.as<AnyExprIndex>()));
          }
#line 3225 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 108: // argument_declarations.list: argument_declarations.list COMMA name EQUALSSIGN OPENPAREN expr.seq
                    // CLOSEPAREN
#line 632 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<DeclareArgumentListIndex>() = cxt.graph.append(
                  yystack_[6].value.as<DeclareArgumentListIndex>(), yylhs.location,
                  cxt.create(DeclareArgumentWithDefaultNode { false }, yylhs.location,
                             yystack_[4].value.as<NamedIdentifierIndex>(), yystack_[1].value.as<AnyExprIndex>()));
          }
#line 3231 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 109: // argument_declarations.pipelist: name literal
#line 637 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<DeclareArgumentListIndex>() =
                  cxt.create(DeclareArgumentList {}, yylhs.location,
                             cxt.create(DeclareArgumentWithDefaultNode { true }, yylhs.location,
                                        yystack_[1].value.as<NamedIdentifierIndex>(),
                                        yystack_[0].value.as<AnyPossiblyLiteralIndex>()));
          }
#line 3237 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 110: // argument_declarations.pipelist: name
#line 639 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<DeclareArgumentListIndex>() =
                  cxt.create(DeclareArgumentList {}, yylhs.location, yystack_[0].value.as<NamedIdentifierIndex>());
          }
#line 3243 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 111: // argument_declarations.pipelist: name EQUALSSIGN literal
#line 641 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<DeclareArgumentListIndex>() =
                  cxt.create(DeclareArgumentList {}, yylhs.location,
                             cxt.create(DeclareArgumentWithDefaultNode { true }, yylhs.location,
                                        yystack_[2].value.as<NamedIdentifierIndex>(),
                                        yystack_[0].value.as<AnyPossiblyLiteralIndex>()));
          }
#line 3249 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 112: // argument_declarations.pipelist: name OPENPAREN expr.seq CLOSEPAREN
#line 643 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<DeclareArgumentListIndex>() = cxt.create(
                  DeclareArgumentList {}, yylhs.location,
                  cxt.create(DeclareArgumentWithDefaultNode { false }, yylhs.location,
                             yystack_[3].value.as<NamedIdentifierIndex>(), yystack_[1].value.as<AnyExprIndex>()));
          }
#line 3255 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 113: // argument_declarations.pipelist: name EQUALSSIGN OPENPAREN expr.seq CLOSEPAREN
#line 645 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<DeclareArgumentListIndex>() = cxt.create(
                  DeclareArgumentList {}, yylhs.location,
                  cxt.create(DeclareArgumentWithDefaultNode { false }, yylhs.location,
                             yystack_[4].value.as<NamedIdentifierIndex>(), yystack_[1].value.as<AnyExprIndex>()));
          }
#line 3261 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 114: // argument_declarations.pipelist: argument_declarations.pipelist comma.opt name literal
#line 647 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<DeclareArgumentListIndex>() =
                  cxt.graph.append(yystack_[3].value.as<DeclareArgumentListIndex>(), yylhs.location,
                                   cxt.create(DeclareArgumentWithDefaultNode { true }, yylhs.location,
                                              yystack_[1].value.as<NamedIdentifierIndex>(),
                                              yystack_[0].value.as<AnyPossiblyLiteralIndex>()));
          }
#line 3267 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 115: // argument_declarations.pipelist: argument_declarations.pipelist comma.opt name
#line 649 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<DeclareArgumentListIndex>() =
                  cxt.graph.append(yystack_[2].value.as<DeclareArgumentListIndex>(), yylhs.location,
                                   yystack_[0].value.as<NamedIdentifierIndex>());
          }
#line 3273 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 116: // argument_declarations.pipelist: argument_declarations.pipelist comma.opt name EQUALSSIGN literal
#line 651 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<DeclareArgumentListIndex>() =
                  cxt.graph.append(yystack_[4].value.as<DeclareArgumentListIndex>(), yylhs.location,
                                   cxt.create(DeclareArgumentWithDefaultNode { true }, yylhs.location,
                                              yystack_[2].value.as<NamedIdentifierIndex>(),
                                              yystack_[0].value.as<AnyPossiblyLiteralIndex>()));
          }
#line 3279 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 117: // argument_declarations.pipelist: argument_declarations.pipelist comma.opt name OPENPAREN expr.seq
                    // CLOSEPAREN
#line 653 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<DeclareArgumentListIndex>() = cxt.graph.append(
                  yystack_[5].value.as<DeclareArgumentListIndex>(), yylhs.location,
                  cxt.create(DeclareArgumentWithDefaultNode { false }, yylhs.location,
                             yystack_[3].value.as<NamedIdentifierIndex>(), yystack_[1].value.as<AnyExprIndex>()));
          }
#line 3285 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 118: // argument_declarations.pipelist: argument_declarations.pipelist comma.opt name EQUALSSIGN
                    // OPENPAREN expr.seq CLOSEPAREN
#line 655 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<DeclareArgumentListIndex>() = cxt.graph.append(
                  yystack_[6].value.as<DeclareArgumentListIndex>(), yylhs.location,
                  cxt.create(DeclareArgumentWithDefaultNode { false }, yylhs.location,
                             yystack_[4].value.as<NamedIdentifierIndex>(), yystack_[1].value.as<AnyExprIndex>()));
          }
#line 3291 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 119: // argument_declarations: ARG SEMICOLON
#line 659 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<DeclareArgumentListIndex>() = cxt.create(DeclareArgumentList {}, yylhs.location);
          }
#line 3297 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 120: // argument_declarations: ARG argument_declarations.list comma.opt SEMICOLON
#line 660 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<DeclareArgumentListIndex>() = yystack_[2].value.as<DeclareArgumentListIndex>();
          }
#line 3303 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 121: // argument_declarations: ARG argument_declarations.list ELLIPSIS name SEMICOLON
#line 662 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<DeclareArgumentListIndex>() =
                  cxt.graph.append(yystack_[3].value.as<DeclareArgumentListIndex>(), yylhs.location,
                                   cxt.create(DeclareArgumentVariadicNode {}, yystack_[1].location,
                                              yystack_[1].value.as<NamedIdentifierIndex>()));
          }
#line 3309 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 122: // argument_declarations: ARG argument_declarations.list ELLIPSIS name COMMA name SEMICOLON
#line 664 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<DeclareArgumentListIndex>() =
                  cxt.graph.append(yystack_[5].value.as<DeclareArgumentListIndex>(), yylhs.location,
                                   cxt.create(DeclareArgumentVariadicNode {}, yystack_[3].location,
                                              yystack_[3].value.as<NamedIdentifierIndex>()),
                                   cxt.create(DeclareArgumentVariadicNode {}, yystack_[1].location,
                                              yystack_[1].value.as<NamedIdentifierIndex>()));
          }
#line 3315 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 123: // argument_declarations: ARG ELLIPSIS name SEMICOLON
#line 666 "langutils/sc_parser/src/sc_grammar.y"
          {
              auto arg_list = cxt.create(DeclareArgumentList {}, yylhs.location);
              yylhs.value.as<DeclareArgumentListIndex>() =
                  cxt.graph.append(arg_list, yylhs.location,
                                   cxt.create(DeclareArgumentVariadicNode {}, yystack_[1].location,
                                              yystack_[1].value.as<NamedIdentifierIndex>()));
          }
#line 3324 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 124: // argument_declarations: ARG ELLIPSIS name COMMA name SEMICOLON
#line 671 "langutils/sc_parser/src/sc_grammar.y"
          {
              auto arg_list = cxt.create(DeclareArgumentList {}, yylhs.location);
              yylhs.value.as<DeclareArgumentListIndex>() =
                  cxt.graph.append(arg_list, yylhs.location,
                                   cxt.create(DeclareArgumentVariadicNode {}, yystack_[3].location,
                                              yystack_[3].value.as<NamedIdentifierIndex>()),
                                   cxt.create(DeclareArgumentVariadicNode {}, yystack_[1].location,
                                              yystack_[1].value.as<NamedIdentifierIndex>()));
          }
#line 3337 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 125: // argument_declarations: PIPE PIPE
#line 680 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<DeclareArgumentListIndex>() = cxt.create(DeclareArgumentList {}, yylhs.location);
          }
#line 3343 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 126: // argument_declarations: PIPE argument_declarations.pipelist comma.opt PIPE
#line 682 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<DeclareArgumentListIndex>() = yystack_[2].value.as<DeclareArgumentListIndex>();
          }
#line 3349 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 127: // argument_declarations: PIPE argument_declarations.pipelist ELLIPSIS name PIPE
#line 684 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<DeclareArgumentListIndex>() =
                  cxt.graph.append(yystack_[3].value.as<DeclareArgumentListIndex>(), yystack_[3].location,
                                   cxt.create(DeclareArgumentVariadicNode {}, yystack_[1].location,
                                              yystack_[1].value.as<NamedIdentifierIndex>()));
          }
#line 3355 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 128: // argument_declarations: PIPE argument_declarations.pipelist ELLIPSIS name COMMA name PIPE
#line 686 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<DeclareArgumentListIndex>() =
                  cxt.graph.append(yystack_[5].value.as<DeclareArgumentListIndex>(), yylhs.location,
                                   cxt.create(DeclareArgumentVariadicNode {}, yystack_[3].location,
                                              yystack_[3].value.as<NamedIdentifierIndex>()),
                                   cxt.create(DeclareArgumentVariadicNode {}, yystack_[1].location,
                                              yystack_[1].value.as<NamedIdentifierIndex>()));
          }
#line 3361 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 129: // argument_declarations: PIPE ELLIPSIS name PIPE
#line 688 "langutils/sc_parser/src/sc_grammar.y"
          {
              auto arg_list = cxt.create(DeclareArgumentList {}, yylhs.location);
              yylhs.value.as<DeclareArgumentListIndex>() =
                  cxt.graph.append(arg_list, yylhs.location,
                                   cxt.create(DeclareArgumentVariadicNode {}, yystack_[1].location,
                                              yystack_[1].value.as<NamedIdentifierIndex>()));
          }
#line 3370 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 130: // argument_declarations: PIPE ELLIPSIS name COMMA name PIPE
#line 693 "langutils/sc_parser/src/sc_grammar.y"
          {
              auto arg_list = cxt.create(DeclareArgumentList {}, yylhs.location);
              yylhs.value.as<DeclareArgumentListIndex>() =
                  cxt.graph.append(arg_list, yylhs.location,
                                   cxt.create(DeclareArgumentVariadicNode {}, yystack_[3].location,
                                              yystack_[3].value.as<NamedIdentifierIndex>()),
                                   cxt.create(DeclareArgumentVariadicNode {}, yystack_[1].location,
                                              yystack_[1].value.as<NamedIdentifierIndex>()));
          }
#line 3383 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 131: // argument_declarations.opt: %empty
#line 705 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<DeclareArgumentListIndex>() = cxt.create(DeclareArgumentList {}, yylhs.location);
          }
#line 3389 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 132: // argument_declarations.opt: argument_declarations
#line 706 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<DeclareArgumentListIndex>() = yystack_[0].value.as<DeclareArgumentListIndex>();
          }
#line 3395 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 133: // variable_declarations.list.item: name
#line 711 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<DeclareAnyVariableIndex>() = yystack_[0].value.as<NamedIdentifierIndex>();
          }
#line 3401 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 134: // variable_declarations.list.item: name EQUALSSIGN expr
#line 713 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<DeclareAnyVariableIndex>() =
                  cxt.create(DeclareVariableWithDefaultNode {}, yylhs.location,
                             yystack_[2].value.as<NamedIdentifierIndex>(), yystack_[0].value.as<AnyExprIndex>());
          }
#line 3407 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 135: // variable_declarations.list.item: name OPENPAREN expr.seq CLOSEPAREN
#line 715 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<DeclareAnyVariableIndex>() =
                  cxt.create(DeclareVariableWithDefaultNode {}, yylhs.location,
                             yystack_[3].value.as<NamedIdentifierIndex>(), yystack_[1].value.as<AnyExprIndex>());
          }
#line 3413 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 136: // variable_declarations.list: variable_declarations.list.item
#line 724 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<DeclareVariableListIndex>() =
                  cxt.create(DeclareVariableList {}, yylhs.location, yystack_[0].value.as<DeclareAnyVariableIndex>());
          }
#line 3419 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 137: // variable_declarations.list: variable_declarations.list COMMA variable_declarations.list.item
#line 726 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<DeclareVariableListIndex>() = cxt.graph.append(
                  yystack_[2].value.as<DeclareVariableListIndex>(), yystack_[0].value.as<DeclareAnyVariableIndex>());
          }
#line 3425 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 138: // variable_declarations: VAR variable_declarations.list
#line 729 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<DeclareVariableListIndex>() = yystack_[0].value.as<DeclareVariableListIndex>();
          }
#line 3431 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 139: // arguments.entries: KEYBINOP expr.seq
#line 733 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<ArgumentEntryIndex>() =
                  cxt.create(KwArgNode {}, yylhs.location,
                             cxt.create(SymbolNode { SymbolNode::Kind::KeyBinOp }, yystack_[1].location),
                             yystack_[0].value.as<AnyExprIndex>());
          }
#line 3437 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 140: // arguments.entries: MULTIPLY expr.seq
#line 734 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<ArgumentEntryIndex>() =
                  cxt.create(VariadicArgNode {}, yylhs.location, yystack_[0].value.as<AnyExprIndex>());
          }
#line 3443 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 141: // arguments.entries: expr.seq
#line 735 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<ArgumentEntryIndex>() = yystack_[0].value.as<AnyExprIndex>();
          }
#line 3449 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 142: // arguments.no_trailing: arguments.entries
#line 739 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<ArgumentListIndex>() =
                  cxt.create(ArgumentList {}, yylhs.location, yystack_[0].value.as<ArgumentEntryIndex>());
          }
#line 3455 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 143: // arguments.no_trailing: arguments.no_trailing COMMA arguments.entries
#line 740 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<ArgumentListIndex>() =
                  cxt.graph.append(yystack_[2].value.as<ArgumentListIndex>(), yylhs.location,
                                   yystack_[0].value.as<ArgumentEntryIndex>());
          }
#line 3461 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 144: // arguments: arguments.no_trailing comma.opt
#line 743 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<ArgumentListIndex>() = yystack_[1].value.as<ArgumentListIndex>();
          }
#line 3467 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 145: // arguments.paren: OPENPAREN arguments CLOSEPAREN
#line 746 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<ArgumentListIndex>() = yystack_[1].value.as<ArgumentListIndex>();
          }
#line 3473 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 146: // arguments.maybe_paren: %empty
#line 749 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<ArgumentListIndex>() = cxt.create(ArgumentList {}, yylhs.location);
          }
#line 3479 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 147: // arguments.maybe_paren: arguments.paren
#line 750 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<ArgumentListIndex>() = yystack_[0].value.as<ArgumentListIndex>();
          }
#line 3485 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 148: // literal.terminal: symbol
#line 754 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<AnyPossiblyLiteralIndex>() = yystack_[0].value.as<SymbolLitIndex>();
          }
#line 3491 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 149: // literal.terminal: string
#line 755 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<AnyPossiblyLiteralIndex>() = yystack_[0].value.as<StringLitIndex>();
          }
#line 3497 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 150: // literal.terminal: integer
#line 756 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<AnyPossiblyLiteralIndex>() = yystack_[0].value.as<IntLitIndex>();
          }
#line 3503 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 151: // literal.terminal: float
#line 757 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<AnyPossiblyLiteralIndex>() = yystack_[0].value.as<FloatProducingIndex>();
          }
#line 3509 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 152: // literal.terminal: boolean
#line 758 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<AnyPossiblyLiteralIndex>() = yystack_[0].value.as<BooleanLitIndex>();
          }
#line 3515 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 153: // literal.terminal: nil
#line 759 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<AnyPossiblyLiteralIndex>() = yystack_[0].value.as<NilLitIndex>();
          }
#line 3521 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 154: // literal.terminal: ascii
#line 760 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<AnyPossiblyLiteralIndex>() = yystack_[0].value.as<ASCIIIndex>();
          }
#line 3527 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 155: // literal.terminal: block
#line 761 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<AnyPossiblyLiteralIndex>() = yystack_[0].value.as<BlockIndex>();
          }
#line 3533 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 156: // literal.array.contents: %empty
#line 766 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<ArrayIndex>() = cxt.create(ArrayNode {}, yylhs.location);
          }
#line 3539 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 157: // literal.array.contents: expr.seq
#line 768 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<ArrayIndex>() =
                  cxt.create(ArrayNode {}, yylhs.location, yystack_[0].value.as<AnyExprIndex>());
          }
#line 3545 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 158: // literal.array.contents: expr.seq COLON expr.seq
#line 770 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<ArrayIndex>() =
                  cxt.create(ArrayNode {}, yylhs.location, yystack_[2].value.as<AnyExprIndex>(),
                             yystack_[0].value.as<AnyExprIndex>());
          }
#line 3551 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 159: // literal.array.contents: KEYBINOP expr.seq
#line 772 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<ArrayIndex>() =
                  cxt.create(ArrayNode {}, yylhs.location,
                             cxt.create(SymbolNode { SymbolNode::Kind::KeyBinOp }, yystack_[1].location),
                             yystack_[0].value.as<AnyExprIndex>());
          }
#line 3557 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 160: // literal.array.contents: literal.array.contents COMMA expr.seq
#line 774 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<ArrayIndex>() = cxt.graph.append(yystack_[2].value.as<ArrayIndex>(), yylhs.location,
                                                              yystack_[0].value.as<AnyExprIndex>());
          }
#line 3563 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 161: // literal.array.contents: literal.array.contents COMMA expr.seq COLON expr.seq
#line 776 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<ArrayIndex>() =
                  cxt.graph.append(yystack_[4].value.as<ArrayIndex>(), yylhs.location,
                                   yystack_[2].value.as<AnyExprIndex>(), yystack_[0].value.as<AnyExprIndex>());
          }
#line 3569 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 162: // literal.array.contents: literal.array.contents COMMA KEYBINOP expr.seq
#line 778 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<ArrayIndex>() =
                  cxt.graph.append(yystack_[3].value.as<ArrayIndex>(), yylhs.location,
                                   cxt.create(SymbolNode { SymbolNode::Kind::KeyBinOp }, yystack_[1].location),
                                   yystack_[0].value.as<AnyExprIndex>());
          }
#line 3575 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 163: // literal.dictionary.entry: expr COLON expr.seq
#line 786 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<DictionaryEntryIndex>() =
                  cxt.create(DictionaryEntryNode {}, yylhs.location, yystack_[2].value.as<AnyExprIndex>(),
                             yystack_[0].value.as<AnyExprIndex>());
          }
#line 3581 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 164: // literal.dictionary.entry: KEYBINOP expr.seq
#line 788 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<DictionaryEntryIndex>() =
                  cxt.create(DictionaryEntryNode {}, yylhs.location,
                             cxt.create(SymbolNode { SymbolNode::Kind::KeyBinOp }, yystack_[1].location),
                             yystack_[0].value.as<AnyExprIndex>());
          }
#line 3587 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 165: // literal.dictionary.entries: %empty
#line 793 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<DictionaryIndex>() = cxt.create(DictionaryNode {}, yylhs.location);
          }
#line 3593 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 166: // literal.dictionary.entries: literal.dictionary.entry
#line 795 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<DictionaryIndex>() =
                  cxt.create(DictionaryNode {}, yylhs.location, yystack_[0].value.as<DictionaryEntryIndex>());
          }
#line 3599 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 167: // literal.dictionary.entries: literal.dictionary.entries COMMA literal.dictionary.entry
#line 797 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<DictionaryIndex>() =
                  cxt.graph.append(yystack_[2].value.as<DictionaryIndex>(), yylhs.location,
                                   yystack_[0].value.as<DictionaryEntryIndex>());
          }
#line 3605 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 168: // literal.dictionary: OPENPAREN literal.dictionary.entries comma.opt CLOSEPAREN
#line 802 "langutils/sc_parser/src/sc_grammar.y"
          {
              cxt.graph.location(*yystack_[2].value.as<DictionaryIndex>()) = yylhs.location;
              yylhs.value.as<DictionaryIndex>() = yystack_[2].value.as<DictionaryIndex>();
          }
#line 3611 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 169: // literal.array: OPENSQUARE literal.array.contents comma.opt CLOSESQUARE
#line 807 "langutils/sc_parser/src/sc_grammar.y"
          {
              cxt.graph.location(*yystack_[2].value.as<ArrayIndex>()) = yylhs.location;
              yylhs.value.as<ArrayIndex>() = yystack_[2].value.as<ArrayIndex>();
          }
#line 3617 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 170: // literal.array: HASH OPENSQUARE literal.array.contents comma.opt CLOSESQUARE
#line 809 "langutils/sc_parser/src/sc_grammar.y"
          {
              cxt.graph.payload(yystack_[2].value.as<ArrayIndex>()).is_immutable = true;
              cxt.graph.location(*yystack_[2].value.as<ArrayIndex>()) = yylhs.location;
              yylhs.value.as<ArrayIndex>() = yystack_[2].value.as<ArrayIndex>();
          }
#line 3627 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 171: // literal: literal.terminal
#line 817 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<AnyPossiblyLiteralIndex>() = yystack_[0].value.as<AnyPossiblyLiteralIndex>();
          }
#line 3633 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 172: // literal: literal.array
#line 818 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<AnyPossiblyLiteralIndex>() = yystack_[0].value.as<ArrayIndex>();
          }
#line 3639 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 173: // literal: literal.dictionary
#line 819 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<AnyPossiblyLiteralIndex>() = yystack_[0].value.as<DictionaryIndex>();
          }
#line 3645 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 174: // name: NAME
#line 822 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<NamedIdentifierIndex>() = cxt.create(NamedIdentifier {}, yylhs.location);
          }
#line 3651 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 175: // binary_op.raw: BINOP
#line 831 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<SelectorIndex>() = cxt.create(SelectorNode { false }, yylhs.location);
          }
#line 3657 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 176: // binary_op.raw: READWRITEVAR
#line 832 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<SelectorIndex>() = cxt.create(SelectorNode { false }, yylhs.location);
          }
#line 3663 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 177: // binary_op.raw: LESSTHAN
#line 833 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<SelectorIndex>() = cxt.create(SelectorNode { false }, yylhs.location);
          }
#line 3669 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 178: // binary_op.raw: GREATERTHAN
#line 834 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<SelectorIndex>() = cxt.create(SelectorNode { false }, yylhs.location);
          }
#line 3675 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 179: // binary_op.raw: MINUS
#line 835 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<SelectorIndex>() = cxt.create(SelectorNode { false }, yylhs.location);
          }
#line 3681 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 180: // binary_op.raw: MULTIPLY
#line 836 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<SelectorIndex>() = cxt.create(SelectorNode { false }, yylhs.location);
          }
#line 3687 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 181: // binary_op.raw: ADD
#line 837 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<SelectorIndex>() = cxt.create(SelectorNode { false }, yylhs.location);
          }
#line 3693 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 182: // binary_op.raw: PIPE
#line 838 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<SelectorIndex>() = cxt.create(SelectorNode { false }, yylhs.location);
          }
#line 3699 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 183: // binary_op.no_adverb: binary_op.raw
#line 842 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<SelectorIndex>() = yystack_[0].value.as<SelectorIndex>();
          }
#line 3705 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 184: // binary_op.no_adverb: KEYBINOP
#line 843 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<SelectorIndex>() = cxt.create(SelectorNode { true }, yylhs.location);
          }
#line 3711 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 185: // binary_op: binary_op.no_adverb adverb
#line 848 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<SelectorMaybeAdverbIndex>() =
                  cxt.create(SelectorWAdverb {}, yylhs.location, yystack_[1].value.as<SelectorIndex>(),
                             yystack_[0].value.as<AdverbIndex>());
          }
#line 3717 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 186: // binary_op: binary_op.no_adverb
#line 850 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<SelectorMaybeAdverbIndex>() = yystack_[0].value.as<SelectorIndex>();
          }
#line 3723 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 191: // ascii: ASCII
#line 856 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<ASCIIIndex>() = cxt.create(ASCIINode {}, yylhs.location);
          }
#line 3729 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 192: // nil: NIL
#line 858 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<NilLitIndex>() = cxt.create(NilNode {}, yylhs.location);
          }
#line 3735 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 193: // boolean: TRUE
#line 861 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<BooleanLitIndex>() = cxt.create(BooleanNode { true }, yylhs.location);
          }
#line 3741 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 194: // boolean: FALSE
#line 862 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<BooleanLitIndex>() = cxt.create(BooleanNode { false }, yylhs.location);
          }
#line 3747 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 195: // symbol: SYMBOL_QUOTE
#line 866 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<SymbolLitIndex>() = cxt.create(SymbolNode { SymbolNode::Kind::Quote }, yylhs.location);
          }
#line 3753 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 196: // symbol: SYMBOL_SLASH
#line 867 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<SymbolLitIndex>() = cxt.create(SymbolNode { SymbolNode::Kind::Slash }, yylhs.location);
          }
#line 3759 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 197: // string: STRINGLINE
#line 871 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<StringLitIndex>() =
                  cxt.create(StringLineList {}, yylhs.location, cxt.create(StringLineNode {}, yylhs.location));
          }
#line 3765 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 198: // string: string STRINGLINE
#line 872 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<StringLitIndex>() = cxt.graph.append(yystack_[1].value.as<StringLitIndex>(),
                                                                  cxt.create(StringLineNode {}, yystack_[0].location));
          }
#line 3771 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 199: // integer: INTEGER
#line 876 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<IntLitIndex>() = cxt.create(IntNode {}, yylhs.location);
          }
#line 3777 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 200: // integer: INTEGER_RADIX
#line 877 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<IntLitIndex>() = cxt.create(IntNode { IntNode::Kind::Radix }, yylhs.location);
          }
#line 3783 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 201: // integer: HEXADECIMAL
#line 878 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<IntLitIndex>() = cxt.create(IntNode { IntNode::Kind::Hexadecimal }, yylhs.location);
          }
#line 3789 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 202: // integer: MINUS integer
#line 880 "langutils/sc_parser/src/sc_grammar.y"
          {
              // Reaches into the previous integer and changes its sign.
              cxt.graph.payload(yystack_[0].value.as<IntLitIndex>()).sign = IntNode::Sign::Negative;
              yylhs.value.as<IntLitIndex>() = yystack_[0].value.as<IntLitIndex>();
          }
#line 3799 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 203: // float.raw_unsigned: FLOAT
#line 888 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<FloatLitIndex>() = cxt.create(FloatNode {}, yylhs.location);
          }
#line 3805 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 204: // float.raw_unsigned: FLOAT_RADIX
#line 889 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<FloatLitIndex>() = cxt.create(FloatNode { FloatNode::Kind::Radix }, yylhs.location);
          }
#line 3811 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 205: // float.raw_unsigned: FLOAT_EXPONENT
#line 890 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<FloatLitIndex>() = cxt.create(FloatNode { FloatNode::Kind::Exponent }, yylhs.location);
          }
#line 3817 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 206: // float.raw_unsigned: FLOAT_INF
#line 891 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<FloatLitIndex>() = cxt.create(FloatNode { FloatNode::Kind::Inf }, yylhs.location);
          }
#line 3823 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 207: // float.raw: float.raw_unsigned
#line 896 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<FloatLitIndex>() = yystack_[0].value.as<FloatLitIndex>();
          }
#line 3829 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 208: // float.raw: MINUS float.raw_unsigned
#line 898 "langutils/sc_parser/src/sc_grammar.y"
          {
              cxt.graph.payload(yystack_[0].value.as<FloatLitIndex>()).sign = FloatNode::Sign::Negative;
              yylhs.value.as<FloatLitIndex>() = yystack_[0].value.as<FloatLitIndex>();
          }
#line 3835 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 209: // accidental.unsigned: ACCIDENTAL_STEPS
#line 903 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<AccidentalIndex>() =
                  cxt.create(AccidentalNode { AccidentalNode::Kind::Steps }, yylhs.location);
          }
#line 3841 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 210: // accidental.unsigned: ACCIDENTAL_CENTS
#line 905 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<AccidentalIndex>() =
                  cxt.create(AccidentalNode { AccidentalNode::Kind::Cents }, yylhs.location);
          }
#line 3847 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 211: // accidental: accidental.unsigned
#line 910 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<AccidentalIndex>() = yystack_[0].value.as<AccidentalIndex>();
          }
#line 3853 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 212: // accidental: MINUS accidental.unsigned
#line 912 "langutils/sc_parser/src/sc_grammar.y"
          {
              cxt.graph.payload(yystack_[0].value.as<AccidentalIndex>()).sign = AccidentalNode::Sign::Negative;
              yylhs.value.as<AccidentalIndex>() = yystack_[0].value.as<AccidentalIndex>();
          }
#line 3859 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 213: // float: float.raw
#line 916 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<FloatProducingIndex>() = yystack_[0].value.as<FloatLitIndex>();
          }
#line 3865 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 214: // float: accidental
#line 917 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<FloatProducingIndex>() = yystack_[0].value.as<AccidentalIndex>();
          }
#line 3871 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 215: // float: float.raw PI
#line 918 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<FloatProducingIndex>() =
                  cxt.create(PiNode {}, yylhs.location, yystack_[1].value.as<FloatLitIndex>());
          }
#line 3877 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 216: // float: integer PI
#line 919 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<FloatProducingIndex>() =
                  cxt.create(PiNode {}, yylhs.location, yystack_[1].value.as<IntLitIndex>());
          }
#line 3883 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 217: // float: PI
#line 920 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<FloatProducingIndex>() =
                  cxt.create(PiNode {}, yylhs.location, cxt.create(Missing {}, yylhs.location));
          }
#line 3889 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 218: // float: MINUS PI
#line 921 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<FloatProducingIndex>() =
                  cxt.create(PiNode { PiNode::Sign::Negative }, yylhs.location, cxt.create(Missing {}, yylhs.location));
          }
#line 3895 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 219: // accessor: %empty
#line 925 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<ReadWriteAccessor>() = ReadWriteAccessor::Private;
          }
#line 3901 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 220: // accessor: LESSTHAN
#line 926 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<ReadWriteAccessor>() = ReadWriteAccessor::PublicRead;
          }
#line 3907 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 221: // accessor: READWRITEVAR
#line 927 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<ReadWriteAccessor>() = ReadWriteAccessor::PublicReadAndWrite;
          }
#line 3913 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;

          case 222: // accessor: GREATERTHAN
#line 928 "langutils/sc_parser/src/sc_grammar.y"
          {
              yylhs.value.as<ReadWriteAccessor>() = ReadWriteAccessor::PublicWrite;
          }
#line 3919 "langutils/sc_parser/src/sc_grammar_parser.cpp"
          break;


#line 3923 "langutils/sc_parser/src/sc_grammar_parser.cpp"

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
                                              "block.contents.no_trailing_return",
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


  const short parser::yypact_ninf_ = -194;

  const signed char parser::yytable_ninf_ = -1;

  const short parser::yypact_[] = {
      143,  17,   359,  -4,   103,  174,  5,    -194, -194, -194, 92,   61,   -194, -194, 1157, 408,  107,  92,   -194,
      -194, -194, -194, -194, -194, -194, -194, -194, -194, -194, -194, -194, -194, 126,  -194, -194, -194, -194, -194,
      1614, 1301, 100,  -194, -194, 110,  1605, -194, -194, -194, -194, 49,   -194, -194, -194, -194, 130,  122,  -194,
      133,  -194, -194, -194, 161,  -194, -194, -194, 1349, 1349, -194, -194, 173,  152,  185,  462,  1301, 1605, 183,
      147,  194,  1301, 92,   96,   -194, 1301, 1614, -194, -194, -194, -194, 75,   -194, 197,  199,  -194, 1592, 567,
      -194, -194, 200,  -194, 204,  1157, 167,  1157, 615,  -194, 30,   -194, 108,  -194, -194, -194, -194, 75,   -194,
      667,  715,  -194, -194, -194, 170,  162,  1301, 764,  1301, 30,   -194, -194, -194, 148,  408,  -194, 1605, -194,
      -194, -194, 156,  -194, 1301, -194, 1301, 1205, 212,  1605, -194, 213,  41,   -194, 92,   15,   46,   -194, 92,
      -194, 105,  1393, 1301, 1060, 215,  1301, -194, 199,  1605, 1253, 217,  16,   194,  1301, 35,   30,   1301, 1301,
      -194, -194, 216,  221,  -194, -194, 199,  184,  227,  813,  862,  30,   93,   28,   -194, 177,  30,   228,  1605,
      252,  232,  -194, -194, 148,  233,  -194, -194, 911,  85,   85,   85,   -194, 231,  148,  1605, -194, 1301, 195,
      -194, 92,   1301, 1301, 20,   92,   92,   235,  1301, 1467, 3,    -194, 92,   24,   1253, 1504, -194, 1605, -194,
      -194, -194, 237,  1301, 1592, -194, -194, 959,  30,   241,  1605, -194, 1205, -194, 30,   -194, -194, 1108, -194,
      30,   244,  1301, 202,  205,  245,  30,   247,  -194, 1108, 1301, -194, 30,   1301, -194, -194, 30,   14,   -194,
      100,  -194, -194, -194, 31,   -194, -194, -194, -194, 242,  92,   242,  242,  156,  -194, 253,  -194, 1301, -194,
      250,  1605, -194, 92,   102,  38,   -194, 254,  1253, -194, 92,   -194, 13,   -194, 1430, 1592, 255,  1253, -194,
      -194, 30,   259,  -194, -194, -194, -194, 1605, 1301, 1301, 214,  -194, -194, 1605, -194, 260,  1301, -194, 518,
      -194, 1060, 85,   -194, -194, -194, -194, -194, 262,  1301, 1541, -194, 92,   -194, 261,  218,  92,   -194, 1253,
      1578, -194, -194, 264,  30,   30,   1605, 1605, 1301, -194, 1605, -194, 265,  199,  -194, -194, 267,  1253, -194,
      268,  -194, -194, 224,  277,  1253, -194, -194, -194, 1605, -194, 1011, 282,  -194, 281,  -194, -194, -194, 285,
      -194, 199,  -194, -194, -194, 290,  -194
  };

  const unsigned char parser::yydefact_[] = {
      0,   21,  0,   0,   0,   0,   0,   14,  16,  17,  0,   19,  8,   46,  156, 165, 0,   0,   174, 199, 200, 201, 203,
      204, 205, 206, 209, 210, 195, 196, 197, 191, 82,  192, 193, 194, 217, 47,  0,   0,   131, 155, 76,  81,  7,   171,
      173, 172, 74,  75,  154, 153, 152, 148, 149, 150, 207, 213, 211, 214, 151, 0,   1,   2,   13,  0,   0,   3,   15,
      0,   0,   0,   165, 0,   95,  187, 157, 189, 0,   0,   0,   175, 184, 179, 177, 178, 180, 181, 182, 176, 56,  187,
      54,  58,  0,   59,  166, 189, 183, 0,   156, 78,  156, 0,   52,  71,  218, 0,   202, 208, 212, 85,  0,   132, 0,
      0,   184, 179, 182, 0,   186, 0,   0,   0,   66,  198, 216, 215, 44,  165, 12,  4,   11,  22,  20,  32,  159, 188,
      97,  0,   190, 0,   60,  136, 138, 133, 119, 0,   189, 101, 164, 0,   125, 189, 110, 0,   188, 0,   0,   10,  187,
      58,  190, 0,   0,   189, 0,   0,   50,  0,   0,   141, 142, 189, 0,   53,  49,  187, 80,  0,   0,   0,   50,  146,
      0,   185, 86,  0,   0,   87,  180, 0,   40,  42,  45,  0,   34,  35,  0,   219, 219, 219, 30,  187, 44,  96,  158,
      0,   160, 169, 0,   0,   0,   0,   190, 0,   0,   0,   0,   0,   190, 0,   0,   165, 0,   109, 57,  55,  77,  163,
      0,   0,   0,   167, 168, 0,   63,  0,   88,  70,  0,   73,  51,  139, 140, 190, 144, 50,  0,   0,   79,  84,  0,
      50,  0,   68,  0,   0,   147, 50,  0,   98,  99,  65,  50,  41,  131, 43,  23,  6,   0,   220, 222, 221, 25,  28,
      0,   27,  29,  188, 33,  0,   162, 0,   137, 0,   134, 123, 0,   105, 0,   120, 0,   165, 102, 0,   129, 0,   126,
      115, 95,  0,   165, 111, 9,   0,   0,   170, 143, 72,  48,  92,  0,   0,   83,  69,  145, 89,  67,  0,   0,   64,
      0,   5,   0,   219, 24,  31,  18,  161, 135, 0,   0,   0,   121, 0,   103, 0,   0,   0,   127, 165, 0,   114, 112,
      0,   62,  50,  91,  94,  0,   100, 90,  37,  187, 187, 26,  124, 0,   165, 106, 0,   104, 130, 0,   0,   165, 116,
      113, 61,  93,  188, 0,   0,   107, 0,   122, 128, 117, 0,   39,  187, 36,  108, 118, 0,   38
  };

  const short parser::yypgoto_[] = {
      -194, -194, 230,  -194, -194, 291,  -194, -194, -194, -194, -27,  -72,  33,   -194, -194, -194, 109,  119,
      -194, 111,  -194, -20,  -166, -21,  -194, -93,  -153, -194, -194, -2,   -194, 0,    -194, -194, -194, -6,
      48,   -193, -194, -194, 71,   -194, -84,  134,  -194, -194, 18,   157,  -194, -194, -194, -150, 67,   -95,
      -7,   -194, -89,  -87,  -194, -194, -194, -194, -194, -32,  -31,  -194, -16,  -194, -194, -194
  };

  const short parser::yydefgoto_[] = { 0,   4,   130, 5,   6,   7,   8,   71,  11,  9,   274, 275, 202, 203,
                                       204, 191, 192, 193, 194, 195, 40,  41,  241, 242, 90,  91,  92,  42,
                                       43,  74,  75,  171, 185, 148, 153, 113, 114, 143, 144, 95,  172, 173,
                                       254, 182, 259, 45,  77,  96,  97,  46,  47,  48,  49,  98,  120, 121,
                                       138, 141, 50,  51,  52,  53,  54,  55,  56,  57,  58,  59,  60,  276 };

  const short parser::yytable_[] = {
      44,  160, 157, 227, 225, 67,  108, 109, 99,  94,  163, 105, 104, 93,  76,  295, 255, 284, 13,  174, 13,  177, 110,
      10,  235, 339, 61,  214, 124, 104, 287, 179, 288, 197, 13,  1,   260, 111, 188, 18,  323, 324, 239, 18,  19,  20,
      21,  240, 334, 211, 335, 108, 109, 13,  217, 215, 37,  122, 37,  296, 3,   216, 320, 131, 131, 99,  222, 110, 294,
      340, 93,  230, 37,  136, 303, 108, 142, 69,  237, 107, 298, 309, 150, 326, 101, 175, 246, 315, 248, 212, 18,  37,
      161, 318, 218, 197, 252, 123, 321, 197, 76,  256, 76,  62,  175, 270, 146, 18,  70,  197, 332, 18,  161, 100, 280,
      151, 115, 220, 165, 186, 167, 189, 99,  198, 19,  20,  21,  93,  277, 278, 13,  152, 102, 80,  103, 205, 147, 271,
      272, 206, 208, 257, 273, 236, 104, 221, 145, 149, 104, 343, 333, 306, 262, 226, 161, 154, 112, 125, 229, 107, 232,
      126, 104, 18,  238, 128, 263, 104, 37,  243, 244, 227, 127, 1,   63,  64,  180, 65,  181, 154, 133, 369, 134, 360,
      66,  18,  183, 2,   199, 135, 200, 201, 367, 137, 139, 196, 161, 81,  3,   117, 84,  85,  190, 87,  118, 89,  140,
      282, 155, 156, 286, 285, 162, 164, 213, 166, 175, 292, 219, 209, 184, 300, 175, 301, 228, 210, 234, 104, 245, 355,
      247, 150, 249, 104, 250, 119, 266, 264, 268, 104, 208, 279, 283, 175, 104, 291, 304, 311, 307, 310, 312, 261, 314,
      313, 325, 317, 316, 196, 328, 330, 319, 196, 350, 336, 344, 372, 373, 18,  347, 351, 362, 196, 357, 368, 363, 371,
      374, 145, 376, 381, 377, 289, 290, 329, 346, 104, 378, 382, 297, 299, 383, 300, 385, 337, 384, 386, 132, 68,  356,
      265, 300, 81,  345, 117, 84,  85,  86,  87,  118, 89,  348, 349, 327, 267, 322, 281, 308, 258, 352, 233, 161, 0,
      161, 0,   0,   0,   175, 104, 0,   0,   0,   0,   358, 0,   0,   0,   0,   0,   0,   300, 0,   365, 0,   145, 0,
      0,   0,   0,   370, 0,   0,   0,   0,   0,   0,   331, 0,   300, 0,   375, 12,  0,   338, 13,  300, 14,  379, 15,
      0,   0,   161, 0,   16,  17,  18,  19,  20,  21,  22,  23,  24,  25,  26,  27,  28,  29,  30,  31,  0,   32,  0,
      0,   0,   0,   0,   33,  34,  35,  36,  0,   0,   37,  361, 0,   0,   0,   364, 0,   0,   0,   38,  0,   13,  0,
      14,  0,   72,  0,   39,  78,  0,   16,  17,  18,  19,  20,  21,  22,  23,  24,  25,  26,  27,  28,  29,  30,  31,
      0,   32,  0,   79,  80,  0,   0,   33,  34,  35,  36,  0,   0,   37,  0,   0,   0,   0,   0,   0,   81,  82,  83,
      84,  85,  86,  87,  88,  89,  13,  39,  14,  0,   72,  0,   0,   78,  0,   16,  17,  18,  19,  20,  21,  22,  23,
      24,  25,  26,  27,  28,  29,  30,  31,  0,   32,  0,   79,  0,   0,   0,   33,  34,  35,  36,  0,   0,   37,  0,
      0,   0,   0,   0,   0,   81,  82,  83,  84,  85,  86,  87,  118, 89,  0,   39,  13,  353, 14,  0,   72,  0,   0,
      78,  0,   16,  17,  18,  19,  20,  21,  22,  23,  24,  25,  26,  27,  28,  29,  30,  31,  354, 32,  0,   79,  0,
      0,   0,   33,  34,  35,  36,  0,   0,   37,  0,   0,   0,   0,   0,   0,   0,   0,   38,  0,   13,  0,   14,  0,
      72,  159, 39,  78,  0,   16,  17,  18,  19,  20,  21,  22,  23,  24,  25,  26,  27,  28,  29,  30,  31,  0,   32,
      0,   79,  0,   0,   0,   33,  34,  35,  36,  0,   0,   37,  0,   0,   0,   0,   0,   0,   0,   0,   38,  13,  0,
      14,  0,   72,  168, 0,   39,  0,   16,  17,  18,  19,  20,  21,  22,  23,  24,  25,  26,  27,  28,  29,  30,  31,
      0,   32,  0,   0,   0,   0,   0,   33,  34,  35,  36,  0,   0,   37,  0,   0,   0,   0,   0,   0,   0,   169, 38,
      0,   0,   170, 0,   13,  176, 14,  39,  72,  0,   0,   78,  0,   16,  17,  18,  19,  20,  21,  22,  23,  24,  25,
      26,  27,  28,  29,  30,  31,  0,   32,  0,   79,  0,   0,   0,   33,  34,  35,  36,  0,   0,   37,  0,   0,   0,
      0,   0,   0,   0,   0,   38,  13,  0,   14,  178, 72,  0,   0,   39,  0,   16,  17,  18,  19,  20,  21,  22,  23,
      24,  25,  26,  27,  28,  29,  30,  31,  0,   32,  0,   0,   0,   0,   0,   33,  34,  35,  36,  0,   0,   37,  0,
      0,   0,   0,   0,   0,   0,   169, 38,  0,   13,  170, 14,  0,   72,  187, 39,  0,   0,   16,  17,  18,  19,  20,
      21,  22,  23,  24,  25,  26,  27,  28,  29,  30,  31,  0,   32,  0,   0,   0,   0,   0,   33,  34,  35,  36,  0,
      0,   37,  0,   0,   0,   0,   0,   0,   0,   169, 38,  0,   13,  170, 14,  251, 72,  0,   39,  0,   0,   16,  17,
      18,  19,  20,  21,  22,  23,  24,  25,  26,  27,  28,  29,  30,  31,  0,   32,  0,   0,   0,   0,   0,   33,  34,
      35,  36,  0,   0,   37,  0,   0,   0,   0,   0,   0,   0,   169, 38,  0,   13,  170, 14,  0,   72,  253, 39,  0,
      0,   16,  17,  18,  19,  20,  21,  22,  23,  24,  25,  26,  27,  28,  29,  30,  31,  0,   32,  0,   0,   0,   0,
      0,   33,  34,  35,  36,  0,   0,   37,  0,   0,   0,   0,   0,   0,   0,   169, 38,  0,   13,  170, 14,  0,   72,
      269, 39,  78,  0,   16,  17,  18,  19,  20,  21,  22,  23,  24,  25,  26,  27,  28,  29,  30,  31,  0,   32,  0,
      79,  0,   0,   0,   33,  34,  35,  36,  0,   0,   37,  0,   0,   0,   0,   0,   0,   0,   0,   38,  13,  0,   14,
      0,   72,  305, 0,   39,  0,   16,  17,  18,  19,  20,  21,  22,  23,  24,  25,  26,  27,  28,  29,  30,  31,  0,
      32,  0,   0,   0,   0,   0,   33,  34,  35,  36,  0,   0,   37,  0,   0,   0,   0,   0,   0,   0,   169, 38,  0,
      0,   170, 0,   13,  380, 14,  39,  72,  0,   0,   78,  0,   16,  17,  18,  19,  20,  21,  22,  23,  24,  25,  26,
      27,  28,  29,  30,  31,  0,   32,  0,   79,  0,   0,   0,   33,  34,  35,  36,  0,   0,   37,  0,   0,   0,   0,
      0,   0,   0,   0,   38,  0,   13,  0,   14,  0,   72,  0,   39,  78,  0,   16,  17,  18,  19,  20,  21,  22,  23,
      24,  25,  26,  27,  28,  29,  30,  31,  0,   32,  0,   79,  0,   0,   0,   33,  34,  35,  36,  0,   0,   37,  0,
      0,   0,   0,   0,   0,   0,   0,   38,  13,  0,   14,  0,   72,  0,   0,   39,  0,   16,  17,  18,  19,  20,  21,
      22,  23,  24,  25,  26,  27,  28,  29,  30,  31,  0,   32,  0,   0,   0,   0,   0,   33,  34,  35,  36,  0,   0,
      37,  0,   0,   0,   0,   0,   0,   0,   169, 38,  0,   13,  170, 14,  0,   72,  0,   39,  0,   0,   16,  17,  18,
      19,  20,  21,  22,  23,  24,  25,  26,  27,  28,  29,  30,  31,  0,   32,  0,   0,   0,   0,   0,   33,  34,  35,
      36,  0,   0,   37,  0,   0,   0,   0,   0,   0,   0,   73,  38,  13,  0,   14,  0,   72,  0,   0,   39,  0,   16,
      17,  18,  19,  20,  21,  22,  23,  24,  25,  26,  27,  28,  29,  30,  31,  0,   32,  0,   0,   0,   0,   0,   33,
      34,  35,  36,  0,   0,   37,  0,   0,   0,   0,   0,   0,   0,   207, 38,  13,  0,   14,  0,   72,  0,   0,   39,
      0,   16,  17,  18,  19,  20,  21,  22,  23,  24,  25,  26,  27,  28,  29,  30,  31,  0,   32,  0,   0,   0,   0,
      0,   33,  34,  35,  36,  0,   0,   37,  0,   0,   0,   0,   0,   0,   0,   231, 38,  13,  0,   14,  0,   72,  0,
      0,   39,  0,   16,  17,  18,  19,  20,  21,  22,  23,  24,  25,  26,  27,  28,  29,  30,  31,  0,   32,  0,   0,
      0,   0,   0,   33,  34,  35,  36,  0,   0,   37,  0,   0,   0,   0,   0,   0,   0,   0,   38,  13,  0,   14,  0,
      129, 0,   0,   39,  0,   16,  17,  18,  19,  20,  21,  22,  23,  24,  25,  26,  27,  28,  29,  30,  31,  0,   32,
      0,   0,   0,   0,   0,   33,  34,  35,  36,  0,   0,   37,  0,   0,   0,   0,   0,   13,  0,   14,  38,  223, 0,
      0,   0,   0,   16,  0,   39,  19,  20,  21,  22,  23,  24,  25,  26,  27,  28,  29,  30,  31,  0,   0,   0,   0,
      0,   0,   0,   33,  34,  35,  36,  0,   13,  37,  14,  0,   341, 0,   0,   224, 0,   16,  38,  0,   19,  20,  21,
      22,  23,  24,  25,  26,  27,  28,  29,  30,  31,  0,   0,   0,   0,   0,   0,   0,   33,  34,  35,  36,  0,   13,
      37,  14,  0,   293, 0,   0,   342, 0,   16,  38,  0,   19,  20,  21,  22,  23,  24,  25,  26,  27,  28,  29,  30,
      31,  0,   0,   0,   0,   0,   0,   0,   33,  34,  35,  36,  0,   13,  37,  14,  0,   302, 0,   0,   0,   0,   16,
      38,  0,   19,  20,  21,  22,  23,  24,  25,  26,  27,  28,  29,  30,  31,  0,   0,   0,   0,   0,   0,   0,   33,
      34,  35,  36,  0,   13,  37,  14,  0,   359, 0,   0,   0,   0,   16,  38,  0,   19,  20,  21,  22,  23,  24,  25,
      26,  27,  28,  29,  30,  31,  0,   0,   0,   0,   0,   0,   0,   33,  34,  35,  36,  0,   13,  37,  14,  0,   366,
      0,   0,   0,   0,   16,  38,  0,   19,  20,  21,  22,  23,  24,  25,  26,  27,  28,  29,  30,  31,  0,   0,   0,
      0,   0,   0,   0,   33,  34,  35,  36,  0,   0,   37,  0,   0,   0,   0,   0,   0,   0,   0,   38,  19,  20,  21,
      22,  23,  24,  25,  26,  27,  158, 0,   81,  116, 117, 84,  85,  86,  87,  118, 89,  119, 0,   0,   106, 81,  116,
      117, 84,  85,  86,  87,  118, 89,  119, 0,   107
  };

  const short parser::yycheck_[] = {
      2,   94,  91,  156, 154, 0,   38,  38,  15,  15,  97,  32,  32,  15,  14,  12,  182, 210, 4,   103, 4,   114, 38,
      6,   8,   12,  30,  12,  49,  49,  10,  115, 12,  128, 4,   30,  8,   39,  122, 15,  9,   10,  7,   15,  16,  17,
      18,  12,  10,  8,   12,  83,  83,  4,   8,   40,  42,  8,   42,  56,  55,  148, 48,  65,  66,  72,  153, 83,  218,
      56,  72,  160, 42,  73,  224, 107, 78,  10,  165, 51,  56,  247, 82,  276, 17,  105, 173, 253, 177, 48,  15,  42,
      94,  259, 48,  190, 180, 48,  264, 194, 100, 8,   102, 0,   124, 198, 10,  15,  47,  204, 8,   15,  114, 6,   203,
      40,  6,   12,  100, 121, 102, 123, 129, 129, 16,  17,  18,  129, 200, 201, 4,   56,  6,   33,  8,   137, 40,  52,
      53,  139, 140, 48,  57,  164, 164, 40,  79,  80,  168, 299, 48,  235, 184, 155, 156, 88,  56,  27,  158, 51,  162,
      39,  182, 15,  166, 4,   187, 187, 42,  169, 170, 324, 39,  30,  0,   1,   6,   3,   8,   112, 7,   347, 30,  333,
      10,  15,  119, 44,  32,  4,   34,  35,  342, 10,  47,  128, 198, 49,  55,  51,  52,  53,  54,  55,  56,  57,  12,
      207, 11,  10,  212, 211, 12,  9,   147, 48,  236, 217, 151, 7,   58,  223, 242, 223, 9,   12,  9,   247, 12,  322,
      9,   231, 48,  253, 7,   58,  4,   9,   5,   259, 240, 10,  47,  263, 264, 10,  9,   249, 7,   5,   48,  184, 7,
      48,  12,  257, 9,   190, 5,   9,   260, 194, 48,  9,   9,   354, 355, 15,  9,   9,   9,   204, 10,  9,   56,  10,
      9,   210, 10,  372, 56,  214, 215, 283, 305, 305, 9,   5,   221, 222, 9,   293, 381, 293, 9,   5,   66,  6,   325,
      190, 302, 49,  302, 51,  52,  53,  54,  55,  56,  57,  312, 313, 279, 194, 266, 204, 245, 183, 320, 162, 322, -1,
      324, -1,  -1,  -1,  346, 347, -1,  -1,  -1,  -1,  332, -1,  -1,  -1,  -1,  -1,  -1,  341, -1,  341, -1,  276, -1,
      -1,  -1,  -1,  350, -1,  -1,  -1,  -1,  -1,  -1,  288, -1,  359, -1,  359, 1,   -1,  295, 4,   366, 6,   366, 8,
      -1,  -1,  372, -1,  13,  14,  15,  16,  17,  18,  19,  20,  21,  22,  23,  24,  25,  26,  27,  28,  -1,  30,  -1,
      -1,  -1,  -1,  -1,  36,  37,  38,  39,  -1,  -1,  42,  335, -1,  -1,  -1,  339, -1,  -1,  -1,  51,  -1,  4,   -1,
      6,   -1,  8,   -1,  59,  11,  -1,  13,  14,  15,  16,  17,  18,  19,  20,  21,  22,  23,  24,  25,  26,  27,  28,
      -1,  30,  -1,  32,  33,  -1,  -1,  36,  37,  38,  39,  -1,  -1,  42,  -1,  -1,  -1,  -1,  -1,  -1,  49,  50,  51,
      52,  53,  54,  55,  56,  57,  4,   59,  6,   -1,  8,   -1,  -1,  11,  -1,  13,  14,  15,  16,  17,  18,  19,  20,
      21,  22,  23,  24,  25,  26,  27,  28,  -1,  30,  -1,  32,  -1,  -1,  -1,  36,  37,  38,  39,  -1,  -1,  42,  -1,
      -1,  -1,  -1,  -1,  -1,  49,  50,  51,  52,  53,  54,  55,  56,  57,  -1,  59,  4,   5,   6,   -1,  8,   -1,  -1,
      11,  -1,  13,  14,  15,  16,  17,  18,  19,  20,  21,  22,  23,  24,  25,  26,  27,  28,  29,  30,  -1,  32,  -1,
      -1,  -1,  36,  37,  38,  39,  -1,  -1,  42,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  51,  -1,  4,   -1,  6,   -1,
      8,   9,   59,  11,  -1,  13,  14,  15,  16,  17,  18,  19,  20,  21,  22,  23,  24,  25,  26,  27,  28,  -1,  30,
      -1,  32,  -1,  -1,  -1,  36,  37,  38,  39,  -1,  -1,  42,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  51,  4,   -1,
      6,   -1,  8,   9,   -1,  59,  -1,  13,  14,  15,  16,  17,  18,  19,  20,  21,  22,  23,  24,  25,  26,  27,  28,
      -1,  30,  -1,  -1,  -1,  -1,  -1,  36,  37,  38,  39,  -1,  -1,  42,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  50,  51,
      -1,  -1,  54,  -1,  4,   5,   6,   59,  8,   -1,  -1,  11,  -1,  13,  14,  15,  16,  17,  18,  19,  20,  21,  22,
      23,  24,  25,  26,  27,  28,  -1,  30,  -1,  32,  -1,  -1,  -1,  36,  37,  38,  39,  -1,  -1,  42,  -1,  -1,  -1,
      -1,  -1,  -1,  -1,  -1,  51,  4,   -1,  6,   7,   8,   -1,  -1,  59,  -1,  13,  14,  15,  16,  17,  18,  19,  20,
      21,  22,  23,  24,  25,  26,  27,  28,  -1,  30,  -1,  -1,  -1,  -1,  -1,  36,  37,  38,  39,  -1,  -1,  42,  -1,
      -1,  -1,  -1,  -1,  -1,  -1,  50,  51,  -1,  4,   54,  6,   -1,  8,   9,   59,  -1,  -1,  13,  14,  15,  16,  17,
      18,  19,  20,  21,  22,  23,  24,  25,  26,  27,  28,  -1,  30,  -1,  -1,  -1,  -1,  -1,  36,  37,  38,  39,  -1,
      -1,  42,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  50,  51,  -1,  4,   54,  6,   7,   8,   -1,  59,  -1,  -1,  13,  14,
      15,  16,  17,  18,  19,  20,  21,  22,  23,  24,  25,  26,  27,  28,  -1,  30,  -1,  -1,  -1,  -1,  -1,  36,  37,
      38,  39,  -1,  -1,  42,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  50,  51,  -1,  4,   54,  6,   -1,  8,   9,   59,  -1,
      -1,  13,  14,  15,  16,  17,  18,  19,  20,  21,  22,  23,  24,  25,  26,  27,  28,  -1,  30,  -1,  -1,  -1,  -1,
      -1,  36,  37,  38,  39,  -1,  -1,  42,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  50,  51,  -1,  4,   54,  6,   -1,  8,
      9,   59,  11,  -1,  13,  14,  15,  16,  17,  18,  19,  20,  21,  22,  23,  24,  25,  26,  27,  28,  -1,  30,  -1,
      32,  -1,  -1,  -1,  36,  37,  38,  39,  -1,  -1,  42,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  51,  4,   -1,  6,
      -1,  8,   9,   -1,  59,  -1,  13,  14,  15,  16,  17,  18,  19,  20,  21,  22,  23,  24,  25,  26,  27,  28,  -1,
      30,  -1,  -1,  -1,  -1,  -1,  36,  37,  38,  39,  -1,  -1,  42,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  50,  51,  -1,
      -1,  54,  -1,  4,   5,   6,   59,  8,   -1,  -1,  11,  -1,  13,  14,  15,  16,  17,  18,  19,  20,  21,  22,  23,
      24,  25,  26,  27,  28,  -1,  30,  -1,  32,  -1,  -1,  -1,  36,  37,  38,  39,  -1,  -1,  42,  -1,  -1,  -1,  -1,
      -1,  -1,  -1,  -1,  51,  -1,  4,   -1,  6,   -1,  8,   -1,  59,  11,  -1,  13,  14,  15,  16,  17,  18,  19,  20,
      21,  22,  23,  24,  25,  26,  27,  28,  -1,  30,  -1,  32,  -1,  -1,  -1,  36,  37,  38,  39,  -1,  -1,  42,  -1,
      -1,  -1,  -1,  -1,  -1,  -1,  -1,  51,  4,   -1,  6,   -1,  8,   -1,  -1,  59,  -1,  13,  14,  15,  16,  17,  18,
      19,  20,  21,  22,  23,  24,  25,  26,  27,  28,  -1,  30,  -1,  -1,  -1,  -1,  -1,  36,  37,  38,  39,  -1,  -1,
      42,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  50,  51,  -1,  4,   54,  6,   -1,  8,   -1,  59,  -1,  -1,  13,  14,  15,
      16,  17,  18,  19,  20,  21,  22,  23,  24,  25,  26,  27,  28,  -1,  30,  -1,  -1,  -1,  -1,  -1,  36,  37,  38,
      39,  -1,  -1,  42,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  50,  51,  4,   -1,  6,   -1,  8,   -1,  -1,  59,  -1,  13,
      14,  15,  16,  17,  18,  19,  20,  21,  22,  23,  24,  25,  26,  27,  28,  -1,  30,  -1,  -1,  -1,  -1,  -1,  36,
      37,  38,  39,  -1,  -1,  42,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  50,  51,  4,   -1,  6,   -1,  8,   -1,  -1,  59,
      -1,  13,  14,  15,  16,  17,  18,  19,  20,  21,  22,  23,  24,  25,  26,  27,  28,  -1,  30,  -1,  -1,  -1,  -1,
      -1,  36,  37,  38,  39,  -1,  -1,  42,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  50,  51,  4,   -1,  6,   -1,  8,   -1,
      -1,  59,  -1,  13,  14,  15,  16,  17,  18,  19,  20,  21,  22,  23,  24,  25,  26,  27,  28,  -1,  30,  -1,  -1,
      -1,  -1,  -1,  36,  37,  38,  39,  -1,  -1,  42,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  51,  4,   -1,  6,   -1,
      8,   -1,  -1,  59,  -1,  13,  14,  15,  16,  17,  18,  19,  20,  21,  22,  23,  24,  25,  26,  27,  28,  -1,  30,
      -1,  -1,  -1,  -1,  -1,  36,  37,  38,  39,  -1,  -1,  42,  -1,  -1,  -1,  -1,  -1,  4,   -1,  6,   51,  8,   -1,
      -1,  -1,  -1,  13,  -1,  59,  16,  17,  18,  19,  20,  21,  22,  23,  24,  25,  26,  27,  28,  -1,  -1,  -1,  -1,
      -1,  -1,  -1,  36,  37,  38,  39,  -1,  4,   42,  6,   -1,  8,   -1,  -1,  48,  -1,  13,  51,  -1,  16,  17,  18,
      19,  20,  21,  22,  23,  24,  25,  26,  27,  28,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  36,  37,  38,  39,  -1,  4,
      42,  6,   -1,  8,   -1,  -1,  48,  -1,  13,  51,  -1,  16,  17,  18,  19,  20,  21,  22,  23,  24,  25,  26,  27,
      28,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  36,  37,  38,  39,  -1,  4,   42,  6,   -1,  8,   -1,  -1,  -1,  -1,  13,
      51,  -1,  16,  17,  18,  19,  20,  21,  22,  23,  24,  25,  26,  27,  28,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  36,
      37,  38,  39,  -1,  4,   42,  6,   -1,  8,   -1,  -1,  -1,  -1,  13,  51,  -1,  16,  17,  18,  19,  20,  21,  22,
      23,  24,  25,  26,  27,  28,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  36,  37,  38,  39,  -1,  4,   42,  6,   -1,  8,
      -1,  -1,  -1,  -1,  13,  51,  -1,  16,  17,  18,  19,  20,  21,  22,  23,  24,  25,  26,  27,  28,  -1,  -1,  -1,
      -1,  -1,  -1,  -1,  36,  37,  38,  39,  -1,  -1,  42,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  51,  16,  17,  18,
      19,  20,  21,  22,  23,  24,  47,  -1,  49,  50,  51,  52,  53,  54,  55,  56,  57,  58,  -1,  -1,  39,  49,  50,
      51,  52,  53,  54,  55,  56,  57,  58,  -1,  51
  };

  const unsigned char parser::yystos_[] = {
      0,   30,  44,  55,  62,  64,  65,  66,  67,  70,  6,   69,  1,   4,   6,   8,   13,  14,  15,  16,  17, 18,  19,
      20,  21,  22,  23,  24,  25,  26,  27,  28,  30,  36,  37,  38,  39,  42,  51,  59,  81,  82,  88,  89, 90,  106,
      110, 111, 112, 113, 119, 120, 121, 122, 123, 124, 125, 126, 127, 128, 129, 30,  0,   0,   1,   3,   10, 0,   66,
      113, 47,  68,  8,   50,  90,  91,  92,  107, 11,  32,  33,  49,  50,  51,  52,  53,  54,  55,  56,  57, 85,  86,
      87,  90,  96,  100, 108, 109, 114, 115, 6,   113, 6,   8,   82,  84,  39,  51,  124, 125, 127, 90,  56, 96,  97,
      6,   50,  51,  56,  58,  115, 116, 8,   48,  84,  27,  39,  39,  4,   8,   63,  90,  63,  7,   30,  4,  92,  10,
      117, 47,  12,  118, 90,  98,  99,  113, 10,  40,  94,  113, 92,  40,  56,  95,  113, 11,  10,  117, 47, 9,   86,
      90,  12,  118, 9,   107, 48,  107, 9,   50,  54,  92,  101, 102, 103, 82,  5,   86,  7,   103, 6,   8,  104, 113,
      58,  93,  90,  9,   103, 90,  54,  76,  77,  78,  79,  80,  113, 114, 96,  32,  34,  35,  73,  74,  75, 90,  92,
      50,  92,  7,   12,  8,   48,  113, 12,  40,  118, 8,   48,  113, 12,  40,  118, 8,   48,  112, 90,  87, 9,   92,
      117, 50,  90,  108, 9,   8,   84,  118, 90,  7,   12,  83,  84,  92,  92,  12,  118, 9,   117, 48,  7,  7,   103,
      9,   103, 83,  8,   48,  104, 105, 8,   113, 124, 84,  9,   77,  4,   78,  5,   9,   86,  52,  53,  57, 71,  72,
      130, 72,  72,  10,  117, 80,  92,  47,  98,  92,  90,  10,  12,  113, 113, 10,  92,  8,   112, 12,  56, 113, 56,
      113, 90,  92,  8,   112, 9,   9,   103, 7,   101, 83,  5,   90,  48,  48,  7,   83,  9,   90,  83,  92, 48,  83,
      97,  9,   10,  12,  98,  73,  5,   92,  9,   113, 8,   48,  10,  12,  9,   92,  113, 12,  56,  8,   48, 112, 9,
      92,  84,  9,   90,  90,  48,  9,   90,  5,   29,  86,  71,  10,  92,  8,   112, 113, 9,   56,  113, 92, 8,   112,
      9,   83,  90,  10,  117, 117, 9,   92,  10,  56,  9,   92,  5,   86,  5,   9,   9,   117, 5
  };

  const unsigned char parser::yyr1_[] = {
      0,   61,  62,  62,  63,  63,  63,  64,  64,  64,  64,  64,  64,  64,  65,  65,  66,  66,  67,  68,  68,  69,  69,
      70,  71,  72,  72,  73,  73,  73,  74,  74,  75,  75,  76,  76,  77,  77,  77,  77,  78,  78,  79,  79,  80,  80,
      81,  81,  82,  82,  83,  83,  84,  84,  85,  85,  86,  86,  87,  87,  87,  88,  88,  88,  88,  88,  88,  88,  88,
      88,  88,  88,  88,  88,  89,  89,  89,  89,  89,  89,  89,  90,  90,  90,  90,  90,  90,  90,  90,  90,  90,  90,
      90,  90,  90,  91,  91,  92,  93,  93,  93,  94,  94,  94,  94,  94,  94,  94,  94,  95,  95,  95,  95,  95,  95,
      95,  95,  95,  95,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  96,  97,  97,  98,  98,  98,  99,  99,
      100, 101, 101, 101, 102, 102, 103, 104, 105, 105, 106, 106, 106, 106, 106, 106, 106, 106, 107, 107, 107, 107, 107,
      107, 107, 108, 108, 109, 109, 109, 110, 111, 111, 112, 112, 112, 113, 114, 114, 114, 114, 114, 114, 114, 114, 115,
      115, 116, 116, 117, 117, 118, 118, 119, 120, 121, 121, 122, 122, 123, 123, 124, 124, 124, 124, 125, 125, 125, 125,
      126, 126, 127, 127, 128, 128, 129, 129, 129, 129, 129, 129, 130, 130, 130, 130
  };

  const signed char parser::yyr2_[] = {
      0, 2, 2, 2, 1, 4, 3, 2, 2, 6, 4, 3, 3, 2, 1, 2, 1, 1, 7, 0, 2, 0, 3, 5, 2, 1, 3, 2, 2, 2, 1, 3, 0, 2, 1, 1, 6, 4,
      8, 6, 1, 2, 1, 2, 0, 1, 1, 1, 5, 3, 0, 1, 1, 2, 1, 3, 1, 3, 1, 1, 2, 7, 6, 4, 5, 4, 2, 5, 4, 5, 4, 2, 5, 4, 1, 1,
      1, 4, 2, 4, 3, 1, 1, 5, 4, 2, 3, 3, 4, 5, 6, 6, 5, 7, 6, 1, 3, 2, 2, 2, 4, 1, 3, 4, 5, 3, 5, 6, 7, 2, 1, 3, 4, 5,
      4, 3, 5, 6, 7, 2, 4, 5, 7, 4, 6, 2, 4, 5, 7, 4, 6, 0, 1, 1, 3, 4, 1, 3, 2, 2, 2, 1, 1, 3, 2, 3, 0, 1, 1, 1, 1, 1,
      1, 1, 1, 1, 0, 1, 3, 2, 3, 5, 4, 3, 2, 0, 1, 3, 4, 4, 5, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 1, 0, 1, 0,
      1, 1, 1, 1, 1, 1, 1, 1, 2, 1, 1, 1, 2, 1, 1, 1, 1, 1, 2, 1, 1, 1, 2, 1, 1, 2, 2, 1, 2, 0, 1, 1, 1
  };


#if YYDEBUG
  const short parser::yyrline_[] = {
      0,   172, 172, 173, 178, 180, 191, 205, 209, 215, 227, 241, 244, 247, 273, 275, 280, 281, 285, 290, 291, 295, 296,
      300, 305, 310, 312, 317, 319, 321, 326, 328, 333, 334, 338, 339, 343, 345, 347, 349, 354, 355, 360, 361, 365, 366,
      369, 369, 372, 374, 379, 380, 384, 385, 389, 390, 394, 396, 404, 405, 406, 410, 419, 422, 426, 435, 438, 441, 452,
      462, 477, 480, 486, 497, 507, 509, 511, 512, 517, 522, 529, 539, 543, 545, 550, 556, 558, 564, 567, 570, 573, 581,
      583, 586, 589, 594, 595, 607, 610, 611, 612, 617, 619, 621, 623, 625, 627, 629, 631, 636, 638, 640, 642, 644, 646,
      648, 650, 652, 654, 659, 660, 661, 663, 665, 670, 679, 681, 683, 685, 687, 692, 705, 706, 710, 712, 714, 723, 725,
      729, 733, 734, 735, 739, 740, 743, 746, 749, 750, 754, 755, 756, 757, 758, 759, 760, 761, 765, 767, 769, 771, 773,
      775, 777, 785, 787, 792, 794, 796, 801, 806, 808, 817, 818, 819, 822, 831, 832, 833, 834, 835, 836, 837, 838, 842,
      843, 847, 849, 853, 853, 854, 854, 856, 858, 861, 862, 866, 867, 871, 872, 876, 877, 878, 879, 888, 889, 890, 891,
      895, 897, 902, 904, 909, 911, 916, 917, 918, 919, 920, 921, 925, 926, 927, 928
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
#line 4863 "langutils/sc_parser/src/sc_grammar_parser.cpp"

#line 931 "langutils/sc_parser/src/sc_grammar.y"
