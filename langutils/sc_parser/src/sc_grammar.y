%require "3.8"
%language "C++"

%define api.value.type variant
%define api.token.prefix {TOKEN_}
%define api.namespace {sc::parser}
%define parse.error custom

%locations
%define api.location.type { sc::lex::SourceCodeRange }


// This goes in the header
%code requires
{

#include "parser_context.hpp"

namespace sc::parser {

struct NoValue {}; // This is used to indicate that token rules don't create anything in the resulting graph. 

enum struct ReadWriteAccessor : std::uint8_t { Private, PublicRead, PublicWrite, PublicReadAndWrite };

}

}

// These are member variables in the parser class
%parse-param {ParserContext& cxt}

// These are arguments to the function yylex
%lex-param {ParserContext& cxt}

// This goes at the top of the source file
%code top
{

#include "sc_grammar_parser.hpp"
#include "indexes_typed.hpp"
#include "nodes.hpp"
#include "lexer.hpp"

namespace sc::parser{
class parser;
}

static int yylex(sc::parser::parser::value_type* v, sc::lex::SourceCodeRange* loc, sc::parser::ParserContext& cxt);


using namespace sc::parser::nodes;

    
}



// There is still location data, just no semantic value (the token tag is all the data you need)
%token <NoValue> OPENCURLY CLOSECURLY OPENSQUARE CLOSESQUARE OPENPAREN CLOSEPAREN 
%token <NoValue> SEMICOLON NONLOCALRETURN COMMA HASH TILDE

// TODO: these literal tokens should be expand into all the types the lexer recognizes and turned into nodes (lower case versions).
%token <NoValue> NAME INTEGER INTEGER_RADIX HEXADECIMAL FLOAT FLOAT_RADIX FLOAT_EXPONENT FLOAT_INF ACCIDENTAL_STEPS ACCIDENTAL_CENTS SYMBOL_QUOTE SYMBOL_SLASH STRINGLINE ASCII PRIMITIVENAME CLASSNAME CURRYARG 
%token <NoValue> VAR ARG CLASSVAR SC_CONST
%token <NoValue> NILOBJ TRUEOBJ FALSEOBJ PI
%token <NoValue> ELLIPSIS DOTDOT BEGINCLOSEDFUNC
%token <NoValue> BADTOKEN INTERPRET
%token <NoValue> LEFTARROW WHILE

%left  <NoValue> COLON
%right <NoValue> EQUALSSIGN
%left  <NoValue> BINOP KEYBINOP MINUS LESSTHAN GREATERTHAN MULTIPLY ADD PIPE READWRITEVAR
%left  <NoValue> DOT
%right <NoValue> BACKTICK
%right <NoValue> UMINUS

////////////////////////////////////////////////////////////////////////////////
// types
////////////////////////////////////////////////////////////////////////////////
%type <MessageIndex> commandLine

%type <MessageIndex> basicBinOp

%type <NameIndex> name

%type <SelectorIndex> binary_op binary_op.raw


// literals
%type <AnyLiteralIndex> literal

%type <NilLitIndex> nil
%type <BooleanLitIndex> boolean
%type <SymbolLitIndex> symbol
%type <StringLitIndex> string
%type <IntLitIndex> integer
%type <FloatLitIndex> float.raw float.raw_unsigned
%type <AccidentalLitIndex> accidental accidental.unsigned
%type <FloatProducingIndex> float



// misc
%type <ReadWriteAccessor> rwspec 


////////////////////////////////////////////////////////////////////////////////
// start rule
////////////////////////////////////////////////////////////////////////////////

%start commandLine


////////////////////////////////////////////////////////////////////////////////
// rules
////////////////////////////////////////////////////////////////////////////////

%%

commandLine: INTERPRET basicBinOp { $$ = $2; };


basicBinOp : literal binary_op literal 
				{ $$ = cxt.create( MessageNode{}, @$, $1, $2, $3  ); }
			;

literal : symbol { $$ = $1; }
		| string { $$ = $1; }
		| integer { $$ = $1; }
		| float { $$ = $1; }
		| boolean { $$ = $1; }
		| nil { $$ = $1; }
		;


name 	: NAME { $$ = cxt.create(NameNode{}, @$); }
		| WHILE { $$ = cxt.create(NameNode{}, @$); }
		;

binary_op.raw	: binary_op { $$ = cxt.create(SelectorNode{ false }, @$); }	
				| READWRITEVAR { $$ = cxt.create(SelectorNode{ false }, @$); }	
				| LESSTHAN { $$ = cxt.create(SelectorNode{ false }, @$); }	
				| GREATERTHAN { $$ = cxt.create(SelectorNode{ false }, @$); }	
				| MINUS { $$ = cxt.create(SelectorNode{ false }, @$); }	
				| MULTIPLY { $$ = cxt.create(SelectorNode{ false }, @$); }	
				| ADD { $$ = cxt.create(SelectorNode{ false }, @$); }	
				| PIPE { $$ = cxt.create(SelectorNode{ false }, @$); }	
				;

binary_op 	: binary_op.raw   { $$ = $1; }
			| KEYBINOP { $$ = cxt.create(SelectorNode{ true }, @$); } 
			;

nil 	: NILOBJ { $$ = cxt.create(NilNode{}, @$); };

boolean	: TRUEOBJ { $$ = cxt.create(BooleanNode{true}, @$); }
		| FALSEOBJ { $$ = cxt.create(BooleanNode{false}, @$); }
		;

symbol 	: SYMBOL_QUOTE { $$ = cxt.create(SymbolNode{SymbolNode::Kind::Quote}, @$); }
		| SYMBOL_SLASH { $$ = cxt.create(SymbolNode{SymbolNode::Kind::Slash}, @$); }
		;

string 	: STRINGLINE 
		{
			auto line = cxt.create(StringLineNode{}, @$);
			auto list = cxt.create(StringLineList{}, @$);
			$$ = cxt.graph.append_to_list(list, line);
		}
		| string STRINGLINE
			{
				auto list = $1;
				auto line = cxt.create(StringLineNode{}, @2);
				$$ = cxt.graph.append_to_list(list, line);
			}
		;

integer	: INTEGER { $$ = cxt.create(IntNode{}, @$); }
		| INTEGER_RADIX { $$ = cxt.create(IntNode{IntNode::Kind::Radix}, @$); }
		| HEXADECIMAL { $$ = cxt.create(IntNode{IntNode::Kind::Hexadecimal}, @$); }
		| MINUS integer %prec UMINUS 
			{
				// Reaches into the previous integer and changes its sign.
				cxt.graph.get_payload($2).sign = IntNode::Sign::Negative;
				$$ = $2;
			}
		;

float.raw_unsigned 	: FLOAT { $$ = cxt.create(FloatNode{}, @$); }
					| FLOAT_RADIX { $$ = cxt.create(FloatNode{FloatNode::Kind::Radix}, @$); }
					| FLOAT_EXPONENT { $$ = cxt.create(FloatNode{FloatNode::Kind::Exponent}, @$); }
					| FLOAT_INF { $$ = cxt.create(FloatNode{FloatNode::Kind::Inf}, @$); }
					;

float.raw	: float.raw_unsigned { $$ = $1; }
			| MINUS float.raw_unsigned %prec UMINUS 
				{
					cxt.graph.get_payload($2).sign = FloatNode::Sign::Negative;
					$$ = $2;
				}
			;

accidental.unsigned : ACCIDENTAL_STEPS { $$ = cxt.create(AccidentalNode{AccidentalNode::Kind::Steps}, @$); }
					| ACCIDENTAL_CENTS { $$ = cxt.create(AccidentalNode{AccidentalNode::Kind::Cents}, @$); }
					;

accidental  : accidental.unsigned { $$ = $1; }
			| MINUS accidental.unsigned %prec UMINUS 
				{ 
					cxt.graph.get_payload($2).sign = AccidentalNode::Sign::Negative;
					$$ = $2;
				}
			;

float	: float.raw { $$ = $1; }
		| accidental { $$ = $1; }
		| float.raw PI { $$ = cxt.create(PiNode{}, @$, $1); }
		| integer PI { $$ = cxt.create(PiNode{}, @$, $1); }
		| PI  { $$ = cxt.create(PiNode{}, @$, cxt.create(Missing{}, @$)); }
		| MINUS PI { $$ = cxt.create(PiNode{PiNode::Sign::Negative}, @$, cxt.create(Missing{}, @$)); }
		;

rwspec	: %empty { $$ = ReadWriteAccessor::Private; }
		| LESSTHAN { $$ = ReadWriteAccessor::Read; }
		| READWRITEVAR { $$ = ReadWriteAccessor::ReadWrite; }
		| GREATERTHAN { $$ = ReadWriteAccessor::Write; }
		;

%%


inline sc::parser::parser::token_kind_type to_parser_token(sc::lex::TokenType t) {
	using T = sc::parser::parser::token_kind_type;
	using TokenType = sc::lex::TokenType;
	switch(t){
		case TokenType::EndOfFile: return T::TOKEN_YYEOF;
        case TokenType::Name: return T::TOKEN_NAME;
        case TokenType::ClassName: return T::TOKEN_CLASSNAME;
        case TokenType::PrimitiveName: return T::TOKEN_YYUNDEF;
		case TokenType::Integer: return T::TOKEN_INTEGER;
		case TokenType::IntegerRadix: return T::TOKEN_INTEGER_RADIX;
		case TokenType::Hexadecimal: return T::TOKEN_HEXADECIMAL;
		case TokenType::Float: return T::TOKEN_FLOAT;
		case TokenType::FloatRadix: return T::TOKEN_FLOAT_RADIX;
		case TokenType::FloatExponent: return T::TOKEN_FLOAT_EXPONENT;
		case TokenType::Pi: return T::TOKEN_PI;
		case TokenType::Inf: return T::TOKEN_FLOAT_INF;
		case TokenType::AccidentalSteps: return T::TOKEN_YYUNDEF;
		case TokenType::AccidentalCents: return T::TOKEN_YYUNDEF;
		case TokenType::SymbolSlash: return T::TOKEN_SYMBOL_SLASH;
		case TokenType::SymbolQuote: return T::TOKEN_SYMBOL_QUOTE;
		case TokenType::Ascii: return T::TOKEN_YYUNDEF;
		case TokenType::True: return T::TOKEN_YYUNDEF;
		case TokenType::False: return T::TOKEN_YYUNDEF;
		case TokenType::Nil: return T::TOKEN_YYUNDEF;
		case TokenType::StringLine: return T::TOKEN_YYUNDEF;
        case TokenType::While: return T::TOKEN_YYUNDEF;
        case TokenType::Var: return T::TOKEN_YYUNDEF;
        case TokenType::Arg: return T::TOKEN_YYUNDEF;
        case TokenType::ClassVar: return T::TOKEN_YYUNDEF;
        case TokenType::Const: return T::TOKEN_YYUNDEF;
		case TokenType::OpenParen : return T::TOKEN_YYUNDEF;
		case TokenType::OpenSquare : return T::TOKEN_YYUNDEF;
		case TokenType::OpenCurly : return T::TOKEN_YYUNDEF;
		case TokenType::BeginClosedFunction: return T::TOKEN_YYUNDEF;
		case TokenType::CloseParen : return T::TOKEN_YYUNDEF;
		case TokenType::CloseSquare: return T::TOKEN_YYUNDEF;
		case TokenType::CloseCurly: return T::TOKEN_YYUNDEF;
        case TokenType::SemiColon : return T::TOKEN_YYUNDEF;
        case TokenType::Colon: return T::TOKEN_YYUNDEF;
        case TokenType::Comma: return T::TOKEN_YYUNDEF;
        case TokenType::EqualsSign: return T::TOKEN_YYUNDEF;
        case TokenType::NonLocalReturn: return T::TOKEN_YYUNDEF;
        case TokenType::BackTick: return T::TOKEN_YYUNDEF;
        case TokenType::Tilde: return T::TOKEN_YYUNDEF;
        case TokenType::Hash: return T::TOKEN_YYUNDEF;
        case TokenType::LeftArrow : return T::TOKEN_YYUNDEF;
        case TokenType::Ellipsis: return T::TOKEN_YYUNDEF;
        case TokenType::Dot: return T::TOKEN_YYUNDEF;
        case TokenType::DotDot: return T::TOKEN_YYUNDEF;
        case TokenType::CurryArg: return T::TOKEN_YYUNDEF;
        case TokenType::Pipe : return T::TOKEN_YYUNDEF;
        case TokenType::ReadWriteVar: return T::TOKEN_YYUNDEF;
        case TokenType::Minus: return T::TOKEN_YYUNDEF;
        case TokenType::Multiply: return T::TOKEN_YYUNDEF;
        case TokenType::Add: return T::TOKEN_ADD;
        case TokenType::LessThan: return T::TOKEN_YYUNDEF;
        case TokenType::GreaterThan: return T::TOKEN_YYUNDEF;
        case TokenType::BinaryOperator : return T::TOKEN_YYUNDEF;
        case TokenType::KeywordBinaryOperator: return T::TOKEN_YYUNDEF;
        case TokenType::Space : return T::TOKEN_YYUNDEF;
        case TokenType::NewLine: return T::TOKEN_YYUNDEF;
        case TokenType::Tab: return T::TOKEN_YYUNDEF;
        case TokenType::Comment : return T::TOKEN_YYUNDEF;
        case TokenType::MultilineComment: return T::TOKEN_YYUNDEF;
		default: return T::TOKEN_YYUNDEF;
	}

}




static int yylex(sc::parser::parser::value_type* v, sc::lex::SourceCodeRange* loc, sc::parser::ParserContext& cxt){
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

	if(sc::lex::is_error(lex_token)) {
		cxt.state = sc::parser::ParserContext::State::Failure;
		cxt.error_handler->operator()(cxt.text_info, lex_token, location, extra_location);
		return T::TOKEN_YYerror;
	}

	return static_cast<int>(to_parser_token(lex_token));
}

void sc::parser::parser::report_syntax_error (const context& symbol_cxt) const {
	
	std::vector<symbol_kind_type> expected(5);
	expected.resize(symbol_cxt.expected_tokens(expected.data(), 5));

	std::vector<const char*> expected_names;
	expected_names.reserve(expected.size());
	for(const auto s : expected)
		expected_names.push_back( sc::parser::parser::symbol_name(s) );
	
    const auto got = symbol_cxt.token();
    const char* got_name = parser::symbol_name(got);
	cxt.error_handler->operator()(cxt.text_info, std::move(expected_names), symbol_cxt.location(),got_name);
}

void sc::parser::parser::error(const sc::lex::SourceCodeRange &loc , const std::string &message) {
	cxt.error_handler->operator()(cxt.text_info, loc, message);
}
