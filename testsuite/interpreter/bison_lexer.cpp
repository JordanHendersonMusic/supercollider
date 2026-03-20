#include <boost/test/unit_test.hpp>

#include "PyrLexer.h"
#include "PyrObjectProto.h"
#include "PyrParseNode.h"
#include "PyrSymbol.h"
#include "PyrSymbolTable.h"
#include "VMGlobals.h"

#include "Bison/lang11d_tab.h"
#include "boost/test/tools/interface.hpp"

#include <array>


inline yytokentype operator""_yyt(char c) { return static_cast<yytokentype>(c); }
inline yytokentype operator""_yyt(unsigned long long c) { return static_cast<yytokentype>(c); }


template <size_t N> void test(const char* txt, const std::array<yytokentype, N>& expected) {
    pyr_init_mem_pools(2 * 1024 * 1024, 256 * 1024);
    void* ptr = pyr_pool_runtime->Alloc(sizeof(SymbolTable));
    gMainVMGlobals->symbolTable = new (ptr) SymbolTable(pyr_pool_runtime, 65536);

    PyrSymbol* fileSym = getsym("some/test.sc");
    fileSym->u.source = const_cast<char*>(txt);

    initSymbols(); // initialize symbol globals
    initSpecialSelectors();
    initSpecialClasses();
    initClasses();
    initParserPool();
    initParseNodes();

    initParser();

    gCompilingFileSym = fileSym;
    gCompilingVMGlobals = nullptr;

    start_lexer_for_testing_class_lib(fileSym);

    size_t i { 0 };
    for (const auto& t : expected) {
        BOOST_TEST_REQUIRE(t == yylex());
    }
}

BOOST_AUTO_TEST_CASE(obj_lexer) {
    test(R"%%(
Object {
	classvar <dependantsDictionary, currentEnvironment, topEnvironment, <uniqueMethods;

	const nl = "\n";

	*new { arg maxSize = 0;
		_BasicNew
		^this.primitiveFailed
		// creates a new instance that can hold up to maxSize
		// indexable slots. the indexed size will be zero.
		// to actually put things in the object you need to
		// add them.
	}
}
)%%",
         // clang-format off
         std::array<yytokentype, 34>  {
             CLASSNAME, '{'_yyt, 
             CLASSVAR, '<'_yyt, NAME, ','_yyt,
             NAME,    ','_yyt,
             NAME, ','_yyt,
             '<'_yyt, NAME, ';'_yyt,
             SC_CONST, NAME, '='_yyt, STRING,  ';'_yyt,
             '*'_yyt,   NAME,    '{'_yyt,  
             ARG,     NAME,     '='_yyt, INTEGER, ';'_yyt, 
             PRIMITIVENAME, 
             '^'_yyt,   NAME,    '.'_yyt,  NAME,    
             '}'_yyt,
             '}'_yyt, 
             0_yyt 
        } // clang-format on
    );
}


BOOST_AUTO_TEST_CASE(some_methods) {
    test(R"%%(
	// equality, identity
	== { arg obj; ^this === obj }
	!= { arg obj; ^not(this == obj) }
)%%",
         // clang-format off
         std::array<yytokentype, 25> {

             BINOP, '{'_yyt, 
                ARG, NAME, ';'_yyt, '^'_yyt, NAME, BINOP, NAME, 
            '}'_yyt,

            BINOP, '{'_yyt, 
                ARG, NAME, ';'_yyt, '^'_yyt, NAME, '('_yyt,  NAME, BINOP, NAME, ')'_yyt, 
            '}'_yyt,
            0_yyt 
        }
         // clang-format on
    );
}
