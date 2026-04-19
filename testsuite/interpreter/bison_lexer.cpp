#include <boost/test/unit_test.hpp>

#include "PyrLexer.h"
#include "PyrObjectProto.h"
#include "PyrParseNode.h"
#include "PyrSlot.h"
#include "PyrSymbol.h"
#include "PyrSymbolTable.h"
#include "SCBase.h"
#include "VMGlobals.h"

#include "bisonHeaderInclude.hpp"
#include "Bison/lang11d_tab.h"
#include "boost/test/tools/interface.hpp"

#include <array>
#include <tuple>


inline yytokentype operator""_yyt(char c) { return static_cast<yytokentype>(c); }
inline yytokentype operator""_yyt(unsigned long long c) { return static_cast<yytokentype>(c); }


template <size_t N> void test_tokens(const char* txt, const std::array<yytokentype, N>& expected) {
    pyr_init_mem_pools(2 * 1024 * 1024, 256 * 1024);
    void* ptr = pyr_pool_runtime->Alloc(sizeof(SymbolTable));
    gMainVMGlobals->symbolTable = new (ptr) SymbolTable(pyr_pool_runtime, 65536);
    gMainVMGlobals->gc = nullptr;

    PyrSymbol* fileSym = getsym("some/test.sc");
    fileSym->u.source = const_cast<char*>(txt);

    initSymbols(); // initialize symbol globals
    initSpecialSelectors();
    initSpecialClasses();
    initClasses();
    initParserPool();

    initParser();

    gCompilingFileSym = fileSym;
    gCompilingVMGlobals = nullptr;

    startLexerForTestingClassLib(fileSym);

    for (const auto& t : expected) {
        BOOST_TEST_REQUIRE(t == yylex());
    }
}


BOOST_AUTO_TEST_CASE(obj_lexer) {
    test_tokens(R"%%(
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
    test_tokens(R"%%(
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
        } // clang-format on
    );
}


template <size_t N> void test_zzval(const char* txt, const std::array<std::tuple<yytokentype, PyrSlot>, N>& expected) {
    PyrSymbol* fileSym = getsym("some/test.sc");
    fileSym->u.source = const_cast<char*>(txt);

    initSymbols(); // initialize symbol globals
    initSpecialSelectors();
    initSpecialClasses();
    initClasses();
    initParserPool();

    initParser();

    gCompilingFileSym = fileSym;
    gCompilingVMGlobals = nullptr;

    startLexerForTestingClassLib(fileSym);

    for (const auto& t : expected) {
        const auto token = yylex();
        BOOST_TEST(token == std::get<0>(t));
        const auto* slot_node = yylval.slotNode;
        BOOST_TEST(slot_node);
        const PyrSlot slot = slot_node->mSlot;
        const PyrSlot expected_slot = std::get<1>(t);
        BOOST_TEST_REQUIRE(slot.getTag() == expected_slot.getTag());
        const auto equals = slot == expected_slot;
        BOOST_TEST(equals);
        if (!equals) {
            switch (slot.getTag()) {
            case tagFloat: {
                std::cout << slot.getDouble() << " " << expected_slot.getDouble() << std::endl;
                BOOST_TEST(slot.getDouble() == expected_slot.getDouble());
                break;
            };
            case tagSym: {
                const auto l = slot.getSymbol()->name;
                const auto r = expected_slot.getSymbol()->name;
                std::cout << "'" << l << "'"
                          << " "
                          << "'" << r << "'" << std::endl;
                BOOST_TEST(l == r);
                break;
            };
            default:
                // Printing not implemented yet.
                BOOST_TEST_REQUIRE(false);
                break;
            }
        }
    }
}

BOOST_AUTO_TEST_CASE(zzval_test) {
    pyr_init_mem_pools(2 * 1024 * 1024, 256 * 1024);
    void* ptr = pyr_pool_runtime->Alloc(sizeof(SymbolTable));
    gMainVMGlobals->symbolTable = new (ptr) SymbolTable(pyr_pool_runtime, 65536);
    gMainVMGlobals->gc = nullptr;

    test_zzval(" \\abc 0.1 12rABClass",
               std::array<std::tuple<yytokentype, PyrSlot>, 4> { {
                   { SYMBOL, PyrSlot::make(getsym("abc")) },
                   { SC_FLOAT, PyrSlot::make(0.1) },
                   { INTEGER, PyrSlot::make(131) },
                   { CLASSNAME, PyrSlot::make(getsym("Class")) },
               } });
}
