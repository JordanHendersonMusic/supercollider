include(${CMAKE_SOURCE_DIR}/cmake_modules/CompilerWarnings.cmake)


add_library(sc_parser STATIC 
	src/main.cpp
	src/node_graph.cpp
	src/sc_parser.cpp
	src/sc_grammar_parser.cpp
)


target_include_directories(sc_parser PUBLIC include)
target_include_directories(sc_parser PRIVATE src)
target_link_libraries(sc_parser PUBLIC sc_lexer)
#set_target_compiler_strict_errors(sc_parser)
set_property(TARGET sc_parser PROPERTY FOLDER Libs)

if(ENABLE_TESTSUITE)
	add_subdirectory(tests)
endif()

