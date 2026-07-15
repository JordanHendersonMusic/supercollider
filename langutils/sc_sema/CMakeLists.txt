include(${CMAKE_SOURCE_DIR}/cmake_modules/CompilerWarnings.cmake)

add_library(sc_sema STATIC 
	src/main.cpp
)

target_include_directories(sc_sema PUBLIC include)
target_include_directories(sc_sema PRIVATE src)
target_link_libraries(sc_sema PUBLIC sc_lexer sc_parser)
set_target_compiler_strict_errors(sc_parser)
set_property(TARGET sc_sema PROPERTY FOLDER Libs)

if(ENABLE_TESTSUITE)
	add_subdirectory(tests)
endif()

