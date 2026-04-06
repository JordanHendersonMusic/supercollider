add_executable(lexer_compare_file compare_file.cpp)

target_include_directories(lexer_compare_file PUBLIC 
	${CMAKE_SOURCE_DIR}/include/lexer
)

set_property(TARGET lexer_compare_file PROPERTY FOLDER Util/Lexer)
