# Fuzz testing currently only works with clang as it uses libfuzz
if (CMAKE_CXX_COMPILER_ID STREQUAL "Clang")
	add_executable(lexer_fuzz_with_old lexer_fuzz_with_old.cpp)

	set_property(TARGET lexer_fuzz_with_old PROPERTY FOLDER Test/Lexer)
	target_compile_options(
		lexer_fuzz_with_old
		PRIVATE
		-fsanitize=fuzzer,address,undefined
		-fno-sanitize-recover=address,undefined
		-g
		-O3
	)

	target_include_directories(lexer_fuzz_with_old PUBLIC 
		${CMAKE_SOURCE_DIR}/include/lexer
	)

	target_link_options(
		lexer_fuzz_with_old
		PRIVATE
		-fsanitize=fuzzer,address,undefined
		-fno-sanitize-recover=address,undefined
	)
endif()
