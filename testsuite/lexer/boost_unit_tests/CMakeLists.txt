set(test_sources 
	lexer_test.cpp
	a_b.cpp
)

foreach(test ${test_sources})
    get_filename_component(test_name ${test} NAME_WE)
    set(test_target ${test_name}_run)
	add_executable(${test_target} ${test})
	target_link_libraries(${test_target} boost_test)
	target_include_directories(${test_target} PUBLIC 
					${boost_include_dirs}
                    ${CMAKE_SOURCE_DIR}/include/lexer
	)
	add_test(${test_name} ${test_target})
    set_property(TARGET ${test_target} PROPERTY FOLDER Test/Lexer)
endforeach(test)
