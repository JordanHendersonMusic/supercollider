add_executable(sc_lsp main.cpp)
target_link_libraries(sc_lsp PRIVATE sc_query)
set_property(TARGET sc_lsp PROPERTY FOLDER LangUtils)
install(TARGETS sc_lsp RUNTIME DESTINATION "bin")
