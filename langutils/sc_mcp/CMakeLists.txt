add_executable(sc_mcp main.cpp)
target_link_libraries(sc_mcp PRIVATE sc_query)
set_property(TARGET sc_mcp PROPERTY FOLDER LangUtils)
install(TARGETS sc_mcp RUNTIME DESTINATION "bin")
