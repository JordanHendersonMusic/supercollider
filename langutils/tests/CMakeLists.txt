add_subdirectory(error_regression)

# sc_lsp and sc_mcp, end to end against the repository's class library.
if(TARGET sc_lsp AND TARGET sc_mcp)
    add_test(
        NAME langutils.servers
        COMMAND python3 ${CMAKE_CURRENT_SOURCE_DIR}/lsp/test_servers.py --lsp=$<TARGET_FILE:sc_lsp>
                --mcp=$<TARGET_FILE:sc_mcp> --library=${CMAKE_SOURCE_DIR}/SCClassLibrary
    )
endif()
