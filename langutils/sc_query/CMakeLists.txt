include(${CMAKE_SOURCE_DIR}/cmake_modules/CompilerWarnings.cmake)

# The protocol-free query core shared by sc_lsp and sc_mcp, and their JSON-RPC transport.
add_library(sc_query STATIC
    src/completion.cpp
    src/document.cpp
    src/hooks.cpp
    src/json.cpp
    src/jsonrpc.cpp
    src/library.cpp
    src/names.cpp
    src/text.cpp
)

target_include_directories(sc_query PUBLIC include)
target_link_libraries(sc_query PUBLIC sc_compiler PRIVATE Boost::thread)
if(SC_LSP_CONNECTED)
    target_compile_definitions(sc_query PRIVATE SC_LSP_CONNECTED)
    if(WIN32)
        target_link_libraries(sc_query PRIVATE ws2_32 mswsock)
    endif()
endif()
set_property(TARGET sc_query PROPERTY FOLDER Libs)
