# sc_query

The query core of the language server ([sc_lsp](../sc_lsp/README.md)) and the MCP server ([sc_mcp](../sc_mcp/README.md)).
It knows nothing about either protocol: it takes paths and text, and returns plain structs or readable text.

## Layout

| File | Contents |
| --- | --- |
| `library.hpp` | `build_library`: compiles the class library from disk, with editor text overlaid, and indexes its classes, methods, variables, symbols and hooks. |
| `document.hpp` | `Document`: an editor's text and its last compile. Interpreted code is compiled against the library, a class file is compiled as part of it. |
| `queries.hpp` | What is at a position (hover, definition, references), completion, signature help, outlines, a lint, and the name queries sc_mcp uses. |
| `hooks.hpp` | Running `LSPHook` subclasses in a running sclang over OSC. |
| `text.hpp` | Lines and codepoint columns, UTF-16 columns, URIs. |
| `json.hpp`, `jsonrpc.hpp` | A small JSON value, and JSON-RPC over stdio for both servers. |

## Design

- Positions are lines and codepoint columns, as the lexer counts them. The servers convert to UTF-16 columns at the edge.
- Hover, definition and references read the compiled IR. Completion and signature help read the text before the
  cursor, as the code being typed rarely parses as a whole. They use the last compile only for the locals in scope.
- Completion asks the parser which tokens it can take at the cursor (`sc::ast::expected_tokens`). So `Foo ba` is only
  the start of a keyword binary operator, `Foo bar: x`, as two expressions can't be next to each other. When an error
  earlier in a bracket hides this, the statement the cursor is in is parsed on its own.
- A receiver is found by parsing its statement with a placeholder in place of what is being typed, and its class by
  compiling it: literals, collections, functions, class names, and `Foo.new(...)` or `Foo(...)`. Otherwise a send
  could reach every implementor, and the queries say so.
- The lint is the tools' own: a send of a selector no class implements is an information diagnostic, as an Event,
  or anything else handling `doesNotUnderstand`, can still answer it.

## Hooks

A hook is a subclass of `LSPHook` (`SCClassLibrary/Common/Tooling/LSPHook.sc`). The library finds them from the class
hierarchy, without running anything. They only run by asking a running sclang:

```
/lsp/hello id                        -> /lsp/reply id "ok" {"version":1,"allowEval":bool,"hooks":{"Name":{"size":n}}}
/lsp/call id Class selector k v ...  -> /lsp/reply id "ok"|"error" json
/lsp/eval id code                    -> /lsp/reply id "ok"|"error" text
```

sclang only answers 127.0.0.1. A hook is refused when sclang compiled a different version of its file (the file sizes
differ) or hasn't compiled it at all, so stale hooks never run. A build without `SC_LSP_CONNECTED` runs no hooks.



# TODO:

Suggestions ordering should be influenced by how far away they are in the heirachy.
