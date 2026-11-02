# sc_lsp

A language server for SuperCollider, over stdin and stdout. The Qt IDE starts it; any editor with an LSP client can too.

```
sc_lsp [--library PATH]... [--exclude PATH]... [--interpreted-version 3.16|legacy] [--sclang-port N] [--no-hooks]
```

Without `--library`, the client can give `initializationOptions.libraryPaths`; otherwise it uses the class library
installed beside it and the default Extensions directories. Other options: `excludePaths`, `interpretedVersion`,
`sclangPort`, `hooks`.

## Features

- Diagnostics: `.scd` files are compiled as the IDE evaluates regions; `.sc` files are parsed as you type, and the whole
  library is rebuilt shortly after you stop (about 0.1 s in a Release build).
- Hover, go to definition, references, document and workspace symbols, signature help.
- Semantic tokens (`textDocument/semanticTokens/full`): the lexer's tokens of the current text, with the names of the
  last compile laid over them, e.g., locals, arguments, instance variables and sends. While the code doesn't parse,
  names from the last code that did are kept where the text around them is unchanged. Custom token types include
  `symbol`, `character`, `builtin`, `primitive`, `environmentVariable` and `keywordArgument`, and the modifier `unknown`
  marks a class name that isn't in the library.
- Completion of classes, messages, keyword arguments, locals, environment variables, and symbols. Symbol completion
  asks the running sclang's hooks, e.g. SynthDef names after `\instrument`.
- `sclang/status`, a custom request: the library and hook connection.
- Regions, for an editor's evaluate and its gutter: each top-level statement of an `.scd` document, with any brackets
  around it. Sent after each analysis as the `sclang/publishRegions` notification, `{uri, version, regions: [Range]}`,
  and answered for the latest text by the `sclang/regions` request, `{textDocument: {uri}}` -> `{version, regions}`.
  Advertised as `capabilities.experimental.regions`.

Tests: `langutils/tests/lsp/test_servers.py` (ctest `langutils.servers`), and `typing_stress.py`, which types files
into the server a few characters at a time.
