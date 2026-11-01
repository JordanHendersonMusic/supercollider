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
- Completion of classes, messages, keyword arguments, locals, environment variables, and symbols. Symbol completion
  asks the running sclang's hooks, e.g. SynthDef names after `\instrument`.
- `sclang/status`, a custom request: the library and hook connection.

Tests: `langutils/tests/lsp/test_servers.py` (ctest `langutils.servers`), and `typing_stress.py`, which types files
into the server a few characters at a time.
