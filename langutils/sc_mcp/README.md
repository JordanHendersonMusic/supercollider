# sc_mcp

An MCP server for SuperCollider, over stdin and stdout, for coding agents.

```
sc_mcp [--library PATH]... [--exclude PATH]... [--interpreted-version 3.16|legacy] [--sclang-port N] [--no-hooks] [--allow-eval]
```

Tools, answering in text with file:line:

| Tool | |
| --- | --- |
| `check_code`, `check_file` | Compile code or a file against the class library, without running it. |
| `class_info`, `implementors`, `find_definition`, `subclasses`, `search`, `references` | Look things up in the class library. |
| `hooks` | The `LSPHook` subclasses, and whether the running sclang can run them. |
| `complete_symbol` | Ask the running sclang's hooks, e.g. which SynthDef names fit `\instrument`. Connected builds only. |
| `evaluate` | Run code in the running sclang. Only with `--allow-eval`, and only if sclang has `LSPHook.allowEval = true`. Each evaluation is posted in sclang's post window. |

The library is read from disk and rebuilt when its files change.
