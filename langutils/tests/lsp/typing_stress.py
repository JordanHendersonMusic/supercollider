"""Types files into sc_lsp a few characters at a time, asking for completion, hover and signature help at each step,
as an editor would, and fails if the server dies. Broken code is what a language server mostly sees.

    python3 typing_stress.py --lsp path/to/sc_lsp [--library DIR] [--step N] FILE...
"""
import argparse
import os
import sys

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from lsp_client import LspClient  # noqa: E402


def position(text):
    lines = text.split('\n')
    return {'line': len(lines) - 1, 'character': len(lines[-1].encode('utf-16-le')) // 2}


def main():
    here = os.path.dirname(os.path.abspath(__file__))
    parser = argparse.ArgumentParser()
    parser.add_argument('--lsp', required=True)
    parser.add_argument('--library', default=os.path.normpath(os.path.join(here, '..', '..', '..', 'SCClassLibrary')))
    parser.add_argument('--step', type=int, default=7)
    parser.add_argument('files', nargs='+')
    args = parser.parse_args()
    c = LspClient([args.lsp, '--library', args.library, '--no-hooks'])
    c.request('initialize', {'capabilities': {}})
    c.notify('initialized', {})
    c.library_built()
    steps = 0
    for path in args.files:
        with open(path, encoding='utf-8', errors='replace') as f:
            full = f.read()
        uri = 'file://' + os.path.abspath(path) + ('.typed.sc' if path.endswith('.sc') else '.typed.scd')
        c.notify('textDocument/didOpen', {'textDocument': {'uri': uri, 'languageId': 'supercollider', 'version': 0,
                                                           'text': ''}})
        version = 0
        for end in list(range(0, len(full), args.step)) + [len(full)]:
            text = full[:end]
            version += 1
            c.notify('textDocument/didChange', {'textDocument': {'uri': uri, 'version': version},
                                                 'contentChanges': [{'text': text}]})
            at = {'textDocument': {'uri': uri}, 'position': position(text)}
            try:
                c.request('textDocument/completion', at)
                c.request('textDocument/hover', at)
                c.request('textDocument/signatureHelp', at)
            except Exception as e:
                print('FAILED at', path, end, repr(text[-60:]), e)
                print(''.join(c.stderr)[-3000:])
                return 1
            steps += 1
            if c.proc.poll() is not None:
                print('CRASHED at', path, end, repr(text[-60:]))
                print(''.join(c.stderr)[-3000:])
                return 1
        c.request('textDocument/documentSymbol', {'textDocument': {'uri': uri}})
        c.notify('textDocument/didClose', {'textDocument': {'uri': uri}})
    print(steps, 'steps,', len(args.files), 'files, no crashes')
    return 0 if c.close() == 0 else 1


if __name__ == '__main__':
    sys.exit(main())
