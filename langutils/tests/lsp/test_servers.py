"""Tests sc_lsp and sc_mcp end to end, against the repository's class library.

    python3 test_servers.py --lsp path/to/sc_lsp --mcp path/to/sc_mcp [--library path/to/SCClassLibrary]

Hooks aren't tested here, as they need a running sclang.
"""
import argparse
import json
import os
import subprocess
import sys
import unittest

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from lsp_client import LspClient  # noqa: E402

ARGS = None
SCD_URI = 'file:///tmp/sc_lsp_test/test.scd'
SC_URI = 'file:///tmp/sc_lsp_test/LspTestClass.sc'

CLASS_TEXT = '''LspTestClass : Object {
    var <>iv, other;
    classvar cv = 3;
    *new { |a = 1| ^super.new.init(a) }
    init { |a| iv = a; ^this.bar(cv) }
    bar { |x| var y = x + 1; ^y + other }
}
'''


class LspTest(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.c = LspClient([ARGS.lsp, '--library', ARGS.library, '--no-hooks'])
        cls.init = cls.c.request('initialize', {'capabilities': {}})
        cls.c.notify('initialized', {})
        cls.c.library_built()
        cls.version = 0

    @classmethod
    def tearDownClass(cls):
        assert cls.c.close() == 0

    def open(self, text, uri=SCD_URI):
        type(self).version += 1
        self.c.notify('textDocument/didOpen', {'textDocument': {
            'uri': uri, 'languageId': 'supercollider', 'version': self.version, 'text': text}})
        return self.c.diagnostics(uri, self.version)

    def change(self, text, uri=SCD_URI):
        type(self).version += 1
        self.c.notify('textDocument/didChange', {'textDocument': {'uri': uri, 'version': self.version},
                                                 'contentChanges': [{'text': text}]})
        return self.c.diagnostics(uri, self.version)

    def at(self, line, character, uri=SCD_URI):
        return {'textDocument': {'uri': uri}, 'position': {'line': line, 'character': character}}

    def hover(self, line, character, uri=SCD_URI):
        r = self.c.request('textDocument/hover', self.at(line, character, uri))
        return r and r['contents']['value']

    def labels(self, line, character, uri=SCD_URI):
        r = self.c.request('textDocument/completion', self.at(line, character, uri))
        return [i['label'] for i in r['items']]

    def test_capabilities(self):
        caps = self.init['capabilities']
        self.assertTrue(caps['hoverProvider'])
        self.assertEqual(caps['positionEncoding'], 'utf-16')

    def test_diagnostics(self):
        d = self.open('(\nvar x = SinOsc.ar(;\n)')
        self.assertTrue(any(x['severity'] == 1 for x in d))
        d = self.change('x = 3.blahblah;')
        self.assertEqual([x['code'] for x in d], ['Unknown selector.'])
        self.assertEqual(d[0]['severity'], 3)
        self.assertEqual(self.change('x = 3.squared;'), [])
        d = self.change('(\nvar a;\nundefinedThing + 1\n)')
        self.assertIn('`undefinedThing` is not defined', d[0]['message'])

    def test_hover_and_definition(self):
        self.open('(\nvar freq = 440, sig;\nsig = SinOsc.ar(freq, 0, 0.1);\n)')
        self.assertIn('SinOsc : PureUGen', self.hover(2, 8))
        self.assertIn('SinOsc.*ar(freq = 440.0', self.hover(2, 14))
        self.assertIn('var freq', self.hover(2, 18))
        d = self.c.request('textDocument/definition', self.at(2, 8))
        self.assertTrue(d[0]['uri'].endswith('/Osc.sc'))
        self.assertEqual(d[0]['range']['end']['character'] - d[0]['range']['start']['character'], 6)
        d = self.c.request('textDocument/definition', self.at(2, 18))
        self.assertEqual(d, [{'uri': SCD_URI, 'range': {'start': {'line': 1, 'character': 4},
                                                         'end': {'line': 1, 'character': 14}}}])
        r = self.c.request('textDocument/references', {**self.at(2, 18), 'context': {'includeDeclaration': True}})
        self.assertEqual(len(r), 2)

    def test_guessed_receivers(self):
        self.open('x = Point(1, 2).x;\ny = Point.new(1, 2).rho;\n')
        self.assertIn('Point:x', self.hover(0, 16))
        self.assertIn('Point:rho', self.hover(1, 20))

    def test_utf16_columns(self):
        # é is one UTF-16 unit, the emoji two.
        self.open('x = "é😀"; SinOsc.ar;')
        self.assertIn('SinOsc : PureUGen', self.hover(0, 13))

    def test_completion(self):
        self.open('(\nvar freq = 440;\nSinOsc.a\n)')
        self.assertIn('ar', self.labels(2, 8))
        self.change('(\nvar freq = 440;\nSinO\n)')
        self.assertEqual(sorted(self.labels(2, 4)), ['SinOsc', 'SinOscFB'])
        self.change('(\nvar freq = 440;\nSinOsc.ar(fr\n)')
        self.assertEqual(self.labels(2, 12), ['freq:', 'freq'])
        self.change('(\nvar freq = 440;\nPbind(\\inst\n)')
        self.assertIn('instrument', self.labels(2, 11))
        self.change('~freq = 3;\n~fr')
        self.assertEqual(self.labels(1, 3), ['freq'])
        self.change('Point(1, 2).rh')
        self.assertEqual(self.labels(0, 14), ['rho'])
        self.change('"a string".siz')
        self.assertEqual(self.labels(0, 14), ['size'])
        self.change('// SinO')
        self.assertEqual(self.labels(0, 7), [])

    def items(self, text, uri=SCD_URI):
        """The completions at the end of text."""
        lines = text.split('\n')
        self.change(text, uri)
        r = self.c.request('textDocument/completion', self.at(len(lines) - 1, len(lines[-1]), uri))
        return r['items']

    def test_grammar_completion(self):
        # What the parser can take at the cursor decides what is offered.
        self.open('')
        # Two expressions can't be next to each other, so this is a keyword binary operator, `SinOsc ar: 440`.
        items = self.items('SinOsc a')
        self.assertIn('ar:', [i['label'] for i in items])
        self.assertEqual([i['textEdit']['newText'] for i in items if i['label'] == 'ar:'], ['ar: '])
        self.assertNotIn('SinOsc', [i['label'] for i in items])
        self.assertIn('dist:', [i['label'] for i in self.items('x = Point(1, 2) dis')])
        # A class name can't follow an expression.
        self.assertEqual([i['label'] for i in self.items('SinOsc Si')], [])
        # Whitespace and comments don't matter.
        self.assertEqual([i['label'] for i in self.items('~ freq = 3;\n~ fr')], ['freq'])
        self.assertEqual([i['label'] for i in self.items('~freq = 3;\n~fr')], ['freq'])
        self.assertIn('ar', [i['label'] for i in self.items('SinOsc /* a comment */ .a')])
        self.assertIn('ar', [i['label'] for i in self.items('SinOsc // a comment\n.a')])
        # Numbers.
        floor = [i.get('detail', '') for i in self.items('1.5.floo')]
        self.assertTrue(any(d.startswith('Float:') or d.startswith('SimpleNumber:') for d in floor), floor)
        self.assertTrue(any(d.startswith('Array:reverse') for d in
                            [i.get('detail', '') for i in self.items('(1..4).reve')]))
        # In a string or a comment.
        self.assertEqual(self.items('"SinO'), [])
        self.assertEqual(self.items('/* SinO'), [])
        # Receivers in arguments, after keywords and argument lists.
        self.assertEqual([i['label'] for i in self.items('SinOsc.ar(freq: Point(1, 2).rh')], ['rho'])
        self.assertTrue(any(i.get('detail', '').startswith('Array:reverse')
                            for i in self.items('f = { |a, b| [1].reve')))
        # Nested non-local returns before it.
        self.assertTrue(any(i.get('detail', '').startswith('Function:value')
                            for i in self.items('f = { |x| if (x) { ^1 }; ^2 };\n{ 1 }.valu')))
        # An error earlier in the bracket, here a missing semicolon, drops it in the parse, so the statement is used.
        self.assertIn('ar', [i['label'] for i in self.items('(\nx = 1\ny = SinOsc.a')])
        self.assertIn('ar:', [i['label'] for i in self.items('(\nx = ;\ny = SinOsc a')])
        self.assertEqual(sorted(i['label'] for i in self.items('(\nx = ;\ny = SinO')), ['SinOsc', 'SinOscFB'])

    def test_grammar_completion_class_file(self):
        # A method's body, after nested non-local returns: `^x ma` is only the start of `^x max: y`.
        text = CLASS_TEXT.replace('    bar {', '    baz { |x| if (x) { ^1 }; ^x ma }\n    bar {')
        self.open(text, SC_URI)
        line = text.split('\n').index('    baz { |x| if (x) { ^1 }; ^x ma }')
        r = self.c.request('textDocument/completion', self.at(line, len('    baz { |x| if (x) { ^1 }; ^x ma'), SC_URI))
        labels = [i['label'] for i in r['items']]
        self.assertIn('max:', labels)
        self.assertTrue(all(label.endswith(':') for label in labels), labels)
        self.c.notify('textDocument/didClose', {'textDocument': {'uri': SC_URI}})

    def test_compiled_receivers(self):
        # Receivers the text alone doesn't give, compiled to find their class.
        cases = [
            ('[1].reve', 'Array:reverse'), ('[1, 2] .reve', 'Array:reverse'), ('#[1, \\a].reve', 'Array:reverse'),
            ('Set[1, 2].remo', 'Set:remove'), ('FloatArray[1, 2].archiveAsC', 'RawArray:archiveAsCompileString'),
            ('(1..4).reve', 'Array:reverse'), ('(1 + 2).isPrim', 'Integer:isPrime'),
            ('(a: 1).isRe', 'Event:isRest'), ('[1, "]"].reve', 'Array:reverse'),
            ('x = [1].reve', 'Array:reverse'), ('[[1]].first.reve', None), ('{ 1 }.valu', 'Function:value'),
        ]
        self.open('')
        for text, detail in cases:
            self.change(text)
            r = self.c.request('textDocument/completion', self.at(0, len(text)))
            details = [i.get('detail', '') for i in r['items']]
            if detail:
                self.assertTrue(any(d.startswith(detail) for d in details), (text, details[:5]))
        self.change('[1, 2].at(')
        r = self.c.request('textDocument/signatureHelp', self.at(0, 10))
        self.assertIn('at(index)', r['signatures'][0]['label'])

    def test_signature_help(self):
        self.open('SinOsc.ar(440, ')
        r = self.c.request('textDocument/signatureHelp', self.at(0, 15))
        self.assertEqual(r['signatures'][0]['label'], 'SinOsc.*ar(freq = 440.0, phase = 0.0, mul = 1.0, add = 0.0)')
        self.assertEqual(r['activeParameter'], 1)
        self.change('SinOsc.ar(mul: ')
        r = self.c.request('textDocument/signatureHelp', self.at(0, 15))
        self.assertEqual(r['activeParameter'], 2)

    def test_workspace_symbol(self):
        names = [s['name'] for s in self.c.request('workspace/symbol', {'query': 'sinosc'})]
        self.assertIn('SinOsc', names)

    def test_class_file(self):
        self.open(CLASS_TEXT, SC_URI)
        self.c.library_built()
        self.assertEqual(self.c.diagnostics(SC_URI, self.version), [])
        self.assertIn('var iv', self.hover(4, 16, SC_URI))
        self.assertIn('classvar cv = 3', self.hover(4, 33, SC_URI))
        self.assertIn('LspTestClass:bar(x)', self.hover(5, 5, SC_URI))
        symbols = self.c.request('textDocument/documentSymbol', {'textDocument': {'uri': SC_URI}})
        self.assertEqual(symbols[0]['name'], 'LspTestClass')
        self.assertIn('bar', [s['name'] for s in symbols[0]['children']])
        # The class can be used from interpreted code once the library has it, its accessors too.
        self.open('LspTestClass.new.iv_(3).bar(1)')
        self.assertIn('LspTestClass:bar(x)', self.hover(0, 26))
        # A mistake is reported after the library is rebuilt.
        type(self).version += 1
        self.c.notify('textDocument/didChange', {'textDocument': {'uri': SC_URI, 'version': self.version},
                                                 'contentChanges': [{'text': CLASS_TEXT.replace('other }', 'nope }')}]})
        self.c.library_built()
        d = self.c.diagnostics(SC_URI, self.version)
        self.assertEqual([x['code'] for x in d], ['Variable not defined'])
        self.c.notify('textDocument/didClose', {'textDocument': {'uri': SC_URI}})


class McpTest(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.p = subprocess.Popen([ARGS.mcp, '--library', ARGS.library, '--no-hooks'], stdin=subprocess.PIPE,
                                 stdout=subprocess.PIPE, stderr=subprocess.DEVNULL, text=True)
        cls.n = 0
        cls.init = cls.request('initialize', {'protocolVersion': '2025-06-18', 'capabilities': {}})

    @classmethod
    def tearDownClass(cls):
        cls.p.stdin.close()
        assert cls.p.wait(timeout=10) == 0

    @classmethod
    def request(cls, method, params=None):
        cls.n += 1
        cls.p.stdin.write(json.dumps({'jsonrpc': '2.0', 'id': cls.n, 'method': method, 'params': params or {}}) + '\n')
        cls.p.stdin.flush()
        return json.loads(cls.p.stdout.readline())['result']

    def call(self, tool, **arguments):
        r = self.request('tools/call', {'name': tool, 'arguments': arguments})
        return r['isError'], r['content'][0]['text']

    def test_tools_listed(self):
        tools = [t['name'] for t in self.request('tools/list')['tools']]
        for t in ['check_code', 'check_file', 'class_info', 'implementors', 'find_definition', 'subclasses', 'search',
                  'references', 'hooks']:
            self.assertIn(t, tools)
        # Not without --allow-eval.
        self.assertNotIn('evaluate', tools)

    def test_check_code(self):
        self.assertEqual(self.call('check_code', code='1 + 2'), (False, 'No problems found.\n'))
        error, text = self.call('check_code', code='(\nvar x = 1;\nx +\n)')
        self.assertIn('snippet.scd:3:3: error:', text)
        error, text = self.call('check_code', code='Foo { bar { ^undefinedName } }', kind='class')
        self.assertIn('snippet.sc:1:14: error: `undefinedName` is not defined.', text)

    def test_lookups(self):
        _, text = self.call('class_info', name='Pbind')
        self.assertIn('class Pbind : Pattern', text)
        self.assertIn('Patterns.sc:', text)
        _, text = self.call('find_definition', name='SinOsc.ar')
        self.assertTrue(text.startswith('SinOsc.*ar(freq = 440.0'))
        _, text = self.call('find_definition', name='Array:collect')
        self.assertIn('inherited from', text)
        _, text = self.call('implementors', selector='squared')
        self.assertIn('implementations of squared', text)
        _, text = self.call('subclasses', name='Pattern')
        self.assertIn('  Pbind', text)
        _, text = self.call('references', name='SinOsc')
        self.assertIn('uses of SinOsc', text)
        _, text = self.call('hooks')
        self.assertIn('SynthDefNameHook', text)


def main():
    global ARGS
    here = os.path.dirname(os.path.abspath(__file__))
    parser = argparse.ArgumentParser()
    parser.add_argument('--lsp', required=True)
    parser.add_argument('--mcp', required=True)
    parser.add_argument('--library', default=os.path.normpath(os.path.join(here, '..', '..', '..', 'SCClassLibrary')))
    ARGS, rest = parser.parse_known_args()
    unittest.main(argv=[sys.argv[0]] + rest, verbosity=2)


if __name__ == '__main__':
    main()
