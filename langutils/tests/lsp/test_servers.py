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
        # A binary operator replaces the `.`, as `1.+ 2` doesn't parse.
        plus = [i for i in self.items('x = 1.') if i['label'] == '+']
        self.assertEqual([(i['textEdit']['newText'], i['textEdit']['range']['start']['character'], i['filterText'])
                          for i in plus], [(' + ', 5, '.+')])
        plus = [i for i in self.items('x = 1 .') if i['label'] == '+']
        self.assertEqual([i['textEdit']['newText'] for i in plus], ['+ '])
        self.assertNotIn('filterText', [i for i in self.items('x = 1.') if i['label'] == 'abs'][0])
        # Methods sort by how far up the hierarchy they are defined, so Object's come last.
        def sort(text):
            return {i['label']: i['sortText'] for i in self.items(text)}
        s = sort('x = 1.')
        self.assertLess(s['isPrime'], s['squared'])
        self.assertLess(s['squared'], s['postln'])
        s = sort('SinOsc.')
        self.assertLess(s['ar'], s['new'])
        self.assertLess(s['new'], s['superclass'])
        s = sort('x = 1 ma')
        self.assertLess(s['max:'], s['matchItem:'])
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

    def test_regions(self):
        def span(start, end):
            return {'start': {'line': start[0], 'character': start[1]}, 'end': {'line': end[0], 'character': end[1]}}

        def published(version):
            return self.c.wait_for(lambda m: m.get('method') == 'sclang/publishRegions'
                                   and m['params']['uri'] == SCD_URI
                                   and m['params']['version'] >= version)['params']

        self.assertTrue(self.init['capabilities']['experimental']['regions'])
        # Each statement, with its brackets. The emoji is two UTF-16 units.
        self.open('(\nvar a = 1;\na\n);\nx = "😀"; ( "hi".postln );\n(1 + 2).postln;\n( var b = ; b )\n')
        regions = [span((0, 0), (3, 1)), span((4, 0), (4, 8)), span((4, 10), (4, 25)), span((5, 0), (5, 14)),
                   span((6, 0), (6, 15))]
        p = published(self.version)
        self.assertEqual((p['version'], p['regions']), (self.version, regions))
        # A request answers for the latest text, before the next publish.
        type(self).version += 1
        self.c.notify('textDocument/didChange', {'textDocument': {'uri': SCD_URI, 'version': self.version},
                                                 'contentChanges': [{'text': '1;\n(\n2\n)'}]})
        r = self.c.request('sclang/regions', {'textDocument': {'uri': SCD_URI}})
        self.assertEqual(r, {'version': self.version, 'regions': [span((0, 0), (0, 1)), span((1, 0), (3, 1))]})
        self.assertEqual(published(self.version)['regions'], r['regions'])
        self.assertEqual(self.c.request('sclang/regions', {'textDocument': {'uri': 'file:///not/open.scd'}}), None)

    def semantic_tokens(self, text, uri=SCD_URI):
        """The tokens of the document as (line, character, text, type, modifiers)."""
        legend = self.init['capabilities']['semanticTokensProvider']['legend']
        data = self.c.request('textDocument/semanticTokens/full', {'textDocument': {'uri': uri}})['data']
        lines = text.split('\n')
        out, line, character = [], 0, 0
        for i in range(0, len(data), 5):
            dl, dc, length, kind, bits = data[i:i + 5]
            character = dc if dl else character + dc
            line += dl
            mods = [m for b, m in enumerate(legend['tokenModifiers']) if bits & (1 << b)]
            out.append((line, character, lines[line][character:character + length], legend['tokenTypes'][kind], mods))
        return out

    def test_semantic_tokens(self):
        text = ('(\nvar freq = 440, f = { |amp| amp * freq * this.x }, g = { |gain = 1| gain };\n'
                '~env = SinOsc.ar(freq: 1, phase: 0.5pi) + \\sym + $c + "s" + nil + NoSuchClass; // done\n)\n')
        self.open(text)
        tokens = self.semantic_tokens(text)
        expected = [
            (1, 0, 'var', 'keyword', []), (1, 4, 'freq', 'variable', ['declaration']), (1, 11, '440', 'number', []),
            (1, 16, 'f', 'variable', ['declaration']), (1, 23, 'amp', 'parameter', ['declaration']),
            (1, 28, 'amp', 'parameter', []), (1, 32, '*', 'method', []), (1, 34, 'freq', 'variable', []),
            (1, 39, '*', 'method', []), (1, 41, 'this', 'keyword', []), (1, 46, 'x', 'method', []),
            # An argument's declaration, without its default value.
            (1, 51, 'g', 'variable', ['declaration']), (1, 58, 'gain', 'parameter', ['declaration']),
            (1, 65, '1', 'number', []), (1, 68, 'gain', 'parameter', []),
            (2, 0, '~env', 'environmentVariable', []), (2, 7, 'SinOsc', 'class', []), (2, 14, 'ar', 'method', []),
            (2, 17, 'freq', 'keywordArgument', []), (2, 23, '1', 'number', []),
            (2, 26, 'phase', 'keywordArgument', []), (2, 33, '0.5', 'number', []), (2, 36, 'pi', 'number', []),
            (2, 40, '+', 'method', []),
        ]
        self.assertEqual(tokens[:len(expected)], expected)
        rest = [(t[2], t[3], t[4]) for t in tokens[len(expected):] if t[3] != 'method']
        self.assertEqual(rest, [('\\sym', 'symbol', []), ('$c', 'character', []), ('"s"', 'string', []),
                                ('nil', 'builtin', []), ('NoSuchClass', 'class', ['unknown']),
                                ('// done', 'comment', [])])
        self.assertTrue(self.init['capabilities']['semanticTokensProvider']['full'])

        # While the text doesn't compile, names away from the change are kept, but not the one being typed.
        broken = text.replace('this.x', 'this.x(').replace('amp * freq', 'amp * freqq')
        self.change(broken)
        tokens = {(t[0], t[1]): (t[2], t[3]) for t in self.semantic_tokens(broken)}
        self.assertEqual(tokens[(1, 4)], ('freq', 'variable'))
        self.assertEqual(tokens[(1, 28)], ('amp', 'parameter'))
        self.assertNotIn((1, 34), tokens)
        self.assertEqual(tokens[(2, 7)], ('SinOsc', 'class'))

        # Multiline comments are split into lines, and columns are UTF-16.
        text = '/* 😀\nb */ "😀" 1'
        self.change(text)
        tokens = self.semantic_tokens(text)
        self.assertEqual([(t[0], t[1], t[3]) for t in tokens], [(0, 0, 'comment'), (1, 0, 'comment'),
                                                                 (1, 5, 'string'), (1, 10, 'number')])
        self.assertEqual(tokens[0][2], '/* 😀')

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
