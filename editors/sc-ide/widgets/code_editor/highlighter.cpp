/*
    SuperCollider Qt IDE
    Copyright (c) 2012 Jakob Leben & Tim Blechmann
    http://www.audiosynth.com

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301  USA
*/

#include <cassert>
#include <algorithm>

#include "highlighter.hpp"
#include "../../core/main.hpp"
#include "../../core/settings/manager.hpp"
#include "../../core/settings/theme.hpp"

#include <QApplication>
#include <QTextBlock>
#include <QTextDocument>

namespace ScIDE {

SyntaxHighlighterGlobals* SyntaxHighlighterGlobals::mInstance = 0;

SyntaxHighlighterGlobals::SyntaxHighlighterGlobals(Main* main, Settings::Manager* settings): QObject(main) {
    Q_ASSERT(mInstance == 0);
    mInstance = this;

    ScLexer::initLexicalRules();

    // initialize formats from settings:
    applySettings(settings);

    connect(main, &Main::applySettingsRequest, this, &SyntaxHighlighterGlobals::applySettings);
}

void SyntaxHighlighterGlobals::applySettings(Settings::Manager* s) {
    applySettingsKey(s, "whitespace", WhitespaceFormat);
    applySettingsKey(s, "keyword", KeywordFormat);
    applySettingsKey(s, "built-in", BuiltinFormat);
    applySettingsKey(s, "primitive", PrimitiveFormat);
    applySettingsKey(s, "class", ClassFormat);
    applySettingsKey(s, "number", NumberFormat);
    applySettingsKey(s, "symbol", SymbolFormat);
    applySettingsKey(s, "env-var", EnvVarFormat);
    applySettingsKey(s, "string", StringFormat);
    applySettingsKey(s, "char", CharFormat);
    applySettingsKey(s, "comment", CommentFormat);

    Q_EMIT(syntaxFormatsChanged());
}

void SyntaxHighlighterGlobals::applySettingsKey(Settings::Manager* s, const QString& key, SyntaxFormat type) {
    mFormats[type] = s->getThemeVal(key);
}

SyntaxHighlighter::SyntaxHighlighter(QTextDocument* parent): QSyntaxHighlighter(static_cast<QObject*>(parent)) {
    mGlobals = SyntaxHighlighterGlobals::instance();

    // Before the document is set, so the tokens are moved before QSyntaxHighlighter highlights the edit.
    connect(parent, &QTextDocument::contentsChange, this, &SyntaxHighlighter::onContentsChange);
    setDocument(parent);

    connect(mGlobals, &SyntaxHighlighterGlobals::syntaxFormatsChanged, this, &SyntaxHighlighter::rehighlight);

    connect(Main::scProcess(), &ScProcess::introspectionChanged, this, &SyntaxHighlighter::rehighlight);
}

////////////////////////////////////////////////////////////////////////////////
// The language server's tokens
////////////////////////////////////////////////////////////////////////////////

void SyntaxHighlighter::setSemanticTokens(const SemanticTokens& tokens) {
    QTextDocument* doc = document();
    if (!doc)
        return;
    if (!mHasSemanticTokens) {
        mSemanticTokens = tokens;
        mHasSemanticTokens = true;
        for (QTextBlock block = doc->begin(); block.isValid(); block = block.next())
            if (auto* data = static_cast<TextBlockData*>(block.userData()))
                data->semanticStale = false;
        rehighlight();
        return;
    }

    // Only the lines whose tokens changed, or that were edited, are highlighted again.
    QVector<int> changed;
    const auto change = [&](const SemanticToken& t) { changed.append(doc->findBlock(t.start).blockNumber()); };
    int i = 0, j = 0;
    while (i < mSemanticTokens.size() || j < tokens.size()) {
        if (i < mSemanticTokens.size() && j < tokens.size() && mSemanticTokens[i] == tokens[j]) {
            ++i;
            ++j;
        } else if (j == tokens.size() || (i < mSemanticTokens.size() && mSemanticTokens[i].start <= tokens[j].start)) {
            change(mSemanticTokens[i++]);
        } else {
            change(tokens[j++]);
        }
    }
    mSemanticTokens = tokens;
    for (QTextBlock block = doc->begin(); block.isValid(); block = block.next()) {
        auto* data = static_cast<TextBlockData*>(block.userData());
        if (!data || data->semanticStale)
            changed.append(block.blockNumber());
        if (data)
            data->semanticStale = false;
    }
    std::sort(changed.begin(), changed.end());
    changed.erase(std::unique(changed.begin(), changed.end()), changed.end());
    for (int number : changed)
        rehighlightBlock(doc->findBlockByNumber(number));
}

void SyntaxHighlighter::clearSemanticTokens() {
    if (!mHasSemanticTokens)
        return;
    mHasSemanticTokens = false;
    mSemanticTokens.clear();
    rehighlight();
}

void SyntaxHighlighter::onContentsChange(int position, int removed, int added) {
    if (!mHasSemanticTokens)
        return;
    // Tokens after the edit move with it, those touching it are dropped.
    const int delta = added - removed;
    SemanticTokens kept;
    kept.reserve(mSemanticTokens.size());
    for (SemanticToken t : mSemanticTokens) {
        if (t.start + t.length <= position) {
            kept.append(t);
        } else if (t.start >= position + removed) {
            t.start += delta;
            kept.append(t);
        }
    }
    mSemanticTokens = std::move(kept);

    QTextDocument* doc = document();
    const QTextBlock last = doc->findBlock(position + added);
    for (QTextBlock block = doc->findBlock(position); block.isValid(); block = block.next()) {
        if (auto* data = static_cast<TextBlockData*>(block.userData()))
            data->semanticStale = true;
        if (block == last)
            break;
    }
}

const QTextCharFormat* SyntaxHighlighter::semanticFormat(const SemanticToken& token) const {
    const QTextCharFormat* formats = mGlobals->formats();
    switch (token.kind) {
    case SemanticToken::Comment:
        return &formats[CommentFormat];
    case SemanticToken::String:
        return &formats[StringFormat];
    case SemanticToken::Character:
        return &formats[CharFormat];
    case SemanticToken::Symbol:
    case SemanticToken::KeywordArgument:
        return &formats[SymbolFormat];
    case SemanticToken::Number:
        return &formats[NumberFormat];
    case SemanticToken::Keyword:
        return &formats[KeywordFormat];
    case SemanticToken::Builtin:
        return &formats[BuiltinFormat];
    case SemanticToken::Primitive:
        return &formats[PrimitiveFormat];
    case SemanticToken::Class:
        return token.modifiers & SemanticToken::Unknown ? nullptr : &formats[ClassFormat];
    case SemanticToken::Environment:
        return &formats[EnvVarFormat];
    default:
        return nullptr;
    }
}

void SyntaxHighlighter::applySemanticTokens() {
    const QTextBlock block = currentBlock();
    const int begin = block.position();
    const int end = begin + block.length();
    auto it = std::lower_bound(mSemanticTokens.cbegin(), mSemanticTokens.cend(), begin,
                               [](const SemanticToken& t, int position) { return t.start < position; });
    for (; it != mSemanticTokens.cend() && it->start < end; ++it)
        if (const QTextCharFormat* format = semanticFormat(*it))
            setFormat(it->start - begin, qMin(it->length, end - it->start), *format);
}

void SyntaxHighlighter::setLexerFormat(int start, int count, SyntaxFormat format) {
    if (mSemanticBlock && format != WhitespaceFormat)
        return;
    setFormat(start, count, mGlobals->format(format));
}

////////////////////////////////////////////////////////////////////////////////
// The lexer
////////////////////////////////////////////////////////////////////////////////

void SyntaxHighlighter::highlightBlockInCode(ScLexer& lexer) {
    TextBlockData* blockData = static_cast<TextBlockData*>(currentBlockUserData());
    Q_ASSERT(blockData);

    do {
        int tokenPosition = lexer.offset();
        int tokenLength;
        Token::Type tokenType = lexer.nextToken(tokenLength);

        switch (tokenType) {
        case Token::WhiteSpace:
            setLexerFormat(tokenPosition, tokenLength, WhitespaceFormat);
            break;

        case Token::Class: {
            auto className = QString(lexer.text().begin() + tokenPosition, tokenLength);

            auto* classInstance = Main::scProcess()->introspection().findClass(className);

            if (classInstance != nullptr)
                setLexerFormat(tokenPosition, tokenLength, ClassFormat);

            break;
        }
        case Token::Builtin:
            setLexerFormat(tokenPosition, tokenLength, BuiltinFormat);
            break;

        case Token::Primitive:
            setLexerFormat(tokenPosition, tokenLength, PrimitiveFormat);
            break;

        case Token::Keyword:
            setLexerFormat(tokenPosition, tokenLength, KeywordFormat);
            break;

        case Token::Symbol:
            setLexerFormat(tokenPosition, tokenLength, SymbolFormat);
            break;

        case Token::SymbolArg:
            // Don't highlight the trailing ':'
            setLexerFormat(tokenPosition, tokenLength - 1, SymbolFormat);
            break;

        case Token::EnvVar:
            setLexerFormat(tokenPosition, tokenLength, EnvVarFormat);
            break;

        case Token::Char:
            setLexerFormat(tokenPosition, tokenLength, CharFormat);
            break;

        case Token::Float:
        case Token::HexInt:
        case Token::ScaleDegreeFloat:
        case Token::RadixFloat:
            setLexerFormat(tokenPosition, tokenLength, NumberFormat);
            break;

        case Token::SingleLineComment:
            setLexerFormat(tokenPosition, tokenLength, CommentFormat);
            break;

        case Token::MultiLineCommentStart:
            setLexerFormat(tokenPosition, tokenLength, CommentFormat);
            break;

        case Token::StringMark:
            setLexerFormat(tokenPosition, tokenLength, StringFormat);
            break;

        case Token::SymbolMark:
            setLexerFormat(tokenPosition, tokenLength, SymbolFormat);
            break;

        default:;
        }

        if ((tokenType != Token::WhiteSpace) && (tokenType != Token::SingleLineComment)
            && (tokenType != Token::MultiLineCommentStart)) {
            Token token(tokenType, tokenPosition, tokenLength);
            if (token.length == 1)
                token.character = lexer.text()[tokenPosition].toLatin1();
            blockData->tokens.push_back(token);
        }

    } while (lexer.state() == ScLexer::InCode && lexer.offset() < lexer.text().size());
}

void SyntaxHighlighter::highlightBlockInString(ScLexer& lexer) {
    int originalOffset = lexer.offset();
    int tokenLength;
    Token::Type tokenType = lexer.nextToken(tokenLength);
    int range = lexer.offset() - originalOffset;
    setLexerFormat(originalOffset, range, StringFormat);

    if (tokenType == Token::Unknown)
        return;

    Q_ASSERT(tokenType == Token::StringMark);
    Token token(tokenType, lexer.offset() - 1, 1);
    token.character = '"';

    TextBlockData* blockData = static_cast<TextBlockData*>(currentBlockUserData());
    Q_ASSERT(blockData);
    blockData->tokens.push_back(token);
}

void SyntaxHighlighter::highlightBlockInSymbol(ScLexer& lexer) {
    int originalOffset = lexer.offset();
    int tokenLength;
    Token::Type tokenType = lexer.nextToken(tokenLength);
    int range = lexer.offset() - originalOffset;
    setLexerFormat(originalOffset, range, SymbolFormat);

    if (tokenType == Token::Unknown)
        return;

    Q_ASSERT(tokenType == Token::SymbolMark);
    Token token(tokenType, lexer.offset() - 1, 1);
    token.character = '\'';

    TextBlockData* blockData = static_cast<TextBlockData*>(currentBlockUserData());
    Q_ASSERT(blockData);
    blockData->tokens.push_back(token);
}

void SyntaxHighlighter::highlightBlockInComment(ScLexer& lexer) {
    int originalOffset = lexer.offset();
    int tokenLength;
    lexer.nextToken(tokenLength);
    int range = lexer.offset() - originalOffset;
    setLexerFormat(originalOffset, range, CommentFormat);
}

void SyntaxHighlighter::highlightBlock(const QString& text) {
    // Without the language server's tokens, nothing is formatted until there is introspection.
    if (!mHasSemanticTokens && !Main::scProcess()->introspection().introspectionAvailable())
        return;

    TextBlockData* blockData = static_cast<TextBlockData*>(currentBlockUserData());
    if (!blockData) {
        blockData = new TextBlockData;
        blockData->tokens.reserve(8);
        setCurrentBlockUserData(blockData);
    } else {
        blockData->tokens.clear();
    }
    mSemanticBlock = mHasSemanticTokens && !blockData->semanticStale;

    int previousState = previousBlockState();
    if (previousState == -1)
        previousState = ScLexer::InCode;

    ScLexer lexer(text, 0, previousState);

    while (lexer.offset() < text.size()) {
        switch (lexer.state()) {
        case ScLexer::InCode:
            highlightBlockInCode(lexer);
            break;

        case ScLexer::InString:
            highlightBlockInString(lexer);
            break;

        case ScLexer::InSymbol:
            highlightBlockInSymbol(lexer);
            break;

        default:
            if (lexer.state() >= ScLexer::InComment) {
                highlightBlockInComment(lexer);
                blockData->isInMultilineComment = true;
            }
        }
    }

    if (mHasSemanticTokens)
        applySemanticTokens();

    setCurrentBlockState(lexer.state());
}

}
