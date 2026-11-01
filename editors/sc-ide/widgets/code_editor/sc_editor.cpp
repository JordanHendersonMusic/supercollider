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

#include "sc_editor.hpp"
#include "autocompleter.hpp"
#include "line_indicator.hpp"
#include "main_window.hpp"
#include "../util/gui_utilities.hpp"
#include "../../core/main.hpp"
#include "../../core/doc_manager.hpp"
#include "../../core/lsp_client.hpp"
#include "../../core/settings/manager.hpp"
#include "../../core/settings/theme.hpp"
#include <qtextcursor.h>

#ifdef SC_USE_QTWEBENGINE
#    include "help_browser.hpp"
#endif // SC_USE_QTWEBENGINE

#include "QtCollider/hacks/hacks_qt.hpp"

#include <QKeyEvent>
#include <QCursor>
#include <QFileInfo>
#include <QHelpEvent>
#include <QJsonArray>
#include <QJsonObject>
#include <QMenu>
#include <QPainter>
#include <QPointer>
#include <QRegularExpression>
#include <QToolTip>
#include <QStack>
#include <QMimeData>
#include <QUrl>

namespace ScIDE {

namespace {
// The width left of the line numbers for the region bars.
constexpr int regionBarMargin = 5;
}

ScCodeEditor::ScCodeEditor(Document* doc, QWidget* parent):
    GenericCodeEditor(doc, parent),
    mSpaceIndent(true),
    mBlinkDuration(600),
    mMouseBracketMatch(false),
    mAutoCompleter(new AutoCompleter(this)) {
    Q_ASSERT(mDoc != 0);

    connect(this, &ScCodeEditor::cursorPositionChanged, this, &ScCodeEditor::matchBrackets);

    connect(Main::instance(), &Main::applySettingsRequest, this, &ScCodeEditor::applySettings);

    mAutoCompleter->documentChanged(textDocument());

    connect(Main::lspClient(), &LspClient::diagnosticsChanged, this, [this](Document* d) {
        if (d == mDoc)
            updateDiagnostics();
    });
    mLineIndicator->setLeftMargin(regionBarMargin);
    connect(Main::lspClient(), &LspClient::regionsChanged, this, [this](Document* d) {
        if (d == mDoc)
            mLineIndicator->update();
    });
    connect(this, &ScCodeEditor::cursorPositionChanged, mLineIndicator, [this] { mLineIndicator->update(); });

    applySettings(Main::settings());
    updateDiagnostics();
}

void ScCodeEditor::applySettings(Settings::Manager* settings) {
    settings->beginGroup("IDE/editor");

    mSpaceIndent = settings->value("spaceIndent").toBool();
    mBlinkDuration = settings->value("blinkDuration").toInt();
    mBracketHighlight = settings->getThemeVal("matchingBrackets");
    mBracketMismatchFormat = settings->getThemeVal("mismatchedBrackets");
    mStepForwardEvaluation = settings->value("stepForwardEvaluation").toBool();
    mInsertMatchingTokens = settings->value("insertMatchingTokens").toBool();
    mHighlightBracketContents = settings->value("highlightBracketContents").toBool();

    settings->endGroup();

    // update bracket match highlighting:
    matchBrackets();
}

bool ScCodeEditor::event(QEvent* e) {
    switch (e->type()) {
    case QEvent::KeyPress: {
        QKeyEvent* ke = static_cast<QKeyEvent*>(e);
        switch (ke->key()) {
        case Qt::Key_Tab:
            if (!tabChangesFocus()) {
                indent();
                e->accept();
                return true;
            }
        default:
            break;
        }
        break;
    }
    case QEvent::ToolTip: {
        auto* help = static_cast<QHelpEvent*>(e);
        showToolTip(viewport()->mapFromGlobal(help->globalPos()), help->globalPos());
        return true;
    }
    default:
        break;
    }

    return GenericCodeEditor::event(e);
}

////////////////////////////////////////////////////////////////////////////////
// The language server
////////////////////////////////////////////////////////////////////////////////

void ScCodeEditor::updateDiagnostics() {
    mDiagnosticSelections.clear();
    for (const auto& d : Main::lspClient()->diagnostics(mDoc)) {
        QTextEdit::ExtraSelection selection;
        selection.cursor = QTextCursor(textDocument());
        int start = d.start;
        int end = d.end;
        // An empty range, e.g., at the end of the file, still needs a character to underline.
        if (end <= start) {
            end = start + 1;
            if (end >= textDocument()->characterCount())
                start = qMax(0, start - 1);
        }
        selection.cursor.setPosition(start);
        selection.cursor.setPosition(qMin(end, textDocument()->characterCount() - 1), QTextCursor::KeepAnchor);
        selection.format.setUnderlineStyle(d.severity <= 2 ? QTextCharFormat::WaveUnderline : QTextCharFormat::DotLine);
        selection.format.setUnderlineColor(d.severity == 1       ? QColor(220, 40, 40)
                                               : d.severity == 2 ? QColor(220, 150, 0)
                                                                 : QColor(70, 130, 220));
        mDiagnosticSelections.append(selection);
    }
    updateExtraSelections();
}

namespace {
// The hover's markdown as rich text: code blocks in <pre>, the rest as paragraphs.
QString hoverHtml(const QString& markdown) {
    QString html;
    bool code = false;
    for (const QString& line : markdown.split('\n')) {
        if (line.startsWith("```")) {
            html += code ? "</pre>" : "<pre>";
            code = !code;
            continue;
        }
        QString escaped = line.toHtmlEscaped();
        if (!code)
            escaped.replace(QRegularExpression("`([^`]*)`"), "<code>\\1</code>");
        html += code ? escaped + "\n" : (escaped.isEmpty() ? QString() : "<p>" + escaped + "</p>");
    }
    return html;
}
}

void ScCodeEditor::showToolTip(const QPoint& viewportPos, const QPoint& globalPos) {
    const QTextCursor cursor = cursorForPosition(viewportPos);
    const int pos = cursor.position();
    // Only over text, not past the end of a line.
    const QRect rect = cursorRect(cursor);
    if (qAbs(rect.center().x() - viewportPos.x()) > fontMetrics().averageCharWidth() * 2
        && cursor.positionInBlock() == cursor.block().length() - 1) {
        QToolTip::hideText();
        return;
    }
    QStringList messages;
    for (const auto& d : Main::lspClient()->diagnostics(mDoc))
        if (pos >= d.start && pos <= qMax(d.end, d.start + 1))
            messages << d.message.toHtmlEscaped().replace('\n', "<br>");
    if (!messages.isEmpty()) {
        QToolTip::showText(globalPos, "<p>" + messages.join("</p><p>") + "</p>", this);
        return;
    }
    if (!Main::lspClient()->isReady()) {
        QToolTip::hideText();
        return;
    }
    const int request = ++mHoverRequest;
    QPointer<ScCodeEditor> self(this);
    Main::lspClient()->requestAt("textDocument/hover", mDoc, pos, [self, request, globalPos](const QJsonValue& r) {
        if (!self || request != self->mHoverRequest)
            return;
        const QString markdown = r.toObject()["contents"].toObject()["value"].toString();
        // Only if the mouse is still there.
        if (markdown.isEmpty() || (QCursor::pos() - globalPos).manhattanLength() > 8) {
            QToolTip::hideText();
            return;
        }
        QToolTip::showText(globalPos, hoverHtml(markdown), self);
    });
}

bool ScCodeEditor::openLocations(const QString& method, bool definition) {
    LspClient* lsp = Main::lspClient();
    if (!lsp->isReady())
        return false;
    QPointer<ScCodeEditor> self(this);
    const QString symbol = symbolUnderCursor();
    QJsonObject extra;
    if (!definition)
        extra["context"] = QJsonObject { { "includeDeclaration", false } };
    lsp->requestAt(
        method, mDoc, textCursor().position(),
        [self, symbol, definition](const QJsonValue& result) {
            if (!self)
                return;
            const QJsonArray found = result.toArray();
            LspClient* lsp = Main::lspClient();
            if (found.isEmpty()) {
                // Fall back on sclang's introspection, which knows the running library.
                if (definition)
                    Main::openDefinition(symbol, self);
                else
                    Main::findReferences(symbol, self);
                return;
            }
            if (found.size() == 1) {
                LspClient::open(lsp->location(found.first()));
                return;
            }
            QMenu menu(self);
            constexpr int shown = 40;
            for (int i = 0; i < found.size() && i < shown; ++i) {
                const auto l = lsp->location(found[i]);
                const QString name = l.document ? l.document->title() : QFileInfo(l.path).fileName();
                QAction* action = menu.addAction(QStringLiteral("%1:%2").arg(name).arg(l.line + 1));
                action->setToolTip(l.path);
                QObject::connect(action, &QAction::triggered, [l] { LspClient::open(l); });
            }
            if (found.size() > shown)
                menu.addAction(QObject::tr("%1 more not shown").arg(found.size() - shown))->setEnabled(false);
            menu.exec(self->viewport()->mapToGlobal(self->cursorRect().bottomLeft()));
        },
        extra);
    return true;
}

void ScCodeEditor::keyPressEvent(QKeyEvent* e) {
    hideMouseCursor(e);
    bool actionInSuper = false;

    QTextCursor cursor(textCursor());
    bool cursorMoved = true;

    if (e == QKeySequence::MoveToNextWord)
        moveToNextToken(cursor, QTextCursor::MoveAnchor);
    else if (e == QKeySequence::MoveToPreviousWord)
        moveToPreviousToken(cursor, QTextCursor::MoveAnchor);
    else if (e == QKeySequence::SelectNextWord)
        moveToNextToken(cursor, QTextCursor::KeepAnchor);
    else if (e == QKeySequence::SelectPreviousWord)
        moveToPreviousToken(cursor, QTextCursor::KeepAnchor);
    else
        cursorMoved = false;

    if (cursorMoved) {
        setTextCursor(cursor);
        doKeyAction(e);
        return;
    }

    switch (e->key()) {
    case Qt::Key_Home: {
        Qt::KeyboardModifiers mods(e->modifiers());
        if (mods && mods != Qt::ShiftModifier) {
            GenericCodeEditor::keyPressEvent(e);
            return;
        }

        QTextCursor::MoveMode mode = mods & Qt::ShiftModifier ? QTextCursor::KeepAnchor : QTextCursor::MoveAnchor;

        QTextCursor c(textCursor());
        QTextBlock b(c.block());

        int pos = indentedStartOfLine(b);
        pos += b.position();

        if (c.position() == pos)
            c.movePosition(QTextCursor::StartOfLine, mode);
        else
            c.setPosition(pos, mode);

        setTextCursor(c);
        doKeyAction(e);
        return;
    }

    case Qt::Key_Backtab: {
        QTextCursor cursor = textCursor();
        insertSpaceToNextTabStop(cursor);
        ensureCursorVisible();
        doKeyAction(e);
        return;
    }
    case Qt::Key_Backspace:
        if (mInsertMatchingTokens && !overwriteMode() && e->modifiers() == 0)
            if (removeMatchingTokens())
                break;
        GenericCodeEditor::keyPressEvent(e);
        actionInSuper = true;
        break;
    case Qt::Key_Enter:
    case Qt::Key_Return: {
        QTextBlock cursorBlock = cursor.block();
        int cursorPosInBlock = cursor.position() - cursorBlock.position();

        TokenIterator prevToken = TokenIterator::leftOf(cursorBlock, cursorPosInBlock);
        TokenIterator nextToken = TokenIterator::rightOf(cursorBlock, cursorPosInBlock);

        if (nextToken.block() == cursorBlock && nextToken.type() == Token::ClosingBracket
            && prevToken.type()
                != Token::ClosingBracket // no double-newline if cursor is between closing brackets, i.e. ])
            && !(prevToken.block().firstLineNumber()
                 < nextToken.block().firstLineNumber()) // no double-nl if only whitespace to the left
        ) {
            cursor.beginEditBlock();
            cursor.insertBlock();
            cursor.insertBlock();
            cursor.endEditBlock();
            cursor.movePosition(QTextCursor::PreviousBlock, QTextCursor::KeepAnchor);
            indent(cursor, JoinEditBlock);
            cursor.movePosition(QTextCursor::EndOfBlock);
        } else {
            cursor.beginEditBlock();
            cursor.insertBlock();
            cursor.endEditBlock();
            indent(cursor, JoinEditBlock);
        }
        cursor.setVerticalMovementX(-1);
        setTextCursor(cursor);
        break;
    }
    default:
        if (!overwriteMode() && insertMatchingTokens(e->text()))
            break;
        GenericCodeEditor::keyPressEvent(e);
        actionInSuper = true;
    }

    mAutoCompleter->keyPress(e);
    if (!actionInSuper)
        doKeyAction(e);
}

void ScCodeEditor::mouseReleaseEvent(QMouseEvent* e) {
    // Prevent deselection of bracket match:
    if (!mMouseBracketMatch)
        GenericCodeEditor::mouseReleaseEvent(e);

    mMouseBracketMatch = false;
}

void ScCodeEditor::mouseDoubleClickEvent(QMouseEvent* e) {
    // Always pass to superclass so as to handle line selection on triple click
    GenericCodeEditor::mouseDoubleClickEvent(e);

    if (e->button() == Qt::LeftButton) {
        QTextCursor cursor = cursorForPosition(e->pos());
        QTextCursor selection = selectionForPosition(cursor.position());

        if (!selection.isNull()) {
            mMouseBracketMatch = true;
            setTextCursor(selection);
            return;
        }
    }
}

void ScCodeEditor::mouseMoveEvent(QMouseEvent* e) {
    // Prevent initiating a text drag:
    if (!mMouseBracketMatch)
        GenericCodeEditor::mouseMoveEvent(e);
}

void ScCodeEditor::dragEnterEvent(QDragEnterEvent* event) {
    // The purpose is only to bypass GenericCodeEditor::dragEnterEvent:
    QPlainTextEdit::dragEnterEvent(event);
}

bool ScCodeEditor::canInsertFromMimeData(const QMimeData* data) const {
    if (data->hasUrls())
        return true;

    return QPlainTextEdit::canInsertFromMimeData(data);
}

void ScCodeEditor::insertFromMimeData(const QMimeData* data) {
    if (data->hasUrls()) {
        QTextCursor cursor = textCursor();
        QList<QUrl> urls = data->urls();
        bool multiple = urls.size() > 1;
        if (multiple) {
            cursor.insertText("[");
            cursor.insertBlock();
        }
        for (int i = 0; i < urls.size(); ++i) {
            QUrl url = urls[i];
            cursor.insertText("\"");
            if (QURL_IS_LOCAL_FILE(url))
                cursor.insertText(url.toLocalFile());
            else
                cursor.insertText(url.toString());
            cursor.insertText("\"");
            if (i < urls.size() - 1) {
                cursor.insertText(",");
                cursor.insertBlock();
            }
        }
        if (multiple) {
            cursor.insertBlock();
            cursor.insertText("]");
        }
    } else
        QPlainTextEdit::insertFromMimeData(data);
}

void ScCodeEditor::moveToNextToken(QTextCursor& cursor, QTextCursor::MoveMode mode) {
    if (cursor.atBlockEnd()) {
        cursor.movePosition(QTextCursor::NextCharacter, mode);
        return;
    }

    QTextBlock block(cursor.block());
    QString blockText = block.text();
    int positionInBlock = cursor.position() - block.position();

    // go to end of token or end of word
    TokenIterator tokenIt(block, positionInBlock);
    if (tokenIt.isValid())
        positionInBlock = tokenIt->positionInBlock + tokenIt->length;
    else {
        int pos = positionInBlock;
        if (blockText[pos].isLetterOrNumber()) {
            ++pos;
            while (pos < blockText.size() && blockText[pos].isLetterOrNumber())
                ++pos;
        } else {
            ++pos;
        }
        positionInBlock = pos;
    }

    // skip whitespace
    while (positionInBlock < blockText.size() && blockText[positionInBlock].isSpace())
        ++positionInBlock;

    cursor.setPosition(positionInBlock + block.position(), mode);
}

void ScCodeEditor::moveToPreviousToken(QTextCursor& cursor, QTextCursor::MoveMode mode) {
    if (cursor.atBlockStart()) {
        cursor.movePosition(QTextCursor::PreviousCharacter, mode);
        return;
    }

    QTextBlock block(cursor.block());
    QString blockText = block.text();
    int positionInBlock = cursor.position() - block.position() - 1;

    // skip whitespace
    while (positionInBlock > 0 && blockText[positionInBlock].isSpace())
        --positionInBlock;

    cursor.setPosition(positionInBlock + block.position(), mode);
    if (positionInBlock == 0)
        return;

    // go to beginning of token or beginning of word
    TokenIterator tokenIt(block, positionInBlock);
    if (tokenIt.isValid()) {
        cursor.setPosition(tokenIt.position(), mode);
    } else {
        int pos = positionInBlock;
        if (blockText[pos].isLetterOrNumber()) {
            while (pos > 0 && blockText[pos - 1].isLetterOrNumber())
                --pos;
        }
        cursor.setPosition(pos + block.position(), mode);
    }
}

bool ScCodeEditor::insertMatchingTokens(const QString& text) {
    if (text.isEmpty())
        return false;

    QTextCursor cursor = textCursor();
    QTextDocument* document = cursor.document();
    int cursorPosition = cursor.position();

    QChar token = text[0];
    QChar matchingToken;

    static QString openingTokens("([{'\"");
    static QString closingTokens(")]}'\"");

    bool isOpeningToken, isClosingToken;

    int idx;
    if ((idx = openingTokens.indexOf(token)) != -1) {
        matchingToken = closingTokens[idx];
        isOpeningToken = true;
        isClosingToken = token == matchingToken;
    } else if ((idx = closingTokens.indexOf(token)) != -1) {
        matchingToken = openingTokens[idx];
        isClosingToken = true;
        isOpeningToken = token == matchingToken;
    } else
        return false;

    cursor.beginEditBlock();

    if (mInsertMatchingTokens) {
        if (cursor.hasSelection()) {
            if (isOpeningToken) {
                int start = cursor.selectionStart();
                int end = cursor.selectionEnd();
                cursor.setPosition(start);
                cursor.insertText(token);
                cursor.setPosition(end + 1);
                cursor.insertText(matchingToken);
            } else
                cursor.insertText(token);
        } else {
            if (isClosingToken && document->characterAt(cursorPosition) == token) {
                cursor.movePosition(QTextCursor::NextCharacter);
            } else if (isOpeningToken) {
                cursor.insertText(token);
                cursor.insertText(matchingToken);
                cursor.movePosition(QTextCursor::PreviousCharacter);
            } else
                cursor.insertText(token);
        }
    } else
        cursor.insertText(token);

    cursor.endEditBlock();

    cursor.setVerticalMovementX(-1);
    setTextCursor(cursor);

    if (isClosingToken)
        indent(cursor, JoinEditBlock);

    ensureCursorVisible();

    return true;
}

bool ScCodeEditor::removeMatchingTokens() {
    QTextCursor cursor = textCursor();
    QTextDocument* document = cursor.document();
    int cursorPosition = cursor.position();
    if (cursorPosition == 0)
        return false;

    QChar previousChar = document->characterAt(cursorPosition - 1);
    QChar nextChar;
    if (previousChar == '{')
        nextChar = '}';
    else if (previousChar == '[')
        nextChar = ']';
    else if (previousChar == '(')
        nextChar = ')';
    else if (previousChar == '\'' || previousChar == '"')
        nextChar = previousChar;
    else
        return false;

    if (document->characterAt(cursorPosition) != nextChar)
        return false;

    cursor.beginEditBlock();
    cursor.deletePreviousChar();
    cursor.deleteChar();
    cursor.endEditBlock();

    setTextCursor(cursor);
    return true;
}

QTextCursor ScCodeEditor::selectionForPosition(int position) {
    QTextBlock block(textDocument()->findBlock(position));
    if (!block.isValid())
        return QTextCursor();

    int positionInBlock = position - block.position();

    TokenIterator it = TokenIterator(block, positionInBlock);
    if (it.type() == Token::Unknown) {
        // Token is invalid, or Token::Unknown (i.e. punctuations).
        // Prefer token at previous position.
        TokenIterator alternativeIt = TokenIterator(block, positionInBlock - 1);
        if (alternativeIt.isValid())
            it = alternativeIt;
    }

    if (it.isValid()) {
        switch (it->type) {
        case Token::OpeningBracket:
        case Token::ClosingBracket: {
            BracketPair match;
            matchBracket(it, match);
            if (match.first.isValid() && match.second.isValid()) {
                int start = match.first.position();
                int end = match.second.position() + 1;
                QTextCursor selection(textDocument());
                if (it == match.second) {
                    selection.setPosition(start);
                    selection.setPosition(end, QTextCursor::KeepAnchor);
                } else {
                    selection.setPosition(end);
                    selection.setPosition(start, QTextCursor::KeepAnchor);
                }
                return selection;
            }
            break;
        }

        default:
            QTextCursor selection(textDocument());
            selection.setPosition(it.position());
            selection.setPosition(selection.position() + it->length, QTextCursor::KeepAnchor);
            return selection;
        }
    }

    return QTextCursor();
}

void ScCodeEditor::matchBrackets() {
    mBracketSelections.clear();

    QTextCursor cursor(textCursor());
    QTextBlock block(cursor.block());
    int posInBlock = cursor.positionInBlock();
    TokenIterator it(block);
    while (it.isValid() && it.block() == block) {
        const Token& token = *it;
        if (token.positionInBlock > posInBlock) {
            it = TokenIterator();
            break;
        } else if ((token.positionInBlock == posInBlock && token.type == Token::OpeningBracket)
                   || (token.positionInBlock == posInBlock - 1 && token.type == Token::ClosingBracket))
            break;
        ++it;
    }

    if (!it.isValid() || it.block() != block) {
        updateExtraSelections();
        return;
    }

    BracketPair match;
    matchBracket(it, match);

    if (match.first.isValid() && match.second.isValid()) {
        const Token& tok1 = *match.first;
        const Token& tok2 = *match.second;

        if ((tok1.character == '(' && tok2.character == ')') || (tok1.character == '[' && tok2.character == ']')
            || (tok1.character == '{' && tok2.character == '}')) {
            QTextEdit::ExtraSelection selection;
            selection.format = mBracketHighlight;
            cursor.setPosition(match.first.position());
            cursor.movePosition(QTextCursor::NextCharacter, QTextCursor::KeepAnchor);
            selection.cursor = cursor;
            mBracketSelections.append(selection);

            cursor.setPosition(match.second.position());
            cursor.movePosition(QTextCursor::NextCharacter, QTextCursor::KeepAnchor);
            selection.cursor = cursor;
            mBracketSelections.append(selection);

            if (mHighlightBracketContents) {
                QTextCharFormat format;
                format.setBackground(mBracketHighlight.background());
                selection.format = format;
                cursor.setPosition(match.first.position() + 1);
                cursor.setPosition(match.second.position(), QTextCursor::KeepAnchor);
                selection.cursor = cursor;
                mBracketSelections.append(selection);
            }
        } else {
            QTextEdit::ExtraSelection selection;
            selection.format = mBracketMismatchFormat;
            cursor.setPosition(match.first.position());
            cursor.setPosition(match.second.position() + 1, QTextCursor::KeepAnchor);
            selection.cursor = cursor;
            mBracketSelections.append(selection);
        }
    } else {
        if (it.type() == Token::OpeningBracket) {
            cursor.setPosition(it.position());
            cursor.movePosition(QTextCursor::End, QTextCursor::KeepAnchor);
        } else {
            cursor.setPosition(it.position() + 1);
            cursor.movePosition(QTextCursor::Start, QTextCursor::KeepAnchor);
        }
        QTextEdit::ExtraSelection selection;
        selection.format = mBracketMismatchFormat;
        selection.cursor = cursor;
        mBracketSelections.append(selection);
    }

    updateExtraSelections();
}

int ScCodeEditor::indentedStartOfLine(const QTextBlock& b) {
    QString t(b.text());
    int n = t.size();
    int i = 0;
    while (i < n) {
        QChar c(t[i]);
        if (c != ' ' && c != '\t')
            break;
        ++i;
    }

    return i;
}

void ScCodeEditor::updateExtraSelections() {
    QList<QTextEdit::ExtraSelection> selections;
    selections.append(mDiagnosticSelections);
    selections.append(mBracketSelections);
    selections.append(mSearchSelections);
    setExtraSelections(selections);
}

void ScCodeEditor::indent(EditBlockMode editBlockMode) { indent(textCursor(), editBlockMode); }

void ScCodeEditor::indent(const QTextCursor& selection, EditBlockMode editBlockMode) {
    if (selection.isNull())
        return;

    QTextCursor cursor(selection);

    if (editBlockMode == NewEditBlock)
        cursor.beginEditBlock();
    else
        cursor.joinPreviousEditBlock();

    QTextDocument* doc = QPlainTextEdit::document();
    int startBlockNum = doc->findBlock(cursor.selectionStart()).blockNumber();
    int endBlockNum = cursor.hasSelection() ? doc->findBlock(cursor.selectionEnd()).blockNumber() : startBlockNum;

    QStack<int> stack;
    int global_level = 0;
    int blockNum = 0;
    bool in_string = false;
    bool in_comment = false;

    QTextBlock block = QPlainTextEdit::document()->begin();
    while (block.isValid()) {
        int initialStackSize = stack.size();
        int level = 0;
        bool block_start_in_string = in_string;

        TextBlockData* data = static_cast<TextBlockData*>(block.userData());
        if (data) {
            int count = data->tokens.size();
            in_comment = data->isInMultilineComment;

            for (int idx = 0; idx < count; ++idx) {
                const Token& token = data->tokens[idx];
                switch (token.type) {
                case Token::OpeningBracket:
                    if (token.character != '(' || stack.size() || token.positionInBlock)
                        ++level;
                    break;

                case Token::ClosingBracket:
                    if (level)
                        --level;
                    else if (global_level) {
                        --global_level;
                        if (!stack.isEmpty() && global_level < stack.top())
                            stack.pop();
                    }
                    break;

                case Token::StringMark:
                    in_string = !in_string;
                    break;

                default:
                    break;
                }
            }
        }

        if (blockNum >= startBlockNum) {
            int indentLevel;
            if (data && data->tokens.size() && data->tokens[0].type == Token::ClosingBracket)
                indentLevel = stack.size();
            else if (!block_start_in_string)
                indentLevel = initialStackSize;
            else
                indentLevel = 0;
            // lexer does not detect "/*" as in comment, therefore we check if the current
            // block is equal to it. If so, also do not indent the multi-line comment start
            if (!in_comment && block.text() != "/*") {
                block = indent(block, indentLevel);
            }
        }

        if (blockNum == endBlockNum)
            break;

        block = block.next();
        ++blockNum;

        if (level) {
            global_level += level;
            stack.push(global_level);
        }
    }

    cursor.endEditBlock();
}

QString ScCodeEditor::makeIndentationString(int level) {
    if (level <= 0)
        return QString();

    if (mSpaceIndent) {
        const int spaces = mDoc->indentWidth() * level;
        QString indentationString(spaces, QChar(' '));
        return indentationString;
    } else {
        const int tabs = level;
        QString indentationString(tabs, QChar('\t'));
        return indentationString;
    }
}

QTextBlock ScCodeEditor::indent(const QTextBlock& block, int level) {
    QTextCursor cursor(block);
    cursor.movePosition(QTextCursor::StartOfBlock);
    cursor.setPosition(cursor.position() + indentedStartOfLine(block), QTextCursor::KeepAnchor);

    cursor.insertText(makeIndentationString(level));

    // modification has invalidated the block, so return a new one
    return cursor.block();
}

int ScCodeEditor::indentationLevel(const QTextCursor& cursor) {
    QTextDocument* doc = QPlainTextEdit::document();
    int startBlockNum = doc->findBlock(cursor.selectionStart()).blockNumber();

    QStack<int> stack;
    int level = 0;
    int blockNum = 0;
    QTextBlock block = QPlainTextEdit::document()->begin();
    while (block.isValid()) {
        if (level > 0) {
            stack.push(level);
            level = 0;
        }

        TextBlockData* data = static_cast<TextBlockData*>(block.userData());
        if (data) {
            int count = data->tokens.size();
            for (int idx = 0; idx < count; ++idx) {
                const Token& token = data->tokens[idx];
                switch (token.type) {
                case Token::OpeningBracket:
                    if (token.character != '(' || stack.size() || token.positionInBlock)
                        level += 1;
                    break;

                case Token::ClosingBracket:
                    if (level)
                        level -= 1;
                    else if (!stack.isEmpty()) {
                        stack.top() -= 1;
                        if (stack.top() <= 0)
                            stack.pop();
                    }
                    break;

                default:;
                }
            }
        }

        if (blockNum == startBlockNum)
            return stack.size();

        block = block.next();
        ++blockNum;
    }

    return -1;
}

void ScCodeEditor::insertSpaceToNextTabStop(QTextCursor& cursor) {
    if (mSpaceIndent) {
        const int indentWidth = mDoc->indentWidth();
        if (indentWidth < 1)
            return;
        const int spaces = indentWidth - (cursor.positionInBlock() % indentWidth);
        QString spaceString(spaces, QChar(' '));
        cursor.insertText(spaceString);
    } else {
        cursor.insertText("\t");
    }
}

void ScCodeEditor::triggerAutoCompletion() { mAutoCompleter->triggerCompletion(); }

void ScCodeEditor::triggerMethodCallAid() { mAutoCompleter->triggerMethodCallAid(); }

static bool isSingleLineComment(QTextBlock const& block) {
    static QRegularExpression commentRegex("^\\s*//.*");
    return commentRegex.match(block.text()).hasMatch();
}

static bool isSingleLineComment(QTextCursor const& selection) {
    QTextCursor cursor(selection);
    cursor.setPosition(selection.selectionStart());
    QTextBlock startBlock = cursor.block();

    cursor.setPosition(selection.selectionEnd() - 1);
    QTextBlock endBlock = cursor.block();

    for (QTextBlock block = startBlock; block != endBlock.next(); block = block.next()) {
        if (!isSingleLineComment(block))
            return false;
    }

    return true;
}

static bool isSelectionComment(QString const& text) {
    QString trimmed = text.trimmed();
    if (trimmed.startsWith(QStringLiteral("/*")) && trimmed.endsWith(QStringLiteral("*/")))
        return true;
    else
        return false;
}

void ScCodeEditor::toggleComment() {
    QTextCursor cursor = textCursor();

    if (cursor.hasSelection())
        toggleCommentSelection();
    else
        toggleCommentSingleLine();
}

void ScCodeEditor::toggleCommentSingleLine() {
    QTextCursor cursor = textCursor();
    cursor.beginEditBlock();

    toggleCommentSingleLine(cursor);

    cursor.endEditBlock();
}

void ScCodeEditor::addSingleLineComment(QTextCursor cursor, int indentation) {
    QTextBlock currentBlock(cursor.block());
    int blockIndentationLevel = indentationLevel(cursor);

    cursor.movePosition(QTextCursor::StartOfBlock);
    cursor.setPosition(cursor.position() + indentedStartOfLine(currentBlock), QTextCursor::KeepAnchor);

    QString commentString = makeIndentationString(indentation) + QStringLiteral("// ")
        + makeIndentationString(blockIndentationLevel - indentation);

    cursor.insertText(commentString);

    cursor.movePosition(QTextCursor::StartOfBlock);
}

void ScCodeEditor::removeSingleLineComment(QTextCursor cursor) {
    QTextBlock currentBlock(cursor.block());
    cursor.movePosition(QTextCursor::StartOfBlock);
    const int startPosition = cursor.position();
    const int indentPosition = indentedStartOfLine(currentBlock);
    const int commentStartPosition = startPosition + indentPosition;

    cursor.setPosition(commentStartPosition);
    cursor.setPosition(commentStartPosition + 3, QTextCursor::KeepAnchor);

    if (!cursor.selectedText().endsWith(QStringLiteral("// "))) {
        cursor.setPosition(commentStartPosition + 2, QTextCursor::KeepAnchor);
        if (!cursor.selectedText().endsWith(QStringLiteral("//")))
            return;
    }

    cursor.insertText("");
}

void ScCodeEditor::toggleCommentSingleLine(QTextCursor cursor) {
    QTextBlock currentBlock(cursor.block());

    cursor.beginEditBlock();

    if (!isSingleLineComment(currentBlock)) {
        int blockIndentation = indentationLevel(cursor);
        addSingleLineComment(cursor, blockIndentation);
    } else
        removeSingleLineComment(cursor);

    cursor.endEditBlock();
    indent(cursor);
}

static bool isBlockOnlySelection(QTextCursor cursor) {
    Q_ASSERT(cursor.hasSelection());

    QTextCursor begin(cursor);
    begin.setPosition(begin.anchor());

    if (begin.atBlockStart() && (cursor.atBlockStart() || cursor.atBlockEnd()))
        return true;
    else
        return false;
}

void ScCodeEditor::toggleCommentSelection() {
    QTextCursor cursor = textCursor();
    cursor.beginEditBlock();

    if (isBlockOnlySelection(cursor)) {
        const bool isComment = isSingleLineComment(cursor);

        QTextCursor selectionCursor(cursor);
        selectionCursor.setPosition(cursor.selectionStart());

        QTextBlock currentBlock = selectionCursor.block();
        int firstBlockIndentation = isComment ? 0 : indentationLevel(selectionCursor);

        do {
            QTextCursor blockCursor(currentBlock);
            if (!isComment)
                addSingleLineComment(blockCursor, firstBlockIndentation);
            else
                removeSingleLineComment(blockCursor);
            currentBlock = currentBlock.next();
        } while (currentBlock.isValid() && currentBlock.position() < cursor.selectionEnd());

        if (!isComment) {
            // fix up selection
            QTextCursor newSelection(cursor);
            if (cursor.anchor() < cursor.position()) {
                newSelection.setPosition(newSelection.selectionStart());
                newSelection.movePosition(QTextCursor::StartOfBlock);
                newSelection.setPosition(cursor.selectionEnd(), QTextCursor::KeepAnchor);
            } else {
                newSelection.setPosition(newSelection.selectionEnd());
                newSelection.setPosition(cursor.selectionStart(), QTextCursor::KeepAnchor);
                newSelection.movePosition(QTextCursor::StartOfBlock, QTextCursor::KeepAnchor);
            }
            setTextCursor(newSelection);
        }
    } else {
        QString selectionText = cursor.selectedText();
        QTextCursor selectionCursor(cursor);
        if (isSelectionComment(selectionText)) {
            selectionText = selectionText.trimmed().remove(0, 2);
            selectionText.chop(2);
            selectionCursor.insertText(selectionText);
        } else {
            selectionText = QStringLiteral("/*") + selectionText + QStringLiteral("*/");
            selectionCursor.insertText(selectionText);
        }

        // fix up selection
        const int position = selectionCursor.position();
        const int anchor = selectionCursor.anchor();
        if (position > anchor) {
            cursor.setPosition(position - selectionText.size());
            cursor.setPosition(position, QTextCursor::KeepAnchor);
        } else {
            cursor.setPosition(position);
            cursor.setPosition(position - selectionText.size(), QTextCursor::KeepAnchor);
        }
        setTextCursor(cursor);
    }

    cursor.endEditBlock();
}

// taking nested brackets into account
TokenIterator ScCodeEditor::previousOpeningBracket(TokenIterator it) {
    int level = 0;
    while (it.isValid()) {
        switch (it->type) {
        case Token::OpeningBracket:
            if (level == 0)
                return it;
            --level;
            break;

        case Token::ClosingBracket:
            ++level;

        default:
            break;
        }
        --it;
    }
    return it;
}

// taking nested brackets into account
TokenIterator ScCodeEditor::nextClosingBracket(TokenIterator it) {
    int level = 0;
    while (it.isValid()) {
        switch (it->type) {
        case Token::ClosingBracket:
            if (level == 0)
                return it;
            --level;
            break;

        case Token::OpeningBracket:
            ++level;

        default:
            break;
        }
        ++it;
    }
    return it;
}

void ScCodeEditor::matchBracket(const TokenIterator& bracket, BracketPair& match) {
    Q_ASSERT(bracket.isValid());

    switch (bracket->type) {
    case Token::OpeningBracket:
        match.first = bracket;
        match.second = nextClosingBracket(bracket.next());
        break;
    case Token::ClosingBracket:
        match.second = bracket;
        match.first = previousOpeningBracket(bracket.previous());
        break;
    default:
        match.first = TokenIterator();
        match.second = TokenIterator();
    }
}

void ScCodeEditor::gotoNextBlock() {
    QTextCursor cursor = textCursor();

    TokenIterator tokenIt = TokenIterator::rightOf(cursor.block(), cursor.positionInBlock());
    if (tokenIt.type() == Token::OpeningBracket && tokenIt.block() == cursor.block()
        && tokenIt->positionInBlock == cursor.positionInBlock())
        ++tokenIt;

    tokenIt = nextClosingBracket(tokenIt);

    if (tokenIt.isValid())
        setTextCursor(cursorAt(tokenIt, 1));
    else {
        cursor.movePosition(QTextCursor::End);
        setTextCursor(cursor);
    }
}

void ScCodeEditor::gotoPreviousBlock() {
    QTextCursor cursor = textCursor();

    TokenIterator tokenIt = TokenIterator::leftOf(cursor.block(), cursor.positionInBlock());
    if (tokenIt.type() == Token::ClosingBracket && tokenIt.block() == cursor.block()
        && tokenIt->positionInBlock == cursor.positionInBlock() - 1)
        --tokenIt;


    tokenIt = previousOpeningBracket(tokenIt);

    if (tokenIt.isValid())
        setTextCursor(cursorAt(tokenIt));
    else {
        cursor.movePosition(QTextCursor::Start);
        setTextCursor(cursor);
    }
}

QTextCursor ScCodeEditor::blockAroundCursor(const QTextCursor& cursor) {
    TokenIterator left_bracket =
        previousOpeningBracket(TokenIterator::leftOf(cursor.block(), cursor.positionInBlock()));
    if (!left_bracket.isValid())
        return QTextCursor();
    TokenIterator right_bracket = nextClosingBracket(TokenIterator::rightOf(cursor.block(), cursor.positionInBlock()));
    if (!right_bracket.isValid())
        return QTextCursor();

    QTextCursor block_cursor = cursor;
    block_cursor.setPosition(left_bracket.position());
    block_cursor.setPosition(right_bracket.position() + 1, QTextCursor::KeepAnchor);
    return block_cursor;
}

void ScCodeEditor::selectBlockAroundCursor() {
    QTextCursor block_cursor = blockAroundCursor(textCursor());
    if (!block_cursor.isNull())
        setTextCursor(block_cursor);
}

// The regions come from the language server, see LspClient::currentRegions. Without it, there are none.
void ScCodeEditor::withRegions(std::function<void(const QList<LspClient::Region>&)> then, bool retried) {
    static bool noted = false;
    QPointer<ScCodeEditor> self(this);
    Main::lspClient()->currentRegions(mDoc, [self, then, retried](const QList<LspClient::Region>& regions, bool ok) {
        if (!self)
            return;
        if (ok) {
            then(regions);
            return;
        }
        // The text changed while the server answered, so ask about the new text.
        if (!retried && Main::lspClient()->hasRegions()) {
            self->withRegions(then, true);
            return;
        }
        if (!noted) {
            QString note = tr("Regions need the language server, sc_lsp. Without it, evaluating uses the line.\n");
            Main::scProcess()->post(note);
        }
        noted = true;
        then({});
    });
}

void ScCodeEditor::withCurrentRegion(std::function<void(const QTextCursor&)> then) {
    const int position = textCursor().position();
    QPointer<ScCodeEditor> self(this);
    withRegions([self, position, then](const QList<LspClient::Region>& regions) {
        // A cursor just before or after a region is in it.
        for (const auto& r : regions) {
            if (r.start <= position && position <= r.end) {
                QTextCursor region(self->textDocument());
                region.setPosition(r.start);
                region.setPosition(r.end, QTextCursor::KeepAnchor);
                then(region);
                return;
            }
        }
        then(QTextCursor());
    });
}

void ScCodeEditor::paintLineIndicatorExtras(QPainter& p, const QRect& rect) {
    // The last regions published, which can be a little older than the text, so positions are kept in it.
    const auto regions = Main::lspClient()->regions(mDoc);
    if (regions.isEmpty())
        return;
    QTextDocument* doc = textDocument();
    const int last = qMax(0, doc->characterCount() - 1);
    const int position = textCursor().position();
    const QPalette palette = mLineIndicator->palette();
    QColor other = palette.color(QPalette::ButtonText);
    other.setAlpha(60);
    const QColor current = palette.color(QPalette::Highlight);
    const QPointF offset = contentOffset();
    for (const auto& r : regions) {
        const QTextBlock first = doc->findBlock(qMin(r.start, last));
        const QTextBlock end = doc->findBlock(qMin(r.end, last));
        if (!first.isValid() || !end.isValid())
            continue;
        const qreal top = blockBoundingGeometry(first).translated(offset).top();
        if (top > rect.bottom())
            break; // in order
        const qreal bottom = blockBoundingGeometry(end).translated(offset).bottom();
        if (bottom < rect.top())
            continue;
        const bool here = r.start <= position && position <= r.end;
        p.fillRect(QRectF(1, top + 1, regionBarMargin - 2, bottom - top - 2), here ? current : other);
    }
}

void ScCodeEditor::selectCurrentRegion() {
    withCurrentRegion([this](const QTextCursor& region) {
        if (!region.isNull() && region.hasSelection())
            setTextCursor(region);
    });
}

void ScCodeEditor::gotoNextRegion() {
    withRegions([this](const QList<LspClient::Region>& regions) {
        QTextCursor cursor = textCursor();
        const int position = cursor.position();
        for (const auto& r : regions) {
            if (r.start > position) {
                cursor.setPosition(r.start);
                setTextCursor(cursor);
                return;
            }
        }
        cursor.movePosition(QTextCursor::End);
        setTextCursor(cursor);
    });
}

void ScCodeEditor::gotoPreviousRegion() {
    withRegions([this](const QList<LspClient::Region>& regions) {
        QTextCursor cursor = textCursor();
        const int position = cursor.position();
        for (auto it = regions.rbegin(); it != regions.rend(); ++it) {
            if (it->end < position) {
                cursor.setPosition(it->end);
                setTextCursor(cursor);
                return;
            }
        }
        cursor.movePosition(QTextCursor::Start);
        setTextCursor(cursor);
    });
}

bool ScCodeEditor::openDocumentation() { return Main::openDocumentation(symbolUnderCursor()); }

void ScCodeEditor::openDefinition() {
    if (!openLocations("textDocument/definition", true))
        Main::openDefinition(symbolUnderCursor(), this);
}

void ScCodeEditor::openCommandLine() { Main::openCommandLine(symbolUnderCursor()); }

void ScCodeEditor::findReferences() {
    if (!openLocations("textDocument/references", false))
        Main::findReferences(symbolUnderCursor(), this);
}

// zero indexed.
int cursorLineNumber(QTextCursor cursor) {
    // https://stackoverflow.com/questions/15814776/how-do-i-get-the-actual-visible-cursors-line-number/15821474#15821474
    cursor.movePosition(QTextCursor::StartOfLine);

    int lines = 0;
    while (cursor.positionInBlock() > 0) {
        cursor.movePosition(QTextCursor::Up);
        lines++;
    }
    QTextBlock block = cursor.block().previous();

    while (block.isValid()) {
        lines += block.lineCount();
        block = block.previous();
    }
    return lines;
}

void ScCodeEditor::evaluateLine() {
    QString text;

#ifdef SC_USE_QTWEBENGINE
    HelpBrowserDocklet* help = MainWindow::instance()->helpBrowserDocklet();
    if (help && help->browser()->helpBrowserHasFocus()) {
        help->browser()->evaluateSelection(false);
        return; // early return
    }
#endif // SC_USE_QTWEBENGINE

    // Try current selection
    QTextCursor cursor = textCursor();
    if (cursor.hasSelection()) {
        text = cursor.selectedText();
    } else {
        text = cursor.block().text();

        if (mStepForwardEvaluation) {
            QTextCursor newCursor = cursor;
            newCursor.movePosition(QTextCursor::NextBlock);
            newCursor.setVerticalMovementX(cursor.verticalMovementX());
            setTextCursor(newCursor);
        }

        // Adjust cursor for code blinking:
        cursor.movePosition(QTextCursor::StartOfBlock);
        cursor.movePosition(QTextCursor::EndOfBlock, QTextCursor::KeepAnchor);
    }

    if (text.isEmpty())
        return;

    text.replace(QChar(0x2029), QChar('\n'));

    Main::evaluateCode(text, false, &document()->filePath(), cursorLineNumber(textCursor()));

    blinkCode(cursor);
}

void ScCodeEditor::evaluateRegion() {
#ifdef SC_USE_QTWEBENGINE
    HelpBrowserDocklet* help = MainWindow::instance()->helpBrowserDocklet();
    if (help && help->browser()->helpBrowserHasFocus()) {
        help->browser()->evaluateSelection(true);
        return; // early return
    }
#endif // SC_USE_QTWEBENGINE

    // Try current selection
    if (textCursor().hasSelection()) {
        evaluateCursor(textCursor());
        return;
    }
    // If no selection, try current region, then current line
    withCurrentRegion([this](const QTextCursor& region) {
        if (!region.isNull()) {
            evaluateCursor(region);
            return;
        }
        QTextCursor cursor = textCursor();
        if (mStepForwardEvaluation) {
            QTextCursor newCursor = cursor;
            newCursor.movePosition(QTextCursor::NextBlock);
            newCursor.setVerticalMovementX(cursor.verticalMovementX());
            setTextCursor(newCursor);
        }
        cursor.movePosition(QTextCursor::StartOfBlock);
        cursor.movePosition(QTextCursor::EndOfBlock, QTextCursor::KeepAnchor);
        evaluateCursor(cursor);
    });
}

void ScCodeEditor::evaluateCursor(const QTextCursor& cursor) {
    QString text = cursor.selectedText();
    if (text.isEmpty())
        return;

    text.replace(QChar(0x2029), QChar('\n'));

    Main::evaluateCode(text, false, &document()->filePath(), cursorLineNumber(textCursor()), 0);

    blinkCode(cursor);
}

void ScCodeEditor::evaluateDocument() {
    QString documentText = textDocument()->toPlainText();
    Main::evaluateCode(documentText, false, &document()->filePath(), 0, 0);
}

QTextCursor ScCodeEditor::cursorAt(const TokenIterator it, int offset) {
    Q_ASSERT(it.isValid());

    QTextCursor textCursor(textDocument());
    textCursor.setPosition(it.position() + offset);

    return textCursor;
}

} // namespace ScIDE
