/*
    SuperCollider Qt IDE
    Copyright (c) 2026 SuperCollider Team
    https://supercollider.github.io/

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

#pragma once

#include <QVector>
#include <QtGlobal>

namespace ScIDE {

// A piece of code the language server says what it is, for highlighting. Each is on one line.
struct SemanticToken {
    enum Kind : quint8 {
        Comment,
        String,
        Character,
        Symbol,
        Number,
        Keyword,
        Builtin,
        Primitive,
        Class,
        Method,
        Variable,
        Parameter,
        Property,
        Environment,
        KeywordArgument,
        Other // a kind the IDE doesn't know
    };
    enum Modifier : quint8 { Declaration = 1 << 0, Static = 1 << 1, Readonly = 1 << 2, Unknown = 1 << 3 };

    int start; // a character position in the document
    int length;
    Kind kind;
    quint8 modifiers;

    bool operator==(const SemanticToken& o) const {
        return start == o.start && length == o.length && kind == o.kind && modifiers == o.modifiers;
    }
};

using SemanticTokens = QVector<SemanticToken>;

} // namespace ScIDE
