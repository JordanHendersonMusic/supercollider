/*
    SuperCollider real time audio synthesis system
    Copyright (c) 2002 James McCartney. All rights reserved.
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


#pragma once

#include "PyrSymbol.h"
#include "SC_Export.h"
#include <filesystem>

extern char* text;

extern int charno, lineno, linepos;
extern int* linestarts;

extern int parseFailed;
extern bool compilingCmdLine;
extern bool compiledOK;

extern int lastClosedFuncCharNo;

extern intptr_t zzval;
extern intptr_t gParserResult;

extern PyrSymbol* gCompilingFileSym;

struct ClassExtFile {
    struct ClassExtFile* next;
    PyrSymbol* fileSym;
    int startPos, endPos, lineOffset;
};

typedef struct classdep {
    struct classdep* next;
    struct classdep* superClassDep;
    struct classdep* subclasses;
    PyrSymbol* className;
    PyrSymbol* superClassName;
    PyrSymbol* fileSym;
    int startPos, endPos, lineOffset;
} ClassDependancy;

struct FatalInterpreterError : public std::runtime_error {
    using std::runtime_error::runtime_error;
};

SCLANG_DLLEXPORT_C bool compileLibrary(bool standalone);
// All exceptions are caught, except FatalInterpreterErrors
SCLANG_DLLEXPORT_C void runLibrary(PyrSymbol* selector);

void startLexerCmdLine(char* textbuf, int textbuflen);
void startLexerForTestingClassLib(PyrSymbol* file_name_with_src);

void finiLexer();

int yylex();
void yyerror(const char* s);
void fatal();

std::filesystem::path relativeToCompileDir(const std::filesystem::path&);

void postErrorLine(int linenum, int start, int charpos);

int rtf2txt(char* txt);
int html2txt(char* txt);
