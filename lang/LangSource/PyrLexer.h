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
#include "SCBase.h"
#include "PyrSymbol.h"
#include "SC_Export.h"
#include "lexer.hpp"
#include <filesystem>
#include <optional>

// These are set
extern bool gCompilingCmdLine;
extern PyrSymbol* gCompilingFileSym;
extern char* gCompilingText;

// Only valid after startLexer* and before finiLexer.
// Is not nullptr
std::optional<sc::lex::CodePointStream* const> getActiveCodePointStream();

// This is set when calling yyparse.
extern int gParseFailed;
extern bool gCompiledOK;

// The following globals are to be removed.
extern int lastClosedFuncCharNo;


struct FatalInterpreterError : public std::runtime_error {
    using std::runtime_error::runtime_error;
};

SCLANG_DLLEXPORT_C bool compileLibrary(bool standalone);
// All exceptions are caught, except FatalInterpreterErrors
SCLANG_DLLEXPORT_C void runLibrary(PyrSymbol* selector);

void startLexerCmdLine(char* textbuf, int textbuflen);
void startLexerForTestingClassLib(PyrSymbol* file_name_with_src);

// Must not be called until all compilation has finished.
void finiLexer();

void printErrorLine(const sc::lex::CodePointStream& char_stream, sc::lex::SourceCodeRange r,
                    const char* short_description = nullptr);

enum struct ErrorType { Lexing, Parsing, Compiling };
struct PrintErrorLineInfo {
    sc::lex::SourceCodeRange r;
    const char* short_description { nullptr };
};
void printError(ErrorType type, const char* error_description, const sc::lex::CodePointStream& char_stream,
                const std::vector<PrintErrorLineInfo>&, const char* file_path = nullptr);


int yylex();
void yyerror(const char* s);
void fatal();

std::filesystem::path relativeToCompileDir(const std::filesystem::path&);

int rtf2txt(char* txt);
int html2txt(char* txt);
