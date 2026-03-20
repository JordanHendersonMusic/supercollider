/*
    SuperCollider real time audio synthesis system
    Copyright (c) 2002 James McCartney. All rights reserved.
    http://www.audiosynth.com
    Copyright (c) 2017 Brian Heim (boost::filesystem additions)

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

#include "PyrLexer.h"
#include "PyrSlot.h"
#include "PyrSymbol.h"
#include "SC_Constants.h"
#include "VMGlobals.h"

#include <algorithm>
#include <optional>
#include <stdlib.h>
#include <string.h>
#include <float.h>
#include <math.h>
#include <new>
#include <stdlib.h>
#include <limits>
#include <set>

#ifdef _WIN32
#    include <direct.h>
#else
#    include <sys/param.h>
#endif

#include <filesystem>
#include <fstream>
#include <string.h>

#include "PyrParseNode.h"
#include "Bison/lang11d_tab.h"
#include "SCBase.h"
#include "PyrObject.h"
#include "PyrObjectProto.h"
#include "PyrSched.h"
#include "GC.h"
#include "SimpleStack.h"

#include "PyrSymbolTable.h"
#include "PyrInterpreter.h"
#include "PyrObjectProto.h"
#include "PyrPrimitiveProto.h"
#include "PyrKernelProto.h"
#include "InitAlloc.h"
#include "PredefinedSymbols.h"
#include "SC_LanguageConfig.hpp"

#include "SC_Filesystem.hpp" // getDirectory, resolveIfAlias, isStandalone
#include "SC_Codecvt.hpp" // path_to_utf8_str
#include "SC_TextUtils.hpp"

#include "lexer.hpp"

int yyparse();
PyrSlot process_accidental_cents(const char* s);
PyrSlot process_accidental_steps(const char* s);

double compileStartTime;
int gNumCompiledFiles;

namespace fs = std::filesystem;
using DirName = SC_Filesystem::DirName;

PyrSymbol* gCompilingFileSym = nullptr;
VMGlobals* gCompilingVMGlobals = nullptr;
static fs::path gCompileDir;

bool gShowWarnings = false;
LongStack closedFuncCharNo;
LongStack generatorStack;
int lastClosedFuncCharNo = 0;

const char* binopchars = "!@%&*-+=|<>?/";
fs::path currfilename;
std::string printingCurrfilename; // for error reporting

int lexCmdLine = 0;
bool compilingCmdLine = false;
bool compilingCmdLineErrorWindow = false;

// TODO: replace with yylval
intptr_t zzval;

// TODO: replace with yyloc
int lineno, charno, linepos;
int* linestarts;
int maxlinestarts;

// This is the text of the source file currently being tokenized.
char* text;
int textlen;
int textpos;
// I don't know what these do.
int errLineOffset, errCharPosOffset;
int parseFailed = 0;
bool compiledOK = false;
std::set<fs::path> compiledDirectories;


int sc_strtoi(const char* str, int n, int base) {
    int z = 0;
    for (int i = 0; i < n; ++i) {
        int c = *str++;
        if (!c)
            break;
        if (c >= '0' && c <= '0' + sc_min(10, base) - 1)
            z = z * base + c - '0';
        else if (c >= 'a' && c <= 'a' + sc_min(36, base) - 11)
            z = z * base + c - 'a' + 10;
        else if (c >= 'A' && c <= 'A' + sc_min(36, base) - 11)
            z = z * base + c - 'A' + 10;
    }
    return z;
}

double sc_strtof(const char* str, int n, int base) {
    double z = 0.;
    int decptpos = 0;
    for (int i = 0; i < n; ++i) {
        int c = *str++;
        if (!c)
            break;
        if (c >= '0' && c <= '0' + sc_min(10, base) - 1)
            z = z * base + c - '0';
        else if (c >= 'a' && c <= 'a' + sc_min(36, base) - 11)
            z = z * base + c - 'a' + 10;
        else if (c >= 'A' && c <= 'A' + sc_min(36, base) - 11)
            z = z * base + c - 'A' + 10;
        else if (c == '.')
            decptpos = i;
    }
    // calculation previously included decimal point in count of columns (was n-decptpos); there are 1 less than n
    // characters which are columns in the number contribution
    z = z / pow((double)base, n - 1 - decptpos);
    return z;
}

bool startLexer(PyrSymbol* fileSym, const fs::path& p, int startPos, int endPos, int lineOffset);
void startLexerCmdLine(char* textbuf, int textbuflen);
void finiLexer();

void initLexer() {
    // strcpy(binopchars, "!@%&*-+=|:<>?/");
}

namespace lex = sc::lex;
using namespace lex::literals;
using TokenType = lex::TokenType;

[[nodiscard]] constexpr std::optional<yytokentype> convert_to_bison_tokentype(TokenType t) {
    const auto i = static_cast<int>(t);
    if (i < 256)
        return static_cast<yytokentype>(i); // easy case, all ascii.
    else if (i == 256)
        return YYerror;
    else if (i == 257)
        return YYUNDEF;
    else if (i == 288)
        return UMINUS;

    // TODO: THIS IS MISSING CASES!!!!
    switch (t) {
    case TokenType::Name:
        return NAME;
        //
    case TokenType::Integer:
    case TokenType::IntegerRadix:
    case TokenType::Hexidecimal:
        return INTEGER;
        //
    case TokenType::Float:
    case TokenType::FloatRadix:
    case TokenType::FloatExponent:
    case TokenType::Inf: // inf is not a keyword in the grammar.
        return SC_FLOAT;
        //
    case TokenType::AccidentalSteps:
    case TokenType::AccidentalCents:
        return ACCIDENTAL;
        //
    case TokenType::Symbol:
    case TokenType::SymbolSlash:
    case TokenType::SymbolQuote:
        return SYMBOL;
        //
    case TokenType::Ascii:
        return ASCII;
        //
    case TokenType::ClassName:
        return CLASSNAME;
    case TokenType::While:
        return WHILE;
    case TokenType::PrimitiveName:
        return PRIMITIVENAME;
    case TokenType::LeftArrow:
        return LEFTARROW;
        //
    case TokenType::Pi:
        return PIE;
    case TokenType::True:
        return TRUEOBJ;
    case TokenType::False:
        return FALSEOBJ;
    case TokenType::Nil:
        return NILOBJ;
    case TokenType::Var:
        return VAR;
    case TokenType::Arg:
        return ARG;
    case TokenType::ClassVar:
        return CLASSVAR;
    case TokenType::Const:
        return SC_CONST;
    case TokenType::Ellipsis:
        return ELLIPSIS;
    case TokenType::DotDot:
        return DOTDOT;
    case TokenType::BeginClosedFunction:
        return BEGINCLOSEDFUNC;
    case TokenType::BadToken:
        return BADTOKEN;
    case TokenType::Interpret:
        return INTERPRET;
    case TokenType::BeginGenerator:
        return BEGINGENERATOR;
    case TokenType::CurryArg:
        return CURRYARG;
    case TokenType::BinaryOperator:
        return BINOP;
    case TokenType::KeywordBinaryOperator:
        return KEYBINOP;
    case TokenType::ReadWriteVar:
        return READWRITEVAR;
    default:
        assert(false); // should not happen
        return std::nullopt;
    }
}

constexpr inline int str_to_int(const char* str, size_t n, int base) {
    int z = 0;
    for (int i = 0; i < n; ++i) {
        int c = *str++;
        if (!c)
            break;
        if (c >= '0' && c <= '0' + std::min(10, base) - 1)
            z = z * base + c - '0';
        else if (c >= 'a' && c <= 'a' + std::min(36, base) - 11)
            z = z * base + c - 'a' + 10;
        else if (c >= 'A' && c <= 'A' + std::min(36, base) - 11)
            z = z * base + c - 'A' + 10;
    }
    return z;
}

extern int lastClosedFuncCharNo;
inline yytokentype operator""_yyt(char c) { return static_cast<yytokentype>(c); }
inline yytokentype operator""_yyt(unsigned long long c) { return static_cast<yytokentype>(c); }
struct BisonSemAction {
public:
    BisonSemAction(const char* source): source(source) {};
    BisonSemAction() = delete;
    BisonSemAction(BisonSemAction&&) noexcept = default;
    BisonSemAction(const BisonSemAction&) = default;
    BisonSemAction& operator=(BisonSemAction&&) noexcept = default;
    BisonSemAction& operator=(const BisonSemAction&) = default;

    const char* source;
    std::vector<std::pair<TokenType, int>> bracket_stack {};

    struct Output {
        TokenType type {};
        lex::SourceCodeRange range {};
        std::optional<PyrSlot> slot {};
        bool is_error { false };
    };

    template <TokenType T> std::optional<Output> token(lex::SourceCodeRange loc) {
        // Discard
        if constexpr (T == TokenType::Comment || T == TokenType::MultiLineComment
                      || T == TokenType::DocumentationComment || T == TokenType::NewLine || T == TokenType::Space
                      || T == TokenType::Tab) {
            return std::nullopt;
        }

        // Basic symbols
        else if constexpr (T == TokenType::Arg || T == TokenType::Var || T == TokenType::Const || T == TokenType::While
                           || T == TokenType::ClassVar || T == TokenType::Name || T == TokenType::ClassName
                           || T == TokenType::PrimitiveName || T == TokenType::BinaryOperator
                           || T == TokenType::ReadWriteVar || T == '<'_tokentype || T == '>'_tokentype
                           || T == '-'_tokentype || T == '*'_tokentype || T == '+'_tokentype || T == '|'_tokentype) {
            return { { T, loc, PyrSlot::make(text_to_symbol(loc)) } };
        }

        // More complex symbols that drop part of the location and/or use escape characters.
        else if constexpr (T == TokenType::KeywordBinaryOperator)
            return { { T, loc, PyrSlot::make(text_to_symbol(loc, 0, 1)) } };
        else if constexpr (T == TokenType::SymbolSlash)
            return { { T, loc, PyrSlot::make(text_to_symbol(loc, 1, 0)) } };
        else if constexpr (T == TokenType::SymbolQuote)
            return { { T, loc, PyrSlot::make(text_to_symbol(loc, 1, 1, true)) } };

        // Constants
        else if constexpr (T == TokenType::Nil || T == TokenType::Inf || T == TokenType::True || T == TokenType::False
                           || T == TokenType::Pi)
            return { { T, loc, to_constant<T>() } };


        // Open brackets
        else if constexpr (T == '('_tokentype || T == '['_tokentype || T == '{'_tokentype
                           || T == TokenType::BeginClosedFunction) {
            bracket_stack.push_back({ T, loc.begin.absolute });
            return { { T, loc } };
        }

        // Closing brackets
        else if constexpr (T == ')'_tokentype || T == ']'_tokentype || T == '}'_tokentype) {
            if (bracket_stack.empty()) {
                ::error("Incorrect brackets — bracket stack was empty.\n");
                return { { TokenType::BadToken, loc } };
            }
            for (const auto& t : get_closing_brackets<T>()) {
                if (bracket_stack.back().first == t) {
                    // This is pushed even if it isn't a closed function.
                    lastClosedFuncCharNo = bracket_stack.back().second;
                    bracket_stack.pop_back();
                    return { { T, loc } };
                }
            }
            ::error("Incorrect brackets — mismatch expected '%d' got %d.\n", static_cast<int>(T),
                    bracket_stack.back().first);
            return { { TokenType::BadToken, loc } };
        }


        // Floats
        else if constexpr (T == TokenType::Float)
            return { { T, loc, PyrSlot::make(atof(fill_temp_buf(loc))) } };
        else if constexpr (T == TokenType::FloatExponent)
            return { { T, loc, PyrSlot::make(atof(fill_temp_buf(loc))) } };

        // Radix, both int and float
        else if constexpr (T == TokenType::IntegerRadix || T == TokenType::FloatRadix) {
            const char* start = source + loc.begin.absolute;
            // Looking for radix.
            const char* it = start;
            while (*it != 'r') // Potentially unsafe, but the lexer guarenteed this was found.
                ++it;
            const int radix = str_to_int(start, it - start, 10);
            ++it; // drop r
            if constexpr (T == TokenType::IntegerRadix) {
                const auto slot_value = sc_strtoi(it, (source + loc.end.absolute) - it, radix);
                return { { T, loc, PyrSlot::make(slot_value) } };
            } else {
                const double slot_value = sc_strtof(it, (source + loc.end.absolute) - it, radix);
                return { { T, loc, PyrSlot::make(slot_value) } };
            }
        }

        else if constexpr (T == TokenType::Integer)
            return { { T, loc, PyrSlot::make(atoi(fill_temp_buf(loc))) } };

        else if constexpr (T == TokenType::Hexidecimal) {
            const char* c = source + loc.begin.absolute;
            const char* const end = source + loc.end.absolute;
            // BUG: this is probably a bug, we are ignoring everything before the 'x'
            while (*c != 'x' && *c != 'X' && *c != 0 && c < end)
                ++c;
            ++c;
            int val = 0;
            while (c < end) {
                if (*c >= '0' && *c <= '9')
                    val = val * 16 + *c - '0';
                else if (*c >= 'a' && *c <= 'z')
                    val = val * 16 + *c - 'a' + 10;
                else if (*c >= 'A' && *c <= 'Z')
                    val = val * 16 + *c - 'A' + 10;
                c++;
            }
            return { { T, loc, PyrSlot::make(val) } };
        }

        else if constexpr (T == TokenType::Ascii) {
            assert(source[loc.begin.absolute] == '$');
            if (loc.size() == 2) {
                const char out = source[loc.begin.absolute + 1];
                if (out == 0)
                    // TODO: consider this change in more detail.
                    // This is a little bit odd, but appears to be the current behaviour.
                    // I believe this is because the language client places extra spaces after the code you evaluate.
                    // There might be discrepencies here between class files, other language clients, and even depending
                    // on how you evaluate a file, I've opted to make this consistent. If we need the null terminator
                    // character, we could use $\0, although that currently produces the same of $0.
                    return { { T, loc, PyrSlot::make(' ') } };
                return { { T, loc, PyrSlot::make(out) } };
            }
            assert(loc.size() == 3);
            assert(source[loc.begin.absolute] == '$');
            assert(source[loc.begin.absolute + 1] == '\\');
            // Three only occurs for the following escape characters.
            char out = source[loc.begin.absolute + 2];
            if (out == 'n')
                out = '\n';
            else if (out == 'r')
                out = '\r';
            else if (out == 't')
                out = '\t';
            else if (out == 'f')
                out = '\f';
            else if (out == 'v')
                out = '\v';
            return { { T, loc, PyrSlot::make(out) } };
        }

        else if constexpr (T == TokenType::AccidentalCents)
            return { { TokenType::AccidentalCents, loc, process_accidental_cents(fill_temp_buf(loc)) } };
        else if constexpr (T == TokenType::AccidentalSteps)
            return { { TokenType::AccidentalSteps, loc, process_accidental_steps(fill_temp_buf(loc)) } };


        // default, empty
        else
            return { { T, loc } };
    }


    // Note: you are NOT allowed to discard the end token, otherwise there would be an infinite loop.
    template <TokenType type> Output end(lex::SourceCodeRange loc) { return { type, loc }; };

    template <TokenType type, typename... ARGS>
    std::optional<Output> error(lex::SourceCodeRange loc, const char* fmt, ARGS... args) {
        ::error(fmt, std::forward<ARGS>(args)...);
        return { { type, loc, {}, true } };
    }

    template <typename... ARGS> void warn(lex::SourceCodeRange loc, const char* fmt, ARGS... args) {
        ::post(fmt, std::forward<ARGS>(args)...);
    }

private:
    std::string temp_buffer {};

    template <TokenType T> PyrSlot to_constant() {
        if constexpr (T == TokenType::Pi)
            return PyrSlot::make(pi);
        else if constexpr (T == TokenType::Nil)
            return PyrSlot::make(PyrNil {});
        else if constexpr (T == TokenType::Inf)
            return PyrSlot::make(std::numeric_limits<double>::max());
        else if constexpr (T == TokenType::True)
            return PyrSlot::make(true);
        else if constexpr (T == TokenType::False)
            return PyrSlot::make(false);
        // else
        // static_assert(false); // old compilers don't like this...
    }

    const char* fill_temp_buf(lex::SourceCodeRange loc) {
        temp_buffer.clear();
        temp_buffer.insert(temp_buffer.begin(), source + loc.begin.absolute, source + loc.end.absolute);
        return temp_buffer.c_str();
    }

    PyrSymbol* text_to_symbol(lex::SourceCodeRange loc, size_t drop_from_start = 0, size_t drop_from_end = 0,
                              bool needs_escaping = false) {
        temp_buffer.clear();
        auto start = source + loc.begin.absolute + drop_from_start;
        const auto end = source + loc.end.absolute - drop_from_end;
        const size_t sz = end - start;
        if (!needs_escaping) {
            temp_buffer.insert(temp_buffer.begin(), start, end);
            return getsym(temp_buffer.c_str());
        }
        bool escaped = false;
        auto from_it = start;
        for (; from_it != end; from_it += 1) {
            if (*from_it == '\\' && !escaped) {
                escaped = true;
                // don't write
                continue;
            }

            // Bit odd, we actually only use the escape character to escape the delimiter.
            temp_buffer.push_back(*from_it);
            escaped = false;
        }
        return getsym(temp_buffer.c_str());
    }

    template <TokenType T> constexpr auto get_closing_brackets() -> decltype(auto) {
        if constexpr (T == ')'_tokentype)
            return std::array<TokenType, 1> { '('_tokentype };
        else if constexpr (T == ']'_tokentype)
            return std::array<TokenType, 1> { '['_tokentype };
        else if constexpr (T == '}'_tokentype)
            return std::array<TokenType, 2> { '{'_tokentype, TokenType::BeginClosedFunction };
        else {
            assert(false);
            return std::array<TokenType, 1> { TokenType::BadToken };
        }
    }
};


struct GlobalBisonLexerState {
    GlobalBisonLexerState(BisonSemAction a, lex::CodePointStream s): action(std::move(a)), char_stream(std::move(s)) {}
    BisonSemAction action;
    lex::CodePointStream char_stream;
    std::optional<BisonSemAction::Output> cached {};
};

std::optional<GlobalBisonLexerState> global_bison_lexer_state {};

// Must also advance through global state.
// Requires having consumed the opening bracket.
bool scanForClosingBracket(char to_find) {
    assert(global_bison_lexer_state);
    GlobalBisonLexerState& s = *global_bison_lexer_state;
    const auto tok = static_cast<TokenType>(to_find);
    assert(!s.action.bracket_stack.empty());
    const auto target_depth = s.action.bracket_stack.size() - 1; // we have just pushed a bracket.

    const auto mutate_global_state_for_return = [&](const BisonSemAction::Output& o) -> int {
        if (o.slot) {
            zzval = (intptr_t)newPyrSlotNode(*o.slot);
        }

        parseFailed = o.is_error ? 1 : 0;

        // Yes it reads from the end point only. Very odd. Causes many issues.
        textpos = o.range.end.absolute;
        lineno = o.range.end.lineNumber + 1; // zero indexed to 1
        linepos = s.char_stream.line_start(o.range.end);
        charno = o.range.end.offsetInLine;
        if (maxlinestarts < o.range.end.lineNumber) {
            maxlinestarts += maxlinestarts;
            linestarts = (int*)pyr_pool_compile->Realloc(linestarts, maxlinestarts * sizeof(int*));
        }
        linestarts[lineno] = linepos;

        return *convert_to_bison_tokentype(o.type);
    };
    BisonSemAction::Output out;
    while (true) {
        out = lex::lexer(s.char_stream, s.action);

        if (out.type == TokenType::End || out.is_error) {
            mutate_global_state_for_return(out); // must do this!
            return false;
        }
        if (out.type == tok && s.action.bracket_stack.size() == target_depth) {
            mutate_global_state_for_return(out); // must do this!
            return true;
        }
    }
}

void scan_for_end() {
    assert(global_bison_lexer_state);
    GlobalBisonLexerState& s = *global_bison_lexer_state;

    BisonSemAction::Output out;
    do {
        out = lex::lexer(s.char_stream, s.action);
    } while (out.type != TokenType::End && out.type != TokenType::BadToken && !out.is_error);

    if (out.slot) {
        zzval = (intptr_t)newPyrSlotNode(*out.slot);
    }

    parseFailed = out.is_error ? 1 : 0;

    // Yes it reads from the end point only. Very odd. Causes many issues.
    textpos = out.range.end.absolute;
    lineno = out.range.end.lineNumber + 1; // zero indexed to 1
    linepos = s.char_stream.line_start(out.range.end);
    charno = out.range.end.offsetInLine;
    if (maxlinestarts < out.range.end.lineNumber) {
        maxlinestarts += maxlinestarts;
        linestarts = (int*)pyr_pool_compile->Realloc(linestarts, maxlinestarts * sizeof(int*));
    }
    linestarts[lineno] = linepos;
}


int yylex() {
    assert(global_bison_lexer_state);
    GlobalBisonLexerState& s = *global_bison_lexer_state;

    const auto mutate_global_state_for_return = [&](const BisonSemAction::Output& o) -> int {
        if (o.slot) {
            zzval = (intptr_t)newPyrSlotNode(*o.slot);
        }

        parseFailed = o.is_error ? 1 : 0;

        // Yes it reads from the end point only. Very odd. Causes many issues.
        textpos = o.range.end.absolute;
        lineno = o.range.end.lineNumber + 1; // zero indexed to 1
        linepos = s.char_stream.line_start(o.range.end);
        charno = o.range.end.offsetInLine;
        if (maxlinestarts < o.range.end.lineNumber) {
            maxlinestarts += maxlinestarts;
            linestarts = (int*)pyr_pool_compile->Realloc(linestarts, maxlinestarts * sizeof(int*));
        }
        linestarts[lineno] = linepos;

        return *convert_to_bison_tokentype(o.type);
    };

    // If we have a cached out return it.
    if (s.cached) {
        const auto o = std::move(*s.cached);
        s.cached.reset();
        return mutate_global_state_for_return(o);
    }

    BisonSemAction::Output out = lex::lexer(s.char_stream, s.action);

    if (out.type == TokenType::Interpret) {
        lexCmdLine = 2;
    }

    if (out.type == TokenType::StringLine) {
        std::string str {};
        str.reserve(128);

        auto prev = out;
        while (true) {
            if (out.type != TokenType::StringLine) {
                assert(!s.cached.has_value());
                s.cached = std::move(out); // save for next time.

                // This is the one case in the whole lexer where we currently have to alloc using the GC.
                // This would be much better pushed into the compiler.
                const int flags = compilingCmdLine ? obj_immutable : obj_permanent | obj_immutable;
                auto sc_str = newPyrString(gMainVMGlobals->gc, str.c_str(), flags, false);
                zzval = (intptr_t)newPyrSlotNode(PyrSlot::make(sc_str));
                parseFailed = prev.is_error ? 1 : 0;

                // Yes it reads from the end point only. Very odd. Causes many issues.
                textpos = prev.range.end.absolute;
                lineno = prev.range.end.lineNumber + 1; // zero indexed to 1
                linepos = s.char_stream.line_start(prev.range.end);
                charno = prev.range.end.offsetInLine;
                if (maxlinestarts < prev.range.end.lineNumber) {
                    maxlinestarts += maxlinestarts;
                    linestarts = (int*)pyr_pool_compile->Realloc(linestarts, maxlinestarts * sizeof(int*));
                }
                linestarts[lineno] = linepos;
                return STRING;
            }
            auto range = out.range;
            // This is dodgy, we are dropping the quotes here.
            // Again, once this is in the compilation phase, this becomes nice.
            range.begin.absolute += 1;
            range.end.absolute -= 1;

            bool escaped = false;
            for (auto [b, e] = s.char_stream.source_range(range); b < e; ++b) {
                if (*b == '\\' && !escaped) {
                    escaped = true;
                    continue;
                }

                if (escaped) {
                    if (*b == 'n')
                        str += '\n';
                    else if (*b == 'r')
                        str += '\r';
                    else if (*b == 't')
                        str += '\t';
                    else if (*b == 'f')
                        str += '\f';
                    else if (*b == 'v')
                        str += '\v';
                    else
                        str += *b;
                    escaped = false;
                } else {
                    str += *b;
                }
            }

            prev = out;
            out = lex::lexer(s.char_stream, s.action);
        }
    }

    return mutate_global_state_for_return(out);
}

PyrSlot process_accidental_cents(const char* s) {
    const char* c = s;
    double degree = 0.;
    double cents = 0.;
    double centsdiv = 1000.;
    while (*c) {
        if (*c >= '0' && *c <= '9')
            degree = degree * 10. + *c - '0';
        else
            break;
        c++;
    }

    if (*c == 'b')
        centsdiv = -1000.;
    else if (*c == 's')
        centsdiv = 1000.;
    c++;

    while (*c) {
        if (*c >= '0' && *c <= '9') {
            cents = cents * 10. + *c - '0';
        } else
            break;
        c++;
    }

    if (cents > 499.)
        cents = 499.;

    return PyrSlot::make(degree + cents / centsdiv);
}

PyrSlot process_accidental_steps(const char* s) {
    const char* c = s;
    double degree = 0.;
    double semitones = 0.;
    while (*c) {
        if (*c >= '0' && *c <= '9')
            degree = degree * 10. + *c - '0';
        else
            break;
        c++;
    }

    while (*c) {
        if (*c == 'b')
            semitones -= 1.;
        else if (*c == 's')
            semitones += 1.;
        c++;
    }

    if (semitones > 4.)
        semitones = 4.;
    else if (semitones < -4.)
        semitones = -4.;

    return PyrSlot::make(degree + semitones / 10.);
}

void yyerror(const char* s) {
    parseFailed = 1;
    error("%s\n", s);
    postErrorLine(lineno, linepos, charno);
    // Debugger();
}

void fatal() {
    parseFailed = 1;
    error("Parse error\n");
    postErrorLine(lineno, linepos, charno);
    // Debugger();
}

void postErrorLine(int linenum, int start, int charpos) {
    int i, j, end, pos;
    char str[256];

    // post("start %d\n", start);
    // parseFailed = true;
    post("  in %s\n", printingCurrfilename.c_str());
    post("  line %d char %d:\n\n", linenum + errLineOffset, charpos);
    // nice: postfl previous line for context

    // postfl("text '%s' %d\n", text, text);

    // postfl error line for context
    pos = start + charpos;
    for (i = pos; i < textlen; ++i) {
        if (text[i] == 0 || text[i] == '\r' || text[i] == '\n')
            break;
    }
    end = i;
    for (i = start, j = 0; i < end && j < 255; ++i) {
        str[j++] = text[i];
    }
    str[j] = 0;

    i = end + 1;
    if (i < textlen) {
        // postfl following line for context
        for (j = 0; j < 255 && i < textlen; ++i) {
            if (text[i] == 0 || text[i] == '\r' || text[i] == '\n')
                break;
            str[j++] = text[i];
        }
        str[j] = 0;
        post("  %s\n", str);
    }
    post("-----------------------------------\n", str);
}

void pstrncpy(unsigned char* s1, unsigned char* s2, int n);
void pstrncpy(unsigned char* s1, unsigned char* s2, int n) {
    int i, m;
    m = *s2++;
    n = (n < m) ? n : m;
    *s1 = n;
    s1++;
    for (i = 0; i < n; ++i) {
        *s1 = *s2;
        s1++;
        s2++;
    }
}

int pstrcmp(unsigned char* s1, unsigned char* s2);
int pstrcmp(unsigned char* s1, unsigned char* s2) {
    int i, len1, len2, len;
    len1 = *s1++;
    len2 = *s2++;
    len = sc_min(len1, len2);
    for (i = 0; i < len; ++i) {
        if (s1[i] < s2[i])
            return -1;
        if (s1[i] > s2[i])
            return 1;
    }
    if (len1 < len2)
        return -1;
    if (len1 > len2)
        return 1;
    return 0;
}


int numClassDeps;
static ClassExtFile* sClassExtFiles;
static ClassExtFile* eClassExtFiles;

ClassExtFile* newClassExtFile(PyrSymbol* fileSym, int startPos, int endPos);
ClassExtFile* newClassExtFile(PyrSymbol* fileSym, int startPos, int endPos) {
    ClassExtFile* classext;
    classext = (ClassExtFile*)pyr_pool_compile->Alloc(sizeof(ClassExtFile));
    MEMFAIL(classext);
    classext->fileSym = fileSym;
    classext->next = nullptr;
    classext->startPos = startPos;
    classext->endPos = endPos;
    if (!sClassExtFiles)
        sClassExtFiles = classext;
    else
        eClassExtFiles->next = classext;
    eClassExtFiles = classext;
    return classext;
}


ClassDependancy* newClassDependancy(PyrSymbol* className, PyrSymbol* superClassName, PyrSymbol* fileSym, int startPos,
                                    int endPos, int lineOffset) {
    ClassDependancy* classdep;

    // post("classdep '%s' '%s' '%s' %d %d\n", className->name, superClassName->name,
    //	fileSym->name, className, superClassName);
    // pyrmalloc:
    // lifetime: kill after compile.
    numClassDeps++;
    if (className->classdep) {
        error("duplicate Class found: '%s' \n", className->name);
        post("%s\n", className->classdep->fileSym->name);
        postfl("%s\n\n", fileSym->name);
        return className->classdep;
    }
    classdep = (ClassDependancy*)pyr_pool_compile->Alloc(sizeof(ClassDependancy));
    MEMFAIL(classdep);
    classdep->className = className;
    classdep->superClassName = superClassName;
    classdep->fileSym = fileSym;
    classdep->superClassDep = nullptr;
    classdep->next = nullptr;
    classdep->subclasses = nullptr;

    classdep->startPos = startPos;
    classdep->endPos = endPos;
    classdep->lineOffset = lineOffset;

    className->classdep = classdep;
    return classdep;
}

void buildDepTree() {
    ClassDependancy* next;
    SymbolTable* symbolTable = gMainVMGlobals->symbolTable;

    // postfl("->buildDepTree\n"); fflush(stdout);
    for (int i = 0; i < symbolTable->TableSize(); ++i) {
        PyrSymbol* sym = symbolTable->Get(i);
        if (sym && (sym->flags & sym_Class)) {
            if (sym->classdep) {
                if (sym->classdep->superClassName->classdep) {
                    next = sym->classdep->superClassName->classdep->subclasses;
                    sym->classdep->superClassName->classdep->subclasses = sym->classdep;
                    sym->classdep->next = next;
                } else if (sym->classdep->superClassName != s_none) {
                    error("Superclass '%s' of class '%s' is not defined in any file.\n%s\n",
                          sym->classdep->superClassName->name, sym->classdep->className->name,
                          sym->classdep->fileSym->name);
                }
            }
        }
    }
    // postfl("<-buildDepTree\n"); fflush(stdout);
}

extern PyrClass* gClassList;

ClassDependancy** gClassCompileOrder;
int gClassCompileOrderNum = 0;
int gClassCompileOrderSize = 1000;

void compileDepTree();

void traverseFullDepTree() {
    // postfl("->traverseFullDepTree\n"); fflush(stdout);
    gClassCompileOrderNum = 0;
    gClassCompileOrder = (ClassDependancy**)pyr_pool_compile->Alloc(gClassCompileOrderSize * sizeof(ClassDependancy));
    MEMFAIL(gClassCompileOrder);

    // parse and compile all files
    initParser(); // sets compiler errors to 0
    gParserResult = -1;

    traverseDepTree(s_object->classdep, 0);
    compileDepTree(); // compiles backwards using the order defined in gClassCompileOrder
    compileClassExtensions();

    pyr_pool_compile->Free(gClassCompileOrder);

    finiParser();
    // postfl("<-traverseFullDepTree\n"); fflush(stdout);
}


void traverseDepTree(ClassDependancy* classdep, int level) {
    ClassDependancy* subclassdep;

    if (!classdep)
        return;

    subclassdep = classdep->subclasses;
    for (; subclassdep; subclassdep = subclassdep->next) {
        traverseDepTree(subclassdep, level + 1);
    }
    if (gClassCompileOrderNum > gClassCompileOrderSize) {
        gClassCompileOrderSize *= 2;
        gClassCompileOrder = (ClassDependancy**)pyr_pool_compile->Realloc(
            gClassCompileOrder, gClassCompileOrderSize * sizeof(ClassDependancy));
        MEMFAIL(gClassCompileOrder);
    }

    /*	postfl("traverse level:%d, gClassCompileOrderNum:%d, '%s' '%s' '%s'\n", level, gClassCompileOrderNum,
       classdep->className->name, classdep->superClassName->name, classdep->fileSym->name); fflush(stdout);
    */

    gClassCompileOrder[gClassCompileOrderNum++] = classdep;
}


void compileClass(PyrSymbol* fileSym, int startPos, int endPos, int lineOffset) {
    // fprintf(stderr, "compileClass: %d\n", fileSym->u.index);

    gCompilingFileSym = fileSym;
    gCompilingVMGlobals = nullptr;
    gRootParseNode = nullptr;
    initParserPool();
    if (startLexer(fileSym, fs::path(), startPos, endPos, lineOffset)) {
        // postfl("->Parsing %s\n", fileSym->name); fflush(stdout);
        parseFailed = yyparse();
        // postfl("<-Parsing %s %d\n", fileSym->name, parseFailed); fflush(stdout);
        // post("parseFailed %d\n", parseFailed); fflush(stdout);
        if (!parseFailed && gRootParseNode) {
            // postfl("Compiling nodes %p\n", gRootParseNode);fflush(stdout);
            compilingCmdLine = false;
            compileNodeList(gRootParseNode, true);
            // postfl("done compiling\n");fflush(stdout);
        } else {
            compileErrors++;
            fs::path pathname(fileSym->name);
            error("file '%s' parse failed\n", SC_Codecvt::path_to_utf8_str(pathname).c_str());
            postfl("error parsing\n");
        }
        finiLexer();
    } else {
        error("file '%s' open failed\n", fileSym->name);
    }
    freeParserPool();
}

void compileDepTree() {
    ClassDependancy* classdep;
    int i;

    for (i = gClassCompileOrderNum - 1; i >= 0; --i) {
        classdep = gClassCompileOrder[i];
        /*postfl("compile %d '%s' '%s' '%s'...%d/%d/%d\n", i, classdep->className->name, classdep->superClassName->name,
            classdep->fileSym->name, classdep->startLine, classdep->endLine, classDep->lineOffset);*/
        compileClass(classdep->fileSym, classdep->startPos, classdep->endPos, classdep->lineOffset);
    }
    // postfl("<compile\n");
}

void compileClassExtensions() {
    if (sClassExtFiles) {
        ClassExtFile* classext = sClassExtFiles;
        do {
            // postfl("compile class ext: %d/%d\n", classext->startPos, classext->endPos);
            compileClass(classext->fileSym, classext->startPos, classext->endPos, -1);
            classext = classext->next;
        } while (classext);
    }
}

void findDiscrepancy();

void traverseFullDepTree2() {
    // assign a class index to all classes
    if (!parseFailed && !compileErrors) {
        buildClassTree();
        gNumClasses = 0;

        // now I index them during pass one
        indexClassTree(class_object, 0);
        setSelectorFlags();
        if (2 * numClassDeps != gNumClasses) {
            error("There is a discrepancy.\n");
            /* not always correct
                    if(2*numClassDeps < gNumClasses) {
                        post("Duplicate files may exist in the directory structure.\n");
                    } else {
                        post("Some class files may be missing.\n");
                    }
                    */
            post("numClassDeps %d   gNumClasses %d\n", numClassDeps, gNumClasses);
            findDiscrepancy();
            compileErrors++;
        } else {
            double elapsed;
            buildBigMethodMatrix();
            SymbolTable* symbolTable = gMainVMGlobals->symbolTable;
            post("\tNumber of Symbols %d\n", symbolTable->NumItems());
            post("\tByte Code Size %d\n", totalByteCodes);
            // elapsed = TickCount() - compileStartTime;
            // elapsed = 0;
            elapsed = elapsedTime() - compileStartTime;
            post("\tcompiled %d files in %.2f seconds\n", gNumCompiledFiles, elapsed);
            if (numOverwrites == 1) {
                post("\nInfo: One method is currently overwritten by an extension. To see which, "
                     "execute:\nMethodOverride.printAll\n\n");
            } else if (numOverwrites > 1) {
                post("\nInfo: %i methods are currently overwritten by extensions. To see which, "
                     "execute:\nMethodOverride.printAll\n\n",
                     numOverwrites);
            }
            post("compile done\n");
        }
    }
}

bool parseOneClass(PyrSymbol* fileSym) {
    int token;
    PyrSymbol *className, *superClassName;
    ClassDependancy* classdep;
    bool res;

    int startPos, startLineOffset;

    res = true;

    startPos = textpos;
    startLineOffset = lineno - 1;

    token = yylex();
    if (token == CLASSNAME) {
        className = slotRawSymbol(&((PyrSlotNode*)zzval)->mSlot);
        // I think this is wrong: zzval is space pool alloced
        // pyrfree((PyrSlot*)zzval);

        token = yylex();
        if (token == 0)
            return false;
        if (token == '[') {
            scanForClosingBracket(']'); // eat indexing spec
            token = yylex();
            if (token == 0)
                return false;
        }
        if (token == ':') {
            token = yylex(); // get super class
            if (token == 0)
                return false;
            if (token == CLASSNAME) {
                superClassName = slotRawSymbol(&((PyrSlotNode*)zzval)->mSlot);
                // I think this is wrong: zzval is space pool alloced
                // pyrfree((PyrSlot*)zzval);
                token = yylex();
                if (token == 0)
                    return false;
                if (token == '{') {
                    scanForClosingBracket('}'); // eat class body
                    classdep =
                        newClassDependancy(className, superClassName, fileSym, startPos, textpos, startLineOffset);
                } else {
                    compileErrors++;
                    postfl("Expected %c.  got token: %d\n", '{', token);
                    postErrorLine(lineno, linepos, charno);
                    return false;
                }
            } else {
                compileErrors++;
                post("Expected superclass name.  got token: ' %d\n", token);
                postErrorLine(lineno, linepos, charno);
                return false;
            }
        } else if (token == '{') {
            if (className == s_object)
                superClassName = s_none;
            else
                superClassName = s_object;
            scanForClosingBracket('}'); // eat class body
            classdep = newClassDependancy(className, superClassName, fileSym, startPos, textpos, startLineOffset);
        } else {
            compileErrors++;
            post("Expected ':' or %c.  got token: %d\n", '{', token);
            postErrorLine(lineno, linepos, charno);
            return false;
        }
    } else if (token == '+') {
        token = yylex();
        if (token == 0)
            return false;

        scan_for_end();

        newClassExtFile(fileSym, startPos, textpos);
        return false;
    } else {
        if (token != 0) {
            compileErrors++;
            post("Expected class name.  got token: %d\n", token);
            postErrorLine(lineno, linepos, charno);
            return false;
        } else {
            res = false;
        }
    }
    return res;
}

void initPassOne() {
    // dump_pool_histo(pyr_pool_runtime);
    pyr_pool_runtime->FreeAllInternal();
    // dump_pool_histo(pyr_pool_runtime);
    // gPermanentObjPool.Init(pyr_pool_runtime, PERMOBJCHUNK);
    sClassExtFiles = nullptr;

    void* ptr = pyr_pool_runtime->Alloc(sizeof(SymbolTable));
    MEMFAIL(ptr);
    gMainVMGlobals->symbolTable = new (ptr) SymbolTable(pyr_pool_runtime, 65536);

    initSymbols(); // initialize symbol globals
    initSpecialSelectors();
    initSpecialClasses();
    initClasses();
    initParserPool();
    initParseNodes();
    initPrimitives();

    initLexer();

    compileErrors = 0;
    numClassDeps = 0;
    compiledOK = false;
    compiledDirectories.clear();

    // main class library folder: only used for relative path resolution
    gCompileDir = SC_Filesystem::instance().getDirectory(DirName::Resource) / "SCClassLibrary";
}

void finiPassOne() {
    // postfl("->finiPassOne\n");
    freeParserPool();
    // postfl("<-finiPassOne\n");
}

/**
 * \brief \c true if \c dir is one of the language config's default classlib directories
 */
static bool isDefaultClassLibraryDirectory(const fs::path& dir) {
    auto const& defaultDirs = gLanguageConfig->defaultClassLibraryDirectories();
    auto const iter = std::find(defaultDirs.begin(), defaultDirs.end(), dir);
    return iter != defaultDirs.end();
}

/**
 * \brief Handles a missing directory encountered during compilation.
 *
 * If the directory is one of the default directories traversed during compilation,
 * try to create it, silently ignoring failure (most likely from permissions failure).
 * Otherwise, warn the user to help catch mistyped/missing directory names. See #3468.
 */
static void passOne_HandleMissingDirectory(const fs::path& dir) {
    if (isDefaultClassLibraryDirectory(dir)) {
        std::error_code ec {};
        fs::create_directories(dir, ec);
    } else {
        post("WARNING: Could not open directory: '%s'\n"
             "\tTo resolve this, either create the directory or remove it from your compilation paths.\n\n",
             SC_Codecvt::path_to_utf8_str(dir).c_str());
    }
}

fs::path relativeToCompileDir(const fs::path& p) { return fs::relative(p, gCompileDir); }

/** \brief Determines whether the directory should be skipped during compilation.
 *
 * \param dir : The directory to check, as a `path` object
 * \returns `true` iff any of the following conditions is true:
 * - the directory has already been compiled
 * - the language configuration says this path is excluded
 * - SC_Filesystem::shouldNotCompileDirectory(dir) returns `true`
 */
static bool passOne_ShouldSkipDirectory(const fs::path& dir) {
    return (compiledDirectories.find(dir) != compiledDirectories.end())
        || (gLanguageConfig && gLanguageConfig->pathIsExcluded(dir))
        || (SC_Filesystem::instance().shouldNotCompileDirectory(dir));
}

/** \brief Compile the contents of a single directory
 *
 * This method compiles any .sc files in a single directory, working
 * via depth-first recursion. This routine is designed to fail gracefully,
 * and only indicates failure if something truly unexpected happens. These
 * conditions are:
 * - an error occurred while trying to open a directory, other than the case
 *    the case that the object doesn't exist.
 * - an error occurred while calling `passOne_processOneFile` on a file
 * - an error occurred in a recursive call of this routine on a macOS alias
 * Otherwise, this method returns success, even if:
 * - `dir` does not exist
 * - Iterating to the next file fails for any reason at all
 *
 * This method returns with a success state immediately if the directory
 * should not be compiled according to the language configuration.
 *
 * \param dir : The directory to traverse, as a `path` object
 * \returns `true` if processing was successful, `false` if it failed.
 *   See above for what constitutes success and failure conditions.
 */
static bool passOne_ProcessDir(const fs::path& dir) {
    // Prefer non-throwing versions of filesystem functions, since they are actually not unexpected
    // and because it's faster to use error codes.
    std::error_code ec;

    // Perform tilde expansion on incoming dir.
    const fs::path expdir = SC_Filesystem::instance().expandTilde(dir);

    // Using a recursive_directory_iterator is much faster than actually calling this function
    // recursively. Speedup from the switch was about 1.5x. _Do_ recurse on symlinks.
    fs::recursive_directory_iterator rditer(expdir, fs::directory_options::follow_directory_symlink, ec);

    // Check preconditions: are we able to access the file, and should we compile it according to
    // the language configuration?
    if (ec) {
        // If we got an error, post a warning if it was because the target wasn't found, and return success.
        // Otherwise, post the error and fail.
        if (ec.default_error_condition() == std::errc::no_such_file_or_directory) {
            passOne_HandleMissingDirectory(expdir);
            return true;
        } else {
            error("Could not open directory '%s': (%d) %s\n", SC_Codecvt::path_to_utf8_str(expdir).c_str(), ec.value(),
                  ec.message().c_str());

            return false;
        }
    } else if (passOne_ShouldSkipDirectory(expdir)) {
        // If we should skip the directory, just return success now.
        return true;
    } else {
        // Let the user know we are in fact compiling this directory.
        post("\tCompiling directory '%s'\n", SC_Codecvt::path_to_utf8_str(expdir).c_str());
    }

    // Record that we have touched this directory already.
    compiledDirectories.insert(expdir);

    // Invariant: we have processed (or begun to process) every directory or file already
    // touched by the iterator.
    while (rditer != fs::end(rditer)) {
        const fs::path path = *rditer;

        // If the file is a directory, perform the same checks as above to see if we should
        // skip compilation on it.
        if (fs::is_directory(path)) {
            if (passOne_ShouldSkipDirectory(path)) {
                rditer.disable_recursion_pending(); // don't "push" into the next level of the hierarchy
            } else {
                // Mark this directory as compiled.
                // By not calling no_push(), we allow the iterator to enter the directory
                compiledDirectories.insert(path);
            }

        } else { // ordinary file
            // Try to resolve a potential alias. Possible outcomes:
            // - it was an alias & is also a directory: try to recurse on it
            // - resolution failed: returns empty path: let the user know
            // - it was not an alias, or was an alias that wasn't a directory: try to process it as a source file
            bool isAlias = false;
            const fs::path& respath = SC_Filesystem::resolveIfAlias(path, isAlias);
            if (isAlias && fs::is_directory(respath)) {
                // If the resolved alias is a directory, recurse on it.
                if (!passOne_ProcessDir(respath)) {
                    return false;
                }
            } else if (respath.empty()) {
                error("Could not resolve symlink: %s\n", SC_Codecvt::path_to_utf8_str(path).c_str());
            } else if (!passOne_ProcessOneFile(respath)) {
                return false;
            }
        }

        // Error-code version of `++`
        rditer.increment(ec);
        if (ec) {
            // If iteration failed, allow compilation to continue, but bail out of this directory.
            error("Could not iterate on '%s': %s\n", SC_Codecvt::path_to_utf8_str(path).c_str(), ec.message().c_str());
            return true;
        }
    }
    return true;
}

bool passOne() {
    initPassOne();
    bool success = gLanguageConfig->forEachIncludedDirectory(passOne_ProcessDir);
    finiPassOne();

    return success;
}

/// True if file doesn't begin with '.', and ends with either '.sc' or '.rtf'
bool isValidSourceFileName(const fs::path& path) {
    const fs::path& ext = path.extension();
    return path.filename().c_str()[0] != '.' && // must not be hidden file
        ((ext == ".sc") || (ext == ".rtf" && path.stem().extension() == ".sc"));
}

/** \brief Attempt to parse a single SuperCollider source file
 *
 * Parsing is aborted if the file doesn't have a valid source file name,
 * or if the file can't be opened.
 * (Sekhar's replacement)
 *
 * \returns Whether parsing was successful. The only failure condition occurs
 * when the file can't be opened.
 */
bool passOne_ProcessOneFile(const fs::path& path) {
    bool success = true;

    const std::string path_str = SC_Codecvt::path_to_utf8_str(path);
    const char* path_c_str = path_str.c_str();
    if (gLanguageConfig && gLanguageConfig->pathIsExcluded(path)) {
        post("\texcluding file: '%s'\n", path_c_str);
        return success;
    }

    if (isValidSourceFileName(path)) {
        gNumCompiledFiles++;
        PyrSymbol* fileSym = getsym(path_c_str);
        fileSym->u.source = nullptr;
        if (startLexer(fileSym, path, -1, -1, -1)) {
            while (parseOneClass(fileSym)) {};
            finiLexer();
        } else {
            error("file '%s' open failed\n", path_c_str);
            success = false;
        }
    } else {
        // wasn't a valid source file; ignore
    }
    return success;
}

void schedRun();

void compileSucceeded();
void compileSucceeded() {
    compiledOK = !(parseFailed || compileErrors);
    if (compiledOK) {
        compiledOK = true;

        compiledOK = initRuntime(gMainVMGlobals, 128 * 1024, pyr_pool_runtime);

        if (compiledOK) {
            VMGlobals* g = gMainVMGlobals;

            g->canCallOS = true;

            ++g->sp;
            SetObject(g->sp, g->process);
            runInterpreter(g, s_startup, 1);
            g->canCallOS = false;

            schedRun();
        }
        flushPostBuf();
    }
}

static void runShutdown() {
    // printf("->aboutToCompileLibrary\n");
    gLangMutex.lock();
    if (compiledOK) {
        VMGlobals* g = gMainVMGlobals;

        g->canCallOS = true;

        ++g->sp;
        SetObject(g->sp, g->process);
        runInterpreter(g, s_shutdown, 1);

        g->canCallOS = false;
    }
    gLangMutex.unlock();
    // printf("<-aboutToCompileLibrary\n");
}

void closeAllGUIScreens();
void TempoClock_stopAll(void);
void closeAllCustomPorts();

void shutdownLibrary() {
    closeAllGUIScreens();

    schedStop();

    runShutdown();

    TempoClock_stopAll();

    gLangMutex.lock();
    closeAllCustomPorts();

    if (compiledOK) {
        VMGlobals* g = gMainVMGlobals;
        g->canCallOS = true;
        g->gc->RunAllFinalizers();
        g->canCallOS = false;
    }

    pyr_pool_runtime->FreeAll();

    compiledOK = false;

    gLangMutex.unlock();
    deinitPrimitives();
}

SCLANG_DLLEXPORT_C bool compileLibrary(bool standalone) {
    // printf("->compileLibrary\n");
    shutdownLibrary();

    gLangMutex.lock();
    gNumCompiledFiles = 0;
    compiledOK = false;

    if (!gLanguageConfig) {
        SC_LanguageConfig::readLibraryConfig(standalone);
    }

    compileStartTime = elapsedTime();

    totalByteCodes = 0;

#ifdef NDEBUG
    postfl("compiling class library...\n");
#else
    postfl("compiling class library (debug build)...\n");
#endif

    bool res = passOne();
    if (res) {
        if (!compileErrors) {
            buildDepTree();
            traverseFullDepTree();
            traverseFullDepTree2();
            flushPostBuf();

            if (!compileErrors && gShowWarnings) {
                SymbolTable* symbolTable = gMainVMGlobals->symbolTable;
                symbolTable->CheckSymbols();
            }
        }
        pyr_pool_compile->FreeAll();
        flushPostBuf();
        compileSucceeded();
    } else {
        compiledOK = false;
    }

    gLangMutex.unlock();
    // printf("<-compileLibrary\n");
    return compiledOK;
}

void dumpByteCodes(PyrBlock* theBlock);

SCLANG_DLLEXPORT_C void runLibrary(PyrSymbol* selector) {
    VMGlobals* g = gMainVMGlobals;
    g->canCallOS = true;
    try {
        if (compiledOK) {
            ++g->sp;
            SetObject(g->sp, g->process);
            runInterpreter(g, selector, 1);
        } else {
            postfl("Library has not been compiled successfully.\n");
        }
    } catch (const FatalInterpreterError& er) {
        error("A fatal interpreter error has occured. Reason: %s\n", er.what());
        throw;
    } catch (const std::exception& ex) {
        PyrMethod* meth = g->method;
        if (meth) {
            int ip = slotRawInt8Array(&meth->code) ? g->ip - slotRawInt8Array(&meth->code)->b : -1;
            post("caught exception in runLibrary %s:%s %3d\n",
                 slotRawSymbol(&slotRawClass(&meth->ownerclass)->name)->name, slotRawSymbol(&meth->name)->name, ip);
            dumpByteCodes(meth);
        } else {
            post("caught exception in runLibrary\n");
        }
        error(ex.what());
    } catch (...) { postfl("DANGER: OUT of MEMORY. Operation failed.\n"); }
    g->canCallOS = false;
}

bool startLexer(PyrSymbol* fileSym, const fs::path& p, int startPos, int endPos, int lineOffset) {
    const char* filename = fileSym->name;

    textlen = -1;

    if (!fileSym->u.source) {
        try {
            std::ifstream file;
            file.exceptions(std::ifstream::failbit | std::ifstream::badbit);
            file.open(p, std::ios_base::binary);
            size_t sz = fs::file_size(p);

            text = (char*)pyr_pool_compile->Alloc((sz + 1) * sizeof(char));
            MEMFAIL(text);
            file.read(text, sz);
            text[sz] = '\0';
            fileSym->u.source = text;
            rtf2txt(text);
        } catch (const std::exception& ex) {
            error("Could not read %s: %s.\n", SC_Codecvt::path_to_utf8_str(p).c_str(), ex.what());
            return false;
        }
    } else
        text = fileSym->u.source;

    if ((startPos >= 0) && (endPos > 0)) {
        textlen = endPos - startPos;
        text += startPos;
    } else if (textlen == -1)
        textlen = strlen(text);

    if (lineOffset > 0)
        errLineOffset = lineOffset;
    else
        errLineOffset = 0;

    if (startPos > 0)
        errCharPosOffset = startPos;
    else
        errCharPosOffset = 0;

    initLongStack(&closedFuncCharNo);
    initLongStack(&generatorStack);
    lastClosedFuncCharNo = 0;
    textpos = 0;
    linepos = 0;
    lineno = 1;
    charno = 0;

    zzval = 0;
    parseFailed = 0;
    lexCmdLine = 0;
    currfilename = fs::path(filename);
    printingCurrfilename = "file '" + SC_Codecvt::path_to_utf8_str(currfilename) + "'";
    maxlinestarts = 1000;
    linestarts = (int*)pyr_pool_compile->Alloc(maxlinestarts * sizeof(int*));
    MEMFAIL(linestarts);
    linestarts[0] = 0;
    linestarts[1] = 0;
    compilingCmdLine = false;

    global_bison_lexer_state.emplace(std::move(BisonSemAction { text }),
                                     std::move(lex::CodePointStream { true, text, static_cast<size_t>(textlen), {} }));

    return true;
}

void start_lexer_for_testing_class_lib(PyrSymbol* file_name_with_src) {
    text = file_name_with_src->u.source;

    textlen = strlen(text);

    initLongStack(&closedFuncCharNo);
    initLongStack(&generatorStack);
    lastClosedFuncCharNo = 0;
    textpos = 0;
    linepos = 0;
    lineno = 1;
    charno = 0;
    errLineOffset = 0;
    errCharPosOffset = 0;

    parseFailed = 0;
    lexCmdLine = 0;
    currfilename = fs::path();
    printingCurrfilename = "file '" + SC_Codecvt::path_to_utf8_str(currfilename) + "'";
    maxlinestarts = 1000;
    linestarts = (int*)pyr_pool_compile->Alloc(maxlinestarts * sizeof(int*));
    MEMFAIL(linestarts);
    linestarts[0] = 0;
    linestarts[1] = 0;

    global_bison_lexer_state.emplace(std::move(BisonSemAction { text }),
                                     std::move(lex::CodePointStream { true, text, static_cast<size_t>(textlen), {} }));
}

void startLexerCmdLine(char* textbuf, int textbuflen) {
    // pyrmalloc:
    // lifetime: kill after compile. (this one gets killed anyway)
    text = (char*)pyr_pool_compile->Alloc((textbuflen + 2) * sizeof(char));
    MEMFAIL(text);
    memcpy(text, textbuf, textbuflen);
    text[textbuflen] = ' ';
    text[textbuflen + 1] = 0;
    textlen = textbuflen + 1;

    rtf2txt(text);

    initLongStack(&closedFuncCharNo);
    initLongStack(&generatorStack);
    lastClosedFuncCharNo = 0;
    textpos = 0;
    linepos = 0;
    lineno = 1;
    charno = 0;

    compilingCmdLine = true;
    zzval = 0;
    parseFailed = 0;
    lexCmdLine = 1;
    currfilename = fs::path("interpreted text");
    printingCurrfilename = currfilename.string();
    maxlinestarts = 1000;
    linestarts = (int*)pyr_pool_compile->Alloc(maxlinestarts * sizeof(int*));
    MEMFAIL(linestarts);
    linestarts[0] = 0;
    linestarts[1] = 0;

    errLineOffset = 0;
    errCharPosOffset = 0;

    global_bison_lexer_state.emplace(std::move(BisonSemAction { text }),
                                     std::move(lex::CodePointStream { false, text, static_cast<size_t>(textlen), {} }));
}

void finiLexer() {
    global_bison_lexer_state.reset();
    pyr_pool_compile->Free(linestarts);
    freeLongStack(&closedFuncCharNo);
    freeLongStack(&generatorStack);
}
