#pragma once
#include <cmath>
#include <cstring>
#include <variant>
#include "lexer.hpp"
#include <iostream>

const char* binopchars = "!@%&*-+=|<>?/";
enum yytokentype {
    YYEMPTY = -2,
    YYEOF = 0, /* "end of file"  */
    YYerror = 256, /* error  */
    YYUNDEF = 257, /* "invalid token"  */
    NAME = 258, /* NAME  */
    INTEGER = 259, /* INTEGER  */
    SC_FLOAT = 260, /* SC_FLOAT  */
    ACCIDENTAL = 261, /* ACCIDENTAL  */
    SYMBOL = 262, /* SYMBOL  */
    STRING = 263, /* STRING  */
    ASCII = 264, /* ASCII  */
    PRIMITIVENAME = 265, /* PRIMITIVENAME  */
    CLASSNAME = 266, /* CLASSNAME  */
    CURRYARG = 267, /* CURRYARG  */
    VAR = 268, /* VAR  */
    ARG = 269, /* ARG  */
    CLASSVAR = 270, /* CLASSVAR  */
    SC_CONST = 271, /* SC_CONST  */
    NILOBJ = 272, /* NILOBJ  */
    TRUEOBJ = 273, /* TRUEOBJ  */
    FALSEOBJ = 274, /* FALSEOBJ  */
    ELLIPSIS = 276, /* ELLIPSIS  */
    DOTDOT = 277, /* DOTDOT  */
    PIE = 278, /* PIE  */
    BEGINCLOSEDFUNC = 279, /* BEGINCLOSEDFUNC  */
    BADTOKEN = 280, /* BADTOKEN  */
    INTERPRET = 281, /* INTERPRET  */
    BEGINGENERATOR = 282, /* BEGINGENERATOR  */
    LEFTARROW = 283, /* LEFTARROW  */
    WHILE = 284, /* WHILE  */
    BINOP = 285, /* BINOP  */
    KEYBINOP = 286, /* KEYBINOP  */
    READWRITEVAR = 287, /* READWRITEVAR  */
    UMINUS = 288 /* UMINUS  */
};


using namespace sc::lex;
using namespace sc::lex::literals;

static constexpr size_t YYLEN_MAX { 999999 };

struct State {
    const char* source;
    size_t source_len;

    size_t token_start;
    size_t token_end;
    size_t text_iter; // current iter

    struct Nil {};
    using Output = std::variant<std::monostate, std::string, double, int, bool, Nil, char>;
    Output output;

    // Buffer
    char yytext[YYLEN_MAX];
    size_t yylen { 0 };
};


inline int input(State& s) {
    int c;
    if (s.text_iter >= s.source_len) {
        c = 0;
    } else {
        c = s.source[s.text_iter++];
    }
    s.yytext[s.yylen++] = c;
    return c;
}

inline int input0(State& s) {
    int c;
    if (s.text_iter >= s.source_len) {
        c = 0;
        s.text_iter++; // so unput will work properly
    } else {
        c = s.source[s.text_iter++];
    }
    return c;
}

inline void unput(State& s, int c) {
    if (s.text_iter > 0)
        s.text_iter--;
    if (c) {
        if (s.yylen)
            --s.yylen;
    }
}

inline void unput0(State& s, int c) {
    if (s.text_iter > 0)
        s.text_iter--;
}

static char OPENPAREN = '(';
static char OPENSQUAR = '[';
static char OPENCURLY = '{';
static char CLOSPAREN = ')';
static char CLOSSQUAR = ']';
static char CLOSCURLY = '}';


inline int processbinop(State& s, char* token, bool synth) {
    if (synth)
        s.output = std::string { token };
    if (strcmp(token, "<-") == 0)
        return LEFTARROW;
    if (strcmp(token, "<>") == 0)
        return READWRITEVAR;
    if (strcmp(token, "|") == 0)
        return '|';
    if (strcmp(token, "<") == 0)
        return '<';
    if (strcmp(token, ">") == 0)
        return '>';
    if (strcmp(token, "-") == 0)
        return '-';
    if (strcmp(token, "*") == 0)
        return '*';
    if (strcmp(token, "+") == 0)
        return '+';
    return BINOP;
}

inline int processkeywordbinop(State& s, char* token, bool synth) {
    token[strlen(token) - 1] = 0; // strip off colon
    if (synth)
        s.output = std::string { token };
    return KEYBINOP;
}

inline int processident(State& s, char* token, bool synth) {
    char c = token[0];

    if (token[0] == '_') {
        if (token[1] == 0) {
            return CURRYARG;
        } else {
            if (synth)
                s.output = std::string { token };
            return PRIMITIVENAME;
        }
    }
    if (token[0] >= 'A' && token[0] <= 'Z') {
        if (synth)
            s.output = std::string { token };
        return CLASSNAME;
    }
    if (strcmp("var", token) == 0)
        return VAR;
    if (strcmp("arg", token) == 0)
        return ARG;
    if (strcmp("classvar", token) == 0)
        return CLASSVAR;
    if (strcmp("const", token) == 0)
        return SC_CONST;

    if (strcmp("while", token) == 0) {
        if (synth)
            s.output = std::string { token };
        return WHILE;
    }
    if (strcmp("pi", token) == 0) {
        if (synth)
            s.output = 3.14;
        return PIE;
    }
    if (strcmp("true", token) == 0) {
        if (synth)
            s.output = true;
        return TRUEOBJ;
    }
    if (strcmp("false", token) == 0) {
        if (synth)
            s.output = false;
        return FALSEOBJ;
    }
    if (strcmp("nil", token) == 0) {
        if (synth)
            s.output = State::Nil {};
        return NILOBJ;
    }
    if (strcmp("inf", token) == 0) {
        if (synth)
            s.output = std::numeric_limits<double>::infinity();
        return SC_FLOAT;
    }

    if (synth)
        s.output = std::string { token };
    return NAME;
}

#if defined(__clang__)
__attribute__((no_sanitize("signed-integer-overflow")))
#endif
inline int
processhex(State& st, char* s, bool synth) {
    char* c;
    int val;
    c = s;
    val = 0;
    while (*c) {
        if (*c >= '0' && *c <= '9')
            val = val * 16 + *c - '0';
        else if (*c >= 'a' && *c <= 'z')
            val = val * 16 + *c - 'a' + 10;
        else if (*c >= 'A' && *c <= 'Z')
            val = val * 16 + *c - 'A' + 10;
        c++;
    }

    if (synth)
        st.output = val;
    return INTEGER;
}


#if defined(__clang__)
__attribute__((no_sanitize("signed-integer-overflow")))
#endif
inline int
sc_strtoi(const char* str, int n, int base) {
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

#if defined(__clang__)
__attribute__((no_sanitize("signed-integer-overflow")))
#endif
inline double
sc_strtof(const char* str, int n, int base) {
    double z = 0.;
    int decptpos = 0;
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
        else if (c == '.')
            decptpos = i;
    }
    // calculation previously included decimal point in count of columns (was n-decptpos); there are 1 less than n
    // characters which are columns in the number contribution
    z = z / std::pow((double)base, n - 1 - decptpos);
    return z;
}

inline int processintradix(State& st, char* s, int n, int radix, bool synth) {
    if (synth)
        st.output = sc_strtoi(s, n, radix);
    return INTEGER;
}

inline int processfloatradix(State& st, char* s, int n, int radix, bool synth) {
    if (synth)
        st.output = sc_strtof(s, n, radix);
    return SC_FLOAT;
}

inline int processint(State& st, char* s, bool synth) {
    if (synth)
        st.output = atoi(s);
    return INTEGER;
}

inline int processchar(State& st, int c, bool synth) {
    if (synth)
        st.output = c;
    return ASCII;
}

inline int processfloat(State& st, char* s, int sawpi, bool synth) {
    if (synth)
        st.output = sawpi ? atof(s) * 3.14 : atof(s);
    return SC_FLOAT;
}


#if defined(__clang__)
__attribute__((no_sanitize("signed-integer-overflow")))
#endif
inline int
processaccidental1(State& st, char* s, bool synth) {
    if (synth) {
        char* c;
        double degree = 0.;
        double cents = 0.;
        double centsdiv = 1000.;

        c = s;
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

        st.output = degree + cents / centsdiv;
    }
    return ACCIDENTAL;
}

#if defined(__clang__)
__attribute__((no_sanitize("signed-integer-overflow")))
#endif
inline int
processaccidental2(State& st, char* s, bool synth) {
    if (synth) {
        char* c;
        double degree = 0.;
        double semitones = 0.;
        c = s;
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

        st.output = degree + semitones / 10.;
    }
    return ACCIDENTAL;
}

inline int processsymbol(State& st, char* s, bool synth) {
    if (synth)
        st.output = std::string { s + 1 };
    return SYMBOL;
}

inline int processstring(State& st, char* s, bool synth) {
    if (synth)
        st.output = std::string { s + 1 };
    return STRING;
}

inline int old_lexer(State& s, bool synth) {
    int r, c, c2;
    intptr_t d;
    int radix;

    s.yylen = 0;
    // finite state machine to parse input stream into tokens

start:
    s.token_start = s.text_iter;
    c = input(s);

    if (c == 0) {
        r = 0;
        goto leave;
    } else if (c == ' ' || c == '\t' || c == '\n' || c == '\r' || c == '\v' || c == '\f') {
        s.yylen = 0;
        goto start;
    } else if ((c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z') || c == '_')
        goto ident;
    else if (c == '/') {
        c = input(s);
        if (c == '/')
            goto comment1;
        else if (c == '*')
            goto comment2;
        else {
            unput(s, c);
            goto binop;
        }
    } else if (c >= '0' && c <= '9')
        goto digits_1;
    else if (c == OPENPAREN || c == OPENSQUAR || c == OPENCURLY) {
        r = c;
        goto leave;
    } else if (c == CLOSSQUAR) {
        r = c;
        goto leave;
    } else if (c == CLOSPAREN) {
        r = c;
        goto leave;
    } else if (c == CLOSCURLY) {
        r = c;
        goto leave;
    } else if (c == '^') {
        r = c;
        goto leave;
    } else if (c == '~') {
        r = c;
        goto leave;
    } else if (c == ';') {
        r = c;
        goto leave;
    } else if (c == ':') {
        r = c;
        goto leave;
    } else if (c == '`') {
        r = c;
        goto leave;
    } else if (c == '\\')
        goto symbol1;
    else if (c == '\'')
        goto symbol3;
    else if (c == '"')
        goto string1;
    else if (c == '.') {
        if ((c = input(s)) == '.') {
            if ((c = input(s)) == '.') {
                r = ELLIPSIS;
                goto leave;
            } else {
                r = DOTDOT;
                unput(s, c);
                goto leave;
            }
        } else {
            unput(s, c);
            r = '.';
            goto leave;
        }

    } else if (c == '#') {
        if ((c = input(s)) == OPENCURLY) {
            r = BEGINCLOSEDFUNC;
        } else {
            unput(s, c);
            r = '#';
        }
        goto leave;
    } else if (c == '$') {
        c = input(s);
        if (c == '\\') {
            c = input(s);
            switch (c) {
            case 'n':
                c = '\n';
                break;
            case 'r':
                c = '\r';
                break;
            case 't':
                c = '\t';
                break;
            case 'f':
                c = '\f';
                break;
            case 'v':
                c = '\v';
                break;
            }
        }
        r = processchar(s, c, synth);
        goto leave;
    } else if (c == ',') {
        r = c;
        goto leave;
    } else if (c == '=') {
        c = input(s);
        if (strchr(binopchars, c))
            goto binop;
        else {
            unput(s, c);
            r = '=';
            goto leave;
        }
    } else if (strchr(binopchars, c))
        goto binop;
    else if (!(isprint(c) || isspace(c) || c == 0)) {
        s.yylen = 0;
        goto start;
    } else
        goto error1;

ident:
    c = input(s);

    if ((c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z') || c == '_' || (c >= '0' && c <= '9'))
        goto ident;
    else if (c == ':') {
        s.yytext[s.yylen] = 0;
        r = processkeywordbinop(s, s.yytext, synth);
        goto leave;
    } else {
        unput(s, c);
        s.yytext[s.yylen] = 0;
        r = processident(s, s.yytext, synth);
        goto leave;
    }

symbol1:
    c = input(s);

    if ((c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z') || c == '_')
        goto symbol2;
    else if (c >= '0' && c <= '9')
        goto symbol4;
    else {
        unput(s, c);
        s.yytext[s.yylen] = 0;
        r = processsymbol(s, s.yytext, synth);
        goto leave;
    }

symbol2:
    c = input(s);

    if ((c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z') || c == '_' || (c >= '0' && c <= '9'))
        goto symbol2;
    else {
        unput(s, c);
        s.yytext[s.yylen] = 0;
        r = processsymbol(s, s.yytext, synth);
        goto leave;
    }

symbol4:
    c = input(s);
    if (c >= '0' && c <= '9')
        goto symbol4;
    else {
        unput(s, c);
        s.yytext[s.yylen] = 0;
        r = processsymbol(s, s.yytext, synth);
        goto leave;
    }


binop:

    c = input(s);

    if (c == 0)
        goto binop2;
    if (strchr(binopchars, c))
        goto binop;
    else {
    binop2:
        unput(s, c);
        s.yytext[s.yylen] = 0;
        r = processbinop(s, s.yytext, synth);
        goto leave;
    }

radix_digits_1:

    c = input(s);
    if (c >= '0' && c <= '0' + std::min(10, radix) - 1)
        goto radix_digits_1;
    if (c >= 'a' && c <= 'a' + std::min(36, radix) - 11)
        goto radix_digits_1;
    if (c >= 'A' && c <= 'A' + std::min(36, radix) - 11)
        goto radix_digits_1;
    if (c == '.') {
        goto radix_digits_2;
    }
    unput(s, c);
    s.yytext[s.yylen] = 0;
    r = processintradix(s, s.yytext, s.yylen, radix, synth);
    goto leave;

radix_digits_2:

    c = input(s);
    if (c >= '0' && c <= '0' + std::min(10, radix) - 1)
        goto radix_digits_2;
    if (c >= 'A' && c <= 'A' + std::min(36, radix) - 11)
        goto radix_digits_2;
    // do not allow lower case after decimal point.
    unput(s, c);
    s.yytext[s.yylen] = 0;
    r = processfloatradix(s, s.yytext, s.yylen, radix, synth);
    goto leave;

hexdigits:

    c = input(s);
    if (c >= '0' && c <= '9')
        goto hexdigits;
    if (c >= 'a' && c <= 'f')
        goto hexdigits;
    if (c >= 'A' && c <= 'F')
        goto hexdigits;
    unput(s, c);
    s.yytext[s.yylen] = 0;
    r = processhex(s, s.yytext, synth);
    goto leave;

digits_1: /* number started with digits */

    c = input(s);

    if (c >= '0' && c <= '9')
        goto digits_1;
    else if (c == 'r') {
        radix = sc_strtoi(s.yytext, s.yylen - 1, 10);
        s.yylen = 0;
        goto radix_digits_1;
    } else if (c == 'e' || c == 'E')
        goto expon_1;
    else if (c == '.') {
        c2 = input(s);
        if (c2 >= '0' && c2 <= '9')
            goto digits_2;
        else {
            unput(s, c2);
            unput(s, c);
            s.yytext[s.yylen] = 0;
            r = processint(s, s.yytext, synth);
            goto leave;
        }
    } else if (c == 'b' || c == 's') {
        d = input(s);
        if (d >= '0' && d <= '9')
            goto accidental1;
        if (d == c)
            goto accidental2;
        goto accidental3;
    accidental1:
        d = input(s);
        if (d >= '0' && d <= '9')
            goto accidental1;
        unput(s, d);
        s.yytext[s.yylen] = 0;
        r = processaccidental1(s, s.yytext, synth);
        goto leave;
    accidental2:
        d = input(s);
        if (d == c)
            goto accidental2;
    accidental3:
        unput(s, d);
        s.yytext[s.yylen] = 0;
        r = processaccidental2(s, s.yytext, synth);
        goto leave;
    } else if (c == 'x') {
        s.yylen = 0;
        goto hexdigits;
    } else {
        unput(s, c);
        s.yytext[s.yylen] = 0;
        r = processint(s, s.yytext, synth);
        goto leave;
    }

digits_2:

    c = input(s);

    if (c >= '0' && c <= '9')
        goto digits_2;
    else if (c == 'e' || c == 'E')
        goto expon_1;

    else {
        unput(s, c);
        s.yytext[s.yylen] = 0;
        r = processfloat(s, s.yytext, 0, synth);
        goto leave;
    }

expon_1: /* e has been seen, need digits */
    c = input(s);

    if (c >= '0' && c <= '9')
        goto expon_3;
    else if (c == '+' || c == '-')
        goto expon_2;
    else {
        unput(s, c);
        goto error1;
    }

expon_2: /* + or - seen but still need digits */
    c = input(s);

    if (c >= '0' && c <= '9')
        goto expon_3;
    else {
        unput(s, c);
        goto error1;
    }

expon_3:
    c = input(s);

    if (c >= '0' && c <= '9')
        goto expon_3;

    else {
        unput(s, c);
        s.yytext[s.yylen] = 0;
        r = processfloat(s, s.yytext, 0, synth);
        goto leave;
    }

symbol3 : {
    const int endchar = '\'';

    /*do {
        c = input();
    } while (c != endchar && c != 0);*/
    for (; s.yylen < YYLEN_MAX;) {
        c = input(s);
        if (c == '\n' || c == '\r') {
            s.yylen = 0;
            r = BADTOKEN;
            goto leave;
        }
        if (c == '\\') {
            s.yylen--;
            c = input(s);
        } else if (c == endchar)
            break;
        if (c == 0)
            break;
    }
    if (c == 0) {
        s.yylen = 0;
        r = BADTOKEN;
        goto leave;
    }
    s.yytext[s.yylen] = 0;
    s.yytext[s.yylen - 1] = 0;
    r = processsymbol(s, s.yytext, synth);
    goto leave;
}

string1 : {
    int endchar = '"';

    for (; s.yylen < YYLEN_MAX;) {
        c = input(s);
        if (c == '\\') {
            s.yylen--;
            c = input(s);
            switch (c) {
            case 'n':
                s.yytext[s.yylen - 1] = '\n';
                break;
            case 'r':
                s.yytext[s.yylen - 1] = '\r';
                break;
            case 't':
                s.yytext[s.yylen - 1] = '\t';
                break;
            case 'f':
                s.yytext[s.yylen - 1] = '\f';
                break;
            case 'v':
                s.yytext[s.yylen - 1] = '\v';
                break;
            }
        } else if (c == '\r')
            c = '\n';
        else if (c == endchar) {
            s.yylen--;
            s.yytext[s.yylen] = 0;
            r = processstring(s, s.yytext, synth);
            goto leave;
        }
        if (c == 0) {
            s.yylen = 0;
            r = BADTOKEN;
            goto leave;
        }
    }

    r = STRING;
    goto leave;
    // do {
    //     c = input0(s);
    // } while (c && isspace(c));

    // if (c == 0)
    //     unput0(s, c);

    // if (c == '"')
    //     goto string1;
    // else if (c)
    //     unput0(s, c);
}

comment1: /* comment -- to end of line */
    do {
        c = input0(s);
    } while (c != '\n' && c != '\r' && c != 0);
    s.yylen = 0;
    goto start;

comment2 : {
    int prevc = 0;
    int clevel = 1;
    do {
        c = input0(s);
        if (c == '/' && prevc == '*') {
            if (--clevel <= 0)
                break;
            else
                prevc = c, c = input0(s); // eat both characters
        } else if (c == '*' && prevc == '/') {
            clevel++;
            prevc = c, c = input0(s); // eat both characters
        }
        prevc = c;
    } while (c != 0);
    s.yylen = 0;
    goto start;
}


error1:
    s.yytext[s.yylen] = 0;

error2:
    r = BADTOKEN;
    goto leave;

leave:
    s.yytext[s.yylen] = 0;
    s.token_end = s.text_iter;
    return r;
}


struct TokenOnlyAction {
    using Output = std::pair<TokenType, SourceCodeRange>;

    template <TokenType type> std::optional<Output> process(SourceCodeRange loc) {
        switch (type) {
        case TokenType::Space:
        case TokenType::NewLine:
        case TokenType::Tab:
        case TokenType::Comment:
        case TokenType::DocumentationComment:
        case TokenType::MultiLineComment:
        case TokenType::ErMultilineCommentUnclosed:
            return std::nullopt;
        default:
            return { { type, loc } };
        }
    }
};


inline bool tokens_equal(int o, TokenType n) {
    if (o < 128)
        return o == static_cast<int>(n);

    if (sc::lex::is_error(n))
        return o == BADTOKEN;

    if (o == NAME)
        return n == TokenType::Name;
    if (o == INTEGER)
        return n == TokenType::Integer || n == TokenType::IntegerRadix || n == TokenType::Hexidecimal;
    if (o == SC_FLOAT)
        return n == TokenType::Float || n == TokenType::FloatRadix || n == TokenType::FloatExponent;
    if (o == ACCIDENTAL)
        return n == TokenType::AccidentalCents || n == TokenType::AccidentalSteps;
    if (o == SYMBOL)
        return n == TokenType::SymbolQuote || n == TokenType::SymbolSlash;
    if (o == STRING)
        return n == TokenType::StringLine;
    if (o == ASCII)
        return n == TokenType::Ascii;
    if (o == PRIMITIVENAME)
        return n == TokenType::PrimitiveName;
    if (o == CLASSNAME)
        return n == TokenType::ClassName;
    if (o == CURRYARG)
        return n == TokenType::CurryArg;
    if (o == VAR)
        return n == TokenType::Var;
    if (o == ARG)
        return n == TokenType::Arg;
    if (o == CLASSVAR)
        return n == TokenType::ClassVar;
    if (o == SC_CONST)
        return n == TokenType::Const;
    if (o == NILOBJ)
        return n == TokenType::Nil;
    if (o == TRUEOBJ)
        return n == TokenType::True;
    if (o == FALSEOBJ)
        return n == TokenType::False;
    if (o == ELLIPSIS)
        return n == TokenType::Ellipsis;
    if (o == DOTDOT)
        return n == TokenType::DotDot;
    if (o == PIE)
        return n == TokenType::Pi;
    if (o == BEGINCLOSEDFUNC)
        return n == TokenType::BeginClosedFunction;
    if (o == BADTOKEN)
        return is_error(n);
    if (o == INTERPRET)
        return n == TokenType::Interpret;
    if (o == LEFTARROW)
        return n == TokenType::LeftArrow;
    if (o == WHILE)
        return n == TokenType::While;
    if (o == BINOP)
        return n == TokenType::BinaryOperator;
    if (o == KEYBINOP)
        return n == TokenType::KeywordBinaryOperator;
    if (o == READWRITEVAR)
        return n == TokenType::ReadWriteVar;

    assert(false);
    return false;
}

// We cannot compare multiline strings here as the old lexer mashes them together.
// Just avoid trying to do that.
struct OldInfo {
    int type;
    size_t start, end;

    template <typename T> void printOn(T& t, const char* source) const {
        if (type < 128)
            t << "Old{ type: " << static_cast<char>(type) << "[" << type << "]";
        else
            t << "Old{ type: " << type;
        t << ", start: " << start << ", end: " << end << ", ";

        // t << ", src: '";
        // const auto sz = end - start;
        // assert(end >= start);
        // for (size_t i { start }; i < end; ++i) {
        //     t << source[i];
        // }

        t << "[ ";
        for (size_t i { start }; i < end; ++i) {
            t << static_cast<int>(source[i]) << " ";
        }
        t << ']';
    }
};
struct NewInfo {
    TokenType type;
    size_t start, end;

    template <typename T> void printOn(T& t, const char* source) const {
        t << "New{ type: " << type << ", start: " << start << ", end: " << end << ", ";

        // t << ", src: '";
        // const auto sz = end - start;
        // assert(end >= start);
        // for (size_t i { start }; i < end; ++i) {
        // t << source[i];
        //}

        t << "[ ";
        for (size_t i { start }; i < end; ++i) {
            t << static_cast<int>(source[i]) << " ";
        }
        t << ']';
    }
};

template <typename T> void print_tokens(const std::vector<T>& n, const char* text) {
    for (size_t i { 0 }; i < n.size(); ++i) {
        const auto o = n.at(i);
        o.printOn(std::cout, text);
        std::cout << std::endl;
    }
}
