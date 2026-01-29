/* A Bison parser, made by GNU Bison 3.8.2.  */

/* Bison implementation for Yacc-like parsers in C

   Copyright (C) 1984, 1989-1990, 2000-2015, 2018-2021 Free Software Foundation,
   Inc.

   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <https://www.gnu.org/licenses/>.  */

/* As a special exception, you may create a larger work that contains
   part or all of the Bison parser skeleton and distribute that work
   under terms of your choice, so long as that work isn't itself a
   parser generator using the skeleton or a modified version thereof
   as a parser skeleton.  Alternatively, if you modify or redistribute
   the parser skeleton itself, you may (at your option) remove this
   special exception, which will cause the skeleton and the resulting
   Bison output files to be licensed under the GNU General Public
   License without this special exception.

   This special exception was added by the Free Software Foundation in
   version 2.2 of Bison.  */

/* C LALR(1) parser skeleton written by Richard Stallman, by
   simplifying the original so-called "semantic" parser.  */

/* DO NOT RELY ON FEATURES THAT ARE NOT DOCUMENTED in the manual,
   especially those whose name start with YY_ or yy_.  They are
   private implementation details that can be changed or removed.  */

/* All symbols defined below should begin with yy or YY, to avoid
   infringing on user name space.  This should be done even for local
   variables, as they might otherwise be expanded by user macros.
   There are some unavoidable exceptions within include files to
   define necessary library symbols; they are noted "INFRINGES ON
   USER NAME SPACE" below.  */

/* Identify Bison output, and Bison version.  */
#define YYBISON 30802

/* Bison version string.  */
#define YYBISON_VERSION "3.8.2"

/* Skeleton name.  */
#define YYSKELETON_NAME "yacc.c"

/* Pure parsers.  */
#define YYPURE 0

/* Push parsers.  */
#define YYPUSH 0

/* Pull parsers.  */
#define YYPULL 1




/* First part of user prologue.  */
#line 17 "lang11d"


#include <iostream>
#include <stdlib.h>
#include <string.h>
#include "PyrLexer.h"
#include "PyrParseNode.h"
#include "SC_Constants.h"
#include "SC_InlineUnaryOp.h"
#include "SC_InlineBinaryOp.h"
#include "InitAlloc.h"
#include "PredefinedSymbols.h"
#include "SimpleStack.h"

void bcopy(void *src, void *dst, size_t size) ;
int yyparse();
extern bool compilingCmdLine;
extern LongStack generatorStack;



#line 93 "lang11d_tab.cpp"

# ifndef YY_CAST
#  ifdef __cplusplus
#   define YY_CAST(Type, Val) static_cast<Type> (Val)
#   define YY_REINTERPRET_CAST(Type, Val) reinterpret_cast<Type> (Val)
#  else
#   define YY_CAST(Type, Val) ((Type) (Val))
#   define YY_REINTERPRET_CAST(Type, Val) ((Type) (Val))
#  endif
# endif
# ifndef YY_NULLPTR
#  if defined __cplusplus
#   if 201103L <= __cplusplus
#    define YY_NULLPTR nullptr
#   else
#    define YY_NULLPTR 0
#   endif
#  else
#   define YY_NULLPTR ((void*)0)
#  endif
# endif

#include "lang11d_tab.h"
/* Symbol kind.  */
enum yysymbol_kind_t
{
  YYSYMBOL_YYEMPTY = -2,
  YYSYMBOL_YYEOF = 0,                      /* "end of file"  */
  YYSYMBOL_YYerror = 1,                    /* error  */
  YYSYMBOL_YYUNDEF = 2,                    /* "invalid token"  */
  YYSYMBOL_NAME = 3,                       /* NAME  */
  YYSYMBOL_INTEGER = 4,                    /* INTEGER  */
  YYSYMBOL_SC_FLOAT = 5,                   /* SC_FLOAT  */
  YYSYMBOL_ACCIDENTAL = 6,                 /* ACCIDENTAL  */
  YYSYMBOL_SYMBOL = 7,                     /* SYMBOL  */
  YYSYMBOL_STRING = 8,                     /* STRING  */
  YYSYMBOL_ASCII = 9,                      /* ASCII  */
  YYSYMBOL_PRIMITIVENAME = 10,             /* PRIMITIVENAME  */
  YYSYMBOL_CLASSNAME = 11,                 /* CLASSNAME  */
  YYSYMBOL_CURRYARG = 12,                  /* CURRYARG  */
  YYSYMBOL_VAR = 13,                       /* VAR  */
  YYSYMBOL_ARG = 14,                       /* ARG  */
  YYSYMBOL_CLASSVAR = 15,                  /* CLASSVAR  */
  YYSYMBOL_SC_CONST = 16,                  /* SC_CONST  */
  YYSYMBOL_NILOBJ = 17,                    /* NILOBJ  */
  YYSYMBOL_TRUEOBJ = 18,                   /* TRUEOBJ  */
  YYSYMBOL_FALSEOBJ = 19,                  /* FALSEOBJ  */
  YYSYMBOL_PSEUDOVAR = 20,                 /* PSEUDOVAR  */
  YYSYMBOL_ELLIPSIS = 21,                  /* ELLIPSIS  */
  YYSYMBOL_DOTDOT = 22,                    /* DOTDOT  */
  YYSYMBOL_PIE = 23,                       /* PIE  */
  YYSYMBOL_BEGINCLOSEDFUNC = 24,           /* BEGINCLOSEDFUNC  */
  YYSYMBOL_BADTOKEN = 25,                  /* BADTOKEN  */
  YYSYMBOL_INTERPRET = 26,                 /* INTERPRET  */
  YYSYMBOL_BEGINGENERATOR = 27,            /* BEGINGENERATOR  */
  YYSYMBOL_LEFTARROW = 28,                 /* LEFTARROW  */
  YYSYMBOL_WHILE = 29,                     /* WHILE  */
  YYSYMBOL_30_ = 30,                       /* ':'  */
  YYSYMBOL_31_ = 31,                       /* '='  */
  YYSYMBOL_BINOP = 32,                     /* BINOP  */
  YYSYMBOL_KEYBINOP = 33,                  /* KEYBINOP  */
  YYSYMBOL_34_ = 34,                       /* '-'  */
  YYSYMBOL_35_ = 35,                       /* '<'  */
  YYSYMBOL_36_ = 36,                       /* '>'  */
  YYSYMBOL_37_ = 37,                       /* '*'  */
  YYSYMBOL_38_ = 38,                       /* '+'  */
  YYSYMBOL_39_ = 39,                       /* '|'  */
  YYSYMBOL_READWRITEVAR = 40,              /* READWRITEVAR  */
  YYSYMBOL_41_ = 41,                       /* '.'  */
  YYSYMBOL_42_ = 42,                       /* '`'  */
  YYSYMBOL_UMINUS = 43,                    /* UMINUS  */
  YYSYMBOL_KWARGEXPAND = 44,               /* KWARGEXPAND  */
  YYSYMBOL_45_ = 45,                       /* '{'  */
  YYSYMBOL_46_ = 46,                       /* '}'  */
  YYSYMBOL_47_ = 47,                       /* '['  */
  YYSYMBOL_48_ = 48,                       /* ']'  */
  YYSYMBOL_49_ = 49,                       /* ';'  */
  YYSYMBOL_50_ = 50,                       /* ','  */
  YYSYMBOL_51_ = 51,                       /* '('  */
  YYSYMBOL_52_ = 52,                       /* ')'  */
  YYSYMBOL_53_ = 53,                       /* '^'  */
  YYSYMBOL_54_ = 54,                       /* '~'  */
  YYSYMBOL_55_ = 55,                       /* '#'  */
  YYSYMBOL_YYACCEPT = 56,                  /* $accept  */
  YYSYMBOL_root = 57,                      /* root  */
  YYSYMBOL_classes = 58,                   /* classes  */
  YYSYMBOL_classextensions = 59,           /* classextensions  */
  YYSYMBOL_classdef = 60,                  /* classdef  */
  YYSYMBOL_classextension = 61,            /* classextension  */
  YYSYMBOL_optname = 62,                   /* optname  */
  YYSYMBOL_superclass = 63,                /* superclass  */
  YYSYMBOL_classvardecls = 64,             /* classvardecls  */
  YYSYMBOL_classvardecl = 65,              /* classvardecl  */
  YYSYMBOL_methods = 66,                   /* methods  */
  YYSYMBOL_methoddef = 67,                 /* methoddef  */
  YYSYMBOL_optsemi = 68,                   /* optsemi  */
  YYSYMBOL_optcomma = 69,                  /* optcomma  */
  YYSYMBOL_optequal = 70,                  /* optequal  */
  YYSYMBOL_funcbody = 71,                  /* funcbody  */
  YYSYMBOL_cmdlinecode = 72,               /* cmdlinecode  */
  YYSYMBOL_methbody = 73,                  /* methbody  */
  YYSYMBOL_primitive = 74,                 /* primitive  */
  YYSYMBOL_retval = 75,                    /* retval  */
  YYSYMBOL_funretval = 76,                 /* funretval  */
  YYSYMBOL_blocklist1 = 77,                /* blocklist1  */
  YYSYMBOL_blocklistitem = 78,             /* blocklistitem  */
  YYSYMBOL_blocklist = 79,                 /* blocklist  */
  YYSYMBOL_msgsend = 80,                   /* msgsend  */
  YYSYMBOL_generator = 81,                 /* generator  */
  YYSYMBOL_82_1 = 82,                      /* $@1  */
  YYSYMBOL_83_2 = 83,                      /* $@2  */
  YYSYMBOL_nextqual = 84,                  /* nextqual  */
  YYSYMBOL_qual = 85,                      /* qual  */
  YYSYMBOL_expr1 = 86,                     /* expr1  */
  YYSYMBOL_valrangex1 = 87,                /* valrangex1  */
  YYSYMBOL_valrangeassign = 88,            /* valrangeassign  */
  YYSYMBOL_valrangexd = 89,                /* valrangexd  */
  YYSYMBOL_valrange2 = 90,                 /* valrange2  */
  YYSYMBOL_valrange3 = 91,                 /* valrange3  */
  YYSYMBOL_expr = 92,                      /* expr  */
  YYSYMBOL_adverb = 93,                    /* adverb  */
  YYSYMBOL_exprn = 94,                     /* exprn  */
  YYSYMBOL_exprseq = 95,                   /* exprseq  */
  YYSYMBOL_arrayelems = 96,                /* arrayelems  */
  YYSYMBOL_arrayelems1 = 97,               /* arrayelems1  */
  YYSYMBOL_arglist1 = 98,                  /* arglist1  */
  YYSYMBOL_arglistv1 = 99,                 /* arglistv1  */
  YYSYMBOL_keyarglist1 = 100,              /* keyarglist1  */
  YYSYMBOL_bigArgs = 101,                  /* bigArgs  */
  YYSYMBOL_bigArgsList = 102,              /* bigArgsList  */
  YYSYMBOL_keyarg = 103,                   /* keyarg  */
  YYSYMBOL_optkeyarglist = 104,            /* optkeyarglist  */
  YYSYMBOL_mavars = 105,                   /* mavars  */
  YYSYMBOL_mavarlist = 106,                /* mavarlist  */
  YYSYMBOL_slotliteral = 107,              /* slotliteral  */
  YYSYMBOL_blockliteral = 108,             /* blockliteral  */
  YYSYMBOL_pushname = 109,                 /* pushname  */
  YYSYMBOL_pushliteral = 110,              /* pushliteral  */
  YYSYMBOL_listliteral = 111,              /* listliteral  */
  YYSYMBOL_block = 112,                    /* block  */
  YYSYMBOL_funcvardecls = 113,             /* funcvardecls  */
  YYSYMBOL_funcvardecls1 = 114,            /* funcvardecls1  */
  YYSYMBOL_funcvardecl = 115,              /* funcvardecl  */
  YYSYMBOL_argdecls = 116,                 /* argdecls  */
  YYSYMBOL_argdecls1 = 117,                /* argdecls1  */
  YYSYMBOL_constdeflist = 118,             /* constdeflist  */
  YYSYMBOL_constdef = 119,                 /* constdef  */
  YYSYMBOL_slotdeflist0 = 120,             /* slotdeflist0  */
  YYSYMBOL_slotdeflist = 121,              /* slotdeflist  */
  YYSYMBOL_slotdef = 122,                  /* slotdef  */
  YYSYMBOL_vardeflist0 = 123,              /* vardeflist0  */
  YYSYMBOL_vardeflist = 124,               /* vardeflist  */
  YYSYMBOL_vardef = 125,                   /* vardef  */
  YYSYMBOL_dictslotdef = 126,              /* dictslotdef  */
  YYSYMBOL_dictslotlist1 = 127,            /* dictslotlist1  */
  YYSYMBOL_dictslotlist = 128,             /* dictslotlist  */
  YYSYMBOL_rwslotdeflist = 129,            /* rwslotdeflist  */
  YYSYMBOL_rwslotdef = 130,                /* rwslotdef  */
  YYSYMBOL_dictlit2 = 131,                 /* dictlit2  */
  YYSYMBOL_litdictslotdef = 132,           /* litdictslotdef  */
  YYSYMBOL_litdictslotlist1 = 133,         /* litdictslotlist1  */
  YYSYMBOL_litdictslotlist = 134,          /* litdictslotlist  */
  YYSYMBOL_listlit = 135,                  /* listlit  */
  YYSYMBOL_listlit2 = 136,                 /* listlit2  */
  YYSYMBOL_literallistc = 137,             /* literallistc  */
  YYSYMBOL_literallist1 = 138,             /* literallist1  */
  YYSYMBOL_rwspec = 139,                   /* rwspec  */
  YYSYMBOL_rspec = 140,                    /* rspec  */
  YYSYMBOL_integer = 141,                  /* integer  */
  YYSYMBOL_floatr = 142,                   /* floatr  */
  YYSYMBOL_accidental = 143,               /* accidental  */
  YYSYMBOL_pie = 144,                      /* pie  */
  YYSYMBOL_floatp = 145,                   /* floatp  */
  YYSYMBOL_name = 146,                     /* name  */
  YYSYMBOL_classname = 147,                /* classname  */
  YYSYMBOL_primname = 148,                 /* primname  */
  YYSYMBOL_trueobj = 149,                  /* trueobj  */
  YYSYMBOL_falseobj = 150,                 /* falseobj  */
  YYSYMBOL_nilobj = 151,                   /* nilobj  */
  YYSYMBOL_ascii = 152,                    /* ascii  */
  YYSYMBOL_symbol = 153,                   /* symbol  */
  YYSYMBOL_string = 154,                   /* string  */
  YYSYMBOL_pseudovar = 155,                /* pseudovar  */
  YYSYMBOL_binop = 156,                    /* binop  */
  YYSYMBOL_keybinop = 157,                 /* keybinop  */
  YYSYMBOL_binop2 = 158,                   /* binop2  */
  YYSYMBOL_curryarg = 159                  /* curryarg  */
};
typedef enum yysymbol_kind_t yysymbol_kind_t;




#ifdef short
# undef short
#endif

/* On compilers that do not define __PTRDIFF_MAX__ etc., make sure
   <limits.h> and (if available) <stdint.h> are included
   so that the code can choose integer types of a good width.  */

#ifndef __PTRDIFF_MAX__
# include <limits.h> /* INFRINGES ON USER NAME SPACE */
# if defined __STDC_VERSION__ && 199901 <= __STDC_VERSION__
#  include <stdint.h> /* INFRINGES ON USER NAME SPACE */
#  define YY_STDINT_H
# endif
#endif

/* Narrow types that promote to a signed type and that can represent a
   signed or unsigned integer of at least N bits.  In tables they can
   save space and decrease cache pressure.  Promoting to a signed type
   helps avoid bugs in integer arithmetic.  */

#ifdef __INT_LEAST8_MAX__
typedef __INT_LEAST8_TYPE__ yytype_int8;
#elif defined YY_STDINT_H
typedef int_least8_t yytype_int8;
#else
typedef signed char yytype_int8;
#endif

#ifdef __INT_LEAST16_MAX__
typedef __INT_LEAST16_TYPE__ yytype_int16;
#elif defined YY_STDINT_H
typedef int_least16_t yytype_int16;
#else
typedef short yytype_int16;
#endif

/* Work around bug in HP-UX 11.23, which defines these macros
   incorrectly for preprocessor constants.  This workaround can likely
   be removed in 2023, as HPE has promised support for HP-UX 11.23
   (aka HP-UX 11i v2) only through the end of 2022; see Table 2 of
   <https://h20195.www2.hpe.com/V2/getpdf.aspx/4AA4-7673ENW.pdf>.  */
#ifdef __hpux
# undef UINT_LEAST8_MAX
# undef UINT_LEAST16_MAX
# define UINT_LEAST8_MAX 255
# define UINT_LEAST16_MAX 65535
#endif

#if defined __UINT_LEAST8_MAX__ && __UINT_LEAST8_MAX__ <= __INT_MAX__
typedef __UINT_LEAST8_TYPE__ yytype_uint8;
#elif (!defined __UINT_LEAST8_MAX__ && defined YY_STDINT_H \
       && UINT_LEAST8_MAX <= INT_MAX)
typedef uint_least8_t yytype_uint8;
#elif !defined __UINT_LEAST8_MAX__ && UCHAR_MAX <= INT_MAX
typedef unsigned char yytype_uint8;
#else
typedef short yytype_uint8;
#endif

#if defined __UINT_LEAST16_MAX__ && __UINT_LEAST16_MAX__ <= __INT_MAX__
typedef __UINT_LEAST16_TYPE__ yytype_uint16;
#elif (!defined __UINT_LEAST16_MAX__ && defined YY_STDINT_H \
       && UINT_LEAST16_MAX <= INT_MAX)
typedef uint_least16_t yytype_uint16;
#elif !defined __UINT_LEAST16_MAX__ && USHRT_MAX <= INT_MAX
typedef unsigned short yytype_uint16;
#else
typedef int yytype_uint16;
#endif

#ifndef YYPTRDIFF_T
# if defined __PTRDIFF_TYPE__ && defined __PTRDIFF_MAX__
#  define YYPTRDIFF_T __PTRDIFF_TYPE__
#  define YYPTRDIFF_MAXIMUM __PTRDIFF_MAX__
# elif defined PTRDIFF_MAX
#  ifndef ptrdiff_t
#   include <stddef.h> /* INFRINGES ON USER NAME SPACE */
#  endif
#  define YYPTRDIFF_T ptrdiff_t
#  define YYPTRDIFF_MAXIMUM PTRDIFF_MAX
# else
#  define YYPTRDIFF_T long
#  define YYPTRDIFF_MAXIMUM LONG_MAX
# endif
#endif

#ifndef YYSIZE_T
# ifdef __SIZE_TYPE__
#  define YYSIZE_T __SIZE_TYPE__
# elif defined size_t
#  define YYSIZE_T size_t
# elif defined __STDC_VERSION__ && 199901 <= __STDC_VERSION__
#  include <stddef.h> /* INFRINGES ON USER NAME SPACE */
#  define YYSIZE_T size_t
# else
#  define YYSIZE_T unsigned
# endif
#endif

#define YYSIZE_MAXIMUM                                  \
  YY_CAST (YYPTRDIFF_T,                                 \
           (YYPTRDIFF_MAXIMUM < YY_CAST (YYSIZE_T, -1)  \
            ? YYPTRDIFF_MAXIMUM                         \
            : YY_CAST (YYSIZE_T, -1)))

#define YYSIZEOF(X) YY_CAST (YYPTRDIFF_T, sizeof (X))


/* Stored state numbers (used for stacks). */
typedef yytype_int16 yy_state_t;

/* State numbers in computations.  */
typedef int yy_state_fast_t;

#ifndef YY_
# if defined YYENABLE_NLS && YYENABLE_NLS
#  if ENABLE_NLS
#   include <libintl.h> /* INFRINGES ON USER NAME SPACE */
#   define YY_(Msgid) dgettext ("bison-runtime", Msgid)
#  endif
# endif
# ifndef YY_
#  define YY_(Msgid) Msgid
# endif
#endif


#ifndef YY_ATTRIBUTE_PURE
# if defined __GNUC__ && 2 < __GNUC__ + (96 <= __GNUC_MINOR__)
#  define YY_ATTRIBUTE_PURE __attribute__ ((__pure__))
# else
#  define YY_ATTRIBUTE_PURE
# endif
#endif

#ifndef YY_ATTRIBUTE_UNUSED
# if defined __GNUC__ && 2 < __GNUC__ + (7 <= __GNUC_MINOR__)
#  define YY_ATTRIBUTE_UNUSED __attribute__ ((__unused__))
# else
#  define YY_ATTRIBUTE_UNUSED
# endif
#endif

/* Suppress unused-variable warnings by "using" E.  */
#if ! defined lint || defined __GNUC__
# define YY_USE(E) ((void) (E))
#else
# define YY_USE(E) /* empty */
#endif

/* Suppress an incorrect diagnostic about yylval being uninitialized.  */
#if defined __GNUC__ && ! defined __ICC && 406 <= __GNUC__ * 100 + __GNUC_MINOR__
# if __GNUC__ * 100 + __GNUC_MINOR__ < 407
#  define YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN                           \
    _Pragma ("GCC diagnostic push")                                     \
    _Pragma ("GCC diagnostic ignored \"-Wuninitialized\"")
# else
#  define YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN                           \
    _Pragma ("GCC diagnostic push")                                     \
    _Pragma ("GCC diagnostic ignored \"-Wuninitialized\"")              \
    _Pragma ("GCC diagnostic ignored \"-Wmaybe-uninitialized\"")
# endif
# define YY_IGNORE_MAYBE_UNINITIALIZED_END      \
    _Pragma ("GCC diagnostic pop")
#else
# define YY_INITIAL_VALUE(Value) Value
#endif
#ifndef YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
# define YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
# define YY_IGNORE_MAYBE_UNINITIALIZED_END
#endif
#ifndef YY_INITIAL_VALUE
# define YY_INITIAL_VALUE(Value) /* Nothing. */
#endif

#if defined __cplusplus && defined __GNUC__ && ! defined __ICC && 6 <= __GNUC__
# define YY_IGNORE_USELESS_CAST_BEGIN                          \
    _Pragma ("GCC diagnostic push")                            \
    _Pragma ("GCC diagnostic ignored \"-Wuseless-cast\"")
# define YY_IGNORE_USELESS_CAST_END            \
    _Pragma ("GCC diagnostic pop")
#endif
#ifndef YY_IGNORE_USELESS_CAST_BEGIN
# define YY_IGNORE_USELESS_CAST_BEGIN
# define YY_IGNORE_USELESS_CAST_END
#endif


#define YY_ASSERT(E) ((void) (0 && (E)))

#if 1

/* The parser invokes alloca or malloc; define the necessary symbols.  */

# ifdef YYSTACK_USE_ALLOCA
#  if YYSTACK_USE_ALLOCA
#   ifdef __GNUC__
#    define YYSTACK_ALLOC __builtin_alloca
#   elif defined __BUILTIN_VA_ARG_INCR
#    include <alloca.h> /* INFRINGES ON USER NAME SPACE */
#   elif defined _AIX
#    define YYSTACK_ALLOC __alloca
#   elif defined _MSC_VER
#    include <malloc.h> /* INFRINGES ON USER NAME SPACE */
#    define alloca _alloca
#   else
#    define YYSTACK_ALLOC alloca
#    if ! defined _ALLOCA_H && ! defined EXIT_SUCCESS
#     include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
      /* Use EXIT_SUCCESS as a witness for stdlib.h.  */
#     ifndef EXIT_SUCCESS
#      define EXIT_SUCCESS 0
#     endif
#    endif
#   endif
#  endif
# endif

# ifdef YYSTACK_ALLOC
   /* Pacify GCC's 'empty if-body' warning.  */
#  define YYSTACK_FREE(Ptr) do { /* empty */; } while (0)
#  ifndef YYSTACK_ALLOC_MAXIMUM
    /* The OS might guarantee only one guard page at the bottom of the stack,
       and a page size can be as small as 4096 bytes.  So we cannot safely
       invoke alloca (N) if N exceeds 4096.  Use a slightly smaller number
       to allow for a few compiler-allocated temporary stack slots.  */
#   define YYSTACK_ALLOC_MAXIMUM 4032 /* reasonable circa 2006 */
#  endif
# else
#  define YYSTACK_ALLOC YYMALLOC
#  define YYSTACK_FREE YYFREE
#  ifndef YYSTACK_ALLOC_MAXIMUM
#   define YYSTACK_ALLOC_MAXIMUM YYSIZE_MAXIMUM
#  endif
#  if (defined __cplusplus && ! defined EXIT_SUCCESS \
       && ! ((defined YYMALLOC || defined malloc) \
             && (defined YYFREE || defined free)))
#   include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
#   ifndef EXIT_SUCCESS
#    define EXIT_SUCCESS 0
#   endif
#  endif
#  ifndef YYMALLOC
#   define YYMALLOC malloc
#   if ! defined malloc && ! defined EXIT_SUCCESS
void *malloc (YYSIZE_T); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
#  ifndef YYFREE
#   define YYFREE free
#   if ! defined free && ! defined EXIT_SUCCESS
void free (void *); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
# endif
#endif /* 1 */

#if (! defined yyoverflow \
     && (! defined __cplusplus \
         || (defined YYSTYPE_IS_TRIVIAL && YYSTYPE_IS_TRIVIAL)))

/* A type that is properly aligned for any stack member.  */
union yyalloc
{
  yy_state_t yyss_alloc;
  YYSTYPE yyvs_alloc;
};

/* The size of the maximum gap between one aligned stack and the next.  */
# define YYSTACK_GAP_MAXIMUM (YYSIZEOF (union yyalloc) - 1)

/* The size of an array large to enough to hold all stacks, each with
   N elements.  */
# define YYSTACK_BYTES(N) \
     ((N) * (YYSIZEOF (yy_state_t) + YYSIZEOF (YYSTYPE)) \
      + YYSTACK_GAP_MAXIMUM)

# define YYCOPY_NEEDED 1

/* Relocate STACK from its old location to the new one.  The
   local variables YYSIZE and YYSTACKSIZE give the old and new number of
   elements in the stack, and YYPTR gives the new location of the
   stack.  Advance YYPTR to a properly aligned location for the next
   stack.  */
# define YYSTACK_RELOCATE(Stack_alloc, Stack)                           \
    do                                                                  \
      {                                                                 \
        YYPTRDIFF_T yynewbytes;                                         \
        YYCOPY (&yyptr->Stack_alloc, Stack, yysize);                    \
        Stack = &yyptr->Stack_alloc;                                    \
        yynewbytes = yystacksize * YYSIZEOF (*Stack) + YYSTACK_GAP_MAXIMUM; \
        yyptr += yynewbytes / YYSIZEOF (*yyptr);                        \
      }                                                                 \
    while (0)

#endif

#if defined YYCOPY_NEEDED && YYCOPY_NEEDED
/* Copy COUNT objects from SRC to DST.  The source and destination do
   not overlap.  */
# ifndef YYCOPY
#  if defined __GNUC__ && 1 < __GNUC__
#   define YYCOPY(Dst, Src, Count) \
      __builtin_memcpy (Dst, Src, YY_CAST (YYSIZE_T, (Count)) * sizeof (*(Src)))
#  else
#   define YYCOPY(Dst, Src, Count)              \
      do                                        \
        {                                       \
          YYPTRDIFF_T yyi;                      \
          for (yyi = 0; yyi < (Count); yyi++)   \
            (Dst)[yyi] = (Src)[yyi];            \
        }                                       \
      while (0)
#  endif
# endif
#endif /* !YYCOPY_NEEDED */

/* YYFINAL -- State number of the termination state.  */
#define YYFINAL  70
/* YYLAST -- Last index in YYTABLE.  */
#define YYLAST   2051

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  56
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  104
/* YYNRULES -- Number of rules.  */
#define YYNRULES  310
/* YYNSTATES -- Number of states.  */
#define YYNSTATES  582

/* YYMAXUTOK -- Last valid token kind.  */
#define YYMAXUTOK   289


/* YYTRANSLATE(TOKEN-NUM) -- Symbol number corresponding to TOKEN-NUM
   as returned by yylex, with out-of-bounds checking.  */
#define YYTRANSLATE(YYX)                                \
  (0 <= (YYX) && (YYX) <= YYMAXUTOK                     \
   ? YY_CAST (yysymbol_kind_t, yytranslate[YYX])        \
   : YYSYMBOL_YYUNDEF)

/* YYTRANSLATE[TOKEN-NUM] -- Symbol number corresponding to TOKEN-NUM
   as returned by yylex.  */
static const yytype_int8 yytranslate[] =
{
       0,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,    55,     2,     2,     2,     2,
      51,    52,    37,    38,    50,    34,    41,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,    30,    49,
      35,    31,    36,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,    47,     2,    48,    53,     2,    42,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,    45,    39,    46,    54,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     1,     2,     3,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    19,    20,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    32,    33,    40,    43,    44
};

#if YYDEBUG
/* YYRLINE[YYN] -- Source line where rule number YYN was defined.  */
static const yytype_int16 yyrline[] =
{
       0,    41,    41,    43,    45,    49,    50,    54,    55,    59,
      63,    70,    76,    77,    80,    81,    85,    86,    90,    92,
      94,    98,    99,   103,   106,   109,   112,   117,   118,   121,
     122,   125,   126,   129,   130,   134,   136,   138,   140,   142,
     144,   146,   150,   151,   155,   156,   161,   162,   167,   168,
     172,   173,   179,   180,   183,   184,   187,   191,   195,   199,
     204,   208,   213,   231,   244,   246,   257,   268,   279,   292,
     313,   322,   331,   336,   350,   372,   376,   382,   400,   405,
     411,   411,   421,   421,   428,   449,   453,   487,   525,   539,
     550,   554,   579,   580,   581,   582,   583,   584,   585,   591,
     601,   603,   605,   607,   609,   611,   624,   627,   654,   672,
     699,   727,   746,   774,   801,   819,   844,   872,   891,   919,
     938,   957,   974,   988,  1009,  1028,  1046,  1063,  1079,  1095,
    1096,  1097,  1098,  1099,  1112,  1126,  1131,  1135,  1146,  1151,
    1161,  1166,  1180,  1196,  1197,  1198,  1199,  1202,  1203,  1209,
    1212,  1213,  1217,  1218,  1220,  1225,  1227,  1234,  1242,  1243,
    1247,  1249,  1261,  1262,  1278,  1279,  1280,  1281,  1284,  1284,
    1288,  1292,  1293,  1296,  1298,  1302,  1303,  1308,  1309,  1310,
    1311,  1312,  1313,  1314,  1315,  1316,  1319,  1322,  1325,  1326,
    1327,  1328,  1329,  1330,  1331,  1332,  1333,  1336,  1337,  1338,
    1339,  1340,  1341,  1342,  1343,  1344,  1345,  1346,  1349,  1352,
    1357,  1358,  1362,  1363,  1367,  1371,  1372,  1376,  1380,  1384,
    1388,  1394,  1398,  1402,  1406,  1410,  1417,  1418,  1422,  1426,
    1427,  1430,  1431,  1435,  1437,  1439,  1447,  1448,  1451,  1452,
    1456,  1458,  1460,  1468,  1470,  1477,  1478,  1482,  1483,  1486,
    1487,  1491,  1493,  1497,  1501,  1503,  1510,  1511,  1515,  1516,
    1521,  1523,  1527,  1529,  1533,  1534,  1537,  1538,  1542,  1543,
    1545,  1547,  1551,  1552,  1556,  1557,  1566,  1567,  1576,  1577,
    1588,  1591,  1592,  1593,  1599,  1607,  1614,  1623,  1624,  1627,
    1630,  1633,  1636,  1639,  1642,  1645,  1648,  1651,  1654,  1655,
    1656,  1657,  1658,  1659,  1660,  1661,  1662,  1665,  1668,  1669,
    1672
};
#endif

/** Accessing symbol of state STATE.  */
#define YY_ACCESSING_SYMBOL(State) YY_CAST (yysymbol_kind_t, yystos[State])

#if 1
/* The user-facing name of the symbol whose (internal) number is
   YYSYMBOL.  No bounds checking.  */
static const char *yysymbol_name (yysymbol_kind_t yysymbol) YY_ATTRIBUTE_UNUSED;

/* YYTNAME[SYMBOL-NUM] -- String name of the symbol SYMBOL-NUM.
   First, the terminals, then, starting at YYNTOKENS, nonterminals.  */
static const char *const yytname[] =
{
  "\"end of file\"", "error", "\"invalid token\"", "NAME", "INTEGER",
  "SC_FLOAT", "ACCIDENTAL", "SYMBOL", "STRING", "ASCII", "PRIMITIVENAME",
  "CLASSNAME", "CURRYARG", "VAR", "ARG", "CLASSVAR", "SC_CONST", "NILOBJ",
  "TRUEOBJ", "FALSEOBJ", "PSEUDOVAR", "ELLIPSIS", "DOTDOT", "PIE",
  "BEGINCLOSEDFUNC", "BADTOKEN", "INTERPRET", "BEGINGENERATOR",
  "LEFTARROW", "WHILE", "':'", "'='", "BINOP", "KEYBINOP", "'-'", "'<'",
  "'>'", "'*'", "'+'", "'|'", "READWRITEVAR", "'.'", "'`'", "UMINUS",
  "KWARGEXPAND", "'{'", "'}'", "'['", "']'", "';'", "','", "'('", "')'",
  "'^'", "'~'", "'#'", "$accept", "root", "classes", "classextensions",
  "classdef", "classextension", "optname", "superclass", "classvardecls",
  "classvardecl", "methods", "methoddef", "optsemi", "optcomma",
  "optequal", "funcbody", "cmdlinecode", "methbody", "primitive", "retval",
  "funretval", "blocklist1", "blocklistitem", "blocklist", "msgsend",
  "generator", "$@1", "$@2", "nextqual", "qual", "expr1", "valrangex1",
  "valrangeassign", "valrangexd", "valrange2", "valrange3", "expr",
  "adverb", "exprn", "exprseq", "arrayelems", "arrayelems1", "arglist1",
  "arglistv1", "keyarglist1", "bigArgs", "bigArgsList", "keyarg",
  "optkeyarglist", "mavars", "mavarlist", "slotliteral", "blockliteral",
  "pushname", "pushliteral", "listliteral", "block", "funcvardecls",
  "funcvardecls1", "funcvardecl", "argdecls", "argdecls1", "constdeflist",
  "constdef", "slotdeflist0", "slotdeflist", "slotdef", "vardeflist0",
  "vardeflist", "vardef", "dictslotdef", "dictslotlist1", "dictslotlist",
  "rwslotdeflist", "rwslotdef", "dictlit2", "litdictslotdef",
  "litdictslotlist1", "litdictslotlist", "listlit", "listlit2",
  "literallistc", "literallist1", "rwspec", "rspec", "integer", "floatr",
  "accidental", "pie", "floatp", "name", "classname", "primname",
  "trueobj", "falseobj", "nilobj", "ascii", "symbol", "string",
  "pseudovar", "binop", "keybinop", "binop2", "curryarg", YY_NULLPTR
};

static const char *
yysymbol_name (yysymbol_kind_t yysymbol)
{
  return yytname[yysymbol];
}
#endif

#define YYPACT_NINF (-346)

#define yypact_value_is_default(Yyn) \
  ((Yyn) == YYPACT_NINF)

#define YYTABLE_NINF (-306)

#define yytable_value_is_error(Yyn) \
  0

/* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
   STATE-NUM.  */
static const yytype_int16 yypact[] =
{
     169,  1049,    32,   101,    32,   130,  -346,  -346,  -346,  -346,
    -346,  -346,  -346,  -346,  -346,  -346,   129,   129,  -346,  -346,
    -346,  -346,  -346,    90,  -346,   305,   129,  1897,    49,  1526,
     936,  1897,   129,    43,  -346,  -346,  -346,  -346,  -346,    87,
    -346,  -346,  -346,  2007,   112,   121,  -346,  -346,  -346,  -346,
    1261,  -346,  1261,  -346,   182,   182,  -346,  -346,  -346,   187,
     228,  -346,  -346,  -346,  -346,  -346,  -346,  -346,  -346,   171,
    -346,  -346,   123,  -346,     0,  -346,   126,   224,   151,   129,
     129,  -346,  -346,  -346,  -346,  -346,   238,    52,  -346,   116,
     996,   193,  1897,  1897,  -346,  -346,   219,   213,   215,  1897,
    1897,  1579,  -346,   305,  -346,  -346,  -346,  -346,    23,  -346,
    -346,   211,    26,  1261,  1261,  -346,   226,   222,  -346,  1897,
     248,  1989,   247,  1985,   271,    72,  -346,   236,  1632,  -346,
    -346,    38,  -346,   267,  1897,  -346,  -346,  -346,  -346,  -346,
    1261,  -346,  -346,  1897,  1314,   118,  -346,  -346,  -346,  1526,
    1102,   118,  -346,    32,   129,   270,  -346,   129,  1897,  1897,
     129,  -346,   291,   175,   297,    76,  1261,   129,  -346,  -346,
     129,  -346,   751,  -346,  -346,  1261,  1897,  -346,  1526,  -346,
    -346,  -346,  1897,   272,    33,  -346,  1897,  1897,  1897,  -346,
     277,   294,  1261,  1526,  -346,  -346,  -346,    79,  -346,  -346,
    1897,  1985,  1950,  -346,  -346,  -346,   295,   298,   182,  -346,
    -346,   302,  -346,  -346,  -346,  -346,  -346,  -346,  1897,   129,
     129,  1985,  1897,  -346,    11,  1685,   831,   253,    66,  1897,
    2007,  -346,  2007,  1897,   118,   300,   303,  -346,   304,   118,
     300,   303,   307,  -346,  1897,   570,  -346,   306,  -346,  -346,
    -346,  2007,   308,   309,   129,  -346,   129,  -346,   313,  -346,
       1,  -346,  1897,    18,  -346,  -346,   182,  -346,  -346,  -346,
    -346,  -346,  -346,  -346,   312,   320,   323,  -346,   341,  1897,
    -346,  -346,  1897,  1897,  -346,  -346,   350,  -346,  -346,   326,
     351,  -346,  1897,  1367,   118,  2007,   334,   360,  -346,   344,
     343,  1985,  -346,  1985,  -346,  1985,  2007,  -346,  -346,   348,
     349,  1738,   367,  1897,  1897,    70,  1897,  1897,   118,  -346,
     300,   303,   307,  -346,   354,  -346,  1897,  1155,   118,  -346,
     395,  1897,  -346,  -346,   176,  -346,   118,  1420,  -346,   356,
     373,   361,  -346,  -346,   363,   364,   373,   365,  -346,   257,
    -346,  -346,   355,   375,   380,   254,  -346,  -346,   372,   140,
    -346,  -346,   129,   370,  1473,  1473,  -346,  1897,  -346,  -346,
     401,  1897,  -346,   118,   300,   303,  -346,  1985,  1950,  -346,
    -346,  -346,  -346,   379,  -346,   399,   407,   392,  1897,  -346,
     400,  1791,   418,  -346,  -346,  -346,   398,   404,   406,  1208,
     409,  2007,   118,   300,   303,   307,   412,  1897,   307,   163,
    -346,   118,  -346,  -346,   118,   420,   423,    90,    90,   429,
     103,   103,   416,  -346,   868,  -346,  -346,   129,   413,  -346,
     129,   225,   430,   425,    29,   436,  -346,  1897,  -346,   118,
     431,   433,  -346,  -346,  -346,  1897,  1897,   455,  2007,   457,
     464,   448,  1897,   118,  -346,   118,  1897,  -346,  -346,  -346,
    -346,  -346,   446,   449,   450,  -346,  -346,  -346,  1897,  -346,
    -346,  -346,    90,    90,  -346,  -346,  -346,  -346,  -346,  -346,
     231,  -346,   129,   273,  -346,   282,  -346,   129,  -346,   468,
    -346,   472,  1897,  1897,  -346,  1473,  -346,  1897,   480,  -346,
    -346,   118,  -346,  2007,  2007,  1897,  1897,  1897,   484,  2007,
    -346,  -346,  -346,   118,  -346,   118,  2007,  -346,  -346,   216,
     216,   254,  -346,   103,   487,  -346,  -346,   416,   488,  -346,
    1897,   425,   425,  -346,   425,  1897,  -346,  2007,  2007,  2007,
    1897,  -346,  -346,   216,   216,  -346,  1844,   473,  1844,   881,
    -346,   792,  -346,   792,   425,  -346,  -346,  -346,   425,  2007,
    1844,  1844,  1897,   477,  -346,   471,  -346,   479,  -346,  -346,
    -346,  -346,  -346,   481,   483,  1989,  -346,  -346,  -346,  -346,
    -346,  -346
};

/* YYDEFACT[STATE-NUM] -- Default reduction number in state STATE-NUM.
   Performed when YYTABLE does not specify something else to do.  Zero
   means the default is an error.  */
static const yytype_int16 yydefact[] =
{
       5,    48,     0,     0,     2,     3,     7,   287,   274,   276,
     278,   295,   296,   294,   289,   310,     0,   236,   293,   291,
     292,   297,   280,   215,   288,     0,   229,     0,   215,   150,
     247,     0,     0,     0,    41,     4,    33,    97,    94,   129,
     106,   131,   130,   147,    27,    48,    93,    95,    92,   186,
      48,   212,    48,   196,   188,   281,   282,   285,   189,   187,
     132,   193,   194,   195,   190,   192,   191,   104,    96,     0,
       1,     6,    14,     8,     0,   238,   240,     0,   237,   236,
     229,   210,   275,   277,   279,   286,     0,    29,   231,    31,
     247,   134,     0,     0,   210,   307,   152,     0,    29,     0,
       0,     0,   298,   302,   300,   301,   303,   304,   229,   299,
     306,     0,     0,    48,    48,   245,    29,     0,   308,   309,
       0,    27,    99,   264,     0,   173,   175,     0,     0,   302,
     305,     0,   309,   143,    28,   149,    34,    40,   213,    39,
      48,   284,   283,     0,     0,    56,    50,    53,    52,   150,
       0,    65,    21,     0,    12,     0,   214,     0,     0,     0,
       0,   221,     0,   237,     0,    29,    48,     0,   223,    30,
       0,    32,     0,    80,    82,    48,     0,   100,    30,   151,
     154,   120,     0,     0,     0,   101,   119,     0,     0,    98,
       0,     0,    48,    30,   248,   103,   244,     0,    28,    49,
       0,   264,   258,   266,   207,   206,     0,    29,   197,   198,
     202,     0,   203,   204,   205,   199,   201,   200,     0,     0,
       0,   264,     0,   158,     0,     0,     0,    54,     0,     0,
     148,    38,   136,     0,     0,    29,    29,    51,     0,    54,
      29,    29,    29,   162,     0,     0,    15,     0,    13,    16,
     239,   241,     0,     0,     0,   216,     0,   218,     0,   211,
       0,   232,     0,     0,   234,   185,   177,   178,   182,   183,
     184,   179,   181,   180,     0,     0,     0,   153,   155,     0,
     124,   102,   125,     0,   121,   243,     0,    37,    36,     0,
       0,   246,     0,     0,    57,   137,     0,     0,   256,    29,
       0,     0,   260,    30,   265,   264,   140,   174,   176,     0,
       0,     0,   105,     0,     0,     0,     0,     0,    54,   158,
      29,    29,    29,   168,    29,   162,     0,     0,    55,    78,
       0,     0,   145,   144,   135,   160,    58,    30,   171,     0,
      30,     0,    64,    66,     0,     0,    30,     0,   170,   303,
      11,    22,     0,     0,    14,    21,   242,   222,     0,     0,
     209,   224,     0,     0,     0,     0,   208,     0,   156,   126,
       0,   123,    35,     0,    29,    29,   262,     0,    30,   259,
     253,   255,   267,     0,   261,   108,   107,     0,     0,   159,
       0,     0,   133,   160,   167,    70,     0,     0,     0,    30,
       0,   138,    54,    29,    29,    29,     0,     0,    29,    54,
      62,    54,    69,   163,    54,     0,     0,   215,   215,     0,
     268,   268,   272,    17,     0,   217,   219,     0,     0,   235,
       0,     0,     0,    84,   187,     0,   157,   127,   122,    60,
       0,     0,   254,   257,   263,     0,     0,   109,   141,   114,
     113,     0,     0,    54,    74,    54,     0,   164,   169,   165,
      79,    75,     0,     0,     0,   146,   161,   172,     0,    59,
      68,    67,   215,   215,   210,   210,    16,   269,   271,   270,
       0,   249,     0,     0,   273,    29,   226,     0,     9,     0,
     225,     0,     0,     0,    81,     0,    89,     0,     0,    83,
     128,    54,    63,   111,   110,     0,     0,     0,   115,   142,
      73,    71,   166,    54,    77,    54,   139,   210,   210,    44,
      44,    21,    19,   268,   251,    18,    20,   272,     0,   220,
       0,    84,    84,    85,    84,     0,    61,   112,   117,   116,
       0,    76,    72,    44,    44,   290,    46,    27,    46,     0,
     250,     0,   227,     0,    84,    91,    90,    86,    84,   118,
      46,    46,     0,     0,    42,    46,    45,     0,    10,   252,
     228,    88,    87,     0,     0,    27,    23,    43,    25,    24,
      26,    47
};

/* YYPGOTO[NTERM-NUM].  */
static const yytype_int16 yypgoto[] =
{
    -346,  -346,  -346,  -346,  -346,   521,  -346,   177,    56,  -346,
    -332,  -346,  -116,   -71,  -346,    96,  -346,  -222,  -345,   -32,
     491,   -48,  -109,  -168,  -346,   -46,  -346,  -346,  -319,  -343,
    -346,  -346,  -346,  -346,  -346,  -346,   -27,  -346,  -346,   284,
     388,  -346,  -113,  -149,  -130,   139,  -346,  -223,   264,  -346,
    -346,  -320,   260,  -346,  -346,  -195,  -346,   -73,    -5,    14,
     -26,   509,  -346,    16,   465,   467,   374,   469,   -11,   393,
     359,  -346,  -346,   128,    35,  -346,   178,  -346,  -346,  -164,
    -346,  -191,  -346,  -346,  -346,   -61,  -346,  -346,   -16,   -37,
       2,   340,  -346,   134,   252,   319,   358,   411,   445,  -346,
    -228,   552,    -6,  -346
};

/* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int16 yydefgoto[] =
{
       0,     3,     4,     5,    71,     6,   247,   155,   355,   423,
     245,   351,   135,   338,   172,    34,    35,   563,   546,   564,
      36,   328,   146,   329,    37,    38,   274,   275,   496,   432,
      39,    40,    41,    42,   111,   183,    43,   229,    44,    45,
      97,    98,   224,   236,   408,   323,   324,   243,   339,   124,
     125,   264,    46,    47,    48,   203,    49,   166,    50,   259,
      81,    52,   485,   486,    86,    87,    88,    77,    74,    75,
     115,   116,   117,   480,   481,   204,   298,   299,   300,    53,
     205,   206,   207,   482,   487,    54,    55,    56,    57,    58,
      59,    60,   547,    61,    62,    63,    64,    65,    66,    67,
     118,   132,   133,    68
};

/* YYTABLE[YYPACT[STATE-NUM]] -- What to do in state STATE-NUM.  If
   positive, shift that token.  If negative, reduce the rule whose
   number is the opposite.  If YYTABLE_NINF, syntax error.  */
static const yytype_int16 yytable[] =
{
      91,   241,    94,   325,   121,   199,    78,   297,   265,    85,
     296,   145,   151,   147,   147,    51,   170,   353,    76,    76,
     242,   175,   435,   424,   120,   113,     7,   179,    89,    14,
     309,   235,     7,   311,   122,   126,   237,   240,   141,   142,
     361,     7,   237,    14,    51,   194,     7,   140,   186,   156,
     157,   362,    24,    23,    14,   282,   187,   497,    24,   312,
     143,   313,   208,    79,   138,   123,    51,    24,   163,     7,
       8,   343,    24,  -230,    28,  -305,   188,   321,   189,    92,
     144,    76,    89,   283,   120,   225,   209,    85,    80,   226,
     123,   168,   391,   219,   170,    24,   322,  -230,    93,   147,
     330,    70,   169,    23,    79,   147,   381,   230,   382,   192,
      89,   266,   315,   320,   383,   257,   232,   331,   392,  -233,
     313,   416,   220,   413,    28,   210,   169,   138,    51,    80,
     293,   251,     7,   227,   128,   267,   304,  -233,   477,   478,
     208,   208,    23,   479,   375,  -233,   137,   171,   139,   294,
     395,   147,   533,   153,   138,  -233,   248,   158,    24,    76,
     208,   134,   253,    28,   209,   209,  -233,   332,     2,   260,
     154,   347,    89,   295,    31,   548,   459,   159,   404,   426,
     374,   147,   442,   297,   209,   237,   336,    23,   147,   549,
     427,   306,   141,   147,   468,     1,   353,   405,   560,   561,
     161,   157,   334,   210,   210,    22,   138,     2,    28,   190,
     191,    23,   555,   556,   403,   557,   152,   131,   143,   237,
     110,   307,   308,   210,   255,   157,   545,   237,   379,    16,
     333,   569,    28,   570,   461,   571,   231,   110,   144,   572,
     208,   469,   208,   470,   208,   160,   471,   352,   147,   176,
     141,   398,    23,   400,   492,   493,   358,   212,   359,   167,
       7,   177,   258,   185,   209,   178,   209,   420,   209,   421,
     422,   276,   147,    28,   195,   149,   193,    23,   200,   150,
     522,   523,   147,   221,   326,   510,    24,   511,   289,   102,
     147,   129,   104,   105,   106,   107,   130,   109,    28,   401,
     197,   110,   218,   210,   327,   210,   268,   210,   228,    82,
      83,    84,   254,    96,   112,   249,   208,   208,   256,   148,
     148,   353,   525,   523,   281,   439,   567,   147,    22,   287,
     237,   526,   169,   536,   464,   212,   212,   467,   573,   574,
     209,   209,    69,   302,    72,   541,   288,   542,   303,   305,
     337,   415,   342,   340,   354,   212,   147,   346,   357,   360,
     356,   448,   364,   147,   428,   147,   434,   434,   147,   366,
     365,   367,   371,   127,   112,   213,   173,   174,   372,   210,
     210,   187,   376,   180,   181,   184,   145,   265,   147,   265,
     377,   474,   475,   147,   378,   380,   384,   385,   388,    82,
     417,   519,   520,   196,   399,   148,    95,   147,   409,   147,
     153,   148,   223,   410,   527,   411,   412,   414,   503,   504,
     418,   425,   429,   437,   269,   509,   352,   444,   223,   489,
     445,   566,   491,    96,   223,   212,   498,   212,   446,   212,
     447,   516,   214,   252,   543,   544,   517,   518,   449,   452,
     453,   484,   490,   213,   213,   147,   454,   148,   455,   581,
     277,   460,   278,   211,   465,   472,   280,   147,   473,   147,
     284,   285,   286,   213,   476,   495,   494,   290,   537,   538,
     539,   215,   499,   501,   524,   502,   505,   148,   506,   528,
     266,   270,   266,   246,   148,   507,   508,   434,   513,   148,
     341,   514,   515,   530,   344,   345,   310,   529,   535,   223,
     319,   212,   212,   559,   267,   540,   267,   335,   551,   553,
     214,   214,   198,   576,   562,   578,    73,   579,   348,   580,
     271,   419,   521,   577,   216,   575,   136,   238,   458,   114,
     214,   211,   211,   552,   261,   164,   363,   165,   162,   483,
     250,   352,   291,   213,   148,   213,   443,   213,   550,   215,
     215,   211,     0,   368,     0,     0,   369,   370,   217,     0,
       0,     0,     0,     7,     0,     0,   196,   223,   148,   215,
       0,    99,   119,   272,   396,   397,     0,     0,   148,     0,
       0,     0,     0,     0,     0,   387,   148,   389,   390,    24,
     393,   394,   102,   127,   129,   104,   105,   349,   107,   130,
     109,   223,   216,   216,   110,   406,   350,   273,     0,     0,
     214,   389,   214,     0,   214,     0,     0,     0,     0,   213,
     213,     0,   216,   148,     0,     0,     0,     0,   440,   441,
       0,   211,   119,   211,     0,   211,   217,   217,   433,   433,
       0,   436,     0,     0,     0,   438,     0,     0,     0,   215,
       0,   215,   148,   215,     0,     0,   217,   462,   463,   148,
       0,   148,     0,     0,   148,   451,     0,     0,     0,     0,
       0,     0,     0,   457,     0,   268,     0,   268,     0,     0,
       0,   466,     0,     0,   148,     0,   214,   214,     0,   148,
       0,    99,   244,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   216,   148,   216,   148,   216,   211,   211,     0,
       0,   500,     0,     0,     0,     0,     0,     0,     0,     0,
     279,     0,     0,     0,     0,   215,   215,     0,     0,     0,
     512,     0,     0,     0,     0,   292,   217,     0,   217,     0,
     217,     0,     0,     0,   301,     8,     9,    10,    11,    12,
      13,   148,     0,     0,     0,     0,     0,     0,    18,    19,
      20,     0,     0,   148,    22,   148,   531,   532,   244,   433,
       0,   534,     0,     0,     0,    25,     0,     0,   216,   216,
       0,     0,     0,     0,     0,     0,     8,     9,    10,    11,
      12,    13,   262,   269,     0,   269,   263,     0,     0,    18,
      19,    20,     0,     0,   554,    22,     0,     0,     0,   558,
       0,     0,   217,   217,     0,     0,    25,     0,     0,     0,
     565,     0,   565,     0,     7,     8,     9,    10,    11,    12,
      13,     0,    14,    15,   565,   565,     0,   263,    18,    19,
      20,    21,     0,     0,    22,    23,     0,     0,     0,     0,
      24,     0,     0,     0,    95,    25,     0,     0,   316,     0,
     270,     7,   270,    27,     0,   317,    28,     0,    29,   244,
       0,     0,    90,   318,     7,    32,    33,     0,     0,   244,
       0,     0,   244,     0,     0,     0,     0,    24,   244,     0,
     102,     0,   129,   104,   105,   349,   107,   130,   109,   271,
      24,   271,   110,   102,   488,   129,   104,   105,   349,   107,
     130,   109,     0,     0,     0,   110,     0,   568,     0,     0,
     301,     0,     0,     0,     0,     0,     0,     0,     0,     7,
       8,     9,    10,    11,    12,    13,     0,    14,    15,    16,
      17,   244,     0,    18,    19,    20,    21,     0,   100,    22,
      23,     0,   272,     0,   272,    24,   101,     0,   102,    95,
     103,   104,   105,   106,   107,   108,   109,     0,    27,     0,
     110,    28,     0,    29,     0,     0,     0,    90,     0,     0,
      32,    33,     0,     0,     0,     0,   273,     0,   273,     7,
       8,     9,    10,    11,    12,    13,     0,    14,    15,     0,
       0,     0,     0,    18,    19,    20,    21,     0,   100,    22,
      23,     0,     0,     0,     0,    24,   101,     0,   102,    95,
     103,   104,   105,   106,   107,   130,   109,     0,    27,     0,
     110,    28,     0,    29,     0,     0,     0,    90,     0,     0,
      32,    33,     7,     8,     9,    10,    11,    12,    13,     0,
      14,    15,    16,    17,     0,     0,    18,    19,    20,    21,
       0,     0,    22,    23,     0,     0,     0,     0,    24,     0,
       0,     0,     0,    25,     0,     0,     0,     0,    26,     0,
       0,    27,     0,     0,    28,     0,    29,     0,     0,     0,
      30,     0,    31,    32,    33,     7,     8,     9,    10,    11,
      12,    13,     0,    14,    15,     0,     0,     0,     0,    18,
      19,    20,    21,     0,     0,    22,    23,     0,     0,     0,
       0,    24,     0,     0,     0,    95,    25,     0,     0,   233,
       0,     0,     0,     0,    27,     0,     0,    28,     0,    29,
       0,     0,     0,    90,   239,     0,    32,    33,     7,     8,
       9,    10,    11,    12,    13,     0,    14,    15,     0,     0,
       0,     0,    18,    19,    20,    21,     0,     0,    22,    23,
       0,     0,     0,     0,    24,     0,     0,     0,    95,    25,
       0,     0,   233,     0,     0,     0,     0,    27,     0,     0,
      28,     0,    29,     0,     0,     0,    90,   402,     0,    32,
      33,     7,     8,     9,    10,    11,    12,    13,     0,    14,
      15,     0,     0,     0,     0,    18,    19,    20,    21,     0,
       0,    22,    23,     0,     0,     0,     0,    24,     0,     0,
       0,    95,    25,     0,     0,   456,     0,     0,     0,     0,
      27,     0,   317,    28,     0,    29,     0,     0,     0,    90,
       0,     0,    32,    33,     7,     8,     9,    10,    11,    12,
      13,     0,    14,    15,    16,     0,     0,     0,    18,    19,
      20,    21,     0,     0,    22,    23,     0,     0,     0,     0,
      24,     0,     0,     0,     0,    25,     0,     0,     0,     0,
       0,     0,     0,    27,     0,     0,    28,     0,    29,     0,
       0,     0,    90,     0,    31,    32,    33,     7,     8,     9,
      10,    11,    12,    13,     0,    14,    15,     0,     0,     0,
       0,    18,    19,    20,    21,     0,     0,    22,    23,     0,
       0,     0,     0,    24,     0,     0,     0,     0,    25,     0,
       0,   233,     0,     0,     0,     0,    27,     0,     0,    28,
       0,    29,     0,     0,     0,    90,   234,     0,    32,    33,
       7,     8,     9,    10,    11,    12,    13,     0,    14,    15,
       0,     0,     0,     0,    18,    19,    20,    21,     0,     0,
      22,    23,     0,     0,     0,     0,    24,     0,     0,     0,
       0,    25,     0,     0,   233,     0,     0,     0,     0,    27,
       0,     0,    28,     0,    29,     0,     0,     0,    90,   373,
       0,    32,    33,     7,     8,     9,    10,    11,    12,    13,
       0,    14,    15,     0,     0,     0,     0,    18,    19,    20,
      21,     0,     0,    22,    23,     0,     0,     0,     0,    24,
       0,     0,     0,    95,    25,     0,     0,   407,     0,     0,
       0,     0,    27,     0,     0,    28,     0,    29,     0,     0,
       0,    90,     0,     0,    32,    33,     7,     8,     9,    10,
      11,    12,    13,     0,    14,    15,   430,     0,     0,     0,
      18,    19,    20,    21,     0,     0,    22,    23,     0,     0,
       0,     0,    24,   431,     0,     0,     0,    25,     0,     0,
       0,     0,     0,     0,     0,    27,     0,     0,    28,     0,
      29,     0,     0,     0,    90,     0,     0,    32,    33,     7,
       8,     9,    10,    11,    12,    13,     0,    14,    15,     0,
       0,     0,     0,    18,    19,    20,    21,     0,     0,    22,
      23,     0,     0,     0,     0,    24,     0,     0,     0,    95,
      25,     0,     0,     0,     0,     0,     0,     0,    27,     0,
       0,    28,     0,    29,     0,     0,     0,    90,     0,     0,
      32,    33,     7,     8,     9,    10,    11,    12,    13,     0,
      14,    15,     0,     0,     0,     0,    18,    19,    20,    21,
       0,   182,    22,    23,     0,     0,     0,     0,    24,     0,
       0,     0,     0,    25,     0,     0,     0,     0,     0,     0,
       0,    27,     0,     0,    28,     0,    29,     0,     0,     0,
      90,     0,     0,    32,    33,     7,     8,     9,    10,    11,
      12,    13,     0,    14,    15,     0,     0,     0,     0,    18,
      19,    20,    21,     0,   222,    22,    23,     0,     0,     0,
       0,    24,     0,     0,     0,     0,    25,     0,     0,     0,
       0,     0,     0,     0,    27,     0,     0,    28,     0,    29,
       0,     0,     0,    90,     0,     0,    32,    33,     7,     8,
       9,    10,    11,    12,    13,     0,    14,    15,     0,     0,
       0,     0,    18,    19,    20,    21,     0,   314,    22,    23,
       0,     0,     0,     0,    24,     0,     0,     0,     0,    25,
       0,     0,     0,     0,     0,     0,     0,    27,     0,     0,
      28,     0,    29,     0,     0,     0,    90,     0,     0,    32,
      33,     7,     8,     9,    10,    11,    12,    13,     0,    14,
      15,     0,     0,     0,     0,    18,    19,    20,    21,     0,
       0,    22,    23,     0,     0,     0,     0,    24,     0,     0,
       0,     0,    25,     0,     0,     0,     0,     0,     0,     0,
      27,     0,     0,    28,     0,    29,   386,     0,     0,    90,
       0,     0,    32,    33,     7,     8,     9,    10,    11,    12,
      13,     0,    14,    15,     0,     0,     0,     0,    18,    19,
      20,    21,     0,     0,    22,    23,     0,     0,     0,     0,
      24,     0,     0,     0,     0,    25,     0,     0,     0,     0,
       0,     0,     0,    27,     0,     0,    28,     0,    29,   450,
       0,     0,    90,     0,     0,    32,    33,     7,     8,     9,
      10,    11,    12,    13,     0,    14,    15,     0,     0,     0,
       0,    18,    19,    20,    21,     0,     0,    22,    23,     0,
       0,     0,     0,    24,     0,     0,     0,     0,    25,     0,
       0,     0,     0,     0,     0,     0,    27,     0,     0,    28,
       0,    29,     0,     0,     0,    90,     0,   562,    32,    33,
       7,     8,     9,    10,    11,    12,    13,     0,    14,    15,
       0,     0,     0,     0,    18,    19,    20,    21,     0,     0,
      22,    23,     0,     0,     0,     0,    24,     0,     0,     0,
       0,    25,     0,     0,     0,     0,     0,     0,     0,    27,
       0,     0,    28,     0,    29,     0,     0,     0,    90,     0,
       0,    32,    33,     7,     8,     9,    10,    11,    12,    13,
       0,    14,     0,     0,     0,     0,     0,    18,    19,    20,
       0,     0,     0,    22,     0,     0,     0,     0,     0,    24,
       0,     0,     0,    95,    25,     0,     0,     0,     7,     8,
       9,    10,    11,    12,    13,     0,    14,   201,     0,     0,
       0,   202,    18,    19,    20,     0,     0,     0,    22,     0,
       0,     0,     0,     0,    24,     0,     0,     0,     0,    25,
       0,   102,    95,   129,   104,   105,   106,   107,   130,   109,
     131,     0,   201,   110,     0,     0,   202,     0,   198,   102,
      95,   129,   104,   105,   106,   107,   130,   109,   131,     0,
       0,   110
};

static const yytype_int16 yycheck[] =
{
      27,   150,    28,   226,    31,   121,    17,   202,   172,    25,
     201,    59,    60,    59,    60,     1,    87,   245,    16,    17,
     150,    94,   365,   355,    30,    30,     3,    98,    26,    11,
     221,   144,     3,    22,    32,    33,   145,   150,    54,    55,
      39,     3,   151,    11,    30,   116,     3,    52,    22,    49,
      50,    50,    29,    24,    11,    22,    30,    28,    29,    48,
      31,    50,   123,    14,    50,    47,    52,    29,    79,     3,
       4,   239,    29,    21,    45,    52,    50,   226,    52,    30,
      51,    79,    80,    50,    90,    47,   123,   103,    39,    51,
      47,    39,    22,    21,   165,    29,   226,    21,    49,   145,
      34,     0,    50,    24,    14,   151,   301,   134,   303,   114,
     108,   172,   225,   226,   305,    39,   143,    51,    48,     3,
      50,   349,    50,   346,    45,   123,    50,   113,   114,    39,
      51,   158,     3,   131,    47,   172,   207,    21,    35,    36,
     201,   202,    24,    40,   293,    29,    50,    31,    52,   197,
     318,   197,   495,    30,   140,    39,   154,    31,    29,   157,
     221,    49,   160,    45,   201,   202,    50,   228,    38,   167,
      47,   242,   170,   200,    53,   520,   399,    51,   327,    39,
     293,   227,   377,   378,   221,   294,   234,    24,   234,   521,
      50,   218,   208,   239,    31,    26,   424,   327,   543,   544,
      49,    50,   229,   201,   202,    23,   192,    38,    45,   113,
     114,    24,   531,   532,   327,   534,    45,    41,    31,   328,
      44,   219,   220,   221,    49,    50,    10,   336,   299,    13,
     228,   551,    45,   553,   402,   554,   140,    44,    51,   558,
     301,   409,   303,   411,   305,    21,   414,   245,   294,    30,
     266,   322,    24,   324,    29,    30,   254,   123,   256,    21,
       3,    48,   166,    52,   301,    50,   303,    13,   305,    15,
      16,   175,   318,    45,    52,    47,    50,    24,    31,    51,
      49,    50,   328,    47,    31,   453,    29,   455,   192,    32,
     336,    34,    35,    36,    37,    38,    39,    40,    45,   326,
      52,    44,    31,   301,    51,   303,   172,   305,    41,     4,
       5,     6,    21,    29,    30,    45,   377,   378,    21,    59,
      60,   549,    49,    50,    52,   373,   548,   373,    23,    52,
     439,    49,    50,   501,   405,   201,   202,   408,   560,   561,
     377,   378,     2,    48,     4,   513,    52,   515,    50,    47,
      50,   349,    48,    50,    48,   221,   402,    50,    49,    46,
      52,   388,    50,   409,   362,   411,   364,   365,   414,    46,
      50,    30,    22,    33,    90,   123,    92,    93,    52,   377,
     378,    30,    48,    99,   100,   101,   434,   551,   434,   553,
      30,   417,   418,   439,    50,    52,    48,    48,    31,     4,
      45,   474,   475,   119,    50,   145,    33,   453,    52,   455,
      30,   151,   128,    52,   485,    52,    52,    52,   445,   446,
      45,    49,    52,    22,   172,   452,   424,    48,   144,   427,
      31,   547,   430,   149,   150,   301,   434,   303,    31,   305,
      48,   468,   123,   159,   517,   518,   472,   473,    48,    31,
      52,    35,    39,   201,   202,   501,    52,   197,    52,   575,
     176,    52,   178,   123,    52,    45,   182,   513,    45,   515,
     186,   187,   188,   221,    45,    50,    46,   193,   505,   506,
     507,   123,    46,    52,   482,    52,    31,   227,    31,   487,
     551,   172,   553,   153,   234,    31,    48,   495,    52,   239,
     236,    52,    52,    31,   240,   241,   222,    39,    28,   225,
     226,   377,   378,   540,   551,    31,   553,   233,    31,    31,
     201,   202,    49,    46,    53,    46,     5,    46,   244,    46,
     172,   354,   476,   565,   123,   562,    45,   149,   399,    30,
     221,   201,   202,   527,   170,    80,   262,    80,    79,   421,
     157,   549,   193,   301,   294,   303,   378,   305,   523,   201,
     202,   221,    -1,   279,    -1,    -1,   282,   283,   123,    -1,
      -1,    -1,    -1,     3,    -1,    -1,   292,   293,   318,   221,
      -1,    29,    30,   172,   320,   321,    -1,    -1,   328,    -1,
      -1,    -1,    -1,    -1,    -1,   311,   336,   313,   314,    29,
     316,   317,    32,   263,    34,    35,    36,    37,    38,    39,
      40,   327,   201,   202,    44,   331,    46,   172,    -1,    -1,
     301,   337,   303,    -1,   305,    -1,    -1,    -1,    -1,   377,
     378,    -1,   221,   373,    -1,    -1,    -1,    -1,   374,   375,
      -1,   301,    90,   303,    -1,   305,   201,   202,   364,   365,
      -1,   367,    -1,    -1,    -1,   371,    -1,    -1,    -1,   301,
      -1,   303,   402,   305,    -1,    -1,   221,   403,   404,   409,
      -1,   411,    -1,    -1,   414,   391,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   399,    -1,   551,    -1,   553,    -1,    -1,
      -1,   407,    -1,    -1,   434,    -1,   377,   378,    -1,   439,
      -1,   149,   150,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   301,   453,   303,   455,   305,   377,   378,    -1,
      -1,   437,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     178,    -1,    -1,    -1,    -1,   377,   378,    -1,    -1,    -1,
     456,    -1,    -1,    -1,    -1,   193,   301,    -1,   303,    -1,
     305,    -1,    -1,    -1,   202,     4,     5,     6,     7,     8,
       9,   501,    -1,    -1,    -1,    -1,    -1,    -1,    17,    18,
      19,    -1,    -1,   513,    23,   515,   492,   493,   226,   495,
      -1,   497,    -1,    -1,    -1,    34,    -1,    -1,   377,   378,
      -1,    -1,    -1,    -1,    -1,    -1,     4,     5,     6,     7,
       8,     9,    51,   551,    -1,   553,    55,    -1,    -1,    17,
      18,    19,    -1,    -1,   530,    23,    -1,    -1,    -1,   535,
      -1,    -1,   377,   378,    -1,    -1,    34,    -1,    -1,    -1,
     546,    -1,   548,    -1,     3,     4,     5,     6,     7,     8,
       9,    -1,    11,    12,   560,   561,    -1,    55,    17,    18,
      19,    20,    -1,    -1,    23,    24,    -1,    -1,    -1,    -1,
      29,    -1,    -1,    -1,    33,    34,    -1,    -1,    37,    -1,
     551,     3,   553,    42,    -1,    44,    45,    -1,    47,   327,
      -1,    -1,    51,    52,     3,    54,    55,    -1,    -1,   337,
      -1,    -1,   340,    -1,    -1,    -1,    -1,    29,   346,    -1,
      32,    -1,    34,    35,    36,    37,    38,    39,    40,   551,
      29,   553,    44,    32,    46,    34,    35,    36,    37,    38,
      39,    40,    -1,    -1,    -1,    44,    -1,    46,    -1,    -1,
     378,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,     3,
       4,     5,     6,     7,     8,     9,    -1,    11,    12,    13,
      14,   399,    -1,    17,    18,    19,    20,    -1,    22,    23,
      24,    -1,   551,    -1,   553,    29,    30,    -1,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    -1,    42,    -1,
      44,    45,    -1,    47,    -1,    -1,    -1,    51,    -1,    -1,
      54,    55,    -1,    -1,    -1,    -1,   551,    -1,   553,     3,
       4,     5,     6,     7,     8,     9,    -1,    11,    12,    -1,
      -1,    -1,    -1,    17,    18,    19,    20,    -1,    22,    23,
      24,    -1,    -1,    -1,    -1,    29,    30,    -1,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    -1,    42,    -1,
      44,    45,    -1,    47,    -1,    -1,    -1,    51,    -1,    -1,
      54,    55,     3,     4,     5,     6,     7,     8,     9,    -1,
      11,    12,    13,    14,    -1,    -1,    17,    18,    19,    20,
      -1,    -1,    23,    24,    -1,    -1,    -1,    -1,    29,    -1,
      -1,    -1,    -1,    34,    -1,    -1,    -1,    -1,    39,    -1,
      -1,    42,    -1,    -1,    45,    -1,    47,    -1,    -1,    -1,
      51,    -1,    53,    54,    55,     3,     4,     5,     6,     7,
       8,     9,    -1,    11,    12,    -1,    -1,    -1,    -1,    17,
      18,    19,    20,    -1,    -1,    23,    24,    -1,    -1,    -1,
      -1,    29,    -1,    -1,    -1,    33,    34,    -1,    -1,    37,
      -1,    -1,    -1,    -1,    42,    -1,    -1,    45,    -1,    47,
      -1,    -1,    -1,    51,    52,    -1,    54,    55,     3,     4,
       5,     6,     7,     8,     9,    -1,    11,    12,    -1,    -1,
      -1,    -1,    17,    18,    19,    20,    -1,    -1,    23,    24,
      -1,    -1,    -1,    -1,    29,    -1,    -1,    -1,    33,    34,
      -1,    -1,    37,    -1,    -1,    -1,    -1,    42,    -1,    -1,
      45,    -1,    47,    -1,    -1,    -1,    51,    52,    -1,    54,
      55,     3,     4,     5,     6,     7,     8,     9,    -1,    11,
      12,    -1,    -1,    -1,    -1,    17,    18,    19,    20,    -1,
      -1,    23,    24,    -1,    -1,    -1,    -1,    29,    -1,    -1,
      -1,    33,    34,    -1,    -1,    37,    -1,    -1,    -1,    -1,
      42,    -1,    44,    45,    -1,    47,    -1,    -1,    -1,    51,
      -1,    -1,    54,    55,     3,     4,     5,     6,     7,     8,
       9,    -1,    11,    12,    13,    -1,    -1,    -1,    17,    18,
      19,    20,    -1,    -1,    23,    24,    -1,    -1,    -1,    -1,
      29,    -1,    -1,    -1,    -1,    34,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    42,    -1,    -1,    45,    -1,    47,    -1,
      -1,    -1,    51,    -1,    53,    54,    55,     3,     4,     5,
       6,     7,     8,     9,    -1,    11,    12,    -1,    -1,    -1,
      -1,    17,    18,    19,    20,    -1,    -1,    23,    24,    -1,
      -1,    -1,    -1,    29,    -1,    -1,    -1,    -1,    34,    -1,
      -1,    37,    -1,    -1,    -1,    -1,    42,    -1,    -1,    45,
      -1,    47,    -1,    -1,    -1,    51,    52,    -1,    54,    55,
       3,     4,     5,     6,     7,     8,     9,    -1,    11,    12,
      -1,    -1,    -1,    -1,    17,    18,    19,    20,    -1,    -1,
      23,    24,    -1,    -1,    -1,    -1,    29,    -1,    -1,    -1,
      -1,    34,    -1,    -1,    37,    -1,    -1,    -1,    -1,    42,
      -1,    -1,    45,    -1,    47,    -1,    -1,    -1,    51,    52,
      -1,    54,    55,     3,     4,     5,     6,     7,     8,     9,
      -1,    11,    12,    -1,    -1,    -1,    -1,    17,    18,    19,
      20,    -1,    -1,    23,    24,    -1,    -1,    -1,    -1,    29,
      -1,    -1,    -1,    33,    34,    -1,    -1,    37,    -1,    -1,
      -1,    -1,    42,    -1,    -1,    45,    -1,    47,    -1,    -1,
      -1,    51,    -1,    -1,    54,    55,     3,     4,     5,     6,
       7,     8,     9,    -1,    11,    12,    13,    -1,    -1,    -1,
      17,    18,    19,    20,    -1,    -1,    23,    24,    -1,    -1,
      -1,    -1,    29,    30,    -1,    -1,    -1,    34,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    42,    -1,    -1,    45,    -1,
      47,    -1,    -1,    -1,    51,    -1,    -1,    54,    55,     3,
       4,     5,     6,     7,     8,     9,    -1,    11,    12,    -1,
      -1,    -1,    -1,    17,    18,    19,    20,    -1,    -1,    23,
      24,    -1,    -1,    -1,    -1,    29,    -1,    -1,    -1,    33,
      34,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    42,    -1,
      -1,    45,    -1,    47,    -1,    -1,    -1,    51,    -1,    -1,
      54,    55,     3,     4,     5,     6,     7,     8,     9,    -1,
      11,    12,    -1,    -1,    -1,    -1,    17,    18,    19,    20,
      -1,    22,    23,    24,    -1,    -1,    -1,    -1,    29,    -1,
      -1,    -1,    -1,    34,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    42,    -1,    -1,    45,    -1,    47,    -1,    -1,    -1,
      51,    -1,    -1,    54,    55,     3,     4,     5,     6,     7,
       8,     9,    -1,    11,    12,    -1,    -1,    -1,    -1,    17,
      18,    19,    20,    -1,    22,    23,    24,    -1,    -1,    -1,
      -1,    29,    -1,    -1,    -1,    -1,    34,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    42,    -1,    -1,    45,    -1,    47,
      -1,    -1,    -1,    51,    -1,    -1,    54,    55,     3,     4,
       5,     6,     7,     8,     9,    -1,    11,    12,    -1,    -1,
      -1,    -1,    17,    18,    19,    20,    -1,    22,    23,    24,
      -1,    -1,    -1,    -1,    29,    -1,    -1,    -1,    -1,    34,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    42,    -1,    -1,
      45,    -1,    47,    -1,    -1,    -1,    51,    -1,    -1,    54,
      55,     3,     4,     5,     6,     7,     8,     9,    -1,    11,
      12,    -1,    -1,    -1,    -1,    17,    18,    19,    20,    -1,
      -1,    23,    24,    -1,    -1,    -1,    -1,    29,    -1,    -1,
      -1,    -1,    34,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      42,    -1,    -1,    45,    -1,    47,    48,    -1,    -1,    51,
      -1,    -1,    54,    55,     3,     4,     5,     6,     7,     8,
       9,    -1,    11,    12,    -1,    -1,    -1,    -1,    17,    18,
      19,    20,    -1,    -1,    23,    24,    -1,    -1,    -1,    -1,
      29,    -1,    -1,    -1,    -1,    34,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    42,    -1,    -1,    45,    -1,    47,    48,
      -1,    -1,    51,    -1,    -1,    54,    55,     3,     4,     5,
       6,     7,     8,     9,    -1,    11,    12,    -1,    -1,    -1,
      -1,    17,    18,    19,    20,    -1,    -1,    23,    24,    -1,
      -1,    -1,    -1,    29,    -1,    -1,    -1,    -1,    34,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    42,    -1,    -1,    45,
      -1,    47,    -1,    -1,    -1,    51,    -1,    53,    54,    55,
       3,     4,     5,     6,     7,     8,     9,    -1,    11,    12,
      -1,    -1,    -1,    -1,    17,    18,    19,    20,    -1,    -1,
      23,    24,    -1,    -1,    -1,    -1,    29,    -1,    -1,    -1,
      -1,    34,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    42,
      -1,    -1,    45,    -1,    47,    -1,    -1,    -1,    51,    -1,
      -1,    54,    55,     3,     4,     5,     6,     7,     8,     9,
      -1,    11,    -1,    -1,    -1,    -1,    -1,    17,    18,    19,
      -1,    -1,    -1,    23,    -1,    -1,    -1,    -1,    -1,    29,
      -1,    -1,    -1,    33,    34,    -1,    -1,    -1,     3,     4,
       5,     6,     7,     8,     9,    -1,    11,    47,    -1,    -1,
      -1,    51,    17,    18,    19,    -1,    -1,    -1,    23,    -1,
      -1,    -1,    -1,    -1,    29,    -1,    -1,    -1,    -1,    34,
      -1,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    -1,    47,    44,    -1,    -1,    51,    -1,    49,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    -1,
      -1,    44
};

/* YYSTOS[STATE-NUM] -- The symbol kind of the accessing symbol of
   state STATE-NUM.  */
static const yytype_uint8 yystos[] =
{
       0,    26,    38,    57,    58,    59,    61,     3,     4,     5,
       6,     7,     8,     9,    11,    12,    13,    14,    17,    18,
      19,    20,    23,    24,    29,    34,    39,    42,    45,    47,
      51,    53,    54,    55,    71,    72,    76,    80,    81,    86,
      87,    88,    89,    92,    94,    95,   108,   109,   110,   112,
     114,   115,   117,   135,   141,   142,   143,   144,   145,   146,
     147,   149,   150,   151,   152,   153,   154,   155,   159,   147,
       0,    60,   147,    61,   124,   125,   146,   123,   124,    14,
      39,   116,     4,     5,     6,   144,   120,   121,   122,   146,
      51,    92,    30,    49,   116,    33,    95,    96,    97,   157,
      22,    30,    32,    34,    35,    36,    37,    38,    39,    40,
      44,    90,    95,   114,   117,   126,   127,   128,   156,   157,
     158,    92,   146,    47,   105,   106,   146,   147,    47,    34,
      39,    41,   157,   158,    49,    68,    76,    71,   115,    71,
     114,   144,   144,    31,    51,    77,    78,    81,   108,    47,
      51,    77,    45,    30,    47,    63,    49,    50,    31,    51,
      21,    49,   123,   124,   120,   121,   113,    21,    39,    50,
      69,    31,    70,    95,    95,   113,    30,    48,    50,    69,
      95,    95,    22,    91,    95,    52,    22,    30,    50,    52,
      71,    71,   114,    50,    69,    52,    95,    52,    49,    68,
      31,    47,    51,   111,   131,   136,   137,   138,   141,   145,
     146,   147,   149,   150,   151,   152,   153,   154,    31,    21,
      50,    47,    22,    95,    98,    47,    51,   146,    41,    93,
      92,    71,    92,    37,    52,    98,    99,    78,    96,    52,
      98,    99,   100,   103,   157,    66,   147,    62,   146,    45,
     125,    92,    95,   146,    21,    49,    21,    39,    71,   115,
     146,   122,    51,    55,   107,   135,   141,   145,   149,   150,
     151,   152,   153,   154,    82,    83,    71,    95,    95,   157,
      95,    52,    22,    50,    95,    95,    95,    52,    52,    71,
      95,   126,   157,    51,    77,    92,   137,   111,   132,   133,
     134,   157,    48,    50,    69,    47,    92,   146,   146,   137,
      95,    22,    48,    50,    22,    98,    37,    44,    52,    95,
      98,    99,   100,   101,   102,   103,    31,    51,    77,    79,
      34,    51,   141,   146,    92,    95,    77,    50,    69,   104,
      50,   104,    48,    79,   104,   104,    50,    69,    95,    37,
      46,    67,   146,   156,    48,    64,    52,    49,   146,   146,
      46,    39,    50,    95,    50,    50,    46,    30,    95,    95,
      95,    22,    52,    52,    98,    99,    48,    30,    50,    69,
      52,   111,   111,   137,    48,    48,    48,    95,    31,    95,
      95,    22,    48,    95,    95,    79,   104,   104,    69,    50,
      69,    92,    52,    98,    99,   100,    95,    37,   100,    52,
      52,    52,    52,   103,    52,   146,   156,    45,    45,    63,
      13,    15,    16,    65,    66,    49,    39,    50,   146,    52,
      13,    30,    85,    95,   146,    85,    95,    22,    95,    77,
     104,   104,   111,   132,    48,    31,    31,    48,    92,    48,
      48,    95,    31,    52,    52,    52,    37,    95,   101,   103,
      52,    79,   104,   104,    69,    52,    95,    69,    31,    79,
      79,    79,    45,    45,   116,   116,    45,    35,    36,    40,
     129,   130,   139,   129,    35,   118,   119,   140,    46,   146,
      39,   146,    29,    30,    46,    50,    84,    28,   146,    46,
      95,    52,    52,    92,    92,    31,    31,    31,    48,    92,
      79,    79,    95,    52,    52,    52,    92,   116,   116,   113,
     113,    64,    49,    50,   146,    49,    49,    69,   146,    39,
      31,    95,    95,    85,    95,    28,    79,    92,    92,    92,
      31,    79,    79,   113,   113,    10,    74,   148,    74,    66,
     130,    31,   119,    31,    95,    84,    84,    84,    95,    92,
      74,    74,    53,    73,    75,    95,    68,    73,    46,   107,
     107,    84,    84,    73,    73,    92,    46,    75,    46,    46,
      46,    68
};

/* YYR1[RULE-NUM] -- Symbol kind of the left-hand side of rule RULE-NUM.  */
static const yytype_uint8 yyr1[] =
{
       0,    56,    57,    57,    57,    58,    58,    59,    59,    60,
      60,    61,    62,    62,    63,    63,    64,    64,    65,    65,
      65,    66,    66,    67,    67,    67,    67,    68,    68,    69,
      69,    70,    70,    71,    71,    72,    72,    72,    72,    72,
      72,    72,    73,    73,    74,    74,    75,    75,    76,    76,
      77,    77,    78,    78,    79,    79,    80,    80,    80,    80,
      80,    80,    80,    80,    80,    80,    80,    80,    80,    80,
      80,    80,    80,    80,    80,    80,    80,    80,    80,    80,
      82,    81,    83,    81,    84,    84,    85,    85,    85,    85,
      85,    85,    86,    86,    86,    86,    86,    86,    86,    86,
      86,    86,    86,    86,    86,    86,    86,    87,    87,    87,
      88,    88,    88,    89,    89,    89,    89,    89,    89,    90,
      90,    90,    90,    90,    91,    91,    91,    91,    91,    92,
      92,    92,    92,    92,    92,    92,    92,    92,    92,    92,
      92,    92,    92,    93,    93,    93,    93,    94,    94,    95,
      96,    96,    97,    97,    97,    97,    97,    97,    98,    98,
      99,    99,   100,   100,   101,   101,   101,   101,   102,   102,
     103,   104,   104,   105,   105,   106,   106,   107,   107,   107,
     107,   107,   107,   107,   107,   107,   108,   109,   110,   110,
     110,   110,   110,   110,   110,   110,   110,   111,   111,   111,
     111,   111,   111,   111,   111,   111,   111,   111,   112,   112,
     113,   113,   114,   114,   115,   116,   116,   116,   116,   116,
     116,   117,   117,   117,   117,   117,   118,   118,   119,   120,
     120,   121,   121,   122,   122,   122,   123,   123,   124,   124,
     125,   125,   125,   126,   126,   127,   127,   128,   128,   129,
     129,   130,   130,   131,   132,   132,   133,   133,   134,   134,
     135,   135,   136,   136,   137,   137,   138,   138,   139,   139,
     139,   139,   140,   140,   141,   141,   142,   142,   143,   143,
     144,   145,   145,   145,   145,   145,   145,   146,   146,   147,
     148,   149,   150,   151,   152,   153,   154,   155,   156,   156,
     156,   156,   156,   156,   156,   156,   156,   157,   158,   158,
     159
};

/* YYR2[RULE-NUM] -- Number of symbols on the right-hand side of rule RULE-NUM.  */
static const yytype_int8 yyr2[] =
{
       0,     2,     1,     1,     2,     0,     2,     1,     2,     6,
       9,     5,     0,     1,     0,     2,     0,     2,     3,     3,
       3,     0,     2,     7,     8,     7,     8,     0,     1,     0,
       1,     0,     1,     1,     2,     5,     4,     4,     3,     2,
       2,     1,     1,     2,     0,     2,     0,     3,     0,     3,
       1,     2,     1,     1,     0,     1,     2,     4,     4,     6,
       6,     8,     5,     7,     4,     2,     4,     6,     6,     5,
       5,     7,     8,     7,     6,     6,     8,     7,     4,     6,
       0,     7,     0,     7,     0,     2,     4,     5,     5,     2,
       4,     4,     1,     1,     1,     1,     1,     1,     3,     2,
       3,     3,     4,     3,     1,     4,     1,     5,     5,     6,
       7,     7,     8,     6,     6,     7,     8,     8,     9,     2,
       2,     3,     5,     4,     2,     2,     3,     4,     5,     1,
       1,     1,     1,     5,     2,     4,     3,     4,     5,     7,
       4,     6,     7,     0,     2,     2,     4,     1,     3,     2,
       0,     2,     1,     3,     2,     3,     4,     5,     1,     3,
       2,     4,     1,     3,     1,     1,     2,     2,     1,     3,
       2,     1,     3,     1,     3,     1,     3,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     5,     5,
       0,     2,     1,     2,     3,     0,     3,     5,     3,     5,
       7,     3,     5,     3,     5,     7,     1,     3,     4,     0,
       1,     1,     3,     1,     3,     5,     0,     1,     1,     3,
       1,     3,     4,     3,     2,     1,     3,     0,     2,     1,
       3,     2,     4,     3,     3,     2,     1,     3,     0,     2,
       4,     5,     3,     4,     0,     2,     1,     3,     0,     1,
       1,     1,     0,     1,     1,     2,     1,     2,     1,     2,
       1,     1,     1,     2,     2,     1,     2,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1
};


enum { YYENOMEM = -2 };

#define yyerrok         (yyerrstatus = 0)
#define yyclearin       (yychar = YYEMPTY)

#define YYACCEPT        goto yyacceptlab
#define YYABORT         goto yyabortlab
#define YYERROR         goto yyerrorlab
#define YYNOMEM         goto yyexhaustedlab


#define YYRECOVERING()  (!!yyerrstatus)

#define YYBACKUP(Token, Value)                                    \
  do                                                              \
    if (yychar == YYEMPTY)                                        \
      {                                                           \
        yychar = (Token);                                         \
        yylval = (Value);                                         \
        YYPOPSTACK (yylen);                                       \
        yystate = *yyssp;                                         \
        goto yybackup;                                            \
      }                                                           \
    else                                                          \
      {                                                           \
        yyerror (YY_("syntax error: cannot back up")); \
        YYERROR;                                                  \
      }                                                           \
  while (0)

/* Backward compatibility with an undocumented macro.
   Use YYerror or YYUNDEF. */
#define YYERRCODE YYUNDEF


/* Enable debugging if requested.  */
#if YYDEBUG

# ifndef YYFPRINTF
#  include <stdio.h> /* INFRINGES ON USER NAME SPACE */
#  define YYFPRINTF fprintf
# endif

# define YYDPRINTF(Args)                        \
do {                                            \
  if (yydebug)                                  \
    YYFPRINTF Args;                             \
} while (0)




# define YY_SYMBOL_PRINT(Title, Kind, Value, Location)                    \
do {                                                                      \
  if (yydebug)                                                            \
    {                                                                     \
      YYFPRINTF (stderr, "%s ", Title);                                   \
      yy_symbol_print (stderr,                                            \
                  Kind, Value); \
      YYFPRINTF (stderr, "\n");                                           \
    }                                                                     \
} while (0)


/*-----------------------------------.
| Print this symbol's value on YYO.  |
`-----------------------------------*/

static void
yy_symbol_value_print (FILE *yyo,
                       yysymbol_kind_t yykind, YYSTYPE const * const yyvaluep)
{
  FILE *yyoutput = yyo;
  YY_USE (yyoutput);
  if (!yyvaluep)
    return;
  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  YY_USE (yykind);
  YY_IGNORE_MAYBE_UNINITIALIZED_END
}


/*---------------------------.
| Print this symbol on YYO.  |
`---------------------------*/

static void
yy_symbol_print (FILE *yyo,
                 yysymbol_kind_t yykind, YYSTYPE const * const yyvaluep)
{
  YYFPRINTF (yyo, "%s %s (",
             yykind < YYNTOKENS ? "token" : "nterm", yysymbol_name (yykind));

  yy_symbol_value_print (yyo, yykind, yyvaluep);
  YYFPRINTF (yyo, ")");
}

/*------------------------------------------------------------------.
| yy_stack_print -- Print the state stack from its BOTTOM up to its |
| TOP (included).                                                   |
`------------------------------------------------------------------*/

static void
yy_stack_print (yy_state_t *yybottom, yy_state_t *yytop)
{
  YYFPRINTF (stderr, "Stack now");
  for (; yybottom <= yytop; yybottom++)
    {
      int yybot = *yybottom;
      YYFPRINTF (stderr, " %d", yybot);
    }
  YYFPRINTF (stderr, "\n");
}

# define YY_STACK_PRINT(Bottom, Top)                            \
do {                                                            \
  if (yydebug)                                                  \
    yy_stack_print ((Bottom), (Top));                           \
} while (0)


/*------------------------------------------------.
| Report that the YYRULE is going to be reduced.  |
`------------------------------------------------*/

static void
yy_reduce_print (yy_state_t *yyssp, YYSTYPE *yyvsp,
                 int yyrule)
{
  int yylno = yyrline[yyrule];
  int yynrhs = yyr2[yyrule];
  int yyi;
  YYFPRINTF (stderr, "Reducing stack by rule %d (line %d):\n",
             yyrule - 1, yylno);
  /* The symbols being reduced.  */
  for (yyi = 0; yyi < yynrhs; yyi++)
    {
      YYFPRINTF (stderr, "   $%d = ", yyi + 1);
      yy_symbol_print (stderr,
                       YY_ACCESSING_SYMBOL (+yyssp[yyi + 1 - yynrhs]),
                       &yyvsp[(yyi + 1) - (yynrhs)]);
      YYFPRINTF (stderr, "\n");
    }
}

# define YY_REDUCE_PRINT(Rule)          \
do {                                    \
  if (yydebug)                          \
    yy_reduce_print (yyssp, yyvsp, Rule); \
} while (0)

/* Nonzero means print parse trace.  It is left uninitialized so that
   multiple parsers can coexist.  */
int yydebug;
#else /* !YYDEBUG */
# define YYDPRINTF(Args) ((void) 0)
# define YY_SYMBOL_PRINT(Title, Kind, Value, Location)
# define YY_STACK_PRINT(Bottom, Top)
# define YY_REDUCE_PRINT(Rule)
#endif /* !YYDEBUG */


/* YYINITDEPTH -- initial size of the parser's stacks.  */
#ifndef YYINITDEPTH
# define YYINITDEPTH 200
#endif

/* YYMAXDEPTH -- maximum size the stacks can grow to (effective only
   if the built-in stack extension method is used).

   Do not make this value too large; the results are undefined if
   YYSTACK_ALLOC_MAXIMUM < YYSTACK_BYTES (YYMAXDEPTH)
   evaluated with infinite-precision integer arithmetic.  */

#ifndef YYMAXDEPTH
# define YYMAXDEPTH 10000
#endif


/* Context of a parse error.  */
typedef struct
{
  yy_state_t *yyssp;
  yysymbol_kind_t yytoken;
} yypcontext_t;

/* Put in YYARG at most YYARGN of the expected tokens given the
   current YYCTX, and return the number of tokens stored in YYARG.  If
   YYARG is null, return the number of expected tokens (guaranteed to
   be less than YYNTOKENS).  Return YYENOMEM on memory exhaustion.
   Return 0 if there are more than YYARGN expected tokens, yet fill
   YYARG up to YYARGN. */
static int
yypcontext_expected_tokens (const yypcontext_t *yyctx,
                            yysymbol_kind_t yyarg[], int yyargn)
{
  /* Actual size of YYARG. */
  int yycount = 0;
  int yyn = yypact[+*yyctx->yyssp];
  if (!yypact_value_is_default (yyn))
    {
      /* Start YYX at -YYN if negative to avoid negative indexes in
         YYCHECK.  In other words, skip the first -YYN actions for
         this state because they are default actions.  */
      int yyxbegin = yyn < 0 ? -yyn : 0;
      /* Stay within bounds of both yycheck and yytname.  */
      int yychecklim = YYLAST - yyn + 1;
      int yyxend = yychecklim < YYNTOKENS ? yychecklim : YYNTOKENS;
      int yyx;
      for (yyx = yyxbegin; yyx < yyxend; ++yyx)
        if (yycheck[yyx + yyn] == yyx && yyx != YYSYMBOL_YYerror
            && !yytable_value_is_error (yytable[yyx + yyn]))
          {
            if (!yyarg)
              ++yycount;
            else if (yycount == yyargn)
              return 0;
            else
              yyarg[yycount++] = YY_CAST (yysymbol_kind_t, yyx);
          }
    }
  if (yyarg && yycount == 0 && 0 < yyargn)
    yyarg[0] = YYSYMBOL_YYEMPTY;
  return yycount;
}




#ifndef yystrlen
# if defined __GLIBC__ && defined _STRING_H
#  define yystrlen(S) (YY_CAST (YYPTRDIFF_T, strlen (S)))
# else
/* Return the length of YYSTR.  */
static YYPTRDIFF_T
yystrlen (const char *yystr)
{
  YYPTRDIFF_T yylen;
  for (yylen = 0; yystr[yylen]; yylen++)
    continue;
  return yylen;
}
# endif
#endif

#ifndef yystpcpy
# if defined __GLIBC__ && defined _STRING_H && defined _GNU_SOURCE
#  define yystpcpy stpcpy
# else
/* Copy YYSRC to YYDEST, returning the address of the terminating '\0' in
   YYDEST.  */
static char *
yystpcpy (char *yydest, const char *yysrc)
{
  char *yyd = yydest;
  const char *yys = yysrc;

  while ((*yyd++ = *yys++) != '\0')
    continue;

  return yyd - 1;
}
# endif
#endif

#ifndef yytnamerr
/* Copy to YYRES the contents of YYSTR after stripping away unnecessary
   quotes and backslashes, so that it's suitable for yyerror.  The
   heuristic is that double-quoting is unnecessary unless the string
   contains an apostrophe, a comma, or backslash (other than
   backslash-backslash).  YYSTR is taken from yytname.  If YYRES is
   null, do not copy; instead, return the length of what the result
   would have been.  */
static YYPTRDIFF_T
yytnamerr (char *yyres, const char *yystr)
{
  if (*yystr == '"')
    {
      YYPTRDIFF_T yyn = 0;
      char const *yyp = yystr;
      for (;;)
        switch (*++yyp)
          {
          case '\'':
          case ',':
            goto do_not_strip_quotes;

          case '\\':
            if (*++yyp != '\\')
              goto do_not_strip_quotes;
            else
              goto append;

          append:
          default:
            if (yyres)
              yyres[yyn] = *yyp;
            yyn++;
            break;

          case '"':
            if (yyres)
              yyres[yyn] = '\0';
            return yyn;
          }
    do_not_strip_quotes: ;
    }

  if (yyres)
    return yystpcpy (yyres, yystr) - yyres;
  else
    return yystrlen (yystr);
}
#endif


static int
yy_syntax_error_arguments (const yypcontext_t *yyctx,
                           yysymbol_kind_t yyarg[], int yyargn)
{
  /* Actual size of YYARG. */
  int yycount = 0;
  /* There are many possibilities here to consider:
     - If this state is a consistent state with a default action, then
       the only way this function was invoked is if the default action
       is an error action.  In that case, don't check for expected
       tokens because there are none.
     - The only way there can be no lookahead present (in yychar) is if
       this state is a consistent state with a default action.  Thus,
       detecting the absence of a lookahead is sufficient to determine
       that there is no unexpected or expected token to report.  In that
       case, just report a simple "syntax error".
     - Don't assume there isn't a lookahead just because this state is a
       consistent state with a default action.  There might have been a
       previous inconsistent state, consistent state with a non-default
       action, or user semantic action that manipulated yychar.
     - Of course, the expected token list depends on states to have
       correct lookahead information, and it depends on the parser not
       to perform extra reductions after fetching a lookahead from the
       scanner and before detecting a syntax error.  Thus, state merging
       (from LALR or IELR) and default reductions corrupt the expected
       token list.  However, the list is correct for canonical LR with
       one exception: it will still contain any token that will not be
       accepted due to an error action in a later state.
  */
  if (yyctx->yytoken != YYSYMBOL_YYEMPTY)
    {
      int yyn;
      if (yyarg)
        yyarg[yycount] = yyctx->yytoken;
      ++yycount;
      yyn = yypcontext_expected_tokens (yyctx,
                                        yyarg ? yyarg + 1 : yyarg, yyargn - 1);
      if (yyn == YYENOMEM)
        return YYENOMEM;
      else
        yycount += yyn;
    }
  return yycount;
}

/* Copy into *YYMSG, which is of size *YYMSG_ALLOC, an error message
   about the unexpected token YYTOKEN for the state stack whose top is
   YYSSP.

   Return 0 if *YYMSG was successfully written.  Return -1 if *YYMSG is
   not large enough to hold the message.  In that case, also set
   *YYMSG_ALLOC to the required number of bytes.  Return YYENOMEM if the
   required number of bytes is too large to store.  */
static int
yysyntax_error (YYPTRDIFF_T *yymsg_alloc, char **yymsg,
                const yypcontext_t *yyctx)
{
  enum { YYARGS_MAX = 5 };
  /* Internationalized format string. */
  const char *yyformat = YY_NULLPTR;
  /* Arguments of yyformat: reported tokens (one for the "unexpected",
     one per "expected"). */
  yysymbol_kind_t yyarg[YYARGS_MAX];
  /* Cumulated lengths of YYARG.  */
  YYPTRDIFF_T yysize = 0;

  /* Actual size of YYARG. */
  int yycount = yy_syntax_error_arguments (yyctx, yyarg, YYARGS_MAX);
  if (yycount == YYENOMEM)
    return YYENOMEM;

  switch (yycount)
    {
#define YYCASE_(N, S)                       \
      case N:                               \
        yyformat = S;                       \
        break
    default: /* Avoid compiler warnings. */
      YYCASE_(0, YY_("syntax error"));
      YYCASE_(1, YY_("syntax error, unexpected %s"));
      YYCASE_(2, YY_("syntax error, unexpected %s, expecting %s"));
      YYCASE_(3, YY_("syntax error, unexpected %s, expecting %s or %s"));
      YYCASE_(4, YY_("syntax error, unexpected %s, expecting %s or %s or %s"));
      YYCASE_(5, YY_("syntax error, unexpected %s, expecting %s or %s or %s or %s"));
#undef YYCASE_
    }

  /* Compute error message size.  Don't count the "%s"s, but reserve
     room for the terminator.  */
  yysize = yystrlen (yyformat) - 2 * yycount + 1;
  {
    int yyi;
    for (yyi = 0; yyi < yycount; ++yyi)
      {
        YYPTRDIFF_T yysize1
          = yysize + yytnamerr (YY_NULLPTR, yytname[yyarg[yyi]]);
        if (yysize <= yysize1 && yysize1 <= YYSTACK_ALLOC_MAXIMUM)
          yysize = yysize1;
        else
          return YYENOMEM;
      }
  }

  if (*yymsg_alloc < yysize)
    {
      *yymsg_alloc = 2 * yysize;
      if (! (yysize <= *yymsg_alloc
             && *yymsg_alloc <= YYSTACK_ALLOC_MAXIMUM))
        *yymsg_alloc = YYSTACK_ALLOC_MAXIMUM;
      return -1;
    }

  /* Avoid sprintf, as that infringes on the user's name space.
     Don't have undefined behavior even if the translation
     produced a string with the wrong number of "%s"s.  */
  {
    char *yyp = *yymsg;
    int yyi = 0;
    while ((*yyp = *yyformat) != '\0')
      if (*yyp == '%' && yyformat[1] == 's' && yyi < yycount)
        {
          yyp += yytnamerr (yyp, yytname[yyarg[yyi++]]);
          yyformat += 2;
        }
      else
        {
          ++yyp;
          ++yyformat;
        }
  }
  return 0;
}


/*-----------------------------------------------.
| Release the memory associated to this symbol.  |
`-----------------------------------------------*/

static void
yydestruct (const char *yymsg,
            yysymbol_kind_t yykind, YYSTYPE *yyvaluep)
{
  YY_USE (yyvaluep);
  if (!yymsg)
    yymsg = "Deleting";
  YY_SYMBOL_PRINT (yymsg, yykind, yyvaluep, yylocationp);

  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  YY_USE (yykind);
  YY_IGNORE_MAYBE_UNINITIALIZED_END
}


/* Lookahead token kind.  */
int yychar;

/* The semantic value of the lookahead symbol.  */
YYSTYPE yylval;
/* Number of syntax errors so far.  */
int yynerrs;




/*----------.
| yyparse.  |
`----------*/

int
yyparse (void)
{
    yy_state_fast_t yystate = 0;
    /* Number of tokens to shift before error messages enabled.  */
    int yyerrstatus = 0;

    /* Refer to the stacks through separate pointers, to allow yyoverflow
       to reallocate them elsewhere.  */

    /* Their size.  */
    YYPTRDIFF_T yystacksize = YYINITDEPTH;

    /* The state stack: array, bottom, top.  */
    yy_state_t yyssa[YYINITDEPTH];
    yy_state_t *yyss = yyssa;
    yy_state_t *yyssp = yyss;

    /* The semantic value stack: array, bottom, top.  */
    YYSTYPE yyvsa[YYINITDEPTH];
    YYSTYPE *yyvs = yyvsa;
    YYSTYPE *yyvsp = yyvs;

  int yyn;
  /* The return value of yyparse.  */
  int yyresult;
  /* Lookahead symbol kind.  */
  yysymbol_kind_t yytoken = YYSYMBOL_YYEMPTY;
  /* The variables used to return semantic value and location from the
     action routines.  */
  YYSTYPE yyval;

  /* Buffer for error messages, and its allocated size.  */
  char yymsgbuf[128];
  char *yymsg = yymsgbuf;
  YYPTRDIFF_T yymsg_alloc = sizeof yymsgbuf;

#define YYPOPSTACK(N)   (yyvsp -= (N), yyssp -= (N))

  /* The number of symbols on the RHS of the reduced rule.
     Keep to zero when no symbol should be popped.  */
  int yylen = 0;

  YYDPRINTF ((stderr, "Starting parse\n"));

  yychar = YYEMPTY; /* Cause a token to be read.  */

  goto yysetstate;


/*------------------------------------------------------------.
| yynewstate -- push a new state, which is found in yystate.  |
`------------------------------------------------------------*/
yynewstate:
  /* In all cases, when you get here, the value and location stacks
     have just been pushed.  So pushing a state here evens the stacks.  */
  yyssp++;


/*--------------------------------------------------------------------.
| yysetstate -- set current state (the top of the stack) to yystate.  |
`--------------------------------------------------------------------*/
yysetstate:
  YYDPRINTF ((stderr, "Entering state %d\n", yystate));
  YY_ASSERT (0 <= yystate && yystate < YYNSTATES);
  YY_IGNORE_USELESS_CAST_BEGIN
  *yyssp = YY_CAST (yy_state_t, yystate);
  YY_IGNORE_USELESS_CAST_END
  YY_STACK_PRINT (yyss, yyssp);

  if (yyss + yystacksize - 1 <= yyssp)
#if !defined yyoverflow && !defined YYSTACK_RELOCATE
    YYNOMEM;
#else
    {
      /* Get the current used size of the three stacks, in elements.  */
      YYPTRDIFF_T yysize = yyssp - yyss + 1;

# if defined yyoverflow
      {
        /* Give user a chance to reallocate the stack.  Use copies of
           these so that the &'s don't force the real ones into
           memory.  */
        yy_state_t *yyss1 = yyss;
        YYSTYPE *yyvs1 = yyvs;

        /* Each stack pointer address is followed by the size of the
           data in use in that stack, in bytes.  This used to be a
           conditional around just the two extra args, but that might
           be undefined if yyoverflow is a macro.  */
        yyoverflow (YY_("memory exhausted"),
                    &yyss1, yysize * YYSIZEOF (*yyssp),
                    &yyvs1, yysize * YYSIZEOF (*yyvsp),
                    &yystacksize);
        yyss = yyss1;
        yyvs = yyvs1;
      }
# else /* defined YYSTACK_RELOCATE */
      /* Extend the stack our own way.  */
      if (YYMAXDEPTH <= yystacksize)
        YYNOMEM;
      yystacksize *= 2;
      if (YYMAXDEPTH < yystacksize)
        yystacksize = YYMAXDEPTH;

      {
        yy_state_t *yyss1 = yyss;
        union yyalloc *yyptr =
          YY_CAST (union yyalloc *,
                   YYSTACK_ALLOC (YY_CAST (YYSIZE_T, YYSTACK_BYTES (yystacksize))));
        if (! yyptr)
          YYNOMEM;
        YYSTACK_RELOCATE (yyss_alloc, yyss);
        YYSTACK_RELOCATE (yyvs_alloc, yyvs);
#  undef YYSTACK_RELOCATE
        if (yyss1 != yyssa)
          YYSTACK_FREE (yyss1);
      }
# endif

      yyssp = yyss + yysize - 1;
      yyvsp = yyvs + yysize - 1;

      YY_IGNORE_USELESS_CAST_BEGIN
      YYDPRINTF ((stderr, "Stack size increased to %ld\n",
                  YY_CAST (long, yystacksize)));
      YY_IGNORE_USELESS_CAST_END

      if (yyss + yystacksize - 1 <= yyssp)
        YYABORT;
    }
#endif /* !defined yyoverflow && !defined YYSTACK_RELOCATE */


  if (yystate == YYFINAL)
    YYACCEPT;

  goto yybackup;


/*-----------.
| yybackup.  |
`-----------*/
yybackup:
  /* Do appropriate processing given the current state.  Read a
     lookahead token if we need one and don't already have one.  */

  /* First try to decide what to do without reference to lookahead token.  */
  yyn = yypact[yystate];
  if (yypact_value_is_default (yyn))
    goto yydefault;

  /* Not known => get a lookahead token if don't already have one.  */

  /* YYCHAR is either empty, or end-of-input, or a valid lookahead.  */
  if (yychar == YYEMPTY)
    {
      YYDPRINTF ((stderr, "Reading a token\n"));
      yychar = yylex ();
    }

  if (yychar <= YYEOF)
    {
      yychar = YYEOF;
      yytoken = YYSYMBOL_YYEOF;
      YYDPRINTF ((stderr, "Now at end of input.\n"));
    }
  else if (yychar == YYerror)
    {
      /* The scanner already issued an error message, process directly
         to error recovery.  But do not keep the error token as
         lookahead, it is too special and may lead us to an endless
         loop in error recovery. */
      yychar = YYUNDEF;
      yytoken = YYSYMBOL_YYerror;
      goto yyerrlab1;
    }
  else
    {
      yytoken = YYTRANSLATE (yychar);
      YY_SYMBOL_PRINT ("Next token is", yytoken, &yylval, &yylloc);
    }

  /* If the proper action on seeing token YYTOKEN is to reduce or to
     detect an error, take that action.  */
  yyn += yytoken;
  if (yyn < 0 || YYLAST < yyn || yycheck[yyn] != yytoken)
    goto yydefault;
  yyn = yytable[yyn];
  if (yyn <= 0)
    {
      if (yytable_value_is_error (yyn))
        goto yyerrlab;
      yyn = -yyn;
      goto yyreduce;
    }

  /* Count tokens shifted since error; after three, turn off error
     status.  */
  if (yyerrstatus)
    yyerrstatus--;

  /* Shift the lookahead token.  */
  YY_SYMBOL_PRINT ("Shifting", yytoken, &yylval, &yylloc);
  yystate = yyn;
  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  *++yyvsp = yylval;
  YY_IGNORE_MAYBE_UNINITIALIZED_END

  /* Discard the shifted token.  */
  yychar = YYEMPTY;
  goto yynewstate;


/*-----------------------------------------------------------.
| yydefault -- do the default action for the current state.  |
`-----------------------------------------------------------*/
yydefault:
  yyn = yydefact[yystate];
  if (yyn == 0)
    goto yyerrlab;
  goto yyreduce;


/*-----------------------------.
| yyreduce -- do a reduction.  |
`-----------------------------*/
yyreduce:
  /* yyn is the number of a rule to reduce with.  */
  yylen = yyr2[yyn];

  /* If YYLEN is nonzero, implement the default value of the action:
     '$$ = $1'.

     Otherwise, the following line sets YYVAL to garbage.
     This behavior is undocumented and Bison
     users should not rely upon it.  Assigning to YYVAL
     unconditionally makes the parser a bit smaller, and it avoids a
     GCC warning that YYVAL may be used uninitialized.  */
  yyval = yyvsp[1-yylen];


  YY_REDUCE_PRINT (yyn);
  switch (yyn)
    {
  case 2: /* root: classes  */
#line 42 "lang11d"
                        { gRootParseNode = (PyrParseNode*)yyvsp[0]; gParserResult = 1; }
#line 2221 "lang11d_tab.cpp"
    break;

  case 3: /* root: classextensions  */
#line 44 "lang11d"
                        { gRootParseNode = (PyrParseNode*)yyvsp[0]; gParserResult = 1; }
#line 2227 "lang11d_tab.cpp"
    break;

  case 4: /* root: INTERPRET cmdlinecode  */
#line 46 "lang11d"
                        { gRootParseNode = (PyrParseNode*)yyvsp[0]; gParserResult = 2; }
#line 2233 "lang11d_tab.cpp"
    break;

  case 5: /* classes: %empty  */
#line 49 "lang11d"
          { yyval = 0; }
#line 2239 "lang11d_tab.cpp"
    break;

  case 6: /* classes: classes classdef  */
#line 51 "lang11d"
                        { yyval = (intptr_t)linkNextNode((PyrParseNode*)yyvsp[-1], (PyrParseNode*)yyvsp[0]); }
#line 2245 "lang11d_tab.cpp"
    break;

  case 8: /* classextensions: classextensions classextension  */
#line 56 "lang11d"
                                { yyval = (intptr_t)linkNextNode((PyrParseNode*)yyvsp[-1], (PyrParseNode*)yyvsp[0]); }
#line 2251 "lang11d_tab.cpp"
    break;

  case 9: /* classdef: classname superclass '{' classvardecls methods '}'  */
#line 60 "lang11d"
                                { yyval = (intptr_t)newPyrClassNode((PyrSlotNode*)yyvsp[-5], (PyrSlotNode*)yyvsp[-4],
					(PyrVarListNode*)yyvsp[-2], (PyrMethodNode*)yyvsp[-1], 0);
				}
#line 2259 "lang11d_tab.cpp"
    break;

  case 10: /* classdef: classname '[' optname ']' superclass '{' classvardecls methods '}'  */
#line 64 "lang11d"
                                { yyval = (intptr_t)newPyrClassNode((PyrSlotNode*)yyvsp[-8], (PyrSlotNode*)yyvsp[-4],
					(PyrVarListNode*)yyvsp[-2], (PyrMethodNode*)yyvsp[-1],
					(PyrSlotNode*)yyvsp[-6]);
				}
#line 2268 "lang11d_tab.cpp"
    break;

  case 11: /* classextension: '+' classname '{' methods '}'  */
#line 71 "lang11d"
                                {
					yyval = (intptr_t)newPyrClassExtNode((PyrSlotNode*)yyvsp[-3], (PyrMethodNode*)yyvsp[-1]);
				}
#line 2276 "lang11d_tab.cpp"
    break;

  case 12: /* optname: %empty  */
#line 76 "lang11d"
                  { yyval = 0; }
#line 2282 "lang11d_tab.cpp"
    break;

  case 14: /* superclass: %empty  */
#line 80 "lang11d"
                  { yyval = 0; }
#line 2288 "lang11d_tab.cpp"
    break;

  case 15: /* superclass: ':' classname  */
#line 82 "lang11d"
                                { yyval = yyvsp[0]; }
#line 2294 "lang11d_tab.cpp"
    break;

  case 16: /* classvardecls: %empty  */
#line 85 "lang11d"
                  { yyval = 0; }
#line 2300 "lang11d_tab.cpp"
    break;

  case 17: /* classvardecls: classvardecls classvardecl  */
#line 87 "lang11d"
                                        { yyval = (intptr_t)linkNextNode((PyrParseNode*)yyvsp[-1], (PyrParseNode*)yyvsp[0]); }
#line 2306 "lang11d_tab.cpp"
    break;

  case 18: /* classvardecl: CLASSVAR rwslotdeflist ';'  */
#line 91 "lang11d"
                                        { yyval = (intptr_t)newPyrVarListNode((PyrVarDefNode*)yyvsp[-1], varClass); }
#line 2312 "lang11d_tab.cpp"
    break;

  case 19: /* classvardecl: VAR rwslotdeflist ';'  */
#line 93 "lang11d"
                                        { yyval = (intptr_t)newPyrVarListNode((PyrVarDefNode*)yyvsp[-1], varInst); }
#line 2318 "lang11d_tab.cpp"
    break;

  case 20: /* classvardecl: SC_CONST constdeflist ';'  */
#line 95 "lang11d"
                                        { yyval = (intptr_t)newPyrVarListNode((PyrVarDefNode*)yyvsp[-1], varConst); }
#line 2324 "lang11d_tab.cpp"
    break;

  case 21: /* methods: %empty  */
#line 98 "lang11d"
                  { yyval = 0; }
#line 2330 "lang11d_tab.cpp"
    break;

  case 22: /* methods: methods methoddef  */
#line 100 "lang11d"
                                { yyval = (intptr_t)linkNextNode((PyrParseNode*)yyvsp[-1], (PyrParseNode*)yyvsp[0]); }
#line 2336 "lang11d_tab.cpp"
    break;

  case 23: /* methoddef: name '{' argdecls funcvardecls primitive methbody '}'  */
#line 104 "lang11d"
                                { yyval = (intptr_t)newPyrMethodNode((PyrSlotNode*)yyvsp[-6], (PyrSlotNode*)yyvsp[-2],
					(PyrArgListNode*)yyvsp[-4], (PyrVarListNode*)yyvsp[-3], (PyrParseNode*)yyvsp[-1], 0); }
#line 2343 "lang11d_tab.cpp"
    break;

  case 24: /* methoddef: '*' name '{' argdecls funcvardecls primitive methbody '}'  */
#line 107 "lang11d"
                                { yyval = (intptr_t)newPyrMethodNode((PyrSlotNode*)yyvsp[-6], (PyrSlotNode*)yyvsp[-2],
					(PyrArgListNode*)yyvsp[-4], (PyrVarListNode*)yyvsp[-3], (PyrParseNode*)yyvsp[-1], 1); }
#line 2350 "lang11d_tab.cpp"
    break;

  case 25: /* methoddef: binop '{' argdecls funcvardecls primitive methbody '}'  */
#line 110 "lang11d"
                                { yyval = (intptr_t)newPyrMethodNode((PyrSlotNode*)yyvsp[-6], (PyrSlotNode*)yyvsp[-2],
					(PyrArgListNode*)yyvsp[-4], (PyrVarListNode*)yyvsp[-3], (PyrParseNode*)yyvsp[-1], 0); }
#line 2357 "lang11d_tab.cpp"
    break;

  case 26: /* methoddef: '*' binop '{' argdecls funcvardecls primitive methbody '}'  */
#line 113 "lang11d"
                                { yyval = (intptr_t)newPyrMethodNode((PyrSlotNode*)yyvsp[-6], (PyrSlotNode*)yyvsp[-2],
					(PyrArgListNode*)yyvsp[-4], (PyrVarListNode*)yyvsp[-3], (PyrParseNode*)yyvsp[-1], 1); }
#line 2364 "lang11d_tab.cpp"
    break;

  case 34: /* funcbody: exprseq funretval  */
#line 131 "lang11d"
                                { yyval = (intptr_t)newPyrDropNode((PyrParseNode*)yyvsp[-1], (PyrParseNode*)yyvsp[0]); }
#line 2370 "lang11d_tab.cpp"
    break;

  case 35: /* cmdlinecode: '(' argdecls1 funcvardecls1 funcbody ')'  */
#line 135 "lang11d"
                                { yyval = (intptr_t)newPyrBlockNode((PyrArgListNode*)yyvsp[-3], (PyrVarListNode*)yyvsp[-2], (PyrParseNode*)yyvsp[-1], false); }
#line 2376 "lang11d_tab.cpp"
    break;

  case 36: /* cmdlinecode: '(' argdecls1 funcbody ')'  */
#line 137 "lang11d"
                                { yyval = (intptr_t)newPyrBlockNode((PyrArgListNode*)yyvsp[-2], NULL, (PyrParseNode*)yyvsp[-1], false); }
#line 2382 "lang11d_tab.cpp"
    break;

  case 37: /* cmdlinecode: '(' funcvardecls1 funcbody ')'  */
#line 139 "lang11d"
                                { yyval = (intptr_t)newPyrBlockNode(NULL, (PyrVarListNode*)yyvsp[-2], (PyrParseNode*)yyvsp[-1], false); }
#line 2388 "lang11d_tab.cpp"
    break;

  case 38: /* cmdlinecode: argdecls1 funcvardecls1 funcbody  */
#line 141 "lang11d"
                                { yyval = (intptr_t)newPyrBlockNode((PyrArgListNode*)yyvsp[-2], (PyrVarListNode*)yyvsp[-1], (PyrParseNode*)yyvsp[0], false); }
#line 2394 "lang11d_tab.cpp"
    break;

  case 39: /* cmdlinecode: argdecls1 funcbody  */
#line 143 "lang11d"
                                { yyval = (intptr_t)newPyrBlockNode((PyrArgListNode*)yyvsp[-1], NULL, (PyrParseNode*)yyvsp[0], false); }
#line 2400 "lang11d_tab.cpp"
    break;

  case 40: /* cmdlinecode: funcvardecls1 funcbody  */
#line 145 "lang11d"
                                { yyval = (intptr_t)newPyrBlockNode(NULL, (PyrVarListNode*)yyvsp[-1], (PyrParseNode*)yyvsp[0], false); }
#line 2406 "lang11d_tab.cpp"
    break;

  case 41: /* cmdlinecode: funcbody  */
#line 147 "lang11d"
                                { yyval = (intptr_t)newPyrBlockNode(NULL, NULL, (PyrParseNode*)yyvsp[0], false); }
#line 2412 "lang11d_tab.cpp"
    break;

  case 43: /* methbody: exprseq retval  */
#line 152 "lang11d"
                                { yyval = (intptr_t)newPyrDropNode((PyrParseNode*)yyvsp[-1], (PyrParseNode*)yyvsp[0]); }
#line 2418 "lang11d_tab.cpp"
    break;

  case 44: /* primitive: %empty  */
#line 155 "lang11d"
                  { yyval = 0; }
#line 2424 "lang11d_tab.cpp"
    break;

  case 45: /* primitive: primname optsemi  */
#line 157 "lang11d"
                                { yyval = yyvsp[-1]; }
#line 2430 "lang11d_tab.cpp"
    break;

  case 46: /* retval: %empty  */
#line 161 "lang11d"
                        { yyval = (intptr_t)newPyrReturnNode(NULL); }
#line 2436 "lang11d_tab.cpp"
    break;

  case 47: /* retval: '^' expr optsemi  */
#line 163 "lang11d"
                        { yyval = (intptr_t)newPyrReturnNode((PyrParseNode*)yyvsp[-1]); }
#line 2442 "lang11d_tab.cpp"
    break;

  case 48: /* funretval: %empty  */
#line 167 "lang11d"
                        { yyval = (intptr_t)newPyrBlockReturnNode(); }
#line 2448 "lang11d_tab.cpp"
    break;

  case 49: /* funretval: '^' expr optsemi  */
#line 169 "lang11d"
                        { yyval = (intptr_t)newPyrReturnNode((PyrParseNode*)yyvsp[-1]); }
#line 2454 "lang11d_tab.cpp"
    break;

  case 51: /* blocklist1: blocklist1 blocklistitem  */
#line 174 "lang11d"
                                {
					yyval = (intptr_t)linkNextNode((PyrParseNode*)yyvsp[-1], (PyrParseNode*)yyvsp[0]);
				}
#line 2462 "lang11d_tab.cpp"
    break;

  case 54: /* blocklist: %empty  */
#line 183 "lang11d"
                        { yyval = 0; }
#line 2468 "lang11d_tab.cpp"
    break;

  case 56: /* msgsend: name blocklist1  */
#line 188 "lang11d"
                        {
				yyval = (intptr_t)newPyrCallNode((PyrSlotNode*)yyvsp[-1], (PyrParseNode*)yyvsp[0], 0, 0);
			}
#line 2476 "lang11d_tab.cpp"
    break;

  case 57: /* msgsend: '(' binop2 ')' blocklist1  */
#line 192 "lang11d"
                        {
				yyval = (intptr_t)newPyrCallNode((PyrSlotNode*)yyvsp[-2], (PyrParseNode*)yyvsp[0], 0, 0);
			}
#line 2484 "lang11d_tab.cpp"
    break;

  case 58: /* msgsend: name '(' ')' blocklist1  */
#line 196 "lang11d"
                        {
				yyval = (intptr_t)newPyrCallNode((PyrSlotNode*)yyvsp[-3], NULL, NULL, (PyrParseNode*)yyvsp[0]);
			}
#line 2492 "lang11d_tab.cpp"
    break;

  case 59: /* msgsend: name '(' arglist1 optkeyarglist ')' blocklist  */
#line 200 "lang11d"
                        {
				yyval = (intptr_t)newPyrCallNode((PyrSlotNode*)yyvsp[-5], (PyrParseNode*)yyvsp[-3],
						(PyrParseNode*)yyvsp[-2], (PyrParseNode*)yyvsp[0]);
			}
#line 2501 "lang11d_tab.cpp"
    break;

  case 60: /* msgsend: '(' binop2 ')' '(' ')' blocklist1  */
#line 205 "lang11d"
                        {
				yyval = (intptr_t)newPyrCallNode((PyrSlotNode*)yyvsp[-4], NULL, NULL, (PyrParseNode*)yyvsp[0]);
			}
#line 2509 "lang11d_tab.cpp"
    break;

  case 61: /* msgsend: '(' binop2 ')' '(' arglist1 optkeyarglist ')' blocklist  */
#line 209 "lang11d"
                        {
				yyval = (intptr_t)newPyrCallNode((PyrSlotNode*)yyvsp[-6], (PyrParseNode*)yyvsp[-3],
						(PyrParseNode*)yyvsp[-2], (PyrParseNode*)yyvsp[0]);
			}
#line 2518 "lang11d_tab.cpp"
    break;

  case 62: /* msgsend: name '(' arglistv1 optkeyarglist ')'  */
#line 214 "lang11d"
                        {
				PyrSlotNode *selectornode;
				PyrSlot slot;
				PyrParseNode* args;

				if (isSuperObjNode((PyrParseNode*)yyvsp[-2])) {
					SetRaw(&((PyrPushNameNode*)yyvsp[-2])->mSlot, s_this);
					SetSymbol(&slot, s_superPerformList);
				} else {
					SetSymbol(&slot, s_performList);
				}
				selectornode = newPyrSlotNode(&slot);
				args = linkAfterHead(
					(PyrParseNode*)yyvsp[-2],
					newPyrPushLitNode((PyrSlotNode*)yyvsp[-4], NULL));
				yyval = (intptr_t)newPyrCallNode(selectornode, args, (PyrParseNode*)yyvsp[-1], 0);
			}
#line 2540 "lang11d_tab.cpp"
    break;

  case 63: /* msgsend: '(' binop2 ')' '(' arglistv1 optkeyarglist ')'  */
#line 232 "lang11d"
                        {
				PyrSlotNode *selectornode;
				PyrSlot slot;
				PyrParseNode* args;

				SetSymbol(&slot, s_performList);
				selectornode = newPyrSlotNode(&slot);
				args = linkAfterHead(
					(PyrParseNode*)yyvsp[-2],
					newPyrPushLitNode((PyrSlotNode*)yyvsp[-5], NULL));
				yyval = (intptr_t)newPyrCallNode(selectornode, args, (PyrParseNode*)yyvsp[-1], 0);
			}
#line 2557 "lang11d_tab.cpp"
    break;

  case 64: /* msgsend: classname '[' arrayelems ']'  */
#line 245 "lang11d"
                        { yyval = (intptr_t)newPyrDynListNode((PyrParseNode*)yyvsp[-3], (PyrParseNode*)yyvsp[-1]); }
#line 2563 "lang11d_tab.cpp"
    break;

  case 65: /* msgsend: classname blocklist1  */
#line 247 "lang11d"
                        {
				PyrSlotNode *selectornode;
				PyrSlot slot;
				PyrParseNode* args;

				SetSymbol(&slot, s_new);
				selectornode = newPyrSlotNode(&slot);
				args = (PyrParseNode*)newPyrPushNameNode((PyrSlotNode*)yyvsp[-1]);
				yyval = (intptr_t)newPyrCallNode(selectornode, args, 0, (PyrParseNode*)yyvsp[0]);
			}
#line 2578 "lang11d_tab.cpp"
    break;

  case 66: /* msgsend: classname '(' ')' blocklist  */
#line 258 "lang11d"
                        {
				PyrSlotNode *selectornode;
				PyrSlot slot;
				PyrParseNode* args;

				SetSymbol(&slot, s_new);
				selectornode = newPyrSlotNode(&slot);
				args = (PyrParseNode*)newPyrPushNameNode((PyrSlotNode*)yyvsp[-3]);
				yyval = (intptr_t)newPyrCallNode(selectornode, args, NULL, (PyrParseNode*)yyvsp[0]);
			}
#line 2593 "lang11d_tab.cpp"
    break;

  case 67: /* msgsend: classname '(' keyarglist1 optcomma ')' blocklist  */
#line 269 "lang11d"
                        {
				PyrSlotNode *selectornode;
				PyrSlot slot;
				PyrParseNode* args;

				SetSymbol(&slot, s_new);
				selectornode = newPyrSlotNode(&slot);
				args = (PyrParseNode*)newPyrPushNameNode((PyrSlotNode*)yyvsp[-5]);
				yyval = (intptr_t)newPyrCallNode(selectornode, args, (PyrParseNode*)yyvsp[-3], (PyrParseNode*)yyvsp[0]);
			}
#line 2608 "lang11d_tab.cpp"
    break;

  case 68: /* msgsend: classname '(' arglist1 optkeyarglist ')' blocklist  */
#line 280 "lang11d"
                        {
				PyrSlotNode *selectornode;
				PyrSlot slot;
				PyrParseNode* args;

				SetSymbol(&slot, s_new);
				selectornode = newPyrSlotNode(&slot);
				args = linkNextNode(
					(PyrParseNode*)newPyrPushNameNode((PyrSlotNode*)yyvsp[-5]),
					(PyrParseNode*)yyvsp[-3]);
				yyval = (intptr_t)newPyrCallNode(selectornode, args, (PyrParseNode*)yyvsp[-2], (PyrParseNode*)yyvsp[0]);
			}
#line 2625 "lang11d_tab.cpp"
    break;

  case 69: /* msgsend: classname '(' arglistv1 optkeyarglist ')'  */
#line 293 "lang11d"
                        {
				PyrSlotNode *selectornode, *selectornode2;
				PyrSlot slot, slot2;
				PyrParseNode* args;

				if (isSuperObjNode((PyrParseNode*)yyvsp[-4])) {
					SetRaw(&((PyrPushNameNode*)yyvsp[-4])->mSlot, s_this);
					SetSymbol(&slot, s_superPerformList);
				} else {
					SetSymbol(&slot, s_performList);
				}
				SetSymbol(&slot2, s_new);
				selectornode = newPyrSlotNode(&slot);
				selectornode2 = newPyrSlotNode(&slot2);
				args = linkNextNode(
					(PyrParseNode*)newPyrPushNameNode((PyrSlotNode*)yyvsp[-4]),
					newPyrPushLitNode(selectornode2, NULL));
				args = linkNextNode(args, (PyrParseNode*)yyvsp[-2]);
				yyval = (intptr_t)newPyrCallNode(selectornode, args, (PyrParseNode*)yyvsp[0], 0);
			}
#line 2650 "lang11d_tab.cpp"
    break;

  case 70: /* msgsend: expr '.' '(' ')' blocklist  */
#line 314 "lang11d"
                        {
				PyrSlotNode *selectornode;
				PyrSlot slot;

				SetSymbol(&slot, s_value);
				selectornode = newPyrSlotNode(&slot);
				yyval = (intptr_t)newPyrCallNode(selectornode, (PyrParseNode*)yyvsp[-4], NULL, (PyrParseNode*)yyvsp[0]);
			}
#line 2663 "lang11d_tab.cpp"
    break;

  case 71: /* msgsend: expr '.' '(' keyarglist1 optcomma ')' blocklist  */
#line 323 "lang11d"
                        {
				PyrSlotNode *selectornode;
				PyrSlot slot;

				SetSymbol(&slot, s_value);
				selectornode = newPyrSlotNode(&slot);
				yyval = (intptr_t)newPyrCallNode(selectornode, (PyrParseNode*)yyvsp[-6], (PyrParseNode*)yyvsp[-3], (PyrParseNode*)yyvsp[0]);
			}
#line 2676 "lang11d_tab.cpp"
    break;

  case 72: /* msgsend: expr '.' name '(' keyarglist1 optcomma ')' blocklist  */
#line 332 "lang11d"
                        {
				yyval = (intptr_t)newPyrCallNode((PyrSlotNode*)yyvsp[-5], (PyrParseNode*)yyvsp[-7],
					(PyrParseNode*)yyvsp[-3], (PyrParseNode*)yyvsp[0]);
			}
#line 2685 "lang11d_tab.cpp"
    break;

  case 73: /* msgsend: expr '.' '(' arglist1 optkeyarglist ')' blocklist  */
#line 337 "lang11d"
                        {
				PyrSlotNode *selectornode;
				PyrSlot slot;
				PyrParseNode* args;

				SetSymbol(&slot, s_value);
				selectornode = newPyrSlotNode(&slot);
				args = linkNextNode(
					(PyrParseNode*)yyvsp[-6],
					(PyrParseNode*)yyvsp[-3]);
				yyval = (intptr_t)newPyrCallNode(selectornode, args, (PyrParseNode*)yyvsp[-2], (PyrParseNode*)yyvsp[0]);
			}
#line 2702 "lang11d_tab.cpp"
    break;

  case 74: /* msgsend: expr '.' '(' arglistv1 optkeyarglist ')'  */
#line 351 "lang11d"
                        {
				PyrSlotNode *selectornode;
				PyrSlot slot, slot2;
				PyrParseNode* args;

				if (isSuperObjNode((PyrParseNode*)yyvsp[-5])) {
					SetRaw(&((PyrPushNameNode*)yyvsp[-5])->mSlot, s_this);
					SetSymbol(&slot, s_superPerformList);
				} else {
					SetSymbol(&slot, s_performList);
				}
				SetSymbol(&slot2, s_value);
				selectornode = newPyrSlotNode(&slot);
				args = linkNextNode(
					(PyrParseNode*)yyvsp[-5],
					newPyrPushLitNode(newPyrSlotNode(&slot2), NULL));
				args = linkNextNode(args, (PyrParseNode*)yyvsp[-2]);
				yyval = (intptr_t)newPyrCallNode(selectornode, args, (PyrParseNode*)yyvsp[-1], 0);
			}
#line 2726 "lang11d_tab.cpp"
    break;

  case 75: /* msgsend: expr '.' name '(' ')' blocklist  */
#line 373 "lang11d"
                        {
				yyval = (intptr_t)newPyrCallNode((PyrSlotNode*)yyvsp[-3], (PyrParseNode*)yyvsp[-5], NULL, (PyrParseNode*)yyvsp[0]);
			}
#line 2734 "lang11d_tab.cpp"
    break;

  case 76: /* msgsend: expr '.' name '(' arglist1 optkeyarglist ')' blocklist  */
#line 377 "lang11d"
                        {
				PyrParseNode* args;
				args = linkNextNode((PyrParseNode*)yyvsp[-7], (PyrParseNode*)yyvsp[-3]);
				yyval = (intptr_t)newPyrCallNode((PyrSlotNode*)yyvsp[-5], args, (PyrParseNode*)yyvsp[-2], (PyrParseNode*)yyvsp[0]);
			}
#line 2744 "lang11d_tab.cpp"
    break;

  case 77: /* msgsend: expr '.' name '(' arglistv1 optkeyarglist ')'  */
#line 383 "lang11d"
                        {
				PyrSlotNode *selectornode;
				PyrSlot slot;
				PyrParseNode* args;

				if (isSuperObjNode((PyrParseNode*)yyvsp[-6])) {
					SetRaw(&((PyrPushNameNode*)yyvsp[-6])->mSlot, s_this);
					SetSymbol(&slot, s_superPerformList);
				} else {
					SetSymbol(&slot, s_performList);
				}
				selectornode = newPyrSlotNode(&slot);

				args = linkNextNode((PyrParseNode*)yyvsp[-6], newPyrPushLitNode((PyrSlotNode*)yyvsp[-4], NULL));
				args = linkNextNode(args, (PyrParseNode*)yyvsp[-2]);
				yyval = (intptr_t)newPyrCallNode(selectornode, args, (PyrParseNode*)yyvsp[-1], 0);
			}
#line 2766 "lang11d_tab.cpp"
    break;

  case 78: /* msgsend: expr '.' name blocklist  */
#line 401 "lang11d"
                        {
				yyval = (intptr_t)newPyrCallNode((PyrSlotNode*)yyvsp[-1], (PyrParseNode*)yyvsp[-3], 0, (PyrParseNode*)yyvsp[0]);
			}
#line 2774 "lang11d_tab.cpp"
    break;

  case 79: /* msgsend: expr '.' '(' bigArgsList optcomma ')'  */
#line 406 "lang11d"
                        {
				std::cout << "GOT A THING!" << std::endl;
			}
#line 2782 "lang11d_tab.cpp"
    break;

  case 80: /* $@1: %empty  */
#line 411 "lang11d"
                            { pushls(&generatorStack, yyvsp[0]); pushls(&generatorStack, 1); }
#line 2788 "lang11d_tab.cpp"
    break;

  case 81: /* generator: '{' ':' exprseq $@1 ',' qual '}'  */
#line 412 "lang11d"
                        {
				PyrSlot slot;
				SetSymbol(&slot, getsym("r"));
				PyrSlotNode* selectornode = newPyrSlotNode(&slot);

				PyrParseNode *block = (PyrParseNode*)newPyrBlockNode(0, 0, (PyrParseNode*)yyvsp[-1], false);
				PyrParseNode *blocklit = (PyrParseNode*)newPyrPushLitNode(NULL, block);
				yyval = (intptr_t)newPyrCallNode(selectornode, (PyrParseNode*)blocklit, 0, 0);
			}
#line 2802 "lang11d_tab.cpp"
    break;

  case 82: /* $@2: %empty  */
#line 421 "lang11d"
                                  { pushls(&generatorStack, yyvsp[0]); pushls(&generatorStack, 2); }
#line 2808 "lang11d_tab.cpp"
    break;

  case 83: /* generator: '{' ';' exprseq $@2 ',' qual '}'  */
#line 422 "lang11d"
                        {
				yyval = yyvsp[-1];
			}
#line 2816 "lang11d_tab.cpp"
    break;

  case 84: /* nextqual: %empty  */
#line 428 "lang11d"
                                {
					// innermost part
					int action = popls(&generatorStack);
					PyrParseNode* expr = (PyrParseNode*)popls(&generatorStack);

					switch (action)
					{
						case 1 :
						{
							PyrSlot slot;
							SetSymbol(&slot, getsym("yield"));
							PyrSlotNode* selectornode = newPyrSlotNode(&slot);

							yyval = (intptr_t)newPyrCallNode(selectornode, expr, 0, 0);
						} break;
						case 2 :
						{
							yyval = (intptr_t)expr;
						} break;
					}
				}
#line 2842 "lang11d_tab.cpp"
    break;

  case 85: /* nextqual: ',' qual  */
#line 450 "lang11d"
                                { yyval = yyvsp[0]; }
#line 2848 "lang11d_tab.cpp"
    break;

  case 86: /* qual: name LEFTARROW exprseq nextqual  */
#line 454 "lang11d"
                        {
				// later should check if exprseq is a series and optimize it to for loop
				PyrParseNode *exprseq = (PyrParseNode*)yyvsp[-1];
				if (exprseq->mClassno == pn_CallNode) {
					PyrCallNode *callnode = (PyrCallNode*)exprseq;
					if (slotRawSymbol(&callnode->mSelector->mSlot) == s_series)
					{
						SetSymbol(&callnode->mSelector->mSlot, getsym("forSeries"));

						PyrVarDefNode* var = newPyrVarDefNode((PyrSlotNode*)yyvsp[-3], NULL, 0);
						PyrArgListNode* args = newPyrArgListNode(var, NULL, NULL);
						PyrParseNode *block = (PyrParseNode*)newPyrBlockNode(args, 0, (PyrParseNode*)yyvsp[0], false);
						PyrParseNode *blocklit = (PyrParseNode*)newPyrPushLitNode(NULL, block);

						callnode->mArglist = linkNextNode(callnode->mArglist, blocklit);
						yyval = (intptr_t)callnode;

					} else goto notoptimized1;
				} else {
					notoptimized1:
					PyrSlot slot;
					SetSymbol(&slot, getsym("do"));
					PyrSlotNode* selectornode = newPyrSlotNode(&slot);

					PyrVarDefNode* var = newPyrVarDefNode((PyrSlotNode*)yyvsp[-3], NULL, 0);
					PyrArgListNode* args = newPyrArgListNode(var, NULL, NULL);
					PyrParseNode *block = (PyrParseNode*)newPyrBlockNode(args, 0, (PyrParseNode*)yyvsp[0], false);
					PyrParseNode *blocklit = (PyrParseNode*)newPyrPushLitNode(NULL, block);

					PyrParseNode* args2 = linkNextNode(exprseq, blocklit);
					yyval = (intptr_t)newPyrCallNode(selectornode, args2, 0, 0);
				}
			}
#line 2886 "lang11d_tab.cpp"
    break;

  case 87: /* qual: name name LEFTARROW exprseq nextqual  */
#line 488 "lang11d"
                        {
				// later should check if exprseq is a series and optimize it to for loop
				PyrParseNode *exprseq = (PyrParseNode*)yyvsp[-1];
				if (exprseq->mClassno == pn_CallNode) {
					PyrCallNode *callnode = (PyrCallNode*)exprseq;
					if (slotRawSymbol(&callnode->mSelector->mSlot) == s_series)
					{
						SetSymbol(&callnode->mSelector->mSlot, getsym("forSeries"));

						PyrVarDefNode* var1 = newPyrVarDefNode((PyrSlotNode*)yyvsp[-4], NULL, 0);
						PyrVarDefNode* var2 = newPyrVarDefNode((PyrSlotNode*)yyvsp[-3], NULL, 0);
						PyrVarDefNode* vars = (PyrVarDefNode*)linkNextNode(var1, var2);
						PyrArgListNode* args = newPyrArgListNode(vars, NULL, NULL);
						PyrParseNode *block = (PyrParseNode*)newPyrBlockNode(args, 0, (PyrParseNode*)yyvsp[0], false);
						PyrParseNode *blocklit = (PyrParseNode*)newPyrPushLitNode(NULL, block);

						callnode->mArglist = linkNextNode(callnode->mArglist, blocklit);
						yyval = (intptr_t)callnode;

					} else goto notoptimized2;
				} else {
					notoptimized2:
					PyrSlot slot;
					SetSymbol(&slot, getsym("do"));
					PyrSlotNode* selectornode = newPyrSlotNode(&slot);

					PyrVarDefNode* var1 = newPyrVarDefNode((PyrSlotNode*)yyvsp[-4], NULL, 0);
					PyrVarDefNode* var2 = newPyrVarDefNode((PyrSlotNode*)yyvsp[-3], NULL, 0);
					PyrVarDefNode* vars = (PyrVarDefNode*)linkNextNode(var1, var2);
					PyrArgListNode* args = newPyrArgListNode(vars, NULL, NULL);
					PyrParseNode *block = (PyrParseNode*)newPyrBlockNode(args, 0, (PyrParseNode*)yyvsp[0], false);
					PyrParseNode *blocklit = (PyrParseNode*)newPyrPushLitNode(NULL, block);

					PyrParseNode* args2 = linkNextNode(exprseq, blocklit);
					yyval = (intptr_t)newPyrCallNode(selectornode, args2, 0, 0);
				}
			}
#line 2928 "lang11d_tab.cpp"
    break;

  case 88: /* qual: VAR name '=' exprseq nextqual  */
#line 526 "lang11d"
                        {
				PyrSlot slot;
				SetSymbol(&slot, s_value);
				PyrSlotNode* selectornode = newPyrSlotNode(&slot);

				PyrVarDefNode* var = newPyrVarDefNode((PyrSlotNode*)yyvsp[-3], NULL, 0);
				PyrArgListNode* args = newPyrArgListNode(var, NULL, NULL);
				PyrParseNode *block = (PyrParseNode*)newPyrBlockNode(args, 0, (PyrParseNode*)yyvsp[0], false);
				PyrParseNode *blocklit = (PyrParseNode*)newPyrPushLitNode(NULL, block);
				PyrParseNode* args2 = (PyrParseNode*)linkNextNode(blocklit, (PyrParseNode*)yyvsp[-1]);

				yyval = (intptr_t)newPyrCallNode(selectornode, args2, 0, 0);
			}
#line 2946 "lang11d_tab.cpp"
    break;

  case 89: /* qual: exprseq nextqual  */
#line 540 "lang11d"
                        {
				PyrSlot slot;
				SetSymbol(&slot, getsym("if"));
				PyrSlotNode* selectornode = newPyrSlotNode(&slot);
				PyrParseNode *block = (PyrParseNode*)newPyrBlockNode(0, 0, (PyrParseNode*)yyvsp[0], false);
				PyrParseNode *blocklit = (PyrParseNode*)newPyrPushLitNode(NULL, block);
				PyrParseNode* args2 = (PyrParseNode*)linkNextNode((PyrParseNode*)yyvsp[-1], blocklit);

				yyval = (intptr_t)newPyrCallNode(selectornode, args2, 0, 0);
			}
#line 2961 "lang11d_tab.cpp"
    break;

  case 90: /* qual: ':' ':' exprseq nextqual  */
#line 551 "lang11d"
                        {
				yyval = (intptr_t)newPyrDropNode((PyrParseNode*)yyvsp[-1], (PyrParseNode*)yyvsp[0]);
			}
#line 2969 "lang11d_tab.cpp"
    break;

  case 91: /* qual: ':' WHILE exprseq nextqual  */
#line 555 "lang11d"
                        {
				PyrSlot slot;
				SetSymbol(&slot, getsym("alwaysYield"));
				PyrSlotNode* selectornode1 = newPyrSlotNode(&slot);

				SetSymbol(&slot, getsym("if"));
				PyrSlotNode* selectornode2 = newPyrSlotNode(&slot);

				SetNil(&slot);
				PyrParseNode *pushnil = (PyrParseNode*)newPyrPushLitNode(newPyrSlotNode(&slot), NULL);

				PyrParseNode *yieldNil = (PyrParseNode*)newPyrCallNode(selectornode1, pushnil, 0, 0);

				PyrParseNode *block1 = (PyrParseNode*)newPyrBlockNode(0, 0, yieldNil, false);
				PyrParseNode *blocklit1 = (PyrParseNode*)newPyrPushLitNode(NULL, block1);
				PyrParseNode *block2 = (PyrParseNode*)newPyrBlockNode(0, 0, (PyrParseNode*)yyvsp[0], false);
				PyrParseNode *blocklit2 = (PyrParseNode*)newPyrPushLitNode(NULL, block2);
				PyrParseNode* args2 = (PyrParseNode*)linkNextNode((PyrParseNode*)yyvsp[-1], blocklit2);
				PyrParseNode* args3 = (PyrParseNode*)linkNextNode(args2, blocklit1);

				yyval = (intptr_t)newPyrCallNode(selectornode2, args3, 0, 0);
			}
#line 2996 "lang11d_tab.cpp"
    break;

  case 98: /* expr1: '(' exprseq ')'  */
#line 586 "lang11d"
                        {
				PyrParseNode* node = (PyrParseNode*)yyvsp[-1];
				node->mParens = 1;
				yyval = yyvsp[-1];
			}
#line 3006 "lang11d_tab.cpp"
    break;

  case 99: /* expr1: '~' name  */
#line 592 "lang11d"
                        {
				PyrParseNode* argnode;
				PyrSlotNode* selectornode;
				PyrSlot slot;
				argnode = (PyrParseNode*)newPyrPushLitNode((PyrSlotNode*)yyvsp[0], NULL);
				SetSymbol(&slot, s_envirGet);
				selectornode = newPyrSlotNode(&slot);
				yyval = (intptr_t)newPyrCallNode(selectornode, argnode, 0, 0);
			}
#line 3020 "lang11d_tab.cpp"
    break;

  case 100: /* expr1: '[' arrayelems ']'  */
#line 602 "lang11d"
                        { yyval = (intptr_t)newPyrDynListNode(0, (PyrParseNode*)yyvsp[-1]); }
#line 3026 "lang11d_tab.cpp"
    break;

  case 101: /* expr1: '(' valrange2 ')'  */
#line 604 "lang11d"
                        { yyval = yyvsp[-1]; }
#line 3032 "lang11d_tab.cpp"
    break;

  case 102: /* expr1: '(' ':' valrange3 ')'  */
#line 606 "lang11d"
                        { yyval = yyvsp[-1]; }
#line 3038 "lang11d_tab.cpp"
    break;

  case 103: /* expr1: '(' dictslotlist ')'  */
#line 608 "lang11d"
                        { yyval = (intptr_t)newPyrDynDictNode((PyrParseNode*)yyvsp[-1]); }
#line 3044 "lang11d_tab.cpp"
    break;

  case 104: /* expr1: pseudovar  */
#line 610 "lang11d"
                        { yyval = (intptr_t)newPyrPushNameNode((PyrSlotNode*)yyvsp[0]); }
#line 3050 "lang11d_tab.cpp"
    break;

  case 105: /* expr1: expr1 '[' arglist1 ']'  */
#line 612 "lang11d"
                        {
				PyrSlotNode *selectornode;
				PyrSlot slot;
				PyrParseNode* args;

				SetSymbol(&slot, s_at);
				selectornode = newPyrSlotNode(&slot);
				args = linkNextNode(
					(PyrParseNode*)yyvsp[-3],
					(PyrParseNode*)yyvsp[-1]);
				yyval = (intptr_t)newPyrCallNode(selectornode, args, 0, 0);
			}
#line 3067 "lang11d_tab.cpp"
    break;

  case 107: /* valrangex1: expr1 '[' arglist1 DOTDOT ']'  */
#line 628 "lang11d"
                        {
				PyrSlotNode *selectornode;
				PyrPushLitNode *nilnode1, *nilnode2;
				PyrSlot selectorSlot, nilSlot;
				PyrParseNode* args;

				int arglen = nodeListLength((PyrParseNode*)yyvsp[-2]);
				if (arglen > 2) {
					error("ArrayedCollection subrange has too many arguments.\n");
					nodePostErrorLine((PyrParseNode*)yyvsp[-2]);
					compileErrors++;
				}

				SetNil(&nilSlot);
				nilnode2 = newPyrPushLitNode(newPyrSlotNode(&nilSlot), NULL);

				SetSymbol(&selectorSlot, s_copyseries);
				selectornode = newPyrSlotNode(&selectorSlot);
				args = linkNextNode((PyrParseNode*)yyvsp[-4], (PyrParseNode*)yyvsp[-2]);
				if (arglen < 2) {
					nilnode1 = newPyrPushLitNode(newPyrSlotNode(&nilSlot), NULL);
					args = linkNextNode(args, nilnode1);
				}
				args = linkNextNode(args, nilnode2);
				yyval = (intptr_t)newPyrCallNode(selectornode, args, 0, 0);
			}
#line 3098 "lang11d_tab.cpp"
    break;

  case 108: /* valrangex1: expr1 '[' DOTDOT exprseq ']'  */
#line 655 "lang11d"
                        {
				PyrSlotNode *selectornode;
				PyrPushLitNode *nilnode1, *nilnode2;
				PyrSlot selectorSlot, nilSlot;
				PyrParseNode* args;

				SetNil(&nilSlot);
				nilnode1 = newPyrPushLitNode(newPyrSlotNode(&nilSlot), NULL);
				nilnode2 = newPyrPushLitNode(newPyrSlotNode(&nilSlot), NULL);

				SetSymbol(&selectorSlot, s_copyseries);
				selectornode = newPyrSlotNode(&selectorSlot);
				args = linkNextNode((PyrParseNode*)yyvsp[-4], nilnode1);
				args = linkNextNode(args, nilnode2);
				args = linkNextNode(args, (PyrParseNode*)yyvsp[-1]);
				yyval = (intptr_t)newPyrCallNode(selectornode, args, 0, 0);
			}
#line 3120 "lang11d_tab.cpp"
    break;

  case 109: /* valrangex1: expr1 '[' arglist1 DOTDOT exprseq ']'  */
#line 673 "lang11d"
                        {
				PyrSlotNode *selectornode;
				PyrPushLitNode *nilnode1;
				PyrSlot selectorSlot, nilSlot;
				PyrParseNode* args;

				int arglen = nodeListLength((PyrParseNode*)yyvsp[-3]);
				if (arglen > 2) {
					error("ArrayedCollection subrange has too many arguments.\n");
					nodePostErrorLine((PyrParseNode*)yyvsp[-3]);
					compileErrors++;
				}

				SetSymbol(&selectorSlot, s_copyseries);
				selectornode = newPyrSlotNode(&selectorSlot);
				args = linkNextNode((PyrParseNode*)yyvsp[-5], (PyrParseNode*)yyvsp[-3]);
				if (arglen < 2) {
					SetNil(&nilSlot);
					nilnode1 = newPyrPushLitNode(newPyrSlotNode(&nilSlot), NULL);
					args = linkNextNode(args, nilnode1);
				}
				args = linkNextNode(args, (PyrParseNode*)yyvsp[-1]);
				yyval = (intptr_t)newPyrCallNode(selectornode, args, 0, 0);
			}
#line 3149 "lang11d_tab.cpp"
    break;

  case 110: /* valrangeassign: expr1 '[' arglist1 DOTDOT ']' '=' expr  */
#line 700 "lang11d"
                        {
				PyrSlotNode *selectornode;
				PyrPushLitNode *nilnode1, *nilnode2;
				PyrSlot selectorSlot, nilSlot;
				PyrParseNode* args;

				int arglen = nodeListLength((PyrParseNode*)yyvsp[-4]);
				if (arglen > 2) {
					error("ArrayedCollection subrange has too many arguments.\n");
					nodePostErrorLine((PyrParseNode*)yyvsp[-4]);
					compileErrors++;
				}

				SetNil(&nilSlot);
				nilnode2 = newPyrPushLitNode(newPyrSlotNode(&nilSlot), NULL);

				SetSymbol(&selectorSlot, s_putseries);
				selectornode = newPyrSlotNode(&selectorSlot);
				args = linkNextNode((PyrParseNode*)yyvsp[-6], (PyrParseNode*)yyvsp[-4]);
				if (arglen < 2) {
					nilnode1 = newPyrPushLitNode(newPyrSlotNode(&nilSlot), NULL);
					args = linkNextNode(args, nilnode1);
				}
				args = linkNextNode(args, nilnode2);
				args = linkNextNode(args, (PyrParseNode*)yyvsp[0]);
				yyval = (intptr_t)newPyrCallNode(selectornode, args, 0, 0);
			}
#line 3181 "lang11d_tab.cpp"
    break;

  case 111: /* valrangeassign: expr1 '[' DOTDOT exprseq ']' '=' expr  */
#line 728 "lang11d"
                        {
				PyrSlotNode *selectornode;
				PyrPushLitNode *nilnode1, *nilnode2;
				PyrSlot selectorSlot, nilSlot;
				PyrParseNode* args;

				SetNil(&nilSlot);
				nilnode1 = newPyrPushLitNode(newPyrSlotNode(&nilSlot), NULL);
				nilnode2 = newPyrPushLitNode(newPyrSlotNode(&nilSlot), NULL);

				SetSymbol(&selectorSlot, s_putseries);
				selectornode = newPyrSlotNode(&selectorSlot);
				args = linkNextNode((PyrParseNode*)yyvsp[-6], nilnode1);
				args = linkNextNode(args, nilnode2);
				args = linkNextNode(args, (PyrParseNode*)yyvsp[-3]);
				args = linkNextNode(args, (PyrParseNode*)yyvsp[0]);
				yyval = (intptr_t)newPyrCallNode(selectornode, args, 0, 0);
			}
#line 3204 "lang11d_tab.cpp"
    break;

  case 112: /* valrangeassign: expr1 '[' arglist1 DOTDOT exprseq ']' '=' expr  */
#line 747 "lang11d"
                        {
				PyrSlotNode *selectornode;
				PyrPushLitNode *nilnode1;
				PyrSlot selectorSlot, nilSlot;
				PyrParseNode* args;

				int arglen = nodeListLength((PyrParseNode*)yyvsp[-5]);
				if (arglen > 2) {
					error("ArrayedCollection subrange has too many arguments.\n");
					nodePostErrorLine((PyrParseNode*)yyvsp[-5]);
					compileErrors++;
				}

				SetSymbol(&selectorSlot, s_putseries);
				selectornode = newPyrSlotNode(&selectorSlot);
				args = linkNextNode((PyrParseNode*)yyvsp[-7], (PyrParseNode*)yyvsp[-5]);
				if (arglen < 2) {
					SetNil(&nilSlot);
					nilnode1 = newPyrPushLitNode(newPyrSlotNode(&nilSlot), NULL);
					args = linkNextNode(args, nilnode1);
				}
				args = linkNextNode(args, (PyrParseNode*)yyvsp[-3]);
				args = linkNextNode(args, (PyrParseNode*)yyvsp[0]);
				yyval = (intptr_t)newPyrCallNode(selectornode, args, 0, 0);
			}
#line 3234 "lang11d_tab.cpp"
    break;

  case 113: /* valrangexd: expr '.' '[' arglist1 DOTDOT ']'  */
#line 775 "lang11d"
                        {
				PyrSlotNode *selectornode;
				PyrPushLitNode *nilnode1, *nilnode2;
				PyrSlot selectorSlot, nilSlot;
				PyrParseNode* args;

				int arglen = nodeListLength((PyrParseNode*)yyvsp[-2]);
				if (arglen > 2) {
					error("ArrayedCollection subrange has too many arguments.\n");
					nodePostErrorLine((PyrParseNode*)yyvsp[-3]);
					compileErrors++;
				}

				SetNil(&nilSlot);
				nilnode2 = newPyrPushLitNode(newPyrSlotNode(&nilSlot), NULL);

				SetSymbol(&selectorSlot, s_copyseries);
				selectornode = newPyrSlotNode(&selectorSlot);
				args = linkNextNode((PyrParseNode*)yyvsp[-5], (PyrParseNode*)yyvsp[-2]);
				if (arglen < 2) {
					nilnode1 = newPyrPushLitNode(newPyrSlotNode(&nilSlot), NULL);
					args = linkNextNode(args, nilnode1);
				}
				args = linkNextNode(args, nilnode2);
				yyval = (intptr_t)newPyrCallNode(selectornode, args, 0, 0);
			}
#line 3265 "lang11d_tab.cpp"
    break;

  case 114: /* valrangexd: expr '.' '[' DOTDOT exprseq ']'  */
#line 802 "lang11d"
                        {
				PyrSlotNode *selectornode;
				PyrPushLitNode *nilnode1, *nilnode2;
				PyrSlot selectorSlot, nilSlot;
				PyrParseNode* args;

				SetNil(&nilSlot);
				nilnode1 = newPyrPushLitNode(newPyrSlotNode(&nilSlot), NULL);
				nilnode2 = newPyrPushLitNode(newPyrSlotNode(&nilSlot), NULL);

				SetSymbol(&selectorSlot, s_copyseries);
				selectornode = newPyrSlotNode(&selectorSlot);
				args = linkNextNode((PyrParseNode*)yyvsp[-5], nilnode1);
				args = linkNextNode(args, nilnode2);
				args = linkNextNode(args, (PyrParseNode*)yyvsp[-1]);
				yyval = (intptr_t)newPyrCallNode(selectornode, args, 0, 0);
			}
#line 3287 "lang11d_tab.cpp"
    break;

  case 115: /* valrangexd: expr '.' '[' arglist1 DOTDOT exprseq ']'  */
#line 820 "lang11d"
                        {
				PyrSlotNode *selectornode;
				PyrPushLitNode *nilnode1;
				PyrSlot selectorSlot, nilSlot;
				PyrParseNode* args;

				int arglen = nodeListLength((PyrParseNode*)yyvsp[-3]);
				if (arglen > 2) {
					error("ArrayedCollection subrange has too many arguments.\n");
					nodePostErrorLine((PyrParseNode*)yyvsp[-4]);
					compileErrors++;
				}

				SetSymbol(&selectorSlot, s_copyseries);
				selectornode = newPyrSlotNode(&selectorSlot);
				args = linkNextNode((PyrParseNode*)yyvsp[-6], (PyrParseNode*)yyvsp[-3]);
				if (arglen < 2) {
					SetNil(&nilSlot);
					nilnode1 = newPyrPushLitNode(newPyrSlotNode(&nilSlot), NULL);
					args = linkNextNode(args, nilnode1);
				}
				args = linkNextNode(args, (PyrParseNode*)yyvsp[-1]);
				yyval = (intptr_t)newPyrCallNode(selectornode, args, 0, 0);
			}
#line 3316 "lang11d_tab.cpp"
    break;

  case 116: /* valrangexd: expr '.' '[' arglist1 DOTDOT ']' '=' expr  */
#line 845 "lang11d"
                        {
				PyrSlotNode *selectornode;
				PyrPushLitNode *nilnode1, *nilnode2;
				PyrSlot selectorSlot, nilSlot;
				PyrParseNode* args;

				int arglen = nodeListLength((PyrParseNode*)yyvsp[-4]);
				if (arglen > 2) {
					error("ArrayedCollection subrange has too many arguments.\n");
					nodePostErrorLine((PyrParseNode*)yyvsp[-5]);
					compileErrors++;
				}

				SetNil(&nilSlot);
				nilnode2 = newPyrPushLitNode(newPyrSlotNode(&nilSlot), NULL);

				SetSymbol(&selectorSlot, s_putseries);
				selectornode = newPyrSlotNode(&selectorSlot);
				args = linkNextNode((PyrParseNode*)yyvsp[-7], (PyrParseNode*)yyvsp[-4]);
				if (arglen < 2) {
					nilnode1 = newPyrPushLitNode(newPyrSlotNode(&nilSlot), NULL);
					args = linkNextNode(args, nilnode1);
				}
				args = linkNextNode(args, nilnode2);
				args = linkNextNode(args, (PyrParseNode*)yyvsp[0]);
				yyval = (intptr_t)newPyrCallNode(selectornode, args, 0, 0);
			}
#line 3348 "lang11d_tab.cpp"
    break;

  case 117: /* valrangexd: expr '.' '[' DOTDOT exprseq ']' '=' expr  */
#line 873 "lang11d"
                        {
				PyrSlotNode *selectornode;
				PyrPushLitNode *nilnode1, *nilnode2;
				PyrSlot selectorSlot, nilSlot;
				PyrParseNode* args;

				SetNil(&nilSlot);
				nilnode1 = newPyrPushLitNode(newPyrSlotNode(&nilSlot), NULL);
				nilnode2 = newPyrPushLitNode(newPyrSlotNode(&nilSlot), NULL);

				SetSymbol(&selectorSlot, s_putseries);
				selectornode = newPyrSlotNode(&selectorSlot);
				args = linkNextNode((PyrParseNode*)yyvsp[-7], nilnode1);
				args = linkNextNode(args, nilnode2);
				args = linkNextNode(args, (PyrParseNode*)yyvsp[-3]);
				args = linkNextNode(args, (PyrParseNode*)yyvsp[0]);
				yyval = (intptr_t)newPyrCallNode(selectornode, args, 0, 0);
			}
#line 3371 "lang11d_tab.cpp"
    break;

  case 118: /* valrangexd: expr '.' '[' arglist1 DOTDOT exprseq ']' '=' expr  */
#line 892 "lang11d"
                        {
				PyrSlotNode *selectornode;
				PyrPushLitNode *nilnode1;
				PyrSlot selectorSlot, nilSlot;
				PyrParseNode* args;

				int arglen = nodeListLength((PyrParseNode*)yyvsp[-5]);
				if (arglen > 2) {
					error("ArrayedCollection subrange has too many arguments.\n");
					nodePostErrorLine((PyrParseNode*)yyvsp[-6]);
					compileErrors++;
				}

				SetSymbol(&selectorSlot, s_putseries);
				selectornode = newPyrSlotNode(&selectorSlot);
				args = linkNextNode((PyrParseNode*)yyvsp[-8], (PyrParseNode*)yyvsp[-5]);
				if (arglen < 2) {
					SetNil(&nilSlot);
					nilnode1 = newPyrPushLitNode(newPyrSlotNode(&nilSlot), NULL);
					args = linkNextNode(args, nilnode1);
				}
				args = linkNextNode(args, (PyrParseNode*)yyvsp[-3]);
				args = linkNextNode(args, (PyrParseNode*)yyvsp[0]);
				yyval = (intptr_t)newPyrCallNode(selectornode, args, 0, 0);
			}
#line 3401 "lang11d_tab.cpp"
    break;

  case 119: /* valrange2: exprseq DOTDOT  */
#line 920 "lang11d"
                        {
				// if this is not used in a 'do' or list comprehension, then should return an error.
				PyrSlotNode *selectornode;
				PyrPushLitNode *nilnode, *nilnode2;
				PyrSlot selectorSlot, nilSlot;
				PyrParseNode* args;

				SetNil(&nilSlot);
				nilnode = newPyrPushLitNode(newPyrSlotNode(&nilSlot), NULL);
				nilnode2 = newPyrPushLitNode(newPyrSlotNode(&nilSlot), NULL);

				SetSymbol(&selectorSlot, s_series);
				selectornode = newPyrSlotNode(&selectorSlot);
				args = linkNextNode((PyrParseNode*)yyvsp[-1], nilnode);
				args = linkNextNode(args, nilnode2);
				yyval = (intptr_t)newPyrCallNode(selectornode, args, 0, 0);
			}
#line 3423 "lang11d_tab.cpp"
    break;

  case 120: /* valrange2: DOTDOT exprseq  */
#line 939 "lang11d"
                        {
				PyrSlotNode *selectornode;
				PyrPushLitNode *nilnode, *zeronode;
				PyrSlot selectorSlot, nilSlot, zeroSlot;
				PyrParseNode* args;

				SetInt(&zeroSlot, 0);
				SetNil(&nilSlot);
				nilnode = newPyrPushLitNode(newPyrSlotNode(&nilSlot), NULL);
				zeronode = newPyrPushLitNode(newPyrSlotNode(&zeroSlot), NULL);

				SetSymbol(&selectorSlot, s_series);
				selectornode = newPyrSlotNode(&selectorSlot);
				args = linkNextNode(zeronode, nilnode);
				args = linkNextNode(args, (PyrParseNode*)yyvsp[0]);
				yyval = (intptr_t)newPyrCallNode(selectornode, args, 0, 0);
			}
#line 3445 "lang11d_tab.cpp"
    break;

  case 121: /* valrange2: exprseq DOTDOT exprseq  */
#line 958 "lang11d"
                        {
				PyrSlotNode *selectornode;
				PyrPushLitNode *nilnode;
				PyrSlot selectorSlot, nilSlot;
				PyrParseNode* args;

				SetNil(&nilSlot);
				nilnode = newPyrPushLitNode(newPyrSlotNode(&nilSlot), NULL);

				SetSymbol(&selectorSlot, s_series);
				selectornode = newPyrSlotNode(&selectorSlot);
				args = linkNextNode((PyrParseNode*)yyvsp[-2], nilnode);
				args = linkNextNode(args, (PyrParseNode*)yyvsp[0]);
				yyval = (intptr_t)newPyrCallNode(selectornode, args, 0, 0);
			}
#line 3465 "lang11d_tab.cpp"
    break;

  case 122: /* valrange2: exprseq ',' exprseq DOTDOT exprseq  */
#line 975 "lang11d"
                        {
				PyrSlotNode *selectornode;
				PyrSlot selectorSlot;
				PyrParseNode* args;

				SetSymbol(&selectorSlot, s_series);
				selectornode = newPyrSlotNode(&selectorSlot);
				args = linkNextNode(
					(PyrParseNode*)yyvsp[-4],
					(PyrParseNode*)yyvsp[-2]);
				args = linkNextNode(args, (PyrParseNode*)yyvsp[0]);
				yyval = (intptr_t)newPyrCallNode(selectornode, args, 0, 0);
			}
#line 3483 "lang11d_tab.cpp"
    break;

  case 123: /* valrange2: exprseq ',' exprseq DOTDOT  */
#line 989 "lang11d"
                        {
				// if this is not used in a 'do' or list comprehension, then should return an error.
				PyrSlotNode *selectornode;
				PyrSlot selectorSlot, nilSlot;
				PyrParseNode* args;
				PyrPushLitNode *nilnode;

				SetNil(&nilSlot);
				nilnode = newPyrPushLitNode(newPyrSlotNode(&nilSlot), NULL);

				SetSymbol(&selectorSlot, s_series);
				selectornode = newPyrSlotNode(&selectorSlot);
				args = linkNextNode(
					(PyrParseNode*)yyvsp[-3],
					(PyrParseNode*)yyvsp[-1]);
				args = linkNextNode(args, nilnode);
				yyval = (intptr_t)newPyrCallNode(selectornode, args, 0, 0);
			}
#line 3506 "lang11d_tab.cpp"
    break;

  case 124: /* valrange3: DOTDOT exprseq  */
#line 1010 "lang11d"
                        {
				PyrSlotNode *selectornode;
				PyrPushLitNode *nilnode, *zeronode;
				PyrSlot selectorSlot, nilSlot, zeroSlot;
				PyrParseNode* args;

				SetInt(&zeroSlot, 0);
				SetNil(&nilSlot);
				nilnode = newPyrPushLitNode(newPyrSlotNode(&nilSlot), NULL);
				zeronode = newPyrPushLitNode(newPyrSlotNode(&zeroSlot), NULL);

				SetSymbol(&selectorSlot, getsym("seriesIter"));
				selectornode = newPyrSlotNode(&selectorSlot);
				args = linkNextNode(zeronode, nilnode);
				args = linkNextNode(args, (PyrParseNode*)yyvsp[0]);
				yyval = (intptr_t)newPyrCallNode(selectornode, args, 0, 0);
			}
#line 3528 "lang11d_tab.cpp"
    break;

  case 125: /* valrange3: exprseq DOTDOT  */
#line 1029 "lang11d"
                        {
				PyrSlotNode *selectornode;
				PyrPushLitNode *nilnode, *nilnode2;
				PyrSlot selectorSlot, nilSlot;
				PyrParseNode* args;

				SetNil(&nilSlot);
				nilnode = newPyrPushLitNode(newPyrSlotNode(&nilSlot), NULL);
				nilnode2 = newPyrPushLitNode(newPyrSlotNode(&nilSlot), NULL);

				SetSymbol(&selectorSlot, getsym("seriesIter"));
				selectornode = newPyrSlotNode(&selectorSlot);
				args = linkNextNode((PyrParseNode*)yyvsp[-1], nilnode);
				args = linkNextNode(args, nilnode2);
				yyval = (intptr_t)newPyrCallNode(selectornode, args, 0, 0);
			}
#line 3549 "lang11d_tab.cpp"
    break;

  case 126: /* valrange3: exprseq DOTDOT exprseq  */
#line 1047 "lang11d"
                        {
				PyrSlotNode *selectornode;
				PyrPushLitNode *nilnode;
				PyrSlot selectorSlot, nilSlot;
				PyrParseNode* args;

				SetNil(&nilSlot);
				nilnode = newPyrPushLitNode(newPyrSlotNode(&nilSlot), NULL);

				SetSymbol(&selectorSlot, getsym("seriesIter"));
				selectornode = newPyrSlotNode(&selectorSlot);
				args = linkNextNode((PyrParseNode*)yyvsp[-2], nilnode);
				args = linkNextNode(args, (PyrParseNode*)yyvsp[0]);
				yyval = (intptr_t)newPyrCallNode(selectornode, args, 0, 0);
			}
#line 3569 "lang11d_tab.cpp"
    break;

  case 127: /* valrange3: exprseq ',' exprseq DOTDOT  */
#line 1064 "lang11d"
                        {
				PyrSlotNode *selectornode;
				PyrPushLitNode *nilnode;
				PyrSlot selectorSlot, nilSlot;
				PyrParseNode* args;

				SetNil(&nilSlot);
				nilnode = newPyrPushLitNode(newPyrSlotNode(&nilSlot), NULL);

				SetSymbol(&selectorSlot, getsym("seriesIter"));
				selectornode = newPyrSlotNode(&selectorSlot);
				args = linkNextNode((PyrParseNode*)yyvsp[-3], (PyrParseNode*)yyvsp[-1]);
				args = linkNextNode(args, nilnode);
				yyval = (intptr_t)newPyrCallNode(selectornode, args, 0, 0);
			}
#line 3589 "lang11d_tab.cpp"
    break;

  case 128: /* valrange3: exprseq ',' exprseq DOTDOT exprseq  */
#line 1080 "lang11d"
                        {
				PyrSlotNode *selectornode;
				PyrSlot selectorSlot;
				PyrParseNode* args;

				SetSymbol(&selectorSlot, getsym("seriesIter"));
				selectornode = newPyrSlotNode(&selectorSlot);
				args = linkNextNode(
					(PyrParseNode*)yyvsp[-4],
					(PyrParseNode*)yyvsp[-2]);
				args = linkNextNode(args, (PyrParseNode*)yyvsp[0]);
				yyval = (intptr_t)newPyrCallNode(selectornode, args, 0, 0);
			}
#line 3607 "lang11d_tab.cpp"
    break;

  case 132: /* expr: classname  */
#line 1098 "lang11d"
                            { yyval = (intptr_t)newPyrPushNameNode((PyrSlotNode*)yyvsp[0]); }
#line 3613 "lang11d_tab.cpp"
    break;

  case 133: /* expr: expr '.' '[' arglist1 ']'  */
#line 1100 "lang11d"
                        {
				PyrSlotNode *selectornode;
				PyrSlot slot;
				PyrParseNode* args;

				SetSymbol(&slot, s_at);
				selectornode = newPyrSlotNode(&slot);
				args = linkNextNode(
					(PyrParseNode*)yyvsp[-4],
					(PyrParseNode*)yyvsp[-1]);
				yyval = (intptr_t)newPyrCallNode(selectornode, args, 0, 0);
			}
#line 3630 "lang11d_tab.cpp"
    break;

  case 134: /* expr: '`' expr  */
#line 1113 "lang11d"
                        {
				PyrParseNode *node, *args;
				PyrSlotNode *slotnode;
				PyrSlot slot;

				SetSymbol(&slot, s_ref);
				slotnode = newPyrSlotNode(&slot);
				node = (PyrParseNode*)newPyrPushNameNode(slotnode);
				args = linkNextNode(node, (PyrParseNode*)yyvsp[0]);
				SetSymbol(&slot, s_new);
				slotnode = newPyrSlotNode(&slot);
				yyval = (intptr_t)newPyrCallNode(slotnode, args, 0, 0);
			}
#line 3648 "lang11d_tab.cpp"
    break;

  case 135: /* expr: expr binop2 adverb expr  */
#line 1127 "lang11d"
                        {
				yyval = (intptr_t)newPyrBinopCallNode((PyrSlotNode*)yyvsp[-2],
						(PyrParseNode*)yyvsp[-3], (PyrParseNode*)yyvsp[0], (PyrParseNode*)yyvsp[-1]);
			}
#line 3657 "lang11d_tab.cpp"
    break;

  case 136: /* expr: name '=' expr  */
#line 1132 "lang11d"
                        {
				yyval = (intptr_t)newPyrAssignNode((PyrSlotNode*)yyvsp[-2], (PyrParseNode*)yyvsp[0], 0);
			}
#line 3665 "lang11d_tab.cpp"
    break;

  case 137: /* expr: '~' name '=' expr  */
#line 1136 "lang11d"
                        {
				PyrParseNode *argnode, *args;
				PyrSlotNode* selectornode;
				PyrSlot slot;
				argnode = (PyrParseNode*)newPyrPushLitNode((PyrSlotNode*)yyvsp[-2], NULL);
				args = linkNextNode(argnode, (PyrParseNode*)yyvsp[0]);
				SetSymbol(&slot, s_envirPut);
				selectornode = newPyrSlotNode(&slot);
				yyval = (intptr_t)newPyrCallNode(selectornode, args, 0, 0);
			}
#line 3680 "lang11d_tab.cpp"
    break;

  case 138: /* expr: expr '.' name '=' expr  */
#line 1147 "lang11d"
                        {
				yyval = (intptr_t)newPyrSetterNode((PyrSlotNode*)yyvsp[-2],
						(PyrParseNode*)yyvsp[-4], (PyrParseNode*)yyvsp[0]);
			}
#line 3689 "lang11d_tab.cpp"
    break;

  case 139: /* expr: name '(' arglist1 optkeyarglist ')' '=' expr  */
#line 1152 "lang11d"
                        {
				if (yyvsp[-3] != 0) {
					error("Setter method called with keyword arguments.\n");
					nodePostErrorLine((PyrParseNode*)yyvsp[-3]);
					compileErrors++;
				}
				yyval = (intptr_t)newPyrSetterNode((PyrSlotNode*)yyvsp[-6],
						(PyrParseNode*)yyvsp[-4], (PyrParseNode*)yyvsp[0]);
			}
#line 3703 "lang11d_tab.cpp"
    break;

  case 140: /* expr: '#' mavars '=' expr  */
#line 1162 "lang11d"
                        {
				yyval = (intptr_t)newPyrMultiAssignNode((PyrMultiAssignVarListNode*)yyvsp[-2],
					(PyrParseNode*)yyvsp[0], 0);
			}
#line 3712 "lang11d_tab.cpp"
    break;

  case 141: /* expr: expr1 '[' arglist1 ']' '=' expr  */
#line 1167 "lang11d"
                        {
				PyrSlotNode *selectornode;
				PyrSlot slot;
				PyrParseNode* args;

				SetSymbol(&slot, s_put);
				selectornode = newPyrSlotNode(&slot);
				args = linkNextNode(
					(PyrParseNode*)yyvsp[-5],
					(PyrParseNode*)yyvsp[-3]);
				args = linkNextNode( args, (PyrParseNode*)yyvsp[0]);
				yyval = (intptr_t)newPyrCallNode(selectornode, args, 0, 0);
			}
#line 3730 "lang11d_tab.cpp"
    break;

  case 142: /* expr: expr '.' '[' arglist1 ']' '=' expr  */
#line 1181 "lang11d"
                        {
				PyrSlotNode *selectornode;
				PyrSlot slot;
				PyrParseNode* args;

				SetSymbol(&slot, s_put);
				selectornode = newPyrSlotNode(&slot);
				args = linkNextNode(
					(PyrParseNode*)yyvsp[-6],
					(PyrParseNode*)yyvsp[-3]);
				args = linkNextNode( args, (PyrParseNode*)yyvsp[0]);
				yyval = (intptr_t)newPyrCallNode(selectornode, args, 0, 0);
			}
#line 3748 "lang11d_tab.cpp"
    break;

  case 143: /* adverb: %empty  */
#line 1196 "lang11d"
          { yyval = 0; }
#line 3754 "lang11d_tab.cpp"
    break;

  case 144: /* adverb: '.' name  */
#line 1197 "lang11d"
                           { yyval = (intptr_t)newPyrPushLitNode((PyrSlotNode*)yyvsp[0], NULL); }
#line 3760 "lang11d_tab.cpp"
    break;

  case 145: /* adverb: '.' integer  */
#line 1198 "lang11d"
                              { yyval = (intptr_t)newPyrPushLitNode((PyrSlotNode*)yyvsp[0], NULL); }
#line 3766 "lang11d_tab.cpp"
    break;

  case 146: /* adverb: '.' '(' exprseq ')'  */
#line 1199 "lang11d"
                                      { yyval = yyvsp[-1]; }
#line 3772 "lang11d_tab.cpp"
    break;

  case 148: /* exprn: exprn ';' expr  */
#line 1204 "lang11d"
                        {
				yyval = (intptr_t)newPyrDropNode((PyrParseNode*)yyvsp[-2], (PyrParseNode*)yyvsp[0]);
			}
#line 3780 "lang11d_tab.cpp"
    break;

  case 150: /* arrayelems: %empty  */
#line 1212 "lang11d"
                  { yyval = 0; }
#line 3786 "lang11d_tab.cpp"
    break;

  case 151: /* arrayelems: arrayelems1 optcomma  */
#line 1214 "lang11d"
                          { yyval = yyvsp[-1]; }
#line 3792 "lang11d_tab.cpp"
    break;

  case 153: /* arrayelems1: exprseq ':' exprseq  */
#line 1219 "lang11d"
                                { yyval = (intptr_t)linkNextNode((PyrParseNode*)yyvsp[-2], (PyrParseNode*)yyvsp[0]); }
#line 3798 "lang11d_tab.cpp"
    break;

  case 154: /* arrayelems1: keybinop exprseq  */
#line 1221 "lang11d"
                                {
					PyrParseNode* key = newPyrPushLitNode((PyrSlotNode*)yyvsp[-1], NULL);
					yyval = (intptr_t)linkNextNode(key, (PyrParseNode*)yyvsp[0]);
				}
#line 3807 "lang11d_tab.cpp"
    break;

  case 155: /* arrayelems1: arrayelems1 ',' exprseq  */
#line 1226 "lang11d"
                                        { yyval = (intptr_t)linkNextNode((PyrParseNode*)yyvsp[-2], (PyrParseNode*)yyvsp[0]); }
#line 3813 "lang11d_tab.cpp"
    break;

  case 156: /* arrayelems1: arrayelems1 ',' keybinop exprseq  */
#line 1228 "lang11d"
                                {
					PyrParseNode* elems;
					PyrParseNode* key = newPyrPushLitNode((PyrSlotNode*)yyvsp[-1], NULL);
					elems = (PyrParseNode*)linkNextNode(key, (PyrParseNode*)yyvsp[0]);
					yyval = (intptr_t)linkNextNode((PyrParseNode*)yyvsp[-3], elems);
				}
#line 3824 "lang11d_tab.cpp"
    break;

  case 157: /* arrayelems1: arrayelems1 ',' exprseq ':' exprseq  */
#line 1235 "lang11d"
                                {
					PyrParseNode* elems;
					elems = (PyrParseNode*)linkNextNode((PyrParseNode*)yyvsp[-2], (PyrParseNode*)yyvsp[0]);
					yyval = (intptr_t)linkNextNode((PyrParseNode*)yyvsp[-4], elems);
				}
#line 3834 "lang11d_tab.cpp"
    break;

  case 159: /* arglist1: arglist1 ',' exprseq  */
#line 1244 "lang11d"
                                        { yyval = (intptr_t)linkNextNode((PyrParseNode*)yyvsp[-2], (PyrParseNode*)yyvsp[0]); }
#line 3840 "lang11d_tab.cpp"
    break;

  case 160: /* arglistv1: '*' exprseq  */
#line 1248 "lang11d"
                                { yyval = yyvsp[0]; }
#line 3846 "lang11d_tab.cpp"
    break;

  case 161: /* arglistv1: arglist1 ',' '*' exprseq  */
#line 1250 "lang11d"
                                        { yyval = (intptr_t)linkNextNode((PyrParseNode*)yyvsp[-3], (PyrParseNode*)yyvsp[0]); }
#line 3852 "lang11d_tab.cpp"
    break;

  case 163: /* keyarglist1: keyarglist1 ',' keyarg  */
#line 1263 "lang11d"
                                        { yyval = (intptr_t)linkNextNode((PyrParseNode*)yyvsp[-2], (PyrParseNode*)yyvsp[0]); }
#line 3858 "lang11d_tab.cpp"
    break;

  case 170: /* keyarg: keybinop exprseq  */
#line 1289 "lang11d"
                                { yyval = (intptr_t)newPyrPushKeyArgNode((PyrSlotNode*)yyvsp[-1], (PyrParseNode*)yyvsp[0]); }
#line 3864 "lang11d_tab.cpp"
    break;

  case 171: /* optkeyarglist: optcomma  */
#line 1292 "lang11d"
                           { yyval = 0; }
#line 3870 "lang11d_tab.cpp"
    break;

  case 172: /* optkeyarglist: ',' keyarglist1 optcomma  */
#line 1293 "lang11d"
                                                           { yyval = yyvsp[-1]; }
#line 3876 "lang11d_tab.cpp"
    break;

  case 173: /* mavars: mavarlist  */
#line 1297 "lang11d"
                        { yyval = (intptr_t)newPyrMultiAssignVarListNode((PyrSlotNode*)yyvsp[0], NULL); }
#line 3882 "lang11d_tab.cpp"
    break;

  case 174: /* mavars: mavarlist ELLIPSIS name  */
#line 1299 "lang11d"
                        { yyval = (intptr_t)newPyrMultiAssignVarListNode((PyrSlotNode*)yyvsp[-2], (PyrSlotNode*)yyvsp[0]); }
#line 3888 "lang11d_tab.cpp"
    break;

  case 176: /* mavarlist: mavarlist ',' name  */
#line 1304 "lang11d"
                                        { yyval = (intptr_t)linkNextNode((PyrParseNode*)yyvsp[-2], (PyrParseNode*)yyvsp[0]); }
#line 3894 "lang11d_tab.cpp"
    break;

  case 177: /* slotliteral: integer  */
#line 1308 "lang11d"
                                { yyval = (intptr_t)newPyrLiteralNode((PyrSlotNode*)yyvsp[0], NULL); }
#line 3900 "lang11d_tab.cpp"
    break;

  case 178: /* slotliteral: floatp  */
#line 1309 "lang11d"
                                { yyval = (intptr_t)newPyrLiteralNode((PyrSlotNode*)yyvsp[0], NULL); }
#line 3906 "lang11d_tab.cpp"
    break;

  case 179: /* slotliteral: ascii  */
#line 1310 "lang11d"
                                { yyval = (intptr_t)newPyrLiteralNode((PyrSlotNode*)yyvsp[0], NULL); }
#line 3912 "lang11d_tab.cpp"
    break;

  case 180: /* slotliteral: string  */
#line 1311 "lang11d"
                                { yyval = (intptr_t)newPyrLiteralNode((PyrSlotNode*)yyvsp[0], NULL); }
#line 3918 "lang11d_tab.cpp"
    break;

  case 181: /* slotliteral: symbol  */
#line 1312 "lang11d"
                                { yyval = (intptr_t)newPyrLiteralNode((PyrSlotNode*)yyvsp[0], NULL); }
#line 3924 "lang11d_tab.cpp"
    break;

  case 182: /* slotliteral: trueobj  */
#line 1313 "lang11d"
                                { yyval = (intptr_t)newPyrLiteralNode((PyrSlotNode*)yyvsp[0], NULL); }
#line 3930 "lang11d_tab.cpp"
    break;

  case 183: /* slotliteral: falseobj  */
#line 1314 "lang11d"
                                { yyval = (intptr_t)newPyrLiteralNode((PyrSlotNode*)yyvsp[0], NULL); }
#line 3936 "lang11d_tab.cpp"
    break;

  case 184: /* slotliteral: nilobj  */
#line 1315 "lang11d"
                                { yyval = (intptr_t)newPyrLiteralNode((PyrSlotNode*)yyvsp[0], NULL); }
#line 3942 "lang11d_tab.cpp"
    break;

  case 185: /* slotliteral: listlit  */
#line 1316 "lang11d"
                                { yyval = (intptr_t)newPyrLiteralNode(NULL, (PyrParseNode*)yyvsp[0]); }
#line 3948 "lang11d_tab.cpp"
    break;

  case 186: /* blockliteral: block  */
#line 1319 "lang11d"
                        { yyval = (intptr_t)newPyrPushLitNode(NULL, (PyrParseNode*)yyvsp[0]); }
#line 3954 "lang11d_tab.cpp"
    break;

  case 187: /* pushname: name  */
#line 1322 "lang11d"
                                { yyval = (intptr_t)newPyrPushNameNode((PyrSlotNode*)yyvsp[0]); }
#line 3960 "lang11d_tab.cpp"
    break;

  case 188: /* pushliteral: integer  */
#line 1325 "lang11d"
                                { yyval = (intptr_t)newPyrPushLitNode((PyrSlotNode*)yyvsp[0], NULL); }
#line 3966 "lang11d_tab.cpp"
    break;

  case 189: /* pushliteral: floatp  */
#line 1326 "lang11d"
                                        { yyval = (intptr_t)newPyrPushLitNode((PyrSlotNode*)yyvsp[0], NULL); }
#line 3972 "lang11d_tab.cpp"
    break;

  case 190: /* pushliteral: ascii  */
#line 1327 "lang11d"
                                        { yyval = (intptr_t)newPyrPushLitNode((PyrSlotNode*)yyvsp[0], NULL); }
#line 3978 "lang11d_tab.cpp"
    break;

  case 191: /* pushliteral: string  */
#line 1328 "lang11d"
                                        { yyval = (intptr_t)newPyrPushLitNode((PyrSlotNode*)yyvsp[0], NULL); }
#line 3984 "lang11d_tab.cpp"
    break;

  case 192: /* pushliteral: symbol  */
#line 1329 "lang11d"
                                        { yyval = (intptr_t)newPyrPushLitNode((PyrSlotNode*)yyvsp[0], NULL); }
#line 3990 "lang11d_tab.cpp"
    break;

  case 193: /* pushliteral: trueobj  */
#line 1330 "lang11d"
                                        { yyval = (intptr_t)newPyrPushLitNode((PyrSlotNode*)yyvsp[0], NULL); }
#line 3996 "lang11d_tab.cpp"
    break;

  case 194: /* pushliteral: falseobj  */
#line 1331 "lang11d"
                                        { yyval = (intptr_t)newPyrPushLitNode((PyrSlotNode*)yyvsp[0], NULL); }
#line 4002 "lang11d_tab.cpp"
    break;

  case 195: /* pushliteral: nilobj  */
#line 1332 "lang11d"
                                        { yyval = (intptr_t)newPyrPushLitNode((PyrSlotNode*)yyvsp[0], NULL); }
#line 4008 "lang11d_tab.cpp"
    break;

  case 196: /* pushliteral: listlit  */
#line 1333 "lang11d"
                                        { yyval = (intptr_t)newPyrPushLitNode(NULL, (PyrParseNode*)yyvsp[0]); }
#line 4014 "lang11d_tab.cpp"
    break;

  case 197: /* listliteral: integer  */
#line 1336 "lang11d"
                                { yyval = (intptr_t)newPyrLiteralNode((PyrSlotNode*)yyvsp[0], NULL); }
#line 4020 "lang11d_tab.cpp"
    break;

  case 198: /* listliteral: floatp  */
#line 1337 "lang11d"
                                        { yyval = (intptr_t)newPyrLiteralNode((PyrSlotNode*)yyvsp[0], NULL); }
#line 4026 "lang11d_tab.cpp"
    break;

  case 199: /* listliteral: ascii  */
#line 1338 "lang11d"
                                        { yyval = (intptr_t)newPyrLiteralNode((PyrSlotNode*)yyvsp[0], NULL); }
#line 4032 "lang11d_tab.cpp"
    break;

  case 200: /* listliteral: string  */
#line 1339 "lang11d"
                                        { yyval = (intptr_t)newPyrLiteralNode((PyrSlotNode*)yyvsp[0], NULL); }
#line 4038 "lang11d_tab.cpp"
    break;

  case 201: /* listliteral: symbol  */
#line 1340 "lang11d"
                                        { yyval = (intptr_t)newPyrLiteralNode((PyrSlotNode*)yyvsp[0], NULL); }
#line 4044 "lang11d_tab.cpp"
    break;

  case 202: /* listliteral: name  */
#line 1341 "lang11d"
                                        { yyval = (intptr_t)newPyrLiteralNode((PyrSlotNode*)yyvsp[0], NULL); }
#line 4050 "lang11d_tab.cpp"
    break;

  case 203: /* listliteral: trueobj  */
#line 1342 "lang11d"
                                        { yyval = (intptr_t)newPyrLiteralNode((PyrSlotNode*)yyvsp[0], NULL); }
#line 4056 "lang11d_tab.cpp"
    break;

  case 204: /* listliteral: falseobj  */
#line 1343 "lang11d"
                                        { yyval = (intptr_t)newPyrLiteralNode((PyrSlotNode*)yyvsp[0], NULL); }
#line 4062 "lang11d_tab.cpp"
    break;

  case 205: /* listliteral: nilobj  */
#line 1344 "lang11d"
                                        { yyval = (intptr_t)newPyrLiteralNode((PyrSlotNode*)yyvsp[0], NULL); }
#line 4068 "lang11d_tab.cpp"
    break;

  case 206: /* listliteral: listlit2  */
#line 1345 "lang11d"
                                        { yyval = (intptr_t)newPyrLiteralNode(NULL, (PyrParseNode*)yyvsp[0]); }
#line 4074 "lang11d_tab.cpp"
    break;

  case 207: /* listliteral: dictlit2  */
#line 1346 "lang11d"
                                    { yyval = (intptr_t)newPyrLiteralNode(NULL, (PyrParseNode*)yyvsp[0]); }
#line 4080 "lang11d_tab.cpp"
    break;

  case 208: /* block: '{' argdecls funcvardecls funcbody '}'  */
#line 1350 "lang11d"
                                { yyval = (intptr_t)newPyrBlockNode((PyrArgListNode*)yyvsp[-3], (PyrVarListNode*)yyvsp[-2],
					(PyrParseNode*)yyvsp[-1], false); }
#line 4087 "lang11d_tab.cpp"
    break;

  case 209: /* block: BEGINCLOSEDFUNC argdecls funcvardecls funcbody '}'  */
#line 1353 "lang11d"
                                { yyval = (intptr_t)newPyrBlockNode((PyrArgListNode*)yyvsp[-3], (PyrVarListNode*)yyvsp[-2],
					(PyrParseNode*)yyvsp[-1], true); }
#line 4094 "lang11d_tab.cpp"
    break;

  case 210: /* funcvardecls: %empty  */
#line 1357 "lang11d"
                  { yyval = 0; }
#line 4100 "lang11d_tab.cpp"
    break;

  case 211: /* funcvardecls: funcvardecls funcvardecl  */
#line 1359 "lang11d"
                                        { yyval = (intptr_t)linkNextNode((PyrParseNode*)yyvsp[-1], (PyrParseNode*)yyvsp[0]); }
#line 4106 "lang11d_tab.cpp"
    break;

  case 213: /* funcvardecls1: funcvardecls1 funcvardecl  */
#line 1364 "lang11d"
                                        { yyval = (intptr_t)linkNextNode((PyrParseNode*)yyvsp[-1], (PyrParseNode*)yyvsp[0]); }
#line 4112 "lang11d_tab.cpp"
    break;

  case 214: /* funcvardecl: VAR vardeflist ';'  */
#line 1368 "lang11d"
                                { yyval = (intptr_t)newPyrVarListNode((PyrVarDefNode*)yyvsp[-1], varLocal); }
#line 4118 "lang11d_tab.cpp"
    break;

  case 215: /* argdecls: %empty  */
#line 1371 "lang11d"
                  { yyval = 0; }
#line 4124 "lang11d_tab.cpp"
    break;

  case 216: /* argdecls: ARG vardeflist ';'  */
#line 1373 "lang11d"
                                {
					yyval = (intptr_t)newPyrArgListNode((PyrVarDefNode*)yyvsp[-1], NULL, NULL);
				}
#line 4132 "lang11d_tab.cpp"
    break;

  case 217: /* argdecls: ARG vardeflist0 ELLIPSIS name ';'  */
#line 1377 "lang11d"
                                {
					yyval = (intptr_t)newPyrArgListNode((PyrVarDefNode*)yyvsp[-3], (PyrSlotNode*)yyvsp[-1], NULL);
				}
#line 4140 "lang11d_tab.cpp"
    break;

  case 218: /* argdecls: '|' slotdeflist '|'  */
#line 1381 "lang11d"
                                {
					yyval = (intptr_t)newPyrArgListNode((PyrVarDefNode*)yyvsp[-1], NULL, NULL);
				}
#line 4148 "lang11d_tab.cpp"
    break;

  case 219: /* argdecls: '|' slotdeflist0 ELLIPSIS name '|'  */
#line 1385 "lang11d"
                                {
					yyval = (intptr_t)newPyrArgListNode((PyrVarDefNode*)yyvsp[-3], (PyrSlotNode*)yyvsp[-1], NULL);
				}
#line 4156 "lang11d_tab.cpp"
    break;

  case 220: /* argdecls: '|' slotdeflist0 ELLIPSIS name ',' name '|'  */
#line 1389 "lang11d"
                            {
					yyval = (intptr_t)newPyrArgListNode((PyrVarDefNode*)yyvsp[-5], (PyrSlotNode*)yyvsp[-3], (PyrSlotNode*)yyvsp[-1]);
			    }
#line 4164 "lang11d_tab.cpp"
    break;

  case 221: /* argdecls1: ARG vardeflist ';'  */
#line 1395 "lang11d"
                                {
					yyval = (intptr_t)newPyrArgListNode((PyrVarDefNode*)yyvsp[-1], NULL, NULL);
				}
#line 4172 "lang11d_tab.cpp"
    break;

  case 222: /* argdecls1: ARG vardeflist0 ELLIPSIS name ';'  */
#line 1399 "lang11d"
                                {
					yyval = (intptr_t)newPyrArgListNode((PyrVarDefNode*)yyvsp[-3], (PyrSlotNode*)yyvsp[-1], NULL);
				}
#line 4180 "lang11d_tab.cpp"
    break;

  case 223: /* argdecls1: '|' slotdeflist '|'  */
#line 1403 "lang11d"
                                {
					yyval = (intptr_t)newPyrArgListNode((PyrVarDefNode*)yyvsp[-1], NULL, NULL);
				}
#line 4188 "lang11d_tab.cpp"
    break;

  case 224: /* argdecls1: '|' slotdeflist0 ELLIPSIS name '|'  */
#line 1407 "lang11d"
                                {
					yyval = (intptr_t)newPyrArgListNode((PyrVarDefNode*)yyvsp[-3], (PyrSlotNode*)yyvsp[-1], NULL);
				}
#line 4196 "lang11d_tab.cpp"
    break;

  case 225: /* argdecls1: '|' slotdeflist0 ELLIPSIS name ',' name '|'  */
#line 1411 "lang11d"
                            {
					yyval = (intptr_t)newPyrArgListNode((PyrVarDefNode*)yyvsp[-5], (PyrSlotNode*)yyvsp[-3], (PyrSlotNode*)yyvsp[-1]);
			    }
#line 4204 "lang11d_tab.cpp"
    break;

  case 227: /* constdeflist: constdeflist optcomma constdef  */
#line 1419 "lang11d"
                                        { yyval = (intptr_t)linkNextNode((PyrParseNode*)yyvsp[-2], (PyrParseNode*)yyvsp[0]); }
#line 4210 "lang11d_tab.cpp"
    break;

  case 228: /* constdef: rspec name '=' slotliteral  */
#line 1423 "lang11d"
                                { yyval = (intptr_t)newPyrVarDefNode((PyrSlotNode*)yyvsp[-2], (PyrParseNode*)yyvsp[0], yyvsp[-3]); }
#line 4216 "lang11d_tab.cpp"
    break;

  case 229: /* slotdeflist0: %empty  */
#line 1426 "lang11d"
                  { yyval = 0; }
#line 4222 "lang11d_tab.cpp"
    break;

  case 232: /* slotdeflist: slotdeflist optcomma slotdef  */
#line 1432 "lang11d"
                                { yyval = (intptr_t)linkNextNode((PyrParseNode*)yyvsp[-2], (PyrParseNode*)yyvsp[0]); }
#line 4228 "lang11d_tab.cpp"
    break;

  case 233: /* slotdef: name  */
#line 1436 "lang11d"
                                { yyval = (intptr_t)newPyrVarDefNode((PyrSlotNode*)yyvsp[0], NULL, 0); }
#line 4234 "lang11d_tab.cpp"
    break;

  case 234: /* slotdef: name optequal slotliteral  */
#line 1438 "lang11d"
                                { yyval = (intptr_t)newPyrVarDefNode((PyrSlotNode*)yyvsp[-2], (PyrParseNode*)yyvsp[0], 0); }
#line 4240 "lang11d_tab.cpp"
    break;

  case 235: /* slotdef: name optequal '(' exprseq ')'  */
#line 1440 "lang11d"
                                {
					PyrParseNode* node = (PyrParseNode*)yyvsp[-1];
					node->mParens = 1;
					yyval = (intptr_t)newPyrVarDefNode((PyrSlotNode*)yyvsp[-4], node, 0);
				}
#line 4250 "lang11d_tab.cpp"
    break;

  case 236: /* vardeflist0: %empty  */
#line 1447 "lang11d"
                  { yyval = 0; }
#line 4256 "lang11d_tab.cpp"
    break;

  case 239: /* vardeflist: vardeflist ',' vardef  */
#line 1453 "lang11d"
                                { yyval = (intptr_t)linkNextNode((PyrParseNode*)yyvsp[-2], (PyrParseNode*)yyvsp[0]); }
#line 4262 "lang11d_tab.cpp"
    break;

  case 240: /* vardef: name  */
#line 1457 "lang11d"
                                { yyval = (intptr_t)newPyrVarDefNode((PyrSlotNode*)yyvsp[0], NULL, 0); }
#line 4268 "lang11d_tab.cpp"
    break;

  case 241: /* vardef: name '=' expr  */
#line 1459 "lang11d"
                                { yyval = (intptr_t)newPyrVarDefNode((PyrSlotNode*)yyvsp[-2], (PyrParseNode*)yyvsp[0], 0); }
#line 4274 "lang11d_tab.cpp"
    break;

  case 242: /* vardef: name '(' exprseq ')'  */
#line 1461 "lang11d"
                                {
									PyrParseNode* node = (PyrParseNode*)yyvsp[-1];
									node->mParens = 1;
									yyval = (intptr_t)newPyrVarDefNode((PyrSlotNode*)yyvsp[-3], node, 0);
								}
#line 4284 "lang11d_tab.cpp"
    break;

  case 243: /* dictslotdef: exprseq ':' exprseq  */
#line 1469 "lang11d"
                                { yyval = (intptr_t)linkNextNode((PyrParseNode*)yyvsp[-2], (PyrParseNode*)yyvsp[0]); }
#line 4290 "lang11d_tab.cpp"
    break;

  case 244: /* dictslotdef: keybinop exprseq  */
#line 1471 "lang11d"
                                {
					PyrParseNode* key = newPyrPushLitNode((PyrSlotNode*)yyvsp[-1], NULL);
					yyval = (intptr_t)linkNextNode(key, (PyrParseNode*)yyvsp[0]);
				}
#line 4299 "lang11d_tab.cpp"
    break;

  case 246: /* dictslotlist1: dictslotlist1 ',' dictslotdef  */
#line 1479 "lang11d"
                                        { yyval = (intptr_t)linkNextNode((PyrParseNode*)yyvsp[-2], (PyrParseNode*)yyvsp[0]); }
#line 4305 "lang11d_tab.cpp"
    break;

  case 247: /* dictslotlist: %empty  */
#line 1482 "lang11d"
                  { yyval = 0; }
#line 4311 "lang11d_tab.cpp"
    break;

  case 250: /* rwslotdeflist: rwslotdeflist ',' rwslotdef  */
#line 1488 "lang11d"
                                        { yyval = (intptr_t)linkNextNode((PyrParseNode*)yyvsp[-2], (PyrParseNode*)yyvsp[0]); }
#line 4317 "lang11d_tab.cpp"
    break;

  case 251: /* rwslotdef: rwspec name  */
#line 1492 "lang11d"
                                        { yyval = (intptr_t)newPyrVarDefNode((PyrSlotNode*)yyvsp[0], NULL, yyvsp[-1]); }
#line 4323 "lang11d_tab.cpp"
    break;

  case 252: /* rwslotdef: rwspec name '=' slotliteral  */
#line 1494 "lang11d"
                                        { yyval = (intptr_t)newPyrVarDefNode((PyrSlotNode*)yyvsp[-2], (PyrParseNode*)yyvsp[0], yyvsp[-3]); }
#line 4329 "lang11d_tab.cpp"
    break;

  case 253: /* dictlit2: '(' litdictslotlist ')'  */
#line 1498 "lang11d"
                                { yyval = (intptr_t)newPyrLitDictNode((PyrParseNode*)yyvsp[-1]); }
#line 4335 "lang11d_tab.cpp"
    break;

  case 254: /* litdictslotdef: listliteral ':' listliteral  */
#line 1502 "lang11d"
                                { yyval = (intptr_t)linkNextNode((PyrParseNode*)yyvsp[-2], (PyrParseNode*)yyvsp[0]); }
#line 4341 "lang11d_tab.cpp"
    break;

  case 255: /* litdictslotdef: keybinop listliteral  */
#line 1504 "lang11d"
                                {
					PyrParseNode* key = newPyrPushLitNode((PyrSlotNode*)yyvsp[-1], NULL);
					yyval = (intptr_t)linkNextNode(key, (PyrParseNode*)yyvsp[0]);
				}
#line 4350 "lang11d_tab.cpp"
    break;

  case 257: /* litdictslotlist1: litdictslotlist1 ',' litdictslotdef  */
#line 1512 "lang11d"
                                        { yyval = (intptr_t)linkNextNode((PyrParseNode*)yyvsp[-2], (PyrParseNode*)yyvsp[0]); }
#line 4356 "lang11d_tab.cpp"
    break;

  case 258: /* litdictslotlist: %empty  */
#line 1515 "lang11d"
                  { yyval = 0; }
#line 4362 "lang11d_tab.cpp"
    break;

  case 260: /* listlit: '#' '[' literallistc ']'  */
#line 1522 "lang11d"
                                { yyval = (intptr_t)newPyrLitListNode(0, (PyrParseNode*)yyvsp[-1]); }
#line 4368 "lang11d_tab.cpp"
    break;

  case 261: /* listlit: '#' classname '[' literallistc ']'  */
#line 1524 "lang11d"
                                { yyval = (intptr_t)newPyrLitListNode((PyrParseNode*)yyvsp[-3], (PyrParseNode*)yyvsp[-1]); }
#line 4374 "lang11d_tab.cpp"
    break;

  case 262: /* listlit2: '[' literallistc ']'  */
#line 1528 "lang11d"
                                { yyval = (intptr_t)newPyrLitListNode(0, (PyrParseNode*)yyvsp[-1]); }
#line 4380 "lang11d_tab.cpp"
    break;

  case 263: /* listlit2: classname '[' literallistc ']'  */
#line 1530 "lang11d"
                                { yyval = (intptr_t)newPyrLitListNode((PyrParseNode*)yyvsp[-3], (PyrParseNode*)yyvsp[-1]); }
#line 4386 "lang11d_tab.cpp"
    break;

  case 264: /* literallistc: %empty  */
#line 1533 "lang11d"
                  { yyval = 0; }
#line 4392 "lang11d_tab.cpp"
    break;

  case 267: /* literallist1: literallist1 ',' listliteral  */
#line 1539 "lang11d"
                                        { yyval = (intptr_t)linkNextNode((PyrParseNode*)yyvsp[-2], (PyrParseNode*)yyvsp[0]); }
#line 4398 "lang11d_tab.cpp"
    break;

  case 268: /* rwspec: %empty  */
#line 1542 "lang11d"
           { yyval = rwPrivate; }
#line 4404 "lang11d_tab.cpp"
    break;

  case 269: /* rwspec: '<'  */
#line 1544 "lang11d"
                        { yyval = rwReadOnly; }
#line 4410 "lang11d_tab.cpp"
    break;

  case 270: /* rwspec: READWRITEVAR  */
#line 1546 "lang11d"
                        { yyval = rwReadWrite; }
#line 4416 "lang11d_tab.cpp"
    break;

  case 271: /* rwspec: '>'  */
#line 1548 "lang11d"
                        { yyval = rwWriteOnly; }
#line 4422 "lang11d_tab.cpp"
    break;

  case 272: /* rspec: %empty  */
#line 1551 "lang11d"
           { yyval = rwPrivate; }
#line 4428 "lang11d_tab.cpp"
    break;

  case 273: /* rspec: '<'  */
#line 1553 "lang11d"
                        { yyval = rwReadOnly; }
#line 4434 "lang11d_tab.cpp"
    break;

  case 274: /* integer: INTEGER  */
#line 1556 "lang11d"
                  { yyval = zzval; }
#line 4440 "lang11d_tab.cpp"
    break;

  case 275: /* integer: '-' INTEGER  */
#line 1558 "lang11d"
                        {
				PyrSlotNode *node;
				node = (PyrSlotNode*)zzval;
				SetRaw(&node->mSlot, -slotRawInt(&node->mSlot));
				yyval = zzval;
			}
#line 4451 "lang11d_tab.cpp"
    break;

  case 276: /* floatr: SC_FLOAT  */
#line 1566 "lang11d"
                   { yyval = zzval; }
#line 4457 "lang11d_tab.cpp"
    break;

  case 277: /* floatr: '-' SC_FLOAT  */
#line 1568 "lang11d"
                        {
				PyrSlotNode *node;
				node = (PyrSlotNode*)zzval;
				SetRaw(&node->mSlot, -slotRawFloat(&node->mSlot));
				yyval = zzval;
			}
#line 4468 "lang11d_tab.cpp"
    break;

  case 278: /* accidental: ACCIDENTAL  */
#line 1576 "lang11d"
                        { yyval = zzval; }
#line 4474 "lang11d_tab.cpp"
    break;

  case 279: /* accidental: '-' ACCIDENTAL  */
#line 1578 "lang11d"
                                {
					PyrSlotNode *node;
					double intval, fracval;
					node = (PyrSlotNode*)zzval;
					intval = floor(slotRawFloat(&node->mSlot) + 0.5);
					fracval = slotRawFloat(&node->mSlot) - intval;
					SetRaw(&node->mSlot, -intval + fracval);
					yyval = zzval;
				}
#line 4488 "lang11d_tab.cpp"
    break;

  case 280: /* pie: PIE  */
#line 1588 "lang11d"
                      { yyval = zzval; }
#line 4494 "lang11d_tab.cpp"
    break;

  case 283: /* floatp: floatr pie  */
#line 1594 "lang11d"
                        {
				PyrSlotNode *node;
				node = (PyrSlotNode*)yyvsp[-1];
				SetRaw(&node->mSlot, slotRawFloat(&node->mSlot) * pi);
			}
#line 4504 "lang11d_tab.cpp"
    break;

  case 284: /* floatp: integer pie  */
#line 1600 "lang11d"
                        {
				PyrSlotNode *node;
				double ival;
				node = (PyrSlotNode*)yyvsp[-1];
				ival = slotRawInt(&node->mSlot);
				SetFloat(&node->mSlot, ival * pi);
			}
#line 4516 "lang11d_tab.cpp"
    break;

  case 285: /* floatp: pie  */
#line 1608 "lang11d"
                        {
				PyrSlotNode *node;
				node = (PyrSlotNode*)zzval;
				SetFloat(&node->mSlot, pi);
				yyval = zzval;
			}
#line 4527 "lang11d_tab.cpp"
    break;

  case 286: /* floatp: '-' pie  */
#line 1615 "lang11d"
                        {
				PyrSlotNode *node;
				node = (PyrSlotNode*)zzval;
				SetFloat(&node->mSlot, -pi);
				yyval = zzval;
			}
#line 4538 "lang11d_tab.cpp"
    break;

  case 287: /* name: NAME  */
#line 1623 "lang11d"
                       { yyval = zzval; }
#line 4544 "lang11d_tab.cpp"
    break;

  case 288: /* name: WHILE  */
#line 1624 "lang11d"
                                { yyval = zzval; }
#line 4550 "lang11d_tab.cpp"
    break;

  case 289: /* classname: CLASSNAME  */
#line 1627 "lang11d"
                                    { yyval = zzval; }
#line 4556 "lang11d_tab.cpp"
    break;

  case 290: /* primname: PRIMITIVENAME  */
#line 1630 "lang11d"
                                        { yyval = zzval; }
#line 4562 "lang11d_tab.cpp"
    break;

  case 291: /* trueobj: TRUEOBJ  */
#line 1633 "lang11d"
                          { yyval = zzval; }
#line 4568 "lang11d_tab.cpp"
    break;

  case 292: /* falseobj: FALSEOBJ  */
#line 1636 "lang11d"
                           { yyval = zzval; }
#line 4574 "lang11d_tab.cpp"
    break;

  case 293: /* nilobj: NILOBJ  */
#line 1639 "lang11d"
                         { yyval = zzval; }
#line 4580 "lang11d_tab.cpp"
    break;

  case 294: /* ascii: ASCII  */
#line 1642 "lang11d"
                        { yyval = zzval; }
#line 4586 "lang11d_tab.cpp"
    break;

  case 295: /* symbol: SYMBOL  */
#line 1645 "lang11d"
                         { yyval = zzval; }
#line 4592 "lang11d_tab.cpp"
    break;

  case 296: /* string: STRING  */
#line 1648 "lang11d"
                         { yyval = zzval; }
#line 4598 "lang11d_tab.cpp"
    break;

  case 297: /* pseudovar: PSEUDOVAR  */
#line 1651 "lang11d"
                            { yyval = zzval; }
#line 4604 "lang11d_tab.cpp"
    break;

  case 298: /* binop: BINOP  */
#line 1654 "lang11d"
                { yyval = zzval; }
#line 4610 "lang11d_tab.cpp"
    break;

  case 299: /* binop: READWRITEVAR  */
#line 1655 "lang11d"
                               { yyval = zzval; }
#line 4616 "lang11d_tab.cpp"
    break;

  case 300: /* binop: '<'  */
#line 1656 "lang11d"
                       { yyval = zzval; }
#line 4622 "lang11d_tab.cpp"
    break;

  case 301: /* binop: '>'  */
#line 1657 "lang11d"
                       { yyval = zzval; }
#line 4628 "lang11d_tab.cpp"
    break;

  case 302: /* binop: '-'  */
#line 1658 "lang11d"
                       { yyval = zzval; }
#line 4634 "lang11d_tab.cpp"
    break;

  case 303: /* binop: '*'  */
#line 1659 "lang11d"
                       { yyval = zzval; }
#line 4640 "lang11d_tab.cpp"
    break;

  case 304: /* binop: '+'  */
#line 1660 "lang11d"
                       { yyval = zzval; }
#line 4646 "lang11d_tab.cpp"
    break;

  case 305: /* binop: '|'  */
#line 1661 "lang11d"
                       { yyval = zzval; }
#line 4652 "lang11d_tab.cpp"
    break;

  case 306: /* binop: KWARGEXPAND  */
#line 1662 "lang11d"
                              { yyval = zzval; }
#line 4658 "lang11d_tab.cpp"
    break;

  case 307: /* keybinop: KEYBINOP  */
#line 1665 "lang11d"
                    { yyval = zzval; }
#line 4664 "lang11d_tab.cpp"
    break;

  case 310: /* curryarg: CURRYARG  */
#line 1672 "lang11d"
                    { yyval = zzval; }
#line 4670 "lang11d_tab.cpp"
    break;


#line 4674 "lang11d_tab.cpp"

      default: break;
    }
  /* User semantic actions sometimes alter yychar, and that requires
     that yytoken be updated with the new translation.  We take the
     approach of translating immediately before every use of yytoken.
     One alternative is translating here after every semantic action,
     but that translation would be missed if the semantic action invokes
     YYABORT, YYACCEPT, or YYERROR immediately after altering yychar or
     if it invokes YYBACKUP.  In the case of YYABORT or YYACCEPT, an
     incorrect destructor might then be invoked immediately.  In the
     case of YYERROR or YYBACKUP, subsequent parser actions might lead
     to an incorrect destructor call or verbose syntax error message
     before the lookahead is translated.  */
  YY_SYMBOL_PRINT ("-> $$ =", YY_CAST (yysymbol_kind_t, yyr1[yyn]), &yyval, &yyloc);

  YYPOPSTACK (yylen);
  yylen = 0;

  *++yyvsp = yyval;

  /* Now 'shift' the result of the reduction.  Determine what state
     that goes to, based on the state we popped back to and the rule
     number reduced by.  */
  {
    const int yylhs = yyr1[yyn] - YYNTOKENS;
    const int yyi = yypgoto[yylhs] + *yyssp;
    yystate = (0 <= yyi && yyi <= YYLAST && yycheck[yyi] == *yyssp
               ? yytable[yyi]
               : yydefgoto[yylhs]);
  }

  goto yynewstate;


/*--------------------------------------.
| yyerrlab -- here on detecting error.  |
`--------------------------------------*/
yyerrlab:
  /* Make sure we have latest lookahead translation.  See comments at
     user semantic actions for why this is necessary.  */
  yytoken = yychar == YYEMPTY ? YYSYMBOL_YYEMPTY : YYTRANSLATE (yychar);
  /* If not already recovering from an error, report this error.  */
  if (!yyerrstatus)
    {
      ++yynerrs;
      {
        yypcontext_t yyctx
          = {yyssp, yytoken};
        char const *yymsgp = YY_("syntax error");
        int yysyntax_error_status;
        yysyntax_error_status = yysyntax_error (&yymsg_alloc, &yymsg, &yyctx);
        if (yysyntax_error_status == 0)
          yymsgp = yymsg;
        else if (yysyntax_error_status == -1)
          {
            if (yymsg != yymsgbuf)
              YYSTACK_FREE (yymsg);
            yymsg = YY_CAST (char *,
                             YYSTACK_ALLOC (YY_CAST (YYSIZE_T, yymsg_alloc)));
            if (yymsg)
              {
                yysyntax_error_status
                  = yysyntax_error (&yymsg_alloc, &yymsg, &yyctx);
                yymsgp = yymsg;
              }
            else
              {
                yymsg = yymsgbuf;
                yymsg_alloc = sizeof yymsgbuf;
                yysyntax_error_status = YYENOMEM;
              }
          }
        yyerror (yymsgp);
        if (yysyntax_error_status == YYENOMEM)
          YYNOMEM;
      }
    }

  if (yyerrstatus == 3)
    {
      /* If just tried and failed to reuse lookahead token after an
         error, discard it.  */

      if (yychar <= YYEOF)
        {
          /* Return failure if at end of input.  */
          if (yychar == YYEOF)
            YYABORT;
        }
      else
        {
          yydestruct ("Error: discarding",
                      yytoken, &yylval);
          yychar = YYEMPTY;
        }
    }

  /* Else will try to reuse lookahead token after shifting the error
     token.  */
  goto yyerrlab1;


/*---------------------------------------------------.
| yyerrorlab -- error raised explicitly by YYERROR.  |
`---------------------------------------------------*/
yyerrorlab:
  /* Pacify compilers when the user code never invokes YYERROR and the
     label yyerrorlab therefore never appears in user code.  */
  if (0)
    YYERROR;
  ++yynerrs;

  /* Do not reclaim the symbols of the rule whose action triggered
     this YYERROR.  */
  YYPOPSTACK (yylen);
  yylen = 0;
  YY_STACK_PRINT (yyss, yyssp);
  yystate = *yyssp;
  goto yyerrlab1;


/*-------------------------------------------------------------.
| yyerrlab1 -- common code for both syntax error and YYERROR.  |
`-------------------------------------------------------------*/
yyerrlab1:
  yyerrstatus = 3;      /* Each real token shifted decrements this.  */

  /* Pop stack until we find a state that shifts the error token.  */
  for (;;)
    {
      yyn = yypact[yystate];
      if (!yypact_value_is_default (yyn))
        {
          yyn += YYSYMBOL_YYerror;
          if (0 <= yyn && yyn <= YYLAST && yycheck[yyn] == YYSYMBOL_YYerror)
            {
              yyn = yytable[yyn];
              if (0 < yyn)
                break;
            }
        }

      /* Pop the current state because it cannot handle the error token.  */
      if (yyssp == yyss)
        YYABORT;


      yydestruct ("Error: popping",
                  YY_ACCESSING_SYMBOL (yystate), yyvsp);
      YYPOPSTACK (1);
      yystate = *yyssp;
      YY_STACK_PRINT (yyss, yyssp);
    }

  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  *++yyvsp = yylval;
  YY_IGNORE_MAYBE_UNINITIALIZED_END


  /* Shift the error token.  */
  YY_SYMBOL_PRINT ("Shifting", YY_ACCESSING_SYMBOL (yyn), yyvsp, yylsp);

  yystate = yyn;
  goto yynewstate;


/*-------------------------------------.
| yyacceptlab -- YYACCEPT comes here.  |
`-------------------------------------*/
yyacceptlab:
  yyresult = 0;
  goto yyreturnlab;


/*-----------------------------------.
| yyabortlab -- YYABORT comes here.  |
`-----------------------------------*/
yyabortlab:
  yyresult = 1;
  goto yyreturnlab;


/*-----------------------------------------------------------.
| yyexhaustedlab -- YYNOMEM (memory exhaustion) comes here.  |
`-----------------------------------------------------------*/
yyexhaustedlab:
  yyerror (YY_("memory exhausted"));
  yyresult = 2;
  goto yyreturnlab;


/*----------------------------------------------------------.
| yyreturnlab -- parsing is finished, clean up and return.  |
`----------------------------------------------------------*/
yyreturnlab:
  if (yychar != YYEMPTY)
    {
      /* Make sure we have latest lookahead translation.  See comments at
         user semantic actions for why this is necessary.  */
      yytoken = YYTRANSLATE (yychar);
      yydestruct ("Cleanup: discarding lookahead",
                  yytoken, &yylval);
    }
  /* Do not reclaim the symbols of the rule whose action triggered
     this YYABORT or YYACCEPT.  */
  YYPOPSTACK (yylen);
  YY_STACK_PRINT (yyss, yyssp);
  while (yyssp != yyss)
    {
      yydestruct ("Cleanup: popping",
                  YY_ACCESSING_SYMBOL (+*yyssp), yyvsp);
      YYPOPSTACK (1);
    }
#ifndef yyoverflow
  if (yyss != yyssa)
    YYSTACK_FREE (yyss);
#endif
  if (yymsg != yymsgbuf)
    YYSTACK_FREE (yymsg);
  return yyresult;
}

