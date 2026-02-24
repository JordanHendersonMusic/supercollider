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
#line 85 "lang11d"


// Preamble for the generated file.

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
  YYSYMBOL_CLASSNAME = 10,                 /* CLASSNAME  */
  YYSYMBOL_PIE = 11,                       /* PIE  */
  YYSYMBOL_WHILE = 12,                     /* WHILE  */
  YYSYMBOL_PRIMITIVENAME = 13,             /* PRIMITIVENAME  */
  YYSYMBOL_LEFTARROW = 14,                 /* LEFTARROW  */
  YYSYMBOL_TRUEOBJ = 15,                   /* TRUEOBJ  */
  YYSYMBOL_FALSEOBJ = 16,                  /* FALSEOBJ  */
  YYSYMBOL_NILOBJ = 17,                    /* NILOBJ  */
  YYSYMBOL_PSEUDOVAR = 18,                 /* PSEUDOVAR  */
  YYSYMBOL_VAR = 19,                       /* VAR  */
  YYSYMBOL_ARG = 20,                       /* ARG  */
  YYSYMBOL_CLASSVAR = 21,                  /* CLASSVAR  */
  YYSYMBOL_SC_CONST = 22,                  /* SC_CONST  */
  YYSYMBOL_ELLIPSIS = 23,                  /* ELLIPSIS  */
  YYSYMBOL_DOTDOT = 24,                    /* DOTDOT  */
  YYSYMBOL_BEGINCLOSEDFUNC = 25,           /* BEGINCLOSEDFUNC  */
  YYSYMBOL_BADTOKEN = 26,                  /* BADTOKEN  */
  YYSYMBOL_INTERPRET = 27,                 /* INTERPRET  */
  YYSYMBOL_BEGINGENERATOR = 28,            /* BEGINGENERATOR  */
  YYSYMBOL_CURRYARG = 29,                  /* CURRYARG  */
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
  YYSYMBOL_44_ = 44,                       /* '{'  */
  YYSYMBOL_45_ = 45,                       /* '}'  */
  YYSYMBOL_46_ = 46,                       /* '['  */
  YYSYMBOL_47_ = 47,                       /* ']'  */
  YYSYMBOL_48_ = 48,                       /* ';'  */
  YYSYMBOL_49_ = 49,                       /* ','  */
  YYSYMBOL_50_ = 50,                       /* '('  */
  YYSYMBOL_51_ = 51,                       /* ')'  */
  YYSYMBOL_52_ = 52,                       /* '^'  */
  YYSYMBOL_53_ = 53,                       /* '~'  */
  YYSYMBOL_54_ = 54,                       /* '#'  */
  YYSYMBOL_YYACCEPT = 55,                  /* $accept  */
  YYSYMBOL_root = 56,                      /* root  */
  YYSYMBOL_classes = 57,                   /* classes  */
  YYSYMBOL_classextensions = 58,           /* classextensions  */
  YYSYMBOL_classdef = 59,                  /* classdef  */
  YYSYMBOL_classextension = 60,            /* classextension  */
  YYSYMBOL_optName = 61,                   /* optName  */
  YYSYMBOL_optSuperName = 62,              /* optSuperName  */
  YYSYMBOL_classvardecls = 63,             /* classvardecls  */
  YYSYMBOL_classvardecl = 64,              /* classvardecl  */
  YYSYMBOL_methods = 65,                   /* methods  */
  YYSYMBOL_methoddef = 66,                 /* methoddef  */
  YYSYMBOL_optsemi = 67,                   /* optsemi  */
  YYSYMBOL_optcomma = 68,                  /* optcomma  */
  YYSYMBOL_optequal = 69,                  /* optequal  */
  YYSYMBOL_funcbody = 70,                  /* funcbody  */
  YYSYMBOL_cmdlinecode = 71,               /* cmdlinecode  */
  YYSYMBOL_methbody = 72,                  /* methbody  */
  YYSYMBOL_optPrim = 73,                   /* optPrim  */
  YYSYMBOL_retval = 74,                    /* retval  */
  YYSYMBOL_funretval = 75,                 /* funretval  */
  YYSYMBOL_blocklist1 = 76,                /* blocklist1  */
  YYSYMBOL_blocklistitem = 77,             /* blocklistitem  */
  YYSYMBOL_blocklist = 78,                 /* blocklist  */
  YYSYMBOL_msgsend = 79,                   /* msgsend  */
  YYSYMBOL_generator = 80,                 /* generator  */
  YYSYMBOL_81_1 = 81,                      /* $@1  */
  YYSYMBOL_82_2 = 82,                      /* $@2  */
  YYSYMBOL_nextqual = 83,                  /* nextqual  */
  YYSYMBOL_qual = 84,                      /* qual  */
  YYSYMBOL_expr1 = 85,                     /* expr1  */
  YYSYMBOL_valrangex1 = 86,                /* valrangex1  */
  YYSYMBOL_valrangeassign = 87,            /* valrangeassign  */
  YYSYMBOL_valrangexd = 88,                /* valrangexd  */
  YYSYMBOL_valrange2 = 89,                 /* valrange2  */
  YYSYMBOL_valrange3 = 90,                 /* valrange3  */
  YYSYMBOL_expr = 91,                      /* expr  */
  YYSYMBOL_adverb = 92,                    /* adverb  */
  YYSYMBOL_exprn = 93,                     /* exprn  */
  YYSYMBOL_exprseq = 94,                   /* exprseq  */
  YYSYMBOL_arrayelems = 95,                /* arrayelems  */
  YYSYMBOL_arrayelems1 = 96,               /* arrayelems1  */
  YYSYMBOL_arglist1 = 97,                  /* arglist1  */
  YYSYMBOL_arglistv1 = 98,                 /* arglistv1  */
  YYSYMBOL_keyarglist1 = 99,               /* keyarglist1  */
  YYSYMBOL_keyarg = 100,                   /* keyarg  */
  YYSYMBOL_optkeyarglist = 101,            /* optkeyarglist  */
  YYSYMBOL_mavars = 102,                   /* mavars  */
  YYSYMBOL_nameList = 103,                 /* nameList  */
  YYSYMBOL_slotliteral = 104,              /* slotliteral  */
  YYSYMBOL_blockliteral = 105,             /* blockliteral  */
  YYSYMBOL_pushname = 106,                 /* pushname  */
  YYSYMBOL_pushliteral = 107,              /* pushliteral  */
  YYSYMBOL_listliteral = 108,              /* listliteral  */
  YYSYMBOL_block = 109,                    /* block  */
  YYSYMBOL_funcvardecls = 110,             /* funcvardecls  */
  YYSYMBOL_funcvardecls1 = 111,            /* funcvardecls1  */
  YYSYMBOL_funcvardecl = 112,              /* funcvardecl  */
  YYSYMBOL_argdecls = 113,                 /* argdecls  */
  YYSYMBOL_argdecls1 = 114,                /* argdecls1  */
  YYSYMBOL_constdeflist = 115,             /* constdeflist  */
  YYSYMBOL_constdef = 116,                 /* constdef  */
  YYSYMBOL_slotdeflist0 = 117,             /* slotdeflist0  */
  YYSYMBOL_slotdeflist = 118,              /* slotdeflist  */
  YYSYMBOL_slotdef = 119,                  /* slotdef  */
  YYSYMBOL_vardeflist0 = 120,              /* vardeflist0  */
  YYSYMBOL_vardeflist = 121,               /* vardeflist  */
  YYSYMBOL_vardef = 122,                   /* vardef  */
  YYSYMBOL_dictslotdef = 123,              /* dictslotdef  */
  YYSYMBOL_dictslotlist1 = 124,            /* dictslotlist1  */
  YYSYMBOL_dictslotlist = 125,             /* dictslotlist  */
  YYSYMBOL_rwslotdeflist = 126,            /* rwslotdeflist  */
  YYSYMBOL_rwslotdef = 127,                /* rwslotdef  */
  YYSYMBOL_dictlit2 = 128,                 /* dictlit2  */
  YYSYMBOL_litdictslotdef = 129,           /* litdictslotdef  */
  YYSYMBOL_litdictslotlist1 = 130,         /* litdictslotlist1  */
  YYSYMBOL_litdictslotlist = 131,          /* litdictslotlist  */
  YYSYMBOL_listlit = 132,                  /* listlit  */
  YYSYMBOL_listlit2 = 133,                 /* listlit2  */
  YYSYMBOL_literallistc = 134,             /* literallistc  */
  YYSYMBOL_literallist1 = 135,             /* literallist1  */
  YYSYMBOL_rwspec = 136,                   /* rwspec  */
  YYSYMBOL_rspec = 137,                    /* rspec  */
  YYSYMBOL_integer = 138,                  /* integer  */
  YYSYMBOL_floatr = 139,                   /* floatr  */
  YYSYMBOL_accidental = 140,               /* accidental  */
  YYSYMBOL_floatp = 141,                   /* floatp  */
  YYSYMBOL_name = 142,                     /* name  */
  YYSYMBOL_binop = 143,                    /* binop  */
  YYSYMBOL_binop2 = 144,                   /* binop2  */
  YYSYMBOL_curryArg = 145                  /* curryArg  */
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
         || (defined YYLTYPE_IS_TRIVIAL && YYLTYPE_IS_TRIVIAL \
             && defined YYSTYPE_IS_TRIVIAL && YYSTYPE_IS_TRIVIAL)))

/* A type that is properly aligned for any stack member.  */
union yyalloc
{
  yy_state_t yyss_alloc;
  YYSTYPE yyvs_alloc;
  YYLTYPE yyls_alloc;
};

/* The size of the maximum gap between one aligned stack and the next.  */
# define YYSTACK_GAP_MAXIMUM (YYSIZEOF (union yyalloc) - 1)

/* The size of an array large to enough to hold all stacks, each with
   N elements.  */
# define YYSTACK_BYTES(N) \
     ((N) * (YYSIZEOF (yy_state_t) + YYSIZEOF (YYSTYPE) \
             + YYSIZEOF (YYLTYPE)) \
      + 2 * YYSTACK_GAP_MAXIMUM)

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
#define YYFINAL  60
/* YYLAST -- Last index in YYTABLE.  */
#define YYLAST   1854

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  55
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  91
/* YYNRULES -- Number of rules.  */
#define YYNRULES  292
/* YYNSTATES -- Number of states.  */
#define YYNSTATES  559

/* YYMAXUTOK -- Last valid token kind.  */
#define YYMAXUTOK   288


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
       2,     2,     2,     2,     2,    54,     2,     2,     2,     2,
      50,    51,    37,    38,    49,    34,    41,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,    30,    48,
      35,    31,    36,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,    46,     2,    47,    52,     2,    42,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,    44,    39,    45,    53,     2,     2,     2,
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
      25,    26,    27,    28,    29,    32,    33,    40,    43
};

#if YYDEBUG
/* YYRLINE[YYN] -- Source line where rule number YYN was defined.  */
static const yytype_int16 yyrline[] =
{
       0,   177,   177,   182,   187,   193,   194,   197,   198,   201,
     203,   206,   209,   209,   212,   212,   214,   215,   218,   220,
     222,   225,   226,   229,   231,   233,   235,   238,   238,   239,
     239,   240,   240,   242,   243,   246,   248,   250,   252,   254,
     256,   258,   261,   262,   265,   266,   268,   269,   272,   274,
     277,   278,   281,   281,   283,   283,   285,   288,   291,   294,
     297,   300,   303,   316,   322,   325,   331,   337,   343,   352,
     370,   375,   380,   382,   387,   403,   405,   407,   420,   429,
     429,   436,   436,   439,   453,   455,   488,   525,   536,   544,
     546,   563,   564,   565,   566,   567,   568,   569,   576,   582,
     584,   586,   588,   590,   596,   598,   614,   624,   642,   659,
     670,   693,   715,   725,   742,   759,   768,   786,   794,   804,
     810,   815,   823,   833,   844,   854,   864,   870,   871,   872,
     873,   874,   879,   888,   894,   896,   902,   904,   913,   915,
     920,   926,   927,   928,   929,   931,   932,   935,   937,   938,
     940,   941,   943,   945,   947,   949,   952,   953,   956,   958,
     961,   962,   965,   968,   969,   971,   973,   976,   977,   980,
     981,   982,   983,   984,   985,   986,   987,   988,   990,   992,
     994,   995,   996,   997,   998,   999,  1000,  1001,  1002,  1004,
    1005,  1006,  1007,  1008,  1009,  1010,  1011,  1012,  1013,  1014,
    1016,  1018,  1022,  1023,  1026,  1027,  1030,  1033,  1034,  1036,
    1038,  1040,  1042,  1044,  1047,  1049,  1051,  1053,  1055,  1057,
    1061,  1062,  1065,  1068,  1069,  1071,  1072,  1075,  1077,  1079,
    1086,  1087,  1089,  1090,  1093,  1095,  1097,  1104,  1106,  1110,
    1111,  1114,  1115,  1117,  1118,  1121,  1123,  1126,  1129,  1131,
    1134,  1135,  1138,  1139,  1143,  1145,  1148,  1150,  1153,  1154,
    1156,  1157,  1160,  1161,  1162,  1163,  1165,  1166,  1168,  1169,
    1176,  1177,  1184,  1185,  1195,  1196,  1197,  1203,  1209,  1210,
    1217,  1217,  1218,  1218,  1218,  1218,  1218,  1218,  1218,  1218,
    1219,  1219,  1220
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
  "SC_FLOAT", "ACCIDENTAL", "SYMBOL", "STRING", "ASCII", "CLASSNAME",
  "PIE", "WHILE", "PRIMITIVENAME", "LEFTARROW", "TRUEOBJ", "FALSEOBJ",
  "NILOBJ", "PSEUDOVAR", "VAR", "ARG", "CLASSVAR", "SC_CONST", "ELLIPSIS",
  "DOTDOT", "BEGINCLOSEDFUNC", "BADTOKEN", "INTERPRET", "BEGINGENERATOR",
  "CURRYARG", "':'", "'='", "BINOP", "KEYBINOP", "'-'", "'<'", "'>'",
  "'*'", "'+'", "'|'", "READWRITEVAR", "'.'", "'`'", "UMINUS", "'{'",
  "'}'", "'['", "']'", "';'", "','", "'('", "')'", "'^'", "'~'", "'#'",
  "$accept", "root", "classes", "classextensions", "classdef",
  "classextension", "optName", "optSuperName", "classvardecls",
  "classvardecl", "methods", "methoddef", "optsemi", "optcomma",
  "optequal", "funcbody", "cmdlinecode", "methbody", "optPrim", "retval",
  "funretval", "blocklist1", "blocklistitem", "blocklist", "msgsend",
  "generator", "$@1", "$@2", "nextqual", "qual", "expr1", "valrangex1",
  "valrangeassign", "valrangexd", "valrange2", "valrange3", "expr",
  "adverb", "exprn", "exprseq", "arrayelems", "arrayelems1", "arglist1",
  "arglistv1", "keyarglist1", "keyarg", "optkeyarglist", "mavars",
  "nameList", "slotliteral", "blockliteral", "pushname", "pushliteral",
  "listliteral", "block", "funcvardecls", "funcvardecls1", "funcvardecl",
  "argdecls", "argdecls1", "constdeflist", "constdef", "slotdeflist0",
  "slotdeflist", "slotdef", "vardeflist0", "vardeflist", "vardef",
  "dictslotdef", "dictslotlist1", "dictslotlist", "rwslotdeflist",
  "rwslotdef", "dictlit2", "litdictslotdef", "litdictslotlist1",
  "litdictslotlist", "listlit", "listlit2", "literallistc", "literallist1",
  "rwspec", "rspec", "integer", "floatr", "accidental", "floatp", "name",
  "binop", "binop2", "curryArg", YY_NULLPTR
};

static const char *
yysymbol_name (yysymbol_kind_t yysymbol)
{
  return yytname[yysymbol];
}
#endif

#define YYPACT_NINF (-437)

#define yypact_value_is_default(Yyn) \
  ((Yyn) == YYPACT_NINF)

#define YYTABLE_NINF (-290)

#define yytable_value_is_error(Yyn) \
  0

/* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
   STATE-NUM.  */
static const yytype_int16 yypact[] =
{
     113,   729,    14,    31,    28,    16,  -437,  -437,  -437,  -437,
    -437,  -437,  -437,  -437,   157,  -437,  -437,  -437,  -437,  -437,
     196,   196,   127,  -437,   243,   196,  1665,    30,  1197,   625,
    1665,   196,   107,  -437,  -437,  -437,  -437,  -437,    88,  -437,
    -437,  -437,  1813,    11,    97,  -437,  -437,  -437,  -437,   937,
    -437,   937,  -437,   160,   193,  -437,  -437,   274,  -437,   162,
    -437,     0,  -437,  -437,  1197,   781,    76,  -437,  -437,  -437,
      93,  -437,    96,   195,   204,   196,   196,  -437,  -437,  -437,
    -437,  -437,   199,    38,  -437,   484,   677,  -437,  1665,  1665,
    -437,  1665,   210,   179,   206,  1665,  1249,  -437,  1665,   243,
    -437,  -437,  -437,  -437,    15,  -437,   207,    44,   937,   937,
    -437,   221,   231,  -437,   252,  1780,   213,   265,  1732,   258,
      13,  -437,  1301,  -437,  -437,  -437,    25,   257,  1665,  -437,
    -437,  -437,  -437,  -437,   937,  -437,  -437,  1665,   989,    76,
    -437,   305,   196,   278,   276,  1665,  1665,    76,  -437,   281,
     283,   287,  -437,  -437,  -437,   196,  1665,  1665,   196,  -437,
     302,   219,   318,    60,   937,   196,  -437,  -437,   196,  -437,
      36,  -437,  -437,   937,  -437,  1665,  -437,  1353,  -437,  -437,
    1665,   299,    56,  -437,  -437,  1665,  1665,  1665,  -437,   309,
     317,   937,  1405,  -437,  -437,   167,  -437,  -437,  1665,  1732,
    -437,  -437,  -437,   307,  -437,  -437,  -437,  1732,  1717,  -437,
    -437,  -437,   298,   327,   160,  -437,  -437,  1665,   196,   196,
    1665,   185,  1457,   833,   338,    22,  1665,  1813,  -437,  1813,
      76,   281,   283,  1749,  -437,   334,  -437,  -437,  -437,  -437,
    -437,    76,  -437,  1041,  -437,   333,   323,   335,   323,   336,
    -437,  1813,   339,   244,   196,  -437,   196,  -437,   344,  -437,
     109,  -437,  -437,  -437,  -437,  -437,  -437,  -437,  1665,     3,
    -437,  -437,   160,  -437,   346,   347,   354,  -437,  1665,   371,
    -437,  -437,  1665,  1665,  -437,  -437,   379,  -437,  -437,   356,
    1665,   378,  -437,  1093,    76,  1813,   363,  1732,   365,  1732,
     386,  -437,   372,   374,  -437,  1732,  -437,  1813,  -437,  -437,
     375,  1509,   395,  1665,  1665,   190,    76,   281,   283,   287,
    1665,   885,  -437,   424,  1665,  -437,  -437,   388,    76,   380,
     382,   125,  -437,  -437,   390,   396,   409,   316,  1665,  -437,
     287,    76,  -437,  -437,    76,  -437,  -437,   196,   247,   147,
    -437,  -437,   196,   393,  1145,  1145,  -437,  -437,  1665,  -437,
     417,  1665,  -437,    76,   281,   283,  -437,   399,  -437,  -437,
    1732,  1717,  -437,  -437,  -437,   416,   421,   407,  1665,   413,
    1561,   430,  -437,   411,   414,   419,  1813,    76,   281,   283,
     287,   425,    81,  -437,   423,   435,   127,   127,   437,   293,
     293,   447,  -437,  1761,  -437,  -437,  -437,  -437,   438,  -437,
     196,  -437,   196,   444,  -437,   196,   124,   440,   445,   477,
     448,  -437,  1665,  -437,    76,   449,   452,  -437,  -437,  -437,
    1665,  1665,   467,  1813,   468,   478,   465,  1665,    76,  -437,
      76,  -437,   471,   474,   481,  -437,  1665,  -437,   127,   127,
    -437,  -437,  -437,  -437,  -437,  -437,   291,  -437,   196,   294,
    -437,   310,  -437,   196,  -437,  -437,   480,   497,  -437,   488,
    1665,  1665,  -437,  1145,  -437,  1665,   523,  -437,  -437,    76,
    -437,  1813,  1813,  1665,  1665,  1665,   507,  1813,  -437,  -437,
      76,  -437,    76,  1813,  -437,  -437,   238,   238,   316,  -437,
     293,   508,  -437,  -437,   447,   510,  -437,  -437,  1665,   445,
     445,  -437,   445,  1665,  -437,  1813,  1813,  1813,  1665,  -437,
    -437,   238,   238,   495,  1613,  1613,  1799,  -437,   546,  -437,
     546,   445,  -437,  -437,  -437,   445,  1813,  1613,  1613,  -437,
    1665,   499,  -437,   493,   501,  -437,  -437,  -437,  -437,  -437,
     502,   503,  1780,  -437,  -437,  -437,  -437,  -437,  -437
};

/* YYDEFACT[STATE-NUM] -- Default reduction number in state STATE-NUM.
   Performed when YYTABLE does not specify something else to do.  Zero
   means the default is an error.  */
static const yytype_int16 yydefact[] =
{
       5,    48,     0,     0,     2,     3,     7,   280,   268,   270,
     272,   184,   183,   182,   130,   278,   281,   185,   186,   187,
       0,   230,   207,   292,     0,   223,     0,   207,   148,   241,
       0,     0,     0,    41,     4,    33,    96,    93,   127,   104,
     129,   128,   145,    27,    48,    92,    94,    91,   178,    48,
     204,    48,   188,   180,   274,   275,   181,   179,    95,     0,
       1,    14,     6,     8,   148,     0,    65,    50,    53,    52,
       0,   232,   234,     0,   231,   230,   223,   202,   269,   271,
     273,   279,     0,    29,   225,    31,   241,   132,     0,     0,
     202,     0,   150,     0,    29,     0,     0,   282,   291,   286,
     284,   285,   287,   288,   223,   283,     0,     0,    48,    48,
     239,    29,     0,   290,     0,    27,    98,     0,   258,     0,
     165,   167,     0,   291,   286,   289,     0,   141,    28,   147,
      34,    40,   205,    39,    48,   277,   276,     0,     0,    56,
      21,     0,    12,     0,     0,     0,     0,    54,   156,    29,
      29,    29,   160,    51,   206,     0,     0,     0,     0,   214,
       0,   231,     0,    29,    48,     0,   217,    30,     0,    32,
       0,    79,    81,    48,   152,     0,    99,    30,   149,   118,
       0,     0,     0,   238,   100,   117,     0,     0,    97,     0,
       0,    48,    30,   242,   102,     0,    28,    49,     0,   258,
     193,   192,   191,     0,   195,   196,   197,   258,   252,   260,
     199,   198,     0,    29,   189,   190,   194,     0,     0,     0,
       0,     0,     0,     0,    54,     0,     0,   146,    38,   134,
       0,    29,    29,     0,    15,     0,    13,    16,    64,   162,
     158,    55,    66,    30,   163,     0,    30,     0,    30,     0,
     233,   235,     0,     0,     0,   208,     0,   211,     0,   203,
       0,   226,   173,   172,   171,   174,   175,   176,     0,     0,
     228,   177,   169,   170,     0,     0,     0,   151,     0,   153,
     122,   101,   123,     0,   119,   237,     0,    37,    36,     0,
       0,     0,   240,     0,    57,   135,     0,   258,     0,     0,
       0,   250,    29,     0,   254,    30,   259,   138,   166,   168,
       0,     0,   103,     0,     0,     0,    54,    29,    29,    29,
       0,     0,    78,     0,     0,   143,   142,   133,    58,     0,
       0,   287,    11,    22,     0,     0,    14,    21,     0,   157,
      29,    54,    69,   161,    54,   236,   215,     0,     0,     0,
     201,   218,     0,     0,     0,     0,   200,   154,     0,   124,
       0,   121,    35,     0,    29,    29,   255,     0,   256,   249,
       0,    30,   253,   247,   261,   106,   105,     0,     0,     0,
       0,   131,    70,     0,     0,     0,   136,    54,    29,    29,
      29,     0,    54,    62,     0,     0,   207,   207,     0,   262,
     262,   266,    17,     0,   159,   164,    68,    67,     0,   209,
       0,   212,     0,     0,   229,     0,     0,     0,    83,   179,
       0,   155,   125,   120,    60,     0,     0,   257,   248,   251,
       0,     0,   107,   139,   112,   111,     0,     0,    54,    74,
      54,    75,     0,     0,     0,   144,     0,    59,   207,   207,
     202,   202,    16,   263,   265,   264,     0,   243,     0,     0,
     267,    29,   220,     0,     9,   216,     0,     0,   219,     0,
       0,     0,    80,     0,    88,     0,     0,    82,   126,    54,
      63,   109,   108,     0,     0,     0,   113,   140,    73,    71,
      54,    77,    54,   137,   202,   202,    44,    44,    21,    19,
     262,   245,    18,    20,   266,     0,   210,   213,     0,    83,
      83,    84,    83,     0,    61,   110,   115,   114,     0,    76,
      72,    44,    44,    27,    46,    46,     0,   244,     0,   221,
       0,    83,    90,    89,    85,    83,   116,    46,    46,    45,
       0,     0,    42,    46,     0,    10,   246,   222,    87,    86,
       0,     0,    27,    23,    43,    25,    24,    26,    47
};

/* YYPGOTO[NTERM-NUM].  */
static const yytype_int16 yypgoto[] =
{
    -437,  -437,  -437,  -437,  -437,   544,  -437,   220,   106,  -437,
    -329,  -437,  -114,   -78,  -437,   426,  -437,  -350,  -237,    23,
     524,    -2,   -31,     5,  -437,    50,  -437,  -437,  -289,  -349,
    -437,  -437,  -437,  -437,  -437,  -437,   -26,  -437,  -437,    -7,
     505,  -437,  -119,  -109,   -56,   319,   -87,  -437,  -437,  -436,
      86,  -437,  -437,  -176,  -437,   -76,   -12,    47,   -25,   541,
    -437,    67,   496,   506,   405,   500,   -10,   422,   387,  -437,
    -437,   181,    83,  -437,   214,  -437,  -437,  -163,  -437,  -184,
    -437,  -437,  -437,    65,  -437,  -437,    80,   101,  -213,   -19,
    -437
};

/* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int16 yydefgoto[] =
{
       0,     3,     4,     5,    62,     6,   235,   143,   337,   402,
     233,   333,   129,   244,   170,    33,    34,   541,   524,   542,
      35,   241,    67,   242,    36,    37,   274,   275,   474,   417,
      38,    39,    40,    41,   106,   181,    42,   226,    43,    44,
      93,    94,   149,   150,   340,   152,   245,   119,   120,   270,
      45,    46,    47,   209,    48,   164,    49,   259,    77,    51,
     461,   462,    82,    83,    84,    73,    70,    71,   110,   111,
     112,   456,   457,   210,   301,   302,   303,    52,   211,   212,
     213,   458,   463,    53,    54,    55,    56,    57,   113,   127,
      58
};

/* YYTABLE[YYPACT[STATE-NUM]] -- What to do in state STATE-NUM.  If
   positive, shift that token.  If negative, reduce the rule whose
   number is the opposite.  If YYTABLE_NINF, syntax error.  */
static const yytype_int16 yytable[] =
{
      87,   197,    90,   221,   115,   168,   420,   271,   403,   151,
     114,    74,    66,   117,   173,   296,   178,   108,     7,   231,
     335,    92,   107,   298,    59,     7,     8,    16,     7,   232,
     141,    60,   300,   193,    16,   153,   218,    16,    61,   134,
       8,     9,    10,   262,   263,   264,   142,    15,    50,   118,
      75,   265,   266,   267,     2,   139,   323,    92,   148,   128,
      88,  -224,   219,   247,    68,   161,  -289,   114,   185,    76,
      24,   222,   324,   249,   186,   223,    50,   166,    89,   107,
     282,   171,   172,  -224,   174,   168,   268,   167,   179,   182,
     269,   183,   546,   187,   547,   188,   132,   191,    50,   257,
      69,    22,   227,   315,   317,   283,    22,    68,   153,   167,
       7,   229,   446,   367,   318,   148,    68,   117,   395,    16,
      27,    72,    72,   369,   511,    27,    85,   156,     7,   374,
     251,   148,   116,   121,   122,   306,   470,    16,   239,   240,
       1,   154,   155,    69,   329,   330,   157,    75,   351,    30,
     252,     2,    69,   118,   471,   132,    50,    97,   352,   124,
     100,   101,   102,   103,   125,   105,    76,   319,   277,   526,
     279,   135,   295,   280,   364,   544,    72,    85,   284,   285,
     286,   132,    22,   214,   365,   291,   411,   550,   551,    68,
     335,   307,    22,   294,   428,   300,   412,    68,   215,     7,
     327,    27,   388,    64,   136,    85,   140,    65,    16,   311,
     153,    27,   389,   310,   380,   148,   148,   293,   158,   216,
     532,   533,   165,   534,   372,    69,   176,   224,   328,   322,
     383,   384,   312,    69,   313,   272,   339,   381,   132,   313,
     175,   385,   548,   236,   198,    68,   549,    78,    79,    80,
     273,   523,   159,   155,    81,   177,    72,    20,   184,   253,
     525,   353,   405,   153,   214,   390,   260,   255,   155,    85,
     192,   357,   214,   214,    68,   359,   360,   425,   426,   215,
      68,    69,   194,   183,   537,   538,   148,   215,   215,   217,
     325,    68,   346,   347,   386,   409,   410,   153,   225,    22,
     216,   442,   443,   195,   377,   137,   339,   379,   216,   216,
      69,   199,   444,   335,   148,   234,    69,   391,    27,   308,
     309,   382,   237,   238,   138,   254,   326,    69,   453,   454,
     243,   404,   246,   455,   334,   399,   248,   400,   401,   499,
     500,   256,   502,   500,    68,   304,   406,   418,   418,   407,
     281,   421,   433,   297,   423,   348,   145,   349,   503,   167,
     287,   424,   214,    22,   214,   271,    68,   271,   288,   320,
     214,   450,   451,   436,   496,   497,   305,   215,    68,   215,
      69,   336,    27,   504,   341,   215,   342,   344,   321,   350,
     345,    68,   441,   153,    68,   354,   355,   447,   216,   356,
     216,   358,    69,   361,   481,   482,   216,   362,   186,   539,
     366,   487,   368,    68,    69,   478,   370,   139,   521,   522,
     493,   371,   375,   494,   495,   373,   378,    69,    78,   126,
      69,   392,   394,   393,   396,   214,   214,    68,   558,   141,
     397,   422,    68,   488,   414,   489,   427,   430,   408,    69,
     215,   215,   431,   413,   432,   419,   419,   515,   516,   517,
     434,   437,   438,   509,   510,   439,   418,   448,   512,    68,
     440,   216,   216,    69,    68,   131,   445,   133,    69,   449,
       7,   452,   460,   468,   514,   472,   465,  -227,    68,    16,
      68,   475,   536,   477,   473,   519,  -227,   520,   483,   484,
     479,   531,    22,   480,   334,    69,   535,  -227,   137,   485,
      69,   466,   486,   467,   552,   169,   469,   543,   543,   508,
     476,    27,   490,  -227,    69,   491,    69,   138,   506,    68,
     543,   543,   492,  -227,   189,   190,   507,   513,   518,   528,
      68,   530,    68,   196,   553,   540,   555,   556,   557,    63,
       8,     9,    10,   262,   263,   264,   398,    15,   498,   501,
     228,   265,   266,   267,   505,    69,   554,   343,   130,   144,
     109,   529,   162,   261,   419,   160,    69,   250,    69,   292,
      24,   459,   163,   527,     0,   429,     0,     0,     0,     0,
     258,     0,     0,   272,     0,   272,     0,     0,     0,   276,
     269,     0,     0,     0,     0,     0,     0,     0,   273,     0,
     273,     0,     0,     0,     0,     0,     0,   289,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   334,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,     0,     0,
      17,    18,    19,     0,    20,    21,     0,     0,     0,    95,
      22,     0,     0,     0,    23,    96,     0,    97,    98,    99,
     100,   101,   102,   103,   104,   105,     0,    26,     0,    27,
       0,    28,     0,     0,     0,    86,     0,     0,    31,    32,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
       0,     0,    17,    18,    19,     0,     0,     0,     0,     0,
       0,    95,    22,     0,     0,     0,    23,    96,     0,    97,
      98,    99,   100,   101,   102,   103,   125,   105,     0,    26,
       0,    27,     0,    28,     0,     0,     0,    86,     0,     0,
      31,    32,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,     0,     0,    17,    18,    19,     0,    20,    21,
       0,     0,     0,     0,    22,     0,     0,     0,    23,     0,
       0,     0,     0,    24,     0,     0,     0,     0,    25,     0,
       0,    26,     0,    27,     0,    28,     0,     0,     0,    29,
       0,    30,    31,    32,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,     0,     0,    17,    18,    19,     0,
       0,     0,     0,     0,     0,     0,    22,     0,     0,     0,
      23,     0,     0,     0,   145,    24,     0,     0,   146,     0,
       0,     0,     0,    26,     0,    27,     0,    28,     0,     0,
       0,    86,   147,     0,    31,    32,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,     0,     0,    17,    18,
      19,     0,     0,     0,     0,     0,     0,     0,    22,     0,
       0,     0,    23,     0,     0,     0,   145,    24,     0,     0,
     146,     0,     0,     0,     0,    26,     0,    27,     0,    28,
       0,     0,     0,    86,   316,     0,    31,    32,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,     0,     0,
      17,    18,    19,     0,     0,     0,     0,     0,     0,     0,
      22,     0,     0,     0,    23,     0,     0,     0,   145,    24,
       0,     0,   146,     0,     0,     0,     0,    26,     0,    27,
       0,    28,     0,     0,     0,    86,   387,     0,    31,    32,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
       0,     0,    17,    18,    19,     0,    20,     0,     0,     0,
       0,     0,    22,     0,     0,     0,    23,     0,     0,     0,
       0,    24,     0,     0,     0,     0,     0,     0,     0,    26,
       0,    27,     0,    28,     0,     0,     0,    86,     0,    30,
      31,    32,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,     0,     0,    17,    18,    19,     0,     0,     0,
       0,     0,     0,     0,    22,     0,     0,     0,    23,     0,
       0,     0,     0,    24,     0,     0,   146,     0,     0,     0,
       0,    26,     0,    27,     0,    28,     0,     0,     0,    86,
     230,     0,    31,    32,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,     0,     0,    17,    18,    19,     0,
       0,     0,     0,     0,     0,     0,    22,     0,     0,     0,
      23,     0,     0,     0,   145,    24,     0,     0,   338,     0,
       0,     0,     0,    26,     0,    27,     0,    28,     0,     0,
       0,    86,     0,     0,    31,    32,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,     0,     0,    17,    18,
      19,     0,     0,     0,     0,     0,     0,     0,    22,     0,
       0,     0,    23,     0,     0,     0,     0,    24,     0,     0,
     146,     0,     0,     0,     0,    26,     0,    27,     0,    28,
       0,     0,     0,    86,   363,     0,    31,    32,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,     0,     0,
      17,    18,    19,     0,   415,     0,     0,     0,     0,     0,
      22,     0,     0,     0,    23,   416,     0,     0,     0,    24,
       0,     0,     0,     0,     0,     0,     0,    26,     0,    27,
       0,    28,     0,     0,     0,    86,     0,     0,    31,    32,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
       0,     0,    17,    18,    19,     0,     0,     0,     0,     0,
       0,     0,    22,     0,     0,     0,    23,     0,     0,     0,
      91,    24,     0,     0,     0,     0,     0,     0,     0,    26,
       0,    27,     0,    28,     0,     0,     0,    86,     0,     0,
      31,    32,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,     0,     0,    17,    18,    19,     0,     0,     0,
       0,     0,     0,   180,    22,     0,     0,     0,    23,     0,
       0,     0,     0,    24,     0,     0,     0,     0,     0,     0,
       0,    26,     0,    27,     0,    28,     0,     0,     0,    86,
       0,     0,    31,    32,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,     0,     0,    17,    18,    19,     0,
       0,     0,     0,     0,     0,   220,    22,     0,     0,     0,
      23,     0,     0,     0,     0,    24,     0,     0,     0,     0,
       0,     0,     0,    26,     0,    27,     0,    28,     0,     0,
       0,    86,     0,     0,    31,    32,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,     0,     0,    17,    18,
      19,     0,     0,     0,     0,     0,     0,     0,    22,     0,
       0,     0,    23,     0,     0,     0,   278,    24,     0,     0,
       0,     0,     0,     0,     0,    26,     0,    27,     0,    28,
       0,     0,     0,    86,     0,     0,    31,    32,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,     0,     0,
      17,    18,    19,     0,     0,     0,     0,     0,     0,     0,
      22,     0,     0,     0,    23,     0,     0,     0,   290,    24,
       0,     0,     0,     0,     0,     0,     0,    26,     0,    27,
       0,    28,     0,     0,     0,    86,     0,     0,    31,    32,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
       0,     0,    17,    18,    19,     0,     0,     0,     0,     0,
       0,   314,    22,     0,     0,     0,    23,     0,     0,     0,
       0,    24,     0,     0,     0,     0,     0,     0,     0,    26,
       0,    27,     0,    28,     0,     0,     0,    86,     0,     0,
      31,    32,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,     0,     0,    17,    18,    19,     0,     0,     0,
       0,     0,     0,     0,    22,     0,     0,     0,    23,     0,
       0,     0,     0,    24,     0,     0,     0,     0,     0,     0,
       0,    26,     0,    27,     0,    28,   376,     0,     0,    86,
       0,     0,    31,    32,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,     0,     0,    17,    18,    19,     0,
       0,     0,     0,     0,     0,     0,    22,     0,     0,     0,
      23,     0,     0,     0,     0,    24,     0,     0,     0,     0,
       0,     0,     0,    26,     0,    27,     0,    28,   435,     0,
       0,    86,     0,     0,    31,    32,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,     0,     0,    17,    18,
      19,     0,     0,     0,     0,     0,     0,     0,    22,     0,
       0,     0,    23,     0,     0,     0,     0,    24,     0,     0,
       0,     0,     0,     0,     0,    26,     0,    27,     0,    28,
       0,     0,     0,    86,     0,   540,    31,    32,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,     0,     0,
      17,    18,    19,     0,     0,     0,     0,     0,     0,     0,
      22,     0,     0,     0,    23,     0,     0,     0,     0,    24,
       0,     0,     0,     0,     0,     0,     0,    26,     0,    27,
       0,    28,     0,     0,     0,    86,     0,     0,    31,    32,
       7,     8,     9,    10,   200,   201,   202,   203,    15,    16,
       0,     0,   204,   205,   206,     7,     8,     9,    10,   200,
     201,   202,   203,    15,    16,     0,     0,   204,   205,   206,
     299,    24,     7,     0,     0,     0,     0,     0,     0,     0,
       0,    16,     0,   207,     7,     0,    24,   208,     0,     0,
       0,     0,     0,    16,     0,     0,     0,     0,   207,     0,
       0,    97,   208,   124,   100,   101,   331,   103,   125,   105,
       0,     0,     0,    97,   332,   124,   100,   101,   331,   103,
     125,   105,     7,     0,     0,     0,   464,     0,     0,     0,
       0,    16,    97,   123,   124,   100,   101,   102,   103,   125,
     105,   126,     0,     0,     0,     0,     0,     0,   196,     0,
       0,    97,     0,   124,   100,   101,   331,   103,   125,   105,
       0,     0,     0,     0,   545,    97,   123,   124,   100,   101,
     102,   103,   125,   105,   126
};

static const yytype_int16 yycheck[] =
{
      26,   115,    27,   122,    30,    83,   355,   170,   337,    65,
      29,    21,    14,    10,    90,   199,    94,    29,     3,   138,
     233,    28,    29,   207,    10,     3,     4,    12,     3,   138,
      30,     0,   208,   111,    12,    66,    23,    12,    10,    51,
       4,     5,     6,     7,     8,     9,    46,    11,     1,    46,
      20,    15,    16,    17,    38,    57,    34,    64,    65,    48,
      30,    23,    49,   150,    14,    75,    51,    86,    24,    39,
      34,    46,    50,   151,    30,    50,    29,    39,    48,    86,
      24,    88,    89,    23,    91,   163,    50,    49,    95,    96,
      54,    98,   528,    49,   530,    51,    49,   109,    51,    39,
      14,    25,   128,   222,   223,    49,    25,    57,   139,    49,
       3,   137,    31,   297,   223,   122,    66,    10,   331,    12,
      44,    20,    21,   299,   473,    44,    25,    31,     3,   305,
     156,   138,    31,    32,    46,   213,    12,    12,   145,   146,
      27,    48,    49,    57,   231,   232,    50,    20,    39,    52,
     157,    38,    66,    46,    30,   108,   109,    32,    49,    34,
      35,    36,    37,    38,    39,    40,    39,   223,   175,   498,
     177,    11,   198,   180,   293,   525,    75,    76,   185,   186,
     187,   134,    25,   118,   293,   192,    39,   537,   538,   139,
     403,   217,    25,   195,   370,   371,    49,   147,   118,     3,
     226,    44,   321,    46,    11,   104,    44,    50,    12,    24,
     241,    44,   321,   220,    24,   222,   223,    50,    23,   118,
     509,   510,    23,   512,   302,   139,    47,   126,   230,   224,
     317,   318,    47,   147,    49,   170,   243,    47,   191,    49,
      30,   319,   531,   142,    31,   195,   535,     4,     5,     6,
     170,    13,    48,    49,    11,    49,   155,    19,    51,   158,
     497,   268,   340,   294,   199,   321,   165,    48,    49,   168,
      49,   278,   207,   208,   224,   282,   283,   364,   365,   199,
     230,   195,    51,   290,   521,   522,   293,   207,   208,    31,
     225,   241,    48,    49,   320,    48,    49,   328,    41,    25,
     199,   388,   389,    51,   311,    31,   313,   314,   207,   208,
     224,    46,   390,   526,   321,    10,   230,   324,    44,   218,
     219,   316,    44,    47,    50,    23,   225,   241,    35,    36,
      49,   338,    49,    40,   233,    19,    49,    21,    22,    48,
      49,    23,    48,    49,   294,    47,   341,   354,   355,   344,
      51,   358,   378,    46,   361,   254,    33,   256,    48,    49,
      51,   363,   297,    25,   299,   528,   316,   530,    51,    31,
     305,   396,   397,   380,   450,   451,    49,   297,   328,   299,
     294,    47,    44,   461,    51,   305,    51,    51,    50,    45,
      51,   341,   387,   424,   344,    49,    49,   392,   297,    45,
     299,    30,   316,    24,   430,   431,   305,    51,    30,   523,
      47,   437,    47,   363,   328,   422,    30,   419,   494,   495,
     446,    49,    47,   448,   449,    51,    31,   341,     4,    41,
     344,    51,   331,    51,    44,   370,   371,   387,   552,    30,
      44,    24,   392,   438,    51,   440,    47,    31,   347,   363,
     370,   371,    31,   352,    47,   354,   355,   483,   484,   485,
      47,    31,    51,   470,   471,    51,   473,    44,   475,   419,
      51,   370,   371,   387,   424,    49,    51,    51,   392,    44,
       3,    44,    35,    39,   479,    45,    48,     3,   438,    12,
     440,    14,   518,    45,    49,   490,    12,   492,    31,    31,
      51,   508,    25,    51,   403,   419,   513,    23,    31,    31,
     424,   410,    47,   412,   540,    31,   415,   524,   525,    31,
     419,    44,    51,    39,   438,    51,   440,    50,    48,   479,
     537,   538,    51,    49,   108,   109,    39,    14,    31,    31,
     490,    31,   492,    48,    45,    52,    45,    45,    45,     5,
       4,     5,     6,     7,     8,     9,   336,    11,   452,   458,
     134,    15,    16,    17,   463,   479,   543,   248,    44,    64,
      29,   504,    76,   168,   473,    75,   490,   155,   492,   192,
      34,   400,    76,   500,    -1,   371,    -1,    -1,    -1,    -1,
     164,    -1,    -1,   528,    -1,   530,    -1,    -1,    -1,   173,
      54,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   528,    -1,
     530,    -1,    -1,    -1,    -1,    -1,    -1,   191,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   526,     3,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    -1,    -1,
      15,    16,    17,    -1,    19,    20,    -1,    -1,    -1,    24,
      25,    -1,    -1,    -1,    29,    30,    -1,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    -1,    42,    -1,    44,
      -1,    46,    -1,    -1,    -1,    50,    -1,    -1,    53,    54,
       3,     4,     5,     6,     7,     8,     9,    10,    11,    12,
      -1,    -1,    15,    16,    17,    -1,    -1,    -1,    -1,    -1,
      -1,    24,    25,    -1,    -1,    -1,    29,    30,    -1,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    -1,    42,
      -1,    44,    -1,    46,    -1,    -1,    -1,    50,    -1,    -1,
      53,    54,     3,     4,     5,     6,     7,     8,     9,    10,
      11,    12,    -1,    -1,    15,    16,    17,    -1,    19,    20,
      -1,    -1,    -1,    -1,    25,    -1,    -1,    -1,    29,    -1,
      -1,    -1,    -1,    34,    -1,    -1,    -1,    -1,    39,    -1,
      -1,    42,    -1,    44,    -1,    46,    -1,    -1,    -1,    50,
      -1,    52,    53,    54,     3,     4,     5,     6,     7,     8,
       9,    10,    11,    12,    -1,    -1,    15,    16,    17,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    25,    -1,    -1,    -1,
      29,    -1,    -1,    -1,    33,    34,    -1,    -1,    37,    -1,
      -1,    -1,    -1,    42,    -1,    44,    -1,    46,    -1,    -1,
      -1,    50,    51,    -1,    53,    54,     3,     4,     5,     6,
       7,     8,     9,    10,    11,    12,    -1,    -1,    15,    16,
      17,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    25,    -1,
      -1,    -1,    29,    -1,    -1,    -1,    33,    34,    -1,    -1,
      37,    -1,    -1,    -1,    -1,    42,    -1,    44,    -1,    46,
      -1,    -1,    -1,    50,    51,    -1,    53,    54,     3,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    -1,    -1,
      15,    16,    17,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      25,    -1,    -1,    -1,    29,    -1,    -1,    -1,    33,    34,
      -1,    -1,    37,    -1,    -1,    -1,    -1,    42,    -1,    44,
      -1,    46,    -1,    -1,    -1,    50,    51,    -1,    53,    54,
       3,     4,     5,     6,     7,     8,     9,    10,    11,    12,
      -1,    -1,    15,    16,    17,    -1,    19,    -1,    -1,    -1,
      -1,    -1,    25,    -1,    -1,    -1,    29,    -1,    -1,    -1,
      -1,    34,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    42,
      -1,    44,    -1,    46,    -1,    -1,    -1,    50,    -1,    52,
      53,    54,     3,     4,     5,     6,     7,     8,     9,    10,
      11,    12,    -1,    -1,    15,    16,    17,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    25,    -1,    -1,    -1,    29,    -1,
      -1,    -1,    -1,    34,    -1,    -1,    37,    -1,    -1,    -1,
      -1,    42,    -1,    44,    -1,    46,    -1,    -1,    -1,    50,
      51,    -1,    53,    54,     3,     4,     5,     6,     7,     8,
       9,    10,    11,    12,    -1,    -1,    15,    16,    17,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    25,    -1,    -1,    -1,
      29,    -1,    -1,    -1,    33,    34,    -1,    -1,    37,    -1,
      -1,    -1,    -1,    42,    -1,    44,    -1,    46,    -1,    -1,
      -1,    50,    -1,    -1,    53,    54,     3,     4,     5,     6,
       7,     8,     9,    10,    11,    12,    -1,    -1,    15,    16,
      17,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    25,    -1,
      -1,    -1,    29,    -1,    -1,    -1,    -1,    34,    -1,    -1,
      37,    -1,    -1,    -1,    -1,    42,    -1,    44,    -1,    46,
      -1,    -1,    -1,    50,    51,    -1,    53,    54,     3,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    -1,    -1,
      15,    16,    17,    -1,    19,    -1,    -1,    -1,    -1,    -1,
      25,    -1,    -1,    -1,    29,    30,    -1,    -1,    -1,    34,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    42,    -1,    44,
      -1,    46,    -1,    -1,    -1,    50,    -1,    -1,    53,    54,
       3,     4,     5,     6,     7,     8,     9,    10,    11,    12,
      -1,    -1,    15,    16,    17,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    25,    -1,    -1,    -1,    29,    -1,    -1,    -1,
      33,    34,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    42,
      -1,    44,    -1,    46,    -1,    -1,    -1,    50,    -1,    -1,
      53,    54,     3,     4,     5,     6,     7,     8,     9,    10,
      11,    12,    -1,    -1,    15,    16,    17,    -1,    -1,    -1,
      -1,    -1,    -1,    24,    25,    -1,    -1,    -1,    29,    -1,
      -1,    -1,    -1,    34,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    42,    -1,    44,    -1,    46,    -1,    -1,    -1,    50,
      -1,    -1,    53,    54,     3,     4,     5,     6,     7,     8,
       9,    10,    11,    12,    -1,    -1,    15,    16,    17,    -1,
      -1,    -1,    -1,    -1,    -1,    24,    25,    -1,    -1,    -1,
      29,    -1,    -1,    -1,    -1,    34,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    42,    -1,    44,    -1,    46,    -1,    -1,
      -1,    50,    -1,    -1,    53,    54,     3,     4,     5,     6,
       7,     8,     9,    10,    11,    12,    -1,    -1,    15,    16,
      17,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    25,    -1,
      -1,    -1,    29,    -1,    -1,    -1,    33,    34,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    42,    -1,    44,    -1,    46,
      -1,    -1,    -1,    50,    -1,    -1,    53,    54,     3,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    -1,    -1,
      15,    16,    17,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      25,    -1,    -1,    -1,    29,    -1,    -1,    -1,    33,    34,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    42,    -1,    44,
      -1,    46,    -1,    -1,    -1,    50,    -1,    -1,    53,    54,
       3,     4,     5,     6,     7,     8,     9,    10,    11,    12,
      -1,    -1,    15,    16,    17,    -1,    -1,    -1,    -1,    -1,
      -1,    24,    25,    -1,    -1,    -1,    29,    -1,    -1,    -1,
      -1,    34,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    42,
      -1,    44,    -1,    46,    -1,    -1,    -1,    50,    -1,    -1,
      53,    54,     3,     4,     5,     6,     7,     8,     9,    10,
      11,    12,    -1,    -1,    15,    16,    17,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    25,    -1,    -1,    -1,    29,    -1,
      -1,    -1,    -1,    34,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    42,    -1,    44,    -1,    46,    47,    -1,    -1,    50,
      -1,    -1,    53,    54,     3,     4,     5,     6,     7,     8,
       9,    10,    11,    12,    -1,    -1,    15,    16,    17,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    25,    -1,    -1,    -1,
      29,    -1,    -1,    -1,    -1,    34,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    42,    -1,    44,    -1,    46,    47,    -1,
      -1,    50,    -1,    -1,    53,    54,     3,     4,     5,     6,
       7,     8,     9,    10,    11,    12,    -1,    -1,    15,    16,
      17,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    25,    -1,
      -1,    -1,    29,    -1,    -1,    -1,    -1,    34,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    42,    -1,    44,    -1,    46,
      -1,    -1,    -1,    50,    -1,    52,    53,    54,     3,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    -1,    -1,
      15,    16,    17,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      25,    -1,    -1,    -1,    29,    -1,    -1,    -1,    -1,    34,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    42,    -1,    44,
      -1,    46,    -1,    -1,    -1,    50,    -1,    -1,    53,    54,
       3,     4,     5,     6,     7,     8,     9,    10,    11,    12,
      -1,    -1,    15,    16,    17,     3,     4,     5,     6,     7,
       8,     9,    10,    11,    12,    -1,    -1,    15,    16,    17,
      33,    34,     3,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    12,    -1,    46,     3,    -1,    34,    50,    -1,    -1,
      -1,    -1,    -1,    12,    -1,    -1,    -1,    -1,    46,    -1,
      -1,    32,    50,    34,    35,    36,    37,    38,    39,    40,
      -1,    -1,    -1,    32,    45,    34,    35,    36,    37,    38,
      39,    40,     3,    -1,    -1,    -1,    45,    -1,    -1,    -1,
      -1,    12,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    -1,    -1,    -1,    -1,    -1,    -1,    48,    -1,
      -1,    32,    -1,    34,    35,    36,    37,    38,    39,    40,
      -1,    -1,    -1,    -1,    45,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41
};

/* YYSTOS[STATE-NUM] -- The symbol kind of the accessing symbol of
   state STATE-NUM.  */
static const yytype_uint8 yystos[] =
{
       0,    27,    38,    56,    57,    58,    60,     3,     4,     5,
       6,     7,     8,     9,    10,    11,    12,    15,    16,    17,
      19,    20,    25,    29,    34,    39,    42,    44,    46,    50,
      52,    53,    54,    70,    71,    75,    79,    80,    85,    86,
      87,    88,    91,    93,    94,   105,   106,   107,   109,   111,
     112,   114,   132,   138,   139,   140,   141,   142,   145,    10,
       0,    10,    59,    60,    46,    50,    76,    77,    80,   105,
     121,   122,   142,   120,   121,    20,    39,   113,     4,     5,
       6,    11,   117,   118,   119,   142,    50,    91,    30,    48,
     113,    33,    94,    95,    96,    24,    30,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    89,    94,   111,   114,
     123,   124,   125,   143,   144,    91,   142,    10,    46,   102,
     103,   142,    46,    33,    34,    39,    41,   144,    48,    67,
      75,    70,   112,    70,   111,    11,    11,    31,    50,    76,
      44,    30,    46,    62,    95,    33,    37,    51,    94,    97,
      98,    99,   100,    77,    48,    49,    31,    50,    23,    48,
     120,   121,   117,   118,   110,    23,    39,    49,    68,    31,
      69,    94,    94,   110,    94,    30,    47,    49,    68,    94,
      24,    90,    94,    94,    51,    24,    30,    49,    51,    70,
      70,   111,    49,    68,    51,    51,    48,    67,    31,    46,
       7,     8,     9,    10,    15,    16,    17,    46,    50,   108,
     128,   133,   134,   135,   138,   141,   142,    31,    23,    49,
      24,    97,    46,    50,   142,    41,    92,    91,    70,    91,
      51,    97,    98,    65,    10,    61,   142,    44,    47,    94,
      94,    76,    78,    49,    68,   101,    49,   101,    49,    68,
     122,    91,    94,   142,    23,    48,    23,    39,    70,   112,
     142,   119,     7,     8,     9,    15,    16,    17,    50,    54,
     104,   132,   138,   141,    81,    82,    70,    94,    33,    94,
      94,    51,    24,    49,    94,    94,    94,    51,    51,    70,
      33,    94,   123,    50,    76,    91,   134,    46,   134,    33,
     108,   129,   130,   131,    47,    49,    68,    91,   142,   142,
      94,    24,    47,    49,    24,    97,    51,    97,    98,    99,
      31,    50,    78,    34,    50,   138,   142,    91,    76,   101,
     101,    37,    45,    66,   142,   143,    47,    63,    37,    94,
      99,    51,    51,   100,    51,    51,    48,    49,   142,   142,
      45,    39,    49,    94,    49,    49,    45,    94,    30,    94,
      94,    24,    51,    51,    97,    98,    47,   134,    47,   108,
      30,    49,    68,    51,   108,    47,    47,    94,    31,    94,
      24,    47,    78,   101,   101,    68,    91,    51,    97,    98,
      99,    94,    51,    51,   142,   143,    44,    44,    62,    19,
      21,    22,    64,    65,    94,    68,    78,    78,   142,    48,
      49,    39,    49,   142,    51,    19,    30,    84,    94,   142,
      84,    94,    24,    94,    76,   101,   101,    47,   108,   129,
      31,    31,    47,    91,    47,    47,    94,    31,    51,    51,
      51,    78,   101,   101,    68,    51,    31,    78,    44,    44,
     113,   113,    44,    35,    36,    40,   126,   127,   136,   126,
      35,   115,   116,   137,    45,    48,   142,   142,    39,   142,
      12,    30,    45,    49,    83,    14,   142,    45,    94,    51,
      51,    91,    91,    31,    31,    31,    47,    91,    78,    78,
      51,    51,    51,    91,   113,   113,   110,   110,    63,    48,
      49,   142,    48,    48,    68,   142,    48,    39,    31,    94,
      94,    84,    94,    14,    78,    91,    91,    91,    31,    78,
      78,   110,   110,    13,    73,    73,    65,   127,    31,   116,
      31,    94,    83,    83,    83,    94,    91,    73,    73,    67,
      52,    72,    74,    94,    72,    45,   104,   104,    83,    83,
      72,    72,    91,    45,    74,    45,    45,    45,    67
};

/* YYR1[RULE-NUM] -- Symbol kind of the left-hand side of rule RULE-NUM.  */
static const yytype_uint8 yyr1[] =
{
       0,    55,    56,    56,    56,    57,    57,    58,    58,    59,
      59,    60,    61,    61,    62,    62,    63,    63,    64,    64,
      64,    65,    65,    66,    66,    66,    66,    67,    67,    68,
      68,    69,    69,    70,    70,    71,    71,    71,    71,    71,
      71,    71,    72,    72,    73,    73,    74,    74,    75,    75,
      76,    76,    77,    77,    78,    78,    79,    79,    79,    79,
      79,    79,    79,    79,    79,    79,    79,    79,    79,    79,
      79,    79,    79,    79,    79,    79,    79,    79,    79,    81,
      80,    82,    80,    83,    83,    84,    84,    84,    84,    84,
      84,    85,    85,    85,    85,    85,    85,    85,    85,    85,
      85,    85,    85,    85,    85,    86,    86,    86,    87,    87,
      87,    88,    88,    88,    88,    88,    88,    89,    89,    89,
      89,    89,    90,    90,    90,    90,    90,    91,    91,    91,
      91,    91,    91,    91,    91,    91,    91,    91,    91,    91,
      91,    92,    92,    92,    92,    93,    93,    94,    95,    95,
      96,    96,    96,    96,    96,    96,    97,    97,    98,    98,
      99,    99,   100,   101,   101,   102,   102,   103,   103,   104,
     104,   104,   104,   104,   104,   104,   104,   104,   105,   106,
     107,   107,   107,   107,   107,   107,   107,   107,   107,   108,
     108,   108,   108,   108,   108,   108,   108,   108,   108,   108,
     109,   109,   110,   110,   111,   111,   112,   113,   113,   113,
     113,   113,   113,   113,   114,   114,   114,   114,   114,   114,
     115,   115,   116,   117,   117,   118,   118,   119,   119,   119,
     120,   120,   121,   121,   122,   122,   122,   123,   123,   124,
     124,   125,   125,   126,   126,   127,   127,   128,   129,   129,
     130,   130,   131,   131,   132,   132,   133,   133,   134,   134,
     135,   135,   136,   136,   136,   136,   137,   137,   138,   138,
     139,   139,   140,   140,   141,   141,   141,   141,   141,   141,
     142,   142,   143,   143,   143,   143,   143,   143,   143,   143,
     144,   144,   145
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
       5,     7,     8,     7,     6,     6,     8,     7,     4,     0,
       7,     0,     7,     0,     2,     4,     5,     5,     2,     4,
       4,     1,     1,     1,     1,     1,     1,     3,     2,     3,
       3,     4,     3,     4,     1,     5,     5,     6,     7,     7,
       8,     6,     6,     7,     8,     8,     9,     2,     2,     3,
       5,     4,     2,     2,     3,     4,     5,     1,     1,     1,
       1,     5,     2,     4,     3,     4,     5,     7,     4,     6,
       7,     0,     2,     2,     4,     1,     3,     2,     0,     2,
       1,     3,     2,     3,     4,     5,     1,     3,     2,     4,
       1,     3,     2,     1,     3,     1,     3,     1,     3,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       5,     5,     0,     2,     1,     2,     3,     0,     3,     5,
       7,     3,     5,     7,     3,     5,     7,     3,     5,     7,
       1,     3,     4,     0,     1,     1,     3,     1,     3,     5,
       0,     1,     1,     3,     1,     3,     4,     3,     2,     1,
       3,     0,     2,     1,     3,     2,     4,     3,     3,     2,
       1,     3,     0,     2,     4,     5,     3,     4,     0,     2,
       1,     3,     0,     1,     1,     1,     0,     1,     1,     2,
       1,     2,     1,     2,     1,     1,     2,     2,     1,     2,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1
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

/* YYLLOC_DEFAULT -- Set CURRENT to span from RHS[1] to RHS[N].
   If N is 0, then set CURRENT to the empty location which ends
   the previous symbol: RHS[0] (always defined).  */

#ifndef YYLLOC_DEFAULT
# define YYLLOC_DEFAULT(Current, Rhs, N)                                \
    do                                                                  \
      if (N)                                                            \
        {                                                               \
          (Current).first_line   = YYRHSLOC (Rhs, 1).first_line;        \
          (Current).first_column = YYRHSLOC (Rhs, 1).first_column;      \
          (Current).last_line    = YYRHSLOC (Rhs, N).last_line;         \
          (Current).last_column  = YYRHSLOC (Rhs, N).last_column;       \
        }                                                               \
      else                                                              \
        {                                                               \
          (Current).first_line   = (Current).last_line   =              \
            YYRHSLOC (Rhs, 0).last_line;                                \
          (Current).first_column = (Current).last_column =              \
            YYRHSLOC (Rhs, 0).last_column;                              \
        }                                                               \
    while (0)
#endif

#define YYRHSLOC(Rhs, K) ((Rhs)[K])


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


/* YYLOCATION_PRINT -- Print the location on the stream.
   This macro was not mandated originally: define only if we know
   we won't break user code: when these are the locations we know.  */

# ifndef YYLOCATION_PRINT

#  if defined YY_LOCATION_PRINT

   /* Temporary convenience wrapper in case some people defined the
      undocumented and private YY_LOCATION_PRINT macros.  */
#   define YYLOCATION_PRINT(File, Loc)  YY_LOCATION_PRINT(File, *(Loc))

#  elif defined YYLTYPE_IS_TRIVIAL && YYLTYPE_IS_TRIVIAL

/* Print *YYLOCP on YYO.  Private, do not rely on its existence. */

YY_ATTRIBUTE_UNUSED
static int
yy_location_print_ (FILE *yyo, YYLTYPE const * const yylocp)
{
  int res = 0;
  int end_col = 0 != yylocp->last_column ? yylocp->last_column - 1 : 0;
  if (0 <= yylocp->first_line)
    {
      res += YYFPRINTF (yyo, "%d", yylocp->first_line);
      if (0 <= yylocp->first_column)
        res += YYFPRINTF (yyo, ".%d", yylocp->first_column);
    }
  if (0 <= yylocp->last_line)
    {
      if (yylocp->first_line < yylocp->last_line)
        {
          res += YYFPRINTF (yyo, "-%d", yylocp->last_line);
          if (0 <= end_col)
            res += YYFPRINTF (yyo, ".%d", end_col);
        }
      else if (0 <= end_col && yylocp->first_column < end_col)
        res += YYFPRINTF (yyo, "-%d", end_col);
    }
  return res;
}

#   define YYLOCATION_PRINT  yy_location_print_

    /* Temporary convenience wrapper in case some people defined the
       undocumented and private YY_LOCATION_PRINT macros.  */
#   define YY_LOCATION_PRINT(File, Loc)  YYLOCATION_PRINT(File, &(Loc))

#  else

#   define YYLOCATION_PRINT(File, Loc) ((void) 0)
    /* Temporary convenience wrapper in case some people defined the
       undocumented and private YY_LOCATION_PRINT macros.  */
#   define YY_LOCATION_PRINT  YYLOCATION_PRINT

#  endif
# endif /* !defined YYLOCATION_PRINT */


# define YY_SYMBOL_PRINT(Title, Kind, Value, Location)                    \
do {                                                                      \
  if (yydebug)                                                            \
    {                                                                     \
      YYFPRINTF (stderr, "%s ", Title);                                   \
      yy_symbol_print (stderr,                                            \
                  Kind, Value, Location); \
      YYFPRINTF (stderr, "\n");                                           \
    }                                                                     \
} while (0)


/*-----------------------------------.
| Print this symbol's value on YYO.  |
`-----------------------------------*/

static void
yy_symbol_value_print (FILE *yyo,
                       yysymbol_kind_t yykind, YYSTYPE const * const yyvaluep, YYLTYPE const * const yylocationp)
{
  FILE *yyoutput = yyo;
  YY_USE (yyoutput);
  YY_USE (yylocationp);
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
                 yysymbol_kind_t yykind, YYSTYPE const * const yyvaluep, YYLTYPE const * const yylocationp)
{
  YYFPRINTF (yyo, "%s %s (",
             yykind < YYNTOKENS ? "token" : "nterm", yysymbol_name (yykind));

  YYLOCATION_PRINT (yyo, yylocationp);
  YYFPRINTF (yyo, ": ");
  yy_symbol_value_print (yyo, yykind, yyvaluep, yylocationp);
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
yy_reduce_print (yy_state_t *yyssp, YYSTYPE *yyvsp, YYLTYPE *yylsp,
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
                       &yyvsp[(yyi + 1) - (yynrhs)],
                       &(yylsp[(yyi + 1) - (yynrhs)]));
      YYFPRINTF (stderr, "\n");
    }
}

# define YY_REDUCE_PRINT(Rule)          \
do {                                    \
  if (yydebug)                          \
    yy_reduce_print (yyssp, yyvsp, yylsp, Rule); \
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
  YYLTYPE *yylloc;
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
            yysymbol_kind_t yykind, YYSTYPE *yyvaluep, YYLTYPE *yylocationp)
{
  YY_USE (yyvaluep);
  YY_USE (yylocationp);
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
/* Location data for the lookahead symbol.  */
YYLTYPE yylloc
# if defined YYLTYPE_IS_TRIVIAL && YYLTYPE_IS_TRIVIAL
  = { 1, 1, 1, 1 }
# endif
;
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

    /* The location stack: array, bottom, top.  */
    YYLTYPE yylsa[YYINITDEPTH];
    YYLTYPE *yyls = yylsa;
    YYLTYPE *yylsp = yyls;

  int yyn;
  /* The return value of yyparse.  */
  int yyresult;
  /* Lookahead symbol kind.  */
  yysymbol_kind_t yytoken = YYSYMBOL_YYEMPTY;
  /* The variables used to return semantic value and location from the
     action routines.  */
  YYSTYPE yyval;
  YYLTYPE yyloc;

  /* The locations where the error started and ended.  */
  YYLTYPE yyerror_range[3];

  /* Buffer for error messages, and its allocated size.  */
  char yymsgbuf[128];
  char *yymsg = yymsgbuf;
  YYPTRDIFF_T yymsg_alloc = sizeof yymsgbuf;

#define YYPOPSTACK(N)   (yyvsp -= (N), yyssp -= (N), yylsp -= (N))

  /* The number of symbols on the RHS of the reduced rule.
     Keep to zero when no symbol should be popped.  */
  int yylen = 0;

  YYDPRINTF ((stderr, "Starting parse\n"));

  yychar = YYEMPTY; /* Cause a token to be read.  */

  yylsp[0] = yylloc;
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
        YYLTYPE *yyls1 = yyls;

        /* Each stack pointer address is followed by the size of the
           data in use in that stack, in bytes.  This used to be a
           conditional around just the two extra args, but that might
           be undefined if yyoverflow is a macro.  */
        yyoverflow (YY_("memory exhausted"),
                    &yyss1, yysize * YYSIZEOF (*yyssp),
                    &yyvs1, yysize * YYSIZEOF (*yyvsp),
                    &yyls1, yysize * YYSIZEOF (*yylsp),
                    &yystacksize);
        yyss = yyss1;
        yyvs = yyvs1;
        yyls = yyls1;
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
        YYSTACK_RELOCATE (yyls_alloc, yyls);
#  undef YYSTACK_RELOCATE
        if (yyss1 != yyssa)
          YYSTACK_FREE (yyss1);
      }
# endif

      yyssp = yyss + yysize - 1;
      yyvsp = yyvs + yysize - 1;
      yylsp = yyls + yysize - 1;

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
      yyerror_range[1] = yylloc;
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
  *++yylsp = yylloc;

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

  /* Default location. */
  YYLLOC_DEFAULT (yyloc, (yylsp - yylen), yylen);
  yyerror_range[1] = yyloc;
  YY_REDUCE_PRINT (yyn);
  switch (yyn)
    {
  case 2: /* root: classes  */
#line 178 "lang11d"
                                        { 
						(yyval.node) = gRootParseNode = (yyvsp[0].node);
						gParserResult = 1;
					}
#line 2268 "lang11d_tab.cpp"
    break;

  case 3: /* root: classextensions  */
#line 183 "lang11d"
                                        { 
						(yyval.node) = gRootParseNode = (yyvsp[0].node);
						gParserResult = 1;
					}
#line 2277 "lang11d_tab.cpp"
    break;

  case 4: /* root: INTERPRET cmdlinecode  */
#line 188 "lang11d"
                                        { 
						(yyval.node) = gRootParseNode = (yyvsp[0].node);
						gParserResult = 2; 
					}
#line 2286 "lang11d_tab.cpp"
    break;

  case 5: /* classes: %empty  */
#line 193 "lang11d"
                                 { (yyval.node) = nullptr; }
#line 2292 "lang11d_tab.cpp"
    break;

  case 6: /* classes: classes classdef  */
#line 195 "lang11d"
                                        { (yyval.node) = linkNextNode((yyvsp[-1].node), (yyvsp[0].node)); }
#line 2298 "lang11d_tab.cpp"
    break;

  case 8: /* classextensions: classextensions classextension  */
#line 199 "lang11d"
                                        { (yyval.node) = linkNextNode((yyvsp[-1].node), (yyvsp[0].node)); }
#line 2304 "lang11d_tab.cpp"
    break;

  case 9: /* classdef: CLASSNAME optSuperName '{' classvardecls methods '}'  */
#line 202 "lang11d"
                                        { (yyval.node) = allocNode<PyrClassNode>((yyloc), (yyvsp[-5].slotNode), (yyvsp[-4].slotNode), nullptr, (yyvsp[-2].varListNode), (yyvsp[-1].methodNode)); }
#line 2310 "lang11d_tab.cpp"
    break;

  case 10: /* classdef: CLASSNAME '[' optName ']' optSuperName '{' classvardecls methods '}'  */
#line 204 "lang11d"
                                        { (yyval.node) = allocNode<PyrClassNode>((yyloc), (yyvsp[-8].slotNode), (yyvsp[-4].slotNode), (yyvsp[-6].slotNode), (yyvsp[-2].varListNode), (yyvsp[-1].methodNode)); }
#line 2316 "lang11d_tab.cpp"
    break;

  case 11: /* classextension: '+' CLASSNAME '{' methods '}'  */
#line 207 "lang11d"
                                        { (yyval.node) = allocNode<PyrClassExtNode>((yyloc), (yyvsp[-3].slotNode), (yyvsp[-1].methodNode)); }
#line 2322 "lang11d_tab.cpp"
    break;

  case 12: /* optName: %empty  */
#line 209 "lang11d"
                                 { (yyval.slotNode) = nullptr; }
#line 2328 "lang11d_tab.cpp"
    break;

  case 14: /* optSuperName: %empty  */
#line 212 "lang11d"
                         { (yyval.slotNode) = nullptr; }
#line 2334 "lang11d_tab.cpp"
    break;

  case 15: /* optSuperName: ':' CLASSNAME  */
#line 212 "lang11d"
                                                           { (yyval.slotNode) = (yyvsp[0].slotNode); }
#line 2340 "lang11d_tab.cpp"
    break;

  case 16: /* classvardecls: %empty  */
#line 214 "lang11d"
                         { (yyval.varListNode) = nullptr; }
#line 2346 "lang11d_tab.cpp"
    break;

  case 17: /* classvardecls: classvardecls classvardecl  */
#line 216 "lang11d"
                                        { (yyval.varListNode) = linkNextNode((yyvsp[-1].varListNode), (yyvsp[0].varListNode)); }
#line 2352 "lang11d_tab.cpp"
    break;

  case 18: /* classvardecl: CLASSVAR rwslotdeflist ';'  */
#line 219 "lang11d"
                                        { (yyval.varListNode) = allocNode<PyrVarListNode>((yyloc), (yyvsp[-1].varDefNode), varClass); }
#line 2358 "lang11d_tab.cpp"
    break;

  case 19: /* classvardecl: VAR rwslotdeflist ';'  */
#line 221 "lang11d"
                                        { (yyval.varListNode) = allocNode<PyrVarListNode>((yyloc), (yyvsp[-1].varDefNode), varInst); }
#line 2364 "lang11d_tab.cpp"
    break;

  case 20: /* classvardecl: SC_CONST constdeflist ';'  */
#line 223 "lang11d"
                                        { (yyval.varListNode) = allocNode<PyrVarListNode>((yyloc), (yyvsp[-1].varDefNode), varConst); }
#line 2370 "lang11d_tab.cpp"
    break;

  case 21: /* methods: %empty  */
#line 225 "lang11d"
                                 { (yyval.methodNode) = nullptr; }
#line 2376 "lang11d_tab.cpp"
    break;

  case 22: /* methods: methods methoddef  */
#line 227 "lang11d"
                                        { (yyval.methodNode) = linkNextNode((yyvsp[-1].methodNode), (yyvsp[0].methodNode)); }
#line 2382 "lang11d_tab.cpp"
    break;

  case 23: /* methoddef: name '{' argdecls funcvardecls optPrim methbody '}'  */
#line 230 "lang11d"
                                        { (yyval.methodNode) = allocNode<PyrMethodNode>((yyloc), (yyvsp[-6].slotNode), (yyvsp[-2].slotNode), (yyvsp[-4].argListNode), (yyvsp[-3].varListNode), (yyvsp[-1].node), false); }
#line 2388 "lang11d_tab.cpp"
    break;

  case 24: /* methoddef: '*' name '{' argdecls funcvardecls optPrim methbody '}'  */
#line 232 "lang11d"
                                        { (yyval.methodNode) = allocNode<PyrMethodNode>((yyloc), (yyvsp[-6].slotNode), (yyvsp[-2].slotNode), (yyvsp[-4].argListNode), (yyvsp[-3].varListNode), (yyvsp[-1].node), true); }
#line 2394 "lang11d_tab.cpp"
    break;

  case 25: /* methoddef: binop '{' argdecls funcvardecls optPrim methbody '}'  */
#line 234 "lang11d"
                                        { (yyval.methodNode) = allocNode<PyrMethodNode>((yyloc), (yyvsp[-6].slotNode), (yyvsp[-2].slotNode), (yyvsp[-4].argListNode), (yyvsp[-3].varListNode), (yyvsp[-1].node), false); }
#line 2400 "lang11d_tab.cpp"
    break;

  case 26: /* methoddef: '*' binop '{' argdecls funcvardecls optPrim methbody '}'  */
#line 236 "lang11d"
                                        { (yyval.methodNode) = allocNode<PyrMethodNode>((yyloc), (yyvsp[-6].slotNode), (yyvsp[-2].slotNode), (yyvsp[-4].argListNode), (yyvsp[-3].varListNode), (yyvsp[-1].node), true); }
#line 2406 "lang11d_tab.cpp"
    break;

  case 34: /* funcbody: exprseq funretval  */
#line 244 "lang11d"
                                        { (yyval.node) = allocNode<PyrDropNode>((yyloc), (yyvsp[-1].node), (yyvsp[0].node)); }
#line 2412 "lang11d_tab.cpp"
    break;

  case 35: /* cmdlinecode: '(' argdecls1 funcvardecls1 funcbody ')'  */
#line 247 "lang11d"
                                        { (yyval.node) = allocNode<PyrBlockNode>((yyloc), (yyvsp[-3].argListNode), (yyvsp[-2].varListNode), (yyvsp[-1].node), false); }
#line 2418 "lang11d_tab.cpp"
    break;

  case 36: /* cmdlinecode: '(' argdecls1 funcbody ')'  */
#line 249 "lang11d"
                                        { (yyval.node) = allocNode<PyrBlockNode>((yyloc), (yyvsp[-2].argListNode), nullptr, (yyvsp[-1].node), false); }
#line 2424 "lang11d_tab.cpp"
    break;

  case 37: /* cmdlinecode: '(' funcvardecls1 funcbody ')'  */
#line 251 "lang11d"
                                        { (yyval.node) = allocNode<PyrBlockNode>((yyloc), nullptr, (yyvsp[-2].varListNode), (yyvsp[-1].node), false); }
#line 2430 "lang11d_tab.cpp"
    break;

  case 38: /* cmdlinecode: argdecls1 funcvardecls1 funcbody  */
#line 253 "lang11d"
                                        { (yyval.node) = allocNode<PyrBlockNode>((yyloc), (yyvsp[-2].argListNode), (yyvsp[-1].varListNode), (yyvsp[0].node), false); }
#line 2436 "lang11d_tab.cpp"
    break;

  case 39: /* cmdlinecode: argdecls1 funcbody  */
#line 255 "lang11d"
                                        { (yyval.node) = allocNode<PyrBlockNode>((yyloc), (yyvsp[-1].argListNode), nullptr, (yyvsp[0].node), false); }
#line 2442 "lang11d_tab.cpp"
    break;

  case 40: /* cmdlinecode: funcvardecls1 funcbody  */
#line 257 "lang11d"
                                        { (yyval.node) = allocNode<PyrBlockNode>((yyloc), nullptr, (yyvsp[-1].varListNode), (yyvsp[0].node), false); }
#line 2448 "lang11d_tab.cpp"
    break;

  case 41: /* cmdlinecode: funcbody  */
#line 259 "lang11d"
                                        { (yyval.node) = allocNode<PyrBlockNode>((yyloc), nullptr, nullptr, (yyvsp[0].node), false); }
#line 2454 "lang11d_tab.cpp"
    break;

  case 43: /* methbody: exprseq retval  */
#line 263 "lang11d"
                                        { (yyval.node) = allocNode<PyrDropNode>((yyloc), (yyvsp[-1].node), (yyvsp[0].node)); }
#line 2460 "lang11d_tab.cpp"
    break;

  case 44: /* optPrim: %empty  */
#line 265 "lang11d"
                                { (yyval.slotNode) = nullptr; }
#line 2466 "lang11d_tab.cpp"
    break;

  case 45: /* optPrim: PRIMITIVENAME optsemi  */
#line 266 "lang11d"
                                                        { (yyval.slotNode) = (yyvsp[-1].slotNode); }
#line 2472 "lang11d_tab.cpp"
    break;

  case 46: /* retval: %empty  */
#line 268 "lang11d"
                                 { (yyval.node) = allocNode<PyrReturnNode>((yyloc), nullptr); }
#line 2478 "lang11d_tab.cpp"
    break;

  case 47: /* retval: '^' expr optsemi  */
#line 270 "lang11d"
                                        { (yyval.node) = allocNode<PyrReturnNode>((yyloc), (yyvsp[-1].node)); }
#line 2484 "lang11d_tab.cpp"
    break;

  case 48: /* funretval: %empty  */
#line 273 "lang11d"
                                        { (yyval.node) = allocNode<PyrBlockReturnNode>((yyloc), nullptr); }
#line 2490 "lang11d_tab.cpp"
    break;

  case 49: /* funretval: '^' expr optsemi  */
#line 275 "lang11d"
                                        { (yyval.node) = allocNode<PyrReturnNode>((yyloc), (yyvsp[-1].node)); }
#line 2496 "lang11d_tab.cpp"
    break;

  case 51: /* blocklist1: blocklist1 blocklistitem  */
#line 279 "lang11d"
                                        { (yyval.node) = linkNextNode((yyvsp[-1].node), (yyvsp[0].node)); }
#line 2502 "lang11d_tab.cpp"
    break;

  case 54: /* blocklist: %empty  */
#line 283 "lang11d"
                                 { (yyval.node) = nullptr; }
#line 2508 "lang11d_tab.cpp"
    break;

  case 56: /* msgsend: name blocklist1  */
#line 286 "lang11d"
                                        { (yyval.node) = allocNode<PyrCallNode>((yyloc), (yyvsp[-1].slotNode), (yyvsp[0].node), nullptr); }
#line 2514 "lang11d_tab.cpp"
    break;

  case 57: /* msgsend: '(' binop2 ')' blocklist1  */
#line 289 "lang11d"
                                        { (yyval.node) = allocNode<PyrCallNode>((yyloc), (yyvsp[-2].slotNode), (yyvsp[0].node), nullptr); }
#line 2520 "lang11d_tab.cpp"
    break;

  case 58: /* msgsend: name '(' ')' blocklist1  */
#line 292 "lang11d"
                                        { (yyval.node) = allocNode<PyrCallNode>((yyloc), (yyvsp[-3].slotNode), (yyvsp[0].node), nullptr); }
#line 2526 "lang11d_tab.cpp"
    break;

  case 59: /* msgsend: name '(' arglist1 optkeyarglist ')' blocklist  */
#line 295 "lang11d"
                                        { (yyval.node) = allocNode<PyrCallNode>((yyloc), (yyvsp[-5].slotNode), linkNextNode((yyvsp[-3].node), (yyvsp[0].node)), (yyvsp[-2].node)); }
#line 2532 "lang11d_tab.cpp"
    break;

  case 60: /* msgsend: '(' binop2 ')' '(' ')' blocklist1  */
#line 298 "lang11d"
                                        { (yyval.node) = allocNode<PyrCallNode>((yyloc), (yyvsp[-4].slotNode), (yyvsp[0].node), nullptr); }
#line 2538 "lang11d_tab.cpp"
    break;

  case 61: /* msgsend: '(' binop2 ')' '(' arglist1 optkeyarglist ')' blocklist  */
#line 301 "lang11d"
                                        { (yyval.node) = allocNode<PyrCallNode>((yyloc), (yyvsp[-6].slotNode), linkNextNode((yyvsp[-3].node), (yyvsp[0].node)), (yyvsp[-2].node)); }
#line 2544 "lang11d_tab.cpp"
    break;

  case 62: /* msgsend: name '(' arglistv1 optkeyarglist ')'  */
#line 304 "lang11d"
                                        {
						PyrSlot slot;
						if (isSuperObjNode((yyvsp[-2].node))) {
							assertCast<PyrSlotNode>((yyvsp[-2].node))->mSlot = PyrSlot::make(s_this);
							slot = PyrSlot::make(s_superPerformList);
						} else {
							slot = PyrSlot::make(s_performList);
						}
						auto* selectornode = allocNode<PyrSlotNode>((yyloc), slot);
						auto* args = linkAfterHead((yyvsp[-2].node), (yyvsp[-4].slotNode)->changeLiteralType(pn_PushLitNode));
						(yyval.node) = allocNode<PyrCallNode>((yyloc), selectornode, args, (yyvsp[-1].node));
					}
#line 2561 "lang11d_tab.cpp"
    break;

  case 63: /* msgsend: '(' binop2 ')' '(' arglistv1 optkeyarglist ')'  */
#line 317 "lang11d"
                                        {
						auto* selectornode = allocNode<PyrSlotNode>((yyloc), PyrSlot::make(s_performList));
						auto* args = linkAfterHead((yyvsp[-2].node), (yyvsp[-5].slotNode)->changeLiteralType(pn_PushLitNode));
						(yyval.node) = allocNode<PyrCallNode>((yyloc), selectornode, args, (yyvsp[-1].node));
					}
#line 2571 "lang11d_tab.cpp"
    break;

  case 64: /* msgsend: CLASSNAME '[' arrayelems ']'  */
#line 323 "lang11d"
                                        { (yyval.node) = allocNode<PyrDynListNode>((yyloc), (yyvsp[-3].slotNode), (yyvsp[-1].node)); }
#line 2577 "lang11d_tab.cpp"
    break;

  case 65: /* msgsend: CLASSNAME blocklist1  */
#line 326 "lang11d"
                                        {
						auto* selectornode = allocNode<PyrSlotNode>((yyloc), PyrSlot::make(s_new));
						auto* args = linkNextNode((yyvsp[-1].slotNode)->changeLiteralType(pn_PushNameNode), (yyvsp[0].node));
						(yyval.node) = allocNode<PyrCallNode>((yyloc), selectornode, args, nullptr);
					}
#line 2587 "lang11d_tab.cpp"
    break;

  case 66: /* msgsend: CLASSNAME '(' ')' blocklist  */
#line 332 "lang11d"
                                        {
						auto* selectornode = allocNode<PyrSlotNode>((yyloc), PyrSlot::make(s_new));
						auto* args = linkNextNode((yyvsp[-3].slotNode)->changeLiteralType(pn_PushNameNode), (yyvsp[0].node));
						(yyval.node) = allocNode<PyrCallNode>((yyloc), selectornode, args, nullptr);
					}
#line 2597 "lang11d_tab.cpp"
    break;

  case 67: /* msgsend: CLASSNAME '(' keyarglist1 optcomma ')' blocklist  */
#line 338 "lang11d"
                                        {
						auto* selectornode = allocNode<PyrSlotNode>((yyloc), PyrSlot::make(s_new));
						auto* args = linkNextNode((yyvsp[-5].slotNode)->changeLiteralType(pn_PushNameNode), (yyvsp[0].node));
						(yyval.node) = allocNode<PyrCallNode>((yyloc), selectornode, args, (yyvsp[-3].node));
					}
#line 2607 "lang11d_tab.cpp"
    break;

  case 68: /* msgsend: CLASSNAME '(' arglist1 optkeyarglist ')' blocklist  */
#line 344 "lang11d"
                                        {
						auto* selectornode = allocNode<PyrSlotNode>((yyloc), PyrSlot::make(s_new));
						auto* args = linkAllNodes(
							(yyvsp[-5].slotNode)->changeLiteralType(pn_PushNameNode), 
							(yyvsp[-3].node),
							(yyvsp[0].node));
						(yyval.node) = allocNode<PyrCallNode>((yyloc), selectornode, args, (yyvsp[-2].node));
					}
#line 2620 "lang11d_tab.cpp"
    break;

  case 69: /* msgsend: CLASSNAME '(' arglistv1 optkeyarglist ')'  */
#line 353 "lang11d"
                                        {
						PyrSlot slot;
						if (isSuperObjNode((yyvsp[-4].slotNode))) {
							assertCast<PyrSlotNode>((yyvsp[-4].slotNode))->mSlot = PyrSlot::make(s_this);
							slot = PyrSlot::make(s_superPerformList);
						} else {
							slot = PyrSlot::make(s_performList);
						}

						auto* new_selector_push_lit = allocNode<PyrSlotNode>((yyloc), PyrSlot::make(s_new), pn_PushLitNode);
						auto* args = linkAllNodes(
							(yyvsp[-4].slotNode)->changeLiteralType(pn_PushNameNode),
							new_selector_push_lit,
							(yyvsp[-2].node));
						auto* selectornode = allocNode<PyrSlotNode>((yyloc), slot);
						(yyval.node) = allocNode<PyrCallNode>((yyloc), selectornode, args, (yyvsp[-1].node));
					}
#line 2642 "lang11d_tab.cpp"
    break;

  case 70: /* msgsend: expr '.' '(' ')' blocklist  */
#line 371 "lang11d"
                                        {
						auto* selectornode = allocNode<PyrSlotNode>((yyloc), PyrSlot::make(s_value));
						(yyval.node) = allocNode<PyrCallNode>((yyloc), selectornode, linkNextNode((yyvsp[-4].node), (yyvsp[0].node)), nullptr);
					}
#line 2651 "lang11d_tab.cpp"
    break;

  case 71: /* msgsend: expr '.' '(' keyarglist1 optcomma ')' blocklist  */
#line 376 "lang11d"
                                        {
						auto* selectornode = allocNode<PyrSlotNode>((yyloc), PyrSlot::make(s_value));
						(yyval.node) = allocNode<PyrCallNode>((yyloc), selectornode, linkNextNode((yyvsp[-6].node), (yyvsp[0].node)), (yyvsp[-3].node));
					}
#line 2660 "lang11d_tab.cpp"
    break;

  case 72: /* msgsend: expr '.' name '(' keyarglist1 optcomma ')' blocklist  */
#line 381 "lang11d"
                                        { (yyval.node) = allocNode<PyrCallNode>((yyloc), (yyvsp[-5].slotNode), linkNextNode((yyvsp[-7].node), (yyvsp[0].node)), (yyvsp[-3].node)); }
#line 2666 "lang11d_tab.cpp"
    break;

  case 73: /* msgsend: expr '.' '(' arglist1 optkeyarglist ')' blocklist  */
#line 383 "lang11d"
                                        {
						auto* selectornode = allocNode<PyrSlotNode>((yyloc), PyrSlot::make(s_value));
						(yyval.node) = allocNode<PyrCallNode>((yyloc), selectornode, linkAllNodes((yyvsp[-6].node), (yyvsp[-3].node), (yyvsp[0].node)), (yyvsp[-2].node));
					}
#line 2675 "lang11d_tab.cpp"
    break;

  case 74: /* msgsend: expr '.' '(' arglistv1 optkeyarglist ')'  */
#line 388 "lang11d"
                                        {
						PyrSlot selectorSlot;
						if (isSuperObjNode((yyvsp[-5].node))) {
							assertCast<PyrSlotNode>((yyvsp[-5].node))->mSlot = PyrSlot::make(s_this);
							selectorSlot = PyrSlot::make(s_superPerformList);
						} else {
							selectorSlot = PyrSlot::make(s_performList);
						}
						auto* selectornode = allocNode<PyrSlotNode>((yyloc), selectorSlot);
						auto* args = linkAllNodes(
							(yyvsp[-5].node), 
							allocNode<PyrSlotNode>((yyloc), PyrSlot::make(s_value), pn_PushLitNode),
							(yyvsp[-2].node));
						(yyval.node) = allocNode<PyrCallNode>((yyloc), selectornode, args, (yyvsp[-1].node));
					}
#line 2695 "lang11d_tab.cpp"
    break;

  case 75: /* msgsend: expr '.' name '(' ')' blocklist  */
#line 404 "lang11d"
                                        { (yyval.node) = allocNode<PyrCallNode>((yyloc), (yyvsp[-3].slotNode), linkNextNode((yyvsp[-5].node), (yyvsp[0].node)), nullptr); }
#line 2701 "lang11d_tab.cpp"
    break;

  case 76: /* msgsend: expr '.' name '(' arglist1 optkeyarglist ')' blocklist  */
#line 406 "lang11d"
                                        { (yyval.node) = allocNode<PyrCallNode>((yyloc), (yyvsp[-5].slotNode), linkAllNodes((yyvsp[-7].node), (yyvsp[-3].node), (yyvsp[0].node)), (yyvsp[-2].node)); }
#line 2707 "lang11d_tab.cpp"
    break;

  case 77: /* msgsend: expr '.' name '(' arglistv1 optkeyarglist ')'  */
#line 408 "lang11d"
                                        {
						PyrSlot slot;
						if (isSuperObjNode((yyvsp[-6].node))) {
							assertCast<PyrSlotNode>((yyvsp[-6].node))->mSlot = PyrSlot::make(s_this);
							slot = PyrSlot::make(s_superPerformList);
						} else {
							slot = PyrSlot::make(s_performList);
						}
						auto* selectornode = allocNode<PyrSlotNode>((yyloc), slot);
						auto* args = linkAllNodes((yyvsp[-6].node), (yyvsp[-4].slotNode)->changeLiteralType(pn_PushLitNode), (yyvsp[-2].node));
						(yyval.node) = allocNode<PyrCallNode>((yyloc), selectornode, args, (yyvsp[-1].node));
					}
#line 2724 "lang11d_tab.cpp"
    break;

  case 78: /* msgsend: expr '.' name blocklist  */
#line 421 "lang11d"
                                        { (yyval.node) = allocNode<PyrCallNode>((yyloc), (yyvsp[-1].slotNode), linkNextNode((yyvsp[-3].node), (yyvsp[0].node)), nullptr); }
#line 2730 "lang11d_tab.cpp"
    break;

  case 79: /* $@1: %empty  */
#line 429 "lang11d"
                                          { pushls(&generatorStack, (intptr_t)(yyvsp[0].node)); pushls(&generatorStack, 1); }
#line 2736 "lang11d_tab.cpp"
    break;

  case 80: /* generator: '{' ':' exprseq $@1 ',' qual '}'  */
#line 430 "lang11d"
                                        {
						PyrSlotNode* selectornode = allocNode<PyrSlotNode>((yyloc), PyrSlot::make(getsym("r")));
						PyrParseNode *block = allocNode<PyrBlockNode>((yyloc), nullptr, nullptr, (yyvsp[-1].node), false);
						PyrParseNode *blocklit = allocNode<PyrSlotNode>((yyloc), PyrSlot::make<void*>(block), pn_PushLitNode);
						(yyval.node) = allocNode<PyrCallNode>((yyloc), selectornode, blocklit, nullptr);
					}
#line 2747 "lang11d_tab.cpp"
    break;

  case 81: /* $@2: %empty  */
#line 436 "lang11d"
                                                  { pushls(&generatorStack, (intptr_t)(yyvsp[0].node)); pushls(&generatorStack, 2); }
#line 2753 "lang11d_tab.cpp"
    break;

  case 82: /* generator: '{' ';' exprseq $@2 ',' qual '}'  */
#line 437 "lang11d"
                                        { (yyval.node) = (yyvsp[-1].node); }
#line 2759 "lang11d_tab.cpp"
    break;

  case 83: /* nextqual: %empty  */
#line 440 "lang11d"
                                        {
						// innermost part
						const int action = popls(&generatorStack);
						PyrParseNode* expr = (PyrParseNode*)popls(&generatorStack);
						switch (action) {
							case 1 : 
								(yyval.node) = allocNode<PyrCallNode>((yyloc), allocNode<PyrSlotNode>((yyloc), PyrSlot::make(getsym("yield"))), expr, nullptr);
								break;
							case 2 : 
								(yyval.node) = expr; 
								break;
						}
					}
#line 2777 "lang11d_tab.cpp"
    break;

  case 84: /* nextqual: ',' qual  */
#line 453 "lang11d"
                                           { (yyval.node) = (yyvsp[0].node); }
#line 2783 "lang11d_tab.cpp"
    break;

  case 85: /* qual: name LEFTARROW exprseq nextqual  */
#line 456 "lang11d"
                                        {
						// later should check if exprseq is a series and optimize it to for loop
						PyrParseNode *exprseq = (yyvsp[-1].node);
						if (exprseq->mClassno == pn_CallNode) {
							PyrCallNode *callnode = assertCast<PyrCallNode>(exprseq);
							if (slotRawSymbol(&callnode->mSelector->mSlot) == s_series) {
								SetSymbol(&callnode->mSelector->mSlot, getsym("forSeries"));

								auto* var = allocNode<PyrVarDefNode>((yyloc), (yyvsp[-3].slotNode), nullptr, ReadWriteAccessor::Private);
								auto* args = allocNode<PyrArgListNode>((yyloc), var, nullptr, nullptr);
								auto *block = allocNode<PyrBlockNode>((yyloc), args, nullptr, (yyvsp[0].node), false);
								auto *blocklit = allocNode<PyrSlotNode>((yyloc), PyrSlot::make<void*>(block), pn_PushLitNode);

								callnode->mArglist = linkNextNode(callnode->mArglist, blocklit);
								(yyval.node) = callnode;

							} else goto notoptimized1;
						} else {
							notoptimized1:
							PyrSlot slot;
							SetSymbol(&slot, getsym("do"));
							auto* selectornode = allocNode<PyrSlotNode>((yyloc), slot);

							auto* var = allocNode<PyrVarDefNode>((yyloc), (yyvsp[-3].slotNode), nullptr, ReadWriteAccessor::Private);
							auto* args = allocNode<PyrArgListNode>((yyloc), var, nullptr, nullptr);
							auto *block = allocNode<PyrBlockNode>((yyloc), args, nullptr, (yyvsp[0].node), false);
							auto *blocklit = allocNode<PyrSlotNode>((yyloc), PyrSlot::make<void*>(block), pn_PushLitNode);

							PyrParseNode* args2 = linkNextNode(exprseq, blocklit);
							(yyval.node) = allocNode<PyrCallNode>((yyloc), selectornode, args2, nullptr);
						}
					}
#line 2820 "lang11d_tab.cpp"
    break;

  case 86: /* qual: name name LEFTARROW exprseq nextqual  */
#line 489 "lang11d"
                                        {
						// later should check if exprseq is a series and optimize it to for loop
						PyrParseNode *exprseq = (yyvsp[-1].node);
						if (exprseq->mClassno == pn_CallNode) {
							PyrCallNode *callnode = assertCast<PyrCallNode>(exprseq);
							if (slotRawSymbol(&callnode->mSelector->mSlot) == s_series) {
								SetSymbol(&callnode->mSelector->mSlot, getsym("forSeries"));

								auto* var1 = allocNode<PyrVarDefNode>((yyloc), (yyvsp[-4].slotNode), nullptr, ReadWriteAccessor::Private);
								auto* var2 = allocNode<PyrVarDefNode>((yyloc), (yyvsp[-3].slotNode), nullptr, ReadWriteAccessor::Private);
								auto* vars = linkNextNode(var1, var2);
								auto* args = allocNode<PyrArgListNode>((yyloc), vars, nullptr, nullptr);
								auto *block = allocNode<PyrBlockNode>((yyloc), args, nullptr, (yyvsp[0].node), false);
								auto *blocklit = allocNode<PyrSlotNode>((yyloc), PyrSlot::make<void*>(block), pn_PushLitNode);

								callnode->mArglist = linkNextNode(callnode->mArglist, blocklit);
								(yyval.node) = callnode;

							} else goto notoptimized2;
						} else {
							notoptimized2:
							PyrSlot slot;
							SetSymbol(&slot, getsym("do"));
							PyrSlotNode* selectornode = allocNode<PyrSlotNode>((yyloc), slot);

							auto* var1 = allocNode<PyrVarDefNode>((yyloc), (yyvsp[-4].slotNode), nullptr, ReadWriteAccessor::Private);
							auto* var2 = allocNode<PyrVarDefNode>((yyloc), (yyvsp[-3].slotNode), nullptr, ReadWriteAccessor::Private);
							auto* vars = linkNextNode(var1, var2);
							auto* args = allocNode<PyrArgListNode>((yyloc), vars, nullptr, nullptr);
							auto *block = allocNode<PyrBlockNode>((yyloc), args, nullptr, (yyvsp[0].node), false);
							auto *blocklit = allocNode<PyrSlotNode>((yyloc), PyrSlot::make<void*>(block), pn_PushLitNode);

							PyrParseNode* args2 = linkNextNode(exprseq, blocklit);
							(yyval.node) = allocNode<PyrCallNode>((yyloc), selectornode, args2, nullptr);
						}
					}
#line 2861 "lang11d_tab.cpp"
    break;

  case 87: /* qual: VAR name '=' exprseq nextqual  */
#line 526 "lang11d"
                                        {
						PyrSlot slot = PyrSlot::make(s_value);
						auto* selectornode = allocNode<PyrSlotNode>((yyloc), slot);
						auto* var = allocNode<PyrVarDefNode>((yyloc), (yyvsp[-3].slotNode), nullptr, ReadWriteAccessor::Private);
						auto* args = allocNode<PyrArgListNode>((yyloc), var, nullptr, nullptr);
						auto *block = allocNode<PyrBlockNode>((yyloc), args, nullptr, (yyvsp[0].node), false);
						auto *blocklit = allocNode<PyrSlotNode>((yyloc), PyrSlot::make<void*>(block), pn_PushLitNode);
						auto* args2 = linkNextNode(blocklit, (yyvsp[-1].node));
						(yyval.node) = allocNode<PyrCallNode>((yyloc), selectornode, args2, nullptr);
					}
#line 2876 "lang11d_tab.cpp"
    break;

  case 88: /* qual: exprseq nextqual  */
#line 537 "lang11d"
                                        {
						PyrSlotNode* selectornode = allocNode<PyrSlotNode>((yyloc), PyrSlot::make(getsym("if")));
						PyrParseNode* block = allocNode<PyrBlockNode>((yyloc), nullptr, nullptr, (yyvsp[0].node), false);
						PyrParseNode* blocklit = allocNode<PyrSlotNode>((yyloc), PyrSlot::make<void*>(block), pn_PushLitNode);
						PyrParseNode* args2 = linkNextNode((yyvsp[-1].node), blocklit);
						(yyval.node) = allocNode<PyrCallNode>((yyloc), selectornode, args2, nullptr);
					}
#line 2888 "lang11d_tab.cpp"
    break;

  case 89: /* qual: ':' ':' exprseq nextqual  */
#line 545 "lang11d"
                                        { (yyval.node) = allocNode<PyrDropNode>((yyloc), (yyvsp[-1].node), (yyvsp[0].node)); }
#line 2894 "lang11d_tab.cpp"
    break;

  case 90: /* qual: ':' WHILE exprseq nextqual  */
#line 547 "lang11d"
                                        {
						PyrSlotNode* selectornode1 = allocNode<PyrSlotNode>((yyloc), PyrSlot::make(getsym("alwaysYield")));
						PyrParseNode* pushnil = allocNode<PyrSlotNode>((yyloc), PyrSlot{}, pn_PushLitNode);
						PyrParseNode* yieldNil = allocNode<PyrCallNode>((yyloc), selectornode1, pushnil, nullptr);
						PyrParseNode* block1 = allocNode<PyrBlockNode>((yyloc), nullptr, nullptr, yieldNil, false);
						PyrParseNode* blocklit1 = allocNode<PyrSlotNode>((yyloc), PyrSlot::make<void*>(block1), pn_PushLitNode);
						PyrParseNode* block2 = allocNode<PyrBlockNode>((yyloc), nullptr, nullptr, (yyvsp[0].node), false);
						PyrParseNode* blocklit2 = allocNode<PyrSlotNode>((yyloc), PyrSlot::make<void*>(block2), pn_PushLitNode);
						PyrParseNode* args2 = linkNextNode((yyvsp[-1].node), blocklit2);
						PyrParseNode* args3 = linkNextNode(args2, blocklit1);

						PyrSlotNode* selectornode2 = allocNode<PyrSlotNode>((yyloc), PyrSlot::make(getsym("if")));
						(yyval.node) = allocNode<PyrCallNode>((yyloc), selectornode2, args3, nullptr);
					}
#line 2913 "lang11d_tab.cpp"
    break;

  case 97: /* expr1: '(' exprseq ')'  */
#line 570 "lang11d"
                                        {
						PyrParseNode* node = (yyvsp[-1].node);
						node->mParens = 1;
						node->mLocation = (yyloc); // make the location include the brackets.
						(yyval.node) = (yyvsp[-1].node);
					}
#line 2924 "lang11d_tab.cpp"
    break;

  case 98: /* expr1: '~' name  */
#line 577 "lang11d"
                                        {
						auto* argnode = (yyvsp[0].slotNode)->changeLiteralType(pn_PushLitNode);
						auto* selectornode = allocNode<PyrSlotNode>((yyloc), PyrSlot::make(s_envirGet));
						(yyval.node) = allocNode<PyrCallNode>((yyloc), selectornode, argnode, nullptr);
					}
#line 2934 "lang11d_tab.cpp"
    break;

  case 99: /* expr1: '[' arrayelems ']'  */
#line 583 "lang11d"
                                        { (yyval.node) = allocNode<PyrDynListNode>((yyloc), nullptr, (yyvsp[-1].node)); }
#line 2940 "lang11d_tab.cpp"
    break;

  case 100: /* expr1: '(' valrange2 ')'  */
#line 585 "lang11d"
                                        { (yyval.node) = (yyvsp[-1].node); }
#line 2946 "lang11d_tab.cpp"
    break;

  case 101: /* expr1: '(' ':' valrange3 ')'  */
#line 587 "lang11d"
                                        { (yyval.node) = (yyvsp[-1].node); }
#line 2952 "lang11d_tab.cpp"
    break;

  case 102: /* expr1: '(' dictslotlist ')'  */
#line 589 "lang11d"
                                        { (yyval.node) = allocNode<PyrDynDictNode>((yyloc), (yyvsp[-1].node)); }
#line 2958 "lang11d_tab.cpp"
    break;

  case 103: /* expr1: expr1 '[' arglist1 ']'  */
#line 591 "lang11d"
                                        {
						auto* selectornode = allocNode<PyrSlotNode>((yyloc), PyrSlot::make(s_at));
						auto* args = linkNextNode((yyvsp[-3].node), (yyvsp[-1].node));
						(yyval.node) = allocNode<PyrCallNode>((yyloc), selectornode, args, nullptr);
					}
#line 2968 "lang11d_tab.cpp"
    break;

  case 105: /* valrangex1: expr1 '[' arglist1 DOTDOT ']'  */
#line 599 "lang11d"
                                        {
						const int arglen = nodeListLength((yyvsp[-2].node));
						if (arglen > 2) {
							error("ArrayedCollection subrange has too many arguments.\n");
							nodePostErrorLine((yyvsp[-2].node));
							compileErrors++;
						}
						auto* args = linkNextNode((yyvsp[-4].node), (yyvsp[-2].node));
						if (arglen < 2) 
							args = linkNextNode(args, allocNode<PyrSlotNode>((yyloc), PyrSlot{}, pn_PushLitNode));

						args = linkNextNode(args, allocNode<PyrSlotNode>((yyloc), PyrSlot{}, pn_PushLitNode));
						auto* selectornode = allocNode<PyrSlotNode>((yyloc), PyrSlot::make(s_copyseries));
						(yyval.node) = allocNode<PyrCallNode>((yyloc), selectornode, args, nullptr);
					}
#line 2988 "lang11d_tab.cpp"
    break;

  case 106: /* valrangex1: expr1 '[' DOTDOT exprseq ']'  */
#line 615 "lang11d"
                                        {
						auto* nilnode1 = allocNode<PyrSlotNode>((yyloc), PyrSlot{}, pn_PushLitNode);
						auto* args = linkNextNode((yyvsp[-4].node), nilnode1);
						auto* nilnode2 = allocNode<PyrSlotNode>((yyloc), PyrSlot{}, pn_PushLitNode);
						args = linkNextNode(args, nilnode2);
						args = linkNextNode(args, (yyvsp[-1].node));
						auto* selectornode = allocNode<PyrSlotNode>((yyloc), PyrSlot::make(s_copyseries));
						(yyval.node) = allocNode<PyrCallNode>((yyloc), selectornode, args, nullptr);
					}
#line 3002 "lang11d_tab.cpp"
    break;

  case 107: /* valrangex1: expr1 '[' arglist1 DOTDOT exprseq ']'  */
#line 625 "lang11d"
                                        {
						const int arglen = nodeListLength((yyvsp[-3].node));
						if (arglen > 2) {
							error("ArrayedCollection subrange has too many arguments.\n");
							nodePostErrorLine((yyvsp[-3].node));
							compileErrors++;
						}

						auto* args = linkNextNode((yyvsp[-5].node), (yyvsp[-3].node));
						if (arglen < 2) 
							args = linkNextNode(args, allocNode<PyrSlotNode>((yyloc), PyrSlot{}, pn_PushLitNode));
						
						args = linkNextNode(args, (yyvsp[-1].node));
						auto* selectornode = allocNode<PyrSlotNode>((yyloc), PyrSlot::make(s_copyseries));
						(yyval.node) = allocNode<PyrCallNode>((yyloc), selectornode, args, nullptr);
					}
#line 3023 "lang11d_tab.cpp"
    break;

  case 108: /* valrangeassign: expr1 '[' arglist1 DOTDOT ']' '=' expr  */
#line 643 "lang11d"
                                        {
						const int arglen = nodeListLength((yyvsp[-4].node));
						if (arglen > 2) {
							error("ArrayedCollection subrange has too many arguments.\n");
							nodePostErrorLine((yyvsp[-4].node));
							compileErrors++;
						}

						auto* args = linkNextNode((yyvsp[-6].node), (yyvsp[-4].node));
						if (arglen < 2) 
							args = linkNextNode(args, allocNode<PyrSlotNode>((yyloc), PyrSlot{}, pn_PushLitNode));
						
						args = linkAllNodes(args, allocNode<PyrSlotNode>((yyloc), PyrSlot{}, pn_PushLitNode), (yyvsp[0].node));
						auto* selectornode = allocNode<PyrSlotNode>((yyloc), PyrSlot::make(s_putseries));
						(yyval.node) = allocNode<PyrCallNode>((yyloc), selectornode, args, nullptr);
					}
#line 3044 "lang11d_tab.cpp"
    break;

  case 109: /* valrangeassign: expr1 '[' DOTDOT exprseq ']' '=' expr  */
#line 660 "lang11d"
                                        {
						auto* args = linkAllNodes(
							(yyvsp[-6].node),
							allocNode<PyrSlotNode>((yyloc), PyrSlot{}, pn_PushLitNode),
							allocNode<PyrSlotNode>((yyloc), PyrSlot{}, pn_PushLitNode),
							(yyvsp[-3].node), 
							(yyvsp[0].node));
						auto* selectornode = allocNode<PyrSlotNode>((yyloc), PyrSlot::make(s_putseries));
						(yyval.node) = allocNode<PyrCallNode>((yyloc), selectornode, args, nullptr);
					}
#line 3059 "lang11d_tab.cpp"
    break;

  case 110: /* valrangeassign: expr1 '[' arglist1 DOTDOT exprseq ']' '=' expr  */
#line 671 "lang11d"
                                        {
						const int arglen = nodeListLength((yyvsp[-5].node));
						if (arglen > 2) {
							error("ArrayedCollection subrange has too many arguments.\n");
							nodePostErrorLine((yyvsp[-5].node));
							compileErrors++;
						}

						auto* args = linkNextNode((yyvsp[-7].node), (yyvsp[-5].node));
						if (arglen < 2) 
							args = linkNextNode(args, allocNode<PyrSlotNode>((yyloc), PyrSlot{}, pn_PushLitNode));
						
						args = linkAllNodes(args, (yyvsp[-3].node), (yyvsp[0].node));
						auto* selectornode = allocNode<PyrSlotNode>((yyloc), PyrSlot::make(s_putseries));
						(yyval.node) = allocNode<PyrCallNode>((yyloc), selectornode, args, nullptr);
					}
#line 3080 "lang11d_tab.cpp"
    break;

  case 111: /* valrangexd: expr '.' '[' arglist1 DOTDOT ']'  */
#line 694 "lang11d"
                                        {
						PyrSlotNode* nilnode1, *nilnode2;
						PyrSlot selectorSlot, nilSlot;
						PyrParseNode* args;

						const int arglen = nodeListLength((yyvsp[-2].node));
						if (arglen > 2) {
							error("ArrayedCollection subrange has too many arguments.\n");
							nodePostErrorLine((yyvsp[-2].node));
							compileErrors++;
						}

						args = linkNextNode((yyvsp[-5].node), (yyvsp[-2].node));
						if (arglen < 2) 
							args = linkNextNode(args, allocNode<PyrSlotNode>((yyloc), nilSlot, pn_PushLitNode));

						args = linkNextNode(args, allocNode<PyrSlotNode>((yyloc), PyrSlot{}, pn_PushLitNode));

						auto* selectornode = allocNode<PyrSlotNode>((yyloc), PyrSlot::make(s_copyseries));
						(yyval.node) = allocNode<PyrCallNode>((yyloc), selectornode, args, nullptr);
					}
#line 3106 "lang11d_tab.cpp"
    break;

  case 112: /* valrangexd: expr '.' '[' DOTDOT exprseq ']'  */
#line 716 "lang11d"
                                        {
						auto* args = linkAllNodes(
							(yyvsp[-5].node),
							allocNode<PyrSlotNode>((yyloc), PyrSlot{}, pn_PushLitNode),
							allocNode<PyrSlotNode>((yyloc), PyrSlot{}, pn_PushLitNode),
							(yyvsp[-1].node));
						auto* selectornode = allocNode<PyrSlotNode>((yyloc), PyrSlot::make(s_copyseries));
						(yyval.node) = allocNode<PyrCallNode>((yyloc), selectornode, args, nullptr);
					}
#line 3120 "lang11d_tab.cpp"
    break;

  case 113: /* valrangexd: expr '.' '[' arglist1 DOTDOT exprseq ']'  */
#line 726 "lang11d"
                                        {
						const int arglen = nodeListLength((yyvsp[-3].node));
						if (arglen > 2) {
							error("ArrayedCollection subrange has too many arguments.\n");
							nodePostErrorLine((yyvsp[-3].node));
							compileErrors++;
						}

						auto* args = linkNextNode((yyvsp[-6].node), (yyvsp[-3].node));
						if (arglen < 2) 
							args = linkNextNode(args, allocNode<PyrSlotNode>((yyloc), PyrSlot{}, pn_PushLitNode));
						
						args = linkNextNode(args, (yyvsp[-1].node));
						auto* selectornode = allocNode<PyrSlotNode>((yyloc), PyrSlot::make(s_copyseries));
						(yyval.node) = allocNode<PyrCallNode>((yyloc), selectornode, args, nullptr);
					}
#line 3141 "lang11d_tab.cpp"
    break;

  case 114: /* valrangexd: expr '.' '[' arglist1 DOTDOT ']' '=' expr  */
#line 743 "lang11d"
                                        {
						const int arglen = nodeListLength((yyvsp[-4].node));
						if (arglen > 2) {
							error("ArrayedCollection subrange has too many arguments.\n");
							nodePostErrorLine((yyvsp[-4].node));
							compileErrors++;
						}

						auto* args = linkNextNode((yyvsp[-7].node), (yyvsp[-4].node));
						if (arglen < 2) 
							args = linkNextNode(args, allocNode<PyrSlotNode>((yyloc), PyrSlot{}, pn_PushLitNode));
						
						args = linkAllNodes(args, allocNode<PyrSlotNode>((yyloc), PyrSlot{}, pn_PushLitNode), (yyvsp[0].node));
						auto* selectornode = allocNode<PyrSlotNode>((yyloc), PyrSlot::make(s_putseries));
						(yyval.node) = allocNode<PyrCallNode>((yyloc), selectornode, args, nullptr);
					}
#line 3162 "lang11d_tab.cpp"
    break;

  case 115: /* valrangexd: expr '.' '[' DOTDOT exprseq ']' '=' expr  */
#line 760 "lang11d"
                                        {
						auto* args = linkNextNode((yyvsp[-7].node), allocNode<PyrSlotNode>((yyloc), PyrSlot{}, pn_PushLitNode));
						args = linkNextNode(args, allocNode<PyrSlotNode>((yyloc), PyrSlot{}, pn_PushLitNode));
						args = linkNextNode(args, (yyvsp[-3].node));
						args = linkNextNode(args, (yyvsp[0].node));
						auto* selectornode = allocNode<PyrSlotNode>((yyloc), PyrSlot::make(s_putseries));
						(yyval.node) = allocNode<PyrCallNode>((yyloc), selectornode, args, nullptr);
					}
#line 3175 "lang11d_tab.cpp"
    break;

  case 116: /* valrangexd: expr '.' '[' arglist1 DOTDOT exprseq ']' '=' expr  */
#line 769 "lang11d"
                                        {
						const int arglen = nodeListLength((yyvsp[-5].node));
						if (arglen > 2) {
							error("ArrayedCollection subrange has too many arguments.\n");
							nodePostErrorLine((yyvsp[-5].node));
							compileErrors++;
						}

						auto* args = linkNextNode((yyvsp[-8].node), (yyvsp[-5].node));
						if (arglen < 2) 
							args = linkNextNode(args, allocNode<PyrSlotNode>((yyloc), PyrSlot{}, pn_PushLitNode));
						
						args = linkAllNodes(args, (yyvsp[-3].node), (yyvsp[0].node));
						auto* selectornode = allocNode<PyrSlotNode>((yyloc), PyrSlot::make(s_putseries));
						(yyval.node) = allocNode<PyrCallNode>((yyloc), selectornode, args, nullptr);
					}
#line 3196 "lang11d_tab.cpp"
    break;

  case 117: /* valrange2: exprseq DOTDOT  */
#line 787 "lang11d"
                                        {
						// if this is not used in a 'do' or list comprehension, then should return an error.
						auto* args = linkNextNode((yyvsp[-1].node), allocNode<PyrSlotNode>((yylsp[0]), PyrSlot{}, pn_PushLitNode));
						args = linkNextNode(args, allocNode<PyrSlotNode>((yylsp[0]), PyrSlot{}, pn_PushLitNode));
						auto* selectornode = allocNode<PyrSlotNode>((yyloc), PyrSlot::make(s_series));
						(yyval.node) = allocNode<PyrCallNode>((yyloc), selectornode, args, nullptr);
					}
#line 3208 "lang11d_tab.cpp"
    break;

  case 118: /* valrange2: DOTDOT exprseq  */
#line 795 "lang11d"
                                        {
						auto* args = linkAllNodes(
							allocNode<PyrSlotNode>((yylsp[-1]), PyrSlot::make<int>(0), pn_PushLitNode),
							allocNode<PyrSlotNode>((yylsp[-1]), PyrSlot{}, pn_PushLitNode),
							(yyvsp[0].node)
						);
						auto* selectornode = allocNode<PyrSlotNode>((yyloc), PyrSlot::make(s_series));
						(yyval.node) = allocNode<PyrCallNode>((yyloc), selectornode, args, nullptr);
					}
#line 3222 "lang11d_tab.cpp"
    break;

  case 119: /* valrange2: exprseq DOTDOT exprseq  */
#line 805 "lang11d"
                                        {
						auto* args = linkAllNodes((yyvsp[-2].node), allocNode<PyrSlotNode>((yyloc), PyrSlot{}, pn_PushLitNode), (yyvsp[0].node));
						auto* selectornode = allocNode<PyrSlotNode>((yyloc), PyrSlot::make(s_series));
						(yyval.node) = allocNode<PyrCallNode>((yyloc), selectornode, args, nullptr);
					}
#line 3232 "lang11d_tab.cpp"
    break;

  case 120: /* valrange2: exprseq ',' exprseq DOTDOT exprseq  */
#line 811 "lang11d"
                                        {
						auto* selectornode = allocNode<PyrSlotNode>((yyloc), PyrSlot::make(s_series));
						(yyval.node) = allocNode<PyrCallNode>((yyloc), selectornode, linkAllNodes((yyvsp[-4].node), (yyvsp[-2].node), (yyvsp[0].node)), nullptr);
					}
#line 3241 "lang11d_tab.cpp"
    break;

  case 121: /* valrange2: exprseq ',' exprseq DOTDOT  */
#line 816 "lang11d"
                                        {
						// if this is not used in a 'do' or list comprehension, then should return an error.
						auto* args = linkAllNodes((yyvsp[-3].node), (yyvsp[-1].node), allocNode<PyrSlotNode>((yyloc), PyrSlot{}, pn_PushLitNode));
						auto* selectornode = allocNode<PyrSlotNode>((yyloc), PyrSlot::make(s_series));
						(yyval.node) = allocNode<PyrCallNode>((yyloc), selectornode, args, nullptr);
					}
#line 3252 "lang11d_tab.cpp"
    break;

  case 122: /* valrange3: DOTDOT exprseq  */
#line 824 "lang11d"
                                        {
						auto* args = linkAllNodes(
							allocNode<PyrSlotNode>((yylsp[-1]), PyrSlot::make<int>(0), pn_PushLitNode),
							allocNode<PyrSlotNode>((yylsp[-1]), PyrSlot{}, pn_PushLitNode),
							(yyvsp[0].node)
						);
						auto* selectornode = allocNode<PyrSlotNode>((yyloc), PyrSlot::make(getsym("seriesIter")));
						(yyval.node) = allocNode<PyrCallNode>((yyloc), selectornode, args, nullptr);
					}
#line 3266 "lang11d_tab.cpp"
    break;

  case 123: /* valrange3: exprseq DOTDOT  */
#line 834 "lang11d"
                                        {
						auto* args = linkAllNodes(
							(yyvsp[-1].node),
							allocNode<PyrSlotNode>((yylsp[-1]), PyrSlot{}, pn_PushLitNode),
							allocNode<PyrSlotNode>((yylsp[-1]), PyrSlot{}, pn_PushLitNode)
						);
						auto* selectornode = allocNode<PyrSlotNode>((yyloc), PyrSlot::make(getsym("seriesIter")));
						(yyval.node) = allocNode<PyrCallNode>((yyloc), selectornode, args, nullptr);
					}
#line 3280 "lang11d_tab.cpp"
    break;

  case 124: /* valrange3: exprseq DOTDOT exprseq  */
#line 845 "lang11d"
                                        {
						auto* args = linkAllNodes(
							(yyvsp[-2].node),
							allocNode<PyrSlotNode>((yylsp[-1]), PyrSlot{}, pn_PushLitNode),
							(yyvsp[0].node)
						);
						auto* selectornode = allocNode<PyrSlotNode>((yyloc), PyrSlot::make(getsym("seriesIter")));
						(yyval.node) = allocNode<PyrCallNode>((yyloc), selectornode, args, nullptr);
					}
#line 3294 "lang11d_tab.cpp"
    break;

  case 125: /* valrange3: exprseq ',' exprseq DOTDOT  */
#line 855 "lang11d"
                                        {
						auto* args = linkAllNodes(
							(yyvsp[-3].node),
							(yyvsp[-1].node),
							allocNode<PyrSlotNode>((yylsp[-2]), PyrSlot{}, pn_PushLitNode)
						);
						auto* selectornode = allocNode<PyrSlotNode>((yyloc), PyrSlot::make(getsym("seriesIter")));
						(yyval.node) = allocNode<PyrCallNode>((yyloc), selectornode, args, nullptr);
					}
#line 3308 "lang11d_tab.cpp"
    break;

  case 126: /* valrange3: exprseq ',' exprseq DOTDOT exprseq  */
#line 865 "lang11d"
                                        {
						auto* selectornode = allocNode<PyrSlotNode>((yyloc), PyrSlot::make(getsym("seriesIter")));
						(yyval.node) = allocNode<PyrCallNode>((yyloc), selectornode, linkAllNodes((yyvsp[-4].node), (yyvsp[-2].node), (yyvsp[0].node)), nullptr);
					}
#line 3317 "lang11d_tab.cpp"
    break;

  case 130: /* expr: CLASSNAME  */
#line 873 "lang11d"
                                            { (yyval.node) = (yyvsp[0].slotNode)->changeLiteralType(pn_PushNameNode); }
#line 3323 "lang11d_tab.cpp"
    break;

  case 131: /* expr: expr '.' '[' arglist1 ']'  */
#line 875 "lang11d"
                                        {
						auto* selectornode = allocNode<PyrSlotNode>((yyloc), PyrSlot::make(s_at));
						(yyval.node) = allocNode<PyrCallNode>((yyloc), selectornode, linkNextNode((yyvsp[-4].node), (yyvsp[-1].node)), nullptr);
					}
#line 3332 "lang11d_tab.cpp"
    break;

  case 132: /* expr: '`' expr  */
#line 880 "lang11d"
                                        {
						auto* args = linkNextNode(
							allocNode<PyrSlotNode>((yylsp[-1]), PyrSlot::make(s_ref), pn_PushNameNode),
							(yyvsp[0].node)
						);
						auto* selectornode = allocNode<PyrSlotNode>((yyloc), PyrSlot::make(s_new));
						(yyval.node) = allocNode<PyrCallNode>((yyloc), selectornode, args, nullptr);
					}
#line 3345 "lang11d_tab.cpp"
    break;

  case 133: /* expr: expr binop2 adverb expr  */
#line 889 "lang11d"
                                        {
						(yyvsp[-3].node)->mNext = (yyvsp[0].node);
						(yyvsp[0].node)->mNext = (yyvsp[-1].node);
						(yyval.node) = allocNode<PyrBinopCallNode>((yyloc), (yyvsp[-2].slotNode), (yyvsp[-3].node));
					}
#line 3355 "lang11d_tab.cpp"
    break;

  case 134: /* expr: name '=' expr  */
#line 895 "lang11d"
                                        { (yyval.node) = allocNode<PyrAssignNode>((yyloc), (yyvsp[-2].slotNode), (yyvsp[0].node)); }
#line 3361 "lang11d_tab.cpp"
    break;

  case 135: /* expr: '~' name '=' expr  */
#line 897 "lang11d"
                                        {
						auto* args = linkNextNode((yyvsp[-2].slotNode)->changeLiteralType(pn_PushLitNode), (yyvsp[0].node));
						auto* selectornode = allocNode<PyrSlotNode>((yyloc), PyrSlot::make(s_envirPut));
						(yyval.node) = allocNode<PyrCallNode>((yyloc), selectornode, args, nullptr);
					}
#line 3371 "lang11d_tab.cpp"
    break;

  case 136: /* expr: expr '.' name '=' expr  */
#line 903 "lang11d"
                                        { (yyval.node) = allocNode<PyrSetterNode>((yyloc), (yyvsp[-2].slotNode), (yyvsp[-4].node), (yyvsp[0].node)); }
#line 3377 "lang11d_tab.cpp"
    break;

  case 137: /* expr: name '(' arglist1 optkeyarglist ')' '=' expr  */
#line 905 "lang11d"
                                        {
						if ((yyvsp[-3].node) != nullptr) {
							error("Setter method called with keyword arguments.\n");
							nodePostErrorLine((yyvsp[-3].node));
							compileErrors++;
						}
						(yyval.node) = allocNode<PyrSetterNode>((yyloc), (yyvsp[-6].slotNode), (yyvsp[-4].node), (yyvsp[0].node));
					}
#line 3390 "lang11d_tab.cpp"
    break;

  case 138: /* expr: '#' mavars '=' expr  */
#line 914 "lang11d"
                                        { (yyval.node) = allocNode<PyrMultiAssignNode>((yyloc), (yyvsp[-2].multiAssignListNode), (yyvsp[0].node)); }
#line 3396 "lang11d_tab.cpp"
    break;

  case 139: /* expr: expr1 '[' arglist1 ']' '=' expr  */
#line 916 "lang11d"
                                        {
						auto* selectornode = allocNode<PyrSlotNode>((yyloc), PyrSlot::make(s_put));
						(yyval.node) = allocNode<PyrCallNode>((yyloc), selectornode, linkAllNodes((yyvsp[-5].node), (yyvsp[-3].node), (yyvsp[0].node)), nullptr);
					}
#line 3405 "lang11d_tab.cpp"
    break;

  case 140: /* expr: expr '.' '[' arglist1 ']' '=' expr  */
#line 921 "lang11d"
                                        {
						auto* selectornode = allocNode<PyrSlotNode>((yyloc), PyrSlot::make(s_put));
						(yyval.node) = allocNode<PyrCallNode>((yyloc), selectornode, linkAllNodes((yyvsp[-6].node), (yyvsp[-3].node), (yyvsp[0].node)), nullptr);
					}
#line 3414 "lang11d_tab.cpp"
    break;

  case 141: /* adverb: %empty  */
#line 926 "lang11d"
                                  { (yyval.node) = nullptr; }
#line 3420 "lang11d_tab.cpp"
    break;

  case 142: /* adverb: '.' name  */
#line 927 "lang11d"
                                           { (yyval.node) = (yyvsp[0].slotNode)->changeLiteralType(pn_PushLitNode); }
#line 3426 "lang11d_tab.cpp"
    break;

  case 143: /* adverb: '.' integer  */
#line 928 "lang11d"
                                              { (yyval.node) = (yyvsp[0].slotNode)->changeLiteralType(pn_PushLitNode); }
#line 3432 "lang11d_tab.cpp"
    break;

  case 144: /* adverb: '.' '(' exprseq ')'  */
#line 929 "lang11d"
                                                      { (yyval.node) = (yyvsp[-1].node); }
#line 3438 "lang11d_tab.cpp"
    break;

  case 146: /* exprn: exprn ';' expr  */
#line 933 "lang11d"
                                        { (yyval.node) = allocNode<PyrDropNode>((yyloc), (yyvsp[-2].node), (yyvsp[0].node)); }
#line 3444 "lang11d_tab.cpp"
    break;

  case 148: /* arrayelems: %empty  */
#line 937 "lang11d"
                                 { (yyval.node) = nullptr; }
#line 3450 "lang11d_tab.cpp"
    break;

  case 149: /* arrayelems: arrayelems1 optcomma  */
#line 938 "lang11d"
                                                       { (yyval.node) = (yyvsp[-1].node); }
#line 3456 "lang11d_tab.cpp"
    break;

  case 151: /* arrayelems1: exprseq ':' exprseq  */
#line 942 "lang11d"
                                        { (yyval.node) = linkNextNode((yyvsp[-2].node), (yyvsp[0].node)); }
#line 3462 "lang11d_tab.cpp"
    break;

  case 152: /* arrayelems1: KEYBINOP exprseq  */
#line 944 "lang11d"
                                        { (yyval.node) = linkNextNode( (yyvsp[-1].slotNode)->changeLiteralType(pn_PushLitNode), (yyvsp[0].node)); }
#line 3468 "lang11d_tab.cpp"
    break;

  case 153: /* arrayelems1: arrayelems1 ',' exprseq  */
#line 946 "lang11d"
                                                { (yyval.node) = linkNextNode((yyvsp[-2].node), (yyvsp[0].node)); }
#line 3474 "lang11d_tab.cpp"
    break;

  case 154: /* arrayelems1: arrayelems1 ',' KEYBINOP exprseq  */
#line 948 "lang11d"
                                        { (yyval.node) = linkAllNodes((yyvsp[-3].node), (yyvsp[-1].slotNode)->changeLiteralType(pn_PushLitNode), (yyvsp[0].node)); }
#line 3480 "lang11d_tab.cpp"
    break;

  case 155: /* arrayelems1: arrayelems1 ',' exprseq ':' exprseq  */
#line 950 "lang11d"
                                        { (yyval.node) = linkAllNodes((yyvsp[-4].node), (yyvsp[-2].node), (yyvsp[0].node)); }
#line 3486 "lang11d_tab.cpp"
    break;

  case 157: /* arglist1: arglist1 ',' exprseq  */
#line 954 "lang11d"
                                        { (yyval.node) = linkNextNode((yyvsp[-2].node), (yyvsp[0].node)); }
#line 3492 "lang11d_tab.cpp"
    break;

  case 158: /* arglistv1: '*' exprseq  */
#line 957 "lang11d"
                                        { (yylsp[0]) = (yyloc); (yyval.node) = (yyvsp[0].node); }
#line 3498 "lang11d_tab.cpp"
    break;

  case 159: /* arglistv1: arglist1 ',' '*' exprseq  */
#line 959 "lang11d"
                                                { (yyval.node) = linkNextNode((yyvsp[-3].node), (yyvsp[0].node)); }
#line 3504 "lang11d_tab.cpp"
    break;

  case 161: /* keyarglist1: keyarglist1 ',' keyarg  */
#line 963 "lang11d"
                                                { (yyval.node) = linkNextNode((yyvsp[-2].node), (yyvsp[0].node)); }
#line 3510 "lang11d_tab.cpp"
    break;

  case 162: /* keyarg: KEYBINOP exprseq  */
#line 966 "lang11d"
                                        { (yyval.node) = allocNode<PyrPushKeyArgNode>((yyloc), (yyvsp[-1].slotNode), (yyvsp[0].node)); }
#line 3516 "lang11d_tab.cpp"
    break;

  case 163: /* optkeyarglist: optcomma  */
#line 968 "lang11d"
                           { (yyval.node) = nullptr; }
#line 3522 "lang11d_tab.cpp"
    break;

  case 164: /* optkeyarglist: ',' keyarglist1 optcomma  */
#line 969 "lang11d"
                                                           { (yyval.node) = (yyvsp[-1].node); }
#line 3528 "lang11d_tab.cpp"
    break;

  case 165: /* mavars: nameList  */
#line 972 "lang11d"
                                        { (yyval.multiAssignListNode) = allocNode<PyrMultiAssignVarListNode>((yyloc), (yyvsp[0].slotNode), nullptr); }
#line 3534 "lang11d_tab.cpp"
    break;

  case 166: /* mavars: nameList ELLIPSIS name  */
#line 974 "lang11d"
                                        { (yyval.multiAssignListNode) = allocNode<PyrMultiAssignVarListNode>((yyloc), (yyvsp[-2].slotNode), (yyvsp[0].slotNode)); }
#line 3540 "lang11d_tab.cpp"
    break;

  case 168: /* nameList: nameList ',' name  */
#line 978 "lang11d"
                                        { (yyval.slotNode) = linkNextNode((yyvsp[-2].slotNode), (yyvsp[0].slotNode)); }
#line 3546 "lang11d_tab.cpp"
    break;

  case 169: /* slotliteral: integer  */
#line 980 "lang11d"
                                { (yyval.node) = (yyvsp[0].slotNode)->changeLiteralType(pn_LiteralNode); }
#line 3552 "lang11d_tab.cpp"
    break;

  case 170: /* slotliteral: floatp  */
#line 981 "lang11d"
                                                { (yyval.node) = (yyvsp[0].slotNode)->changeLiteralType(pn_LiteralNode); }
#line 3558 "lang11d_tab.cpp"
    break;

  case 171: /* slotliteral: ASCII  */
#line 982 "lang11d"
                                                { (yyval.node) = (yyvsp[0].slotNode)->changeLiteralType(pn_LiteralNode); }
#line 3564 "lang11d_tab.cpp"
    break;

  case 172: /* slotliteral: STRING  */
#line 983 "lang11d"
                                                { (yyval.node) = (yyvsp[0].slotNode)->changeLiteralType(pn_LiteralNode); }
#line 3570 "lang11d_tab.cpp"
    break;

  case 173: /* slotliteral: SYMBOL  */
#line 984 "lang11d"
                                                { (yyval.node) = (yyvsp[0].slotNode)->changeLiteralType(pn_LiteralNode); }
#line 3576 "lang11d_tab.cpp"
    break;

  case 174: /* slotliteral: TRUEOBJ  */
#line 985 "lang11d"
                                                { (yyval.node) = (yyvsp[0].slotNode)->changeLiteralType(pn_LiteralNode); }
#line 3582 "lang11d_tab.cpp"
    break;

  case 175: /* slotliteral: FALSEOBJ  */
#line 986 "lang11d"
                                                { (yyval.node) = (yyvsp[0].slotNode)->changeLiteralType(pn_LiteralNode); }
#line 3588 "lang11d_tab.cpp"
    break;

  case 176: /* slotliteral: NILOBJ  */
#line 987 "lang11d"
                                                { (yyval.node) = (yyvsp[0].slotNode)->changeLiteralType(pn_LiteralNode); }
#line 3594 "lang11d_tab.cpp"
    break;

  case 177: /* slotliteral: listlit  */
#line 988 "lang11d"
                                                { (yyval.node) = allocNode<PyrSlotNode>((yyloc), PyrSlot::make<void*>((yyvsp[0].node)), pn_LiteralNode); }
#line 3600 "lang11d_tab.cpp"
    break;

  case 178: /* blockliteral: block  */
#line 990 "lang11d"
                        { (yyval.node) = allocNode<PyrSlotNode>((yyloc), PyrSlot::make<void*>((yyvsp[0].node)), pn_PushLitNode); }
#line 3606 "lang11d_tab.cpp"
    break;

  case 179: /* pushname: name  */
#line 992 "lang11d"
                               { (yyval.node) = (yyvsp[0].slotNode)->changeLiteralType(pn_PushNameNode); }
#line 3612 "lang11d_tab.cpp"
    break;

  case 180: /* pushliteral: integer  */
#line 994 "lang11d"
                                        { (yyval.node) = (yyvsp[0].slotNode)->changeLiteralType(pn_PushLitNode); }
#line 3618 "lang11d_tab.cpp"
    break;

  case 181: /* pushliteral: floatp  */
#line 995 "lang11d"
                                                { (yyval.node) = (yyvsp[0].slotNode)->changeLiteralType(pn_PushLitNode); }
#line 3624 "lang11d_tab.cpp"
    break;

  case 182: /* pushliteral: ASCII  */
#line 996 "lang11d"
                                                { (yyval.node) = (yyvsp[0].slotNode)->changeLiteralType(pn_PushLitNode); }
#line 3630 "lang11d_tab.cpp"
    break;

  case 183: /* pushliteral: STRING  */
#line 997 "lang11d"
                                                { (yyval.node) = (yyvsp[0].slotNode)->changeLiteralType(pn_PushLitNode); }
#line 3636 "lang11d_tab.cpp"
    break;

  case 184: /* pushliteral: SYMBOL  */
#line 998 "lang11d"
                                                { (yyval.node) = (yyvsp[0].slotNode)->changeLiteralType(pn_PushLitNode); }
#line 3642 "lang11d_tab.cpp"
    break;

  case 185: /* pushliteral: TRUEOBJ  */
#line 999 "lang11d"
                                                { (yyval.node) = (yyvsp[0].slotNode)->changeLiteralType(pn_PushLitNode); }
#line 3648 "lang11d_tab.cpp"
    break;

  case 186: /* pushliteral: FALSEOBJ  */
#line 1000 "lang11d"
                                                { (yyval.node) = (yyvsp[0].slotNode)->changeLiteralType(pn_PushLitNode); }
#line 3654 "lang11d_tab.cpp"
    break;

  case 187: /* pushliteral: NILOBJ  */
#line 1001 "lang11d"
                                                { (yyval.node) = (yyvsp[0].slotNode)->changeLiteralType(pn_PushLitNode); }
#line 3660 "lang11d_tab.cpp"
    break;

  case 188: /* pushliteral: listlit  */
#line 1002 "lang11d"
                                                { (yyval.node) = allocNode<PyrSlotNode>((yyloc), PyrSlot::make<void*>((yyvsp[0].node)), pn_PushLitNode); }
#line 3666 "lang11d_tab.cpp"
    break;

  case 189: /* listliteral: integer  */
#line 1004 "lang11d"
                                        { (yyval.node) = (yyvsp[0].slotNode)->changeLiteralType(pn_LiteralNode);}
#line 3672 "lang11d_tab.cpp"
    break;

  case 190: /* listliteral: floatp  */
#line 1005 "lang11d"
                                                { (yyval.node) = (yyvsp[0].slotNode)->changeLiteralType(pn_LiteralNode); }
#line 3678 "lang11d_tab.cpp"
    break;

  case 191: /* listliteral: ASCII  */
#line 1006 "lang11d"
                                                { (yyval.node) = (yyvsp[0].slotNode)->changeLiteralType(pn_LiteralNode); }
#line 3684 "lang11d_tab.cpp"
    break;

  case 192: /* listliteral: STRING  */
#line 1007 "lang11d"
                                                { (yyval.node) = (yyvsp[0].slotNode)->changeLiteralType(pn_LiteralNode); }
#line 3690 "lang11d_tab.cpp"
    break;

  case 193: /* listliteral: SYMBOL  */
#line 1008 "lang11d"
                                                { (yyval.node) = (yyvsp[0].slotNode)->changeLiteralType(pn_LiteralNode); }
#line 3696 "lang11d_tab.cpp"
    break;

  case 194: /* listliteral: name  */
#line 1009 "lang11d"
                                                { (yyval.node) = (yyvsp[0].slotNode)->changeLiteralType(pn_LiteralNode); }
#line 3702 "lang11d_tab.cpp"
    break;

  case 195: /* listliteral: TRUEOBJ  */
#line 1010 "lang11d"
                                                { (yyval.node) = (yyvsp[0].slotNode)->changeLiteralType(pn_LiteralNode); }
#line 3708 "lang11d_tab.cpp"
    break;

  case 196: /* listliteral: FALSEOBJ  */
#line 1011 "lang11d"
                                                { (yyval.node) = (yyvsp[0].slotNode)->changeLiteralType(pn_LiteralNode); }
#line 3714 "lang11d_tab.cpp"
    break;

  case 197: /* listliteral: NILOBJ  */
#line 1012 "lang11d"
                                                { (yyval.node) = (yyvsp[0].slotNode)->changeLiteralType(pn_LiteralNode); }
#line 3720 "lang11d_tab.cpp"
    break;

  case 198: /* listliteral: listlit2  */
#line 1013 "lang11d"
                                                { (yyval.node) = allocNode<PyrSlotNode>((yyloc), PyrSlot::make<void*>((yyvsp[0].node)), pn_LiteralNode); }
#line 3726 "lang11d_tab.cpp"
    break;

  case 199: /* listliteral: dictlit2  */
#line 1014 "lang11d"
                                            { (yyval.node) = allocNode<PyrSlotNode>((yyloc), PyrSlot::make<void*>((yyvsp[0].node)), pn_LiteralNode); }
#line 3732 "lang11d_tab.cpp"
    break;

  case 200: /* block: '{' argdecls funcvardecls funcbody '}'  */
#line 1017 "lang11d"
                                        { (yyval.node) = allocNode<PyrBlockNode>((yyloc), (yyvsp[-3].argListNode), (yyvsp[-2].varListNode), (yyvsp[-1].node), false); }
#line 3738 "lang11d_tab.cpp"
    break;

  case 201: /* block: BEGINCLOSEDFUNC argdecls funcvardecls funcbody '}'  */
#line 1019 "lang11d"
                                        { (yyval.node) = allocNode<PyrBlockNode>((yyloc), (yyvsp[-3].argListNode), (yyvsp[-2].varListNode), (yyvsp[-1].node), true); }
#line 3744 "lang11d_tab.cpp"
    break;

  case 202: /* funcvardecls: %empty  */
#line 1022 "lang11d"
                         { (yyval.varListNode) = nullptr; }
#line 3750 "lang11d_tab.cpp"
    break;

  case 203: /* funcvardecls: funcvardecls funcvardecl  */
#line 1024 "lang11d"
                                        { (yyval.varListNode) = linkNextNode((yyvsp[-1].varListNode), (yyvsp[0].varListNode)); }
#line 3756 "lang11d_tab.cpp"
    break;

  case 205: /* funcvardecls1: funcvardecls1 funcvardecl  */
#line 1028 "lang11d"
                                        { (yyval.varListNode) = linkNextNode((yyvsp[-1].varListNode), (yyvsp[0].varListNode)); }
#line 3762 "lang11d_tab.cpp"
    break;

  case 206: /* funcvardecl: VAR vardeflist ';'  */
#line 1031 "lang11d"
                                        { (yyval.varListNode) = allocNode<PyrVarListNode>((yyloc), (yyvsp[-1].varDefNode), varLocal); }
#line 3768 "lang11d_tab.cpp"
    break;

  case 207: /* argdecls: %empty  */
#line 1033 "lang11d"
                                 { (yyval.argListNode) = nullptr; }
#line 3774 "lang11d_tab.cpp"
    break;

  case 208: /* argdecls: ARG vardeflist ';'  */
#line 1035 "lang11d"
                                        { (yyval.argListNode) = allocNode<PyrArgListNode>((yyloc), (yyvsp[-1].varDefNode), nullptr, nullptr); }
#line 3780 "lang11d_tab.cpp"
    break;

  case 209: /* argdecls: ARG vardeflist0 ELLIPSIS name ';'  */
#line 1037 "lang11d"
                                        { (yyval.argListNode) = allocNode<PyrArgListNode>((yyloc), (yyvsp[-3].varDefNode), (yyvsp[-1].slotNode), nullptr); }
#line 3786 "lang11d_tab.cpp"
    break;

  case 210: /* argdecls: ARG vardeflist0 ELLIPSIS name ',' name ';'  */
#line 1039 "lang11d"
                                        { (yyval.argListNode) = allocNode<PyrArgListNode>((yyloc), (yyvsp[-5].varDefNode), (yyvsp[-3].slotNode), (yyvsp[-1].slotNode)); }
#line 3792 "lang11d_tab.cpp"
    break;

  case 211: /* argdecls: '|' slotdeflist '|'  */
#line 1041 "lang11d"
                                        { (yyval.argListNode) = allocNode<PyrArgListNode>((yyloc), (yyvsp[-1].varDefNode), nullptr, nullptr); }
#line 3798 "lang11d_tab.cpp"
    break;

  case 212: /* argdecls: '|' slotdeflist0 ELLIPSIS name '|'  */
#line 1043 "lang11d"
                                        { (yyval.argListNode) = allocNode<PyrArgListNode>((yyloc), (yyvsp[-3].varDefNode), (yyvsp[-1].slotNode), nullptr); }
#line 3804 "lang11d_tab.cpp"
    break;

  case 213: /* argdecls: '|' slotdeflist0 ELLIPSIS name ',' name '|'  */
#line 1045 "lang11d"
                                    { (yyval.argListNode) = allocNode<PyrArgListNode>((yyloc), (yyvsp[-5].varDefNode), (yyvsp[-3].slotNode), (yyvsp[-1].slotNode)); }
#line 3810 "lang11d_tab.cpp"
    break;

  case 214: /* argdecls1: ARG vardeflist ';'  */
#line 1048 "lang11d"
                                        { (yyval.argListNode) = allocNode<PyrArgListNode>((yyloc), (yyvsp[-1].varDefNode), nullptr, nullptr); }
#line 3816 "lang11d_tab.cpp"
    break;

  case 215: /* argdecls1: ARG vardeflist0 ELLIPSIS name ';'  */
#line 1050 "lang11d"
                                        { (yyval.argListNode) = allocNode<PyrArgListNode>((yyloc), (yyvsp[-3].varDefNode), (yyvsp[-1].slotNode), nullptr); }
#line 3822 "lang11d_tab.cpp"
    break;

  case 216: /* argdecls1: ARG vardeflist0 ELLIPSIS name ',' name ';'  */
#line 1052 "lang11d"
                                        { (yyval.argListNode) = allocNode<PyrArgListNode>((yyloc), (yyvsp[-5].varDefNode), (yyvsp[-3].slotNode), (yyvsp[-1].slotNode)); }
#line 3828 "lang11d_tab.cpp"
    break;

  case 217: /* argdecls1: '|' slotdeflist '|'  */
#line 1054 "lang11d"
                                        { (yyval.argListNode) = allocNode<PyrArgListNode>((yyloc), (yyvsp[-1].varDefNode), nullptr, nullptr); }
#line 3834 "lang11d_tab.cpp"
    break;

  case 218: /* argdecls1: '|' slotdeflist0 ELLIPSIS name '|'  */
#line 1056 "lang11d"
                                        { (yyval.argListNode) = allocNode<PyrArgListNode>((yyloc), (yyvsp[-3].varDefNode), (yyvsp[-1].slotNode), nullptr); }
#line 3840 "lang11d_tab.cpp"
    break;

  case 219: /* argdecls1: '|' slotdeflist0 ELLIPSIS name ',' name '|'  */
#line 1058 "lang11d"
                                    { (yyval.argListNode) = allocNode<PyrArgListNode>((yyloc), (yyvsp[-5].varDefNode), (yyvsp[-3].slotNode), (yyvsp[-1].slotNode)); }
#line 3846 "lang11d_tab.cpp"
    break;

  case 221: /* constdeflist: constdeflist optcomma constdef  */
#line 1063 "lang11d"
                                        { (yyval.varDefNode) = linkNextNode((yyvsp[-2].varDefNode), (yyvsp[0].varDefNode)); }
#line 3852 "lang11d_tab.cpp"
    break;

  case 222: /* constdef: rspec name '=' slotliteral  */
#line 1066 "lang11d"
                                        { (yyval.varDefNode) = allocNode<PyrVarDefNode>((yyloc), (yyvsp[-2].slotNode), (yyvsp[0].node), (yyvsp[-3].rwAccessor)); }
#line 3858 "lang11d_tab.cpp"
    break;

  case 223: /* slotdeflist0: %empty  */
#line 1068 "lang11d"
                         { (yyval.varDefNode) = nullptr; }
#line 3864 "lang11d_tab.cpp"
    break;

  case 226: /* slotdeflist: slotdeflist optcomma slotdef  */
#line 1073 "lang11d"
                                        { (yyval.varDefNode) = linkNextNode((yyvsp[-2].varDefNode), (yyvsp[0].varDefNode)); }
#line 3870 "lang11d_tab.cpp"
    break;

  case 227: /* slotdef: name  */
#line 1076 "lang11d"
                                        { (yyval.varDefNode) = allocNode<PyrVarDefNode>((yyloc), (yyvsp[0].slotNode), nullptr, ReadWriteAccessor::Private); }
#line 3876 "lang11d_tab.cpp"
    break;

  case 228: /* slotdef: name optequal slotliteral  */
#line 1078 "lang11d"
                                        { (yyval.varDefNode) = allocNode<PyrVarDefNode>((yyloc), (yyvsp[-2].slotNode), (yyvsp[0].node), ReadWriteAccessor::Private); }
#line 3882 "lang11d_tab.cpp"
    break;

  case 229: /* slotdef: name optequal '(' exprseq ')'  */
#line 1080 "lang11d"
                                        {
						PyrParseNode* node = (yyvsp[-1].node);
						node->mParens = 1;
						(yyval.varDefNode) = allocNode<PyrVarDefNode>((yyloc), (yyvsp[-4].slotNode), node, ReadWriteAccessor::Private);
					}
#line 3892 "lang11d_tab.cpp"
    break;

  case 230: /* vardeflist0: %empty  */
#line 1086 "lang11d"
                          { (yyval.varDefNode) = nullptr; }
#line 3898 "lang11d_tab.cpp"
    break;

  case 233: /* vardeflist: vardeflist ',' vardef  */
#line 1091 "lang11d"
                                        { (yyval.varDefNode) = linkNextNode((yyvsp[-2].varDefNode), (yyvsp[0].varDefNode)); }
#line 3904 "lang11d_tab.cpp"
    break;

  case 234: /* vardef: name  */
#line 1094 "lang11d"
                                        { (yyval.varDefNode) = allocNode<PyrVarDefNode>((yyloc), (yyvsp[0].slotNode), nullptr, ReadWriteAccessor::Private); }
#line 3910 "lang11d_tab.cpp"
    break;

  case 235: /* vardef: name '=' expr  */
#line 1096 "lang11d"
                                        { (yyval.varDefNode) = allocNode<PyrVarDefNode>((yyloc), (yyvsp[-2].slotNode), (yyvsp[0].node), ReadWriteAccessor::Private); }
#line 3916 "lang11d_tab.cpp"
    break;

  case 236: /* vardef: name '(' exprseq ')'  */
#line 1098 "lang11d"
                                        {
						PyrParseNode* node = (yyvsp[-1].node);
						node->mParens = 1;
						(yyval.varDefNode) = allocNode<PyrVarDefNode>((yyloc), (yyvsp[-3].slotNode), node, ReadWriteAccessor::Private);
					}
#line 3926 "lang11d_tab.cpp"
    break;

  case 237: /* dictslotdef: exprseq ':' exprseq  */
#line 1105 "lang11d"
                                        { (yyval.node) = linkNextNode((yyvsp[-2].node), (yyvsp[0].node)); }
#line 3932 "lang11d_tab.cpp"
    break;

  case 238: /* dictslotdef: KEYBINOP exprseq  */
#line 1107 "lang11d"
                                        { (yyval.node) = linkNextNode((yyvsp[-1].slotNode)->changeLiteralType(pn_PushLitNode), (yyvsp[0].node)); }
#line 3938 "lang11d_tab.cpp"
    break;

  case 240: /* dictslotlist1: dictslotlist1 ',' dictslotdef  */
#line 1112 "lang11d"
                                        { (yyval.node) = linkNextNode((yyvsp[-2].node), (yyvsp[0].node)); }
#line 3944 "lang11d_tab.cpp"
    break;

  case 241: /* dictslotlist: %empty  */
#line 1114 "lang11d"
                         { (yyval.node) = nullptr; }
#line 3950 "lang11d_tab.cpp"
    break;

  case 244: /* rwslotdeflist: rwslotdeflist ',' rwslotdef  */
#line 1119 "lang11d"
                                        { (yyval.varDefNode) = linkNextNode((yyvsp[-2].varDefNode), (yyvsp[0].varDefNode)); }
#line 3956 "lang11d_tab.cpp"
    break;

  case 245: /* rwslotdef: rwspec name  */
#line 1122 "lang11d"
                                        { (yyval.varDefNode) = allocNode<PyrVarDefNode>((yyloc), (yyvsp[0].slotNode), nullptr, (yyvsp[-1].rwAccessor)); }
#line 3962 "lang11d_tab.cpp"
    break;

  case 246: /* rwslotdef: rwspec name '=' slotliteral  */
#line 1124 "lang11d"
                                        { (yyval.varDefNode) = allocNode<PyrVarDefNode>((yyloc), (yyvsp[-2].slotNode), (yyvsp[0].node), (yyvsp[-3].rwAccessor)); }
#line 3968 "lang11d_tab.cpp"
    break;

  case 247: /* dictlit2: '(' litdictslotlist ')'  */
#line 1127 "lang11d"
                                        { (yyval.node) = allocNode<PyrLitDictNode>((yyloc), (yyvsp[-1].node)); }
#line 3974 "lang11d_tab.cpp"
    break;

  case 248: /* litdictslotdef: listliteral ':' listliteral  */
#line 1130 "lang11d"
                                        { (yyval.node) = linkNextNode((yyvsp[-2].node), (yyvsp[0].node)); }
#line 3980 "lang11d_tab.cpp"
    break;

  case 249: /* litdictslotdef: KEYBINOP listliteral  */
#line 1132 "lang11d"
                                        { (yyval.node) = linkNextNode((yyvsp[-1].slotNode)->changeLiteralType(pn_PushLitNode), (yyvsp[0].node)); }
#line 3986 "lang11d_tab.cpp"
    break;

  case 251: /* litdictslotlist1: litdictslotlist1 ',' litdictslotdef  */
#line 1136 "lang11d"
                                        { (yyval.node) = linkNextNode((yyvsp[-2].node), (yyvsp[0].node)); }
#line 3992 "lang11d_tab.cpp"
    break;

  case 252: /* litdictslotlist: %empty  */
#line 1138 "lang11d"
                         { (yyval.node) = nullptr; }
#line 3998 "lang11d_tab.cpp"
    break;

  case 254: /* listlit: '#' '[' literallistc ']'  */
#line 1144 "lang11d"
                                        { (yyval.node) = allocNode<PyrLitListNode>((yyloc), nullptr, (yyvsp[-1].node)); }
#line 4004 "lang11d_tab.cpp"
    break;

  case 255: /* listlit: '#' CLASSNAME '[' literallistc ']'  */
#line 1146 "lang11d"
                                                { (yyval.node) = allocNode<PyrLitListNode>((yyloc), (yyvsp[-3].slotNode), (yyvsp[-1].node)); }
#line 4010 "lang11d_tab.cpp"
    break;

  case 256: /* listlit2: '[' literallistc ']'  */
#line 1149 "lang11d"
                                        { (yyval.node) = allocNode<PyrLitListNode>((yyloc), nullptr, (yyvsp[-1].node)); }
#line 4016 "lang11d_tab.cpp"
    break;

  case 257: /* listlit2: CLASSNAME '[' literallistc ']'  */
#line 1151 "lang11d"
                                        { (yyval.node) = allocNode<PyrLitListNode>((yyloc), (yyvsp[-3].slotNode), (yyvsp[-1].node)); }
#line 4022 "lang11d_tab.cpp"
    break;

  case 258: /* literallistc: %empty  */
#line 1153 "lang11d"
                         { (yyval.node) = nullptr; }
#line 4028 "lang11d_tab.cpp"
    break;

  case 261: /* literallist1: literallist1 ',' listliteral  */
#line 1158 "lang11d"
                                        { (yyval.node) = linkNextNode((yyvsp[-2].node), (yyvsp[0].node)); }
#line 4034 "lang11d_tab.cpp"
    break;

  case 262: /* rwspec: %empty  */
#line 1160 "lang11d"
                                  { (yyval.rwAccessor) = ReadWriteAccessor::Private; }
#line 4040 "lang11d_tab.cpp"
    break;

  case 263: /* rwspec: '<'  */
#line 1161 "lang11d"
                                      { (yyval.rwAccessor) = ReadWriteAccessor::Read; }
#line 4046 "lang11d_tab.cpp"
    break;

  case 264: /* rwspec: READWRITEVAR  */
#line 1162 "lang11d"
                                               { (yyval.rwAccessor) = ReadWriteAccessor::ReadWrite; }
#line 4052 "lang11d_tab.cpp"
    break;

  case 265: /* rwspec: '>'  */
#line 1163 "lang11d"
                                      { (yyval.rwAccessor) = ReadWriteAccessor::Write; }
#line 4058 "lang11d_tab.cpp"
    break;

  case 266: /* rspec: %empty  */
#line 1165 "lang11d"
                                 { (yyval.rwAccessor) = ReadWriteAccessor::Private; }
#line 4064 "lang11d_tab.cpp"
    break;

  case 267: /* rspec: '<'  */
#line 1166 "lang11d"
                                      { (yyval.rwAccessor) = ReadWriteAccessor::Read; }
#line 4070 "lang11d_tab.cpp"
    break;

  case 269: /* integer: '-' INTEGER  */
#line 1170 "lang11d"
                                        {
						const auto v = (yyvsp[0].slotNode)->mSlot.getInt();
						(yyvsp[0].slotNode)->mSlot = PyrSlot::make(-v);
						(yyval.slotNode) = (yyvsp[0].slotNode);
					}
#line 4080 "lang11d_tab.cpp"
    break;

  case 271: /* floatr: '-' SC_FLOAT  */
#line 1178 "lang11d"
                                        {
						const double v = (yyvsp[0].slotNode)->mSlot.getDouble();
						(yyvsp[0].slotNode)->mSlot = PyrSlot::make(-v);
						(yyval.slotNode) = (yyvsp[0].slotNode);
					}
#line 4090 "lang11d_tab.cpp"
    break;

  case 273: /* accidental: '-' ACCIDENTAL  */
#line 1186 "lang11d"
                                        {
						const double in = (yyvsp[0].slotNode)->mSlot.getDouble();
						const double intval = floor(in + 0.5);
						const double fracval = in - intval;
						(yyvsp[0].slotNode)->mSlot = PyrSlot::make(-intval + fracval);
						(yyval.slotNode) = (yyvsp[0].slotNode);
					}
#line 4102 "lang11d_tab.cpp"
    break;

  case 276: /* floatp: floatr PIE  */
#line 1198 "lang11d"
                                        {
						(yyvsp[-1].slotNode)->mSlot = PyrSlot::make((yyvsp[-1].slotNode)->mSlot.getDouble() * pi);
						(yyvsp[-1].slotNode)->mLocation = (yyloc);
						(yyval.slotNode) = (yyvsp[-1].slotNode);
					}
#line 4112 "lang11d_tab.cpp"
    break;

  case 277: /* floatp: integer PIE  */
#line 1204 "lang11d"
                                        {
						(yyvsp[-1].slotNode)->mSlot = PyrSlot::make((yyvsp[-1].slotNode)->mSlot.getInt() * pi);
						(yyvsp[-1].slotNode)->mLocation = (yyloc);
						(yyval.slotNode) = (yyvsp[-1].slotNode);
					}
#line 4122 "lang11d_tab.cpp"
    break;

  case 279: /* floatp: '-' PIE  */
#line 1211 "lang11d"
                                        {
						(yyvsp[0].slotNode)->mSlot = PyrSlot::make(-pi);
						(yyvsp[0].slotNode)->mLocation = (yyloc);
						(yyval.slotNode) = (yyvsp[0].slotNode);
					}
#line 4132 "lang11d_tab.cpp"
    break;

  case 292: /* curryArg: CURRYARG  */
#line 1220 "lang11d"
                                   { (yyval.node) = allocNode<PyrCurryArgNode>((yyloc)); }
#line 4138 "lang11d_tab.cpp"
    break;


#line 4142 "lang11d_tab.cpp"

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
  *++yylsp = yyloc;

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
          = {yyssp, yytoken, &yylloc};
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

  yyerror_range[1] = yylloc;
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
                      yytoken, &yylval, &yylloc);
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

      yyerror_range[1] = *yylsp;
      yydestruct ("Error: popping",
                  YY_ACCESSING_SYMBOL (yystate), yyvsp, yylsp);
      YYPOPSTACK (1);
      yystate = *yyssp;
      YY_STACK_PRINT (yyss, yyssp);
    }

  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  *++yyvsp = yylval;
  YY_IGNORE_MAYBE_UNINITIALIZED_END

  yyerror_range[2] = yylloc;
  ++yylsp;
  YYLLOC_DEFAULT (*yylsp, yyerror_range, 2);

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
                  yytoken, &yylval, &yylloc);
    }
  /* Do not reclaim the symbols of the rule whose action triggered
     this YYABORT or YYACCEPT.  */
  YYPOPSTACK (yylen);
  YY_STACK_PRINT (yyss, yyssp);
  while (yyssp != yyss)
    {
      yydestruct ("Cleanup: popping",
                  YY_ACCESSING_SYMBOL (+*yyssp), yyvsp, yylsp);
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

