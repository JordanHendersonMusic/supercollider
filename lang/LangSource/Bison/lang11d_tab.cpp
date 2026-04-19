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
#line 64 "lang11d"


// Preamble for the generated source file.

#include <stdlib.h>
#include <string.h>
#include "bisonHeaderInclude.hpp"
#include "PyrLexer.h"
#include "PyrParseNode.h"
#include "SC_Constants.h"
#include "SC_InlineUnaryOp.h"
#include "SC_InlineBinaryOp.h"
#include "InitAlloc.h"
#include "PredefinedSymbols.h"
#include "SimpleStack.h"

int yyparse();
extern LongStack generatorStack;


#line 92 "lang11d_tab.cpp"

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
  YYSYMBOL_listlit = 128,                  /* listlit  */
  YYSYMBOL_listlit2 = 129,                 /* listlit2  */
  YYSYMBOL_literallistc = 130,             /* literallistc  */
  YYSYMBOL_literallist1 = 131,             /* literallist1  */
  YYSYMBOL_rwspec = 132,                   /* rwspec  */
  YYSYMBOL_rspec = 133,                    /* rspec  */
  YYSYMBOL_integer = 134,                  /* integer  */
  YYSYMBOL_floatr = 135,                   /* floatr  */
  YYSYMBOL_accidental = 136,               /* accidental  */
  YYSYMBOL_floatp = 137,                   /* floatp  */
  YYSYMBOL_name = 138,                     /* name  */
  YYSYMBOL_binop = 139,                    /* binop  */
  YYSYMBOL_binop2 = 140,                   /* binop2  */
  YYSYMBOL_curryArg = 141                  /* curryArg  */
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
#define YYLAST   1762

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  55
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  87
/* YYNRULES -- Number of rules.  */
#define YYNRULES  284
/* YYNSTATES -- Number of states.  */
#define YYNSTATES  545

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
       0,   157,   157,   161,   165,   170,   171,   174,   175,   178,
     180,   183,   186,   186,   189,   189,   191,   192,   195,   197,
     199,   202,   203,   206,   208,   210,   212,   215,   215,   216,
     216,   217,   217,   219,   220,   223,   225,   227,   229,   231,
     233,   235,   238,   239,   242,   243,   245,   246,   249,   251,
     254,   255,   258,   258,   260,   260,   262,   265,   268,   271,
     274,   277,   280,   295,   301,   304,   310,   316,   322,   331,
     351,   356,   361,   363,   368,   384,   386,   388,   401,   410,
     410,   417,   417,   420,   431,   433,   450,   470,   480,   487,
     489,   503,   504,   505,   506,   507,   508,   509,   516,   522,
     524,   526,   528,   530,   536,   538,   553,   563,   580,   597,
     609,   632,   653,   664,   680,   696,   705,   722,   730,   740,
     746,   751,   759,   769,   780,   790,   800,   806,   807,   808,
     809,   811,   816,   825,   831,   833,   839,   841,   849,   851,
     856,   862,   863,   864,   865,   867,   868,   871,   873,   874,
     876,   877,   879,   881,   883,   885,   888,   889,   892,   894,
     897,   898,   901,   904,   905,   907,   909,   912,   913,   916,
     917,   918,   919,   920,   921,   922,   923,   924,   926,   928,
     930,   931,   932,   933,   934,   935,   936,   937,   938,   940,
     941,   942,   943,   944,   945,   946,   947,   948,   949,   951,
     953,   957,   958,   961,   962,   965,   968,   969,   971,   973,
     975,   977,   979,   982,   984,   986,   988,   990,   992,   995,
     996,   999,  1002,  1003,  1005,  1006,  1009,  1011,  1013,  1020,
    1021,  1023,  1024,  1027,  1029,  1031,  1038,  1040,  1044,  1045,
    1048,  1049,  1051,  1052,  1055,  1057,  1060,  1062,  1065,  1067,
    1070,  1071,  1073,  1074,  1077,  1078,  1079,  1080,  1082,  1083,
    1085,  1086,  1093,  1094,  1101,  1102,  1111,  1112,  1113,  1119,
    1125,  1126,  1133,  1133,  1134,  1134,  1134,  1134,  1134,  1134,
    1134,  1134,  1135,  1135,  1136
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
  "rwslotdef", "listlit", "listlit2", "literallistc", "literallist1",
  "rwspec", "rspec", "integer", "floatr", "accidental", "floatp", "name",
  "binop", "binop2", "curryArg", YY_NULLPTR
};

static const char *
yysymbol_name (yysymbol_kind_t yysymbol)
{
  return yytname[yysymbol];
}
#endif

#define YYPACT_NINF (-456)

#define yypact_value_is_default(Yyn) \
  ((Yyn) == YYPACT_NINF)

#define YYTABLE_NINF (-282)

#define yytable_value_is_error(Yyn) \
  0

/* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
   STATE-NUM.  */
static const yytype_int16 yypact[] =
{
      96,   698,    83,    31,    86,    60,  -456,  -456,  -456,  -456,
    -456,  -456,  -456,  -456,   191,  -456,  -456,  -456,  -456,  -456,
     239,   239,    64,  -456,   248,   239,  1634,     9,  1166,   646,
    1634,   239,   114,  -456,  -456,  -456,  -456,  -456,   105,  -456,
    -456,  -456,  1721,    58,    80,  -456,  -456,  -456,  -456,   906,
    -456,   906,  -456,   163,   171,  -456,  -456,   125,  -456,   181,
    -456,   176,  -456,  -456,  1166,   750,   -12,  -456,  -456,  -456,
     175,  -456,    57,   196,   217,   239,   239,  -456,  -456,  -456,
    -456,  -456,   220,     5,  -456,   115,   551,  -456,  1634,  1634,
    -456,  1634,   244,   211,   227,  1634,  1218,  -456,  1634,   248,
    -456,  -456,  -456,  -456,    18,  -456,   218,    98,   906,   906,
    -456,   241,   243,  -456,   245,  1704,   261,   264,  1701,   273,
       2,  -456,  1270,  -456,  -456,  -456,    35,   271,  1634,  -456,
    -456,  -456,  -456,  -456,   906,  -456,  -456,  1634,   958,   -12,
    -456,   306,   239,   276,   274,  1634,  1634,   -12,  -456,   275,
     278,   279,  -456,  -456,  -456,   239,  1634,  1634,   239,  -456,
     300,   229,   302,    51,   906,   239,  -456,  -456,   239,  -456,
    1649,  -456,  -456,   906,  -456,  1634,  -456,  1322,  -456,  -456,
    1634,   280,    70,  -456,  -456,  1634,  1634,  1634,  -456,   282,
     284,   906,  1374,  -456,  -456,    91,  -456,  -456,  1634,  1701,
    -456,  -456,  -456,   290,  -456,  -456,  -456,  1701,  -456,  -456,
     283,   294,   163,  -456,  -456,  1634,   239,   239,  1634,    52,
    1426,   802,   155,    33,  1634,  1721,  -456,  1721,   -12,   275,
     278,   437,  -456,   297,  -456,  -456,  -456,  -456,  -456,   -12,
    -456,  1010,  -456,   295,   314,   304,   314,   305,  -456,  1721,
     307,   240,   239,  -456,   239,  -456,   308,  -456,   106,  -456,
    -456,  -456,  -456,  -456,  -456,  -456,  1634,     7,  -456,  -456,
     163,  -456,   321,   322,   312,  -456,  1634,   343,  -456,  -456,
    1634,  1634,  -456,  -456,   327,  -456,  -456,   323,  1634,   346,
    -456,  1062,   -12,  1721,   331,  1701,   333,  -456,  1701,  -456,
    1721,  -456,  -456,   338,  1478,   358,  1634,  1634,   161,   -12,
     275,   278,   279,  1634,   854,  -456,   389,  1634,  -456,  -456,
     355,   -12,   351,   352,   604,  -456,  -456,   361,   364,   385,
     226,  1634,  -456,   279,   -12,  -456,  -456,   -12,  -456,  -456,
     239,   254,   124,  -456,  -456,   239,   366,  1114,  1114,  -456,
    -456,  1634,  -456,   396,  1634,  -456,   -12,   275,   278,  -456,
     375,  -456,  -456,   392,   398,   380,  1634,   384,  1530,   401,
    -456,   382,   383,   387,  1721,   -12,   275,   278,   279,   390,
      21,  -456,   391,   407,    64,    64,   409,   228,   228,   419,
    -456,   496,  -456,  -456,  -456,  -456,   388,  -456,   239,  -456,
     239,   416,  -456,   239,   128,   413,   410,   363,   415,  -456,
    1634,  -456,   -12,   412,   414,  -456,  1634,  1634,   430,  1721,
     433,   435,   420,  1634,   -12,  -456,   -12,  -456,   429,   436,
     438,  -456,  1634,  -456,    64,    64,  -456,  -456,  -456,  -456,
    -456,  -456,   258,  -456,   239,   266,  -456,   270,  -456,   239,
    -456,  -456,   444,   447,  -456,   462,  1634,  1634,  -456,  1114,
    -456,  1634,   480,  -456,  -456,   -12,  -456,  1721,  1721,  1634,
    1634,  1634,   464,  1721,  -456,  -456,   -12,  -456,   -12,  1721,
    -456,  -456,    14,    14,   226,  -456,   228,   465,  -456,  -456,
     419,   472,  -456,  -456,  1634,   410,   410,  -456,   410,  1634,
    -456,  1721,  1721,  1721,  1634,  -456,  -456,    14,    14,   457,
    1582,  1582,  1688,  -456,   613,  -456,   613,   410,  -456,  -456,
    -456,   410,  1721,  1582,  1582,  -456,  1634,   461,  -456,   458,
     466,  -456,  -456,  -456,  -456,  -456,   467,   468,  1704,  -456,
    -456,  -456,  -456,  -456,  -456
};

/* YYDEFACT[STATE-NUM] -- Default reduction number in state STATE-NUM.
   Performed when YYTABLE does not specify something else to do.  Zero
   means the default is an error.  */
static const yytype_int16 yydefact[] =
{
       5,    48,     0,     0,     2,     3,     7,   272,   260,   262,
     264,   184,   183,   182,   130,   270,   273,   185,   186,   187,
       0,   229,   206,   284,     0,   222,     0,   206,   148,   240,
       0,     0,     0,    41,     4,    33,    96,    93,   127,   104,
     129,   128,   145,    27,    48,    92,    94,    91,   178,    48,
     203,    48,   188,   180,   266,   267,   181,   179,    95,     0,
       1,    14,     6,     8,   148,     0,    65,    50,    53,    52,
       0,   231,   233,     0,   230,   229,   222,   201,   261,   263,
     265,   271,     0,    29,   224,    31,   240,   132,     0,     0,
     201,     0,   150,     0,    29,     0,     0,   274,   283,   278,
     276,   277,   279,   280,   222,   275,     0,     0,    48,    48,
     238,    29,     0,   282,     0,    27,    98,     0,   250,     0,
     165,   167,     0,   283,   278,   281,     0,   141,    28,   147,
      34,    40,   204,    39,    48,   269,   268,     0,     0,    56,
      21,     0,    12,     0,     0,     0,     0,    54,   156,    29,
      29,    29,   160,    51,   205,     0,     0,     0,     0,   213,
       0,   230,     0,    29,    48,     0,   216,    30,     0,    32,
       0,    79,    81,    48,   152,     0,    99,    30,   149,   118,
       0,     0,     0,   237,   100,   117,     0,     0,    97,     0,
       0,    48,    30,   241,   102,     0,    28,    49,     0,   250,
     193,   192,   191,     0,   195,   196,   197,   250,   252,   198,
       0,    29,   189,   190,   194,     0,     0,     0,     0,     0,
       0,     0,    54,     0,     0,   146,    38,   134,     0,    29,
      29,     0,    15,     0,    13,    16,    64,   162,   158,    55,
      66,    30,   163,     0,    30,     0,    30,     0,   232,   234,
       0,     0,     0,   207,     0,   210,     0,   202,     0,   225,
     173,   172,   171,   174,   175,   176,     0,     0,   227,   177,
     169,   170,     0,     0,     0,   151,     0,   153,   122,   101,
     123,     0,   119,   236,     0,    37,    36,     0,     0,     0,
     239,     0,    57,   135,     0,   250,     0,   246,    30,   251,
     138,   166,   168,     0,     0,   103,     0,     0,     0,    54,
      29,    29,    29,     0,     0,    78,     0,     0,   143,   142,
     133,    58,     0,     0,   279,    11,    22,     0,     0,    14,
      21,     0,   157,    29,    54,    69,   161,    54,   235,   214,
       0,     0,     0,   200,   217,     0,     0,     0,     0,   199,
     154,     0,   124,     0,   121,    35,     0,    29,    29,   247,
       0,   248,   253,   106,   105,     0,     0,     0,     0,   131,
      70,     0,     0,     0,   136,    54,    29,    29,    29,     0,
      54,    62,     0,     0,   206,   206,     0,   254,   254,   258,
      17,     0,   159,   164,    68,    67,     0,   208,     0,   211,
       0,     0,   228,     0,     0,     0,    83,   179,     0,   155,
     125,   120,    60,     0,     0,   249,     0,     0,   107,   139,
     112,   111,     0,     0,    54,    74,    54,    75,     0,     0,
       0,   144,     0,    59,   206,   206,   201,   201,    16,   255,
     257,   256,     0,   242,     0,     0,   259,    29,   219,     0,
       9,   215,     0,     0,   218,     0,     0,     0,    80,     0,
      88,     0,     0,    82,   126,    54,    63,   109,   108,     0,
       0,     0,   113,   140,    73,    71,    54,    77,    54,   137,
     201,   201,    44,    44,    21,    19,   254,   244,    18,    20,
     258,     0,   209,   212,     0,    83,    83,    84,    83,     0,
      61,   110,   115,   114,     0,    76,    72,    44,    44,    27,
      46,    46,     0,   243,     0,   220,     0,    83,    90,    89,
      85,    83,   116,    46,    46,    45,     0,     0,    42,    46,
       0,    10,   245,   221,    87,    86,     0,     0,    27,    23,
      43,    25,    24,    26,    47
};

/* YYPGOTO[NTERM-NUM].  */
static const yytype_int16 yypgoto[] =
{
    -456,  -456,  -456,  -456,  -456,   504,  -456,   187,    79,  -456,
    -325,  -456,  -114,    50,  -456,   177,  -456,  -294,  -268,   -11,
     475,    -7,   -48,   147,  -456,    -8,  -456,  -456,  -455,  -339,
    -456,  -456,  -456,  -456,  -456,  -456,   -26,  -456,  -456,    -9,
     456,  -456,  -112,   -96,   -54,   277,   -77,  -456,  -456,  -453,
     170,  -456,  -456,   223,  -456,   -55,   -17,   141,   -24,   495,
    -456,    47,   449,   453,   370,   469,     3,   393,   347,  -456,
    -456,   152,    56,  -162,  -456,  -185,  -456,  -456,  -456,  -102,
    -456,  -456,   -95,    39,  -229,   -14,  -456
};

/* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int16 yydefgoto[] =
{
       0,     3,     4,     5,    62,     6,   233,   143,   330,   390,
     231,   326,   129,   242,   170,    33,    34,   527,   510,   528,
      35,   239,    67,   240,    36,    37,   272,   273,   460,   405,
      38,    39,    40,    41,   106,   181,    42,   224,    43,    44,
      93,    94,   149,   150,   333,   152,   243,   119,   120,   268,
      45,    46,    47,   208,    48,   164,    49,   257,    77,    51,
     447,   448,    82,    83,    84,    73,    70,    71,   110,   111,
     112,   442,   443,    52,   209,   210,   211,   444,   449,    53,
      54,    55,    56,    57,   113,   127,    58
};

/* YYTABLE[YYPACT[STATE-NUM]] -- What to do in state STATE-NUM.  If
   positive, shift that token.  If negative, reduce the rule whose
   number is the opposite.  If YYTABLE_NINF, syntax error.  */
static const yytype_int16 yytable[] =
{
      87,   197,   328,    90,   115,   391,    68,    66,   269,   408,
     219,   151,   108,    22,   294,   114,   212,   117,   153,    92,
     107,     7,   296,   213,    74,   216,   229,   509,  -223,    75,
      16,    60,    27,    20,   134,   173,     7,     8,     7,    88,
     518,   519,   230,   520,   166,    16,    22,    16,    76,    68,
     139,   217,   432,   118,   167,    92,   148,    89,    68,    72,
      72,   532,   534,   533,    85,    27,   535,   316,   270,  -281,
     116,   121,   114,   245,  -223,   271,   304,   107,   161,   171,
     172,   220,   174,   317,    75,   221,   179,   182,   156,   183,
     255,   153,   191,    59,   280,   383,    61,   212,     2,   305,
     167,   306,   225,    76,   213,   212,   128,   157,   308,   310,
     360,   227,   213,   148,    72,    85,    22,     7,  -226,   281,
     497,   318,   185,     1,   117,   311,    16,  -226,   186,   148,
     249,    68,    30,   168,     2,    27,   237,   238,  -226,    68,
     456,   291,    50,    85,   178,   344,   169,   187,   250,   188,
      22,   122,   322,   323,  -226,   345,   137,   214,   457,   512,
     118,   193,   328,   399,  -226,   222,   275,   312,   277,    27,
      50,   278,   293,   400,   135,   138,   282,   283,   284,   357,
      22,   234,   136,   289,    69,   368,   313,    68,   292,   300,
     132,   153,    50,   212,    72,   358,   212,   251,   320,    27,
     213,   247,   376,   213,   258,   314,   141,    85,   369,   303,
     306,   148,   148,   168,    68,   511,    22,   530,   377,   158,
      68,   321,   142,   154,   155,   140,   131,    69,   133,   536,
     537,    68,   332,   371,   372,    27,    69,    64,   214,   523,
     524,    65,     7,   165,   153,   387,   214,   388,   389,   132,
      50,    16,    78,    79,    80,   301,   302,   346,   176,    81,
     378,   299,   319,   439,   440,   159,   155,   350,   441,   184,
     327,   352,   353,   153,   175,   132,   177,   253,   155,   183,
     413,   414,   148,   328,    68,   189,   190,   374,   339,   340,
     192,   341,   198,   342,   194,   365,   195,   332,   367,   428,
     429,    68,   397,   398,   215,   148,   485,   486,   379,    69,
     199,   226,   223,    68,   488,   486,   232,    69,   489,   167,
     235,   236,   392,   252,   241,   254,    68,   244,   246,    68,
     297,   279,   132,   285,   214,   286,   295,   214,   406,   406,
     419,   256,   409,   298,   329,   411,   334,   145,    68,   412,
     274,   354,   269,   343,   269,   335,   337,   349,   338,   422,
     436,   437,   373,   382,   153,    69,     7,    68,   287,   315,
     347,   348,    68,   351,   355,    16,   186,   461,   359,   396,
     361,   482,   483,   393,   401,   363,   407,   407,    22,   366,
     467,   468,    69,    78,   137,   525,   126,   473,    69,    68,
     139,   464,   380,   381,    68,   384,   479,    27,   385,    69,
     480,   481,   270,   138,   270,   141,    68,   402,    68,   271,
     410,   271,   415,   416,   544,   507,   508,   418,   430,   417,
     327,   420,   423,   424,   425,   434,   451,   452,   426,   453,
       7,   431,   455,   501,   502,   503,   462,   495,   496,    16,
     406,   435,   498,   438,   446,   454,   370,    68,   458,   459,
     463,   469,    69,   465,   470,   466,   471,   472,    68,    97,
      68,   124,   100,   101,   324,   103,   125,   105,   522,    69,
     476,   394,   325,   487,   395,   517,   493,   477,   491,   478,
     521,    69,   492,   494,   499,   504,   514,   490,   407,     7,
     538,   529,   529,   516,    69,   196,   539,    69,    16,    63,
     526,   541,   542,   543,   529,   529,   386,   484,   540,   130,
     144,   362,   427,   336,   109,   162,    69,   433,    97,   163,
     124,   100,   101,   324,   103,   125,   105,   515,   259,   290,
     445,   450,   513,     0,   160,    69,     0,     0,   248,     0,
      69,   327,     0,     0,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,     0,     0,    17,    18,    19,     0,
       0,   474,     0,   475,     0,    95,    22,    69,     0,     0,
      23,    96,    69,    97,    98,    99,   100,   101,   102,   103,
     125,   105,     0,    26,    69,    27,    69,    28,     0,     0,
       0,    86,     0,     0,    31,    32,     0,     7,     0,     0,
       0,     0,   500,     0,     0,     0,    16,     8,     9,    10,
     260,   261,   262,   505,    15,   506,     0,     0,   263,   264,
     265,     0,     0,     0,     0,    69,    97,     0,   124,   100,
     101,   102,   103,   125,   105,     0,    69,    24,    69,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,     0,
       0,    17,    18,    19,     0,    20,    21,   267,     0,     0,
      95,    22,     0,     0,     0,    23,    96,     0,    97,    98,
      99,   100,   101,   102,   103,   104,   105,     0,    26,     0,
      27,     0,    28,     0,     0,     0,    86,     0,     0,    31,
      32,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,     0,     0,    17,    18,    19,     0,    20,    21,     0,
       0,     0,     0,    22,     0,     0,     0,    23,     0,     0,
       0,     0,    24,     0,     0,     0,     0,    25,     0,     0,
      26,     0,    27,     0,    28,     0,     0,     0,    29,     0,
      30,    31,    32,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,     0,     0,    17,    18,    19,     0,     0,
       0,     0,     0,     0,     0,    22,     0,     0,     0,    23,
       0,     0,     0,   145,    24,     0,     0,   146,     0,     0,
       0,     0,    26,     0,    27,     0,    28,     0,     0,     0,
      86,   147,     0,    31,    32,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,     0,     0,    17,    18,    19,
       0,     0,     0,     0,     0,     0,     0,    22,     0,     0,
       0,    23,     0,     0,     0,   145,    24,     0,     0,   146,
       0,     0,     0,     0,    26,     0,    27,     0,    28,     0,
       0,     0,    86,   309,     0,    31,    32,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,     0,     0,    17,
      18,    19,     0,     0,     0,     0,     0,     0,     0,    22,
       0,     0,     0,    23,     0,     0,     0,   145,    24,     0,
       0,   146,     0,     0,     0,     0,    26,     0,    27,     0,
      28,     0,     0,     0,    86,   375,     0,    31,    32,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,     0,
       0,    17,    18,    19,     0,    20,     0,     0,     0,     0,
       0,    22,     0,     0,     0,    23,     0,     0,     0,     0,
      24,     0,     0,     0,     0,     0,     0,     0,    26,     0,
      27,     0,    28,     0,     0,     0,    86,     0,    30,    31,
      32,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,     0,     0,    17,    18,    19,     0,     0,     0,     0,
       0,     0,     0,    22,     0,     0,     0,    23,     0,     0,
       0,     0,    24,     0,     0,   146,     0,     0,     0,     0,
      26,     0,    27,     0,    28,     0,     0,     0,    86,   228,
       0,    31,    32,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,     0,     0,    17,    18,    19,     0,     0,
       0,     0,     0,     0,     0,    22,     0,     0,     0,    23,
       0,     0,     0,   145,    24,     0,     0,   331,     0,     0,
       0,     0,    26,     0,    27,     0,    28,     0,     0,     0,
      86,     0,     0,    31,    32,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,     0,     0,    17,    18,    19,
       0,     0,     0,     0,     0,     0,     0,    22,     0,     0,
       0,    23,     0,     0,     0,     0,    24,     0,     0,   146,
       0,     0,     0,     0,    26,     0,    27,     0,    28,     0,
       0,     0,    86,   356,     0,    31,    32,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,     0,     0,    17,
      18,    19,     0,   403,     0,     0,     0,     0,     0,    22,
       0,     0,     0,    23,   404,     0,     0,     0,    24,     0,
       0,     0,     0,     0,     0,     0,    26,     0,    27,     0,
      28,     0,     0,     0,    86,     0,     0,    31,    32,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,     0,
       0,    17,    18,    19,     0,     0,     0,     0,     0,     0,
       0,    22,     0,     0,     0,    23,     0,     0,     0,    91,
      24,     0,     0,     0,     0,     0,     0,     0,    26,     0,
      27,     0,    28,     0,     0,     0,    86,     0,     0,    31,
      32,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,     0,     0,    17,    18,    19,     0,     0,     0,     0,
       0,     0,   180,    22,     0,     0,     0,    23,     0,     0,
       0,     0,    24,     0,     0,     0,     0,     0,     0,     0,
      26,     0,    27,     0,    28,     0,     0,     0,    86,     0,
       0,    31,    32,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,     0,     0,    17,    18,    19,     0,     0,
       0,     0,     0,     0,   218,    22,     0,     0,     0,    23,
       0,     0,     0,     0,    24,     0,     0,     0,     0,     0,
       0,     0,    26,     0,    27,     0,    28,     0,     0,     0,
      86,     0,     0,    31,    32,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,     0,     0,    17,    18,    19,
       0,     0,     0,     0,     0,     0,     0,    22,     0,     0,
       0,    23,     0,     0,     0,   276,    24,     0,     0,     0,
       0,     0,     0,     0,    26,     0,    27,     0,    28,     0,
       0,     0,    86,     0,     0,    31,    32,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,     0,     0,    17,
      18,    19,     0,     0,     0,     0,     0,     0,     0,    22,
       0,     0,     0,    23,     0,     0,     0,   288,    24,     0,
       0,     0,     0,     0,     0,     0,    26,     0,    27,     0,
      28,     0,     0,     0,    86,     0,     0,    31,    32,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,     0,
       0,    17,    18,    19,     0,     0,     0,     0,     0,     0,
     307,    22,     0,     0,     0,    23,     0,     0,     0,     0,
      24,     0,     0,     0,     0,     0,     0,     0,    26,     0,
      27,     0,    28,     0,     0,     0,    86,     0,     0,    31,
      32,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,     0,     0,    17,    18,    19,     0,     0,     0,     0,
       0,     0,     0,    22,     0,     0,     0,    23,     0,     0,
       0,     0,    24,     0,     0,     0,     0,     0,     0,     0,
      26,     0,    27,     0,    28,   364,     0,     0,    86,     0,
       0,    31,    32,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,     0,     0,    17,    18,    19,     0,     0,
       0,     0,     0,     0,     0,    22,     0,     0,     0,    23,
       0,     0,     0,     0,    24,     0,     0,     0,     0,     0,
       0,     0,    26,     0,    27,     0,    28,   421,     0,     0,
      86,     0,     0,    31,    32,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,     0,     0,    17,    18,    19,
       0,     0,     0,     0,     0,     0,     0,    22,     0,     0,
       0,    23,     0,     0,     0,     0,    24,     0,     0,     0,
       0,     0,     0,     0,    26,     0,    27,     0,    28,     0,
       0,     0,    86,     0,   526,    31,    32,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,     0,     0,    17,
      18,    19,     0,     8,     9,    10,   260,   261,   262,    22,
      15,     0,     0,    23,   263,   264,   265,     0,    24,     0,
       0,     0,     0,     0,     0,     0,    26,     0,    27,     0,
      28,     0,     0,    24,    86,     0,     0,    31,    32,     0,
       0,     7,     0,     0,     0,     0,     0,     0,     0,   266,
      16,     0,     0,   267,     7,     8,     9,    10,   200,   201,
     202,   203,    15,    16,     0,     0,   204,   205,   206,     0,
      97,     0,   124,   100,   101,   324,   103,   125,   105,     0,
       0,     0,     0,   531,     0,    24,    97,   123,   124,   100,
     101,   102,   103,   125,   105,   126,     0,   207,     0,     0,
       0,     0,   196,    97,   123,   124,   100,   101,   102,   103,
     125,   105,   126
};

static const yytype_int16 yycheck[] =
{
      26,   115,   231,    27,    30,   330,    14,    14,   170,   348,
     122,    65,    29,    25,   199,    29,   118,    10,    66,    28,
      29,     3,   207,   118,    21,    23,   138,    13,    23,    20,
      12,     0,    44,    19,    51,    90,     3,     4,     3,    30,
     495,   496,   138,   498,    39,    12,    25,    12,    39,    57,
      57,    49,    31,    46,    49,    64,    65,    48,    66,    20,
      21,   514,   517,   516,    25,    44,   521,    34,   170,    51,
      31,    32,    86,   150,    23,   170,    24,    86,    75,    88,
      89,    46,    91,    50,    20,    50,    95,    96,    31,    98,
      39,   139,   109,    10,    24,   324,    10,   199,    38,    47,
      49,    49,   128,    39,   199,   207,    48,    50,   220,   221,
     295,   137,   207,   122,    75,    76,    25,     3,     3,    49,
     459,   223,    24,    27,    10,   221,    12,    12,    30,   138,
     156,   139,    52,    83,    38,    44,   145,   146,    23,   147,
      12,    50,     1,   104,    94,    39,    31,    49,   157,    51,
      25,    46,   229,   230,    39,    49,    31,   118,    30,   484,
      46,   111,   391,    39,    49,   126,   175,   221,   177,    44,
      29,   180,   198,    49,    11,    50,   185,   186,   187,   291,
      25,   142,    11,   192,    14,    24,    31,   195,   195,   215,
      49,   239,    51,   295,   155,   291,   298,   158,   224,    44,
     295,   151,   314,   298,   165,    50,    30,   168,    47,   218,
      49,   220,   221,   163,   222,   483,    25,   511,   314,    23,
     228,   228,    46,    48,    49,    44,    49,    57,    51,   523,
     524,   239,   241,   310,   311,    44,    66,    46,   199,   507,
     508,    50,     3,    23,   292,    19,   207,    21,    22,   108,
     109,    12,     4,     5,     6,   216,   217,   266,    47,    11,
     314,   211,   223,    35,    36,    48,    49,   276,    40,    51,
     231,   280,   281,   321,    30,   134,    49,    48,    49,   288,
     357,   358,   291,   512,   292,   108,   109,   313,    48,    49,
      49,   252,    31,   254,    51,   304,    51,   306,   307,   376,
     377,   309,    48,    49,    31,   314,    48,    49,   317,   139,
      46,   134,    41,   321,    48,    49,    10,   147,    48,    49,
      44,    47,   331,    23,    49,    23,   334,    49,    49,   337,
      47,    51,   191,    51,   295,    51,    46,   298,   347,   348,
     366,   164,   351,    49,    47,   354,    51,    33,   356,   356,
     173,    24,   514,    45,   516,    51,    51,    45,    51,   368,
     384,   385,   312,   324,   412,   195,     3,   375,   191,   222,
      49,    49,   380,    30,    51,    12,    30,    14,    47,   340,
      47,   436,   437,   333,   345,    47,   347,   348,    25,    31,
     416,   417,   222,     4,    31,   509,    41,   423,   228,   407,
     407,   410,    51,    51,   412,    44,   432,    44,    44,   239,
     434,   435,   514,    50,   516,    30,   424,    51,   426,   514,
      24,   516,    47,    31,   538,   480,   481,    47,   378,    31,
     391,    47,    31,    51,    51,    44,    48,   398,    51,   400,
       3,    51,   403,   469,   470,   471,   407,   456,   457,    12,
     459,    44,   461,    44,    35,    39,   309,   465,    45,    49,
      45,    31,   292,    51,    31,    51,    31,    47,   476,    32,
     478,    34,    35,    36,    37,    38,    39,    40,   504,   309,
      51,   334,    45,   444,   337,   494,    39,    51,   449,    51,
     499,   321,    48,    31,    14,    31,    31,   447,   459,     3,
     526,   510,   511,    31,   334,    48,    45,   337,    12,     5,
      52,    45,    45,    45,   523,   524,   329,   438,   529,    44,
      64,   298,   375,   246,    29,    76,   356,   380,    32,    76,
      34,    35,    36,    37,    38,    39,    40,   490,   168,   192,
     388,    45,   486,    -1,    75,   375,    -1,    -1,   155,    -1,
     380,   512,    -1,    -1,     3,     4,     5,     6,     7,     8,
       9,    10,    11,    12,    -1,    -1,    15,    16,    17,    -1,
      -1,   424,    -1,   426,    -1,    24,    25,   407,    -1,    -1,
      29,    30,   412,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    -1,    42,   424,    44,   426,    46,    -1,    -1,
      -1,    50,    -1,    -1,    53,    54,    -1,     3,    -1,    -1,
      -1,    -1,   465,    -1,    -1,    -1,    12,     4,     5,     6,
       7,     8,     9,   476,    11,   478,    -1,    -1,    15,    16,
      17,    -1,    -1,    -1,    -1,   465,    32,    -1,    34,    35,
      36,    37,    38,    39,    40,    -1,   476,    34,   478,     3,
       4,     5,     6,     7,     8,     9,    10,    11,    12,    -1,
      -1,    15,    16,    17,    -1,    19,    20,    54,    -1,    -1,
      24,    25,    -1,    -1,    -1,    29,    30,    -1,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    -1,    42,    -1,
      44,    -1,    46,    -1,    -1,    -1,    50,    -1,    -1,    53,
      54,     3,     4,     5,     6,     7,     8,     9,    10,    11,
      12,    -1,    -1,    15,    16,    17,    -1,    19,    20,    -1,
      -1,    -1,    -1,    25,    -1,    -1,    -1,    29,    -1,    -1,
      -1,    -1,    34,    -1,    -1,    -1,    -1,    39,    -1,    -1,
      42,    -1,    44,    -1,    46,    -1,    -1,    -1,    50,    -1,
      52,    53,    54,     3,     4,     5,     6,     7,     8,     9,
      10,    11,    12,    -1,    -1,    15,    16,    17,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    25,    -1,    -1,    -1,    29,
      -1,    -1,    -1,    33,    34,    -1,    -1,    37,    -1,    -1,
      -1,    -1,    42,    -1,    44,    -1,    46,    -1,    -1,    -1,
      50,    51,    -1,    53,    54,     3,     4,     5,     6,     7,
       8,     9,    10,    11,    12,    -1,    -1,    15,    16,    17,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    25,    -1,    -1,
      -1,    29,    -1,    -1,    -1,    33,    34,    -1,    -1,    37,
      -1,    -1,    -1,    -1,    42,    -1,    44,    -1,    46,    -1,
      -1,    -1,    50,    51,    -1,    53,    54,     3,     4,     5,
       6,     7,     8,     9,    10,    11,    12,    -1,    -1,    15,
      16,    17,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    25,
      -1,    -1,    -1,    29,    -1,    -1,    -1,    33,    34,    -1,
      -1,    37,    -1,    -1,    -1,    -1,    42,    -1,    44,    -1,
      46,    -1,    -1,    -1,    50,    51,    -1,    53,    54,     3,
       4,     5,     6,     7,     8,     9,    10,    11,    12,    -1,
      -1,    15,    16,    17,    -1,    19,    -1,    -1,    -1,    -1,
      -1,    25,    -1,    -1,    -1,    29,    -1,    -1,    -1,    -1,
      34,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    42,    -1,
      44,    -1,    46,    -1,    -1,    -1,    50,    -1,    52,    53,
      54,     3,     4,     5,     6,     7,     8,     9,    10,    11,
      12,    -1,    -1,    15,    16,    17,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    25,    -1,    -1,    -1,    29,    -1,    -1,
      -1,    -1,    34,    -1,    -1,    37,    -1,    -1,    -1,    -1,
      42,    -1,    44,    -1,    46,    -1,    -1,    -1,    50,    51,
      -1,    53,    54,     3,     4,     5,     6,     7,     8,     9,
      10,    11,    12,    -1,    -1,    15,    16,    17,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    25,    -1,    -1,    -1,    29,
      -1,    -1,    -1,    33,    34,    -1,    -1,    37,    -1,    -1,
      -1,    -1,    42,    -1,    44,    -1,    46,    -1,    -1,    -1,
      50,    -1,    -1,    53,    54,     3,     4,     5,     6,     7,
       8,     9,    10,    11,    12,    -1,    -1,    15,    16,    17,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    25,    -1,    -1,
      -1,    29,    -1,    -1,    -1,    -1,    34,    -1,    -1,    37,
      -1,    -1,    -1,    -1,    42,    -1,    44,    -1,    46,    -1,
      -1,    -1,    50,    51,    -1,    53,    54,     3,     4,     5,
       6,     7,     8,     9,    10,    11,    12,    -1,    -1,    15,
      16,    17,    -1,    19,    -1,    -1,    -1,    -1,    -1,    25,
      -1,    -1,    -1,    29,    30,    -1,    -1,    -1,    34,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    42,    -1,    44,    -1,
      46,    -1,    -1,    -1,    50,    -1,    -1,    53,    54,     3,
       4,     5,     6,     7,     8,     9,    10,    11,    12,    -1,
      -1,    15,    16,    17,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    25,    -1,    -1,    -1,    29,    -1,    -1,    -1,    33,
      34,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    42,    -1,
      44,    -1,    46,    -1,    -1,    -1,    50,    -1,    -1,    53,
      54,     3,     4,     5,     6,     7,     8,     9,    10,    11,
      12,    -1,    -1,    15,    16,    17,    -1,    -1,    -1,    -1,
      -1,    -1,    24,    25,    -1,    -1,    -1,    29,    -1,    -1,
      -1,    -1,    34,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      42,    -1,    44,    -1,    46,    -1,    -1,    -1,    50,    -1,
      -1,    53,    54,     3,     4,     5,     6,     7,     8,     9,
      10,    11,    12,    -1,    -1,    15,    16,    17,    -1,    -1,
      -1,    -1,    -1,    -1,    24,    25,    -1,    -1,    -1,    29,
      -1,    -1,    -1,    -1,    34,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    42,    -1,    44,    -1,    46,    -1,    -1,    -1,
      50,    -1,    -1,    53,    54,     3,     4,     5,     6,     7,
       8,     9,    10,    11,    12,    -1,    -1,    15,    16,    17,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    25,    -1,    -1,
      -1,    29,    -1,    -1,    -1,    33,    34,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    42,    -1,    44,    -1,    46,    -1,
      -1,    -1,    50,    -1,    -1,    53,    54,     3,     4,     5,
       6,     7,     8,     9,    10,    11,    12,    -1,    -1,    15,
      16,    17,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    25,
      -1,    -1,    -1,    29,    -1,    -1,    -1,    33,    34,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    42,    -1,    44,    -1,
      46,    -1,    -1,    -1,    50,    -1,    -1,    53,    54,     3,
       4,     5,     6,     7,     8,     9,    10,    11,    12,    -1,
      -1,    15,    16,    17,    -1,    -1,    -1,    -1,    -1,    -1,
      24,    25,    -1,    -1,    -1,    29,    -1,    -1,    -1,    -1,
      34,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    42,    -1,
      44,    -1,    46,    -1,    -1,    -1,    50,    -1,    -1,    53,
      54,     3,     4,     5,     6,     7,     8,     9,    10,    11,
      12,    -1,    -1,    15,    16,    17,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    25,    -1,    -1,    -1,    29,    -1,    -1,
      -1,    -1,    34,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      42,    -1,    44,    -1,    46,    47,    -1,    -1,    50,    -1,
      -1,    53,    54,     3,     4,     5,     6,     7,     8,     9,
      10,    11,    12,    -1,    -1,    15,    16,    17,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    25,    -1,    -1,    -1,    29,
      -1,    -1,    -1,    -1,    34,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    42,    -1,    44,    -1,    46,    47,    -1,    -1,
      50,    -1,    -1,    53,    54,     3,     4,     5,     6,     7,
       8,     9,    10,    11,    12,    -1,    -1,    15,    16,    17,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    25,    -1,    -1,
      -1,    29,    -1,    -1,    -1,    -1,    34,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    42,    -1,    44,    -1,    46,    -1,
      -1,    -1,    50,    -1,    52,    53,    54,     3,     4,     5,
       6,     7,     8,     9,    10,    11,    12,    -1,    -1,    15,
      16,    17,    -1,     4,     5,     6,     7,     8,     9,    25,
      11,    -1,    -1,    29,    15,    16,    17,    -1,    34,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    42,    -1,    44,    -1,
      46,    -1,    -1,    34,    50,    -1,    -1,    53,    54,    -1,
      -1,     3,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    50,
      12,    -1,    -1,    54,     3,     4,     5,     6,     7,     8,
       9,    10,    11,    12,    -1,    -1,    15,    16,    17,    -1,
      32,    -1,    34,    35,    36,    37,    38,    39,    40,    -1,
      -1,    -1,    -1,    45,    -1,    34,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    -1,    46,    -1,    -1,
      -1,    -1,    48,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41
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
     112,   114,   128,   134,   135,   136,   137,   138,   141,    10,
       0,    10,    59,    60,    46,    50,    76,    77,    80,   105,
     121,   122,   138,   120,   121,    20,    39,   113,     4,     5,
       6,    11,   117,   118,   119,   138,    50,    91,    30,    48,
     113,    33,    94,    95,    96,    24,    30,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    89,    94,   111,   114,
     123,   124,   125,   139,   140,    91,   138,    10,    46,   102,
     103,   138,    46,    33,    34,    39,    41,   140,    48,    67,
      75,    70,   112,    70,   111,    11,    11,    31,    50,    76,
      44,    30,    46,    62,    95,    33,    37,    51,    94,    97,
      98,    99,   100,    77,    48,    49,    31,    50,    23,    48,
     120,   121,   117,   118,   110,    23,    39,    49,    68,    31,
      69,    94,    94,   110,    94,    30,    47,    49,    68,    94,
      24,    90,    94,    94,    51,    24,    30,    49,    51,    70,
      70,   111,    49,    68,    51,    51,    48,    67,    31,    46,
       7,     8,     9,    10,    15,    16,    17,    46,   108,   129,
     130,   131,   134,   137,   138,    31,    23,    49,    24,    97,
      46,    50,   138,    41,    92,    91,    70,    91,    51,    97,
      98,    65,    10,    61,   138,    44,    47,    94,    94,    76,
      78,    49,    68,   101,    49,   101,    49,    68,   122,    91,
      94,   138,    23,    48,    23,    39,    70,   112,   138,   119,
       7,     8,     9,    15,    16,    17,    50,    54,   104,   128,
     134,   137,    81,    82,    70,    94,    33,    94,    94,    51,
      24,    49,    94,    94,    94,    51,    51,    70,    33,    94,
     123,    50,    76,    91,   130,    46,   130,    47,    49,    68,
      91,   138,   138,    94,    24,    47,    49,    24,    97,    51,
      97,    98,    99,    31,    50,    78,    34,    50,   134,   138,
      91,    76,   101,   101,    37,    45,    66,   138,   139,    47,
      63,    37,    94,    99,    51,    51,   100,    51,    51,    48,
      49,   138,   138,    45,    39,    49,    94,    49,    49,    45,
      94,    30,    94,    94,    24,    51,    51,    97,    98,    47,
     130,    47,   108,    47,    47,    94,    31,    94,    24,    47,
      78,   101,   101,    68,    91,    51,    97,    98,    99,    94,
      51,    51,   138,   139,    44,    44,    62,    19,    21,    22,
      64,    65,    94,    68,    78,    78,   138,    48,    49,    39,
      49,   138,    51,    19,    30,    84,    94,   138,    84,    94,
      24,    94,    76,   101,   101,    47,    31,    31,    47,    91,
      47,    47,    94,    31,    51,    51,    51,    78,   101,   101,
      68,    51,    31,    78,    44,    44,   113,   113,    44,    35,
      36,    40,   126,   127,   132,   126,    35,   115,   116,   133,
      45,    48,   138,   138,    39,   138,    12,    30,    45,    49,
      83,    14,   138,    45,    94,    51,    51,    91,    91,    31,
      31,    31,    47,    91,    78,    78,    51,    51,    51,    91,
     113,   113,   110,   110,    63,    48,    49,   138,    48,    48,
      68,   138,    48,    39,    31,    94,    94,    84,    94,    14,
      78,    91,    91,    91,    31,    78,    78,   110,   110,    13,
      73,    73,    65,   127,    31,   116,    31,    94,    83,    83,
      83,    94,    91,    73,    73,    67,    52,    72,    74,    94,
      72,    45,   104,   104,    83,    83,    72,    72,    91,    45,
      74,    45,    45,    45,    67
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
     108,   108,   108,   108,   108,   108,   108,   108,   108,   109,
     109,   110,   110,   111,   111,   112,   113,   113,   113,   113,
     113,   113,   113,   114,   114,   114,   114,   114,   114,   115,
     115,   116,   117,   117,   118,   118,   119,   119,   119,   120,
     120,   121,   121,   122,   122,   122,   123,   123,   124,   124,
     125,   125,   126,   126,   127,   127,   128,   128,   129,   129,
     130,   130,   131,   131,   132,   132,   132,   132,   133,   133,
     134,   134,   135,   135,   136,   136,   137,   137,   137,   137,
     137,   137,   138,   138,   139,   139,   139,   139,   139,   139,
     139,   139,   140,   140,   141
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
       1,     1,     1,     1,     1,     1,     1,     1,     1,     5,
       5,     0,     2,     1,     2,     3,     0,     3,     5,     7,
       3,     5,     7,     3,     5,     7,     3,     5,     7,     1,
       3,     4,     0,     1,     1,     3,     1,     3,     5,     0,
       1,     1,     3,     1,     3,     4,     3,     2,     1,     3,
       0,     2,     1,     3,     2,     4,     4,     5,     3,     4,
       0,     2,     1,     3,     0,     1,     1,     1,     0,     1,
       1,     2,     1,     2,     1,     2,     1,     1,     2,     2,
       1,     2,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1
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
#line 158 "lang11d"
                                        { 
						(yyval.node) = gRootParseNode = (yyvsp[0].node);
					}
#line 2235 "lang11d_tab.cpp"
    break;

  case 3: /* root: classextensions  */
#line 162 "lang11d"
                                        { 
						(yyval.node) = gRootParseNode = (yyvsp[0].node);
					}
#line 2243 "lang11d_tab.cpp"
    break;

  case 4: /* root: INTERPRET cmdlinecode  */
#line 166 "lang11d"
                                        { 
						(yyval.node) = gRootParseNode = (yyvsp[0].node);
					}
#line 2251 "lang11d_tab.cpp"
    break;

  case 5: /* classes: %empty  */
#line 170 "lang11d"
                                 { (yyval.node) = nullptr; }
#line 2257 "lang11d_tab.cpp"
    break;

  case 6: /* classes: classes classdef  */
#line 172 "lang11d"
                                        { (yyval.node) = linkNodes((yyvsp[-1].node), (yyvsp[0].node)); }
#line 2263 "lang11d_tab.cpp"
    break;

  case 8: /* classextensions: classextensions classextension  */
#line 176 "lang11d"
                                        { (yyval.node) = linkNodes((yyvsp[-1].node), (yyvsp[0].node)); }
#line 2269 "lang11d_tab.cpp"
    break;

  case 9: /* classdef: CLASSNAME optSuperName '{' classvardecls methods '}'  */
#line 179 "lang11d"
                                        { (yyval.node) = allocParseNode<PyrClassNode>((yyloc), (yyvsp[-5].slotNode), (yyvsp[-4].slotNode), nullptr, (yyvsp[-2].varListNode), (yyvsp[-1].methodNode)); }
#line 2275 "lang11d_tab.cpp"
    break;

  case 10: /* classdef: CLASSNAME '[' optName ']' optSuperName '{' classvardecls methods '}'  */
#line 181 "lang11d"
                                        { (yyval.node) = allocParseNode<PyrClassNode>((yyloc), (yyvsp[-8].slotNode), (yyvsp[-4].slotNode), (yyvsp[-6].slotNode), (yyvsp[-2].varListNode), (yyvsp[-1].methodNode)); }
#line 2281 "lang11d_tab.cpp"
    break;

  case 11: /* classextension: '+' CLASSNAME '{' methods '}'  */
#line 184 "lang11d"
                                        { (yyval.node) = allocParseNode<PyrClassExtNode>((yyloc), (yyvsp[-3].slotNode), (yyvsp[-1].methodNode)); }
#line 2287 "lang11d_tab.cpp"
    break;

  case 12: /* optName: %empty  */
#line 186 "lang11d"
                                 { (yyval.slotNode) = nullptr; }
#line 2293 "lang11d_tab.cpp"
    break;

  case 14: /* optSuperName: %empty  */
#line 189 "lang11d"
                         { (yyval.slotNode) = nullptr; }
#line 2299 "lang11d_tab.cpp"
    break;

  case 15: /* optSuperName: ':' CLASSNAME  */
#line 189 "lang11d"
                                                           { (yyval.slotNode) = (yyvsp[0].slotNode); }
#line 2305 "lang11d_tab.cpp"
    break;

  case 16: /* classvardecls: %empty  */
#line 191 "lang11d"
                         { (yyval.varListNode) = nullptr; }
#line 2311 "lang11d_tab.cpp"
    break;

  case 17: /* classvardecls: classvardecls classvardecl  */
#line 193 "lang11d"
                                        { (yyval.varListNode) = linkNodes((yyvsp[-1].varListNode), (yyvsp[0].varListNode)); }
#line 2317 "lang11d_tab.cpp"
    break;

  case 18: /* classvardecl: CLASSVAR rwslotdeflist ';'  */
#line 196 "lang11d"
                                        { (yyval.varListNode) = allocParseNode<PyrVarListNode>((yyloc), (yyvsp[-1].varDefNode), varClass); }
#line 2323 "lang11d_tab.cpp"
    break;

  case 19: /* classvardecl: VAR rwslotdeflist ';'  */
#line 198 "lang11d"
                                        { (yyval.varListNode) = allocParseNode<PyrVarListNode>((yyloc), (yyvsp[-1].varDefNode), varInst); }
#line 2329 "lang11d_tab.cpp"
    break;

  case 20: /* classvardecl: SC_CONST constdeflist ';'  */
#line 200 "lang11d"
                                        { (yyval.varListNode) = allocParseNode<PyrVarListNode>((yyloc), (yyvsp[-1].varDefNode), varConst); }
#line 2335 "lang11d_tab.cpp"
    break;

  case 21: /* methods: %empty  */
#line 202 "lang11d"
                                 { (yyval.methodNode) = nullptr; }
#line 2341 "lang11d_tab.cpp"
    break;

  case 22: /* methods: methods methoddef  */
#line 204 "lang11d"
                                        { (yyval.methodNode) = linkNodes((yyvsp[-1].methodNode), (yyvsp[0].methodNode)); }
#line 2347 "lang11d_tab.cpp"
    break;

  case 23: /* methoddef: name '{' argdecls funcvardecls optPrim methbody '}'  */
#line 207 "lang11d"
                                        { (yyval.methodNode) = allocParseNode<PyrMethodNode>((yyloc), (yyvsp[-6].slotNode), (yyvsp[-2].slotNode), (yyvsp[-4].argListNode), (yyvsp[-3].varListNode), (yyvsp[-1].node), false); }
#line 2353 "lang11d_tab.cpp"
    break;

  case 24: /* methoddef: '*' name '{' argdecls funcvardecls optPrim methbody '}'  */
#line 209 "lang11d"
                                        { (yyval.methodNode) = allocParseNode<PyrMethodNode>((yyloc), (yyvsp[-6].slotNode), (yyvsp[-2].slotNode), (yyvsp[-4].argListNode), (yyvsp[-3].varListNode), (yyvsp[-1].node), true); }
#line 2359 "lang11d_tab.cpp"
    break;

  case 25: /* methoddef: binop '{' argdecls funcvardecls optPrim methbody '}'  */
#line 211 "lang11d"
                                        { (yyval.methodNode) = allocParseNode<PyrMethodNode>((yyloc), (yyvsp[-6].slotNode), (yyvsp[-2].slotNode), (yyvsp[-4].argListNode), (yyvsp[-3].varListNode), (yyvsp[-1].node), false); }
#line 2365 "lang11d_tab.cpp"
    break;

  case 26: /* methoddef: '*' binop '{' argdecls funcvardecls optPrim methbody '}'  */
#line 213 "lang11d"
                                        { (yyval.methodNode) = allocParseNode<PyrMethodNode>((yyloc), (yyvsp[-6].slotNode), (yyvsp[-2].slotNode), (yyvsp[-4].argListNode), (yyvsp[-3].varListNode), (yyvsp[-1].node), true); }
#line 2371 "lang11d_tab.cpp"
    break;

  case 34: /* funcbody: exprseq funretval  */
#line 221 "lang11d"
                                        { (yyval.node) = allocParseNode<PyrDropNode>((yyloc), (yyvsp[-1].node), (yyvsp[0].node)); }
#line 2377 "lang11d_tab.cpp"
    break;

  case 35: /* cmdlinecode: '(' argdecls1 funcvardecls1 funcbody ')'  */
#line 224 "lang11d"
                                        { (yyval.node) = allocParseNode<PyrBlockNode>((yyloc), (yyvsp[-3].argListNode), (yyvsp[-2].varListNode), (yyvsp[-1].node), false); }
#line 2383 "lang11d_tab.cpp"
    break;

  case 36: /* cmdlinecode: '(' argdecls1 funcbody ')'  */
#line 226 "lang11d"
                                        { (yyval.node) = allocParseNode<PyrBlockNode>((yyloc), (yyvsp[-2].argListNode), nullptr, (yyvsp[-1].node), false); }
#line 2389 "lang11d_tab.cpp"
    break;

  case 37: /* cmdlinecode: '(' funcvardecls1 funcbody ')'  */
#line 228 "lang11d"
                                        { (yyval.node) = allocParseNode<PyrBlockNode>((yyloc), nullptr, (yyvsp[-2].varListNode), (yyvsp[-1].node), false); }
#line 2395 "lang11d_tab.cpp"
    break;

  case 38: /* cmdlinecode: argdecls1 funcvardecls1 funcbody  */
#line 230 "lang11d"
                                        { (yyval.node) = allocParseNode<PyrBlockNode>((yyloc), (yyvsp[-2].argListNode), (yyvsp[-1].varListNode), (yyvsp[0].node), false); }
#line 2401 "lang11d_tab.cpp"
    break;

  case 39: /* cmdlinecode: argdecls1 funcbody  */
#line 232 "lang11d"
                                        { (yyval.node) = allocParseNode<PyrBlockNode>((yyloc), (yyvsp[-1].argListNode), nullptr, (yyvsp[0].node), false); }
#line 2407 "lang11d_tab.cpp"
    break;

  case 40: /* cmdlinecode: funcvardecls1 funcbody  */
#line 234 "lang11d"
                                        { (yyval.node) = allocParseNode<PyrBlockNode>((yyloc), nullptr, (yyvsp[-1].varListNode), (yyvsp[0].node), false); }
#line 2413 "lang11d_tab.cpp"
    break;

  case 41: /* cmdlinecode: funcbody  */
#line 236 "lang11d"
                                        { (yyval.node) = allocParseNode<PyrBlockNode>((yyloc), nullptr, nullptr, (yyvsp[0].node), false); }
#line 2419 "lang11d_tab.cpp"
    break;

  case 43: /* methbody: exprseq retval  */
#line 240 "lang11d"
                                        { (yyval.node) = allocParseNode<PyrDropNode>((yyloc), (yyvsp[-1].node), (yyvsp[0].node)); }
#line 2425 "lang11d_tab.cpp"
    break;

  case 44: /* optPrim: %empty  */
#line 242 "lang11d"
                                { (yyval.slotNode) = nullptr; }
#line 2431 "lang11d_tab.cpp"
    break;

  case 45: /* optPrim: PRIMITIVENAME optsemi  */
#line 243 "lang11d"
                                                        { (yyval.slotNode) = (yyvsp[-1].slotNode); }
#line 2437 "lang11d_tab.cpp"
    break;

  case 46: /* retval: %empty  */
#line 245 "lang11d"
                                 { (yyval.node) = allocParseNode<PyrReturnNode>((yyloc), nullptr); }
#line 2443 "lang11d_tab.cpp"
    break;

  case 47: /* retval: '^' expr optsemi  */
#line 247 "lang11d"
                                        { (yyval.node) = allocParseNode<PyrReturnNode>((yyloc), (yyvsp[-1].node)); }
#line 2449 "lang11d_tab.cpp"
    break;

  case 48: /* funretval: %empty  */
#line 250 "lang11d"
                                        { (yyval.node) = allocParseNode<PyrBlockReturnNode>((yyloc), nullptr); }
#line 2455 "lang11d_tab.cpp"
    break;

  case 49: /* funretval: '^' expr optsemi  */
#line 252 "lang11d"
                                        { (yyval.node) = allocParseNode<PyrReturnNode>((yyloc), (yyvsp[-1].node)); }
#line 2461 "lang11d_tab.cpp"
    break;

  case 51: /* blocklist1: blocklist1 blocklistitem  */
#line 256 "lang11d"
                                        { (yyval.node) = linkNodes((yyvsp[-1].node), (yyvsp[0].node)); }
#line 2467 "lang11d_tab.cpp"
    break;

  case 54: /* blocklist: %empty  */
#line 260 "lang11d"
                                 { (yyval.node) = nullptr; }
#line 2473 "lang11d_tab.cpp"
    break;

  case 56: /* msgsend: name blocklist1  */
#line 263 "lang11d"
                                        { (yyval.node) = allocParseNode<PyrCallNode>((yyloc), (yyvsp[-1].slotNode), (yyvsp[0].node), nullptr); }
#line 2479 "lang11d_tab.cpp"
    break;

  case 57: /* msgsend: '(' binop2 ')' blocklist1  */
#line 266 "lang11d"
                                        { (yyval.node) = allocParseNode<PyrCallNode>((yyloc), (yyvsp[-2].slotNode), (yyvsp[0].node), nullptr); }
#line 2485 "lang11d_tab.cpp"
    break;

  case 58: /* msgsend: name '(' ')' blocklist1  */
#line 269 "lang11d"
                                        { (yyval.node) = allocParseNode<PyrCallNode>((yyloc), (yyvsp[-3].slotNode), (yyvsp[0].node), nullptr); }
#line 2491 "lang11d_tab.cpp"
    break;

  case 59: /* msgsend: name '(' arglist1 optkeyarglist ')' blocklist  */
#line 272 "lang11d"
                                        { (yyval.node) = allocParseNode<PyrCallNode>((yyloc), (yyvsp[-5].slotNode), linkNodes((yyvsp[-3].node), (yyvsp[0].node)), (yyvsp[-2].node)); }
#line 2497 "lang11d_tab.cpp"
    break;

  case 60: /* msgsend: '(' binop2 ')' '(' ')' blocklist1  */
#line 275 "lang11d"
                                        { (yyval.node) = allocParseNode<PyrCallNode>((yyloc), (yyvsp[-4].slotNode), (yyvsp[0].node), nullptr); }
#line 2503 "lang11d_tab.cpp"
    break;

  case 61: /* msgsend: '(' binop2 ')' '(' arglist1 optkeyarglist ')' blocklist  */
#line 278 "lang11d"
                                        { (yyval.node) = allocParseNode<PyrCallNode>((yyloc), (yyvsp[-6].slotNode), linkNodes((yyvsp[-3].node), (yyvsp[0].node)), (yyvsp[-2].node)); }
#line 2509 "lang11d_tab.cpp"
    break;

  case 62: /* msgsend: name '(' arglistv1 optkeyarglist ')'  */
#line 281 "lang11d"
                                        {
						PyrSlot selector;
						// Is the keyword 'super' in 'msg(super, *[])'.
						if (isSuperObjNode((yyvsp[-2].node))) {
							auto* slot_node = node_cast<PyrSlotNode>((yyvsp[-2].node));
							slot_node->mSlot = PyrSlot::make(s_this);
							selector = PyrSlot::make(s_superPerformList);
						} else {
							selector = PyrSlot::make(s_performList);
						}
						auto* selectornode = allocParseNode<PyrSlotNode>((yyloc), selector);
						auto* args = linkAfterHead((yyvsp[-2].node), (yyvsp[-4].slotNode)->changeLiteralType<PyrParseNodeType::PushLitNode>());
						(yyval.node) = allocParseNode<PyrCallNode>((yyloc), selectornode, args, (yyvsp[-1].node));
					}
#line 2528 "lang11d_tab.cpp"
    break;

  case 63: /* msgsend: '(' binop2 ')' '(' arglistv1 optkeyarglist ')'  */
#line 296 "lang11d"
                                        {
						auto* selectornode = allocParseNode<PyrSlotNode>((yyloc), PyrSlot::make(s_performList));
						auto* args = linkAfterHead((yyvsp[-2].node), (yyvsp[-5].slotNode)->changeLiteralType<PyrParseNodeType::PushLitNode>());
						(yyval.node) = allocParseNode<PyrCallNode>((yyloc), selectornode, args, (yyvsp[-1].node));
					}
#line 2538 "lang11d_tab.cpp"
    break;

  case 64: /* msgsend: CLASSNAME '[' arrayelems ']'  */
#line 302 "lang11d"
                                        { (yyval.node) = allocParseNode<PyrDynListNode>((yyloc), (yyvsp[-3].slotNode), (yyvsp[-1].node)); }
#line 2544 "lang11d_tab.cpp"
    break;

  case 65: /* msgsend: CLASSNAME blocklist1  */
#line 305 "lang11d"
                                        {
						auto* selectornode = allocParseNode<PyrSlotNode>((yyloc), PyrSlot::make(s_new));
						auto* args = linkNodes((yyvsp[-1].slotNode)->changeLiteralType<PyrParseNodeType::PushNameNode>(), (yyvsp[0].node));
						(yyval.node) = allocParseNode<PyrCallNode>((yyloc), selectornode, args, nullptr);
					}
#line 2554 "lang11d_tab.cpp"
    break;

  case 66: /* msgsend: CLASSNAME '(' ')' blocklist  */
#line 311 "lang11d"
                                        {
						auto* selectornode = allocParseNode<PyrSlotNode>((yyloc), PyrSlot::make(s_new));
						auto* args = linkNodes((yyvsp[-3].slotNode)->changeLiteralType<PyrParseNodeType::PushNameNode>(), (yyvsp[0].node));
						(yyval.node) = allocParseNode<PyrCallNode>((yyloc), selectornode, args, nullptr);
					}
#line 2564 "lang11d_tab.cpp"
    break;

  case 67: /* msgsend: CLASSNAME '(' keyarglist1 optcomma ')' blocklist  */
#line 317 "lang11d"
                                        {
						auto* selectornode = allocParseNode<PyrSlotNode>((yyloc), PyrSlot::make(s_new));
						auto* args = linkNodes((yyvsp[-5].slotNode)->changeLiteralType<PyrParseNodeType::PushNameNode>(), (yyvsp[0].node));
						(yyval.node) = allocParseNode<PyrCallNode>((yyloc), selectornode, args, (yyvsp[-3].node));
					}
#line 2574 "lang11d_tab.cpp"
    break;

  case 68: /* msgsend: CLASSNAME '(' arglist1 optkeyarglist ')' blocklist  */
#line 323 "lang11d"
                                        {
						auto* selectornode = allocParseNode<PyrSlotNode>((yyloc), PyrSlot::make(s_new));
						auto* args = linkNodes(
							(yyvsp[-5].slotNode)->changeLiteralType<PyrParseNodeType::PushNameNode>(), 
							(yyvsp[-3].node),
							(yyvsp[0].node));
						(yyval.node) = allocParseNode<PyrCallNode>((yyloc), selectornode, args, (yyvsp[-2].node));
					}
#line 2587 "lang11d_tab.cpp"
    break;

  case 69: /* msgsend: CLASSNAME '(' arglistv1 optkeyarglist ')'  */
#line 332 "lang11d"
                                        {
						PyrSlot selector;
						if (isSuperObjNode((yyvsp[-4].slotNode))) {
							// Ehh? this is not possible. CLASSNAME cannot be 'super'.
							(yyvsp[-4].slotNode)->mSlot = PyrSlot::make(s_this);
							selector = PyrSlot::make(s_superPerformList);
						} else {
							selector = PyrSlot::make(s_performList);
						}

						auto* new_selector_push_lit = allocParseNode<PyrSlotNode>((yyloc), PyrSlot::make(s_new), PyrParseNodeType::PushLitNode);
						auto* args = linkNodes(
							(yyvsp[-4].slotNode)->changeLiteralType<PyrParseNodeType::PushNameNode>(),
							new_selector_push_lit,
							(yyvsp[-2].node)
						);
						auto* selectornode = allocParseNode<PyrSlotNode>((yyloc), selector);
						(yyval.node) = allocParseNode<PyrCallNode>((yyloc), selectornode, args, (yyvsp[-1].node));
					}
#line 2611 "lang11d_tab.cpp"
    break;

  case 70: /* msgsend: expr '.' '(' ')' blocklist  */
#line 352 "lang11d"
                                        {
						auto* selectornode = allocParseNode<PyrSlotNode>((yyloc), PyrSlot::make(s_value));
						(yyval.node) = allocParseNode<PyrCallNode>((yyloc), selectornode, linkNodes((yyvsp[-4].node), (yyvsp[0].node)), nullptr);
					}
#line 2620 "lang11d_tab.cpp"
    break;

  case 71: /* msgsend: expr '.' '(' keyarglist1 optcomma ')' blocklist  */
#line 357 "lang11d"
                                        {
						auto* selectornode = allocParseNode<PyrSlotNode>((yyloc), PyrSlot::make(s_value));
						(yyval.node) = allocParseNode<PyrCallNode>((yyloc), selectornode, linkNodes((yyvsp[-6].node), (yyvsp[0].node)), (yyvsp[-3].node));
					}
#line 2629 "lang11d_tab.cpp"
    break;

  case 72: /* msgsend: expr '.' name '(' keyarglist1 optcomma ')' blocklist  */
#line 362 "lang11d"
                                        { (yyval.node) = allocParseNode<PyrCallNode>((yyloc), (yyvsp[-5].slotNode), linkNodes((yyvsp[-7].node), (yyvsp[0].node)), (yyvsp[-3].node)); }
#line 2635 "lang11d_tab.cpp"
    break;

  case 73: /* msgsend: expr '.' '(' arglist1 optkeyarglist ')' blocklist  */
#line 364 "lang11d"
                                        {
						auto* selectornode = allocParseNode<PyrSlotNode>((yyloc), PyrSlot::make(s_value));
						(yyval.node) = allocParseNode<PyrCallNode>((yyloc), selectornode, linkNodes((yyvsp[-6].node), (yyvsp[-3].node), (yyvsp[0].node)), (yyvsp[-2].node));
					}
#line 2644 "lang11d_tab.cpp"
    break;

  case 74: /* msgsend: expr '.' '(' arglistv1 optkeyarglist ')'  */
#line 369 "lang11d"
                                        {
						PyrSlot selectorSlot;
						if (isSuperObjNode((yyvsp[-5].node))) {
							node_cast<PyrSlotNode>((yyvsp[-5].node))->mSlot = PyrSlot::make(s_this);
							selectorSlot = PyrSlot::make(s_superPerformList);
						} else {
							selectorSlot = PyrSlot::make(s_performList);
						}
						auto* selectornode = allocParseNode<PyrSlotNode>((yyloc), selectorSlot);
						auto* args = linkNodes(
							(yyvsp[-5].node), 
							allocParseNode<PyrSlotNode>((yyloc), PyrSlot::make(s_value), PyrParseNodeType::PushLitNode),
							(yyvsp[-2].node));
						(yyval.node) = allocParseNode<PyrCallNode>((yyloc), selectornode, args, (yyvsp[-1].node));
					}
#line 2664 "lang11d_tab.cpp"
    break;

  case 75: /* msgsend: expr '.' name '(' ')' blocklist  */
#line 385 "lang11d"
                                        { (yyval.node) = allocParseNode<PyrCallNode>((yyloc), (yyvsp[-3].slotNode), linkNodes((yyvsp[-5].node), (yyvsp[0].node)), nullptr); }
#line 2670 "lang11d_tab.cpp"
    break;

  case 76: /* msgsend: expr '.' name '(' arglist1 optkeyarglist ')' blocklist  */
#line 387 "lang11d"
                                        { (yyval.node) = allocParseNode<PyrCallNode>((yyloc), (yyvsp[-5].slotNode), linkNodes((yyvsp[-7].node), (yyvsp[-3].node), (yyvsp[0].node)), (yyvsp[-2].node)); }
#line 2676 "lang11d_tab.cpp"
    break;

  case 77: /* msgsend: expr '.' name '(' arglistv1 optkeyarglist ')'  */
#line 389 "lang11d"
                                        {
						PyrSlot slot;
						if (isSuperObjNode((yyvsp[-6].node))) {
							node_cast<PyrSlotNode>((yyvsp[-6].node))->mSlot = PyrSlot::make(s_this);
							slot = PyrSlot::make(s_superPerformList);
						} else {
							slot = PyrSlot::make(s_performList);
						}
						auto* selectornode = allocParseNode<PyrSlotNode>((yyloc), slot);
						auto* args = linkNodes((yyvsp[-6].node), (yyvsp[-4].slotNode)->changeLiteralType<PyrParseNodeType::PushLitNode>(), (yyvsp[-2].node));
						(yyval.node) = allocParseNode<PyrCallNode>((yyloc), selectornode, args, (yyvsp[-1].node));
					}
#line 2693 "lang11d_tab.cpp"
    break;

  case 78: /* msgsend: expr '.' name blocklist  */
#line 402 "lang11d"
                                        { (yyval.node) = allocParseNode<PyrCallNode>((yyloc), (yyvsp[-1].slotNode), linkNodes((yyvsp[-3].node), (yyvsp[0].node)), nullptr); }
#line 2699 "lang11d_tab.cpp"
    break;

  case 79: /* $@1: %empty  */
#line 410 "lang11d"
                                          { pushls(&generatorStack, (intptr_t)(yyvsp[0].node)); pushls(&generatorStack, 1); }
#line 2705 "lang11d_tab.cpp"
    break;

  case 80: /* generator: '{' ':' exprseq $@1 ',' qual '}'  */
#line 411 "lang11d"
                                        {
						PyrSlotNode* selectornode = allocParseNode<PyrSlotNode>((yyloc), PyrSlot::make(getsym("r")));
						PyrParseNode *block = allocParseNode<PyrBlockNode>((yyloc), nullptr, nullptr, (yyvsp[-1].node), false);
						PyrParseNode *blocklit = allocParseNode<PyrSlotNode>((yyloc), PyrSlot::make(static_cast<void*>(block)), PyrParseNodeType::PushLitNode);
						(yyval.node) = allocParseNode<PyrCallNode>((yyloc), selectornode, blocklit, nullptr);
					}
#line 2716 "lang11d_tab.cpp"
    break;

  case 81: /* $@2: %empty  */
#line 417 "lang11d"
                                                  { pushls(&generatorStack, (intptr_t)(yyvsp[0].node)); pushls(&generatorStack, 2); }
#line 2722 "lang11d_tab.cpp"
    break;

  case 82: /* generator: '{' ';' exprseq $@2 ',' qual '}'  */
#line 418 "lang11d"
                                        { (yyval.node) = (yyvsp[-1].node); }
#line 2728 "lang11d_tab.cpp"
    break;

  case 83: /* nextqual: %empty  */
#line 421 "lang11d"
                                        {
						// innermost part
						const int action = popls(&generatorStack);
						auto* expr = (PyrParseNode*)popls(&generatorStack);
						if (action == 1) 
							(yyval.node) = allocParseNode<PyrCallNode>((yyloc), allocParseNode<PyrSlotNode>((yyloc), PyrSlot::make(getsym("yield"))), expr, nullptr);
						else if (action == 2)
							(yyval.node) = expr; 
						// if action is neither, then what?
					}
#line 2743 "lang11d_tab.cpp"
    break;

  case 84: /* nextqual: ',' qual  */
#line 431 "lang11d"
                                           { (yyval.node) = (yyvsp[0].node); }
#line 2749 "lang11d_tab.cpp"
    break;

  case 85: /* qual: name LEFTARROW exprseq nextqual  */
#line 434 "lang11d"
                                        {
						// later should check if exprseq is a series and optimize it to for loop
						
						auto* var = allocParseNode<PyrVarDefNode>((yylsp[-3]), (yyvsp[-3].slotNode), nullptr, ReadWriteAccessor::Private);
						auto* args = allocParseNode<PyrArgListNode>((yylsp[-3]), var, nullptr, nullptr);
						auto *block = allocParseNode<PyrBlockNode>((yylsp[-3]), args, nullptr, (yyvsp[0].node), false);
						auto *blocklit = allocParseNode<PyrSlotNode>((yyloc), PyrSlot::make(static_cast<void*>(block)), PyrParseNodeType::PushLitNode);
						if (auto* call = node_cast<PyrCallNode>((yyvsp[-1].node)); call && call->mSelector->mSlot.getSymbol() == s_series) {
								call->mSelector->mSlot = PyrSlot::make(getsym("forSeries"));
								call->mArglist = linkNodes(call->mArglist, blocklit);
								(yyval.node) = call;
						} else {
							auto* selectornode = allocParseNode<PyrSlotNode>((yylsp[-1]), PyrSlot::make(getsym("do")));
							(yyval.node) = allocParseNode<PyrCallNode>((yyloc), selectornode, linkNodes((yyvsp[-1].node), blocklit), nullptr);
						}
					}
#line 2770 "lang11d_tab.cpp"
    break;

  case 86: /* qual: name name LEFTARROW exprseq nextqual  */
#line 451 "lang11d"
                                        {
						auto* call = node_cast<PyrCallNode>((yyvsp[-1].node));
						const auto is_series = call ? (call->mSelector->mSlot.getSymbol() == s_series) : false;

						auto* var1 = allocParseNode<PyrVarDefNode>((yyloc), (yyvsp[-4].slotNode), nullptr, ReadWriteAccessor::Private);
						auto* var2 = allocParseNode<PyrVarDefNode>((yyloc), (yyvsp[-3].slotNode), nullptr, ReadWriteAccessor::Private);
						auto* args = allocParseNode<PyrArgListNode>((yyloc), linkNodes(var1, var2), nullptr, nullptr);
						auto *block = allocParseNode<PyrBlockNode>((yyloc), args, nullptr, (yyvsp[0].node), false);
						auto *blocklit = allocParseNode<PyrSlotNode>((yyloc), PyrSlot::make(static_cast<void*>(block)), PyrParseNodeType::PushLitNode);

						if (auto* call = node_cast<PyrCallNode>((yyvsp[-2].slotNode)); call && call->mSelector->mSlot.getSymbol() == s_series) {
							call->mSelector->mSlot = PyrSlot::make(getsym("forSeries"));
							call->mArglist = linkNodes(call->mArglist, blocklit);
							(yyval.node) = call;
						} else {
							auto* selectornode = allocParseNode<PyrSlotNode>((yylsp[-2]), PyrSlot::make(getsym("do")));
							(yyval.node) = allocParseNode<PyrCallNode>((yyloc), selectornode, linkNodes((yyvsp[-1].node), blocklit), nullptr);
						}
					}
#line 2794 "lang11d_tab.cpp"
    break;

  case 87: /* qual: VAR name '=' exprseq nextqual  */
#line 471 "lang11d"
                                        {
						PyrSlot slot = PyrSlot::make(s_value);
						auto* selectornode = allocParseNode<PyrSlotNode>((yyloc), slot);
						auto* var = allocParseNode<PyrVarDefNode>((yyloc), (yyvsp[-3].slotNode), nullptr, ReadWriteAccessor::Private);
						auto* args = allocParseNode<PyrArgListNode>((yyloc), var, nullptr, nullptr);
						auto *block = allocParseNode<PyrBlockNode>((yyloc), args, nullptr, (yyvsp[0].node), false);
						auto *blocklit = allocParseNode<PyrSlotNode>((yyloc), PyrSlot::make(static_cast<void*>(block)), PyrParseNodeType::PushLitNode);
						(yyval.node) = allocParseNode<PyrCallNode>((yyloc), selectornode, linkNodes(blocklit, (yyvsp[-1].node)), nullptr);
					}
#line 2808 "lang11d_tab.cpp"
    break;

  case 88: /* qual: exprseq nextqual  */
#line 481 "lang11d"
                                        {
						auto* selectornode = allocParseNode<PyrSlotNode>((yyloc), PyrSlot::make(getsym("if")));
						auto* block = allocParseNode<PyrBlockNode>((yyloc), nullptr, nullptr, (yyvsp[0].node), false);
						auto* blocklit = allocParseNode<PyrSlotNode>((yyloc), PyrSlot::make(static_cast<void*>(block)), PyrParseNodeType::PushLitNode);
						(yyval.node) = allocParseNode<PyrCallNode>((yyloc), selectornode, linkNodes((yyvsp[-1].node), blocklit), nullptr);
					}
#line 2819 "lang11d_tab.cpp"
    break;

  case 89: /* qual: ':' ':' exprseq nextqual  */
#line 488 "lang11d"
                                        { (yyval.node) = allocParseNode<PyrDropNode>((yyloc), (yyvsp[-1].node), (yyvsp[0].node)); }
#line 2825 "lang11d_tab.cpp"
    break;

  case 90: /* qual: ':' WHILE exprseq nextqual  */
#line 490 "lang11d"
                                        {
						auto* selectornode1 = allocParseNode<PyrSlotNode>((yyloc), PyrSlot::make(getsym("alwaysYield")));
						auto* pushnil = allocParseNode<PyrSlotNode>((yyloc), PyrSlot{}, PyrParseNodeType::PushLitNode);
						auto* yieldNil = allocParseNode<PyrCallNode>((yyloc), selectornode1, pushnil, nullptr);
						auto* block1 = allocParseNode<PyrBlockNode>((yyloc), nullptr, nullptr, yieldNil, false);
						auto* blocklit1 = allocParseNode<PyrSlotNode>((yyloc), PyrSlot::make(static_cast<void*>(block1)), PyrParseNodeType::PushLitNode);
						auto* block2 = allocParseNode<PyrBlockNode>((yyloc), nullptr, nullptr, (yyvsp[0].node), false);
						auto* blocklit2 = allocParseNode<PyrSlotNode>((yyloc), PyrSlot::make(static_cast<void*>(block2)), PyrParseNodeType::PushLitNode);

						auto* selectornode2 = allocParseNode<PyrSlotNode>((yyloc), PyrSlot::make(getsym("if")));
						(yyval.node) = allocParseNode<PyrCallNode>((yyloc), selectornode2, linkNodes((yyvsp[-1].node), blocklit2, blocklit1), nullptr);
					}
#line 2842 "lang11d_tab.cpp"
    break;

  case 97: /* expr1: '(' exprseq ')'  */
#line 510 "lang11d"
                                        {
						PyrParseNode* node = (yyvsp[-1].node);
						node->mParens = true; // This means  (((((1))))), will not count the brackets, but just indicates they exist.
						node->location = (yyloc); // make the location include the brackets.
						(yyval.node) = (yyvsp[-1].node);
					}
#line 2853 "lang11d_tab.cpp"
    break;

  case 98: /* expr1: '~' name  */
#line 517 "lang11d"
                                        {
						auto* argnode = (yyvsp[0].slotNode)->changeLiteralType<PyrParseNodeType::PushLitNode>();
						auto* selectornode = allocParseNode<PyrSlotNode>((yyloc), PyrSlot::make(s_envirGet));
						(yyval.node) = allocParseNode<PyrCallNode>((yyloc), selectornode, argnode, nullptr);
					}
#line 2863 "lang11d_tab.cpp"
    break;

  case 99: /* expr1: '[' arrayelems ']'  */
#line 523 "lang11d"
                                        { (yyval.node) = allocParseNode<PyrDynListNode>((yyloc), nullptr, (yyvsp[-1].node)); }
#line 2869 "lang11d_tab.cpp"
    break;

  case 100: /* expr1: '(' valrange2 ')'  */
#line 525 "lang11d"
                                        { (yyval.node) = (yyvsp[-1].node); }
#line 2875 "lang11d_tab.cpp"
    break;

  case 101: /* expr1: '(' ':' valrange3 ')'  */
#line 527 "lang11d"
                                        { (yyval.node) = (yyvsp[-1].node); }
#line 2881 "lang11d_tab.cpp"
    break;

  case 102: /* expr1: '(' dictslotlist ')'  */
#line 529 "lang11d"
                                        { (yyval.node) = allocParseNode<PyrDynDictNode>((yyloc), (yyvsp[-1].node)); }
#line 2887 "lang11d_tab.cpp"
    break;

  case 103: /* expr1: expr1 '[' arglist1 ']'  */
#line 531 "lang11d"
                                        {
						auto* selectornode = allocParseNode<PyrSlotNode>((yyloc), PyrSlot::make(s_at));
						auto* args = linkNodes((yyvsp[-3].node), (yyvsp[-1].node));
						(yyval.node) = allocParseNode<PyrCallNode>((yyloc), selectornode, args, nullptr);
					}
#line 2897 "lang11d_tab.cpp"
    break;

  case 105: /* valrangex1: expr1 '[' arglist1 DOTDOT ']'  */
#line 539 "lang11d"
                                        {
						const int arglen = nodeListLength((yyvsp[-2].node));
						if (arglen > 2) {
							printErrorLine(**getActiveCodePointStream(), (yyvsp[-2].node)->location, "ArrayedCollection subrange has too many arguments, expected 2 or less.");
							compileErrors++;
						}
						auto* args = linkNodes((yyvsp[-4].node), (yyvsp[-2].node));
						if (arglen < 2) 
							args = linkNodes(args, allocParseNode<PyrSlotNode>((yyloc), PyrSlot{}, PyrParseNodeType::PushLitNode));

						args = linkNodes(args, allocParseNode<PyrSlotNode>((yyloc), PyrSlot{}, PyrParseNodeType::PushLitNode));
						auto* selectornode = allocParseNode<PyrSlotNode>((yyloc), PyrSlot::make(s_copyseries));
						(yyval.node) = allocParseNode<PyrCallNode>((yyloc), selectornode, args, nullptr);
					}
#line 2916 "lang11d_tab.cpp"
    break;

  case 106: /* valrangex1: expr1 '[' DOTDOT exprseq ']'  */
#line 554 "lang11d"
                                        {
						auto* nilnode1 = allocParseNode<PyrSlotNode>((yyloc), PyrSlot{}, PyrParseNodeType::PushLitNode);
						auto* args = linkNodes((yyvsp[-4].node), nilnode1);
						auto* nilnode2 = allocParseNode<PyrSlotNode>((yyloc), PyrSlot{}, PyrParseNodeType::PushLitNode);
						args = linkNodes(args, nilnode2);
						args = linkNodes(args, (yyvsp[-1].node));
						auto* selectornode = allocParseNode<PyrSlotNode>((yyloc), PyrSlot::make(s_copyseries));
						(yyval.node) = allocParseNode<PyrCallNode>((yyloc), selectornode, args, nullptr);
					}
#line 2930 "lang11d_tab.cpp"
    break;

  case 107: /* valrangex1: expr1 '[' arglist1 DOTDOT exprseq ']'  */
#line 564 "lang11d"
                                        {
						const int arglen = nodeListLength((yyvsp[-3].node));
						if (arglen > 2) {
							printErrorLine(**getActiveCodePointStream(), (yyvsp[-3].node)->location, "ArrayedCollection subrange has too many arguments, expected 2 or less.");
							compileErrors++;
						}

						auto* args = linkNodes((yyvsp[-5].node), (yyvsp[-3].node));
						if (arglen < 2) 
							args = linkNodes(args, allocParseNode<PyrSlotNode>((yyloc), PyrSlot{}, PyrParseNodeType::PushLitNode));
						
						args = linkNodes(args, (yyvsp[-1].node));
						auto* selectornode = allocParseNode<PyrSlotNode>((yyloc), PyrSlot::make(s_copyseries));
						(yyval.node) = allocParseNode<PyrCallNode>((yyloc), selectornode, args, nullptr);
					}
#line 2950 "lang11d_tab.cpp"
    break;

  case 108: /* valrangeassign: expr1 '[' arglist1 DOTDOT ']' '=' expr  */
#line 581 "lang11d"
                                        {
						const int arglen = nodeListLength((yyvsp[-4].node));
						if (arglen > 2) {
							error("ArrayedCollection subrange has too many arguments.\n");
							printErrorLine(**getActiveCodePointStream(), (yyvsp[-4].node)->location, "ArrayedCollection subrange has too many arguments, expected 2 or less.");
							compileErrors++;
						}

						auto* args = linkNodes((yyvsp[-6].node), (yyvsp[-4].node));
						if (arglen < 2) 
							args = linkNodes(args, allocParseNode<PyrSlotNode>((yyloc), PyrSlot{}, PyrParseNodeType::PushLitNode));
						
						args = linkNodes(args, allocParseNode<PyrSlotNode>((yyloc), PyrSlot{}, PyrParseNodeType::PushLitNode), (yyvsp[0].node));
						auto* selectornode = allocParseNode<PyrSlotNode>((yyloc), PyrSlot::make(s_putseries));
						(yyval.node) = allocParseNode<PyrCallNode>((yyloc), selectornode, args, nullptr);
					}
#line 2971 "lang11d_tab.cpp"
    break;

  case 109: /* valrangeassign: expr1 '[' DOTDOT exprseq ']' '=' expr  */
#line 598 "lang11d"
                                        {
						auto* args = linkNodes(
							(yyvsp[-6].node),
							allocParseNode<PyrSlotNode>((yyloc), PyrSlot{}, PyrParseNodeType::PushLitNode),
							allocParseNode<PyrSlotNode>((yyloc), PyrSlot{}, PyrParseNodeType::PushLitNode),
							(yyvsp[-3].node), 
							(yyvsp[0].node)
						);
						auto* selectornode = allocParseNode<PyrSlotNode>((yyloc), PyrSlot::make(s_putseries));
						(yyval.node) = allocParseNode<PyrCallNode>((yyloc), selectornode, args, nullptr);
					}
#line 2987 "lang11d_tab.cpp"
    break;

  case 110: /* valrangeassign: expr1 '[' arglist1 DOTDOT exprseq ']' '=' expr  */
#line 610 "lang11d"
                                        {
						const int arglen = nodeListLength((yyvsp[-5].node));
						if (arglen > 2) {
							error("ArrayedCollection subrange has too many arguments.\n");
							printErrorLine(**getActiveCodePointStream(), (yyvsp[-5].node)->location, "ArrayedCollection subrange has too many arguments, expected 2 or less.");
							compileErrors++;
						}

						auto* args = linkNodes((yyvsp[-7].node), (yyvsp[-5].node));
						if (arglen < 2) 
							args = linkNodes(args, allocParseNode<PyrSlotNode>((yyloc), PyrSlot{}, PyrParseNodeType::PushLitNode));
						
						args = linkNodes(args, (yyvsp[-3].node), (yyvsp[0].node));
						auto* selectornode = allocParseNode<PyrSlotNode>((yyloc), PyrSlot::make(s_putseries));
						(yyval.node) = allocParseNode<PyrCallNode>((yyloc), selectornode, args, nullptr);
					}
#line 3008 "lang11d_tab.cpp"
    break;

  case 111: /* valrangexd: expr '.' '[' arglist1 DOTDOT ']'  */
#line 633 "lang11d"
                                        {
						PyrSlotNode* nilnode1, *nilnode2;
						PyrSlot selectorSlot, nilSlot;
						PyrParseNode* args;

						const int arglen = nodeListLength((yyvsp[-2].node));
						if (arglen > 2) {
							printErrorLine(**getActiveCodePointStream(), (yyvsp[-2].node)->location, "ArrayedCollection subrange has too many arguments, expected 2 or less.");
							compileErrors++;
						}

						args = linkNodes((yyvsp[-5].node), (yyvsp[-2].node));
						if (arglen < 2) 
							args = linkNodes(args, allocParseNode<PyrSlotNode>((yyloc), nilSlot, PyrParseNodeType::PushLitNode));

						args = linkNodes(args, allocParseNode<PyrSlotNode>((yyloc), PyrSlot{}, PyrParseNodeType::PushLitNode));

						auto* selectornode = allocParseNode<PyrSlotNode>((yyloc), PyrSlot::make(s_copyseries));
						(yyval.node) = allocParseNode<PyrCallNode>((yyloc), selectornode, args, nullptr);
					}
#line 3033 "lang11d_tab.cpp"
    break;

  case 112: /* valrangexd: expr '.' '[' DOTDOT exprseq ']'  */
#line 654 "lang11d"
                                        {
						auto* args = linkNodes(
							(yyvsp[-5].node),
							allocParseNode<PyrSlotNode>((yyloc), PyrSlot{}, PyrParseNodeType::PushLitNode),
							allocParseNode<PyrSlotNode>((yyloc), PyrSlot{}, PyrParseNodeType::PushLitNode),
							(yyvsp[-1].node)
						);
						auto* selectornode = allocParseNode<PyrSlotNode>((yyloc), PyrSlot::make(s_copyseries));
						(yyval.node) = allocParseNode<PyrCallNode>((yyloc), selectornode, args, nullptr);
					}
#line 3048 "lang11d_tab.cpp"
    break;

  case 113: /* valrangexd: expr '.' '[' arglist1 DOTDOT exprseq ']'  */
#line 665 "lang11d"
                                        {
						const int arglen = nodeListLength((yyvsp[-3].node));
						if (arglen > 2) {
							printErrorLine(**getActiveCodePointStream(), (yyvsp[-3].node)->location, "ArrayedCollection subrange has too many arguments, expected 2 or less.");
							compileErrors++;
						}

						auto* args = linkNodes((yyvsp[-6].node), (yyvsp[-3].node));
						if (arglen < 2) 
							args = linkNodes(args, allocParseNode<PyrSlotNode>((yyloc), PyrSlot{}, PyrParseNodeType::PushLitNode));
						
						args = linkNodes(args, (yyvsp[-1].node));
						auto* selectornode = allocParseNode<PyrSlotNode>((yyloc), PyrSlot::make(s_copyseries));
						(yyval.node) = allocParseNode<PyrCallNode>((yyloc), selectornode, args, nullptr);
					}
#line 3068 "lang11d_tab.cpp"
    break;

  case 114: /* valrangexd: expr '.' '[' arglist1 DOTDOT ']' '=' expr  */
#line 681 "lang11d"
                                        {
						const int arglen = nodeListLength((yyvsp[-4].node));
						if (arglen > 2) {
							printErrorLine(**getActiveCodePointStream(), (yyvsp[-4].node)->location, "ArrayedCollection subrange has too many arguments, expected 2 or less.");
							compileErrors++;
						}

						auto* args = linkNodes((yyvsp[-7].node), (yyvsp[-4].node));
						if (arglen < 2) 
							args = linkNodes(args, allocParseNode<PyrSlotNode>((yyloc), PyrSlot{}, PyrParseNodeType::PushLitNode));
						
						args = linkNodes(args, allocParseNode<PyrSlotNode>((yyloc), PyrSlot{}, PyrParseNodeType::PushLitNode), (yyvsp[0].node));
						auto* selectornode = allocParseNode<PyrSlotNode>((yyloc), PyrSlot::make(s_putseries));
						(yyval.node) = allocParseNode<PyrCallNode>((yyloc), selectornode, args, nullptr);
					}
#line 3088 "lang11d_tab.cpp"
    break;

  case 115: /* valrangexd: expr '.' '[' DOTDOT exprseq ']' '=' expr  */
#line 697 "lang11d"
                                        {
						auto* args = linkNodes((yyvsp[-7].node), allocParseNode<PyrSlotNode>((yyloc), PyrSlot{}, PyrParseNodeType::PushLitNode));
						args = linkNodes(args, allocParseNode<PyrSlotNode>((yyloc), PyrSlot{}, PyrParseNodeType::PushLitNode));
						args = linkNodes(args, (yyvsp[-3].node));
						args = linkNodes(args, (yyvsp[0].node));
						auto* selectornode = allocParseNode<PyrSlotNode>((yyloc), PyrSlot::make(s_putseries));
						(yyval.node) = allocParseNode<PyrCallNode>((yyloc), selectornode, args, nullptr);
					}
#line 3101 "lang11d_tab.cpp"
    break;

  case 116: /* valrangexd: expr '.' '[' arglist1 DOTDOT exprseq ']' '=' expr  */
#line 706 "lang11d"
                                        {
						const int arglen = nodeListLength((yyvsp[-5].node));
						if (arglen > 2) {
							printErrorLine(**getActiveCodePointStream(), (yyvsp[-5].node)->location, "ArrayedCollection subrange has too many arguments, expected 2 or less.");
							compileErrors++;
						}

						auto* args = linkNodes((yyvsp[-8].node), (yyvsp[-5].node));
						if (arglen < 2) 
							args = linkNodes(args, allocParseNode<PyrSlotNode>((yyloc), PyrSlot{}, PyrParseNodeType::PushLitNode));
						
						args = linkNodes(args, (yyvsp[-3].node), (yyvsp[0].node));
						auto* selectornode = allocParseNode<PyrSlotNode>((yyloc), PyrSlot::make(s_putseries));
						(yyval.node) = allocParseNode<PyrCallNode>((yyloc), selectornode, args, nullptr);
					}
#line 3121 "lang11d_tab.cpp"
    break;

  case 117: /* valrange2: exprseq DOTDOT  */
#line 723 "lang11d"
                                        {
						// if this is not used in a 'do' or list comprehension, then should return an error.
						auto* args = linkNodes((yyvsp[-1].node), allocParseNode<PyrSlotNode>((yylsp[0]), PyrSlot{}, PyrParseNodeType::PushLitNode));
						args = linkNodes(args, allocParseNode<PyrSlotNode>((yylsp[0]), PyrSlot{}, PyrParseNodeType::PushLitNode));
						auto* selectornode = allocParseNode<PyrSlotNode>((yyloc), PyrSlot::make(s_series));
						(yyval.node) = allocParseNode<PyrCallNode>((yyloc), selectornode, args, nullptr);
					}
#line 3133 "lang11d_tab.cpp"
    break;

  case 118: /* valrange2: DOTDOT exprseq  */
#line 731 "lang11d"
                                        {
						auto* args = linkNodes(
							allocParseNode<PyrSlotNode>((yylsp[-1]), PyrSlot::make(0), PyrParseNodeType::PushLitNode),
							allocParseNode<PyrSlotNode>((yylsp[-1]), PyrSlot{}, PyrParseNodeType::PushLitNode),
							(yyvsp[0].node)
						);
						auto* selectornode = allocParseNode<PyrSlotNode>((yyloc), PyrSlot::make(s_series));
						(yyval.node) = allocParseNode<PyrCallNode>((yyloc), selectornode, args, nullptr);
					}
#line 3147 "lang11d_tab.cpp"
    break;

  case 119: /* valrange2: exprseq DOTDOT exprseq  */
#line 741 "lang11d"
                                        {
						auto* args = linkNodes((yyvsp[-2].node), allocParseNode<PyrSlotNode>((yyloc), PyrSlot{}, PyrParseNodeType::PushLitNode), (yyvsp[0].node));
						auto* selectornode = allocParseNode<PyrSlotNode>((yyloc), PyrSlot::make(s_series));
						(yyval.node) = allocParseNode<PyrCallNode>((yyloc), selectornode, args, nullptr);
					}
#line 3157 "lang11d_tab.cpp"
    break;

  case 120: /* valrange2: exprseq ',' exprseq DOTDOT exprseq  */
#line 747 "lang11d"
                                        {
						auto* selectornode = allocParseNode<PyrSlotNode>((yyloc), PyrSlot::make(s_series));
						(yyval.node) = allocParseNode<PyrCallNode>((yyloc), selectornode, linkNodes((yyvsp[-4].node), (yyvsp[-2].node), (yyvsp[0].node)), nullptr);
					}
#line 3166 "lang11d_tab.cpp"
    break;

  case 121: /* valrange2: exprseq ',' exprseq DOTDOT  */
#line 752 "lang11d"
                                        {
						// if this is not used in a 'do' or list comprehension, then should return an error.
						auto* args = linkNodes((yyvsp[-3].node), (yyvsp[-1].node), allocParseNode<PyrSlotNode>((yyloc), PyrSlot{}, PyrParseNodeType::PushLitNode));
						auto* selectornode = allocParseNode<PyrSlotNode>((yyloc), PyrSlot::make(s_series));
						(yyval.node) = allocParseNode<PyrCallNode>((yyloc), selectornode, args, nullptr);
					}
#line 3177 "lang11d_tab.cpp"
    break;

  case 122: /* valrange3: DOTDOT exprseq  */
#line 760 "lang11d"
                                        {
						auto* args = linkNodes(
							allocParseNode<PyrSlotNode>((yylsp[-1]), PyrSlot::make(0), PyrParseNodeType::PushLitNode),
							allocParseNode<PyrSlotNode>((yylsp[-1]), PyrSlot{}, PyrParseNodeType::PushLitNode),
							(yyvsp[0].node)
						);
						auto* selectornode = allocParseNode<PyrSlotNode>((yyloc), PyrSlot::make(getsym("seriesIter")));
						(yyval.node) = allocParseNode<PyrCallNode>((yyloc), selectornode, args, nullptr);
					}
#line 3191 "lang11d_tab.cpp"
    break;

  case 123: /* valrange3: exprseq DOTDOT  */
#line 770 "lang11d"
                                        {
						auto* args = linkNodes(
							(yyvsp[-1].node),
							allocParseNode<PyrSlotNode>((yylsp[-1]), PyrSlot{}, PyrParseNodeType::PushLitNode),
							allocParseNode<PyrSlotNode>((yylsp[-1]), PyrSlot{}, PyrParseNodeType::PushLitNode)
						);
						auto* selectornode = allocParseNode<PyrSlotNode>((yyloc), PyrSlot::make(getsym("seriesIter")));
						(yyval.node) = allocParseNode<PyrCallNode>((yyloc), selectornode, args, nullptr);
					}
#line 3205 "lang11d_tab.cpp"
    break;

  case 124: /* valrange3: exprseq DOTDOT exprseq  */
#line 781 "lang11d"
                                        {
						auto* args = linkNodes(
							(yyvsp[-2].node),
							allocParseNode<PyrSlotNode>((yylsp[-1]), PyrSlot{}, PyrParseNodeType::PushLitNode),
							(yyvsp[0].node)
						);
						auto* selectornode = allocParseNode<PyrSlotNode>((yyloc), PyrSlot::make(getsym("seriesIter")));
						(yyval.node) = allocParseNode<PyrCallNode>((yyloc), selectornode, args, nullptr);
					}
#line 3219 "lang11d_tab.cpp"
    break;

  case 125: /* valrange3: exprseq ',' exprseq DOTDOT  */
#line 791 "lang11d"
                                        {
						auto* args = linkNodes(
							(yyvsp[-3].node),
							(yyvsp[-1].node),
							allocParseNode<PyrSlotNode>((yylsp[-2]), PyrSlot{}, PyrParseNodeType::PushLitNode)
						);
						auto* selectornode = allocParseNode<PyrSlotNode>((yyloc), PyrSlot::make(getsym("seriesIter")));
						(yyval.node) = allocParseNode<PyrCallNode>((yyloc), selectornode, args, nullptr);
					}
#line 3233 "lang11d_tab.cpp"
    break;

  case 126: /* valrange3: exprseq ',' exprseq DOTDOT exprseq  */
#line 801 "lang11d"
                                        {
						auto* selectornode = allocParseNode<PyrSlotNode>((yyloc), PyrSlot::make(getsym("seriesIter")));
						(yyval.node) = allocParseNode<PyrCallNode>((yyloc), selectornode, linkNodes((yyvsp[-4].node), (yyvsp[-2].node), (yyvsp[0].node)), nullptr);
					}
#line 3242 "lang11d_tab.cpp"
    break;

  case 130: /* expr: CLASSNAME  */
#line 810 "lang11d"
                                        { (yyval.node) = (yyvsp[0].slotNode)->changeLiteralType<PyrParseNodeType::PushNameNode>(); }
#line 3248 "lang11d_tab.cpp"
    break;

  case 131: /* expr: expr '.' '[' arglist1 ']'  */
#line 812 "lang11d"
                                        {
						auto* selectornode = allocParseNode<PyrSlotNode>((yyloc), PyrSlot::make(s_at));
						(yyval.node) = allocParseNode<PyrCallNode>((yyloc), selectornode, linkNodes((yyvsp[-4].node), (yyvsp[-1].node)), nullptr);
					}
#line 3257 "lang11d_tab.cpp"
    break;

  case 132: /* expr: '`' expr  */
#line 817 "lang11d"
                                        {
						auto* args = linkNodes(
							allocParseNode<PyrSlotNode>((yylsp[-1]), PyrSlot::make(s_ref), PyrParseNodeType::PushNameNode),
							(yyvsp[0].node)
						);
						auto* selectornode = allocParseNode<PyrSlotNode>((yyloc), PyrSlot::make(s_new));
						(yyval.node) = allocParseNode<PyrCallNode>((yyloc), selectornode, args, nullptr);
					}
#line 3270 "lang11d_tab.cpp"
    break;

  case 133: /* expr: expr binop2 adverb expr  */
#line 826 "lang11d"
                                        {
						(yyvsp[-3].node)->mNext = (yyvsp[0].node);
						(yyvsp[0].node)->mNext = (yyvsp[-1].node);
						(yyval.node) = allocParseNode<PyrBinopCallNode>((yyloc), (yyvsp[-2].slotNode), (yyvsp[-3].node));
					}
#line 3280 "lang11d_tab.cpp"
    break;

  case 134: /* expr: name '=' expr  */
#line 832 "lang11d"
                                        { (yyval.node) = allocParseNode<PyrAssignNode>((yyloc), (yyvsp[-2].slotNode), (yyvsp[0].node)); }
#line 3286 "lang11d_tab.cpp"
    break;

  case 135: /* expr: '~' name '=' expr  */
#line 834 "lang11d"
                                        {
						auto* args = linkNodes((yyvsp[-2].slotNode)->changeLiteralType<PyrParseNodeType::PushLitNode>(), (yyvsp[0].node));
						auto* selectornode = allocParseNode<PyrSlotNode>((yyloc), PyrSlot::make(s_envirPut));
						(yyval.node) = allocParseNode<PyrCallNode>((yyloc), selectornode, args, nullptr);
					}
#line 3296 "lang11d_tab.cpp"
    break;

  case 136: /* expr: expr '.' name '=' expr  */
#line 840 "lang11d"
                                        { (yyval.node) = allocParseNode<PyrSetterNode>((yyloc), (yyvsp[-2].slotNode), (yyvsp[-4].node), (yyvsp[0].node)); }
#line 3302 "lang11d_tab.cpp"
    break;

  case 137: /* expr: name '(' arglist1 optkeyarglist ')' '=' expr  */
#line 842 "lang11d"
                                        {
						if ((yyvsp[-3].node) != nullptr) {
							printErrorLine(**getActiveCodePointStream(), (yyvsp[-3].node)->location, "Setter method called with keyword arguments");
							compileErrors++;
						}
						(yyval.node) = allocParseNode<PyrSetterNode>((yyloc), (yyvsp[-6].slotNode), (yyvsp[-4].node), (yyvsp[0].node));
					}
#line 3314 "lang11d_tab.cpp"
    break;

  case 138: /* expr: '#' mavars '=' expr  */
#line 850 "lang11d"
                                        { (yyval.node) = allocParseNode<PyrMultiAssignNode>((yyloc), (yyvsp[-2].multiAssignListNode), (yyvsp[0].node)); }
#line 3320 "lang11d_tab.cpp"
    break;

  case 139: /* expr: expr1 '[' arglist1 ']' '=' expr  */
#line 852 "lang11d"
                                        {
						auto* selectornode = allocParseNode<PyrSlotNode>((yyloc), PyrSlot::make(s_put));
						(yyval.node) = allocParseNode<PyrCallNode>((yyloc), selectornode, linkNodes((yyvsp[-5].node), (yyvsp[-3].node), (yyvsp[0].node)), nullptr);
					}
#line 3329 "lang11d_tab.cpp"
    break;

  case 140: /* expr: expr '.' '[' arglist1 ']' '=' expr  */
#line 857 "lang11d"
                                        {
						auto* selectornode = allocParseNode<PyrSlotNode>((yyloc), PyrSlot::make(s_put));
						(yyval.node) = allocParseNode<PyrCallNode>((yyloc), selectornode, linkNodes((yyvsp[-6].node), (yyvsp[-3].node), (yyvsp[0].node)), nullptr);
					}
#line 3338 "lang11d_tab.cpp"
    break;

  case 141: /* adverb: %empty  */
#line 862 "lang11d"
                                  { (yyval.node) = nullptr; }
#line 3344 "lang11d_tab.cpp"
    break;

  case 142: /* adverb: '.' name  */
#line 863 "lang11d"
                                           { (yyval.node) = (yyvsp[0].slotNode)->changeLiteralType<PyrParseNodeType::PushLitNode>(); }
#line 3350 "lang11d_tab.cpp"
    break;

  case 143: /* adverb: '.' integer  */
#line 864 "lang11d"
                                              { (yyval.node) = (yyvsp[0].slotNode)->changeLiteralType<PyrParseNodeType::PushLitNode>(); }
#line 3356 "lang11d_tab.cpp"
    break;

  case 144: /* adverb: '.' '(' exprseq ')'  */
#line 865 "lang11d"
                                                      { (yyval.node) = (yyvsp[-1].node); }
#line 3362 "lang11d_tab.cpp"
    break;

  case 146: /* exprn: exprn ';' expr  */
#line 869 "lang11d"
                                        { (yyval.node) = allocParseNode<PyrDropNode>((yyloc), (yyvsp[-2].node), (yyvsp[0].node)); }
#line 3368 "lang11d_tab.cpp"
    break;

  case 148: /* arrayelems: %empty  */
#line 873 "lang11d"
                                 { (yyval.node) = nullptr; }
#line 3374 "lang11d_tab.cpp"
    break;

  case 149: /* arrayelems: arrayelems1 optcomma  */
#line 874 "lang11d"
                                                       { (yyval.node) = (yyvsp[-1].node); }
#line 3380 "lang11d_tab.cpp"
    break;

  case 151: /* arrayelems1: exprseq ':' exprseq  */
#line 878 "lang11d"
                                        { (yyval.node) = linkNodes((yyvsp[-2].node), (yyvsp[0].node)); }
#line 3386 "lang11d_tab.cpp"
    break;

  case 152: /* arrayelems1: KEYBINOP exprseq  */
#line 880 "lang11d"
                                        { (yyval.node) = linkNodes( (yyvsp[-1].slotNode)->changeLiteralType<PyrParseNodeType::PushLitNode>(), (yyvsp[0].node)); }
#line 3392 "lang11d_tab.cpp"
    break;

  case 153: /* arrayelems1: arrayelems1 ',' exprseq  */
#line 882 "lang11d"
                                                { (yyval.node) = linkNodes((yyvsp[-2].node), (yyvsp[0].node)); }
#line 3398 "lang11d_tab.cpp"
    break;

  case 154: /* arrayelems1: arrayelems1 ',' KEYBINOP exprseq  */
#line 884 "lang11d"
                                        { (yyval.node) = linkNodes((yyvsp[-3].node), (yyvsp[-1].slotNode)->changeLiteralType<PyrParseNodeType::PushLitNode>(), (yyvsp[0].node)); }
#line 3404 "lang11d_tab.cpp"
    break;

  case 155: /* arrayelems1: arrayelems1 ',' exprseq ':' exprseq  */
#line 886 "lang11d"
                                        { (yyval.node) = linkNodes((yyvsp[-4].node), (yyvsp[-2].node), (yyvsp[0].node)); }
#line 3410 "lang11d_tab.cpp"
    break;

  case 157: /* arglist1: arglist1 ',' exprseq  */
#line 890 "lang11d"
                                        { (yyval.node) = linkNodes((yyvsp[-2].node), (yyvsp[0].node)); }
#line 3416 "lang11d_tab.cpp"
    break;

  case 158: /* arglistv1: '*' exprseq  */
#line 893 "lang11d"
                                        { (yylsp[0]) = (yyloc); (yyval.node) = (yyvsp[0].node); }
#line 3422 "lang11d_tab.cpp"
    break;

  case 159: /* arglistv1: arglist1 ',' '*' exprseq  */
#line 895 "lang11d"
                                                { (yyval.node) = linkNodes((yyvsp[-3].node), (yyvsp[0].node)); }
#line 3428 "lang11d_tab.cpp"
    break;

  case 161: /* keyarglist1: keyarglist1 ',' keyarg  */
#line 899 "lang11d"
                                                { (yyval.node) = linkNodes((yyvsp[-2].node), (yyvsp[0].node)); }
#line 3434 "lang11d_tab.cpp"
    break;

  case 162: /* keyarg: KEYBINOP exprseq  */
#line 902 "lang11d"
                                        { (yyval.node) = allocParseNode<PyrPushKeyArgNode>((yyloc), (yyvsp[-1].slotNode), (yyvsp[0].node)); }
#line 3440 "lang11d_tab.cpp"
    break;

  case 163: /* optkeyarglist: optcomma  */
#line 904 "lang11d"
                           { (yyval.node) = nullptr; }
#line 3446 "lang11d_tab.cpp"
    break;

  case 164: /* optkeyarglist: ',' keyarglist1 optcomma  */
#line 905 "lang11d"
                                                           { (yyval.node) = (yyvsp[-1].node); }
#line 3452 "lang11d_tab.cpp"
    break;

  case 165: /* mavars: nameList  */
#line 908 "lang11d"
                                        { (yyval.multiAssignListNode) = allocParseNode<PyrMultiAssignVarListNode>((yyloc), (yyvsp[0].slotNode), nullptr); }
#line 3458 "lang11d_tab.cpp"
    break;

  case 166: /* mavars: nameList ELLIPSIS name  */
#line 910 "lang11d"
                                        { (yyval.multiAssignListNode) = allocParseNode<PyrMultiAssignVarListNode>((yyloc), (yyvsp[-2].slotNode), (yyvsp[0].slotNode)); }
#line 3464 "lang11d_tab.cpp"
    break;

  case 168: /* nameList: nameList ',' name  */
#line 914 "lang11d"
                                        { (yyval.slotNode) = linkNodes((yyvsp[-2].slotNode), (yyvsp[0].slotNode)); }
#line 3470 "lang11d_tab.cpp"
    break;

  case 169: /* slotliteral: integer  */
#line 916 "lang11d"
                                { (yyval.node) = (yyvsp[0].slotNode)->changeLiteralType<PyrParseNodeType::LiteralNode>(); }
#line 3476 "lang11d_tab.cpp"
    break;

  case 170: /* slotliteral: floatp  */
#line 917 "lang11d"
                                                { (yyval.node) = (yyvsp[0].slotNode)->changeLiteralType<PyrParseNodeType::LiteralNode>(); }
#line 3482 "lang11d_tab.cpp"
    break;

  case 171: /* slotliteral: ASCII  */
#line 918 "lang11d"
                                                { (yyval.node) = (yyvsp[0].slotNode)->changeLiteralType<PyrParseNodeType::LiteralNode>(); }
#line 3488 "lang11d_tab.cpp"
    break;

  case 172: /* slotliteral: STRING  */
#line 919 "lang11d"
                                                { (yyval.node) = (yyvsp[0].slotNode)->changeLiteralType<PyrParseNodeType::LiteralNode>(); }
#line 3494 "lang11d_tab.cpp"
    break;

  case 173: /* slotliteral: SYMBOL  */
#line 920 "lang11d"
                                                { (yyval.node) = (yyvsp[0].slotNode)->changeLiteralType<PyrParseNodeType::LiteralNode>(); }
#line 3500 "lang11d_tab.cpp"
    break;

  case 174: /* slotliteral: TRUEOBJ  */
#line 921 "lang11d"
                                                { (yyval.node) = (yyvsp[0].slotNode)->changeLiteralType<PyrParseNodeType::LiteralNode>(); }
#line 3506 "lang11d_tab.cpp"
    break;

  case 175: /* slotliteral: FALSEOBJ  */
#line 922 "lang11d"
                                                { (yyval.node) = (yyvsp[0].slotNode)->changeLiteralType<PyrParseNodeType::LiteralNode>(); }
#line 3512 "lang11d_tab.cpp"
    break;

  case 176: /* slotliteral: NILOBJ  */
#line 923 "lang11d"
                                                { (yyval.node) = (yyvsp[0].slotNode)->changeLiteralType<PyrParseNodeType::LiteralNode>(); }
#line 3518 "lang11d_tab.cpp"
    break;

  case 177: /* slotliteral: listlit  */
#line 924 "lang11d"
                                                { (yyval.node) = allocParseNode<PyrSlotNode>((yyloc), PyrSlot::make(static_cast<void*>((yyvsp[0].node))), PyrParseNodeType::LiteralNode); }
#line 3524 "lang11d_tab.cpp"
    break;

  case 178: /* blockliteral: block  */
#line 926 "lang11d"
                        { (yyval.node) = allocParseNode<PyrSlotNode>((yyloc), PyrSlot::make(static_cast<void*>((yyvsp[0].node))), PyrParseNodeType::PushLitNode); }
#line 3530 "lang11d_tab.cpp"
    break;

  case 179: /* pushname: name  */
#line 928 "lang11d"
                               { (yyval.node) = (yyvsp[0].slotNode)->changeLiteralType<PyrParseNodeType::PushNameNode>(); }
#line 3536 "lang11d_tab.cpp"
    break;

  case 180: /* pushliteral: integer  */
#line 930 "lang11d"
                                        { (yyval.node) = (yyvsp[0].slotNode)->changeLiteralType<PyrParseNodeType::PushLitNode>(); }
#line 3542 "lang11d_tab.cpp"
    break;

  case 181: /* pushliteral: floatp  */
#line 931 "lang11d"
                                                { (yyval.node) = (yyvsp[0].slotNode)->changeLiteralType<PyrParseNodeType::PushLitNode>(); }
#line 3548 "lang11d_tab.cpp"
    break;

  case 182: /* pushliteral: ASCII  */
#line 932 "lang11d"
                                                { (yyval.node) = (yyvsp[0].slotNode)->changeLiteralType<PyrParseNodeType::PushLitNode>(); }
#line 3554 "lang11d_tab.cpp"
    break;

  case 183: /* pushliteral: STRING  */
#line 933 "lang11d"
                                                { (yyval.node) = (yyvsp[0].slotNode)->changeLiteralType<PyrParseNodeType::PushLitNode>(); }
#line 3560 "lang11d_tab.cpp"
    break;

  case 184: /* pushliteral: SYMBOL  */
#line 934 "lang11d"
                                                { (yyval.node) = (yyvsp[0].slotNode)->changeLiteralType<PyrParseNodeType::PushLitNode>(); }
#line 3566 "lang11d_tab.cpp"
    break;

  case 185: /* pushliteral: TRUEOBJ  */
#line 935 "lang11d"
                                                { (yyval.node) = (yyvsp[0].slotNode)->changeLiteralType<PyrParseNodeType::PushLitNode>(); }
#line 3572 "lang11d_tab.cpp"
    break;

  case 186: /* pushliteral: FALSEOBJ  */
#line 936 "lang11d"
                                                { (yyval.node) = (yyvsp[0].slotNode)->changeLiteralType<PyrParseNodeType::PushLitNode>(); }
#line 3578 "lang11d_tab.cpp"
    break;

  case 187: /* pushliteral: NILOBJ  */
#line 937 "lang11d"
                                                { (yyval.node) = (yyvsp[0].slotNode)->changeLiteralType<PyrParseNodeType::PushLitNode>(); }
#line 3584 "lang11d_tab.cpp"
    break;

  case 188: /* pushliteral: listlit  */
#line 938 "lang11d"
                                                { (yyval.node) = allocParseNode<PyrSlotNode>((yyloc), PyrSlot::make(static_cast<void*>((yyvsp[0].node))), PyrParseNodeType::PushLitNode); }
#line 3590 "lang11d_tab.cpp"
    break;

  case 189: /* listliteral: integer  */
#line 940 "lang11d"
                                        { (yyval.node) = (yyvsp[0].slotNode)->changeLiteralType<PyrParseNodeType::LiteralNode>();}
#line 3596 "lang11d_tab.cpp"
    break;

  case 190: /* listliteral: floatp  */
#line 941 "lang11d"
                                                { (yyval.node) = (yyvsp[0].slotNode)->changeLiteralType<PyrParseNodeType::LiteralNode>(); }
#line 3602 "lang11d_tab.cpp"
    break;

  case 191: /* listliteral: ASCII  */
#line 942 "lang11d"
                                                { (yyval.node) = (yyvsp[0].slotNode)->changeLiteralType<PyrParseNodeType::LiteralNode>(); }
#line 3608 "lang11d_tab.cpp"
    break;

  case 192: /* listliteral: STRING  */
#line 943 "lang11d"
                                                { (yyval.node) = (yyvsp[0].slotNode)->changeLiteralType<PyrParseNodeType::LiteralNode>(); }
#line 3614 "lang11d_tab.cpp"
    break;

  case 193: /* listliteral: SYMBOL  */
#line 944 "lang11d"
                                                { (yyval.node) = (yyvsp[0].slotNode)->changeLiteralType<PyrParseNodeType::LiteralNode>(); }
#line 3620 "lang11d_tab.cpp"
    break;

  case 194: /* listliteral: name  */
#line 945 "lang11d"
                                                { (yyval.node) = (yyvsp[0].slotNode)->changeLiteralType<PyrParseNodeType::LiteralNode>(); }
#line 3626 "lang11d_tab.cpp"
    break;

  case 195: /* listliteral: TRUEOBJ  */
#line 946 "lang11d"
                                                { (yyval.node) = (yyvsp[0].slotNode)->changeLiteralType<PyrParseNodeType::LiteralNode>(); }
#line 3632 "lang11d_tab.cpp"
    break;

  case 196: /* listliteral: FALSEOBJ  */
#line 947 "lang11d"
                                                { (yyval.node) = (yyvsp[0].slotNode)->changeLiteralType<PyrParseNodeType::LiteralNode>(); }
#line 3638 "lang11d_tab.cpp"
    break;

  case 197: /* listliteral: NILOBJ  */
#line 948 "lang11d"
                                                { (yyval.node) = (yyvsp[0].slotNode)->changeLiteralType<PyrParseNodeType::LiteralNode>(); }
#line 3644 "lang11d_tab.cpp"
    break;

  case 198: /* listliteral: listlit2  */
#line 949 "lang11d"
                                                { (yyval.node) = allocParseNode<PyrSlotNode>((yyloc), PyrSlot::make(static_cast<void*>((yyvsp[0].node))), PyrParseNodeType::LiteralNode); }
#line 3650 "lang11d_tab.cpp"
    break;

  case 199: /* block: '{' argdecls funcvardecls funcbody '}'  */
#line 952 "lang11d"
                                        { (yyval.node) = allocParseNode<PyrBlockNode>((yyloc), (yyvsp[-3].argListNode), (yyvsp[-2].varListNode), (yyvsp[-1].node), false); }
#line 3656 "lang11d_tab.cpp"
    break;

  case 200: /* block: BEGINCLOSEDFUNC argdecls funcvardecls funcbody '}'  */
#line 954 "lang11d"
                                        { (yyval.node) = allocParseNode<PyrBlockNode>((yyloc), (yyvsp[-3].argListNode), (yyvsp[-2].varListNode), (yyvsp[-1].node), true); }
#line 3662 "lang11d_tab.cpp"
    break;

  case 201: /* funcvardecls: %empty  */
#line 957 "lang11d"
                         { (yyval.varListNode) = nullptr; }
#line 3668 "lang11d_tab.cpp"
    break;

  case 202: /* funcvardecls: funcvardecls funcvardecl  */
#line 959 "lang11d"
                                        { (yyval.varListNode) = linkNodes((yyvsp[-1].varListNode), (yyvsp[0].varListNode)); }
#line 3674 "lang11d_tab.cpp"
    break;

  case 204: /* funcvardecls1: funcvardecls1 funcvardecl  */
#line 963 "lang11d"
                                        { (yyval.varListNode) = linkNodes((yyvsp[-1].varListNode), (yyvsp[0].varListNode)); }
#line 3680 "lang11d_tab.cpp"
    break;

  case 205: /* funcvardecl: VAR vardeflist ';'  */
#line 966 "lang11d"
                                        { (yyval.varListNode) = allocParseNode<PyrVarListNode>((yyloc), (yyvsp[-1].varDefNode), varLocal); }
#line 3686 "lang11d_tab.cpp"
    break;

  case 206: /* argdecls: %empty  */
#line 968 "lang11d"
                                 { (yyval.argListNode) = nullptr; }
#line 3692 "lang11d_tab.cpp"
    break;

  case 207: /* argdecls: ARG vardeflist ';'  */
#line 970 "lang11d"
                                        { (yyval.argListNode) = allocParseNode<PyrArgListNode>((yyloc), (yyvsp[-1].varDefNode), nullptr, nullptr); }
#line 3698 "lang11d_tab.cpp"
    break;

  case 208: /* argdecls: ARG vardeflist0 ELLIPSIS name ';'  */
#line 972 "lang11d"
                                        { (yyval.argListNode) = allocParseNode<PyrArgListNode>((yyloc), (yyvsp[-3].varDefNode), (yyvsp[-1].slotNode), nullptr); }
#line 3704 "lang11d_tab.cpp"
    break;

  case 209: /* argdecls: ARG vardeflist0 ELLIPSIS name ',' name ';'  */
#line 974 "lang11d"
                                        { (yyval.argListNode) = allocParseNode<PyrArgListNode>((yyloc), (yyvsp[-5].varDefNode), (yyvsp[-3].slotNode), (yyvsp[-1].slotNode)); }
#line 3710 "lang11d_tab.cpp"
    break;

  case 210: /* argdecls: '|' slotdeflist '|'  */
#line 976 "lang11d"
                                        { (yyval.argListNode) = allocParseNode<PyrArgListNode>((yyloc), (yyvsp[-1].varDefNode), nullptr, nullptr); }
#line 3716 "lang11d_tab.cpp"
    break;

  case 211: /* argdecls: '|' slotdeflist0 ELLIPSIS name '|'  */
#line 978 "lang11d"
                                        { (yyval.argListNode) = allocParseNode<PyrArgListNode>((yyloc), (yyvsp[-3].varDefNode), (yyvsp[-1].slotNode), nullptr); }
#line 3722 "lang11d_tab.cpp"
    break;

  case 212: /* argdecls: '|' slotdeflist0 ELLIPSIS name ',' name '|'  */
#line 980 "lang11d"
                                    { (yyval.argListNode) = allocParseNode<PyrArgListNode>((yyloc), (yyvsp[-5].varDefNode), (yyvsp[-3].slotNode), (yyvsp[-1].slotNode)); }
#line 3728 "lang11d_tab.cpp"
    break;

  case 213: /* argdecls1: ARG vardeflist ';'  */
#line 983 "lang11d"
                                        { (yyval.argListNode) = allocParseNode<PyrArgListNode>((yyloc), (yyvsp[-1].varDefNode), nullptr, nullptr); }
#line 3734 "lang11d_tab.cpp"
    break;

  case 214: /* argdecls1: ARG vardeflist0 ELLIPSIS name ';'  */
#line 985 "lang11d"
                                        { (yyval.argListNode) = allocParseNode<PyrArgListNode>((yyloc), (yyvsp[-3].varDefNode), (yyvsp[-1].slotNode), nullptr); }
#line 3740 "lang11d_tab.cpp"
    break;

  case 215: /* argdecls1: ARG vardeflist0 ELLIPSIS name ',' name ';'  */
#line 987 "lang11d"
                                        { (yyval.argListNode) = allocParseNode<PyrArgListNode>((yyloc), (yyvsp[-5].varDefNode), (yyvsp[-3].slotNode), (yyvsp[-1].slotNode)); }
#line 3746 "lang11d_tab.cpp"
    break;

  case 216: /* argdecls1: '|' slotdeflist '|'  */
#line 989 "lang11d"
                                        { (yyval.argListNode) = allocParseNode<PyrArgListNode>((yyloc), (yyvsp[-1].varDefNode), nullptr, nullptr); }
#line 3752 "lang11d_tab.cpp"
    break;

  case 217: /* argdecls1: '|' slotdeflist0 ELLIPSIS name '|'  */
#line 991 "lang11d"
                                        { (yyval.argListNode) = allocParseNode<PyrArgListNode>((yyloc), (yyvsp[-3].varDefNode), (yyvsp[-1].slotNode), nullptr); }
#line 3758 "lang11d_tab.cpp"
    break;

  case 218: /* argdecls1: '|' slotdeflist0 ELLIPSIS name ',' name '|'  */
#line 993 "lang11d"
                                    { (yyval.argListNode) = allocParseNode<PyrArgListNode>((yyloc), (yyvsp[-5].varDefNode), (yyvsp[-3].slotNode), (yyvsp[-1].slotNode)); }
#line 3764 "lang11d_tab.cpp"
    break;

  case 220: /* constdeflist: constdeflist optcomma constdef  */
#line 997 "lang11d"
                                        { (yyval.varDefNode) = linkNodes((yyvsp[-2].varDefNode), (yyvsp[0].varDefNode)); }
#line 3770 "lang11d_tab.cpp"
    break;

  case 221: /* constdef: rspec name '=' slotliteral  */
#line 1000 "lang11d"
                                        { (yyval.varDefNode) = allocParseNode<PyrVarDefNode>((yyloc), (yyvsp[-2].slotNode), (yyvsp[0].node), (yyvsp[-3].rwAccessor)); }
#line 3776 "lang11d_tab.cpp"
    break;

  case 222: /* slotdeflist0: %empty  */
#line 1002 "lang11d"
                         { (yyval.varDefNode) = nullptr; }
#line 3782 "lang11d_tab.cpp"
    break;

  case 225: /* slotdeflist: slotdeflist optcomma slotdef  */
#line 1007 "lang11d"
                                        { (yyval.varDefNode) = linkNodes((yyvsp[-2].varDefNode), (yyvsp[0].varDefNode)); }
#line 3788 "lang11d_tab.cpp"
    break;

  case 226: /* slotdef: name  */
#line 1010 "lang11d"
                                        { (yyval.varDefNode) = allocParseNode<PyrVarDefNode>((yyloc), (yyvsp[0].slotNode), nullptr, ReadWriteAccessor::Private); }
#line 3794 "lang11d_tab.cpp"
    break;

  case 227: /* slotdef: name optequal slotliteral  */
#line 1012 "lang11d"
                                        { (yyval.varDefNode) = allocParseNode<PyrVarDefNode>((yyloc), (yyvsp[-2].slotNode), (yyvsp[0].node), ReadWriteAccessor::Private); }
#line 3800 "lang11d_tab.cpp"
    break;

  case 228: /* slotdef: name optequal '(' exprseq ')'  */
#line 1014 "lang11d"
                                        {
						PyrParseNode* node = (yyvsp[-1].node);
						node->mParens = 1;
						(yyval.varDefNode) = allocParseNode<PyrVarDefNode>((yyloc), (yyvsp[-4].slotNode), node, ReadWriteAccessor::Private);
					}
#line 3810 "lang11d_tab.cpp"
    break;

  case 229: /* vardeflist0: %empty  */
#line 1020 "lang11d"
                          { (yyval.varDefNode) = nullptr; }
#line 3816 "lang11d_tab.cpp"
    break;

  case 232: /* vardeflist: vardeflist ',' vardef  */
#line 1025 "lang11d"
                                        { (yyval.varDefNode) = linkNodes((yyvsp[-2].varDefNode), (yyvsp[0].varDefNode)); }
#line 3822 "lang11d_tab.cpp"
    break;

  case 233: /* vardef: name  */
#line 1028 "lang11d"
                                        { (yyval.varDefNode) = allocParseNode<PyrVarDefNode>((yyloc), (yyvsp[0].slotNode), nullptr, ReadWriteAccessor::Private); }
#line 3828 "lang11d_tab.cpp"
    break;

  case 234: /* vardef: name '=' expr  */
#line 1030 "lang11d"
                                        { (yyval.varDefNode) = allocParseNode<PyrVarDefNode>((yyloc), (yyvsp[-2].slotNode), (yyvsp[0].node), ReadWriteAccessor::Private); }
#line 3834 "lang11d_tab.cpp"
    break;

  case 235: /* vardef: name '(' exprseq ')'  */
#line 1032 "lang11d"
                                        {
						PyrParseNode* node = (yyvsp[-1].node);
						node->mParens = 1;
						(yyval.varDefNode) = allocParseNode<PyrVarDefNode>((yyloc), (yyvsp[-3].slotNode), node, ReadWriteAccessor::Private);
					}
#line 3844 "lang11d_tab.cpp"
    break;

  case 236: /* dictslotdef: exprseq ':' exprseq  */
#line 1039 "lang11d"
                                        { (yyval.node) = linkNodes((yyvsp[-2].node), (yyvsp[0].node)); }
#line 3850 "lang11d_tab.cpp"
    break;

  case 237: /* dictslotdef: KEYBINOP exprseq  */
#line 1041 "lang11d"
                                        { (yyval.node) = linkNodes((yyvsp[-1].slotNode)->changeLiteralType<PyrParseNodeType::PushLitNode>(), (yyvsp[0].node)); }
#line 3856 "lang11d_tab.cpp"
    break;

  case 239: /* dictslotlist1: dictslotlist1 ',' dictslotdef  */
#line 1046 "lang11d"
                                        { (yyval.node) = linkNodes((yyvsp[-2].node), (yyvsp[0].node)); }
#line 3862 "lang11d_tab.cpp"
    break;

  case 240: /* dictslotlist: %empty  */
#line 1048 "lang11d"
                         { (yyval.node) = nullptr; }
#line 3868 "lang11d_tab.cpp"
    break;

  case 243: /* rwslotdeflist: rwslotdeflist ',' rwslotdef  */
#line 1053 "lang11d"
                                        { (yyval.varDefNode) = linkNodes((yyvsp[-2].varDefNode), (yyvsp[0].varDefNode)); }
#line 3874 "lang11d_tab.cpp"
    break;

  case 244: /* rwslotdef: rwspec name  */
#line 1056 "lang11d"
                                        { (yyval.varDefNode) = allocParseNode<PyrVarDefNode>((yyloc), (yyvsp[0].slotNode), nullptr, (yyvsp[-1].rwAccessor)); }
#line 3880 "lang11d_tab.cpp"
    break;

  case 245: /* rwslotdef: rwspec name '=' slotliteral  */
#line 1058 "lang11d"
                                        { (yyval.varDefNode) = allocParseNode<PyrVarDefNode>((yyloc), (yyvsp[-2].slotNode), (yyvsp[0].node), (yyvsp[-3].rwAccessor)); }
#line 3886 "lang11d_tab.cpp"
    break;

  case 246: /* listlit: '#' '[' literallistc ']'  */
#line 1061 "lang11d"
                                        { (yyval.node) = allocParseNode<PyrLitListNode>((yyloc), nullptr, (yyvsp[-1].node)); }
#line 3892 "lang11d_tab.cpp"
    break;

  case 247: /* listlit: '#' CLASSNAME '[' literallistc ']'  */
#line 1063 "lang11d"
                                                { (yyval.node) = allocParseNode<PyrLitListNode>((yyloc), (yyvsp[-3].slotNode), (yyvsp[-1].node)); }
#line 3898 "lang11d_tab.cpp"
    break;

  case 248: /* listlit2: '[' literallistc ']'  */
#line 1066 "lang11d"
                                        { (yyval.node) = allocParseNode<PyrLitListNode>((yyloc), nullptr, (yyvsp[-1].node)); }
#line 3904 "lang11d_tab.cpp"
    break;

  case 249: /* listlit2: CLASSNAME '[' literallistc ']'  */
#line 1068 "lang11d"
                                        { (yyval.node) = allocParseNode<PyrLitListNode>((yyloc), (yyvsp[-3].slotNode), (yyvsp[-1].node)); }
#line 3910 "lang11d_tab.cpp"
    break;

  case 250: /* literallistc: %empty  */
#line 1070 "lang11d"
                         { (yyval.node) = nullptr; }
#line 3916 "lang11d_tab.cpp"
    break;

  case 253: /* literallist1: literallist1 ',' listliteral  */
#line 1075 "lang11d"
                                        { (yyval.node) = linkNodes((yyvsp[-2].node), (yyvsp[0].node)); }
#line 3922 "lang11d_tab.cpp"
    break;

  case 254: /* rwspec: %empty  */
#line 1077 "lang11d"
                                  { (yyval.rwAccessor) = ReadWriteAccessor::Private; }
#line 3928 "lang11d_tab.cpp"
    break;

  case 255: /* rwspec: '<'  */
#line 1078 "lang11d"
                                      { (yyval.rwAccessor) = ReadWriteAccessor::Read; }
#line 3934 "lang11d_tab.cpp"
    break;

  case 256: /* rwspec: READWRITEVAR  */
#line 1079 "lang11d"
                                               { (yyval.rwAccessor) = ReadWriteAccessor::ReadWrite; }
#line 3940 "lang11d_tab.cpp"
    break;

  case 257: /* rwspec: '>'  */
#line 1080 "lang11d"
                                      { (yyval.rwAccessor) = ReadWriteAccessor::Write; }
#line 3946 "lang11d_tab.cpp"
    break;

  case 258: /* rspec: %empty  */
#line 1082 "lang11d"
                                 { (yyval.rwAccessor) = ReadWriteAccessor::Private; }
#line 3952 "lang11d_tab.cpp"
    break;

  case 259: /* rspec: '<'  */
#line 1083 "lang11d"
                                      { (yyval.rwAccessor) = ReadWriteAccessor::Read; }
#line 3958 "lang11d_tab.cpp"
    break;

  case 261: /* integer: '-' INTEGER  */
#line 1087 "lang11d"
                                        {
						const auto v = (yyvsp[0].slotNode)->mSlot.getInt();
						(yyvsp[0].slotNode)->mSlot = PyrSlot::make(-v);
						(yyval.slotNode) = (yyvsp[0].slotNode);
					}
#line 3968 "lang11d_tab.cpp"
    break;

  case 263: /* floatr: '-' SC_FLOAT  */
#line 1095 "lang11d"
                                        {
						const double v = (yyvsp[0].slotNode)->mSlot.getDouble();
						(yyvsp[0].slotNode)->mSlot = PyrSlot::make(-v);
						(yyval.slotNode) = (yyvsp[0].slotNode);
					}
#line 3978 "lang11d_tab.cpp"
    break;

  case 265: /* accidental: '-' ACCIDENTAL  */
#line 1103 "lang11d"
                                        {
						const double in = (yyvsp[0].slotNode)->mSlot.getDouble();
						const double intval = floor(in + 0.5);
						const double fracval = in - intval;
						(yyvsp[0].slotNode)->mSlot = PyrSlot::make(-intval + fracval);
						(yyval.slotNode) = (yyvsp[0].slotNode);
					}
#line 3990 "lang11d_tab.cpp"
    break;

  case 268: /* floatp: floatr PIE  */
#line 1114 "lang11d"
                                        {
						(yyvsp[-1].slotNode)->mSlot = PyrSlot::make((yyvsp[-1].slotNode)->mSlot.getDouble() * pi);
						(yyvsp[-1].slotNode)->location = (yyloc);
						(yyval.slotNode) = (yyvsp[-1].slotNode);
					}
#line 4000 "lang11d_tab.cpp"
    break;

  case 269: /* floatp: integer PIE  */
#line 1120 "lang11d"
                                        {
						(yyvsp[-1].slotNode)->mSlot = PyrSlot::make((yyvsp[-1].slotNode)->mSlot.getInt() * pi);
						(yyvsp[-1].slotNode)->location = (yyloc);
						(yyval.slotNode) = (yyvsp[-1].slotNode);
					}
#line 4010 "lang11d_tab.cpp"
    break;

  case 271: /* floatp: '-' PIE  */
#line 1127 "lang11d"
                                        {
						(yyvsp[0].slotNode)->mSlot = PyrSlot::make(-pi);
						(yyvsp[0].slotNode)->location = (yyloc);
						(yyval.slotNode) = (yyvsp[0].slotNode);
					}
#line 4020 "lang11d_tab.cpp"
    break;

  case 284: /* curryArg: CURRYARG  */
#line 1136 "lang11d"
                                   { (yyval.node) = allocParseNode<PyrCurryArgNode>((yyloc)); }
#line 4026 "lang11d_tab.cpp"
    break;


#line 4030 "lang11d_tab.cpp"

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

