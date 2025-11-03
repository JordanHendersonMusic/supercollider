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
#line 16 "lang11d"


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
  YYSYMBOL_44_ = 44,                       /* '{'  */
  YYSYMBOL_45_ = 45,                       /* '}'  */
  YYSYMBOL_46_ = 46,                       /* '['  */
  YYSYMBOL_47_ = 47,                       /* ']'  */
  YYSYMBOL_48_ = 48,                       /* ';'  */
  YYSYMBOL_49_ = 49,                       /* '#'  */
  YYSYMBOL_50_ = 50,                       /* ','  */
  YYSYMBOL_51_ = 51,                       /* '('  */
  YYSYMBOL_52_ = 52,                       /* ')'  */
  YYSYMBOL_53_ = 53,                       /* '^'  */
  YYSYMBOL_54_ = 54,                       /* '~'  */
  YYSYMBOL_YYACCEPT = 55,                  /* $accept  */
  YYSYMBOL_root = 56,                      /* root  */
  YYSYMBOL_classes = 57,                   /* classes  */
  YYSYMBOL_classextensions = 58,           /* classextensions  */
  YYSYMBOL_classdef = 59,                  /* classdef  */
  YYSYMBOL_classextension = 60,            /* classextension  */
  YYSYMBOL_optname = 61,                   /* optname  */
  YYSYMBOL_superclass = 62,                /* superclass  */
  YYSYMBOL_classvardecls = 63,             /* classvardecls  */
  YYSYMBOL_classvardecl = 64,              /* classvardecl  */
  YYSYMBOL_methods = 65,                   /* methods  */
  YYSYMBOL_methodMacro = 66,               /* methodMacro  */
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
  YYSYMBOL_keyarg = 101,                   /* keyarg  */
  YYSYMBOL_optkeyarglist = 102,            /* optkeyarglist  */
  YYSYMBOL_mavars = 103,                   /* mavars  */
  YYSYMBOL_mavarlist = 104,                /* mavarlist  */
  YYSYMBOL_slotliteral = 105,              /* slotliteral  */
  YYSYMBOL_blockliteral = 106,             /* blockliteral  */
  YYSYMBOL_pushname = 107,                 /* pushname  */
  YYSYMBOL_pushliteral = 108,              /* pushliteral  */
  YYSYMBOL_listliteral = 109,              /* listliteral  */
  YYSYMBOL_block = 110,                    /* block  */
  YYSYMBOL_funcvardecls = 111,             /* funcvardecls  */
  YYSYMBOL_funcvardecls1 = 112,            /* funcvardecls1  */
  YYSYMBOL_funcvardecl = 113,              /* funcvardecl  */
  YYSYMBOL_argdecls = 114,                 /* argdecls  */
  YYSYMBOL_argdecls1 = 115,                /* argdecls1  */
  YYSYMBOL_constdeflist = 116,             /* constdeflist  */
  YYSYMBOL_constdef = 117,                 /* constdef  */
  YYSYMBOL_slotdeflist0 = 118,             /* slotdeflist0  */
  YYSYMBOL_slotdeflist = 119,              /* slotdeflist  */
  YYSYMBOL_slotdef = 120,                  /* slotdef  */
  YYSYMBOL_vardeflist0 = 121,              /* vardeflist0  */
  YYSYMBOL_vardeflist = 122,               /* vardeflist  */
  YYSYMBOL_vardef = 123,                   /* vardef  */
  YYSYMBOL_dictslotdef = 124,              /* dictslotdef  */
  YYSYMBOL_dictslotlist1 = 125,            /* dictslotlist1  */
  YYSYMBOL_dictslotlist = 126,             /* dictslotlist  */
  YYSYMBOL_rwslotdeflist = 127,            /* rwslotdeflist  */
  YYSYMBOL_rwslotdef = 128,                /* rwslotdef  */
  YYSYMBOL_dictlit2 = 129,                 /* dictlit2  */
  YYSYMBOL_litdictslotdef = 130,           /* litdictslotdef  */
  YYSYMBOL_litdictslotlist1 = 131,         /* litdictslotlist1  */
  YYSYMBOL_litdictslotlist = 132,          /* litdictslotlist  */
  YYSYMBOL_listlit = 133,                  /* listlit  */
  YYSYMBOL_listlit2 = 134,                 /* listlit2  */
  YYSYMBOL_literallistc = 135,             /* literallistc  */
  YYSYMBOL_literallist1 = 136,             /* literallist1  */
  YYSYMBOL_rwspec = 137,                   /* rwspec  */
  YYSYMBOL_rspec = 138,                    /* rspec  */
  YYSYMBOL_integer = 139,                  /* integer  */
  YYSYMBOL_floatr = 140,                   /* floatr  */
  YYSYMBOL_accidental = 141,               /* accidental  */
  YYSYMBOL_pie = 142,                      /* pie  */
  YYSYMBOL_floatp = 143,                   /* floatp  */
  YYSYMBOL_name = 144,                     /* name  */
  YYSYMBOL_classname = 145,                /* classname  */
  YYSYMBOL_primname = 146,                 /* primname  */
  YYSYMBOL_trueobj = 147,                  /* trueobj  */
  YYSYMBOL_falseobj = 148,                 /* falseobj  */
  YYSYMBOL_nilobj = 149,                   /* nilobj  */
  YYSYMBOL_ascii = 150,                    /* ascii  */
  YYSYMBOL_symbol = 151,                   /* symbol  */
  YYSYMBOL_string = 152,                   /* string  */
  YYSYMBOL_pseudovar = 153,                /* pseudovar  */
  YYSYMBOL_binop = 154,                    /* binop  */
  YYSYMBOL_keybinop = 155,                 /* keybinop  */
  YYSYMBOL_binop2 = 156,                   /* binop2  */
  YYSYMBOL_curryarg = 157                  /* curryarg  */
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
#define YYLAST   1936

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  55
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  103
/* YYNRULES -- Number of rules.  */
#define YYNRULES  304
/* YYNSTATES -- Number of states.  */
#define YYNSTATES  571

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
       2,     2,     2,     2,     2,    49,     2,     2,     2,     2,
      51,    52,    37,    38,    50,    34,    41,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,    30,    48,
      35,    31,    36,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,    46,     2,    47,    53,     2,    42,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,    44,    39,    45,    54,     2,     2,     2,
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
       0,    39,    39,    41,    43,    47,    48,    52,    53,    57,
      61,    68,    74,    75,    78,    79,    83,    84,    88,    90,
      92,    96,    97,   101,   101,   103,   105,   107,   109,   113,
     114,   117,   118,   121,   122,   125,   126,   130,   132,   134,
     136,   138,   140,   142,   146,   147,   151,   152,   157,   158,
     163,   164,   168,   169,   175,   176,   179,   180,   183,   187,
     191,   195,   200,   204,   209,   227,   240,   242,   253,   264,
     275,   288,   309,   318,   327,   332,   346,   368,   372,   378,
     396,   402,   402,   412,   412,   419,   440,   444,   478,   516,
     530,   541,   545,   570,   571,   572,   573,   574,   575,   576,
     582,   592,   594,   596,   598,   600,   602,   615,   618,   645,
     663,   690,   718,   737,   765,   792,   810,   835,   863,   882,
     910,   929,   948,   965,   979,  1000,  1019,  1037,  1054,  1070,
    1086,  1087,  1088,  1089,  1090,  1103,  1117,  1122,  1126,  1137,
    1142,  1152,  1157,  1171,  1187,  1188,  1189,  1190,  1193,  1194,
    1200,  1203,  1204,  1208,  1209,  1211,  1216,  1218,  1225,  1233,
    1234,  1238,  1240,  1244,  1245,  1249,  1253,  1254,  1257,  1259,
    1263,  1264,  1269,  1270,  1271,  1272,  1273,  1274,  1275,  1276,
    1277,  1280,  1283,  1286,  1287,  1288,  1289,  1290,  1291,  1292,
    1293,  1294,  1297,  1298,  1299,  1300,  1301,  1302,  1303,  1304,
    1305,  1306,  1307,  1310,  1313,  1318,  1319,  1323,  1324,  1328,
    1332,  1333,  1337,  1341,  1345,  1349,  1355,  1359,  1363,  1367,
    1371,  1378,  1379,  1383,  1387,  1388,  1391,  1392,  1396,  1398,
    1400,  1408,  1409,  1412,  1413,  1417,  1419,  1421,  1429,  1431,
    1438,  1439,  1443,  1444,  1447,  1448,  1452,  1454,  1458,  1462,
    1464,  1471,  1472,  1476,  1477,  1482,  1484,  1488,  1490,  1494,
    1495,  1498,  1499,  1503,  1504,  1506,  1508,  1512,  1513,  1517,
    1518,  1527,  1528,  1537,  1538,  1549,  1552,  1553,  1554,  1560,
    1568,  1575,  1584,  1585,  1588,  1591,  1594,  1597,  1600,  1603,
    1606,  1609,  1612,  1615,  1616,  1617,  1618,  1619,  1620,  1621,
    1622,  1625,  1628,  1629,  1632
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
  "'{'", "'}'", "'['", "']'", "';'", "'#'", "','", "'('", "')'", "'^'",
  "'~'", "$accept", "root", "classes", "classextensions", "classdef",
  "classextension", "optname", "superclass", "classvardecls",
  "classvardecl", "methods", "methodMacro", "methoddef", "optsemi",
  "optcomma", "optequal", "funcbody", "cmdlinecode", "methbody",
  "primitive", "retval", "funretval", "blocklist1", "blocklistitem",
  "blocklist", "msgsend", "generator", "$@1", "$@2", "nextqual", "qual",
  "expr1", "valrangex1", "valrangeassign", "valrangexd", "valrange2",
  "valrange3", "expr", "adverb", "exprn", "exprseq", "arrayelems",
  "arrayelems1", "arglist1", "arglistv1", "keyarglist1", "keyarg",
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

#define YYPACT_NINF (-387)

#define yypact_value_is_default(Yyn) \
  ((Yyn) == YYPACT_NINF)

#define YYTABLE_NINF (-301)

#define yytable_value_is_error(Yyn) \
  0

/* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
   STATE-NUM.  */
static const yytype_int16 yypact[] =
{
     136,   917,    80,    31,    80,    63,  -387,  -387,  -387,  -387,
    -387,  -387,  -387,  -387,  -387,  -387,    58,    58,  -387,  -387,
    -387,  -387,  -387,    23,  -387,   275,    58,  1749,    38,  1385,
     152,   795,  1749,    58,  -387,  -387,  -387,  -387,  -387,    59,
    -387,  -387,  -387,  1840,    70,    74,  -387,  -387,  -387,  -387,
    1125,  -387,  1125,  -387,   112,   112,  -387,  -387,  -387,   203,
     206,  -387,  -387,  -387,  -387,  -387,  -387,  -387,  -387,   142,
    -387,  -387,    68,  -387,   -23,  -387,    62,   127,   117,    58,
      58,  -387,  -387,  -387,  -387,  -387,   140,    15,  -387,   214,
     864,  -387,  1749,  1749,  -387,  -387,   160,   135,   144,  1749,
    1837,   177,     7,  -387,   166,  1749,  1437,  -387,   275,  -387,
    -387,  -387,  -387,    11,  -387,   157,   104,  1125,  1125,  -387,
     169,   170,  -387,  1749,   173,  1888,   185,  1489,  -387,  -387,
      29,  -387,   229,  1749,  -387,  -387,  -387,  -387,  -387,  1125,
    -387,  -387,  1749,  1177,    73,  -387,  -387,  -387,  1385,   969,
      73,  -387,    80,    58,   233,  -387,    58,  1749,  1749,    58,
    -387,   234,   183,   257,   138,  1125,    58,  -387,  -387,    58,
    -387,   637,  -387,  -387,  1125,  1749,  -387,  1385,  -387,  -387,
    1837,  1801,  -387,  -387,  -387,   244,   237,   112,  -387,  -387,
     254,  -387,  -387,  -387,  -387,  -387,  -387,  1749,    58,    58,
    1837,  -387,  1749,   252,    16,  -387,  1749,  1749,  1749,  -387,
     255,   258,  1125,  1385,  -387,  -387,  -387,   242,  -387,  -387,
    1749,  1749,  -387,    17,  1541,  1021,   218,    99,  1749,  1840,
    -387,  1840,  1749,    73,   266,   270,  -387,   280,    73,   266,
     270,   281,  -387,  1749,   731,  -387,   291,  -387,  -387,  -387,
    1840,   285,   293,    58,  -387,    58,  -387,   297,  -387,   130,
    -387,    13,  1749,  -387,  -387,   112,  -387,  -387,  -387,  -387,
    -387,  -387,  -387,   294,   295,   298,  -387,   318,  1749,   304,
     323,  -387,   306,   308,  1837,  -387,  1837,  -387,  1837,  1840,
    -387,  -387,   316,  -387,  -387,  1749,  1749,  -387,  -387,   342,
    -387,  -387,   313,   339,  -387,  1749,  1229,    73,  1840,   324,
    1593,   341,  1749,  1749,    89,    73,   266,   270,   281,  1749,
    1073,    73,  -387,   366,  1749,  -387,  -387,   333,  -387,    73,
    1281,  -387,   326,   346,   328,  -387,  -387,   331,   332,   346,
     335,  -387,  1879,  -387,  -387,   340,   340,   358,   290,  -387,
    -387,   345,   161,  -387,  -387,    58,   343,  1333,  1333,  -387,
    1749,  -387,  -387,  1837,  1801,  -387,  -387,  -387,  -387,   347,
    -387,  -387,   378,  1749,  -387,    73,   266,   270,   373,   374,
     359,  1749,  -387,   360,  1645,   379,  -387,   361,   364,   365,
    1840,    73,   266,   270,   281,   368,  1749,   281,    -2,  -387,
      73,  -387,  -387,    73,   340,   340,    58,   381,   389,   392,
     201,   201,   387,  -387,  1855,  -387,  -387,    58,   398,  -387,
      58,    79,   393,   394,    45,   401,  -387,  -387,  -387,  -387,
    1749,  -387,    73,   396,   397,  1749,  1749,   408,  1840,   423,
     425,   410,  1749,    73,  -387,    73,  -387,   411,   417,   421,
    -387,  -387,  -387,  1749,  -387,  -387,  -387,   416,   418,  -387,
      23,    23,  -387,  -387,  -387,  -387,   215,  -387,    58,   223,
    -387,   240,  -387,    58,  -387,   442,  -387,   452,  1749,  1749,
    -387,  1333,  -387,  1749,   458,  -387,  -387,    73,  -387,  1840,
    1840,  1749,  1749,  1749,   456,  1840,  -387,  -387,    73,  -387,
      73,  1840,    23,    23,  -387,  -387,   290,  -387,   201,   457,
    -387,  -387,   387,   459,  -387,  1749,   394,   394,  -387,   394,
    1749,  -387,  1840,  1840,  1840,  1749,  -387,  -387,  -387,  -387,
      20,    20,  1867,  -387,   705,  -387,   705,   394,  -387,  -387,
    -387,   394,  1840,    20,    20,  -387,  1697,   441,  1697,  -387,
    -387,  -387,  -387,  -387,  1697,  1697,  1749,   447,  -387,   449,
    -387,   465,   466,   468,  1888,  -387,  -387,  -387,  -387,  -387,
    -387
};

/* YYDEFACT[STATE-NUM] -- Default reduction number in state STATE-NUM.
   Performed when YYTABLE does not specify something else to do.  Zero
   means the default is an error.  */
static const yytype_int16 yydefact[] =
{
       5,    50,     0,     0,     2,     3,     7,   282,   269,   271,
     273,   290,   291,   289,   284,   304,     0,   231,   288,   286,
     287,   292,   275,   210,   283,     0,   224,     0,   210,   151,
       0,   242,     0,     0,    43,     4,    35,    98,    95,   130,
     107,   132,   131,   148,    29,    50,    94,    96,    93,   181,
      50,   207,    50,   191,   183,   276,   277,   280,   184,   182,
     133,   188,   189,   190,   185,   187,   186,   105,    97,     0,
       1,     6,    14,     8,     0,   233,   235,     0,   232,   231,
     224,   205,   270,   272,   274,   281,     0,    31,   226,    33,
     242,   135,     0,     0,   205,   301,   153,     0,    31,     0,
     259,     0,   168,   170,     0,     0,     0,   293,   297,   295,
     296,   298,   299,   224,   294,     0,     0,    50,    50,   240,
      31,     0,   302,   303,     0,    29,   100,     0,   297,   300,
       0,   303,   144,    30,   150,    36,    42,   208,    41,    50,
     279,   278,     0,     0,    58,    52,    55,    54,   151,     0,
      67,    21,     0,    12,     0,   209,     0,     0,     0,     0,
     216,     0,   232,     0,    31,    50,     0,   218,    32,     0,
      34,     0,    81,    83,    50,     0,   101,    32,   152,   155,
     259,   253,   261,   202,   201,     0,    31,   192,   193,   197,
       0,   198,   199,   200,   194,   196,   195,     0,     0,     0,
     259,   121,     0,     0,     0,   102,   120,     0,     0,    99,
       0,     0,    50,    32,   243,   104,   239,     0,    30,    51,
       0,     0,   159,     0,     0,     0,    56,     0,     0,   149,
      40,   137,     0,     0,    31,    31,    53,     0,    56,    31,
      31,    31,   163,     0,     0,    15,     0,    13,    16,   234,
     236,     0,     0,     0,   211,     0,   213,     0,   206,     0,
     227,     0,     0,   229,   180,   172,   173,   177,   178,   179,
     174,   176,   175,     0,     0,     0,   154,   156,     0,     0,
       0,   251,    31,     0,     0,   255,    32,   260,   259,   141,
     169,   171,     0,   125,   103,   126,     0,   122,   238,     0,
      39,    38,     0,     0,   241,     0,     0,    59,   138,     0,
       0,   106,     0,     0,     0,    56,    31,    31,    31,     0,
       0,    57,    80,     0,     0,   146,   145,   136,   161,    60,
      32,   166,     0,    32,     0,    66,    68,     0,     0,    32,
       0,   165,   298,    11,    22,    23,    23,    14,    21,   237,
     217,     0,     0,   204,   219,     0,     0,     0,     0,   203,
       0,   157,   257,     0,    32,   254,   248,   250,   262,     0,
     256,   127,     0,   124,    37,     0,    31,    31,   109,   108,
       0,     0,   160,     0,     0,   134,    72,     0,     0,     0,
     139,    56,    31,    31,    31,     0,     0,    31,    56,    64,
      56,    71,   164,    56,    23,    23,     0,     0,     0,     0,
     263,   263,   267,    17,     0,   212,   214,     0,     0,   230,
       0,     0,     0,    85,   182,     0,   158,   249,   252,   258,
     128,   123,    62,     0,     0,     0,     0,   110,   142,   115,
     114,     0,     0,    56,    76,    56,    77,     0,     0,     0,
     147,   162,   167,     0,    61,    70,    69,     0,     0,    24,
     210,   210,    16,   264,   266,   265,     0,   244,     0,     0,
     268,    31,   221,     0,     9,     0,   220,     0,     0,     0,
      82,     0,    90,     0,     0,    84,   129,    56,    65,   112,
     111,     0,     0,     0,   116,   143,    75,    73,    56,    79,
      56,   140,   210,   210,   205,   205,    21,    19,   263,   246,
      18,    20,   267,     0,   215,     0,    85,    85,    86,    85,
       0,    63,   113,   118,   117,     0,    78,    74,   205,   205,
      46,    46,     0,   245,     0,   222,     0,    85,    92,    91,
      87,    85,   119,    46,    46,   285,    48,    29,    48,    10,
     247,   223,    89,    88,    48,    48,     0,     0,    44,    48,
      47,     0,     0,     0,    29,    25,    45,    27,    26,    28,
      49
};

/* YYPGOTO[NTERM-NUM].  */
static const yytype_int16 yypgoto[] =
{
    -387,  -387,  -387,  -387,  -387,   510,  -387,   171,    65,  -387,
    -335,  -320,  -387,  -124,   -42,  -387,   519,  -387,  -280,  -386,
     -39,   476,   -49,  -103,   163,  -387,   164,  -387,  -387,     9,
    -352,  -387,  -387,  -387,  -387,  -387,  -387,   -27,  -387,  -387,
     -11,   376,  -387,  -100,  -133,  -142,   191,    82,  -387,  -387,
    -204,   274,  -387,  -387,  -135,  -387,   -77,    -8,     3,   -26,
     502,  -387,    28,   461,   462,   363,   469,    -9,   395,   344,
    -387,  -387,   141,    60,  -387,   189,  -387,  -387,  -168,  -387,
    -165,  -387,  -387,  -387,   -81,  -387,  -387,    -4,   -40,   159,
     501,  -387,   -28,   196,   211,   250,   261,   419,  -387,  -235,
     527,   -19,  -387
};

/* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int16 yydefgoto[] =
{
       0,     3,     4,     5,    71,     6,   246,   154,   348,   413,
     244,   407,   344,   134,   331,   171,    34,    35,   557,   546,
     558,    36,   321,   145,   322,    37,    38,   273,   274,   482,
     422,    39,    40,    41,    42,   115,   203,    43,   228,    44,
      45,    97,    98,   223,   235,   397,   242,   332,   101,   102,
     263,    46,    47,    48,   182,    49,   165,    50,   258,    81,
      52,   471,   472,    86,    87,    88,    77,    74,    75,   119,
     120,   121,   466,   467,   183,   281,   282,   283,    53,   184,
     185,   186,   468,   473,    54,    55,    56,    57,    58,    59,
      60,   547,    61,    62,    63,    64,    65,    66,    67,   122,
     131,   132,    68
};

/* YYTABLE[YYPACT[STATE-NUM]] -- What to do in state STATE-NUM.  If
   positive, shift that token.  If negative, reduce the rule whose
   number is the opposite.  If YYTABLE_NINF, syntax error.  */
static const yytype_int16 yytable[] =
{
      91,   219,    94,   264,    51,   125,   425,   241,    78,   346,
     144,   150,   124,   414,     7,   279,   240,   174,    96,   187,
     116,    85,    23,   117,    14,   155,   408,   156,   198,   453,
     545,    70,     7,    16,    51,   292,  -225,    79,   295,   310,
      24,   236,    28,   234,   139,   169,   280,   236,     7,   239,
     140,   141,    79,   137,   167,    51,   178,   199,    24,   100,
     188,     7,    80,  -300,   311,   168,   296,   312,    92,    23,
     162,   124,   191,   483,    24,   224,   142,    80,   214,   116,
     225,   172,   173,   318,   457,   458,    93,    24,   179,    28,
     265,    14,   317,   157,   201,   204,   143,    23,   152,   187,
     187,     2,     7,     8,    85,   127,   229,   405,   478,   479,
     212,   384,   216,   158,   153,   231,   222,    28,   133,   187,
     137,    51,   169,   369,   314,   316,   206,    32,    24,   518,
     250,   266,   222,   323,   207,    22,   385,    96,   222,   312,
     188,   188,   137,   267,   287,   548,   325,   251,   159,   367,
     324,   368,   191,   191,   208,     7,   209,   554,   555,  -225,
     188,   166,     1,    14,   276,   160,   277,   156,   307,   354,
     289,   532,   191,   377,     2,    76,    76,   256,   394,   346,
     355,    24,   176,   140,   329,    89,   151,   393,   168,   103,
     175,   293,   126,   308,   177,   297,   298,   299,   100,   340,
     416,   327,   303,   187,   236,   187,   376,   187,   197,   205,
     309,   417,   200,   222,   222,   137,   220,  -228,   236,   213,
     392,   328,   215,   146,   146,   217,   236,    23,   427,   280,
      23,   254,   341,   156,   142,  -228,   463,   464,    76,    89,
     365,   465,    23,  -228,   188,   170,   188,    28,   188,   319,
      28,   356,   148,  -228,   143,   253,   191,   149,   191,   189,
     191,   140,    28,   507,  -228,   508,    23,   361,   561,   320,
     227,   510,    89,   508,   562,   563,   389,   248,   255,    82,
      83,    84,   187,   187,   371,   372,    28,   286,   511,   226,
     168,   285,   390,   306,   216,   222,   192,   346,    22,   380,
     288,   382,   383,   410,   294,   411,   412,   300,   146,   222,
     301,   193,   247,   395,   146,    76,   330,   334,   252,   382,
     333,   337,   338,   188,   188,   259,   432,   335,    89,   236,
     550,   339,   551,   147,   147,   191,   191,   349,   347,   189,
     189,   350,   353,   359,   357,   358,   423,   423,   360,   426,
     194,   362,   449,   363,   438,   452,   364,   290,   291,   189,
     366,   195,   431,   370,   373,   374,   264,   268,   264,   207,
      82,   378,   381,   441,   130,   144,   192,   192,   398,    95,
     399,   146,   269,   400,   401,   451,   326,   403,   152,   406,
     146,   193,   193,   415,   429,   419,   192,   146,   387,   388,
     430,   336,   146,   345,   435,   436,   437,   439,   489,   490,
     442,   193,   351,   443,   352,   495,   444,   445,   147,   486,
     450,   270,   470,   560,   147,   460,   501,   530,   531,   512,
     194,   194,   271,   461,   504,   505,   462,   476,   480,   491,
     570,   195,   195,   189,   481,   189,   485,   189,   487,   488,
     194,   543,   544,   265,   492,   265,   493,   494,   433,   434,
     502,   195,   503,   498,   522,   523,   524,   516,   517,   499,
     423,   146,   519,   500,   447,   448,   528,   529,   386,   146,
     192,   514,   192,   515,   192,   146,   520,   525,   534,   218,
     536,   147,   565,   146,   266,   193,   266,   193,   542,   193,
     147,   404,   556,    69,   537,    72,   267,   147,   267,   541,
     567,   568,   147,   569,   418,    73,   424,   424,   409,   196,
     566,   135,   189,   189,   237,   538,   539,   506,   540,   564,
     402,   104,   260,   118,   194,   559,   194,   559,   194,   146,
     535,   163,   164,   559,   559,   195,   552,   195,   161,   195,
     553,   249,   469,   428,   446,   146,    99,   304,   123,   192,
     192,   454,   146,   455,   146,   459,   456,   146,   533,   136,
       0,   138,     0,   345,   193,   193,   475,     0,     0,   477,
       0,   147,     0,   484,     0,     0,     0,     0,   146,   147,
     272,     0,     0,     0,     0,   147,   146,     0,     0,   196,
     196,   190,     0,   147,     0,     0,   496,   146,   497,   146,
       0,     0,     0,   194,   194,     0,     0,   123,     0,   196,
       0,     0,     0,     0,   195,   195,     0,   509,     0,     0,
       0,     0,   513,     0,     0,     0,   210,   211,     0,     0,
     424,     8,     9,    10,    11,    12,    13,     0,     0,   147,
     521,   146,     0,   245,    18,    19,    20,     0,   230,     0,
      22,   526,   146,   527,   146,   147,     0,     0,     0,     0,
       0,    25,   147,     0,   147,    99,   243,   147,     0,     0,
       0,   190,   190,     0,   257,     0,   261,     0,   262,     0,
       0,   345,     0,   275,     0,     0,     0,     0,   147,     0,
       0,   190,     0,   196,   278,   196,   147,   196,   284,     8,
       9,    10,    11,    12,    13,     0,     0,   147,     0,   147,
       0,     0,    18,    19,    20,     0,     0,     0,    22,     0,
     268,   302,   268,     0,     7,     0,     0,     0,     0,    25,
     305,     0,     0,     0,     0,   269,     0,   269,     0,     0,
       0,     0,   243,     0,   261,     0,     0,     0,     0,     0,
      24,   147,   104,   107,     0,   128,   109,   110,   342,   112,
     129,   114,   147,     0,   147,     0,   343,     0,     0,     0,
       0,     0,   196,   196,   270,   190,   270,   190,     0,   190,
       0,     0,     0,     0,     0,   271,     0,   271,     7,     8,
       9,    10,    11,    12,    13,     0,    14,    15,    16,    17,
       0,     0,    18,    19,    20,    21,     0,   105,    22,    23,
       0,     0,     0,     0,    24,   106,     0,   107,    95,   108,
     109,   110,   111,   112,   113,   114,     0,    27,     0,    28,
       0,    29,     0,     0,    30,     0,    90,   243,     0,    33,
       0,     0,     0,     0,     0,     0,     0,   243,     0,     0,
     243,     0,     0,     0,   190,   190,   243,     7,     8,     9,
      10,    11,    12,    13,     0,    14,    15,     0,     0,     0,
       0,    18,    19,    20,    21,     0,   105,    22,    23,     0,
       0,   284,     0,    24,   106,     0,   107,    95,   108,   109,
     110,   111,   112,   129,   114,     0,    27,     0,    28,     0,
      29,     0,     0,    30,     0,    90,     0,     0,    33,     0,
       7,     8,     9,    10,    11,    12,    13,     0,    14,    15,
      16,    17,     0,     0,    18,    19,    20,    21,     0,     0,
      22,    23,     0,     0,     0,     0,    24,     0,     0,     0,
       0,    25,     0,   272,     0,   272,    26,     0,     0,    27,
       0,    28,     0,    29,     0,     0,    30,     0,    31,     0,
      32,    33,     7,     8,     9,    10,    11,    12,    13,     0,
      14,    15,     0,     0,     0,     0,    18,    19,    20,    21,
       0,     0,    22,    23,     0,     0,     0,     0,    24,     0,
       0,     0,    95,    25,     0,     0,   232,     0,     0,     0,
       0,    27,     0,    28,     0,    29,     0,     0,    30,     0,
      90,   238,     0,    33,     7,     8,     9,    10,    11,    12,
      13,     0,    14,    15,     0,     0,     0,     0,    18,    19,
      20,    21,     0,     0,    22,    23,     0,     0,     0,     0,
      24,     0,     0,     0,    95,    25,     0,     0,   232,     0,
       0,     0,     0,    27,     0,    28,     0,    29,     0,     0,
      30,     0,    90,   315,     0,    33,     7,     8,     9,    10,
      11,    12,    13,     0,    14,    15,     0,     0,     0,     0,
      18,    19,    20,    21,     0,     0,    22,    23,     0,     0,
       0,     0,    24,     0,     0,     0,    95,    25,     0,     0,
     232,     0,     0,     0,     0,    27,     0,    28,     0,    29,
       0,     0,    30,     0,    90,   391,     0,    33,     7,     8,
       9,    10,    11,    12,    13,     0,    14,    15,    16,     0,
       0,     0,    18,    19,    20,    21,     0,     0,    22,    23,
       0,     0,     0,     0,    24,     0,     0,     0,     0,    25,
       0,     0,     0,     0,     0,     0,     0,    27,     0,    28,
       0,    29,     0,     0,    30,     0,    90,     0,    32,    33,
       7,     8,     9,    10,    11,    12,    13,     0,    14,    15,
       0,     0,     0,     0,    18,    19,    20,    21,     0,     0,
      22,    23,     0,     0,     0,     0,    24,     0,     0,     0,
       0,    25,     0,     0,   232,     0,     0,     0,     0,    27,
       0,    28,     0,    29,     0,     0,    30,     0,    90,   233,
       0,    33,     7,     8,     9,    10,    11,    12,    13,     0,
      14,    15,     0,     0,     0,     0,    18,    19,    20,    21,
       0,     0,    22,    23,     0,     0,     0,     0,    24,     0,
       0,     0,     0,    25,     0,     0,   232,     0,     0,     0,
       0,    27,     0,    28,     0,    29,     0,     0,    30,     0,
      90,   375,     0,    33,     7,     8,     9,    10,    11,    12,
      13,     0,    14,    15,     0,     0,     0,     0,    18,    19,
      20,    21,     0,     0,    22,    23,     0,     0,     0,     0,
      24,     0,     0,     0,    95,    25,     0,     0,   396,     0,
       0,     0,     0,    27,     0,    28,     0,    29,     0,     0,
      30,     0,    90,     0,     0,    33,     7,     8,     9,    10,
      11,    12,    13,     0,    14,    15,   420,     0,     0,     0,
      18,    19,    20,    21,     0,     0,    22,    23,     0,     0,
       0,     0,    24,   421,     0,     0,     0,    25,     0,     0,
       0,     0,     0,     0,     0,    27,     0,    28,     0,    29,
       0,     0,    30,     0,    90,     0,     0,    33,     7,     8,
       9,    10,    11,    12,    13,     0,    14,    15,     0,     0,
       0,     0,    18,    19,    20,    21,     0,     0,    22,    23,
       0,     0,     0,     0,    24,     0,     0,     0,    95,    25,
       0,     0,     0,     0,     0,     0,     0,    27,     0,    28,
       0,    29,     0,     0,    30,     0,    90,     0,     0,    33,
       7,     8,     9,    10,    11,    12,    13,     0,    14,    15,
       0,     0,     0,     0,    18,    19,    20,    21,     0,   202,
      22,    23,     0,     0,     0,     0,    24,     0,     0,     0,
       0,    25,     0,     0,     0,     0,     0,     0,     0,    27,
       0,    28,     0,    29,     0,     0,    30,     0,    90,     0,
       0,    33,     7,     8,     9,    10,    11,    12,    13,     0,
      14,    15,     0,     0,     0,     0,    18,    19,    20,    21,
       0,   221,    22,    23,     0,     0,     0,     0,    24,     0,
       0,     0,     0,    25,     0,     0,     0,     0,     0,     0,
       0,    27,     0,    28,     0,    29,     0,     0,    30,     0,
      90,     0,     0,    33,     7,     8,     9,    10,    11,    12,
      13,     0,    14,    15,     0,     0,     0,     0,    18,    19,
      20,    21,     0,   313,    22,    23,     0,     0,     0,     0,
      24,     0,     0,     0,     0,    25,     0,     0,     0,     0,
       0,     0,     0,    27,     0,    28,     0,    29,     0,     0,
      30,     0,    90,     0,     0,    33,     7,     8,     9,    10,
      11,    12,    13,     0,    14,    15,     0,     0,     0,     0,
      18,    19,    20,    21,     0,     0,    22,    23,     0,     0,
       0,     0,    24,     0,     0,     0,     0,    25,     0,     0,
       0,     0,     0,     0,     0,    27,     0,    28,     0,    29,
     379,     0,    30,     0,    90,     0,     0,    33,     7,     8,
       9,    10,    11,    12,    13,     0,    14,    15,     0,     0,
       0,     0,    18,    19,    20,    21,     0,     0,    22,    23,
       0,     0,     0,     0,    24,     0,     0,     0,     0,    25,
       0,     0,     0,     0,     0,     0,     0,    27,     0,    28,
       0,    29,   440,     0,    30,     0,    90,     0,     0,    33,
       7,     8,     9,    10,    11,    12,    13,     0,    14,    15,
       0,     0,     0,     0,    18,    19,    20,    21,     0,     0,
      22,    23,     0,     0,     0,     0,    24,     0,     0,     0,
       0,    25,     0,     0,     0,     0,     0,     0,     0,    27,
       0,    28,     0,    29,     0,     0,    30,     0,    90,     0,
     556,    33,     7,     8,     9,    10,    11,    12,    13,     0,
      14,    15,     0,     0,     0,     0,    18,    19,    20,    21,
       0,     0,    22,    23,     0,     0,     0,     0,    24,     0,
       0,     0,     0,    25,     0,     0,     0,     0,     0,     0,
       0,    27,     0,    28,     0,    29,     0,     0,    30,     0,
      90,     0,     0,    33,     7,     8,     9,    10,    11,    12,
      13,     0,    14,     0,     0,     0,     0,     0,    18,    19,
      20,     0,     0,     0,    22,     0,     0,     0,     0,     0,
      24,     0,     0,     0,    95,    25,     0,     0,     0,     0,
       7,     8,     9,    10,    11,    12,    13,   180,    14,     0,
       0,     0,   181,     0,    18,    19,    20,     0,     7,     0,
      22,     0,     0,     0,     0,     0,    24,     0,     0,     0,
       7,    25,   107,    95,   128,   109,   110,   111,   112,   129,
     114,   130,     7,   180,    24,     0,     0,   107,   181,   128,
     109,   110,   342,   112,   129,   114,    24,     0,     0,   107,
     474,   128,   109,   110,   342,   112,   129,   114,    24,     0,
       0,   107,   549,   128,   109,   110,   111,   112,   129,   114,
     107,    95,   128,   109,   110,   111,   112,   129,   114,   130,
       0,     0,     0,     0,     0,     0,   218
};

static const yytype_int16 yycheck[] =
{
      27,   125,    28,   171,     1,    32,   358,   149,    17,   244,
      59,    60,    31,   348,     3,   180,   149,    94,    29,   100,
      31,    25,    24,    31,    11,    48,   346,    50,    21,    31,
      10,     0,     3,    13,    31,   200,    21,    14,    22,    22,
      29,   144,    44,   143,    52,    87,   181,   150,     3,   149,
      54,    55,    14,    50,    39,    52,    98,    50,    29,    46,
     100,     3,    39,    52,    47,    50,    50,    50,    30,    24,
      79,    90,   100,    28,    29,    46,    31,    39,   120,    90,
      51,    92,    93,   225,   404,   405,    48,    29,    99,    44,
     171,    11,   225,    31,   105,   106,    51,    24,    30,   180,
     181,    38,     3,     4,   108,    46,   133,   342,    29,    30,
     118,    22,   123,    51,    46,   142,   127,    44,    48,   200,
     117,   118,   164,   288,   224,   225,    22,    53,    29,   481,
     157,   171,   143,    34,    30,    23,    47,   148,   149,    50,
     180,   181,   139,   171,   186,   531,   227,   158,    21,   284,
      51,   286,   180,   181,    50,     3,    52,   543,   544,    21,
     200,    21,    26,    11,   175,    48,   177,    50,   217,    39,
     197,   506,   200,   306,    38,    16,    17,    39,   320,   414,
      50,    29,    47,   187,   233,    26,    44,   320,    50,    30,
      30,   202,    33,   220,    50,   206,   207,   208,    46,   241,
      39,   228,   213,   284,   307,   286,   306,   288,    31,    52,
     221,    50,    46,   224,   225,   212,    31,     3,   321,    50,
     320,   232,    52,    59,    60,    52,   329,    24,   363,   364,
      24,    48,   243,    50,    31,    21,    35,    36,    79,    80,
     282,    40,    24,    29,   284,    31,   286,    44,   288,    31,
      44,   262,    46,    39,    51,    21,   284,    51,   286,   100,
     288,   265,    44,    48,    50,    50,    24,   278,   548,    51,
      41,    48,   113,    50,   554,   555,   318,    44,    21,     4,
       5,     6,   363,   364,   295,   296,    44,    50,    48,   130,
      50,    47,   319,    51,   305,   306,   100,   532,    23,   310,
      46,   312,   313,    13,    52,    15,    16,    52,   144,   320,
      52,   100,   153,   324,   150,   156,    50,   235,   159,   330,
      50,   239,   240,   363,   364,   166,   375,    47,   169,   432,
     534,    50,   536,    59,    60,   363,   364,    52,    47,   180,
     181,    48,    45,    45,    50,    50,   357,   358,    30,   360,
     100,    47,   394,    30,   381,   397,    50,   198,   199,   200,
      52,   100,   373,    47,    22,    52,   534,   171,   536,    30,
       4,    47,    31,   384,    41,   424,   180,   181,    52,    33,
      52,   217,   171,    52,    52,   396,   227,    52,    30,    49,
     226,   180,   181,    48,    47,    52,   200,   233,   316,   317,
      22,   238,   238,   244,    31,    31,    47,    47,   435,   436,
      31,   200,   253,    52,   255,   442,    52,    52,   144,   430,
      52,   171,    35,   547,   150,    44,   453,   504,   505,   471,
     180,   181,   171,    44,   460,   461,    44,    39,    45,    31,
     564,   180,   181,   284,    50,   286,    45,   288,    52,    52,
     200,   528,   529,   534,    31,   536,    31,    47,   376,   377,
      44,   200,    44,    52,   491,   492,   493,   478,   479,    52,
     481,   307,   483,    52,   392,   393,   502,   503,   315,   315,
     284,    39,   286,    31,   288,   321,    28,    31,    31,    48,
      31,   217,    45,   329,   534,   284,   536,   286,   525,   288,
     226,   342,    53,     2,   515,     4,   534,   233,   536,   520,
      45,    45,   238,    45,   355,     5,   357,   358,   347,   100,
     559,    45,   363,   364,   148,   516,   517,   462,   519,   556,
     339,    30,   169,    31,   284,   546,   286,   548,   288,   375,
     512,    80,    80,   554,   555,   284,   537,   286,    79,   288,
     541,   156,   411,   364,   391,   391,    29,   213,    31,   363,
     364,   398,   398,   400,   400,   406,   403,   403,   508,    50,
      -1,    52,    -1,   414,   363,   364,   417,    -1,    -1,   420,
      -1,   307,    -1,   424,    -1,    -1,    -1,    -1,   424,   315,
     171,    -1,    -1,    -1,    -1,   321,   432,    -1,    -1,   180,
     181,   100,    -1,   329,    -1,    -1,   443,   443,   445,   445,
      -1,    -1,    -1,   363,   364,    -1,    -1,    90,    -1,   200,
      -1,    -1,    -1,    -1,   363,   364,    -1,   468,    -1,    -1,
      -1,    -1,   473,    -1,    -1,    -1,   117,   118,    -1,    -1,
     481,     4,     5,     6,     7,     8,     9,    -1,    -1,   375,
     487,   487,    -1,   152,    17,    18,    19,    -1,   139,    -1,
      23,   498,   498,   500,   500,   391,    -1,    -1,    -1,    -1,
      -1,    34,   398,    -1,   400,   148,   149,   403,    -1,    -1,
      -1,   180,   181,    -1,   165,    -1,    49,    -1,    51,    -1,
      -1,   532,    -1,   174,    -1,    -1,    -1,    -1,   424,    -1,
      -1,   200,    -1,   284,   177,   286,   432,   288,   181,     4,
       5,     6,     7,     8,     9,    -1,    -1,   443,    -1,   445,
      -1,    -1,    17,    18,    19,    -1,    -1,    -1,    23,    -1,
     534,   212,   536,    -1,     3,    -1,    -1,    -1,    -1,    34,
     213,    -1,    -1,    -1,    -1,   534,    -1,   536,    -1,    -1,
      -1,    -1,   225,    -1,    49,    -1,    -1,    -1,    -1,    -1,
      29,   487,   261,    32,    -1,    34,    35,    36,    37,    38,
      39,    40,   498,    -1,   500,    -1,    45,    -1,    -1,    -1,
      -1,    -1,   363,   364,   534,   284,   536,   286,    -1,   288,
      -1,    -1,    -1,    -1,    -1,   534,    -1,   536,     3,     4,
       5,     6,     7,     8,     9,    -1,    11,    12,    13,    14,
      -1,    -1,    17,    18,    19,    20,    -1,    22,    23,    24,
      -1,    -1,    -1,    -1,    29,    30,    -1,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    -1,    42,    -1,    44,
      -1,    46,    -1,    -1,    49,    -1,    51,   320,    -1,    54,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   330,    -1,    -1,
     333,    -1,    -1,    -1,   363,   364,   339,     3,     4,     5,
       6,     7,     8,     9,    -1,    11,    12,    -1,    -1,    -1,
      -1,    17,    18,    19,    20,    -1,    22,    23,    24,    -1,
      -1,   364,    -1,    29,    30,    -1,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    -1,    42,    -1,    44,    -1,
      46,    -1,    -1,    49,    -1,    51,    -1,    -1,    54,    -1,
       3,     4,     5,     6,     7,     8,     9,    -1,    11,    12,
      13,    14,    -1,    -1,    17,    18,    19,    20,    -1,    -1,
      23,    24,    -1,    -1,    -1,    -1,    29,    -1,    -1,    -1,
      -1,    34,    -1,   534,    -1,   536,    39,    -1,    -1,    42,
      -1,    44,    -1,    46,    -1,    -1,    49,    -1,    51,    -1,
      53,    54,     3,     4,     5,     6,     7,     8,     9,    -1,
      11,    12,    -1,    -1,    -1,    -1,    17,    18,    19,    20,
      -1,    -1,    23,    24,    -1,    -1,    -1,    -1,    29,    -1,
      -1,    -1,    33,    34,    -1,    -1,    37,    -1,    -1,    -1,
      -1,    42,    -1,    44,    -1,    46,    -1,    -1,    49,    -1,
      51,    52,    -1,    54,     3,     4,     5,     6,     7,     8,
       9,    -1,    11,    12,    -1,    -1,    -1,    -1,    17,    18,
      19,    20,    -1,    -1,    23,    24,    -1,    -1,    -1,    -1,
      29,    -1,    -1,    -1,    33,    34,    -1,    -1,    37,    -1,
      -1,    -1,    -1,    42,    -1,    44,    -1,    46,    -1,    -1,
      49,    -1,    51,    52,    -1,    54,     3,     4,     5,     6,
       7,     8,     9,    -1,    11,    12,    -1,    -1,    -1,    -1,
      17,    18,    19,    20,    -1,    -1,    23,    24,    -1,    -1,
      -1,    -1,    29,    -1,    -1,    -1,    33,    34,    -1,    -1,
      37,    -1,    -1,    -1,    -1,    42,    -1,    44,    -1,    46,
      -1,    -1,    49,    -1,    51,    52,    -1,    54,     3,     4,
       5,     6,     7,     8,     9,    -1,    11,    12,    13,    -1,
      -1,    -1,    17,    18,    19,    20,    -1,    -1,    23,    24,
      -1,    -1,    -1,    -1,    29,    -1,    -1,    -1,    -1,    34,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    42,    -1,    44,
      -1,    46,    -1,    -1,    49,    -1,    51,    -1,    53,    54,
       3,     4,     5,     6,     7,     8,     9,    -1,    11,    12,
      -1,    -1,    -1,    -1,    17,    18,    19,    20,    -1,    -1,
      23,    24,    -1,    -1,    -1,    -1,    29,    -1,    -1,    -1,
      -1,    34,    -1,    -1,    37,    -1,    -1,    -1,    -1,    42,
      -1,    44,    -1,    46,    -1,    -1,    49,    -1,    51,    52,
      -1,    54,     3,     4,     5,     6,     7,     8,     9,    -1,
      11,    12,    -1,    -1,    -1,    -1,    17,    18,    19,    20,
      -1,    -1,    23,    24,    -1,    -1,    -1,    -1,    29,    -1,
      -1,    -1,    -1,    34,    -1,    -1,    37,    -1,    -1,    -1,
      -1,    42,    -1,    44,    -1,    46,    -1,    -1,    49,    -1,
      51,    52,    -1,    54,     3,     4,     5,     6,     7,     8,
       9,    -1,    11,    12,    -1,    -1,    -1,    -1,    17,    18,
      19,    20,    -1,    -1,    23,    24,    -1,    -1,    -1,    -1,
      29,    -1,    -1,    -1,    33,    34,    -1,    -1,    37,    -1,
      -1,    -1,    -1,    42,    -1,    44,    -1,    46,    -1,    -1,
      49,    -1,    51,    -1,    -1,    54,     3,     4,     5,     6,
       7,     8,     9,    -1,    11,    12,    13,    -1,    -1,    -1,
      17,    18,    19,    20,    -1,    -1,    23,    24,    -1,    -1,
      -1,    -1,    29,    30,    -1,    -1,    -1,    34,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    42,    -1,    44,    -1,    46,
      -1,    -1,    49,    -1,    51,    -1,    -1,    54,     3,     4,
       5,     6,     7,     8,     9,    -1,    11,    12,    -1,    -1,
      -1,    -1,    17,    18,    19,    20,    -1,    -1,    23,    24,
      -1,    -1,    -1,    -1,    29,    -1,    -1,    -1,    33,    34,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    42,    -1,    44,
      -1,    46,    -1,    -1,    49,    -1,    51,    -1,    -1,    54,
       3,     4,     5,     6,     7,     8,     9,    -1,    11,    12,
      -1,    -1,    -1,    -1,    17,    18,    19,    20,    -1,    22,
      23,    24,    -1,    -1,    -1,    -1,    29,    -1,    -1,    -1,
      -1,    34,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    42,
      -1,    44,    -1,    46,    -1,    -1,    49,    -1,    51,    -1,
      -1,    54,     3,     4,     5,     6,     7,     8,     9,    -1,
      11,    12,    -1,    -1,    -1,    -1,    17,    18,    19,    20,
      -1,    22,    23,    24,    -1,    -1,    -1,    -1,    29,    -1,
      -1,    -1,    -1,    34,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    42,    -1,    44,    -1,    46,    -1,    -1,    49,    -1,
      51,    -1,    -1,    54,     3,     4,     5,     6,     7,     8,
       9,    -1,    11,    12,    -1,    -1,    -1,    -1,    17,    18,
      19,    20,    -1,    22,    23,    24,    -1,    -1,    -1,    -1,
      29,    -1,    -1,    -1,    -1,    34,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    42,    -1,    44,    -1,    46,    -1,    -1,
      49,    -1,    51,    -1,    -1,    54,     3,     4,     5,     6,
       7,     8,     9,    -1,    11,    12,    -1,    -1,    -1,    -1,
      17,    18,    19,    20,    -1,    -1,    23,    24,    -1,    -1,
      -1,    -1,    29,    -1,    -1,    -1,    -1,    34,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    42,    -1,    44,    -1,    46,
      47,    -1,    49,    -1,    51,    -1,    -1,    54,     3,     4,
       5,     6,     7,     8,     9,    -1,    11,    12,    -1,    -1,
      -1,    -1,    17,    18,    19,    20,    -1,    -1,    23,    24,
      -1,    -1,    -1,    -1,    29,    -1,    -1,    -1,    -1,    34,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    42,    -1,    44,
      -1,    46,    47,    -1,    49,    -1,    51,    -1,    -1,    54,
       3,     4,     5,     6,     7,     8,     9,    -1,    11,    12,
      -1,    -1,    -1,    -1,    17,    18,    19,    20,    -1,    -1,
      23,    24,    -1,    -1,    -1,    -1,    29,    -1,    -1,    -1,
      -1,    34,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    42,
      -1,    44,    -1,    46,    -1,    -1,    49,    -1,    51,    -1,
      53,    54,     3,     4,     5,     6,     7,     8,     9,    -1,
      11,    12,    -1,    -1,    -1,    -1,    17,    18,    19,    20,
      -1,    -1,    23,    24,    -1,    -1,    -1,    -1,    29,    -1,
      -1,    -1,    -1,    34,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    42,    -1,    44,    -1,    46,    -1,    -1,    49,    -1,
      51,    -1,    -1,    54,     3,     4,     5,     6,     7,     8,
       9,    -1,    11,    -1,    -1,    -1,    -1,    -1,    17,    18,
      19,    -1,    -1,    -1,    23,    -1,    -1,    -1,    -1,    -1,
      29,    -1,    -1,    -1,    33,    34,    -1,    -1,    -1,    -1,
       3,     4,     5,     6,     7,     8,     9,    46,    11,    -1,
      -1,    -1,    51,    -1,    17,    18,    19,    -1,     3,    -1,
      23,    -1,    -1,    -1,    -1,    -1,    29,    -1,    -1,    -1,
       3,    34,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,     3,    46,    29,    -1,    -1,    32,    51,    34,
      35,    36,    37,    38,    39,    40,    29,    -1,    -1,    32,
      45,    34,    35,    36,    37,    38,    39,    40,    29,    -1,
      -1,    32,    45,    34,    35,    36,    37,    38,    39,    40,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      -1,    -1,    -1,    -1,    -1,    -1,    48
};

/* YYSTOS[STATE-NUM] -- The symbol kind of the accessing symbol of
   state STATE-NUM.  */
static const yytype_uint8 yystos[] =
{
       0,    26,    38,    56,    57,    58,    60,     3,     4,     5,
       6,     7,     8,     9,    11,    12,    13,    14,    17,    18,
      19,    20,    23,    24,    29,    34,    39,    42,    44,    46,
      49,    51,    53,    54,    71,    72,    76,    80,    81,    86,
      87,    88,    89,    92,    94,    95,   106,   107,   108,   110,
     112,   113,   115,   133,   139,   140,   141,   142,   143,   144,
     145,   147,   148,   149,   150,   151,   152,   153,   157,   145,
       0,    59,   145,    60,   122,   123,   144,   121,   122,    14,
      39,   114,     4,     5,     6,   142,   118,   119,   120,   144,
      51,    92,    30,    48,   114,    33,    95,    96,    97,   155,
      46,   103,   104,   144,   145,    22,    30,    32,    34,    35,
      36,    37,    38,    39,    40,    90,    95,   112,   115,   124,
     125,   126,   154,   155,   156,    92,   144,    46,    34,    39,
      41,   155,   156,    48,    68,    76,    71,   113,    71,   112,
     142,   142,    31,    51,    77,    78,    81,   106,    46,    51,
      77,    44,    30,    46,    62,    48,    50,    31,    51,    21,
      48,   121,   122,   118,   119,   111,    21,    39,    50,    69,
      31,    70,    95,    95,   111,    30,    47,    50,    69,    95,
      46,    51,   109,   129,   134,   135,   136,   139,   143,   144,
     145,   147,   148,   149,   150,   151,   152,    31,    21,    50,
      46,    95,    22,    91,    95,    52,    22,    30,    50,    52,
      71,    71,   112,    50,    69,    52,    95,    52,    48,    68,
      31,    22,    95,    98,    46,    51,   144,    41,    93,    92,
      71,    92,    37,    52,    98,    99,    78,    96,    52,    98,
      99,   100,   101,   155,    65,   145,    61,   144,    44,   123,
      92,    95,   144,    21,    48,    21,    39,    71,   113,   144,
     120,    49,    51,   105,   133,   139,   143,   147,   148,   149,
     150,   151,   152,    82,    83,    71,    95,    95,   155,   135,
     109,   130,   131,   132,   155,    47,    50,    69,    46,    92,
     144,   144,   135,    95,    52,    22,    50,    95,    95,    95,
      52,    52,    71,    95,   124,   155,    51,    77,    92,    95,
      22,    47,    50,    22,    98,    52,    98,    99,   100,    31,
      51,    77,    79,    34,    51,   139,   144,    92,    95,    77,
      50,    69,   102,    50,   102,    47,    79,   102,   102,    50,
      69,    95,    37,    45,    67,   144,   154,    47,    63,    52,
      48,   144,   144,    45,    39,    50,    95,    50,    50,    45,
      30,    95,    47,    30,    50,    69,    52,   109,   109,   135,
      47,    95,    95,    22,    52,    52,    98,    99,    47,    47,
      95,    31,    95,    95,    22,    47,    79,   102,   102,    69,
      92,    52,    98,    99,   100,    95,    37,   100,    52,    52,
      52,    52,   101,    52,   144,   154,    49,    66,    66,    62,
      13,    15,    16,    64,    65,    48,    39,    50,   144,    52,
      13,    30,    85,    95,   144,    85,    95,   109,   130,    47,
      22,    95,    77,   102,   102,    31,    31,    47,    92,    47,
      47,    95,    31,    52,    52,    52,    79,   102,   102,    69,
      52,    95,    69,    31,    79,    79,    79,    66,    66,   144,
      44,    44,    44,    35,    36,    40,   127,   128,   137,   127,
      35,   116,   117,   138,    45,   144,    39,   144,    29,    30,
      45,    50,    84,    28,   144,    45,    95,    52,    52,    92,
      92,    31,    31,    31,    47,    92,    79,    79,    52,    52,
      52,    92,    44,    44,   114,   114,    63,    48,    50,   144,
      48,    48,    69,   144,    39,    31,    95,    95,    85,    95,
      28,    79,    92,    92,    92,    31,    79,    79,   114,   114,
     111,   111,    65,   128,    31,   117,    31,    95,    84,    84,
      84,    95,    92,   111,   111,    10,    74,   146,    74,    45,
     105,   105,    84,    84,    74,    74,    53,    73,    75,    95,
      68,    73,    73,    73,    92,    45,    75,    45,    45,    45,
      68
};

/* YYR1[RULE-NUM] -- Symbol kind of the left-hand side of rule RULE-NUM.  */
static const yytype_uint8 yyr1[] =
{
       0,    55,    56,    56,    56,    57,    57,    58,    58,    59,
      59,    60,    61,    61,    62,    62,    63,    63,    64,    64,
      64,    65,    65,    66,    66,    67,    67,    67,    67,    68,
      68,    69,    69,    70,    70,    71,    71,    72,    72,    72,
      72,    72,    72,    72,    73,    73,    74,    74,    75,    75,
      76,    76,    77,    77,    78,    78,    79,    79,    80,    80,
      80,    80,    80,    80,    80,    80,    80,    80,    80,    80,
      80,    80,    80,    80,    80,    80,    80,    80,    80,    80,
      80,    82,    81,    83,    81,    84,    84,    85,    85,    85,
      85,    85,    85,    86,    86,    86,    86,    86,    86,    86,
      86,    86,    86,    86,    86,    86,    86,    86,    87,    87,
      87,    88,    88,    88,    89,    89,    89,    89,    89,    89,
      90,    90,    90,    90,    90,    91,    91,    91,    91,    91,
      92,    92,    92,    92,    92,    92,    92,    92,    92,    92,
      92,    92,    92,    92,    93,    93,    93,    93,    94,    94,
      95,    96,    96,    97,    97,    97,    97,    97,    97,    98,
      98,    99,    99,   100,   100,   101,   102,   102,   103,   103,
     104,   104,   105,   105,   105,   105,   105,   105,   105,   105,
     105,   106,   107,   108,   108,   108,   108,   108,   108,   108,
     108,   108,   109,   109,   109,   109,   109,   109,   109,   109,
     109,   109,   109,   110,   110,   111,   111,   112,   112,   113,
     114,   114,   114,   114,   114,   114,   115,   115,   115,   115,
     115,   116,   116,   117,   118,   118,   119,   119,   120,   120,
     120,   121,   121,   122,   122,   123,   123,   123,   124,   124,
     125,   125,   126,   126,   127,   127,   128,   128,   129,   130,
     130,   131,   131,   132,   132,   133,   133,   134,   134,   135,
     135,   136,   136,   137,   137,   137,   137,   138,   138,   139,
     139,   140,   140,   141,   141,   142,   143,   143,   143,   143,
     143,   143,   144,   144,   145,   146,   147,   148,   149,   150,
     151,   152,   153,   154,   154,   154,   154,   154,   154,   154,
     154,   155,   156,   156,   157
};

/* YYR2[RULE-NUM] -- Number of symbols on the right-hand side of rule RULE-NUM.  */
static const yytype_int8 yyr2[] =
{
       0,     2,     1,     1,     2,     0,     2,     1,     2,     6,
       9,     5,     0,     1,     0,     2,     0,     2,     3,     3,
       3,     0,     2,     0,     2,     8,     9,     8,     9,     0,
       1,     0,     1,     0,     1,     1,     2,     5,     4,     4,
       3,     2,     2,     1,     1,     2,     0,     2,     0,     3,
       0,     3,     1,     2,     1,     1,     0,     1,     2,     4,
       4,     6,     6,     8,     5,     7,     4,     2,     4,     6,
       6,     5,     5,     7,     8,     7,     6,     6,     8,     7,
       4,     0,     7,     0,     7,     0,     2,     4,     5,     5,
       2,     4,     4,     1,     1,     1,     1,     1,     1,     3,
       2,     3,     3,     4,     3,     1,     4,     1,     5,     5,
       6,     7,     7,     8,     6,     6,     7,     8,     8,     9,
       2,     2,     3,     5,     4,     2,     2,     3,     4,     5,
       1,     1,     1,     1,     5,     2,     4,     3,     4,     5,
       7,     4,     6,     7,     0,     2,     2,     4,     1,     3,
       2,     0,     2,     1,     3,     2,     3,     4,     5,     1,
       3,     2,     4,     1,     3,     2,     1,     3,     1,     3,
       1,     3,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     5,     5,     0,     2,     1,     2,     3,
       0,     3,     5,     3,     5,     7,     3,     5,     3,     5,
       7,     1,     3,     4,     0,     1,     1,     3,     1,     3,
       5,     0,     1,     1,     3,     1,     3,     4,     3,     2,
       1,     3,     0,     2,     1,     3,     2,     4,     3,     3,
       2,     1,     3,     0,     2,     4,     5,     3,     4,     0,
       2,     1,     3,     0,     1,     1,     1,     0,     1,     1,
       2,     1,     2,     1,     2,     1,     1,     1,     2,     2,
       1,     2,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
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
#line 40 "lang11d"
                        { gRootParseNode = (PyrParseNode*)yyvsp[0]; gParserResult = 1; }
#line 2188 "lang11d_tab.cpp"
    break;

  case 3: /* root: classextensions  */
#line 42 "lang11d"
                        { gRootParseNode = (PyrParseNode*)yyvsp[0]; gParserResult = 1; }
#line 2194 "lang11d_tab.cpp"
    break;

  case 4: /* root: INTERPRET cmdlinecode  */
#line 44 "lang11d"
                        { gRootParseNode = (PyrParseNode*)yyvsp[0]; gParserResult = 2; }
#line 2200 "lang11d_tab.cpp"
    break;

  case 5: /* classes: %empty  */
#line 47 "lang11d"
          { yyval = 0; }
#line 2206 "lang11d_tab.cpp"
    break;

  case 6: /* classes: classes classdef  */
#line 49 "lang11d"
                        { yyval = (intptr_t)linkNextNode((PyrParseNode*)yyvsp[-1], (PyrParseNode*)yyvsp[0]); }
#line 2212 "lang11d_tab.cpp"
    break;

  case 8: /* classextensions: classextensions classextension  */
#line 54 "lang11d"
                                { yyval = (intptr_t)linkNextNode((PyrParseNode*)yyvsp[-1], (PyrParseNode*)yyvsp[0]); }
#line 2218 "lang11d_tab.cpp"
    break;

  case 9: /* classdef: classname superclass '{' classvardecls methods '}'  */
#line 58 "lang11d"
                                { yyval = (intptr_t)newPyrClassNode((PyrSlotNode*)yyvsp[-5], (PyrSlotNode*)yyvsp[-4],
					(PyrVarListNode*)yyvsp[-2], (PyrMethodNode*)yyvsp[-1], 0);
				}
#line 2226 "lang11d_tab.cpp"
    break;

  case 10: /* classdef: classname '[' optname ']' superclass '{' classvardecls methods '}'  */
#line 62 "lang11d"
                                { yyval = (intptr_t)newPyrClassNode((PyrSlotNode*)yyvsp[-8], (PyrSlotNode*)yyvsp[-4],
					(PyrVarListNode*)yyvsp[-2], (PyrMethodNode*)yyvsp[-1],
					(PyrSlotNode*)yyvsp[-6]);
				}
#line 2235 "lang11d_tab.cpp"
    break;

  case 11: /* classextension: '+' classname '{' methods '}'  */
#line 69 "lang11d"
                                {
					yyval = (intptr_t)newPyrClassExtNode((PyrSlotNode*)yyvsp[-3], (PyrMethodNode*)yyvsp[-1]);
				}
#line 2243 "lang11d_tab.cpp"
    break;

  case 12: /* optname: %empty  */
#line 74 "lang11d"
                  { yyval = 0; }
#line 2249 "lang11d_tab.cpp"
    break;

  case 14: /* superclass: %empty  */
#line 78 "lang11d"
                  { yyval = 0; }
#line 2255 "lang11d_tab.cpp"
    break;

  case 15: /* superclass: ':' classname  */
#line 80 "lang11d"
                                { yyval = yyvsp[0]; }
#line 2261 "lang11d_tab.cpp"
    break;

  case 16: /* classvardecls: %empty  */
#line 83 "lang11d"
                  { yyval = 0; }
#line 2267 "lang11d_tab.cpp"
    break;

  case 17: /* classvardecls: classvardecls classvardecl  */
#line 85 "lang11d"
                                        { yyval = (intptr_t)linkNextNode((PyrParseNode*)yyvsp[-1], (PyrParseNode*)yyvsp[0]); }
#line 2273 "lang11d_tab.cpp"
    break;

  case 18: /* classvardecl: CLASSVAR rwslotdeflist ';'  */
#line 89 "lang11d"
                                        { yyval = (intptr_t)newPyrVarListNode((PyrVarDefNode*)yyvsp[-1], varClass); }
#line 2279 "lang11d_tab.cpp"
    break;

  case 19: /* classvardecl: VAR rwslotdeflist ';'  */
#line 91 "lang11d"
                                        { yyval = (intptr_t)newPyrVarListNode((PyrVarDefNode*)yyvsp[-1], varInst); }
#line 2285 "lang11d_tab.cpp"
    break;

  case 20: /* classvardecl: SC_CONST constdeflist ';'  */
#line 93 "lang11d"
                                        { yyval = (intptr_t)newPyrVarListNode((PyrVarDefNode*)yyvsp[-1], varConst); }
#line 2291 "lang11d_tab.cpp"
    break;

  case 21: /* methods: %empty  */
#line 96 "lang11d"
                  { yyval = 0; }
#line 2297 "lang11d_tab.cpp"
    break;

  case 22: /* methods: methods methoddef  */
#line 98 "lang11d"
                                { yyval = (intptr_t)linkNextNode((PyrParseNode*)yyvsp[-1], (PyrParseNode*)yyvsp[0]); }
#line 2303 "lang11d_tab.cpp"
    break;

  case 23: /* methodMacro: %empty  */
#line 101 "lang11d"
              { yyval = 0; }
#line 2309 "lang11d_tab.cpp"
    break;

  case 24: /* methodMacro: '#' name  */
#line 101 "lang11d"
                                     { yyval = yyvsp[0]; }
#line 2315 "lang11d_tab.cpp"
    break;

  case 25: /* methoddef: name methodMacro '{' argdecls funcvardecls primitive methbody '}'  */
#line 104 "lang11d"
                                { yyval = (intptr_t)newPyrMethodNode((PyrSlotNode*)yyvsp[-7], (PyrSlotNode*)yyvsp[-2], (PyrArgListNode*)yyvsp[-4], (PyrVarListNode*)yyvsp[-3], (PyrParseNode*)yyvsp[-1], 0, (PyrSlotNode*)yyvsp[-6]); }
#line 2321 "lang11d_tab.cpp"
    break;

  case 26: /* methoddef: '*' name methodMacro '{' argdecls funcvardecls primitive methbody '}'  */
#line 106 "lang11d"
                                { yyval = (intptr_t)newPyrMethodNode((PyrSlotNode*)yyvsp[-7], (PyrSlotNode*)yyvsp[-2], (PyrArgListNode*)yyvsp[-4], (PyrVarListNode*)yyvsp[-3], (PyrParseNode*)yyvsp[-1], 1, (PyrSlotNode*)yyvsp[-6]); }
#line 2327 "lang11d_tab.cpp"
    break;

  case 27: /* methoddef: binop methodMacro '{' argdecls funcvardecls primitive methbody '}'  */
#line 108 "lang11d"
                                { yyval = (intptr_t)newPyrMethodNode((PyrSlotNode*)yyvsp[-7], (PyrSlotNode*)yyvsp[-2], (PyrArgListNode*)yyvsp[-4], (PyrVarListNode*)yyvsp[-3], (PyrParseNode*)yyvsp[-1], 0, (PyrSlotNode*)yyvsp[-6]); }
#line 2333 "lang11d_tab.cpp"
    break;

  case 28: /* methoddef: '*' binop methodMacro '{' argdecls funcvardecls primitive methbody '}'  */
#line 110 "lang11d"
                                { yyval = (intptr_t)newPyrMethodNode((PyrSlotNode*)yyvsp[-7], (PyrSlotNode*)yyvsp[-2], (PyrArgListNode*)yyvsp[-4], (PyrVarListNode*)yyvsp[-3], (PyrParseNode*)yyvsp[-1], 1, (PyrSlotNode*)yyvsp[-6]); }
#line 2339 "lang11d_tab.cpp"
    break;

  case 36: /* funcbody: exprseq funretval  */
#line 127 "lang11d"
                                { yyval = (intptr_t)newPyrDropNode((PyrParseNode*)yyvsp[-1], (PyrParseNode*)yyvsp[0]); }
#line 2345 "lang11d_tab.cpp"
    break;

  case 37: /* cmdlinecode: '(' argdecls1 funcvardecls1 funcbody ')'  */
#line 131 "lang11d"
                                { yyval = (intptr_t)newPyrBlockNode((PyrArgListNode*)yyvsp[-3], (PyrVarListNode*)yyvsp[-2], (PyrParseNode*)yyvsp[-1], false); }
#line 2351 "lang11d_tab.cpp"
    break;

  case 38: /* cmdlinecode: '(' argdecls1 funcbody ')'  */
#line 133 "lang11d"
                                { yyval = (intptr_t)newPyrBlockNode((PyrArgListNode*)yyvsp[-2], NULL, (PyrParseNode*)yyvsp[-1], false); }
#line 2357 "lang11d_tab.cpp"
    break;

  case 39: /* cmdlinecode: '(' funcvardecls1 funcbody ')'  */
#line 135 "lang11d"
                                { yyval = (intptr_t)newPyrBlockNode(NULL, (PyrVarListNode*)yyvsp[-2], (PyrParseNode*)yyvsp[-1], false); }
#line 2363 "lang11d_tab.cpp"
    break;

  case 40: /* cmdlinecode: argdecls1 funcvardecls1 funcbody  */
#line 137 "lang11d"
                                { yyval = (intptr_t)newPyrBlockNode((PyrArgListNode*)yyvsp[-2], (PyrVarListNode*)yyvsp[-1], (PyrParseNode*)yyvsp[0], false); }
#line 2369 "lang11d_tab.cpp"
    break;

  case 41: /* cmdlinecode: argdecls1 funcbody  */
#line 139 "lang11d"
                                { yyval = (intptr_t)newPyrBlockNode((PyrArgListNode*)yyvsp[-1], NULL, (PyrParseNode*)yyvsp[0], false); }
#line 2375 "lang11d_tab.cpp"
    break;

  case 42: /* cmdlinecode: funcvardecls1 funcbody  */
#line 141 "lang11d"
                                { yyval = (intptr_t)newPyrBlockNode(NULL, (PyrVarListNode*)yyvsp[-1], (PyrParseNode*)yyvsp[0], false); }
#line 2381 "lang11d_tab.cpp"
    break;

  case 43: /* cmdlinecode: funcbody  */
#line 143 "lang11d"
                                { yyval = (intptr_t)newPyrBlockNode(NULL, NULL, (PyrParseNode*)yyvsp[0], false); }
#line 2387 "lang11d_tab.cpp"
    break;

  case 45: /* methbody: exprseq retval  */
#line 148 "lang11d"
                                { yyval = (intptr_t)newPyrDropNode((PyrParseNode*)yyvsp[-1], (PyrParseNode*)yyvsp[0]); }
#line 2393 "lang11d_tab.cpp"
    break;

  case 46: /* primitive: %empty  */
#line 151 "lang11d"
                  { yyval = 0; }
#line 2399 "lang11d_tab.cpp"
    break;

  case 47: /* primitive: primname optsemi  */
#line 153 "lang11d"
                                { yyval = yyvsp[-1]; }
#line 2405 "lang11d_tab.cpp"
    break;

  case 48: /* retval: %empty  */
#line 157 "lang11d"
                        { yyval = (intptr_t)newPyrReturnNode(NULL); }
#line 2411 "lang11d_tab.cpp"
    break;

  case 49: /* retval: '^' expr optsemi  */
#line 159 "lang11d"
                        { yyval = (intptr_t)newPyrReturnNode((PyrParseNode*)yyvsp[-1]); }
#line 2417 "lang11d_tab.cpp"
    break;

  case 50: /* funretval: %empty  */
#line 163 "lang11d"
                        { yyval = (intptr_t)newPyrBlockReturnNode(); }
#line 2423 "lang11d_tab.cpp"
    break;

  case 51: /* funretval: '^' expr optsemi  */
#line 165 "lang11d"
                        { yyval = (intptr_t)newPyrReturnNode((PyrParseNode*)yyvsp[-1]); }
#line 2429 "lang11d_tab.cpp"
    break;

  case 53: /* blocklist1: blocklist1 blocklistitem  */
#line 170 "lang11d"
                                {
					yyval = (intptr_t)linkNextNode((PyrParseNode*)yyvsp[-1], (PyrParseNode*)yyvsp[0]);
				}
#line 2437 "lang11d_tab.cpp"
    break;

  case 56: /* blocklist: %empty  */
#line 179 "lang11d"
                        { yyval = 0; }
#line 2443 "lang11d_tab.cpp"
    break;

  case 58: /* msgsend: name blocklist1  */
#line 184 "lang11d"
                        {
				yyval = (intptr_t)newPyrCallNode((PyrSlotNode*)yyvsp[-1], (PyrParseNode*)yyvsp[0], 0, 0);
			}
#line 2451 "lang11d_tab.cpp"
    break;

  case 59: /* msgsend: '(' binop2 ')' blocklist1  */
#line 188 "lang11d"
                        {
				yyval = (intptr_t)newPyrCallNode((PyrSlotNode*)yyvsp[-2], (PyrParseNode*)yyvsp[0], 0, 0);
			}
#line 2459 "lang11d_tab.cpp"
    break;

  case 60: /* msgsend: name '(' ')' blocklist1  */
#line 192 "lang11d"
                        {
				yyval = (intptr_t)newPyrCallNode((PyrSlotNode*)yyvsp[-3], NULL, NULL, (PyrParseNode*)yyvsp[0]);
			}
#line 2467 "lang11d_tab.cpp"
    break;

  case 61: /* msgsend: name '(' arglist1 optkeyarglist ')' blocklist  */
#line 196 "lang11d"
                        {
				yyval = (intptr_t)newPyrCallNode((PyrSlotNode*)yyvsp[-5], (PyrParseNode*)yyvsp[-3],
						(PyrParseNode*)yyvsp[-2], (PyrParseNode*)yyvsp[0]);
			}
#line 2476 "lang11d_tab.cpp"
    break;

  case 62: /* msgsend: '(' binop2 ')' '(' ')' blocklist1  */
#line 201 "lang11d"
                        {
				yyval = (intptr_t)newPyrCallNode((PyrSlotNode*)yyvsp[-4], NULL, NULL, (PyrParseNode*)yyvsp[0]);
			}
#line 2484 "lang11d_tab.cpp"
    break;

  case 63: /* msgsend: '(' binop2 ')' '(' arglist1 optkeyarglist ')' blocklist  */
#line 205 "lang11d"
                        {
				yyval = (intptr_t)newPyrCallNode((PyrSlotNode*)yyvsp[-6], (PyrParseNode*)yyvsp[-3],
						(PyrParseNode*)yyvsp[-2], (PyrParseNode*)yyvsp[0]);
			}
#line 2493 "lang11d_tab.cpp"
    break;

  case 64: /* msgsend: name '(' arglistv1 optkeyarglist ')'  */
#line 210 "lang11d"
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
#line 2515 "lang11d_tab.cpp"
    break;

  case 65: /* msgsend: '(' binop2 ')' '(' arglistv1 optkeyarglist ')'  */
#line 228 "lang11d"
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
#line 2532 "lang11d_tab.cpp"
    break;

  case 66: /* msgsend: classname '[' arrayelems ']'  */
#line 241 "lang11d"
                        { yyval = (intptr_t)newPyrDynListNode((PyrParseNode*)yyvsp[-3], (PyrParseNode*)yyvsp[-1]); }
#line 2538 "lang11d_tab.cpp"
    break;

  case 67: /* msgsend: classname blocklist1  */
#line 243 "lang11d"
                        {
				PyrSlotNode *selectornode;
				PyrSlot slot;
				PyrParseNode* args;

				SetSymbol(&slot, s_new);
				selectornode = newPyrSlotNode(&slot);
				args = (PyrParseNode*)newPyrPushNameNode((PyrSlotNode*)yyvsp[-1]);
				yyval = (intptr_t)newPyrCallNode(selectornode, args, 0, (PyrParseNode*)yyvsp[0]);
			}
#line 2553 "lang11d_tab.cpp"
    break;

  case 68: /* msgsend: classname '(' ')' blocklist  */
#line 254 "lang11d"
                        {
				PyrSlotNode *selectornode;
				PyrSlot slot;
				PyrParseNode* args;

				SetSymbol(&slot, s_new);
				selectornode = newPyrSlotNode(&slot);
				args = (PyrParseNode*)newPyrPushNameNode((PyrSlotNode*)yyvsp[-3]);
				yyval = (intptr_t)newPyrCallNode(selectornode, args, NULL, (PyrParseNode*)yyvsp[0]);
			}
#line 2568 "lang11d_tab.cpp"
    break;

  case 69: /* msgsend: classname '(' keyarglist1 optcomma ')' blocklist  */
#line 265 "lang11d"
                        {
				PyrSlotNode *selectornode;
				PyrSlot slot;
				PyrParseNode* args;

				SetSymbol(&slot, s_new);
				selectornode = newPyrSlotNode(&slot);
				args = (PyrParseNode*)newPyrPushNameNode((PyrSlotNode*)yyvsp[-5]);
				yyval = (intptr_t)newPyrCallNode(selectornode, args, (PyrParseNode*)yyvsp[-3], (PyrParseNode*)yyvsp[0]);
			}
#line 2583 "lang11d_tab.cpp"
    break;

  case 70: /* msgsend: classname '(' arglist1 optkeyarglist ')' blocklist  */
#line 276 "lang11d"
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
#line 2600 "lang11d_tab.cpp"
    break;

  case 71: /* msgsend: classname '(' arglistv1 optkeyarglist ')'  */
#line 289 "lang11d"
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
#line 2625 "lang11d_tab.cpp"
    break;

  case 72: /* msgsend: expr '.' '(' ')' blocklist  */
#line 310 "lang11d"
                        {
				PyrSlotNode *selectornode;
				PyrSlot slot;

				SetSymbol(&slot, s_value);
				selectornode = newPyrSlotNode(&slot);
				yyval = (intptr_t)newPyrCallNode(selectornode, (PyrParseNode*)yyvsp[-4], NULL, (PyrParseNode*)yyvsp[0]);
			}
#line 2638 "lang11d_tab.cpp"
    break;

  case 73: /* msgsend: expr '.' '(' keyarglist1 optcomma ')' blocklist  */
#line 319 "lang11d"
                        {
				PyrSlotNode *selectornode;
				PyrSlot slot;

				SetSymbol(&slot, s_value);
				selectornode = newPyrSlotNode(&slot);
				yyval = (intptr_t)newPyrCallNode(selectornode, (PyrParseNode*)yyvsp[-6], (PyrParseNode*)yyvsp[-3], (PyrParseNode*)yyvsp[0]);
			}
#line 2651 "lang11d_tab.cpp"
    break;

  case 74: /* msgsend: expr '.' name '(' keyarglist1 optcomma ')' blocklist  */
#line 328 "lang11d"
                        {
				yyval = (intptr_t)newPyrCallNode((PyrSlotNode*)yyvsp[-5], (PyrParseNode*)yyvsp[-7],
					(PyrParseNode*)yyvsp[-3], (PyrParseNode*)yyvsp[0]);
			}
#line 2660 "lang11d_tab.cpp"
    break;

  case 75: /* msgsend: expr '.' '(' arglist1 optkeyarglist ')' blocklist  */
#line 333 "lang11d"
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
#line 2677 "lang11d_tab.cpp"
    break;

  case 76: /* msgsend: expr '.' '(' arglistv1 optkeyarglist ')'  */
#line 347 "lang11d"
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
#line 2701 "lang11d_tab.cpp"
    break;

  case 77: /* msgsend: expr '.' name '(' ')' blocklist  */
#line 369 "lang11d"
                        {
				yyval = (intptr_t)newPyrCallNode((PyrSlotNode*)yyvsp[-3], (PyrParseNode*)yyvsp[-5], NULL, (PyrParseNode*)yyvsp[0]);
			}
#line 2709 "lang11d_tab.cpp"
    break;

  case 78: /* msgsend: expr '.' name '(' arglist1 optkeyarglist ')' blocklist  */
#line 373 "lang11d"
                        {
				PyrParseNode* args;
				args = linkNextNode((PyrParseNode*)yyvsp[-7], (PyrParseNode*)yyvsp[-3]);
				yyval = (intptr_t)newPyrCallNode((PyrSlotNode*)yyvsp[-5], args, (PyrParseNode*)yyvsp[-2], (PyrParseNode*)yyvsp[0]);
			}
#line 2719 "lang11d_tab.cpp"
    break;

  case 79: /* msgsend: expr '.' name '(' arglistv1 optkeyarglist ')'  */
#line 379 "lang11d"
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
#line 2741 "lang11d_tab.cpp"
    break;

  case 80: /* msgsend: expr '.' name blocklist  */
#line 397 "lang11d"
                        {
				yyval = (intptr_t)newPyrCallNode((PyrSlotNode*)yyvsp[-1], (PyrParseNode*)yyvsp[-3], 0, (PyrParseNode*)yyvsp[0]);
			}
#line 2749 "lang11d_tab.cpp"
    break;

  case 81: /* $@1: %empty  */
#line 402 "lang11d"
                            { pushls(&generatorStack, yyvsp[0]); pushls(&generatorStack, 1); }
#line 2755 "lang11d_tab.cpp"
    break;

  case 82: /* generator: '{' ':' exprseq $@1 ',' qual '}'  */
#line 403 "lang11d"
                        {
				PyrSlot slot;
				SetSymbol(&slot, getsym("r"));
				PyrSlotNode* selectornode = newPyrSlotNode(&slot);

				PyrParseNode *block = (PyrParseNode*)newPyrBlockNode(0, 0, (PyrParseNode*)yyvsp[-1], false);
				PyrParseNode *blocklit = (PyrParseNode*)newPyrPushLitNode(NULL, block);
				yyval = (intptr_t)newPyrCallNode(selectornode, (PyrParseNode*)blocklit, 0, 0);
			}
#line 2769 "lang11d_tab.cpp"
    break;

  case 83: /* $@2: %empty  */
#line 412 "lang11d"
                                  { pushls(&generatorStack, yyvsp[0]); pushls(&generatorStack, 2); }
#line 2775 "lang11d_tab.cpp"
    break;

  case 84: /* generator: '{' ';' exprseq $@2 ',' qual '}'  */
#line 413 "lang11d"
                        {
				yyval = yyvsp[-1];
			}
#line 2783 "lang11d_tab.cpp"
    break;

  case 85: /* nextqual: %empty  */
#line 419 "lang11d"
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
#line 2809 "lang11d_tab.cpp"
    break;

  case 86: /* nextqual: ',' qual  */
#line 441 "lang11d"
                                { yyval = yyvsp[0]; }
#line 2815 "lang11d_tab.cpp"
    break;

  case 87: /* qual: name LEFTARROW exprseq nextqual  */
#line 445 "lang11d"
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
#line 2853 "lang11d_tab.cpp"
    break;

  case 88: /* qual: name name LEFTARROW exprseq nextqual  */
#line 479 "lang11d"
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
#line 2895 "lang11d_tab.cpp"
    break;

  case 89: /* qual: VAR name '=' exprseq nextqual  */
#line 517 "lang11d"
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
#line 2913 "lang11d_tab.cpp"
    break;

  case 90: /* qual: exprseq nextqual  */
#line 531 "lang11d"
                        {
				PyrSlot slot;
				SetSymbol(&slot, getsym("if"));
				PyrSlotNode* selectornode = newPyrSlotNode(&slot);
				PyrParseNode *block = (PyrParseNode*)newPyrBlockNode(0, 0, (PyrParseNode*)yyvsp[0], false);
				PyrParseNode *blocklit = (PyrParseNode*)newPyrPushLitNode(NULL, block);
				PyrParseNode* args2 = (PyrParseNode*)linkNextNode((PyrParseNode*)yyvsp[-1], blocklit);

				yyval = (intptr_t)newPyrCallNode(selectornode, args2, 0, 0);
			}
#line 2928 "lang11d_tab.cpp"
    break;

  case 91: /* qual: ':' ':' exprseq nextqual  */
#line 542 "lang11d"
                        {
				yyval = (intptr_t)newPyrDropNode((PyrParseNode*)yyvsp[-1], (PyrParseNode*)yyvsp[0]);
			}
#line 2936 "lang11d_tab.cpp"
    break;

  case 92: /* qual: ':' WHILE exprseq nextqual  */
#line 546 "lang11d"
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
#line 2963 "lang11d_tab.cpp"
    break;

  case 99: /* expr1: '(' exprseq ')'  */
#line 577 "lang11d"
                        {
				PyrParseNode* node = (PyrParseNode*)yyvsp[-1];
				node->mParens = 1;
				yyval = yyvsp[-1];
			}
#line 2973 "lang11d_tab.cpp"
    break;

  case 100: /* expr1: '~' name  */
#line 583 "lang11d"
                        {
				PyrParseNode* argnode;
				PyrSlotNode* selectornode;
				PyrSlot slot;
				argnode = (PyrParseNode*)newPyrPushLitNode((PyrSlotNode*)yyvsp[0], NULL);
				SetSymbol(&slot, s_envirGet);
				selectornode = newPyrSlotNode(&slot);
				yyval = (intptr_t)newPyrCallNode(selectornode, argnode, 0, 0);
			}
#line 2987 "lang11d_tab.cpp"
    break;

  case 101: /* expr1: '[' arrayelems ']'  */
#line 593 "lang11d"
                        { yyval = (intptr_t)newPyrDynListNode(0, (PyrParseNode*)yyvsp[-1]); }
#line 2993 "lang11d_tab.cpp"
    break;

  case 102: /* expr1: '(' valrange2 ')'  */
#line 595 "lang11d"
                        { yyval = yyvsp[-1]; }
#line 2999 "lang11d_tab.cpp"
    break;

  case 103: /* expr1: '(' ':' valrange3 ')'  */
#line 597 "lang11d"
                        { yyval = yyvsp[-1]; }
#line 3005 "lang11d_tab.cpp"
    break;

  case 104: /* expr1: '(' dictslotlist ')'  */
#line 599 "lang11d"
                        { yyval = (intptr_t)newPyrDynDictNode((PyrParseNode*)yyvsp[-1]); }
#line 3011 "lang11d_tab.cpp"
    break;

  case 105: /* expr1: pseudovar  */
#line 601 "lang11d"
                        { yyval = (intptr_t)newPyrPushNameNode((PyrSlotNode*)yyvsp[0]); }
#line 3017 "lang11d_tab.cpp"
    break;

  case 106: /* expr1: expr1 '[' arglist1 ']'  */
#line 603 "lang11d"
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
#line 3034 "lang11d_tab.cpp"
    break;

  case 108: /* valrangex1: expr1 '[' arglist1 DOTDOT ']'  */
#line 619 "lang11d"
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
#line 3065 "lang11d_tab.cpp"
    break;

  case 109: /* valrangex1: expr1 '[' DOTDOT exprseq ']'  */
#line 646 "lang11d"
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
#line 3087 "lang11d_tab.cpp"
    break;

  case 110: /* valrangex1: expr1 '[' arglist1 DOTDOT exprseq ']'  */
#line 664 "lang11d"
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
#line 3116 "lang11d_tab.cpp"
    break;

  case 111: /* valrangeassign: expr1 '[' arglist1 DOTDOT ']' '=' expr  */
#line 691 "lang11d"
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
#line 3148 "lang11d_tab.cpp"
    break;

  case 112: /* valrangeassign: expr1 '[' DOTDOT exprseq ']' '=' expr  */
#line 719 "lang11d"
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
#line 3171 "lang11d_tab.cpp"
    break;

  case 113: /* valrangeassign: expr1 '[' arglist1 DOTDOT exprseq ']' '=' expr  */
#line 738 "lang11d"
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
#line 3201 "lang11d_tab.cpp"
    break;

  case 114: /* valrangexd: expr '.' '[' arglist1 DOTDOT ']'  */
#line 766 "lang11d"
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
#line 3232 "lang11d_tab.cpp"
    break;

  case 115: /* valrangexd: expr '.' '[' DOTDOT exprseq ']'  */
#line 793 "lang11d"
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
#line 3254 "lang11d_tab.cpp"
    break;

  case 116: /* valrangexd: expr '.' '[' arglist1 DOTDOT exprseq ']'  */
#line 811 "lang11d"
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
#line 3283 "lang11d_tab.cpp"
    break;

  case 117: /* valrangexd: expr '.' '[' arglist1 DOTDOT ']' '=' expr  */
#line 836 "lang11d"
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
#line 3315 "lang11d_tab.cpp"
    break;

  case 118: /* valrangexd: expr '.' '[' DOTDOT exprseq ']' '=' expr  */
#line 864 "lang11d"
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
#line 3338 "lang11d_tab.cpp"
    break;

  case 119: /* valrangexd: expr '.' '[' arglist1 DOTDOT exprseq ']' '=' expr  */
#line 883 "lang11d"
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
#line 3368 "lang11d_tab.cpp"
    break;

  case 120: /* valrange2: exprseq DOTDOT  */
#line 911 "lang11d"
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
#line 3390 "lang11d_tab.cpp"
    break;

  case 121: /* valrange2: DOTDOT exprseq  */
#line 930 "lang11d"
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
#line 3412 "lang11d_tab.cpp"
    break;

  case 122: /* valrange2: exprseq DOTDOT exprseq  */
#line 949 "lang11d"
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
#line 3432 "lang11d_tab.cpp"
    break;

  case 123: /* valrange2: exprseq ',' exprseq DOTDOT exprseq  */
#line 966 "lang11d"
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
#line 3450 "lang11d_tab.cpp"
    break;

  case 124: /* valrange2: exprseq ',' exprseq DOTDOT  */
#line 980 "lang11d"
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
#line 3473 "lang11d_tab.cpp"
    break;

  case 125: /* valrange3: DOTDOT exprseq  */
#line 1001 "lang11d"
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
#line 3495 "lang11d_tab.cpp"
    break;

  case 126: /* valrange3: exprseq DOTDOT  */
#line 1020 "lang11d"
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
#line 3516 "lang11d_tab.cpp"
    break;

  case 127: /* valrange3: exprseq DOTDOT exprseq  */
#line 1038 "lang11d"
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
#line 3536 "lang11d_tab.cpp"
    break;

  case 128: /* valrange3: exprseq ',' exprseq DOTDOT  */
#line 1055 "lang11d"
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
#line 3556 "lang11d_tab.cpp"
    break;

  case 129: /* valrange3: exprseq ',' exprseq DOTDOT exprseq  */
#line 1071 "lang11d"
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
#line 3574 "lang11d_tab.cpp"
    break;

  case 133: /* expr: classname  */
#line 1089 "lang11d"
                            { yyval = (intptr_t)newPyrPushNameNode((PyrSlotNode*)yyvsp[0]); }
#line 3580 "lang11d_tab.cpp"
    break;

  case 134: /* expr: expr '.' '[' arglist1 ']'  */
#line 1091 "lang11d"
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
#line 3597 "lang11d_tab.cpp"
    break;

  case 135: /* expr: '`' expr  */
#line 1104 "lang11d"
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
#line 3615 "lang11d_tab.cpp"
    break;

  case 136: /* expr: expr binop2 adverb expr  */
#line 1118 "lang11d"
                        {
				yyval = (intptr_t)newPyrBinopCallNode((PyrSlotNode*)yyvsp[-2],
						(PyrParseNode*)yyvsp[-3], (PyrParseNode*)yyvsp[0], (PyrParseNode*)yyvsp[-1]);
			}
#line 3624 "lang11d_tab.cpp"
    break;

  case 137: /* expr: name '=' expr  */
#line 1123 "lang11d"
                        {
				yyval = (intptr_t)newPyrAssignNode((PyrSlotNode*)yyvsp[-2], (PyrParseNode*)yyvsp[0], 0);
			}
#line 3632 "lang11d_tab.cpp"
    break;

  case 138: /* expr: '~' name '=' expr  */
#line 1127 "lang11d"
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
#line 3647 "lang11d_tab.cpp"
    break;

  case 139: /* expr: expr '.' name '=' expr  */
#line 1138 "lang11d"
                        {
				yyval = (intptr_t)newPyrSetterNode((PyrSlotNode*)yyvsp[-2],
						(PyrParseNode*)yyvsp[-4], (PyrParseNode*)yyvsp[0]);
			}
#line 3656 "lang11d_tab.cpp"
    break;

  case 140: /* expr: name '(' arglist1 optkeyarglist ')' '=' expr  */
#line 1143 "lang11d"
                        {
				if (yyvsp[-3] != 0) {
					error("Setter method called with keyword arguments.\n");
					nodePostErrorLine((PyrParseNode*)yyvsp[-3]);
					compileErrors++;
				}
				yyval = (intptr_t)newPyrSetterNode((PyrSlotNode*)yyvsp[-6],
						(PyrParseNode*)yyvsp[-4], (PyrParseNode*)yyvsp[0]);
			}
#line 3670 "lang11d_tab.cpp"
    break;

  case 141: /* expr: '#' mavars '=' expr  */
#line 1153 "lang11d"
                        {
				yyval = (intptr_t)newPyrMultiAssignNode((PyrMultiAssignVarListNode*)yyvsp[-2],
					(PyrParseNode*)yyvsp[0], 0);
			}
#line 3679 "lang11d_tab.cpp"
    break;

  case 142: /* expr: expr1 '[' arglist1 ']' '=' expr  */
#line 1158 "lang11d"
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
#line 3697 "lang11d_tab.cpp"
    break;

  case 143: /* expr: expr '.' '[' arglist1 ']' '=' expr  */
#line 1172 "lang11d"
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
#line 3715 "lang11d_tab.cpp"
    break;

  case 144: /* adverb: %empty  */
#line 1187 "lang11d"
          { yyval = 0; }
#line 3721 "lang11d_tab.cpp"
    break;

  case 145: /* adverb: '.' name  */
#line 1188 "lang11d"
                           { yyval = (intptr_t)newPyrPushLitNode((PyrSlotNode*)yyvsp[0], NULL); }
#line 3727 "lang11d_tab.cpp"
    break;

  case 146: /* adverb: '.' integer  */
#line 1189 "lang11d"
                              { yyval = (intptr_t)newPyrPushLitNode((PyrSlotNode*)yyvsp[0], NULL); }
#line 3733 "lang11d_tab.cpp"
    break;

  case 147: /* adverb: '.' '(' exprseq ')'  */
#line 1190 "lang11d"
                                      { yyval = yyvsp[-1]; }
#line 3739 "lang11d_tab.cpp"
    break;

  case 149: /* exprn: exprn ';' expr  */
#line 1195 "lang11d"
                        {
				yyval = (intptr_t)newPyrDropNode((PyrParseNode*)yyvsp[-2], (PyrParseNode*)yyvsp[0]);
			}
#line 3747 "lang11d_tab.cpp"
    break;

  case 151: /* arrayelems: %empty  */
#line 1203 "lang11d"
                  { yyval = 0; }
#line 3753 "lang11d_tab.cpp"
    break;

  case 152: /* arrayelems: arrayelems1 optcomma  */
#line 1205 "lang11d"
                          { yyval = yyvsp[-1]; }
#line 3759 "lang11d_tab.cpp"
    break;

  case 154: /* arrayelems1: exprseq ':' exprseq  */
#line 1210 "lang11d"
                                { yyval = (intptr_t)linkNextNode((PyrParseNode*)yyvsp[-2], (PyrParseNode*)yyvsp[0]); }
#line 3765 "lang11d_tab.cpp"
    break;

  case 155: /* arrayelems1: keybinop exprseq  */
#line 1212 "lang11d"
                                {
					PyrParseNode* key = newPyrPushLitNode((PyrSlotNode*)yyvsp[-1], NULL);
					yyval = (intptr_t)linkNextNode(key, (PyrParseNode*)yyvsp[0]);
				}
#line 3774 "lang11d_tab.cpp"
    break;

  case 156: /* arrayelems1: arrayelems1 ',' exprseq  */
#line 1217 "lang11d"
                                        { yyval = (intptr_t)linkNextNode((PyrParseNode*)yyvsp[-2], (PyrParseNode*)yyvsp[0]); }
#line 3780 "lang11d_tab.cpp"
    break;

  case 157: /* arrayelems1: arrayelems1 ',' keybinop exprseq  */
#line 1219 "lang11d"
                                {
					PyrParseNode* elems;
					PyrParseNode* key = newPyrPushLitNode((PyrSlotNode*)yyvsp[-1], NULL);
					elems = (PyrParseNode*)linkNextNode(key, (PyrParseNode*)yyvsp[0]);
					yyval = (intptr_t)linkNextNode((PyrParseNode*)yyvsp[-3], elems);
				}
#line 3791 "lang11d_tab.cpp"
    break;

  case 158: /* arrayelems1: arrayelems1 ',' exprseq ':' exprseq  */
#line 1226 "lang11d"
                                {
					PyrParseNode* elems;
					elems = (PyrParseNode*)linkNextNode((PyrParseNode*)yyvsp[-2], (PyrParseNode*)yyvsp[0]);
					yyval = (intptr_t)linkNextNode((PyrParseNode*)yyvsp[-4], elems);
				}
#line 3801 "lang11d_tab.cpp"
    break;

  case 160: /* arglist1: arglist1 ',' exprseq  */
#line 1235 "lang11d"
                                        { yyval = (intptr_t)linkNextNode((PyrParseNode*)yyvsp[-2], (PyrParseNode*)yyvsp[0]); }
#line 3807 "lang11d_tab.cpp"
    break;

  case 161: /* arglistv1: '*' exprseq  */
#line 1239 "lang11d"
                                { yyval = yyvsp[0]; }
#line 3813 "lang11d_tab.cpp"
    break;

  case 162: /* arglistv1: arglist1 ',' '*' exprseq  */
#line 1241 "lang11d"
                                        { yyval = (intptr_t)linkNextNode((PyrParseNode*)yyvsp[-3], (PyrParseNode*)yyvsp[0]); }
#line 3819 "lang11d_tab.cpp"
    break;

  case 164: /* keyarglist1: keyarglist1 ',' keyarg  */
#line 1246 "lang11d"
                                        { yyval = (intptr_t)linkNextNode((PyrParseNode*)yyvsp[-2], (PyrParseNode*)yyvsp[0]); }
#line 3825 "lang11d_tab.cpp"
    break;

  case 165: /* keyarg: keybinop exprseq  */
#line 1250 "lang11d"
                                { yyval = (intptr_t)newPyrPushKeyArgNode((PyrSlotNode*)yyvsp[-1], (PyrParseNode*)yyvsp[0]); }
#line 3831 "lang11d_tab.cpp"
    break;

  case 166: /* optkeyarglist: optcomma  */
#line 1253 "lang11d"
                           { yyval = 0; }
#line 3837 "lang11d_tab.cpp"
    break;

  case 167: /* optkeyarglist: ',' keyarglist1 optcomma  */
#line 1254 "lang11d"
                                                           { yyval = yyvsp[-1]; }
#line 3843 "lang11d_tab.cpp"
    break;

  case 168: /* mavars: mavarlist  */
#line 1258 "lang11d"
                        { yyval = (intptr_t)newPyrMultiAssignVarListNode((PyrSlotNode*)yyvsp[0], NULL); }
#line 3849 "lang11d_tab.cpp"
    break;

  case 169: /* mavars: mavarlist ELLIPSIS name  */
#line 1260 "lang11d"
                        { yyval = (intptr_t)newPyrMultiAssignVarListNode((PyrSlotNode*)yyvsp[-2], (PyrSlotNode*)yyvsp[0]); }
#line 3855 "lang11d_tab.cpp"
    break;

  case 171: /* mavarlist: mavarlist ',' name  */
#line 1265 "lang11d"
                                        { yyval = (intptr_t)linkNextNode((PyrParseNode*)yyvsp[-2], (PyrParseNode*)yyvsp[0]); }
#line 3861 "lang11d_tab.cpp"
    break;

  case 172: /* slotliteral: integer  */
#line 1269 "lang11d"
                                { yyval = (intptr_t)newPyrLiteralNode((PyrSlotNode*)yyvsp[0], NULL); }
#line 3867 "lang11d_tab.cpp"
    break;

  case 173: /* slotliteral: floatp  */
#line 1270 "lang11d"
                                { yyval = (intptr_t)newPyrLiteralNode((PyrSlotNode*)yyvsp[0], NULL); }
#line 3873 "lang11d_tab.cpp"
    break;

  case 174: /* slotliteral: ascii  */
#line 1271 "lang11d"
                                { yyval = (intptr_t)newPyrLiteralNode((PyrSlotNode*)yyvsp[0], NULL); }
#line 3879 "lang11d_tab.cpp"
    break;

  case 175: /* slotliteral: string  */
#line 1272 "lang11d"
                                { yyval = (intptr_t)newPyrLiteralNode((PyrSlotNode*)yyvsp[0], NULL); }
#line 3885 "lang11d_tab.cpp"
    break;

  case 176: /* slotliteral: symbol  */
#line 1273 "lang11d"
                                { yyval = (intptr_t)newPyrLiteralNode((PyrSlotNode*)yyvsp[0], NULL); }
#line 3891 "lang11d_tab.cpp"
    break;

  case 177: /* slotliteral: trueobj  */
#line 1274 "lang11d"
                                { yyval = (intptr_t)newPyrLiteralNode((PyrSlotNode*)yyvsp[0], NULL); }
#line 3897 "lang11d_tab.cpp"
    break;

  case 178: /* slotliteral: falseobj  */
#line 1275 "lang11d"
                                { yyval = (intptr_t)newPyrLiteralNode((PyrSlotNode*)yyvsp[0], NULL); }
#line 3903 "lang11d_tab.cpp"
    break;

  case 179: /* slotliteral: nilobj  */
#line 1276 "lang11d"
                                { yyval = (intptr_t)newPyrLiteralNode((PyrSlotNode*)yyvsp[0], NULL); }
#line 3909 "lang11d_tab.cpp"
    break;

  case 180: /* slotliteral: listlit  */
#line 1277 "lang11d"
                                { yyval = (intptr_t)newPyrLiteralNode(NULL, (PyrParseNode*)yyvsp[0]); }
#line 3915 "lang11d_tab.cpp"
    break;

  case 181: /* blockliteral: block  */
#line 1280 "lang11d"
                        { yyval = (intptr_t)newPyrPushLitNode(NULL, (PyrParseNode*)yyvsp[0]); }
#line 3921 "lang11d_tab.cpp"
    break;

  case 182: /* pushname: name  */
#line 1283 "lang11d"
                                { yyval = (intptr_t)newPyrPushNameNode((PyrSlotNode*)yyvsp[0]); }
#line 3927 "lang11d_tab.cpp"
    break;

  case 183: /* pushliteral: integer  */
#line 1286 "lang11d"
                                { yyval = (intptr_t)newPyrPushLitNode((PyrSlotNode*)yyvsp[0], NULL); }
#line 3933 "lang11d_tab.cpp"
    break;

  case 184: /* pushliteral: floatp  */
#line 1287 "lang11d"
                                        { yyval = (intptr_t)newPyrPushLitNode((PyrSlotNode*)yyvsp[0], NULL); }
#line 3939 "lang11d_tab.cpp"
    break;

  case 185: /* pushliteral: ascii  */
#line 1288 "lang11d"
                                        { yyval = (intptr_t)newPyrPushLitNode((PyrSlotNode*)yyvsp[0], NULL); }
#line 3945 "lang11d_tab.cpp"
    break;

  case 186: /* pushliteral: string  */
#line 1289 "lang11d"
                                        { yyval = (intptr_t)newPyrPushLitNode((PyrSlotNode*)yyvsp[0], NULL); }
#line 3951 "lang11d_tab.cpp"
    break;

  case 187: /* pushliteral: symbol  */
#line 1290 "lang11d"
                                        { yyval = (intptr_t)newPyrPushLitNode((PyrSlotNode*)yyvsp[0], NULL); }
#line 3957 "lang11d_tab.cpp"
    break;

  case 188: /* pushliteral: trueobj  */
#line 1291 "lang11d"
                                        { yyval = (intptr_t)newPyrPushLitNode((PyrSlotNode*)yyvsp[0], NULL); }
#line 3963 "lang11d_tab.cpp"
    break;

  case 189: /* pushliteral: falseobj  */
#line 1292 "lang11d"
                                        { yyval = (intptr_t)newPyrPushLitNode((PyrSlotNode*)yyvsp[0], NULL); }
#line 3969 "lang11d_tab.cpp"
    break;

  case 190: /* pushliteral: nilobj  */
#line 1293 "lang11d"
                                        { yyval = (intptr_t)newPyrPushLitNode((PyrSlotNode*)yyvsp[0], NULL); }
#line 3975 "lang11d_tab.cpp"
    break;

  case 191: /* pushliteral: listlit  */
#line 1294 "lang11d"
                                        { yyval = (intptr_t)newPyrPushLitNode(NULL, (PyrParseNode*)yyvsp[0]); }
#line 3981 "lang11d_tab.cpp"
    break;

  case 192: /* listliteral: integer  */
#line 1297 "lang11d"
                                { yyval = (intptr_t)newPyrLiteralNode((PyrSlotNode*)yyvsp[0], NULL); }
#line 3987 "lang11d_tab.cpp"
    break;

  case 193: /* listliteral: floatp  */
#line 1298 "lang11d"
                                        { yyval = (intptr_t)newPyrLiteralNode((PyrSlotNode*)yyvsp[0], NULL); }
#line 3993 "lang11d_tab.cpp"
    break;

  case 194: /* listliteral: ascii  */
#line 1299 "lang11d"
                                        { yyval = (intptr_t)newPyrLiteralNode((PyrSlotNode*)yyvsp[0], NULL); }
#line 3999 "lang11d_tab.cpp"
    break;

  case 195: /* listliteral: string  */
#line 1300 "lang11d"
                                        { yyval = (intptr_t)newPyrLiteralNode((PyrSlotNode*)yyvsp[0], NULL); }
#line 4005 "lang11d_tab.cpp"
    break;

  case 196: /* listliteral: symbol  */
#line 1301 "lang11d"
                                        { yyval = (intptr_t)newPyrLiteralNode((PyrSlotNode*)yyvsp[0], NULL); }
#line 4011 "lang11d_tab.cpp"
    break;

  case 197: /* listliteral: name  */
#line 1302 "lang11d"
                                        { yyval = (intptr_t)newPyrLiteralNode((PyrSlotNode*)yyvsp[0], NULL); }
#line 4017 "lang11d_tab.cpp"
    break;

  case 198: /* listliteral: trueobj  */
#line 1303 "lang11d"
                                        { yyval = (intptr_t)newPyrLiteralNode((PyrSlotNode*)yyvsp[0], NULL); }
#line 4023 "lang11d_tab.cpp"
    break;

  case 199: /* listliteral: falseobj  */
#line 1304 "lang11d"
                                        { yyval = (intptr_t)newPyrLiteralNode((PyrSlotNode*)yyvsp[0], NULL); }
#line 4029 "lang11d_tab.cpp"
    break;

  case 200: /* listliteral: nilobj  */
#line 1305 "lang11d"
                                        { yyval = (intptr_t)newPyrLiteralNode((PyrSlotNode*)yyvsp[0], NULL); }
#line 4035 "lang11d_tab.cpp"
    break;

  case 201: /* listliteral: listlit2  */
#line 1306 "lang11d"
                                        { yyval = (intptr_t)newPyrLiteralNode(NULL, (PyrParseNode*)yyvsp[0]); }
#line 4041 "lang11d_tab.cpp"
    break;

  case 202: /* listliteral: dictlit2  */
#line 1307 "lang11d"
                                    { yyval = (intptr_t)newPyrLiteralNode(NULL, (PyrParseNode*)yyvsp[0]); }
#line 4047 "lang11d_tab.cpp"
    break;

  case 203: /* block: '{' argdecls funcvardecls funcbody '}'  */
#line 1311 "lang11d"
                                { yyval = (intptr_t)newPyrBlockNode((PyrArgListNode*)yyvsp[-3], (PyrVarListNode*)yyvsp[-2],
					(PyrParseNode*)yyvsp[-1], false); }
#line 4054 "lang11d_tab.cpp"
    break;

  case 204: /* block: BEGINCLOSEDFUNC argdecls funcvardecls funcbody '}'  */
#line 1314 "lang11d"
                                { yyval = (intptr_t)newPyrBlockNode((PyrArgListNode*)yyvsp[-3], (PyrVarListNode*)yyvsp[-2],
					(PyrParseNode*)yyvsp[-1], true); }
#line 4061 "lang11d_tab.cpp"
    break;

  case 205: /* funcvardecls: %empty  */
#line 1318 "lang11d"
                  { yyval = 0; }
#line 4067 "lang11d_tab.cpp"
    break;

  case 206: /* funcvardecls: funcvardecls funcvardecl  */
#line 1320 "lang11d"
                                        { yyval = (intptr_t)linkNextNode((PyrParseNode*)yyvsp[-1], (PyrParseNode*)yyvsp[0]); }
#line 4073 "lang11d_tab.cpp"
    break;

  case 208: /* funcvardecls1: funcvardecls1 funcvardecl  */
#line 1325 "lang11d"
                                        { yyval = (intptr_t)linkNextNode((PyrParseNode*)yyvsp[-1], (PyrParseNode*)yyvsp[0]); }
#line 4079 "lang11d_tab.cpp"
    break;

  case 209: /* funcvardecl: VAR vardeflist ';'  */
#line 1329 "lang11d"
                                { yyval = (intptr_t)newPyrVarListNode((PyrVarDefNode*)yyvsp[-1], varLocal); }
#line 4085 "lang11d_tab.cpp"
    break;

  case 210: /* argdecls: %empty  */
#line 1332 "lang11d"
                  { yyval = 0; }
#line 4091 "lang11d_tab.cpp"
    break;

  case 211: /* argdecls: ARG vardeflist ';'  */
#line 1334 "lang11d"
                                {
					yyval = (intptr_t)newPyrArgListNode((PyrVarDefNode*)yyvsp[-1], NULL, NULL);
				}
#line 4099 "lang11d_tab.cpp"
    break;

  case 212: /* argdecls: ARG vardeflist0 ELLIPSIS name ';'  */
#line 1338 "lang11d"
                                {
					yyval = (intptr_t)newPyrArgListNode((PyrVarDefNode*)yyvsp[-3], (PyrSlotNode*)yyvsp[-1], NULL);
				}
#line 4107 "lang11d_tab.cpp"
    break;

  case 213: /* argdecls: '|' slotdeflist '|'  */
#line 1342 "lang11d"
                                {
					yyval = (intptr_t)newPyrArgListNode((PyrVarDefNode*)yyvsp[-1], NULL, NULL);
				}
#line 4115 "lang11d_tab.cpp"
    break;

  case 214: /* argdecls: '|' slotdeflist0 ELLIPSIS name '|'  */
#line 1346 "lang11d"
                                {
					yyval = (intptr_t)newPyrArgListNode((PyrVarDefNode*)yyvsp[-3], (PyrSlotNode*)yyvsp[-1], NULL);
				}
#line 4123 "lang11d_tab.cpp"
    break;

  case 215: /* argdecls: '|' slotdeflist0 ELLIPSIS name ',' name '|'  */
#line 1350 "lang11d"
                            {
					yyval = (intptr_t)newPyrArgListNode((PyrVarDefNode*)yyvsp[-5], (PyrSlotNode*)yyvsp[-3], (PyrSlotNode*)yyvsp[-1]);
			    }
#line 4131 "lang11d_tab.cpp"
    break;

  case 216: /* argdecls1: ARG vardeflist ';'  */
#line 1356 "lang11d"
                                {
					yyval = (intptr_t)newPyrArgListNode((PyrVarDefNode*)yyvsp[-1], NULL, NULL);
				}
#line 4139 "lang11d_tab.cpp"
    break;

  case 217: /* argdecls1: ARG vardeflist0 ELLIPSIS name ';'  */
#line 1360 "lang11d"
                                {
					yyval = (intptr_t)newPyrArgListNode((PyrVarDefNode*)yyvsp[-3], (PyrSlotNode*)yyvsp[-1], NULL);
				}
#line 4147 "lang11d_tab.cpp"
    break;

  case 218: /* argdecls1: '|' slotdeflist '|'  */
#line 1364 "lang11d"
                                {
					yyval = (intptr_t)newPyrArgListNode((PyrVarDefNode*)yyvsp[-1], NULL, NULL);
				}
#line 4155 "lang11d_tab.cpp"
    break;

  case 219: /* argdecls1: '|' slotdeflist0 ELLIPSIS name '|'  */
#line 1368 "lang11d"
                                {
					yyval = (intptr_t)newPyrArgListNode((PyrVarDefNode*)yyvsp[-3], (PyrSlotNode*)yyvsp[-1], NULL);
				}
#line 4163 "lang11d_tab.cpp"
    break;

  case 220: /* argdecls1: '|' slotdeflist0 ELLIPSIS name ',' name '|'  */
#line 1372 "lang11d"
                            {
					yyval = (intptr_t)newPyrArgListNode((PyrVarDefNode*)yyvsp[-5], (PyrSlotNode*)yyvsp[-3], (PyrSlotNode*)yyvsp[-1]);
			    }
#line 4171 "lang11d_tab.cpp"
    break;

  case 222: /* constdeflist: constdeflist optcomma constdef  */
#line 1380 "lang11d"
                                        { yyval = (intptr_t)linkNextNode((PyrParseNode*)yyvsp[-2], (PyrParseNode*)yyvsp[0]); }
#line 4177 "lang11d_tab.cpp"
    break;

  case 223: /* constdef: rspec name '=' slotliteral  */
#line 1384 "lang11d"
                                { yyval = (intptr_t)newPyrVarDefNode((PyrSlotNode*)yyvsp[-2], (PyrParseNode*)yyvsp[0], yyvsp[-3]); }
#line 4183 "lang11d_tab.cpp"
    break;

  case 224: /* slotdeflist0: %empty  */
#line 1387 "lang11d"
                  { yyval = 0; }
#line 4189 "lang11d_tab.cpp"
    break;

  case 227: /* slotdeflist: slotdeflist optcomma slotdef  */
#line 1393 "lang11d"
                                { yyval = (intptr_t)linkNextNode((PyrParseNode*)yyvsp[-2], (PyrParseNode*)yyvsp[0]); }
#line 4195 "lang11d_tab.cpp"
    break;

  case 228: /* slotdef: name  */
#line 1397 "lang11d"
                                { yyval = (intptr_t)newPyrVarDefNode((PyrSlotNode*)yyvsp[0], NULL, 0); }
#line 4201 "lang11d_tab.cpp"
    break;

  case 229: /* slotdef: name optequal slotliteral  */
#line 1399 "lang11d"
                                { yyval = (intptr_t)newPyrVarDefNode((PyrSlotNode*)yyvsp[-2], (PyrParseNode*)yyvsp[0], 0); }
#line 4207 "lang11d_tab.cpp"
    break;

  case 230: /* slotdef: name optequal '(' exprseq ')'  */
#line 1401 "lang11d"
                                {
					PyrParseNode* node = (PyrParseNode*)yyvsp[-1];
					node->mParens = 1;
					yyval = (intptr_t)newPyrVarDefNode((PyrSlotNode*)yyvsp[-4], node, 0);
				}
#line 4217 "lang11d_tab.cpp"
    break;

  case 231: /* vardeflist0: %empty  */
#line 1408 "lang11d"
                  { yyval = 0; }
#line 4223 "lang11d_tab.cpp"
    break;

  case 234: /* vardeflist: vardeflist ',' vardef  */
#line 1414 "lang11d"
                                { yyval = (intptr_t)linkNextNode((PyrParseNode*)yyvsp[-2], (PyrParseNode*)yyvsp[0]); }
#line 4229 "lang11d_tab.cpp"
    break;

  case 235: /* vardef: name  */
#line 1418 "lang11d"
                                { yyval = (intptr_t)newPyrVarDefNode((PyrSlotNode*)yyvsp[0], NULL, 0); }
#line 4235 "lang11d_tab.cpp"
    break;

  case 236: /* vardef: name '=' expr  */
#line 1420 "lang11d"
                                { yyval = (intptr_t)newPyrVarDefNode((PyrSlotNode*)yyvsp[-2], (PyrParseNode*)yyvsp[0], 0); }
#line 4241 "lang11d_tab.cpp"
    break;

  case 237: /* vardef: name '(' exprseq ')'  */
#line 1422 "lang11d"
                                {
									PyrParseNode* node = (PyrParseNode*)yyvsp[-1];
									node->mParens = 1;
									yyval = (intptr_t)newPyrVarDefNode((PyrSlotNode*)yyvsp[-3], node, 0);
								}
#line 4251 "lang11d_tab.cpp"
    break;

  case 238: /* dictslotdef: exprseq ':' exprseq  */
#line 1430 "lang11d"
                                { yyval = (intptr_t)linkNextNode((PyrParseNode*)yyvsp[-2], (PyrParseNode*)yyvsp[0]); }
#line 4257 "lang11d_tab.cpp"
    break;

  case 239: /* dictslotdef: keybinop exprseq  */
#line 1432 "lang11d"
                                {
					PyrParseNode* key = newPyrPushLitNode((PyrSlotNode*)yyvsp[-1], NULL);
					yyval = (intptr_t)linkNextNode(key, (PyrParseNode*)yyvsp[0]);
				}
#line 4266 "lang11d_tab.cpp"
    break;

  case 241: /* dictslotlist1: dictslotlist1 ',' dictslotdef  */
#line 1440 "lang11d"
                                        { yyval = (intptr_t)linkNextNode((PyrParseNode*)yyvsp[-2], (PyrParseNode*)yyvsp[0]); }
#line 4272 "lang11d_tab.cpp"
    break;

  case 242: /* dictslotlist: %empty  */
#line 1443 "lang11d"
                  { yyval = 0; }
#line 4278 "lang11d_tab.cpp"
    break;

  case 245: /* rwslotdeflist: rwslotdeflist ',' rwslotdef  */
#line 1449 "lang11d"
                                        { yyval = (intptr_t)linkNextNode((PyrParseNode*)yyvsp[-2], (PyrParseNode*)yyvsp[0]); }
#line 4284 "lang11d_tab.cpp"
    break;

  case 246: /* rwslotdef: rwspec name  */
#line 1453 "lang11d"
                                        { yyval = (intptr_t)newPyrVarDefNode((PyrSlotNode*)yyvsp[0], NULL, yyvsp[-1]); }
#line 4290 "lang11d_tab.cpp"
    break;

  case 247: /* rwslotdef: rwspec name '=' slotliteral  */
#line 1455 "lang11d"
                                        { yyval = (intptr_t)newPyrVarDefNode((PyrSlotNode*)yyvsp[-2], (PyrParseNode*)yyvsp[0], yyvsp[-3]); }
#line 4296 "lang11d_tab.cpp"
    break;

  case 248: /* dictlit2: '(' litdictslotlist ')'  */
#line 1459 "lang11d"
                                { yyval = (intptr_t)newPyrLitDictNode((PyrParseNode*)yyvsp[-1]); }
#line 4302 "lang11d_tab.cpp"
    break;

  case 249: /* litdictslotdef: listliteral ':' listliteral  */
#line 1463 "lang11d"
                                { yyval = (intptr_t)linkNextNode((PyrParseNode*)yyvsp[-2], (PyrParseNode*)yyvsp[0]); }
#line 4308 "lang11d_tab.cpp"
    break;

  case 250: /* litdictslotdef: keybinop listliteral  */
#line 1465 "lang11d"
                                {
					PyrParseNode* key = newPyrPushLitNode((PyrSlotNode*)yyvsp[-1], NULL);
					yyval = (intptr_t)linkNextNode(key, (PyrParseNode*)yyvsp[0]);
				}
#line 4317 "lang11d_tab.cpp"
    break;

  case 252: /* litdictslotlist1: litdictslotlist1 ',' litdictslotdef  */
#line 1473 "lang11d"
                                        { yyval = (intptr_t)linkNextNode((PyrParseNode*)yyvsp[-2], (PyrParseNode*)yyvsp[0]); }
#line 4323 "lang11d_tab.cpp"
    break;

  case 253: /* litdictslotlist: %empty  */
#line 1476 "lang11d"
                  { yyval = 0; }
#line 4329 "lang11d_tab.cpp"
    break;

  case 255: /* listlit: '#' '[' literallistc ']'  */
#line 1483 "lang11d"
                                { yyval = (intptr_t)newPyrLitListNode(0, (PyrParseNode*)yyvsp[-1]); }
#line 4335 "lang11d_tab.cpp"
    break;

  case 256: /* listlit: '#' classname '[' literallistc ']'  */
#line 1485 "lang11d"
                                { yyval = (intptr_t)newPyrLitListNode((PyrParseNode*)yyvsp[-3], (PyrParseNode*)yyvsp[-1]); }
#line 4341 "lang11d_tab.cpp"
    break;

  case 257: /* listlit2: '[' literallistc ']'  */
#line 1489 "lang11d"
                                { yyval = (intptr_t)newPyrLitListNode(0, (PyrParseNode*)yyvsp[-1]); }
#line 4347 "lang11d_tab.cpp"
    break;

  case 258: /* listlit2: classname '[' literallistc ']'  */
#line 1491 "lang11d"
                                { yyval = (intptr_t)newPyrLitListNode((PyrParseNode*)yyvsp[-3], (PyrParseNode*)yyvsp[-1]); }
#line 4353 "lang11d_tab.cpp"
    break;

  case 259: /* literallistc: %empty  */
#line 1494 "lang11d"
                  { yyval = 0; }
#line 4359 "lang11d_tab.cpp"
    break;

  case 262: /* literallist1: literallist1 ',' listliteral  */
#line 1500 "lang11d"
                                        { yyval = (intptr_t)linkNextNode((PyrParseNode*)yyvsp[-2], (PyrParseNode*)yyvsp[0]); }
#line 4365 "lang11d_tab.cpp"
    break;

  case 263: /* rwspec: %empty  */
#line 1503 "lang11d"
           { yyval = rwPrivate; }
#line 4371 "lang11d_tab.cpp"
    break;

  case 264: /* rwspec: '<'  */
#line 1505 "lang11d"
                        { yyval = rwReadOnly; }
#line 4377 "lang11d_tab.cpp"
    break;

  case 265: /* rwspec: READWRITEVAR  */
#line 1507 "lang11d"
                        { yyval = rwReadWrite; }
#line 4383 "lang11d_tab.cpp"
    break;

  case 266: /* rwspec: '>'  */
#line 1509 "lang11d"
                        { yyval = rwWriteOnly; }
#line 4389 "lang11d_tab.cpp"
    break;

  case 267: /* rspec: %empty  */
#line 1512 "lang11d"
           { yyval = rwPrivate; }
#line 4395 "lang11d_tab.cpp"
    break;

  case 268: /* rspec: '<'  */
#line 1514 "lang11d"
                        { yyval = rwReadOnly; }
#line 4401 "lang11d_tab.cpp"
    break;

  case 269: /* integer: INTEGER  */
#line 1517 "lang11d"
                  { yyval = zzval; }
#line 4407 "lang11d_tab.cpp"
    break;

  case 270: /* integer: '-' INTEGER  */
#line 1519 "lang11d"
                        {
				PyrSlotNode *node;
				node = (PyrSlotNode*)zzval;
				SetRaw(&node->mSlot, -slotRawInt(&node->mSlot));
				yyval = zzval;
			}
#line 4418 "lang11d_tab.cpp"
    break;

  case 271: /* floatr: SC_FLOAT  */
#line 1527 "lang11d"
                   { yyval = zzval; }
#line 4424 "lang11d_tab.cpp"
    break;

  case 272: /* floatr: '-' SC_FLOAT  */
#line 1529 "lang11d"
                        {
				PyrSlotNode *node;
				node = (PyrSlotNode*)zzval;
				SetRaw(&node->mSlot, -slotRawFloat(&node->mSlot));
				yyval = zzval;
			}
#line 4435 "lang11d_tab.cpp"
    break;

  case 273: /* accidental: ACCIDENTAL  */
#line 1537 "lang11d"
                        { yyval = zzval; }
#line 4441 "lang11d_tab.cpp"
    break;

  case 274: /* accidental: '-' ACCIDENTAL  */
#line 1539 "lang11d"
                                {
					PyrSlotNode *node;
					double intval, fracval;
					node = (PyrSlotNode*)zzval;
					intval = floor(slotRawFloat(&node->mSlot) + 0.5);
					fracval = slotRawFloat(&node->mSlot) - intval;
					SetRaw(&node->mSlot, -intval + fracval);
					yyval = zzval;
				}
#line 4455 "lang11d_tab.cpp"
    break;

  case 275: /* pie: PIE  */
#line 1549 "lang11d"
                      { yyval = zzval; }
#line 4461 "lang11d_tab.cpp"
    break;

  case 278: /* floatp: floatr pie  */
#line 1555 "lang11d"
                        {
				PyrSlotNode *node;
				node = (PyrSlotNode*)yyvsp[-1];
				SetRaw(&node->mSlot, slotRawFloat(&node->mSlot) * pi);
			}
#line 4471 "lang11d_tab.cpp"
    break;

  case 279: /* floatp: integer pie  */
#line 1561 "lang11d"
                        {
				PyrSlotNode *node;
				double ival;
				node = (PyrSlotNode*)yyvsp[-1];
				ival = slotRawInt(&node->mSlot);
				SetFloat(&node->mSlot, ival * pi);
			}
#line 4483 "lang11d_tab.cpp"
    break;

  case 280: /* floatp: pie  */
#line 1569 "lang11d"
                        {
				PyrSlotNode *node;
				node = (PyrSlotNode*)zzval;
				SetFloat(&node->mSlot, pi);
				yyval = zzval;
			}
#line 4494 "lang11d_tab.cpp"
    break;

  case 281: /* floatp: '-' pie  */
#line 1576 "lang11d"
                        {
				PyrSlotNode *node;
				node = (PyrSlotNode*)zzval;
				SetFloat(&node->mSlot, -pi);
				yyval = zzval;
			}
#line 4505 "lang11d_tab.cpp"
    break;

  case 282: /* name: NAME  */
#line 1584 "lang11d"
                       { yyval = zzval; }
#line 4511 "lang11d_tab.cpp"
    break;

  case 283: /* name: WHILE  */
#line 1585 "lang11d"
                                { yyval = zzval; }
#line 4517 "lang11d_tab.cpp"
    break;

  case 284: /* classname: CLASSNAME  */
#line 1588 "lang11d"
                                    { yyval = zzval; }
#line 4523 "lang11d_tab.cpp"
    break;

  case 285: /* primname: PRIMITIVENAME  */
#line 1591 "lang11d"
                                        { yyval = zzval; }
#line 4529 "lang11d_tab.cpp"
    break;

  case 286: /* trueobj: TRUEOBJ  */
#line 1594 "lang11d"
                          { yyval = zzval; }
#line 4535 "lang11d_tab.cpp"
    break;

  case 287: /* falseobj: FALSEOBJ  */
#line 1597 "lang11d"
                           { yyval = zzval; }
#line 4541 "lang11d_tab.cpp"
    break;

  case 288: /* nilobj: NILOBJ  */
#line 1600 "lang11d"
                         { yyval = zzval; }
#line 4547 "lang11d_tab.cpp"
    break;

  case 289: /* ascii: ASCII  */
#line 1603 "lang11d"
                        { yyval = zzval; }
#line 4553 "lang11d_tab.cpp"
    break;

  case 290: /* symbol: SYMBOL  */
#line 1606 "lang11d"
                         { yyval = zzval; }
#line 4559 "lang11d_tab.cpp"
    break;

  case 291: /* string: STRING  */
#line 1609 "lang11d"
                         { yyval = zzval; }
#line 4565 "lang11d_tab.cpp"
    break;

  case 292: /* pseudovar: PSEUDOVAR  */
#line 1612 "lang11d"
                            { yyval = zzval; }
#line 4571 "lang11d_tab.cpp"
    break;

  case 293: /* binop: BINOP  */
#line 1615 "lang11d"
                { yyval = zzval; }
#line 4577 "lang11d_tab.cpp"
    break;

  case 294: /* binop: READWRITEVAR  */
#line 1616 "lang11d"
                               { yyval = zzval; }
#line 4583 "lang11d_tab.cpp"
    break;

  case 295: /* binop: '<'  */
#line 1617 "lang11d"
                       { yyval = zzval; }
#line 4589 "lang11d_tab.cpp"
    break;

  case 296: /* binop: '>'  */
#line 1618 "lang11d"
                       { yyval = zzval; }
#line 4595 "lang11d_tab.cpp"
    break;

  case 297: /* binop: '-'  */
#line 1619 "lang11d"
                       { yyval = zzval; }
#line 4601 "lang11d_tab.cpp"
    break;

  case 298: /* binop: '*'  */
#line 1620 "lang11d"
                       { yyval = zzval; }
#line 4607 "lang11d_tab.cpp"
    break;

  case 299: /* binop: '+'  */
#line 1621 "lang11d"
                       { yyval = zzval; }
#line 4613 "lang11d_tab.cpp"
    break;

  case 300: /* binop: '|'  */
#line 1622 "lang11d"
                       { yyval = zzval; }
#line 4619 "lang11d_tab.cpp"
    break;

  case 301: /* keybinop: KEYBINOP  */
#line 1625 "lang11d"
                    { yyval = zzval; }
#line 4625 "lang11d_tab.cpp"
    break;

  case 304: /* curryarg: CURRYARG  */
#line 1632 "lang11d"
                    { yyval = zzval; }
#line 4631 "lang11d_tab.cpp"
    break;


#line 4635 "lang11d_tab.cpp"

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

