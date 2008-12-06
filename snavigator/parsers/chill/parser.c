/* A Bison parser, made by GNU Bison 1.875c.  */

/* Skeleton parser for Yacc-like parsing with Bison,
   Copyright (C) 1984, 1989, 1990, 2000, 2001, 2002, 2003 Free Software Foundation, Inc.

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 59 Temple Place - Suite 330,
   Boston, MA 02111-1307, USA.  */

/* As a special exception, when this file is copied by Bison into a
   Bison output file, you may use that output file without restriction.
   This special exception was added by the Free Software Foundation
   in version 1.24 of Bison.  */

/* Written by Richard Stallman by simplifying the original so called
   ``semantic'' parser.  */

/* All symbols defined below should begin with yy or YY, to avoid
   infringing on user name space.  This should be done even for local
   variables, as they might otherwise be expanded by user macros.
   There are some unavoidable exceptions within include files to
   define necessary library symbols; they are noted "INFRINGES ON
   USER NAME SPACE" below.  */

/* Identify Bison output.  */
#define YYBISON 1

/* Skeleton name.  */
#define YYSKELETON_NAME "yacc.c"

/* Pure parsers.  */
#define YYPURE 0

/* Using locations.  */
#define YYLSP_NEEDED 0



/* Tokens.  */
#ifndef YYTOKENTYPE
# define YYTOKENTYPE
   /* Put the tokens into the symbol table, so that GDB and other debuggers
      know about them.  */
   enum yytokentype {
     DYNAMIC = 258,
     IN = 259,
     OUT = 260,
     INOUT = 261,
     LOC = 262,
     ACCESS = 263,
     ARRAY = 264,
     BIN = 265,
     BOOLS = 266,
     BUFFER = 267,
     CHARS = 268,
     EVENT = 269,
     POWERSET = 270,
     RANGE = 271,
     READ = 272,
     REF = 273,
     ROW = 274,
     SET = 275,
     STRUCT = 276,
     TEXT = 277,
     MODULE = 278,
     REGION = 279,
     SPEC = 280,
     END = 281,
     AFTER = 282,
     ALL = 283,
     AND = 284,
     ANDIF = 285,
     ASSERT = 286,
     AT = 287,
     BASED = 288,
     BEG = 289,
     BODY = 290,
     BY = 291,
     CASE = 292,
     CAUSE = 293,
     CONTEXT = 294,
     CONTINUE = 295,
     CYCLE = 296,
     DCL = 297,
     DELAY = 298,
     DO = 299,
     DOWN = 300,
     ELSE = 301,
     ELSIF = 302,
     ESAC = 303,
     EVER = 304,
     EXCEPTIONS = 305,
     EXIT = 306,
     FI = 307,
     FOR = 308,
     FORBID = 309,
     GENERAL = 310,
     GOTO = 311,
     GRANT = 312,
     IF = 313,
     INIT = 314,
     INLINE = 315,
     MOD = 316,
     NEWMODE = 317,
     NONREF = 318,
     NOPACK = 319,
     NOT = 320,
     OD = 321,
     OF = 322,
     ON = 323,
     OR = 324,
     ORIF = 325,
     PACK = 326,
     POS = 327,
     PREFIXED = 328,
     PRIORITY = 329,
     PROC = 330,
     PROCESS = 331,
     RECEIVE = 332,
     RECURSIVE = 333,
     REM = 334,
     REMOTE = 335,
     RESULT = 336,
     RETURN = 337,
     RETURNS = 338,
     SEIZE = 339,
     SEND = 340,
     SIGNAL = 341,
     SIMPLE = 342,
     START = 343,
     STATIC = 344,
     STEP = 345,
     STOP = 346,
     SYN = 347,
     SYNMODE = 348,
     THEN = 349,
     THIS = 350,
     TIMEOUT = 351,
     TO = 352,
     UP = 353,
     VARYING = 354,
     WHILE = 355,
     WITH = 356,
     XOR = 357,
     BITLITERAL = 358,
     BOOLITERAL = 359,
     INTLITERAL = 360,
     STRINGLITERAL = 361,
     CHARLITERAL = 362,
     EMPTYLITERAL = 363,
     FLOATLITERAL = 364,
     NAME = 365,
     ASSIGN = 366,
     LESSTHANEQ = 367,
     GREATERTHANEQ = 368,
     NOTEQ = 369,
     LEFTTUPLE = 370,
     RIGHTTUPLE = 371,
     ARROW = 372,
     DOUBLESOLIDUS = 373,
     EXPONENT = 374
   };
#endif
#define DYNAMIC 258
#define IN 259
#define OUT 260
#define INOUT 261
#define LOC 262
#define ACCESS 263
#define ARRAY 264
#define BIN 265
#define BOOLS 266
#define BUFFER 267
#define CHARS 268
#define EVENT 269
#define POWERSET 270
#define RANGE 271
#define READ 272
#define REF 273
#define ROW 274
#define SET 275
#define STRUCT 276
#define TEXT 277
#define MODULE 278
#define REGION 279
#define SPEC 280
#define END 281
#define AFTER 282
#define ALL 283
#define AND 284
#define ANDIF 285
#define ASSERT 286
#define AT 287
#define BASED 288
#define BEG 289
#define BODY 290
#define BY 291
#define CASE 292
#define CAUSE 293
#define CONTEXT 294
#define CONTINUE 295
#define CYCLE 296
#define DCL 297
#define DELAY 298
#define DO 299
#define DOWN 300
#define ELSE 301
#define ELSIF 302
#define ESAC 303
#define EVER 304
#define EXCEPTIONS 305
#define EXIT 306
#define FI 307
#define FOR 308
#define FORBID 309
#define GENERAL 310
#define GOTO 311
#define GRANT 312
#define IF 313
#define INIT 314
#define INLINE 315
#define MOD 316
#define NEWMODE 317
#define NONREF 318
#define NOPACK 319
#define NOT 320
#define OD 321
#define OF 322
#define ON 323
#define OR 324
#define ORIF 325
#define PACK 326
#define POS 327
#define PREFIXED 328
#define PRIORITY 329
#define PROC 330
#define PROCESS 331
#define RECEIVE 332
#define RECURSIVE 333
#define REM 334
#define REMOTE 335
#define RESULT 336
#define RETURN 337
#define RETURNS 338
#define SEIZE 339
#define SEND 340
#define SIGNAL 341
#define SIMPLE 342
#define START 343
#define STATIC 344
#define STEP 345
#define STOP 346
#define SYN 347
#define SYNMODE 348
#define THEN 349
#define THIS 350
#define TIMEOUT 351
#define TO 352
#define UP 353
#define VARYING 354
#define WHILE 355
#define WITH 356
#define XOR 357
#define BITLITERAL 358
#define BOOLITERAL 359
#define INTLITERAL 360
#define STRINGLITERAL 361
#define CHARLITERAL 362
#define EMPTYLITERAL 363
#define FLOATLITERAL 364
#define NAME 365
#define ASSIGN 366
#define LESSTHANEQ 367
#define GREATERTHANEQ 368
#define NOTEQ 369
#define LEFTTUPLE 370
#define RIGHTTUPLE 371
#define ARROW 372
#define DOUBLESOLIDUS 373
#define EXPONENT 374




/* Copy the first part of user declarations.  */
#line 38 "./parser.y"


#include <assert.h>
#include <stdlib.h>
#include <string.h>

#include <tcl.h>

#include "common.h"
#include "symtab.h"
#include "emit.h"

static char brackets[] = "()";

/* Should we emit information about procedure calls and variables when
   encountered inside an expr?  This will mostly be set to 1, but
   there are some situations when this should be set to 0. */

static int emit = 0;



/* Enabling traces.  */
#ifndef YYDEBUG
# define YYDEBUG 0
#endif

/* Enabling verbose error messages.  */
#ifdef YYERROR_VERBOSE
# undef YYERROR_VERBOSE
# define YYERROR_VERBOSE 1
#else
# define YYERROR_VERBOSE 0
#endif

#if ! defined (YYSTYPE) && ! defined (YYSTYPE_IS_DECLARED)
#line 60 "./parser.y"
typedef union YYSTYPE {
   struct punctuation punct;
   struct blockregion block;
   struct identifier id;
   struct identifierlist * idlist;
   struct kword keyword;
   struct datatype type;
   struct attribute attrib;
   struct sig signature;
} YYSTYPE;
/* Line 186 of yacc.c.  */
#line 347 "parser.c"
# define yystype YYSTYPE /* obsolescent; will be withdrawn */
# define YYSTYPE_IS_DECLARED 1
# define YYSTYPE_IS_TRIVIAL 1
#endif



/* Copy the second part of user declarations.  */


/* Line 214 of yacc.c.  */
#line 359 "parser.c"

#if ! defined (yyoverflow) || YYERROR_VERBOSE

# ifndef YYFREE
#  define YYFREE free
# endif
# ifndef YYMALLOC
#  define YYMALLOC malloc
# endif

/* The parser invokes alloca or malloc; define the necessary symbols.  */

# ifdef YYSTACK_USE_ALLOCA
#  if YYSTACK_USE_ALLOCA
#   define YYSTACK_ALLOC alloca
#  endif
# else
#  if defined (alloca) || defined (_ALLOCA_H)
#   define YYSTACK_ALLOC alloca
#  else
#   ifdef __GNUC__
#    define YYSTACK_ALLOC __builtin_alloca
#   endif
#  endif
# endif

# ifdef YYSTACK_ALLOC
   /* Pacify GCC's `empty if-body' warning. */
#  define YYSTACK_FREE(Ptr) do { /* empty */; } while (0)
# else
#  if defined (__STDC__) || defined (__cplusplus)
#   include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
#   define YYSIZE_T size_t
#  endif
#  define YYSTACK_ALLOC YYMALLOC
#  define YYSTACK_FREE YYFREE
# endif
#endif /* ! defined (yyoverflow) || YYERROR_VERBOSE */


#if (! defined (yyoverflow) \
     && (! defined (__cplusplus) \
	 || (defined (YYSTYPE_IS_TRIVIAL) && YYSTYPE_IS_TRIVIAL)))

/* A type that is properly aligned for any stack member.  */
union yyalloc
{
  short yyss;
  YYSTYPE yyvs;
  };

/* The size of the maximum gap between one aligned stack and the next.  */
# define YYSTACK_GAP_MAXIMUM (sizeof (union yyalloc) - 1)

/* The size of an array large to enough to hold all stacks, each with
   N elements.  */
# define YYSTACK_BYTES(N) \
     ((N) * (sizeof (short) + sizeof (YYSTYPE))				\
      + YYSTACK_GAP_MAXIMUM)

/* Copy COUNT objects from FROM to TO.  The source and destination do
   not overlap.  */
# ifndef YYCOPY
#  if defined (__GNUC__) && 1 < __GNUC__
#   define YYCOPY(To, From, Count) \
      __builtin_memcpy (To, From, (Count) * sizeof (*(From)))
#  else
#   define YYCOPY(To, From, Count)		\
      do					\
	{					\
	  register YYSIZE_T yyi;		\
	  for (yyi = 0; yyi < (Count); yyi++)	\
	    (To)[yyi] = (From)[yyi];		\
	}					\
      while (0)
#  endif
# endif

/* Relocate STACK from its old location to the new one.  The
   local variables YYSIZE and YYSTACKSIZE give the old and new number of
   elements in the stack, and YYPTR gives the new location of the
   stack.  Advance YYPTR to a properly aligned location for the next
   stack.  */
# define YYSTACK_RELOCATE(Stack)					\
    do									\
      {									\
	YYSIZE_T yynewbytes;						\
	YYCOPY (&yyptr->Stack, Stack, yysize);				\
	Stack = &yyptr->Stack;						\
	yynewbytes = yystacksize * sizeof (*Stack) + YYSTACK_GAP_MAXIMUM; \
	yyptr += yynewbytes / sizeof (*yyptr);				\
      }									\
    while (0)

#endif

#if defined (__STDC__) || defined (__cplusplus)
   typedef signed char yysigned_char;
#else
   typedef short yysigned_char;
#endif

/* YYFINAL -- State number of the termination state. */
#define YYFINAL  2
/* YYLAST -- Last index in YYTABLE.  */
#define YYLAST   2737

/* YYNTOKENS -- Number of terminals. */
#define YYNTOKENS  134
/* YYNNTS -- Number of nonterminals. */
#define YYNNTS  142
/* YYNRULES -- Number of rules. */
#define YYNRULES  414
/* YYNRULES -- Number of states. */
#define YYNSTATES  912

/* YYTRANSLATE(YYLEX) -- Bison symbol number corresponding to YYLEX.  */
#define YYUNDEFTOK  2
#define YYMAXUTOK   374

#define YYTRANSLATE(YYX) 						\
  ((unsigned int) (YYX) <= YYMAXUTOK ? yytranslate[YYX] : YYUNDEFTOK)

/* YYTRANSLATE[YYLEX] -- Bison symbol number corresponding to YYLEX.  */
static const unsigned char yytranslate[] =
{
       0,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,   126,     2,     2,     2,     2,     2,     2,
     123,   124,   129,   131,   122,   128,   127,   130,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,   121,     3,
     133,   125,   132,     2,     2,     2,     2,     2,     2,     2,
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
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     1,     2,     4,     5,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    18,    19,    20,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    47,    48,    49,    50,    51,    52,    53,    54,    55,
      56,    57,    58,    59,    60,    61,    62,    63,    64,    65,
      66,    67,    68,    69,    70,    71,    72,    73,    74,    75,
      76,    77,    78,    79,    80,    81,    82,    83,    84,    85,
      86,    87,    88,    89,    90,    91,    92,    93,    94,    95,
      96,    97,    98,    99,   100,   101,   102,   103,   104,   105,
     106,   107,   108,   109,   110,   111,   112,   113,   114,   115,
     116,   117,   118,   119,   120
};

#if YYDEBUG
/* YYPRHS[YYN] -- Index of the first RHS symbol of rule number YYN in
   YYRHS.  */
static const unsigned short yyprhs[] =
{
       0,     0,     3,     4,     8,     9,    15,    16,    20,    21,
      27,    28,    35,    44,    47,    48,    50,    54,    56,    59,
      63,    65,    69,    74,    82,    93,    96,    99,   100,   107,
     114,   119,   121,   125,   130,   136,   143,   149,   156,   164,
     166,   168,   172,   174,   178,   183,   190,   192,   198,   202,
     210,   212,   216,   219,   222,   223,   225,   226,   228,   229,
     233,   239,   244,   248,   249,   251,   253,   257,   260,   262,
     270,   279,   282,   290,   294,   296,   299,   302,   308,   312,
     314,   316,   319,   322,   327,   333,   339,   348,   351,   354,
     360,   369,   375,   383,   385,   387,   388,   391,   393,   396,
     401,   407,   414,   421,   429,   430,   437,   445,   448,   453,
     456,   459,   462,   464,   466,   467,   469,   473,   481,   483,
     486,   489,   490,   493,   494,   497,   498,   500,   504,   506,
     510,   512,   516,   523,   526,   527,   534,   535,   542,   543,
     548,   550,   554,   558,   560,   562,   564,   566,   568,   570,
     572,   574,   576,   577,   582,   583,   585,   587,   592,   601,
     608,   615,   619,   627,   636,   645,   649,   657,   666,   675,
     680,   689,   699,   709,   711,   713,   717,   722,   725,   728,
     730,   734,   736,   740,   744,   748,   752,   754,   758,   762,
     766,   768,   772,   776,   780,   784,   788,   792,   796,   798,
     802,   806,   808,   812,   816,   820,   822,   824,   826,   828,
     830,   832,   834,   836,   838,   840,   842,   844,   846,   848,
     850,   851,   853,   855,   858,   861,   865,   869,   872,   875,
     878,   879,   883,   888,   891,   894,   897,   900,   904,   906,
     910,   914,   918,   922,   924,   927,   933,   936,   941,   948,
     957,   963,   971,   978,   987,   988,   991,   992,   999,  1000,
    1007,  1008,  1012,  1013,  1019,  1026,  1035,  1036,  1037,  1045,
    1053,  1060,  1069,  1070,  1074,  1078,  1082,  1086,  1090,  1094,
    1098,  1103,  1107,  1112,  1114,  1118,  1120,  1124,  1132,  1141,
    1143,  1147,  1148,  1152,  1153,  1158,  1159,  1161,  1162,  1164,
    1165,  1168,  1171,  1172,  1178,  1186,  1193,  1202,  1206,  1211,
    1217,  1225,  1227,  1230,  1236,  1240,  1244,  1246,  1250,  1252,
    1256,  1258,  1260,  1264,  1268,  1275,  1282,  1291,  1298,  1299,
    1300,  1309,  1311,  1314,  1318,  1320,  1324,  1331,  1333,  1337,
    1339,  1343,  1345,  1348,  1349,  1351,  1355,  1358,  1360,  1364,
    1368,  1377,  1385,  1389,  1394,  1395,  1397,  1401,  1403,  1404,
    1408,  1410,  1412,  1416,  1422,  1430,  1438,  1443,  1450,  1451,
    1452,  1458,  1459,  1466,  1467,  1473,  1475,  1479,  1483,  1485,
    1489,  1492,  1497,  1503,  1510,  1518,  1523,  1529,  1535,  1538,
    1544,  1546,  1551,  1558,  1564,  1566,  1569,  1571,  1578,  1581,
    1584,  1587,  1592,  1594,  1599,  1605,  1611,  1618,  1620,  1622,
    1624,  1626,  1628,  1629,  1631
};

/* YYRHS -- A `-1'-separated list of the rules' RHS. */
static const short yyrhs[] =
{
     135,     0,    -1,    -1,   135,   136,   148,    -1,    -1,   135,
     111,   121,   137,   148,    -1,    -1,   135,   138,   184,    -1,
      -1,   135,   111,   121,   139,   184,    -1,    -1,    38,   150,
      68,   141,   143,    49,    -1,    38,   150,    68,   141,   143,
      47,   218,    49,    -1,   142,     3,    -1,    -1,   262,    -1,
     142,   122,   262,    -1,   144,    -1,   143,   144,    -1,   244,
     121,   218,    -1,   111,    -1,   145,   122,   111,    -1,    44,
      38,   147,    49,    -1,    44,    38,    21,   203,     3,   147,
      49,    -1,    44,    38,    21,   203,     3,    75,   203,     3,
     147,    49,    -1,   149,   147,    -1,   165,   147,    -1,    -1,
      24,   219,    27,   159,   261,     3,    -1,    25,   219,    27,
     159,   261,     3,    -1,   123,   150,   124,   121,    -1,   203,
      -1,   203,   122,   150,    -1,    38,    68,   153,    49,    -1,
      38,    68,   153,    47,    49,    -1,    38,    68,   153,    47,
     152,    49,    -1,    38,   145,    68,   153,    49,    -1,    38,
     145,    68,   153,    47,    49,    -1,    38,   145,    68,   153,
      47,   152,    49,    -1,   190,    -1,   190,    -1,   152,   122,
     190,    -1,   249,    -1,   153,   122,   249,    -1,   123,   111,
     124,   121,    -1,   123,   111,     5,   150,   124,   121,    -1,
     111,    -1,   111,   125,   123,   156,   124,    -1,   111,    98,
     262,    -1,   111,   125,   123,   156,   124,    98,   262,    -1,
     273,    -1,   156,   122,   273,    -1,   154,   157,    -1,   165,
     157,    -1,    -1,   100,    -1,    -1,   160,    -1,    -1,    69,
     237,    27,    -1,    69,   237,    47,   218,    27,    -1,   123,
     260,   124,   121,    -1,   159,   261,     3,    -1,    -1,   164,
      -1,   203,    -1,   163,   122,   203,    -1,   166,   162,    -1,
       3,    -1,    28,   203,     5,   218,    97,   218,    27,    -1,
      28,   203,    44,     5,   218,    97,   218,    27,    -1,    32,
     203,    -1,    33,   191,     5,   218,    97,   218,    27,    -1,
      35,   219,    27,    -1,   140,    -1,    39,   262,    -1,    41,
     203,    -1,    42,   203,     5,   218,    27,    -1,    44,   203,
     180,    -1,   146,    -1,   246,    -1,    52,   262,    -1,    57,
     262,    -1,    59,   203,   212,    53,    -1,    59,   203,   212,
     211,    53,    -1,    78,    38,   157,   168,    49,    -1,    78,
      38,    21,   111,     3,   154,   168,    49,    -1,    82,   203,
      -1,    83,   167,    -1,    86,   262,   178,   179,   180,    -1,
      86,   262,   123,   150,   124,   178,   179,   180,    -1,    89,
     262,   123,   163,   124,    -1,    89,   262,   123,   163,   124,
      21,   111,    -1,    92,    -1,   203,    -1,    -1,    47,   218,
      -1,   175,    -1,   254,   210,    -1,    84,   123,   273,   124,
      -1,    84,   123,   273,     8,   124,    -1,    84,   123,   273,
       8,     4,   124,    -1,    84,   123,   273,    64,     8,   124,
      -1,    84,   123,   273,    64,     8,     4,   124,    -1,    -1,
      76,   123,   124,   170,   259,   174,    -1,    76,   123,   172,
     124,   170,   259,   174,    -1,   273,   274,    -1,   172,   122,
     273,   274,    -1,    56,   174,    -1,    88,   174,    -1,    61,
     174,    -1,   174,    -1,    79,    -1,    -1,   176,    -1,   175,
     122,   176,    -1,   123,   261,   118,   261,   124,   126,   255,
      -1,   175,    -1,   254,   210,    -1,   102,   203,    -1,    -1,
      98,   203,    -1,    -1,    75,   203,    -1,    -1,   182,    -1,
     181,   122,   182,    -1,   111,    -1,   111,   125,   203,    -1,
     155,    -1,   183,   122,   155,    -1,    26,    24,   185,    27,
     261,     3,    -1,   185,   226,    -1,    -1,   185,   111,   121,
      76,   186,   258,    -1,    -1,   185,   111,   121,    77,   187,
     268,    -1,    -1,    22,   123,   189,   124,    -1,   151,    -1,
     189,   122,   151,    -1,   145,   273,   205,    -1,   106,    -1,
     104,    -1,   105,    -1,   108,    -1,   109,    -1,   110,    -1,
     107,    -1,    96,    -1,   214,    -1,    -1,   192,   123,   163,
     124,    -1,    -1,   127,    -1,   111,    -1,   111,   123,   163,
     124,    -1,   111,   123,   163,   124,   123,   163,   124,   192,
      -1,   111,   123,   203,   121,   203,   124,    -1,   111,   123,
     203,    99,   203,   124,    -1,   118,   193,   111,    -1,   118,
     193,   111,   123,   163,   124,   192,    -1,   118,   193,   111,
     123,   203,   121,   203,   124,    -1,   118,   193,   111,   123,
     203,    99,   203,   124,    -1,   194,   127,   111,    -1,   194,
     127,   111,   123,   163,   124,   192,    -1,   194,   127,   111,
     123,   203,   121,   203,   124,    -1,   194,   127,   111,   123,
     203,    99,   203,   124,    -1,   194,   118,   193,   111,    -1,
     194,   118,   193,   111,   123,   163,   124,   192,    -1,   194,
     118,   193,   111,   123,   203,   121,   203,   124,    -1,   194,
     118,   193,   111,   123,   203,    99,   203,   124,    -1,   191,
      -1,   194,    -1,   123,   202,   124,    -1,   123,   202,   124,
     191,    -1,    66,   195,    -1,   128,   195,    -1,   195,    -1,
     197,   120,   196,    -1,   196,    -1,   198,   129,   197,    -1,
     198,   130,   197,    -1,   198,    62,   197,    -1,   198,    80,
     197,    -1,   197,    -1,   199,   131,   198,    -1,   199,   128,
     198,    -1,   199,   119,   198,    -1,   198,    -1,   200,     5,
     199,    -1,   200,   132,   199,    -1,   200,   114,   199,    -1,
     200,   133,   199,    -1,   200,   113,   199,    -1,   200,   125,
     199,    -1,   200,   115,   199,    -1,   199,    -1,   201,    30,
     200,    -1,   201,    31,   200,    -1,   200,    -1,   202,    70,
     201,    -1,   202,   103,   201,    -1,   202,    71,   201,    -1,
     201,    -1,   207,    -1,   239,    -1,   202,    -1,   131,    -1,
     128,    -1,   130,    -1,   129,    -1,    62,    -1,    80,    -1,
     119,    -1,    30,    -1,    70,    -1,   103,    -1,   206,    -1,
      -1,    72,    -1,    65,    -1,    73,   263,    -1,    91,   264,
      -1,    59,   208,    53,    -1,   203,   213,   209,    -1,    48,
     208,    -1,    47,   203,    -1,    74,   261,    -1,    -1,    48,
     203,   212,    -1,    48,   203,   212,   211,    -1,    47,   218,
      -1,    95,   218,    -1,    95,   203,    -1,   116,   117,    -1,
     116,   215,   117,    -1,   216,    -1,   215,   122,   216,    -1,
     217,   121,   203,    -1,   242,   121,   203,    -1,   203,   121,
     203,    -1,   203,    -1,   127,   111,    -1,   217,   122,   127,
     111,   122,    -1,   218,   165,    -1,   218,   111,   121,   165,
      -1,   218,   111,   123,   163,   124,   162,    -1,   218,   111,
     121,   111,   123,   163,   124,   162,    -1,   218,   231,   112,
     203,   162,    -1,   218,   111,   121,   231,   112,   203,   162,
      -1,   218,   231,   204,   112,   203,   162,    -1,   218,   111,
     121,   231,   204,   112,   203,   162,    -1,    -1,   219,   226,
      -1,    -1,   219,   111,   121,    76,   220,   258,    -1,    -1,
     219,   111,   121,    77,   221,   268,    -1,    -1,   219,   222,
     165,    -1,    -1,   219,   111,   121,   223,   165,    -1,   219,
     111,   123,   163,   124,   162,    -1,   219,   111,   121,   111,
     123,   163,   124,   162,    -1,    -1,    -1,   219,   231,   112,
     224,   203,   225,   162,    -1,   219,   111,   121,   231,   112,
     203,   162,    -1,   219,   231,   204,   112,   203,   162,    -1,
     219,   111,   121,   231,   204,   112,   203,   162,    -1,    -1,
      43,   229,     3,    -1,    58,   169,     3,    -1,    63,   256,
       3,    -1,    85,   177,     3,    -1,    87,   183,     3,    -1,
      93,   228,     3,    -1,    94,   256,     3,    -1,   145,   273,
     125,   203,    -1,   145,   125,   203,    -1,   145,   125,   111,
     203,    -1,   227,    -1,   228,   122,   227,    -1,   230,    -1,
     229,   122,   230,    -1,   145,   273,   234,   235,   236,   232,
     159,    -1,   145,   273,    90,   234,   235,   236,   232,   159,
      -1,   194,    -1,   231,   122,   194,    -1,    -1,   112,   233,
     203,    -1,    -1,    34,   123,   262,   124,    -1,    -1,     8,
      -1,    -1,    60,    -1,    -1,   237,   161,    -1,   237,   165,
      -1,    -1,   231,   112,   203,    98,   203,    -1,   231,   112,
     203,    37,   203,    98,   203,    -1,   231,   112,   203,    46,
      98,   203,    -1,   231,   112,   203,    37,   203,    46,    98,
     203,    -1,   231,     5,   203,    -1,   231,    46,     5,   203,
      -1,    38,   150,    68,   240,    49,    -1,    38,   150,    68,
     240,    47,   203,    49,    -1,   241,    -1,   240,   241,    -1,
     123,   244,   121,   203,     3,    -1,   123,   129,   124,    -1,
     123,   243,   124,    -1,   245,    -1,   243,   122,   245,    -1,
     242,    -1,   244,   122,   242,    -1,    47,    -1,   203,    -1,
     203,   121,   203,    -1,    45,   218,    67,    -1,    45,    54,
      50,     3,   218,    67,    -1,    45,    54,   252,     3,   218,
      67,    -1,    45,    54,   252,   101,   203,     3,   218,    67,
      -1,    45,   101,   203,     3,   218,    67,    -1,    -1,    -1,
      45,   102,   247,   203,     3,   218,   248,    67,    -1,   190,
      -1,   121,   190,    -1,   244,   121,   190,    -1,   251,    -1,
     250,   122,   251,    -1,   111,   123,   203,   121,   203,   124,
      -1,   273,    -1,   203,   121,   203,    -1,   238,    -1,   252,
     122,   238,    -1,    55,    -1,    55,    29,    -1,    -1,   255,
      -1,   254,   122,   255,    -1,   261,   253,    -1,   257,    -1,
     256,   122,   257,    -1,   145,   125,   273,    -1,   123,   271,
     124,   170,   259,   173,   275,   265,    -1,   123,   124,   170,
     259,   173,   275,   265,    -1,    51,   123,   124,    -1,    51,
     123,   260,   124,    -1,    -1,   262,    -1,   260,   122,   262,
      -1,   262,    -1,    -1,   111,   126,   261,    -1,   111,    -1,
      29,    -1,   123,   203,   124,    -1,   123,   203,   122,   203,
     124,    -1,   123,   203,   122,   203,   122,   203,   124,    -1,
     123,   203,   122,   203,   121,   203,   124,    -1,   123,    73,
     263,   124,    -1,   123,    73,   263,   122,   203,   124,    -1,
      -1,    -1,   266,   219,   267,    27,   162,    -1,    -1,   123,
     271,   124,   269,   275,   265,    -1,    -1,   123,   124,   270,
     275,   265,    -1,   272,    -1,   271,   122,   272,    -1,   145,
     273,   274,    -1,     9,    -1,     9,   273,     4,    -1,     9,
     273,    -1,     9,   123,   251,   124,    -1,     9,   123,   251,
     124,   273,    -1,     9,   123,   251,   124,   273,     4,    -1,
      10,   123,   250,   124,   158,   273,   205,    -1,    11,   123,
     203,   124,    -1,    12,   123,   203,   124,   158,    -1,    13,
     123,   203,   124,   273,    -1,    13,   273,    -1,    14,   123,
     203,   124,   158,    -1,    15,    -1,    15,   123,   203,   124,
      -1,   273,   123,   203,   121,   203,   124,    -1,   273,   123,
     203,   124,   158,    -1,   111,    -1,    16,   273,    -1,   171,
      -1,    17,   123,   203,   121,   203,   124,    -1,    18,   273,
      -1,    19,   273,    -1,    20,   273,    -1,    21,   123,   181,
     124,    -1,   188,    -1,    23,   123,   203,   124,    -1,    23,
     123,   203,   124,     4,    -1,    23,   123,   203,   124,   273,
      -1,    23,   123,   203,   124,   273,     4,    -1,     5,    -1,
       6,    -1,     7,    -1,     8,    -1,     4,    -1,    -1,     3,
      -1,    -1
};

/* YYRLINE[YYN] -- source line where rule number YYN was defined.  */
static const unsigned short yyrline[] =
{
       0,   118,   118,   118,   122,   122,   126,   126,   130,   130,
     134,   140,   141,   145,   146,   150,   151,   155,   156,   160,
     166,   172,   181,   182,   183,   187,   188,   189,   193,   200,
     210,   214,   215,   219,   220,   221,   222,   223,   224,   225,
     229,   230,   234,   235,   239,   240,   244,   246,   248,   250,
     255,   257,   262,   263,   264,   268,   269,   273,   274,   278,
     279,   283,   287,   290,   292,   296,   297,   301,   302,   307,
     308,   309,   310,   311,   312,   313,   314,   315,   316,   317,
     318,   319,   320,   321,   322,   323,   324,   325,   326,   327,
     328,   329,   333,   337,   341,   342,   346,   350,   351,   355,
     359,   363,   367,   371,   376,   382,   386,   393,   395,   400,
     401,   402,   403,   407,   408,   412,   413,   417,   421,   422,
     426,   427,   431,   432,   436,   437,   441,   447,   456,   460,
     467,   468,   472,   482,   483,   483,   484,   484,   485,   489,
     496,   497,   501,   508,   509,   510,   511,   512,   513,   514,
     515,   516,   519,   521,   524,   526,   530,   535,   540,   545,
     550,   555,   560,   565,   570,   575,   576,   577,   578,   579,
     580,   581,   582,   586,   587,   611,   612,   616,   617,   618,
     622,   623,   627,   628,   629,   630,   631,   635,   636,   637,
     638,   642,   643,   644,   645,   646,   647,   648,   649,   653,
     654,   655,   659,   660,   661,   662,   666,   667,   668,   672,
     672,   672,   672,   672,   672,   672,   672,   672,   672,   676,
     677,   681,   682,   683,   684,   688,   693,   697,   699,   704,
     705,   709,   711,   713,   718,   723,   727,   728,   732,   733,
     737,   738,   739,   740,   744,   745,   749,   750,   751,   755,
     759,   763,   767,   771,   775,   779,   780,   780,   784,   784,
     788,   788,   789,   789,   790,   794,   798,   798,   798,   802,
     806,   810,   814,   818,   819,   820,   821,   822,   823,   824,
     828,   832,   836,   843,   844,   848,   849,   853,   862,   874,
     880,   889,   889,   890,   894,   895,   899,   904,   910,   911,
     915,   916,   917,   921,   925,   929,   933,   937,   939,   944,
     946,   951,   953,   958,   962,   963,   967,   968,   972,   973,
     977,   978,   979,   983,   984,   985,   986,   987,   988,   988,
     988,   992,   993,   994,   998,  1000,  1005,  1007,  1009,  1014,
    1016,  1021,  1022,  1023,  1027,  1028,  1032,  1039,  1040,  1044,
    1051,  1078,  1086,  1087,  1088,  1092,  1094,  1099,  1102,  1106,
    1110,  1114,  1119,  1120,  1121,  1122,  1126,  1127,  1131,  1131,
    1131,  1135,  1135,  1141,  1141,  1150,  1156,  1165,  1209,  1213,
    1217,  1221,  1226,  1231,  1236,  1240,  1244,  1248,  1252,  1256,
    1260,  1264,  1268,  1272,  1276,  1280,  1284,  1288,  1292,  1296,
    1300,  1304,  1308,  1312,  1316,  1320,  1324,  1331,  1335,  1339,
    1343,  1347,  1352,  1358,  1360
};
#endif

#if YYDEBUG || YYERROR_VERBOSE
/* YYTNME[SYMBOL-NUM] -- String name of the symbol SYMBOL-NUM.
   First, the terminals, then, starting at YYNTOKENS, nonterminals. */
static const char *const yytname[] =
{
  "$end", "error", "$undefined", "';'", "DYNAMIC", "IN", "OUT", "INOUT",
  "LOC", "ACCESS", "ARRAY", "BIN", "BOOLS", "BUFFER", "CHARS", "EVENT",
  "POWERSET", "RANGE", "READ", "REF", "ROW", "SET", "STRUCT", "TEXT",
  "MODULE", "REGION", "SPEC", "END", "AFTER", "ALL", "AND", "ANDIF",
  "ASSERT", "AT", "BASED", "BEG", "BODY", "BY", "CASE", "CAUSE", "CONTEXT",
  "CONTINUE", "CYCLE", "DCL", "DELAY", "DO", "DOWN", "ELSE", "ELSIF",
  "ESAC", "EVER", "EXCEPTIONS", "EXIT", "FI", "FOR", "FORBID", "GENERAL",
  "GOTO", "GRANT", "IF", "INIT", "INLINE", "MOD", "NEWMODE", "NONREF",
  "NOPACK", "NOT", "OD", "OF", "ON", "OR", "ORIF", "PACK", "POS",
  "PREFIXED", "PRIORITY", "PROC", "PROCESS", "RECEIVE", "RECURSIVE", "REM",
  "REMOTE", "RESULT", "RETURN", "RETURNS", "SEIZE", "SEND", "SIGNAL",
  "SIMPLE", "START", "STATIC", "STEP", "STOP", "SYN", "SYNMODE", "THEN",
  "THIS", "TIMEOUT", "TO", "UP", "VARYING", "WHILE", "WITH", "XOR",
  "BITLITERAL", "BOOLITERAL", "INTLITERAL", "STRINGLITERAL", "CHARLITERAL",
  "EMPTYLITERAL", "FLOATLITERAL", "NAME", "ASSIGN", "LESSTHANEQ",
  "GREATERTHANEQ", "NOTEQ", "LEFTTUPLE", "RIGHTTUPLE", "ARROW",
  "DOUBLESOLIDUS", "EXPONENT", "':'", "','", "'('", "')'", "'='", "'!'",
  "'.'", "'-'", "'*'", "'/'", "'+'", "'>'", "'<'", "$accept", "program",
  "@1", "@2", "@3", "@4", "case_action", "opt_range_list", "range_list",
  "case_alternative_list", "case_alternative", "def_occ_list",
  "delay_case_action", "dcase_event_superlist", "modulion",
  "delay_case_event_list", "expr_list", "field", "variant_field_list",
  "variant_alternative_list", "receive_spec", "signal_definition",
  "mode_list", "receive_spec_list", "opt_varying", "opt_handler",
  "handler", "on_exception_list", "end", "args", "arglist", "action",
  "other_actions", "opt_expr", "opt_else_actions", "grant_stmt",
  "opt_result_spec", "procedure_mode", "proc_mode_list",
  "opt_procedureattr", "opt_recursive", "rename_clauses", "rename_clause",
  "seize_stmt", "opt_with_expr", "opt_to_expr", "opt_prio_expr",
  "set_list", "set_list_element", "signal_definition_stmt", "spec_module",
  "spec_definitions", "@5", "@6", "structure_mode", "fields",
  "fixed_field", "primval", "opt_args", "opt_dot", "variable", "operand7",
  "operand6", "operand5", "operand4", "operand3", "operand2", "operand1",
  "operand0", "expr", "dyadic_operator", "opt_layout", "layout", "if_expr",
  "if_expr_body", "else_alternative", "opt_prefix_clause", "else_clause",
  "then_clause", "then_alternative", "tuple", "tuple_element_list",
  "tuple_element", "tuple_fieldname_list", "opt_actions", "body", "@7",
  "@8", "@9", "@10", "@11", "@12", "definition", "synonym_definition",
  "synonym_definition_stmt", "declaration_stmt", "declaration",
  "variable_list", "opt_assign", "@13", "opt_based", "opt_loc", "opt_init",
  "on_alternatives", "iteration", "case_expr", "case_way_list", "case_way",
  "case_label_list", "case_label_list2", "case_label_specification",
  "case_label", "do_action", "@14", "@15", "variant_alternative",
  "index_mode_list", "index_mode", "iteration_list", "opt_forbid",
  "postfix_list", "postfix", "mode_definition_stmt", "mode_definition",
  "procedure_definition", "opt_except", "name_string_list",
  "opt_name_string", "name_string", "pos", "step", "proc_body", "@16",
  "@17", "process_definition", "@18", "@19", "param_list", "param", "mode",
  "param_attr", "opt_semicolon", 0
};
#endif

# ifdef YYPRINT
/* YYTOKNUM[YYLEX-NUM] -- Internal token number corresponding to
   token YYLEX-NUM.  */
static const unsigned short yytoknum[] =
{
       0,   256,   257,    59,   258,   259,   260,   261,   262,   263,
     264,   265,   266,   267,   268,   269,   270,   271,   272,   273,
     274,   275,   276,   277,   278,   279,   280,   281,   282,   283,
     284,   285,   286,   287,   288,   289,   290,   291,   292,   293,
     294,   295,   296,   297,   298,   299,   300,   301,   302,   303,
     304,   305,   306,   307,   308,   309,   310,   311,   312,   313,
     314,   315,   316,   317,   318,   319,   320,   321,   322,   323,
     324,   325,   326,   327,   328,   329,   330,   331,   332,   333,
     334,   335,   336,   337,   338,   339,   340,   341,   342,   343,
     344,   345,   346,   347,   348,   349,   350,   351,   352,   353,
     354,   355,   356,   357,   358,   359,   360,   361,   362,   363,
     364,   365,   366,   367,   368,   369,   370,   371,   372,   373,
     374,    58,    44,    40,    41,    61,    33,    46,    45,    42,
      47,    43,    62,    60
};
# endif

/* YYR1[YYN] -- Symbol number of symbol that rule YYN derives.  */
static const unsigned short yyr1[] =
{
       0,   134,   136,   135,   137,   135,   138,   135,   139,   135,
     135,   140,   140,   141,   141,   142,   142,   143,   143,   144,
     145,   145,   146,   146,   146,   147,   147,   147,   148,   148,
     149,   150,   150,   151,   151,   151,   151,   151,   151,   151,
     152,   152,   153,   153,   154,   154,   155,   155,   155,   155,
     156,   156,   157,   157,   157,   158,   158,   159,   159,   160,
     160,   161,   162,   163,   163,   164,   164,   165,   165,   166,
     166,   166,   166,   166,   166,   166,   166,   166,   166,   166,
     166,   166,   166,   166,   166,   166,   166,   166,   166,   166,
     166,   166,   166,   166,   167,   167,   168,   169,   169,   170,
     170,   170,   170,   170,   170,   171,   171,   172,   172,   173,
     173,   173,   173,   174,   174,   175,   175,   176,   177,   177,
     178,   178,   179,   179,   180,   180,   181,   181,   182,   182,
     183,   183,   184,   185,   186,   185,   187,   185,   185,   188,
     189,   189,   190,   191,   191,   191,   191,   191,   191,   191,
     191,   191,   192,   192,   193,   193,   194,   194,   194,   194,
     194,   194,   194,   194,   194,   194,   194,   194,   194,   194,
     194,   194,   194,   195,   195,   195,   195,   196,   196,   196,
     197,   197,   198,   198,   198,   198,   198,   199,   199,   199,
     199,   200,   200,   200,   200,   200,   200,   200,   200,   201,
     201,   201,   202,   202,   202,   202,   203,   203,   203,   204,
     204,   204,   204,   204,   204,   204,   204,   204,   204,   205,
     205,   206,   206,   206,   206,   207,   208,   209,   209,   210,
     210,   211,   211,   211,   212,   213,   214,   214,   215,   215,
     216,   216,   216,   216,   217,   217,   218,   218,   218,   218,
     218,   218,   218,   218,   218,   219,   220,   219,   221,   219,
     222,   219,   223,   219,   219,   219,   224,   225,   219,   219,
     219,   219,   219,   226,   226,   226,   226,   226,   226,   226,
     227,   227,   227,   228,   228,   229,   229,   230,   230,   231,
     231,   233,   232,   232,   234,   234,   235,   235,   236,   236,
     237,   237,   237,   238,   238,   238,   238,   238,   238,   239,
     239,   240,   240,   241,   242,   242,   243,   243,   244,   244,
     245,   245,   245,   246,   246,   246,   246,   246,   247,   248,
     246,   249,   249,   249,   250,   250,   251,   251,   251,   252,
     252,   253,   253,   253,   254,   254,   255,   256,   256,   257,
     258,   258,   259,   259,   259,   260,   260,   261,   261,   262,
     262,   262,   263,   263,   263,   263,   264,   264,   266,   267,
     265,   269,   268,   270,   268,   271,   271,   272,   273,   273,
     273,   273,   273,   273,   273,   273,   273,   273,   273,   273,
     273,   273,   273,   273,   273,   273,   273,   273,   273,   273,
     273,   273,   273,   273,   273,   273,   273,   274,   274,   274,
     274,   274,   274,   275,   275
};

/* YYR2[YYN] -- Number of symbols composing right hand side of rule YYN.  */
static const unsigned char yyr2[] =
{
       0,     2,     0,     3,     0,     5,     0,     3,     0,     5,
       0,     6,     8,     2,     0,     1,     3,     1,     2,     3,
       1,     3,     4,     7,    10,     2,     2,     0,     6,     6,
       4,     1,     3,     4,     5,     6,     5,     6,     7,     1,
       1,     3,     1,     3,     4,     6,     1,     5,     3,     7,
       1,     3,     2,     2,     0,     1,     0,     1,     0,     3,
       5,     4,     3,     0,     1,     1,     3,     2,     1,     7,
       8,     2,     7,     3,     1,     2,     2,     5,     3,     1,
       1,     2,     2,     4,     5,     5,     8,     2,     2,     5,
       8,     5,     7,     1,     1,     0,     2,     1,     2,     4,
       5,     6,     6,     7,     0,     6,     7,     2,     4,     2,
       2,     2,     1,     1,     0,     1,     3,     7,     1,     2,
       2,     0,     2,     0,     2,     0,     1,     3,     1,     3,
       1,     3,     6,     2,     0,     6,     0,     6,     0,     4,
       1,     3,     3,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     0,     4,     0,     1,     1,     4,     8,     6,
       6,     3,     7,     8,     8,     3,     7,     8,     8,     4,
       8,     9,     9,     1,     1,     3,     4,     2,     2,     1,
       3,     1,     3,     3,     3,     3,     1,     3,     3,     3,
       1,     3,     3,     3,     3,     3,     3,     3,     1,     3,
       3,     1,     3,     3,     3,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       0,     1,     1,     2,     2,     3,     3,     2,     2,     2,
       0,     3,     4,     2,     2,     2,     2,     3,     1,     3,
       3,     3,     3,     1,     2,     5,     2,     4,     6,     8,
       5,     7,     6,     8,     0,     2,     0,     6,     0,     6,
       0,     3,     0,     5,     6,     8,     0,     0,     7,     7,
       6,     8,     0,     3,     3,     3,     3,     3,     3,     3,
       4,     3,     4,     1,     3,     1,     3,     7,     8,     1,
       3,     0,     3,     0,     4,     0,     1,     0,     1,     0,
       2,     2,     0,     5,     7,     6,     8,     3,     4,     5,
       7,     1,     2,     5,     3,     3,     1,     3,     1,     3,
       1,     1,     3,     3,     6,     6,     8,     6,     0,     0,
       8,     1,     2,     3,     1,     3,     6,     1,     3,     1,
       3,     1,     2,     0,     1,     3,     2,     1,     3,     3,
       8,     7,     3,     4,     0,     1,     3,     1,     0,     3,
       1,     1,     3,     5,     7,     7,     4,     6,     0,     0,
       5,     0,     6,     0,     5,     1,     3,     3,     1,     3,
       2,     4,     5,     6,     7,     4,     5,     5,     2,     5,
       1,     4,     6,     5,     1,     2,     1,     6,     2,     2,
       2,     4,     1,     4,     5,     5,     6,     1,     1,     1,
       1,     1,     0,     1,     0
};

/* YYDEFACT[STATE-NAME] -- Default rule to reduce with in state
   STATE-NUM when YYTABLE doesn't specify something else to do.  Zero
   means the default is an error.  */
static const unsigned short yydefact[] =
{
      10,     2,     1,     0,     0,     0,     4,   272,   272,     3,
       0,     7,     0,     0,   260,   260,   138,     5,     9,    58,
       0,   358,     0,   358,     0,     0,     0,   156,   154,   289,
       0,   255,     0,    58,     0,   302,   358,    57,    20,     0,
       0,   285,   361,   360,   358,     0,    97,   115,   230,   344,
     343,   357,     0,     0,   347,   118,     0,   230,    46,   130,
       0,     0,   283,     0,     0,   262,    63,   155,     0,   154,
       0,    68,     0,     0,     0,   272,     0,     0,     0,     0,
       0,   254,     0,     0,     0,     0,     0,    95,     0,     0,
      93,    74,    79,   261,    58,    80,   216,   213,   217,   214,
     218,   266,   215,     0,   210,   212,   211,   209,     0,   358,
     358,     0,   133,     0,     0,   378,     0,     0,     0,     0,
       0,   390,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   394,     0,   396,   402,   295,   273,     0,   358,     0,
     274,     0,   358,   358,    98,   341,   346,     0,   275,     0,
     276,   119,     0,     0,   277,     0,     0,     0,   278,     0,
     279,   256,   258,   156,     0,     0,     0,     0,     0,   150,
     144,   145,   143,   149,   146,   147,   148,   156,     0,     0,
       0,     0,    64,   173,   174,   179,   181,   186,   190,   198,
     201,   205,   208,    65,   206,   151,   207,   161,     0,   165,
       0,    71,     0,   260,     0,    31,    75,    76,     0,    27,
     125,     0,     0,   328,     0,    81,    82,     0,    54,    87,
      88,    94,   121,     0,   358,    67,     0,   290,     0,     0,
       0,     0,    59,   254,     0,   300,   301,    28,     0,   380,
       0,     0,     0,     0,   388,     0,     0,   395,     0,   398,
     399,   400,     0,     0,     0,     0,    21,     0,   295,     0,
     297,   286,   359,   358,   116,   229,   345,   342,   349,   348,
      48,     0,   131,   156,   281,     0,   284,     0,     0,    63,
     263,     0,     0,     0,     0,     0,   177,    63,   236,     0,
       0,   243,     0,   238,     0,     0,     0,   178,     0,   157,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    63,   169,    63,   254,     0,   254,    73,    14,
       0,   254,     0,     0,     0,     0,     0,    27,    27,     0,
      78,     0,     0,   339,     0,     0,     0,   323,   156,   246,
       0,   254,     0,     0,     0,    54,     0,    54,     0,     0,
     123,    63,     0,   267,    58,    29,   132,   134,   136,     0,
       0,   355,   156,     0,     0,   337,   379,     0,   334,     0,
       0,     0,     0,     0,     0,   128,     0,   126,     0,     0,
     140,     0,    39,     0,   104,     0,   412,     0,   297,     0,
     296,   299,     0,     0,    50,    63,   282,   280,     0,   257,
       0,   259,     0,    58,     0,     0,     0,     0,   225,     0,
     320,     0,   208,   321,     0,   316,   244,     0,   237,     0,
       0,     0,     0,   175,    66,    63,   264,   180,   184,   185,
     182,   183,   189,   188,   187,   191,   195,   193,   197,   196,
     192,   194,   199,   200,   202,   204,   203,     0,     0,     0,
      65,    63,     0,    65,     0,   254,     0,     0,     0,    15,
      32,     0,     0,     0,     0,     0,    22,     0,    25,    26,
     124,   254,     0,     0,     0,   254,     0,     0,   254,     0,
       0,    63,     0,     0,   234,   254,     0,    83,     0,     0,
       0,    52,   254,     0,    53,   120,     0,     0,   125,     0,
      65,    62,    58,   270,     0,     0,    60,     0,     0,    63,
       0,   381,     0,    56,   385,    56,     0,    56,   391,     0,
       0,     0,   401,     0,     0,   220,     0,   139,   403,     0,
     354,     0,   104,   411,   407,   408,   409,   410,   107,     0,
     299,     0,    56,   298,   293,     0,     0,    47,   104,     0,
       0,   375,   373,     0,   157,   269,    58,     0,     0,   311,
     235,     0,     0,   226,   157,   314,     0,     0,   315,   242,
     239,   240,     0,   241,   176,     0,   160,   159,   152,     0,
       0,     0,    65,   152,     0,     0,   254,     0,   254,     0,
       0,    17,   318,     0,    13,     0,    77,    27,     0,   254,
       0,     0,   307,     0,     0,     0,     0,   340,     0,   254,
     156,   247,     0,     0,    58,     0,   233,     0,    84,     0,
       0,     0,    96,    85,   121,   122,    89,    91,   268,   135,
     137,   356,    61,    65,   338,   382,   335,    55,     0,   386,
     387,   389,     0,   129,   127,     0,     0,   331,     0,    42,
       0,   222,   221,     0,     0,   142,   219,   141,   404,   405,
       0,     0,   114,   412,   354,   294,   293,     0,   393,   291,
      58,   358,    51,     0,   354,   412,     0,   104,   414,   371,
     265,   271,     0,     0,   309,   312,   228,   227,   322,   317,
       0,   152,   162,     0,     0,   152,     0,     0,   166,     0,
       0,     0,   254,     0,   254,    11,    18,   254,     0,    16,
       0,     0,    30,   324,   308,     0,     0,     0,   325,   254,
     327,   329,    63,     0,     0,   157,   250,    58,   231,     0,
       0,    44,   123,     0,     0,   383,   220,   397,   332,     0,
      33,     0,     0,     0,     0,   223,     0,   224,   406,     0,
       0,   113,   105,   108,   114,    58,   392,     0,   287,   117,
      49,   114,   377,   376,   354,   413,   368,   414,     0,     0,
     245,   158,    63,   164,   163,   170,     0,     0,   168,   167,
      69,     0,    72,     0,    19,   319,     0,    23,     0,     0,
     303,     0,     0,     0,    58,     0,   248,   252,   232,     0,
       0,   125,    92,     0,   384,    34,     0,    40,    43,   333,
       0,    36,     0,     0,     0,     0,    99,   352,     0,   106,
     288,   292,   114,   114,   114,   414,   112,   114,   374,   272,
     368,     0,   310,     0,   172,   171,    70,    12,    27,     0,
       0,   305,   326,   330,   157,   251,    58,    86,    45,    90,
     159,    35,     0,    37,     0,     0,   362,     0,     0,   100,
       0,   353,   109,   111,   110,   368,   414,   260,   372,   313,
     153,     0,     0,   304,   249,   253,    41,    38,     0,     0,
     366,   101,     0,   102,   351,   368,     0,    24,   306,     0,
       0,   363,     0,   103,   350,    58,     0,     0,   367,   370,
     365,   364
};

/* YYDEFGOTO[NTERM-NUM]. */
static const short yydefgoto[] =
{
      -1,     1,     4,    12,     5,    13,    91,   467,   468,   600,
     601,   389,    92,   336,     9,   337,   283,   390,   816,   656,
     355,    59,   403,   356,   648,   224,    37,   235,   225,   419,
     182,   349,    94,   220,   503,    45,   540,   133,   395,   835,
     836,    46,    47,    56,   360,   508,   340,   386,   387,    60,
      11,    34,   514,   515,   134,   391,   657,   183,   702,    68,
     184,   185,   186,   187,   188,   189,   190,   191,   192,   205,
     108,   665,   666,   194,   285,   573,   144,   498,   352,   417,
     195,   292,   293,   294,   494,    14,   277,   278,    30,   164,
     226,   512,    31,    62,    63,    40,    41,   350,   680,   767,
     260,   401,   554,   113,   343,   196,   568,   569,   602,   424,
     658,   425,    95,   346,   802,   659,   377,   374,   344,   146,
      48,    49,    53,    54,   409,   672,   370,    50,    51,   755,
     757,   838,   839,   896,   411,   777,   688,   560,   561,   375,
     548,   776
};

/* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
   STATE-NUM.  */
#define YYPACT_NINF -774
static const short yypact[] =
{
    -774,    65,  -774,   -85,   362,    73,   140,  -774,  -774,  -774,
     134,  -774,   362,    73,   481,   709,  -774,  -774,  -774,   110,
     133,   102,   133,   102,   196,   133,   133,   156,   184,   -15,
    2645,  -774,   367,   110,  1276,  -774,    31,  -774,  -774,  2306,
      51,  -774,  -774,   192,    31,   354,   245,  -774,    -6,  -774,
     308,  -774,   219,    55,  -774,   245,   388,    -6,   132,  -774,
      56,  1240,  -774,    76,    81,   243,  2470,  -774,   262,   184,
     284,  -774,  2470,  2470,   598,  -774,  2470,    31,  2470,  2470,
    2499,     9,    31,    31,  2470,   360,  2470,  2470,    31,    31,
    -774,  -774,  -774,  -774,   110,  -774,  -774,  -774,  -774,  -774,
    -774,  -774,  -774,   142,  -774,  -774,  -774,  -774,   289,    31,
      31,   306,  -774,   932,   402,   985,   286,   312,   319,  2273,
     325,   328,  2357,   346,  2357,  2357,  2357,   361,   365,   368,
     380,  -774,   366,  -774,  -774,    46,  -774,   133,    31,   398,
    -774,   417,    31,    31,  -774,   471,  -774,  2357,  -774,   133,
    -774,  -774,    31,   420,  -774,   196,  2524,   200,  -774,   133,
    -774,  -774,  -774,   426,  2645,   609,  2470,  2470,   942,  -774,
    -774,  -774,  -774,  -774,  -774,  -774,  -774,   430,  2382,  1109,
     942,   257,  -774,  -774,   -15,  -774,  -774,   414,   154,   205,
      42,   401,    24,   -17,  -774,  -774,  -774,   438,   446,   453,
     108,  -774,   595,   801,   539,   488,  -774,  -774,   608,   840,
     541,   169,  2470,  -774,  1266,  -774,  -774,   519,   307,  -774,
    -774,  -774,     6,   494,    31,  -774,  2470,   -15,  2470,   616,
     633,   435,  -774,  -774,    31,  -774,  -774,  -774,  1172,    41,
    1172,  2470,  2470,  2470,   514,  2470,  2470,   514,  2470,   514,
     514,   514,   527,     3,  2470,   611,  -774,   517,   607,  2470,
     634,  -774,  -774,    31,  -774,  -774,  -774,  -774,   514,  -774,
    -774,  2357,  -774,  2553,  -774,  2470,  -774,   520,   521,  2470,
    -774,  2470,   533,   578,   552,   597,  -774,  2470,  -774,  2353,
     540,   531,   -16,  -774,   399,   536,   188,  -774,  2470,    28,
    1109,  1109,  1109,  1109,  1109,  1109,  1109,  1109,  1109,  1109,
    1109,  1109,  1109,  1109,  1109,  1109,  1109,  1109,  1109,  1109,
    2470,  2470,  2470,   535,  2470,  -774,   649,  -774,  -774,    31,
    2470,  -774,  2470,  2470,  2470,  2470,   610,  1036,  1036,  2470,
    -774,   657,   116,  -774,    43,   660,  2470,  -774,   206,  -774,
     631,  -774,   290,   553,   554,  1088,   619,  1088,  2470,  2470,
     570,  2470,   669,  -774,   110,  -774,  -774,  -774,  -774,  1347,
     270,  -774,   322,   557,   549,   514,  -774,   280,  -774,   556,
     558,   559,   560,   561,   567,   565,   288,  -774,   174,  2306,
    -774,   291,  -774,   568,   626,   297,    69,    31,   634,   244,
    -774,   635,   576,   301,   514,  2470,  -774,  -774,   -28,  -774,
     141,  -774,   341,   110,  2470,   590,  2470,   482,  -774,   344,
    -774,   591,   188,   602,   363,  -774,  -774,  2470,  -774,  2407,
    2470,   564,  2470,   598,  -774,  2470,  -774,  -774,   414,   414,
     414,   414,   154,   154,   154,   205,   205,   205,   205,   205,
     205,   205,    42,    42,   401,   401,   401,   594,   601,   370,
     128,  2470,   377,   147,  1409,  -774,  1472,   596,    82,  -774,
    -774,  1535,   723,   661,   637,   606,  -774,  2470,  -774,  -774,
    -774,  -774,  2470,   737,  2470,  -774,  2470,   142,  -774,   741,
    2160,  2470,  2470,   639,  2222,  -774,  2470,  -774,   692,   743,
      87,  -774,  -774,   700,  -774,  -774,   630,  2470,   541,   382,
    -774,  -774,   110,  -774,   520,   521,  -774,    31,   636,  2470,
    2470,  2357,  1172,   658,  -774,   658,  2357,   658,  -774,  2470,
    2470,   527,  -774,   239,     1,   182,     3,  -774,  2342,   640,
     713,  2357,   626,  -774,  -774,  -774,  -774,  -774,  -774,   641,
     635,  2470,   658,  -774,   656,   644,  2357,   675,   626,  2306,
     385,  -774,  -774,   393,    28,  -774,   110,   596,   186,  -774,
    -774,  2470,  2470,  -774,   652,  -774,  2470,  2436,  -774,  -774,
    -774,  -774,   665,  -774,  -774,   403,  -774,  -774,  -774,  2470,
    2470,   404,   157,  -774,  2470,  2470,  -774,  1597,  -774,  2353,
     190,  -774,  -774,   443,  -774,    31,  -774,   984,   103,  2470,
     662,  1660,  -774,  2470,   199,  1722,   775,  -774,  1784,  -774,
     659,  -774,   686,   411,   110,  2470,  2222,   519,  -774,   667,
    2470,   663,  2222,  -774,   679,  -774,  -774,   764,  -774,  -774,
    -774,  -774,  -774,   187,  -774,    83,  -774,  -774,  2357,  -774,
     514,  -774,   664,  -774,  -774,   133,   173,  -774,   473,  -774,
     239,  -774,  -774,   668,   670,  -774,  -774,  -774,  -774,    89,
    2357,   672,   718,    69,   713,  -774,   656,   676,  -774,  -774,
     110,    31,   514,    31,   713,    69,   133,   626,   796,  -774,
    -774,  -774,   484,  2470,  -774,  -774,  -774,  -774,  -774,  -774,
     682,  -774,   678,   685,   687,  -774,  2470,  2470,   678,   688,
     695,  1847,  -774,  1910,  -774,  -774,  -774,  -774,   596,  -774,
    2470,   757,  -774,  -774,  -774,  2470,   712,  2470,  -774,  -774,
    -774,  2222,  2470,  2470,   710,    28,  -774,   110,   555,   619,
     697,  -774,   570,   715,  2470,  -774,   182,  -774,  -774,    15,
    -774,   239,   133,   194,  2470,  -774,   750,  -774,  -774,    70,
      13,  -774,  -774,  -774,   718,   110,  -774,  2470,  -774,  -774,
    -774,   238,  -774,  -774,   713,  -774,  -774,   796,  2470,   780,
    -774,   678,  2470,  -774,  -774,   678,   708,   714,  -774,  -774,
    -774,  1973,  -774,  2036,  2222,  -774,   831,  -774,    35,  2470,
    -774,  2098,   769,   424,   110,  2470,  -774,  -774,  -774,   788,
     719,   541,  -774,   721,  -774,  -774,    23,  -774,  -774,  -774,
      17,  -774,   434,   668,    29,   838,  -774,  -774,   447,  -774,
    -774,  -774,   718,   718,   718,   796,  -774,   238,  -774,  -774,
    -774,   836,  -774,   448,  -774,  -774,  -774,  -774,  1036,   749,
    2470,  -774,  -774,  -774,    28,  -774,   110,  -774,  -774,  -774,
     455,  -774,   133,  -774,    37,  2470,  -774,   458,   725,  -774,
      36,  -774,  -774,  -774,  -774,  -774,   796,  1115,  -774,  -774,
    -774,   803,  2470,  -774,  -774,  -774,  -774,  -774,   254,  2470,
    -774,  -774,   726,  -774,  -774,  -774,   826,  -774,  -774,  2470,
    2470,  -774,   730,  -774,  -774,   110,   731,   732,  -774,  -774,
    -774,  -774
};

/* YYPGOTO[NTERM-NUM].  */
static const short yypgoto[] =
{
    -774,  -774,  -774,  -774,  -774,  -774,  -774,  -774,  -774,  -774,
     258,    12,  -774,  -325,   845,  -774,   -37,   324,    47,   202,
     237,   716,  -774,   241,  -277,   -10,  -774,  -774,  -294,   -55,
    -774,   -23,  -774,  -774,   130,  -774,  -515,  -774,  -774,    33,
    -624,   851,   735,  -774,   246,   135,  -490,  -774,   352,  -774,
     874,  -774,  -774,  -774,  -774,  -774,  -247,   -50,  -558,   820,
     -14,   -80,   593,   152,   153,   277,   293,   164,  -154,   -58,
    -148,   144,  -774,  -774,   326,  -774,   834,   158,   273,  -774,
    -774,  -774,   472,  -774,    57,    -4,  -774,  -774,  -774,  -774,
    -774,  -774,   868,   744,  -774,  -774,   767,   -12,   229,  -774,
     650,   509,   359,  -774,   423,  -774,  -774,   343,  -168,  -774,
    -437,   336,  -774,  -774,  -774,   163,  -774,  -221,  -774,  -774,
     892,  -127,   890,   768,   406,  -569,   161,     8,   -27,   101,
    -774,  -773,  -774,  -774,   410,  -774,  -774,   518,   247,   292,
    -381,  -728
};

/* YYTABLE[YYPACT[STATE-NUM]].  What to do in state STATE-NUM.  If
   positive, shift that token.  If negative, reduce the rule which
   number is the opposite.  If zero, do what YYDEFACT says.
   If YYTABLE_NINF, syntax error.  */
#define YYTABLE_NINF -395
static const short yytable[] =
{
      29,    29,    32,    32,    15,   436,   392,    93,   193,    36,
     295,   181,   478,   479,   200,   201,   266,   282,   636,   378,
     207,   208,   210,   109,   202,   296,   217,   674,   219,   221,
     603,   -58,    39,   868,    52,   708,     6,    61,    52,   204,
     892,   388,    42,   684,   114,   376,   485,   308,   762,   840,
     206,    29,   139,   165,   136,   215,   216,   -58,   148,   154,
      42,   222,   223,   211,   815,     2,   863,   878,   142,   660,
     513,   203,   861,   543,   544,   545,   546,   547,   824,   158,
     257,   849,   320,    38,   160,   604,   887,   745,   286,   227,
     236,    -6,   630,   758,   317,   318,   558,    35,   274,    10,
     297,   428,   894,    69,   321,   764,   429,   875,   358,   284,
     212,   213,    70,   325,    38,   771,   143,   229,   230,   565,
     291,   482,   904,   132,    43,   270,    38,   319,    38,   359,
     692,    42,    42,   850,   825,   422,   258,   827,   214,   -58,
     829,   280,    43,   781,   486,   862,   262,   785,   895,    39,
     265,   435,   326,   869,   345,   309,   310,   311,    16,   862,
     893,    52,   483,   603,   259,   487,    -8,   312,   363,   259,
     364,    61,   774,   137,   313,   314,     3,   149,   155,    35,
     373,   422,   373,   379,   380,   381,   338,   382,   383,    29,
     384,    32,   259,   259,   826,   357,   393,    29,   159,   342,
      29,   399,   493,   149,   605,   837,   259,   371,   872,   873,
     874,   631,   259,    43,    43,   406,   301,   407,   638,   341,
     749,   193,   750,   413,   412,    44,   567,   589,   484,   193,
     152,   423,   362,   693,   302,   694,   725,   714,   103,   715,
     434,   820,   533,   821,    38,   726,   594,   661,   649,   590,
     651,   422,    38,   177,   662,   663,   706,   153,   317,   318,
      28,   295,   457,   458,   460,   562,   463,   459,   595,   462,
     690,   402,   691,   664,   472,   678,   474,    65,   707,    66,
     177,   480,   721,   303,   304,    38,   320,    28,   489,   392,
     369,   319,   763,   470,   832,   751,   473,   727,   475,   833,
     505,   646,   469,   510,   772,   259,   509,    58,   744,   567,
      71,    67,   433,   599,   338,   338,   751,   761,   138,   161,
     162,   859,   506,   259,   305,   275,   834,   490,   353,   491,
     736,   135,   357,   306,   357,    72,   307,   495,   496,    73,
      74,   132,    75,   497,   147,    76,    77,   193,    78,    79,
      38,    80,    81,   157,   163,    29,   566,   140,   570,    82,
     655,    28,   599,   145,    83,   551,    84,   141,   552,   579,
     549,   291,   581,   197,   583,   899,   900,   510,   901,   298,
     585,   299,   464,   584,   466,    85,     7,     8,   471,    86,
      87,   150,   517,    88,   518,   199,    89,    96,   218,    90,
     534,   228,   522,   592,   523,   237,   591,   239,   748,   240,
     531,   244,   532,   536,   247,   537,   249,   250,   251,   541,
     559,   542,   559,   556,   612,   557,   614,   231,   616,    97,
     354,   315,   316,   193,   624,   241,   623,    98,   627,   268,
     475,   806,   242,   807,  -394,   519,  -394,    99,   245,   635,
      29,   246,    29,   438,   439,   440,   441,    29,   442,   443,
     444,   643,   644,   298,   373,   564,   298,   621,   574,   248,
     100,   652,   653,    29,   734,   342,    29,   256,   622,   101,
      29,   454,   455,   456,   252,   577,   102,   578,   253,   103,
     641,   254,   298,   677,   588,   104,   105,   106,   107,   298,
     267,   593,   817,   255,   298,   819,   637,   686,    19,   687,
     855,   367,   368,   696,   284,   686,   263,   689,   698,   423,
     430,   431,   597,   881,    20,   298,   298,   701,   705,   571,
     572,   703,   704,   298,   300,   735,   709,   710,   611,    21,
      44,   423,   615,   271,    22,   618,   298,   396,   854,   279,
     795,   570,   626,   287,   769,   724,   865,   323,   866,   632,
     884,   322,   885,   404,   717,   718,    23,   737,    24,   517,
     298,   871,   880,   817,    25,    26,   324,  -336,   719,  -336,
     889,   469,   890,    29,   338,   445,   446,   447,   448,   449,
     450,   451,    27,   740,   752,   718,   501,    29,   504,    28,
     327,    29,   495,   496,    29,   778,   718,   329,   452,   453,
     330,   909,    29,   331,   351,   886,   339,   361,    29,   365,
     115,   116,   117,   118,   119,   120,   121,   122,   123,   124,
     125,   126,   127,   128,   129,   779,   366,   259,   385,    96,
     397,   257,   400,   408,   410,   414,   415,   416,   786,   787,
     418,   426,   427,   711,   465,   713,   770,   432,   461,   476,
     481,    96,   796,   488,   499,   500,   502,   798,   507,   800,
     768,    97,   511,   521,   193,   804,   731,   803,   520,    98,
     524,   535,   525,   526,   527,   528,   813,   130,   529,    99,
     530,   582,   538,    97,   169,   553,   822,    29,   559,    29,
     555,    98,   170,   171,   172,   173,   174,   175,   176,   831,
     539,    99,   100,   567,   178,   575,    96,    29,   586,   599,
     841,   281,   131,   576,   510,   587,   607,   843,   102,   608,
     610,   103,   609,   371,   100,   394,    33,   104,   105,   106,
     107,   851,   613,   492,   619,   628,   629,   856,    97,   633,
     102,   625,    20,   103,   634,   830,    98,   642,   647,   104,
     105,   106,   107,   670,   671,   675,    99,    21,   679,   791,
     681,   793,    22,   683,   794,   435,   700,    29,   729,    29,
      29,   358,   732,   722,   741,   743,   801,    29,   747,   100,
     354,   754,   883,   756,    23,   760,    24,   761,   733,   775,
     766,   782,    25,    26,   780,   102,   797,   888,   103,   783,
     799,   784,   788,   645,   104,   105,   106,   107,   650,   789,
      27,   810,   805,   823,   898,   338,   812,    28,   328,   842,
     669,   902,   844,   673,   848,   877,   853,   857,   845,   879,
     858,   906,   907,    71,    20,   860,   870,   882,   682,   891,
     903,   685,   897,   905,   908,   910,   911,    17,   716,    21,
     667,   332,   753,    29,    22,    32,   739,   864,    72,   809,
     876,   272,    73,    74,    55,    75,   264,   811,   333,    77,
     742,    78,    79,   654,    80,    81,    23,    18,    24,   198,
     814,   151,    82,   437,    25,    26,   808,    83,   697,   334,
     738,   580,   112,   276,   261,   765,   168,   550,   398,   676,
     617,   695,    27,   699,   818,    57,    64,   269,    85,    28,
     639,   828,    86,    87,   867,   640,    88,     0,   563,    89,
       0,     0,    90,   773,     0,    71,   169,     0,     0,     0,
     746,     0,     0,     0,   170,   171,   172,   173,   174,   175,
     176,   177,     0,     0,     0,     0,   178,     0,    28,   232,
      72,     0,   759,   335,    73,    74,     0,    75,   180,     0,
      76,    77,     0,    78,    79,     0,    80,    81,     0,   233,
       0,     0,     0,     0,    82,     0,     0,    71,     0,    83,
       0,    84,     0,     0,   115,   116,   117,   118,   119,   120,
     121,   122,   123,   124,   125,   126,   127,   128,   129,     0,
      85,     0,    72,     0,    86,    87,    73,    74,    88,    75,
       0,    89,    76,    77,    90,    78,    79,     0,    80,    81,
       0,     0,     0,     0,     0,     0,    82,     0,   169,    71,
       0,    83,     0,    84,     0,     0,   170,   171,   172,   173,
     174,   175,   176,   177,     0,   234,     0,     0,   178,   720,
      28,   130,    85,     0,    72,   179,    86,    87,    73,    74,
      88,    75,     0,    89,    76,    77,    90,    78,    79,     0,
      80,    81,     0,     0,     0,     0,     0,     0,    82,     0,
       0,    71,     0,    83,     0,    84,   131,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   477,   238,     0,
       0,     0,     0,     0,    85,     0,    72,     0,    86,    87,
      73,    74,    88,    75,     0,    89,    76,    77,    90,    78,
      79,     0,    80,    81,     0,     0,     0,     0,     0,     0,
      82,     0,  -369,     0,     0,    83,     0,    84,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    20,   477,
       0,     0,     0,     0,     0,     0,    85,     0,     0,     0,
      86,    87,     0,    21,    88,   168,     0,    89,    22,     0,
      90,   115,   116,   117,   118,   119,   120,   121,   122,   123,
     124,   125,   126,   127,   128,   129,     0,     0,     0,     0,
      23,     0,    24,     0,     0,   169,     0,     0,    25,    26,
     166,   354,     0,   170,   171,   172,   173,   174,   175,   176,
     177,     0,     0,     0,     0,   178,    27,    28,     0,     0,
       0,   167,   179,    28,     0,     0,     0,   180,   168,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   130,   115,
     116,   117,   118,   119,   120,   121,   122,   123,   124,   125,
     126,   127,   128,   129,     0,     0,     0,     0,   169,    71,
       0,     0,     0,     0,     0,     0,   170,   171,   172,   173,
     174,   175,   176,   372,     0,     0,     0,     0,   178,     0,
      28,     0,     0,     0,    72,   179,     0,     0,    73,    74,
     180,    75,     0,   110,    76,    77,     0,    78,    79,     0,
      80,    81,     0,     0,     0,     0,   130,     0,    82,    20,
       0,     0,     0,    83,     0,    84,     0,     0,     0,     0,
       0,     0,     0,   347,    21,     0,     0,     0,     0,    22,
       0,     0,     0,     0,    85,     0,     0,     0,    86,    87,
      71,   131,    88,     0,     0,    89,     0,     0,    90,     0,
       0,    23,   132,    24,     0,   156,     0,     0,     0,    25,
      26,     0,     0,     0,   516,    72,     0,   348,     0,    73,
      74,     0,    75,     0,    28,    76,    77,   111,    78,    79,
       0,    80,    81,     0,     0,     0,     0,     0,     0,    82,
       0,     0,     0,     0,    83,     0,    84,     0,     0,     0,
       0,     0,    71,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    85,     0,     0,     0,    86,
      87,     0,     0,    88,     0,     0,    89,    72,     0,    90,
       0,    73,    74,     0,    75,     0,     0,    76,    77,     0,
      78,    79,     0,    80,    81,     0,     0,     0,   348,     0,
       0,    82,     0,     0,     0,    28,    83,     0,    84,     0,
       0,     0,     0,     0,     0,    71,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    85,     0,     0,
       0,    86,    87,     0,     0,    88,     0,     0,    89,     0,
      72,    90,     0,     0,    73,    74,   596,    75,     0,     0,
      76,    77,     0,    78,    79,     0,    80,    81,     0,     0,
     348,     0,     0,     0,    82,     0,     0,    28,     0,    83,
       0,    84,     0,     0,     0,     0,     0,     0,    71,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      85,     0,     0,     0,    86,    87,     0,     0,    88,     0,
       0,    89,   606,    72,    90,     0,     0,    73,    74,   598,
      75,     0,     0,    76,    77,     0,    78,    79,     0,    80,
      81,     0,     0,   348,     0,     0,     0,    82,     0,     0,
      28,     0,    83,     0,    84,     0,     0,     0,     0,     0,
      71,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    85,     0,     0,     0,    86,    87,     0,
       0,    88,     0,     0,    89,    72,     0,    90,     0,    73,
      74,     0,    75,     0,     0,    76,    77,     0,    78,    79,
       0,    80,    81,     0,     0,     0,   348,     0,     0,    82,
       0,     0,     0,    28,    83,     0,    84,     0,     0,     0,
       0,     0,     0,    71,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    85,     0,     0,     0,    86,
      87,     0,     0,    88,     0,     0,    89,     0,    72,    90,
       0,     0,    73,    74,   712,    75,     0,     0,    76,    77,
       0,    78,    79,     0,    80,    81,     0,     0,   348,     0,
       0,     0,    82,     0,     0,    28,     0,    83,     0,    84,
       0,     0,     0,     0,     0,    71,     0,   723,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    85,     0,
       0,     0,    86,    87,     0,     0,    88,     0,     0,    89,
      72,     0,    90,     0,    73,    74,     0,    75,     0,     0,
      76,    77,     0,    78,    79,     0,    80,    81,     0,     0,
       0,   348,     0,     0,    82,     0,     0,     0,    28,    83,
       0,    84,     0,     0,     0,     0,     0,    71,     0,   728,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      85,     0,     0,     0,    86,    87,     0,     0,    88,     0,
       0,    89,    72,     0,    90,     0,    73,    74,     0,    75,
       0,     0,    76,    77,     0,    78,    79,     0,    80,    81,
       0,     0,     0,   348,     0,     0,    82,     0,     0,     0,
      28,    83,     0,    84,     0,     0,     0,     0,     0,     0,
      71,   730,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    85,     0,     0,     0,    86,    87,     0,     0,
      88,     0,     0,    89,   790,    72,    90,     0,     0,    73,
      74,     0,    75,     0,     0,    76,    77,     0,    78,    79,
       0,    80,    81,     0,     0,   348,     0,     0,     0,    82,
       0,     0,    28,     0,    83,     0,    84,     0,     0,     0,
       0,     0,     0,    71,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    85,     0,     0,     0,    86,
      87,     0,     0,    88,     0,     0,    89,   792,    72,    90,
       0,     0,    73,    74,     0,    75,     0,     0,    76,    77,
       0,    78,    79,     0,    80,    81,     0,     0,   348,     0,
       0,     0,    82,     0,     0,    28,     0,    83,     0,    84,
       0,     0,     0,     0,     0,     0,    71,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    85,     0,
       0,     0,    86,    87,     0,     0,    88,     0,     0,    89,
     846,    72,    90,     0,     0,    73,    74,     0,    75,     0,
       0,    76,    77,     0,    78,    79,     0,    80,    81,     0,
       0,   348,     0,     0,     0,    82,     0,     0,    28,     0,
      83,     0,    84,     0,     0,     0,     0,     0,     0,    71,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    85,     0,     0,     0,    86,    87,     0,     0,    88,
       0,     0,    89,     0,    72,    90,     0,     0,    73,    74,
       0,    75,     0,     0,    76,    77,     0,    78,    79,     0,
      80,    81,     0,     0,   348,   847,     0,     0,    82,     0,
       0,    28,     0,    83,     0,    84,     0,     0,     0,     0,
       0,    71,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    85,     0,     0,     0,    86,    87,
       0,     0,    88,     0,     0,    89,    72,     0,    90,     0,
      73,    74,     0,    75,     0,     0,    76,    77,     0,    78,
      79,     0,    80,    81,     0,     0,     0,   348,     0,     0,
      82,     0,     0,     0,    28,    83,     0,    84,     0,     0,
       0,     0,     0,    71,     0,   852,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    85,     0,     0,     0,
      86,    87,     0,     0,    88,     0,     0,    89,    72,     0,
      90,     0,    73,    74,     0,    75,     0,     0,    76,    77,
       0,    78,    79,     0,    80,    81,     0,     0,     0,   348,
       0,     0,    82,     0,     0,     0,    28,    83,     0,    84,
       0,     0,     0,     0,     0,    71,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    85,     0,
       0,     0,    86,    87,     0,     0,    88,     0,     0,    89,
      72,     0,    90,     0,    73,    74,     0,    75,     0,     0,
      76,    77,     0,    78,    79,     0,    80,    81,     0,     0,
       0,   620,     0,     0,    82,     0,     0,     0,    28,    83,
       0,    84,   115,   116,   117,   118,   119,   120,   121,   122,
     123,   124,   125,   126,   127,   128,   129,     0,     0,     0,
      85,     0,     0,     0,    86,    87,     0,     0,    88,     0,
       0,    89,     0,     0,    90,   115,   116,   117,   118,   119,
     120,   121,   122,   123,   124,   125,   126,   127,   128,   129,
       0,     0,     0,   348,     0,     0,     0,     0,     0,     0,
      28,     0,     0,     0,     0,     0,   668,     0,     0,   130,
       0,   115,   116,   117,   118,   119,   120,   121,   122,   123,
     124,   125,   126,   127,   128,   129,   115,   116,   117,   118,
     119,   120,   121,   122,   123,   124,   125,   126,   127,   128,
     129,     0,   130,     0,   131,     0,     0,     0,     0,     0,
       0,   166,     0,     0,     0,     0,   243,     0,     0,     0,
     420,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   167,     0,     0,     0,     0,   131,   130,   168,
     166,     0,     0,     0,     0,     0,     0,     0,   132,     0,
       0,     0,     0,   130,     0,     0,     0,     0,     0,     0,
       0,   167,     0,     0,     0,   166,     0,     0,   168,   169,
       0,     0,     0,   131,     0,     0,     0,   170,   171,   172,
     173,   174,   175,   176,   177,     0,   167,     0,   131,   178,
       0,    28,     0,   168,   166,     0,   179,     0,   169,     0,
       0,   180,   421,   420,     0,     0,   170,   171,   172,   173,
     174,   175,   176,   177,     0,   167,     0,     0,   178,   288,
      28,     0,   168,   169,     0,   289,     0,     0,   166,   290,
     180,   170,   171,   172,   173,   174,   175,   176,   177,     0,
       0,     0,     0,   178,     0,    28,     0,     0,     0,   167,
     289,     0,   169,     0,   290,   180,   168,   209,     0,     0,
     170,   171,   172,   173,   174,   175,   176,   177,     0,     0,
       0,     0,   178,     0,    28,     0,     0,     0,   167,   179,
       0,     0,   166,     0,   180,   168,   169,     0,     0,     0,
       0,     0,     0,     0,   170,   171,   172,   173,   174,   175,
     176,   177,     0,   167,     0,     0,   178,     0,    28,     0,
     168,   166,     0,   179,     0,   169,     0,     0,   180,     0,
       0,     0,     0,   170,   171,   172,   173,   174,   175,   176,
     177,     0,   167,     0,     0,   178,     0,    28,     0,   168,
     169,     0,   179,     0,     0,     0,     0,   180,   170,   171,
     172,   173,   174,   175,   176,   273,     0,     0,     0,     0,
     178,     0,    28,     0,     0,     0,     0,   179,    71,   169,
       0,     0,   180,     0,     0,     0,     0,   170,   171,   172,
     173,   174,   175,   176,   177,     0,     0,     0,     0,   178,
       0,    28,     0,    72,     0,     0,   405,    73,    74,     0,
      75,   180,     0,    76,    77,     0,    78,    79,     0,    80,
      81,     0,     0,     0,     0,     0,     0,    82,     0,     0,
       0,     0,    83,     0,    84,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    85,     0,     0,     0,    86,    87,     0,
       0,    88,     0,     0,    89,     0,     0,    90
};

static const short yycheck[] =
{
      14,    15,    14,    15,     8,   299,   253,    30,    66,    19,
     178,    66,   337,   338,    72,    73,   143,   165,   508,   240,
      78,    79,    80,    33,    74,   179,    84,   542,    86,    87,
     467,     3,    20,     4,    22,   593,   121,    25,    26,    76,
       4,    38,    29,   558,    36,     4,     3,     5,   672,   777,
      77,    65,    44,    65,     3,    82,    83,    29,     3,     3,
      29,    88,    89,    54,    49,     0,    49,   840,    74,    68,
     364,    75,    49,     4,     5,     6,     7,     8,     8,     3,
      34,    46,    99,   111,     3,     3,    49,     4,   168,   103,
     113,    26,     5,     4,    70,    71,   124,    69,   156,    26,
     180,   117,   875,   118,   121,   674,   122,   835,   102,   167,
     101,   102,   127,     5,   111,   684,   122,   109,   110,   413,
     178,     5,   895,   122,   111,   152,   111,   103,   111,   123,
     567,    29,    29,    98,    64,   289,    90,   124,    81,   111,
     764,   164,   111,   701,   101,   122,   138,   705,   876,   137,
     142,   123,    44,   124,   212,   113,   114,   115,    24,   122,
     124,   149,    46,   600,   123,   122,    26,   125,   226,   123,
     228,   159,   687,   122,   132,   133,   111,   122,   122,    69,
     238,   335,   240,   241,   242,   243,   209,   245,   246,   203,
     248,   203,   123,   123,   124,   218,   254,   211,   122,   211,
     214,   259,   350,   122,   122,   774,   123,   234,   832,   833,
     834,   124,   123,   111,   111,   273,    62,   275,   512,    50,
      47,   279,    49,   281,   279,   123,   123,    99,   112,   287,
      98,   289,   224,    47,    80,    49,    37,    47,   122,    49,
     298,    47,    68,    49,   111,    46,    99,    65,   525,   121,
     527,   405,   111,   111,    72,    73,    99,   125,    70,    71,
     118,   429,   320,   321,   322,   124,   324,   322,   121,   324,
     564,   263,   566,    91,   332,   552,   334,   121,   121,   123,
     111,   339,   607,   129,   130,   111,    99,   118,   346,   536,
     233,   103,   673,   330,    56,   122,   333,    98,   335,    61,
     358,   522,   329,   361,   685,   123,   361,   111,   121,   123,
       3,   127,   124,   123,   337,   338,   122,    79,   126,    76,
      77,   811,   359,   123,   119,   125,    88,   121,    21,   123,
     624,    39,   355,   128,   357,    28,   131,    47,    48,    32,
      33,   122,    35,    53,   125,    38,    39,   405,    41,    42,
     111,    44,    45,    61,   111,   369,   414,     3,   416,    52,
     121,   118,   123,    55,    57,   121,    59,   122,   124,   427,
     397,   429,   430,   111,   432,   121,   122,   435,   124,   122,
     435,   124,   325,   433,   327,    78,    24,    25,   331,    82,
      83,     3,   122,    86,   124,   111,    89,    30,    38,    92,
     388,   112,   122,   461,   124,     3,   461,   115,   655,   123,
     122,   119,   124,   122,   122,   124,   124,   125,   126,   122,
     408,   124,   410,   122,   482,   124,   484,   121,   486,    62,
     123,    30,    31,   491,   492,   123,   491,    70,   496,   147,
     477,   735,   123,   737,   122,   123,   124,    80,   123,   507,
     464,   123,   466,   301,   302,   303,   304,   471,   305,   306,
     307,   519,   520,   122,   522,   124,   122,   490,   124,   123,
     103,   529,   530,   487,   622,   487,   490,   111,   490,   112,
     494,   317,   318,   319,   123,   122,   119,   124,   123,   122,
     517,   123,   122,   551,   124,   128,   129,   130,   131,   122,
      29,   124,   749,   123,   122,   752,   124,   122,    27,   124,
     804,    76,    77,   571,   572,   122,   118,   124,   576,   577,
     121,   122,   465,   848,    43,   122,   122,   124,   124,    47,
      48,   589,   590,   122,   120,   124,   594,   595,   481,    58,
     123,   599,   485,   123,    63,   488,   122,   255,   124,   123,
     718,   609,   495,   123,   681,   613,   122,   111,   124,   502,
     854,   123,   856,   271,   121,   122,    85,   625,    87,   122,
     122,   124,   124,   820,    93,    94,   123,   122,   605,   124,
     122,   608,   124,   597,   607,   308,   309,   310,   311,   312,
     313,   314,   111,   630,   121,   122,   355,   611,   357,   118,
       5,   615,    47,    48,   618,   121,   122,    68,   315,   316,
     122,   905,   626,     5,    95,   862,    75,   123,   632,     3,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
      19,    20,    21,    22,    23,   693,     3,   123,   111,    30,
     123,    34,     8,   123,   123,   112,    68,    95,   706,   707,
      53,   111,   121,   596,     5,   598,   683,   121,   123,    49,
       3,    30,   720,     3,   111,   111,    47,   725,    98,   727,
     680,    62,     3,   124,   732,   733,   619,   732,   121,    70,
     124,   389,   124,   124,   124,   124,   744,    76,   121,    80,
     125,   127,   124,    62,    96,    60,   754,   711,   686,   713,
     124,    70,   104,   105,   106,   107,   108,   109,   110,   767,
      84,    80,   103,   123,   116,   124,    30,   731,   124,   123,
     778,   112,   111,   121,   782,   124,     3,   782,   119,    68,
     124,   122,    95,   760,   103,   124,    27,   128,   129,   130,
     131,   799,     5,   112,     3,    53,     3,   805,    62,    49,
     119,   112,    43,   122,   124,   765,    70,   121,   100,   128,
     129,   130,   131,   123,    51,   124,    80,    58,   112,   712,
     126,   714,    63,    98,   717,   123,   111,   791,     3,   793,
     794,   102,   123,   121,   121,    21,   729,   801,   124,   103,
     123,   123,   850,   123,    85,   123,    87,    79,   112,     3,
     124,   123,    93,    94,   122,   119,    49,   865,   122,   124,
      98,   124,   124,   521,   128,   129,   130,   131,   526,   124,
     111,   124,   112,    73,   882,   848,   111,   118,    27,    49,
     538,   889,   124,   541,     3,   839,    67,    49,   124,     3,
     121,   899,   900,     3,    43,   124,     8,    98,   556,   124,
     124,   559,    49,    27,   124,   124,   124,    12,   600,    58,
     536,    21,   660,   877,    63,   877,   629,   820,    28,   739,
     837,   155,    32,    33,    23,    35,   141,   742,    38,    39,
     634,    41,    42,   531,    44,    45,    85,    13,    87,    69,
     746,    57,    52,   300,    93,    94,   738,    57,   572,    59,
     627,   429,    34,   159,   137,   676,    66,   398,   258,   550,
     487,   568,   111,   577,   751,    23,    26,   149,    78,   118,
     514,   760,    82,    83,   823,   515,    86,    -1,   410,    89,
      -1,    -1,    92,   686,    -1,     3,    96,    -1,    -1,    -1,
     648,    -1,    -1,    -1,   104,   105,   106,   107,   108,   109,
     110,   111,    -1,    -1,    -1,    -1,   116,    -1,   118,    27,
      28,    -1,   670,   123,    32,    33,    -1,    35,   128,    -1,
      38,    39,    -1,    41,    42,    -1,    44,    45,    -1,    47,
      -1,    -1,    -1,    -1,    52,    -1,    -1,     3,    -1,    57,
      -1,    59,    -1,    -1,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    19,    20,    21,    22,    23,    -1,
      78,    -1,    28,    -1,    82,    83,    32,    33,    86,    35,
      -1,    89,    38,    39,    92,    41,    42,    -1,    44,    45,
      -1,    -1,    -1,    -1,    -1,    -1,    52,    -1,    96,     3,
      -1,    57,    -1,    59,    -1,    -1,   104,   105,   106,   107,
     108,   109,   110,   111,    -1,   123,    -1,    -1,   116,    75,
     118,    76,    78,    -1,    28,   123,    82,    83,    32,    33,
      86,    35,    -1,    89,    38,    39,    92,    41,    42,    -1,
      44,    45,    -1,    -1,    -1,    -1,    -1,    -1,    52,    -1,
      -1,     3,    -1,    57,    -1,    59,   111,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   123,   123,    -1,
      -1,    -1,    -1,    -1,    78,    -1,    28,    -1,    82,    83,
      32,    33,    86,    35,    -1,    89,    38,    39,    92,    41,
      42,    -1,    44,    45,    -1,    -1,    -1,    -1,    -1,    -1,
      52,    -1,    27,    -1,    -1,    57,    -1,    59,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    43,   123,
      -1,    -1,    -1,    -1,    -1,    -1,    78,    -1,    -1,    -1,
      82,    83,    -1,    58,    86,    66,    -1,    89,    63,    -1,
      92,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      18,    19,    20,    21,    22,    23,    -1,    -1,    -1,    -1,
      85,    -1,    87,    -1,    -1,    96,    -1,    -1,    93,    94,
      38,   123,    -1,   104,   105,   106,   107,   108,   109,   110,
     111,    -1,    -1,    -1,    -1,   116,   111,   118,    -1,    -1,
      -1,    59,   123,   118,    -1,    -1,    -1,   128,    66,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    76,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    18,    19,
      20,    21,    22,    23,    -1,    -1,    -1,    -1,    96,     3,
      -1,    -1,    -1,    -1,    -1,    -1,   104,   105,   106,   107,
     108,   109,   110,   111,    -1,    -1,    -1,    -1,   116,    -1,
     118,    -1,    -1,    -1,    28,   123,    -1,    -1,    32,    33,
     128,    35,    -1,    27,    38,    39,    -1,    41,    42,    -1,
      44,    45,    -1,    -1,    -1,    -1,    76,    -1,    52,    43,
      -1,    -1,    -1,    57,    -1,    59,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    67,    58,    -1,    -1,    -1,    -1,    63,
      -1,    -1,    -1,    -1,    78,    -1,    -1,    -1,    82,    83,
       3,   111,    86,    -1,    -1,    89,    -1,    -1,    92,    -1,
      -1,    85,   122,    87,    -1,   125,    -1,    -1,    -1,    93,
      94,    -1,    -1,    -1,    27,    28,    -1,   111,    -1,    32,
      33,    -1,    35,    -1,   118,    38,    39,   111,    41,    42,
      -1,    44,    45,    -1,    -1,    -1,    -1,    -1,    -1,    52,
      -1,    -1,    -1,    -1,    57,    -1,    59,    -1,    -1,    -1,
      -1,    -1,     3,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    78,    -1,    -1,    -1,    82,
      83,    -1,    -1,    86,    -1,    -1,    89,    28,    -1,    92,
      -1,    32,    33,    -1,    35,    -1,    -1,    38,    39,    -1,
      41,    42,    -1,    44,    45,    -1,    -1,    -1,   111,    -1,
      -1,    52,    -1,    -1,    -1,   118,    57,    -1,    59,    -1,
      -1,    -1,    -1,    -1,    -1,     3,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    78,    -1,    -1,
      -1,    82,    83,    -1,    -1,    86,    -1,    -1,    89,    -1,
      28,    92,    -1,    -1,    32,    33,    97,    35,    -1,    -1,
      38,    39,    -1,    41,    42,    -1,    44,    45,    -1,    -1,
     111,    -1,    -1,    -1,    52,    -1,    -1,   118,    -1,    57,
      -1,    59,    -1,    -1,    -1,    -1,    -1,    -1,     3,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      78,    -1,    -1,    -1,    82,    83,    -1,    -1,    86,    -1,
      -1,    89,    27,    28,    92,    -1,    -1,    32,    33,    97,
      35,    -1,    -1,    38,    39,    -1,    41,    42,    -1,    44,
      45,    -1,    -1,   111,    -1,    -1,    -1,    52,    -1,    -1,
     118,    -1,    57,    -1,    59,    -1,    -1,    -1,    -1,    -1,
       3,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    78,    -1,    -1,    -1,    82,    83,    -1,
      -1,    86,    -1,    -1,    89,    28,    -1,    92,    -1,    32,
      33,    -1,    35,    -1,    -1,    38,    39,    -1,    41,    42,
      -1,    44,    45,    -1,    -1,    -1,   111,    -1,    -1,    52,
      -1,    -1,    -1,   118,    57,    -1,    59,    -1,    -1,    -1,
      -1,    -1,    -1,     3,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    78,    -1,    -1,    -1,    82,
      83,    -1,    -1,    86,    -1,    -1,    89,    -1,    28,    92,
      -1,    -1,    32,    33,    97,    35,    -1,    -1,    38,    39,
      -1,    41,    42,    -1,    44,    45,    -1,    -1,   111,    -1,
      -1,    -1,    52,    -1,    -1,   118,    -1,    57,    -1,    59,
      -1,    -1,    -1,    -1,    -1,     3,    -1,    67,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    78,    -1,
      -1,    -1,    82,    83,    -1,    -1,    86,    -1,    -1,    89,
      28,    -1,    92,    -1,    32,    33,    -1,    35,    -1,    -1,
      38,    39,    -1,    41,    42,    -1,    44,    45,    -1,    -1,
      -1,   111,    -1,    -1,    52,    -1,    -1,    -1,   118,    57,
      -1,    59,    -1,    -1,    -1,    -1,    -1,     3,    -1,    67,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      78,    -1,    -1,    -1,    82,    83,    -1,    -1,    86,    -1,
      -1,    89,    28,    -1,    92,    -1,    32,    33,    -1,    35,
      -1,    -1,    38,    39,    -1,    41,    42,    -1,    44,    45,
      -1,    -1,    -1,   111,    -1,    -1,    52,    -1,    -1,    -1,
     118,    57,    -1,    59,    -1,    -1,    -1,    -1,    -1,    -1,
       3,    67,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    78,    -1,    -1,    -1,    82,    83,    -1,    -1,
      86,    -1,    -1,    89,    27,    28,    92,    -1,    -1,    32,
      33,    -1,    35,    -1,    -1,    38,    39,    -1,    41,    42,
      -1,    44,    45,    -1,    -1,   111,    -1,    -1,    -1,    52,
      -1,    -1,   118,    -1,    57,    -1,    59,    -1,    -1,    -1,
      -1,    -1,    -1,     3,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    78,    -1,    -1,    -1,    82,
      83,    -1,    -1,    86,    -1,    -1,    89,    27,    28,    92,
      -1,    -1,    32,    33,    -1,    35,    -1,    -1,    38,    39,
      -1,    41,    42,    -1,    44,    45,    -1,    -1,   111,    -1,
      -1,    -1,    52,    -1,    -1,   118,    -1,    57,    -1,    59,
      -1,    -1,    -1,    -1,    -1,    -1,     3,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    78,    -1,
      -1,    -1,    82,    83,    -1,    -1,    86,    -1,    -1,    89,
      27,    28,    92,    -1,    -1,    32,    33,    -1,    35,    -1,
      -1,    38,    39,    -1,    41,    42,    -1,    44,    45,    -1,
      -1,   111,    -1,    -1,    -1,    52,    -1,    -1,   118,    -1,
      57,    -1,    59,    -1,    -1,    -1,    -1,    -1,    -1,     3,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    78,    -1,    -1,    -1,    82,    83,    -1,    -1,    86,
      -1,    -1,    89,    -1,    28,    92,    -1,    -1,    32,    33,
      -1,    35,    -1,    -1,    38,    39,    -1,    41,    42,    -1,
      44,    45,    -1,    -1,   111,    49,    -1,    -1,    52,    -1,
      -1,   118,    -1,    57,    -1,    59,    -1,    -1,    -1,    -1,
      -1,     3,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    78,    -1,    -1,    -1,    82,    83,
      -1,    -1,    86,    -1,    -1,    89,    28,    -1,    92,    -1,
      32,    33,    -1,    35,    -1,    -1,    38,    39,    -1,    41,
      42,    -1,    44,    45,    -1,    -1,    -1,   111,    -1,    -1,
      52,    -1,    -1,    -1,   118,    57,    -1,    59,    -1,    -1,
      -1,    -1,    -1,     3,    -1,    67,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    78,    -1,    -1,    -1,
      82,    83,    -1,    -1,    86,    -1,    -1,    89,    28,    -1,
      92,    -1,    32,    33,    -1,    35,    -1,    -1,    38,    39,
      -1,    41,    42,    -1,    44,    45,    -1,    -1,    -1,   111,
      -1,    -1,    52,    -1,    -1,    -1,   118,    57,    -1,    59,
      -1,    -1,    -1,    -1,    -1,     3,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    78,    -1,
      -1,    -1,    82,    83,    -1,    -1,    86,    -1,    -1,    89,
      28,    -1,    92,    -1,    32,    33,    -1,    35,    -1,    -1,
      38,    39,    -1,    41,    42,    -1,    44,    45,    -1,    -1,
      -1,   111,    -1,    -1,    52,    -1,    -1,    -1,   118,    57,
      -1,    59,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    18,    19,    20,    21,    22,    23,    -1,    -1,    -1,
      78,    -1,    -1,    -1,    82,    83,    -1,    -1,    86,    -1,
      -1,    89,    -1,    -1,    92,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    18,    19,    20,    21,    22,    23,
      -1,    -1,    -1,   111,    -1,    -1,    -1,    -1,    -1,    -1,
     118,    -1,    -1,    -1,    -1,    -1,     4,    -1,    -1,    76,
      -1,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      18,    19,    20,    21,    22,    23,     9,    10,    11,    12,
      13,    14,    15,    16,    17,    18,    19,    20,    21,    22,
      23,    -1,    76,    -1,   111,    -1,    -1,    -1,    -1,    -1,
      -1,    38,    -1,    -1,    -1,    -1,   123,    -1,    -1,    -1,
      47,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    59,    -1,    -1,    -1,    -1,   111,    76,    66,
      38,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   122,    -1,
      -1,    -1,    -1,    76,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    59,    -1,    -1,    -1,    38,    -1,    -1,    66,    96,
      -1,    -1,    -1,   111,    -1,    -1,    -1,   104,   105,   106,
     107,   108,   109,   110,   111,    -1,    59,    -1,   111,   116,
      -1,   118,    -1,    66,    38,    -1,   123,    -1,    96,    -1,
      -1,   128,   129,    47,    -1,    -1,   104,   105,   106,   107,
     108,   109,   110,   111,    -1,    59,    -1,    -1,   116,   117,
     118,    -1,    66,    96,    -1,   123,    -1,    -1,    38,   127,
     128,   104,   105,   106,   107,   108,   109,   110,   111,    -1,
      -1,    -1,    -1,   116,    -1,   118,    -1,    -1,    -1,    59,
     123,    -1,    96,    -1,   127,   128,    66,    38,    -1,    -1,
     104,   105,   106,   107,   108,   109,   110,   111,    -1,    -1,
      -1,    -1,   116,    -1,   118,    -1,    -1,    -1,    59,   123,
      -1,    -1,    38,    -1,   128,    66,    96,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   104,   105,   106,   107,   108,   109,
     110,   111,    -1,    59,    -1,    -1,   116,    -1,   118,    -1,
      66,    38,    -1,   123,    -1,    96,    -1,    -1,   128,    -1,
      -1,    -1,    -1,   104,   105,   106,   107,   108,   109,   110,
     111,    -1,    59,    -1,    -1,   116,    -1,   118,    -1,    66,
      96,    -1,   123,    -1,    -1,    -1,    -1,   128,   104,   105,
     106,   107,   108,   109,   110,   111,    -1,    -1,    -1,    -1,
     116,    -1,   118,    -1,    -1,    -1,    -1,   123,     3,    96,
      -1,    -1,   128,    -1,    -1,    -1,    -1,   104,   105,   106,
     107,   108,   109,   110,   111,    -1,    -1,    -1,    -1,   116,
      -1,   118,    -1,    28,    -1,    -1,   123,    32,    33,    -1,
      35,   128,    -1,    38,    39,    -1,    41,    42,    -1,    44,
      45,    -1,    -1,    -1,    -1,    -1,    -1,    52,    -1,    -1,
      -1,    -1,    57,    -1,    59,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    78,    -1,    -1,    -1,    82,    83,    -1,
      -1,    86,    -1,    -1,    89,    -1,    -1,    92
};

/* YYSTOS[STATE-NUM] -- The (internal number of the) accessing
   symbol of state STATE-NUM.  */
static const unsigned short yystos[] =
{
       0,   135,     0,   111,   136,   138,   121,    24,    25,   148,
      26,   184,   137,   139,   219,   219,    24,   148,   184,    27,
      43,    58,    63,    85,    87,    93,    94,   111,   118,   194,
     222,   226,   231,    27,   185,    69,   159,   160,   111,   145,
     229,   230,    29,   111,   123,   169,   175,   176,   254,   255,
     261,   262,   145,   256,   257,   175,   177,   254,   111,   155,
     183,   145,   227,   228,   256,   121,   123,   127,   193,   118,
     127,     3,    28,    32,    33,    35,    38,    39,    41,    42,
      44,    45,    52,    57,    59,    78,    82,    83,    86,    89,
      92,   140,   146,   165,   166,   246,    30,    62,    70,    80,
     103,   112,   119,   122,   128,   129,   130,   131,   204,   159,
      27,   111,   226,   237,   261,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    18,    19,    20,    21,    22,    23,
      76,   111,   122,   171,   188,   273,     3,   122,   126,   261,
       3,   122,    74,   122,   210,    55,   253,   125,     3,   122,
       3,   210,    98,   125,     3,   122,   125,   273,     3,   122,
       3,    76,    77,   111,   223,   231,    38,    59,    66,    96,
     104,   105,   106,   107,   108,   109,   110,   111,   116,   123,
     128,   163,   164,   191,   194,   195,   196,   197,   198,   199,
     200,   201,   202,   203,   207,   214,   239,   111,   193,   111,
     203,   203,   191,   219,   150,   203,   262,   203,   203,    38,
     203,    54,   101,   102,   218,   262,   262,   203,    38,   203,
     167,   203,   262,   262,   159,   162,   224,   194,   112,   261,
     261,   121,    27,    47,   123,   161,   165,     3,   123,   273,
     123,   123,   123,   123,   273,   123,   123,   273,   123,   273,
     273,   273,   123,   123,   123,   123,   111,    34,    90,   123,
     234,   230,   261,   118,   176,   261,   255,    29,   273,   257,
     262,   123,   155,   111,   203,   125,   227,   220,   221,   123,
     165,   112,   204,   150,   203,   208,   195,   123,   117,   123,
     127,   203,   215,   216,   217,   242,   202,   195,   122,   124,
     120,    62,    80,   129,   130,   119,   128,   131,     5,   113,
     114,   115,   125,   132,   133,    30,    31,    70,    71,   103,
      99,   121,   123,   111,   123,     5,    44,     5,    27,    68,
     122,     5,    21,    38,    59,   123,   147,   149,   165,    75,
     180,    50,   231,   238,   252,   203,   247,    67,   111,   165,
     231,    95,   212,    21,   123,   154,   157,   165,   102,   123,
     178,   123,   261,   203,   203,     3,     3,    76,    77,   218,
     260,   262,   111,   203,   251,   273,     4,   250,   251,   203,
     203,   203,   203,   203,   203,   111,   181,   182,    38,   145,
     151,   189,   190,   203,   124,   172,   273,   123,   234,   203,
       8,   235,   261,   156,   273,   123,   203,   203,   123,   258,
     123,   268,   163,   203,   112,    68,    95,   213,    53,   163,
      47,   129,   202,   203,   243,   245,   111,   121,   117,   122,
     121,   122,   121,   124,   203,   123,   162,   196,   197,   197,
     197,   197,   198,   198,   198,   199,   199,   199,   199,   199,
     199,   199,   200,   200,   201,   201,   201,   203,   203,   163,
     203,   123,   163,   203,   218,     5,   218,   141,   142,   262,
     150,   218,   203,   150,   203,   150,    49,   123,   147,   147,
     203,     3,     5,    46,   112,     3,   101,   122,     3,   203,
     121,   123,   112,   204,   218,    47,    48,    53,   211,   111,
     111,   157,    47,   168,   157,   203,   150,    98,   179,   163,
     203,     3,   225,   162,   186,   187,    27,   122,   124,   123,
     121,   124,   122,   124,   124,   124,   124,   124,   124,   121,
     125,   122,   124,    68,   145,   273,   122,   124,   124,    84,
     170,   122,   124,     4,     5,     6,     7,     8,   274,   262,
     235,   121,   124,    60,   236,   124,   122,   124,   124,   145,
     271,   272,   124,   271,   124,   162,   203,   123,   240,   241,
     203,    47,    48,   209,   124,   124,   121,   122,   124,   203,
     216,   203,   127,   203,   191,   163,   124,   124,   124,    99,
     121,   163,   203,   124,    99,   121,    97,   218,    97,   123,
     143,   144,   242,   244,     3,   122,    27,     3,    68,    95,
     124,   218,   203,     5,   203,   218,   203,   238,   218,     3,
     111,   165,   231,   163,   203,   112,   218,   203,    53,     3,
       5,   124,   218,    49,   124,   203,   180,   124,   162,   258,
     268,   262,   121,   203,   203,   273,   251,   100,   158,   158,
     273,   158,   203,   203,   182,   121,   153,   190,   244,   249,
      68,    65,    72,    73,    91,   205,   206,   151,     4,   273,
     123,    51,   259,   273,   170,   124,   236,   203,   158,   112,
     232,   126,   273,    98,   170,   273,   122,   124,   270,   124,
     162,   162,   244,    47,    49,   241,   203,   208,   203,   245,
     111,   124,   192,   203,   203,   124,    99,   121,   192,   203,
     203,   218,    97,   218,    47,    49,   144,   121,   122,   262,
      75,   147,   121,    67,   203,    37,    46,    98,    67,     3,
      67,   218,   123,   112,   204,   124,   162,   203,   212,   154,
     150,   121,   178,    21,   121,     4,   273,   124,   190,    47,
      49,   122,   121,   153,   123,   263,   123,   264,     4,   273,
     123,    79,   174,   274,   259,   232,   124,   233,   159,   255,
     262,   259,   274,   272,   170,     3,   275,   269,   121,   203,
     122,   192,   123,   124,   124,   192,   203,   203,   124,   124,
      27,   218,    27,   218,   218,   242,   203,    49,   203,    98,
     203,   218,   248,   163,   203,   112,   162,   162,   211,   168,
     124,   179,   111,   203,   205,    49,   152,   190,   249,   190,
      47,    49,   203,    73,     8,    64,   124,   124,   260,   174,
     159,   203,    56,    61,    88,   173,   174,   259,   265,   266,
     275,   203,    49,   163,   124,   124,    27,    49,     3,    46,
      98,   203,    67,    67,   124,   162,   203,    49,   121,   180,
     124,    49,   122,    49,   152,   122,   124,   263,     4,   124,
       8,   124,   174,   174,   174,   275,   173,   219,   265,     3,
     124,   147,    98,   203,   162,   162,   190,    49,   203,   122,
     124,   124,     4,   124,   265,   275,   267,    49,   203,   121,
     122,   124,   203,   124,   265,    27,   203,   203,   124,   162,
     124,   124
};

#if ! defined (YYSIZE_T) && defined (__SIZE_TYPE__)
# define YYSIZE_T __SIZE_TYPE__
#endif
#if ! defined (YYSIZE_T) && defined (size_t)
# define YYSIZE_T size_t
#endif
#if ! defined (YYSIZE_T)
# if defined (__STDC__) || defined (__cplusplus)
#  include <stddef.h> /* INFRINGES ON USER NAME SPACE */
#  define YYSIZE_T size_t
# endif
#endif
#if ! defined (YYSIZE_T)
# define YYSIZE_T unsigned int
#endif

#define yyerrok		(yyerrstatus = 0)
#define yyclearin	(yychar = YYEMPTY)
#define YYEMPTY		(-2)
#define YYEOF		0

#define YYACCEPT	goto yyacceptlab
#define YYABORT		goto yyabortlab
#define YYERROR		goto yyerrorlab


/* Like YYERROR except do call yyerror.  This remains here temporarily
   to ease the transition to the new meaning of YYERROR, for GCC.
   Once GCC version 2 has supplanted version 1, this can go.  */

#define YYFAIL		goto yyerrlab

#define YYRECOVERING()  (!!yyerrstatus)

#define YYBACKUP(Token, Value)					\
do								\
  if (yychar == YYEMPTY && yylen == 1)				\
    {								\
      yychar = (Token);						\
      yylval = (Value);						\
      yytoken = YYTRANSLATE (yychar);				\
      YYPOPSTACK;						\
      goto yybackup;						\
    }								\
  else								\
    { 								\
      yyerror ("syntax error: cannot back up");\
      YYERROR;							\
    }								\
while (0)

#define YYTERROR	1
#define YYERRCODE	256

/* YYLLOC_DEFAULT -- Compute the default location (before the actions
   are run).  */

#ifndef YYLLOC_DEFAULT
# define YYLLOC_DEFAULT(Current, Rhs, N)		\
   ((Current).first_line   = (Rhs)[1].first_line,	\
    (Current).first_column = (Rhs)[1].first_column,	\
    (Current).last_line    = (Rhs)[N].last_line,	\
    (Current).last_column  = (Rhs)[N].last_column)
#endif

/* YYLEX -- calling `yylex' with the right arguments.  */

#ifdef YYLEX_PARAM
# define YYLEX yylex (YYLEX_PARAM)
#else
# define YYLEX yylex ()
#endif

/* Enable debugging if requested.  */
#if YYDEBUG

# ifndef YYFPRINTF
#  include <stdio.h> /* INFRINGES ON USER NAME SPACE */
#  define YYFPRINTF fprintf
# endif

# define YYDPRINTF(Args)			\
do {						\
  if (yydebug)					\
    YYFPRINTF Args;				\
} while (0)

# define YYDSYMPRINT(Args)			\
do {						\
  if (yydebug)					\
    yysymprint Args;				\
} while (0)

# define YYDSYMPRINTF(Title, Token, Value, Location)		\
do {								\
  if (yydebug)							\
    {								\
      YYFPRINTF (stderr, "%s ", Title);				\
      yysymprint (stderr, 					\
                  Token, Value);	\
      YYFPRINTF (stderr, "\n");					\
    }								\
} while (0)

/*------------------------------------------------------------------.
| yy_stack_print -- Print the state stack from its BOTTOM up to its |
| TOP (included).                                                   |
`------------------------------------------------------------------*/

#if defined (__STDC__) || defined (__cplusplus)
static void
yy_stack_print (short *bottom, short *top)
#else
static void
yy_stack_print (bottom, top)
    short *bottom;
    short *top;
#endif
{
  YYFPRINTF (stderr, "Stack now");
  for (/* Nothing. */; bottom <= top; ++bottom)
    YYFPRINTF (stderr, " %d", *bottom);
  YYFPRINTF (stderr, "\n");
}

# define YY_STACK_PRINT(Bottom, Top)				\
do {								\
  if (yydebug)							\
    yy_stack_print ((Bottom), (Top));				\
} while (0)


/*------------------------------------------------.
| Report that the YYRULE is going to be reduced.  |
`------------------------------------------------*/

#if defined (__STDC__) || defined (__cplusplus)
static void
yy_reduce_print (int yyrule)
#else
static void
yy_reduce_print (yyrule)
    int yyrule;
#endif
{
  int yyi;
  unsigned int yylno = yyrline[yyrule];
  YYFPRINTF (stderr, "Reducing stack by rule %d (line %u), ",
             yyrule - 1, yylno);
  /* Print the symbols being reduced, and their result.  */
  for (yyi = yyprhs[yyrule]; 0 <= yyrhs[yyi]; yyi++)
    YYFPRINTF (stderr, "%s ", yytname [yyrhs[yyi]]);
  YYFPRINTF (stderr, "-> %s\n", yytname [yyr1[yyrule]]);
}

# define YY_REDUCE_PRINT(Rule)		\
do {					\
  if (yydebug)				\
    yy_reduce_print (Rule);		\
} while (0)

/* Nonzero means print parse trace.  It is left uninitialized so that
   multiple parsers can coexist.  */
int yydebug;
#else /* !YYDEBUG */
# define YYDPRINTF(Args)
# define YYDSYMPRINT(Args)
# define YYDSYMPRINTF(Title, Token, Value, Location)
# define YY_STACK_PRINT(Bottom, Top)
# define YY_REDUCE_PRINT(Rule)
#endif /* !YYDEBUG */


/* YYINITDEPTH -- initial size of the parser's stacks.  */
#ifndef	YYINITDEPTH
# define YYINITDEPTH 200
#endif

/* YYMAXDEPTH -- maximum size the stacks can grow to (effective only
   if the built-in stack extension method is used).

   Do not make this value too large; the results are undefined if
   SIZE_MAX < YYSTACK_BYTES (YYMAXDEPTH)
   evaluated with infinite-precision integer arithmetic.  */

#if defined (YYMAXDEPTH) && YYMAXDEPTH == 0
# undef YYMAXDEPTH
#endif

#ifndef YYMAXDEPTH
# define YYMAXDEPTH 10000
#endif



#if YYERROR_VERBOSE

# ifndef yystrlen
#  if defined (__GLIBC__) && defined (_STRING_H)
#   define yystrlen strlen
#  else
/* Return the length of YYSTR.  */
static YYSIZE_T
#   if defined (__STDC__) || defined (__cplusplus)
yystrlen (const char *yystr)
#   else
yystrlen (yystr)
     const char *yystr;
#   endif
{
  register const char *yys = yystr;

  while (*yys++ != '\0')
    continue;

  return yys - yystr - 1;
}
#  endif
# endif

# ifndef yystpcpy
#  if defined (__GLIBC__) && defined (_STRING_H) && defined (_GNU_SOURCE)
#   define yystpcpy stpcpy
#  else
/* Copy YYSRC to YYDEST, returning the address of the terminating '\0' in
   YYDEST.  */
static char *
#   if defined (__STDC__) || defined (__cplusplus)
yystpcpy (char *yydest, const char *yysrc)
#   else
yystpcpy (yydest, yysrc)
     char *yydest;
     const char *yysrc;
#   endif
{
  register char *yyd = yydest;
  register const char *yys = yysrc;

  while ((*yyd++ = *yys++) != '\0')
    continue;

  return yyd - 1;
}
#  endif
# endif

#endif /* !YYERROR_VERBOSE */



#if YYDEBUG
/*--------------------------------.
| Print this symbol on YYOUTPUT.  |
`--------------------------------*/

#if defined (__STDC__) || defined (__cplusplus)
static void
yysymprint (FILE *yyoutput, int yytype, YYSTYPE *yyvaluep)
#else
static void
yysymprint (yyoutput, yytype, yyvaluep)
    FILE *yyoutput;
    int yytype;
    YYSTYPE *yyvaluep;
#endif
{
  /* Pacify ``unused variable'' warnings.  */
  (void) yyvaluep;

  if (yytype < YYNTOKENS)
    {
      YYFPRINTF (yyoutput, "token %s (", yytname[yytype]);
# ifdef YYPRINT
      YYPRINT (yyoutput, yytoknum[yytype], *yyvaluep);
# endif
    }
  else
    YYFPRINTF (yyoutput, "nterm %s (", yytname[yytype]);

  switch (yytype)
    {
      default:
        break;
    }
  YYFPRINTF (yyoutput, ")");
}

#endif /* ! YYDEBUG */
/*-----------------------------------------------.
| Release the memory associated to this symbol.  |
`-----------------------------------------------*/

#if defined (__STDC__) || defined (__cplusplus)
static void
yydestruct (int yytype, YYSTYPE *yyvaluep)
#else
static void
yydestruct (yytype, yyvaluep)
    int yytype;
    YYSTYPE *yyvaluep;
#endif
{
  /* Pacify ``unused variable'' warnings.  */
  (void) yyvaluep;

  switch (yytype)
    {

      default:
        break;
    }
}


/* Prevent warnings from -Wmissing-prototypes.  */

#ifdef YYPARSE_PARAM
# if defined (__STDC__) || defined (__cplusplus)
int yyparse (void *YYPARSE_PARAM);
# else
int yyparse ();
# endif
#else /* ! YYPARSE_PARAM */
#if defined (__STDC__) || defined (__cplusplus)
int yyparse (void);
#else
int yyparse ();
#endif
#endif /* ! YYPARSE_PARAM */



/* The lookahead symbol.  */
int yychar;

/* The semantic value of the lookahead symbol.  */
YYSTYPE yylval;

/* Number of syntax errors so far.  */
int yynerrs;



/*----------.
| yyparse.  |
`----------*/

#ifdef YYPARSE_PARAM
# if defined (__STDC__) || defined (__cplusplus)
int yyparse (void *YYPARSE_PARAM)
# else
int yyparse (YYPARSE_PARAM)
  void *YYPARSE_PARAM;
# endif
#else /* ! YYPARSE_PARAM */
#if defined (__STDC__) || defined (__cplusplus)
int
yyparse (void)
#else
int
yyparse ()

#endif
#endif
{
  
  register int yystate;
  register int yyn;
  int yyresult;
  /* Number of tokens to shift before error messages enabled.  */
  int yyerrstatus;
  /* Lookahead token as an internal (translated) token number.  */
  int yytoken = 0;

  /* Three stacks and their tools:
     `yyss': related to states,
     `yyvs': related to semantic values,
     `yyls': related to locations.

     Refer to the stacks thru separate pointers, to allow yyoverflow
     to reallocate them elsewhere.  */

  /* The state stack.  */
  short	yyssa[YYINITDEPTH];
  short *yyss = yyssa;
  register short *yyssp;

  /* The semantic value stack.  */
  YYSTYPE yyvsa[YYINITDEPTH];
  YYSTYPE *yyvs = yyvsa;
  register YYSTYPE *yyvsp;



#define YYPOPSTACK   (yyvsp--, yyssp--)

  YYSIZE_T yystacksize = YYINITDEPTH;

  /* The variables used to return semantic value and location from the
     action routines.  */
  YYSTYPE yyval;


  /* When reducing, the number of symbols on the RHS of the reduced
     rule.  */
  int yylen;

  YYDPRINTF ((stderr, "Starting parse\n"));

  yystate = 0;
  yyerrstatus = 0;
  yynerrs = 0;
  yychar = YYEMPTY;		/* Cause a token to be read.  */

  /* Initialize stack pointers.
     Waste one element of value and location stack
     so that they stay on the same level as the state stack.
     The wasted elements are never initialized.  */

  yyssp = yyss;
  yyvsp = yyvs;

  goto yysetstate;

/*------------------------------------------------------------.
| yynewstate -- Push a new state, which is found in yystate.  |
`------------------------------------------------------------*/
 yynewstate:
  /* In all cases, when you get here, the value and location stacks
     have just been pushed. so pushing a state here evens the stacks.
     */
  yyssp++;

 yysetstate:
  *yyssp = yystate;

  if (yyss + yystacksize - 1 <= yyssp)
    {
      /* Get the current used size of the three stacks, in elements.  */
      YYSIZE_T yysize = yyssp - yyss + 1;

#ifdef yyoverflow
      {
	/* Give user a chance to reallocate the stack. Use copies of
	   these so that the &'s don't force the real ones into
	   memory.  */
	YYSTYPE *yyvs1 = yyvs;
	short *yyss1 = yyss;


	/* Each stack pointer address is followed by the size of the
	   data in use in that stack, in bytes.  This used to be a
	   conditional around just the two extra args, but that might
	   be undefined if yyoverflow is a macro.  */
	yyoverflow ("parser stack overflow",
		    &yyss1, yysize * sizeof (*yyssp),
		    &yyvs1, yysize * sizeof (*yyvsp),

		    &yystacksize);

	yyss = yyss1;
	yyvs = yyvs1;
      }
#else /* no yyoverflow */
# ifndef YYSTACK_RELOCATE
      goto yyoverflowlab;
# else
      /* Extend the stack our own way.  */
      if (YYMAXDEPTH <= yystacksize)
	goto yyoverflowlab;
      yystacksize *= 2;
      if (YYMAXDEPTH < yystacksize)
	yystacksize = YYMAXDEPTH;

      {
	short *yyss1 = yyss;
	union yyalloc *yyptr =
	  (union yyalloc *) YYSTACK_ALLOC (YYSTACK_BYTES (yystacksize));
	if (! yyptr)
	  goto yyoverflowlab;
	YYSTACK_RELOCATE (yyss);
	YYSTACK_RELOCATE (yyvs);

#  undef YYSTACK_RELOCATE
	if (yyss1 != yyssa)
	  YYSTACK_FREE (yyss1);
      }
# endif
#endif /* no yyoverflow */

      yyssp = yyss + yysize - 1;
      yyvsp = yyvs + yysize - 1;


      YYDPRINTF ((stderr, "Stack size increased to %lu\n",
		  (unsigned long int) yystacksize));

      if (yyss + yystacksize - 1 <= yyssp)
	YYABORT;
    }

  YYDPRINTF ((stderr, "Entering state %d\n", yystate));

  goto yybackup;

/*-----------.
| yybackup.  |
`-----------*/
yybackup:

/* Do appropriate processing given the current state.  */
/* Read a lookahead token if we need one and don't already have one.  */
/* yyresume: */

  /* First try to decide what to do without reference to lookahead token.  */

  yyn = yypact[yystate];
  if (yyn == YYPACT_NINF)
    goto yydefault;

  /* Not known => get a lookahead token if don't already have one.  */

  /* YYCHAR is either YYEMPTY or YYEOF or a valid lookahead symbol.  */
  if (yychar == YYEMPTY)
    {
      YYDPRINTF ((stderr, "Reading a token: "));
      yychar = YYLEX;
    }

  if (yychar <= YYEOF)
    {
      yychar = yytoken = YYEOF;
      YYDPRINTF ((stderr, "Now at end of input.\n"));
    }
  else
    {
      yytoken = YYTRANSLATE (yychar);
      YYDSYMPRINTF ("Next token is", yytoken, &yylval, &yylloc);
    }

  /* If the proper action on seeing token YYTOKEN is to reduce or to
     detect an error, take that action.  */
  yyn += yytoken;
  if (yyn < 0 || YYLAST < yyn || yycheck[yyn] != yytoken)
    goto yydefault;
  yyn = yytable[yyn];
  if (yyn <= 0)
    {
      if (yyn == 0 || yyn == YYTABLE_NINF)
	goto yyerrlab;
      yyn = -yyn;
      goto yyreduce;
    }

  if (yyn == YYFINAL)
    YYACCEPT;

  /* Shift the lookahead token.  */
  YYDPRINTF ((stderr, "Shifting token %s, ", yytname[yytoken]));

  /* Discard the token being shifted unless it is eof.  */
  if (yychar != YYEOF)
    yychar = YYEMPTY;

  *++yyvsp = yylval;


  /* Count tokens shifted since error; after three, turn off error
     status.  */
  if (yyerrstatus)
    yyerrstatus--;

  yystate = yyn;
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
| yyreduce -- Do a reduction.  |
`-----------------------------*/
yyreduce:
  /* yyn is the number of a rule to reduce with.  */
  yylen = yyr2[yyn];

  /* If YYLEN is nonzero, implement the default value of the action:
     `$$ = $1'.

     Otherwise, the following line sets YYVAL to garbage.
     This behavior is undocumented and Bison
     users should not rely upon it.  Assigning to YYVAL
     unconditionally makes the parser a bit smaller, and it avoids a
     GCC warning that YYVAL may be used uninitialized.  */
  yyval = yyvsp[1-yylen];


  YY_REDUCE_PRINT (yyn);
  switch (yyn)
    {
        case 2:
#line 118 "./parser.y"
    { set_module_name(NULL); }
    break;

  case 3:
#line 119 "./parser.y"
    {
		  emit_module(NULL, yyvsp[0].block);
		}
    break;

  case 4:
#line 122 "./parser.y"
    { set_module_name(yyvsp[-1].id.name); }
    break;

  case 5:
#line 123 "./parser.y"
    {
		  emit_module(&yyvsp[-3].id, yyvsp[0].block);
		}
    break;

  case 6:
#line 126 "./parser.y"
    { set_spec_module_name(NULL); }
    break;

  case 7:
#line 127 "./parser.y"
    {
		  emit_spec_module(NULL, yyvsp[0].block);
		}
    break;

  case 8:
#line 130 "./parser.y"
    { set_spec_module_name(yyvsp[-1].id.name); }
    break;

  case 9:
#line 131 "./parser.y"
    {
		  emit_spec_module(&yyvsp[-3].id, yyvsp[0].block);
		}
    break;

  case 15:
#line 150 "./parser.y"
    {}
    break;

  case 20:
#line 167 "./parser.y"
    {
		  yyval.idlist = ckalloc(sizeof(struct identifierlist));
		  memcpy(yyval.idlist, &yyvsp[0].id, sizeof(struct identifier));
		  yyval.idlist->next = NULL;
		}
    break;

  case 21:
#line 173 "./parser.y"
    {
		  yyval.idlist = ckalloc(sizeof(struct identifierlist));
		  memcpy(yyval.idlist, &yyvsp[0].id, sizeof(struct identifier));
		  yyval.idlist->next = yyvsp[-2].idlist;
		}
    break;

  case 28:
#line 194 "./parser.y"
    {
		  yyval.block.startline = yyvsp[-5].keyword.line;
		  yyval.block.startcol = yyvsp[-5].keyword.startcol;
		  yyval.block.endline = yyvsp[0].punct.line;
		  yyval.block.endcol = yyvsp[0].punct.endcol;
		}
    break;

  case 29:
#line 201 "./parser.y"
    {
		  yyval.block.startline = yyvsp[-5].keyword.line;
		  yyval.block.startcol = yyvsp[-5].keyword.startcol;
		  yyval.block.endline = yyvsp[0].punct.line;
		  yyval.block.endcol = yyvsp[0].punct.endcol;
		}
    break;

  case 46:
#line 245 "./parser.y"
    { }
    break;

  case 47:
#line 247 "./parser.y"
    { }
    break;

  case 48:
#line 249 "./parser.y"
    { }
    break;

  case 49:
#line 251 "./parser.y"
    { }
    break;

  case 50:
#line 256 "./parser.y"
    { }
    break;

  case 51:
#line 258 "./parser.y"
    { }
    break;

  case 68:
#line 303 "./parser.y"
    { }
    break;

  case 91:
#line 330 "./parser.y"
    {
		  emit_xref_procedure(yyvsp[-3].id.name, yyvsp[-3].id.line);
		}
    break;

  case 92:
#line 334 "./parser.y"
    {
		  emit_xref_procedure(yyvsp[-5].id.name, yyvsp[-5].id.line);
		}
    break;

  case 99:
#line 356 "./parser.y"
    {
		  yyval.type = yyvsp[-1].type;
		}
    break;

  case 100:
#line 360 "./parser.y"
    {
		  yyval.type = yyvsp[-2].type;
		}
    break;

  case 101:
#line 364 "./parser.y"
    {
		  yyval.type = yyvsp[-3].type;
		}
    break;

  case 102:
#line 368 "./parser.y"
    {
		  yyval.type = yyvsp[-3].type;
		}
    break;

  case 103:
#line 372 "./parser.y"
    {
		  yyval.type = yyvsp[-4].type;
		}
    break;

  case 104:
#line 376 "./parser.y"
    {
		  yyval.type.text = NULL;
		}
    break;

  case 105:
#line 383 "./parser.y"
    {
		  yyval.type.text = SN_StrDup("PROC");
		}
    break;

  case 106:
#line 387 "./parser.y"
    {
		  yyval.type.text = SN_StrDup("PROC");
		}
    break;

  case 107:
#line 394 "./parser.y"
    { }
    break;

  case 108:
#line 396 "./parser.y"
    { }
    break;

  case 126:
#line 442 "./parser.y"
    {
		  yyval.idlist = ckalloc(sizeof(struct identifierlist));
		  memcpy(yyval.idlist, &yyvsp[0].id, sizeof(struct identifier));
		  yyval.idlist->next = NULL;
		}
    break;

  case 127:
#line 448 "./parser.y"
    {
		  yyval.idlist = ckalloc(sizeof(struct identifierlist));
		  memcpy(yyval.idlist, &yyvsp[0].id, sizeof(struct identifier));
		  yyval.idlist->next = yyvsp[-2].idlist;
		}
    break;

  case 128:
#line 457 "./parser.y"
    {
		  yyval.id = yyvsp[0].id;
		}
    break;

  case 129:
#line 461 "./parser.y"
    {
		  yyval.id = yyvsp[-2].id;
		}
    break;

  case 132:
#line 473 "./parser.y"
    {
	  yyval.block.startline = yyvsp[-5].keyword.line;
	  yyval.block.startcol = yyvsp[-5].keyword.startcol;
	  yyval.block.endline = yyvsp[0].punct.line;
	  yyval.block.endcol = yyvsp[0].punct.endcol;
	}
    break;

  case 134:
#line 483 "./parser.y"
    { set_proc_name(yyvsp[-2].id.name); }
    break;

  case 136:
#line 484 "./parser.y"
    { set_process_name(yyvsp[-2].id.name); }
    break;

  case 139:
#line 490 "./parser.y"
    {
		  yyval.type = yyvsp[-3].type;
		}
    break;

  case 142:
#line 502 "./parser.y"
    {
		  /* for now */
		}
    break;

  case 156:
#line 531 "./parser.y"
    {
		  yyval.id = yyvsp[0].id;
		  yyval.id.type = simple;
		}
    break;

  case 157:
#line 536 "./parser.y"
    {
		  yyval.id = yyvsp[-3].id;
		  yyval.id.type = call;
		}
    break;

  case 158:
#line 541 "./parser.y"
    {
		  yyval.id = yyvsp[-7].id;
		  yyval.id.type = array; /* multidimensional array */
		}
    break;

  case 159:
#line 546 "./parser.y"
    {
		  yyval.id = yyvsp[-5].id;
		  yyval.id.type = array; /* slice */
		}
    break;

  case 160:
#line 551 "./parser.y"
    {
		  yyval.id = yyvsp[-5].id;
		  yyval.id.type = array; /* slice */
		}
    break;

  case 161:
#line 556 "./parser.y"
    {
		  yyval.id = yyvsp[0].id;
		  yyval.id.type = simple;
		}
    break;

  case 162:
#line 561 "./parser.y"
    {
		  yyval.id = yyvsp[-4].id;
		  yyval.id.type = array;
		}
    break;

  case 163:
#line 566 "./parser.y"
    {
		  yyval.id = yyvsp[-5].id;
		  yyval.id.type = array; /* slice */
		}
    break;

  case 164:
#line 571 "./parser.y"
    {
		  yyval.id = yyvsp[-5].id;
		  yyval.id.type = array; /* slice */
		}
    break;

  case 174:
#line 588 "./parser.y"
    {
		  if (with_active())
		  {
			if (get_with_scope() == NULL)
			{
#ifdef DEBUG
			  printf("WITH: into scope %s\n", yyvsp[0].id.name);
#endif
			  set_with_scope(yyvsp[0].id.name);
			}
		  }
		  else if (emit)
		  {
			if (yyvsp[0].id.type == call)
		    	{
			  emit_xref_procedure(yyvsp[0].id.name, yyvsp[0].id.line);
		    	}
			else 
			{
			  emit_xref_variable(yyvsp[0].id.name, yyvsp[0].id.line);
			}
		  }
		}
    break;

  case 225:
#line 689 "./parser.y"
    { }
    break;

  case 227:
#line 698 "./parser.y"
    { }
    break;

  case 228:
#line 700 "./parser.y"
    { }
    break;

  case 231:
#line 710 "./parser.y"
    { }
    break;

  case 232:
#line 712 "./parser.y"
    { }
    break;

  case 233:
#line 714 "./parser.y"
    { }
    break;

  case 234:
#line 719 "./parser.y"
    { }
    break;

  case 248:
#line 752 "./parser.y"
    {
		  emit_xref_procedure(yyvsp[-4].id.name, yyvsp[-4].id.line);
		}
    break;

  case 249:
#line 756 "./parser.y"
    {
		  emit_xref_procedure(yyvsp[-4].id.name, yyvsp[-4].id.line);
		}
    break;

  case 250:
#line 760 "./parser.y"
    {
		  emit_xref_assignment(yyvsp[-3].idlist);
		}
    break;

  case 251:
#line 764 "./parser.y"
    {
		  emit_xref_assignment(yyvsp[-3].idlist);
		}
    break;

  case 252:
#line 768 "./parser.y"
    {
		  emit_xref_assignment(yyvsp[-4].idlist);
		}
    break;

  case 253:
#line 772 "./parser.y"
    {
		  emit_xref_assignment(yyvsp[-4].idlist);
		}
    break;

  case 256:
#line 780 "./parser.y"
    { set_proc_name(yyvsp[-2].id.name); }
    break;

  case 257:
#line 781 "./parser.y"
    {
		  emit_procedure(&yyvsp[-4].id, yyvsp[0].signature);
		}
    break;

  case 258:
#line 784 "./parser.y"
    { set_process_name(yyvsp[-2].id.name); }
    break;

  case 259:
#line 785 "./parser.y"
    {
		  emit_process(&yyvsp[-4].id, yyvsp[0].signature);
		}
    break;

  case 260:
#line 788 "./parser.y"
    { emit = 1; }
    break;

  case 261:
#line 788 "./parser.y"
    { emit = 0; }
    break;

  case 262:
#line 789 "./parser.y"
    { emit = 1; }
    break;

  case 263:
#line 789 "./parser.y"
    { emit = 0; }
    break;

  case 264:
#line 791 "./parser.y"
    {
		  emit_xref_procedure(yyvsp[-4].id.name, yyvsp[-4].id.line);
		}
    break;

  case 265:
#line 795 "./parser.y"
    {
		  emit_xref_procedure(yyvsp[-4].id.name, yyvsp[-4].id.line);
		}
    break;

  case 266:
#line 798 "./parser.y"
    { emit = 1; }
    break;

  case 267:
#line 798 "./parser.y"
    { emit = 0; }
    break;

  case 268:
#line 799 "./parser.y"
    {
		  emit_xref_assignment(yyvsp[-5].idlist);
		}
    break;

  case 269:
#line 803 "./parser.y"
    {
		  emit_xref_assignment(yyvsp[-3].idlist);
		}
    break;

  case 270:
#line 807 "./parser.y"
    {
		  emit_xref_assignment(yyvsp[-4].idlist);
		}
    break;

  case 271:
#line 811 "./parser.y"
    {
		  emit_xref_assignment(yyvsp[-4].idlist);
		}
    break;

  case 280:
#line 829 "./parser.y"
    {
		  emit_synonyms(yyvsp[-3].idlist, yyvsp[-2].type.text);
		}
    break;

  case 281:
#line 833 "./parser.y"
    {
		  emit_synonyms(yyvsp[-2].idlist, "unknown");
		}
    break;

  case 282:
#line 837 "./parser.y"
    {
		  emit_synonyms(yyvsp[-3].idlist, "unknown");
		}
    break;

  case 287:
#line 854 "./parser.y"
    {
		  /* We have a bunch of names and their mode. */
		  if (local)
		  {
		    remember_locals(yyvsp[-6].idlist);
		  }
		  emit_declarations(yyvsp[-6].idlist, yyvsp[-5].type.text);
		}
    break;

  case 288:
#line 863 "./parser.y"
    {
		  /* Likewise, but take qualifiers into account. */
		  if (local)
		  {
		    remember_locals(yyvsp[-7].idlist);
		  }
		  emit_declarations(yyvsp[-7].idlist, yyvsp[-6].type.text);
		}
    break;

  case 289:
#line 875 "./parser.y"
    {
                  yyval.idlist = ckalloc(sizeof(struct identifierlist));
                  memcpy(yyval.idlist, &yyvsp[0].id, sizeof(struct identifier));
                  yyval.idlist->next = NULL;
                }
    break;

  case 290:
#line 881 "./parser.y"
    {
                  yyval.idlist = ckalloc(sizeof(struct identifierlist));
                  memcpy(yyval.idlist, &yyvsp[0].id, sizeof(struct identifier));
                  yyval.idlist->next = yyvsp[-2].idlist;
                }
    break;

  case 291:
#line 889 "./parser.y"
    { emit = 1; }
    break;

  case 292:
#line 889 "./parser.y"
    { emit = 0; }
    break;

  case 296:
#line 900 "./parser.y"
    {
		  yyval.attrib = yyvsp[0].attrib;
		}
    break;

  case 297:
#line 904 "./parser.y"
    {
		  yyval.attrib.text = SN_StrDup("");
		}
    break;

  case 303:
#line 922 "./parser.y"
    {
		  emit_xref_assignment(yyvsp[-4].idlist);
		}
    break;

  case 304:
#line 926 "./parser.y"
    {
		  emit_xref_assignment(yyvsp[-6].idlist);
		}
    break;

  case 305:
#line 930 "./parser.y"
    {
		  emit_xref_assignment(yyvsp[-5].idlist);
		}
    break;

  case 306:
#line 934 "./parser.y"
    {
		  emit_xref_assignment(yyvsp[-7].idlist);
		}
    break;

  case 307:
#line 938 "./parser.y"
    { }
    break;

  case 308:
#line 940 "./parser.y"
    { }
    break;

  case 309:
#line 945 "./parser.y"
    { }
    break;

  case 310:
#line 947 "./parser.y"
    { }
    break;

  case 311:
#line 952 "./parser.y"
    { }
    break;

  case 312:
#line 954 "./parser.y"
    { }
    break;

  case 328:
#line 988 "./parser.y"
    { activate_with(); }
    break;

  case 329:
#line 988 "./parser.y"
    { leave_with(); }
    break;

  case 334:
#line 999 "./parser.y"
    { }
    break;

  case 335:
#line 1001 "./parser.y"
    { }
    break;

  case 336:
#line 1006 "./parser.y"
    { }
    break;

  case 337:
#line 1008 "./parser.y"
    { yyval.type = yyvsp[0].type; }
    break;

  case 338:
#line 1010 "./parser.y"
    { }
    break;

  case 339:
#line 1015 "./parser.y"
    { }
    break;

  case 340:
#line 1017 "./parser.y"
    { }
    break;

  case 346:
#line 1033 "./parser.y"
    {
		  /* forbid for GRANT only, but we'll relax it */
		}
    break;

  case 349:
#line 1045 "./parser.y"
    {
		  emit_type_synonyms(yyvsp[-2].idlist, yyvsp[0].type.text);
		}
    break;

  case 350:
#line 1052 "./parser.y"
    {
		  unsigned length = 0;
		  struct identifierlist * idlist = yyvsp[-6].idlist;
		  while (idlist != NULL)
		  {
		    length += strlen(idlist->name);
		    if (idlist->next != NULL)
		    {
		      length += 2; /* ", " */
		    }
		    idlist = idlist->next;
		  }
		  yyval.signature.args = ckalloc((length + 1)*sizeof(char));
		  memset (yyval.signature.args, 0, (length + 1)*sizeof(char));
		  idlist = yyvsp[-6].idlist;
		  while (idlist != NULL)
		  {
		    strcat(yyval.signature.args, idlist->name);
		    if (idlist->next != NULL) {
		      strcat(yyval.signature.args, ", ");
		    }
		    idlist = idlist->next;
		  }
		  yyval.signature.rettype = yyvsp[-4].type.text;
		}
    break;

  case 351:
#line 1079 "./parser.y"
    {
		  yyval.signature.rettype = yyvsp[-4].type.text;
		  yyval.signature.args = NULL;
		}
    break;

  case 355:
#line 1093 "./parser.y"
    { }
    break;

  case 356:
#line 1095 "./parser.y"
    { }
    break;

  case 357:
#line 1100 "./parser.y"
    { }
    break;

  case 358:
#line 1102 "./parser.y"
    { yyval.id.name = SN_StrDup(""); }
    break;

  case 359:
#line 1107 "./parser.y"
    {
		  yyval.id = yyvsp[-2].id;
		}
    break;

  case 360:
#line 1111 "./parser.y"
    {
		  yyval.id = yyvsp[0].id;
		}
    break;

  case 361:
#line 1115 "./parser.y"
    { }
    break;

  case 368:
#line 1131 "./parser.y"
    { local = 1; }
    break;

  case 369:
#line 1131 "./parser.y"
    { local = 0; }
    break;

  case 371:
#line 1135 "./parser.y"
    { local = 1; }
    break;

  case 372:
#line 1136 "./parser.y"
    {
		  yyval.signature.rettype = NULL;
		  yyval.signature.args = NULL; /* for now */
		  local = 0;
		}
    break;

  case 373:
#line 1141 "./parser.y"
    { local = 1; }
    break;

  case 374:
#line 1142 "./parser.y"
    {
		  yyval.signature.rettype = NULL;
		  yyval.signature.args = NULL; /* for now */
		  local = 0;
		}
    break;

  case 375:
#line 1151 "./parser.y"
    {
		  yyval.idlist = ckalloc(sizeof(struct identifierlist));
		  yyval.idlist->name = yyvsp[0].type.text;
		  yyval.idlist->next = NULL;
		}
    break;

  case 376:
#line 1157 "./parser.y"
    {
		  yyval.idlist = ckalloc(sizeof(struct identifierlist));
		  yyval.idlist->name = yyvsp[0].type.text;
		  yyval.idlist->next = yyvsp[-2].idlist;
		}
    break;

  case 377:
#line 1166 "./parser.y"
    {
		  struct identifierlist * idlist = yyvsp[-2].idlist;
		  yyval.type.text = NULL;

		  /* Emit the actual local variable declarations produced
		     by these formal parameters while we have context. */
		  
		  emit_declarations(yyvsp[-2].idlist, yyvsp[-1].type.text);
		  remember_locals(yyvsp[-2].idlist);

		  while (idlist != NULL)
		  {
		    unsigned length = strlen(idlist->name) + 1 + strlen(yyvsp[-1].type.text) + 1 + strlen(yyvsp[0].attrib.text) + 2 /* ", " */;
		    if (yyval.type.text == NULL)
		    {
		      yyval.type.text = ckalloc(length + 1 /*NUL*/);
		      yyval.type.text[0] = 0;
		    }
		    else
		    {
		      yyval.type.text = ckrealloc(yyval.type.text, strlen(yyval.type.text) + length);
		    } 
			   
		    /* Now populate the string. */
		    strcat(yyval.type.text, idlist->name);
		    strcat(yyval.type.text, " ");
		    strcat(yyval.type.text, yyvsp[-1].type.text);
		    if (strcmp(yyvsp[0].attrib.text, "") != 0)
		    {
		  	strcat(yyval.type.text, " ");
		        strcat(yyval.type.text, yyvsp[0].attrib.text);
		    }
		    strcat(yyval.type.text, ", ");
		    if (idlist->next == NULL)
		    {
		      yyval.type.text[strlen(yyval.type.text) - 2] = 0;
		    }
		    idlist = idlist->next;
		  }
		}
    break;

  case 378:
#line 1210 "./parser.y"
    {
		  yyval.type = yyvsp[0].type;
		}
    break;

  case 379:
#line 1214 "./parser.y"
    {
		  yyval.type.text = strjoin(yyvsp[-2].type.text, strjoin(yyvsp[-1].type.text, yyvsp[0].attrib.text));
		}
    break;

  case 380:
#line 1218 "./parser.y"
    {
		  yyval.type.text = strjoin(yyvsp[-1].type.text, yyvsp[0].type.text);
		}
    break;

  case 381:
#line 1222 "./parser.y"
    {
		  yyval.type.text = (char *) ckalloc(strlen(yyvsp[-3].type.text) + strlen(yyvsp[-1].type.text) + 3); /* (,) and a NULL. */
		  sprintf(yyval.type.text, "%s (%s)", yyvsp[-3].type.text, yyvsp[-1].type.text);
		}
    break;

  case 382:
#line 1227 "./parser.y"
    {
		  yyval.type.text = (char *) ckalloc(strlen(yyvsp[-4].type.text) + strlen(yyvsp[-2].type.text) + strlen(yyvsp[0].type.text) + 3);
		  sprintf(yyval.type.text, "%s (%s) %s", yyvsp[-4].type.text, yyvsp[-2].type.text, yyvsp[0].type.text);
		}
    break;

  case 383:
#line 1232 "./parser.y"
    {
		  yyval.type.text = (char *) ckalloc(strlen(yyvsp[-5].type.text) + strlen(yyvsp[-3].type.text) + strlen(yyvsp[-1].type.text) + 3);
		  sprintf(yyval.type.text, "%s (%s) %s", yyvsp[-5].type.text, yyvsp[-3].type.text, yyvsp[-1].type.text);
		}
    break;

  case 384:
#line 1237 "./parser.y"
    {
		  /* $$.text = strjoin($1.text, strjoin(brackets, $5.text)); */
		}
    break;

  case 385:
#line 1241 "./parser.y"
    {
		  yyval.type = yyvsp[-3].type;
		}
    break;

  case 386:
#line 1245 "./parser.y"
    {	
		  yyval.type.text = strjoin(yyvsp[-4].type.text, brackets);
		}
    break;

  case 387:
#line 1249 "./parser.y"
    {
		  yyval.type.text = strjoin(yyvsp[-4].type.text, yyvsp[0].type.text);
		}
    break;

  case 388:
#line 1253 "./parser.y"
    {	
		  yyval.type.text = strjoin(yyvsp[-1].type.text, yyvsp[0].type.text);
		}
    break;

  case 389:
#line 1257 "./parser.y"
    {
		  yyval.type.text = strjoin(yyvsp[-4].type.text, brackets);
		}
    break;

  case 390:
#line 1261 "./parser.y"
    {
		  yyval.type = yyvsp[0].type;
		}
    break;

  case 391:
#line 1265 "./parser.y"
    {	
		  yyval.type = yyvsp[-3].type;
		}
    break;

  case 392:
#line 1269 "./parser.y"
    {
		  yyval.type.text = strjoin(yyvsp[-5].type.text, brackets);
		}
    break;

  case 393:
#line 1273 "./parser.y"
    {
		  yyval.type.text = strjoin(yyvsp[-4].type.text, brackets);
		}
    break;

  case 394:
#line 1277 "./parser.y"
    {
		  yyval.type.text = yyvsp[0].id.name;
		}
    break;

  case 395:
#line 1281 "./parser.y"
    {
		  yyval.type.text = strjoin(yyvsp[-1].type.text, yyvsp[0].type.text);
		}
    break;

  case 396:
#line 1285 "./parser.y"
    {
		  yyval.type = yyvsp[0].type;
		}
    break;

  case 397:
#line 1289 "./parser.y"
    {
		  yyval.type = yyvsp[-5].type;
		}
    break;

  case 398:
#line 1293 "./parser.y"
    {
		  yyval.type.text = strjoin(yyvsp[-1].type.text, yyvsp[0].type.text);
		}
    break;

  case 399:
#line 1297 "./parser.y"
    {
		  yyval.type.text = strjoin(yyvsp[-1].type.text, yyvsp[0].type.text);
		}
    break;

  case 400:
#line 1301 "./parser.y"
    {
		  yyval.type.text = strjoin(yyvsp[-1].type.text, yyvsp[0].type.text);
		}
    break;

  case 401:
#line 1305 "./parser.y"
    {
		  yyval.type = yyvsp[-3].type; emit_enumeration(); emit_enumeration_values(yyvsp[-1].idlist);
		}
    break;

  case 402:
#line 1309 "./parser.y"
    {
		  yyval.type = yyvsp[0].type;
		}
    break;

  case 403:
#line 1313 "./parser.y"
    {
		  yyval.type = yyvsp[-3].type;
		}
    break;

  case 404:
#line 1317 "./parser.y"
    {
		  yyval.type.text = strjoin(yyvsp[-4].type.text, yyvsp[0].attrib.text);
		}
    break;

  case 405:
#line 1321 "./parser.y"
    {
		  yyval.type.text = strjoin(yyvsp[-4].type.text, yyvsp[0].type.text);
		}
    break;

  case 406:
#line 1325 "./parser.y"
    {
		  yyval.type.text = strjoin(yyvsp[-5].type.text, strjoin(yyvsp[-1].type.text, yyvsp[0].attrib.text));
		}
    break;

  case 407:
#line 1332 "./parser.y"
    {
	  	  yyval.attrib = yyvsp[0].attrib;
		}
    break;

  case 408:
#line 1336 "./parser.y"
    {
		  yyval.attrib =yyvsp[0].attrib;
		}
    break;

  case 409:
#line 1340 "./parser.y"
    {
		  yyval.attrib = yyvsp[0].attrib;
		}
    break;

  case 410:
#line 1344 "./parser.y"
    {
		  yyval.attrib = yyvsp[0].attrib;
		}
    break;

  case 411:
#line 1348 "./parser.y"
    {
		  yyval.attrib = yyvsp[0].attrib;
		}
    break;

  case 412:
#line 1352 "./parser.y"
    {
		  yyval.attrib.text = SN_StrDup("");
		}
    break;

  case 413:
#line 1359 "./parser.y"
    { }
    break;


    }

/* Line 1000 of yacc.c.  */
#line 3650 "parser.c"

  yyvsp -= yylen;
  yyssp -= yylen;


  YY_STACK_PRINT (yyss, yyssp);

  *++yyvsp = yyval;


  /* Now `shift' the result of the reduction.  Determine what state
     that goes to, based on the state we popped back to and the rule
     number reduced by.  */

  yyn = yyr1[yyn];

  yystate = yypgoto[yyn - YYNTOKENS] + *yyssp;
  if (0 <= yystate && yystate <= YYLAST && yycheck[yystate] == *yyssp)
    yystate = yytable[yystate];
  else
    yystate = yydefgoto[yyn - YYNTOKENS];

  goto yynewstate;


/*------------------------------------.
| yyerrlab -- here on detecting error |
`------------------------------------*/
yyerrlab:
  /* If not already recovering from an error, report this error.  */
  if (!yyerrstatus)
    {
      ++yynerrs;
#if YYERROR_VERBOSE
      yyn = yypact[yystate];

      if (YYPACT_NINF < yyn && yyn < YYLAST)
	{
	  YYSIZE_T yysize = 0;
	  int yytype = YYTRANSLATE (yychar);
	  const char* yyprefix;
	  char *yymsg;
	  int yyx;

	  /* Start YYX at -YYN if negative to avoid negative indexes in
	     YYCHECK.  */
	  int yyxbegin = yyn < 0 ? -yyn : 0;

	  /* Stay within bounds of both yycheck and yytname.  */
	  int yychecklim = YYLAST - yyn;
	  int yyxend = yychecklim < YYNTOKENS ? yychecklim : YYNTOKENS;
	  int yycount = 0;

	  yyprefix = ", expecting ";
	  for (yyx = yyxbegin; yyx < yyxend; ++yyx)
	    if (yycheck[yyx + yyn] == yyx && yyx != YYTERROR)
	      {
		yysize += yystrlen (yyprefix) + yystrlen (yytname [yyx]);
		yycount += 1;
		if (yycount == 5)
		  {
		    yysize = 0;
		    break;
		  }
	      }
	  yysize += (sizeof ("syntax error, unexpected ")
		     + yystrlen (yytname[yytype]));
	  yymsg = (char *) YYSTACK_ALLOC (yysize);
	  if (yymsg != 0)
	    {
	      char *yyp = yystpcpy (yymsg, "syntax error, unexpected ");
	      yyp = yystpcpy (yyp, yytname[yytype]);

	      if (yycount < 5)
		{
		  yyprefix = ", expecting ";
		  for (yyx = yyxbegin; yyx < yyxend; ++yyx)
		    if (yycheck[yyx + yyn] == yyx && yyx != YYTERROR)
		      {
			yyp = yystpcpy (yyp, yyprefix);
			yyp = yystpcpy (yyp, yytname[yyx]);
			yyprefix = " or ";
		      }
		}
	      yyerror (yymsg);
	      YYSTACK_FREE (yymsg);
	    }
	  else
	    yyerror ("syntax error; also virtual memory exhausted");
	}
      else
#endif /* YYERROR_VERBOSE */
	yyerror ("syntax error");
    }



  if (yyerrstatus == 3)
    {
      /* If just tried and failed to reuse lookahead token after an
	 error, discard it.  */

      if (yychar <= YYEOF)
        {
          /* If at end of input, pop the error token,
	     then the rest of the stack, then return failure.  */
	  if (yychar == YYEOF)
	     for (;;)
	       {
		 YYPOPSTACK;
		 if (yyssp == yyss)
		   YYABORT;
		 YYDSYMPRINTF ("Error: popping", yystos[*yyssp], yyvsp, yylsp);
		 yydestruct (yystos[*yyssp], yyvsp);
	       }
        }
      else
	{
	  YYDSYMPRINTF ("Error: discarding", yytoken, &yylval, &yylloc);
	  yydestruct (yytoken, &yylval);
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

#ifdef __GNUC__
  /* Pacify GCC when the user code never invokes YYERROR and the label
     yyerrorlab therefore never appears in user code.  */
  if (0)
     goto yyerrorlab;
#endif

  yyvsp -= yylen;
  yyssp -= yylen;
  yystate = *yyssp;
  goto yyerrlab1;


/*-------------------------------------------------------------.
| yyerrlab1 -- common code for both syntax error and YYERROR.  |
`-------------------------------------------------------------*/
yyerrlab1:
  yyerrstatus = 3;	/* Each real token shifted decrements this.  */

  for (;;)
    {
      yyn = yypact[yystate];
      if (yyn != YYPACT_NINF)
	{
	  yyn += YYTERROR;
	  if (0 <= yyn && yyn <= YYLAST && yycheck[yyn] == YYTERROR)
	    {
	      yyn = yytable[yyn];
	      if (0 < yyn)
		break;
	    }
	}

      /* Pop the current state because it cannot handle the error token.  */
      if (yyssp == yyss)
	YYABORT;

      YYDSYMPRINTF ("Error: popping", yystos[*yyssp], yyvsp, yylsp);
      yydestruct (yystos[yystate], yyvsp);
      YYPOPSTACK;
      yystate = *yyssp;
      YY_STACK_PRINT (yyss, yyssp);
    }

  if (yyn == YYFINAL)
    YYACCEPT;

  YYDPRINTF ((stderr, "Shifting error token, "));

  *++yyvsp = yylval;


  yystate = yyn;
  goto yynewstate;


/*-------------------------------------.
| yyacceptlab -- YYACCEPT comes here.  |
`-------------------------------------*/
yyacceptlab:
  yyresult = 0;
  goto yyreturn;

/*-----------------------------------.
| yyabortlab -- YYABORT comes here.  |
`-----------------------------------*/
yyabortlab:
  yyresult = 1;
  goto yyreturn;

#ifndef yyoverflow
/*----------------------------------------------.
| yyoverflowlab -- parser overflow comes here.  |
`----------------------------------------------*/
yyoverflowlab:
  yyerror ("parser stack overflow");
  yyresult = 2;
  /* Fall through.  */
#endif

yyreturn:
#ifndef yyoverflow
  if (yyss != yyssa)
    YYSTACK_FREE (yyss);
#endif
  return yyresult;
}


#line 1363 "./parser.y"



