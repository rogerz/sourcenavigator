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
/* Line 1231 of yacc.c.  */
#line 286 "parser.h"
# define yystype YYSTYPE /* obsolescent; will be withdrawn */
# define YYSTYPE_IS_DECLARED 1
# define YYSTYPE_IS_TRIVIAL 1
#endif

extern YYSTYPE yylval;



