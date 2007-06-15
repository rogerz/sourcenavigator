
/*  A Bison parser, made from ./syn-rules.y
    by GNU Bison version 1.28  */

#define YYBISON 1  /* Identify Bison output.  */

#define yyparse syn_parse
#define yylex syn_lex
#define yyerror syn_error
#define yylval syn_lval
#define yychar syn_char
#define yydebug syn_debug
#define yynerrs syn_nerrs
#define	S_ALWAYS	257
#define	S_ASSIGN	258
#define	S_ASSIGN_MEM	259
#define	S_ASSIGN_MUX	260
#define	S_ELSE	261
#define	S_EVENT	262
#define	S_EXPR	263
#define	S_IF	264
#define	S_INITIAL	265

#line 2 "./syn-rules.y"

/*
 * Copyright (c) 2000 Stephen Williams (steve@icarus.com)
 *
 *    This source code is free software; you can redistribute it
 *    and/or modify it in source code form under the terms of the GNU
 *    General Public License as published by the Free Software
 *    Foundation; either version 2 of the License, or (at your option)
 *    any later version.
 *
 *    This program is distributed in the hope that it will be useful,
 *    but WITHOUT ANY WARRANTY; without even the implied warranty of
 *    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *    GNU General Public License for more details.
 *
 *    You should have received a copy of the GNU General Public License
 *    along with this program; if not, write to the Free Software
 *    Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA
 */
#if !defined(WINNT) && !defined(macintosh)
#ident "$Id: syn-rules.cc,v 1.1 2000/12/21 21:57:13 jrandrews Exp $"
#endif

/*
 * This file implements synthesys based on matching threads and
 * converting them to equivilent devices. The trick here is that the
 * proc_match_t functor can be used to scan a process and generate a
 * string of tokens. That string of tokens can then be matched by the
 * rules to determin what kind of device is to be made.
 */

# include  "netlist.h"
# include  "netmisc.h"
# include  "functor.h"
# include  <assert.h>

struct syn_token_t {
      int token;

      NetAssign_*assign;
      NetAssignMem_*assign_mem;
      NetProcTop*top;
      NetEvWait*evwait;
      NetEvent*event;
      NetExpr*expr;

      syn_token_t*next_;
};
#define YYSTYPE syn_token_t*

static int yylex();
static void yyerror(const char*);
static Design*des_;

static void make_DFF_CE(Design*des, NetProcTop*top, NetEvWait*wclk,
			NetEvent*eclk, NetExpr*cexp, NetAssign_*asn);
static void make_RAM_CE(Design*des, NetProcTop*top, NetEvWait*wclk,
			NetEvent*eclk, NetExpr*cexp, NetAssignMem_*asn);
static void make_initializer(Design*des, NetProcTop*top, NetAssign_*asn);

#ifndef YYSTYPE
#define YYSTYPE int
#endif
#include <stdio.h>

#ifndef __cplusplus
#ifndef __STDC__
#define const
#endif
#endif



#define	YYFINAL		22
#define	YYFLAG		-32768
#define	YYNTBASE	16

#define YYTRANSLATE(x) ((unsigned)(x) <= 265 ? yytranslate[x] : 17)

static const char yytranslate[] = {     0,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,    13,
    14,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,    15,     2,
     2,     2,     2,    12,     2,     2,     2,     2,     2,     2,
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
     2,     2,     2,     2,     2,     1,     3,     4,     5,     6,
     7,     8,     9,    10,    11
};

#if YYDEBUG != 0
static const short yyprhs[] = {     0,
     0,     8,    19,    22,    30
};

static const short yyrhs[] = {     3,
    12,    13,     8,    14,     4,    15,     0,     3,    12,    13,
     8,    14,    10,     9,     4,    15,    15,     0,    11,     4,
     0,     3,    12,    13,     8,    14,     5,    15,     0,     3,
    12,    13,     8,    14,    10,     9,     5,    15,    15,     0
};

#endif

#if YYDEBUG != 0
static const short yyrline[] = { 0,
    85,    90,    98,   117,   122
};
#endif


#if YYDEBUG != 0 || defined (YYERROR_VERBOSE)

static const char * const yytname[] = {   "$","error","$undefined.","S_ALWAYS",
"S_ASSIGN","S_ASSIGN_MEM","S_ASSIGN_MUX","S_ELSE","S_EVENT","S_EXPR","S_IF",
"S_INITIAL","'@'","'('","')'","';'","start", NULL
};
#endif

static const short yyr1[] = {     0,
    16,    16,    16,    16,    16
};

static const short yyr2[] = {     0,
     7,    10,     2,     7,    10
};

static const short yydefact[] = {     0,
     0,     0,     0,     3,     0,     0,     0,     0,     0,     0,
     1,     4,     0,     0,     0,     0,     0,     2,     5,     0,
     0,     0
};

static const short yydefgoto[] = {    20
};

static const short yypact[] = {    -3,
   -11,    -2,    -6,-32768,     2,     0,    -1,    -4,     3,     4,
-32768,-32768,     1,     5,     6,     7,     8,-32768,-32768,    12,
    15,-32768
};

static const short yypgoto[] = {-32768
};


#define	YYLAST		23


static const short yytable[] = {     1,
     3,     4,     8,     9,    14,    15,     5,     2,    10,     6,
    11,    21,    13,     7,    22,     0,     0,    12,     0,    16,
    17,    18,    19
};

static const short yycheck[] = {     3,
    12,     4,     4,     5,     4,     5,    13,    11,    10,     8,
    15,     0,     9,    14,     0,    -1,    -1,    15,    -1,    15,
    15,    15,    15
};
/* -*-C-*-  Note some compilers choke on comments on `#line' lines.  */
#line 3 "/usr/lib/bison.simple"
/* This file comes from bison-1.28.  */

/* Skeleton output parser for bison,
   Copyright (C) 1984, 1989, 1990 Free Software Foundation, Inc.

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

/* This is the parser code that is written into each bison parser
  when the %semantic_parser declaration is not specified in the grammar.
  It was written by Richard Stallman by simplifying the hairy parser
  used when %semantic_parser is specified.  */

#ifndef YYSTACK_USE_ALLOCA
#ifdef alloca
#define YYSTACK_USE_ALLOCA
#else /* alloca not defined */
#ifdef __GNUC__
#define YYSTACK_USE_ALLOCA
#define alloca __builtin_alloca
#else /* not GNU C.  */
#if (!defined (__STDC__) && defined (sparc)) || defined (__sparc__) || defined (__sparc) || defined (__sgi) || (defined (__sun) && defined (__i386))
#define YYSTACK_USE_ALLOCA
#include <alloca.h>
#else /* not sparc */
/* We think this test detects Watcom and Microsoft C.  */
/* This used to test MSDOS, but that is a bad idea
   since that symbol is in the user namespace.  */
#if (defined (_MSDOS) || defined (_MSDOS_)) && !defined (__TURBOC__)
#if 0 /* No need for malloc.h, which pollutes the namespace;
	 instead, just don't use alloca.  */
#include <malloc.h>
#endif
#else /* not MSDOS, or __TURBOC__ */
#if defined(_AIX)
/* I don't know what this was needed for, but it pollutes the namespace.
   So I turned it off.   rms, 2 May 1997.  */
/* #include <malloc.h>  */
 #pragma alloca
#define YYSTACK_USE_ALLOCA
#else /* not MSDOS, or __TURBOC__, or _AIX */
#if 0
#ifdef __hpux /* haible@ilog.fr says this works for HPUX 9.05 and up,
		 and on HPUX 10.  Eventually we can turn this on.  */
#define YYSTACK_USE_ALLOCA
#define alloca __builtin_alloca
#endif /* __hpux */
#endif
#endif /* not _AIX */
#endif /* not MSDOS, or __TURBOC__ */
#endif /* not sparc */
#endif /* not GNU C */
#endif /* alloca not defined */
#endif /* YYSTACK_USE_ALLOCA not defined */

#ifdef YYSTACK_USE_ALLOCA
#define YYSTACK_ALLOC alloca
#else
#define YYSTACK_ALLOC malloc
#endif

/* Note: there must be only one dollar sign in this file.
   It is replaced by the list of actions, each action
   as one case of the switch.  */

#define yyerrok		(yyerrstatus = 0)
#define yyclearin	(yychar = YYEMPTY)
#define YYEMPTY		-2
#define YYEOF		0
#define YYACCEPT	goto yyacceptlab
#define YYABORT 	goto yyabortlab
#define YYERROR		goto yyerrlab1
/* Like YYERROR except do call yyerror.
   This remains here temporarily to ease the
   transition to the new meaning of YYERROR, for GCC.
   Once GCC version 2 has supplanted version 1, this can go.  */
#define YYFAIL		goto yyerrlab
#define YYRECOVERING()  (!!yyerrstatus)
#define YYBACKUP(token, value) \
do								\
  if (yychar == YYEMPTY && yylen == 1)				\
    { yychar = (token), yylval = (value);			\
      yychar1 = YYTRANSLATE (yychar);				\
      YYPOPSTACK;						\
      goto yybackup;						\
    }								\
  else								\
    { yyerror ("syntax error: cannot back up"); YYERROR; }	\
while (0)

#define YYTERROR	1
#define YYERRCODE	256

#ifndef YYPURE
#define YYLEX		yylex()
#endif

#ifdef YYPURE
#ifdef YYLSP_NEEDED
#ifdef YYLEX_PARAM
#define YYLEX		yylex(&yylval, &yylloc, YYLEX_PARAM)
#else
#define YYLEX		yylex(&yylval, &yylloc)
#endif
#else /* not YYLSP_NEEDED */
#ifdef YYLEX_PARAM
#define YYLEX		yylex(&yylval, YYLEX_PARAM)
#else
#define YYLEX		yylex(&yylval)
#endif
#endif /* not YYLSP_NEEDED */
#endif

/* If nonreentrant, generate the variables here */

#ifndef YYPURE

int	yychar;			/*  the lookahead symbol		*/
YYSTYPE	yylval;			/*  the semantic value of the		*/
				/*  lookahead symbol			*/

#ifdef YYLSP_NEEDED
YYLTYPE yylloc;			/*  location data for the lookahead	*/
				/*  symbol				*/
#endif

int yynerrs;			/*  number of parse errors so far       */
#endif  /* not YYPURE */

#if YYDEBUG != 0
int yydebug;			/*  nonzero means print parse trace	*/
/* Since this is uninitialized, it does not stop multiple parsers
   from coexisting.  */
#endif

/*  YYINITDEPTH indicates the initial size of the parser's stacks	*/

#ifndef	YYINITDEPTH
#define YYINITDEPTH 200
#endif

/*  YYMAXDEPTH is the maximum size the stacks can grow to
    (effective only if the built-in stack extension method is used).  */

#if YYMAXDEPTH == 0
#undef YYMAXDEPTH
#endif

#ifndef YYMAXDEPTH
#define YYMAXDEPTH 10000
#endif

/* Define __yy_memcpy.  Note that the size argument
   should be passed with type unsigned int, because that is what the non-GCC
   definitions require.  With GCC, __builtin_memcpy takes an arg
   of type size_t, but it can handle unsigned int.  */

#if __GNUC__ > 1		/* GNU C and GNU C++ define this.  */
#define __yy_memcpy(TO,FROM,COUNT)	__builtin_memcpy(TO,FROM,COUNT)
#else				/* not GNU C or C++ */
#ifndef __cplusplus

/* This is the most reliable way to avoid incompatibilities
   in available built-in functions on various systems.  */
static void
__yy_memcpy (to, from, count)
     char *to;
     char *from;
     unsigned int count;
{
  register char *f = from;
  register char *t = to;
  register int i = count;

  while (i-- > 0)
    *t++ = *f++;
}

#else /* __cplusplus */

/* This is the most reliable way to avoid incompatibilities
   in available built-in functions on various systems.  */
static void
__yy_memcpy (char *to, char *from, unsigned int count)
{
  register char *t = to;
  register char *f = from;
  register int i = count;

  while (i-- > 0)
    *t++ = *f++;
}

#endif
#endif

#line 217 "/usr/lib/bison.simple"

/* The user can define YYPARSE_PARAM as the name of an argument to be passed
   into yyparse.  The argument should have type void *.
   It should actually point to an object.
   Grammar actions can access the variable by casting it
   to the proper pointer type.  */

#ifdef YYPARSE_PARAM
#ifdef __cplusplus
#define YYPARSE_PARAM_ARG void *YYPARSE_PARAM
#define YYPARSE_PARAM_DECL
#else /* not __cplusplus */
#define YYPARSE_PARAM_ARG YYPARSE_PARAM
#define YYPARSE_PARAM_DECL void *YYPARSE_PARAM;
#endif /* not __cplusplus */
#else /* not YYPARSE_PARAM */
#define YYPARSE_PARAM_ARG
#define YYPARSE_PARAM_DECL
#endif /* not YYPARSE_PARAM */

/* Prevent warning if -Wstrict-prototypes.  */
#ifdef __GNUC__
#ifdef YYPARSE_PARAM
int yyparse (void *);
#else
int yyparse (void);
#endif
#endif

int
yyparse(YYPARSE_PARAM_ARG)
     YYPARSE_PARAM_DECL
{
  register int yystate;
  register int yyn;
  register short *yyssp;
  register YYSTYPE *yyvsp;
  int yyerrstatus;	/*  number of tokens to shift before error messages enabled */
  int yychar1 = 0;		/*  lookahead token as an internal (translated) token number */

  short	yyssa[YYINITDEPTH];	/*  the state stack			*/
  YYSTYPE yyvsa[YYINITDEPTH];	/*  the semantic value stack		*/

  short *yyss = yyssa;		/*  refer to the stacks thru separate pointers */
  YYSTYPE *yyvs = yyvsa;	/*  to allow yyoverflow to reallocate them elsewhere */

#ifdef YYLSP_NEEDED
  YYLTYPE yylsa[YYINITDEPTH];	/*  the location stack			*/
  YYLTYPE *yyls = yylsa;
  YYLTYPE *yylsp;

#define YYPOPSTACK   (yyvsp--, yyssp--, yylsp--)
#else
#define YYPOPSTACK   (yyvsp--, yyssp--)
#endif

  int yystacksize = YYINITDEPTH;
  int yyfree_stacks = 0;

#ifdef YYPURE
  int yychar;
  YYSTYPE yylval;
  int yynerrs;
#ifdef YYLSP_NEEDED
  YYLTYPE yylloc;
#endif
#endif

  YYSTYPE yyval;		/*  the variable used to return		*/
				/*  semantic values from the action	*/
				/*  routines				*/

  int yylen;

#if YYDEBUG != 0
  if (yydebug)
    fprintf(stderr, "Starting parse\n");
#endif

  yystate = 0;
  yyerrstatus = 0;
  yynerrs = 0;
  yychar = YYEMPTY;		/* Cause a token to be read.  */

  /* Initialize stack pointers.
     Waste one element of value and location stack
     so that they stay on the same level as the state stack.
     The wasted elements are never initialized.  */

  yyssp = yyss - 1;
  yyvsp = yyvs;
#ifdef YYLSP_NEEDED
  yylsp = yyls;
#endif

/* Push a new state, which is found in  yystate  .  */
/* In all cases, when you get here, the value and location stacks
   have just been pushed. so pushing a state here evens the stacks.  */
yynewstate:

  *++yyssp = yystate;

  if (yyssp >= yyss + yystacksize - 1)
    {
      /* Give user a chance to reallocate the stack */
      /* Use copies of these so that the &'s don't force the real ones into memory. */
      YYSTYPE *yyvs1 = yyvs;
      short *yyss1 = yyss;
#ifdef YYLSP_NEEDED
      YYLTYPE *yyls1 = yyls;
#endif

      /* Get the current used size of the three stacks, in elements.  */
      int size = yyssp - yyss + 1;

#ifdef yyoverflow
      /* Each stack pointer address is followed by the size of
	 the data in use in that stack, in bytes.  */
#ifdef YYLSP_NEEDED
      /* This used to be a conditional around just the two extra args,
	 but that might be undefined if yyoverflow is a macro.  */
      yyoverflow("parser stack overflow",
		 &yyss1, size * sizeof (*yyssp),
		 &yyvs1, size * sizeof (*yyvsp),
		 &yyls1, size * sizeof (*yylsp),
		 &yystacksize);
#else
      yyoverflow("parser stack overflow",
		 &yyss1, size * sizeof (*yyssp),
		 &yyvs1, size * sizeof (*yyvsp),
		 &yystacksize);
#endif

      yyss = yyss1; yyvs = yyvs1;
#ifdef YYLSP_NEEDED
      yyls = yyls1;
#endif
#else /* no yyoverflow */
      /* Extend the stack our own way.  */
      if (yystacksize >= YYMAXDEPTH)
	{
	  yyerror("parser stack overflow");
	  if (yyfree_stacks)
	    {
	      free (yyss);
	      free (yyvs);
#ifdef YYLSP_NEEDED
	      free (yyls);
#endif
	    }
	  return 2;
	}
      yystacksize *= 2;
      if (yystacksize > YYMAXDEPTH)
	yystacksize = YYMAXDEPTH;
#ifndef YYSTACK_USE_ALLOCA
      yyfree_stacks = 1;
#endif
      yyss = (short *) YYSTACK_ALLOC (yystacksize * sizeof (*yyssp));
      __yy_memcpy ((char *)yyss, (char *)yyss1,
		   size * (unsigned int) sizeof (*yyssp));
      yyvs = (YYSTYPE *) YYSTACK_ALLOC (yystacksize * sizeof (*yyvsp));
      __yy_memcpy ((char *)yyvs, (char *)yyvs1,
		   size * (unsigned int) sizeof (*yyvsp));
#ifdef YYLSP_NEEDED
      yyls = (YYLTYPE *) YYSTACK_ALLOC (yystacksize * sizeof (*yylsp));
      __yy_memcpy ((char *)yyls, (char *)yyls1,
		   size * (unsigned int) sizeof (*yylsp));
#endif
#endif /* no yyoverflow */

      yyssp = yyss + size - 1;
      yyvsp = yyvs + size - 1;
#ifdef YYLSP_NEEDED
      yylsp = yyls + size - 1;
#endif

#if YYDEBUG != 0
      if (yydebug)
	fprintf(stderr, "Stack size increased to %d\n", yystacksize);
#endif

      if (yyssp >= yyss + yystacksize - 1)
	YYABORT;
    }

#if YYDEBUG != 0
  if (yydebug)
    fprintf(stderr, "Entering state %d\n", yystate);
#endif

  goto yybackup;
 yybackup:

/* Do appropriate processing given the current state.  */
/* Read a lookahead token if we need one and don't already have one.  */
/* yyresume: */

  /* First try to decide what to do without reference to lookahead token.  */

  yyn = yypact[yystate];
  if (yyn == YYFLAG)
    goto yydefault;

  /* Not known => get a lookahead token if don't already have one.  */

  /* yychar is either YYEMPTY or YYEOF
     or a valid token in external form.  */

  if (yychar == YYEMPTY)
    {
#if YYDEBUG != 0
      if (yydebug)
	fprintf(stderr, "Reading a token: ");
#endif
      yychar = YYLEX;
    }

  /* Convert token to internal form (in yychar1) for indexing tables with */

  if (yychar <= 0)		/* This means end of input. */
    {
      yychar1 = 0;
      yychar = YYEOF;		/* Don't call YYLEX any more */

#if YYDEBUG != 0
      if (yydebug)
	fprintf(stderr, "Now at end of input.\n");
#endif
    }
  else
    {
      yychar1 = YYTRANSLATE(yychar);

#if YYDEBUG != 0
      if (yydebug)
	{
	  fprintf (stderr, "Next token is %d (%s", yychar, yytname[yychar1]);
	  /* Give the individual parser a way to print the precise meaning
	     of a token, for further debugging info.  */
#ifdef YYPRINT
	  YYPRINT (stderr, yychar, yylval);
#endif
	  fprintf (stderr, ")\n");
	}
#endif
    }

  yyn += yychar1;
  if (yyn < 0 || yyn > YYLAST || yycheck[yyn] != yychar1)
    goto yydefault;

  yyn = yytable[yyn];

  /* yyn is what to do for this token type in this state.
     Negative => reduce, -yyn is rule number.
     Positive => shift, yyn is new state.
       New state is final state => don't bother to shift,
       just return success.
     0, or most negative number => error.  */

  if (yyn < 0)
    {
      if (yyn == YYFLAG)
	goto yyerrlab;
      yyn = -yyn;
      goto yyreduce;
    }
  else if (yyn == 0)
    goto yyerrlab;

  if (yyn == YYFINAL)
    YYACCEPT;

  /* Shift the lookahead token.  */

#if YYDEBUG != 0
  if (yydebug)
    fprintf(stderr, "Shifting token %d (%s), ", yychar, yytname[yychar1]);
#endif

  /* Discard the token being shifted unless it is eof.  */
  if (yychar != YYEOF)
    yychar = YYEMPTY;

  *++yyvsp = yylval;
#ifdef YYLSP_NEEDED
  *++yylsp = yylloc;
#endif

  /* count tokens shifted since error; after three, turn off error status.  */
  if (yyerrstatus) yyerrstatus--;

  yystate = yyn;
  goto yynewstate;

/* Do the default action for the current state.  */
yydefault:

  yyn = yydefact[yystate];
  if (yyn == 0)
    goto yyerrlab;

/* Do a reduction.  yyn is the number of a rule to reduce with.  */
yyreduce:
  yylen = yyr2[yyn];
  if (yylen > 0)
    yyval = yyvsp[1-yylen]; /* implement default value of the action */

#if YYDEBUG != 0
  if (yydebug)
    {
      int i;

      fprintf (stderr, "Reducing via rule %d (line %d), ",
	       yyn, yyrline[yyn]);

      /* Print the symbols being reduced, and their result.  */
      for (i = yyprhs[yyn]; yyrhs[i] > 0; i++)
	fprintf (stderr, "%s ", yytname[yyrhs[i]]);
      fprintf (stderr, " -> %s\n", yytname[yyr1[yyn]]);
    }
#endif


  switch (yyn) {

case 1:
#line 86 "./syn-rules.y"
{ make_DFF_CE(des_, yyvsp[-6]->top, yyvsp[-5]->evwait, yyvsp[-3]->event,
			      0, yyvsp[-1]->assign);
		;
    break;}
case 2:
#line 91 "./syn-rules.y"
{ make_DFF_CE(des_, yyvsp[-9]->top, yyvsp[-8]->evwait, yyvsp[-6]->event,
			      yyvsp[-3]->expr, yyvsp[-2]->assign);
		;
    break;}
case 3:
#line 99 "./syn-rules.y"
{ make_initializer(des_, yyvsp[-1]->top, yyvsp[0]->assign);
		;
    break;}
case 4:
#line 118 "./syn-rules.y"
{ make_RAM_CE(des_, yyvsp[-6]->top, yyvsp[-5]->evwait, yyvsp[-3]->event,
			      0, yyvsp[-1]->assign_mem);
		;
    break;}
case 5:
#line 123 "./syn-rules.y"
{ make_RAM_CE(des_, yyvsp[-9]->top, yyvsp[-8]->evwait, yyvsp[-6]->event,
			      yyvsp[-3]->expr, yyvsp[-2]->assign_mem);
		;
    break;}
}
   /* the action file gets copied in in place of this dollarsign */
#line 543 "/usr/lib/bison.simple"

  yyvsp -= yylen;
  yyssp -= yylen;
#ifdef YYLSP_NEEDED
  yylsp -= yylen;
#endif

#if YYDEBUG != 0
  if (yydebug)
    {
      short *ssp1 = yyss - 1;
      fprintf (stderr, "state stack now");
      while (ssp1 != yyssp)
	fprintf (stderr, " %d", *++ssp1);
      fprintf (stderr, "\n");
    }
#endif

  *++yyvsp = yyval;

#ifdef YYLSP_NEEDED
  yylsp++;
  if (yylen == 0)
    {
      yylsp->first_line = yylloc.first_line;
      yylsp->first_column = yylloc.first_column;
      yylsp->last_line = (yylsp-1)->last_line;
      yylsp->last_column = (yylsp-1)->last_column;
      yylsp->text = 0;
    }
  else
    {
      yylsp->last_line = (yylsp+yylen-1)->last_line;
      yylsp->last_column = (yylsp+yylen-1)->last_column;
    }
#endif

  /* Now "shift" the result of the reduction.
     Determine what state that goes to,
     based on the state we popped back to
     and the rule number reduced by.  */

  yyn = yyr1[yyn];

  yystate = yypgoto[yyn - YYNTBASE] + *yyssp;
  if (yystate >= 0 && yystate <= YYLAST && yycheck[yystate] == *yyssp)
    yystate = yytable[yystate];
  else
    yystate = yydefgoto[yyn - YYNTBASE];

  goto yynewstate;

yyerrlab:   /* here on detecting error */

  if (! yyerrstatus)
    /* If not already recovering from an error, report this error.  */
    {
      ++yynerrs;

#ifdef YYERROR_VERBOSE
      yyn = yypact[yystate];

      if (yyn > YYFLAG && yyn < YYLAST)
	{
	  int size = 0;
	  char *msg;
	  int x, count;

	  count = 0;
	  /* Start X at -yyn if nec to avoid negative indexes in yycheck.  */
	  for (x = (yyn < 0 ? -yyn : 0);
	       x < (sizeof(yytname) / sizeof(char *)); x++)
	    if (yycheck[x + yyn] == x)
	      size += strlen(yytname[x]) + 15, count++;
	  msg = (char *) malloc(size + 15);
	  if (msg != 0)
	    {
	      strcpy(msg, "parse error");

	      if (count < 5)
		{
		  count = 0;
		  for (x = (yyn < 0 ? -yyn : 0);
		       x < (sizeof(yytname) / sizeof(char *)); x++)
		    if (yycheck[x + yyn] == x)
		      {
			strcat(msg, count == 0 ? ", expecting `" : " or `");
			strcat(msg, yytname[x]);
			strcat(msg, "'");
			count++;
		      }
		}
	      yyerror(msg);
	      free(msg);
	    }
	  else
	    yyerror ("parse error; also virtual memory exceeded");
	}
      else
#endif /* YYERROR_VERBOSE */
	yyerror("parse error");
    }

  goto yyerrlab1;
yyerrlab1:   /* here on error raised explicitly by an action */

  if (yyerrstatus == 3)
    {
      /* if just tried and failed to reuse lookahead token after an error, discard it.  */

      /* return failure if at end of input */
      if (yychar == YYEOF)
	YYABORT;

#if YYDEBUG != 0
      if (yydebug)
	fprintf(stderr, "Discarding token %d (%s).\n", yychar, yytname[yychar1]);
#endif

      yychar = YYEMPTY;
    }

  /* Else will try to reuse lookahead token
     after shifting the error token.  */

  yyerrstatus = 3;		/* Each real token shifted decrements this */

  goto yyerrhandle;

yyerrdefault:  /* current state does not do anything special for the error token. */

#if 0
  /* This is wrong; only states that explicitly want error tokens
     should shift them.  */
  yyn = yydefact[yystate];  /* If its default is to accept any token, ok.  Otherwise pop it.*/
  if (yyn) goto yydefault;
#endif

yyerrpop:   /* pop the current state because it cannot handle the error token */

  if (yyssp == yyss) YYABORT;
  yyvsp--;
  yystate = *--yyssp;
#ifdef YYLSP_NEEDED
  yylsp--;
#endif

#if YYDEBUG != 0
  if (yydebug)
    {
      short *ssp1 = yyss - 1;
      fprintf (stderr, "Error: state stack now");
      while (ssp1 != yyssp)
	fprintf (stderr, " %d", *++ssp1);
      fprintf (stderr, "\n");
    }
#endif

yyerrhandle:

  yyn = yypact[yystate];
  if (yyn == YYFLAG)
    goto yyerrdefault;

  yyn += YYTERROR;
  if (yyn < 0 || yyn > YYLAST || yycheck[yyn] != YYTERROR)
    goto yyerrdefault;

  yyn = yytable[yyn];
  if (yyn < 0)
    {
      if (yyn == YYFLAG)
	goto yyerrpop;
      yyn = -yyn;
      goto yyreduce;
    }
  else if (yyn == 0)
    goto yyerrpop;

  if (yyn == YYFINAL)
    YYACCEPT;

#if YYDEBUG != 0
  if (yydebug)
    fprintf(stderr, "Shifting error token, ");
#endif

  *++yyvsp = yylval;
#ifdef YYLSP_NEEDED
  *++yylsp = yylloc;
#endif

  yystate = yyn;
  goto yynewstate;

 yyacceptlab:
  /* YYACCEPT comes here.  */
  if (yyfree_stacks)
    {
      free (yyss);
      free (yyvs);
#ifdef YYLSP_NEEDED
      free (yyls);
#endif
    }
  return 0;

 yyabortlab:
  /* YYABORT comes here.  */
  if (yyfree_stacks)
    {
      free (yyss);
      free (yyvs);
#ifdef YYLSP_NEEDED
      free (yyls);
#endif
    }
  return 1;
}
#line 129 "./syn-rules.y"



  /* Various actions. */
static void make_DFF_CE(Design*des, NetProcTop*top, NetEvWait*wclk,
			NetEvent*eclk, NetExpr*cexp, NetAssign_*asn)
{
      NetEvProbe*pclk = eclk->probe(0);
      NetESignal*d = dynamic_cast<NetESignal*> (asn->rval());
      NetNet*ce = cexp? cexp->synthesize(des) : 0;

      assert(d);

      NetFF*ff = new NetFF(asn->name(), asn->pin_count());

      for (unsigned idx = 0 ;  idx < ff->width() ;  idx += 1) {
	    connect(ff->pin_Data(idx), d->pin(idx));
	    connect(ff->pin_Q(idx), asn->pin(idx));
      }

      connect(ff->pin_Clock(), pclk->pin(0));
      if (ce) connect(ff->pin_Enable(), ce->pin(0));

      ff->attribute("LPM_FFType", "DFF");
      if (pclk->edge() == NetEvProbe::NEGEDGE)
	    ff->attribute("Clock:LPM_Polarity", "INVERT");

      des->add_node(ff);
      des->delete_process(top);
}

static void make_RAM_CE(Design*des, NetProcTop*top, NetEvWait*wclk,
			NetEvent*eclk, NetExpr*cexp, NetAssignMem_*asn)
{
      NetMemory*mem = asn->memory();
      NetExpr*adr_e = asn->index();

      NetNet*adr = adr_e->synthesize(des);
      assert(adr);

      NetEvProbe*pclk = eclk->probe(0);
      NetESignal*d = dynamic_cast<NetESignal*> (asn->rval());
      NetNet*ce = cexp? cexp->synthesize(des) : 0;

      assert(d);

      NetRamDq*ram = new NetRamDq(des->local_symbol(mem->name()), mem,
				  adr->pin_count());

      for (unsigned idx = 0 ;  idx < adr->pin_count() ;  idx += 1)
	    connect(adr->pin(idx), ram->pin_Address(idx));

      for (unsigned idx = 0 ;  idx < ram->width() ;  idx += 1)
	    connect(ram->pin_Data(idx), d->pin(idx));

      if (ce) connect(ram->pin_WE(), ce->pin(0));

      assert(pclk->edge() == NetEvProbe::POSEDGE);
      connect(ram->pin_InClock(), pclk->pin(0));

      ram->absorb_partners();

      des->add_node(ram);
      des->delete_process(top);
}

/*
 * An assignment in an initial statement is the same as giving the
 * nexus an initial value. For synthesized netlists, we can just set
 * the initial value for the link and get rid of the assignment
 * process.
 */
static void make_initializer(Design*des, NetProcTop*top, NetAssign_*asn)
{
      NetESignal*rsig = dynamic_cast<NetESignal*> (asn->rval());
      assert(rsig);

      for (unsigned idx = 0 ;  idx < asn->pin_count() ;  idx += 1) {

	    verinum::V bit = driven_value(rsig->pin(idx));

	    Nexus*nex = asn->pin(idx).nexus();
	    for (Link*cur = nex->first_nlink()
		       ;  cur ;  cur = cur->next_nlink()) {

		  if (NetNet*net = dynamic_cast<NetNet*> (cur->get_obj()))
			cur->set_init(bit);

	    }
      }

      des->delete_process(top);
}

static syn_token_t*first_ = 0;
static syn_token_t*last_ = 0;
static syn_token_t*ptr_ = 0;

/*
 * The match class is used to take a process and turn it into a stream
 * of tokens. This stream is used by the yylex function to feed tokens
 * to the parser.
 */
struct tokenize : public proc_match_t {
      tokenize() { }
      ~tokenize() { }

      int assign(NetAssign*dev)
      {
	    syn_token_t*cur;
	    cur = new syn_token_t;
	    cur->token = dev->bmux() ? S_ASSIGN_MUX : S_ASSIGN;
	    cur->assign = dev;
	    cur->next_ = 0;
	    last_->next_ = cur;
	    last_ = cur;
	    return 0;
      }

      int assign_nb(NetAssignNB*dev)
      {
	    syn_token_t*cur;
	    cur = new syn_token_t;
	    cur->token = dev->bmux() ? S_ASSIGN_MUX : S_ASSIGN;
	    cur->assign = dev;
	    cur->next_ = 0;
	    last_->next_ = cur;
	    last_ = cur;
	    return 0;
      }

      int assign_mem(NetAssignMem*dev)
      {
	    syn_token_t*cur;
	    cur = new syn_token_t;
	    cur->token = S_ASSIGN_MEM;
	    cur->assign_mem = dev;
	    cur->next_ = 0;
	    last_->next_ = cur;
	    last_ = cur;
	    return 0;
      }

      int assign_mem_nb(NetAssignMemNB*dev)
      {
	    syn_token_t*cur;
	    cur = new syn_token_t;
	    cur->token = S_ASSIGN_MEM;
	    cur->assign_mem = dev;
	    cur->next_ = 0;
	    last_->next_ = cur;
	    last_ = cur;
	    return 0;
      }

      int condit(NetCondit*dev)
      {
	    syn_token_t*cur;
	    
	    cur = new syn_token_t;
	    cur->token = S_IF;
	    cur->next_ = 0;
	    last_->next_ = cur;
	    last_ = cur;

	    cur = new syn_token_t;
	    cur->token = S_EXPR;
	    cur->expr = dev->expr();
	    cur->next_ = 0;
	    last_->next_ = cur;
	    last_ = cur;

	    dev -> if_clause() -> match_proc(this);

	    if (dev->else_clause()) {
		  cur = new syn_token_t;
		  cur->token = S_ELSE;
		  cur->next_ = 0;
		  last_->next_ = cur;
		  last_ = cur;

		  dev -> else_clause() -> match_proc(this);
	    }

	    cur = new syn_token_t;
	    cur->token = ';';
	    cur->next_ = 0;
	    last_->next_ = cur;
	    last_ = cur;
	    return 0;
      }

      int event_wait(NetEvWait*dev)
      {
	    syn_token_t*cur;
	    
	    cur = new syn_token_t;
	    cur->token = '@';
	    cur->evwait = dev;
	    cur->next_ = 0;
	    last_->next_ = cur;
	    last_ = cur;

	    cur = new syn_token_t;
	    cur->token = '(';
	    cur->next_ = 0;
	    last_->next_ = cur;
	    last_ = cur;

	    for (unsigned idx = 0;  idx < dev->nevents(); idx += 1) {
		  cur = new syn_token_t;
		  cur->token = S_EVENT;
		  cur->event = dev->event(idx);
		  cur->next_ = 0;
		  last_->next_ = cur;
		  last_ = cur;
	    }

	    cur = new syn_token_t;
	    cur->token = ')';
	    cur->next_ = 0;
	    last_->next_ = cur;
	    last_ = cur;

	    dev -> statement() -> match_proc(this);

	    cur = new syn_token_t;
	    cur->token = ';';
	    cur->next_ = 0;
	    last_->next_ = cur;
	    last_ = cur;
	    return 0;
      }
};

static void syn_start_process(NetProcTop*t)
{
      first_ = new syn_token_t;
      last_ = first_;
      ptr_ = first_;

      first_->token = (t->type() == NetProcTop::KALWAYS)? S_ALWAYS : S_INITIAL;
      first_->top = t;
      first_->next_ = 0;

      tokenize go;
      t -> statement() -> match_proc(&go);
}

static void syn_done_process()
{
      while (first_) {
	    syn_token_t*cur = first_;
	    first_ = cur->next_;
	    delete cur;
      }
}

static int yylex()
{
      if (ptr_ == 0) {
	    yylval = 0;
	    return EOF;
      }

      yylval = ptr_;
      ptr_ = ptr_->next_;
      return yylval->token;
}

struct syn_rules_f  : public functor_t {
      ~syn_rules_f() { }

      void process(class Design*des, class NetProcTop*top)
      {
	    syn_start_process(top);
	    yyparse();
	    syn_done_process();
      }
};

void syn_rules(Design*d)
{
      des_ = d;
      syn_rules_f obj;
      des_->functor(&obj);
}

static void yyerror(const char*)
{
}
