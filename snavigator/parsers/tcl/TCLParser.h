/*
 * TCLParser: P a r s e r  H e a d e r 
 *
 * Generated from: tcl.g
 *
 * Terence Parr, Russell Quong, Will Cohen, and Hank Dietz: 1989-2001
 * Parr Research Corporation
 * with Purdue University Electrical Engineering
 * with AHPCRC, University of Minnesota
 * ANTLR Version 1.33MR33
 */

#ifndef TCLParser_h
#define TCLParser_h

#ifndef ANTLR_VERSION
#define ANTLR_VERSION 13333
#endif

#include "AParser.h"


#include <charbuf.h>

#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <fcntl.h>

#include "sntoolkit.h"
#undef panic
#define DEBUG	1

#define ZZCOL
//#include "DLexerBase.h"
// Base info for DLG-generated scanner
//#include "DLGLexer.h"
// The scanner generated by DLG from this file
//#include "AToken.h"
//typedef ANTLRCommonToken ANTLRToken;

#include "myToken.h"
#include "myPanic.h"
#include "myTokenBuffer.h"
#include "DLGLexer.h"

extern void reset_comment_level();
extern void incr_comment_level();
extern void decr_comment_level();
extern int get_comment_level();

  
class TCLParser : public ANTLRParser {
public:
	static  const ANTLRChar *tokenName(int tk);
	enum { SET_SIZE = 125 };
protected:
	static const ANTLRChar *_token_tbl[];
private:


public:
int Attri;

  int getAttri() {
  return Attri;
}

  void setAttri(int attri) {
  Attri = attri;
}

// We can turn off Syntax error reporting by overriding
// the syn method:
/*  void syn(_ANTLRTokenPtr tok, ANTLRChar *egroup,\
SetWordType *eset, ANTLRTokenType etok, int k){
  // Just ignore syntax errors for now
};
*/

  
protected:
	static SetWordType setwd1[125];
	static SetWordType IDENT_set[16];
	static SetWordType IDENT_errset[16];
	static SetWordType LVALUE_set[16];
	static SetWordType LVALUE_errset[16];
	static SetWordType setwd2[125];
	static SetWordType setwd3[125];
	static SetWordType setwd4[125];
	static SetWordType setwd5[125];
	static SetWordType setwd6[125];
	static SetWordType setwd7[125];
	static SetWordType NUMBER_set[16];
	static SetWordType NUMBER_errset[16];
	static SetWordType setwd8[125];
	static SetWordType setwd9[125];
	static SetWordType setwd10[125];
	static SetWordType setwd11[125];
	static SetWordType setwd12[125];
	static SetWordType setwd13[125];
	static SetWordType setwd14[125];
	static SetWordType setwd15[125];
	static SetWordType P_OPERATORS_set[16];
	static SetWordType P_OPERATORS_errset[16];
	static SetWordType setwd16[125];
	static SetWordType setwd17[125];
	static SetWordType setwd18[125];
	static SetWordType setwd19[125];
	static SetWordType setwd20[125];
	static SetWordType setwd21[125];
	static SetWordType setwd22[125];
	static SetWordType setwd23[125];
private:
	void zzdflthandlers( int _signal, int *_retsignal );

public:

struct _rv3 {
	int line;
	int column;
};

struct _rv13 {
	int line;
	int column;
};

struct _rv15 {
	int line;
	int column;
};

struct _rv37 {
	char * ident;
	char * qual;
	int start;
	int end;
	int line;
};
	TCLParser(ANTLRTokenBuffer *input);
	void program(int *_retsignal);
	void stmt(int *_retsignal);
	struct _rv3 stmt_block(int *_retsignal);
	void proc_stmt(int *_retsignal,char * scope);
	void proc_stmt2(int *_retsignal);
	void body_stmt(int *_retsignal);
	void parameters_declr(int *_retsignal,SN_Symbol * symbolData);
	void parameter_declr(int *_retsignal,SN_Symbol * symbolData);
	void parameters_declr2(int *_retsignal);
	void parameter_declr2(int *_retsignal);
	void proc_call(int *_retsignal);
	void class_stmt(int *_retsignal);
	struct _rv13 itcl30_declr(int *_retsignal,char * scope);
	void itcl30_class_stmts(int *_retsignal,char * scope);
	struct _rv15 itcl15_declr(int *_retsignal,char * scope);
	void itcl15_class_stmts(int *_retsignal,char * scope);
	void inherit_stmt(int *_retsignal,char * scope);
	void constructor_stmt(int *_retsignal,char * scope);
	void destructor_stmt(int *_retsignal,char * scope);
	void method15_stmt(int *_retsignal,char * scope);
	void method30_stmt(int *_retsignal,char * scope);
	void variable30_stmt(int *_retsignal,char * scope);
	void public15_stmt(int *_retsignal,char * scope);
	void protected15_stmt(int *_retsignal,char * scope);
	void secure30_stmt(int *_retsignal,char * scope);
	void itcl30_secure_cmd(int *_retsignal,char * scope);
	void namespace_stmt(int *_retsignal);
	void namespace_eval_stmt(int *_retsignal);
	void eval_stmt(int *_retsignal);
	void common_stmt(int *_retsignal,char * scope);
	void expression(int *_retsignal);
	void l_expression(int *_retsignal);
	void embedded_command(int *_retsignal);
	void var_ref(int *_retsignal);
	void tcl_list(int *_retsignal);
	void array_index(int *_retsignal);
	struct _rv37 qual_ident(int *_retsignal);
	void other_stmts(int *_retsignal);
	void rest_of_line(int *_retsignal);
	void set_stmt(int *_retsignal);
	void unset_stmt(int *_retsignal);
	void bind_stmt(int *_retsignal);
	void source_stmt(int *_retsignal);
	void lappend_stmt(int *_retsignal);
	void llength_stmt(int *_retsignal);
	void linsert_stmt(int *_retsignal);
	void list_stmt(int *_retsignal);
	void info_stmt(int *_retsignal);
	void uplevel_stmt(int *_retsignal);
	void global_stmt(int *_retsignal);
	void return_stmt(int *_retsignal);
	void puts_stmt(int *_retsignal);
	void comment_stmt(int *_retsignal);
	void catch_stmt(int *_retsignal);
	void if_stmt(int *_retsignal);
	void cond_expression(int *_retsignal);
	void cond_expression2(int *_retsignal);
	void sub_cond_expression(int *_retsignal);
	void elseif_stmt(int *_retsignal);
	void else_stmt(int *_retsignal);
	void switch_stmt(int *_retsignal);
	void switch_case(int *_retsignal);
	void switch_body2(int *_retsignal);
	void foreach_stmt(int *_retsignal);
	void for_stmt(int *_retsignal);
	void while_stmt(int *_retsignal);
	void expr_stmt(int *_retsignal);
	void sub_expression(int *_retsignal);
	void math_expression(int *_retsignal);
	void math_function(int *_retsignal);
	void sub_expression2(int *_retsignal);
	void math_expression2(int *_retsignal);
	void math_function2(int *_retsignal);
	static SetWordType ITCL30_CLASS_RESYNC_set[16];
};

#endif /* TCLParser_h */
