//
//  colToken.cpp
//

/*  This file should be accompanied by DISCLAIMER.TXT stating disclaimers */

#include "noleakTok.h"
#include "colToken.h"
#include "string.h"
#include "myPanic.h"
#include <stdio.h>
//#include "P.h"

void ColToken::init() {
  pText=0;
}

ColToken::ColToken() {
  init();
  setLine(0);
}

ColToken::~ColToken() {
////   dumpNode("Token Destructor");
   delete [] pText;
   pText=0;
}

ColToken::ColToken(ANTLRTokenType tokenTypeNew) {
   init();
   setType(tokenTypeNew);
   setLine(0);
}

ColToken::ColToken(ANTLRTokenType   tokenTypeNew,
           ANTLRChar *      textNew,   
           int          lineNew) {
   init();
   setType(tokenTypeNew);
   setLine(lineNew);
   setText(textNew);
}         

ColToken::ColToken(const ColToken & from) :
    NoLeakToken(from) {
   init();
   begcol=from.begcol;
   endcol=from.endcol;
   setText(from.pText);
};  

//  create new copy of text - not just another reference to existing text

ColToken & ColToken::operator = (const ColToken & from) {

   this->NoLeakToken::operator = (from);

   if (this != &from) {
     begcol=from.begcol;
     endcol=from.endcol;
     setText(from.pText);
   };
   return *this;
}      

//  create new copy of text - not just another reference to existing text

void ColToken::setText(const ANTLRChar *s) {

   if (pText != 0) {
      delete [] pText;
      pText=0;
   };
   if (s != 0) {
     pText=new char [strlen(s)+1];
     if (pText == 0) myPanic ("ColToken::setText strdup failed");
     strcpy(pText,s);
   };
}

ANTLRAbstractToken * ColToken::makeToken(ANTLRTokenType tokenType,
                                         ANTLRChar *    text,
                                         int            line) {
   return new ColToken(tokenType,text,line);
}

void ColToken::dumpNode(const char * s) {

   char * theText;

   if (s != 0) {printf("%s ",s);};
   if (getType() == EOF) {
     printf("TokenType \"EOF\" Token # %d\n",serial);
   } else {
     if (pText == 0) {
        theText="";
     } else if (strcmp(pText,"\n") == 0) {
    theText="NL";
     } else {
    theText=pText;
     }; 
     printf("TokenType (%-s) Text (%s) Token # %d  Line=%d  BegCol=%d\n",
	    /* P::tokenName(getType()),*/
        theText,
        serial,     
        getLine(),
        begcol);
   };
}
