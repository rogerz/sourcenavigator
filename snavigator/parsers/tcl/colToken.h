#ifndef COLTOKEN_H
#define COLTOKEN_H

/*  This file should be accompanied by DISCLAIMER.TXT stating disclaimers */

#include "tokens.h"
#include "noleakTok.h"
#include "AToken.h"

struct ColToken : NoLeakToken {

       /* constructor */        ColToken();
       /* destructor */ virtual ~ColToken();
       /* constructor */        ColToken(ANTLRTokenType t);
       /* constructor */        ColToken(ANTLRTokenType t,
                                         ANTLRChar *    text,
                                         int        line);
       /* copy constructor */   ColToken(const ColToken &);

       ColToken &               operator = (const ColToken &);
       virtual void             dumpNode(const char * s=0);
       virtual ANTLRChar *      getText() const {return pText;};
       virtual ANTLRAbstractToken * makeToken(ANTLRTokenType    ANTLRTokenType,
                                              ANTLRChar         *text,
                                              int               line);
       virtual void             setText(const ANTLRChar *s);

       int                      begcol;        // starting column
       int                      endcol;        // ending column
       ANTLRChar *              pText;
private:
       void                     init();
};

#endif

