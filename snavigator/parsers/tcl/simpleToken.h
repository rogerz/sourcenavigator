#ifndef SIMPLETOKEN_H
#define SIMPLETOKEN_H

/*  This file should be accompanied by DISCLAIMER.TXT stating disclaimers */

#include "AToken.h"

class SimpleToken : public ANTLRAbstractToken {

protected:
    ANTLRTokenType      _type;
public:
    int             _line;
    SimpleToken(ANTLRTokenType t)
        { setType(t);
          _line = 0;
        }
    SimpleToken()
        { setType((ANTLRTokenType)0);
          _line = 0;
        }
    ANTLRTokenType  getType() const { return _type; }
    void setType(ANTLRTokenType t)  { _type = t; }
    virtual int getLine() const     { return _line; }
    void setLine(int line)          { _line = line; }
};

#endif
