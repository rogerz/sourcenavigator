#ifndef MYTOKENBUFFER_H
#define MYTOKENBUFFER_H

/*  This file should be accompanied by DISCLAIMER.TXT stating disclaimers */

#include "tokens.h"
#include "ATokenBuffer.h"

struct MyTokenBuffer : ANTLRTokenBuffer {
   
   /* constructor */        MyTokenBuffer(
                    ANTLRTokenStream *in,
                    int k=1,
                    int chksz=200);
   virtual /* destructor */ ~MyTokenBuffer() {};
   virtual ANTLRAbstractToken * getANTLRToken();
}; 

#endif
