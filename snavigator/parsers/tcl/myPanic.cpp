//
//  myPanic.cpp
//

/*  This file should be accompanied by DISCLAIMER.TXT stating disclaimers */

#include <stdio.h>
#include <stdlib.h>

void myPanic(const char * s) {
   printf ("\nPanic: %s\n",s);
   exit(1);
};
