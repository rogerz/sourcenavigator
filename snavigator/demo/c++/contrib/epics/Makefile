# $Header$
#
#    Makefile for EPICS-channel-access glish client, caserv
#
#    Note: Glish requires an ANSI C compiler.

EPICS_INC = -IepicsH
EPICS_LIB = -Lepics_lib -lca -lCom -lUnix

RM = rm -f

CC = gcc
CFLAGS = -g

ISTK = ../..
INCL = -I$(ISTK)/include $(EPICS_INC) -I.

GLISH_LIBS = -L$(ISTK)/lib/$(ARCH) -lglish -lsds
LIBS = $(GLISH_LIBS) $(EPICS_LIB) -lm

all: echan

echan: EpicsChannel.o main.o
	$(CPLUS) $(CFLAGS) $(INCL) -o echan \
		main.o EpicsChannel.o $(LIBS)

main.o: main.cc
	$(CPLUS) $(CFLAGS) $(INCL) -c main.cc 

EpicsChannel.o: EpicsChannel.cc EpicsChannel.h
	$(CPLUS) $(CFLAGS) -DUNIX $(INCL) -c EpicsChannel.cc 

clean:
	rm -f echan *.o errs
