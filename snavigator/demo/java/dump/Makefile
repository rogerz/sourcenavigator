.SUFFIXES:
.SUFFIXES: .java .o

GCJ= gcj
FLAGS= 
#-fassume-compiled
KAWA=Access.o ArrayType.o AttrContainer.o Attribute.o ClassFileInput.o \
	ClassType.o ClassTypeWriter.o CodeAttr.o ConstantPool.o CpoolClass.o \
	CpoolEntry.o CpoolFloat.o CpoolNameAndType.o CpoolRef.o CpoolString.o \
	CpoolUtf8.o CpoolValue1.o CpoolValue2.o Field.o IfState.o Label.o \
	LineNumbersAttr.o LocalVarsAttr.o Location.o Method.o MiscAttr.o \
	ObjectType.o PrimType.o Scope.o SourceFileAttr.o TryState.o Type.o \
	VarEnumerator.o Variable.o ZipArchive.o ZipMember.o dump.o
KAWA_O := $(patsubst %.o,gnu/bytecode/%.o, $(KAWA))
KAWA_SRC := $(patsubst %.o,gnu/bytecode/%.java, $(KAWA))
OBJDIR=
BINARY= dump
MAIN=gnu.bytecode.dump
MAIN_JAVA= gnu/bytecode/dump.java

# Documentation on target(s)
all:
	@echo "Targets available in this makefile:"
	@echo ""
	@echo "  make dump	Non optimized native executable"
	@echo "  make clean	Clean objects for a fresh built"
	@echo ""

# The `linpack' target natively builds a debuggable, non-optimize
# linpack executable. Record time and sizes.
dump:	
	@date '+%H:%M:%S' > .time_stamp_started
	make dump_do
	@date '+%H:%M:%S' > .time_stamp_finished
	@echo "Unoptimized, debuggable version \`$(BINARY)' is ready."
	@echo "Try in on the file \`test.class' in this directory."
	@echo "Time started: `cat .time_stamp_started`"
	@echo "Time finished: `cat .time_stamp_finished`"
	@echo "\`gnu.bytecode' package is `wc -l $(KAWA_SRC) | \
		egrep total | awk '{print $$1}'` lines"

dump_do: $(KAWA_O)
	$(GCJ) $(FLAGS) --main=$(MAIN) $(KAWA_O) -o $(BINARY)

# Clean-up the .o files and the `dump' executable
clean:
	@rm -f $(KAWA_O) $(BINARY)

# Ensure that some of the right things are set
common-check:
	@if [ "`which $(GCJ) | egrep \"which: no\"`" ] ; then \
	  echo "No \`$(GCJ)' found in \$$PATH" ; \
	  exit 1 ; \
	fi ; \
	if [ ! "$(CLASSPATH)" ] ; then \
	  echo "\$$CLASSPATH environment variable isn't set!" ; \
	  exit 1 ; \
	fi ; \

# Rules to compile all .java
.java.o:
	$(GCJ) $(FLAGS) $< -c -o $@
