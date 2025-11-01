#-----------------------------------------------------------------------------
#                               EXAMPLES  Makefile
#-----------------------------------------------------------------------------
#
# Intructions to Add a new example 
#
#   1. Update the variable LIST_EXEC with the new target (executable name)
#
#   2. Add a new entry in the dependencies with the following format :
#       name_exec_DEP= {list of .o files needed to build the executable}
#       name_exec : $(name_exec_DEP)
#-----------------------------------------------------------------------------

# the C compiler
CC=gcc

# the Ada Compiler
ADAC=gcc

# Gnat1 compilation flags
GF= 

# Gnatbind binder flags
BF=

# Put here the final target of each test case.
# it can be a '.o' file if the test is just a compilation one
# otherwise it is the final executable
LIST_COMPILE_ONLY=
LIST_EXEC= hello text_io_example demo1 demo2 


#-----------------------------------------------------------------------------
# Main rule 

process : clean $(LIST_EXEC)

#-----------------------------------------------------------------------------
# Dependancies

hello_DEP= hello.o io.o cio.o
hello : $(hello_DEP)

demo1_DEP= demo1.o ada-tags.o instr.o io.o cio.o
demo1 : $(demo1_DEP)

demo2_DEP= demo2.o ada-tags.o instr.o instr-child.o io.o cio.o
demo2 : $(demo2_DEP)

text_io_example_DEP = text_io_example.o ada.o ada-io_exceptions.o \
 ada-text_io.o ada-text_io-aux.o system.o io-aux.o  text_io.o
text_io_example : $(text_io_example_DEP)

###############################################################################
# General rules
###############################################################################
.SUFFIXES: .adb .ads .ali .o
.PHONY: clean process force

.adb.o:
	$(ADAC) -c $(GF)  $< 

.ads.o:
	$(ADAC) -c $(GF)  $< 
.c.o:
	$(CC) -c $(CFLAGS) $<

% : %.o
	$(BDIR)gnatbind $@.ali
	$(CC) -c $(CFLAGS) bind_$@.c
	$(CC) $(LDFLAGS) -o $@ bind_$@.o $($@_DEP)
	$@


clean : force
	@/bin/rm -f  *.o *.ali bind_*.c *.s  $(LIST_EXEC)

force : 
