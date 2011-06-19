CC = g++
COMPILE = $(CC) -Wall -O3 -c -ffriend-injection
LINK = $(CC) -dynamiclib

CHEADERS = Heap.h VarOrder.h SolverTypes.h Solver.h Proof.h Sort.h Global.h File.h 
CSRCS = Solver.C File.C Proof.C 
COBJS = Solver.o File.o Proof.o 

U0=$(shell uname)
U1=$(subst SunOS,solaris,${U0})
U2=$(subst CYGWIN.*,cygwin,${U1})
U3=$(subst Darwin,darwin,${U2})
OS=$(subst Linux,linux,${U3})

JNIDIR = /System/Library/Java/JavaVirtualMachines/1.6.0.jdk/Contents/Home/include
JLIBDIR = /Library/Java/Extensions/
JNILIB = libminisatprover.jnilib

JHEADERS = kodkod_engine_satlab_MiniSatProver.h 
JSRCS = kodkod_engine_satlab_MiniSatProver.cpp 
JOBJS = $(JSRCS:.cpp=.o)

$(COBJS):  $(CHEADERS)
	$(COMPILE) $(CSRCS)

$(JOBJS):  $(COBJS) $(JHEADERS)
	$(COMPILE) -I$(JNIDIR) -I. $(JSRCS)

EXTRALIBS=
ifeq "${OS}" "darwin"
	EXTRALIBS += -framework JavaVM
endif

$(JLIBDIR):
	mkdir -vp $@

$(JNILIB): $(JLIBDIR) $(COBJS) $(JOBJS)
	$(LINK) -o $(JLIBDIR)$(JNILIB) $(COBJS) $(JOBJS) $(EXTRALIBS)

clean:
	rm -f *.o

all: $(JNILIB)
