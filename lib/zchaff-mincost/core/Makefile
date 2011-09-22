CC = g++
COMPILE = $(CC) -Wall -O3 -c -ffriend-injection
LINK = $(CC) -dynamiclib

CHEADERS = zchaff_dbase.h zchaff_solver.h zchaff_header.h zchaff_version.h zchaff_base.h 
CSRCS = zchaff_utils.cpp zchaff_base.cpp zchaff_solver.cpp zchaff_dbase.cpp 
COBJS = zchaff_utils.o zchaff_base.o zchaff_solver.o zchaff_dbase.o 

U0=$(shell uname)
U1=$(subst SunOS,solaris,${U0})
U2=$(subst CYGWIN.*,cygwin,${U1})
U3=$(subst Darwin,darwin,${U2})
OS=$(subst Linux,linux,${U3})

JNIDIR = /System/Library/Java/JavaVirtualMachines/1.6.0.jdk/Contents/Home/include
JLIBDIR = ./
JNILIB = libzchaffmincost.jnilib

JHEADERS = kodkod_engine_satlab_ZChaffMincost.h 
JSRCS = kodkod_engine_satlab_ZChaffMincost.cpp 
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
