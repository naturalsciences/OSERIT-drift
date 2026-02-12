#
#
# Last Update - 4 Nov 2008              @{COHERENS}Makefile   V2
#

# file paths
CURDIR := $(dir $(abspath $(lastword $(MAKEFILE_LIST))))
OBJDIR := $(CURDIR)../../bin/obj
MODDIR := $(CURDIR)../../bin/mod
# Set the VPATH relative to the current directory
VPATH := $(CURDIR)../coherens/source:$(OBJDIR):$(CURDIR)../oserit


# make command
MAKE = make

# include file with cpp options
DOPTSFILE = options.cpp

# main program
#EXEFILE = coherens
#MAINFILE = Coherens_Program.o
EXEFILE = oserit
MAINFILE = main.o

# default machine dependent parameters.
FC =
FCOPTS =
FCDEFS =
FCDEBUG =
FCIFLAGS =
FLIBS =
CPP =
CPPF =
CPPOPTS =
CPPDEFS =

# no target
none:
	@echo No target provided

# options for cpp processing
include $(DOPTSFILE)

# invoke make
include compilers.cmp

# object files
include objects.cmp
include objects_coherens.cmp
MODFILES = $(MODULES) $(MODULES_BIO) $(MODULES_OSE)
SUBFILES = $(SUBPROGS) $(SUBPROGS_BIO) $(SUBPROGS_OSE)

# compilation flags
CPPFLAGS = $(CPPOPTS) $(CPPDEFS)
FFLAGS = $(FCIFLAGS) $(FCOPTS) $(FCDEFS) $(FCDEBUG) -J$(MODDIR)

# install:
install:
	echo "@cp $(EXEFILE) ../../$(EXEFILE)"
	@cp $(EXEFILE) ../../$(EXEFILE)

# clean unnecessary files
clean:
	@rm -f $(MODFILES) $(SUBFILES) $(MAINFILE) ../../$(EXEFILE) $(EXEFILE) *.mod $(MODDIR)/*.mod $(OBJDIR)/*.o

# create executable
#$(EXEFILE): $(MODFILES) $(SUBFILES) $(MAINFILE)
#	$(FC) $(FFLAGS)  -o $(EXEFILE) $(MODFILES) $(SUBFILES) $(FLIBS)\
#        $(MAINFILE)
NETCDF_CONFIG := $(shell command -v nf-config 2>/dev/null || command -v nc-config 2>/dev/null)

PATH_NETCDF_FFLAGS := $(shell $(NETCDF_CONFIG) --fflags)
PATH_NETCDF_FLIBS  := $(shell $(NETCDF_CONFIG) --flibs)

$(EXEFILE): $(MODFILES) $(SUBFILES) $(MAINFILE)
	$(FC) $(FFLAGS) $(OBJDIR)/*.o -o ../../$(EXEFILE) $(FLIBS)

# dependencies
include dependencies.cmp
include dependencies_coherens.cmp

# suffix rules
.SUFFIXES:
.SUFFIXES:.o .f90 .F90

.F90.f90:
	@rm -f $*.f90
	$(CPPF) $(CPPFLAGS) $< .$(OBJDIR)/$*.f90
.f90.o:
	$(FC) $(FFLAGS) -c $< -o $(OBJDIR)/$*.o
.F90.o:
	@rm -f $*.f90
	$(CPPF) $(CPPFLAGS) $< ./$*.f90
	$(FC) $(FFLAGS) -c ./$*.f90 -o $(OBJDIR)/$*.o
	@rm -f $*.f90
