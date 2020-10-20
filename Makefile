#================================================================
#                     TRACMASS MAKEFILE
#================================================================

# Project and case definition
PROJECT	          = Theoretical
CASE              = Theoretical
RUNFILE 	        = runtracmass
ARCH              =
NETCDFLIBS        = none
#================================================================

# Possible architectures:
# tetralith    (Swedish HPC with intel)

# Possible netCDF settings:
# automatic    (set by nc-config)
# automatic-44 (set by nf-config, for netCDF version >4.4)
# none         (no netCDF library)

#================================================================
# ***************************************************************
#================================================================

# Read the project Makefile
PROJMAKE           := $(wildcard projects/$(PROJECT)/Makefile.prj)
CASEMAKE           := $(wildcard projects/$(PROJECT)/Makefile.prj)

ifneq ($(strip $(CASEMAKE)),)
include projects/$(PROJECT)/Makefile.prj
else
ifneq ($(strip $(PROJMAKE)),)
include projects/$(PROJECT)/Makefile.prj
endif
endif

PROJECT_FLAG      = -DPROJECT_NAME=\'$(PROJECT)\'
CASE_FLAG         = -DCASE_NAME=\'$(CASE)\'

#================================================================

# NetCDF libraries
ifeq ($(NETCDFLIBS),none)
LIB_DIR =
INC_DIR =
ORM_FLAGS += -Dno_netcdf

else ifeq ($(NETCDFLIBS),automatic)
LIB_DIR = $(shell nc-config --flibs)
INC_DIR = -I$(shell nc-config --includedir)

else ifeq ($(NETCDFLIBS),automatic-44)
LIB_DIR = $(shell nf-config --flibs)
INC_DIR = $(shell nf-config --cflags)

else
NCDF_ROOT = /usr

LIB_DIR = -L$(NCDF_ROOT)/lib -lnetcdf -lnetcdff
INC_DIR	= -I$(NCDF_ROOT)/include

endif

# Fortran compiler and flags
ifeq ($(ARCH),tetralith)
FC = ifort
FF = -g -O3 -traceback -pg

else
FC = gfortran
FF = -g -O3 -fbacktrace -fbounds-check -Wall -Wno-maybe-uninitialized -Wno-unused-dummy-argument

endif

# Path to sources
VPATH = src:projects/$(PROJECT)

all: runfile

ifneq ($(strip $(CASE)),)
	cp projects/$(PROJECT)/namelist_$(CASE).in namelist.in
else
ifneq ($(strip $(PROJECT)),)
	cp projects/$(PROJECT)/namelist_$(PROJECT).in namelist.in
endif
endif


#================================================================

# Object definitions
OBJDIR := _build

objects := $(addprefix $(OBJDIR)/,mod_vars.o mod_subdomain.o mod_getfile.o mod_calendar.o \
	mod_tracerf.o mod_tracers.o setup_grid.o kill_zones.o mod_vertvel.o mod_swap.o read_field.o mod_clock.o  \
	mod_write.o mod_error.o mod_seed.o  mod_stream.o \
	mod_pos.o mod_init.o mod_print.o mod_loop.o mod_postprocess.o TRACMASS.o)

$(OBJDIR)/%.o : %.F90
		$(FC) $(FF) -c $(ORM_FLAGS) $(PROJECT_FLAG) $(CASE_FLAG) $(INC_DIR) $(LIB_DIR) $< -o $@

$(objects) : | $(OBJDIR)

$(OBJDIR):
			mkdir -p $(OBJDIR)

#================================================================

runfile : $(objects)

	$(FC) $(FF) $(ORM_FLAGS) -o $(RUNFILE) $(objects) $(INC_DIR) $(LIB_DIR)

test :

	sed '42s~.*~NCDIR="$(LIB_DIR) $(INC_DIR)" ~' src/_funit/runtest.sh > runtest.sh
	chmod +x runtest.sh

.PHONY : clean

clean:

	-rm -rf *.o *.mod *.out *.dSYM *.csv fort.* *.x *.in
	-rm runtest.sh
	-rm -rf _build
	-rm $(RUNFILE)
