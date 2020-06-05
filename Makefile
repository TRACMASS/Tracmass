# Project and case definition
PROJECT	          = Theoretical
CASE              = Theoretical
RUNFILE 	        = runtracmass
NETCDFLIBS        =
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
ifeq ($(NETCDFLIBS),"none")
LIB_DIR =
INC_DIR =
ORM_FLAGS += -Dno_netcdf

else ifeq ($(NETCDFLIBS),"automatic")
LIB_DIR = $(shell nc-config --flibs)
INC_DIR = -I$(shell nc-config --includedir)

else ifeq ($(NETCDFLIBS),"automatic-44")
LIB_DIR = $(shell nf-config --flibs)
INC_DIR = $(shell nf-config --cflags)

else
NCDF_ROOT = /usr/local/Cellar/netcdf/4.6.2

LIB_DIR = -L$(NCDF_ROOT)/lib -lnetcdf -lnetcdff
INC_DIR	= -I$(NCDF_ROOT)/include

endif

# Fortran compiler and flags
FC = gfortran
FF = -g -O3 -fbacktrace -fbounds-check -Wall -Wno-unused-dummy-argument

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
	setup_grid.o kill_zones.o read_field.o mod_clock.o  \
	mod_write.o mod_error.o mod_vertvel.o mod_tracers.o mod_seed.o  mod_stream.o \
	mod_pos.o mod_init.o mod_print.o mod_loop.o TRACMASS.o)

$(OBJDIR)/%.o : %.F90
		$(FC) $(FF) -c $(ORM_FLAGS) $(PROJECT_FLAG) $(CASE_FLAG) $(INC_DIR) $(LIB_DIR) $< -o $@

$(objects) : | $(OBJDIR)

$(OBJDIR):
			mkdir -p $(OBJDIR)

#================================================================

runfile : $(objects)

	$(FC) $(FF) $(ORM_FLAGS) -o $(RUNFILE) $(objects) $(INC_DIR) $(LIB_DIR)

.PHONY : clean

compile_test:

	$(FC) $(FF) $(PROJECT_FLAG) -o test_mod_calendar.x src/mod_vars.F90 src/mod_calendar.F90 src/mod_write.F90 src/mod_error.F90 src/mod_vertvel.F90 src/mod_seed.F90 src/mod_init.F90   src/_tests/mod_calendar_test.F90

	$(FC) $(FF) $(PROJECT_FLAG) -o test_mod_seed.x src/mod_vars.F90 src/mod_calendar.F90 src/mod_write.F90 src/mod_error.F90 src/mod_vertvel.F90 src/mod_seed.F90 src/mod_init.F90  src/_tests/mod_seed_test.F90

	$(FC) $(FF) $(PROJECT_FLAG) -o test_mod_pos.x src/mod_vars.F90 src/mod_calendar.F90 src/mod_stream.F90 src/mod_write.F90 src/mod_error.F90 src/mod_pos.F90 src/mod_vertvel.F90  src/mod_seed.F90 src/mod_init.F90 src/_tests/mod_pos_test.F90

	$(FC) $(FF) $(PROJECT_FLAG) -o test_mod_error.x src/mod_vars.F90 src/mod_calendar.F90 src/mod_stream.F90 src/mod_write.F90 src/mod_error.F90 src/mod_pos.F90 src/mod_vertvel.F90  src/mod_seed.F90 src/mod_init.F90 src/_tests/mod_error_test.F90


test:

	cp src/_tests/testFiles/namelist_test.in namelist.in
	cp src/_tests/testFiles/seedFile seedFile
	cp src/_tests/testFiles/seedTime seedTime

	./test_mod_calendar.x
	./test_mod_seed.x
	./test_mod_pos.x
	./test_mod_error.x

	-rm -rf seedTime seedFile namelist.in

clean:

	-rm -rf *.o *.mod *.out *.dSYM *.csv fort.* *.x *.in
	-rm -rf _build
	-rm $(RUNFILE)
