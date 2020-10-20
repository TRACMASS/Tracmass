#!/bin/bash

#================================================================
#                     TRACMASS test_suite
#================================================================

# The following test_suite is computed using fUNIT.
# netCDF libraries are required for mod_getfile.F90

# Move .fun files to src directory
cd src/
cp _funit/*.fun .

# Test_suite for main modules funit mod_calendar
funit mod_clock
funit mod_error
funit mod_pos
funit mod_subdomain
funit mod_swap
funit mod_tracers
funit mod_vertvel

# The test suite for mod_seed requires to read two files
cat <<EOF > seedTime
1
3
5
11
12
24
EOF

cat <<EOF > seedFile
 1  2  3  1 -1
 4  5 10  1 -1
 6  7  8  1  1
12 14  7  1  1
EOF

funit mod_seed

NCDIR=""
if [[ -n $NCDIR ]];
then
  export FCFLAGS=$NCDIR

  # Create example nc-file
  wget https://www.unidata.ucar.edu/software/netcdf/examples/programs/pres_temp_4D_wr.f90

  gfortran pres_temp_4D_wr.f90 -o writenc.out $FCFLAGS
  ./writenc.out

  funit mod_getfile
fi

# Clean after tests
funit --clean
rm -rf *.out *.fun *.mod *.o fort.* seed* pres_temp_4D_wr.* *.nc
