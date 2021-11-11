#!/bin/bash

#================================================================
#                     TRACMASS test_suite
#================================================================
# The following test_suite is computed using fUNIT.
# netCDF libraries are required for mod_getfile.F90

# Move .fun files to src directory
cd src/_funit
cp ../*.F90 .

# Test_suite for main modules funit mod_calendar
funit mod_calendar
funit mod_clock
funit mod_error
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

# There are two possible mod_pos depending on the scheme

# 1 - Regular time step scheme
cp mod_pos_tstep.fun mod_pos.fun

# Remove the ifndef and endif
sed '1 s/#ifndef time_analytical/ /' mod_pos_tstep.F90 > temp1.F90
sed '608 s/#endif/ /' temp1.F90 > temp2.F90
mv temp2.F90 mod_pos.F90
funit mod_pos

rm mod_pos.F90 mod_pos.fun
rm mod_pos_tstep.F90 mod_pos.o mod_pos.mod
rm temp1.F90 temp2.F90
funit --clean

# 2 - Time analytical scheme
cp mod_pos_tanalytical.fun mod_pos.fun

# Remove the ifndef and endif
sed '1 s/#ifdef time_analytical/ /' mod_pos_tanalytical.F90 > temp1.F90
sed '2037 s/#endif/ /' temp1.F90 > temp2.F90
mv temp2.F90 mod_pos.F90
funit mod_pos

# The test suite for mod_getfile requires to read a netcdf file
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
rm -rf *.out *.F90 *.mod *.o fort.* seed* pres_temp_4D_wr.* *.nc
rm -rf mod_pos.fun
