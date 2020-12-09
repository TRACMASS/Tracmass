test_suite mod_getfile

! test_suite for mod_getfile
! Contains 28 tests
! 9  -> filledFileName
! 1  -> get1DfieldNC
! 6  -> get2DfieldNC
! 12 -> get3DfieldNC

SETUP
  l_subdom = .FALSE.
  imindom  = 1
  jmindom  = 1
END SETUP

TEARDOWN
  ! This code runs immediately after each test
END TEARDOWN

TEST test_filledFileName_1

   ! Test for filledFileName
   ! fillePattern --> filledFileName
   INTEGER :: inyear = 2000
   INTEGER :: inmon  = 1
   INTEGER :: inday  = 1

   CHARACTER(len=100) :: filePattern

   PRINT *, ' * Test filledFileName  : DD (day<10)'

   filePattern = filledFileName('DATASET_200001DD',inyear,inmon,inday)

   ASSERT_EQUAL(filePattern, 'DATASET_20000101')

END TEST

TEST test_filledFileName_2

   ! Test for filledFileName
   ! fillePattern --> filledFileName
   INTEGER :: inyear = 2000
   INTEGER :: inmon  = 1
   INTEGER :: inday  = 11

   CHARACTER(len=100) :: filePattern

   PRINT *, ' * Test filledFileName  : DD (day>=10)'

   filePattern = filledFileName('DATASET_200001DD',inyear,inmon,inday)

   ASSERT_EQUAL(filePattern, 'DATASET_20000111')

END TEST

TEST test_filledFileName_3

   ! Test for filledFileName
   ! fillePattern --> filledFileName
   INTEGER :: inyear = 2000
   INTEGER :: inmon  = 1
   INTEGER :: inday  = 1

   CHARACTER(len=100) :: filePattern

   PRINT *, ' * Test filledFileName  : MM (month<10)'

   filePattern = filledFileName('DATASET_2000MM01',inyear,inmon,inday)

   ASSERT_EQUAL(filePattern, 'DATASET_20000101')

END TEST

TEST test_filledFileName_4

   ! Test for filledFileName
   ! fillePattern --> filledFileName
   INTEGER :: inyear = 2000
   INTEGER :: inmon  = 11
   INTEGER :: inday  = 1

   CHARACTER(len=100) :: filePattern

   PRINT *, ' * Test filledFileName  : MM (month>=10)'

   filePattern = filledFileName('DATASET_2000MM01',inyear,inmon,inday)

   ASSERT_EQUAL(filePattern, 'DATASET_20001101')

END TEST

TEST test_filledFileName_5

   ! Test for filledFileName
   ! fillePattern --> filledFileName
   INTEGER :: inyear = 2
   INTEGER :: inmon  = 11
   INTEGER :: inday  = 1

   CHARACTER(len=100) :: filePattern

   PRINT *, ' * Test filledFileName  : YYYY (year<10)'

   filePattern = filledFileName('DATASET_YYYY0101',inyear,inmon,inday)

   ASSERT_EQUAL(filePattern, 'DATASET_00020101')

END TEST

TEST test_filledFileName_6

   ! Test for filledFileName
   ! fillePattern --> filledFileName
   INTEGER :: inyear = 20
   INTEGER :: inmon  = 11
   INTEGER :: inday  = 1

   CHARACTER(len=100) :: filePattern

   PRINT *, ' * Test filledFileName  : YYYY (year<100)'

   filePattern = filledFileName('DATASET_YYYY0101',inyear,inmon,inday)

   ASSERT_EQUAL(filePattern, 'DATASET_00200101')

END TEST

TEST test_filledFileName_7

   ! Test for filledFileName
   ! fillePattern --> filledFileName
   INTEGER :: inyear = 200
   INTEGER :: inmon  = 11
   INTEGER :: inday  = 1

   CHARACTER(len=100) :: filePattern

   PRINT *, ' * Test filledFileName  : YYYY (year<1000)'

   filePattern = filledFileName('DATASET_YYYY0101',inyear,inmon,inday)

   ASSERT_EQUAL(filePattern, 'DATASET_02000101')

END TEST

TEST test_filledFileName_8

   ! Test for filledFileName
   ! fillePattern --> filledFileName
   INTEGER :: inyear = 2000
   INTEGER :: inmon  = 11
   INTEGER :: inday  = 1

   CHARACTER(len=100) :: filePattern

   PRINT *, ' * Test filledFileName  : YYYY (year>=1000)'

   filePattern = filledFileName('DATASET_YYYY0101',inyear,inmon,inday)

   ASSERT_EQUAL(filePattern, 'DATASET_20000101')

END TEST

TEST test_filledFileName_9

   ! Test for filledFileName
   ! fillePattern --> filledFileName
   INTEGER :: inyear = 2000
   INTEGER :: inmon  = 11
   INTEGER :: inday  = 1

   CHARACTER(len=100) :: filePattern

   PRINT *, ' * Test filledFileName  : YYYYMMDD'

   filePattern = filledFileName('DATASET_YYYYMMDD',inyear,inmon,inday)

   ASSERT_EQUAL(filePattern, 'DATASET_20001101')

END TEST

TEST test_get1DfieldNC

   ! Test for get1DfieldNC
   ! get1DfieldNC --> 1D
   REAL, DIMENSION(6):: field1D

   PRINT *, ' * Test get1DfieldNC    '

   field1D = get1DfieldNC('pres_temp_4D.nc','latitude',1,6)

   ASSERT_EQUAL(SIZE(field1D), 6)
   ASSERT_EQUAL(field1D(1),25)
   ASSERT_EQUAL(field1D(6),50)

END TEST

TEST test_get2DfieldNC_1

   ! Test for get2DfieldNC
   ! get2DfieldNC --> 2D
   REAL, DIMENSION(12,6):: field2D

   PRINT *, ' * Test get2DfieldNC    : no subdomain (st)'

   field2D = get2DfieldNC('pres_temp_4D.nc','temperature',[1,1,1,1],[12,6,1,1],'st')

   ASSERT_EQUAL(SIZE(field2D,1), 12)
   ASSERT_EQUAL(SIZE(field2D,2),  6)
   ASSERT_EQUAL(field2D(1,1),9)
   ASSERT_EQUAL(field2D(1,2),21)
   ASSERT_EQUAL(field2D(2,1),10)
   ASSERT_EQUAL(field2D(2,2),22)
   ASSERT_EQUAL(field2D(11,6),79)
   ASSERT_EQUAL(field2D(12,6),80)

END TEST

TEST test_get2DfieldNC_2

   ! Test for get2DfieldNC
   ! get2DfieldNC --> 2D
   REAL, DIMENSION(6,12):: field2D

   PRINT *, ' * Test get2DfieldNC    : no subdomain (st_r)'

   field2D = get2DfieldNC('pres_temp_4D.nc','temperature',[1,1,1,1],[6,12,1,1],'st_r')

   ASSERT_EQUAL(SIZE(field2D,1), 6)
   ASSERT_EQUAL(SIZE(field2D,2), 12)
   ASSERT_EQUAL(field2D(1,1),9)
   ASSERT_EQUAL(field2D(1,2),10)
   ASSERT_EQUAL(field2D(2,1),21)
   ASSERT_EQUAL(field2D(2,2),22)
   ASSERT_EQUAL(field2D(6,11),79)
   ASSERT_EQUAL(field2D(6,12),80)

END TEST

TEST test_get2DfieldNC_3

   ! Test for get2DfieldNC
   ! get2DfieldNC --> 2D
   REAL, ALLOCATABLE, DIMENSION(:,:) :: field2D

   PRINT *, ' * Test get2DfieldNC    : subdomain RB (st)'

   ! Define subdomain
   l_subdom = .TRUE.

   imaxdom = 10; imindom = 5
   jmaxdom =  5; jmindom = 2

   imt = imaxdom - imindom + 1
   jmt = jmaxdom - jmindom + 1

   ! Allocate field2D
   ALLOCATE(field2D(imt,jmt))

   field2D = get2DfieldNC('pres_temp_4D.nc','temperature',[imindom,jmindom,1,1],[imt,jmt,1,1],'st')

   ASSERT_EQUAL(SIZE(field2D,1), 6)
   ASSERT_EQUAL(SIZE(field2D,2), 4)
   ASSERT_EQUAL(field2D(1,1),25)
   ASSERT_EQUAL(field2D(1,4),61)
   ASSERT_EQUAL(field2D(6,1),30)
   ASSERT_EQUAL(field2D(6,4),66)

END TEST

TEST test_get2DfieldNC_4

   ! Test for get2DfieldNC
   ! get2DfieldNC --> 2D
   REAL, ALLOCATABLE, DIMENSION(:,:) :: field2D

   PRINT *, ' * Test get2DfieldNC    : subdomain RB (st_r)'

   ! Define subdomain
   l_subdom = .TRUE.

   imaxdom = 10; imindom = 5
   jmaxdom =  5; jmindom = 2

   imt = imaxdom - imindom + 1
   jmt = jmaxdom - jmindom + 1

   ! Allocate field2D
   ALLOCATE(field2D(jmt,imt))

   field2D = get2DfieldNC('pres_temp_4D.nc','temperature',[jmindom,imindom,1,1],[jmt,imt,1,1],'st_r')

   ASSERT_EQUAL(SIZE(field2D,1), 4)
   ASSERT_EQUAL(SIZE(field2D,2), 6)
   ASSERT_EQUAL(field2D(1,1),25)
   ASSERT_EQUAL(field2D(4,1),61)
   ASSERT_EQUAL(field2D(1,6),30)
   ASSERT_EQUAL(field2D(4,6),66)

END TEST

TEST test_get2DfieldNC_5

   ! Test for get2DfieldNC
   ! get2DfieldNC --> 2D
   REAL, ALLOCATABLE, DIMENSION(:,:) :: field2D

   PRINT *, ' * Test get2DfieldNC    : subdomain SB (st)'

   ! Grid size
   imt = 12; jmt = 6

   ! Define subdomain
   l_subdom = .TRUE.

   imaxdom =  5; imindom = 10
   jmaxdom =  5; jmindom = 2

   ! Define the two sub-subdomain
   imthalf1 = imt - imindom + 1
   imthalf2 = imaxdom

   ! Recalculate the values of imt and jmt
   imt = imthalf1 + imthalf2
   jmt = jmaxdom - jmindom + 1

   ! Allocate field2D
   ALLOCATE(field2D(imt,jmt))

   field2D = get2DfieldNC('pres_temp_4D.nc','temperature',[imindom,jmindom,1,1],[imt,jmt,1,1],'st')

   ASSERT_EQUAL(SIZE(field2D,1), 8)
   ASSERT_EQUAL(SIZE(field2D,2), 4)
   ASSERT_EQUAL(field2D(1,1),30)
   ASSERT_EQUAL(field2D(1,4),66)
   ASSERT_EQUAL(field2D(8,1),25)
   ASSERT_EQUAL(field2D(8,4),61)

END TEST

TEST test_get2DfieldNC_6

   ! Test for get2DfieldNC
   ! get2DfieldNC --> 2D
   REAL, ALLOCATABLE, DIMENSION(:,:) :: field2D

   PRINT *, ' * Test get2DfieldNC    : subdomain SB (st_r)'

   ! Grid size
   imt = 6; jmt = 12

   ! Define subdomain
   l_subdom = .TRUE.

   imaxdom =  2; imindom = 5
   jmaxdom =  5; jmindom = 2

   ! Define the two sub-subdomain
   imthalf1 = imt - imindom + 1
   imthalf2 = imaxdom

   ! Recalculate the values of imt and jmt
   imt = imthalf1 + imthalf2
   jmt = jmaxdom - jmindom + 1

   ! Allocate field2D
   ALLOCATE(field2D(jmt,imt))

   field2D = get2DfieldNC('pres_temp_4D.nc','temperature',[jmindom,imindom,1,1],[jmt,imt,1,1],'st_r')

   ASSERT_EQUAL(SIZE(field2D,1), 4)
   ASSERT_EQUAL(SIZE(field2D,2), 4)
   ASSERT_EQUAL(field2D(1,1),61)
   ASSERT_EQUAL(field2D(4,1),25)
   ASSERT_EQUAL(field2D(4,4),28)
   ASSERT_EQUAL(field2D(1,4),64)

END TEST

TEST test_get3DfieldNC_1

   ! Test for get3DfieldNC
   ! get3DfieldNC --> 3D
   REAL, DIMENSION(12,6,2):: field3D

   PRINT *, ' * Test get3DfieldNC    : no subdomain (st)'

   field3D = get3DfieldNC('pres_temp_4D.nc','temperature',[1,1,1,1],[12,6,2,1],'st')

   ASSERT_EQUAL(SIZE(field3D,1), 12)
   ASSERT_EQUAL(SIZE(field3D,2),  6)
   ASSERT_EQUAL(SIZE(field3D,3),  2)
   ASSERT_EQUAL(field3D(1,1,1),9)
   ASSERT_EQUAL(field3D(1,1,2),81)
   ASSERT_EQUAL(field3D(12,6,1),80)
   ASSERT_EQUAL(field3D(12,6,2),152)

END TEST

TEST test_get3DfieldNC_2

   ! Test for get3DfieldNC
   ! get3DfieldNC --> 3D
   REAL, DIMENSION(2,6,12):: field3D

   PRINT *, ' * Test get3DfieldNC    : no subdomain (st_r)'

   field3D = get3DfieldNC('pres_temp_4D.nc','temperature',[1,1,1,1],[2,6,12,1],'st_r')

   ASSERT_EQUAL(SIZE(field3D,1),  2)
   ASSERT_EQUAL(SIZE(field3D,2),  6)
   ASSERT_EQUAL(SIZE(field3D,3),  12)
   ASSERT_EQUAL(field3D(1,1,1),9)
   ASSERT_EQUAL(field3D(2,1,1),81)
   ASSERT_EQUAL(field3D(1,6,12),80)
   ASSERT_EQUAL(field3D(2,6,12),152)

END TEST

TEST test_get3DfieldNC_3

   ! Test for get3DfieldNC
   ! get3DfieldNC --> 3D
   REAL, DIMENSION(6,2,2):: field3D

   PRINT *, ' * Test get3DfieldNC    : no subdomain (ts)'

   field3D = get3DfieldNC('pres_temp_4D.nc','temperature',[1,1,1,1],[6,2,2,1],'ts')

   ASSERT_EQUAL(SIZE(field3D,1),  6)
   ASSERT_EQUAL(SIZE(field3D,2),  2)
   ASSERT_EQUAL(SIZE(field3D,3),  2)
   ASSERT_EQUAL(field3D(1,1,1),9)
   ASSERT_EQUAL(field3D(1,1,2),9)
   ASSERT_EQUAL(field3D(6,2,1),141)
   ASSERT_EQUAL(field3D(6,2,2),141)

END TEST

TEST test_get3DfieldNC_4

   ! Test for get3DfieldNC
   ! get3DfieldNC --> 3D
   REAL, DIMENSION(2,2,6):: field3D

   PRINT *, ' * Test get3DfieldNC    : no subdomain (ts_r)'

   field3D = get3DfieldNC('pres_temp_4D.nc','temperature',[1,1,1,1],[2,2,6,1],'ts_r')

   ASSERT_EQUAL(SIZE(field3D,1),  2)
   ASSERT_EQUAL(SIZE(field3D,2),  2)
   ASSERT_EQUAL(SIZE(field3D,3),  6)
   ASSERT_EQUAL(field3D(1,1,1),9)
   ASSERT_EQUAL(field3D(2,1,1),9)
   ASSERT_EQUAL(field3D(1,2,6),141)
   ASSERT_EQUAL(field3D(2,2,6),141)

END TEST

TEST test_get3DfieldNC_5

   ! Test for get3DfieldNC
   ! get3DfieldNC --> 3D
   REAL, ALLOCATABLE, DIMENSION(:,:,:):: field3D

   PRINT *, ' * Test get3DfieldNC    : subdomain RB (st)'

   ! Define subdomain
   l_subdom = .TRUE.

   imaxdom = 10; imindom = 5
   jmaxdom =  5; jmindom = 2

   imt = imaxdom - imindom + 1
   jmt = jmaxdom - jmindom + 1

   ! Allocate field2D
   ALLOCATE(field3D(imt,jmt,2))

   field3D = get3DfieldNC('pres_temp_4D.nc','temperature',[imindom,jmindom,1,1],[imt,jmt,2,1],'st')

   ASSERT_EQUAL(SIZE(field3D,1), 6)
   ASSERT_EQUAL(SIZE(field3D,2), 4)
   ASSERT_EQUAL(SIZE(field3D,3), 2)
   ASSERT_EQUAL(field3D(1,1,1),25)
   ASSERT_EQUAL(field3D(1,1,2),97)
   ASSERT_EQUAL(field3D(6,4,1),66)
   ASSERT_EQUAL(field3D(6,4,2),138)

END TEST

TEST test_get3DfieldNC_6

   ! Test for get3DfieldNC
   ! get3DfieldNC --> 3D
   REAL, ALLOCATABLE, DIMENSION(:,:,:):: field3D

   PRINT *, ' * Test get3DfieldNC    : subdomain SB (st)'

   ! Grid size
   imt = 12; jmt = 6

   ! Define subdomain
   l_subdom = .TRUE.

   imaxdom =  5; imindom = 10
   jmaxdom =  5; jmindom = 2

   ! Define the two sub-subdomain
   imthalf1 = imt - imindom + 1
   imthalf2 = imaxdom

   ! Recalculate the values of imt and jmt
   imt = imthalf1 + imthalf2
   jmt = jmaxdom - jmindom + 1
   ! Allocate field2D
   ALLOCATE(field3D(imt,jmt,2))

   field3D = get3DfieldNC('pres_temp_4D.nc','temperature',[imindom,jmindom,1,1],[imt,jmt,2,1],'st')

   ASSERT_EQUAL(SIZE(field3D,1), 8)
   ASSERT_EQUAL(SIZE(field3D,2), 4)
   ASSERT_EQUAL(SIZE(field3D,3), 2)
   ASSERT_EQUAL(field3D(1,1,1),30)
   ASSERT_EQUAL(field3D(1,1,2),102)
   ASSERT_EQUAL(field3D(8,4,1),61)
   ASSERT_EQUAL(field3D(8,4,2),133)

END TEST

TEST test_get3DfieldNC_7

   ! Test for get3DfieldNC
   ! get3DfieldNC --> 3D
   REAL, ALLOCATABLE, DIMENSION(:,:,:):: field3D

   PRINT *, ' * Test get3DfieldNC    : subdomain RB (st_r)'

   ! Define subdomain
   l_subdom = .TRUE.

   imaxdom = 10; imindom = 5
   jmaxdom =  5; jmindom = 2

   imt = imaxdom - imindom + 1
   jmt = jmaxdom - jmindom + 1

   ! Allocate field2D
   ALLOCATE(field3D(2,jmt,imt))

   field3D = get3DfieldNC('pres_temp_4D.nc','temperature',[1,jmindom,imindom,1],[2,jmt,imt,1],'st_r')

   ASSERT_EQUAL(SIZE(field3D,1), 2)
   ASSERT_EQUAL(SIZE(field3D,2), 4)
   ASSERT_EQUAL(SIZE(field3D,3), 6)
   ASSERT_EQUAL(field3D(1,1,1),25)
   ASSERT_EQUAL(field3D(2,1,1),97)
   ASSERT_EQUAL(field3D(1,4,6),66)
   ASSERT_EQUAL(field3D(2,4,6),138)

END TEST

TEST test_get3DfieldNC_8

   ! Test for get3DfieldNC
   ! get3DfieldNC --> 3D
   REAL, ALLOCATABLE, DIMENSION(:,:,:) :: field3D

   PRINT *, ' * Test get3DfieldNC    : subdomain SB (st_r)'

   ! Grid size
   imt = 2; jmt = 6
   imtdom = 2; jmtdom = 6

   ! Define subdomain
   l_subdom = .TRUE.

   imaxdom =  1; imindom = 2
   jmaxdom =  5; jmindom = 2

   ! Define the two sub-subdomain
   imthalf1 = imt - imindom + 1
   imthalf2 = imaxdom

   ! Recalculate the values of imt and jmt
   imt = imthalf1 + imthalf2
   jmt = jmaxdom - jmindom + 1

   ! Allocate field2D
   ALLOCATE(field3D(2,jmt,imt))

   field3D = get3DfieldNC('pres_temp_4D.nc','temperature',[1,jmindom,imindom,1],[2,jmt,imt,1],'st_r')

   ASSERT_EQUAL(SIZE(field3D,1), 2)
   ASSERT_EQUAL(SIZE(field3D,2), 4)
   ASSERT_EQUAL(SIZE(field3D,3), 2)
   ASSERT_EQUAL(field3D(1,1,1),94)
   ASSERT_EQUAL(field3D(2,1,1),22)
   ASSERT_EQUAL(field3D(1,1,2),95)
   ASSERT_EQUAL(field3D(2,1,2),23)

END TEST

TEST test_get3DfieldNC_9

   ! Test for get3DfieldNC
   ! get3DfieldNC --> 3D
   REAL, ALLOCATABLE, DIMENSION(:,:,:):: field3D

   PRINT *, ' * Test get3DfieldNC    : subdomain RB (ts)'

   ! Define subdomain
   l_subdom = .TRUE.

   imaxdom =  5; imindom = 2
   jmaxdom =  2; jmindom = 1

   imt = imaxdom - imindom + 1
   jmt = jmaxdom - jmindom + 1

   ! Allocate field2D
   ALLOCATE(field3D(imt,jmt,2))

   field3D = get3DfieldNC('pres_temp_4D.nc','temperature',[imindom,jmindom,1,1],[imt,jmt,2,1],'ts')

   ASSERT_EQUAL(SIZE(field3D,1),  4)
   ASSERT_EQUAL(SIZE(field3D,2),  2)
   ASSERT_EQUAL(SIZE(field3D,3),  2)
   ASSERT_EQUAL(field3D(1,1,1),21)
   ASSERT_EQUAL(field3D(1,1,2),21)
   ASSERT_EQUAL(field3D(4,2,1),129)
   ASSERT_EQUAL(field3D(4,2,2),129)

END TEST

TEST test_get3DfieldNC_10

   ! Test for get3DfieldNC
   ! get3DfieldNC --> 3D
   REAL, ALLOCATABLE, DIMENSION(:,:,:):: field3D

   PRINT *, ' * Test get3DfieldNC    : subdomain SB (ts)'

   ! Grid size
   imt = 6; jmt = 2
   imtdom = 6; jmtdom = 2

   ! Define subdomain
   l_subdom = .TRUE.

   imaxdom =  2; imindom = 5
   jmaxdom =  2; jmindom = 1

   ! Define the two sub-subdomain
   imthalf1 = imt - imindom + 1
   imthalf2 = imaxdom

   ! Recalculate the values of imt and jmt
   imt = imthalf1 + imthalf2
   jmt = jmaxdom - jmindom + 1

   ! Allocate field2D
   ALLOCATE(field3D(imt,jmt,2))

   field3D = get3DfieldNC('pres_temp_4D.nc','temperature',[imindom,jmindom,1,1],[imt,jmt,2,1],'ts')

   ASSERT_EQUAL(SIZE(field3D,1),  4)
   ASSERT_EQUAL(SIZE(field3D,2),  2)
   ASSERT_EQUAL(SIZE(field3D,3),  2)
   ASSERT_EQUAL(field3D(1,1,1),57)
   ASSERT_EQUAL(field3D(1,1,2),57)
   ASSERT_EQUAL(field3D(4,2,1),93)
   ASSERT_EQUAL(field3D(4,2,2),93)

END TEST

TEST test_get3DfieldNC_11

   ! Test for get3DfieldNC
   ! get3DfieldNC --> 3D
   REAL, ALLOCATABLE, DIMENSION(:,:,:):: field3D

   PRINT *, ' * Test get3DfieldNC    : subdomain RB (ts_r)'

   ! Define subdomain
   l_subdom = .TRUE.

   imaxdom =  2; imindom = 1
   jmaxdom =  2; jmindom = 1

   imt = imaxdom - imindom + 1
   jmt = jmaxdom - jmindom + 1

   ! Allocate field2D
   ALLOCATE(field3D(imt,jmt,6))

   field3D = get3DfieldNC('pres_temp_4D.nc','temperature',[imindom,jmindom,1,1],[imt,jmt,6,1],'ts_r')

   ASSERT_EQUAL(SIZE(field3D,1),  2)
   ASSERT_EQUAL(SIZE(field3D,2),  2)
   ASSERT_EQUAL(SIZE(field3D,3),  6)
   ASSERT_EQUAL(field3D(1,1,1),9)
   ASSERT_EQUAL(field3D(2,1,1),9)
   ASSERT_EQUAL(field3D(1,2,6),141)
   ASSERT_EQUAL(field3D(2,2,6),141)


END TEST

TEST test_get3DfieldNC_12

   ! Test for get3DfieldNC
   ! get3DfieldNC --> 3D
   REAL, ALLOCATABLE, DIMENSION(:,:,:):: field3D

   PRINT *, ' * Test get3DfieldNC    : subdomain SB (ts_r)'

   ! Grid size
   imt = 2; jmt = 2
   imtdom = 2; jmtdom = 2

   ! Define subdomain
   l_subdom = .TRUE.

   imaxdom =  1; imindom = 2
   jmaxdom =  2; jmindom = 1

   ! Define the two sub-subdomain
   imthalf1 = imt - imindom + 1
   imthalf2 = imaxdom

   ! Recalculate the values of imt and jmt
   imt = imthalf1 + imthalf2
   jmt = jmaxdom - jmindom + 1

   ! Allocate field2D
   ALLOCATE(field3D(imt,jmt,6))

   field3D = get3DfieldNC('pres_temp_4D.nc','temperature',[imindom,jmindom,1,1],[imt,jmt,6,1],'ts_r')

   ASSERT_EQUAL(SIZE(field3D,1),  2)
   ASSERT_EQUAL(SIZE(field3D,2),  2)
   ASSERT_EQUAL(SIZE(field3D,3),  6)
   ASSERT_EQUAL(field3D(1,1,1),9)
   ASSERT_EQUAL(field3D(2,1,1),9)
   ASSERT_EQUAL(field3D(1,2,6),141)
   ASSERT_EQUAL(field3D(2,2,6),141)


END TEST

end test_suite
