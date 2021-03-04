test_suite mod_pos

! test_suite for mod_pos_tstep
! Contains 16 tests
! 6 --> cross_time
! 3 --> calc_pos
! 7 --> update_traj

SETUP

  ! Time interpolation
  intrpg = 0.5; intrpr = 0.5

  ! Grid information
  imt = 10; jmt = 10; km = 10

  ! Allocate uflux
  ALLOCATE(uflux(10,10,10,2))
  ALLOCATE(vflux(10,10,10,2))
  ALLOCATE(wflux(10,2))

  vflux(:,:,:,:) = 0.; wflux(:,:) = 0.

  ! Velocities
  dsn = 0.; dss = 0.; dsu = 0.; dsd = 0.

  ! No stream functions
  l_psi     = .FALSE.
  l_offline = .TRUE.

  ! jperio
  jperio = 0

END SETUP

TEARDOWN
  ! This code runs immediately after each test
  DEALLOCATE(uflux, vflux, wflux)
END TEARDOWN

TEST test_cross_time_1

   ! Test for cross_time
   ! uu=um>0 ---> cross_time ---> dse

   PRINT *, ' * Test cross_time  :  eastward/northward/upward (uu = um) '

   uflux(:,:,:,:) =  1.

   CALL cross_time(1,2,5,5,1.5d0,dse,dsw)

   ASSERT_EQUAL(dse, 0.5)
   ASSERT_EQUAL(dsw, UNDEF)

END TEST

TEST test_cross_time_2

   ! Test for cross_time
   ! uu/=um ---> cross_time ---> dse

   PRINT *, ' * Test cross_time  :  eastward/northward/upward (uu / um) '

   uflux(:,:,:,:) =  1.
   uflux(1,:,:,:) =  2.

   CALL cross_time(1,2,5,5,1.5d0,dse,dsw)

   ASSERT_EQUAL(dse, DLOG(1.5d0))
   ASSERT_EQUAL(dsw, UNDEF)

END TEST

TEST test_cross_time_3

   ! Test for cross_time
   ! uu=um ---> cross_time ---> dsw

   PRINT *, ' * Test cross_time  :  westward/southward/downward (uu = um) '

   uflux(:,:,:,:) =  -1.

   CALL cross_time(1,2,5,5,1.5d0,dse,dsw)

   ASSERT_EQUAL(dsw, 0.5)
   ASSERT_EQUAL(dse, UNDEF)

END TEST

TEST test_cross_time_4

   ! Test for cross_time
   ! uu/=um ---> cross_time ---> dsw

   PRINT *, ' * Test cross_time  :  westward/southward/downward (uu /= um) '

   uflux(:,:,:,:) =  -1.
   uflux(1,:,:,:) =  -2.

   CALL cross_time(1,2,5,5,1.5d0,dse,dsw)

   ASSERT_EQUAL_WITHIN(dsw, DLOG(1/0.75d0),1e-6)
   ASSERT_EQUAL(dse, UNDEF)

END TEST

TEST test_cross_time_5

   ! Test for cross_time
   ! convergence ---> cross_time ---> UNDEF

   PRINT *, ' * Test cross_time  :  convergence'

   uflux(:,:,:,:) =  -1.
   uflux(1,:,:,:) =   1.

   CALL cross_time(1,2,5,5,1.5d0,dse,dsw)

   ASSERT_EQUAL(dsw, UNDEF)
   ASSERT_EQUAL(dse, UNDEF)

END TEST

TEST test_cross_time_6

   ! Test for cross_time
   ! divergence ---> cross_time ---> UNDEF

   PRINT *, ' * Test cross_time  :  divergence'

   uflux(:,:,:,:) =   1.
   uflux(1,:,:,:) =  -1.

   CALL cross_time(1,2,5,5,1.5d0,dse,dsw)

   ASSERT_EQUAL(dsw, UNDEF)
   ASSERT_EQUAL(dse, UNDEF)

END TEST

TEST test_calc_pos_1

   ! Test for calc_pos

   PRINT *, ' * Test calc_pos    :  uu = um > 0'

   uflux(:,:,:,:) =   1.

   CALL cross_time(1,2,5,5,1.5d0,dse,dsw)
   CALL calc_pos(1,2,5,5,1.5d0,x1,dse)

   ASSERT_EQUAL(x1, 2.0d0)

END TEST

TEST test_calc_pos_2

   ! Test for calc_pos

   PRINT *, ' * Test calc_pos    :  uu = um < 0'

   uflux(:,:,:,:) =   -1.

   CALL cross_time(1,2,5,5,1.5d0,dse,dsw)
   CALL calc_pos(1,2,5,5,1.5d0,x1,dsw)

   ASSERT_EQUAL(x1, 1.0d0)

END TEST

TEST test_calc_pos_3

   ! Test for calc_pos

   PRINT *, ' * Test calc_pos    :  convergence'

   uflux(:,:,:,:) =  -1.
   uflux(1,:,:,:) =   1.

   CALL cross_time(1,2,5,5,1.5d0,dse,dsw)
   CALL calc_pos(1,2,5,5,1.5d0,x1,dsw)

   ASSERT_EQUAL(x1, 1.5d0)

   CALL cross_time(1,2,5,5,1.4d0,dse,dsw)
   CALL calc_pos(1,2,5,5,1.4d0,x1,dsw)

   ASSERT_EQUAL(x1, 1.5d0)

   CALL cross_time(1,2,5,5,1.6d0,dse,dsw)
   CALL calc_pos(1,2,5,5,1.6d0,x1,dse)

   ASSERT_EQUAL(x1, 1.5d0)

END TEST

TEST test_update_traj_1

   ! Test for update_traj

   PRINT *, ' * Test update_traj :  eastward (uu = um)'


   uflux(:,:,:,:) =  1.

   ! Original position
   x0 = 1.6d0; y0 = 5.d0; z0 = 5.d0

   ! Initial position indexes
   ia = 2; iam = 1; ib =2;
   ja = 5; jb = 5;
   ka = 5; kb = 5;

   CALL cross_time(1,2,5,5,x0,dse,dsw)

   ds = MIN(dse,dsw)

   CALL update_traj(ia,iam,ja,ka,ib,jb,kb,x0,y0,z0,x1,y1,z1)

   ASSERT_EQUAL(x1,2.0d0)
   ASSERT_EQUAL(y1,5.0d0)
   ASSERT_EQUAL(z1,5.0d0)
   ASSERT_EQUAL(ib,3)

END TEST

TEST test_update_traj_2

   ! Test for update_traj

   PRINT *, ' * Test update_traj :  eastward (uu /= um)'


   uflux(:,:,:,:) =  1.
   uflux(1,:,:,:) =  2.

   ! Original position
   x0 = 1.6d0; y0 = 5.d0; z0 = 5.d0

   ! Initial position indexes
   ia = 2; iam = 1; ib =2;
   ja = 5; jb = 5;
   ka = 5; kb = 5;

   CALL cross_time(1,2,5,5,x0,dse,dsw)

   ds = MIN(dse,dsw)

   CALL update_traj(ia,iam,ja,ka,ib,jb,kb,x0,y0,z0,x1,y1,z1)

   ASSERT_EQUAL(x1,2.0d0)
   ASSERT_EQUAL(y1,5.0d0)
   ASSERT_EQUAL(z1,5.0d0)
   ASSERT_EQUAL(ib,3)

END TEST

TEST test_update_traj_3

   ! Test for update_traj

   PRINT *, ' * Test update_traj :  westward (uu = um)'


   uflux(:,:,:,:) =  -1.

   ! Original position
   x0 = 1.6d0; y0 = 5.d0; z0 = 5.d0

   ! Initial position indexes
   ia = 2; iam = 1; ib =2;
   ja = 5; jb = 5;
   ka = 5; kb = 5;

   CALL cross_time(1,2,5,5,x0,dse,dsw)

   ds = MIN(dse,dsw)

   CALL update_traj(ia,iam,ja,ka,ib,jb,kb,x0,y0,z0,x1,y1,z1)

   ASSERT_EQUAL(x1,1.0d0)
   ASSERT_EQUAL(y1,5.0d0)
   ASSERT_EQUAL(z1,5.0d0)
   ASSERT_EQUAL(ib,1)

END TEST

TEST test_update_traj_4

   ! Test for update_traj

   PRINT *, ' * Test update_traj :  westward (uu /= um)'


   uflux(:,:,:,:) =  -1.
   uflux(1,:,:,:) =  -2.

   ! Original position
   x0 = 1.6d0; y0 = 5.d0; z0 = 5.d0

   ! Initial position indexes
   ia = 2; iam = 1; ib =2;
   ja = 5; jb = 5;
   ka = 5; kb = 5;

   CALL cross_time(1,2,5,5,x0,dse,dsw)

   ds = MIN(dse,dsw)

   CALL update_traj(ia,iam,ja,ka,ib,jb,kb,x0,y0,z0,x1,y1,z1)

   ASSERT_EQUAL(x1,1.0d0)
   ASSERT_EQUAL(y1,5.0d0)
   ASSERT_EQUAL(z1,5.0d0)
   ASSERT_EQUAL(ib,1)

END TEST

TEST test_update_traj_5

   ! Test for update_traj

   PRINT *, ' * Test update_traj :  corners'


   uflux(:,:,:,:) =  1.
   vflux(:,:,:,:) =  1.

   ! Original position
   x0 = 1.5d0; y0 = 4.5d0; z0 = 5.d0

   ! Initial position indexes
   ia = 2; iam = 1; ib =2;
   ja = 5; jb = 5;
   ka = 5; kb = 5;

   CALL cross_time(1,2,5,5,x0,dse,dsw)
   CALL cross_time(2,2,5,5,y0,dsn,dss)

   ds = MIN(dse,dsw,dsn,dss)

   CALL update_traj(ia,iam,ja,ka,ib,jb,kb,x0,y0,z0,x1,y1,z1)

   ASSERT_EQUAL(x1,2.0d0)
   ASSERT_EQUAL(y1,5.0d0)
   ASSERT_EQUAL(z1,5.0d0)
   ASSERT_EQUAL(ib,3)
   ASSERT_EQUAL(jb,6)

END TEST

TEST test_update_traj_6

   ! Test for update_traj

   PRINT *, ' * Test update_traj :  boundary condition -->'

   uflux(:,:,:,:) =  1.

   ! Zonal periodicity
   iperio = 1

   ! Original position
   x0 = 9.5d0; y0 = 5.d0; z0 = 5.d0

   ! Initial position indexes
   ia = 10; iam = 9; ib = 10;
   ja = 5; jb = 5;
   ka = 5; kb = 5;

   CALL cross_time(1,ia,ja,ka,x0,dse,dsw)

   ds = MIN(dse,dsw)

   CALL update_traj(ia,iam,ja,ka,ib,jb,kb,x0,y0,z0,x1,y1,z1)

   ! Update indexes
   x0  = x1
   y0  = y1
   z0  = z1

   ia  = ib
   iam = ia-1
   IF (iam == 0) iam = IMT
   ja  = jb
   ka  = kb

   CALL cross_time(1,ia,ja,ka,x0,dse,dsw)

   ds = MIN(dse,dsw)

   CALL update_traj(ia,iam,ja,ka,ib,jb,kb,x0,y0,z0,x1,y1,z1)

   ASSERT_EQUAL(x1,1.0d0)
   ASSERT_EQUAL(y1,5.0d0)
   ASSERT_EQUAL(z1,5.0d0)
   ASSERT_EQUAL(ib,2)
   ASSERT_EQUAL(jb,5)

END TEST

TEST test_update_traj_7

   ! Test for update_traj

   PRINT *, ' * Test update_traj :  boundary condition <--'

   uflux(:,:,:,:) =  -1.

   ! Zonal periodicity
   iperio = 1

   ! Original position
   x0 = 1.5d0; y0 = 5.d0; z0 = 5.d0

   ! Initial position indexes
   ia = 2; iam = 1; ib = 2;
   ja = 5; jb = 5;
   ka = 5; kb = 5;

   CALL cross_time(1,ia,ja,ka,x0,dse,dsw)

   ds = MIN(dse,dsw)

   CALL update_traj(ia,iam,ja,ka,ib,jb,kb,x0,y0,z0,x1,y1,z1)

   ! Update indexes
   x0  = x1
   y0  = y1
   z0  = z1

   ia  = ib
   iam = ia-1
   IF (iam == 0) iam = IMT
   ja  = jb
   ka  = kb

   CALL cross_time(1,ia,ja,ka,x0,dse,dsw)

   ds = MIN(dse,dsw)

   CALL update_traj(ia,iam,ja,ka,ib,jb,kb,x0,y0,z0,x1,y1,z1)

   ASSERT_EQUAL(x1,10.0d0)
   ASSERT_EQUAL(y1,5.0d0)
   ASSERT_EQUAL(z1,5.0d0)
   ASSERT_EQUAL(ib,10)
   ASSERT_EQUAL(jb,5)

END TEST

end test_suite
