test_suite mod_error

! test_suite for mod_error
! Contains 6 tests
! 6 --> errorCheck

SETUP

  ! Allocate one trajectory
  ntrac = 1;  ALLOCATE ( trajectories(ntrac))

  ! Set trajectory's niter
  trajectories(1)%niter  = 1
  trajectories(1)%active = .TRUE.


  ! Size of grid
  imt = 10; jmt = 10
  imtdom = 10; jmtdom = 10
  km = 10

  ! Original position
  ib = 1; ja =1; jb =1; y0 =1; y1 = 1
  ka = 1; kb =1; z0 =1; z1 =1 

END SETUP

TEARDOWN
  ! This code runs immediately after each test
  DEALLOCATE(trajectories)
END TEARDOWN

TEST test_errorCheck_1

   ! Test for errorCheck
   ! Infinite loop ---> errorCheck ---> 1

   INTEGER :: errCodePass ! Pass error code
   INTEGER :: errCodeFail ! Fail error code

   PRINT *, ' * Test errorCheck  : Infinite loop (infLoopError)'

   ! niter < nitermax
   niter = 100
   CALL errorCheck('infLoopError',errCodePass)

   ! niter > nitermaxx
   niter = 40000
   CALL errorCheck('infLoopError',errCodeFail)

   ASSERT_EQUAL(errCodePass,0)
   ASSERT_EQUAL(errCodeFail,1)

   ASSERT_FALSE(trajectories(ntrac)%active)

END TEST

TEST test_errorCheck_2

   ! Test for errorCheck
   ! Negative volume ---> errorCheck ---> 2

   INTEGER :: errCodePass ! Pass error code
   INTEGER :: errCodeFail ! Fail error code

   PRINT *, ' * Test errorCheck  : Volume error (dxyzError)'

   ! positive volume
   dxyz = 1000
   CALL errorCheck('dxyzError',errCodePass)

   ! negative volume
   dxyz = -10
   CALL errorCheck('dxyzError',errCodeFail)

   ASSERT_EQUAL(errCodePass,0)
   ASSERT_EQUAL(errCodeFail,2)

   ASSERT_FALSE(trajectories(ntrac)%active)

END TEST

TEST test_errorCheck_3

   ! Test for errorCheck
   ! Trajectory outside domain ---> errorCheck ---> 3

   INTEGER :: errCodePass ! Pass error code
   INTEGER :: errCodeFail ! Fail error code

   PRINT *, ' * Test errorCheck  : Boundary error (boundError)'

   ! trajectory in the domain
   ia = imt
   CALL errorCheck('boundError',errCodePass)

   ! trajectory out the domain
   ia = -1
   CALL errorCheck('boundError',errCodeFail)

   ASSERT_EQUAL(errCodePass,0)
   ASSERT_EQUAL(errCodeFail,3)

   ASSERT_FALSE(trajectories(ntrac)%active)

END TEST

TEST test_errorCheck_4

   ! Test for errorCheck
   ! Land error ---> errorCheck ---> 4

   INTEGER :: errCodePass ! Pass error code
   INTEGER :: errCodeFail ! Fail error code

   PRINT *, ' * Test errorCheck  : Land error (landError)'

   ! Allocate kmt
   ALLOCATE(kmt(imt,jmt))

   ! No land
   kmt(:,:) = 1
   CALL errorCheck('landError',errCodePass)

   ! Trajectory hits land
   kmt(:,:) = 0
   CALL errorCheck('landError',errCodeFail)

   ASSERT_EQUAL(errCodePass,0)
   ASSERT_EQUAL(errCodeFail,4)

   ASSERT_FALSE(trajectories(ntrac)%active)

END TEST

TEST test_errorCheck_5

   ! Test for errorCheck
   ! Wrong box error ---> errorCheck ---> 5/6

   INTEGER :: errCodePass1, errCodePass2 ! Pass error code
   INTEGER :: errCodeFail1, errCodeFail2 ! Fail error code

   PRINT *, ' * Test errorCheck  : Wrong box error (coordboxError)'

   ! Correct box
   x1 = 8.5; ib = 9
   CALL errorCheck('coordboxError',errCodePass1)

   y1 = 5.5; jb = 6
   CALL errorCheck('coordboxError',errCodePass2)

   ! Wrong box
   x1 = 8.5; ib = 10
   CALL errorCheck('coordboxError',errCodeFail1)

   x1 = 8.5; ib = 9
   y1 = 5.5; jb = 7
   CALL errorCheck('coordboxError',errCodeFail2)

   ASSERT_EQUAL(errCodePass1,0)
   ASSERT_EQUAL(errCodePass2,0)
   ASSERT_EQUAL(errCodeFail1,5)
   ASSERT_EQUAL(errCodeFail2,6)

   ASSERT_FALSE(trajectories(ntrac)%active)

END TEST

TEST test_errorCheck_6

   ! Test for errorCheck
   ! Unknown path ---> errorCheck ---> 8

   INTEGER :: errCodePass ! Pass error code
   INTEGER :: errCodeFail ! Fail error code

   PRINT *, ' * Test errorCheck  : Unknown path (dsCrossError)'

   ! Right path
   ds = 1.d0
   CALL errorCheck('dsCrossError',errCodePass)

   ! Unknown path
   ds = UNDEF
   CALL errorCheck('dsCrossError',errCodeFail)

   ASSERT_EQUAL(errCodePass,0)
   ASSERT_EQUAL(errCodeFail,8)

   ASSERT_FALSE(trajectories(ntrac)%active)

END TEST

end test_suite
