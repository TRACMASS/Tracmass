test_suite mod_vertvel

! test_suite for mod_vertvel
! Contains 3 tests
!  3 --> vertvel

SETUP
  ! Grid size
  INTEGER :: imt, jmt, km

  imt = 10; jmt = 10; km = 3

  ! Allocate fluxes
  ALLOCATE(uflux(imt,jmt,km,2), vflux(imt,0:jmt,km,2))
  ALLOCATE(dzdt(imt,jmt,km,2))
  ALLOCATE(dxdy(imt,jmt), kmt(imt,jmt))
  ALLOCATE(wflux(0:km,2))

  uflux(:,:,:,:) = 1.;
  vflux(:,:,:,:) = 0.;
  dzdt(:,:,:,:)  = 0.;
  dxdy(:,:)  = 1.;
  kmt(:,:)   = km;
  wflux(:,:) = 0.;

END SETUP

TEARDOWN
  ! This code runs immediately after each test
  DEALLOCATE(uflux, vflux)
  DEALLOCATE(wflux)
  DEALLOCATE(dzdt)
  DEALLOCATE(dxdy, kmt)
END TEARDOWN

TEST test_vertvel_1

   ! Test for vertvel
   ! u,v ---> vertvel ---> w

   PRINT *, ' * Test vertvel  : fix max  '

   ! Define fluxes
   uflux(2,:,1,:)   = -1.;
   uflux(1,:,2:3,:) = -1.;

   ! Compute vertical fluxes
   CALL vertvel(2,1,1,3)

   ASSERT_EQUAL(wflux(1,1),2.d0)
   ASSERT_EQUAL(wflux(2,1),0.d0)
   ASSERT_EQUAL(wflux(3,1),-2.d0)


END TEST

TEST test_vertvel_2

   ! Test for vertvel
   ! u,v ---> vertvel ---> w

   PRINT *, ' * Test vertvel  : vertical mask (kmt) '

   ! Define fluxes
   uflux(2,:,1,:)   = -1.;
   uflux(1,:,2:3,:) = -1.;

   ! Define km and kmt
   km = 3; kmt(:,:) = 2

   ! Compute vertical fluxes
   CALL vertvel(2,1,1,3)

   ASSERT_EQUAL(wflux(1,1),0.d0)
   ASSERT_EQUAL(wflux(2,1),-2.d0)
   ASSERT_EQUAL(wflux(3,1),-4.d0)


END TEST

TEST test_vertvel_3

   ! Test for vertvel
   ! u,v ---> vertvel ---> w

   PRINT *, ' * Test vertvel  : include mass change dm/dt '

   ! Define fluxes
   uflux(2,:,1,:)   = -1.;
   uflux(1,:,2:3,:) = -1.;

   ! Define km and kmt
   km = 3; kmt(:,:) = 2

   ! Include dzdt
   dzdt(:,:,2,:) = 1
   dzdt(:,:,3,:) = -1

   ! Compute vertical fluxes
   CALL vertvel(2,1,1,3)

   ASSERT_EQUAL(wflux(1,1),0.d0)
   ASSERT_EQUAL(wflux(2,1),-3.d0)
   ASSERT_EQUAL(wflux(3,1),-4.d0)


END TEST

end test_suite
