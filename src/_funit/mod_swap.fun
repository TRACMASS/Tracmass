test_suite mod_swap

! test_suite for mod_swap
! Contains 2 tests
!  1 --> swap_time
!  1 --> swap_sign

SETUP
  ! Grid size
  INTEGER :: imt, jmt, km

  imt = 10; jmt = 10; km = 5

  ! Allocate fluxes
  ALLOCATE(uflux(imt,jmt,km,2), vflux(imt,jmt,km,2))
  ALLOCATE(dzt(imt,jmt,km,3),dzu(imt,jmt,km,2),dzv(imt,jmt,km,2))
  ALLOCATE(dzdt(imt,jmt,km,2))
  ALLOCATE(hs(imt,jmt,-1:1),zstot(imt,jmt,-1:1))

  uflux(:,:,:,:) = 0.;
  vflux(:,:,:,:) = 0.;
  dzt(:,:,:,:) = 0.;
  dzu(:,:,:,:) = 0.;
  dzv(:,:,:,:) = 0.;
  dzdt(:,:,:,:) = 0.;
  hs(:,:,:) = 0.;
  zstot(:,:,:) = 0.;

  ! Tracers
  l_tracers = .FALSE.

END SETUP

TEARDOWN
  ! This code runs immediately after each test
  DEALLOCATE(uflux, vflux)
  DEALLOCATE(dzt, dzu, dzv)
  DEALLOCATE(dzdt)
  DEALLOCATE(hs, zstot)
END TEARDOWN

TEST test_swap_time

   ! Test for swap_time
   ! 2 ---> swap_time ---> 1

   PRINT *, ' * Test swap_time '

   ! Define fluxes
   uflux(:,:,:,2) = 10.;
   vflux(:,:,:,2) = 10.;
   dzt(:,:,:,2) = 5.;  dzt(:,:,:,3) = 10.;
   dzu(:,:,:,2) = 10.;
   dzv(:,:,:,2) = 10.;
   dzdt(:,:,:,2) = 10.;
   hs(:,:,0) = 5.;     hs(:,:,1) = 10.;
   zstot(:,:,0) = 5.;  zstot(:,:,1) = 10.;

   ! Swap indexes
   CALL swap_time()

   ASSERT_EQUAL(uflux(1,1,1,1),10.)
   ASSERT_EQUAL(vflux(1,1,1,1),10.)
   ASSERT_EQUAL(dzt(1,1,1,1),5.)
   ASSERT_EQUAL(dzt(1,1,1,2),10.)
   ASSERT_EQUAL(dzu(1,1,1,1),10.)
   ASSERT_EQUAL(dzv(1,1,1,1),10.)
   ASSERT_EQUAL(dzdt(1,1,1,1),10.)
   ASSERT_EQUAL(hs(1,1,-1),5.)
   ASSERT_EQUAL(hs(1,1,0),10.)
   ASSERT_EQUAL(zstot(1,1,-1),5.)
   ASSERT_EQUAL(zstot(1,1,0),10.)

END TEST

TEST test_swap_sign

   ! Test for swap_sign
   ! nff ---> swap_sign

   PRINT *, ' * Test swap_time '

   ! Define fluxes
   uflux(:,:,:,2) = 10.;
   vflux(:,:,:,2) = 10.;

   ! Forward
   nff = 1

   ! Swap sign
   CALL swap_sign()

   ASSERT_EQUAL(uflux(1,1,1,2),10.)
   ASSERT_EQUAL(vflux(1,1,1,2),10.)

   ! Backward
   nff = -1

   ! Swap sign
   CALL swap_sign()

   ASSERT_EQUAL(uflux(1,1,1,2),-10.)
   ASSERT_EQUAL(vflux(1,1,1,2),-10.)


END TEST

end test_suite
