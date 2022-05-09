test_suite mod_seed

! test_suite for mod_seed
! Contains  18 tests
! 8  -> init_seed
! 10 -> seed


SETUP
    ! Define the grid
    imt = 20; jmt = 20; km = 10

    ! Seeding options
    nqua = 1
    partQuant = 1
    nff = 1

    ! Allocate mask/kmt
    ALLOCATE(mask(imt,jmt))
    ALLOCATE(kmt(imt,jmt))
    mask(:,:) = 1
    kmt(:,:) = km

    ! Allocate fluxes
    ALLOCATE(uflux(imt,jmt,km,2))
    uflux(:,:,:,:) = 1.

    ! Seeding area
    ist1           = 1;	ist2           = 10
    jst1           = 1; jst2           = 10
    kst1           = 1; kst2           = 2

    tst1           = 1; tst2           = 10

    ! seedType/idir/isec
    seedType = 1; seedTime = 1
    isec = 1
    idir = 1

    ! seedDir
    seedDir = ''

    ! ntrac
    ntractot = 0
    ntrac    = 0

    ! Time step
    tseas = 60.

END SETUP

TEARDOWN
    ! This code runs immediately after each test
    IF (ALLOCATED(mask))         DEALLOCATE(mask)
    IF (ALLOCATED(kmt))          DEALLOCATE(kmt)
    IF (ALLOCATED(uflux))        DEALLOCATE(uflux)
    IF (ALLOCATED(seed_ijk))     DEALLOCATE(seed_ijk)
    IF (ALLOCATED(seed_set))     DEALLOCATE(seed_set)
    IF (ALLOCATED(seed_tim))     DEALLOCATE(seed_tim)
    IF (ALLOCATED(trajectories)) DEALLOCATE(trajectories)
    IF (ALLOCATED(nsavewrite))   DEALLOCATE(nsavewrite)
END TEARDOWN

TEST test_init_seed_1

   ! Test for init_seed
   ! ist,jst,kst --> init_seed --> nsdMax

   PRINT *, ' * Test init_seed (seed 1): nsdMax (nomask)'

   CALL init_seed

   ASSERT_EQUAL(nsdmax, 200)

END TEST

TEST test_init_seed_2

   ! Test for init_seed
   ! ist,jst,kst --> init_seed --> nsdMax

   PRINT *, ' * Test init_seed (seed 1): nsdMax (mask)'

   ! Fill mask
   mask(6:,:) = 0
   mask(:,6:) = 0

   CALL init_seed

   ASSERT_EQUAL(nsdmax, 50)

END TEST

TEST test_init_seed_3

   ! Test for init_seed
   ! tst --> init_seed --> nsdTim

   PRINT *, ' * Test init_seed (seed 1): nsdTim (seedTime 1)'

   CALL init_seed

   ASSERT_EQUAL(nsdTim,10)
   ASSERT_EQUAL(seed_tim(1), 1)
   ASSERT_EQUAL(seed_tim(10),10)

END TEST

TEST test_init_seed_4

   ! Test for init_seed
   ! tst --> init_seed --> nsdTim

   PRINT *, ' * Test init_seed (seed 1): nsdTim (seedTime 2)'

   ! seedTime
   seedtime = 2
   timeFile = 'seedTime'

   CALL init_seed

   ASSERT_EQUAL(nsdTim,6)
   ASSERT_EQUAL(seed_tim(1), 1)
   ASSERT_EQUAL(seed_tim(6),24)

END TEST

TEST test_init_seed_5

   ! Test for init_seed
   ! init_seed --> nsdMax

   PRINT *, ' * Test init_seed (seed 2): nsdMax'

   ! seedType
   seedType = 2
   seedFile = 'seedFile'

   CALL init_seed

   ASSERT_EQUAL(nsdMax,4)

END TEST

TEST test_init_seed_6

   ! Test for init_seed
   ! init_seed --> seed_ijk/seed_set

   PRINT *, ' * Test init_seed (seed 2): seed_ijk/seed_set'

   ! seedType
   seedType = 2
   seedFile = 'seedFile'

   CALL init_seed

   ASSERT_EQUAL(seed_ijk(4,1),12)
   ASSERT_EQUAL(seed_ijk(3,2),7)
   ASSERT_EQUAL(seed_ijk(2,3),10)
   ASSERT_EQUAL(seed_set(1,1),1)
   ASSERT_EQUAL(seed_set(2,2),-1)

END TEST

TEST test_init_seed_7

   ! Test for init_seed
   ! tst --> init_seed --> nsdTim

   PRINT *, ' * Test init_seed (seed 2): nsdTim (seedTime 1)'

   ! seedType
   seedType = 2
   seedFile = 'seedFile'

   CALL init_seed

   ASSERT_EQUAL(nsdTim,10)
   ASSERT_EQUAL(seed_tim(1), 1)
   ASSERT_EQUAL(seed_tim(10),10)

END TEST

TEST test_init_seed_8

   ! Test for init_seed
   ! tst --> init_seed --> nsdTim

   PRINT *, ' * Test init_seed (seed 2): nsdTim (seedTime 2)'

   ! seedTime
   seedtime = 2
   timeFile = 'seedTime'

   ! seedType
   seedType = 2
   seedFile = 'seedFile'

   CALL init_seed

   ASSERT_EQUAL(nsdTim,6)
   ASSERT_EQUAL(seed_tim(1), 1)
   ASSERT_EQUAL(seed_tim(6),24)

END TEST

TEST test_seed_1

   ! Test for seed
   ! seed --> findtime

   PRINT *, ' * Test seed (seedTime 1) : findTime (ints in range)'

   ! Time step
   ints = 2

   CALL init_seed
   CALL seed

   ASSERT_EQUAL(itim,2)

END TEST

TEST test_seed_2

   ! Test for seed
   ! seed --> findtime

   PRINT *, ' * Test seed (seedTime 2) : findTime (ints in range)'

   ! seedTime
   seedTime = 2

   ! Time step
   ints = 3

   CALL init_seed
   CALL seed

   ASSERT_EQUAL(itim,3)

END TEST

TEST test_seed_3

   ! Test for seed
   ! seed --> findtime

   PRINT *, ' * Test seed (seedTime 1) : findTime (ints out range)'

   ! Time step
   ints = 12

   CALL init_seed
   CALL seed

   ASSERT_EQUAL(itim,-1)

END TEST

TEST test_seed_4

   ! Test for seed
   ! seed --> findtime

   PRINT *, ' * Test seed (seedTime 2) : findTime (ints out range)'

   ! seedTime
   seedTime = 2

   ! Time step
   ints = 10

   CALL init_seed
   CALL seed

   ASSERT_EQUAL(itim,-1)

END TEST

TEST test_seed_5

   ! Test for seed
   ! seed --> x,y,z,subvol

   PRINT *, ' * Test seed (seedType 1) : x,y,z,subvol'


   ! Time step
   ints = 2

   ! Uflux
   uflux(:,:,:,:) = 10.

   CALL init_seed
   CALL seed

   ASSERT_EQUAL(itim,2)
   ASSERT_EQUAL(x1,10.)
   ASSERT_EQUAL(y1,9.5)
   ASSERT_EQUAL(z1,1.5)
   ASSERT_EQUAL(subvol,10.)

END TEST

TEST test_seed_6

   ! Test for seed
   ! seed --> x,y,z,subvol

   PRINT *, ' * Test seed (seedType 2) : x,y,z,subvol'


   ! Time step
   ints = 10

   !seedType
   seedType = 2

   ! Uflux
   uflux(:,:,:,:) = 10.

   CALL init_seed
   CALL seed

   ASSERT_EQUAL(itim,10)
   ASSERT_EQUAL(x1,12.)
   ASSERT_EQUAL(y1,13.5)
   ASSERT_EQUAL(z1,6.5)
   ASSERT_EQUAL(subvol,10.)

END TEST

TEST test_seed_7

   ! Test for seed
   ! seed --> partQuant

   PRINT *, ' * Test seed              : nqua partQuant(10)'

   ! Time step
   ints = 2

   ! Uflux
   uflux(:,:,:,:) = 10.

   !partQuant
   partQuant = 10
   nqua = 2

   CALL init_seed
   CALL seed

   ASSERT_EQUAL(itim,2)
   ASSERT_EQUAL(x1,10.)
   ASSERT_EQUAL(y1,9.5)
   ASSERT_EQUAL(z1,1.5)
   ASSERT_EQUAL(subvol,10.)

END TEST

TEST test_seed_8

   ! Test for seed
   ! seed --> partQuant

   PRINT *, ' * Test seed              : nqua partQuant(1)'

   ! Time step
   ints = 2

   ! Uflux
   uflux(:,:,:,:) = 10.

   !partQuant
   partQuant = 1
   nqua = 2

   CALL init_seed
   CALL seed

   ASSERT_EQUAL(itim,2)
   ASSERT_EQUAL(x1,10.)
   ASSERT_EQUAL(y1,9.875)
   ASSERT_EQUAL(z1,1.875d0)
   ASSERT_EQUAL(subvol,0.625)

END TEST

TEST test_seed_9

   ! Test for seed
   ! seed --> trajectories

   PRINT *, ' * Test seed              : trajectories'

   ! Uflux
   uflux(:,:,:,:) = 10.

   CALL init_seed
   DO ints = 1, 20
      CALL seed
   END DO

   ASSERT_EQUAL(itim,-1)
   ASSERT_EQUAL(ntractot,2000)
   ASSERT_EQUAL(trajectories(1)%x1,1.)
   ASSERT_EQUAL(trajectories(ntrac)%x1,10.)
   ASSERT_EQUAL(trajectories(ntrac)%y1,9.5)
   ASSERT_EQUAL(trajectories(ntrac)%z1,1.5d0)
   ASSERT_EQUAL(trajectories(ntrac)%subvol,10.)
   ASSERT_EQUAL(trajectories(ntrac)%tt,540.d0)
   ASSERT_EQUAL(trajectories(1)%t0,0.d0)
   ASSERT_EQUAL(trajectories(ntrac)%t0,540.d0)
   ASSERT_EQUAL(trajectories(ntrac)%nts,9)
   ASSERT_EQUAL(trajectories(ntrac)%ib,11)
   ASSERT_EQUAL(trajectories(ntrac)%jb,10)
   ASSERT_EQUAL(trajectories(ntrac)%kb,2)
   ASSERT_TRUE(trajectories(ntrac)%active)

END TEST

TEST test_seed_10

   ! Test for seed
   ! seed --> loneparticle

   PRINT *, ' * Test seed              : loneparticle'

   ! Uflux
   uflux(:,:,:,:) = 10.

   ! Loneparticle
   loneparticle = 1000

   CALL init_seed
   DO ints = 1, 20
      CALL seed
   END DO

   ASSERT_FALSE(trajectories(1)%active)
   ASSERT_FALSE(trajectories(ntrac)%active)
   ASSERT_TRUE(trajectories(1000)%active)

END TEST

end test_suite
