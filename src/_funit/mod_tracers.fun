test_suite mod_tracers

! test_suite for mod_tracers
! Contains 7 tests
! 1  --> init_tracer
! 1  --> compute_tracer
! 3  --> update_tracer
! 2  --> tracerbin

SETUP

  ! Grid size
  imt = 10; jmt = 10; km = 5

  ! resolution
  resolution = 11

  ! Tracer description
  tracername(1:2) = (/'tracer1', 'tracer2'/)
  tracerunit(1:2) = (/'unit1', 'unit2'/)
  tracermin(1:2) = (/0., 0./)
  tracermax(1:2) = (/10, 100/)
  traceraction(1:2) = (/'read   ','compute'/)
  tracervarname(1:2) = (/'tracername','          '/)
  tracerdimension(1:2) = (/'2 ','2d'/)

  ! Time resolution
  intrpg = 0.25; intrpr = 0.75

  ! tracertrajscale
  tracertrajscale = 10.d0

  ! Allocate trajectories
  ALLOCATE(trajectories(1))
  ALLOCATE(trajectories(1)%tracerval(10))

END SETUP

TEARDOWN

  ! Deallocate arrays
  IF (ALLOCATED(tracers)) DEALLOCATE(tracers, tracervalue, tracerbinvalue, dtracervalue)
  DEALLOCATE(trajectories)

END TEARDOWN

TEST test_init_tracer

   ! Test for init_tracer
   ! init_tracer --> tracers

   PRINT *, ' * Test init_tracer'

   CALL init_tracer

   ASSERT_EQUAL(numtracers,2)
   ASSERT_EQUAL(tracers(1)%name, 'tracer1')
   ASSERT_EQUAL(tracers(2)%unit, 'unit2')
   ASSERT_EQUAL(tracers(2)%minimum, 0.)
   ASSERT_EQUAL(tracers(2)%maximum, 100.)
   ASSERT_EQUAL(tracers(1)%action, 'read')
   ASSERT_EQUAL(tracers(2)%action, 'compute')
   ASSERT_EQUAL(tracers(1)%varname, 'tracername')
   ASSERT_EQUAL(tracers(1)%dimension, '2D')
   ASSERT_EQUAL(SIZE(tracers(2)%data,1), imt)
   ASSERT_EQUAL(SIZE(tracers(2)%data,2), jmt)
   ASSERT_EQUAL(SIZE(tracers(2)%data,3), 1)
   ASSERT_EQUAL(SIZE(tracers(2)%data,4), 2)
   ASSERT_EQUAL(dtracervalue(1),1 )
   ASSERT_EQUAL(dtracervalue(2),10 )
   ASSERT_EQUAL(tracertrajscale,1.d0 )


END TEST

TEST test_compute_tracer

   ! Test for compute_tracer
   ! compute_tracer --> tracer

   REAL(8), DIMENSION(imt,jmt,1) :: newtracer
   CHARACTER(len=100)            :: tracname

   PRINT *, ' * Test compute_tracer   : thermo_dens0'

   ! Name of variable
   tracname = 'sigma0'

   CALL init_tracer
   CALL compute_tracer(tracname,newtracer(:,:,:))

   ASSERT_EQUAL(newtracer(1,1,1),999.842594-1000.)


END TEST

TEST test_update_tracer_1

   ! Test for update_tracer
   ! tracervalue (old) update_tracer --> tracervalue (new)


   PRINT *, ' * Test update_tracer    : position within the grid box'

   CALL init_tracer

   tracers(1)%data(1,:,:,1) = 10
   tracers(1)%data(1,:,:,2) = 20
   tracers(1)%data(2,:,:,2) = 20
   tracers(1)%data(2,:,:,1) = 10

   ! Position of the trajectory
   ia = 1; ib = 2; ja = 1; jb = 2; ka = 1; kb = 2

   x1 = 1.5; y1 = 1.5; z1 = 1.5

   CALL update_tracer(1,ia,ja,ka,ib,jb,kb,x1,y1,z1)

   ASSERT_EQUAL(trajectories(1)%tracerval(1),17.5)


END TEST

TEST test_update_tracer_2

   ! Test for update_tracer
   ! tracervalue (old) update_tracer --> tracervalue (new)


   PRINT *, ' * Test update_tracer    : position boundary between two grid boxes'

   CALL init_tracer

   tracers(1)%data(1,:,:,1) = 10
   tracers(1)%data(1,:,:,2) = 20
   tracers(1)%data(2,:,:,1) = 20
   tracers(1)%data(2,:,:,2) = 10

   ! Reduce the test to a single tracer
   numtracers = 1

   ! Position of the trajectory
   ia = 1; ib = 2; ja = 1; jb = 2; ka = 1; kb = 2

   x1 = 2.; y1 = 1.5; z1 = 1.5

   CALL update_tracer(1,ia,ja,ka,ib,jb,kb,x1,y1,z1)

   ASSERT_EQUAL(trajectories(1)%tracerval(1),15.0)


END TEST

TEST test_update_tracer_3

   ! Test for update_tracer
   ! update_tracer --> tracervalue (new)


   PRINT *, ' * Test update_tracer    : tracerbin value'

   CALL init_tracer

   tracers(1)%data(1,:,:,1) = 2
   tracers(1)%data(1,:,:,2) = 5
   tracers(1)%data(2,:,:,1) = 6
   tracers(1)%data(2,:,:,2) = 3

   ! Reduce the test to a single tracer
   numtracers = 1

   ! Position of the trajectory
   ia = 1; ib = 2; ja = 1; jb = 2; ka = 1; kb = 2

   x1 = 2.; y1 = 1.5; z1 = 1.5

   CALL update_tracer(1,ia,ja,ka,ib,jb,kb,x1,y1,z1)

   ASSERT_EQUAL(trajectories(1)%tracerval(1),4.0)
   ASSERT_EQUAL(tracerbinvalue(1,2),5)


END TEST

TEST test_tracerbin_1

   ! Test for tracerbin
   ! tracer value --> tracerbin --> bin value

   INTEGER :: binvalue

   PRINT *, ' * Test tracerbin        : tracerbin value (inside range)'

   CALL init_tracer

   binvalue = tracerbin(2.5d0,1)
   ASSERT_EQUAL(binvalue,4)
   binvalue = tracerbin(2.2d0,1)
   ASSERT_EQUAL(binvalue,3)
   binvalue = tracerbin(5.d0,1)
   ASSERT_EQUAL(binvalue,6)

END TEST

TEST test_tracerbin_2

   ! Test for tracerbin
   ! tracer value --> tracerbin --> bin value

   INTEGER :: binvalue

   PRINT *, ' * Test tracerbin        : tracerbin value (outside range)'

   CALL init_tracer

   binvalue = tracerbin(-0.5d0,1)
   ASSERT_EQUAL(binvalue,1)
   binvalue = tracerbin(22.d0,1)
   ASSERT_EQUAL(binvalue,11)

END TEST

end test_suite
