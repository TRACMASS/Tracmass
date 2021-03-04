test_suite mod_clock

! test_suite for mod_clock
! Contains 4 tests

SETUP

  ! tseas (1h)
  tseas = 60*3600.

  ! Volume grid
  dxyz = 1.d0

  ! Number of subcycles
  iter = 4
  dtmin = REAL(tseas,8)/iter
  dsubstep = 1/REAL(iter,8)

  ! Current time step
  ts  = 1.6d0
  tt  = 1.6d0*tseas
  tss = 2.4d0

  !dtreg
  dtreg = dtmin*(DBLE(INT(DBLE(iter)*tt/tseas,8)) + &
                    1.d0 - DBLE(iter)*tt/tseas)

END SETUP

TEARDOWN
  ! This code runs immediately after each test
END TEARDOWN

TEST test_update_time_1

   ! Test for update_time

   PRINT *, ' * Test update_time : Exceed the time when fields change'

   ! Time step that exceeds the time when fields change
   ds = 0.5*tseas

   CALL update_time

   ASSERT_EQUAL_WITHIN(dt,0.4d0*tseas,1e-3)
   ASSERT_EQUAL(tt,2*tseas)
   ASSERT_EQUAL(ts,2)
   ASSERT_EQUAL(tss,4)
   ASSERT_EQUAL_WITHIN(dts,0.4d0,1e-3)

   ASSERT_EQUAL(intrpg,0)
   ASSERT_EQUAL(intrpr,1)

END TEST

TEST test_update_time_2

   ! Test for update_time

   PRINT *, ' * Test update_time : dt = dtmin'

   ! If the time step is equal to the minimum time step
   ds = dtmin

   CALL update_time

   ASSERT_EQUAL(tt,1.85d0*tseas)
   ASSERT_EQUAL(ts,1.85d0)
   ASSERT_EQUAL(tss,3.4d0)
   ASSERT_EQUAL_WITHIN(dts,0.25d0,1e-3)

   ASSERT_EQUAL_WITHIN(intrpg,0.85d0,1e-3)
   ASSERT_EQUAL_WITHIN(intrpr,0.15d0,1e-3)

END TEST

TEST test_update_time_3

   ! Test for update_time

   PRINT *, ' * Test update_time : dt = dtreg'

   ! If the time step is equal to the minimum time step
   ds = dtreg

   CALL update_time

   ASSERT_EQUAL_WITHIN(tt,1.75d0*tseas,1e-3)
   ASSERT_EQUAL(ts,1.75d0)
   ASSERT_EQUAL(tss,3.d0)
   ASSERT_EQUAL_WITHIN(dts,0.15d0,1e-3)

   ASSERT_EQUAL_WITHIN(intrpg,0.75d0,1e-3)
   ASSERT_EQUAL_WITHIN(intrpr,0.25d0,1e-3)

END TEST

TEST test_update_time_4

   ! Test for update_time

   PRINT *, ' * Test update_time : else'

   ! If the time step is equal to the minimum time step
   ds = 0.3d0*tseas

   CALL update_time

   ASSERT_EQUAL_WITHIN(tt,1.9d0*tseas,1e-6)
   ASSERT_EQUAL_WITHIN(ts,1.9d0,1e-6)
   ASSERT_EQUAL_WITHIN(tss,3.6d0,1e-6)
   ASSERT_EQUAL_WITHIN(dts,0.3d0,1e-6)

   ASSERT_EQUAL_WITHIN(intrpg,0.9d0,1e-6)
   ASSERT_EQUAL_WITHIN(intrpr,0.1d0,1e-6)

END TEST

end test_suite
