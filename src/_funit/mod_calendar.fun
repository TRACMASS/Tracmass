test_suite mod_calendar

! Test_suite for mod_calendar
!
! This test suite contains 19 tests 
! for the following subroutine/functions:
!
! 1 -> init_calendar
! 2 -> previous_calendar
! 2 -> end_calendar
! 8 -> update_calendar
! 4 -> tt_calendar

SETUP

  ! Initial date 01/01/2000 00:00:00
  startYear = 2000; startMon = 1; startDay = 1
  startHour = 0;    startMin = 0; startSec = 0

  ! Initial step: 1 month
  ngcm_step = 1;    ngcm_unit = 5

  ! Forward in time
  nff = 1

  ! LoopYears deactivated
  loopYears = .FALSE.;  loopIndex = 0
  loopEndYear = 2000; loopStartYear = 1950

END SETUP

TEARDOWN
  ! This code runs immediately after each test
END TEARDOWN

TEST test_init_calendar

   ! Test for init_calendar
   ! StartDate ---> init_calendar ---> NextDate

   PRINT *, ' * Test init_calendar   '

   CALL init_calendar

   ASSERT_EQUAL(startYear, nextYear)
   ASSERT_EQUAL(startMon,   nextMon)
   ASSERT_EQUAL(startDay,   nextDay)
   ASSERT_EQUAL(startHour, nextHour)
   ASSERT_EQUAL(startMin,   nextMin)
   ASSERT_EQUAL(startSec,   nextSec)

END TEST

TEST test_end_calendar_1

   ! Test for end_calendar
   ! intrun ---> end_calendar ---> endDate

   PRINT*, ' * Test end_calendar      : nff =  1  '

   ! Number of time steps
   intrun = 13

   CALL end_calendar

   ASSERT_EQUAL(startYear +1 , endYear)
   ASSERT_EQUAL(startMon,       endMon)

END TEST

TEST test_end_calendar_2

   ! Test for end_calendar
   ! intrun ---> end_calendar ---> endDate

   PRINT*, ' * Test end_calendar      : nff = -1  '

   ! Backward in time
   nff = -1

   ! Number of time steps
   intrun = 13

   CALL end_calendar

   ASSERT_EQUAL(startYear -1 , endYear)
   ASSERT_EQUAL(startMon,       endMon)

END TEST

TEST test_previous_calendar_1

   ! Test for previous_calendar
   ! StartYear ---> previous_calendar ---> prevDate

   PRINT *, ' * Test previous_calendar : nff =  1   '

   CALL previous_calendar

   ASSERT_EQUAL(startYear -1 , prevYear)
   ASSERT_EQUAL(      12,       prevMon)
   ASSERT_EQUAL(startDay,       prevDay)
   ASSERT_EQUAL(startHour,     prevHour)
   ASSERT_EQUAL(startMin,       prevMin)
   ASSERT_EQUAL(startSec,       prevSec)

END TEST

TEST test_previous_calendar_2

   ! Test for previous_calendar
   ! StartYear ---> previous_calendar ---> prevDate

   PRINT *, ' * Test previous_calendar : nff = -1   '

   ! Reverse time
   nff = -1

   CALL previous_calendar

   ASSERT_EQUAL(startYear,     prevYear)
   ASSERT_EQUAL(       2,       prevMon)
   ASSERT_EQUAL(startDay,       prevDay)
   ASSERT_EQUAL(startHour,     prevHour)
   ASSERT_EQUAL(startMin,       prevMin)
   ASSERT_EQUAL(startSec,       prevSec)

END TEST

TEST test_update_calendar_1

   ! Test for update_calendar
   ! nextYear ---> update_calendar ---> currDate

   INTEGER :: nstep

   PRINT*, ' * Test update_calendar   : forward in time'

   CALL init_calendar

   DO nstep = 1, 12

      CALL update_calendar

      IF (nstep ==6 ) THEN
        ASSERT_EQUAL(6,  currMon)
        ASSERT_EQUAL(7,  nextMon)
      END IF

   END DO

   ASSERT_EQUAL(12,          currMon )
   ASSERT_EQUAL( 1,          nextMon )
   ASSERT_EQUAL(startYear,   currYear)
   ASSERT_EQUAL(startYear+1, nextYear)

   ASSERT_EQUAL(iyear, 2)

END TEST

TEST test_update_calendar_2

   ! Test for update_calendar
   ! nextYear ---> update_calendar ---> currDate

   INTEGER :: nstep

   PRINT*, ' * Test update_calendar   : forward in time (loop)'

   ! Activate Loop
   loopYears = .TRUE.

   CALL init_calendar

   DO nstep = 1, 12
      CALL update_calendar
   END DO

   ASSERT_EQUAL(12,          currMon )
   ASSERT_EQUAL( 1,          nextMon )

   ASSERT_EQUAL(startYear,   currYear)
   ASSERT_EQUAL(loopStartYear, nextYear)

   ASSERT_EQUAL(iyear, 2)

END TEST

TEST test_update_calendar_3

   ! Test for update_calendar
   ! nextYear ---> update_calendar ---> currDate

   INTEGER :: nstep

   PRINT*, ' * Test update_calendar   : forward in time (1d step)'

   ! Initial step: 1 day
   ngcm_step = 1;    ngcm_unit = 4

   CALL init_calendar

   DO nstep = 1, 365
      CALL update_calendar
   END DO

   ASSERT_EQUAL(31,       currDay )
   ASSERT_EQUAL(startDay, nextDay )
   ASSERT_EQUAL(12,       currMon )
   ASSERT_EQUAL( 1,       nextMon )

   ASSERT_EQUAL(startYear,   currYear)
   ASSERT_EQUAL(startYear+1, nextYear)

   ASSERT_EQUAL(iyear, 2)

END TEST

TEST test_update_calendar_4

   ! Test for update_calendar
   ! nextYear ---> update_calendar ---> currDate

   INTEGER :: nstep

   PRINT*, ' * Test update_calendar   : forward in time (6h step)'

   ! Initial step: 6 hours
   ngcm_step = 6;    ngcm_unit = 3

   CALL init_calendar

   DO nstep = 1, 40
      CALL update_calendar
   END DO

   ASSERT_EQUAL(startDay+9,    currDay )
   ASSERT_EQUAL(startDay+10,   nextDay )
   ASSERT_EQUAL(startMon,       currMon )
   ASSERT_EQUAL(startMon,       nextMon )

   ASSERT_EQUAL(startYear,   currYear)
   ASSERT_EQUAL(startYear, nextYear)

END TEST

TEST test_update_calendar_5

   ! Test for update_calendar
   ! nextYear ---> update_calendar ---> currDate

   INTEGER :: nstep

   PRINT*, ' * Test update_calendar   : forward in time (6h step - loop)'

   ! Initial step: 6 hours
   ngcm_step = 6;    ngcm_unit = 3

   ! StartMon to december
   startMon  = 12

   ! Loop
   loopYears = .TRUE.

   CALL init_calendar

   DO nstep = 1, 124
      CALL update_calendar
   END DO

   ASSERT_EQUAL(18,   currHour )
   ASSERT_EQUAL( 0,   nextHour )
   ASSERT_EQUAL(31,   currDay )
   ASSERT_EQUAL( 1,   nextDay )
   ASSERT_EQUAL(12,   currMon )
   ASSERT_EQUAL( 1,   nextMon )

   ASSERT_EQUAL(  startYear,   currYear)
   ASSERT_EQUAL(loopStartYear, nextYear)

END TEST

TEST test_update_calendar_6

   ! Test for update_calendar
   ! nextYear ---> update_calendar ---> currDate

   INTEGER :: nstep

   PRINT*, ' * Test update_calendar   : backward in time'

   ! Backward
   nff = -1

   ! Change startYear
   startYear = 1999

   CALL init_calendar

   DO nstep = 1, 12
      CALL update_calendar

      IF (nstep ==6 ) THEN
        ASSERT_EQUAL(8,  currMon)
        ASSERT_EQUAL(7,  nextMon)
      END IF

   END DO

   ASSERT_EQUAL( 2,          currMon )
   ASSERT_EQUAL( 1,          nextMon )
   ASSERT_EQUAL(startYear-1, currYear)
   ASSERT_EQUAL(startYear-1, nextYear)

   ASSERT_EQUAL(iyear, 2)

END TEST

TEST test_update_calendar_7

   ! Test for update_calendar
   ! nextYear ---> update_calendar ---> currDate

   INTEGER :: nstep

   PRINT*, ' * Test update_calendar   : backward in time (loop)'

   ! Backward
   nff = -1

   ! Change startYear
   startYear = 1999

   ! Activate loop
   loopYears = .True.
   loopEndYear = 1998; loopStartYear = 2005

   CALL init_calendar

   DO nstep = 1, 13
      CALL update_calendar
   END DO

   ASSERT_EQUAL( 1,          currMon )
   ASSERT_EQUAL( 12,          nextMon )
   ASSERT_EQUAL(startYear-1, currYear)
   ASSERT_EQUAL(loopStartYear, nextYear)

   ASSERT_EQUAL(iyear, 3)

END TEST

TEST test_update_calendar_8

   ! Test for update_calendar
   ! nextYear ---> update_calendar ---> currDate

   INTEGER :: nstep

   PRINT*, ' * Test update_calendar   : backward in time (1d step)'

   ! Backward
   nff = -1

   ! Initial step: 1 day
   ngcm_step = 1;    ngcm_unit = 4

   CALL init_calendar

   DO nstep = 1, 366
      CALL update_calendar
   END DO

   ASSERT_EQUAL(  1,  currDay )
   ASSERT_EQUAL( 31,  nextDay )
   ASSERT_EQUAL(  1,  currMon )
   ASSERT_EQUAL( 12,  nextMon )

   ASSERT_EQUAL(startYear-1,   currYear)
   ASSERT_EQUAL(startYear-2, nextYear)

   ASSERT_EQUAL(iyear, 3)

END TEST

TEST test_update_calendar_9

   ! Test for update_calendar
   ! nextYear ---> update_calendar ---> currDate

   INTEGER :: nstep

   PRINT*, ' * Test update_calendar   : backward in time (6h step)'

   ! Backward
   nff = -1

   ! Initial step: 1 day
   ngcm_step = 6;    ngcm_unit = 3

   CALL init_calendar

   DO nstep = 1, 125
      CALL update_calendar
   END DO

   ASSERT_EQUAL(  0,  currHour )
   ASSERT_EQUAL( 18,  nextHour)
   ASSERT_EQUAL(  1,  currDay )
   ASSERT_EQUAL( 30,  nextDay )
   ASSERT_EQUAL( 12,  currMon )
   ASSERT_EQUAL( 11,  nextMon )

   ASSERT_EQUAL(startYear-1,   currYear)
   ASSERT_EQUAL(startYear-1,   nextYear)

   ASSERT_EQUAL(iyear, 2)

END TEST

TEST test_update_calendar_10

   ! Test for update_calendar
   ! nextYear ---> update_calendar ---> currDate

   INTEGER :: nstep

   PRINT*, ' * Test update_calendar   : backward in time (6h step - loop)'

   ! Backward
   nff = -1

   ! Initial step: 1 day
   ngcm_step = 6;    ngcm_unit = 3

   ! Move to 1950
   startYear = 1950; startMon = 1; startDay = 31

   ! Activate Loop
   loopYears = .TRUE.
   loopEndYear   = 1950; loopStartYear = 2000

   CALL init_calendar

   DO nstep = 1, 121
      CALL update_calendar
   END DO

   ASSERT_EQUAL(  0,  currHour )
   ASSERT_EQUAL( 18,  nextHour)
   ASSERT_EQUAL(  1,  currDay )
   ASSERT_EQUAL( 31,  nextDay )
   ASSERT_EQUAL(  1,  currMon )
   ASSERT_EQUAL( 12,  nextMon )

   ASSERT_EQUAL(startYear,       currYear)
   ASSERT_EQUAL(LoopStartYear,   nextYear)

   ASSERT_EQUAL(iyear, 2)

END TEST

TEST test_tt_calendar_1

   ! Test for tt_calendar
   ! tt ---> tt_calendar ---> dateDate

   REAL(8) :: tt

   PRINT*, ' * Test tt_calendar       : forward in time'

   CALL init_calendar

   ! Step in seconds (6h 10days)
   tt = 10*24*3600. + 6*3600.

   CALL tt_calendar(tt)

   ASSERT_EQUAL(  6,  dateHour )
   ASSERT_EQUAL( 11,  dateDay )
   ASSERT_EQUAL(  1,  dateMon )

   ASSERT_EQUAL(startYear,   dateYear)

END TEST

TEST test_tt_calendar_2

   ! Test for tt_calendar
   ! tt ---> tt_calendar ---> dateDate

   REAL(8) :: tt

   PRINT*, ' * Test tt_calendar       : forward in time (loop)'

   ! Activate loop
   loopYears = .TRUE.

   CALL init_calendar

   ! Step in seconds (6h 10days)
   tt = 360*24*3600. + 6*3600

   CALL tt_calendar(tt)

   ASSERT_EQUAL(  6,  dateHour )
   ASSERT_EQUAL(  1,  dateDay )
   ASSERT_EQUAL(  1,  dateMon )

   ASSERT_EQUAL(LoopStartYear,   dateYear)

END TEST

TEST test_tt_calendar_3

   ! Test for tt_calendar
   ! tt ---> tt_calendar ---> dateDate

   REAL(8) :: tt

   PRINT*, ' * Test tt_calendar       : backward in time'

   ! Backward in time
   nff = -1

   CALL init_calendar

   ! Step in seconds (6h 10days)
   tt = -10*24*3600. - 6*3600.

   CALL tt_calendar(tt)

   ASSERT_EQUAL( 18,  dateHour )
   ASSERT_EQUAL( 20,  dateDay )
   ASSERT_EQUAL( 12,  dateMon )

   ASSERT_EQUAL(startYear-1,   dateYear)

END TEST

TEST test_tt_calendar_4

   ! Test for tt_calendar
   ! tt ---> tt_calendar ---> dateDate

   REAL(8) :: tt

   PRINT*, ' * Test tt_calendar       : backward in time (loop)'

   ! Backward in time
   nff = -1

   ! Activate Loop
   loopYears = .TRUE.;   startYear = 1950
   loopEndYear   = 1950; loopStartYear = 2000

   CALL init_calendar

   ! Step in seconds (6h 10days)
   tt = -10*24*3600. - 6*3600.

   CALL tt_calendar(tt)

   ASSERT_EQUAL( 18,  dateHour )
   ASSERT_EQUAL( 20,  dateDay )
   ASSERT_EQUAL( 12,  dateMon )

   ASSERT_EQUAL(loopStartYear,   dateYear)

END TEST

end test_suite
