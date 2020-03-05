PROGRAM mod_calendar_test
    !!------------------------------------------------------------------------------
    !!
    !!       MODULE: mod_calendar_test
    !!                Test module for MOD_CALENDAR
    !!
    !!--------------------------------------------------------------------------

    USE mod_init
    USE mod_calendar
    USE mod_write

    IMPLICIT NONE

    INTEGER :: ierr, nstep

    ! Read namelist
    CALL init_namelist

    ! Header
    PRINT *, ''
    PRINT *, ' MOD_CALENDAR :: TEST'
    PRINT *, ' ----------------------------------------------------------------'
    PRINT *, ''


    ! TEST 1 - Initialisation
    !!--------------------------------------------------------------------------
    PRINT *, ' * Test init_calendar   : initialisation'
    ierr = 0

    CALL init_calendar

    ierr = ERR_CALC(startSec  == currSec, ierr)
    ierr = ERR_CALC(startMin  == currMin, ierr)
    ierr = ERR_CALC(startHour == currHour, ierr)
    ierr = ERR_CALC(startDay  == currDay, ierr)
    ierr = ERR_CALC(startMon  == currMon, ierr)
    ierr = ERR_CALC(startYear == currYear, ierr)

    CALL ERR_PRINT(ierr)

    ! TEST 2 - Calendar forward
    !!--------------------------------------------------------------------------
    PRINT*, ''
    PRINT*, ' * Test update_calendar : forward in time'
    ierr = 0

    CALL init_calendar

    DO nstep = 1, 12
        CALL update_calendar

        IF (nstep == 6) ierr = ERR_CALC(startMon+6  == currMon, ierr)

    END DO

    ierr = ERR_CALC(startSec    == currSec, ierr)
    ierr = ERR_CALC(startMin    == currMin, ierr)
    ierr = ERR_CALC(startHour   == currHour, ierr)
    ierr = ERR_CALC(startDay    == currDay, ierr)
    ierr = ERR_CALC(startMon    == currMon, ierr)
    ierr = ERR_CALC(startYear+1 == currYear, ierr)

    ierr = ERR_CALC(iyear == 2, ierr)

    CALL ERR_PRINT(ierr)

    ! TEST 3 - Calendar loop forward
    !!--------------------------------------------------------------------------
    PRINT*, ' * Test update_calendar : forward in time (loop)'
    ierr = 0

    loopYears     = .TRUE.
    loopEndYear   = 2000
    loopStartYear = 1950

    CALL init_calendar

    DO nstep = 1, 24
        CALL update_calendar
    END DO

    ierr = ERR_CALC(startSec    == currSec, ierr)
    ierr = ERR_CALC(startMin    == currMin, ierr)
    ierr = ERR_CALC(startHour   == currHour, ierr)
    ierr = ERR_CALC(startDay    == currDay, ierr)
    ierr = ERR_CALC(startMon    == currMon, ierr)

    ierr = ERR_CALC(loopStartYear == currYear, ierr)

    ierr = ERR_CALC(iyear == 3, ierr)

    CALL ERR_PRINT(ierr)

    ! LoopIndex reset
    loopIndex = 0
    ! Loop Years
    loopYears = .FALSE.

    ! TEST 4 - Forward in time 1 day
    !!--------------------------------------------------------------------------
    PRINT*, ' * Test update_calendar : forward in time (1d step)'
    ierr = 0

    ! 1 day step
    ngcm_unit = 4
    ngcm_step = 1

    CALL init_calendar

    DO nstep = 1, 365
        CALL update_calendar
    END DO

    ierr = ERR_CALC(startSec    == currSec, ierr)
    ierr = ERR_CALC(startMin    == currMin, ierr)
    ierr = ERR_CALC(startHour   == currHour, ierr)
    ierr = ERR_CALC(startDay    == currDay, ierr)
    ierr = ERR_CALC(startMon    == currMon, ierr)
    ierr = ERR_CALC(startYear+1 == currYear, ierr)

    ierr = ERR_CALC(iyear == 2, ierr)

    CALL ERR_PRINT(ierr)

    ! TEST 5 - Forward in time 6h
    !!--------------------------------------------------------------------------
    PRINT*, ' * Test update_calendar : forward in time (6h step)'
    ierr = 0

    ! 1 day step
    ngcm_unit = 3
    ngcm_step = 6

    CALL init_calendar

    DO nstep = 1, 40
        CALL update_calendar
    END DO

    ierr = ERR_CALC(startSec    == currSec, ierr)
    ierr = ERR_CALC(startMin    == currMin, ierr)
    ierr = ERR_CALC(startHour   == currHour, ierr)
    ierr = ERR_CALC(startDay+10 == currDay, ierr)
    ierr = ERR_CALC(startMon    == currMon, ierr)
    ierr = ERR_CALC(startYear   == currYear, ierr)

    CALL ERR_PRINT(ierr)

    ! TEST 6 - Loop Forward in time 6h
    !!--------------------------------------------------------------------------
    PRINT*, ' * Test update_calendar : forward in time (6h step - loop)'
    ierr = 0

    ! Move to december
    startMon  = 12
    startYear = 2000

    ! 1 day step
    ngcm_unit = 3
    ngcm_step = 6

    ! Loop
    loopYears = .TRUE.

    CALL init_calendar

    DO nstep = 1, 64
        CALL update_calendar
    END DO

    ierr = ERR_CALC(startSec    == currSec, ierr)
    ierr = ERR_CALC(startMin    == currMin, ierr)
    ierr = ERR_CALC(startHour   == currHour, ierr)
    ierr = ERR_CALC(1 == currDay, ierr)
    ierr = ERR_CALC(1 == currMon, ierr)
    ierr = ERR_CALC(loopstartYear == currYear, ierr)

    CALL ERR_PRINT(ierr)

    ! LoopIndex reset
    loopIndex = 0
    ! Loop Years
    loopYears = .FALSE.

    ! TEST 7 - Calendar backward
    !!--------------------------------------------------------------------------
    PRINT*, ''
    PRINT*, ' * Test update_calendar : backward in time'
    ierr = 0

    ! Backward
    nff = -1

    ! Start in January
    startMon  = 1
    startYear = 1999

    ! Monthly
    ngcm_unit = 5
    ngcm_step = 1

    CALL init_calendar

    DO nstep = 1, 12
        CALL update_calendar
        IF (nstep == 6) ierr = ERR_CALC(startMon+6  == currMon, ierr)
    END DO

    ierr = ERR_CALC(startSec    == currSec, ierr)
    ierr = ERR_CALC(startMin    == currMin, ierr)
    ierr = ERR_CALC(startHour   == currHour, ierr)
    ierr = ERR_CALC(startDay    == currDay, ierr)
    ierr = ERR_CALC(startMon    == currMon, ierr)
    ierr = ERR_CALC(startYear-1 == currYear, ierr)

    ierr = ERR_CALC(iyear == 2, ierr)

    CALL ERR_PRINT(ierr)

    ! TEST 8 - Calendar loop backward
    !!--------------------------------------------------------------------------
    PRINT*, ' * Test update_calendar : backward in time (loop)'
    ierr = 0

    ! Loop
    loopYears = .True.
    loopEndYear   = 1998
    loopStartYear = 2005

    CALL init_calendar

    DO nstep = 1, 24
        CALL update_calendar
    END DO

    ierr = ERR_CALC(startSec    == currSec, ierr)
    ierr = ERR_CALC(startMin    == currMin, ierr)
    ierr = ERR_CALC(startHour   == currHour, ierr)
    ierr = ERR_CALC(startDay    == currDay, ierr)
    ierr = ERR_CALC(startMon    == currMon, ierr)
    ierr = ERR_CALC(loopStartYear == currYear, ierr)

    ierr = ERR_CALC(iyear == 3, ierr)

    CALL ERR_PRINT(ierr)

    ! LoopIndex reset
    loopIndex = 0
    ! Loop Years
    loopYears = .FALSE.

    ! TEST 9 - Backward in Time (1 day)
    !!--------------------------------------------------------------------------
    PRINT*, ' * Test update_calendar : backward in time (1d step)'
    ierr = 0

    ! 1d step
    ngcm_unit = 4
    ngcm_step = 1

    CALL init_calendar

    DO nstep = 1, 365
        CALL update_calendar
    END DO

    ierr = ERR_CALC(startSec    == currSec, ierr)
    ierr = ERR_CALC(startMin    == currMin, ierr)
    ierr = ERR_CALC(startHour   == currHour, ierr)
    ierr = ERR_CALC(startDay    == currDay, ierr)
    ierr = ERR_CALC(startMon    == currMon, ierr)
    ierr = ERR_CALC(startYear-1 == currYear, ierr)

    ierr = ERR_CALC(iyear == 2, ierr)

    CALL ERR_PRINT(ierr)

    ! TEST 10 - Backward in Time (1 day)
    !!--------------------------------------------------------------------------
    PRINT*, ' * Test update_calendar : backward in time (6h step)'
    ierr = 0

    ! 1d step
    ngcm_unit = 3
    ngcm_step = 6

    CALL init_calendar

    DO nstep = 1, 64
        CALL update_calendar
    END DO

    ierr = ERR_CALC(startSec    == currSec, ierr)
    ierr = ERR_CALC(startMin    == currMin, ierr)
    ierr = ERR_CALC(startHour   == currHour, ierr)
    ierr = ERR_CALC(31    == currDay, ierr)
    ierr = ERR_CALC(12    == currMon, ierr)
    ierr = ERR_CALC(startYear-1 == currYear, ierr)

    CALL ERR_PRINT(ierr)

    ! TEST 11 - Backward in Time (1 day)
    !!--------------------------------------------------------------------------
    PRINT*, ' * Test update_calendar : backward in time (6h step - loop)'
    ierr = 0

    ! Move to 1950
    startYear = 1950

    ! Loop
    loopYears = .TRUE.
    loopEndYear   = 1950
    loopStartYear = 2000

    CALL init_calendar

    DO nstep = 1, 64
        CALL update_calendar
    END DO

    ierr = ERR_CALC(startSec    == currSec, ierr)
    ierr = ERR_CALC(startMin    == currMin, ierr)
    ierr = ERR_CALC(startHour   == currHour, ierr)
    ierr = ERR_CALC(31          == currDay, ierr)
    ierr = ERR_CALC(12          == currMon, ierr)
    ierr = ERR_CALC(loopStartYear == currYear, ierr)

    CALL ERR_PRINT(ierr)

    ! TEST 12 - End Date
    !!--------------------------------------------------------------------------
    PRINT *, ''
    PRINT *, ' * Test end_calendar   : forward in time'
    ierr = 0

    CALL init_namelist

    ! StartDay
    startDay = 1

    CALL init_calendar
    CALL end_calendar

    ierr = ERR_CALC(endSec  == startSec, ierr)
    ierr = ERR_CALC(endMin  == startMin, ierr)
    ierr = ERR_CALC(endHour == startHour, ierr)
    ierr = ERR_CALC(endDay  == startDay, ierr)
    ierr = ERR_CALC(endMon  == startMon+5, ierr)
    ierr = ERR_CALC(endYear == startYear+2, ierr)

    CALL ERR_PRINT(ierr)

    ! TEST 13 - End Date
    !!--------------------------------------------------------------------------
    PRINT *, ' * Test end_calendar   : backward in time'
    ierr = 0

    CALL init_namelist

    ! Backward
    nff = -1

    CALL init_calendar
    CALL end_calendar

    ierr = ERR_CALC(endSec  == startSec, ierr)
    ierr = ERR_CALC(endMin  == startMin, ierr)
    ierr = ERR_CALC(endHour == startHour, ierr)
    ierr = ERR_CALC(endDay  == startDay, ierr)
    ierr = ERR_CALC(endMon  == startMon+7, ierr)
    ierr = ERR_CALC(endYear == startYear-3, ierr)

    CALL ERR_PRINT(ierr)

    ! TEST 14 - tt_calendar forward in time
    !!--------------------------------------------------------------------------
    PRINT *, ''
    PRINT *, ' * Test tt_calendar   : forward in time'
    ierr = 0

    nff = 1

    CALL init_calendar

    tt = 10*24*3600 + 6*3600

    CALL tt_calendar(tt)

    ierr = ERR_CALC(dateHour == startHour+6, ierr)
    ierr = ERR_CALC(dateDay  == startDay+10, ierr)
    ierr = ERR_CALC(dateMon  == startMon, ierr)
    ierr = ERR_CALC(dateYear == startYear, ierr)

    CALL ERR_PRINT(ierr)

    ! TEST 15 - tt_calendar forward in time
    !!--------------------------------------------------------------------------
    PRINT *, ' * Test tt_calendar   : forward in time (loop)'
    ierr = 0

    loopYears     = .TRUE.
    loopEndYear   = 2000
    loopStartYear = 1950

    CALL init_calendar

    startYear = 2000
    startMon  = 12
    startDay  = 30

    tt = 3*24*3600

    CALL tt_calendar(tt)

    ierr = ERR_CALC(dateHour == startHour, ierr)
    ierr = ERR_CALC(dateDay  == 2, ierr)
    ierr = ERR_CALC(dateMon  == 1, ierr)
    ierr = ERR_CALC(dateYear == loopStartYear, ierr)

    CALL ERR_PRINT(ierr)

    ! LoopIndex reset
    loopIndex = 0
    ! Loop Years
    loopYears = .FALSE.

    ! TEST 16 - tt_calendar backward in time
    !!--------------------------------------------------------------------------
    PRINT *, ' * Test tt_calendar   : backward in time'
    ierr = 0

    nff = -1

    CALL init_calendar

    tt = -(10*24*3600 + 6*3600)

    CALL tt_calendar(tt)

    ierr = ERR_CALC(dateHour == startHour-6, ierr)
    ierr = ERR_CALC(dateDay  == startDay-10, ierr)
    ierr = ERR_CALC(dateMon  == startMon, ierr)
    ierr = ERR_CALC(dateYear == startYear, ierr)

    CALL ERR_PRINT(ierr)

    ! TEST 17 - tt_calendar backward in time
    !!--------------------------------------------------------------------------
    PRINT *, ' * Test tt_calendar   : backward in time (loop)'
    ierr = 0

    loopYears     = .TRUE.
    loopEndYear   = 1998
    loopStartYear = 2005

    CALL init_calendar

    startYear = 1998
    startMon  = 1
    startDay  = 2

    tt = -3*24*3600

    CALL tt_calendar(tt)

    ierr = ERR_CALC(dateHour == startHour, ierr)
    ierr = ERR_CALC(dateDay  == 30, ierr)
    ierr = ERR_CALC(dateMon  == 12, ierr)
    ierr = ERR_CALC(dateYear == loopStartYear, ierr)

    CALL ERR_PRINT(ierr)

    ! LoopIndex reset
    loopIndex = 0
    ! Loop Years
    loopYears = .FALSE.

    PRINT *, ''
    PRINT *, ' ----------------------------------------------------------------'
    PRINT *, ''

    ! **************************************************************************

    CONTAINS

    SUBROUTINE ERR_PRINT(ierr_dum)

      INTEGER :: ierr_dum

      IF (ierr_dum == 0) THEN
          PRINT*, '   =====================> PASS'
      ELSE
          PRINT*, '   =====================> FAIL'
          STOP
      END IF

    END SUBROUTINE

    INTEGER FUNCTION ERR_CALC(log_dum, ierr_dum)

      LOGICAL :: log_dum
      INTEGER :: ierr_dum

      IF (log_dum .EQV. .TRUE.) THEN
          ERR_CALC = ierr_dum
      ELSE
          ERR_CALC = ierr_dum + 1
      END IF

    END FUNCTION

END PROGRAM  mod_calendar_test
