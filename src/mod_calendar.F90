MODULE mod_calendar
   !!------------------------------------------------------------------------------
   !!
   !!       MODULE: mod_calendar
   !!
   !!          Defines and updates the TRACMASS calendar
   !!
   !!          Subroutines included:
   !!               - init_calendar
   !!               - end_calendar
   !!               - previous_calendar
   !!               - update_calendar
   !!               - tt_calendar
   !!
   !!------------------------------------------------------------------------------

   USE mod_precdef
   USE mod_time
   USE mod_param
   USE mod_log

   IMPLICIT NONE

   CONTAINS

     SUBROUTINE init_calendar
     ! --------------------------------------------------
     !
     ! Purpose:
     ! Initialize calendar in TRACMASS.
     !
     !
     ! Method:
     ! Populate the daysInMonth array.
     ! Set startDay,startHour to nextDay,nextHour etc
     !
     ! --------------------------------------------------

        INTEGER                            :: jcaltype
        REAL(DP)                           :: currStep

        IF (log_level >= 5) THEN
           PRINT*,'* Entering init_calendar '
        END IF

        daysInMonth(:,1) = (/ 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 /)
        daysInMonth(:,2) = (/ 31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 /)
        daysInMonth(:,3) = (/ 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30 /)

        nextSec  = startSec
        nextMin  = startMin
        nextHour = startHour
        nextDay  = startDay
        nextMon  = startMon
        nextYear = startYear

        iyear = 1
        ! For forward trajectories, we need the current month to calculate step length
        ! but for backward trajectories, we need the previous month
        ! E.g. if we start 1 Feb, currStep is 28 days for fwd trajs
        ! but currStep is 31 days for backwd trajs (duration of Jan)
        IF (nff > 0) THEN
           imon = nextMon
        ELSE IF (nff < 0) THEN
           imon = nextMon - 1
           IF (imon <= 0) THEN
              imon = 12
           END IF
        END IF

        IF (log_level >=3) THEN
           print*,'Before setting first time step. nextyear, mon,day, iyear, imon ',nextYear,nextMon,nextDay,iyear,imon
        END IF

        ! Find current step (secs)
        IF (ngcm_unit == 1) THEN ! sec
           currStep = ngcm_step
        ELSE IF (ngcm_unit == 2) THEN ! min
           currStep = ngcm_step * 60.d0
        ELSE IF (ngcm_unit == 3) THEN ! hour
           currStep = ngcm_step * 3600.d0
        ELSE IF (ngcm_unit == 4) THEN ! days
           currStep = ngcm_step * 86400.d0
        ELSE IF (ngcm_unit == 5) THEN ! months
           currStep = ngcm_step * daysInMonth(imon, 3) * 86400.d0
        ELSE IF (ngcm_unit == 6) THEN ! years
           jcaltype = 1
           IF (.not. noleap .AND. ((MOD(nextYear, 4) .EQ. 0 .AND. MOD(nextYear, 100) .NE. 0) &
               .OR. MOD(nextYear, 400) .EQ. 0)) jcaltype = 2
           IF (mon30day) jcaltype = 3
           currStep = ngcm_step * SUM(daysInMonth(:, jcaltype)) * 86400.d0
        ELSE
           PRINT*," Error [init_calendar]"
           PRINT*," ================================================="
           PRINT*," The ngcm_unit ",ngcm_unit," is not recognised "
           PRINT*," Valid units are 1 (second), 2 (minute), 3 (hour) "
           PRINT*," 4 (day), 5 (month), 6 (year) "
           PRINT*," You can also code new units in calendar.f95"
           PRINT*," For now, I stop "
           STOP
        END IF

        ! ngcm
        ngcm = INT(currStep / (60*60)) ! hours

        ! ncgcm_seconds
        tseas= DBLE(currStep)

        IF (log_level >=5 ) THEN
           PRINT*,' Done initialising calendar. ngcm = ',ngcm
           PRINT*,' leaving init_calendar '
        END IF

        RETURN

     END SUBROUTINE init_calendar

     SUBROUTINE end_calendar
     ! --------------------------------------------------
     !
     ! Purpose:
     ! Compute the final date in TRACMASS.
     !
     ! --------------------------------------------------

        INTEGER :: nt

        DO nt = 1, intrun-1
          CALL update_calendar
        END DO

        endYear = nextYear; currYear = 0;
        endMon  = nextMon ; currMon  = 0;
        endDay  = nextDay;  currDay  = 0;
        endHour = nextHour; currHour = 0;
        endMin  = nextMin;  currMin  = 0;
        endSec  = nextSec;  currSec  = 0;
        loopindex = 0

        CALL init_calendar

     END SUBROUTINE end_calendar

     SUBROUTINE previous_calendar
     ! --------------------------------------------------
     !
     ! Purpose:
     ! Compute the previous date to
     ! the starting date in TRACMASS.
     !
     ! --------------------------------------------------

        INTEGER  :: tempLoopstart, tempLoopend

        ! Reverse loopstart and loopend/ and nff
        nff = -1*nff

        tempLoopstart = loopStartYear; tempLoopend = loopEndYear
        loopStartYear = tempLoopend; loopEndYear = tempLoopstart


        CALL update_calendar

        prevYear = nextYear
        prevMon  = nextMon
        prevDay  = nextDay
        prevHour = nextHour
        prevMin  = nextMin
        prevSec  = nextSec

        ! Reset nff, loopstart and loopend and loopindex
        loopindex = 0

        nff = -1*nff
        loopStartYear = tempLoopstart; loopEndYear = tempLoopend

        CALL init_calendar

     END SUBROUTINE previous_calendar

     SUBROUTINE update_calendar()
     ! ---------------------------------------------------
     !
     ! Purpose:
     ! Update date and time of TRACMASS
     !
     ! Method:
     ! Add some seconds to the clock and update dates accordingly
     !
     ! ---------------------------------------------------

         REAL(DP)                            :: currStep
         INTEGER                             :: jcaltype

         IF(log_level >= 5) THEN
            PRINT*,' entering update_calendar '
         END IF

         ! Update calendar
         currSec = nextSec; currMin = nextMin; currHour = nextHour
         currDay = nextDay; currMon = nextMon; currYear = nextYear

         ! see comment for init_calendar for explanation of the following lines
         ! iyear is corrected if loopYear is True
         IF (nff > 0) THEN
            imon  = currMon
            iyear = currYear - startYear + 1 + loopIndex*(loopEndYear - loopStartYear + 1)
         ELSE IF (nff < 0) THEN
            imon = currMon - 1
            IF (imon <= 0) THEN
                imon = 12
            END IF
            iyear = startYear - currYear + 1 + loopIndex*(loopStartYear - loopEndYear + 1)
         END IF

         IF (log_level >= 3) THEN
            print*,'b4 update calendar',nextYear,nextMon,nextDay,iyear,imon
         END IF

         ! Find number of minutes to add
         IF (ngcm_unit == 1) THEN ! sec
            currStep = ngcm_step
         ELSE IF (ngcm_unit == 2) THEN ! min
            currStep = ngcm_step * 60.d0
         ELSE IF (ngcm_unit == 3) THEN ! hour
            currStep = ngcm_step * 3600.d0
         ELSE IF (ngcm_unit == 4) THEN ! days
            currStep = ngcm_step * 86400.d0
         ELSE IF (ngcm_unit == 5) THEN ! months
            currStep = ngcm_step*daysInMonth(imon, 3) * 86400.d0
         ELSE IF (ngcm_unit == 6) THEN ! years
            jcaltype = 1
            IF (.not. noleap .AND. ((MOD(currYear, 4) .EQ. 0 .AND. MOD(currYear, 100) .NE. 0) &
                .OR. MOD(currYear, 400) .EQ. 0)) jcaltype = 2
            IF (mon30day) jcaltype = 3
            currStep = ngcm_step * SUM(daysInMonth(:, jcaltype)) * 86400.d0
         ELSE
            PRINT*," The ngcm_unit ",ngcm_unit," is not recognised "
            PRINT*," Valid units are 1 (second), 2 (minute), 3 (hour) "
            PRINT*," 4 (day), 5 (month), 6 (year) "
            PRINT*," You can also code new units in calendar.f95"
            PRINT*," For now, I stop "
            STOP
         END IF

         ! ngcm
         ngcm = INT(currStep / (60*60)) ! hours

         ! Now update the time and date
         nextSec  = currSec + currStep * nff

         IF (log_level >= 3) THEN
            PRINT*,' set up currStep, ngcm ',currStep,ngcm
         END IF

         ! If nextSec > 60 we update the minutes,
         ! and hours etc until we have nextSec < 60 again
         DO WHILE (nextSec >= 60)
            nextSec = nextSec - 60
            nextMin = nextMin + 1
            IF (nextMin >= 60) THEN
               nextMin = nextMin - 60
               nextHour = nextHour + 1
               IF (nextHour >= 24) THEN
                  nextHour = nextHour - 24
                  nextDay  = nextDay + 1

                  jcaltype = 1
                  IF (.not. noleap .AND.((MOD(nextYear, 4) .EQ. 0 .AND. MOD(nextYear, 100) .NE. 0) &
                      .OR. MOD(nextYear, 400) .EQ. 0)) jcaltype = 2
                  IF (mon30day .OR. ngcm_unit == 5) jcaltype = 3

                  IF (nextDay > daysInMonth(nextMon,jcaltype)) THEN
                     nextDay = nextDay - daysInMonth(nextMon,jcaltype)
                     nextMon = nextMon + 1
                     IF (nextMon > 12) THEN
                        nextMon = nextMon - 12
                        nextYear = nextYear + 1
                        iyear = iyear + 1
                     END IF
                  END IF
               END IF
            END IF

         END DO

         IF (loopYears) THEN
            IF (nextYear > loopEndYear .and. nff > 0) THEN
               IF (log_level >= 3) THEN
                  PRINT*,' nextYear > loopEndYear. Going back to loopStartYear '
               END IF
               nextYear = loopStartYear
               loopIndex = loopIndex + 1
            END IF
         END IF

         ! If nextSec < 0 (backward trajs) we update the minutes,
         ! and hours etc until we have nextSec >= 0 again
         DO WHILE (nextSec < 0)
            nextSec = nextSec + 60
            nextMin = nextMin - 1
            IF (nextMin < 0) THEN
               nextMin = nextMin + 60
               nextHour = nextHour - 1
               IF (nextHour < 0) THEN
                  nextHour = nextHour + 24
                  nextDay  = nextDay - 1
                  IF (nextDay <= 0) THEN
                     nextMon = nextMon - 1
                     IF (nextMon <= 0) THEN
                        nextMon = nextMon + 12
                        nextYear = nextYear - 1
                        iyear = iyear + 1
                     END IF

                     jcaltype = 1
                     IF (.not. noleap .AND.((MOD(nextYear, 4) .EQ. 0 .AND. MOD(nextYear, 100) .NE. 0) &
                         .OR. MOD(nextYear, 400) .EQ. 0)) jcaltype = 2
                     IF (mon30day .OR. ngcm_unit == 5) jcaltype = 3

                     nextDay = nextDay + daysInMonth(nextMon, jcaltype)
                  END IF
               END IF
            END IF
         END DO

         IF (loopYears) THEN
            IF (nextYear < loopEndYear .and. nff < 0) THEN
               IF (log_level >= 3) THEN
                  PRINT*,' nextYear < loopEndYear. Going back to loopStartYear '
               END IF
               nextYear = loopStartYear
               loopIndex = loopIndex + 1
            END IF
         END IF


         IF (log_level >= 3) THEN
            print*,'af update calendar',nextYear,nextMon,nextDay,iyear
         END IF

         IF (log_level >= 5) THEN
            PRINT*,' leaving update_calendar '
         END IF

         RETURN

     END SUBROUTINE update_calendar

     SUBROUTINE tt_calendar(tv)
     ! ---------------------------------------------------
     !
     ! Purpose:
     ! Transforms tt into a date YYYY MM DD HH:MM:SS
     !
     ! Method:
     ! Using the starting date and tt calculates the corresponding date
     !
     ! ---------------------------------------------------

         REAL(DP), INTENT(IN)  :: tv
         INTEGER               :: jcaltype

         ! Now update the time and date
         dateSec   = tv
         dateMin   = startMin
         dateHour  = startHour
         dateDay   = startDay
         dateMon   = startMon
         dateYear  = startYear

         ! If dateSec > 60 we update the minutes,
         ! and hours etc until we have dateSec < 60 again
         DO WHILE (dateSec >= 60)

            dateSec = dateSec - 60
            dateMin = dateMin + 1
            IF (dateMin >= 60) THEN
               dateMin = dateMin - 60
               dateHour = dateHour + 1
               IF (dateHour >= 24) THEN
                  dateHour = dateHour - 24
                  dateDay  = dateDay + 1

                  jcaltype = 1
                  IF (.not. noleap .AND.((MOD(dateYear, 4) .EQ. 0 .AND. MOD(dateYear, 100) .NE. 0) &
                      .OR. MOD(dateYear, 400) .EQ. 0)) jcaltype = 2
                  IF (mon30day .OR. ngcm_unit == 5) jcaltype = 3
                  IF (dateDay > daysInMonth(dateMon, jcaltype)) THEN
                     dateDay = dateDay - daysInMonth(dateMon, jcaltype)
                     dateMon = dateMon + 1
                     IF (dateMon > 12) THEN
                        dateMon = dateMon - 12
                        dateYear = dateYear + 1
                        IF (dateYear > loopEndYear .AND. nff > 0 .AND. loopYears) THEN
                           dateYear = loopStartYear
                        END IF
                     END IF
                  END IF
               END IF
            END IF

         END DO

         IF (loopYears) THEN
            IF (dateYear > loopEndYear .AND. nff > 0) THEN
               dateYear = loopStartYear
            END IF
         END IF

         ! If dateSec < 0 (backward trajs) we update the minutes,
         ! and hours etc until we have dateSec >= 0 again
         DO WHILE (dateSec < 0)
            dateSec = dateSec + 60
            dateMin = dateMin - 1
            IF (dateMin < 0) THEN
               dateMin = dateMin + 60
               dateHour = dateHour - 1
               IF (dateHour < 0) THEN
                  dateHour = dateHour + 24
                  dateDay  = dateDay - 1
                  IF (dateDay <= 0) THEN
                     dateMon = dateMon - 1
                     IF (dateMon <= 0) THEN
                        dateMon = dateMon + 12
                        dateYear = dateYear - 1
                        IF (dateYear < loopEndYear .AND. nff < 0 .AND. loopYears) THEN
                           dateYear = loopStartYear
                        END IF
                     END IF

                     jcaltype = 1
                     IF (.not. noleap .AND.((MOD(dateYear, 4) .EQ. 0 .AND. MOD(dateYear, 100) .NE. 0) &
                         .OR. MOD(dateYear, 400) .EQ. 0)) jcaltype = 2
                     IF (mon30day .OR. ngcm_unit == 5) jcaltype = 3

                     dateDay = dateDay + daysInMonth(dateMon,jcaltype)
                  END IF
               END IF
            END IF
         END DO

         IF (loopYears) THEN
            IF (dateYear < loopEndYear .and. nff < 0) THEN
               dateYear = loopStartYear
            END IF
         END IF

         RETURN

     END SUBROUTINE tt_calendar

END MODULE mod_calendar
