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
   !!               - update_calendar
   !!               - tt_calendar
   !!
   !!------------------------------------------------------------------------------

   USE mod_precdef
   USE mod_time
   USE mod_param
   USE mod_log

   IMPLICIT NONE

   INTEGER, DIMENSION(10000,12)       :: daysInMonth ! Number of days per month

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
     ! Set startDay,startHour to futDay,futHour etc
     !
     ! --------------------------------------------------

        INTEGER                            :: jyear
        INTEGER                            :: currStep

        IF (log_level >= 5) THEN
           PRINT*,'* Entering init_calendar '
        END IF

        IF (.not. noleap) THEN
           DO jyear=1,10000
              daysInMonth(jyear,:) = (/ 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 /)
              IF ( MOD(jyear,4) == 0 ) THEN
                 daysInMonth(jyear,2) = 29
                 IF ( MOD(jyear,100) == 0 .AND. MOD(jyear,400) /= 0 ) THEN
                    daysInMonth(jyear,2) = 28
                 END IF
              END IF

              IF (ngcm_unit == 5) daysInMonth(jyear,:) = (/ 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30 /)
           END DO
        ELSE
           DO jyear=1,10000
              daysInMonth(jyear,:) = (/ 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 /)
              IF (ngcm_unit == 5) daysInMonth(jyear,:) = (/ 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30 /)
           END DO
        END IF

        futSec  = startSec
        futMin  = startMin
        futHour = startHour
        futDay  = startDay
        futMon  = startMon
        futYear = startYear

        iyear = 1
        ! For forward trajectories, we need the current month to calculate step length
        ! but for backward trajectories, we need the previous month
        ! E.g. if we start 1 Feb, currStep is 28 days for fwd trajs
        ! but currStep is 31 days for backwd trajs (duration of Jan)
        IF (nff > 0) THEN
           imon = futMon
        ELSE IF (nff < 0) THEN
           imon = futMon - 1
           IF (imon <= 0) THEN
              imon = 12
           END IF
        END IF

        IF (log_level >=3) THEN
           print*,'Before setting first time step. futyear, mon,day, iyear, imon ',futYear,futMon,futDay,iyear,imon
        END IF

        ! Find current step (secs)
        IF (ngcm_unit == 1) THEN ! sec
           currStep = ngcm_step
        ELSE IF (ngcm_unit == 2) THEN ! min
           currStep = ngcm_step * 60
        ELSE IF (ngcm_unit == 3) THEN ! hour
           currStep = ngcm_step * 60 * 60
        ELSE IF (ngcm_unit == 4) THEN ! days
           currStep = ngcm_step * 24 * 60 * 60
        ELSE IF (ngcm_unit == 5) THEN ! months
           currStep = daysInMonth(futYear,imon) * 24 * 60 * 60
        ELSE IF (ngcm_unit == 6) THEN ! years
           currStep = SUM(daysInMonth(futYear,:)) * 24 * 60 * 60
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
        ngcm = currStep / (60*60) ! hours

        ! ncgcm_seconds
        tseas= dble(currStep)

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

        endYear = currYear
        endMon  = currMon
        endDay  = currDay
        endHour = currHour
        endMin  = currMin
        endSec  = currSec

        CALL init_calendar

     END SUBROUTINE end_calendar

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

         INTEGER                            :: currStep

         IF(log_level >= 5) THEN
            PRINT*,' entering update_calendar '
         END IF

         ! Update calendar
         currSec = futSec; currMin = futMin; currHour = futHour
         currDay = futDay; currMon = futMon; currYear = futYear

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
            print*,'b4 update calendar',futYear,futMon,futDay,iyear,imon
         END IF

         ! Find number of minutes to add
         IF (ngcm_unit == 1) THEN ! sec
            currStep = ngcm_step
         ELSE IF (ngcm_unit == 2) THEN ! min
            currStep = ngcm_step * 60
         ELSE IF (ngcm_unit == 3) THEN ! hour
            currStep = ngcm_step * 60 * 60
         ELSE IF (ngcm_unit == 4) THEN ! days
            currStep = ngcm_step * 24 * 60 * 60
         ELSE IF (ngcm_unit == 5) THEN ! months
            currStep = daysInMonth(currYear,imon) * 24 * 60 * 60
         ELSE IF (ngcm_unit == 6) THEN ! years
            currStep = SUM(daysInMonth(currYear,:)) * 24 * 60 * 60
         ELSE
            PRINT*," The ngcm_unit ",ngcm_unit," is not recognised "
            PRINT*," Valid units are 1 (second), 2 (minute), 3 (hour) "
            PRINT*," 4 (day), 5 (month), 6 (year) "
            PRINT*," You can also code new units in calendar.f95"
            PRINT*," For now, I stop "
            STOP
         END IF

         ! ngcm
         ngcm = currStep / (60*60) ! hours

         ! Now update the time and date
         futSec  = currSec + currStep * nff

         IF (log_level >= 3) THEN
            PRINT*,' set up currStep, ngcm ',currStep,ngcm
         END IF

         ! If futSec > 60 we update the minutes,
         ! and hours etc until we have futSec < 60 again
         DO WHILE (futSec >= 60)

            futSec = futSec - 60
            futMin = futMin + 1
            IF (futMin >= 60) THEN
               futMin = futMin - 60
               futHour = futHour + 1
               IF (futHour >= 24) THEN
                  futHour = futHour - 24
                  futDay  = futDay + 1
                  IF (futDay > daysInMonth(futYear,futMon)) THEN
                     futDay = futDay - daysInMonth(futYear,futMon)
                     futMon = futMon + 1
                     IF (futMon > 12) THEN
                        futMon = futMon - 12
                        futYear = futYear + 1
                        iyear = iyear + 1
                     END IF
                  END IF
               END IF
            END IF

         END DO

         IF (loopYears) THEN
            IF (futYear > loopEndYear .and. nff > 0) THEN
               IF (log_level >= 3) THEN
                  PRINT*,' futYear > loopEndYear. Going back to loopStartYear '
               END IF
               futYear = loopStartYear
               loopIndex = loopIndex + 1
            END IF
         END IF

         ! If futSec < 0 (backward trajs) we update the minutes,
         ! and hours etc until we have futSec >= 0 again
         DO WHILE (futSec < 0)
            futSec = futSec + 60
            futMin = futMin - 1
            IF (futMin < 0) THEN
               futMin = futMin + 60
               futHour = futHour - 1
               IF (futHour < 0) THEN
                  futHour = futHour + 24
                  futDay  = futDay - 1
                  IF (futDay <= 0) THEN
                     futMon = futMon - 1
                     IF (futMon <= 0) THEN
                        futMon = futMon + 12
                        futYear = futYear - 1
                        iyear = iyear + 1
                     END IF
                     futDay = futDay + daysInMonth(futYear,futMon)
                  END IF
               END IF
            END IF
         END DO

         IF (loopYears) THEN
            IF (futYear < loopEndYear .and. nff < 0) THEN
               IF (log_level >= 3) THEN
                  PRINT*,' futYear < loopEndYear. Going back to loopStartYear '
               END IF
               futYear = loopStartYear
               loopIndex = loopIndex + 1
            END IF
         END IF


         IF (log_level >= 3) THEN
            print*,'af update calendar',futYear,futMon,futDay,iyear
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
                  IF (dateDay > daysInMonth(dateYear,dateMon)) THEN
                     dateDay = dateDay - daysInMonth(dateYear,dateMon)
                     dateMon = dateMon + 1
                     IF (dateMon > 12) THEN
                        dateMon = dateMon - 12
                        dateYear = dateYear + 1
                        IF (dateYear > loopEndYear .AND. nff > 0) THEN
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
                        IF (dateYear < loopEndYear .and. nff < 0) THEN
                           dateYear = loopStartYear
                        END IF
                     END IF
                     dateDay = dateDay + daysInMonth(dateYear,dateMon)
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
