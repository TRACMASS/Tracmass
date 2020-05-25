MODULE mod_error
  !!---------------------------------------------------------------------------
  !!
  !!       MODULE mod_error:
  !!
  !!          This module creates of diagnostics of possible errors
  !!          in the simulation
  !!
  !!          Subroutines included:
  !!              - errorCheck
  !!              - write_error
  !!
  !!              - errorType (P)
  !!
  !!---------------------------------------------------------------------------

  USE mod_precdef
  USE mod_traj
  USE mod_trajdef
  USE mod_time
  USE mod_loopvars
  USE mod_calendar
  USE mod_grid

  USE mod_write, only      : timeformat

  IMPLICIT NONE

  REAL(DP)              :: xw,yw,zw

  PRIVATE :: errorType

  CONTAINS

  SUBROUTINE errorCheck(teststr,errCode)
  ! --------------------------------------------------
  !
  ! Purpose:
  ! Check for errors in TRACMASS
  !
  ! --------------------------------------------------
      CHARACTER (LEN=*),INTENT(IN)         :: teststr
      INTEGER, INTENT(OUT)                 :: errCode

      SELECT CASE(TRIM(teststr))
      CASE('infLoopError')

        errCode = 0
        ! If a trajectory expends too much time without crossing a wall
        ! or reaching a killing zone
        IF(niter-trajectories(ntrac)%niter > 30000) THEN

          nloop = nloop + 1
          errCode = 1
          trajectories(ntrac)%active = .FALSE.

          CALL write_error(errCode)

        END IF


      CASE ('dxyzError')

        errCode = 0
        ! If the volume at a given point is zero or negative
        IF (dxyz <= 0.d0) THEN

          nerror = nerror + 1
          errCode = 2
          trajectories(ntrac)%active = .FALSE.

          CALL write_error(errCode)

        END IF

      CASE ('boundError')

         errCode = 0
         ! Trajectory leaving a model area
         IF(ia<1 .OR. ia>imtdom .OR. ib<1 .OR. ib>imtdom .OR.    &
            ja<1 .OR. ja>jmtdom .OR. jb<1 .OR. jb>jmtdom .OR.    &
            y0<0 .OR. y0>jmtdom .OR. y1<0 .OR. y1>jmtdom .OR.    &
            z1>=DBLE(KM) ) THEN

          nerror = nerror + 1
          errCode = 3

          trajectories(ntrac)%active = .FALSE.

          CALL write_error(errCode)

         END IF

      CASE ('landError')

        errCode = 0
        ! Trajectory hits land
        IF(kmt(ib,jb) == 0) THEN

           nerror = nerror + 1
           errCode = 4

           trajectories(ntrac)%active = .FALSE.

           CALL write_error(errCode)

        END IF

      CASE ('coordboxError')

        errCode = 0
        ! Check that coordinates belongs to
        ! correct box. Valuable for debugging
        IF ( DBLE(ib-1) .GT. x1 .OR. DBLE(ib) .LT. x1 )  THEN

          nerror = nerror + 1
          errCode = 5

          trajectories(ntrac)%active = .FALSE.

          CALL write_error(errCode)

        ELSEIF( DBLE(jb-1) .GT. y1 .OR. DBLE(jb) .LT. y1 )  THEN

          nerror = nerror + 1
          errCode = 6

          trajectories(ntrac)%active = .FALSE.

          CALL write_error(errCode)

        ELSEIF((DBLE(kb-1) .GT. z1 .AND. kb .NE. KM) .OR. &
             DBLE(kb).LT.z1 ) THEN

           nerror = nerror + 1
           errCode = 7

           trajectories(ntrac)%active = .FALSE.

           CALL write_error(errCode)

        END IF

      CASE ('dsCrossError')

         errCode = 0
         ! Cannot find any path for unknown reasons
         IF (ds == UNDEF .OR. ds == 0.d0) THEN

           nerror = nerror + 1
           errCode = 8

           trajectories(ntrac)%active = .FALSE.

           CALL write_error(errCode)

         END IF

      CASE DEFAULT

          errCode = 0

     END SELECT

  END SUBROUTINE errorCheck

  SUBROUTINE write_error(errCode)
  ! --------------------------------------------------
  !
  ! Purpose:
  ! Write error file
  !
  ! --------------------------------------------------
       INTEGER, INTENT(IN)                 :: errCode

       xw = x1; yw = y1; zw = z1
       CALL reverse()

       SELECT CASE(timeformat)

           CASE(0)
           ! Include time - tt in seconds
           WRITE(53,"(I8,3(',',F13.5),2(',',F20.5),1(',  ',A100))")  ntrac, xw, yw, zw, subvol, tt, &
                 ADJUSTL(errorType(errCode))
           RETURN

           CASE(1)
           ! Include time - Fraction ts
           WRITE(53,"(I8,3(',',F13.5),1(',',F20.5),1(',',F13.5),1(',  ',A100))")  ntrac, xw, yw, zw, subvol, ts, &
                 ADJUSTL(errorType(errCode))
           RETURN

           CASE(2)
           ! Include time - YYYY MM DD HH MM SS
           CALL tt_calendar(tt)
           WRITE(53,"(I8,3(',',F13.5),1(',',F20.5),(',',I5),3(',',I3),1(',  ',A100))")  ntrac, xw, yw, zw, &
                 subvol, dateYear, dateMon, dateDay, dateHour, ADJUSTL(errorType(errCode))
           RETURN

       END SELECT

   END SUBROUTINE


  CHARACTER(LEN=100) FUNCTION errorType(errCode)
  ! --------------------------------------------------
  !
  ! Purpose:
  ! Describes the error types
  !
  ! --------------------------------------------------
    INTEGER, INTENT(IN)         :: errCode

    SELECT CASE(errCode)
    CASE(1)
        errorType = 'Trapped in an infinite loop'
    CASE(2)
        errorType = 'Volume error - negative or zero'
    CASE(3)
        errorType = 'Trajectory leaving domain'
    CASE(4)
        errorType = 'Trajectory hits land'
    CASE(5)
        errorType = 'Wrong box horizontal (i - index)'
    CASE(6)
        errorType = 'Wrong box horizontal (j - index)'
    CASE(7)
        errorType = 'Wrong box vertical (k - index)'
    CASE(8)
        errorType = 'ds error - unknown path'
    END SELECT

  END FUNCTION errorType

  SUBROUTINE reverse()
  ! --------------------------------------------------
  !
  ! Purpose:
  ! Reverse seeding indexes according to the project type
  !
  ! --------------------------------------------------

      IF (griddir(2) == -1) THEN
          yw = jmt - yw    ! Meridional reverse
      END IF

      IF (griddir(3) == -1) THEN
          zw = km - zw     ! Vertical reverse
      END IF

  END SUBROUTINE reverse

END MODULE mod_error
