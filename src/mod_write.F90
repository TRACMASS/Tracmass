MODULE mod_write
    !!------------------------------------------------------------------------------
    !!
    !!       MODULE: mod_write
    !!
    !!          Opening/ Closing outfiles
    !!          & Writing trajectories to files
    !!
    !!          Opening/ Reading rerun files
    !!
    !!          Subroutines included:
    !!               - open_outfiles
    !!               - reopen_outfiles
    !!               - close_outfiles
    !!               - write_data
    !!               - read_data
    !!               - read_rerun
    !!               - open_outstream
    !!               - close_outstream
    !!               - write_stream
    !!
    !!               - reverse (P)
    !!               - writeformat (P)
    !!
    !!------------------------------------------------------------------------------

    USE mod_precdef
    USE mod_traj
    USE mod_trajdef
    USE mod_time
    USE mod_loopvars
    USE mod_calendar
    USE mod_psi
    USE mod_grid
    USE mod_tracervars
    USE mod_postprocessvars

    IMPLICIT NONE

    CHARACTER(LEN=200)    :: fullWritePref
    CHARACTER(LEN=200)    :: outDataDir, outDataFile
    CHARACTER(LEN=50)     :: psiformat
    CHARACTER(LEN=100)    :: outformat
    CHARACTER(LEN=*), PARAMETER                :: reform = "(I8,I3)"

    REAL(DP)              :: xw,yw,zw

    INTEGER               :: timeformat
    INTEGER               :: filestat
    INTEGER               :: numline
    INTEGER               :: ll
    INTEGER               :: lbas
    INTEGER               :: ilvar, itrac

    LOGICAL               :: fileexists

    PRIVATE               :: reverse, writeformat

    CONTAINS

      SUBROUTINE open_outfiles
      ! --------------------------------------------------
      !
      ! Purpose:
      ! Open outfiles ini, run, out, err, rerun
      !
      ! --------------------------------------------------

          ! Call mkdir to create directory if it does not exist
          IF (TRIM(outDataDir)=='') THEN
                CALL GETCWD(outDataDir)
                outDataDir = TRIM(outDataDir)//'/'
          END IF
          IF (TRIM(outDatafile)=='') outDatafile = 'TRACMASS'

          CALL SYSTEM( 'mkdir -p '//TRIM(outDataDir) )

          fullWritePref =  TRIM(outDataDir)//TRIM(outDataFile)

          OPEN(UNIT=50, FILE = TRIM(fullWritePref)//'_ini.csv', STATUS='replace')
          OPEN(UNIT=51, FILE = TRIM(fullWritePref)//'_run.csv', STATUS='replace')
          OPEN(UNIT=52, FILE = TRIM(fullWritePref)//'_out.csv', STATUS='replace')
          OPEN(UNIT=53, FILE = TRIM(fullWritePref)//'_err.csv', STATUS='replace')
          OPEN(UNIT=54, FILE = TRIM(fullWritePref)//'_rerun.csv', STATUS='replace')

      END SUBROUTINE open_outfiles

      SUBROUTINE reopen_outfiles
      ! --------------------------------------------------
      !
      ! Purpose:
      ! Re-Open outfiles ini, run, out, err, rerun
      ! for postprocessing.
      !
      ! --------------------------------------------------

          ! Call mkdir to create directory if it does not exist
          IF (TRIM(outDataDir)=='') THEN
                CALL GETCWD(outDataDir)
                outDataDir = TRIM(outDataDir)//'/'
          END IF
          IF (TRIM(outDatafile)=='') outDatafile = 'TRACMASS'

          CALL SYSTEM( 'mkdir -p '//TRIM(outDataDir) )

          fullWritePref =  TRIM(outDataDir)//TRIM(outDataFile)

          OPEN(UNIT=50, FILE = TRIM(fullWritePref)//'_ini.csv', STATUS='old',ACTION='read')
          OPEN(UNIT=51, FILE = TRIM(fullWritePref)//'_run.csv', STATUS='old',ACTION='read')
          OPEN(UNIT=52, FILE = TRIM(fullWritePref)//'_out.csv', STATUS='old',ACTION='read')
          OPEN(UNIT=53, FILE = TRIM(fullWritePref)//'_err.csv', STATUS='old',ACTION='read')
          OPEN(UNIT=54, FILE = TRIM(fullWritePref)//'_rerun.csv', STATUS='old',ACTION='read')

      END SUBROUTINE reopen_outfiles

      SUBROUTINE close_outfiles
      ! --------------------------------------------------
      !
      ! Purpose:
      ! Close outfiles ini, run, out, err, rerun
      !
      ! --------------------------------------------------

          CLOSE (50)
          CLOSE (51)
          CLOSE (52)
          CLOSE (53)
          CLOSE (54)

      END SUBROUTINE close_outfiles

      SUBROUTINE write_data(sel)
      ! --------------------------------------------------
      !
      ! Purpose:
      ! Write data to outfile
      !
      ! --------------------------------------------------

          CHARACTER(LEN=*)     :: sel

          REAL(DP)  :: tout

          SELECT CASE (TRIM(sel))


          ! INI file
          CASE ('ini')

              xw = x1; yw = y1; zw = z1

              IF (l_subdom) THEN
                  xw = xw + imindom - 1;
                  yw = yw + jmindom - 1;

                  IF ( (imindom > imaxdom) .AND. xw >imtdom )  xw = xw - imtdom
              END IF

              CALL reverse()

              SELECT CASE(timeformat)

                  CASE(0)
                  ! Include time - tt in seconds
                  IF (l_tracers) THEN
                      CALL writeformat(timeformat)
                      WRITE(50,FMT=TRIM(outformat))  ntrac, xw, yw, zw, subvol/trunit, nff*tt, trajectories(ntrac)%tracerval
                  ELSE
                      CALL writeformat(timeformat)
                      WRITE(50,FMT=TRIM(outformat))  ntrac, xw, yw, zw, subvol/trunit, nff*tt
                  END IF

                  RETURN

                  CASE(1)
                  ! Include time - Fraction ts
                  IF (l_tracers) THEN
                      CALL writeformat(timeformat)
                      WRITE(50,FMT=TRIM(outformat))  ntrac, xw, yw, zw, subvol/trunit, ts, trajectories(ntrac)%tracerval
                  ELSE
                      CALL writeformat(timeformat)
                      WRITE(50,FMT=TRIM(outformat))  ntrac, xw, yw, zw, subvol/trunit, ts
                  END IF

                  RETURN

                  CASE(2)
                  ! Include time - YYYY MM DD HH MM SS
                  CALL tt_calendar(nff*tt)
                  IF (l_tracers) THEN
                      CALL writeformat(timeformat)
                      WRITE(50,FMT=TRIM(outformat))  ntrac, xw, yw, zw, &
                          subvol/trunit, dateYear, dateMon, dateDay, dateHour, trajectories(ntrac)%tracerval
                  ELSE
                      CALL writeformat(timeformat)
                      WRITE(50,FMT=TRIM(outformat))  ntrac, xw, yw, zw, &
                          subvol/trunit, dateYear, dateMon, dateDay, dateHour
                  END IF

                  RETURN

              END SELECT

          ! RUN file
          CASE ('run')

              IF(  ( write_frec == 1 .AND. trajectories(ntrac)%niter == niter-1) .OR. &
                   ( write_frec == 2 .AND. ABS(tss-DBLE(INT(tss)))<1e-11 .AND. ints == NINT(ts)) .OR. &
                   ( write_frec == 3 .AND. .not.scrivi) .OR. &
                   ( write_frec == 4 ) .OR. &
                   ( write_frec == 2 .AND. tt == 0.d0)) THEN

                  ! If postprocessing is activated
                  nsavewrite(ntrac) = nsavewrite(ntrac) + 1

                  IF (write_frec == 1) THEN
                        xw = x0; yw = y0; zw = z0
                  ELSE
                        xw = x1; yw = y1; zw = z1
                  END IF

                  IF (l_subdom) THEN
                      xw = xw + imindom - 1;
                      yw = yw + jmindom - 1;

                      IF ( (imindom > imaxdom) .AND. xw > imtdom )  xw = xw - imtdom
                  END IF

                  CALL reverse()

                  SELECT CASE(timeformat)

                      CASE(0)
                      ! Include time - tt in seconds
                      tout = nff*tt
                      IF (write_frec == 1) tout = nff*(tt-dt)

                      IF (l_tracers) THEN
                          CALL writeformat(timeformat)
                          WRITE(51,FMT=TRIM(outformat))  ntrac, xw, yw, zw, subvol/trunit, tout, trajectories(ntrac)%tracerval
                      ELSE
                          CALL writeformat(timeformat)
                          WRITE(51,FMT=TRIM(outformat))  ntrac, xw, yw, zw, subvol/trunit, tout
                      END IF

                      RETURN

                      CASE(1)
                      ! Include time - Fraction ts
                      tout = ts
                      IF (write_frec == 1) tout = ts-dts

                      IF (l_tracers) THEN
                          CALL writeformat(timeformat)
                          WRITE(51,FMT=TRIM(outformat))  ntrac, xw, yw, zw, subvol/trunit, tout, trajectories(ntrac)%tracerval
                      ELSE
                          CALL writeformat(timeformat)
                          WRITE(51,FMT=TRIM(outformat))  ntrac, xw, yw, zw, subvol/trunit, tout
                      END IF

                      RETURN

                      CASE(2)
                      ! Include time - YYYY MM DD HH MM SS
                      IF (write_frec == 1) THEN
                        CALL tt_calendar(nff*REAL(NINT(tt - dt),8))
                      ELSE
                        CALL tt_calendar(nff*REAL(NINT(tt),8))
                      END IF

                      IF (l_tracers) THEN
                          CALL writeformat(timeformat)
                          WRITE(51,FMT=TRIM(outformat))  ntrac, xw, yw, zw, &
                              subvol/trunit, dateYear, dateMon, dateDay, dateHour, trajectories(ntrac)%tracerval
                      ELSE
                          CALL writeformat(timeformat)
                          WRITE(51,FMT=TRIM(outformat))  ntrac, xw, yw, zw, &
                              subvol/trunit, dateYear, dateMon, dateDay, dateHour
                      END IF

                      RETURN

                  END SELECT
              END IF

          ! OUT file
          CASE ('out')

              ! If postprocessing is activated
              IF (l_psi .OR. l_summary) THEN
                  nsave = MAX(nsavewrite(ntrac), nsave)
              END IF

              ! Correct indexes
              xw = x1; yw = y1; zw = z1

              IF (l_subdom) THEN
                  xw = xw + imindom - 1;
                  yw = yw + jmindom - 1;

                  IF ( (imindom > imaxdom) .AND. xw > imtdom )  xw = xw - imtdom
              END IF

              CALL reverse()

              SELECT CASE(timeformat)

                  CASE(0)
                  ! Include time - tt in seconds
                  IF (l_tracers) THEN
                      CALL writeformat(timeformat)
                      WRITE(52,FMT=TRIM(outformat))  ntrac, xw, yw, zw, subvol/trunit, nff*tt, trajectories(ntrac)%tracerval
                  ELSE
                      CALL writeformat(timeformat)
                      WRITE(52,FMT=TRIM(outformat))  ntrac, xw, yw, zw, subvol/trunit, nff*tt
                  END IF

                  RETURN

                  CASE(1)
                  ! Include time - Fraction ts
                  IF (l_tracers) THEN
                      CALL writeformat(timeformat)
                      WRITE(52,FMT=TRIM(outformat))  ntrac, xw, yw, zw, subvol/trunit, ts, trajectories(ntrac)%tracerval
                  ELSE
                      CALL writeformat(timeformat)
                      WRITE(52,FMT=TRIM(outformat))  ntrac, xw, yw, zw, subvol/trunit, ts
                  END IF

                  RETURN

                  CASE(2)
                  ! Include time - YYYY MM DD HH MM SS
                  CALL tt_calendar(nff*tt)
                  IF (l_tracers) THEN
                      CALL writeformat(timeformat)
                      WRITE(52,FMT=TRIM(outformat))  ntrac, xw, yw, zw, &
                          subvol/trunit, dateYear, dateMon, dateDay, dateHour, trajectories(ntrac)%tracerval
                  ELSE
                      CALL writeformat(timeformat)
                      WRITE(52,FMT=TRIM(outformat))  ntrac, xw, yw, zw, &
                          subvol/trunit, dateYear, dateMon, dateDay, dateHour
                  END IF

                  RETURN

              END SELECT

          ! RERUN file
          CASE ('rerun')
              WRITE(54,"(I8,',',I3,',',I10)")  ntrac, nend, nsavewrite(ntrac)
          END SELECT

      END SUBROUTINE write_data

      SUBROUTINE read_data(nunit, ierr)
      ! --------------------------------------------------
      !
      ! Purpose:
      ! Read data from file unit nunit
      !
      ! --------------------------------------------------

      INTEGER, INTENT(IN)  :: nunit
      INTEGER, INTENT(OUT) :: ierr

        SELECT CASE(timeformat)

          CASE(0)

            ! Include time - tt in seconds
            IF (l_tracers) THEN
                READ(nunit,FMT=*,iostat=ierr)  ntrac, x1, y1, z1, subvol, tt, tracervalue
            ELSE
                READ(nunit,FMT=*,iostat=ierr)  ntrac, x1, y1, z1, subvol, tt
            END IF

         CASE(1)

           ! Include time - Fraction ts
           IF (l_tracers) THEN
               READ(nunit,FMT=*,iostat=ierr)  ntrac, x1, y1, z1, subvol, ts, tracervalue
           ELSE
               READ(nunit,FMT=*,iostat=ierr)  ntrac, x1, y1, z1, subvol, ts
           END IF

        CASE(2)

           ! Include time - YYYY MM DD HH MM SS
           IF (l_tracers) THEN
               READ(nunit,FMT=*,iostat=ierr)  ntrac, x1, y1, z1, &
                   subvol, dateYear, dateMon, dateDay, dateHour, tracervalue
           ELSE
               READ(nunit,FMT=*,iostat=ierr)  ntrac, x1, y1, z1, &
                   subvol, dateYear, dateMon, dateDay, dateHour
           END IF

        END SELECT

      END SUBROUTINE read_data

      SUBROUTINE read_rerun
      ! --------------------------------------------------
      !
      ! Purpose:
      ! Open and read_rerun information
      !
      ! --------------------------------------------------

      ! Test if file exists, and read it if it does
      fullWritePref =  TRIM(outDataDir)//TRIM(outDataFile)

      INQUIRE (FILE = TRIM(fullWritePref)//'_rerun.csv', exist=fileexists)
      IF (fileexists) THEN

          numline=0

          OPEN(UNIT=34,FILE=TRIM(fullWritePref)//'_rerun.csv', ACCESS = 'SEQUENTIAL', &
              FORM = 'FORMATTED', ACTION = 'READ')

          findRecl: DO
          READ (UNIT=34, fmt=reform,iostat=filestat)
          IF (filestat < 0) THEN
              EXIT findRecl
          END IF
          numline = numline+1
          END DO findRecl

          REWIND (34)

          DO ll = 1, numline
              READ (UNIT=34, fmt="(I8,I3,I10)") ntrac, lbas, nsavewrite(ntrac)
              trajectories(ntrac)%lbas = lbas
          END DO

          CLOSE(34)

      ELSE
          PRINT*, '-----------------------------------'
          PRINT*, 'ERROR!'
          PRINT*, 'No rerun file'
          PRINT*, '-----------------------------------'
      END IF

      END SUBROUTINE read_rerun

      SUBROUTINE open_outstream(ccase)
      ! --------------------------------------------------
      !
      ! Purpose:
      ! Open streamfunction files
      !
      ! --------------------------------------------------
          CHARACTER(LEN=2), INTENT(IN) :: ccase

          fullWritePref =  TRIM(outDataDir)//TRIM(outDataFile)

          IF (TRIM(ccase) == "xy") OPEN(UNIT=60, FILE = TRIM(fullWritePref)//'_psixy.csv', STATUS='replace')
          IF (TRIM(ccase) == "xz") OPEN(UNIT=61, FILE = TRIM(fullWritePref)//'_psixz.csv', STATUS='replace')
          IF (TRIM(ccase) == "yz") OPEN(UNIT=62, FILE = TRIM(fullWritePref)//'_psiyz.csv', STATUS='replace')
          IF (TRIM(ccase) == "xr") OPEN(UNIT=63, FILE = TRIM(fullWritePref)//'_psixr.csv', STATUS='replace')
          IF (TRIM(ccase) == "yr") OPEN(UNIT=64, FILE = TRIM(fullWritePref)//'_psiyr.csv', STATUS='replace')
          IF (TRIM(ccase) == "rr") OPEN(UNIT=65, FILE = TRIM(fullWritePref)//'_psirr.csv', STATUS='replace')


      END SUBROUTINE open_outstream

      SUBROUTINE close_outstream(ccase)
      ! --------------------------------------------------
      !
      ! Purpose:
      ! Close streamfunction files
      !
      ! --------------------------------------------------
          CHARACTER(LEN=2), INTENT(IN) :: ccase

          IF (TRIM(ccase) == "xy") CLOSE(60)
          IF (TRIM(ccase) == "xz") CLOSE(61)
          IF (TRIM(ccase) == "yz") CLOSE(62)
          IF (TRIM(ccase) == "xr") CLOSE(63)
          IF (TRIM(ccase) == "yr") CLOSE(64)
          IF (TRIM(ccase) == "rr") CLOSE(65)

      END SUBROUTINE close_outstream

      SUBROUTINE write_stream(ijk1, ijk2, psicase)
      ! --------------------------------------------------
      !
      ! Purpose:
      ! Write stream functions
      !
      ! --------------------------------------------------


      INTEGER, INTENT(IN)          :: ijk1, ijk2
      CHARACTER(LEN=2), INTENT(IN) :: psicase

      psiformat = "(F22.5,XXXXXX(',',F22.5))"

      WRITE(psiformat(8:13),"(I6)") ijk1-1

      IF (psicase=='xr' .OR. psicase=='yr') THEN
          DO itrac = 1, numtracers
            DO ilvar = 1, ijk2
              IF (psicase=='xr') WRITE(63,TRIM(psiformat)) psi_xr(:,ilvar,itrac)
              IF (psicase=='xr') WRITE(64,TRIM(psiformat)) psi_yr(:,ilvar,itrac)
            END DO
          END DO
      ELSE IF (psicase == 'rr') THEN
          DO ilvar = 1, ijk2
             WRITE(65,TRIM(psiformat)) psi_rr(:,ilvar)
          END DO
      ELSE
          DO ilvar = 1, ijk2
            IF (psicase=='xy') WRITE(60,TRIM(psiformat)) psi_xy(:,ilvar)
            IF (psicase=='xz') WRITE(61,TRIM(psiformat)) psi_xz(:,ilvar)
            IF (psicase=='yz') WRITE(62,TRIM(psiformat)) psi_yz(:,ilvar)
          END DO
      END IF

      END SUBROUTINE write_stream

      SUBROUTINE reverse()
      ! --------------------------------------------------
      !
      ! Purpose:
      ! Reverse seeding indexes according to the project type
      !
      ! --------------------------------------------------

        IF (zeroindx) THEN
           xw = xw - 1
           IF (xw<0.) xw = imtdom + xw
        END IF

        IF (griddir(2) == -1) THEN
            yw = jmt - yw    ! Meridional reverse
        END IF

        IF (griddir(3) == -1) THEN
            zw = km - zw     ! Vertical reverse
        END IF

      END SUBROUTINE reverse

      SUBROUTINE writeformat(tform)
      ! --------------------------------------------------
      !
      ! Purpose:
      ! Updates the write/read format
      !
      ! --------------------------------------------------

        INTEGER, INTENT(IN) :: tform

        SELECT CASE(tform)
          CASE(0)

            IF (write_form == 0 .AND. l_tracers) THEN
              outformat = "(I8,3(',',F8.2),2(',',F16.2),XX(',',F8.2))"
              WRITE(outformat(30:31),"(I2)") numtracers
            ELSE IF (write_form == 0 ) THEN
              outformat = "(I8,3(',',F8.2),2(',',F16.2))"
            ELSE IF (write_form == 1 .AND. l_tracers) THEN
              outformat = "(I8,3(',',F13.5),2(',',F20.5),XX(',',F13.5))"
              WRITE(outformat(31:32),"(I2)") numtracers
            ELSE IF (write_form == 1 ) THEN
              outformat = "(I8,3(',',F13.5),2(',',F20.5))"
            END IF

          CASE(1)

            IF (write_form == 0 .AND. l_tracers) THEN
              outformat = "(I8,3(',',F8.2),2(',',F16.2),XX(',',F8.2))"
              WRITE(outformat(30:31),"(I2)") numtracers
            ELSE IF (write_form == 0 ) THEN
              outformat = "(I8,3(',',F8.2),2(',',F16.2))"
            ELSE IF (write_form == 1 .AND. l_tracers) THEN
              outformat = "(I8,3(',',F13.5),2(',',F20.5),XX(',',F13.5))"
              WRITE(outformat(31:32),"(I2)") numtracers
            ELSE IF (write_form == 1 ) THEN
              outformat = "(I8,3(',',F13.5),2(',',F20.5))"
            END IF

          CASE(2)

            IF (write_form == 0 .AND. l_tracers) THEN
              outformat = "(I8,3(',',F8.2),1(',',F16.2),(',',I5),3(',',I3),XX(',',F8.2))"
              WRITE(outformat(49:50),"(I2)") numtracers
            ELSE IF (write_form == 0 ) THEN
              outformat = "(I8,3(',',F8.2),1(',',F16.2),(',',I5),3(',',I3))"
            ELSE IF (write_form == 1 .AND. l_tracers) THEN
              outformat = "(I8,3(',',F13.5),1(',',F20.5),(',',I5),3(',',I3),XX(',',F13.5))"
              WRITE(outformat(50:51),"(I2)") numtracers
            ELSE IF (write_form == 1 ) THEN
              outformat = "(I8,3(',',F13.5),1(',',F20.5),(',',I5),3(',',I3))"
            END IF

        END SELECT

      END SUBROUTINE writeformat

END MODULE mod_write
