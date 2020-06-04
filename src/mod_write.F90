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
    !!               - close_outfiles
    !!               - write_data
    !!               - read_rerun
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

    PRIVATE               :: reverse

    CONTAINS

    SUBROUTINE open_outfiles
    ! --------------------------------------------------
    !
    ! Purpose:
    ! Open outfiles ini, run, out, err, rerun
    !
    ! --------------------------------------------------

        fullWritePref =  TRIM(outDataDir)//TRIM(outDataFile)

        OPEN(UNIT=50, FILE = TRIM(fullWritePref)//'_ini.csv', STATUS='replace')
        IF (l_psi .EQV..FALSE.) OPEN(UNIT=51, FILE = TRIM(fullWritePref)//'_run.csv', STATUS='replace')
        OPEN(UNIT=52, FILE = TRIM(fullWritePref)//'_out.csv', STATUS='replace')
        OPEN(UNIT=53, FILE = TRIM(fullWritePref)//'_err.csv', STATUS='replace')
        OPEN(UNIT=54, FILE = TRIM(fullWritePref)//'_rerun.csv', STATUS='replace')

    END SUBROUTINE open_outfiles

    SUBROUTINE close_outfiles
    ! --------------------------------------------------
    !
    ! Purpose:
    ! Close outfiles ini, run, out, err, rerun
    !
    ! --------------------------------------------------

        CLOSE (50)
        IF (l_psi .EQV..FALSE.) CLOSE (51)
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
                    outformat = "(I8,3(',',F13.5),2(',',F20.5),XX(',',F13.5))"
                    WRITE(outformat(31:32),"(I2)") numtracers

                    WRITE(50,FMT=TRIM(outformat))  ntrac, xw, yw, zw, subvol, tt, trajectories(ntrac)%tracerval
                ELSE
                    outformat = "(I8,3(',',F13.5),2(',',F20.5))"

                    WRITE(50,FMT=TRIM(outformat))  ntrac, xw, yw, zw, subvol, tt
                END IF

                RETURN

                CASE(1)
                ! Include time - Fraction ts
                IF (l_tracers) THEN
                    outformat = "(I8,3(',',F13.5),2(',',F20.5),XX(',',F13.5))"
                    WRITE(outformat(31:32),"(I2)") numtracers+1

                    WRITE(50,FMT=TRIM(outformat))  ntrac, xw, yw, zw, subvol, ts, trajectories(ntrac)%tracerval
                ELSE
                    outformat = "(I8,3(',',F13.5),2(',',F20.5),1(',',F13.5))"

                    WRITE(50,FMT=TRIM(outformat))  ntrac, xw, yw, zw, subvol, ts
                END IF

                RETURN

                CASE(2)
                ! Include time - YYYY MM DD HH MM SS
                CALL tt_calendar(tt)
                IF (l_tracers) THEN
                    outformat = "(I8,3(',',F13.5),1(',',F20.5),(',',I5),3(',',I3),XX(',',F13.5))"
                    WRITE(outformat(50:51),"(I2)") numtracers

                    WRITE(50,FMT=TRIM(outformat))  ntrac, xw, yw, zw, &
                        subvol, dateYear, dateMon, dateDay, dateHour, trajectories(ntrac)%tracerval
                ELSE
                    outformat = "(I8,3(',',F13.5),1(',',F20.5),(',',I5),3(',',I3))"

                    WRITE(50,FMT=TRIM(outformat))  ntrac, xw, yw, zw, &
                        subvol, dateYear, dateMon, dateDay, dateHour
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
                    tout = tt
                    IF (write_frec == 1) tout = tt-dt

                    IF (l_tracers) THEN
                        outformat = "(I8,3(',',F13.5),2(',',F20.5),XX(',',F13.5))"
                        WRITE(outformat(31:32),"(I2)") numtracers
                        WRITE(51,FMT=TRIM(outformat))  ntrac, xw, yw, zw, subvol, tout, trajectories(ntrac)%tracerval
                    ELSE
                        outformat = "(I8,3(',',F13.5),2(',',F20.5))"

                        WRITE(51,FMT=TRIM(outformat))  ntrac, xw, yw, zw, subvol, tout
                    END IF

                    RETURN

                    CASE(1)
                    ! Include time - Fraction ts
                    tout = ts
                    IF (write_frec == 1) tout = ts-dts

                    IF (l_tracers) THEN
                        outformat = "(I8,3(',',F13.5),2(',',F20.5),XX(',',F13.5))"
                        WRITE(outformat(31:32),"(I2)") numtracers+1

                        WRITE(51,FMT=TRIM(outformat))  ntrac, xw, yw, zw, subvol, tout, trajectories(ntrac)%tracerval
                    ELSE
                        outformat = "(I8,3(',',F13.5),2(',',F20.5),1(',',F13.5))"

                        WRITE(51,FMT=TRIM(outformat))  ntrac, xw, yw, zw, subvol, tout
                    END IF

                    RETURN

                    CASE(2)
                    ! Include time - YYYY MM DD HH MM SS
                    IF (write_frec == 1) THEN
                      CALL tt_calendar(REAL(NINT(tt - dt),8))
                    ELSE
                      CALL tt_calendar(REAL(NINT(tt),8))
                    END IF

                    IF (l_tracers) THEN
                        outformat = "(I8,3(',',F13.5),1(',',F20.5),(',',I5),3(',',I3),XX(',',F13.5))"
                        WRITE(outformat(50:51),"(I2)") numtracers

                        WRITE(51,FMT=TRIM(outformat))  ntrac, xw, yw, zw, &
                            subvol, dateYear, dateMon, dateDay, dateHour, trajectories(ntrac)%tracerval
                    ELSE
                        outformat = "(I8,3(',',F13.5),1(',',F20.5),(',',I5),3(',',I3))"

                        WRITE(51,FMT=TRIM(outformat))  ntrac, xw, yw, zw, &
                            subvol, dateYear, dateMon, dateDay, dateHour
                    END IF

                    RETURN

                END SELECT
            END IF

        ! OUT file
        CASE ('out')

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
                    outformat = "(I8,3(',',F13.5),2(',',F20.5),XX(',',F13.5))"
                    WRITE(outformat(31:32),"(I2)") numtracers

                    WRITE(52,FMT=TRIM(outformat))  ntrac, xw, yw, zw, subvol, tt, trajectories(ntrac)%tracerval
                ELSE
                    outformat = "(I8,3(',',F13.5),2(',',F20.5))"

                    WRITE(52,FMT=TRIM(outformat))  ntrac, xw, yw, zw, subvol, tt
                END IF

                RETURN

                CASE(1)
                ! Include time - Fraction ts
                IF (l_tracers) THEN
                    outformat = "(I8,3(',',F13.5),2(',',F20.5),XX(',',F13.5))"
                    WRITE(outformat(31:32),"(I2)") numtracers+1

                    WRITE(52,FMT=TRIM(outformat))  ntrac, xw, yw, zw, subvol, ts, trajectories(ntrac)%tracerval
                ELSE
                    outformat = "(I8,3(',',F13.5),2(',',F20.5),1(',',F13.5))"

                    WRITE(52,FMT=TRIM(outformat))  ntrac, xw, yw, zw, subvol, ts
                END IF

                RETURN

                CASE(2)
                ! Include time - YYYY MM DD HH MM SS
                CALL tt_calendar(tt)
                IF (l_tracers) THEN
                    outformat = "(I8,3(',',F13.5),1(',',F20.5),(',',I5),3(',',I3),XX(',',F13.5))"
                    WRITE(outformat(50:51),"(I2)") numtracers

                    WRITE(52,FMT=TRIM(outformat))  ntrac, xw, yw, zw, &
                        subvol, dateYear, dateMon, dateDay, dateHour, trajectories(ntrac)%tracerval
                ELSE
                    outformat = "(I8,3(',',F13.5),1(',',F20.5),(',',I5),3(',',I3))"

                    WRITE(52,FMT=TRIM(outformat))  ntrac, xw, yw, zw, &
                        subvol, dateYear, dateMon, dateDay, dateHour
                END IF

                RETURN

            END SELECT

        ! RERUN file
        CASE ('rerun')
            WRITE(54,"(I8,I3)")  ntrac, nend
        END SELECT

    END SUBROUTINE write_data

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
            READ (UNIT=34, fmt="(I8,I3)") ntrac, lbas
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
        IF (TRIM(ccase) == "yz") OPEN(UNIT=61, FILE = TRIM(fullWritePref)//'_psiyz.csv', STATUS='replace')
        IF (TRIM(ccase) == "yr") OPEN(UNIT=62, FILE = TRIM(fullWritePref)//'_psiyr.csv', STATUS='replace')


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
        IF (TRIM(ccase) == "yz") CLOSE(61)
        IF (TRIM(ccase) == "yr") CLOSE(62)

    END SUBROUTINE close_outstream

    SUBROUTINE write_stream(ijk1, ijk2, psicase)
    ! --------------------------------------------------
    !
    ! Purpose:
    ! Write stream functions
    !
    ! --------------------------------------------------


    INTEGER, INTENT(IN) :: ijk1, ijk2
    CHARACTER(LEN=2), INTENT(IN) :: psicase

    psiformat = "(F22.5,XXXXXX(',',F22.5))"

    WRITE(psiformat(8:13),"(I6)") ijk1-1

    IF (psicase =='yr') THEN
        DO itrac = 1, numtracers
          DO ilvar = 1, ijk2
            IF (psicase=='yr') WRITE(62,TRIM(psiformat)) psi_yr(:,ilvar,itrac)
          END DO
        END DO
    ELSE
        DO ilvar = 1, ijk2
          IF (psicase=='xy') WRITE(60,TRIM(psiformat)) psi_xy(:,ilvar)
          IF (psicase=='yz') WRITE(61,TRIM(psiformat)) psi_yz(:,ilvar)
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

END MODULE mod_write
