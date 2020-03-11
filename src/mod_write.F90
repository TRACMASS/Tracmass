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

    IMPLICIT NONE

    CHARACTER(LEN=200)    :: fullWritePref
    CHARACTER(LEN=200)    :: outDataDir, outDataFile
    CHARACTER(LEN=*), PARAMETER                :: reform = "(I8,I3)"

    INTEGER               :: timeformat
    INTEGER               :: filestat
    INTEGER               :: numline
    INTEGER               :: ll
    INTEGER               :: lbas

    LOGICAL               :: fileexists

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
        OPEN(UNIT=51, FILE = TRIM(fullWritePref)//'_run.csv', STATUS='replace')
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

        SELECT CASE (TRIM(sel))


        ! INI file
        CASE ('ini')

            SELECT CASE(timeformat)

                CASE(0)
                ! Include time - tt in seconds
                WRITE(50,"(I8,3(',',F13.5),2(',',F20.5))")  ntrac, x1, y1, z1, subvol, tt
                RETURN

                CASE(1)
                ! Include time - Fraction ts
                WRITE(50,"(I8,3(',',F13.5),1(',',F20.5),1(',',F13.5))")  ntrac, x1, y1, z1, subvol, ts
                RETURN

                CASE(2)
                ! Include time - YYYY MM DD HH MM SS
                CALL tt_calendar(tt)
                WRITE(50,"(I8,3(',',F13.5),1(',',F20.5),(',',I5),3(',',I3))")  ntrac, x1, y1, z1, &
                    subvol, dateYear, dateMon, dateDay, dateHour
                RETURN

            END SELECT

        ! RUN file
        CASE ('run')
            IF(  ( write_frec == 1 .AND. trajectories(ntrac)%niter == niter-1) .OR. &
                 ( write_frec == 2 .AND. tss==DBLE(INT(tss))     ) .OR. &
                 ( write_frec == 3 .AND. .not.scrivi ) .OR. &
                 ( write_frec == 4 ) ) THEN

                SELECT CASE(timeformat)

                    CASE(0)
                    ! Include time - tt in seconds
                    IF (write_frec == 1) THEN
                      WRITE(51,"(I8,3(',',F13.5),2(',',F20.5))")  ntrac, x0, y0, z0, subvol, tt - dt
                    ELSE
                      WRITE(51,"(I8,3(',',F13.5),2(',',F20.5))")  ntrac, x1, y1, z1, subvol, tt
                    END IF
                    RETURN

                    CASE(1)
                    ! Include time - Fraction ts
                    IF (write_frec == 1) THEN
                      WRITE(51,"(I8,3(',',F13.5),1(',',F20.5),1(',',F13.5))")  ntrac, x0, y0, z0, subvol, ts - dts
                    ELSE
                      WRITE(51,"(I8,3(',',F13.5),1(',',F20.5),1(',',F13.5))")  ntrac, x1, y1, z1, subvol, ts
                    END IF
                    RETURN

                    CASE(2)
                    ! Include time - YYYY MM DD HH MM SS
                    IF (write_frec == 1) THEN
                      CALL tt_calendar(tt - dt)
                      WRITE(51,"(I8,3(',',F13.5),1(',',F20.5),(',',I5),3(',',I3))")  ntrac, x1, y1, z1, &
                          subvol, dateYear, dateMon, dateDay, dateHour
                    ELSE
                      CALL tt_calendar(tt)
                      WRITE(51,"(I8,3(',',F13.5),1(',',F20.5),(',',I5),3(',',I3))")  ntrac, x1, y1, z1, &
                          subvol, dateYear, dateMon, dateDay, dateHour
                    END IF
                    RETURN

                END SELECT
            END IF

        ! OUT file
        CASE ('out')
            SELECT CASE(timeformat)

                CASE(0)
                ! Include time - tt in seconds
                WRITE(52,"(I8,3(',',F13.5),2(',',F20.5))")  ntrac, x1, y1, z1, subvol, tt
                RETURN

                CASE(1)
                ! Include time - Fraction ts
                WRITE(52,"(I8,3(',',F13.5),1(',',F20.5),1(',',F13.5))")  ntrac, x1, y1, z1, subvol, ts
                RETURN

                CASE(2)
                ! Include time - YYYY MM DD HH MM SS
                CALL tt_calendar(tt)
                WRITE(52,"(I8,3(',',F13.5),1(',',F20.5),(',',I5),3(',',I3))")  ntrac, x1, y1, z1, &
                    subvol, dateYear, dateMon, dateDay, dateHour
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

END MODULE mod_write
