MODULE mod_postprocess
    !!------------------------------------------------------------------------------
    !!
    !!       MODULE: mod_postprocess
    !!
    !!          Postprocess the output data of TRACMASS
    !!          and computes a short summary of the output
    !!
    !!          Subroutines included:
    !!               - postprocessing
    !!               - init_alloc_stream
    !!               - print_summary
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
    USE mod_domain
    USE mod_postprocessvars
    USE mod_stream
    USE mod_write

    IMPLICIT NONE

    INTEGER                      :: filestat0, filestat1, filestat2, filestat3

    INTEGER                      :: ntrac1, nkzone, maxnkzone = 0, nn = 0

    CONTAINS

      SUBROUTINE postprocessing()
      ! ------------------------------------------------------------
      !
      ! Purpose:
      ! Main post processing subroutine.
      ! Rereads the output files and computes the summary and the streamfunctions
      !
      ! ------------------------------------------------------------

        INTEGER :: nsave1

        filestat0 = 0; filestat1 = 0; filestat2 = 0; filestat3 = 0

        PRINT*, '- Reading output data'

        ! Compute total number of trajectories and allocate arrays
        CALL init_alloc_stream()

        DO WHILE (filestat2 ==0)

            ! Read rerun file
            ! --------------------------------------------------------------------
            READ(54,*, iostat=filestat0) ntrac1, nkzone, nsave1

            maxnkzone = MAX(nkzone, maxnkzone)

            IF (filestat0 == 0) traj_out(ntrac1)  = nkzone

            ! Number of trajectories per killing zone
            IF (filestat0==0 .AND. l_summary) ntrajout(nkzone) = ntrajout(nkzone) + 1


            ! Read ini file
            ! --------------------------------------------------------------------
            CALL read_data(50, filestat1)

            ! Total volume and number of trajectories
            IF (filestat1==0 .AND. l_summary)  ntrajtot = ntrajtot + 1
            IF (filestat1==0 .AND. l_summary)  voltot   = voltot + subvol

            ! Volume transported by the trajectories
            IF (filestat1==0 .AND. l_psi)  traj_subvol(ntrac) = subvol


            ! Read out file
            ! --------------------------------------------------------------------
            CALL read_data(52, filestat3)

            ! Total volume transport per killing zone
            IF (filestat3==0 .AND. ntrac1 == ntrac .AND. l_summary)  volout(nkzone) = volout(nkzone) + subvol


            ! Read run file
            ! --------------------------------------------------------------------
            CALL read_data(51, filestat2)

            ! If stream fuctions are computed
            IF (l_psi .AND. traj_out(ntrac)>0 .AND. l_offline .AND. filestat2 == 0) THEN
                  counter(ntrac) = counter(ntrac) + 1

                  traj_x(ntrac, counter(ntrac)) = x1
                  traj_y(ntrac, counter(ntrac)) = y1
                  traj_z(ntrac, counter(ntrac)) = z1

                  IF (l_tracers) traj_t(ntrac, counter(ntrac), :) = tracervalue
            END IF

        END DO

        IF (l_summary)  CALL print_summary
        IF (l_psi)      CALL compute_stream

      END SUBROUTINE postprocessing


      SUBROUTINE init_alloc_stream()
      ! ------------------------------------------------------------
      !
      ! Purpose:
      ! Allocate arrays to compute streamfunctions
      !
      ! ------------------------------------------------------------


        INTEGER         :: nc1, nc2, nc3
        INTEGER         :: ierr = 0

        ! ntractot not calculated
        IF (ntractot==0) THEN
            DO WHILE (ierr ==0)
                READ(50,FMT=*,iostat=ierr) ntractot
            END DO

            REWIND(50)
            ierr = 0
        END IF

        ! nsave not calculated
        IF (nsave==0) THEN
            DO WHILE (ierr ==0)
                READ(54,FMT=*,iostat=ierr) nc1, nc2, nc3
                nsave = MAX(nsave, nc3+10)
            END DO

            REWIND(54)
            ierr = 0

        END IF

        ! Zonal index
        ALLOCATE(traj_x(ntractot,nsave)); traj_x = -999.
        ! Meridional index
        ALLOCATE(traj_y(ntractot,nsave)); traj_y = -999.
        ! Vertical index
        ALLOCATE(traj_z(ntractot,nsave)); traj_z = -999.
        ! Subvol
        ALLOCATE(traj_subvol(ntractot));  traj_subvol = 0.

        ! Tracer array
        IF (l_tracers) THEN
          ALLOCATE(traj_t(ntractot,nsave, numtracers))
          traj_t = -999.
        END IF

        ! Counter
        ALLOCATE(counter(ntractot)); counter = 0
        ALLOCATE(traj_out(ntractot)); traj_out = -1

      END SUBROUTINE


      SUBROUTINE print_summary()
      ! ------------------------------------------------------------
      !
      ! Purpose:
      ! Print an extended summary of the number of trajectories and
      ! volume transport.
      !
      ! ------------------------------------------------------------

        PRINT*, ' '
        PRINT*, '- Summary:'
        PRINT*, '                     ----------------------------------------------------------'
        PRINT*, '                     | Trajectories |    [%]  ||     Transport      |    [%]  |'
        PRINT*, '-------------------------------------------------------------------------------'

        PRINT"(A24,I12,A14,F18.2,A13)", '  Seeded (Total)     | ',ntrajtot,' | 100.000 || ',voltot,' | 100.000 | '
        PRINT*, '-------------------------------------------------------------------------------'

        PRINT"(A24,I12,A3,F7.3,A4,F18.2,A3,F7.3,A3)", '   Terminated         | ', SUM(ntrajout),' | ',&
                100*REAL(SUM(ntrajout))/REAL(ntrajtot),' || ',SUM(volout),' | ', 100*SUM(volout)/voltot,' | '
        PRINT*, '-------------------------------------------------------------------------------'
        PRINT"(A24,I12,A3,F7.3,A4,F18.2,A3,F7.3,A3)", '   - Excess of time   | ',ntrajout(0),' | ',&
                100.*REAL(ntrajout(0))/REAL(ntrajtot),' || ',volout(0),' | ',100.*volout(0)/voltot,' | '
        PRINT*, '-------------------------------------------------------------------------------'

        PRINT"(A24,I12,A3,F7.3,A4,F18.2,A3,F7.3,A3)", '   - Reach surface    | ',ntrajout(1),' | ',&
                100.*REAL(ntrajout(1))/REAL(ntrajtot),' || ',volout(1),' | ',100.*volout(1)/voltot,' | '
        PRINT*, '-------------------------------------------------------------------------------'

        DO nn = 2, maxnkzone
          PRINT"(A19,I2,A3,I12,A3,F7.3,A4,F18.2,A3,F7.3,A3)", '  - Reach domain (',nn-1,')| ',ntrajout(nn),' | ',&
                  100.*REAL(ntrajout(nn))/REAL(ntrajtot),' || ',volout(nn),' | ',100.*volout(nn)/voltot,' | '
        END DO

        PRINT*, '-------------------------------------------------------------------------------'

      END SUBROUTINE print_summary

END MODULE mod_postprocess
