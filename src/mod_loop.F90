MODULE mod_loop
!!---------------------------------------------------------------------------
!!
!!       MODULE mod_loop:
!!
!!          The main loop where new trajectory positions are
!!          calculated
!!
!!          Contains subroutines for computing the grid box volume, time,
!!          and writing data to files.
!!
!!          Subroutines included:
!!              - loop
!!
!!
!!
!!---------------------------------------------------------------------------

  USE mod_log,      only: log_level
  USE mod_print
  USE mod_seed
  USE mod_param
  USE mod_grid
  USE mod_domain
  USE mod_write
  USE mod_pos
  USE mod_calendar
  USE mod_clock
  USE mod_error

  USE mod_loopvars, only: niter, scrivi

  IMPLICIT NONE

  CONTAINS

  SUBROUTINE loop
  ! --------------------------------------------------
  !
  ! Purpose:
  ! TRACMASS main loop that updates trajectories
  !
  ! --------------------------------------------------

    IF(log_level >= 5) THEN
        PRINT*,' Entering loop '
    END IF

    CALL end_calendar

    CALL print_start_loop

    ! Time sub steps
    dstep = 1.d0 / DBLE(iter)
    dtmin = dstep * tseas

    ! Start main time loop
    ! =======================================================================
    intsTimeLoop: DO ints=1, intrun-1

        ! Read fields
        CALL read_field

        ! Seed trajectories
        CALL seed()

        ! Loop over all trajectories and calculate a new position for this time step
        ntracLoop: DO ntrac=1, ntractot

          IF (trajectories(ntrac)%active .EQV. .FALSE.) CYCLE ntracLoop

          ! Read in the position and other particles parameters
          x1         = trajectories(ntrac)%x1
          y1         = trajectories(ntrac)%y1
          z1         = trajectories(ntrac)%z1
          tt         = trajectories(ntrac)%tt
          subvol     = trajectories(ntrac)%subvol
          t0         = trajectories(ntrac)%t0

          ib     = trajectories(ntrac)%ib
          jb     = trajectories(ntrac)%jb
          kb     = trajectories(ntrac)%kb
          niter  = trajectories(ntrac)%niter
          ts     = DBLE(trajectories(ntrac)%nts)

          ! Error code
          errCode = 0

          ! To check the subcycles
          tss = 0.d0

          ! Start loop for each trajectory
          scrivi = .TRUE.

          niterLoop: DO

            niter = niter + 1 ! Iterative step of trajectory

            ! Store trajectory positions
            IF (niter/=1 .AND. trajectories(ntrac)%icycle/=1 .AND. tss==DBLE(iter)) THEN

              trajectories(ntrac)%x1     = x1
              trajectories(ntrac)%y1     = y1
              trajectories(ntrac)%z1     = z1
              trajectories(ntrac)%tt     = tt
              trajectories(ntrac)%subvol = subvol
              trajectories(ntrac)%ib     = ib
              trajectories(ntrac)%jb     = jb
              trajectories(ntrac)%kb     = kb
              trajectories(ntrac)%niter  = niter
              trajectories(ntrac)%nts    = IDINT(ts)
              trajectories(ntrac)%icycle = 1

              CYCLE ntracLoop

            END IF

            trajectories(ntrac)%icycle = 0

            ! time interpolation constant between 0 and 1
            intrpg = dmod(ts,1.d0) ! -> gets the fractional part
            intrpr = 1.d0-intrpg


            IF(intrpg.LT.0.d0 .OR. intrpg.GT.1.d0) THEN
               PRINT *,'* intrpg = ',intrpg
               PRINT *,'* intrpr = ',intrpr
               PRINT *,'Something went wrong with the time interpolation'
               STOP
            END IF

            ! Cyclic world ocean/atmosphere
            IF (iperio /= 0) THEN

               IF (ib == 1 .AND. x1 >= DBLE (IMT)) THEN
                  x1 = x1 - DBLE(IMT)
               END IF

               IF (x1 < 0.d0 ) THEN
                  x1 = x1 + DBLE(IMT)
               END IF

            END IF

            x0  = x1
            y0  = y1
            z0  = z1

            ia  = ib
            iam = ia-1
            IF (iam == 0) iam = IMT
            ja  = jb
            ka  = kb


            CALL errorCheck('coordBoxError' ,errCode)
            CALL errorCheck('infLoopError'  ,errCode)
            IF (errCode .NE. 0) CYCLE ntracLoop

            ! Interpolate volume
            !CALL calc_dxyz(intrpr, intrpg)
            dxyz = dxdy(ib,jb)*dz(ib,jb,kb)

            CALL errorCheck('dxyzError'     ,errCode)
            IF (errCode .NE. 0) CYCLE ntracLoop

            ! Calculate the 3 crossing times over the box  !
            ! choose the shortest time and calculate the   !
            ! new positions                                !
            !                                              !
            ! Solving the differential equations           !
            ! Note:                                        !
            ! space variables (x,...) are dimensionless    !
            ! time variables (ds,...) are in seconds/m^3   !
            !==============================================!

            dtreg = dtmin* (DBLE(INT(DBLE(iter)*tt/tseas,8)) + &
                    1.d0 - DBLE(iter)*tt/tseas)
            dt    = dtreg
            dsmin = dtreg/dxyz

            CALL vertvel(ia, iam, ja, ka)

            CALL cross_time(1,ia,ja,ka,x0,dse,dsw) ! zonal
            CALL cross_time(2,ia,ja,ka,y0,dsn,dss) ! meridional
            CALL cross_time(3,ia,ja,ka,z0,dsu,dsd) ! vertical

            ds = MIN(dse, dsw, dsn, dss, dsu, dsd, dsmin)
            CALL errorCheck('dsCrossError', errCode)
            IF (errCode.ne.0) CYCLE ntracLoop

            CALL update_traj(ia,iam,ja,ka,ib,jb,kb,x0,y0,z0,x1,y1,z1)


            CALL errorCheck('boundError', errCode)
            IF (errCode.ne.0) CYCLE ntracLoop
            CALL errorCheck('landError', errCode)
            IF (errCode.ne.0) CYCLE ntracLoop

            ! Update time tt and ts
            CALL update_time

            ! End trajectory if outside chosen domain
            IF ( z1>= DBLE(KM) ) THEN
                kb = KM
                !PRINT*, 'z1 is outside the domain'
                !PRINT*, 'z1 =', z1
                EXIT niterLoop

            ELSE IF (ia>imt .OR. ib>imt .OR. ja>jmt .OR. jb>jmt &
                 .OR. ia<1 .OR. ib<1 .OR. ja<1 .OR. jb<1) THEN
               !PRINT*, 'Trajectory outside domain'
               !PRINT*, 'ia =', ia, 'ib =', ib
               !PRINT*, 'ja =', ja, 'jb =', jb
               EXIT niterLoop

            ! Emd trajectory if time are exceeded
            ELSE IF (tt-t0 .GT. timax*24*3600) THEN
               !PRINT*, 'Time exceeded'
               !PRINT*, 'tt-t0 =', tt-t0
               !PRINT*, 'timax =', timax
               EXIT niterLoop

            ELSE
               CALL write_data('run')

            END IF

          END DO niterLoop

          nout = nout + 1

          ! Write out data
          CALL write_data('out')

          trajectories(ntrac)%active = .FALSE.

        END DO ntracLoop

        CALL print_cycle_loop()

        IF (ntractot /= 0 .AND. ntractot - nout - nerror == 0) THEN
           EXIT intsTimeLoop
        END IF

        CALL update_calendar

    END DO intsTimeLoop

    CALL print_end_loop

  END SUBROUTINE loop

END MODULE mod_loop
