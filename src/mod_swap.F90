MODULE mod_swap
  !!------------------------------------------------------------------------------
  !!
  !!       MODULE: mod_swap
  !!
  !!          Swaps the variable's time indexes before reading the new data,
  !!          corrects the sign of data depending of the time direction.
  !!
  !!
  !!          Subroutines included:
  !!               - swap_time
  !!               - swap_sign
  !!
  !!------------------------------------------------------------------------------

  USE mod_vel,  only            : uflux, vflux, wflux
  USE mod_grid, only            : dzt, dzu, dzv, dzdt, zstot, hs
  USE mod_tracervars, only      : l_tracers, l_swtraj, tracers, numtracers, tracertraj
  USE mod_time, only            : nff

  IMPLICIT NONE

  CONTAINS

  SUBROUTINE swap_time()
  ! --------------------------------------------------
  !
  ! Purpose:
  ! Swaps the time index of variables such as uflux, vflux, etc
  !
  ! --------------------------------------------------

        IMPLICIT NONE

        INTEGER  :: itrac

        ! Swap fluxes
        uflux(:,:,:,1) = uflux(:,:,:,2)
        vflux(:,:,:,1) = vflux(:,:,:,2)
#if defined w_explicit
        wflux(:,:,:,1) = wflux(:,:,:,2)
#endif


        ! Vertical resolution and mass changes
        dzt(:,:,:,1)  = dzt(:,:,:,2)
        dzt(:,:,:,2)  = dzt(:,:,:,3)

        dzu(:,:,:,1) = dzu(:,:,:,2)
        dzv(:,:,:,1) = dzv(:,:,:,2)

        dzdt(:,:,:,1) = dzdt(:,:,:,2)


        ! Scale factors and surface variables
        zstot(:,:,-1) = zstot(:,:,0)
        zstot(:,:, 0) = zstot(:,:,1)

        hs(:,:,-1)   = hs(:,:,0)
        hs(:,:,0)    = hs(:,:,1)


        ! Tracers and tracer trajectories
        IF (l_tracers) THEN

            DO itrac = 1, numtracers
                tracers(itrac)%data(:,:,:,1) = tracers(itrac)%data(:,:,:,2)
            END DO

            IF (l_swtraj) THEN
                tracertraj(:,:,:,-1) = tracertraj(:,:,:,0)
                tracertraj(:,:,:,0)  = tracertraj(:,:,:,1)
            END IF

        END IF

  END SUBROUTINE swap_time

  SUBROUTINE swap_sign()
  ! --------------------------------------------------
  !
  ! Purpose:
  ! Reverses the sign of fluxes if trajectories are run
  ! backward in time.
  !
  ! --------------------------------------------------

        uflux(:,:,:,2) = nff*uflux(:,:,:,2)
        vflux(:,:,:,2) = nff*vflux(:,:,:,2)
#if defined w_explicit
        wflux(:,:,:,2) = nff*wflux(:,:,:,2)
#endif

  END SUBROUTINE swap_sign

END MODULE mod_swap
