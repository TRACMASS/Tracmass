SUBROUTINE read_field

  !==========================================================================
  !
  ! Purpose
  ! -------
  !
  ! Read test model output to advect trajectories.
  ! Will be called by the loop each time step.
  !
  ! Method
  ! ------
  !
  ! Read velocities and optionally some tracers from netCDF files and
  ! update velocity fields for TRACMASS.
  !
  ! Updates the variables:
  !   uflux and vflux
  ! ==========================================================================

      USE mod_vel
      USE mod_grid
      USE mod_time
      USE mod_param

      IMPLICIT NONE

      REAL(DP)    :: ug, u0, fcor, gamma, gammag, omtime

      INTEGER     :: jj, ii

      ! Parameters for Nicoletta Fabboni velocities.
      ug     = 0.04
      u0     = 0.5
      fcor   = 2. * 2. * pi/(24.*3600.) * COS(45.*radian)
      gamma  = 1./(2.89*24.*3600.)
      gammag = 1./(28.9*24.*3600.)
      omtime = nff * DBLE(ints) * DBLE(ngcm*3600)

      DO jj=1,JMT
          DO ii=1,IMT

             uflux(ii,jj,1,1) = uflux(ii,jj,1,2)
             vflux(ii,jj,1,1) = vflux(ii,jj,1,2)

             uflux(ii,jj,1,2) = nff*dyu(ii,jj) * dzt(ii,jj,1,2) * ( ug*DEXP(-gammag*omtime) + &
                                                   (u0-ug) * DEXP(-gamma*omtime) * COS(fcor*omtime) )
             vflux(ii,jj,1,2) = nff*dxv(ii,jj) * dzt(ii,jj,1,2) * ( -(u0-ug) * DEXP(-gamma*omtime) * SIN(fcor*omtime) )

          END DO
      END DO

      ! zero at north and south boundaries
      vflux(:,0  ,:,:) = 0.d0
      vflux(:,JMT,:,:) = 0.d0

END SUBROUTINE read_field
