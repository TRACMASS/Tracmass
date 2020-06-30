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

      ! Parameters for Nicoletta Fabboni velocities.
      ug     = 0.04
      u0     = 0.5
      fcor   = 2. * 2. * pi/(24.*3600.) * COS(45.*radian)
      gamma  = 1./(2.89*24.*3600.)
      gammag = 1./(28.9*24.*3600.)
      omtime = nff * DBLE(ints) * DBLE(ngcm*3600)

      ! Definition of fluxes
      uflux(:,:,1,1) = uflux(:,:,1,2)
      vflux(:,:,1,1) = vflux(:,:,1,2)

      uflux(1:imt,1:jmt,1,2) = nff*dyu(1:imt,1:jmt) * dzt(1:imt,1:jmt,1,2) * ( ug*DEXP(-gammag*omtime) + &
                                            (u0-ug) * DEXP(-gamma*omtime) * COS(fcor*omtime) )

      vflux(1:imt,1:jmt,1,2) = nff*dxv(1:imt,1:jmt) * dzt(1:imt,1:jmt,1,2) * ( -(u0-ug) * DEXP(-gamma*omtime) * SIN(fcor*omtime) )

      ! zero at north and south boundaries
      vflux(:,0  ,:,:) = 0.d0
      vflux(:,JMT,:,:) = 0.d0

END SUBROUTINE read_field
