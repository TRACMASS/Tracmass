SUBROUTINE setup_grid
  ! =============================================================
  ! Set up the grid
  ! =============================================================
  ! Subroutine for defining the grid of the GCM. Run once
  ! before the loop starts.
  ! -------------------------------------------------------------
  ! The following arrays have to be populated:
  !
  !  dxdy - Area of horizontal cell-walls.
  !  dzt  - Height of k-cells in 4 dim
  !  kmt  - Number of k-cells from surface to seafloor.
  !
  ! The following might be needed to calculate
  ! dxdy, uflux, and vflux
  !
  !  dzu - Height of each u-gridcell.
  !  dzv - Height of each v-gridcell.
  !  dxv -
  !  dyu -
  ! -------------------------------------------------------------

  USE mod_grid

  IMPLICIT NONE

  ! Dx and Dy in [m]
  dxv(:,:) = 250.d0
  dyu(:,:) = 250.d0

  ! Vertical resolution
  dzt(:,:,:,:) = 10.d0

  ! Total area
  dxdy(:,:) = dxv(:,:)*dyu(:,:)

  ! Mask
  mask(:,:) = 1

END SUBROUTINE setup_grid
