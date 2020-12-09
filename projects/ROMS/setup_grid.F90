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

    USE mod_precdef
    USE mod_param
    USE mod_getfile
    USE mod_grid
    USE mod_seedvars

    IMPLICIT none

    REAL(DP), ALLOCATABLE, DIMENSION(:,:)      :: dx_t, dy_t

    ! Allocate and define mask
    mask(:,:) = INT(get2DfieldNC(TRIM(topoDataDir)//TRIM(bathyFile), kmt_name,[imindom,jmindom,1,1],[imt,jmt,1,1],'st'))

    ! dx and dy in T points
    dy_t = get2DfieldNC(TRIM(topoDataDir)//TRIM(hgridFile), dy_name,[imindom,jmindom,1,1],[imt,jmt,1,1],'st')
    dx_t = get2DfieldNC(TRIM(topoDataDir)//TRIM(hgridFile), dx_name,[imindom,jmindom,1,1],[imt,jmt,1,1],'st')

    WHERE(dx_t/=0.) dx_t = 1./dx_t
    WHERE(dy_t/=0.) dy_t = 1./dy_t

    ! Grid area
    dxdy(1:imt,1:jmt) = dx_t(1:imt,1:jmt) * dy_t(1:imt,1:jmt)

    ! dx in V points and dy in U points
    dyu(1:imt,1:jmt) = dy_t(1:imt,1:jmt)

    dxv(1:imt,1:jmt-1) = (dx_t(1:imt,1:jmt-1)*dy_t(1:imt,2:jmt) + dx_t(1:imt,2:jmt)*dy_t(1:imt,1:jmt-1))&
                    /(dy_t(1:imt,1:jmt-1) + dy_t(1:imt,2:jmt))

    ! Total depth
    ALLOCATE(depth(imt,jmt))
    depth(:,:) = INT(get2DfieldNC(TRIM(topoDataDir)//TRIM(zgridFile), dep_name,[imindom,jmindom,1,1],[imt,jmt,1,1],'st'))


END SUBROUTINE setup_grid
