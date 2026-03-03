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
    !  dxv - Length of each v-gridcell.
    !  dyu - Length of each u-gridcell.
    ! -------------------------------------------------------------

    USE mod_precdef
    USE mod_param
    USE mod_getfile
    USE mod_grid
    USE mod_seedvars

    IMPLICIT NONE

    REAL(DP), DIMENSION(imt,jmt)  :: tmp2d

    ! dx and dy in u and v points
    dxv(  1:imt,1:jmt) = get2DfieldNC(TRIM(topoDataDir)//TRIM(hgridFile), dxv_name,[imindom,jmindom+1,1,1],[imt,  jmt,1,1],'st')
    tmp2d(1:imt,1:jmt) = get2DfieldNC(TRIM(topoDataDir)//TRIM(hgridFile), dyu_name,[imindom,jmindom,1,1],[imt,jmt,1,1],'st')
    dyu(1:imt-1,1:jmt) = tmp2d(2:imt,1:jmt)
    dyu(imt,    1:jmt) = tmp2d(1,    1:jmt)

    ! Grid area
    dxdy = get2DfieldNC(TRIM(topoDataDir)//TRIM(hgridFile), 'parea',[imindom,jmindom,1,1],[imt,jmt,1,1],'st')
    mask = get2DfieldNC(TRIM(topoDataDir)//TRIM(hgridFile), 'pmask',[imindom,jmindom,1,1],[imt,jmt,1,1],'st')

END SUBROUTINE setup_grid
