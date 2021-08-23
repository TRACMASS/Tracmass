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

    INTEGER               :: jj, ii, ip, jp, kk

    INTEGER, ALLOCATABLE, DIMENSION(:,:)       :: tmp2d
    REAL(DP), ALLOCATABLE, DIMENSION(:,:,:)    :: tmp3d
    REAL(DP), ALLOCATABLE, DIMENSION(:,:)      :: dx_t, dy_t

    ! Allocate and define kmu and kmv, and kmt
    ALLOCATE ( kmu(imt,jmt), kmv(imt,jmt), tmp2d(imt,jmt) )

    kmt(:,:) = INT(get2DfieldNC(TRIM(topoDataDir)//TRIM(bathyFile), kmt_name,[imindom,jmindom,1,1],[imt,jmt,1,1],'st'))

    WHERE (kmt>0)
      tmp2d(:,:) = 1
    END WHERE
    IF (isec == 3) mask(:,:) = tmp2d(:,:)

    kmu(:,:)=0 ; kmv(:,:)=0
    DO jj=1,jmt
       jp=jj+1
       IF(jp == jmt+1) jp = jmt
       DO ii=1,imt
          ip = ii+1
          IF(ip == imt+1) ip = 1
          kmu(ii,jj) = MIN(kmt(ii,jj), kmt(ip,jj), km)
          kmv(ii,jj) = MIN(kmt(ii,jj), kmt(ii,jp), km)

          ! mask definition
          IF (isec == 1) THEN
              mask(ii,jj) = tmp2d(ii,jj)*tmp2d(ip,jj)
          ELSE IF (isec == 2) THEN
              mask(ii,jj) = tmp2d(ii,jj)*tmp2d(ii,jp)
          END IF

       END DO
    END DO

    ! dx and dy in T points
    dy_t  = get2DfieldNC(TRIM(topoDataDir)//TRIM(hgridFile), dy_name,[imindom,jmindom,1,1],[imt,jmt,1,1],'st')
    dx_t  = get2DfieldNC(TRIM(topoDataDir)//TRIM(hgridFile), dx_name,[imindom,jmindom,1,1],[imt,jmt,1,1],'st')

    ! dx and dy in u and v points
    dyu = get2DfieldNC(TRIM(topoDataDir)//TRIM(hgridFile), dyu_name,[imindom,jmindom,1,1],[imt,jmt,1,1],'st')
    dxv = get2DfieldNC(TRIM(topoDataDir)//TRIM(hgridFile), dxv_name,[imindom,jmindom,1,1],[imt,jmt,1,1],'st')

    ! Grid area
    dxdy(1:imt,1:jmt) = dx_t(1:imt,1:jmt) * dy_t(1:imt,1:jmt)

    ! Sensitivity to dz value
    ALLOCATE(tmp3d(imt,jmt,km))
    tmp3d(:,:,:) = get3DfieldNC(TRIM(topoDataDir)//TRIM(zgridFile), dzt_name,[imindom,jmindom,1,1],[imt,jmt,km,1],'st')
    DO kk = 1, km
      dzt(:,:,km-kk+1,1) = tmp3d(:,:,kk); dzt(:,:,km-kk+1,2) = tmp3d(:,:,kk); dzt(:,:,km-kk+1,3) = tmp3d(:,:,kk)
      WHERE (kk > kmt(1:imt,1:jmt))
        dzt(:,:,km-kk+1,1) = 0.; dzt(:,:,km-kk+1,2) = 0.; dzt(:,:,km-kk+1,3) = 0.
      END WHERE
    END DO

    tmp3d(:,:,:) = get3DfieldNC(TRIM(topoDataDir)//TRIM(zgridFile), dzu_name,[imindom,jmindom,1,1],[imt,jmt,km,1],'st')
    DO kk = 1, km
      dzu(:,:,km-kk+1,1) = tmp3d(:,:,kk); dzu(:,:,km-kk+1,2) = tmp3d(:,:,kk);
      WHERE (kk > kmu(1:imt,1:jmt))
          dzu(:,:,km-kk+1,1) = 0.; dzu(:,:,km-kk+1,2) = 0.;
      END WHERE
    END DO

    tmp3d(:,:,:) = get3DfieldNC(TRIM(topoDataDir)//TRIM(zgridFile), dzv_name,[imindom,jmindom,1,1],[imt,jmt,km,1],'st')
    DO kk = 1, km
      dzv(:,:,km-kk+1,1) = tmp3d(:,:,kk); dzv(:,:,km-kk+1,2) = tmp3d(:,:,kk);
      WHERE (kk > kmv(1:imt,1:jmt))
          dzv(:,:,km-kk+1,1) = 0.; dzv(:,:,km-kk+1,2) = 0.;
      END WHERE
    END DO

END SUBROUTINE setup_grid
