MODULE mod_init
    !------------------------------------------------------------------------------
    !!
    !!       MODULE: mod_init
    !!
    !!          Defines and allocates variables and matrices necessary.
    !!
    !!          Subroutines included:
    !!               - init_namelist
    !!               - init_alloc
    !!
    !!               - reverse (P)
    !!
    !!------------------------------------------------------------------------------
    USE mod_vel
    USE mod_grid
    USE mod_traj
    USE mod_param
    USE mod_seedvars
    USE mod_time
    USE mod_write
    USE mod_domain
    USE mod_tracervars

    IMPLICIT NONE

    PRIVATE :: reverse

    CONTAINS

      SUBROUTINE init_namelist()
      ! --------------------------------------------------
      !
      ! Purpose:
      ! Reads the variables from the namelist
      !
      ! --------------------------------------------------

          ! Setup namelists
          namelist /INIT_GRID_DESCRIPTION/ griddir, zeroindx,l_onestep, physDataDir, physPrefixForm, &
                                           tGridName, uGridName, vGridName, &
                                           fileSuffix, hs_name, ueul_name, veul_name, &
                                           usgs_name, vsgs_name
          namelist /INIT_GRID_SIZE/        imt, jmt, km, nst, iperio, jperio, &
                                           topoDataDir, &
                                           hgridFile, dy_name, dyu_name, dx_name, dxv_name, &
                                           zgridFile, dzt_name, dzu_name, dzv_name,&
                                           bathyFile, kmt_name
          namelist /INIT_GRID_SUBDOMAIN/   l_subdom, imindom, imaxdom, jmindom, jmaxdom
          namelist /INIT_GRID_TIME/        ngcm_step, ngcm_unit, iter
          namelist /INIT_START_DATE/       startSec, startMin, startHour,           &
                                           startDay, startMon, startYear,           &
                                           noleap
          namelist /INIT_RUN_TIME/         loopYears, loopStartYear, loopEndYear, &
                                           log_level, intrun
          namelist /INIT_WRITE_TRAJ/       write_frec, outDataDir, outDataFile, timeformat
          namelist /INIT_SEEDING/          nff, isec, idir, nqua, partQuant,             &
                                           loneparticle, SeedType, ist1,  &
                                           ist2, jst1, jst2, kst1, kst2, tst1, tst2,&
                                           seedDir, seedFile, seedTime, timeFile
          namelist /INIT_TRACERS/          l_tracers, &
                                           tracername, tracerunit, tracervarname,&
                                           traceraction,tracermin, tracermax, &
                                           tracerdimension
          namelist /INIT_TRACERS_SEEDING/  tracer0min, tracer0max
          namelist /INIT_KILLZONES/        timax, exitType, ienw, iene, jens, jenn, &
                                           tracerchoice, tracere, maxormin
          namelist /INIT_STREAMFUNCTION/   dirpsi

          ! Read namelist
          OPEN (8,file='namelist.in',    &
               & status='OLD', delim='APOSTROPHE')
          READ (8,nml=INIT_GRID_DESCRIPTION)
          READ (8,nml=INIT_GRID_SIZE)
          READ (8,nml=INIT_GRID_SUBDOMAIN)
          READ (8,nml=INIT_GRID_TIME)
          READ (8,nml=INIT_START_DATE)
          READ (8,nml=INIT_RUN_TIME)
          READ (8,nml=INIT_WRITE_TRAJ)
          READ (8,nml=INIT_SEEDING)
          READ (8,nml=INIT_TRACERS)
          READ (8,nml=INIT_TRACERS_SEEDING)
          READ (8,nml=INIT_KILLZONES)
          READ (8,nml=INIT_STREAMFUNCTION)
          CLOSE(8)

          ! Reverse killing zones
          CALL reverse()

          ! Shift in case of netcdf data starting at zero
          IF (zeroindx) THEN
            ienw = ienw + 1
            iene = iene + 1
          END IF

      END SUBROUTINE init_namelist

      SUBROUTINE init_alloc()
      ! --------------------------------------------------
      !
      ! Purpose:
      ! Allocates all the arrays in TRACMASS
      !
      ! --------------------------------------------------

          ! Allocate information about the coordinates and grid
          ALLOCATE( dxv(imt,jmt), dyu(imt,jmt))
          dxv(:,:)  = 0
          dyu(:,:)  = 0

          ALLOCATE( dxdy(imt,jmt))

          ALLOCATE( mask(imt,jmt))
          mask(:,:) = 1.

          ALLOCATE ( kmt(imt,jmt))
          kmt(:,:) = km

          ALLOCATE( dzt(imt,jmt,km,nst+1), dzu(imt,jmt,km,nst), dzv(imt,jmt,km,nst))
          dzt(:,:,:,:) = 0.; dzu(:,:,:,:) = 0.; dzv(:,:,:,:) = 0.

          ALLOCATE( dzdt(imt,jmt,km,nst))
          dzdt(:,:,:,:) = 0.

          ALLOCATE( zstot(imt,jmt,-1:1), zstou(imt,jmt), zstov(imt,jmt))
          zstot(:,:,:) = 1.; zstou(:,:) = 1.; zstov(:,:) = 1.

          ! Allocate surface parameter (SSH ocean, surface pressure atmosphere)
          ALLOCATE ( hs(imt+1,jmt+1,-1:1))
          hs(:,:,:) = 0.

          ! Allocate velocity fields
          ALLOCATE ( uflux(imt,jmt,km,nst), vflux(imt,0:jmt,km,nst) )
          uflux(:,:,:,:) = 0.
          vflux(:,:,:,:) = 0.

          ALLOCATE ( uvel(imt,jmt,km), vvel(imt,0:jmt,km) )
          uvel(:,:,:) = 0.
          vvel(:,:,:) = 0.

#if defined w_explicit
          ALLOCATE ( wflux(imt ,jmt ,0:km, nst) )
          wflux(:,:,:,:) = 0.
#else
          ALLOCATE ( wflux(0:km, nst))
          wflux(:,:) = 0.
#endif

          IF (l_psi) THEN

              ALLOCATE( fluxes_xy(imt, jmt, 10), psi_xy(imt, jmt))
              fluxes_xy(:,:,:) = 0.
              psi_xy(:,:) = 0.

              ALLOCATE( fluxes_yz(jmt, km, 10), psi_yz(jmt, km))
              fluxes_yz(:,:,:) = 0.
              psi_yz(:,:) = 0.

              IF (l_tracers) THEN
                ALLOCATE( fluxes_yr(jmt, resolution, 10, numtracers), psi_yr(jmt, resolution, numtracers))
                fluxes_yr(:,:,:,:) = 0.
                psi_yr(:,:,:) = 0.

                ALLOCATE( fluxes_rr(resolution, resolution, 10, 2), psi_rr(resolution, resolution, 2))
                fluxes_rr(:,:,:,:) = 0.
                psi_rr(:,:,:)      = 0.

              END IF

          END IF

      END SUBROUTINE init_alloc

      SUBROUTINE reverse()
      ! --------------------------------------------------
      !
      ! Purpose:
      ! Reverse seeding indexes according to the project type
      ! --------------------------------------------------

        INTEGER :: ii

        IF (griddir(2) == -1) THEN
              DO ii = 1, 10
                IF (isec == 2) THEN
                  jenn(ii) = jmt - jenn(ii)    ! Meridional reverse
                  jens(ii) = jmt - jens(ii)    ! Meridional reverse
                ELSE
                  jenn(ii) = jmt - jenn(ii) + 1    ! Meridional reverse
                  jens(ii) = jmt - jens(ii) + 1   ! Meridional reverse
                END IF
              END DO
        END IF

      END SUBROUTINE

END MODULE mod_init

!!----------------------------------------------------------------------------
!!----------------------------------------------------------------------------
