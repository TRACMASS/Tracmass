MODULE mod_init
    !------------------------------------------------------------------------------
    !!
    !!       MODULE: mod_init
    !!
    !!          Defines and allocates variables and matrices necessary.
    !!
    !!
    !!------------------------------------------------------------------------------
    USE mod_vel
    USE mod_grid
    USE mod_traj
    USE mod_param
    USE mod_seed
    USE mod_time
    USE mod_write
    USE mod_domain

    IMPLICIT NONE

    CONTAINS

    SUBROUTINE init_namelist()
    ! --------------------------------------------------
    !
    ! Purpose:
    ! Reads the variables from the namelist
    !
    ! --------------------------------------------------

        ! Setup namelists
        namelist /INIT_GRID_DESCRIPTION/ physDataDir, physPrefixForm, &
                                         tGridName, uGridName, vGridName, &
                                         fileSuffix, ssh_name, ueul_name, veul_name
        namelist /INIT_GRID_SIZE/        imt, jmt, km, nst, iperio, jperio
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
        namelist /INIT_KILLZONES/        timax, exitType, ienw, iene, jens, jenn

        ! Read namelist
        OPEN (8,file='namelist.in',    &
             & status='OLD', delim='APOSTROPHE')
        READ (8,nml=INIT_GRID_DESCRIPTION)
        READ (8,nml=INIT_GRID_SIZE)
        READ (8,nml=INIT_GRID_TIME)
        READ (8,nml=INIT_START_DATE)
        READ (8,nml=INIT_RUN_TIME)
        READ (8,nml=INIT_WRITE_TRAJ)
        READ (8,nml=INIT_SEEDING)
        READ (8,nml=INIT_KILLZONES)
        CLOSE(8)

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
        mask(:,:) = 1

        ALLOCATE ( kmt(imt,jmt))
        kmt(:,:) = 1

        ALLOCATE( dzt(imt,jmt,km,nst))

        ! Allocate velocity fields and sea-surface height
        ALLOCATE (hs(imt,jmt,nst) )
        hs(:,:,:) = 0.

        ALLOCATE ( uflux(imt,jmt,km,nst), vflux(imt,0:jmt,km,nst) )
        uflux(:,:,:,:) = 0.
        vflux(:,:,:,:) = 0.

        ALLOCATE ( uvel(imt,jmt,km), vvel(imt,0:jmt,km) )
        uvel(:,:,:) = 0.
        vvel(:,:,:) = 0.

#if defined w_3dim || full_wflux
        ALLOCATE ( wflux(imt ,jmt ,0:km, nst) )
        wflux(:,:,:,:) = 0.
#else
        ALLOCATE ( wflux(0:km, nst))
        wflux(:,:) = 0.
#endif

    END SUBROUTINE init_alloc

END MODULE mod_init

!!----------------------------------------------------------------------------
!!----------------------------------------------------------------------------
