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


   USE mod_precdef
   USE mod_param
   USE mod_vel
   USE mod_time
   USE mod_grid
   USE mod_getfile
   USE mod_tracervars
   USE mod_tracers
   USE mod_calendar
   USE mod_swap

   USE netcdf

   IMPLICIT none

   INTEGER        :: kk, itrac

   ! Variables for converting from S to Z
   REAL(DP), ALLOCATABLE, DIMENSION(:)    :: sc_w,Cs_w
   REAL(DP), ALLOCATABLE, DIMENSION(:,:)  :: dzt00
   INTEGER                                :: hc

   REAL(DP), ALLOCATABLE, DIMENSION(:,:,:)  :: tmp3d
   CHARACTER (len=200)                      :: fieldFile, dateprefix

   ALLOCATE ( sc_w(km), Cs_w(km) )
   ALLOCATE ( dzt00(imt,jmt))

   ! Data files
   dateprefix = ' '

   ! Reading variables to transform variables
   Cs_w = 0; sc_w = 0;

   fieldFile = TRIM(topoDataDir)//TRIM(hgridFile)

   Cs_w = get1DfieldNC(TRIM(fieldFile), 'Cs_w', 1, km)
   sc_w = get1DfieldNC(TRIM(fieldFile), 's_w', 1, km)
   hc   = INT(getScalarNC(TRIM(fieldFile), 'hc'))

   ! Reassign the time index of uflux and vflux, dzt, dzdt, hs, ...
   CALL swap_time()

   ! Reading 3-time step variables
   ! In this case: hs and zstot
   ! ===========================================================================
   IF (ints == 0) THEN

     ! 1 - Past
     IF (loopYears) THEN

       nctstep = prevMon
       IF (l_onestep) nctstep = 1

       dateprefix = filledFileName(dateFormat, prevYear, prevMon, prevDay)

       fieldFile = TRIM(physDataDir)//TRIM(physPrefixForm)//TRIM(dateprefix)//TRIM(tGridName)//TRIM(fileSuffix)
       hs(1:imt,1:jmt,-1) = get2DfieldNC(fieldFile, hs_name,[imindom,jmindom,nctstep,1],[imt,jmt,1,1,1],'st')
       hs(imt+1,:,-1)     = hs(1,:,-1)

     END IF

     ! 2 - Present
     nctstep = currMon
     IF (l_onestep) nctstep = 1

     dateprefix = filledFileName(dateFormat, currYear, currMon, currDay)

     fieldFile = TRIM(physDataDir)//TRIM(physPrefixForm)//TRIM(dateprefix)//TRIM(tGridName)//TRIM(fileSuffix)
     hs(1:imt,1:jmt,0) = get2DfieldNC(fieldFile, hs_name,[imindom,jmindom,nctstep,1],[imt,jmt,1,1],'st')
     hs(imt+1,:,0)     = hs(1,:,0)

     DO kk = 1, km
       dzt00 = (hc*sc_w(kk) + depth(:,:)*Cs_w(kk)) / (hc + depth(:,:))
       dzt(:,:,kk,1) = hs(1:imt,1:jmt,-1) + (hs(1:imt,1:jmt,-1) + depth(:,:)) * dzt00(:,:)

       dzt00 = (hc*sc_w(kk) + depth(:,:)*Cs_w(kk)) / (hc + depth(:,:))
       dzt(:,:,kk,2) = hs(1:imt,1:jmt,0) + (hs(1:imt,1:jmt,0) + depth(:,:)) * dzt00(:,:)
     END DO

     dzt(:,:,1:km-1,1) = dzt(:,:,2:km,1) - dzt(:,:,1:km-1,1)
     dzt(:,:,km,1)     = hs(1:imt,1:jmt,-1) - dzt(:,:,km,1)

     dzt(:,:,1:km-1,2) = dzt(:,:,2:km,2) - dzt(:,:,1:km-1,2)
     dzt(:,:,km,2)     = hs(1:imt,1:jmt,0) - dzt(:,:,km,2)

   END IF

   dzu(1:imt-1,:,:,1) = 0.5*(dzt(1:imt-1,:,:,1) + dzt(2:imt,:,:,1))
   dzv(:,1:jmt-1,:,1) = 0.5*(dzt(:,1:jmt-1,:,1) + dzt(:,2:jmt,:,1))

   dzu(1:imt-1,:,:,2) = 0.5*(dzt(1:imt-1,:,:,2) + dzt(2:imt,:,:,2))
   dzv(:,1:jmt-1,:,2) = 0.5*(dzt(:,1:jmt-1,:,2) + dzt(:,2:jmt,:,2))

   ! 3 - Future
   IF (ints<intrun-1 .OR. loopYears) THEN

     nctstep = nextMon
     IF (l_onestep) nctstep = 1

     dateprefix = filledFileName(dateFormat, nextYear, nextMon, nextDay)

     fieldFile = TRIM(physDataDir)//TRIM(physPrefixForm)//TRIM(dateprefix)//TRIM(tGridName)//TRIM(fileSuffix)
     hs(1:imt,1:jmt,1) = get2DfieldNC(fieldFile, hs_name,[imindom,jmindom,nctstep,1],[imt,jmt,1,1],'st')
     hs(imt+1,:,1)     = hs(1,:,1)

     DO kk = 1, km
       dzt00 = (hc*sc_w(kk) + depth(:,:)*Cs_w(kk)) / (hc + depth(:,:))
       dzt(:,:,kk,3) = hs(1:imt,1:jmt,1) + (hs(1:imt,1:jmt,1) + depth(:,:)) * dzt00(:,:)
     END DO

     dzt(:,:,1:km-1,3) = dzt(:,:,2:km,3) - dzt(:,:,1:km-1,3)
     dzt(:,:,km,3)     = hs(1:imt,1:jmt,1) - dzt(:,:,km,3)

   END IF


   ! Reading 2-time step variables
   ! In this case: velocities and tracers
   ! ===========================================================================
   ALLOCATE(tmp3d(imt,jmt,km))

   nctstep = currMon
   IF (l_onestep) nctstep = 1

   dateprefix = filledFileName(dateFormat, currYear, currMon, currDay)

   uvel(:,:,:) = 0.d0
   fieldFile = TRIM(physDataDir)//TRIM(physPrefixForm)//TRIM(dateprefix)//TRIM(uGridName)//TRIM(fileSuffix)
   uvel(1:imt-1,1:jmt,:) = get3DfieldNC(fieldFile, ueul_name,[imindom,jmindom,1,nctstep],[imt-1,jmt,km,1],'st')


   vvel(:,:,:) = 0.d0
   fieldFile = TRIM(physDataDir)//TRIM(physPrefixForm)//TRIM(dateprefix)//TRIM(vGridName)//TRIM(fileSuffix)
   vvel(1:imt,1:jmt-1,:) = get3DfieldNC(fieldFile, veul_name,[imindom,jmindom,1,nctstep],[imt,jmt-1,km,1],'st')

   WHERE (uvel > 1000) uvel = 0.d0
   WHERE (vvel > 1000) vvel = 0.d0
   WHERE (hs > 1000) hs = 0.d0

   !! Tracers
   IF (l_tracers) THEN

     DO itrac = 1, numtracers

        ! Make sure the data array is empty
        tmp3d(:,:,:) = 0.d0

        IF (tracers(itrac)%action == 'read') THEN

            ! Read the tracer from a netcdf file
            fieldFile = TRIM(physDataDir)//TRIM(physPrefixForm)//TRIM(dateprefix)//TRIM(tGridName)//TRIM(fileSuffix)
            IF (tracers(itrac)%dimension == '3D') THEN
                tmp3d(1:imt,1:jmt,:) = get3DfieldNC(fieldFile, tracers(itrac)%varname,[imindom,jmindom,1,nctstep] &
                          ,[imt,jmt,km,1],'st')
            ELSE IF (tracers(itrac)%dimension == '2D') THEN
                tmp3d(1:imt,1:jmt,1) = get2DfieldNC(fieldFile, tracers(itrac)%varname,[imindom,jmindom,nctstep,1] &
                                        ,[imt,jmt,1,1],'st')
            END IF

        ELSE IF (tracers(itrac)%action == 'compute') THEN

            ! Compute the tracer from a function defined in mod_tracer.F90
            CALL compute_tracer(tracers(itrac)%name, tmp3d(1:imt,1:jmt,1:km))

        ELSE
            PRINT '(A34,I4)', 'No action defined for this tracer:', itrac
            STOP 10
        END IF

        ! Store the information
        IF (tracers(itrac)%dimension == '3D') THEN
          tracers(itrac)%data(:,1:jmt,:,2) = tracers(itrac)%scale*tmp3d(:,:,:) + tracers(itrac)%shift
        ELSE IF (tracers(itrac)%dimension == '2D') THEN
          tracers(itrac)%data(:,1:jmt,1,2) = tracers(itrac)%scale*tmp3d(:,:,1) + tracers(itrac)%shift
        END IF

     END DO
   END IF

   ! ===========================================================================

   ! uflux and vflux computation
   FORALL (kk = 1:km) uflux(:imt-1,:,kk,2)  = uvel(:imt-1,:,kk)*dyu(:imt-1,:)*dzu(:imt-1,:,kk,2)
   FORALL (kk = 1:km) vflux(:,1:jmt-1,kk,2) = vvel(:,1:jmt-1,kk)*dxv(:,1:jmt-1)*dzv(:,1:jmt-1,kk,2)

   ! dzdt calculation
   IF (ints == 0 .AND. ( loopYears .EQV..FALSE.)) THEN
      FORALL (kk = 1:km) dzdt(:,:,kk,2) = (dzt(:,:,kk,3) - dzt(:,:,kk,2))/tseas
   ELSE IF (ints == intrun-1 .AND. ( loopYears .EQV..FALSE.)) THEN
      FORALL (kk = 1:km) dzdt(:,:,kk,2) = (dzt(:,:,kk,2) - dzt(:,:,kk,1))/tseas
   ELSE
      FORALL (kk = 1:km) dzdt(:,:,kk,2) = 0.5*(dzt(:,:,kk,3) - dzt(:,:,kk,1))/tseas
   END IF

   !! Impose zero values to fluxes
   vflux(:,0  ,:,:) = 0.d0
   vflux(:,jmt,:,:) = 0.d0
   uflux(imt,:,:,:) = 0.d0

   ! Reverse the sign of fluxes if trajectories are run backward in time.
   CALL swap_sign()


END SUBROUTINE read_field
