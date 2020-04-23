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

   USE mod_calendar

   USE netcdf

   IMPLICIT none

   INTEGER        :: kk, k

   REAL(DP), ALLOCATABLE, DIMENSION(:,:,:)  :: tmp3d
   CHARACTER (len=200)                      :: fieldFile, dataprefix

   ! Data swap
   uflux(:,:,:,1) = uflux(:,:,:,2)
   vflux(:,:,:,1) = vflux(:,:,:,2)

   hs(:,:,-1)     = hs(:,:,0)
   hs(:,:,0)      = hs(:,:,1)

   zstot(:,:,-1) = zstot(:,:,0)
   zstot(:,:, 0) = zstot(:,:,1)

   dzdt(:,:,:,1)  = dzdt(:,:,:,2)

   ! Data files
   dataprefix = 'YYYY'

   IF (ints == 0) THEN

     ! 1 - Past
     IF (loopYears) THEN
       WRITE(dataprefix(1:4),'(i4.4)')   endYear
       fieldFile = TRIM(physDataDir)//TRIM(physPrefixForm)//TRIM(dataprefix)//TRIM(tGridName)//TRIM(fileSuffix)
       hs(1:imt,1:jmt,-1) = get2DfieldNC(fieldFile, hs_name,[1,1,endMon,1],[imt,jmt,1,1,1])
       hs(imt+1,:,-1)     = hs(1,:,-1)
     END IF

     ! 2 - Present
     WRITE(dataprefix(1:4),'(i4.4)')   dateYear
     fieldFile = TRIM(physDataDir)//TRIM(physPrefixForm)//TRIM(dataprefix)//TRIM(tGridName)//TRIM(fileSuffix)
     hs(1:imt,1:jmt,0) = get2DfieldNC(fieldFile, hs_name,[1,1,dateMon,1],[imt,jmt,1,1])
     hs(imt+1,:,0)     = hs(1,:,0)

     WHERE (SUM(dzt(:,:,:,2),3) /= 0)
          zstot(1:imt,1:jmt,-1) = hs(:imt,:jmt,-1)/SUM(dzt(:,:,:,2),3) + 1
          zstot(1:imt,1:jmt,0) = hs(:imt,:jmt,0)/SUM(dzt(:,:,:,2),3) + 1
     ELSEWHERE
          zstot(:,:,-1) = 0.d0
          zstot(:,:,0) = 0.d0
     END WHERE

   END IF

   ! 3 - Future
   IF (ints<intrun-1 .OR. loopYears) THEN
     tempYear = dateYear; tempMon = dateMon;

     CALL tt_calendar(nff*(ints+1)*tseas)

     WRITE(dataprefix(1:4),'(i4.4)')   dateYear
     fieldFile = TRIM(physDataDir)//TRIM(physPrefixForm)//TRIM(dataprefix)//TRIM(tGridName)//TRIM(fileSuffix)
     hs(1:imt,1:jmt,1) = get2DfieldNC(fieldFile, hs_name,[1,1,dateMon,1],[imt,jmt,1,1])
     hs(imt+1,:,1)     = hs(1,:,1)

     dateYear = tempYear; dateMon = tempMon;

   END IF

   hs(imt+1,:,:)     = hs(1,:,:)

   ! Calculate SSH/depth
   WHERE (SUM(dzt(:,:,:,2),3) /= 0)
        zstot(1:imt,1:jmt,1)  = hs(:imt,:jmt,1)/SUM(dzt(:,:,:,2),3) + 1
   ELSEWHERE
        zstot(:,:,1) = 0.d0
   END WHERE

   WHERE (SUM(dzu(:,:,:,2),3) /= 0)
        zstou(1:imt,1:jmt) = 0.5*(hs(:imt,:jmt,0)+hs(2:imt+1,:jmt,0))/SUM(dzu(:,:,:,2),3) + 1
   ELSEWHERE
        zstou = 0.d0
   END WHERE

   WHERE (SUM(dzv(:,:,:,2),3) /= 0)
        zstov(1:imt,1:jmt) = 0.5*(hs(:imt,:jmt,0)+hs(:imt,2:jmt+1,0))/SUM(dzv(:,:,:,2),3) + 1
   ELSEWHERE
        zstov = 0.d0
   END WHERE

   ! Velocity files
   ALLOCATE(tmp3d(imt,jmt,km))
   WRITE(dataprefix(1:4),'(i4.4)')   dateYear
   fieldFile = TRIM(physDataDir)//TRIM(physPrefixForm)//TRIM(dataprefix)//TRIM(uGridName)//TRIM(fileSuffix)
   tmp3d(1:imt,1:jmt,1:km) = get3DfieldNC(fieldFile, ueul_name,[1,1,1,dateMon],[imt,jmt,km,1],'st')
   uvel(1:imt,1:jmt,1:km) = tmp3d(1:imt,1:jmt,1:km)
   tmp3d(1:imt,1:jmt,1:km)  = get3DfieldNC(fieldFile, usgs_name,[1,1,1,dateMon],[imt,jmt,km,1],'st')
   uvel(1:imt,1:jmt,1:km) = uvel(1:imt,1:jmt,1:km) + tmp3d(1:imt,1:jmt,1:km)

   fieldFile = TRIM(physDataDir)//TRIM(physPrefixForm)//TRIM(dataprefix)//TRIM(vGridName)//TRIM(fileSuffix)
   tmp3d(1:imt,1:jmt,1:km) = get3DfieldNC(fieldFile, veul_name,[1,1,1,dateMon],[imt,jmt,km,1],'st')
   vvel(1:imt,1:jmt,1:km) = tmp3d(1:imt,1:jmt,1:km)
   tmp3d(1:imt,1:jmt,1:km) = get3DfieldNC(fieldFile, vsgs_name,[1,1,1,dateMon],[imt,jmt,km,1],'st')
   vvel(1:imt,1:jmt,1:km) = vvel(1:imt,1:jmt,1:km) + tmp3d(1:imt,1:jmt,1:km)

   ! uflux and vflux computation
   DO kk = 1, km
     k = km - kk + 1

     ! uflux and vflux computation
     dzu(:,:,kk,2)       = dzu(:,:,kk,2)*zstou(:,:)
     dzv(:,1:jmt,kk,2)   = dzv(:,1:jmt,kk,2)*zstov(:,:)

     uflux(:,:,kk,2)     = uvel(:,:,k)*dyu(:,:)*dzu(:,:,kk,2)
     vflux(:,1:jmt,kk,2) = vvel(:,1:jmt,k)*dxv(:,1:jmt)*dzv(:,1:jmt,kk,2)

     ! dzdt calculation
     IF (ints == 0 .AND. ( loopYears .EQV..FALSE.)) THEN
        dzdt(:,:,kk,2)   = dzt(:,:,kk,2)*(zstot(:,:,1) - zstot(:,:,0))/tseas
     ELSE IF (ints == intrun-1 .AND. ( loopYears .EQV..FALSE.)) THEN
        dzdt(:,:,kk,2)   = dzt(:,:,kk,2)*(zstot(:,:,0) - zstot(:,:,-1))/tseas
     ELSE
        dzdt(:,:,kk,2)   = 0.5*dzt(:,:,kk,2)*(zstot(:,:,1) - zstot(:,:,-1))/tseas
     END IF

   END DO

   uflux(:,:,:,2) = nff*uflux(:,:,:,2)
   vflux(:,:,:,2) = nff*vflux(:,:,:,2)
   dzdt(:,:,:,2) = nff*dzdt(:,:,:,2)

   !! Zero meridional flux at j=0 and j=jmt
   vflux(:,0  ,:,:) = 0.d0
   vflux(:,jmt,:,:) = 0.d0


END SUBROUTINE read_field
