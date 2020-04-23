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

   INTEGER        :: ii, jj, ip, jp, jm
   INTEGER        :: nctpos

   REAL(DP), ALLOCATABLE, DIMENSION(:,:,:)  :: tmp3d, tmp3du
   REAL(DP), ALLOCATABLE, DIMENSION(:,:)    :: tmp2d
   CHARACTER (len=200)                      :: fieldFile, dataprefix

   ! time position
   nctpos = 1 + (24*(dateDay-1) + dateHour)/ngcm_step

   ! Data swap
   uflux(:,:,:,1) = uflux(:,:,:,2)
   vflux(:,:,:,1) = vflux(:,:,:,2)

   dzu(:,:,:,1) = dzu(:,:,:,2)
   dzv(:,:,:,1) = dzv(:,:,:,2)

   hs(:,:,-1) = hs(:,:,0)
   hs(:,:,0)  = hs(:,:,1)

   dzdt(:,:,:,1)  = dzdt(:,:,:,2)

   ! Data files
   dataprefix = 'YYYYMM'

   ALLOCATE(tmp3d(imt,jmt+1,km),tmp3du(imt,0:jmt,km), tmp2d(imt,0:jmt))

   ! surface pressure
   IF (ints == 0) THEN

     ! 1 - Past
     IF (loopYears) THEN
       WRITE(dataprefix(1:4),'(i4.4)')   endYear
       WRITE(dataprefix(5:6),'(i2.2)')   12

       fieldFile = TRIM(physDataDir)//TRIM(physPrefixForm)//TRIM(dataprefix)//TRIM(tGridName)//TRIM(fileSuffix)
       hs(1:imt,jmt+1:1:-1,-1) = EXP(get2DfieldNC(fieldFile, hs_name,[1,1,1,nctpos],[imt,jmt+1,1,1],'extend'))

     END IF


     WRITE(dataprefix(1:4),'(i4.4)')   dateYear
     WRITE(dataprefix(5:6),'(i2.2)')   dateMon

     fieldFile = TRIM(physDataDir)//TRIM(physPrefixForm)//TRIM(dataprefix)//TRIM(tGridName)//TRIM(fileSuffix)
     hs(1:imt,jmt+1:1:-1,0) = EXP(get2DfieldNC(fieldFile, hs_name,[1,1,1,nctpos],[imt,jmt+1,1,1],'extend'))

   END IF

   ! 3 - Future
   IF (ints<intrun-1 .OR. loopYears) THEN
     tempYear = dateYear; tempMon = dateMon;
     tempDay  = dateDay;  tempHour = dateHour;

     CALL tt_calendar(nff*(ints+1)*tseas)

     WRITE(dataprefix(1:4),'(i4.4)')   dateYear
     WRITE(dataprefix(5:6),'(i2.2)')   dateMon

     fieldFile = TRIM(physDataDir)//TRIM(physPrefixForm)//TRIM(dataprefix)//TRIM(tGridName)//TRIM(fileSuffix)
     hs(1:imt,jmt+1:1:-1,1) = EXP(get2DfieldNC(fieldFile, hs_name,[1,1,1,nctpos],[imt,jmt+1,1,1],'extend'))

     dateYear = tempYear; dateMon = tempMon;
     dateDay  = tempDay; dateHour = tempHour;

   END IF

   WRITE(dataprefix(1:4),'(i4.4)')   dateYear
   WRITE(dataprefix(5:6),'(i2.2)')   dateMon

   ! Velocity data
   fieldFile = TRIM(physDataDir)//TRIM(physPrefixForm)//TRIM(dataprefix)//TRIM(uGridName)//TRIM(fileSuffix)
   tmp3du(1:imt,jmt:0:-1,1:km) = get3DfieldNC(fieldFile, ueul_name,[1,1,1,nctpos],[imt,jmt+1,km,1],'st','extend')

   fieldFile = TRIM(physDataDir)//TRIM(physPrefixForm)//TRIM(dataprefix)//TRIM(vGridName)//TRIM(fileSuffix)
   vvel(1:imt,jmt:0:-1,1:km) = get3DfieldNC(fieldFile, veul_name,[1,1,1,dateMon],[imt,jmt+1,km,1],'st','extend')

   ! uflux and vflux computation
   DO ii = 1, imt
      DO jj = 1, jmt

        jp = jj + 1; jm = jj - 1
        ip = ii + 1
        IF (ip == imt+1) ip = 1

        ! dzu and dzv calculation
        dzu(ii,jj,1:km,2) =  (aa(1:km)-aa(0:km-1) + (bb(1:km)-bb(0:km-1))*&
                                0.5*(hs(ip,jp,0)+hs(ip,jj,0))) / grav

        dzv(ii,jj,1:km,2) =  (aa(1:km)-aa(0:km-1) + (bb(1:km)-bb(0:km-1))*&
                                0.5*(hs(ii,jp,0)+hs(ip,jp,0)))/ grav

        ! ufluxes
        uflux(ii,jj,:,2) = 0.5 * (tmp3du(ip,jj,:) + tmp3du(ip,jm,:)) * dyu(ii,jj) * dzu(ii,jj,:,2)

        ! vfluxes
        vflux(ii,jj,:,2) = 0.5 * (vvel(ii,jj,:) + vvel(ip,jj,:)) * dxv(ii,jj) * dzv(ii,jj,:,2)


        ! dzt and dz/dt calculation
        dzt(ii,jj,1:km,1) =  (aa(1:km)-aa(0:km-1) + (bb(1:km)-bb(0:km-1))*&
                    0.25*(hs(ii,jp,-1)+hs(ii,jj,-1)+hs(ip,jp,-1)+hs(ip,jj,-1))) / grav

        dzt(ii,jj,1:km,2) =  (aa(1:km)-aa(0:km-1) + (bb(1:km)-bb(0:km-1))*&
                    0.25*(hs(ii,jp,0)+hs(ii,jj,0)+hs(ip,jp,0)+hs(ip,jj,0))) / grav

        dzt(ii,jj,1:km,3) =  (aa(1:km)-aa(0:km-1) + (bb(1:km)-bb(0:km-1))*&
                    0.25*(hs(ii,jp,1)+hs(ii,jj,1)+hs(ip,jp,1)+hs(ip,jj,1))) / grav

        ! dzdt calculation
        IF (ints == 0 .AND. ( loopYears .EQV..FALSE.)) THEN
           dzdt(ii,jj,:,2)  = (dzt(ii,jj,:,3) - dzt(ii,jj,:,2))/tseas
        ELSE IF (ints == intrun-1 .AND. ( loopYears .EQV..FALSE.)) THEN
           dzdt(ii,jj,:,2)   = (dzt(ii,jj,:,2) - dzt(ii,jj,:,1))/tseas
        ELSE
           dzdt(ii,jj,:,2)   = 0.5*(dzt(ii,jj,:,3) - dzt(ii,jj,:,1))/tseas
        END IF

      END DO
   END DO

   uflux(:,:,:,2) = nff*uflux(:,:,:,2)
   vflux(:,:,:,2) = nff*vflux(:,:,:,2)
   dzdt(:,:,:,2)  = nff*dzdt(:,:,:,2)

   !! Zero meridional flux at j=0 and j=jmt
   vflux(:,0  ,:,:) = 0.d0
   vflux(:,jmt,:,:) = 0.d0


END SUBROUTINE read_field
