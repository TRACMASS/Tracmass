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

   REAL(DP), ALLOCATABLE, DIMENSION(:,:,:)  :: tmp3d, tmpuflux, tmpvflux, tmptracer
   REAL(DP), ALLOCATABLE, DIMENSION(:,:)    :: tmp2d
   REAL(DP), ALLOCATABLE, DIMENSION(:)      :: da, db
   CHARACTER (len=200)                      :: fieldFile, dateprefix

   ! Reassign the time index of uflux and vflux, dzt, dzdt, hs, ...
   CALL swap_time()

   ! Data files
   dateprefix = ' '

   ALLOCATE(tmptracer(imt,jmt,km),tmpuflux(imt,0:jmt,km), tmpvflux(imt,0:jmt,km))
   ALLOCATE(tmp3d(imt,0:jmt,km), tmp2d(imt,0:jmt), da(km), db(km))

   ! surface pressure
   IF (ints == 0) THEN

     ! 1 - Past
     IF (loopYears) THEN

       ! time position
       nctstep = 1 + (24*(prevDay-1) + prevHour)/ngcm_step

       dateprefix = filledFileName(dateFormat, prevYear, prevMon, prevDay)

       fieldFile = TRIM(physDataDir)//TRIM(physPrefixForm)//TRIM(dateprefix)//TRIM(tGridName)//TRIM(fileSuffix)
       hs(1:imt,jmt:0:-1,-1) = EXP(get2DfieldNC(fieldFile, hs_name,[imindom,jmindom,1,nctstep],[imt,jmt+1,1,1],'st'))

       IF (l_tracers .AND. l_swtraj) THEN

           tmp3d(:,:,:) = 0.d0

           ! Read the tracer from a netcdf file
           fieldFile = TRIM(physDataDir)//TRIM(physPrefixForm)//TRIM(dateprefix)//TRIM(tGridName)//TRIM(fileSuffix)
           tmp3d(1:imt,jmt:0:-1,1:km) = get3DfieldNC(fieldFile, tracers(1)%varname,[imindom,jmindom,1,nctstep] &
                         ,[imt,jmt,km,1],'st')

           tracertraj(:,:,:,-1) = tracertrajscale*tmp3d(:,:,:)

        END IF

     END IF

     ! 2 - Present
     ! time position
     nctstep = 1 + (24*(currDay-1) + currHour)/ngcm_step

     dateprefix = filledFileName(dateFormat, currYear, currMon, currDay)

     fieldFile = TRIM(physDataDir)//TRIM(physPrefixForm)//TRIM(dateprefix)//TRIM(tGridName)//TRIM(fileSuffix)
     hs(1:imt,jmt:0:-1,0) = EXP(get2DfieldNC(fieldFile, hs_name,[imindom,jmindom,1,nctstep],[imt,jmt+1,1,1],'st'))

     IF (l_tracers .AND. l_swtraj) THEN

         tmp3d(:,:,:) = 0.d0

         ! Read the tracer from a netcdf file
         fieldFile = TRIM(physDataDir)//TRIM(physPrefixForm)//TRIM(dateprefix)//TRIM(tGridName)//TRIM(fileSuffix)
         tmp3d(1:imt,jmt:0:-1,1:km) = get3DfieldNC(fieldFile, tracers(1)%varname,[imindom,jmindom,1,nctstep] &
                       ,[imt,jmt,km,1],'st')

         tracertraj(:,:,:,0) = tracertrajscale*tmp3d(:,:,:)

      END IF

   END IF

   ! 3 - Future
   IF (ints<intrun-1 .OR. loopYears) THEN

     ! time position
     nctstep = 1 + (24*(nextDay-1) + nextHour)/ngcm_step

     dateprefix = filledFileName(dateFormat, nextYear, nextMon, nextDay)

     fieldFile = TRIM(physDataDir)//TRIM(physPrefixForm)//TRIM(dateprefix)//TRIM(tGridName)//TRIM(fileSuffix)
     hs(1:imt,jmt:0:-1,1) = EXP(get2DfieldNC(fieldFile, hs_name,[imindom,jmindom,1,nctstep],[imt,jmt+1,1,1],'st'))

     IF (l_tracers .AND. l_swtraj) THEN

         tmp3d(:,:,:) = 0.d0

         ! Read the tracer from a netcdf file
         fieldFile = TRIM(physDataDir)//TRIM(physPrefixForm)//TRIM(dateprefix)//TRIM(tGridName)//TRIM(fileSuffix)
         tmp3d(1:imt,jmt:0:-1,1:km) = get3DfieldNC(fieldFile, tracers(1)%varname,[imindom,jmindom,1,nctstep] &
                       ,[imt,jmt,km,1],'st')

          tracertraj(:,:,:,1) = tracertrajscale*tmp3d(:,:,:)

      END IF

   END IF

   ! Velocity data
   nctstep = 1 + (24*(currDay-1) + currHour)/ngcm_step

   dateprefix = filledFileName(dateFormat, currYear, currMon, currDay)

   fieldFile = TRIM(physDataDir)//TRIM(physPrefixForm)//TRIM(dateprefix)//TRIM(uGridName)//TRIM(fileSuffix)
   tmpuflux(1:imt,jmt:0:-1,1:km) = get3DfieldNC(fieldFile, ueul_name,[imindom,jmindom,1,nctstep],[imt,jmt+1,km,1],'st')

   fieldFile = TRIM(physDataDir)//TRIM(physPrefixForm)//TRIM(dateprefix)//TRIM(vGridName)//TRIM(fileSuffix)
   tmpvflux(1:imt,jmt:0:-1,1:km) = get3DfieldNC(fieldFile, veul_name,[imindom,jmindom,1,nctstep],[imt,jmt+1,km,1],'st')


   !! Tracers
   IF (l_tracers) THEN

     DO itrac = 1, numtracers

        ! Make sure the data array is empty
        tmp3d(:,:,:) = 0.d0

        ! Reassign temporal indexes
        tracers(itrac)%data(:,:,:,1) = tracers(itrac)%data(:,:,:,2)

        IF (tracers(itrac)%action == 'read') THEN

            ! Read the tracer from a netcdf file
            fieldFile = TRIM(physDataDir)//TRIM(physPrefixForm)//TRIM(dateprefix)//TRIM(tGridName)//TRIM(fileSuffix)
            IF (tracers(itrac)%dimension == '3D') THEN
                tmp3d(1:imt,jmt:0:-1,1:km) = get3DfieldNC(fieldFile, tracers(itrac)%varname,[imindom,jmindom,1,nctstep] &
                          ,[imt,jmt,km,1],'st')
            ELSE IF (tracers(itrac)%dimension == '2D') THEN
                tmp3d(1:imt,jmt:0:-1,1) = get2DfieldNC(fieldFile, tracers(itrac)%varname,[imindom,jmindom,nctstep,1] &
                                        ,[imt,jmt,1,1],'st')
            END IF

            ! Place tracer value from A to C grid
            tmptracer(1:imt-1,1:jmt,1:km) = 0.25*( tmp3d(1:imt-1,0:jmt-1,1:km) + tmp3d(1:imt-1,1:jmt,1:km) + &
                    tmp3d(2:imt,0:jmt-1,1:km) + tmp3d(2:imt,1:jmt,1:km))
            tmptracer(imt,1:jmt,1:km)     = 0.25*( tmp3d(imt,0:jmt-1,1:km) + tmp3d(imt,1:jmt,1:km) + &
                    tmp3d(1,0:jmt-1,1:km) + tmp3d(1,1:jmt,1:km))


        ELSE IF (tracers(itrac)%action == 'compute') THEN

            ! Compute the tracer from a function defined in mod_tracer.F90
            CALL compute_tracer(tracers(itrac)%name, tmptracer(1:imt,1:jmt,1:km))

        ELSE
            PRINT '(A34,I4)', 'No action defined for this tracer:', itrac
            STOP 10
        END IF

        ! Store the information
        IF (tracers(itrac)%dimension == '3D') THEN
          tracers(itrac)%data(:,:,:,2) = tracers(itrac)%scale*tmptracer(:,:,:) + tracers(itrac)%shift
        ELSE IF (tracers(itrac)%dimension == '2D') THEN
          tracers(itrac)%data(:,:,1,2) = tracers(itrac)%scale*tmptracer(:,:,1) + tracers(itrac)%shift
        END IF


     END DO
   END IF

   ! da/db values
   da(1:km) = aa(1:km)-aa(0:km-1)
   db(1:km) = bb(1:km)-bb(0:km-1)

   ! u/v fluxes on A points
   tmpvflux(:,0,:) = 0.d0

   DO kk = 1, km

     ! j = 0, south pole
     tmpuflux(1:imt,0,kk) = tracertraj(1:imt,0,kk,0)*tmpuflux(1:imt,0,kk)*&
                            (da(kk) + db(kk)*hs(1:imt,0,0))/(grav*tracertrajscale)

     ! Other points
     tmpuflux(1:imt,1:jmt,kk) = tracertraj(1:imt,1:jmt,kk,0)*tmpuflux(1:imt,1:jmt,kk)*&
                            (da(kk) + db(kk)*hs(1:imt,1:jmt,0))/(grav*tracertrajscale)

     tmpvflux(1:imt,1:jmt,kk) = tracertraj(1:imt,1:jmt,kk,0)*tmpvflux(1:imt,1:jmt,kk)*&
                            (da(kk) + db(kk)*hs(1:imt,1:jmt,0))/(grav*tracertrajscale)

   END DO

   ! uflux and vflux computation (C points)

   ! dz in (C grid)
   DO kk = 1, km

     ! ufluxes
     uflux(1:imt-1,1:jmt,kk,2) = 0.5 * trunit*(tmpuflux(2:imt,1:jmt,kk) + tmpuflux(2:imt,0:jmt-1,kk))*dyu(1:imt-1,1:jmt)
     uflux(imt,1:jmt,kk,2)     = 0.5 * trunit*(tmpuflux(1,1:jmt,kk) + tmpuflux(1,0:jmt-1,kk))*dyu(imt,1:jmt)

     ! vfluxes
     vflux(1:imt-1,1:jmt,kk,2) = 0.5 * trunit*(tmpvflux(1:imt-1,1:jmt,kk) + tmpvflux(2:imt,1:jmt,kk))*dxv(1:imt-1,1:jmt)
     vflux(imt,1:jmt,kk,2)     = 0.5 * trunit*(tmpvflux(imt,1:jmt,kk) + tmpvflux(1,1:jmt,kk))*dxv(imt,1:jmt)

     ! Initial values of dz on C grids
     IF (ints==0) THEN

         dzt(1:imt-1,1:jmt,kk,1) =  0.25*trunit*(tracertraj(1:imt-1,1:jmt,kk,-1)*(da(kk) + db(kk)*hs(1:imt-1,1:jmt,-1))+ &
                               tracertraj(1:imt-1,0:jmt-1,kk,-1)*(da(kk) + db(kk)*hs(1:imt-1,0:jmt-1,-1))+ &
                               tracertraj(2:imt,0:jmt-1,kk,-1)*(da(kk) + db(kk)*hs(2:imt,0:jmt-1,-1))+ &
                               tracertraj(2:imt,1:jmt,kk,-1)*(da(kk) + db(kk)*hs(2:imt,1:jmt,-1)))/ (grav*tracertrajscale)

         dzt(imt,1:jmt,kk,1) =  0.25*trunit*(tracertraj(imt,1:jmt,kk,-1)*(da(kk) + db(kk)*hs(imt,1:jmt,-1))+ &
                               tracertraj(imt,0:jmt-1,kk,-1)*(da(kk) + db(kk)*hs(imt,0:jmt-1,-1))+ &
                               tracertraj(1,0:jmt-1,kk,-1)*(da(kk) + db(kk)*hs(1,0:jmt-1,-1))+ &
                               tracertraj(1,1:jmt,kk,-1)*(da(kk) + db(kk)*hs(1,1:jmt,-1)))/ (grav*tracertrajscale)


         dzt(1:imt-1,1:jmt,kk,2) =  0.25*trunit*(tracertraj(1:imt-1,1:jmt,kk,0)*(da(kk) + db(kk)*hs(1:imt-1,1:jmt,0))+ &
                                tracertraj(1:imt-1,0:jmt-1,kk,0)*(da(kk) + db(kk)*hs(1:imt-1,0:jmt-1,0))+ &
                                tracertraj(2:imt,0:jmt-1,kk,0)*(da(kk) + db(kk)*hs(2:imt,0:jmt-1,0))+ &
                                tracertraj(2:imt,1:jmt,kk,0)*(da(kk) + db(kk)*hs(2:imt,1:jmt,0)))/ (grav*tracertrajscale)

         dzt(imt,1:jmt,kk,2) =  0.25*trunit*(tracertraj(imt,1:jmt,kk,0)*(da(kk) + db(kk)*hs(imt,1:jmt,0))+ &
                              tracertraj(imt,0:jmt-1,kk,0)*(da(kk) + db(kk)*hs(imt,0:jmt-1,0))+ &
                              tracertraj(1,0:jmt-1,kk,0)*(da(kk) + db(kk)*hs(1,0:jmt-1,0))+ &
                              tracertraj(1,1:jmt,kk,0)*(da(kk) + db(kk)*hs(1,1:jmt,0)))/ (grav*tracertrajscale)

     END IF

     dzt(1:imt-1,1:jmt,kk,3) =  0.25*trunit*(tracertraj(1:imt-1,1:jmt,kk,1)*(da(kk) + db(kk)*hs(1:imt-1,1:jmt,1))+ &
                           tracertraj(1:imt-1,0:jmt-1,kk,1)*(da(kk) + db(kk)*hs(1:imt-1,0:jmt-1,1))+ &
                           tracertraj(2:imt,0:jmt-1,kk,1)*(da(kk) + db(kk)*hs(2:imt,0:jmt-1,1))+ &
                           tracertraj(2:imt,1:jmt,kk,1)*(da(kk) + db(kk)*hs(2:imt,1:jmt,1)))/ (grav*tracertrajscale)

     dzt(imt,1:jmt,kk,3) =  0.25*trunit*(tracertraj(imt,1:jmt,kk,1)*(da(kk) + db(kk)*hs(imt,1:jmt,1))+ &
                          tracertraj(imt,0:jmt-1,kk,1)*(da(kk) + db(kk)*hs(imt,0:jmt-1,1))+ &
                          tracertraj(1,0:jmt-1,kk,1)*(da(kk) + db(kk)*hs(1,0:jmt-1,1))+ &
                          tracertraj(1,1:jmt,kk,1)*(da(kk) + db(kk)*hs(1,1:jmt,1)))/ (grav*tracertrajscale)

   END DO

   ! dzdt calculation
   IF (ints == 0 .AND. ( loopYears .EQV..FALSE.)) THEN
       dzdt(1:imt,1:jmt,:,2)   = (dzt(1:imt,1:jmt,:,3) - dzt(1:imt,1:jmt,:,2))/tseas
   ELSE IF (ints == intrun-1 .AND. ( loopYears .EQV..FALSE.)) THEN
       dzdt(1:imt,1:jmt,:,2)   = (dzt(1:imt,1:jmt,:,2) - dzt(1:imt,1:jmt,:,1))/tseas
   ELSE
       dzdt(1:imt,1:jmt,:,2)   = 0.5*(dzt(1:imt,1:jmt,:,3) - dzt(1:imt,1:jmt,:,1))/tseas
   END IF

   !! Zero meridional flux at j=0 and j=jmt
   vflux(:,0  ,:,:) = 0.d0
   vflux(:,jmt,:,:) = 0.d0

   ! Correction of fluxes at the pole
   CALL pole_correction()

   ! Reverse the sign of fluxes if trajectories are run backward in time.
   CALL swap_sign()

END SUBROUTINE read_field


SUBROUTINE pole_correction()

    !==========================================================================
    !
    ! Purpose
    ! -------
    !
    ! Corrects the local abnormalous values in the poles
    !
    ! Method
    ! ------
    !
    ! Treat all the grids in the poles as a single one, compute the mean vertical
    ! flux and assume that all grids in the poles have that value. Ussing the continuity
    ! equation readjust the values of zonal fluxes.
    !
    ! Updates the variables:
    !   uflux
    ! ==========================================================================

    USE mod_precdef
    USE mod_param
    USE mod_vel
    USE mod_grid
    USE mod_vertvel

    IMPLICIT NONE


    INTEGER :: ii, kk

    REAL(DP), DIMENSION(:,:,:), ALLOCATABLE :: tmpwf, tmpuf


    !! Allocate fluxes
    ALLOCATE(tmpwf(imt,4,0:km))  ! 2dim : 1/2 - South Pole  3/4 - North Pole
    ALLOCATE(tmpuf(imt,2,km))    ! 2dim : 1   - South Pole    2 - North Pole

    tmpwf(:,:,:) = 0.d0; tmpuf(:,:,:) = 0.d0;

    !! 1 - Compute mean Wflux
    DO ii = 1, imt

        ! South pole
        IF (ii==1)  CALL vertvel(1,imt,1,km)
        IF (ii/=1)  CALL vertvel(ii,ii-1,1,km)

        tmpwf(ii,2,:) = wflux(:,2)

        ! North pole
        IF (ii==1)  CALL vertvel(1,imt,jmt,km)
        IF (ii/=1)  CALL vertvel(ii,ii-1,jmt,km)

        tmpwf(ii,4,:) = wflux(:,2)

    END DO

    FORALL (ii=1:imt) tmpwf(ii,1,:) = SUM(tmpwf(:,2,:),1)/imt
    FORALL (ii=1:imt) tmpwf(ii,3,:) = SUM(tmpwf(:,4,:),1)/imt

    !! 2 - Recalculate ufluxes from continuity
    DO ii = 1, imt
        DO kk = 1, km

            ! South pole
            tmpuf(ii,1,kk) = tmpwf(ii,1,kk-1)-tmpwf(ii,1,kk) - &
          ( + vflux(ii,1,kk,2) - vflux(ii,0,kk,2) ) &
            - dzdt(ii,1,kk,2)*dxdy(ii,1)

            ! North pole
            tmpuf(ii,2,kk) = tmpwf(ii,3,kk-1)-tmpwf(ii,3,kk) - &
          ( + vflux(ii,jmt,kk,2) - vflux(ii,jmt-1,kk,2) ) &
            - dzdt(ii,jmt,kk,2)*dxdy(ii,jmt)

           IF (ii == 1) THEN
             tmpuf(ii,1,kk) = tmpuf(ii,1,kk) + uflux(imt,1,kk,2)
             tmpuf(ii,2,kk) = tmpuf(ii,2,kk) + uflux(imt,jmt,kk,2)
           ELSE
             tmpuf(ii,1,kk) = tmpuf(ii,1,kk) + tmpuf(ii-1,1,kk)
             tmpuf(ii,2,kk) = tmpuf(ii,2,kk) + tmpuf(ii-1,2,kk)
           END IF

        END DO
    END DO

    ! Reassign the values of uflux
    uflux(:,1,:,2) = tmpuf(:,1,:); uflux(:,jmt,:,2) = tmpuf(:,2,:);

END SUBROUTINE
