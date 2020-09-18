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
       hs(1:imt,jmt:0:-1,-1) = EXP(get2DfieldNC(fieldFile, hs_name,[imindom,jmindom,1,nctstep],[imt,jmt+1,1,1],'extend'))

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
     hs(1:imt,jmt:0:-1,0) = EXP(get2DfieldNC(fieldFile, hs_name,[imindom,jmindom,1,nctstep],[imt,jmt+1,1,1],'extend'))

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
     hs(1:imt,jmt:0:-1,1) = EXP(get2DfieldNC(fieldFile, hs_name,[imindom,jmindom,1,nctstep],[imt,jmt+1,1,1],'extend'))

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
   tmpuflux(1:imt,jmt:0:-1,1:km) = get3DfieldNC(fieldFile, ueul_name,[imindom,jmindom,1,nctstep],[imt,jmt+1,km,1],'st','extend')

   fieldFile = TRIM(physDataDir)//TRIM(physPrefixForm)//TRIM(dateprefix)//TRIM(vGridName)//TRIM(fileSuffix)
   tmpvflux(1:imt,jmt:0:-1,1:km) = get3DfieldNC(fieldFile, veul_name,[imindom,jmindom,1,nctstep],[imt,jmt+1,km,1],'st','extend')


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
                                        ,[imt,jmt,1,1])
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
          tracers(itrac)%data(:,:,:,2) = tmptracer(:,:,:)
        ELSE IF (tracers(itrac)%dimension == '2D') THEN
          tracers(itrac)%data(:,:,1,2) = tmptracer(:,:,1)
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

   ! Reverse the sign of fluxes if trajectories are run backward in time.
   CALL swap_sign()


END SUBROUTINE read_field
