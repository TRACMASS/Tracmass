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

  INTEGER        :: itrac
  INTEGER        :: k

  REAL(DP), ALLOCATABLE, DIMENSION(:,:,:)  :: tmp3d,dz3d,tmptracer
  CHARACTER (len=200)                      :: fieldFile, dateprefix
  REAL, PARAMETER                          :: missing_value=9.9692100E+36

  ! Reassign the time index of uflux and vflux, dzt, dzdt, hs, ...
  CALL swap_time()

  ! Data files
  dateprefix = ' '

  ALLOCATE(tmp3d(imt,jmt,km),dz3d(imt,jmt,km),tmptracer(imt,jmt,km))
  tmp3d(:,:,:) = 0.d0
  dz3d(:,:,:) = 0.d0
  tmptracer(:,:,:) = 0.d0

  ! Reading 3-time step variables
  ! In this case: dzt
  ! ===========================================================================
  IF (ints == 0) THEN

    ! 1 - Past
    IF (loopYears) THEN
      IF (l_onestep) nctstep = 1

      dateprefix = filledFileName(dateFormat, prevYear, prevMon, prevDay)

      fieldFile = TRIM(physDataDir)//TRIM(physPrefixForm)//TRIM(dateprefix)//TRIM(fileSuffix)
      dzt(:,:,km:1:-1,1) = get3DfieldNC(fieldFile,dzt_name,[imindom,jmindom,1,nctstep],[imt,jmt,km,1],'st')
      WHERE (dzt(:,:,:,1) .eq. missing_value) dzt(:,:,:,1) = 0.
      dzt(:,:,:,1) = dzt(:,:,:,1)/grav

      CALL count_ke(dzt(:,:,:,1),ke(:,:,1))
      !CALL write_ke(ke(:,:,1))
    END IF

    ! 2 - Present
    nctstep = currMon
    IF (l_onestep) nctstep = 1

    dateprefix = filledFileName(dateFormat, currYear, currMon, currDay)

    fieldFile = TRIM(physDataDir)//TRIM(physPrefixForm)//TRIM(dateprefix)//TRIM(fileSuffix)
    dzt(:,:,km:1:-1,2) = get3DfieldNC(fieldFile,dzt_name,[imindom,jmindom,1,nctstep],[imt,jmt,km,1],'st')
    WHERE (dzt(:,:,:,2) .eq. missing_value) dzt(:,:,:,2) = 0.
    dzt(:,:,:,2) = dzt(:,:,:,2)/grav   ! dp -> dp/g

    CALL count_ke(dzt(:,:,:,2),ke(:,:,2))
    !CALL write_ke(ke(:,:,2))

  END IF

  ! 3 - Future
  IF (ints<intrun-1 .OR. loopYears) THEN

    nctstep = nextMon
    IF (l_onestep) nctstep = 1

    dateprefix = filledFileName(dateFormat, nextYear, nextMon, nextDay)

    fieldFile = TRIM(physDataDir)//TRIM(physPrefixForm)//TRIM(dateprefix)//TRIM(fileSuffix)
    dzt(:,:,km:1:-1,3) = get3DfieldNC(fieldFile,dzt_name,[imindom,jmindom,1,nctstep],[imt,jmt,km,1],'st')
    dzt(:,:,:,3) = dzt(:,:,:,3)/grav   ! dp -> dp/g
    WHERE (dzt(:,:,:,3) .eq. missing_value) dzt(:,:,:,3) = 0.

    CALL count_ke(dzt(:,:,:,2),ke(:,:,2))

  END IF

   ! dzdt calculation
   IF (ints == 0 .AND. ( loopYears .EQV..FALSE.)) THEN
       dzdt(1:imt,1:jmt,:,2)   = (dzt(1:imt,1:jmt,:,3) - dzt(1:imt,1:jmt,:,2))/tseas
   ELSE IF (ints == intrun-1 .AND. ( loopYears .EQV..FALSE.)) THEN
       dzdt(1:imt,1:jmt,:,2)   = (dzt(1:imt,1:jmt,:,2) - dzt(1:imt,1:jmt,:,1))/tseas
   ELSE
       dzdt(1:imt,1:jmt,:,2)   = 0.5*(dzt(1:imt,1:jmt,:,3) - dzt(1:imt,1:jmt,:,1))/tseas
   END IF

  ! Reading 2-time step variables
  ! In this case: velocities and tracers
  ! ===========================================================================

   nctstep = currMon
   IF (l_onestep) nctstep = 1

   dateprefix = filledFileName(dateFormat, currYear, currMon, currDay)

   fieldFile = TRIM(physDataDir)//TRIM(physPrefixForm)//TRIM(dateprefix)//TRIM(fileSuffix)

   uvel(1:imt,1:jmt,km:1:-1) = get3DfieldNC(fieldFile, ueul_name,[imindom,jmindom,1,nctstep],[imt,jmt,km,1],'st')
   IF (usgs_name/='') THEN
     tmp3d(1:imt,1:jmt,km:1:-1)  = get3DfieldNC(fieldFile, usgs_name,[imindom,jmindom,1,nctstep],[imt,jmt,km,1],'st')
     uvel(1:imt,1:jmt,1:km) = uvel(1:imt,1:jmt,1:km) + tmp3d(1:imt,1:jmt,1:km)
   END IF
   WHERE (dzt(:,:,:,2) <= dzteps ) uvel(:,:,:) = 0.

   vvel(1:imt,0:jmt,km:1:-1) = get3DfieldNC(fieldFile, veul_name,[imindom,jmindom,1,nctstep],[imt,jmt+1,km,1],'st')
   IF (vsgs_name/='') THEN
     tmp3d(1:imt,0:jmt,km:1:-1) = get3DfieldNC(fieldFile, vsgs_name,[imindom,jmindom,1,nctstep],[imt,jmt+1,km,1],'st')
     vvel(1:imt,0:jmt,1:km) = vvel(1:imt,0:jmt,1:km) + tmp3d(1:imt,0:jmt,1:km)
   END IF
   ! set vvel=0 if dzt<dzteps; set vvel(jmt)=0 whenever each folding side cell has dzt<dzteps
   WHERE (dzt(:,1:jmt-1,:,2) <= dzteps ) vvel(:,1:jmt-1,:) = 0.                                     
   WHERE (dzt(1:imt,jmt,:,2) <= dzteps .or. dzt(imt:1:-1,jmt,:,2) <= dzteps ) vvel(:,jmt,:) = 0.

   ! shift u,v points staggerring from west/south to east/east
   uflux(1:imt-1,:,:,2)   = uvel(2:imt,:,:)
   uflux(imt,    :,:,2)   = uvel(1,    :,:)
   vflux(:,  0:jmt,:,2)   = vvel(:,0:jmt,:)

#if defined w_explicit
   wflux(1:imt,1:jmt,km:1:-1,2) = get3DfieldNC(fieldFile, w_name,[imindom,jmindom,1,nctstep],[imt,jmt,km,1],'st')
   WHERE (dzt(:,:,1:km,2) <= dzteps .and. abs(wflux(:,:,1:km,2)) < 0.1 ) wflux(:,:,1:km,2) = 0.
#endif

   !! Tracers
   IF (l_tracers) THEN

     DO itrac = 1, numtracers

        ! Make sure the data array is empty
        tmp3d(:,:,:) = 0.d0

        IF (tracers(itrac)%action == 'read') THEN

            ! Read the tracer from a netcdf file
            IF (tracers(itrac)%dimension == '3D') THEN
                IF(tracers(itrac)%name == 'depth') THEN
                  dz3d(1:imt,1:jmt,km:1:-1) = get3DfieldNC(fieldFile, tracers(itrac)%varname,[imindom,jmindom,1,nctstep] &
                                                ,[imt,jmt,km,1],'st')
                  tmp3d(:,:,km) = 0.
                  DO k = km-1, 1, -1
                    tmp3d(:,:,k) = tmp3d(:,:,k+1) - dz3d(:,:,k+1)
                  END DO
                ELSE
                  tmp3d(1:imt,1:jmt,km:1:-1) = get3DfieldNC(fieldFile, tracers(itrac)%varname,[imindom,jmindom,1,nctstep] &
                                                ,[imt,jmt,km,1],'st')
                END IF
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
          tmptracer  = tracers(itrac)%scale*tmp3d(:,:,:) + tracers(itrac)%shift
          tracers(itrac)%data(:,1:jmt,:,2) = tmptracer
        ELSE IF (tracers(itrac)%dimension == '2D') THEN
          tracers(itrac)%data(:,1:jmt,1,2) = tracers(itrac)%scale*tmp3d(:,:,1) + tracers(itrac)%shift
        END IF

     END DO
   END IF
   deallocate(tmp3d)
   deallocate(tmptracer)

   ! ===========================================================================

   ! Reverse the sign of fluxes if trajectories are run backward in time.
   CALL swap_sign()

 CONTAINS

   SUBROUTINE count_ke(dzt3d, ke2d)
   ! Count the empty cells below the mixed layer

    IMPLICIT NONE

    ! Input and output variables
    REAL(DP), INTENT(IN) :: dzt3d(imt, jmt, km)
    INTEGER, INTENT(OUT) :: ke2d(imt, jmt)

    ! Local variables
    INTEGER :: i, j, k
    LOGICAL :: l_found

    !! Initialize
    ke2d = 0

    ! Count vertical empty cells below mixed layers and above bottom (empty) layers
    DO j = 1, jmt
      DO i = 1, imt
        IF (mask(i,j) .ne. 0) THEN  ! land cells
          l_found = .false.
          DO k = km-2,1,-1          ! two mixed layers
            IF (dzt3d(i, j, k) < dzteps) THEN
              ke2d(i, j) = ke2d(i, j) + 1
              l_found = .true.
            ELSE IF (l_found) THEN ! stop at non-empty cell under empty-cell
              EXIT
            END IF
          END DO
          IF (k == 0) ke2d(i,j)=0 ! if all empty cells below mixed-layers
        END IF
      END DO
    END DO
    WHERE(mask .eq. 0) ke2d = -1  ! -1 for land cells

   END SUBROUTINE count_ke

  SUBROUTINE write_ke(ke2d)

   IMPLICIT NONE 

   INTEGER  :: i, j
   INTEGER, INTENT(in) :: ke2d(imt, jmt)

   WRITE(*,*) "WRITE ke to file ke.txt"
   OPEN(999, file='ke.txt', status='replace')
   WRITE(999, '(360I3)') ((ke(i, j,2), i = 1, imt), j = 1, jmt)
   CLOSE(999)
  END SUBROUTINE write_ke

END SUBROUTINE read_field
