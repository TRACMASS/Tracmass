MODULE mod_tracers
    !!------------------------------------------------------------------------------
    !!
    !!       MODULE: mod_tracers
    !!
    !!       This module includes subroutines to read the tracers file
    !!              - init_tracers
    !!              - update_tracer
    !!              - tracers_default
    !!
    !!------------------------------------------------------------------------------

    USE mod_tracervars
    USE mod_grid
    USE mod_traj
    USE mod_time

    IMPLICIT NONE

    INTEGER                :: itrac
    INTEGER, DIMENSION(20) :: numtracerarray = 0

    PRIVATE :: tracers_default

    CONTAINS

    SUBROUTINE init_tracer()
    ! --------------------------------------------------
    !
    ! Purpose:
    ! Read the tracer information from the namelist, and initialise
    ! and allocate the tracer array.
    !
    ! --------------------------------------------------

        ! Calculate the number of tracers
        WHERE (tracername==' ') numtracerarray = 1
        numtracers = 20 - SUM(numtracerarray)

        ! Allocate the tracer array
        ALLOCATE(tracers(numtracers), tracervalue(numtracers))
        ALLOCATE(tracerbin(numtracers,2),dtracervalue(numtracers))

        ! Assigned default values
        CALL tracers_default

        ! Assigned values to tracer array from namelist
        DO itrac = 1, numtracers

          tracers(itrac)%name = tracername(itrac)        ! Description of the tracer
          tracers(itrac)%unit = tracerunit(itrac)        ! Unit of the tracer

          tracers(itrac)%minimum = tracermin(itrac)      ! minimum value of the tracer
          tracers(itrac)%maximum = tracermax(itrac)      ! Maximum value of the tracer

          tracers(itrac)%action = traceraction(itrac)    ! read or compute tracer

          tracers(itrac)%varname = tracervarname(itrac)  ! if the file is read, this is the name of the variable

          ! Allocate and define the data array
          IF (tracerdimension(itrac) == '3D' .OR. tracerdimension(itrac) == '3d' .OR. tracerdimension(itrac) == '3') THEN
              tracers(itrac)%dimension = '3D' ! Dimension of the tracer
              ALLOCATE( tracers(itrac)%data(imt, jmt, km, 2))
          ELSE IF (tracerdimension(itrac) == '2D' .OR. tracerdimension(itrac) == '2d' .OR. tracerdimension(itrac) == '2') THEN
              tracers(itrac)%dimension = '2D'
              ALLOCATE( tracers(itrac)%data(imt, jmt, 1, 2))
          END IF

          ! Allocate and define the data array
          tracers(itrac)%data(:,:,:,:) = 0.d0

          ! tracer bins
          dtracervalue(itrac) = (tracermax(itrac) - tracermin(itrac))/DBLE(resolution - 1)

        END DO

    END SUBROUTINE init_tracer

    SUBROUTINE update_tracer(ntrac,ia,ja,ka, ib,jb,kb,x1,y1,z1)
    ! --------------------------------------------------
    !
    ! Purpose:
    ! Update the value of the tracer
    !
    ! --------------------------------------------------
        INTEGER  :: ntrac

        INTEGER  :: ia  ! zonal index
        INTEGER  :: ja  ! meridional index
        INTEGER  :: ka  ! vertical index

        INTEGER  :: ib  ! future zonal index
        INTEGER  :: jb  ! future meridional index
        INTEGER  :: kb  ! future vertical index

        REAL(DP) :: x1, y1, z1  ! future position (referenced to the box)

        REAL(DP)    :: tu, tm
        INTEGER     :: kbtracer, katracer
        INTEGER     :: itrac

        DO itrac = 1, numtracers

          ! Interpolation method (nearest point)
          kbtracer = kb; katracer = ka
          IF (tracers(itrac)%dimension == '2D') kbtracer = 1
          IF (tracers(itrac)%dimension == '2D') katracer = 1

          ! The trajectory is within the grid cell
          IF (x1/=DBLE(ib) .AND. y1/=DBLE(jb) .AND. z1/=DBLE(kb)) THEN
              tu = tracers(itrac)%data(ib,jb,kbtracer,nsp)
              tm = tracers(itrac)%data(ib,jb,kbtracer,nsm)
          ! The trajectory crosses a wall
          ELSE
              tu  = 0.5*(tracers(itrac)%data(ib,jb,kbtracer,nsm) + tracers(itrac)%data(ia,ja,katracer,nsm))
              tm  = 0.5*(tracers(itrac)%data(ib,jb,kbtracer,nsp) + tracers(itrac)%data(ia,ja,katracer,nsp))
          END IF

          tracervalue(itrac) = (intrpg*tm + intrpr*tu)

          ! Tracerbins
          tracerbin(itrac,1) =  tracerbin(itrac,2)
          tracerbin(itrac,2) =  NINT((tracervalue(itrac) - tracers(itrac)%minimum)/dtracervalue(itrac)) + 1

          tracerbin(itrac,2) = MAX(1,tracerbin(itrac,2))
          tracerbin(itrac,2) = MIN(resolution,tracerbin(itrac,2))

        END DO

        trajectories(ntrac)%tracerval(:) = tracervalue




    END SUBROUTINE update_tracer

    SUBROUTINE tracers_default()
    ! --------------------------------------------------
    !
    ! Purpose:
    ! Define the default values of the most common tracers.
    !
    ! --------------------------------------------------

      DO itrac = 1, numtracers

        ! Temperature (ocean)
        IF (TRIM(tracername(itrac)) == 'To') THEN

                  tracers(itrac)%unit = 'degC'    ! Unit of the tracer

                  tracers(itrac)%minimum = -3.d0  ! minimum value of the tracer
                  tracers(itrac)%maximum = 33.d0  ! Maximum value of the tracer

                  tracers(itrac)%action = 'read'  ! read or compute tracer

                  tracers(itrac)%varname = 'T'    ! if the file is read, this is the name of the variable

        ! Temperature (atmosphere)
        ELSE IF (TRIM(tracername(itrac)) == 'Ta') THEN

                  tracers(itrac)%unit = 'K'    ! Unit of the tracer

                  tracers(itrac)%minimum = 173.d0  ! minimum value of the tracer
                  tracers(itrac)%maximum = 223.d0  ! Maximum value of the tracer

                  tracers(itrac)%action = 'read'  ! read or compute tracer

                  tracers(itrac)%varname = 'T'    ! if the file is read, this is the name of the variable

        ! Potential temperature (atmosphere)
        ELSE IF (TRIM(tracername(itrac)) == 'Tp') THEN

                  tracers(itrac)%unit = 'K'    ! Unit of the tracer

                  tracers(itrac)%minimum = 173.d0  ! minimum value of the tracer
                  tracers(itrac)%maximum = 223.d0  ! Maximum value of the tracer

                  tracers(itrac)%action = 'read'  ! read or compute tracer

                  tracers(itrac)%varname = 'Tp'    ! if the file is read, this is the name of the variable

        ! Salinity
        ELSE IF (TRIM(tracername(itrac)) == 'S') THEN

                  tracers(itrac)%unit = 'g kg-1'    ! Unit of the tracer

                  tracers(itrac)%minimum = 32.d0    ! minimum value of the tracer
                  tracers(itrac)%maximum = 38.d0    ! Maximum value of the tracer

                  tracers(itrac)%action = 'read'    ! read or compute tracer

                  tracers(itrac)%varname = 'S'      ! if the file is read, this is the name of the variable

        ! Specific humidity
        ELSE IF (TRIM(tracername(itrac)) == 'q') THEN

                  tracers(itrac)%unit = 'g kg-1'    ! Unit of the tracer

                  tracers(itrac)%minimum = 0.d0     ! minimum value of the tracer
                  tracers(itrac)%maximum = 25.d0    ! Maximum value of the tracer

                  tracers(itrac)%action = 'read'    ! read or compute tracer

                  tracers(itrac)%varname = 'q'      ! if the file is read, this is the name of the variable

        ! Density (sigma0)
        ELSE IF (TRIM(tracername(itrac)) == 'sigma0') THEN

                  tracers(itrac)%unit = 'kg m-3'     ! Unit of the tracer

                  tracers(itrac)%minimum = 19.d0     ! minimum value of the tracer
                  tracers(itrac)%maximum = 29.d0     ! Maximum value of the tracer

                  tracers(itrac)%action = 'compute'  ! read or compute tracer

                  tracers(itrac)%varname = ''        ! if the file is read, this is the name of the variable

        ! Pressure (atmosphere)
        ELSE IF (TRIM(tracername(itrac)) == 'p') THEN

                  tracers(itrac)%unit = 'hPa'     ! Unit of the tracer

                  tracers(itrac)%minimum = 0.d0     ! minimum value of the tracer
                  tracers(itrac)%maximum = 1100.d0     ! Maximum value of the tracer

                  tracers(itrac)%action = 'compute'  ! read or compute tracer

                  tracers(itrac)%varname = ''        ! if the file is read, this is the name of the variable

        ! Other tracer
        ELSE

                  tracers(itrac)%unit = 'n.u.'     ! Unit of the tracer

                  tracers(itrac)%minimum = 0.d0     ! minimum value of the tracer
                  tracers(itrac)%maximum = 100.d0     ! Maximum value of the tracer

                  tracers(itrac)%action = 'read'  ! read or compute tracer

                  tracers(itrac)%varname = ''        ! if the file is read, this is the name of the variable

        END IF
      END DO

    END SUBROUTINE tracers_default

END MODULE mod_tracers
