PROGRAM mod_pos_test
    !!------------------------------------------------------------------------------
    !!
    !!       MODULE: mod_pos_test
    !!                Test module for MOD_POS
    !!
    !!--------------------------------------------------------------------------

    USE mod_init
    USE mod_pos
    USE mod_time, only: intrpr, intrpg

    IMPLICIT NONE

    INTEGER :: ierr

    ! Read namelist and allocate arrays
    CALL init_namelist
    CALL init_alloc

    ! Header
    PRINT *, ''
    PRINT *, ' MOD_POS :: TEST'
    PRINT *, ' ----------------------------------------------------------------'
    PRINT *, ''


    ! TEST 1 -
    !!--------------------------------------------------------------------------
    PRINT *, ' * Test cross_time: eastward/northward/upward (uu = um) '
    ierr = 0

    ! time interpolation
    intrpg = 0.5; intrpr = 0.5

    ! zonal flux definition
    uflux(:,:,:,:) = 1.

    CALL cross_time(1,2,5,5,1.5d0,dse,dsw)

    ierr = ERR_CALC(dsw == UNDEF, ierr)
    ierr = ERR_CALC(dse == 0.5, ierr)

    CALL ERR_PRINT(ierr)

    ! TEST 2 -
    !!--------------------------------------------------------------------------
    PRINT *, ' * Test cross_time: eastward/northward/upward (uu /= um) '
    ierr = 0

    ! time interpolation
    intrpg = 0.5; intrpr = 0.5

    ! zonal flux definition
    uflux(:,:,:,:) =  1.
    uflux(1,:,:,:) = -1.

    CALL cross_time(1,2,5,5,1.5d0,dse,dsw)

    ierr = ERR_CALC(dsw == UNDEF, ierr)
    ierr = ERR_CALC(dse == UNDEF, ierr)

    uflux(1,:,:,:) = 2.

    CALL cross_time(1,2,5,5,1.5d0,dse,dsw)

    ierr = ERR_CALC(dse == DLOG(1.5d0), ierr)
    ierr = ERR_CALC(dsw == UNDEF, ierr)

    CALL ERR_PRINT(ierr)

    ! TEST 3 -
    !!--------------------------------------------------------------------------
    PRINT *, ' * Test cross_time: westward/southward/downward (uu = um) '
    ierr = 0

    ! time interpolation
    intrpg = 0.5; intrpr = 0.5

    ! zonal flux definition
    uflux(:,:,:,:) = -1.

    CALL cross_time(1,2,5,5,1.5d0,dse,dsw)

    ierr = ERR_CALC(dse == UNDEF, ierr)
    ierr = ERR_CALC(dsw == 0.5, ierr)

    CALL ERR_PRINT(ierr)

    ! TEST 4 -
    !!--------------------------------------------------------------------------
    PRINT *, ' * Test cross_time: westward/southward/downward (uu /= um) '
    ierr = 0

    ! time interpolation
    intrpg = 0.5; intrpr = 0.5

    ! zonal flux definition
    uflux(:,:,:,:) = -1.
    uflux(1,:,:,:) =  1.

    CALL cross_time(1,2,5,5,1.5d0,dse,dsw)

    ierr = ERR_CALC(dsw == UNDEF, ierr)
    ierr = ERR_CALC(dse == UNDEF, ierr)

    uflux(1,:,:,:) = -2.

    CALL cross_time(1,2,5,5,1.5d0,dse,dsw)

    ierr = ERR_CALC(APPROX(dsw-DLOG(1/0.75d0)), ierr)
    ierr = ERR_CALC(dse == UNDEF, ierr)

    CALL ERR_PRINT(ierr)

    ! TEST 5 -
    !!--------------------------------------------------------------------------
    PRINT *, ''
    PRINT *, ' * Test calc_pos: (uu = um) '
    ierr = 0

    ! time interpolation
    intrpg = 0.5; intrpr = 0.5

    ! zonal flux definition
    uflux(:,:,:,:) = 1.

    CALL cross_time(1,2,5,5,1.5d0,dse,dsw)
    CALL calc_pos(1,2,5,5,1.5d0,x1,dse)

    ierr = ERR_CALC(APPROX(x1-2.0), ierr)

    CALL cross_time(1,10,5,5,9.5d0,dse,dsw)
    CALL calc_pos(1,10,5,5,9.5d0,x1,dse)

    ierr = ERR_CALC(APPROX(x1-10.0), ierr)

    ! zonal flux definition
    uflux(:,:,:,:) = -1.

    CALL cross_time(1,2,5,5,1.5d0,dse,dsw)
    CALL calc_pos(1,2,5,5,1.5d0,x1,dsw)

    ierr = ERR_CALC(APPROX(x1-1.0), ierr)

    CALL ERR_PRINT(ierr)

    ! TEST 6 -
    !!--------------------------------------------------------------------------
    PRINT *, ' * Test calc_pos: (uu /= um) '
    ierr = 0

    ! time interpolation
    intrpg = 0.5; intrpr = 0.5

    ! zonal flux definition
    uflux(:,:,:,:) = -1.
    uflux(1,:,:,:) =  1.

    CALL cross_time(1,2,5,5,1.5d0,dse,dsw)
    CALL calc_pos(1,2,5,5,1.5d0,x1,dsw)
    ierr = ERR_CALC(APPROX(x1-1.5d0), ierr)

    CALL cross_time(1,2,5,5,1.4d0,dse,dsw)
    CALL calc_pos(1,2,5,5,1.4d0,x1,dsw)
    ierr = ERR_CALC(APPROX(x1-1.5d0), ierr)

    CALL cross_time(1,2,5,5,1.6d0,dse,dsw)
    CALL calc_pos(1,2,5,5,1.6d0,x1,dse)
    ierr = ERR_CALC(APPROX(x1-1.5d0), ierr)

    ! zonal flux definition
    uflux(:,:,:,:) =  1.
    uflux(1,:,:,:) = -1.

    CALL cross_time(1,2,5,5,1.4d0,dse,dsw)
    CALL calc_pos(1,2,5,5,1.4d0,x1,dsw)
    ierr = ERR_CALC(APPROX(x1-1.d0), ierr)

    CALL cross_time(1,2,5,5,1.6d0,dse,dsw)
    CALL calc_pos(1,2,5,5,1.6d0,x1,dse)
    ierr = ERR_CALC(APPROX(x1-2.d0), ierr)

    CALL ERR_PRINT(ierr)

    ! TEST 7 -
    !!--------------------------------------------------------------------------
    PRINT *, ''
    PRINT *, ' * Test update_traj: eastward (uu = um) '
    ierr = 0

    ! time interpolation
    intrpg = 0.5; intrpr = 0.5

    ! zonal flux definition
    uflux(:,:,:,:) = 1.

    CALL cross_time(1,2,5,5,1.6d0,dse,dsw)

    ds = MIN(dse,dsw)
    ib = 2

    CALL update_traj(2,1,5,5,ib,5,5,1.6d0,5.d0,5.d0,x1,y1,z1)

    ierr = ERR_CALC(APPROX(x1-2.d0), ierr)
    ierr = ERR_CALC(APPROX(y1-5.d0), ierr)
    ierr = ERR_CALC(APPROX(z1-5.d0), ierr)
    ierr = ERR_CALC(ib == 3, ierr)

    CALL ERR_PRINT(ierr)

    PRINT *, ' * Test update_traj: westward (uu = um) '
    ierr = 0

    ! time interpolation
    intrpg = 0.5; intrpr = 0.5

    ! zonal flux definition
    uflux(:,:,:,:) = -1.

    CALL cross_time(1,2,5,5,1.6d0,dse,dsw)

    ds = MIN(dse,dsw)
    ib = 2

    CALL update_traj(2,1,5,5,ib,5,5,1.6d0,5.d0,5.d0,x1,y1,z1)

    ierr = ERR_CALC(APPROX(x1-1.d0), ierr)
    ierr = ERR_CALC(APPROX(y1-5.d0), ierr)
    ierr = ERR_CALC(APPROX(z1-5.d0), ierr)
    ierr = ERR_CALC(ib == 1, ierr)

    CALL ERR_PRINT(ierr)

    ! TEST 8 -
    !!--------------------------------------------------------------------------
    PRINT *, ' * Test update_traj: eastward (uu /= um) '
    ierr = 0

    ! time interpolation
    intrpg = 0.5; intrpr = 0.5

    ! zonal flux definition
    uflux(:,:,:,:) = 1.
    uflux(1,:,:,:) = 2.

    CALL cross_time(1,2,5,5,1.6d0,dse,dsw)

    ds = MIN(dse,dsw)
    ib = 2

    CALL update_traj(2,1,5,5,ib,5,5,1.6d0,5.d0,5.d0,x1,y1,z1)

    ierr = ERR_CALC(APPROX(x1-2.d0), ierr)
    ierr = ERR_CALC(APPROX(y1-5.d0), ierr)
    ierr = ERR_CALC(APPROX(z1-5.d0), ierr)
    ierr = ERR_CALC(ib == 3, ierr)

    CALL ERR_PRINT(ierr)

    PRINT *, ' * Test update_traj: westward (uu = um) '
    ierr = 0

    ! time interpolation
    intrpg = 0.5; intrpr = 0.5

    ! zonal flux definition
    uflux(:,:,:,:) = -1.
    uflux(1,:,:,:) = -2.

    CALL cross_time(1,2,5,5,1.6d0,dse,dsw)

    ds = MIN(dse,dsw)
    ib = 2

    CALL update_traj(2,1,5,5,ib,5,5,1.6d0,5.d0,5.d0,x1,y1,z1)

    ierr = ERR_CALC(APPROX(x1-1.d0), ierr)
    ierr = ERR_CALC(APPROX(y1-5.d0), ierr)
    ierr = ERR_CALC(APPROX(z1-5.d0), ierr)
    ierr = ERR_CALC(ib == 1, ierr)

    CALL ERR_PRINT(ierr)

    ! TEST 9 -
    !!--------------------------------------------------------------------------
    PRINT *, ' * Test update_traj: corners '

    ierr = 0

    ! time interpolation
    intrpg = 0.5; intrpr = 0.5

    ! zonal flux definition
    uflux(:,:,:,:) = 1.
    vflux(:,:,:,:) = 1.

    CALL cross_time(1,2,2,5,1.5d0,dse,dsw)
    CALL cross_time(2,2,2,5,1.5d0,dsn,dss)

    ds = MIN(dse,dsw,dsn,dss)

    ib = 2; jb = 2
    CALL update_traj(2,1,2,5,ib,jb,5,1.5d0,1.5d0,5.d0,x1,y1,z1)

    ierr = ERR_CALC(APPROX(x1-2.d0), ierr)
    ierr = ERR_CALC(APPROX(y1-2.d0), ierr)
    ierr = ERR_CALC(APPROX(z1-5.d0), ierr)
    ierr = ERR_CALC(ib == 3, ierr)
    ierr = ERR_CALC(jb == 3, ierr)

    CALL ERR_PRINT(ierr)

    PRINT *, ''
    PRINT *, ' ----------------------------------------------------------------'
    PRINT *, ''

    ! **************************************************************************

    CONTAINS

    SUBROUTINE ERR_PRINT(ierr_dum)

      INTEGER :: ierr_dum

      IF (ierr_dum == 0) THEN
          PRINT*, '   =====================> PASS'
      ELSE
          PRINT*, '   =====================> FAIL'
          STOP
      END IF

    END SUBROUTINE

    INTEGER FUNCTION ERR_CALC(log_dum, ierr_dum)

      LOGICAL :: log_dum
      INTEGER :: ierr_dum

      IF (log_dum .EQV. .TRUE.) THEN
          ERR_CALC = ierr_dum
      ELSE
          ERR_CALC = ierr_dum + 1
      END IF

    END FUNCTION

    LOGICAL FUNCTION APPROX(dum)

      REAL(8) :: dum

      IF (ABS(dum)<1e-6) THEN
          APPROX = .TRUE.
      ELSE
          APPROX = .FALSE.
      END IF

    END FUNCTION

END PROGRAM  mod_pos_test
