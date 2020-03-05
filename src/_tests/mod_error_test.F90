PROGRAM mod_pos_test
    !!------------------------------------------------------------------------------
    !!
    !!       MODULE: mod_error_test
    !!                Test module for MOD_ERROR
    !!
    !!--------------------------------------------------------------------------

    USE mod_trajdef
    USE mod_error
    USE mod_write
    USE mod_init

    INTEGER :: ierr

    ! Open file
    CALL init_namelist
    CALL init_alloc
    CALL open_outfiles


    ! Header
    PRINT *, ''
    PRINT *, ' MOD_ERROR :: TEST'
    PRINT *, ' ----------------------------------------------------------------'
    PRINT *, ''


    ! TEST 1 -
    !!--------------------------------------------------------------------------
    PRINT *, ' * Test errorCheck :: Infinite loop'
    ierr = 0

    ntrac = 1;  ALLOCATE ( trajectories(ntrac))

    trajectories(1)%niter = 1

    ! No infinite loop
    niter = 100
    CALL errorCheck('infLoopError',errCode)

    ierr = ERR_CALC(errCode==0, ierr)

    ! Infinite loop
    niter = 40000
    CALL errorCheck('infLoopError',errCode)

    ierr = ERR_CALC(errCode==1, ierr)

    CALL ERR_PRINT(ierr)

    ! TEST 2 -
    !!--------------------------------------------------------------------------
    PRINT *, ' * Test errorCheck :: Volume error'
    ierr = 0

    ! Positive volume
    dxyz = 100.
    CALL errorCheck('dxyzError',errCode)

    ierr = ERR_CALC(errCode==0, ierr)

    ! Negative volume
    dxyz = -10.
    CALL errorCheck('dxyzError',errCode)

    ierr = ERR_CALC(errCode==2, ierr)

    CALL ERR_PRINT(ierr)

    ! TEST 3 -
    !!--------------------------------------------------------------------------
    PRINT *, ' * Test errorCheck :: Boundary error'
    ierr = 0

    ib = 1; ja =1; jb =1; y0 =1; y1 = 1

    ! trajectory in the domain
    ia = imt
    CALL errorCheck('boundError',errCode)

    ierr = ERR_CALC(errCode==0, ierr)

    ! trajectory out the domain
    ia = -1
    CALL errorCheck('boundError',errCode)

    ierr = ERR_CALC(errCode==3, ierr)

    CALL ERR_PRINT(ierr)

    ! TEST 4 -
    !!--------------------------------------------------------------------------
    PRINT *, ' * Test errorCheck :: Land error'
    ierr = 0

    ib = 1; jb = 1

    ! No land
    kmt(:,:) = 1
    CALL errorCheck('landError',errCode)

    ierr = ERR_CALC(errCode==0, ierr)

    ! Trajectory hits land
    kmt(:,:) = 0
    CALL errorCheck('landError',errCode)

    ierr = ERR_CALC(errCode==4, ierr)

    CALL ERR_PRINT(ierr)

    ! TEST 5 -
    !!--------------------------------------------------------------------------
    PRINT *, ' * Test errorCheck :: Wrong box error'
    ierr = 0

    ! Correct box
    x1 = 8.5; ib = 9
    CALL errorCheck('coordboxError',errCode)
    ierr = ERR_CALC(errCode==0, ierr)

    y1 = 5.5; jb = 6
    CALL errorCheck('coordboxError',errCode)
    ierr = ERR_CALC(errCode==0, ierr)

    ! Wrong box
    x1 = 8.5; ib = 10
    CALL errorCheck('coordboxError',errCode)
    ierr = ERR_CALC(errCode==5, ierr)

    x1 = 8.5; ib = 9
    y1 = 5.5; jb = 4
    CALL errorCheck('coordboxError',errCode)
    ierr = ERR_CALC(errCode==6, ierr)

    CALL ERR_PRINT(ierr)

    ! TEST 6 -
    !!--------------------------------------------------------------------------
    PRINT *, ' * Test errorCheck :: Unknown path'
    ierr = 0

    ! Right path
    ds = 1.d0
    CALL errorCheck('dsCrossError',errCode)

    ierr = ERR_CALC(errCode==0, ierr)

    ! Unknown path
    ds = UNDEF
    CALL errorCheck('dsCrossError',errCode)

    ierr = ERR_CALC(errCode==8, ierr)

    CALL ERR_PRINT(ierr)


    CALL close_outfiles

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
