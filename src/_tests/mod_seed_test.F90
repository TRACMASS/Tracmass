PROGRAM mod_seed_test
    !!------------------------------------------------------------------------------
    !!
    !!       MODULE: mod_seed_test
    !!                Test module for MOD_SEED
    !!
    !!--------------------------------------------------------------------------


    USE mod_init
    USE mod_seed
    USE mod_calendar
    USE mod_write

    IMPLICIT NONE

    INTEGER :: ierr

    ! Read namelist and allocate arrays
    CALL init_namelist
    CALL init_alloc

    CALL init_calendar

    ! Header
    PRINT *, ''
    PRINT *, ' MOD_SEED :: TEST'
    PRINT *, ' ----------------------------------------------------------------'
    PRINT *, ''


    ! TEST 1 - Initialisation nsdMax (nomask)
    !!--------------------------------------------------------------------------
    PRINT *, ' * Test init_seed (seed 1): nsdMax (nomask)'
    ierr = 0

    CALL init_seed

    ierr = ERR_CALC(nsdMax == 200, ierr)

    CALL ERR_PRINT(ierr)

    ! Reset seed_time and seed_ijk/set
    DEALLOCATE(seed_ijk, seed_set, seed_tim)
    ! Reset trajectories
    DEALLOCATE(trajectories)


    ! TEST 2 - Initialisation nsdMax (mask)
    !!--------------------------------------------------------------------------
    PRINT *, ' * Test init_seed (seed 1): nsdMax (mask)'
    ierr = 0

    ! Fill mask
    mask(6:,:) = 0
    mask(:,6:) = 0

    CALL init_seed

    ierr = ERR_CALC(nsdMax == 50, ierr)

    CALL ERR_PRINT(ierr)

    ! Reset seed_time and seed_ijk/set
    DEALLOCATE(seed_ijk, seed_set, seed_tim)
    ! Reset trajectories
    DEALLOCATE(trajectories)

    ! TEST 3 - Initialisation nsdTim (seedTime 1)
    !!--------------------------------------------------------------------------
    PRINT *, ' * Test init_seed (seed 1): nsdTim (seedTime 1)'
    ierr = 0

    ! Fill mask
    mask(:,:) = 1

    ! seedTime
    seedtime = 1

    CALL init_seed

    ierr = ERR_CALC(seed_tim(1)  == 1, ierr)
    ierr = ERR_CALC(seed_tim(2)  == 2, ierr)
    ierr = ERR_CALC(seed_tim(10) == 10, ierr)

    CALL ERR_PRINT(ierr)

    ! Reset seed_time and seed_ijk/set
    DEALLOCATE(seed_ijk, seed_set, seed_tim)
    ! Reset trajectories
    DEALLOCATE(trajectories)

    ! TEST 4 - Initialisation nsdTim (seedTime 2)
    !!--------------------------------------------------------------------------
    PRINT *, ' * Test init_seed (seed 1): nsdTim (seedTime 2)'
    ierr = 0

    ! seedTime
    seedtime = 2

    CALL init_seed

    ierr = ERR_CALC(seed_tim(1)  == 1, ierr)
    ierr = ERR_CALC(seed_tim(2)  == 2, ierr)
    ierr = ERR_CALC(seed_tim(10) == 16, ierr)

    CALL ERR_PRINT(ierr)

    ! Reset seed_time and seed_ijk/set
    DEALLOCATE(seed_ijk, seed_set, seed_tim)
    ! Reset trajectories
    DEALLOCATE(trajectories)

    ! TEST 5 - Initialisation nsdMax (seed 2)
    !!--------------------------------------------------------------------------
    PRINT *, ' * Test init_seed (seed 2): nsdMax'
    ierr = 0

    ! seedType
    seedType = 2

    CALL init_seed

    ierr = ERR_CALC(nsdMax == 4, ierr)

    CALL ERR_PRINT(ierr)

    ! TEST 6 - Initialisation seed_ijk/ seed_set
    !!--------------------------------------------------------------------------
    PRINT*, ' * Test init_seed (seed 2): seed_ijk, seed_set'

    ierr = ERR_CALC(seed_ijk(4,1) == 12, ierr)
    ierr = ERR_CALC(seed_ijk(3,2) == 7, ierr)
    ierr = ERR_CALC(seed_ijk(2,3) == 10, ierr)
    ierr = ERR_CALC(seed_set(1,1) == 1, ierr)
    ierr = ERR_CALC(seed_set(2,2) == -1, ierr)

    CALL ERR_PRINT(ierr)

    ! Reset seed_time and seed_ijk/set
    DEALLOCATE(seed_ijk, seed_set, seed_tim)
    ! Reset trajectories
    DEALLOCATE(trajectories)

    ! TEST 7 - Initialisation nsdTim (seedTime 1)
    !!--------------------------------------------------------------------------
    PRINT *, ' * Test init_seed (seed 2): nsdTim (seedTime 1)'
    ierr = 0

    ! seedTime
    seedType = 2
    seedtime = 1

    CALL init_seed

    ierr = ERR_CALC(seed_tim(1)  == 1, ierr)
    ierr = ERR_CALC(seed_tim(2)  == 2, ierr)
    ierr = ERR_CALC(seed_tim(10) == 10, ierr)

    CALL ERR_PRINT(ierr)

    ! Reset seed_time and seed_ijk/set
    DEALLOCATE(seed_ijk, seed_set, seed_tim)
    ! Reset trajectories
    DEALLOCATE(trajectories)

    ! TEST 8 - Initialisation nsdTim (seedTime 2)
    !!--------------------------------------------------------------------------
    PRINT *, ' * Test init_seed (seed 2): nsdTim (seedTime 2)'
    ierr = 0

    ! seedTime
    seedType = 2
    seedtime = 2

    CALL init_seed

    ierr = ERR_CALC(seed_tim(1)  == 1, ierr)
    ierr = ERR_CALC(seed_tim(2)  == 2, ierr)
    ierr = ERR_CALC(seed_tim(10) == 16, ierr)

    CALL ERR_PRINT(ierr)

    ! Reset seed_time and seed_ijk/set
    DEALLOCATE(seed_ijk, seed_set, seed_tim)
    ! Reset trajectories
    DEALLOCATE(trajectories)

    ! TEST 9 - findtime (seedTime 1)
    !!--------------------------------------------------------------------------
    PRINT *, ''
    PRINT *, ' * Test seed (seedTime 1): findTime (ints in range)'
    ierr = 0

    ints = 2

    CALL init_seed
    CALL seed

    ierr = ERR_CALC(itim  == 2, ierr)

    CALL ERR_PRINT(ierr)

    PRINT *, ' * Test seed (seedTime 1): findTime (ints out range)'
    ierr = 0

    ints = 12

    CALL seed

    ierr = ERR_CALC(itim  == -1, ierr)

    CALL ERR_PRINT(ierr)

    ! Reset seed_time and seed_ijk/set
    DEALLOCATE(seed_ijk, seed_set, seed_tim)
    ! Reset trajectories
    DEALLOCATE(trajectories)

    ! TEST 10 - findtime (seedTime 2)
    !!--------------------------------------------------------------------------
    PRINT *, ' * Test seed (seedTime 2): findTime (ints in range)'
    ierr = 0

    ! SeedTime
    seedTime = 2

    ints = 2

    CALL init_seed
    CALL seed

    ierr = ERR_CALC(itim  == 2, ierr)

    CALL ERR_PRINT(ierr)

    PRINT *, ' * Test seed (seedTime 2): findTime (ints out range)'
    ierr = 0

    ints = 12

    CALL seed

    ierr = ERR_CALC(itim  == -1, ierr)

    CALL ERR_PRINT(ierr)

    ! Reset seed_time and seed_ijk/set
    DEALLOCATE(seed_ijk, seed_set, seed_tim)
    ! Reset trajectories
    DEALLOCATE(trajectories)

    ! TEST 11 - x,y,z (seedType 1)
    !!--------------------------------------------------------------------------
    PRINT *, ' * Test seed (seedType 1): x,y,z,subvol'

    CALL init_namelist

    ierr = 0

    ints = 2

    ! seedType/seedTime
    seedType = 1
    seedTime = 1

    uflux(:,:,:,:) = 10

    CALL open_outfiles

    CALL init_seed
    CALL seed

    CALL close_outfiles

    ierr = ERR_CALC(itim  == 2, ierr)
    ierr = ERR_CALC(x1  == 10, ierr)
    ierr = ERR_CALC(y1  == 9.5, ierr)
    ierr = ERR_CALC(z1  == 1.5, ierr)
    ierr = ERR_CALC(subvol  == 10., ierr)

    CALL ERR_PRINT(ierr)

    ! Reset seed_time and seed_ijk/set
    DEALLOCATE(seed_ijk, seed_set, seed_tim)
    ! Reset trajectories
    DEALLOCATE(trajectories)

    ! TEST 12 - x,y,z (seedType 2)
    !!--------------------------------------------------------------------------
    PRINT *, ' * Test seed (seedType 2): x,y,z,subvol'

    CALL init_namelist
    ierr = 0

    ints = 3
    ntractot = 0

    ! seedType/seedTime
    seedType = 2
    seedTime = 1

    uflux(:,:,:,:) = 10

    CALL init_seed
    CALL seed

    ierr = ERR_CALC(itim  == 3, ierr)
    ierr = ERR_CALC(x1  == 12, ierr)
    ierr = ERR_CALC(y1  == 13.5, ierr)
    ierr = ERR_CALC(z1  == 18.5, ierr)
    ierr = ERR_CALC(subvol  == 10., ierr)

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

END PROGRAM  mod_seed_test
