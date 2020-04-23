SUBROUTINE kill_zones
  !==========================================================================
  !
  ! Purpose
  ! -------
  !
  ! Defines the limits of the domain.
  ! If a trajectory is outside the domain the subroutine will identify it
  ! with a flag (nend)
  !
  ! nend = 1 is reserved to the time limit
  !
  ! ==========================================================================


  USE mod_traj
  USE mod_domain

  IMPLICIT NONE

  INTEGER :: nexit

  nend = 0

  SELECT CASE(exitType)

  ! Exit domain defined by the boxes [ienw, iene]x[jens, jenn]
  ! in the namelist
  CASE(1)
      DO nexit =1, 10
         IF(ienw(nexit) <= x1 .AND. x1 <= iene(nexit) .AND. &
              jens(nexit) <= y1 .AND. y1 <= jenn(nexit) ) THEN
              nend = nexit +1
         END IF
      END DO

  ! If the exit domained is defined in a different way:
  ! - Read from a file
  ! - Defined by curve, no linear shape, ...
  ! This must be hard coded below.
  CASE(2)
      PRINT*, 'Hard coded limits of the domain'
  END SELECT

END SUBROUTINE kill_zones
