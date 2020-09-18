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
  ! nend = 0 is reserved to the time limit
  ! nend = 1 is reserved to trajectories reaching the surface
  !
  ! ==========================================================================


  USE mod_traj
  USE mod_domain
  USE mod_tracervars

  IMPLICIT NONE

  INTEGER  :: nexit, itrac, numexit

  nend = -1

  SELECT CASE(exitType)

  ! Exit domain defined by the boxes [ienw, iene]x[jens, jenn]
  ! in the namelist
  CASE(1)
      DO nexit = 1, 10
         IF(ienw(nexit) <= x1 .AND. x1 <= iene(nexit) .AND. &
              jens(nexit) <= y1 .AND. y1 <= jenn(nexit) ) THEN
              nend = nexit +1
         END IF
      END DO

  ! Exit domain defined by the value of tracers in the namelist tracere
  CASE(2)

      numexit = MINLOC(tracerchoice, DIM=1,MASK=(tracerchoice==999)) - 1

      DO nexit = 1, numexit
         itrac = tracerchoice(nexit)

         IF (  maxormin(nexit)*tracervalue(itrac)>= maxormin(nexit)*tracere(nexit) ) THEN
                nend = nexit +1
         END IF

      END DO

  ! Exit domain defined by the value of tracers in the namelist tracere and domain
  CASE(3)

      ! First thermodynamic killing zone
      numexit = MINLOC(tracerchoice, DIM=1,MASK=(tracerchoice==999)) - 1

      DO nexit = 1, numexit
         itrac = tracerchoice(nexit)

         IF (  maxormin(nexit)*tracervalue(itrac)>= maxormin(nexit)*tracere(nexit) ) THEN
                nend = nexit +1
         END IF

      END DO

      ! Next the geographical killing zone
      DO nexit = 1, 10
         IF(ienw(nexit) <= x1 .AND. x1 <= iene(nexit) .AND. &
              jens(nexit) <= y1 .AND. y1 <= jenn(nexit) ) THEN
              nend = nexit +1 + numexit
         END IF
      END DO

  ! If the exit domained is defined in a different way:
  ! - Read from a file
  ! - Defined by curve, no linear shape, ...
  ! This must be hard coded below.

  CASE(4)
      PRINT*, 'Hard coded limits of the domain'

  END SELECT

END SUBROUTINE kill_zones
