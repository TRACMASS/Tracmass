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
  USE mod_tracervars

  IMPLICIT NONE

  INTEGER  :: nexit, itrac, numexit

  nend = 0

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

  ! Exit domain defined by the value of tracers in the namelist traceremin, traceremax
  CASE(2)

      numexit = MINLOC(tracerchoice, DIM=1,MASK=(tracerchoice==999)) - 1

      DO nexit = 1, numexit
         itrac = tracerchoice(nexit)

         IF ( &
            ! tracere is a maximum value of tracer
            (tracer0max(itrac)/=999.d0 .AND. tracer0max(itrac)<=tracere(nexit) .AND. tracervalue(itrac)>=tracere(nexit)) .OR. &
            ! tracere is a minimum value of tracer
            (tracer0min(itrac)/=-999.d0 .AND. tracer0min(itrac)>=tracere(nexit) .AND. tracervalue(itrac)<=tracere(nexit)) &
            ) THEN
                nend = nexit +1
         END IF
      END DO


  ! If the exit domained is defined in a different way:
  ! - Read from a file
  ! - Defined by curve, no linear shape, ...
  ! This must be hard coded below.
  CASE(3)
      PRINT*, 'Hard coded limits of the domain'
  END SELECT

END SUBROUTINE kill_zones
