MODULE mod_diffusion
  !!---------------------------------------------------------------------------
  !!
  !!       MODULE mod_diffusion:
  !!
  !!          This module adds a small displacement to a particle
  !!
  !!          Subroutines included:
  !!              - diffuse
  !!              - displacement
  !!              - kill_zones_diffusion
  !!
  !!---------------------------------------------------------------------------

  USE mod_precdef
  USE mod_param
  USE mod_grid
  USE mod_error
  USE mod_activevars
  USE mod_domain

  IMPLICIT NONE

  CONTAINS

    SUBROUTINE diffuse(x1,y1,z1,ib,jb,kb,dt)
    ! --------------------------------------------------
    !
    ! Purpose:
    ! Add a small displacement to a particle within the model area
    !
    ! ---------------------------------------------------

      REAL(DP) :: x1, y1, z1        ! Position of trajectory
      REAL(DP) :: xd, yd, zd        ! Position of displacement
      REAL(DP) :: tmpX, tmpY, tmpZ  ! Temporary position of trajectory

      REAL(DP) :: dt                ! time step

      INTEGER  :: ib, jb, kb        ! Indexes
      INTEGER  :: tmpi, tmpj, tmpk  ! Temporary indexes

      INTEGER  :: numint = 0        ! Number of iterations

      LOGICAL  :: tryAgain

      ! Declare tmp values
      tmpX = x1;  tmpY = y1; tmpZ = z1
      tmpi = ib;  tmpj = jb; tmpk = kb

      ! initialise tryAgain
      tryAgain = .TRUE.

      DO WHILE(tryAgain)

          ! Update number of iterations
          numint = numint + 1

          ! Find random displacement
          CALL displacement(dt,xd,yd,zd)

          ! Convert displacement from meters to model coordinates
          xd = xd / dxv(ib,jb)
          yd = yd / dyu(ib,jb)
          zd = zd / dzt(ib,jb,kb,2)

          ! Update position temporarily
          tmpX = x1 + xd
          tmpY = y1 + yd
          tmpZ = z1 + zd

          !  East-west cyclic
          IF (iperio /= 0) THEN
              IF (tmpX <=  0.d0) THEN
                  tmpX = tmpX + DBLE(IMT)
              ELSE IF (tmpX > DBLE(IMT)) THEN
                  tmpX = tmpX - DBLE(IMT)
              END IF
          END IF

          ! North fold
          IF (jperio /= 0) THEN

            IF( tmpY + jmindom - 1 >= DBLE(JMTdom-1) .AND. jperio == 1) THEN
               tmpX = DBLE(IMT+2) - tmpX
               tmpY = 2*(JMTdom - jmindom) - tmpY
            END IF

          END IF

          ! Update box number temporarily
          tmpi = INT(tmpX) + 1
          tmpj = INT(tmpY) + 1
          tmpk = INT(tmpZ) + 1

          IF (tmpk==KM+1) THEN   ! prevent particles to cross the boundaries
             tmpk=KM
             tmpZ=DBLE(KM)       ! put them exactly at the surface for hydro
          END IF

          ! Check that particle is within column
          ! If False then a new position for the particle
          ! has been found.
          IF( 1<=tmpi .AND. tmpi<=IMT .AND. 1<=tmpj .AND. tmpj<=JMT .AND. tmpk<=km &
              .AND. kmt(tmpi,tmpj)>0 .AND. tmpk > km - kmt(tmpi,tmpj)) THEN
                  tryAgain = .FALSE.
          END IF

          ! If trajectory is out of kill_zones place the trajectory at the boundary
          CALL kill_zones_diffusion(x1, tmpX, y1, tmpY)

          ! If tryAgain is still true, the particle is outside model area. The
      		! displacement is not saved, but we make a new try to displace.

      		! "Infinite loop?"
          IF (numint>=100000 .AND. tryAgain) THEN
              tryAgain = .FALSE.

              ! Write warning
              CALL write_error(9)

              !Particle stuck in infinite diffusion loop. No diffusion added
              tmpX=x1 ; tmpY=y1 ; tmpZ=z1
              tmpi=ib ; tmpj=jb ; tmpk=kb
          END IF

      END DO

      ! Update return position
      x1 = tmpX
      y1 = tmpY
      ib = tmpi
      jb = tmpj

#ifndef w_2dim
      z1 = tmpZ
      kb = tmpk
#endif

    END SUBROUTINE diffuse

    SUBROUTINE displacement(dt, xd, yd, zd)
    ! --------------------------------------------------
    !
    ! Purpose:
    ! Calculates displacement based on four
    ! random numbers (q1,q2,q3,q4) and parameters
    ! Av/Ah defined in the namelist.in.
    !
    ! Method:
    ! The random displacement are computed following
    !  sqrt(-4*Ah*dt*log(1-q1))*cos(2 Pi q2),
    !  sqrt(-4*Ah*dt*log(1-q1))*sin(2 Pi q2),
    !  sqrt(-4*Av*dt*log(1-q3))*cos(2 Pi q4))
    !
    ! --------------------------------------------------

        REAL(DP), DIMENSION(4)    :: qrandom
        REAL(DP), INTENT(OUT)     :: xd, yd, zd
        REAL(DP)                  :: dt
        REAL(DP)                  :: Rh, Rv       ! Disk radius

        CALL random_number(qrandom)

        ! Horizontal displacement
        Rh = SQRT(-4.d0*Ah*dt*LOG(1-qrandom(1)))
        xd = Rh * COS(2.d0*PI*qrandom(2))
        yd = Rh * SIN(2.d0*PI*qrandom(2))

        ! Horizontal displacement
        Rv = SQRT(-4.d0*Av*dt*LOG(1-qrandom(3)))
#ifndef w_2dim
        zd = Rv * COS(2.d0*PI*qrandom(4))
#else
        zd = 0.d0
#endif

    END SUBROUTINE displacement

    SUBROUTINE kill_zones_diffusion(xb, xa, yb, ya)
    ! --------------------------------------------------
    !
    ! Purpose:
    ! If killing zones are activated, the subroutine makes
    ! sure that if the perturbation places the trajectory
    ! across the killing zones, the trajectory will be
    ! terminated correctly.
    !
    ! --------------------------------------------------

        REAL(DP), INTENT(INOUT) :: xb, xa, yb, ya
        INTEGER :: nexit

        DO nexit = 1, 10

            ! Latitude band
            IF (jens(nexit) == jenn(nexit)) THEN

              IF (ienw(nexit) <= xb .AND. ienw(nexit) <= xa .AND. &
                    xb <= iene(nexit) .AND. xa <= iene(nexit) .AND. &
                    ((ya > yb .AND. ya >= jens(nexit) .AND. jens(nexit) >= yb) .OR. &
                     (yb > ya .AND. yb >= jens(nexit) .AND. jens(nexit) >= ya))) THEN

                    ! Place the trajectory at the boundary
                    ya = DBLE(jens(nexit))

              END IF

            ! Longitude band
            ELSE IF (ienw(nexit) == iene(nexit)) THEN

              IF (jens(nexit) <= yb .AND. jens(nexit) <= ya .AND. &
                    yb <= jenn(nexit) .AND. ya <= jenn(nexit) .AND. &
                    ((xb > xa .AND. xb >= ienw(nexit) .AND. ienw(nexit) >= xa) .OR. &
                     (xa > xb .AND. xa >= ienw(nexit) .AND. ienw(nexit) >= xb))) THEN

                    ! Place the trajectory at the boundary
                    xa = DBLE(ienw(nexit))

              END IF

          END IF
      END DO

    END SUBROUTINE kill_zones_diffusion

END MODULE mod_diffusion
