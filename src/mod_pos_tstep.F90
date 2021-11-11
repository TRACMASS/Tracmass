#ifndef time_analytical

MODULE mod_pos
    !!------------------------------------------------------------------------------
    !!
    !!       MODULE: mod_pos (tstep)
    !!
    !!          Calculate the new positions of the trajectory
    !!          and the time it will take to cross a wall
    !!
    !!          Time configuration : time step
    !!
    !!          Subroutines included:
    !!               - cross_time
    !!               - calc_pos
    !!               - update_traj
    !!
    !!               - update_trajdir (P)
    !!               - update_bounce (P)
    !!
    !!------------------------------------------------------------------------------

    USE mod_precdef
    USE mod_loopvars
    USE mod_vertvel
    USE mod_traj
    USE mod_stream

    USE mod_grid, only: undef, imt, jmt, km, nsm, nsp, iperio, jperio
    USE mod_vel, only: uflux, vflux, wflux, uu, um, vv, vm
    USE mod_time, only: dtreg, intrpr, intrpg

    IMPLICIT none

    REAL(DP)                                   :: r0, r1
    REAL(DP)                                   :: ba, sp, sn
    INTEGER                                    :: ijk, ii, im, jm

    CONTAINS

      SUBROUTINE cross_time(ijk,ia,ja,ka,r0,sp,sn)
      ! --------------------------------------------------
      !
      ! Purpose:
      ! Computes the time (spn, sn) when trajectory
      ! crosses face of box (ia,ja,ka)
      !
      ! --------------------------------------------------

          INTEGER  :: ijk ! considered direction
          INTEGER  :: ia  ! zonal index
          INTEGER  :: ja  ! meridional index
          INTEGER  :: ka  ! vertical index

          REAL(DP) :: r0  ! current position (referenced to the box)
          REAL(DP) :: sp  ! crossing time east/north/up direction
          REAL(DP) :: sn  ! crossing time west/south/down direction

          ! ijk : considered direction (1=zonal, 2=meridional, 3=vertical)
          IF ( ijk .EQ. 1) THEN
              ii=ia
              im=ia-1
              uu = (intrpg*uflux(ia,ja,ka,nsp) + intrpr*uflux(ia,ja,ka,nsm))
              IF( (im .EQ. 0) .AND. (iperio .EQ. 1) ) im = IMT
              IF( (im .EQ. 0) .AND. (iperio .EQ. 0)) THEN
                um = 0.d0
              ELSE
                um = (intrpg*uflux(im,ja,ka,nsp) + intrpr*uflux(im,ja,ka,nsm))
              END IF

          ELSEIF (ijk .EQ. 2) THEN
              ii=ja
              jm=ja-1
              uu = (intrpg*vflux(ia,ja,ka,nsp) + intrpr*vflux(ia,ja,ka,nsm))
              um = (intrpg*vflux(ia,jm,ka,nsp) + intrpr*vflux(ia,jm,ka,nsm))

          ELSEIF (ijk .EQ. 3) THEN
              ii=ka
#if defined  w_explicit
              uu = (intrpg*wflux(ia,ja,ka  ,nsp) + intrpr*wflux(ia,ja,ka  ,nsm))
              um = (intrpg*wflux(ia,ja,ka-1,nsp) + intrpr*wflux(ia,ja,ka-1,nsm))
#else
              CALL vertvel(ia,iam,ja,ka)
              uu = (intrpg*wflux(ka  ,nsp) + intrpr*wflux(ka  ,nsm))
              um = (intrpg*wflux(ka-1,nsp) + intrpr*wflux(ka-1,nsm))
#endif
          END IF

          ! East, North or Upward crossing
          IF (uu .GT. 0.d0 .AND. r0 .NE. DBLE(ii)) THEN
              IF (um .NE. uu) THEN
                ba = (r0+DBLE(-ii+1)) * (uu-um) + um
                IF(ba .GT. 0.d0) THEN
                  sp = (DLOG(ba) - DLOG(uu))/(um-uu)
                ELSE
                  sp = UNDEF
                END IF
              ELSE
                sp = (DBLE(ii)-r0)/uu
              END IF
          ELSE
              sp = UNDEF
          END IF
          IF (sp .LE. 0.d0) sp = UNDEF

          ! West, South or Downward crossing
          IF (um .LT. 0.d0 .AND. r0 .NE. DBLE(ii-1)) THEN
             IF (um .NE. uu) THEN
               ba = -((r0-DBLE(ii)) * (uu-um) + uu)
               IF (ba .GT. 0.d0) THEN
                 sn = (DLOG(ba) - DLOG(-um))/(um-uu)
               ELSE
                 sn = UNDEF
               END IF
             ELSE
               sn = (DBLE(ii-1)-r0)/uu
             END IF
          ELSE
             sn = UNDEF
          END IF

          IF (sn .LE. 0.d0) sn = UNDEF

          RETURN

      END SUBROUTINE cross_time

      SUBROUTINE  calc_pos(ijk,ia,ja,ka,r0,r1,ds)
      ! ------------------------------------------------------------------
      !
      ! Purpose:
      ! Computes new position (r0 --> r1) of trajectory after time ds
      ! the new coordinate is still on one of the faces of box at ia,ja,ka
      !
      ! ------------------------------------------------------------------

          INTEGER  :: ijk ! considered direction
          INTEGER  :: ia  ! zonal index
          INTEGER  :: ja  ! meridional index
          INTEGER  :: ka  ! vertical index

          REAL(DP) :: r0  ! current position (referenced to the box)
          REAL(DP) :: r1  ! new position
          REAL(DP) :: ds  ! time interval

#ifdef w_2dim
          IF (ijk .EQ. 3) THEN
             r1 = r0
             RETURN
          END IF
#endif

          IF (ijk .EQ. 1) THEN
             ii = ia
             im = ia-1
             uu = (intrpg*uflux(ia,ja,ka,nsp) + intrpr*uflux(ia,ja,ka,nsm))
             IF( (im .EQ. 0) .AND. (iperio .EQ. 0)) THEN
               um = 0.d0
             ELSE
               IF( (im .EQ. 0) .AND. (iperio .EQ. 1)) im = IMT
               um = (intrpg*uflux(im,ja,ka,nsp) + intrpr*uflux(im,ja,ka,nsm))
             END IF

          ELSE IF (ijk .EQ. 2) THEN
             ii = ja
             jm = ja-1
             uu = (intrpg*vflux(ia,ja,ka,nsp) + intrpr*vflux(ia,ja,ka,nsm))
             um = (intrpg*vflux(ia,jm,ka,nsp) + intrpr*vflux(ia,jm,ka,nsm))

          ELSE IF (ijk .EQ. 3) THEN
              ii = ka
#if defined  w_explicit
              uu = (intrpg*wflux(ia,ja,ka  ,nsp) + intrpr*wflux(ia,ja,ka  ,nsm))
              um = (intrpg*wflux(ia,ja,ka-1,nsp) + intrpr*wflux(ia,ja,ka-1,nsm))
#else
              CALL vertvel(ia,iam,ja,ka)
              uu = (intrpg*wflux(ka  ,nsp) + intrpr*wflux(ka  ,nsm))
              um = (intrpg*wflux(ka-1,nsp) + intrpr*wflux(ka-1,nsm))
#endif

          END IF

          ! New position
          IF (ABS(um/(uu-um)) < 1.e14) THEN
             IF ((iperio .EQ. 1)  .AND. r0 == DBLE(IMT) .AND. ii==1 .AND. ijk==1) THEN
               r1 = (-DBLE(ii-1) + um/(uu-um)) * &
                    DEXP( (uu-um)*ds ) + DBLE(ii-1) - um/(uu-um)
             ELSE
               r1 = (r0+(-DBLE(ii-1) + um/(uu-um))) * &
                    DEXP( (uu-um)*ds ) + DBLE(ii-1) - um/(uu-um)
             END IF
          ELSE
             r1 = r0 + uu*ds
          END IF

          RETURN

      END SUBROUTINE calc_pos

      SUBROUTINE update_traj(ia,iam,ja,ka,ib,jb,kb,x0,y0,z0,x1,y1,z1)
      ! --------------------------------------------------
      !
      ! Purpose:
      ! Update of the trajectory position x1,y1,z1
      !
      ! --------------------------------------------------

          INTEGER  :: ia  ! zonal index
          INTEGER  :: iam ! neighbouring zonal index (left)
          INTEGER  :: ja  ! meridional index
          INTEGER  :: ka  ! vertical index

          INTEGER  :: ib  ! future zonal index
          INTEGER  :: jb  ! future meridional index
          INTEGER  :: kb  ! future vertical index

          REAL(DP), INTENT(INOUT)  :: x0, y0, z0  ! current position (referenced to the box)
          REAL(DP), INTENT(OUT) :: x1, y1, z1  ! future position (referenced to the box)

          ! New position
          scrivi  = .FALSE.

          ! Direction array
          trajdir(:) = 0

          ! Boxface
          boxface = 0

          ! Eastward grid-cell exit
          IF (ds==dse) THEN
             scrivi=.FALSE.
             uu = (intrpg*uflux(ia,ja,ka,nsp) + intrpr*uflux(ia ,ja,ka,nsm))

             IF (uu .GT. 0.d0) THEN
                ib=ia+1
                IF((ib .GT. IMT) .AND. (iperio .EQ. 1)) ib=ib-IMT
             END IF
             x1=DBLE(ia)

             CALL calc_pos(2,ia,ja,ka,y0,y1,ds) ! meridional position
             CALL calc_pos(3,ia,ja,ka,z0,z1,ds) ! vertical position

             ! In case of corners
             IF (ds == dsn) jb = ja + 1
             IF (ds == dss) jb = ja - 1
             IF (ds == dsu) kb = ka + 1
             IF (ds == dsd) kb = ka - 1

             ! Trajectory direction
             trajdir(1) = 1

             ! Boxface
             boxface = 1

             ! Streamfunction
             IF (l_psi .AND. (l_offline .EQV. .FALSE.) .AND. (xyflux==1)) CALL update_fluxes(ia, ja, trajdir(1), 'xy')
             IF (l_psi .AND. (l_offline .EQV. .FALSE.))                   CALL update_fluxes(ia, ka, trajdir(1), 'xz')

          ! Westward grid-cell exit
          ELSE IF (ds==dsw) THEN
             scrivi=.FALSE.
             uu = (intrpg*uflux(iam,ja,ka,nsp) + intrpr*uflux(iam ,ja,ka,nsm))

             IF (uu .LT. 0.d0) THEN
                ib=iam
             END IF
             x1=DBLE(iam)

             CALL calc_pos(2,ia,ja,ka,y0,y1,ds) ! meridional position
             CALL calc_pos(3,ia,ja,ka,z0,z1,ds) ! vertical position

             ! In case of corners
             IF (ds == dsn) jb = ja + 1
             IF (ds == dss) jb = ja - 1
             IF (ds == dsu) kb = ka + 1
             IF (ds == dsd) kb = ka - 1

             ! Trajectory direction
             trajdir(1) = -1

             ! Boxface
             boxface = 2

             ! Streamfunction
             IF (l_psi .AND. (l_offline .EQV. .FALSE.) .AND. (xyflux==1)) CALL update_fluxes(iam, ja, trajdir(1), 'xy')
             IF (l_psi .AND. (l_offline .EQV. .FALSE.))                   CALL update_fluxes(iam, ka, trajdir(1), 'xz')

          ! Northward grid-cell exit
          ELSE IF (ds==dsn) THEN
             scrivi=.FALSE.
             uu = (intrpg*vflux(ia,ja,ka,nsp) + intrpr*vflux(ia,ja,ka,nsm))

             IF (uu .GT. 0.d0) THEN
                jb=ja+1
             END IF
             y1=DBLE(ja)

             CALL calc_pos(1,ia,ja,ka,x0,x1,ds) ! zonal position
             CALL calc_pos(3,ia,ja,ka,z0,z1,ds) ! vertical position

             ! In case of corners
             IF (ds == dse) ib = ia + 1
             IF (ds == dsw) ib = iam
             IF (ds == dsu) kb = ka + 1
             IF (ds == dsd) kb = ka - 1

             ! Trajectory direction
             trajdir(2) = 1

             ! Boxface
             boxface = 3

             ! Streamfunction
             IF (l_psi .AND. (l_offline .EQV. .FALSE.) .AND. (xyflux==2)) CALL update_fluxes(ia, ja, trajdir(2), 'xy')
             IF (l_psi .AND. (l_offline .EQV. .FALSE.))                   CALL update_fluxes(ja, ka, trajdir(2), 'yz')

          ! Southward grid-cell exit
          ELSE IF (ds==dss) THEN
             scrivi=.FALSE.
             uu = (intrpg*vflux(ia,ja-1,ka,nsp) + intrpr*vflux(ia,ja-1,ka,nsm))

             IF (uu .LT. 0.d0) THEN
                jb=ja-1
             END IF
             y1=DBLE(ja-1)

             CALL calc_pos(1,ia,ja,ka,x0,x1,ds) ! zonal position
             CALL calc_pos(3,ia,ja,ka,z0,z1,ds) ! vertical position

             ! In case of corners
             IF (ds == dse) ib = ia + 1
             IF (ds == dsw) ib = iam
             IF (ds == dsu) kb = ka + 1
             IF (ds == dsd) kb = ka - 1

             ! Trajectory direction
             trajdir(2) = -1

             ! Boxface
             boxface = 4

             ! Streamfunction
             IF (l_psi .AND. (l_offline .EQV. .FALSE.) .AND. (xyflux==2)) CALL update_fluxes(ia, ja-1, trajdir(2), 'xy')
             IF (l_psi .AND. (l_offline .EQV. .FALSE.))                   CALL update_fluxes(ja-1, ka, trajdir(2), 'yz')

          ! Upward grid-cell exit
          ELSE IF (ds==dsu) THEN
             scrivi=.FALSE.
             CALL vertvel(ia, iam, ja, ka)
#if defined w_explicit
             uu = (intrpg*wflux(ia,ja,ka,nsp) + intrpr*wflux(ia,ja,ka,nsm))
#else
             uu = (intrpg*wflux(ka,nsp) + intrpr*wflux(ka,nsm))
#endif

             IF (uu .GT. 0.d0) kb=ka+1

             z1=DBLE(ka)

             IF (kb==KM+1) THEN   ! prevent particles to cross the boundaries
                kb=KM
                IF (l_nosurface) THEN
                  z1 = DBLE(KM) - 0.5d0 ! place them back in the middle of the gridbox
                ELSE
                  z1 = DBLE(KM)         ! put them exactly at the surface
                END IF
             END IF

             CALL calc_pos(1,ia,ja,ka,x0,x1,ds) ! zonal position
             CALL calc_pos(2,ia,ja,ka,y0,y1,ds) ! meridional position

             ! In case of corners
             IF (ds == dse) ib = ia + 1
             IF (ds == dsw) ib = iam
             IF (ds == dsn) jb = ja + 1
             IF (ds == dss) jb = ja - 1

             ! Trajectory direction
             trajdir(3) = 1

             ! Boxface
             boxface = 5

             IF (l_nosurface) THEN
               IF(z1<z0)trajdir(3) = -1
               boxface    = 0
             END IF

          ! Downward grid-cell exit
          ELSE IF (ds==dsd) THEN

             scrivi=.FALSE.
             CALL vertvel(ia, iam, ja, ka)
#if defined w_explicit
             uu = (intrpg*wflux(ia,ja,ka-1,nsp) + intrpr*wflux(ia,ja,ka-1,nsm))
#else
             uu = (intrpg*wflux(ka-1,nsp) + intrpr*wflux(ka-1,nsm))
#endif

             IF (uu .LT. 0.d0) kb=ka-1

             z1=DBLE(ka-1)

             CALL calc_pos(1,ia,ja,ka,x0,x1,ds) ! zonal position
             CALL calc_pos(2,ia,ja,ka,y0,y1,ds) ! meridional position

             ! In case of corners
             IF (ds == dse) ib = ia + 1
             IF (ds == dsw) ib = iam
             IF (ds == dsn) jb = ja + 1
             IF (ds == dss) jb = ja - 1

             ! Trajectory direction
             trajdir(3) = -1

             ! Boxface
             boxface = 6

          ! Shortest time is the time-steping
          ELSE IF (ds==dsc .OR. ds==dsmin) THEN
             scrivi=.TRUE.

             ! If there is no spatial solution, i.e. a convergence zone
             IF(dse==UNDEF .AND. dsw==UNDEF .AND. dsn==UNDEF .AND. &
                dss==UNDEF .AND. dsu==UNDEF .AND. dsd==UNDEF ) THEN
                ib=ia ; jb=ja ; kb=ka
             END IF

             CALL calc_pos(1,ia,ja,ka,x0,x1,ds) ! zonal crossing
             CALL calc_pos(2,ia,ja,ka,y0,y1,ds) ! merid. crossing
             CALL calc_pos(3,ia,ja,ka,z0,z1,ds) ! vert. crossing

          END IF

          !  East-west cyclic
          IF (iperio /= 0) THEN
              IF (x1 <=  0.d0) THEN
                  x1 = x1 + DBLE(IMT)
              ELSE IF (x1 > DBLE(IMT)) THEN
                  x1 = x1 - DBLE(IMT)
              END IF
          END IF

          ! North fold
          IF (jperio /= 0) THEN

            IF( y1 + jmindom - 1 == DBLE(JMTdom-1)  .AND. jperio == 1) THEN
               x1 = DBLE(IMT+2) - x1
               ib = IDINT(x1) + 1
               jb = JMTdom - jmindom + 1
               ia=ib ; ja=jb
               x0 = x1; y0 = y1
            END IF

          END IF

          ! Make sure that trajectory is inside ib,jb,kb box
          IF (x1 /= DBLE(IDINT(x1))) ib = IDINT(x1)+1
          IF (y1 /= DBLE(IDINT(y1))) jb = IDINT(y1)+1
          IF (z1 /= DBLE(IDINT(z1))) kb = IDINT(z1)+1

          ! Update direction array
          CALL update_trajdir()

      END SUBROUTINE update_traj

      SUBROUTINE update_trajdir()
      ! --------------------------------------------------
      !
      ! Purpose:
      ! Update of the direction array
      !
      ! --------------------------------------------------

          ! Zonal direction
          IF (x0 < x1 .AND. x0<=1. .AND. x1>=DBLE(imt-1)) THEN
              trajdir(1) =  -1
          ELSEIF (x0 < x1 ) THEN
              trajdir(1) = 1
          ELSEIF (x0 > x1 .AND. x0>=DBLE(imt-1) .AND. x1<=1)  THEN
              trajdir(1) = 1
          ELSEIF (x0 > x1) THEN
              trajdir(1) = -1
          END IF

          ! Meridional direction
          IF (y0 < y1) trajdir(2) =  1
          IF (y0 > y1) trajdir(2) = -1

          ! Vertical direction
          IF (z0 < z1) trajdir(3) =  1
          IF (z0 > z1) trajdir(3) = -1

      END SUBROUTINE update_trajdir

      SUBROUTINE update_bounce(ia, iam, ja, ka, x0, y0, z0)
      ! --------------------------------------------------
      !
      ! Purpose:
      ! Update the indexes of the trajectory in case of
      ! a sudden change of fluxes sign
      !
      ! --------------------------------------------------

          ! Position indexes
          INTEGER, INTENT(INOUT)   :: ia, iam, ja, ka
          INTEGER                  :: tmpia, tmpiam, tmpja, tmpka

          ! Real positions
          REAL(DP), INTENT(INOUT)  :: x0, y0, z0

          ! Fluxes
          REAL(DP) :: uu, um ! Fluxes

          ! Temporal storage of indexes
          tmpia  = ia
          tmpiam = iam
          tmpja  = ja
          tmpka  = ka

          ! Zonal walls
          IF (x0 == DBLE(ia)) THEN

              uu = (intrpg*uflux(ia,ja,ka,nsp) + intrpr*uflux(ia,ja,ka,nsm))

              IF (uu .GT. 0.d0) THEN
                  ! Redifine the indexes
                  tmpiam  = ia
                  tmpia   = ia + 1
                  IF ( (tmpia .EQ. IMT + 1) .AND. (iperio .EQ. 1)) tmpia = 1
              END IF

          ELSE IF (x0 == DBLE(iam)) THEN

              um = (intrpg*uflux(iam,ja,ka,nsp) + intrpr*uflux(iam,ja,ka,nsm))

              IF (um .LT. 0.d0) THEN
                  ! Redifine the indexes
                  tmpia  = iam
                  tmpiam = tmpia - 1
                  IF( (tmpiam .EQ. 0) .AND. (iperio .EQ. 1) ) tmpiam = IMT
              END IF

          END IF

          ! Meridional wall
          IF (y0 == DBLE(ja)) THEN

              uu = (intrpg*vflux(ia,ja,ka,nsp) + intrpr*vflux(ia,ja,ka,nsm))

              IF (uu .GT. 0.d0) THEN
                  ! Redifine the indexes
                  tmpja   = ja + 1
              END IF

          ELSE IF (y0 == DBLE(ja-1)) THEN

              um = (intrpg*vflux(ia,ja-1,ka,nsp) + intrpr*vflux(ia,ja-1,ka,nsm))

              IF (um .LT. 0.d0) THEN
                  ! Redifine the indexes
                  tmpja  = ja - 1
              END IF

          END IF

          ! Vertical wall
          IF (z0 == DBLE(ka)) THEN

            ! Recalculate the fluxes
#if defined  w_explicit
            uu = (intrpg*wflux(ia,ja,ka  ,nsp) + intrpr*wflux(ia,ja,ka  ,nsm))
#else
            CALL vertvel(ia,iam,ja,ka)
            uu = (intrpg*wflux(ka  ,nsp) + intrpr*wflux(ka  ,nsm))
#endif
              IF (uu .GT. 0.d0) THEN
                  ! Redifine the indexes
                  tmpka   = ka + 1
              END IF

          ELSE IF (z0 == DBLE(ka-1)) THEN

#if defined  w_explicit
              um = (intrpg*wflux(ia,ja,ka-1,nsp) + intrpr*wflux(ia,ja,ka-1,nsm))
#else
              CALL vertvel(ia,iam,ja,ka)
              um = (intrpg*wflux(ka-1,nsp) + intrpr*wflux(ka-1,nsm))
#endif

              IF (um .LT. 0.d0) THEN
                  ! Redifine the indexes
                  tmpka  = ka - 1
              END IF

          END IF

          ! Reassign indexes
          ia  = tmpia
          iam = tmpiam
          ja  = tmpja
          ka  = tmpka

      END SUBROUTINE update_bounce

END MODULE mod_pos

#endif
