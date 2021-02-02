#ifdef time_analytical

MODULE mod_pos
    !!------------------------------------------------------------------------------
    !!
    !!       MODULE: mod_pos (time analytical solution)
    !!
    !!          Calculate the new positions of the trajectory
    !!          and the time it will take to cross a wall
    !!
    !!          Time configuration : time analytical solution
    !!
    !!          Subroutines included:
    !!               - cross_time
    !!               - azer
    !!               - calc_pos
    !!               - update_traj
    !!
    !!               - update_trajdir (P)
    !!
    !!------------------------------------------------------------------------------

    USE mod_precdef
    USE mod_loopvars
    USE mod_vertvel
    USE mod_traj
    USE mod_stream

    USE mod_param, only: errlim
    USE mod_grid, only: undef, imt, jmt, km, nsm, nsp, iperio, jperio
    USE mod_vel, only: uflux, vflux, wflux, uu, um, vv, vm
    USE mod_time, only: dtreg, intrpr, intrpg

    IMPLICIT none

    REAL(DP)                                   :: r0, r1
    REAL(DP)                                   :: ba, sp, sn
    INTEGER                                    :: ijk, ii, im, jm, il

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
          REAL(DP) :: rw  ! crossing wall

          REAL(DP) :: sp  ! crossing time east/north/up direction
          REAL(DP) :: sn  ! crossing time west/south/down direction
          REAL(DP) :: sm  ! s^n-1
          REAL(DP) :: sw  ! crossing time
          REAL(DP) :: s0  ! initial time


          REAL(DP) :: alfa ! Alpha value

          ! Lower time step
          sm = DBLE(INT(ts))*tseas/dxyz

          ! Initial time
          s0 = tt/dxyz

#ifdef w_2dim
          IF (ijk .EQ. 3) THEN
             sp = UNDEF
             sn = UNDEF
             RETURN
          END IF
#endif

          ! ijk : considered direction (i=zonal, 2=meridional, 3=vertical)
          IF ( ijk .EQ. 1) THEN
              ii=ia
              im=ia-1; il=im
              IF( (il .EQ. 0) .AND. (iperio .EQ. 1) ) il = IMT
              uu = uflux(ia,ja,ka,nsm)
              um = uflux(il,ja,ka,nsm)
              vv = uflux(ia,ja,ka,nsp)
              vm = uflux(il,ja,ka,nsp)

          ELSEIF (ijk .EQ. 2) THEN
              ii=ja
              im=ja-1
              uu = vflux(ia,ja,ka,nsm)
              um = vflux(ia,ja-1,ka,nsm)
              vv = vflux(ia,ja,ka,nsp)
              vm = vflux(ia,ja-1,ka,nsp)

          ELSEIF (ijk .EQ. 3) THEN
              ii=ka
              im=ka-1
#if defined  w_explicit
              uu = wflux(ia,ja,ka  ,nsm)
              um = wflux(ia,ja,ka-1,nsm)
              vv = wflux(ia,ja,ka  ,nsp)
              vm = wflux(ia,ja,ka-1,nsp)
#else
              uu = wflux(ka  ,nsm)
              um = wflux(ka-1,nsm)
              vv = wflux(ka  ,nsp)
              vm = wflux(ka-1,nsp)
#endif

          END IF

          !print*,'what i get', ia, ja, ii, im, uu, um , vv, vm
          !Define alpha constant
          alfa = -(vv-vm-uu+um)

          ! Solutions depending on the value of alpha
          IF (alfa>0.0d0) THEN
            !CALL apos()
            stop 'alfa>0'
          ELSE IF (alfa<0.0d0) THEN
            !CALL aneg()
            stop 'alfa<0'
          ELSE
            CALL azer(ii,im,uu,um,vv,vm,r0,rw,sm,s0,sw)
          END IF

          ! Eastern/northern/upper wall
          IF (rw == DBLE(ii)) THEN
            sp = sw - s0
            sn = UNDEF
          ! Western/southern/lower wall
          ELSEIF (rw == DBLE(im)) THEN
            sp = UNDEF
            sn = sw - s0
          ELSE
            sp = UNDEF
            sn = UNDEF
          END IF

          IF (sp .LE. 0.d0) sp = UNDEF
          IF (sn .LE. 0.d0) sn = UNDEF

          RETURN

      END SUBROUTINE cross_time

      SUBROUTINE azer(ii,iim,uu,um,vv,vm,r0,rw,sm,s0,sw)
      ! ------------------------------------------------------------------
      !
      ! Purpose:
      ! Computes the crossing time for alpha = 0 case
      !
      ! ------------------------------------------------------------------

          ! Position indexes
          INTEGER,INTENT(IN)  :: ii   ! East, north, up boxface index
          INTEGER,INTENT(IN)  :: iim  ! West, south, low boxface index

          ! Fluxes
          REAL(DP) :: uu   ! Fi   (n-1)
          REAL(DP) :: um   ! Fi-1 (n-1)
          REAL(DP) :: vv   ! Fi   (n)
          REAL(DP) :: vm   ! Fi-1 (n)

          ! Position variables
          REAL(DP)             :: r0   ! current position
          REAL(DP),INTENT(OUT) :: rw   ! crossing wall position

          ! Time Variables
          REAL(DP) :: sm              ! s^n-1
          REAL(DP) :: s0              ! current time
          REAL(DP) :: ssii, ssiim
          REAL(DP),INTENT(OUT) :: sw  ! crossing wall time
          REAL(DP) :: si00, sia       ! ds

          ! Parameters from the paper
          REAL(DP) :: beta
          REAL(DP) :: delta
          REAL(DP) :: gamma

          REAL(DP) :: del_ga     ! delta/gamma

          ! Other variables
          REAL(DP) :: xi
          REAL(DP) :: exp_term    ! terms that go with the exponential exp(-beta(s-s0))
          REAL(DP) :: drds        ! dr/ds term
          REAL(DP) :: dr, dra, rs, ss

          INTEGER  :: rootloop = 0
          INTEGER  :: wcase = 1    ! wcase = 1 e/n/u wall // -1 w/s/d wall


          ! sw by default is UNDEF
          sw = UNDEF
          ! Beta parameter
          beta = um - uu

          ! Gamma parameter
          gamma = (um - vm)/dstep

          ! Transport across the grid constant
          IF (beta == 0.0d0) THEN

            ! Fluxes constant time and space
            IF (gamma == 0.0d0) THEN

                ! Delta parameter
                delta = -um

                ! Eastward/northward/upper flow
                IF (delta<0.d0) THEN
                  rw = DBLE(ii)
                  sw = s0 - (rw-r0)/delta

                ! Westward/southward/lower flow
                ELSE IF (delta>0.d0) THEN
                  rw = DBLE(ii-1)
                  sw = s0 - (rw-r0)/delta
                END IF

            ! Fluxes not constant in time
            ELSE

                IF (ABS(gamma)<EPS) STOP 'Gamma to small'

                del_ga = -(um + beta*DBLE(iim))/gamma - sm

                IF (s0 + del_ga >= 0.d0) THEN

                  IF (gamma>0.d0)  rw = DBLE(iim)
                  IF (gamma<0.d0)  rw = DBLE(ii)

                  sw = -del_ga + SQRT( (s0 + del_ga)**2. - 2*(rw-r0)/gamma)

                ELSE

                  IF (gamma>0.d0) rw = DBLE(ii)
                  IF (gamma<0.d0) rw = DBLE(iim)

                  IF ( (s0 + del_ga)**2 >= 2.*(rw-r0)/gamma ) THEN

                    sw = -del_ga - SQRT( (s0 + del_ga)**2. - 2*(rw-r0)/gamma)

                  ELSE
                    IF (gamma>0.d0)  rw = DBLE(iim)
                    IF (gamma<0.d0)  rw = DBLE(ii)

                    sw = -del_ga + SQRT( (s0 + del_ga)**2. - 2*(rw-r0)/gamma)
                  END IF

                END IF

            END IF

          ELSE

            ! Fluxes constant in time
            IF (gamma == 0.0d0) THEN

              ! Positive u_i flux
              IF (uu > 0.d0) THEN
                xi = 1.0d0+(DBLE(ii)-r0)*beta/(beta*(r0-DBLE(iim))-um)
                IF (xi>0.0d0) THEN
                    IF ((xi < EPS) .OR. (ABS(beta) < EPS)) STOP 'Unable to find a solution'
                    rw = DBLE(ii)
                    sw = s0 - 1.0d0*LOG(xi)/beta

                    IF (sw < EPS) STOP
                    IF (sw <= 0.d0) sw = UNDEF
                END IF
              END IF

              ! Negative u_i-1 flux
              IF (um < 0.d0) THEN
                xi = 1.0d0-(r0-DBLE(iim))*beta/(beta*(r0-DBLE(iim))-um)
                IF (xi>0.0d0) THEN
                    IF ((xi < EPS) .OR. (ABS(beta) < EPS)) STOP 'Unable to find a solution'
                    rw = DBLE(iim)
                    sw = s0 - 1.0d0*LOG(xi)/beta

                    IF (sw < EPS) STOP 'Very small time step'
                    IF (sw <= 0.d0) sw = UNDEF
                END IF
              END IF

            ! Fluxes not constant in time
            ELSE

              IF (ABS(beta)<EPS) STOP 'Beta equal to zero'

              ! 1 - Initial configuration

              ! Define boundary times
              ssii  =  uu/gamma         ! Fi^(n-1) / gamma    [time unit]
              ssiim =  um/gamma         ! Fi-1^(n-1) / gamma  [time unit]

              ! Terms with exp(-beta(s-s0))
              exp_term = (r0-DBLE(iim)) + (gamma*(s0-sm-1.d0/beta)-um)/beta

              ! drds at r0, s0
              drds = um - gamma*(s0-sm) - beta*(r0-DBLE(iim))

              ! 2.1 - ii direction (four velocity configurations at edge)
              iiselect: DO
                  IF ((ssii <= 0.d0) .OR. (ssii>= dstep)) THEN
                      ! 2a - configuration (1) paper + + +
                      IF (uu > 0.d0 .OR. vv > 0.d0) THEN
                        rw = DBLE(ii)

                        ! si00 = s^n - s0
                        si00 = sm + dstep - s0
                      ! 2b - configuaration (4) paper - - -
                      ELSE
                        rw = DBLE(iim)
                        wcase = -1
                        EXIT iiselect
                      END IF
                  ELSE
                     ! 2c - configuration (3) paper - 0 +
                     IF (vv > 0.d0) THEN
                       rw = DBLE(ii)

                       ! si00 = s^n - s0
                       si00 = sm + dstep - s0
                     ! 2d - configuration (2) paper + 0 -
                     ELSE IF ( s0 <= sm + ssii) THEN
                       rw = DBLE(ii)

                       ! si00 = s^# where F = 0 -s0
                       si00 = sm + ssii - s0
                     ELSE
                       rw = DBLE(iim)
                       wcase = -1
                       EXIT iiselect
                     END IF
                  END IF

                  ! delta r = r(s=si00) - r_i
                  dr = (r0-DBLE(ii)) + exp_term*(EXP(-beta*si00)-1.d0) - gamma*si00/beta

                  ! No crossing the wall
                  IF (dr<0.d0) THEN
                     wcase = -1
                     EXIT iiselect
                  END IF

                  ! check crossing at iim configuration: - 0 +
                  IF ( drds<=0.d0 .AND. (ssiim > 0.d0 .OR. ssiim < dstep) .AND. vm > 0.d0) THEN
                      sia = sm + ssiim - s0
                      ! delta r = r(s=sia) - r_i-1
                      dra = (r0-DBLE(iim)) + exp_term*(EXP(-beta*sia)-1.d0) - gamma*sia/beta
                      IF (dra < 0.d0) THEN
                         wcase = -1
                         EXIT iiselect
                      END IF
                  END IF

                  EXIT iiselect

              END DO iiselect

              ! 2.2 - iim direction (four velocity configurations at edge)
              iimselect: DO
                  IF (wcase == 1) EXIT iimselect

                  IF (ssiim <= 0.d0 .OR. ssiim >= dstep) THEN
                      ! 3a - configuration (4) paper + + + no solution
                      IF (um > 0.d0 .OR. vm > 0.d0) THEN
                        wcase = 0
                        EXIT iimselect
                      ! 3b - configuration (1) paper - - -
                      ELSE
                        rw = DBLE(iim)

                        ! si00 = s^n-1 - s0
                        si00  = sm + dstep -s0
                        wcase = -1
                      END IF
                  ELSE
                      IF (vm > 0.d0) THEN
                        ! 3c - configuration (3) paper  - 0 +
                        IF (s0 >= sm + ssiim) THEN
                           wcase = 0
                        ELSE
                           rw = DBLE(iim)

                           ! si00 = s^n-1 - s0
                           si00 = sm + ssiim - s0
                           wcase = -1
                        END IF
                      ELSE
                        ! 3d - configuration (2) paper + 0 -
                        rw = DBLE(iim)

                        ! si00 = s^n-1 - s0
                        si00 = sm + dstep - s0
                        wcase = -1
                      END IF
                  END IF

                  ! distance dr : r(si00) - r_i-1
                  dr = (r0-DBLE(iim)) + exp_term*(EXP(-beta*si00) - 1.d0) - gamma*si00/Beta

                  IF (dr > 0.d0) THEN
                     wcase = 0
                     EXIT iimselect
                  END IF

                  EXIT iimselect
              END DO iimselect

              ! 3 - find root (Newton method)
              IF (wcase/=0) THEN
                  ! Initial time and position
                  ss = s0
                  rs = r0

                  ! Initial derivative
                  drds = um - gamma*(ss-sm) - beta*(rs-DBLE(iim))

                  IF ((drds<=0.d0) .AND. (rw==DBLE(ii))) THEN
                      ! First guess the upper time
                      ss = sm + dstep
                      ! Initial value of r(s)
                      rs = r0 + exp_term*(EXP(-beta*(ss-s0))-1.d0) - gamma*(ss-s0)/beta
                      ! Recalculate value of drds
                      drds = um - gamma*(ss-sm) - beta*(rs-DBLE(iim))
                  ELSE IF ((drds>=0.d0) .AND. (rw==DBLE(iim))) THEN
                      ! First guess the lower time
                      ss = sm
                      ! Initial value of r(s)
                      rs = r0 + exp_term*(EXP(-beta*(ss-s0))-1.d0) - gamma*(ss-s0)/beta
                      ! Recalculate value of drds
                      drds = um - gamma*(ss-sm) - beta*(rs-DBLE(iim))
                  END IF

                  ! Function to find root
                  dr = rs-rw

                  rootfind: DO WHILE (rootloop<100)

                    IF (abs(dr) < errlim)  EXIT rootfind

                    ! Update value of ss
                    ss = ss - dr/drds

                    IF (ABS(ss-s0) > dstep) THEN
                        ss = UNDEF
                        EXIT rootfind
                    END IF

                    ! Update position
                    rs = r0 + exp_term*(EXP(-beta*(ss-s0))-1.d0) - gamma*(ss-s0)/beta

                    ! Update value of dr/ds at the new location
                    drds = um - gamma*(ss-sm) - beta*(rs-DBLE(iim))

                    ! New value of the
                    dr = rs-rw

                    ! loop root finding
                    rootloop = rootloop + 1

                  END DO rootfind

                  sw = ss

              END IF

            END IF

          END IF

          IF (sw == UNDEF) rw = -99

          RETURN

      END SUBROUTINE azer

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
          REAL(DP) :: ds  ! time step

          ! Time Variables
          REAL(DP) :: sm          ! s^n-1
          REAL(DP) :: s0          ! current time
          REAL(DP) :: ss          ! update time

          ! Parameters from the paper
          REAL(DP) :: alfa
          REAL(DP) :: beta
          REAL(DP) :: gamma

          REAL(DP) :: exp_term    ! terms that go with the exponential exp(-beta(s-s0))


#ifdef w_2dim
          IF (ijk .EQ. 3) THEN
             r1 = r0
             RETURN
          END IF
#endif

          ! Lower time step
          sm = DBLE(INT(ts))*tseas/dxyz

          ! Initial time
          s0 = tt/dxyz

          ! Updated value of s
          ss = s0 + ds

          ! ijk : considered direction (i=zonal, 2=meridional, 3=vertical)
          IF ( ijk .EQ. 1) THEN
              ii=ia
              im=ia-1; il=im
              IF( (il .EQ. 0) .AND. (iperio .EQ. 1) ) il = IMT
              uu = uflux(ia,ja,ka,nsm)
              um = uflux(il,ja,ka,nsm)
              vv = uflux(ia,ja,ka,nsp)
              vm = uflux(il,ja,ka,nsp)

          ELSEIF (ijk .EQ. 2) THEN
              ii=ja
              im=ja-1
              uu = vflux(ia,ja,ka,nsm)
              um = vflux(ia,ja-1,ka,nsm)
              vv = vflux(ia,ja,ka,nsp)
              vm = vflux(ia,ja-1,ka,nsp)

          ELSEIF (ijk .EQ. 3) THEN
              ii=ka
              im=ka-1
#if defined  w_explicit
              uu = wflux(ia,ja,ka  ,nsm)
              um = wflux(ia,ja,ka-1,nsm)
              vv = wflux(ia,ja,ka  ,nsp)
              vm = wflux(ia,ja,ka-1,nsp)
#else
              uu = wflux(ka  ,nsm)
              um = wflux(ka-1,nsm)
              vv = wflux(ka  ,nsp)
              vm = wflux(ka-1,nsp)
#endif

          END IF

          !Define alpha constant
          alfa = -(vv-vm-uu+um)/dstep

          ! Beta parameter
          beta = um - uu - alfa*sm

          ! Gamma parameter
          gamma = (um - vm)/dstep - alfa*im

          ! New position
          IF (alfa == 0.d0) THEN

             IF (beta == 0.d0) THEN
                r1 = r0 - (ss-s0)*(0.5*gamma*(ss+s0-2.*sm) - um)
             ELSE
                exp_term = (r0-DBLE(im)) + (gamma*(s0-sm-1.d0/beta)-um)/beta
                r1 = r0 + exp_term*(EXP(-beta*(ds))-1.d0) - gamma*ds/beta
             END IF

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
             CALL vertvel(ia,iam,ja,ka)
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

          ! Make sure that trajectory is inside ib,jb,kb box
          IF (x1 /= DBLE(IDINT(x1))) ib = IDINT(x1)+1
          IF (y1 /= DBLE(IDINT(y1))) jb = IDINT(y1)+1
          IF (z1 /= DBLE(IDINT(z1))) kb = IDINT(z1)+1

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

            IF( y1 == DBLE(JMTdom-1) .AND. jperio == 1) THEN
               x1 = DBLE(IMT+2) - x1
               ib = IDINT(x1) + 1
               jb = JMTdom
               ia=ib ; ja=jb
               x0 = x1; y0 = y1
            END IF

          END IF

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

END MODULE mod_pos

#endif
