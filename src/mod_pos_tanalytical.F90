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
    !!          Subroutines/functions included:
    !!               - cross_time
    !!               - apos
    !!               - aneg
    !!               - azer
    !!               - calc_pos
    !!               - update_traj
    !!
    !!               - update_trajdir (P)
    !!               - daw (P)
    !!               - erfexp (P)
    !!               - fun_erfc (P)
    !!               - fun_erf (P)
    !!               - gammp (P)
    !!               - gammq (P)
    !!               - gser (P)
    !!               - gcf (P)
    !!               - gammln (P)
    !!               - update_bounce (P)
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
    REAL(DP)                                   :: dzt1, dzt2, dzs
    REAL(DP)                                   :: f0, f1
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

              ! No vertical thickness correction
              f0 = 1.d0; f1 = 1.d0

          ELSEIF (ijk .EQ. 2) THEN
              ii=ja
              im=ja-1
              uu = vflux(ia,ja,ka,nsm)
              um = vflux(ia,ja-1,ka,nsm)
              vv = vflux(ia,ja,ka,nsp)
              vm = vflux(ia,ja-1,ka,nsp)

              ! No vertical thickness correction
              f0 = 1.d0; f1 = 1.d0

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

              ! Layer thickness at time step n-1
              dzt1 = dzt(ia,ja,ka,nsm)
              IF (dzt1 == 0.d0) STOP 'Layer thickness at time step n-1 is zero'

              ! Layer thickness at time step n
              dzt2 = dzt(ia,ja,ka,nsp)
              IF (dzt2 == 0.d0) STOP 'Layer thickness at time step n is zero'

              ! Layer thickness at time interpolated for "present"
              dzs= intrpr*dzt1 + intrpg*dzt2

              ! F0(dzt1/dzs) and F1(dzt2/dzs) definition
              f0 = dzt1/dzs;  f1 = dzt2/dzs
              IF (ABS(1/f0)<=EPS) STOP 'F0 too large dzt1 >>> dzs'
              IF (ABS(1/f1)<=EPS) STOP 'F1 too large dzt2 >>> dzs'

              ! Readjust the fluxes to the 'present' thickness
              uu = uu/f0; um = um/f0
              vv = vv/f1; vm = vm/f1

          END IF

          !Define alpha constant
          alfa = -(vv-vm-uu+um)

          ! Solutions depending on the value of alpha
          IF (alfa>0.0d0) THEN
            CALL apos(ii,im,uu,um,vv,vm,r0,rw,sm,s0,sw,f0,f1)
          ELSE IF (alfa<0.0d0) THEN
            CALL aneg(ii,im,uu,um,vv,vm,r0,rw,sm,s0,sw,f0,f1)
          ELSE
            CALL azer(ii,im,uu,um,vv,vm,r0,rw,sm,s0,sw,f0,f1)
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

      SUBROUTINE apos(ii,iim,uu,um,vv,vm,r0,rw,sm,s0,sw,f0,f1)
      ! ------------------------------------------------------------------
      !
      ! Purpose:
      ! Computes the crossing time for alpha > 0 case
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

          ! Thickness correction
          REAL(DP) :: f0, f1

          ! Parameters from the paper
          REAL(DP) :: alfa
          REAL(DP) :: beta
          REAL(DP) :: delta
          REAL(DP) :: gamma

          REAL(DP) :: bega_aldel     ! beta*gamma - alpha*delta
          REAL(DP) :: gamma_alfa     ! gamma/alpha

          ! Xi variable
          REAL(DP) :: xi0            ! Xi (s=s0)
          REAL(DP) :: xin            ! Xi (s=sn)
          REAL(DP) :: xia            ! Xi (s=sn)

          ! Other Variables
          REAL(DP) :: drds
          REAL(DP) :: const
          REAL(DP) :: dr, dra

          INTEGER  :: rootloop = 0
          INTEGER  :: wcase          ! wcase = 1 e/n/u wall // -1 w/s/d wall

          REAL(DP) :: drb, drc, xa, xb, xc

          ! Default value
          wcase = 1
          ! sw by default is UNDEF
          sw = UNDEF

          ! Alpha parameter
          alfa = -(vv-vm-uu+um)/dstep
          ! Beta parameter
          beta = um - uu - alfa*sm
          ! Gamma parameter
          gamma = (f0*um-f1*vm)/dstep - alfa*DBLE(iim)
          ! Delta parameter
          delta = -f0*um - DBLE(iim)*(um-uu) - gamma*sm

          ! beta*gamma - alpha*delta
          bega_aldel = vv*um - uu*vm

          ! Xi(s=s0)
          xi0 = (beta + alfa*s0)/SQRT(2.*alfa)

          ! If land points are at ii
          IF (bega_aldel==0.d0 .AND. (uu==0.d0 .AND. vv==0.d0)) THEN

              xin = -LOG(DBLE(ii)-r0)

              ! If the flux at r_i-1^n-1 is negative and r(s#)<r_i-1
              IF (xi0<0.d0 .AND. xi0*xi0>=xin) THEN
                  rw = iim

                  xin = -SQRT(xi0*xi0-xin)
                  sw  = 2.*(xin-xi0)/SQRT(2.*alfa) + s0

                  ! If the crossing happens for times longer than s^n
                  IF (sw > sm + dstep)  sw = UNDEF

              END IF

          ! If land points are at iim
          ELSEIF (bega_aldel==0.d0 .AND. (um==0.d0 .AND. vm==0.d0)) THEN

              xin = -LOG(r0-DBLE(iim))

              ! If the flux at r_i^n-1 is positive and ri<r(s#)
              IF (xi0<0.d0 .AND. xi0*xi0>=xin) THEN
                  rw = ii

                  xin = -SQRT(xi0*xi0-xin)
                  sw  = 2.*(xin-xi0)/SQRT(2.*alfa) + s0

                  ! If the crossing happens for times longer than s^n
                  IF (sw > sm + dstep)  sw = UNDEF

              END IF

          ! Other cases
          ELSE

              ! 1 - Initial configuration

              ! Define boundary times
              IF ( f0==1.d0 .AND. f1 == 1.d0) THEN

                  IF (uu==vv) THEN
                    !ssii  = SIGN(2.d0,uu)
                    ssii = uu/EPS
                  ELSE
                    ssii  =  uu/(uu-vv)         ! Fi^(n-1) / (alfa*ri+gamma)    [time unit]
                  END IF

                  IF (um==vm) THEN
                    !ssiim  = SIGN(2.d0,um)
                    ssiim = um/EPS
                  ELSE
                    ssiim =  um/(um-vm)         ! Fi-1^(n-1) / (alfa*r_i-1+gamma)  [time unit]
                  END IF

              ELSE

                  IF (uu+um*(f0-1.d0)+vm*(1.d0-f1)==vv) THEN
                      ssii = uu/EPS
                  ELSE
                      ssii = (uu+um*(f0-1.d0))/(uu-vv+um*(f0-1.d0)+vm*(1.d0-f1))  ! Fi^(n-1) / (alfa*ri+gamma)    [time unit]
                  END IF

                  IF (f0*um==f1*vm) THEN
                      ssiim = f0*um/EPS
                  ELSE
                      ssiim = f0*um/(f0*um-f1*vm)   ! Fi-1^(n-1) / (alfa*r_i-1+gamma)  [time unit]
                  END IF
              END IF

              ! gamma/alpha parameters
              gamma_alfa = - DBLE(iim) + ((f1*vm-f0*um)/(vv-vm-uu+um))

              ! Constant in front of the Dawson function according to the paper
              IF ( f0==1.d0 .AND. f1 == 1.d0) THEN
                  const = SQRT(2.d0/alfa)*(vv*um - uu*vm)/(vv-vm-uu+um)
              ELSE
                  const = SQRT(2.d0/alfa)*(f0*um*(vv-vm)+f1*vm*(um-uu))/(vv-vm-uu+um)
              END IF

              ! 2.1 - ii direction (four velocity configurations at edge)
              iiselect: DO
                  IF ((ssii <= 0.d0) .OR. (ssii>= 1.d0)) THEN
                      ! 2a - configuration (1) paper + + +
                      IF (uu > 0.d0 .OR. vv > 0.d0) THEN
                        rw = DBLE(ii)

                        ! xi(s^n)
                        xin = (vm-vv)/SQRT(2.d0*alfa)
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

                       ! xi(s^n)
                       xin = (vm-vv)/SQRT(2.d0*alfa)
                     ! 2d - configuration (2) paper + 0 -
                     ELSE IF ( s0 <= sm + ssii*dstep) THEN
                       rw = DBLE(ii)

                       ! xi(s^#) where F(s^#) is zero
                       xin = (beta + alfa*(ssii*dstep+sm))/SQRT(2.d0*alfa)
                     ELSE
                       rw = DBLE(iim)
                       wcase = -1
                       EXIT iiselect
                     END IF
                  END IF

                  ! Fluxes at xi_0
                  drds = SQRT(alfa/2.d0)*const - SQRT(2.d0*alfa)*xi0*(r0 + gamma_alfa)

                  ! delta r = r(xi=xin) - r_i
                  dr = (r0+gamma_alfa)*EXP(xi0**2 - xin**2) - gamma_alfa + &
                      const*(daw(xin) - EXP(xi0**2 - xin**2)*daw(xi0)) - DBLE(ii)

                  ! No crossing the wall
                  IF (dr<0.d0) THEN
                     wcase = -1
                     EXIT iiselect
                  END IF

                  ! check crossing at iim configuration: - 0 +
                  IF ( drds<=0.d0 .AND. (ssiim > 0.d0 .AND. ssiim < 1.d0) .AND. vm > 0.d0) THEN
                      xia = (beta + alfa*(sm + ssiim*dstep))/SQRT(2.d0*alfa)
                      ! delta r = r(xi=xia) - r_i-1
                      dra = (r0+gamma_alfa)*EXP(xi0**2 - xia**2) - gamma_alfa + &
                          const*(daw(xia) - EXP(xi0**2 - xia**2)*daw(xi0)) - DBLE(iim)

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

                  IF (ssiim <= 0.d0 .OR. ssiim >= 1.d0) THEN
                      ! 3a - configuration (4) paper + + + no solution
                      IF (um > 0.d0 .OR. vm > 0.d0) THEN
                        wcase = 0
                        EXIT iimselect
                      ! 3b - configuration (1) paper - - -
                      ELSE
                        rw = DBLE(iim)

                        ! xin = xi(sn)
                        xin  = (vm-vv)/SQRT(2.d0*alfa)
                        wcase = -1
                      END IF
                  ELSE
                      IF (vm > 0.d0) THEN
                        ! 3c - configuration (3) paper  - 0 +
                        IF (s0 >= sm + ssiim*dstep) THEN
                           wcase = 0
                        ELSE
                           rw = DBLE(iim)

                           ! xin = xi(s^#)
                           xin  = (beta + alfa*(sm + ssiim*dstep))/SQRT(2.d0*alfa)
                           wcase = -1
                        END IF
                      ELSE
                        ! 3d - configuration (2) paper + 0 -
                        rw = DBLE(iim)

                        ! xin = xi(s^n)
                        xin  = (vm-vv)/SQRT(2.d0*alfa)
                        wcase = -1
                      END IF
                  END IF

                  ! delta r = r(xi=xin) - r_i-i
                  dr = (r0+gamma_alfa)*EXP(xi0**2 - xin**2) - gamma_alfa + &
                      const*(daw(xin) - EXP(xi0**2 - xin**2)*daw(xi0)) - DBLE(iim)

                  IF (dr > 0.d0) THEN
                     wcase = 0
                     EXIT iimselect
                  END IF

                  EXIT iimselect
              END DO iimselect

              ! 3 - find root (Bisection method)
              IF (wcase/=0) THEN

                  xa = xi0
                  xb = xin

                  dra = r0 - rw
                  drb = (r0+gamma_alfa)*EXP(xi0**2 - xb**2) - gamma_alfa + &
                      const*(daw(xb) - EXP(xi0**2 - xb**2)*daw(xi0)) - rw

                  IF (dra*drb > 0.d0) STOP 'Impossible to find a root'

                  xc = 0.5*(xa+xb)

                  drc = (r0+gamma_alfa)*EXP(xi0**2 - xc**2) - gamma_alfa + &
                      const*(daw(xc) - EXP(xi0**2 - xc**2)*daw(xi0)) - rw

                  ! Restart rootloop
                  rootloop = 0

                  DO WHILE (ABS(drc) > errlim)

                      IF (dra*drc>0.d0) THEN
                        xa = xc
                        xc = 0.5*(xa+xb)
                      ELSE IF (dra*drc<0.d0) THEN
                        xb = xc
                        xc = 0.5*(xa+xb)
                      END IF

                      dra = (r0+gamma_alfa)*EXP(xi0**2 - xa**2) - gamma_alfa + &
                          const*(daw(xa) - EXP(xi0**2 - xa**2)*daw(xi0)) - rw

                      drb = (r0+gamma_alfa)*EXP(xi0**2 - xb**2) - gamma_alfa + &
                          const*(daw(xb) - EXP(xi0**2 - xb**2)*daw(xi0)) - rw

                      drc = (r0+gamma_alfa)*EXP(xi0**2 - xc**2) - gamma_alfa + &
                          const*(daw(xc) - EXP(xi0**2 - xc**2)*daw(xi0)) - rw

                      rootloop = rootloop + 1

                      IF (rootloop>100) EXIT

                   END DO

                   sw = (SQRT(2.d0*alfa)*xc - beta)/alfa

              END IF

          END IF

          IF (sw == UNDEF) rw = -99

          RETURN

      END SUBROUTINE apos

      SUBROUTINE aneg(ii,iim,uu,um,vv,vm,r0,rw,sm,s0,sw,f0,f1)
      ! ------------------------------------------------------------------
      !
      ! Purpose:
      ! Computes the crossing time for alpha < 0 case
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

          ! Thickness correction
          REAL(DP) :: f0, f1

          ! Parameters from the paper
          REAL(DP) :: alfa
          REAL(DP) :: beta
          REAL(DP) :: delta
          REAL(DP) :: gamma

          REAL(DP) :: bega_aldel     ! beta*gamma - alpha*delta
          REAL(DP) :: gamma_alfa     ! gamma/alpha

          ! Xi variable
          REAL(DP) :: xi0            ! Xi (s=s0)
          REAL(DP) :: xin            ! Xi (s=sn)
          REAL(DP) :: xia            ! Xi (s=sn)

          ! Other Variables
          REAL(DP) :: drds
          REAL(DP) :: const
          REAL(DP) :: dr, dra
          REAL(DP) :: xilim=3.0d0

          INTEGER  :: rootloop = 0
          INTEGER  :: wcase         ! wcase = 1 e/n/u wall // -1 w/s/d wall

          REAL(DP) :: drb, drc, xa, xb, xc, erf0

          ! Default value of wcase
          wcase = 1
          ! sw by default is UNDEF
          sw = UNDEF

          ! Alpha parameter
          alfa = -(vv-vm-uu+um)/dstep
          ! Beta parameter
          beta = um - uu - alfa*sm
          ! Gamma parameter
          gamma = (f0*um-f1*vm)/dstep - alfa*DBLE(iim)
          ! Delta parameter
          delta = -f0*um - DBLE(iim)*(um-uu) - gamma*sm

          ! beta*gamma - alpha*delta
          bega_aldel = vv*um - uu*vm

          ! Xi(s=s0)
          xi0 = (beta + alfa*s0)/SQRT(-2.d0*alfa)

          ! If land points are at ii
          IF (bega_aldel==0.d0 .AND. (uu==0.d0 .AND. vv==0.d0)) THEN

            rw  = iim

            xin = -LOG(DBLE(ii)-r0)
            xin = SQRT(xi0*xi0+xin)
            sw  = 2.*(xin+xi0)/SQRT(-2.*alfa) + s0

            ! If the crossing happens for times longer than s^n
            IF (sw > sm + dstep)  sw = UNDEF

          ! If land points are at iim
          ELSEIF (bega_aldel==0.d0 .AND. (um==0.d0 .AND. vm==0.d0)) THEN

            rw = ii

            xin = -LOG(r0-DBLE(iim))
            xin = SQRT(xi0*xi0+xin)
            sw  = 2.*(xin+xi0)/SQRT(-2.*alfa) + s0

            ! If the crossing happens for times longer than s^n
            IF (sw > sm + dstep)  sw = UNDEF

          ! Other cases
          ELSE

            ! 1 - Initial configuration

            ! Define boundary times
            IF ( f0==1.d0 .AND. f1 == 1.d0) THEN

                IF (uu==vv) THEN
                  !ssii  = SIGN(2.d0,uu)
                  ssii = uu/EPS
                ELSE
                  ssii  =  uu/(uu-vv)         ! Fi^(n-1) / (alfa*ri+gamma)    [time unit]
                END IF

                IF (um==vm) THEN
                  !ssiim  = SIGN(2.d0,um)
                  ssiim = um/EPS
                ELSE
                  ssiim =  um/(um-vm)         ! Fi-1^(n-1) / (alfa*r_i-1+gamma)  [time unit]
                END IF

            ELSE

                IF (uu+um*(f0-1.d0)+vm*(1.d0-f1)==vv) THEN
                    ssii = uu/EPS
                ELSE
                    ssii = (uu+um*(f0-1.d0))/(uu-vv+um*(f0-1.d0)+vm*(1.d0-f1))  ! Fi^(n-1) / (alfa*ri+gamma)    [time unit]
                END IF

                IF (f0*um==f1*vm) THEN
                    ssiim = f0*um/EPS
                ELSE
                    ssiim = f0*um/(f0*um-f1*vm)   ! Fi-1^(n-1) / (alfa*r_i-1+gamma)  [time unit]
                END IF
            END IF

            ! gamma/alpha parameters
            gamma_alfa = - DBLE(iim) + ((f1*vm-f0*um)/(vv-vm-uu+um))

            ! Constant in front of the Error function according to the paper
            IF ( f0==1.d0 .AND. f1 == 1.d0) THEN
                const = SQRT(PI/(-2.d0*alfa))*(vv*um - uu*vm)/(vv-vm-uu+um)
            ELSE
                const = SQRT(PI/(-2.d0*alfa))*(f0*um*(vv-vm)+f1*vm*(um-uu))/(vv-vm-uu+um)
            END IF

            ! Erf at xi0
            IF (xi0.GT.xilim) THEN
                erf0 = fun_erfc(xi0)     ! Complementary error function
            ELSE IF (xi0.LT.-xilim) THEN
                erf0 = -fun_erfc(-xi0)   ! Complementary error function
            ELSE
                erf0 = -fun_erf(xi0)     ! Error function
            END IF

            ! 2.1 - ii direction (four velocity configurations at edge)
            iiselect: DO
                IF ((ssii <= 0.d0) .OR. (ssii>= 1.d0)) THEN
                    ! 2a - configuration (1) paper + + +
                    IF (uu > 0.d0 .OR. vv > 0.d0) THEN

                      rw = DBLE(ii)

                      ! xi(s^n)
                      xin = (vm-vv)/SQRT(-2.d0*alfa)
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

                     ! xi(s^n)
                     xin = (vm-vv)/SQRT(-2.d0*alfa)

                   ! 2d - configuration (2) paper + 0 -
                   ELSE IF ( s0 <= sm + ssii*dstep) THEN
                     rw = DBLE(ii)

                     ! xi(s^#) where F(s^#) is zero
                     xin = (beta + alfa*(ssii*dstep+sm))/SQRT(-2.d0*alfa)

                   ELSE
                     rw = DBLE(iim)
                     wcase = -1

                   END IF
                END IF

                ! Fluxes at xi_0
                drds = const*SQRT(-2.d0*alfa/PI) - SQRT(-2.d0*alfa)*xi0*(r0 + gamma_alfa)

                ! delta r = r(xi=xin) - r_i
                dr = (r0+gamma_alfa)*EXP(xin**2 - xi0**2) + erfexp(const,erf0,xi0,xin) - gamma_alfa - DBLE(ii)

                ! No crossing the wall
                IF (dr<0.d0) THEN
                   wcase = -1
                   EXIT iiselect

                END IF

                ! check crossing at iim configuration: - 0 +
                IF ( drds<=0.d0 .AND. (ssiim > 0.d0 .AND. ssiim < 1.d0) .AND. vm > 0.d0) THEN
                    xia = (beta + alfa*(sm + ssiim*dstep))/SQRT(-2.d0*alfa)
                    ! delta r = r(xi=xia) - r_i-1
                    dra = (r0+gamma_alfa)*EXP(xia**2 - xi0**2) + erfexp(const,erf0,xi0,xia) - gamma_alfa - DBLE(iim)

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

                IF (ssiim <= 0.d0 .OR. ssiim >= 1.d0) THEN
                    ! 3a - configuration (4) paper + + + no solution
                    IF (um > 0.d0 .OR. vm > 0.d0) THEN
                      wcase = 0
                      EXIT iimselect

                    ! 3b - configuration (1) paper - - -
                    ELSE
                      rw = DBLE(iim)

                      ! xin = xi(sn)
                      xin  = (vm-vv)/SQRT(-2.d0*alfa)
                      wcase = -1

                    END IF
                ELSE
                    IF (vm > 0.d0) THEN
                      ! 3c - configuration (3) paper  - 0 +
                      IF (s0 >= sm + ssiim*dstep) THEN
                         wcase = 0

                      ELSE
                         rw = DBLE(iim)

                         ! xin = xi(s^#)
                         xin  = (beta + alfa*(sm + ssiim*dstep))/SQRT(-2.d0*alfa)
                         wcase = -1

                      END IF
                    ELSE
                      ! 3d - configuration (2) paper + 0 -
                      rw = DBLE(iim)

                      ! xin = xi(s^n)
                      xin  = (vm-vv)/SQRT(-2.d0*alfa)
                      wcase = -1

                    END IF
                END IF

                ! delta r = r(xi=xin) - r_i-i
                dr = (r0+gamma_alfa)*EXP(xin**2 - xi0**2) + erfexp(const,erf0,xi0,xin) - gamma_alfa - DBLE(iim)

                IF (dr > 0.d0) THEN
                   wcase = 0

                   EXIT iimselect

                END IF

                EXIT iimselect
            END DO iimselect

            ! 3 - find root (Bisection method)
            IF (wcase/=0) THEN

                xa = xi0
                xb = xin

                dra = r0 - rw
                drb = (r0+gamma_alfa)*EXP(xb**2 - xi0**2) + erfexp(const,erf0,xi0,xb) - gamma_alfa - rw

                IF (dra*drb > 0.d0) STOP 'a<0 Impossible to find a root'

                xc = 0.5*(xa+xb)

                drc = (r0+gamma_alfa)*EXP(xc**2 - xi0**2) + erfexp(const,erf0,xi0,xc) - gamma_alfa - rw

                rootloop = 0

                DO WHILE (ABS(drc) > errlim)

                    IF (dra*drc>0.d0) THEN
                      xa = xc
                      xc = 0.5*(xa+xb)
                    ELSE IF (dra*drc<0.d0) THEN
                      xb = xc
                      xc = 0.5*(xa+xb)
                    END IF

                    dra = (r0+gamma_alfa)*EXP(xa**2 - xi0**2) + erfexp(const,erf0,xi0,xa) - gamma_alfa - rw
                    drb = (r0+gamma_alfa)*EXP(xb**2 - xi0**2) + erfexp(const,erf0,xi0,xb) - gamma_alfa - rw
                    drc = (r0+gamma_alfa)*EXP(xc**2 - xi0**2) + erfexp(const,erf0,xi0,xc) - gamma_alfa - rw

                    rootloop = rootloop + 1

                    IF (rootloop>100) EXIT

                 END DO

                 sw = (SQRT(-2.d0*alfa)*xc - beta)/alfa

            END IF

        END IF

        IF (sw == UNDEF) rw = -99

        RETURN

      END SUBROUTINE aneg

      SUBROUTINE azer(ii,iim,uu,um,vv,vm,r0,rw,sm,s0,sw,f0,f1)
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

          ! Thickness correction
          REAL(DP) :: f0, f1

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
          REAL(DP) :: dr, dra

          INTEGER  :: rootloop = 0
          INTEGER  :: wcase       ! wcase = 1 e/n/u wall // -1 w/s/d wall

          REAL(DP) :: drb, drc, xa, xb, xc

          ! Default value of wcase
          wcase = 1

          ! sw by default is UNDEF
          sw = UNDEF

          ! Beta parameter
          beta = um - uu
          ! Gamma parameter
          gamma = (f0*um - f1*vm)/dstep

          ! Transport across the grid constant
          IF (beta == 0.0d0) THEN

            ! Fluxes constant time and space
            IF (gamma == 0.0d0) THEN

                ! Delta parameter
                delta = -f0*um

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

                del_ga = -(f0*um)/gamma - sm

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
                xi = 1.0d0+(DBLE(ii)-r0)*beta/(uu - (1-f0)*um)
                IF (xi>0.0d0) THEN
                    IF ((xi < EPS) .OR. (ABS(beta) < EPS)) STOP 'Unable to find a solution'
                    rw = DBLE(ii)
                    sw = s0 + 1.0d0*LOG(xi)/beta

                    IF (sw < EPS) STOP
                    IF (sw <= 0.d0) sw = UNDEF
                END IF
              END IF

              ! Negative u_i-1 flux
              IF (um < 0.d0) THEN
                xi = 1.0d0-(r0-DBLE(iim))*beta/(f0*um)
                IF (xi>0.0d0) THEN
                    IF ((xi < EPS) .OR. (ABS(beta) < EPS)) STOP 'Unable to find a solution'
                    rw = DBLE(iim)
                    sw = s0 + 1.0d0*LOG(xi)/beta

                    IF (sw < EPS) STOP 'Very small time step'
                    IF (sw <= 0.d0) sw = UNDEF
                END IF
              END IF

            ! Fluxes not constant in time
            ELSE

              IF (ABS(beta)<EPS) STOP 'Beta equal to zero'

              ! 1 - Initial configuration

              ! Define boundary times
              ssii  =  (f0*uu-beta)/gamma      ! Fi^(n-1) / gamma    [time unit]
              ssiim =  (f0*um)/gamma           ! Fi-1^(n-1) / gamma  [time unit]

              ! Terms with exp(-beta(s-s0))
              exp_term = (r0-DBLE(iim)) + (gamma*(s0-sm-1.d0/beta)-f0*um)/beta

              ! drds at r0, s0
              drds = f0*um - gamma*(s0-sm) - beta*(r0-DBLE(iim))

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
                  IF ( drds<=0.d0 .AND. (ssiim > 0.d0 .AND. ssiim < dstep) .AND. vm > 0.d0) THEN
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
                           EXIT iimselect
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

              ! 3 - find root (Bisection method)
              IF (wcase/=0) THEN

                  xa = s0
                  xb = si00 + s0

                  dra = r0 - rw
                  drb = r0 + exp_term*(EXP(-beta*(xb-s0))-1.d0) - gamma*(xb-s0)/beta - rw

                  IF (dra*drb > 0.d0) STOP 'Impossible to find a root'

                  xc = 0.5*(xa+xb)

                  drc = r0 + exp_term*(EXP(-beta*(xc-s0))-1.d0) - gamma*(xc-s0)/beta - rw

                  ! Restart rootloop
                  rootloop = 0

                  DO WHILE (ABS(drc) > errlim)

                      IF (dra*drc>0.d0) THEN
                        xa = xc
                        xc = 0.5*(xa+xb)
                      ELSE IF (dra*drc<0.d0) THEN
                        xb = xc
                        xc = 0.5*(xa+xb)
                      END IF

                      dra = r0 + exp_term*(EXP(-beta*(xa-s0))-1.d0) - gamma*(xa-s0)/beta - rw

                      drb = r0 + exp_term*(EXP(-beta*(xb-s0))-1.d0) - gamma*(xb-s0)/beta - rw

                      drc = r0 + exp_term*(EXP(-beta*(xc-s0))-1.d0) - gamma*(xc-s0)/beta - rw

                      rootloop = rootloop + 1

                      IF (rootloop>100) EXIT

                   END DO

                   sw = xc

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
          REAL(DP) :: xi0, xis     ! xi values

          ! Thickness correction
          REAL(DP) :: f0, f1

          ! Parameters from the paper
          REAL(DP) :: alfa
          REAL(DP) :: beta
          REAL(DP) :: gamma
          REAL(DP) :: gamma_alfa

          REAL(DP) :: exp_term    ! terms that go with the exponential exp(-beta(s-s0))
          REAL(DP) :: daw_term    ! terms that go with the Dawson function
          REAL(DP) :: erf_term    ! terms that go with the Error function

          ! Other variables
          REAL(DP) :: xilim = 3.d0
          REAL(DP) :: erf0

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

              ! No vertical thickness correction
              f0 = 1.d0; f1 = 1.d0;

          ELSEIF (ijk .EQ. 2) THEN
              ii=ja
              im=ja-1
              uu = vflux(ia,ja,ka,nsm)
              um = vflux(ia,ja-1,ka,nsm)
              vv = vflux(ia,ja,ka,nsp)
              vm = vflux(ia,ja-1,ka,nsp)

              ! No vertical thickness correction
              f0 = 1.d0; f1 = 1.d0;

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

              ! Layer thickness at time step n-1
              dzt1 = dzt(ia,ja,ka,nsm)
              IF (dzt1 == 0.d0) STOP 'Layer thickness at time step n-1 is zero'

              ! Layer thickness at time step n
              dzt2 = dzt(ia,ja,ka,nsp)
              IF (dzt2 == 0.d0) STOP 'Layer thickness at time step n is zero'

              ! Layer thickness at time interpolated for "present"
              dzs= intrpr*dzt1 + intrpg*dzt2

              ! F0(dzt1/dzs) and F1(dzt2/dzs) definition
              f0 = dzt1/dzs;  f1 = dzt2/dzs
              IF (ABS(1/f0)<=EPS) STOP 'F0 too large dzt1 >>> dzs'
              IF (ABS(1/f1)<=EPS) STOP 'F1 too large dzt2 >>> dzs'

              ! Readjust the fluxes to the 'present' thickness
              uu = uu/f0; um = um/f0
              vv = vv/f1; vm = vm/f1

          END IF

          !Define alpha constant
          alfa = -(vv-vm-uu+um)/dstep

          ! Beta parameter
          beta = um - uu - alfa*sm

          ! Gamma parameter
          gamma = (f0*um - f1*vm)/dstep - alfa*im

          ! New position
          IF (alfa > 0.d0) THEN

             ! Xi values
             xi0 = (beta + alfa*s0)/SQRT(2.d0*alfa)
             xis = (beta + alfa*ss)/SQRT(2.d0*alfa)

             IF (f0 == 1.d0 .AND. f1 == 1.d0) THEN
                  ! gamma/alpha parameters
                  gamma_alfa = - DBLE(im) + ((vm-um)/(vv-vm-uu+um))

                  ! Constant in front of the Dawson function according to the paper
                  daw_term = SQRT(2.d0/alfa)*(vv*um - uu*vm)/(vv-vm-uu+um)
             ELSE
                  ! gamma/alpha parameters
                  gamma_alfa = - DBLE(im) + ((f1*vm-f0*um)/(vv-vm-uu+um))

                  ! Constant in front of the Dawson function according to the paper
                  daw_term = SQRT(2.d0/alfa)*(f0*um*(vv-vm)+f1*vm*(um-uu))/(vv-vm-uu+um)
             END IF

             r1 = (r0+gamma_alfa)*EXP(xi0**2 - xis**2) - gamma_alfa + &
                 daw_term*(daw(xis) - EXP(xi0**2 - xis**2)*daw(xi0))

          ELSE IF (alfa < 0.d0) THEN

             ! Xi values
             xi0 = (beta + alfa*s0)/SQRT(-2.d0*alfa)
             xis = (beta + alfa*ss)/SQRT(-2.d0*alfa)

             ! gamma/alpha parameters
             gamma_alfa = - DBLE(im) + ((vm-um)/(vv-vm-uu+um))

             IF (f0 == 1.d0 .AND. f1 == 1.d0) THEN
                  ! gamma/alpha parameters
                  gamma_alfa = - DBLE(im) + ((vm-um)/(vv-vm-uu+um))

                  ! Constant in front of the Error function according to the paper
                  erf_term = SQRT(PI/(-2.d0*alfa))*(vv*um - uu*vm)/(vv-vm-uu+um)
             ELSE
                  ! gamma/alpha parameters
                  gamma_alfa = - DBLE(im) + ((f1*vm-f0*um)/(vv-vm-uu+um))

                  ! Constant in front of the Error function according to the paper
                  erf_term = SQRT(PI/(-2.d0*alfa))*(f0*um*(vv-vm)+f1*vm*(um-uu))/(vv-vm-uu+um)
             END IF

             ! Erf at xi0
             IF (xi0.GT.xilim) THEN
                 erf0 = fun_erfc(xi0)   ! complementary error function
             ELSE IF (xi0.LT.-xilim) THEN
                 erf0 = -fun_erfc(-xi0) ! complementary error function
             ELSE
                 erf0 = -fun_erf(xi0)   ! error function
             END IF

             r1 = (r0+gamma_alfa)*EXP(xis**2 - xi0**2) + erfexp(erf_term,erf0,xi0,xis) - gamma_alfa

          ELSE IF (alfa == 0.d0) THEN

             IF (beta == 0.d0) THEN
                r1 = r0 - (ss-s0)*(0.5*gamma*(ss+s0-2.*sm) - f0*um)
             ELSE
                exp_term = (r0-DBLE(im)) + (gamma*(s0-sm-1.d0/beta)-f0*um)/beta
                r1 = r0 + exp_term*(EXP(-beta*(ds))-1.d0) - gamma*ds/beta
             END IF

          END IF

          IF (r1-DBLE(ii) >0.0 .AND. r1-DBLE(ii) <1.d-7)  r1 = DBLE(ii )- 1.d-7
          IF (DBLE(im)-r1 >0.0 .AND. DBLE(im)-r1 <1.d-7)  r1 = DBLE(im) + 1.d-7

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

      FUNCTION daw(x)
      ! --------------------------------------------------
      !
      ! Purpose:
      ! This function program evaluates Dawson's integral,
      !
      !   F(x) = exp(-x^2) INTEGRAL (exp(-t^2), 0, x)
      !
      ! --------------------------------------------------

            USE mod_param

            IMPLICIT NONE

            INTEGER,   PARAMETER ::  NMAX=6
            REAL(DP),  PARAMETER ::  H=0.4d0,A1=2.d0/3.d0,A2=0.4d0,A3=2.d0/7.d0

            REAL(DP)             :: daw,x,dd1,dd2,e1,e2,summ,x2,xp,xx,pisqin
            REAL(DP), SAVE       :: c(NMAX)

            INTEGER              :: i,n0
            INTEGER, SAVE        :: init=0

            pisqin = 1.d0/DSQRT(PI)

            IF (init.EQ.0) THEN
                init=1
                DO i = 1,NMAX
                  c(i) = DEXP(-((2.d0*DBLE(i)-1.d0)*H)**2)
                END DO
            END IF

            !  Use series expansion.
            IF(DABS(x).LT.0.2d0) THEN
                x2 = x**2

                daw = x*(1.d0-A1*x2*(1.d0-A2*x2*(1.d0-A3*x2)))

            !  Use sampling theorem representation.
            ELSE
                xx = DABS(x)
                n0 = 2*NINT(0.5d0*xx/H)
                xp = xx-DBLE(n0)*H
                e1 = DEXP(2.d0*xp*H)
                e2 = e1**2
                dd1= DBLE(n0+1)
                dd2= dd1-2.d0
                summ= 0.d0

                DO i=1,NMAX
                  summ = summ + c(i)*(e1/dd1+1.d0/(dd2*e1))
                  dd1 = dd1+2.d0
                  dd2 = dd2-2.d0
                  e1  = e2*e1
                END DO

                daw = pisqin*DSIGN(DEXP(-xp**2),x)*summ
            END IF

            RETURN

      END FUNCTION daw

      FUNCTION erfexp (consterr,erf0,xi0,xi)
      ! --------------------------------------------------
      !
      ! Purpose:
      ! This function program evaluates the terms with the
      ! error function:
      !
      !   F(x) = consterr*exp(xi**2)*(erf(xi)-erf(xi0))
      !
      ! --------------------------------------------------

          USE mod_param

          IMPLICIT NONE

          REAL(DP),  PARAMETER :: xilim=3.0d0
          REAL(DP)             :: erfexp,consterr
          REAL(DP)             :: erf0,xi0,xi,erfv,hh0,hh
          INTEGER              :: i


          IF (DABS(xi).GT.25.d0) then
              hh0 = 1.d0
              hh  = 1.d0
              DO i = 1,10
                hh0 = -0.5d0*hh0*DBLE(i+i-1)/(xi0*xi0)
                hh  = hh+hh0
              END DO

              erfv = DEXP(xi**2-xi0**2)*hh/xi0
              hh0  = 1.d0
              hh   = 1.d0
              DO i = 1,10
                hh0 = -0.5d0*hh0*DBLE(i+i-1)/(xi*xi)
                hh  = hh+hh0
              END DO
              erfv = 1.d0/DSQRT(pi)*((hh/xi)-erfv)
          ELSE

              IF (xi0.GT.xilim) THEN
                erfv  = fun_erfc(xi)    ! complementary error function
              ELSE IF (xi0.LT.-xilim) THEN
                erfv  = -fun_erfc(-xi)  ! complementary error function
              ELSE
                erfv  = -fun_erf(xi)    ! error function
              END IF

              erfv = DEXP(xi**2)*(erfv-erf0)
          END IF

          erfexp = consterr*erfv

          RETURN

      END FUNCTION erfexp

      FUNCTION fun_erfc(x)
      ! --------------------------------------------------
      !
      !   Purpose
      !   Returns the complementary error function erfc(x)
      !
      ! --------------------------------------------------

            IMPLICIT NONE

            REAL(DP)  :: fun_erfc,x

            IF (x.LT.0.d0) THEN
              fun_erfc = 1.d0 + gammp(0.5d0,x**2)
            ELSE
              fun_erfc = gammq(0.5d0,x**2)
            END IF

            RETURN

      END FUNCTION fun_erfc

      FUNCTION fun_erf(x)
      ! --------------------------------------------------
      !
      !   Purpose
      !   Returns the error function erf(x)
      !
      ! --------------------------------------------------

            IMPLICIT NONE

            REAL(DP) ::  fun_erf,x

            IF (x<0.d0) THEN
              fun_erf = -gammp(0.5d0,x**2)
            ELSE
              fun_erf =  gammp(0.5d0,x**2)
            END IF

            RETURN

      END FUNCTION fun_erf

      FUNCTION gammp(a,x)
      ! --------------------------------------------------
      !
      !  Purpose:
      !  Returns the normalised incomplete gamma function
      !      P(a,x) = gamma(a,x)/Gamma(x)
      !
      ! --------------------------------------------------

            IMPLICIT NONE

            REAL(DP) :: a,gammp,x,gammcf,gamser

            IF(x<0.d0 .OR. a<=0.d0) STOP 'Bad arguments in P(a,x)'

            ! Use the series representation.
            IF (x < a+1.d0) THEN
                CALL gser(gamser,a,x)
                gammp = gamser
            ! Use the continued fraction representation
            ELSE
                CALL gcf(gammcf,a,x)
                gammp = 1.d0-gammcf ! and take its complement.
            END IF

            RETURN

      END FUNCTION gammp

      FUNCTION gammq(a,x)
      ! --------------------------------------------------
      !
      !  Purpose:
      !  Returns the normalised incomplete gamma function
      !      Q(a,x) = Gamma(a,x)/Gamma(x)
      !
      ! --------------------------------------------------

            IMPLICIT NONE

            REAL(DP) :: a,gammq,x,gammcf,gamser

            IF (x<0.d0 .OR. a<=0.d0) STOP 'Bad arguments in gammp'

            ! Use the series representation
            IF (x < a+1.d0) THEN
              CALL gser(gamser,a,x)
              gammq = 1.d0 - gamser ! and take its complement.
            ! Use the continued fraction representation.
            ELSE
              CALL gcf(gammcf,a,x)
              gammq=gammcf
            END IF

            RETURN

      END FUNCTION gammq

      SUBROUTINE gser(gamser,a,x)
      ! --------------------------------------------------
      !
      !  Purpose:
      !  Returns the normalised incomplete gamma function      !
      !  Computed from series expansion
      !
      ! --------------------------------------------------

            USE mod_param

            IMPLICIT NONE

            REAL(DP)           ::  a,gamser,gln,x,ap,del,summ

            INTEGER, PARAMETER ::  ITMAX=1000
            INTEGER            ::  n

            ! Ln of the gamma function
            gln = gammln(a)

            IF (x.LE.0.d0) THEN
              IF (x.LT.0.d0) PRINT*,'Warning GSER: x < 0'
              gamser=0.d0
              RETURN
            END IF

            ap  = a
            summ = 1.d0/a
            del = summ

            loopconverge: DO n = 1,ITMAX
              ap  = ap+1.d0
              del = del*x/ap
              summ = summ+del
              IF (DABS(del).LT.DABS(summ)*EPS) CYCLE loopconverge

              IF (n == ITMAX) THEN
                  STOP 'Error in gser, a too large, ITMAX too small'
              END IF
            END DO loopconverge

            gamser = summ*DEXP(-x+a*DLOG(x)-gln)

            RETURN

      END SUBROUTINE gser

      SUBROUTINE gcf(gammcf,a,x)
      ! --------------------------------------------------
      !
      !  Purpose:
      !  Returns the normalised incomplete gamma function      !
      !  Computed from continued fraction representation
      !
      ! --------------------------------------------------

            USE mod_param

            IMPLICIT NONE

            INTEGER,   PARAMETER ::  ITMAX=1000
            REAL(DP),  PARAMETER ::  FPMIN=1.d-30
            REAL(DP)             ::  a,gammcf,gln,x,an,b,c,d,del,h

            INTEGER              ::  i

            gln = gammln(a)  ! Ln(gamma(a))
            b = x+1.d0-a     ! Set up for evaluating continued fraction by modied
            c = 1.d0/FPMIN   !Lentz's method (x5.2) with b0 = 0.
            d = 1.d0/b
            h = d

            loopconverge: DO i = 1, ITMAX  ! Iterate to convergence.
              an = -DBLE(i)*(DBLE(i)-a)
              b  = b+2.d0
              d  = an*d+b
              IF(DABS(d)<FPMIN) d=FPMIN
              c  = b+an/c
              IF(DABS(c)<FPMIN) c=FPMIN
              d  = 1.d0/d
              del= d*c
              h  = h*del
              IF (DABS(del-1.d0)<EPS) CYCLE loopconverge

              IF (i == ITMAX) THEN
                PRINT *, 'ERROR in gcf'
                PRINT *, 'a too large, ITMAX too small in gcf',DABS(del-1.d0),EPS,del
                PRINT *, 'cd =',an
                STOP
              END IF
            END DO loopconverge

            gammcf = DEXP(-x+a*DLOG(x)-gln)*h  ! Put factors in front.

            RETURN

      END SUBROUTINE gcf

      FUNCTION gammln(xx)
      ! --------------------------------------------------
      !
      ! Purpose:
      ! This function calculates the value of Ln[gamma(xx)]
      !
      ! --------------------------------------------------

            IMPLICIT NONE

            REAL(DP)  :: gammln,xx,ser,stp,tmp,x,y,cof(6)
            INTEGER   :: j

            ! Parameters
            stp    = 2.5066282746310005d0
            cof(:) = (/76.18009172947146d0,-86.50532032941677d0, &
                   24.01409824083091d0,-1.231739572450155d0,.1208650973866179d-2, &
                    -.5395239384953d-5/)

            x   = xx
            y   = x
            tmp = x + 5.5d0
            tmp = (x + 0.5d0)*DLOG(tmp)-tmp
            ser = 1.000000000190015d0

            DO j = 1,6
              y   = y + 1.d0
              ser = ser + cof(j)/y
            END DO

            IF (x<1.d-10) STOP 'Function gammln: x is too small'

            gammln = tmp + DLOG(stp*ser/x)

            RETURN

      END FUNCTION gammln

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
