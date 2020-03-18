MODULE mod_pos
    !!------------------------------------------------------------------------------
    !!
    !!       MODULE: mod_pos
    !!
    !!          Calculate the new positions of the trajectory
    !!          and the time it will take to cross a wall
    !!
    !!          Subroutines included:
    !!               - cross_time
    !!               - calc_pos
    !!               - update_traj
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

        ! ijk : considered direction (i=zonal, 2=meridional, 3=vertical)
        IF ( ijk .EQ. 1) THEN
            ii=ia
            im=ia-1
            IF( (im .EQ. 0) .AND. (iperio .EQ. 1) ) im = IMT
            uu = (intrpg*uflux(ia,ja,ka,nsp) + intrpr*uflux(ia,ja,ka,nsm))
            um = (intrpg*uflux(im,ja,ka,nsp) + intrpr*uflux(im,ja,ka,nsm))

        ELSEIF (ijk .EQ. 2) THEN
            ii=ja
            jm=ja-1
            uu = (intrpg*vflux(ia,ja,ka,nsp) + intrpr*vflux(ia,ja,ka,nsm))
            um = (intrpg*vflux(ia,jm,ka,nsp) + intrpr*vflux(ia,jm,ka,nsm))

        ELSEIF (ijk .EQ. 3) THEN
            ii=ka
#if defined  w_3dim
            uu = wflux(ia ,ja ,ka   ,nsm)
            um = wflux(ia ,ja ,ka-1 ,nsm)
#else
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

#ifdef w_3dim
        IF (ijk .EQ. 3) THEN
           r1 = r0
           RETURN
        END IF
#endif

        IF (ijk .EQ. 1) THEN
           ii = ia
           im = ia-1
           IF ( (im .EQ. 0) .AND. (iperio .EQ. 1)) im = IMT
           uu = (intrpg*uflux(ia,ja,ka,nsp) + intrpr*uflux(ia,ja,ka,nsm))
           um = (intrpg*uflux(im,ja,ka,nsp) + intrpr*uflux(im,ja,ka,nsm))

        ELSE IF (ijk .EQ. 2) THEN
           ii = ja
           jm = ja-1
           uu = (intrpg*vflux(ia,ja,ka,nsp) + intrpr*vflux(ia,ja,ka,nsm))
           um = (intrpg*vflux(ia,jm,ka,nsp) + intrpr*vflux(ia,jm,ka,nsm))

        ELSE IF (ijk .EQ. 3) THEN
            ii = ka
#if defined w_3dim
            uu = (intrpg * wflux(ia ,ja, ka  , nsp) + intrpr * wflux(ia, ja, ka  , nsm))
            um = (intrpg * wflux(ia, ja, ka-1, nsp) + intrpr * wflux(ia, ja, ka-1, nsm))
#else
            uu = (intrpg * wflux(ka  ,nsp) + intrpr * wflux(ka  ,nsm))
            um = (intrpg * wflux(ka-1,nsp) + intrpr * wflux(ka-1,nsm))
#endif

        END IF

        ! New position
        IF (um .NE. uu) THEN
           r1 = (r0+(-DBLE(ii-1) + um/(uu-um))) * &
                DEXP( (uu-um)*ds ) + DBLE(ii-1) - um/(uu-um)
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

        REAL(DP), INTENT(IN)  :: x0, y0, z0  ! current position (referenced to the box)
        REAL(DP), INTENT(OUT) :: x1, y1, z1  ! future position (referenced to the box)

        ! New position
        scrivi=.FALSE.

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

           IF (l_psi) CALL update_stream(ia, ja, ka, 1)

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
           IF (ds == dse) ib = ia + 1
           IF (ds == dsw) ib = iam
           IF (ds == dsu) kb = ka + 1
           IF (ds == dsd) kb = ka - 1

           IF (l_psi) CALL update_stream(iam, ja, ka, -1)

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

        ! Upward grid-cell exit
        ELSE IF (ds==dsu) THEN
           scrivi=.FALSE.
           CALL vertvel(ia,iam,ja,ka)
#if defined w_3dim
           uu = wflux(ia,ja,ka,nsm)
#else
           uu = (intrpg*wflux(ka,nsp) + intrpr*wflux(ka,nsm))
#endif

           IF (uu .GT. 0.d0) kb=ka+1

           z1=DBLE(ka)

           IF (kb==KM+1) THEN   ! prevent particles to cross the boundaries
              kb=KM
              z1=DBLE(KM)       ! put them exactly at the surface for hydro
           END IF

           CALL calc_pos(1,ia,ja,ka,x0,x1,ds) ! zonal position
           CALL calc_pos(2,ia,ja,ka,y0,y1,ds) ! meridional position

           ! In case of corners
           IF (ds == dse) ib = ia + 1
           IF (ds == dsw) ib = iam
           IF (ds == dsn) jb = ja + 1
           IF (ds == dss) jb = ja - 1

        ! Downward grid-cell exit
        ELSE IF (ds==dsd) THEN
           scrivi=.FALSE.
           CALL vertvel(ia, iam, ja, ka)
#if defined w_3dim
           uu = wflux(ia,ja,ka-1,nsm)
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

        ! Shortest time is the time-steping
        ELSE IF (ds==dsc .OR. ds==dsmin) THEN
           scrivi=.TRUE.

           IF (ds<1e-8) scrivi=.FALSE.

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
            IF (x1 <  0.d0) THEN
                x1 = x1 + DBLE(IMT)
            ELSE IF (x1 > DBLE(IMT)) THEN
                x1 = x1 - DBLE(IMT)
            END IF
        END IF

    END SUBROUTINE update_traj

END MODULE mod_pos
