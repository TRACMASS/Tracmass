MODULE mod_stream
    !!------------------------------------------------------------------------------
    !!
    !!       MODULE: mod_stream
    !!
    !!       This module updates the fluxes and computes different streamfunctions
    !!
    !!       Subroutines included in this modules:
    !!              - compute_stream
    !!              - init_stream
    !!              - update_fluxes
    !!              - compute_fluxes
    !!
    !!------------------------------------------------------------------------------

    USE mod_precdef
    USE mod_psi
    USE mod_grid
    USE mod_seed
    USE mod_write
    USE mod_postprocessvars
    USE mod_tracers

    IMPLICIT NONE

    INTEGER :: ilvar1, ilvar2, ilvar3, ilvar4, irow, intraj

    CONTAINS

      SUBROUTINE compute_stream()
      ! --------------------------------------------------
      !
      ! Purpose:
      ! Compute streamfunctions
      !
      ! --------------------------------------------------

        PRINT*, ''
        PRINT*, '- Computing streamfunctions'

        ! If offline calculation: Compute fluxes from out files
        IF (l_offline) CALL compute_fluxes()

        ! Compute and save streamfunction
        CALL open_outstream('xy')
        CALL open_outstream('xz')
        CALL open_outstream('yz')

        IF (l_tracers) CALL open_outstream('xr')
        IF (l_tracers) CALL open_outstream('yr')
        IF (l_tracers) CALL open_outstream('rr')

        DO ilvar1 = 1, 21

            psi_xy(:,:) = 0.; psi_xz(:,:) = 0.; psi_yz(:,:) = 0.
            IF (l_tracers) THEN
               psi_xr(:,:,:) = 0.
               psi_yr(:,:,:) = 0.
               psi_rr(:,:)   = 0.
            END IF

            ! For online calculation - adding fluxes
            IF (l_offline .EQV. .FALSE.) THEN

              ! Cleaning of fluxes
              fluxes_xy(:,:,0) = 0.d0;   fluxes_xz(:,:,0) = 0.d0;   fluxes_yz(:,:,0) = 0.d0

              IF (l_tracers) fluxes_xr(:,:,0,:) = 0.d0;
              IF (l_tracers) fluxes_yr(:,:,0,:) = 0.d0;
              IF (l_tracers) fluxes_rr(:,:,0)   = 0.d0

              intrajLoop: DO  intraj = 1, ntractot

                IF (trajectories(intraj)%lbas/=ilvar1) CYCLE intrajLoop

                ! Geographical fluxes
                fluxes_xy(:,:,0) = fluxes_xy(:,:,0) + fluxes_xy(:,:,intraj)
                fluxes_xz(:,:,0) = fluxes_xz(:,:,0) + fluxes_xz(:,:,intraj)
                fluxes_yz(:,:,0) = fluxes_yz(:,:,0) + fluxes_yz(:,:,intraj)

                IF (l_tracers)  THEN

                  ! Geographical+tracer fluxes
                  DO ilvar4 = 1, numtracers
                    fluxes_xr(:,:,0,ilvar4) = fluxes_xr(:,:,0,ilvar4) + fluxes_xr(:,:,intraj,ilvar4)
                    fluxes_yr(:,:,0,ilvar4) = fluxes_yr(:,:,0,ilvar4) + fluxes_yr(:,:,intraj,ilvar4)
                  END DO

                  ! Tracer+tracer fluxes
                  fluxes_rr(:,:,0) = fluxes_rr(:,:,0) + fluxes_rr(:,:,intraj)

                END IF

              END DO intrajLoop
            END IF

            ilvar3 = 0
            IF (l_offline) ilvar3 = ilvar1

            IF (dirpsi(ilvar1) == 1) THEN
                DO ilvar2 = 2, MAX(imtdom,jmtdom,km,resolution)

                    ! Geographical streamfunctions
                    IF (ilvar2<=jmtdom .AND. xyflux==1) psi_xy(:,ilvar2) = psi_xy(:,ilvar2-1) - fluxes_xy(:,ilvar2,ilvar3)
                    IF (ilvar2<=imtdom .AND. xyflux==2) psi_xy(ilvar2,:) = psi_xy(ilvar2-1,:) - fluxes_xy(ilvar2,:,ilvar3)
                    IF (ilvar2<=km)     psi_xz(:,ilvar2) = psi_xz(:,ilvar2-1) - fluxes_xz(:,ilvar2,ilvar3)
                    IF (ilvar2<=km)     psi_yz(:,ilvar2) = psi_yz(:,ilvar2-1) - fluxes_yz(:,ilvar2,ilvar3)

                    ! Geographical+tracer streamfunctions
                    DO ilvar4 = 1, numtracers
                        IF (ilvar2<=resolution .AND. l_tracers)  THEN
                          psi_xr(:,ilvar2,ilvar4) = psi_xr(:,ilvar2-1,ilvar4) - fluxes_xr(:,ilvar2,ilvar3,ilvar4)
                          psi_yr(:,ilvar2,ilvar4) = psi_yr(:,ilvar2-1,ilvar4) - fluxes_yr(:,ilvar2,ilvar3,ilvar4)
                        END IF
                    END DO

                    ! Tracer+tracer Streamfunctions
                    IF (l_tracers) psi_rr(:,ilvar2) = psi_rr(:,ilvar2-1) - fluxes_rr(:,ilvar2,ilvar3)

                END DO
            ELSE IF (dirpsi(ilvar1) == -1) THEN
                DO ilvar2 = MAX(imtdom-1,jmtdom-1,km-1,resolution-1), 1, -1

                    ! Geographical streamfunctions
                    IF (ilvar2<=jmtdom-1 .AND. xyflux==1) psi_xy(:,ilvar2) = psi_xy(:,ilvar2+1) + fluxes_xy(:,ilvar2,ilvar3)
                    IF (ilvar2<=imtdom-1 .AND. xyflux==2) psi_xy(ilvar2,:) = psi_xy(ilvar2+1,:) + fluxes_xy(ilvar2,:,ilvar3)
                    IF (ilvar2<=km-1)     psi_xz(:,ilvar2) = psi_xz(:,ilvar2+1) + fluxes_xz(:,ilvar2,ilvar3)
                    IF (ilvar2<=km-1)     psi_yz(:,ilvar2) = psi_yz(:,ilvar2+1) + fluxes_yz(:,ilvar2,ilvar3)

                    ! Geographical+tracer streamfunctions
                    DO ilvar4 = 1, numtracers
                        IF (ilvar2<=resolution-1 .AND. l_tracers) THEN
                          psi_xr(:,ilvar2, ilvar4) = psi_xr(:,ilvar2+1, ilvar4) + fluxes_xr(:,ilvar2,ilvar3,ilvar4)
                          psi_yr(:,ilvar2, ilvar4) = psi_yr(:,ilvar2+1, ilvar4) + fluxes_yr(:,ilvar2,ilvar3,ilvar4)
                        END IF
                    END DO

                    ! Tracer+tracer Streamfunctions
                    IF (l_tracers) psi_rr(:,ilvar2) = psi_rr(:,ilvar2+1) + fluxes_rr(:,ilvar2,ilvar3)

                END DO
            END IF

            CALL write_stream(imtdom, jmtdom,'xy')
            CALL write_stream(imtdom,  km,'xz')
            CALL write_stream(jmtdom,  km,'yz')
            IF (l_tracers) CALL write_stream(imtdom,  resolution,'xr')
            IF (l_tracers) CALL write_stream(jmtdom,  resolution,'yr')
            IF (l_tracers) CALL write_stream(resolution,  resolution,'rr')

        END DO

        PRINT*, '- Saving  streamfunctions'
        CALL close_outstream('xy')
        CALL close_outstream('xz')
        CALL close_outstream('yz')
        IF (l_tracers) CALL close_outstream('xr')
        IF (l_tracers) CALL close_outstream('yr')
        IF (l_tracers) CALL close_outstream('rr')

      END SUBROUTINE compute_stream

      SUBROUTINE init_stream()
      ! --------------------------------------------------
      !
      ! Purpose:
      ! Initialise streamfunctions and allocate arrays
      !
      ! --------------------------------------------------

          INTEGER :: index

          index = 21
          IF (l_offline .EQV. .FALSE.) THEN
              index  = ntracmax
          END IF

          ! Allocate flux arrays
          ALLOCATE( fluxes_xy(imtdom, jmtdom, 0:index), psi_xy(imtdom, jmtdom))
          ALLOCATE( fluxes_xz(imtdom, km, 0:index), psi_xz(imtdom, km))
          ALLOCATE( fluxes_yz(jmtdom, km, 0:index), psi_yz(jmtdom, km))
          IF (l_tracers) THEN
              ALLOCATE( fluxes_xr(imtdom, resolution, 0:index, numtracers), psi_xr(imtdom, resolution, numtracers))
              ALLOCATE( fluxes_yr(jmtdom, resolution, 0:index, numtracers), psi_yr(jmtdom, resolution, numtracers))
              ALLOCATE( fluxes_rr(resolution, resolution, 0:index), psi_rr(resolution, resolution))
          END IF

          fluxes_xy(:,:,:) = 0.;  psi_xy(:,:) = 0.
          fluxes_xz(:,:,:) = 0.;  psi_xz(:,:) = 0.
          fluxes_yz(:,:,:) = 0.;  psi_yz(:,:) = 0.

          IF (l_tracers) THEN
            fluxes_xr(:,:,:,:) = 0.; psi_xr(:,:,:) = 0.
            fluxes_yr(:,:,:,:) = 0.; psi_yr(:,:,:) = 0.
            fluxes_rr(:,:,:) = 0.  ; psi_rr(:,:)   = 0.
          END IF

      END SUBROUTINE init_stream

      SUBROUTINE update_fluxes(indx1, indx2, dir, psicase, indt1, indt2)
      ! --------------------------------------------------
      !
      ! Purpose:
      ! Update fluxes the stream
      !
      ! --------------------------------------------------

        INTEGER, INTENT(IN)           :: indx1, indx2, dir
        INTEGER, INTENT(IN), OPTIONAL :: indt1, indt2
        CHARACTER(LEN=2), INTENT(IN)  :: psicase
        INTEGER                       :: index1, index2, indm1, indm2
        REAL(DP)                      :: slope

        !
        index1 = indx1
        index2 = indx2

        ! Adjust stream functions to subdomains
        IF (l_subdom .AND. psicase(1:1)=='x') THEN

            index1 = index1 + imindom - 1;
            IF ( (imindom > imaxdom) .AND. index1 >imtdom )  index1 = index1 - imtdom

            IF (psicase(2:2)=='y') index2 = index2 + jmindom - 1;

        ELSE IF (l_subdom .AND. psicase(1:1)=='y') THEN

           IF ( (imindom > imaxdom) .AND. index1 >imtdom )  index1 = index1 - imtdom

        END IF

        ! Geographical streamfunctions
        IF (psicase=='xy') fluxes_xy(index1,index2,ntrac) = fluxes_xy(index1,index2,ntrac) + dir*subvol
        IF (psicase=='xz') fluxes_xz(index1,index2,ntrac) = fluxes_xz(index1,index2,ntrac) + dir*subvol
        IF (psicase=='yz') fluxes_yz(index1,index2,ntrac) = fluxes_yz(index1,index2,ntrac) + dir*subvol

        ! Geographical + tracer
        IF (psicase=='xr' .AND. PRESENT(indt1)) THEN
           fluxes_xr(index1,index2,ntrac,indt1) = fluxes_xr(index1,index2,ntrac,indt1) + dir*subvol
        ELSE IF (psicase=='yr' .AND. PRESENT(indt1)) THEN
           fluxes_yr(index1,index2,ntrac,indt1) = fluxes_yr(index1,index2,ntrac,indt1) + dir*subvol
        END IF

        ! Tracer-tracer equation
        IF (psicase=='rr' .AND. PRESENT(indt1) .AND. PRESENT(indt2)) THEN
          ! Isohaline tracer 2
          DO indm1 = index1, index2-1

              slope  = (FLOAT(indm1)-FLOAT(index1))*(FLOAT(indt2)-FLOAT(indt1))/(FLOAT(index2)-FLOAT(index1))
              indm2 = NINT( slope + indt1)

              fluxes_rr(indm1,indm2,ntrac) = fluxes_rr(indm1,indm2,ntrac) + subvol
          END DO

          DO indm1 = index2, index1-1

              slope  = (FLOAT(indm1)-FLOAT(index1))*(FLOAT(indt2)-FLOAT(indt1))/(FLOAT(index2)-FLOAT(index1))
              indm2 = NINT( slope + indt1)

              fluxes_rr(indm1,indm2,ntrac) = fluxes_rr(indm1,indm2,ntrac) - subvol
          END DO

        END IF

      END SUBROUTINE update_fluxes

      SUBROUTINE compute_fluxes
      ! --------------------------------------------------
      !
      ! Purpose:
      ! Compute fluxes from saved trajectory positions
      !
      ! --------------------------------------------------

        INTEGER                       :: index1, index2, index3, index4, idir = 1
        INTEGER                       :: ilooptraj, iloopsave
        INTEGER                       :: m1b, m1a, m2b, m2a
        REAL(DP)                      :: slope


        ! ilooptraj : Loop over trajectories
        ! iloopsave : Loop over saved records
        DO ilooptraj = 1, ntractot

          ! lbas
          index3 = traj_out(ilooptraj)

          IF (index3 > 0) THEN
            DO iloopsave = 2, nsave

              ! If trajectories are stored when crossing a wall
              IF (write_frec == 3 .OR. write_frec == 4) THEN

                  ! Fluxes through x-wall
                  IF ( traj_x(ilooptraj,iloopsave) == FLOAT(INT(traj_x(ilooptraj,iloopsave)))  &
                      .AND. (traj_boxf(ilooptraj,iloopsave) == 1 .OR. traj_boxf(ilooptraj,iloopsave) == 2) &
                      .AND. traj_x(ilooptraj, iloopsave)/=-999. ) THEN

                      ! Direction of trajectory
                      idir = 1
                      IF ( traj_x(ilooptraj,iloopsave) <  traj_x(ilooptraj,iloopsave-1) &
                          .AND.  traj_x(ilooptraj,iloopsave-1)/=imtdom) idir = -1
                      IF ( traj_x(ilooptraj,iloopsave) == traj_x(ilooptraj,iloopsave-1) ) idir = 0

                      ! Barotropic streamfunction (x-y)
                      index1 =  INT(traj_x(ilooptraj,iloopsave)) + 1
                      index2 =  INT(traj_y(ilooptraj,iloopsave)) + 1

                      IF (xyflux == 1) fluxes_xy(index1, index2, index3) = fluxes_xy(index1, index2, index3) &
                                        + idir*traj_subvol(ilooptraj)

                      ! Zonal indexes
                      index1 =  INT(traj_x(ilooptraj,iloopsave)) + 1

                      ! Zonal streamfunction (x-z)
                      index2 = INT(traj_z(ilooptraj,iloopsave)) + 1

                      fluxes_xz(index1, index2, index3) = fluxes_xz(index1, index2, index3) + idir*traj_subvol(ilooptraj)

                      ! Zonal-tracer streamfunction (x-r)
                      IF (l_tracers) THEN

                          DO index4 = 1, numtracers

                            index2 = tracerbin(traj_t(ilooptraj,iloopsave,index4),index4)

                            fluxes_xr(index1, index2, index3, index4) = fluxes_xr(index1, index2, index3, index4) &
                                    + idir*traj_subvol(ilooptraj)

                          END DO
                      END IF

                  END IF

                  ! Fluxes through y-wall
                  IF ( traj_y(ilooptraj,iloopsave) == FLOAT(INT(traj_y(ilooptraj,iloopsave)))  &
                      .AND. (traj_boxf(ilooptraj,iloopsave) == 3 .OR. traj_boxf(ilooptraj,iloopsave) == 4) &
                      .AND. traj_y(ilooptraj, iloopsave)/=-999. ) THEN

                      ! Direction of trajectory
                      idir = 1
                      IF ( traj_y(ilooptraj,iloopsave) <  traj_y(ilooptraj,iloopsave-1) ) idir = -1
                      IF ( traj_y(ilooptraj,iloopsave) == traj_y(ilooptraj,iloopsave-1) ) idir = 0

                      ! Barotropic streamfunction (x-y)
                      index1 =  INT(traj_x(ilooptraj,iloopsave)) + 1
                      index2 =  INT(traj_y(ilooptraj,iloopsave)) + 1

                      IF (xyflux == 2) fluxes_xy(index1, index2, index3) = fluxes_xy(index1, index2, index3) &
                                        + idir*traj_subvol(ilooptraj)

                      ! Meridional indexes
                      index1 =  INT(traj_y(ilooptraj,iloopsave)) + 1

                      ! Meridional streamfunction (y-z)
                      index2 = INT(traj_z(ilooptraj,iloopsave)) + 1

                      fluxes_yz(index1, index2, index3) = fluxes_yz(index1, index2, index3) + idir*traj_subvol(ilooptraj)

                      ! Latitude-tracer streamfunction (y-r)
                      IF (l_tracers) THEN

                          DO index4 = 1, numtracers

                            index2 = tracerbin(traj_t(ilooptraj,iloopsave,index4),index4)

                            fluxes_yr(index1, index2, index3, index4) = fluxes_yr(index1, index2, index3, index4) &
                                    + idir*traj_subvol(ilooptraj)

                          END DO
                      END IF

                  END IF

              ! If trajectories are not stored when crossing the wall (less accurate)
              ELSE

                 ! Through x walls
                 IF (traj_x(ilooptraj, iloopsave)/=-999.) THEN

                   ! Zonal streamfunction (x-z)
                   m1a = INT(traj_x(ilooptraj,iloopsave)) + 1;  m1b = INT(traj_x(ilooptraj,iloopsave-1)) + 1
                   m2a = INT(traj_z(ilooptraj,iloopsave)) + 1;  m2b = INT(traj_z(ilooptraj,iloopsave-1)) + 1

                   DO index1 = m1b, m1a-1
                     slope  = (FLOAT(index1)-FLOAT(m1b))*(FLOAT(m2a)-FLOAT(m2b))/(FLOAT(m1a)-FLOAT(m1b))
                     index2 = NINT( slope + m2b)

                     fluxes_xz(index1, index2, index3) =  fluxes_xz(index1, index2, index3) + traj_subvol(ilooptraj)
                   END DO

                   DO index1 = m1a, m1b-1
                     slope  = (FLOAT(index1)-FLOAT(m1b))*(FLOAT(m2a)-FLOAT(m2b))/(FLOAT(m1a)-FLOAT(m1b))
                     index2 = NINT( slope + m2b)

                     fluxes_xz(index1, index2, index3) =  fluxes_xz(index1, index2, index3) - traj_subvol(ilooptraj)
                   END DO

                   ! Zonal-tracer streamfunction (x-r)
                   IF (l_tracers) THEN

                       DO index4 = 1, numtracers

                         m2a = tracerbin(traj_t(ilooptraj,iloopsave,index4),index4)
                         m2b = tracerbin(traj_t(ilooptraj,iloopsave-1,index4),index4)

                         DO index1 = m1b, m1a-1
                           slope  = (FLOAT(index1)-FLOAT(m1b))*(FLOAT(m2a)-FLOAT(m2b))/(FLOAT(m1a)-FLOAT(m1b))
                           index2 = NINT( slope + m2b)

                           fluxes_xr(index1, index2, index3, index4) =  fluxes_xr(index1, index2, index3, index4) &
                                  + traj_subvol(ilooptraj)
                         END DO

                         DO index1 = m1a, m1b-1
                           slope  = (FLOAT(index1)-FLOAT(m1b))*(FLOAT(m2a)-FLOAT(m2b))/(FLOAT(m1a)-FLOAT(m1b))
                           index2 = NINT( slope + m2b)

                           fluxes_xr(index1, index2, index3, index4) =  fluxes_xr(index1, index2, index3, index4) &
                                  - traj_subvol(ilooptraj)
                         END DO

                       END DO
                   END IF
                 END IF


                 ! Through y walls
                 IF (traj_y(ilooptraj, iloopsave)/=-999.) THEN

                   ! Barotropic streamfunction (x-y)
                   m1a = INT(traj_x(ilooptraj,iloopsave)) + 1;  m1b = INT(traj_x(ilooptraj,iloopsave-1)) + 1
                   m2a = INT(traj_y(ilooptraj,iloopsave)) + 1;  m2b = INT(traj_y(ilooptraj,iloopsave-1)) + 1

                   DO index1 = m1b, m1a-1
                     slope  = (FLOAT(index1)-FLOAT(m1b))*(FLOAT(m2a)-FLOAT(m2b))/(FLOAT(m1a)-FLOAT(m1b))
                     index2 = NINT( slope + m2b)

                     fluxes_xy(index1, index2, index3) =  fluxes_xy(index1, index2, index3) + traj_subvol(ilooptraj)
                   END DO

                   DO index1 = m1a, m1b-1
                     slope  = (FLOAT(index1)-FLOAT(m1b))*(FLOAT(m2a)-FLOAT(m2b))/(FLOAT(m1a)-FLOAT(m1b))
                     index2 = NINT( slope + m2b)

                     fluxes_xy(index1, index2, index3) =  fluxes_xy(index1, index2, index3) - traj_subvol(ilooptraj)
                   END DO

                   ! Meridional streamfunction (y-z)
                   m1a = INT(traj_y(ilooptraj,iloopsave)) + 1;  m1b = INT(traj_y(ilooptraj,iloopsave-1)) + 1
                   m2a = INT(traj_z(ilooptraj,iloopsave)) + 1;  m2b = INT(traj_z(ilooptraj,iloopsave-1)) + 1

                   DO index1 = m1b, m1a-1
                     slope  = (FLOAT(index1)-FLOAT(m1b))*(FLOAT(m2a)-FLOAT(m2b))/(FLOAT(m1a)-FLOAT(m1b))
                     index2 = NINT( slope + m2b)

                     fluxes_yz(index1, index2, index3) =  fluxes_yz(index1, index2, index3) + traj_subvol(ilooptraj)
                   END DO

                   DO index1 = m1a, m1b-1
                     slope  = (FLOAT(index1)-FLOAT(m1b))*(FLOAT(m2a)-FLOAT(m2b))/(FLOAT(m1a)-FLOAT(m1b))
                     index2 = NINT( slope + m2b)

                     fluxes_yz(index1, index2, index3) =  fluxes_yz(index1, index2, index3) - traj_subvol(ilooptraj)
                   END DO

                   ! Latitude-tracer streamfunction (y-r)
                   IF (l_tracers) THEN

                       DO index4 = 1, numtracers

                         m2a = tracerbin(traj_t(ilooptraj,iloopsave,index4),index4)
                         m2b = tracerbin(traj_t(ilooptraj,iloopsave-1,index4),index4)

                         DO index1 = m1b, m1a-1
                           slope  = (FLOAT(index1)-FLOAT(m1b))*(FLOAT(m2a)-FLOAT(m2b))/(FLOAT(m1a)-FLOAT(m1b))
                           index2 = NINT( slope + m2b)

                           fluxes_yr(index1, index2, index3, index4) =  fluxes_yr(index1, index2, index3, index4) &
                                  + traj_subvol(ilooptraj)
                         END DO

                         DO index1 = m1a, m1b-1
                           slope  = (FLOAT(index1)-FLOAT(m1b))*(FLOAT(m2a)-FLOAT(m2b))/(FLOAT(m1a)-FLOAT(m1b))
                           index2 = NINT( slope + m2b)

                           fluxes_yr(index1, index2, index3, index4) =  fluxes_yr(index1, index2, index3, index4) &
                                  - traj_subvol(ilooptraj)
                         END DO

                       END DO
                   END IF
                 END IF

              END IF

              ! Tracer-tracer fluxes
              IF (l_tracers .AND. numtracers>=2) THEN

                  IF ( traj_t(ilooptraj, iloopsave,1)/=-999. ) THEN

                      ! Tracer index
                      m1a = tracerbin(traj_t(ilooptraj,iloopsave,1),1)
                      m2a = tracerbin(traj_t(ilooptraj,iloopsave,2),2)
                      m1b = tracerbin(traj_t(ilooptraj,iloopsave-1,1),1)
                      m2b = tracerbin(traj_t(ilooptraj,iloopsave-1,2),2)

                      DO index1 = m1b, m1a-1
                        slope  = (FLOAT(index1)-FLOAT(m1b))*(FLOAT(m2a)-FLOAT(m2b))/(FLOAT(m1a)-FLOAT(m1b))
                        index2 = NINT( slope + m2b)

                        fluxes_rr(index1, index2, index3) =  fluxes_rr(index1, index2, index3) + traj_subvol(ilooptraj)
                      END DO

                      DO index1 = m1a, m1b-1
                        slope  = (FLOAT(index1)-FLOAT(m1b))*(FLOAT(m2a)-FLOAT(m2b))/(FLOAT(m1a)-FLOAT(m1b))
                        index2 = NINT( slope + m2b)

                        fluxes_rr(index1, index2, index3) =  fluxes_rr(index1, index2, index3) - traj_subvol(ilooptraj)
                      END DO
                  END IF
              END IF

            END DO
          END IF
        END DO

      END SUBROUTINE compute_fluxes

END MODULE mod_stream
