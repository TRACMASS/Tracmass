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
    !!              - update_stream
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

        INTEGER :: ilooptraj

        PRINT*, ''
        PRINT*, '- Computing streamfunctions'

        ! If offline calculation: Compute fluxes from out files
        IF (l_offline) CALL compute_fluxes()

        ! Compute and save streamfunction
        CALL open_outstream('xy')
        CALL open_outstream('yz')

        IF (l_tracers) CALL open_outstream('yr')
        IF (l_tracers) CALL open_outstream('rr')

        ilooptraj = 1
        IF (l_offline .EQV. .FALSE.) ilooptraj = ntractot

        DO  intraj = 1, ilooptraj
          lbasLoop:DO ilvar1 = 1, 9

              IF (l_offline .EQV. .FALSE.) THEN
                  IF (trajectories(intraj)%lbas/=ilvar1+1) CYCLE lbasLoop
              END IF

              psi_xy(:,:) = 0.; psi_yz(:,:) = 0.
              IF (l_tracers) psi_yr(:,:,:) = 0.; psi_rr(:,:) = 0.

              ilvar3 = intraj
              IF (l_offline) ilvar3 = ilvar1+1

              IF (dirpsi(ilvar1) == 1) THEN
                  DO ilvar2 = 2, MAX(jmt,km,resolution)

                      ! Geographical streamfunctions
                      IF (ilvar2<=jmt) psi_xy(:,ilvar2) = psi_xy(:,ilvar2-1) - fluxes_xy(:,ilvar2,ilvar3)
                      IF (ilvar2<=km)  psi_yz(:,ilvar2) = psi_yz(:,ilvar2-1) - fluxes_yz(:,ilvar2,ilvar3)

                      ! Geographical+tracer streamfunctions
                      DO ilvar4 = 1, numtracers
                          IF (ilvar2<=resolution .AND. l_tracers)  THEN
                            psi_yr(:,ilvar2,ilvar4) = psi_yr(:,ilvar2-1,ilvar4) - fluxes_yr(:,ilvar2,ilvar3,ilvar4)
                          END IF
                      END DO

                      ! Tracer+tracer Streamfunctions
                      psi_rr(:,ilvar2) = psi_rr(:,ilvar2-1) - fluxes_rr(:,ilvar2,ilvar3)

                  END DO
              ELSE IF (dirpsi(ilvar1) == -1) THEN
                  DO ilvar2 = MAX(jmt-1,km-1,resolution-1), 1, -1

                      ! Geographical streamfunctions
                      IF (ilvar2<=jmt-1) psi_xy(:,ilvar2) = psi_xy(:,ilvar2+1) + fluxes_xy(:,ilvar2,ilvar3)
                      IF (ilvar2<=km-1)  psi_yz(:,ilvar2) = psi_yz(:,ilvar2+1) + fluxes_yz(:,ilvar2,ilvar3)

                      ! Geographical+tracer streamfunctions
                      DO ilvar4 = 1, numtracers
                          IF (ilvar2<=resolution-1 .AND. l_tracers) THEN
                            psi_yr(:,ilvar2, ilvar4) = psi_yr(:,ilvar2+1, ilvar4) + fluxes_yr(:,ilvar2,ilvar3,ilvar4)
                          END IF
                      END DO

                      ! Tracer+tracer Streamfunctions
                      psi_rr(:,ilvar2) = psi_rr(:,ilvar2+1) + fluxes_rr(:,ilvar2,ilvar3)

                  END DO
              END IF

              CALL write_stream(imt, jmt,'xy')
              CALL write_stream(jmt,  km,'yz')
              IF (l_tracers) CALL write_stream(jmt,  resolution,'yr')
              IF (l_tracers) CALL write_stream(resolution,  resolution,'rr')

          END DO lbasLoop
        END DO

        PRINT*, '- Saving  streamfunctions'
        CALL close_outstream('xy')
        CALL close_outstream('yz')
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

          index = 10
          IF (l_offline .EQV. .FALSE.) index = ntracmax

          ! Allocate flux arrays
          ALLOCATE( fluxes_xy(imt, jmt, index), psi_xy(imt, jmt))
          ALLOCATE( fluxes_yz(jmt, km, index), psi_yz(jmt, km))
          IF (l_tracers) THEN
              ALLOCATE( fluxes_yr(jmt, resolution, index, numtracers), psi_yr(jmt, resolution, numtracers))
              ALLOCATE( fluxes_rr(resolution, resolution, index), psi_rr(resolution, resolution))
          END IF

          fluxes_xy(:,:,:) = 0.;  psi_xy(:,:) = 0.
          fluxes_yz(:,:,:) = 0.;  psi_yz(:,:) = 0.

          IF (l_tracers) THEN
            fluxes_yr(:,:,:,:) = 0.; psi_yr(:,:,:) = 0.
            fluxes_rr(:,:,:) = 0.  ; psi_rr(:,:)   = 0.
          END IF

      END SUBROUTINE init_stream

      SUBROUTINE update_fluxes(index1, index2, dir, psicase, indt1, indt2)
      ! --------------------------------------------------
      !
      ! Purpose:
      ! Update fluxes the stream
      !
      ! --------------------------------------------------

        INTEGER, INTENT(IN)           :: index1, index2, dir
        INTEGER, INTENT(IN), OPTIONAL :: indt1, indt2
        CHARACTER(LEN=2), INTENT(IN)  :: psicase
        INTEGER                       :: indm1, indm2
        REAL(DP)                      :: slope

        ! Geographical streamfunctions
        IF (psicase=='xy') fluxes_xy(index1,index2,ntrac) = fluxes_xy(index1,index2,ntrac) + dir*subvol
        IF (psicase=='yz') fluxes_yz(index1,index2,ntrac) = fluxes_yz(index1,index2,ntrac) + dir*subvol

        ! Geographical + tracer
        IF (psicase=='yr' .AND. PRESENT(indt1)) THEN
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

          IF (index3 > 1) THEN
            DO iloopsave = 2, nsave

              ! If trajectories are stored when crossing a wall
              IF (write_frec == 3 .OR. write_frec == 4) THEN

                  ! Fluxes through x-wall
                  IF ( traj_x(ilooptraj,iloopsave) == FLOAT(INT(traj_x(ilooptraj,iloopsave)))  &
                      .AND. traj_x(ilooptraj, iloopsave)/=-999. ) THEN

                      ! Direction of trajectory
                      idir = 1
                      IF ( traj_x(ilooptraj,iloopsave) <  traj_x(ilooptraj,iloopsave-1) &
                          .AND.  traj_x(ilooptraj,iloopsave-1)/=360.) idir = -1
                      IF ( traj_x(ilooptraj,iloopsave) == traj_x(ilooptraj,iloopsave-1) ) idir = 0

                      ! Barotropic streamfunction (x-y)
                      index1 =  INT(traj_x(ilooptraj,iloopsave)) + 1
                      index2 =  INT(traj_y(ilooptraj,iloopsave)) + 1

                      fluxes_xy(index1, index2, index3) = fluxes_xy(index1, index2, index3) + idir*traj_subvol(ilooptraj)

                  END IF

                  ! Fluxes through y-wall
                  IF ( traj_y(ilooptraj,iloopsave) == FLOAT(INT(traj_y(ilooptraj,iloopsave)))  &
                      .AND. traj_y(ilooptraj, iloopsave)/=-999. ) THEN

                      ! Direction of trajectory
                      idir = 1
                      IF ( traj_y(ilooptraj,iloopsave) <  traj_y(ilooptraj,iloopsave-1) ) idir = -1
                      IF ( traj_y(ilooptraj,iloopsave) == traj_y(ilooptraj,iloopsave-1) ) idir = 0


                      ! Meridional streamfunction (y-z)
                      index1 = INT(traj_y(ilooptraj,iloopsave)) + 1
                      index2 = INT(traj_z(ilooptraj,iloopsave)) + 1

                      fluxes_yz(index1, index2, index3) = fluxes_yz(index1, index2, index3) + idir*traj_subvol(ilooptraj)

                      ! Latitude-tracer streamfunction (y-z)
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

                 ! Barotropic streamfunction (x-y)
                 IF (traj_y(ilooptraj, iloopsave)/=-999.) THEN
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

                   ! Latitude-tracer streamfunction (y-z)
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
              IF (l_tracers .AND. numtracers>=2 .AND. traj_t(ilooptraj, iloopsave,1)/=-999. ) THEN

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

            END DO
          END IF
        END DO

      END SUBROUTINE compute_fluxes

END MODULE mod_stream
