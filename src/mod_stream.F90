MODULE mod_stream
    !!------------------------------------------------------------------------------
    !!
    !!       MODULE: mod_stream
    !!
    !!       This module updates the fluxes and computes different streamfunctions
    !!
    !!       Subroutines included in this modules:
    !!              - update_stream
    !!              - compute_stream
    !!
    !!------------------------------------------------------------------------------

    USE mod_precdef
    USE mod_psi
    USE mod_grid
    USE mod_seed
    USE mod_write

    IMPLICIT NONE

    INTEGER :: ilvar1, ilvar2, ilvar3

    CONTAINS

    SUBROUTINE update_stream(index1, index2, dir, psicase, indt)
    ! --------------------------------------------------
    !
    ! Purpose:
    ! Update fluxes the stream
    !
    ! --------------------------------------------------

    INTEGER, INTENT(IN)           :: index1, index2, dir
    INTEGER, INTENT(IN), OPTIONAL :: indt
    CHARACTER(LEN=2), INTENT(IN)  :: psicase

    ilvar2 = trajectories(ntrac)%lbas

    IF (ilvar2>0) THEN
       ! Geographical streamfunctions
       IF (psicase=='xy') fluxes_xy(index1,index2,ilvar2) = fluxes_xy(index1,index2,ilvar2) + dir*subvol
       IF (psicase=='yz') fluxes_yz(index1,index2,ilvar2) = fluxes_yz(index1,index2,ilvar2) + dir*subvol

       ! Geographical + tracer
       IF (psicase=='yr' .AND. PRESENT(indt)) THEN
          fluxes_yr(index1,index2,ilvar2,indt) = fluxes_yr(index1,index2,ilvar2,indt) + dir*subvol
       END IF
    END IF

    END SUBROUTINE update_stream

    SUBROUTINE compute_stream()
    ! --------------------------------------------------
    !
    ! Purpose:
    ! Compute streamfunctions
    !
    ! --------------------------------------------------

    PRINT*, 'Streamfunction computation'
    PRINT*, '----------------------------------------'

    ! Compute and save streamfunction
    CALL open_outstream('xy')
    CALL open_outstream('yz')

    IF (l_tracers) CALL open_outstream('yr')

    PRINT*, ' * Computing streamfunctions'
    DO ilvar1 = 1, 9
        psi_xy(:,:) = 0.; psi_yz(:,:) = 0.
        IF (l_tracers) psi_yr(:,:,:) = 0.

        IF (dirpsi(ilvar1) == 1) THEN
            DO ilvar2 = 2, MAX(jmt,km,resolution)
                ! Geographical streamfunctions
                IF (ilvar2<=jmt) psi_xy(:,ilvar2) = psi_xy(:,ilvar2-1) - fluxes_xy(:,ilvar2,ilvar1+1)
                IF (ilvar2<=km)  psi_yz(:,ilvar2) = psi_yz(:,ilvar2-1) - fluxes_yz(:,ilvar2,ilvar1+1)

                ! Geographical+tracer streamfunctions
                DO ilvar3 = 1, numtracers
                    IF (ilvar2<=resolution .AND. l_tracers)  THEN
                      psi_yr(:,ilvar2,ilvar3) = psi_yr(:,ilvar2-1,ilvar3) - fluxes_yr(:,ilvar2,ilvar1+1,ilvar3)
                    END IF
                END DO
            END DO
        ELSE IF (dirpsi(ilvar1) == -1) THEN
            DO ilvar2 = MAX(jmt-1,km-1,resolution-1), 1, -1
                ! Geographical streamfunctions
                IF (ilvar2<=jmt-1) psi_xy(:,ilvar2) = psi_xy(:,ilvar2+1) + fluxes_xy(:,ilvar2,ilvar1+1)
                IF (ilvar2<=km-1)  psi_yz(:,ilvar2) = psi_yz(:,ilvar2+1) + fluxes_yz(:,ilvar2,ilvar1+1)

                ! Geographical+tracer streamfunctions
                DO ilvar3 = 1, numtracers
                    IF (ilvar2<=resolution-1 .AND. l_tracers) THEN
                      psi_yr(:,ilvar2, ilvar3) = psi_yr(:,ilvar2+1, ilvar3) + fluxes_yr(:,ilvar2,ilvar1+1, ilvar3)
                    END IF
                END DO
            END DO
        END IF

        CALL write_stream(imt, jmt,'xy')
        CALL write_stream(jmt,  km,'yz')
        IF (l_tracers) CALL write_stream(jmt,  resolution,'yr')

    END DO

    PRINT*, ' * Saving  streamfunctions'
    CALL close_outstream('xy')
    CALL close_outstream('yz')
    IF (l_tracers) CALL close_outstream('yr')

    END SUBROUTINE compute_stream

END MODULE mod_stream
