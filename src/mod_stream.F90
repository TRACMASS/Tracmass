MODULE mod_stream
    !!------------------------------------------------------------------------------
    !!
    !!       MODULE: mod_stream
    !!
    !!------------------------------------------------------------------------------

    USE mod_precdef
    USE mod_psi
    USE mod_grid
    USE mod_seed
    USE mod_write

    IMPLICIT NONE

    INTEGER :: ilvar1, ilvar2

    CONTAINS

    SUBROUTINE update_stream(indx, indy, indz, dir, psicase)
    ! --------------------------------------------------
    !
    ! Purpose:
    ! Update fluxes the stream
    !
    ! --------------------------------------------------

    INTEGER, INTENT(IN)           :: indx, indy, indz, dir
    CHARACTER(LEN=2), INTENT(IN) :: psicase

    ilvar2 = trajectories(ntrac)%lbas

    IF (ilvar2>0) THEN
       IF (psicase=='xy') fluxes_xy(indx,indy,ilvar2) = fluxes_xy(indx,indy,ilvar2) + dir*subvol
       IF (psicase=='yz') fluxes_yz(indy,indz,ilvar2) = fluxes_yz(indy,indz,ilvar2) + dir*subvol
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

    PRINT*, ' * Computing streamfunctions'
    DO ilvar1 = 1, 9
        psi_xy(:,:) = 0.; psi_yz(:,:) = 0.

        IF (dirpsi(ilvar1) == 1) THEN
            DO ilvar2 = 2, MAX(jmt,km)
                IF (ilvar2<=jmt) psi_xy(:,ilvar2) = psi_xy(:,ilvar2-1) - fluxes_xy(:,ilvar2,ilvar1+1)
                IF (ilvar2<=km)  psi_yz(:,ilvar2) = psi_yz(:,ilvar2-1) - fluxes_yz(:,ilvar2,ilvar1+1)
            END DO
        ELSE IF (dirpsi(ilvar1) == -1) THEN
            DO ilvar2 = MAX(jmt-1,km-1), 1, -1
                IF (ilvar2<=jmt-1) psi_xy(:,ilvar2) = psi_xy(:,ilvar2+1) + fluxes_xy(:,ilvar2,ilvar1+1)
                IF (ilvar2<=km-1)  psi_yz(:,ilvar2) = psi_yz(:,ilvar2+1) + fluxes_yz(:,ilvar2,ilvar1+1)
            END DO
        END IF

        CALL write_stream(imt, jmt,'xy')
        CALL write_stream(jmt,  km,'yz')

    END DO

    PRINT*, ' * Saving  streamfunctions'
    CALL close_outstream('xy')
    CALL close_outstream('yz')

    END SUBROUTINE compute_stream

END MODULE mod_stream
