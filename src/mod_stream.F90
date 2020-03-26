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

    SUBROUTINE update_stream(indx, indy, indz, dir)
    ! --------------------------------------------------
    !
    ! Purpose:
    ! Update fluxes the stream
    !
    ! --------------------------------------------------

    INTEGER, INTENT(IN) :: indx, indy, indz, dir

    ilvar2 = trajectories(ntrac)%lbas

    IF (ilvar2>0) fluxes_xy(indx,indy,ilvar2) = fluxes_xy(indx,indy,ilvar2) + dir*subvol

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
    CALL open_outstream('streamfunction')

    PRINT*, ' * Computing streamfunctions'
    DO ilvar1 = 1, 9
        psi_xy(:,:) = 0.

        IF (dirpsi(ilvar1) == 1) THEN
            DO ilvar2 = 2, jmt
                psi_xy(:,ilvar2) = psi_xy(:,ilvar2-1) - fluxes_xy(:,ilvar2,ilvar1+1)
            END DO
        ELSE IF (dirpsi(ilvar1) == -1) THEN
            DO ilvar2 = jmt-1, 1, -1
                psi_xy(:,ilvar2) = psi_xy(:,ilvar2+1) + fluxes_xy(:,ilvar2,ilvar1+1)
            END DO
        END IF

        CALL write_stream(imt, jmt)

    END DO

    PRINT*, ' * Saving  streamfunctions'
    CALL close_outstream('streamfunction')

    END SUBROUTINE compute_stream

END MODULE mod_stream
