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

    SUBROUTINE init_stream
    ! --------------------------------------------------
    !
    ! Purpose:
    ! Initialize the stream
    !
    ! --------------------------------------------------

    ALLOCATE( fluxes(ntracmax) )

    DO ilvar1 = 1, ntracmax
      ALLOCATE( fluxes(ilvar1)%xy(imt,jmt) )
#ifndef w_2dim
      ALLOCATE( fluxes(ilvar1)%xz(imt,km) )
      ALLOCATE( fluxes(ilvar1)%yz(imt,km) )
#endif
    END DO

    END SUBROUTINE init_stream

    SUBROUTINE update_stream(indx, indy, indz, dir)
    ! --------------------------------------------------
    !
    ! Purpose:
    ! Update fluxes the stream
    !
    ! --------------------------------------------------

    INTEGER, INTENT(IN) :: indx, indy, indz, dir

    fluxes(ntrac)%xy(indx,indy) = fluxes(ntrac)%xy(indx,indy) + dir*subvol

    END SUBROUTINE update_stream

    SUBROUTINE compute_fluxes()
    ! --------------------------------------------------
    !
    ! Purpose:
    ! Compute total fluxes
    !
    ! --------------------------------------------------
    CALL read_rerun()

    ! Compute and save streamfunction
    CALL open_outstream('fluxes')

    ! Add fluxes
    DO ilvar1 = 1, ntracmax

      ilvar2 = trajectories(ilvar1)%lbas

      IF (ilvar2>0) fluxes_xy(:,:,ilvar2) = fluxes_xy(:,:,ilvar2) + fluxes(ilvar1)%xy(:,:)

    END DO

    DO ilvar1 = 1, 10

        CALL write_fluxes(imt, jmt, ilvar1)

    END DO

    CALL close_outstream('fluxes')

    END SUBROUTINE compute_fluxes

    SUBROUTINE compute_stream()
    ! --------------------------------------------------
    !
    ! Purpose:
    ! Compute streamfunctions
    !
    ! --------------------------------------------------
    CALL compute_fluxes()

    ! Compute and save streamfunction
    CALL open_outstream('streamfunction')

    DO ilvar1 = 1, 9
        psi_xy(:,:) = 0.

        IF (dirpsi(ilvar1) == 1) THEN
            DO ilvar2 = 2, jmt
                psi_xy(:,ilvar2) = psi_xy(:,ilvar2-1) + fluxes_xy(:,ilvar2,ilvar1+1)
            END DO
        ELSE IF (dirpsi(ilvar1) == -1) THEN
            DO ilvar2 = jmt-1, 1, -1
                psi_xy(:,ilvar2) = psi_xy(:,ilvar2+1) - fluxes_xy(:,ilvar2,ilvar1+1)
            END DO
        END IF

        CALL write_stream(imt, jmt)

    END DO

    CALL close_outstream('streamfunction')

    END SUBROUTINE compute_stream

END MODULE mod_stream
