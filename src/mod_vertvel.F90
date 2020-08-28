MODULE mod_vertvel
    !!------------------------------------------------------------------------------
    !!
    !!       MODULE: mod_vertvel
    !!
    !!          Defines vertical fluxes
    !!
    !!          Subroutines included:
    !!               - vertvel
    !!
    !!------------------------------------------------------------------------------

    USE mod_vel,             only : nsm, nsp, uflux, vflux, wflux, uu, um, vv, vm
    USE mod_time,            only : intrpr, intrpg, tseas
    USE mod_grid


    IMPLICIT NONE

    INTEGER  :: k = 0

    CONTAINS

    SUBROUTINE vertvel(ix, ixm, jy, kz)

        INTEGER :: ix, ixm, jy, kz

! 1
#if defined w_2dim || w_explicit
            ! If 2D_w no w --// -- If 3D_w, w is read in the readfield
            RETURN
! 2
#else
        kloop: DO k = 1, kz

            IF (k> km - kmt(ix,jy)) THEN
                wflux(k, 1) = wflux(k-1, 1) - &
                      ( uflux(ix,jy,k,1) - uflux(ixm,jy,k,1) + vflux(ix,jy,k,1) - vflux(ix,jy-1,k,1) ) &
                      - dzdt(ix,jy,k,1)*dxdy(ix,jy)

                wflux(k, 2) = wflux(k-1, 2) - &
                      ( uflux(ix,jy,k,2) - uflux(ixm,jy,k,2) + vflux(ix,jy,k,2) - vflux(ix,jy-1,k,2) ) &
                      - dzdt(ix,jy,k,2)*dxdy(ix,jy)
            ELSE
                wflux(k,:) = 0.d0
            END IF

       END DO kloop
#endif

    RETURN

    END SUBROUTINE vertvel

END MODULE mod_vertvel
