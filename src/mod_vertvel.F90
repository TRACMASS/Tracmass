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

    USE mod_vel,             only : nsm, nsp, uflux, vflux, wflux
    USE mod_time,            only : intrpr, intrpg, tseas
    USE mod_grid
#ifdef isopycnic_model
    USE mod_param,          only  : dzteps
#endif


    IMPLICIT NONE

    INTEGER  :: k = 0

    REAL    :: fnsm = 1.d0, fnsp =1.d0

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

#if defined isopycnic_model
            IF ( dzt(ix,jy,k,nsm) <= dzteps ) fnsm = 0.d0
            IF ( dzt(ix,jy,k,nsp) <= dzteps ) fnsp = 0.d0
#endif

            IF (k> km - kmt(ix,jy)) THEN
                wflux(k,nsm) = wflux(k-1,nsm) - &
                      ( uflux(ix,jy,k,nsm) - uflux(ixm,jy,k,nsm) + vflux(ix,jy,k,nsm) - vflux(ix,jy-1,k,nsm) ) &
                      - dzdt(ix,jy,k,nsm)*dxdy(ix,jy)*fnsm

                wflux(k,nsp) = wflux(k-1,nsp) - &
                      ( uflux(ix,jy,k,nsp) - uflux(ixm,jy,k,nsp) + vflux(ix,jy,k,nsp) - vflux(ix,jy-1,k,nsp) ) &
                      - dzdt(ix,jy,k,nsp)*dxdy(ix,jy)*fnsp
            ELSE
                wflux(k,:) = 0.d0
            END IF
#if defined isopycnic_model
            ! reset
            fnsm = 1.d0
            fnsp = 1.d0
#endif

       END DO kloop
#endif

    RETURN

    END SUBROUTINE vertvel

END MODULE mod_vertvel
