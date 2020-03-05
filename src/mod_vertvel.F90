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
#if defined w_2dim || w_3dim
            ! If 2D_w no w --// -- If 3D_w, w is read in the readfield
            RETURN
! 2
#else
        kloop: DO k = 1, kz
#if defined z_timevar

            ! Under development
#else
#if defined full_wflux
            uu = intrpg * uflux(ix ,jy  ,k, 2) + intrpr * uflux(ix ,jy  ,k, 1)
            um = intrpg * uflux(ixm,jy  ,k, 2) + intrpr * uflux(ixm,jy  ,k, 1)
            vv = intrpg * vflux(ix ,jy  ,k, 2) + intrpr * vflux(ix ,jy  ,k, 1)
            vm = intrpg * vflux(ix ,jy-1,k, 2) + intrpr * vflux(ix ,jy-1,k, 1)

            wflux(ix,jy,k,1) = wflux(ix,jy,k-1,1) - ( uu - um + vv - vm )
#else
            wflux(k, 1) = wflux(k-1, 1) - &
                ( uflux(ix,jy,k,1) - uflux(ixm,jy,k,1) + vflux(ix,jy,k,1) - vflux(ix,jy-1,k,1) )

            wflux(k, 2) = wflux(k-1, 2) - &
                ( uflux(ix,jy,k,2) - uflux(ixm,jy,k,2) + vflux(ix,jy,k,2) - vflux(ix,jy-1,k,2) )
#endif

#endif
       END DO kloop

       DO k = 0, KM-kmt(ix,jy)
#if defined full_wflux
          wflux(:,:,k,:) = 0.d0
#else
          wflux(k,:) = 0.d0
#endif

       END DO
#endif

    RETURN

    END SUBROUTINE vertvel

END MODULE mod_vertvel
