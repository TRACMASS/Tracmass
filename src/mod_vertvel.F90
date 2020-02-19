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
    USE mod_time,            only: intrpr, intrpg, tseas
    USE mod_grid

    IMPLICIT NONE

    INTEGER  :: k = 0

    REAL(DP) :: uu, um, vv, vm

    CONTAINS

    SUBROUTINE vertvel(ia,iam,ja,ka)

        INTEGER :: ia, iam, ja, ka

! 1
#if defined w_2dim || w_3dim
            ! If 2D_w no w --// -- If 3D_w, w is read in the readfield
            RETURN
! 2
#else
        kloop: DO k = 1, ka
#if defined z_timevar

            ! Under development
#else
#if defined full_wflux
            uu = intrpg * uflux(ia ,ja  ,k, 2) + intrpr * uflux(ia ,ja  ,k, 1)
            um = intrpg * uflux(iam,ja  ,k, 2) + intrpr * uflux(iam,ja  ,k, 1)
            vv = intrpg * vflux(ia ,ja  ,k, 2) + intrpr * vflux(ia ,ja  ,k, 1)
            vm = intrpg * vflux(ia ,ja-1,k, 2) + intrpr * vflux(ia ,ja-1,k, 1)

            wflux(ia,ja,k,1) = wflux(ia,ja,k-1,1) - ( uu - um + vv - vm )
#else
            wflux(k, 1) = wflux(k-1, 1) - &
                ( uflux(ia,ja,k,1) - uflux(iam,ja,k,1) + vflux(ia,ja,k,1) - vflux(ia,ja-1,k,1) )

            wflux(k, 2) = wflux(k-1, 2) - &
                ( uflux(ia,ja,k,2) - uflux(iam,ja,k,2) + vflux(ia,ja,k,2) - vflux(ia,ja-1,k,2) )
#endif

#endif
       END DO kloop

       DO k = 0, KM-kmt(ia,ja)
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
