MODULE mod_subdomain
    !!------------------------------------------------------------------------------
    !!
    !!       MODULE: mod_subdomain
    !!
    !!          If a subdomain is chosen this module will update
    !!          the indexes and the killing zones.
    !!
    !!          Subroutines included:
    !!               - init_subdomain
    !!               - update_subindex
    !!
    !!------------------------------------------------------------------------------

    USE mod_domain
    USE mod_grid
    USE mod_tracervars
    USE mod_postprocessvars

    IMPLICIT NONE

    CONTAINS

      SUBROUTINE init_subdomain()
      ! --------------------------------------------------
      !
      ! Purpose:
      ! Redifine the size of the domain if a subdomain is chosen.
      !
      ! --------------------------------------------------

          INTEGER :: ntracerkill   ! number of tracer-based killing zones
          INTEGER :: subgeomax     ! highest subdomain wall killing-zone slot
          INTEGER :: ngeo          ! number of geographic killing-zone slots

          subgeomax = 0
          ! Subdomain wall killing zones occupy the LAST 4 geographic slots,
          ! leaving the lower slots for user-defined namelist zones.
          ngeo      = SIZE(ienw)

          ! Make sure killing zones are on
          IF (exitType==2 .AND. l_subdom) THEN
              exitType = 3 ! Include both thermodynamic and geographical killing zones
          ELSE IF (exitType/=3 .AND. l_subdom) THEN
              exitType = 1
          END IF

          ! imtdom and jmtdom definition equal to imt/jmt
          imtdom = imt; jmtdom = jmt

          IF (l_subdom) THEN

              IF (zeroindx) THEN
                  imindom = imindom + 1 ; imaxdom = imaxdom + 1
              END IF

              ! Regular subdomain  (imindom ->- imaxdom) x (jmindom ->- jmaxdom)
              IF (imaxdom > imindom) THEN

                  ! Recalculate the size of the domain
                  imt = imaxdom - imindom + 1
                  jmt = jmaxdom - jmindom + 1


                  ! The last 4 kill zones are reserved to the Subdomain

                  ! These killing zones will not be activated for hemispheric cap subdomains
                  IF ((iperio == 1 .AND. imt == imtdom .AND. jmaxdom == 1) .EQV. .FALSE.) THEN
                    ! south wall (4th-from-last slot)
                    ienw(ngeo-3) = -1; iene(ngeo-3) = imtdom + 1; jens(ngeo-3) = jmindom + 1; jenn(ngeo-3) = jmindom + 1
                    subgeomax = ngeo-3
                  END IF

                  IF ((iperio == 1 .AND. imt == imtdom .AND. jmaxdom == jmtdom) .EQV. .FALSE.) THEN
                    ! north wall (3rd-from-last slot)
                    ienw(ngeo-2) = -1; iene(ngeo-2) = imtdom + 1; jens(ngeo-2) = jmaxdom - 1; jenn(ngeo-2) = jmaxdom - 1
                    subgeomax = ngeo-2
                  END IF

                  ! These killing zones will not be activated if iperio = 1
                  ! and imindom = 1 and imaxdom = imt
                  IF ((iperio == 1 .AND. imt == imtdom) .EQV. .FALSE.) THEN
                    ! east wall (2nd-from-last slot)
                    ienw(ngeo-1) = imindom + 1; iene(ngeo-1) = imindom + 1; jens(ngeo-1) = -1; jenn(ngeo-1) = jmtdom + 1
                    ! west wall (last slot)
                    ienw(ngeo) = imaxdom - 1; iene(ngeo) = imaxdom - 1; jens(ngeo) = - 1; jenn(ngeo) = jmtdom + 1
                    subgeomax = ngeo
                  END IF

                  ! Redefine the killing zones in the new reference system
                  ienw = ienw - imindom + 1 ; iene = iene - imindom + 1
                  jens = jens - jmindom + 1 ; jenn = jenn - jmindom + 1

              ! Subdomain part in half (imindom -> imt/1 -> imaxdom) x (jmindom ->- jmaxdom)
              ELSE IF (imindom > imaxdom) THEN

                  ! Define the two sub-subdomain
                  imthalf1 = imt - imindom + 1
                  imthalf2 = imaxdom

                  ! Recalculate the values of imt and jmt
                  imt = imthalf1 + imthalf2
                  jmt = jmaxdom - jmindom + 1

                  ! The last 4 kill zones are reserved to the Subdomain
                  ! south wall (4th-from-last slot)
                  ienw(ngeo-3) = -1; iene(ngeo-3) = imt + 1; jens(ngeo-3) = jmindom + 1; jenn(ngeo-3) = jmindom + 1
                  ! north wall (3rd-from-last slot)
                  ienw(ngeo-2) = -1; iene(ngeo-2) = imt + 1; jens(ngeo-2) = jmaxdom - 1; jenn(ngeo-2) = jmaxdom - 1
                  ! east wall (2nd-from-last slot)
                  ienw(ngeo-1) = imindom + 1; iene(ngeo-1) = imindom + 1; jens(ngeo-1) = -1; jenn(ngeo-1) = jmt + 1
                  ! west wall (last slot)
                  ienw(ngeo) = imaxdom - 1; iene(ngeo) = imaxdom - 1; jens(ngeo) = - 1; jenn(ngeo) = jmt + 1
                  subgeomax = ngeo

                  ! Redefine the killing zones in the new reference system
                  ienw = ienw - imindom + 1; iene = iene - imindom + 1;

                  WHERE (ienw<0) ienw = ienw + imtdom
                  WHERE (iene<0) iene = iene + imtdom

                  jens = jens - jmindom + 1 ; jenn = jenn - jmindom + 1

                  ! iperio is deactivated
                  iperio = 0

              END IF

          ELSE
              ! If l_subdom is false the subdomain is the entire domain
              imindom =   1; jmindom =   1

          END IF

          ! Finalise maxlbas now that exitType (possibly promoted above) and all
          ! killing zones are known. Size the per-zone streamfunction/summary
          ! arrays to the actual run. nend (lbas) encoding in the project
          ! kill_zones.F90 routines:
          !   nend = 0               -> time limit
          !   nend = 1               -> reaching the surface
          !   nend = nexit+1         -> geographic killing zone nexit
          !   nend = nexit+1+numexit -> tracer killing zone (exitType 3)
          ! ngeozones (highest geographic slot) was seeded from the namelist in
          ! init_namelist; bump it for the subdomain wall zones added above.
          ! Tracer zones are counted via the 999 sentinel default.
          ngeozones   = MAX(ngeozones, subgeomax)
          ntracerkill = COUNT(tracerchoice /= 999)

          SELECT CASE (exitType)
          CASE (1)        ! geographic killing zones only
              maxlbas = 1 + ngeozones
          CASE (2)        ! tracer-based killing zones only
              maxlbas = 1 + ntracerkill
          CASE (3)        ! tracer + geographic killing zones
              maxlbas = 1 + ntracerkill + ngeozones
          CASE DEFAULT    ! exitType 4 (hard coded) or unset: full safety
              maxlbas = MAXZONES
          END SELECT

          IF (maxlbas > MAXZONES) THEN
              PRINT*, 'ERROR: number of killing zones (maxlbas =', maxlbas, &
                      ') exceeds MAXZONES =', MAXZONES
              PRINT*, 'Increase MAXZONES in mod_precdef (src/mod_vars.F90).'
              STOP
          END IF

      END SUBROUTINE init_subdomain

      SUBROUTINE update_subindex(ji,jj)
      ! --------------------------------------------------
      !
      ! Purpose:
      ! Redifine the size of the domain if a subdomain is chosen.
      !
      ! --------------------------------------------------
          INTEGER, INTENT(INOUT)       :: ji, jj

          ! Shift for the i index
          IF (zeroindx) THEN
              ji = ji + 1
              IF (ji == imt + 1) ji = 1
          END IF

          ! Subdomain splitted in two half
          IF ( (l_subdom) .AND. (imindom > imaxdom) ) THEN
              IF (imindom<=ji)  THEN
                  ji = ji - imindom + 1
              ELSE
                  ji = ji + imthalf1
              END IF

          ! No Subdomain or a box subdomain
          ELSE
              ji = ji - imindom + 1
          END IF

          !  j index is updated
          jj = jj-jmindom+1

      END SUBROUTINE update_subindex

END MODULE mod_subdomain
