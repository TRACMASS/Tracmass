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

    IMPLICIT NONE

    CONTAINS

      SUBROUTINE init_subdomain()
      ! --------------------------------------------------
      !
      ! Purpose:
      ! Redifine the size of the domain if a subdomain is chosen.
      !
      ! --------------------------------------------------

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
                  ! 7 - south wall
                  ienw(7) = -1; iene(7) = imtdom + 1; jens(7) = jmindom + 1; jenn(7) = jmindom + 1
                  ! 8 - north wall
                  ienw(8) = -1; iene(8) = imtdom + 1; jens(8) = jmaxdom - 1; jenn(8) = jmaxdom - 1

                  ! 9 - east wall
                  ienw(9) = imindom + 1; iene(9) = imindom + 1; jens(9) = -1; jenn(9) = jmtdom + 1
                  ! 10 - west wall
                  ienw(10) = imaxdom - 1; iene(10) = imaxdom - 1; jens(10) = - 1; jenn(10) = jmtdom + 1

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
                  ! 7 - south wall
                  ienw(7) = -1; iene(7) = imt + 1; jens(7) = jmindom + 1; jenn(7) = jmindom + 1
                  ! 8 - north wall
                  ienw(8) = -1; iene(8) = imt + 1; jens(8) = jmaxdom - 1; jenn(8) = jmaxdom - 1
                  ! 9 - east wall
                  ienw(9) = imindom + 1; iene(9) = imindom + 1; jens(9) = -1; jenn(9) = jmt + 1
                  ! 10 - west wall
                  ienw(10) = imaxdom - 1; iene(10) = imaxdom - 1; jens(10) = - 1; jenn(10) = jmt + 1

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
