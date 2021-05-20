MODULE mod_divergence
    !!------------------------------------------------------------------------------
    !!
    !!       MODULE: mod_divergence
    !!
    !!       This module computes different tracer divergence
    !!
    !!       Subroutines included in this modules:
    !!              - init_divergence
    !!              - update_divergence
    !!
    !!------------------------------------------------------------------------------

    USE mod_precdef
    USE mod_divvars
    USE mod_grid
    USE mod_postprocessvars
    USE mod_tracers
    USE mod_write

    IMPLICIT NONE

    CONTAINS

      SUBROUTINE init_divergence()
      ! --------------------------------------------------
      !
      ! Purpose:
      ! Initialise tracer divergence and allocate arrays
      !
      ! --------------------------------------------------

        ALLOCATE( tracerdiv(imtdom, jmtdom, 20, numtracers))

        tracerdiv(:,:,:,:) = 0.d0

      END SUBROUTINE init_divergence

      SUBROUTINE compute_divergence()
      ! --------------------------------------------------
      !
      ! Purpose:
      ! Compute tracer divergence
      !
      ! --------------------------------------------------

      INTEGER                       :: index1f, index1b, index2f, index2b, index3
      INTEGER                       :: ilooptraj, iloopsave, itrac

      PRINT*, ''
      PRINT*, '- Computing tracer divergences'

      ! Open divergence file
      CALL open_outdiv()

      ! ilooptraj : Loop over trajectories
      ! iloopsave : Loop over saved records
      DO ilooptraj = 1, ntractot

        ! lbas
        index3 = traj_out(ilooptraj)

        ! Only killing zones (no trajectories linked to those reaching the surface)
        IF (index3 > 1) THEN
          DO iloopsave = 1, nsave

            ! Check if the trajectory is at a gridface
            IF ( traj_boxf(ilooptraj,iloopsave)>0  .AND. traj_boxf(ilooptraj,iloopsave)<5) THEN

                ! East wall
                IF (traj_boxf(ilooptraj,iloopsave) == 1) THEN

                      ! Box leaving (back)
                      index1b = INT(traj_x(ilooptraj,iloopsave))
                      index2b = INT(traj_y(ilooptraj,iloopsave)) + 1

                      ! Entering box (forward)
                      index1f = INT(traj_x(ilooptraj,iloopsave)) + 1
                      index2f = INT(traj_y(ilooptraj,iloopsave)) + 1

                      IF (zeroindx) THEN
                          index1b = index1b + 1
                          index1f = index1f + 1
                      END IF

                      IF (index1f > imtdom .AND. iperio==1) index1f = 1

                ! West wall
                ELSE IF (traj_boxf(ilooptraj,iloopsave) == 2) THEN

                      ! Box leaving (back)
                      index1b = INT(traj_x(ilooptraj,iloopsave)) + 1
                      index2b = INT(traj_y(ilooptraj,iloopsave)) + 1

                      ! Entering box (forward)
                      index1f = INT(traj_x(ilooptraj,iloopsave))
                      index2f = INT(traj_y(ilooptraj,iloopsave)) + 1

                      IF (zeroindx) THEN
                          index1b = index1b + 1
                          index1f = index1f + 1
                      END IF

                      IF (index1b > imtdom .AND. iperio==1) index1b = 1

                ! North wall
                ELSE IF (traj_boxf(ilooptraj,iloopsave) == 3) THEN

                      ! Box leaving (back)
                      index1b = INT(traj_x(ilooptraj,iloopsave)) + 1
                      index2b = INT(traj_y(ilooptraj,iloopsave))

                      ! Entering box (forward)
                      index1f = INT(traj_x(ilooptraj,iloopsave)) + 1
                      index2f = INT(traj_y(ilooptraj,iloopsave)) + 1

                ! South wall
                ELSE IF (traj_boxf(ilooptraj,iloopsave) == 4) THEN

                      ! Box leaving (back)
                      index1b = INT(traj_x(ilooptraj,iloopsave)) + 1
                      index2b = INT(traj_y(ilooptraj,iloopsave)) + 1

                      ! Entering box (forward)
                      index1f = INT(traj_x(ilooptraj,iloopsave)) + 1
                      index2f = INT(traj_y(ilooptraj,iloopsave))

                END IF

                DO itrac = 1, numtracers
                  IF (iloopsave>1) THEN
                    tracerdiv(index1b, index2b, index3-1, itrac ) = tracerdiv(index1b, index2b, index3-1, itrac ) -&
                        traj_subvol(ilooptraj)*traj_t(ilooptraj,iloopsave,itrac)
                  END IF

                  IF (index2f <= jmtdom .AND. iloopsave<counter(ilooptraj)) tracerdiv(index1f, index2f, index3-1, itrac ) = &
                        tracerdiv(index1f, index2f, index3-1, itrac ) +&
                        traj_subvol(ilooptraj)*traj_t(ilooptraj,iloopsave,itrac)
                END DO

            END IF
          END DO
        END IF
      END DO

      ! Save data
      CALL write_div(imtdom, jmtdom, 20)

      PRINT*, '- Saving  tracer divergences'
      CALL close_outdiv()

      END SUBROUTINE compute_divergence

END MODULE mod_divergence
