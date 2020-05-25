PROGRAM TRACMASS

  USE mod_init
  USE mod_print
  USE mod_loop
  USE mod_calendar
  USE mod_domain
  USE mod_psi
  USE mod_stream
  USE mod_param
  USE mod_subdomain

  IMPLICIT none

  CHARACTER(LEN=20) :: ARG1

  CALL GET_COMMAND_ARGUMENT(1,ARG1)

  IF (ARG1 == 'rerun') l_rerun = .TRUE.
  IF (ARG1 == 'streamfunction') l_psi = .TRUE.


  ! Read namelist, define the domain and allocate the arrays
  CALL init_namelist
  CALL init_subdomain
  CALL init_alloc

  ! Welcome heading and setup info
  CALL print_header_main
  CALL writesetup_main

  ! Setup grid
  CALL setup_grid

  ! Initialise calendar and seeding
  CALL init_calendar
  CALL init_seed

  ! Read rerun
  IF (l_rerun .OR. l_psi) CALL read_rerun

  ! Open outfiles
  CALL open_outfiles

  ! Main loop
  CALL loop

  ! Close outfiles
  CALL close_outfiles

  ! If streamfunctions ON: compute fluxes and streamfunctions
  IF (l_psi) CALL compute_stream()


END PROGRAM TRACMASS
