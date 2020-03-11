PROGRAM TRACMASS

  USE mod_init
  USE mod_print
  USE mod_loop
  USE mod_calendar
  USE mod_domain

  IMPLICIT none

  CHARACTER(LEN=20) :: ARG1

  CALL GET_COMMAND_ARGUMENT(1,ARG1)

  IF (ARG1 == 'rerun') l_rerun = .TRUE.


  ! Read namelist and allocate the arrays
  CALL init_namelist
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
  IF (l_rerun) CALL read_rerun

  ! Open outfiles
  CALL open_outfiles

  ! Main loop
  CALL loop

  ! Close outfiles
  CALL close_outfiles

END PROGRAM TRACMASS
