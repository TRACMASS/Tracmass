PROGRAM TRACMASS

  USE mod_init
  USE mod_print
  USE mod_loop
  USE mod_calendar

  IMPLICIT none

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

  ! Open outfiles
  CALL open_outfiles
  
  ! Main loop
  CALL loop

  ! Close outfiles
  CALL close_outfiles

END PROGRAM TRACMASS
