MODULE mod_print
  !!------------------------------------------------------------------------------
  !!
  !!       MODULE: mod_print
  !!
  !!          Defines printing options
  !!
  !!          Subroutines included:
  !!               - print_header_main
  !!               - writesetup_main
  !!               - print_start_loop
  !!               - print_cycle_loop
  !!               - print_end_loop
  !!
  !!               - write_lines (PRIVATE)
  !!
  !!------------------------------------------------------------------------------

  USE mod_time
  USE mod_traj
  USE mod_param
  USE mod_seed
  USE mod_write

  IMPLICIT NONE

  CHARACTER(79)       :: thinline, thickline

  PRIVATE :: write_lines

  CONTAINS
  SUBROUTINE print_header_main
    ! --------------------------------------------------
    !
    ! Purpose:
    ! Prints main header
    !
    ! --------------------------------------------------

     CALL write_lines
     PRINT *, thickline!=================================================
     PRINT *,'             TRACMASS Lagrangian off-line particle tracking '
     PRINT *, thickline!=================================================

   END SUBROUTINE print_header_main


   SUBROUTINE  writesetup_main
   ! --------------------------------------------------
   !
   ! Purpose:
   ! Prints the main settings of TRACMASS
   !
   ! --------------------------------------------------

       CHARACTER (len=15)                           :: currDate ,currTime, cloneparticle

       CALL write_lines
       CALL date_and_time(currDate, currTime) ! Fortran internal function

       Project  = PROJECT_NAME
       Case     = CASE_NAME

       IF ((IARGC() > 0) )  THEN
          CALL getarg(1,Case)
       END IF

       CALL getenv('TRMPROJDIR',projdir)
       IF (len(TRIM(projdir)) == 0) THEN
          CALL getenv('TRMDIR',ormdir)
          IF (len(TRIM(ormdir)) .ne. 0) THEN
             projdir = TRIM(ormdir)//'/projects/'//TRIM(Project)//'/'
          ELSE
             projdir = 'projects/'//TRIM(Project)
          END IF
       END IF


       PRINT *,''
       PRINT *,'Start date  : '//currDate(1:4)//'-'//currDate(5:6)//'-'//currDate(7:8)
       PRINT *,'Start time  : '//currTime(1:2)// ':'//currTime(3:4)// ':'//currTime(5:6)
       PRINT *, thinline !---------------------------------------------------
       PRINT *,''
       PRINT *,'Model information:'
       PRINT *,'Project code  : '//TRIM(Project)
       PRINT *,'Case name     : '//TRIM(Case)
       PRINT *,'Namelist file : '//TRIM(projdir)//'/namelist_'//TRIM(Case)//'.in'

       PRINT *, thinline !---------------------------------------------------
       PRINT *,''
       PRINT *,'Output information:'
       PRINT *,'Directory for output files : ' ,TRIM(outDataDir)
       PRINT *,'Prefix for output files    : ' ,TRIM(outDataFile)
       PRINT *, thinline !---------------------------------------------------
       PRINT *,''
       PRINT *,"Configuration options:"

#if defined w_2dim
       PRINT *,' - Two-dimensional trajectories, no change in depth'
#endif
#if defined full_wflux
       PRINT *,' - 3D vertival volume flux field.'
#endif
#if defined w_3dim
       PRINT *,' - Explicit vertical velocities from the GCM.'
#endif

       WRITE(cloneparticle,"(I15)") loneparticle
       IF (loneparticle>0) PRINT *, ' - Running loneparticle: ', ADJUSTL(cloneparticle)
       IF (l_psi) PRINT *,' - Computing streamfunctions.'


       PRINT *, thinline
       PRINT *,''

   END SUBROUTINE writesetup_main

   SUBROUTINE print_start_loop
   ! --------------------------------------------------
   !
   ! Purpose:
   ! Prints the header of the main loop
   !
   ! --------------------------------------------------

        CALL write_lines
        WRITE(6,FMT='(A,I4,A,I2.2,A,I2.2,A,I2.2,A,I2.2)')          &
             ' Start date in model-time     : ' , startYear, '-',  &
             startMon, '-', startDay,' ' ,startHour, ':', startMin
        WRITE(6,FMT='(A,I4,A,I2.2,A,I2.2,A,I2.2,A,I2.2)')          &
             ' End date in model-time       : ' , endYear, '-',  &
             endMon, '-', endDay,' ' ,endHour, ':', endMin
        WRITE(6,FMT='(A,I6)') ' Length of run in timesteps   : ' ,intrun
        WRITE(6,FMT='(A,I6)') ' Number of seeding timesteps  : ' ,nsdTim
        WRITE(6,FMT='(A,I6)') ' Steps between two GCM fields : ' ,iter

        PRINT *,''
        PRINT *, thinline !---------------------------------------------------
        PRINT *,'t-step        run        out        err '  // &
                '       tot                 model date'
        PRINT *, thinline !---------------------------------------------------

   END SUBROUTINE print_start_loop

   SUBROUTINE print_cycle_loop()
   ! --------------------------------------------------
   !
   ! Purpose:
   ! Prints basic information of the loop
   !
   ! --------------------------------------------------


        PRINT "(5(I7,' |  '),I5,2('-',I2.2),'    ',2(I2.2,':'),I2.2)", ints, ntractot-nout-nerror-nloop, &
                nout, nerror+nloop, ntractot, &
                currYear,currMon,currDay,currHour,currMin,INT(currSec)
   END SUBROUTINE print_cycle_loop

   SUBROUTINE print_end_loop()
   ! --------------------------------------------------
   !
   ! Purpose:
   ! Prints the header of the main loop
   !
   ! --------------------------------------------------


     CHARACTER (len=15)                           :: currDate ,currTime

     CALL write_lines
     PRINT *, thickline!=================================================
     PRINT *, ntractot ,  ' particles calculated'
     PRINT *, nout     ,  ' particles exited the space and time domain'
     PRINT *, nerror+nloop   ,  ' particles flagged with errors'
     PRINT *, ntractot-nout-nerror-nloop,' particles in domain'
     PRINT *, thinline !---------------------------------------------------

     CALL date_and_time(currDate, currTime)
     PRINT *,'End date  : '//currDate(1:4)//'-'//currDate(5:6)//'-'//currDate(7:8)
     PRINT *,'End time  : '//currTime(1:2)// ':'//currTime(3:4)// ':'//currTime(5:6)
     PRINT *, thickline

   END SUBROUTINE print_end_loop

   SUBROUTINE write_lines

       thickline = "==============================================" // &
                   "================================="
       thinline  = "----------------------------------------------" // &
                   "---------------------------------"

   END SUBROUTINE write_lines



END MODULE mod_print
