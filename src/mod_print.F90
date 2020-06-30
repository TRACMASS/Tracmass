MODULE mod_print
  !!------------------------------------------------------------------------------
  !!
  !!       MODULE: mod_print
  !!
  !!          Defines printing options
  !!
  !!          Subroutines included:
  !!               - print_header_main
  !!               - print_header_postprocess
  !!               - writesetup_main
  !!               - print_start_loop
  !!               - print_cycle_loop
  !!               - print_end_loop
  !!               - print_end_main
  !!
  !!               - write_lines (PRIVATE)
  !!
  !!------------------------------------------------------------------------------

  USE mod_time
  USE mod_traj
  USE mod_param
  USE mod_seed
  USE mod_write
  USE mod_tracervars

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

       CHARACTER (len=15)                           :: currDate ,currTime

       CALL write_lines
       PRINT *, thickline!=================================================
       PRINT *,'             TRACMASS Lagrangian off-line particle tracking '
       PRINT *, thickline!=================================================

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

     END SUBROUTINE print_header_main

     SUBROUTINE print_header_postprocess
       ! --------------------------------------------------
       !
       ! Purpose:
       ! Prints main header of postprocessing
       !
       ! --------------------------------------------------

        CALL write_lines

        PRINT *, ''
        PRINT *, thickline!=================================================
        PRINT *,'                           TRACMASS postprocesing  '
        PRINT *, thickline!=================================================

     END SUBROUTINE print_header_postprocess

     SUBROUTINE  writesetup_main
     ! --------------------------------------------------
     !
     ! Purpose:
     ! Prints the main settings of TRACMASS
     !
     ! --------------------------------------------------
         INTEGER                                      :: itrac

         CHARACTER (len=15)                           :: cloneparticle
         CHARACTER (len=5), DIMENSION(4)              :: csubdomain


         PRINT *,"Configuration options:"

         ! Subdomain
         IF (l_subdom) THEN
                PRINT *,' - A subdomain has been choosen'
                IF (zeroindx) THEN
                    WRITE(csubdomain(1),"(I5)") imindom - 1
                    WRITE(csubdomain(2),"(I5)") imaxdom - 1
                    WRITE(csubdomain(3),"(I5)") jmindom - 1
                    WRITE(csubdomain(4),"(I5)") jmaxdom - 1
                ELSE
                    WRITE(csubdomain(1),"(I5)") imindom
                    WRITE(csubdomain(2),"(I5)") imaxdom
                    WRITE(csubdomain(3),"(I5)") jmindom
                    WRITE(csubdomain(4),"(I5)") jmaxdom
                END IF
                PRINT *, '     i index: ', ADJUSTR(csubdomain(1)),' - ',ADJUSTL(csubdomain(2))
                PRINT *, '     j index: ', ADJUSTR(csubdomain(3)),' - ',ADJUSTL(csubdomain(4))
         END IF

         ! Vertical fluxes
#if defined w_2dim
         PRINT *,' - Two-dimensional trajectories, no change in depth'
#endif
#if defined w_3dim
         PRINT *,' - Three-dimensional trajectories (vertical fluxes computed)'
#endif
#if defined w_explicit
         PRINT *,' - Three-dimensional trajectories (explicit vertical fluxes)'
#endif

         ! Loneparticle
         WRITE(cloneparticle,"(I15)") loneparticle
         IF (loneparticle>0) PRINT *, ' - Running loneparticle: ', ADJUSTL(cloneparticle)

         ! Tracers
         IF (l_tracers) THEN
           PRINT *,' - Evaluate the value of these tracers along the trajectories:'
           DO itrac = 1, numtracers
             PRINT '(A4,I2,A)','    ',itrac,ADJUSTL('- '//TRIM(tracername(itrac))//' ('//TRIM(tracerunit(itrac))//') ')
           END DO
         END IF

         ! Streamfunctions
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

          PRINT "(5(I7,' |  '),I5,2('-',I2.2),'    ',2(I2.2,':'),I2.2)", 1, ntractot-nout-nerror-nloop, &
                  nout, nerror+nloop, ntractot, &
                  startYear,startMon,startDay,startHour,startMin,INT(startSec)

     END SUBROUTINE print_start_loop

     SUBROUTINE print_cycle_loop()
     ! --------------------------------------------------
     !
     ! Purpose:
     ! Prints basic information of the loop
     !
     ! --------------------------------------------------


          PRINT "(5(I7,' |  '),I5,2('-',I2.2),'    ',2(I2.2,':'),I2.2)", ints+1, ntractot-nout-nerror-nloop, &
                  nout, nerror+nloop, ntractot, &
                  currYear,currMon,currDay,currHour,currMin,INT(currSec)
     END SUBROUTINE print_cycle_loop

     SUBROUTINE print_end_loop()
     ! --------------------------------------------------
     !
     ! Purpose:
     ! Prints the ending information  of the main loop
     !
     ! --------------------------------------------------

       CALL write_lines
       PRINT *, thickline!=================================================
       PRINT *, ntractot ,  ' particles calculated'
       PRINT *, nout     ,  ' particles exited the space and time domain'
       PRINT *, nerror+nloop   ,  ' particles flagged with errors'
       PRINT *, ntractot-nout-nerror-nloop,' particles in domain'
       PRINT *, thinline !---------------------------------------------------

     END SUBROUTINE print_end_loop

     SUBROUTINE print_end_main()
     ! --------------------------------------------------
     !
     ! Purpose:
     ! Prints the end time of simulation
     !
     ! --------------------------------------------------

       CHARACTER (len=15)                           :: currDate ,currTime

       CALL write_lines

       CALL date_and_time(currDate, currTime)

       PRINT *, ''
       PRINT *, thickline
       PRINT *,'End date  : '//currDate(1:4)//'-'//currDate(5:6)//'-'//currDate(7:8)
       PRINT *,'End time  : '//currTime(1:2)// ':'//currTime(3:4)// ':'//currTime(5:6)
       PRINT *, thickline

     END SUBROUTINE print_end_main

     SUBROUTINE write_lines

         thickline = "==============================================" // &
                     "================================="
         thinline  = "----------------------------------------------" // &
                     "---------------------------------"

     END SUBROUTINE write_lines

END MODULE mod_print
