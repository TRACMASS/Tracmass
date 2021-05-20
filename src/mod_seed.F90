MODULE mod_seed
    !!------------------------------------------------------------------------------
    !!
    !!       MODULE: mod_seed
    !!
    !!          Defines variables and matrices necessary for initialising
    !!          particles.
    !!
    !!          Subroutines included:
    !!               - init_seed
    !!               - seed
    !!
    !!               - read_mask  (PRIVATE)
    !!               - split_grid (PRIVATE)
    !!               - reverse    (PRIVATE)
    !!
    !!------------------------------------------------------------------------------

    USE mod_log,  only            : log_level
    USE mod_grid, only            : imt, jmt, km, kmt, nsm, mask, dzt, griddir
    USE mod_time, only            : ints, tseas, tt, ts, nff
    USE mod_vel,  only            : uflux, vflux, wflux
    USE mod_loopvars, only        : subvol
    USE mod_write, only           : write_data
    USE mod_domain, only          : l_rerun
    USE mod_postprocessvars, only : nsavewrite, l_summary

    USE mod_seedvars
    USE mod_traj
    USE mod_vertvel
    USE mod_psi
    USE mod_subdomain
    USE mod_tracervars
    USE mod_tracers

    IMPLICIT NONE

    INTEGER                                    :: num, itrac
    INTEGER                                    :: iist, ijst, ikst, iistp
    INTEGER                                    :: ijt,  ikt,  jjt, jkt
    INTEGER                                    :: ji, jii, jj, jjj, jk, jsd
    INTEGER                                    :: ktracer
    INTEGER                                    :: filestat
    INTEGER                                    :: numsd, landsd=0
    INTEGER                                    :: nsdTim, nsdMax, ntracmax
    INTEGER                                    :: itim

    INTEGER, ALLOCATABLE, DIMENSION(:,:)       :: seed_ijk, seed_set
    INTEGER, ALLOCATABLE, DIMENSION(:)         :: seed_tim

    CHARACTER(LEN=200)                         :: fullSeedFile

    CHARACTER(LEN=*), PARAMETER                :: timform = "(i12)"
    CHARACTER(LEN=*), PARAMETER                :: ijkform = "(6i6)"

    LOGICAL                                    :: fileexists

    PRIVATE :: split_grid, reverse, read_mask

    CONTAINS

    SUBROUTINE init_seed()
    ! --------------------------------------------------
    !
    ! Purpose:
    ! Initialize seeding in TRACMASS.
    !
    !
    ! Method:
    ! Populate the position seeding array seed_ijk.
    !
    ! --------------------------------------------------

        IF (log_level>0) PRINT *, ' '

        SELECT CASE (seedType) ! Seeding type: from selection or file

        CASE (1)      ! Seed particles within a given interval
                      ! defined by ist1, ist2, jst1, jst2, kst1, kst2
            nsdMax = (kst2-kst1+1)*(jst2-jst1+1)*(ist2-ist1+1)

            ALLOCATE ( seed_ijk(nsdMax, 3), seed_set(nsdMax, 2) )
            seed_ijk(:,:) = 0
            seed_set(:,:) = 0

            numsd = 0
            landsd = 0

            IF (maskFile .NE. '')  CALL read_mask()

            jiloop: DO ji=ist1,ist2
               jjloop: DO jj=jst1,jst2


                  ! If seeding region is outside the subdomain
                  IF (l_subdom .AND. (imindom>imaxdom) .AND. (ji>imaxdom .AND. ji<imindom)) CYCLE jiloop
                  IF (l_subdom .AND. (ji>imaxdom .OR. ji<imindom)) CYCLE jiloop
                  IF (l_subdom .AND. (jj>jmaxdom .OR. jj<jmindom)) CYCLE jjloop

                  ! Update the indexes
                  jii = ji; jjj = jj

                  CALL update_subindex(jii,jjj)

                  IF (mask(jii,jjj) .ne. 0) THEN
                     DO jk=kst1,kst2
                        numsd = numsd+1
                        seed_ijk (numsd,1:3) = [ jii, jjj, jk ]
                        seed_set (numsd,1:2) = [ isec, idir ]
                     END DO
                  ELSE
                     landsd = landsd+1
                  END IF

               END DO jjloop
            END DO jiloop

            nsdMax = numsd

            IF (log_level>0) THEN
                PRINT *,'Particles are seeded in a box defined as:'
                PRINT '(A,I7,A,I7)', '        ist1 : ', ist1, '   ist2 : ', ist2
                PRINT '(A,I7,A,I7)', '        jst1 : ', jst1, '   jst2 : ', jst2
                PRINT '(A,I7,A,I7)', '        kst1 : ', kst1, '   kst2 : ', kst2
            END IF

        CASE (2)      ! Seed particles according to indices given in a list

            fullSeedFile=trim(seedDir) // trim(seedFile)
            IF (log_level>0) PRINT *,'Particles are seeded from a given listfile  '

            ! Test if file exists, and read it if it does
            INQUIRE (FILE = fullSeedFile, exist=fileexists)
            IF (fileexists) THEN

                nsdMax=0

                OPEN(unit=34,file=fullSeedFile, ACCESS = 'SEQUENTIAL', &
                    FORM = 'FORMATTED', ACTION = 'READ')

                findRecl: DO
                READ (UNIT=34, fmt=timform,iostat=filestat)
                IF (filestat < 0) THEN
                    EXIT findRecl
                END IF
                nsdMax = nsdMax+1
                END DO findRecl

                ALLOCATE (seed_ijk(nsdMax,3))
                ALLOCATE (seed_set(nsdMax,2))

                REWIND (34)

                DO jsd = 1,nsdMax
                   READ (unit=34,fmt=*) seed_ijk(jsd,1), &
                                               seed_ijk(jsd,2), &
                                               seed_ijk(jsd,3), &
                                               seed_set(jsd,1), &
                                               seed_set(jsd,2)

                   ji = seed_ijk(jsd,1); jj = seed_ijk(jsd,2)
                   CALL update_subindex(ji,jj)
                   seed_ijk(jsd,1) = ji; seed_ijk(jsd,2) = jj

                END DO

                CLOSE (34)

                IF (log_level>0)  PRINT *,'   File name   : '//trim(fullSeedFile)
            ELSE

               PRINT *,'-----------------------------------------------------'
               PRINT *,'*** ERROR!                                        ***'
               PRINT *,'*** Seed files does not exist                     ***'
               PRINT *,'File name    : '//trim(fullSeedFile)
               PRINT *,'*** Run terminated.                               ***'
               STOP

            END IF

        END SELECT

        IF (log_level>0) THEN
            PRINT '(A,I9)','        Total number of cells : ', nsdMax + landsd
            PRINT '(A,I9)','         Cells masked as land : ', landsd
        END IF


        ! loneparticles
        IF (loneparticle>0 .AND. log_level>0) THEN
            PRINT '(A,I7)','WARNING! Loneparticle is set to : ', loneparticle
        ELSEIF (log_level>0) THEN
            PRINT '(A,I9)','                     Cells used : ', nsdMax
        END IF



        SELECT CASE (seedTime) ! Seeding time: from selection or file

            CASE (1) ! Time interval

                nsdTim = tst2-tst1+1
                ALLOCATE ( seed_tim(nsdTim) )

                DO jsd=1,nsdTim
                    seed_tim(jsd) = tst1 + (jsd-1)
                END DO

                IF (log_level>0) THEN
                    PRINT *,'------------------------------------------------------'
                    PRINT *,' Particles seeded in a time interval tst1 - tst2    '
                    PRINT *,'    tst         : ',tst1,tst2
                END IF

            CASE (2) ! From file

                fullSeedFile=trim(seedDir)//trim(timeFile)

                ! Test if file exists, and read it if it does
                INQUIRE (FILE = fullSeedFile, exist=fileexists)
                IF (fileexists) THEN
                    OPEN(UNIT=34,FILE=fullSeedFile, ACCESS = 'SEQUENTIAL', &
                    &    FORM = 'FORMATTED', ACTION = 'READ')

                    ! Find number of time steps to seed
                    nsdTim = 0
                    findNumberOfTimeSteps: DO
                        READ (UNIT=34, fmt=timform,iostat=filestat)
                        IF (filestat < 0) THEN
                            EXIT findNumberOfTimeSteps
                        END IF
                        nsdTim = nsdTim+1
                    END DO findNumberOfTimeSteps

                    ALLOCATE( seed_tim(nsdTim) )

                    REWIND (34)

                    DO jsd=1,nsdTim
                        READ (UNIT=34,FMT=timform,iostat=filestat) itim
                        IF (filestat < 0) THEN
                            PRINT*,'Error in reading seed time steps'
                            PRINT*,'Seed time file: '//trim(timeFile)
                            STOP
                        END IF
                        seed_tim(jsd) = itim
                    END DO

                    CLOSE(34)

                ELSE
                    PRINT *,'-----------------------------------------------------'
                    PRINT *,'*** ERROR!                                        ***'
                    PRINT *,'*** Seed files does not exist                     ***'
                    PRINT *,'File name    : '//trim(fullSeedFile)
                    PRINT *,'*** Run terminated.                               ***'
                    STOP

                END IF

                IF (log_level>0) THEN

                   PRINT*,'------------------------------------------------------'
                   PRINT*,' Particles are seeded at time steps given in list  '
                   PRINT*,' File name: '//trim(fullSeedFile)
                   PRINT*,' Seed size: ',nsdTim

                END IF

            END SELECT

            ! Allocate trajectories
            IF (nqua == 1) THEN
                ntracmax = nsdMax*nsdTim*INT(partQuant)
            ELSE
                ntracmax = nsdMax*nsdTim*100
            END IF

            ALLOCATE ( trajectories(ntracmax) )
            trajectories(:)%x1 = 0.
            trajectories(:)%y1 = 0.
            trajectories(:)%z1 = 0.
            trajectories(:)%tt = 0.
            trajectories(:)%t0 = 0.
            trajectories(:)%subvol = 0.
            trajectories(:)%ib = 0
            trajectories(:)%jb = 0
            trajectories(:)%kb = 0
            trajectories(:)%nts = 0
            trajectories(:)%niter = 0
            trajectories(:)%icycle = 0
            trajectories(:)%active = .TRUE.
            trajectories(:)%lbas = -1


            ! If postprocessing is activated
            ALLOCATE( nsavewrite(ntracmax) )
            nsavewrite = 0

        END SUBROUTINE init_seed


        SUBROUTINE seed()
        ! --------------------------------------------------
        !
        ! Purpose:
        ! Seeding particles in TRACMASS.
        !
        !
        ! Method:
        ! Populate the position seeding array in trj and nrj.
        !
        ! --------------------------------------------------

              REAL(DP)            :: vol

              IF (log_level >= 5) THEN
                 PRINT*,' entering seed '
              END IF

              findTime: DO jsd=1,nsdTim
                 IF (seed_tim(jsd) == ints) THEN
                    itim = seed_tim(jsd)
                    EXIT findTime
                 ELSE IF (seed_tim(jsd) /= ints .AND. jsd == nsdTim) THEN
                    itim = -1
                    RETURN
                 END IF
              END DO findTime

              !Loop over the seed size (nsdMax)
              startLoop: DO jsd=1,nsdMax

                 IF ( (seedTime == 1 .OR. seedTime == 2) .AND. &
                 &    (ints /= itim) ) THEN

                      CYCLE startLoop
                 ELSE IF (seedTime /= 1 .AND. seedTime /= 2) THEN
                      PRINT*,'timeStart =',seedTime,' is not a valid configuration!'
                      STOP
                 END IF

                 iist  = seed_ijk(jsd,1)
                 ijst  = seed_ijk(jsd,2)
                 ikst  = seed_ijk(jsd,3)
                 CALL reverse()

                 isec  = seed_set(jsd,1)
                 idir  = seed_set(jsd,2)

                 ! Skip the loop
                 IF (iist <   1) CYCLE startLoop
                 IF (ijst <   1) CYCLE startLoop
                 IF (ikst <   1) CYCLE startLoop
                 IF (iist > imt) CYCLE startLoop
                 IF (ijst > jmt) CYCLE startLoop
                 IF (ikst >  km) CYCLE startLoop

                 IF (kmt(iist,ijst) == 0) CYCLE startLoop

                 vol = 0.

                 kb  = ikst
                 jb  = ijst
                 ib  = iist
                 ibm = ib - 1

                 IF (ibm == 0)  ibm = imt

                 SELECT CASE(isec)

                 CASE(1) ! Through eastern meridional-vertical surface
                     vol = uflux(iist,ijst,ikst,nsm)

                 CASE (2)  ! Through northern zonal-vertical surface
                     vol = vflux(iist,ijst,ikst,nsm)

                  CASE (3)  ! Through upper zonal-meridional surface
                     vol = 1.

                    ! Vertical
                     CALL vertvel (ib,ibm,jb,kb)
#if defined w_explicit
                     vol = wflux(ib,jb,kb,nsm)
#elif w_2dim
                     vol = 1.
#else
                     vol = wflux(kb,nsm)
#endif

                  END SELECT

                  IF ((nff*idir*vol <= 0.d0 .AND. idir /= 0 ) .OR. (vol == 0.)) CYCLE startLoop
                  ! Volume/mass transport needs to be positive
                  vol = ABS(vol)

                  ! Number of trajectories for box (iist,ijst,ikst)
                  SELECT CASE (nqua)
                  CASE (1) ! partQuant particles per seed gridcell
                      num = INT(partQuant)
                  CASE (2) ! particles reflect mass transport at seeding.
													 ! set by partQuant
                      num = INT(vol/partQuant)
                  CASE (3) ! particle reflects air/water mass/volume at seeding
                      vol = dzt(ib,jb,kb,1)
                      num = INT(vol/partQuant)
                  END SELECT

                  IF (num == 0)  num = 1

                  ! Subvol definition
                  IF (num == 1) THEN
                      ijt = 1
                      ikt = 1
                  ELSE IF (num > 100) THEN
                      PRINT *, ' num is above the limit!!'
                      PRINT *, '    num :', num

                      STOP
                      !ijt    = NINT (SQRT (FLOAT(num)) )
                      !ikt    = NINT (FLOAT (num) / FLOAT (ijt))
                  ELSE
                      CALL split_grid()
                  END IF

#if w_2dim
                  ! Makes no difference in depth
                  ijt = num; ikt = 1
#endif

                  subvol = vol / DBLE (ijt*ikt)

                  IF (subvol == 0.d0) THEN
                      PRINT *, ' Transport of particle is zero!!!'
                      PRINT *, '    vol :', vol
                      PRINT *, '  uflux :', uflux (ib, jb, kb,nsm)
                      PRINT *, '  vflux :', vflux (ib, jb, kb,nsm)
                      PRINT *, ' subvol :', subvol
                      STOP
                  ENDIF

                  ! Determine start position for each particle
                  ! --------------------------------------------------
                  ijjLoop: DO jjt=1,ijt
                     kkkLoop: DO jkt=1,ikt

                       SELECT CASE (isec)
                       CASE (1)   ! Meridional-vertical section
                          x1 = DBLE (iist)
                          y1 = DBLE (jb-1) + (DBLE (jjt) - 0.5d0) / DBLE (ijt)
                          z1 = DBLE (kb-1) + (DBLE (jkt) - 0.5d0) / DBLE (ikt)
                          IF (nff*idir == 1) THEN
                              ib = iist+1
                          ELSE IF (nff*idir == -1) THEN
                              ib=iist
                          END IF

                       CASE (2)   ! Zonal-vertical section
                          x1 = DBLE (ib-1) + (DBLE (jjt) - 0.5d0) / DBLE (ijt)
                          y1 = DBLE (ijst)
                          z1 = DBLE (kb-1) + (DBLE (jkt) - 0.5d0) / DBLE (ikt)
                          IF (idir*nff == 1) THEN
                             jb = ijst+1
                          ELSE IF (idir*nff == -1) THEN
                             jb = ijst
                          END IF

                        CASE (3)   ! Horizontal section
                          x1 = DBLE (ib-1) + (DBLE (jjt) - 0.5d0) / DBLE (ijt)
                          y1 = DBLE (jb-1) + (DBLE (jkt) - 0.5d0) / DBLE (ikt)
                          z1 = DBLE (ikst)
                          IF (idir*nff == 1) THEN
                            kb = ikst+1
                          ELSE IF (idir*nff == -1) THEN
                            kb = ikst
                          END IF

                        END SELECT


                        IF(x1>IMT .or. x1<0 .or. y1>JMT .or. y1<0 .or. z1>KM .or. z1<0 ) THEN

                          PRINT *, ' Particles outside the domain !!!'
                          PRINT *, ' ib:', ib, 'ibm:',ib-1
                          PRINT *, ' jb:', jb, ' kb:',kb
                          PRINT *, ' [x1,y1,z1] = ',x1,y1,z1
                          STOP

                        END IF

                        ! If tracers are activated, initialise only those defined between the limits imposed by tracer0min and tracermax0
                        IF (l_tracers) THEN
                            DO itrac = 1, numtracers

                                ! Depending on the dimensions
                                ktracer = kb
                                IF (tracers(itrac)%dimension == '2D') ktracer = 1

                                ! Compute the value of the tracer
                                IF (isec == 1) THEN           ! Zonal wall
                                      iistp = iist + 1
                                      IF (iist == IMT) iistp = 1

                                      tracervalue(itrac) = 0.5*tracers(itrac)%data(iist,jb,ktracer,nsm) &
                                                         + 0.5*tracers(itrac)%data(iistp,jb,ktracer,nsm)

                                ELSE IF (isec == 2) THEN      ! Meridional wall

                                      tracervalue(itrac) = 0.5*tracers(itrac)%data(ib,ijst,ktracer,nsm) &
                                                         + 0.5*tracers(itrac)%data(ib,ijst+1,ktracer,nsm)

                                ELSE IF (isec == 3) THEN      ! Horizontal wall

                                      IF (ikst == KM) THEN
                                          tracervalue(itrac) = tracers(itrac)%data(ib,jb,ikst,nsm)
                                      ELSE
                                          tracervalue(itrac) = 0.5*tracers(itrac)%data(ib,jb,ikst,nsm) &
                                                             + 0.5*tracers(itrac)%data(ib,jb,ikst+1,nsm)
                                      END IF

                                END IF

                                tracerbinvalue(itrac,2) =  tracerbin(tracervalue(itrac),itrac)

                                IF ((tracervalue(itrac)<tracer0min(itrac)) .OR. (tracervalue(itrac)>tracer0max(itrac))) THEN
                                        CYCLE kkkLoop
                                END IF
                            END DO
                        END IF

                        ! Update trajectory numbers
                        ntractot = ntractot + 1
                        ntrac = ntractot

                        ! Only one particle for diagnistics purposes
                        IF ((loneparticle>0) .and. (ntrac.ne.loneparticle)) THEN
                          trajectories(ntrac)%active = .FALSE.
                          CYCLE kkkLoop
                        END IF

                        ! Rerun/streamfunction options
                        IF ((l_rerun .EQV..TRUE.) .AND. (trajectories(ntrac)%lbas==-1)) THEN
                          trajectories(ntrac)%active = .FALSE.
                          CYCLE kkkLoop
                        END IF

                        ! tt - time, fractions of ints
                        ! ts - time [s] rel to start
                        ts = DBLE (ints-1)
                        tt = ts * tseas

                        ! Define the trajectories
                        trajectories(ntrac)%x1 = x1
                        trajectories(ntrac)%y1 = y1
                        trajectories(ntrac)%z1 = z1
                        trajectories(ntrac)%tt = tt
                        trajectories(ntrac)%subvol = subvol
                        trajectories(ntrac)%t0 = tt

                        trajectories(ntrac)%ib = ib
                        trajectories(ntrac)%jb = jb
                        trajectories(ntrac)%kb = kb
                        trajectories(ntrac)%niter = 0
                        trajectories(ntrac)%nts = IDINT(ts)
                        trajectories(ntrac)%icycle = 1

                        ! Tracer definition
                        IF (l_tracers) THEN
                            ALLOCATE(trajectories(ntrac)%tracerval(numtracers))
                            trajectories(ntrac)%tracerval(:) = tracervalue
                        END IF

                        ! Define the direction of the trajectory
                        trajdir(:) = 0; trajdir(isec) = nff*idir

                        ! Boxface
                        IF (isec ==1 .AND. nff*idir>0) boxface = 1
                        IF (isec ==1 .AND. nff*idir<0) boxface = 2
                        IF (isec ==2 .AND. nff*idir>0) boxface = 3
                        IF (isec ==2 .AND. nff*idir<0) boxface = 4
                        IF (isec ==3 .AND. nff*idir>0) boxface = 5
                        IF (isec ==3 .AND. nff*idir<0) boxface = 6

                        !Save initial particle position
                        IF(log_level >= 3) THEN
                           PRINT*,' write initial trajectory position '
                        END IF

                        CALL write_data('ini')
                        CALL write_data('run')

                      END DO kkkLoop
                    END DO ijjLoop
              END DO startLoop

              IF (log_level >= 5) THEN
                PRINT*,' leaving seed'
              END IF

        END SUBROUTINE seed

        SUBROUTINE read_mask()
        ! --------------------------------------------------
        !
        ! Purpose:
        ! Read the mask file if any
        !
        ! --------------------------------------------------

          INQUIRE (FILE = TRIM(seedDir)//TRIM(maskFile), exist=fileexists)

          IF (fileexists) THEN
              OPEN (UNIT=10, FILE= TRIM(seedDir)//TRIM(maskFile), ACTION = 'READ')
              READ (UNIT=10, FMT=*) mask(:,:)
              CLOSE (UNIT=10)
          ELSE
              PRINT *,'-----------------------------------------------------'
              PRINT *,'*** ERROR!                                        ***'
              PRINT *,'*** Mask file does not exist                      ***'
              PRINT *,'File name    : '//trim(maskFile)
              PRINT *,'*** Run terminated.                               ***'
              STOP
          END IF

        END SUBROUTINE

        SUBROUTINE split_grid()
        ! --------------------------------------------------
        !
        ! Purpose:
        ! Splits the grid in equal parts.
        !
        !
        ! Method:
        ! It divides in quasi-exact parts (no approximation).
        !
        ! --------------------------------------------------

            ! First 10 prime numbers
            INTEGER, DIMENSION(25)  :: num_prime = (/2,3,5,7,11,13,17,19,23,29,31, &
                                    & 37,41,43,47,53,59,61,67,71, 73, 79, 83, 89, 97/)
            INTEGER    :: nsg, isg, jsg

            LOGICAL :: l_square

            ! Initialise l_square
            l_square = .FALSE.

            DO nsg = 1, num

              ! Is num a square number?
              IF (FLOAT(INT(SQRT(FLOAT(num)))) == SQRT(FLOAT(num))) THEN
                  ijt    = INT (SQRT (FLOAT(num)) )
                  ikt    = ijt

                  l_square = .TRUE.
              END IF

              IF (l_square .EQV. .FALSE.) THEN
                DO isg = 1, 10

                    ! Is num a prime number?
                    IF (num == num_prime(isg)) THEN
                        ijt = num
                        ikt = 1
                        EXIT

                    ! Integer factorisation of num
                    ELSE
                        ijt = INT (SQRT (FLOAT(num)) )
                        ikt = NINT (FLOAT (num) / FLOAT (ijt))

                        IF (ijt*ikt /= num) THEN
                            DO jsg = 1, 10
                                ijt = num_prime(jsg)
                                ikt = num/ijt

                                IF (ijt*ikt == num) EXIT
                            END DO
                        END IF

                    END IF
                END DO
              END IF

            END DO

        END SUBROUTINE split_grid

        SUBROUTINE reverse()
        ! --------------------------------------------------
        !
        ! Purpose:
        ! Reverse seeding indexes according to the project type
        !
        ! --------------------------------------------------

          IF (griddir(2) == -1) THEN
                IF (isec == 2) THEN
                  ijst = jmt - ijst    ! Meridional reverse
                ELSE
                  ijst = jmt - ijst + 1   ! Meridional reverse
                END IF
          END IF

          IF (griddir(3) == -1) THEN
                IF (isec == 3) THEN
                  ikst = km - ikst       ! Vertical reverse
                ELSE
                  ikst = km - ikst + 1   ! Vertical reverse
                END IF
          END IF

        END SUBROUTINE reverse


END MODULE mod_seed
