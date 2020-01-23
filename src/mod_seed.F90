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
    !!------------------------------------------------------------------------------

    USE mod_log,  only      : log_level
    USE mod_grid, only      : imt, jmt, km, kmt, nsm, mask, dz, dzt
    USE mod_time, only      : ntime, ints, tseas, partQuant
    USE mod_vel,  only      : uflux, vflux, wflux
    USE mod_traj
    USE mod_loopvars, only  : subvol

    IMPLICIT NONE

    INTEGER                                    :: isec, idir
    INTEGER                                    :: nqua, num, nsdMax
    INTEGER                                    :: seedType, seedTime
    INTEGER                                    :: ist1, ist2, jst1, jst2
    INTEGER                                    :: kst1, kst2, tst1, tst2
    INTEGER                                    :: iist, ijst, ikst
    INTEGER                                    :: ijt,  ikt,  jjt, jkt
    INTEGER                                    :: ji, jj, jk, jsd
    INTEGER                                    :: filestat
    INTEGER                                    :: numsd, landsd=0
    INTEGER                                    :: nsdTim
    INTEGER                                    :: loneparticle
    INTEGER*8                                  :: itim

    INTEGER, ALLOCATABLE, DIMENSION(:,:)       :: seed_ijk, seed_set
    INTEGER, ALLOCATABLE, DIMENSION(:)         :: seed_tim

    CHARACTER(LEN=200)                         :: seedDir, seedFile
    CHARACTER(LEN=200)                         :: timeFile
    CHARACTER(LEN=200)                         :: fullSeedFile

    CHARACTER(LEN=*), PARAMETER                :: timform = "(i12)"
    CHARACTER(LEN=*), PARAMETER                :: ijkform = "(6i6)"

    LOGICAL                                    :: fileexists

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

    !-------------------------------------------------------------------------
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

            DO ji=ist1,ist2
               DO jj=jst1,jst2
                  IF (mask(ji,jj) .ne. 0) THEN
                     DO jk=kst1,kst2
                        numsd = numsd+1
                        seed_ijk (numsd,1:3) = [ ji, jj, jk ]
                        seed_set (numsd,1:2) = [ isec, idir ]
                     END DO
                  ELSE
                     landsd = landsd+1
                  END IF

               END DO
            END DO

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
                   !READ (unit=34, fmt=ijkform) seed_ijk(jsd,1), &
                   READ (unit=34,fmt=*) seed_ijk(jsd,1), &
                                               seed_ijk(jsd,2), &
                                               seed_ijk(jsd,3), &
                                               seed_set(jsd,1), &
                                               seed_set(jsd,2)
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
                    seed_tim(jsd) = tst1 + (jsd-1)*(tst2-tst1)/(nsdTim-1)
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

        END SUBROUTINE


        SUBROUTINE seed()
        ! --------------------------------------------------
        !
        ! Purpose:
        ! Seeding particles in TRACMASS.
        !
        !
        ! Method:
        ! Populate the position seeding array in trj and nrj
        !
        ! --------------------------------------------------

              REAL(DP)            :: tfraction, trelative
              REAL(DP)            :: vol

              IF (log_level >= 5) THEN
                 PRINT*,' entering seed '
              END IF

              findTime: DO jsd=1,nsdTim
                 IF (seed_tim(jsd) == ntime) THEN
                    itim = seed_tim(jsd)
                    EXIT findTime
                 ELSE IF (seed_tim(jsd) /= ntime .AND. jsd == nsdTim) THEN
                    itim = -1
                    RETURN
                 END IF
              END DO findTime

              !Loop over the seed size (nsdMax)
              startLoop: DO jsd=1,nsdMax

                 ! Test if it is time to launch the particle ---
                 ! -------------------------------------------------
                 !itim  = seed_tim (jsd)

                 IF ( (seedTime == 1 .OR. seedTime == 2) .AND. &
                 &    (ntime /= itim) ) THEN
                      CYCLE startLoop
                 ELSE IF (seedTime /= 1 .AND. seedTime /= 2) THEN
                      PRINT*,'timeStart =',seedTime,' is not a valid configuration!'
                      STOP
                 END IF

                 iist  = seed_ijk (jsd,1)
                 ijst  = seed_ijk (jsd,2)
                 ikst  = seed_ijk (jsd,3)
                 isec  = seed_set (jsd,1)
                 idir  = seed_set (jsd,2)

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
                    vol = uflux (iist,ijst,ikst,nsm)

                 CASE (2)  ! Through northern zonal-vertical surface
                    vol = vflux (iist,ijst,ikst,nsm)

                  CASE (3)  ! Through upper zonal-meridional surface
                     vol = 1.

                     ! VERTICAL VELOCITIES GO HERE [ADD!]
                     ! ****************  CALL VERTVEL *****************

                     !CALL vertvel (ib,ibm,jb,kb)
                     !#if defined explicit_w || full_wflux
                     !      vol = wflux(ib,jb,kb,nsm)
                     !#elif twodim
                     !      vol = 1.
                     !#else
                     !      vol = ;*wflux(kb,nsm)

                  END SELECT

                  IF (vol == 0.) CYCLE startLoop

                  ! Volume/mass transport needs to be positive
                  vol = ABS(vol)

                  ! Number of trajectories for box (iist,ijst,ikst)
                  SELECT CASE (nqua)
                  CASE (1) ! partQuant particles per seed gridcell
                      num = partQuant
                  CASE (2) ! particles reflect mass transport at seeding.
													 ! set by partQuant
                      num = INT(vol/partQuant)
                  CASE (3) ! particle reflects air/water mass/volume at seeding
                      vol = dzt(ib,jb,kb,1)
                      num = INT(vol/partQuant)
                  END SELECT

                  IF (num == 0)  num = 1

                  ! Subvol definition
                  ijt    = NINT (SQRT (FLOAT(num)) )
                  ikt    = NINT (FLOAT (num) / FLOAT (ijt))
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
                          x1 = DBLE (ib)
                          y1 = DBLE (jb-1) + (DBLE (jjt) - 0.5d0) / DBLE (ijt)
                          z1 = DBLE (kb-1) + (DBLE (jkt) - 0.5d0) / DBLE (ikt)
                          IF (idir == 1) THEN
                              ib = iist+1
                          ELSE IF (idir == -1) THEN
                              ib=iist
                          END IF

                       CASE (2)   ! Zonal-vertical section
                          x1 = DBLE (ib-1) + (DBLE (jjt) - 0.5d0) / DBLE (ijt)
                          y1 = DBLE (jb)
                          z1 = DBLE (kb-1) + (DBLE (jkt) - 0.5d0) / DBLE (ikt)
                          IF (idir == 1) THEN
                             jb = ijst+1
                          ELSE IF (idir == -1) THEN
                             jb = ijst
                          END IF

                        CASE (3)   ! Horizontal section
                          x1 = DBLE (ib-1) + (DBLE (jjt) - 0.5d0) / DBLE (ijt)
                          y1 = DBLE (jb-1) + (DBLE (jkt) - 0.5d0) / DBLE (ikt)
                          z1 = DBLE (kb)
                          IF (idir == 1) THEN
                            kb = ikst+1
                          ELSE IF (idir == -1) THEN
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


                        ! TRACERS GO HERE [ADD!]
                        ! ****************  TRACERS INTERP *****************

                        ! Update trajectory numbers
                        ntractot = ntractot + 1
                        ntrac = ntractot

                        ! Only one particle for diagnistics purposes
                        IF ((loneparticle>0) .and. (ntrac.ne.loneparticle)) THEN
                          trajectories(ntrac)%active = .FALSE.
                          CYCLE kkkLoop
                        END IF

                        ! tfraction - time, fractions of ints
                        ! trelative - time [s] rel to start
                        tfraction = DBLE (ints)
                        trelative = tfraction * tseas

                        ! Put the new particle into the vectors trj and nrj ---
                        ! ---------------------------------------------------------
                        !trj(1:7,ntrac) = [ x1, y1, z1, trelative, subvol, 0.d0, tt ]
                        !nrj(1:5,ntrac) = [ ib, jb, kb,  0, IDINT(tfraction)]
                        !nrj(7,ntrac)=1

                        trajectories(ntrac)%x1 = x1
                        trajectories(ntrac)%y1 = y1
                        trajectories(ntrac)%z1 = z1
                        trajectories(ntrac)%tt = trelative
                        trajectories(ntrac)%subvol = subvol
                        trajectories(ntrac)%t0 = trelative

                        trajectories(ntrac)%ib = ib
                        trajectories(ntrac)%jb = jb
                        trajectories(ntrac)%kb = kb
                        trajectories(ntrac)%niter = 0
                        trajectories(ntrac)%nts = IDINT(tfraction)
                        trajectories(ntrac)%icycle = 1

                        ! LAPLACIAN GOES HERE [ADD!]
                        ! ******************lapu/lapv definition **************

                        !Save initial particle position
                        IF(log_level >= 3) THEN
                           PRINT*,' write initial trajectory position '
                        END IF

                        ! WRITE DATA GOES HERE [ADD!]
                        ! ****************** call writedata **************
                        ! CALL writedata(10)

                      END DO kkkLoop
                    END DO ijjLoop
              END DO startLoop

              IF (log_level >= 5) THEN
                PRINT*,' leaving seed'
              END IF

        END SUBROUTINE
        !-------------------------------------------------------------------------

END MODULE mod_seed
