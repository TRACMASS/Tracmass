MODULE mod_getfile
  !!------------------------------------------------------------------------------
  !!
  !!       MODULE: mod_getfile
  !!
  !!          Reads data from netCDF files
  !!
  !!          Functions included:
  !!
  !!              - filledFileName
  !!              - getScalarNC
  !!              - get1DfieldNC
  !!              - get2DfieldNC
  !!              - get3DfieldNC
  !!
  !!              - error_getfieldNC (P)
  !!
  !!------------------------------------------------------------------------------

#ifndef no_netcdf

  USE mod_grid
  USE mod_log
  USE netcdf
  USE mod_time

  IMPLICIT NONE

  INTEGER                                    :: ierr, varid,ncid

  PRIVATE :: error_getfieldNC

  CONTAINS

      FUNCTION filledFileName(filePattern, inyear, inmon, inday)
      ! --------------------------------------------------
      !
      ! Purpose:
      ! Take filepattern and fill in year, month, day etc.
      !
      ! --------------------------------------------------

          CHARACTER (len=*)                       :: filePattern
          CHARACTER (len=LEN(filePattern))        :: filledFileName
          CHARACTER (len=8)                       :: timestamp_yyyymmdd

          INTEGER                                 :: inyear, inmon, inday
          INTEGER                                 :: ichar

          filledFileName = filePattern

          WRITE(timestamp_yyyymmdd(1:4),'(I4)') inyear
          IF (inyear<10) WRITE(timestamp_yyyymmdd(1:4),'(A3,I1)') '000',inyear
          IF (inyear<100  .AND. inyear >=10) WRITE(timestamp_yyyymmdd(1:4),'(A2,I2)') '00',inyear
          IF (inyear<1000 .AND. inyear >=100) WRITE(timestamp_yyyymmdd(1:4),'(A1,I3)') '0',inyear

          WRITE(timestamp_yyyymmdd(5:6),'(I2)') inmon
          IF (inmon<10) WRITE(timestamp_yyyymmdd(5:6),'(A1,I1)') '0',inmon
          WRITE(timestamp_yyyymmdd(7:8),'(I2)') inday
          IF (inday<10) WRITE(timestamp_yyyymmdd(7:8),'(A1,I1)') '0',inday

          ! Filled with calendar
          ichar = INDEX(filledFileName,'YYYYMMDD')
          DO WHILE (ichar /= 0)
              filledFileName = TRIM(filledFileName(:ichar-1))//TRIM(timestamp_yyyymmdd)//TRIM(filledFileName(ichar+8:))
              ichar = INDEX(filledFileName,'YYYYMMDD')
          END DO

          ! Filled with years
          ichar = INDEX(filledFileName,'YYYY')
          DO WHILE (ichar /= 0)
              WRITE(filledFileName(ichar:ichar+3),'(A4)') timestamp_yyyymmdd(1:4)
              ichar = INDEX(filledFileName,'YYYY')
          END DO

          ! Filled with month
          ichar = INDEX(filledFileName,'MM')
          DO WHILE (ichar /= 0)
              WRITE(filledFileName(ichar:ichar+1),'(A2)') timestamp_yyyymmdd(5:6)
              ichar = INDEX(filledFileName,'MM')
          END DO

          ! Filled with day
          ichar = INDEX(filledFileName,'DD')
          DO WHILE (ichar /= 0)
              write(filledFileName(ichar:ichar+1),'(A2)') timestamp_yyyymmdd(7:8)
              ichar = INDEX(filledFileName,'DD')
          END DO

      END FUNCTION filledFileName

      FUNCTION getScalarNC(fieldFile ,varName)
        CHARACTER (len=*)                       :: fieldFile ,varName
        REAL                                    :: getScalarNC
        INTEGER                                 :: varid ,ncid

        ierr=NF90_OPEN(TRIM(fieldFile) ,NF90_NOWRITE ,ncid)
        IF(ierr .NE. 0) CALL error_getfieldNC(1,fieldFile,varName)
        ierr=NF90_INQ_VARID(ncid ,varName ,varid)
        IF(ierr .NE. 0) CALL error_getfieldNC(2,fieldFile,varName)
        ierr=NF90_GET_VAR(ncid ,varid ,getScalarNC)
        IF(ierr .NE. 0) CALL error_getfieldNC(3,fieldFile,varName)
        ierr=NF90_CLOSE(ncid)
        IF(ierr .NE. 0) CALL error_getfieldNC(4,fieldFile,varName)

      END FUNCTION getScalarNC

      FUNCTION get1DfieldNC(fieldFile ,varName, start1D, count1D)
      ! --------------------------------------------------
      !
      ! Purpose:
      ! Get 1D field data
      !
      ! --------------------------------------------------

          REAL, ALLOCATABLE,   DIMENSION(:)       :: get1DfieldNC
          REAL, ALLOCATABLE,   DIMENSION(:)       :: field
          REAL                                    :: scale_factor, add_offset

          INTEGER                                 :: start1D  ,count1D

          CHARACTER (len=*)                       :: fieldFile ,varName

          ALLOCATE(field(count1D),get1DfieldNC(count1D))

          ierr=NF90_OPEN(TRIM(fieldFile) ,NF90_NOWRITE ,ncid)
          IF(ierr .NE. 0) CALL error_getfieldNC(1,fieldFile,varName)
          ierr=NF90_INQ_VARID(ncid ,varName ,varid)
          IF(ierr .NE. 0) CALL error_getfieldNC(2,fieldFile,varName)

          ierr=NF90_GET_VAR(ncid ,varid ,field, [start1D], [count1D] )
          IF(ierr .NE. 0) CALL error_getfieldNC(3,fieldFile,varName)

          ierr = NF90_GET_ATT(ncid, varid,"scale_factor", scale_factor)
          IF(ierr .NE. 0) scale_factor = 1.0
          ierr = NF90_GET_ATT(ncid, varid,"add_offset", add_offset)
          IF(ierr .NE. 0) add_offset = 0.0

          ierr=NF90_CLOSE(ncid)
          IF(ierr .NE. 0) CALL error_getfieldNC(4,fieldFile,varName)

          get1DfieldNC(:) = field(:)*scale_factor + add_offset

      END FUNCTION get1DfieldNC

      FUNCTION get2DfieldNC(fieldFile ,varName, start2D, count2D, stcase)
      ! --------------------------------------------------
      !
      ! Purpose:
      ! Get 2D field data
      !
      ! --------------------------------------------------

          REAL, ALLOCATABLE,   DIMENSION(:,:)     :: get2DfieldNC
          REAL, ALLOCATABLE,   DIMENSION(:,:)     :: field
          REAL                                    :: scale_factor, add_offset

          INTEGER, DIMENSION(4)                   :: start2D  ,count2D, ss, cc
          INTEGER                                 :: ii

          CHARACTER (len=*)                       :: fieldFile ,varName, stcase

          IF (stcase == 'st')  THEN
               ALLOCATE(field(count2D(1), count2D(2)))
          ELSE IF (stcase == 'st_r') THEN
               ALLOCATE(field(count2D(2), count2D(1)))
          END IF

          ALLOCATE(get2DfieldNC(count2D(1),count2D(2)))

          IF (stcase == 'st') THEN
             ss = start2D
             cc = count2D
          ELSE IF (stcase == 'st_r') THEN
             ss(1) = start2D(2); ss(2) = start2D(1); ss(3) = start2D(3); ss(4) = start2D(4)
             cc(1) = count2D(2); cc(2) = count2D(1); cc(3) = count2D(3); cc(4) = count2D(4)
          END IF

          ierr=NF90_OPEN(TRIM(fieldFile) ,NF90_NOWRITE ,ncid)
          IF(ierr .NE. 0) CALL error_getfieldNC(1,fieldFile,varName)
          ierr=NF90_INQ_VARID(ncid ,varName ,varid)
          IF(ierr .NE. 0) CALL error_getfieldNC(2,fieldFile,varName)

          IF ( (l_subdom) .AND. (imindom > imaxdom) ) THEN

              IF (stcase == 'st') THEN
                 ss(1) = imindom; cc(1) = imthalf1
                 ierr=NF90_GET_VAR(ncid, varid, field(1:imthalf1,:), ss, cc)
                 ss(1) = 1; cc(1) = imthalf2
                 ierr=NF90_GET_VAR(ncid ,varid ,field(imthalf1+1:imt,:), ss, cc )
              ELSE IF (stcase == 'st_r') THEN
                 ss(2) = imindom; cc(2) = imthalf1
                 ierr=NF90_GET_VAR(ncid, varid, field(:,1:imthalf1), ss, cc)
                 ss(2) = 1; cc(2) = imthalf2
                 ierr=NF90_GET_VAR(ncid ,varid ,field(:,imthalf1+1:imt), ss, cc )
              END IF

          ELSE
              ierr=NF90_GET_VAR(ncid ,varid ,field, ss, cc )
          END IF

          IF(ierr .NE. 0) CALL error_getfieldNC(3,fieldFile,varName)

          ierr = NF90_GET_ATT(ncid, varid,"scale_factor", scale_factor)
          IF(ierr .NE. 0) scale_factor = 1.0
          ierr = NF90_GET_ATT(ncid, varid,"add_offset", add_offset)
          IF(ierr .NE. 0) add_offset = 0.0

          ierr=NF90_CLOSE(ncid)
          IF(ierr .NE. 0) CALL error_getfieldNC(4,fieldFile,varName)

          IF (stcase == 'st') THEN
             get2DfieldNC(:,:) = field(:,:)*scale_factor + add_offset
          ELSE IF (stcase == 'st_r') THEN
             DO ii = 1, count2D(1)
                get2DfieldNC(ii,:) = field(:,ii)*scale_factor + add_offset
             END DO
          END IF
       END FUNCTION get2DfieldNC

       FUNCTION get3DfieldNC(fieldFile ,varName, start3D, count3D, stcase)
       ! --------------------------------------------------
       !
       ! Purpose:
       ! Get 3D field data
       !
       ! --------------------------------------------------

           REAL, ALLOCATABLE,   DIMENSION(:,:,:)       :: get3DfieldNC
           REAL, ALLOCATABLE,   DIMENSION(:,:,:)       :: field
           REAL, ALLOCATABLE,   DIMENSION(:,:,:,:)     :: field4
           REAL                                        :: scale_factor, add_offset

           INTEGER, DIMENSION(4)                     :: start3D,count3D, ss, cc
           INTEGER                                   :: ii, kk

           CHARACTER (len=*)                         :: fieldFile ,varName, stcase

           IF (stcase == 'st')  THEN
                ALLOCATE(field(count3D(1), count3D(2),count3D(3)))
           ELSE IF (stcase == 'ts')  THEN
                ALLOCATE(field4(1,count3D(1), count3D(2),count3D(3)))
           ELSE IF (stcase == 'st_r') THEN
                ALLOCATE(field(count3D(3), count3D(2),count3D(1)))
           ELSE IF (stcase == 'ts_r') THEN
                ALLOCATE(field4(1,count3D(3), count3D(2),count3D(1)))
           END IF

           ALLOCATE(get3DfieldNC(count3D(1), count3D(2),count3D(3)))

           IF (stcase == 'st') THEN
              ss = start3D
              cc = count3D
           ELSE IF (stcase == 'ts') THEN
              ss(1) = start3D(4); ss(2:4) = start3D(1:3)
              cc(1) = count3D(4); cc(2:4) = count3D(1:3)
           ELSE IF (stcase == 'st_r') THEN
              ss(1) = start3D(3); ss(2) = start3D(2); ss(3) = start3D(1); ss(4) = start3D(4)
              cc(1) = count3D(3); cc(2) = count3D(2); cc(3) = count3D(1); cc(4) = count3D(4)
           ELSE IF (stcase == 'ts_r') THEN
              ss(1) = start3D(4); ss(2) = start3D(3); ss(3) = start3D(2); ss(4) = start3D(1)
              cc(1) = count3D(4); cc(2) = count3D(3); cc(3) = count3D(2); cc(4) = count3D(1)
           END IF

           ierr=NF90_OPEN(TRIM(fieldFile), NF90_NOWRITE, ncid)
           IF(ierr .NE. 0) CALL error_getfieldNC(1,fieldFile,varName)
           ierr=NF90_INQ_VARID(ncid, varName, varid)
           IF(ierr .NE. 0) CALL error_getfieldNC(2,fieldFile,varName)
           IF ( (l_subdom) .AND. (imindom > imaxdom) ) THEN

               IF (stcase == 'st') THEN
                  ss(1) = imindom; cc(1) = imthalf1
                  ierr=NF90_GET_VAR(ncid, varid, field(1:imthalf1,:,:), ss, cc)
                  ss(1) = 1; cc(1) = imthalf2
                  ierr=NF90_GET_VAR(ncid ,varid ,field(imthalf1+1:imt,:,:), ss, cc )
               ELSE IF (stcase == 'st_r') THEN
                  ss(3) = imindom; cc(3) = imthalf1
                  ierr=NF90_GET_VAR(ncid, varid, field(:,:,1:imthalf1), ss, cc)
                  ss(3) = 1; cc(3) = imthalf2
                  ierr=NF90_GET_VAR(ncid ,varid ,field(:,:,imthalf1+1:imt), ss, cc )
               ELSE IF (stcase == 'ts') THEN
                  ss(2) = imindom; cc(2) = imthalf1
                  ierr=NF90_GET_VAR(ncid, varid, field4(:,1:imthalf1,:,:), ss, cc)
                  ss(2) = 1; cc(2) = imthalf2
                  ierr=NF90_GET_VAR(ncid ,varid ,field4(:,imthalf1+1:imt,:,:), ss, cc )
               ELSE IF (stcase == 'ts_r') THEN
                 ss(4) = imindom; cc(4) = imthalf1
                 ierr=NF90_GET_VAR(ncid, varid, field4(:,:,:,1:imthalf1), ss, cc)
                 ss(4) = 1; cc(4) = imthalf2
                 ierr=NF90_GET_VAR(ncid ,varid ,field4(:,:,:,imthalf1+1:imt), ss, cc )
               END IF

           ELSE
               IF (stcase == 'st' .OR. stcase == 'st_r') ierr=NF90_GET_VAR(ncid, varid, field, ss, cc)
               IF (stcase == 'ts' .OR. stcase == 'ts_r') ierr=NF90_GET_VAR(ncid, varid, field4, ss, cc)

           END IF

           IF(ierr .NE. 0) CALL error_getfieldNC(3,fieldFile,varName)

           ierr = NF90_GET_ATT(ncid, varid,"scale_factor", scale_factor)
           IF(ierr .NE. 0) scale_factor = 1.0
           ierr = NF90_GET_ATT(ncid, varid,"add_offset", add_offset)
           IF(ierr .NE. 0) add_offset = 0.0

           ierr=NF90_CLOSE(ncid)
           IF(ierr .NE. 0) CALL error_getfieldNC(4,fieldFile,varName)

           IF (stcase == 'st') THEN
              get3DfieldNC(:,:,:) = field(:,:,:)*scale_factor + add_offset
           ELSE IF (stcase == 'ts') THEN
              get3DfieldNC(:,:,:) = field4(1,:,:,:)*scale_factor + add_offset
           ELSE IF (stcase == 'st_r' .OR. stcase == 'ts_r') THEN
              DO ii = 1, count3D(1)
                 DO kk = 1, count3D(3)
                   IF (stcase == 'st_r') get3DfieldNC(ii,:,kk) = field(kk,:,ii)*scale_factor + add_offset
                   IF (stcase == 'ts_r') get3DfieldNC(ii,:,kk) = field4(1,kk,:,ii)*scale_factor + add_offset
                 END DO
              END DO
           END IF

        END FUNCTION get3DfieldNC

        SUBROUTINE error_getfieldNC(ierror,fieldFile,varName)
        ! --------------------------------------------------
        !
        ! Purpose:
        ! Describe the possible errors after reading the netCDF file
        !
        ! --------------------------------------------------

          INTEGER, INTENT(IN)              :: ierror
          CHARACTER (len=*), INTENT(IN)    :: fieldFile ,varName

          SELECT CASE(ierror)

              CASE(1)
                  PRINT*,'ERROR:'
                  PRINT*,'Could not find the following file: ',TRIM(fieldFile)
                  STOP
              CASE(2)
                  PRINT*,'ERROR:'
                  PRINT*,'Could not find the following variable ',TRIM(varName)
                  PRINT*,'in file', TRIM(fieldFile)
                  STOP
              CASE(3)
                  PRINT*,'ERROR:'
                  PRINT*,'The dimensions of variable ',TRIM(varName),' do not match'
                  STOP
              CASE(4)
                  PRINT*,'ERROR:'
                  PRINT*,'Could not close the following file: ',TRIM(fieldFile)
                  STOP
              CASE DEFAULT
                RETURN

          END SELECT

        END SUBROUTINE error_getfieldNC

#endif
END MODULE mod_getfile
