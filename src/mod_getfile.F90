MODULE mod_getfile
  !!------------------------------------------------------------------------------
  !!
  !!       MODULE: mod_getfile
  !!
  !!          Reads data from netCDF files
  !!
  !!          Functions included:
  !!
  !!              - get2DfieldNC
  !!              - get3DfieldNC
  !!
  !!------------------------------------------------------------------------------

#ifndef no_netcdf

  USE mod_grid
  USE mod_log
  USE netcdf

  IMPLICIT NONE

  INTEGER                                    :: ierr, varid,ncid

  PRIVATE :: error_getfieldNC

  CONTAINS

      FUNCTION get2DfieldNC(fieldFile ,varName, start2D, count2D, cextend)
      ! --------------------------------------------------
      !
      ! Purpose:
      ! Get 2D field data
      !
      ! --------------------------------------------------

          REAL, ALLOCATABLE,   DIMENSION(:,:)     :: get2DfieldNC
          REAL, ALLOCATABLE,   DIMENSION(:,:)     :: field
          REAL                                    :: scale_factor, add_offset

          INTEGER, DIMENSION(4)                   :: start2D  ,count2D

          CHARACTER (len=6), OPTIONAL             :: cextend

          CHARACTER (len=*)                       :: fieldFile ,varName

          ALLOCATE(field(count2D(1), count2D(2)))

          IF ( PRESENT(cextend)) THEN
                ALLOCATE(get2DfieldNC(1:imt, 1:jmt+1))
          ELSE
                ALLOCATE(get2DfieldNC(1:imt, 1:jmt))
          END IF

          ierr=NF90_OPEN(TRIM(fieldFile) ,NF90_NOWRITE ,ncid)
          IF(ierr .NE. 0) CALL error_getfieldNC(1,fieldFile,varName)
          ierr=NF90_INQ_VARID(ncid ,varName ,varid)
          IF(ierr .NE. 0) CALL error_getfieldNC(2,fieldFile,varName)

          IF ( (l_subdom) .AND. (imindom > imaxdom) ) THEN
              start2D(1) = imindom; count2D(1) = imthalf1
              ierr=NF90_GET_VAR(ncid ,varid ,field(1:imthalf1,:), start2D, count2D )
              start2D(1) = 1; count2D(1) = imthalf2
              ierr=NF90_GET_VAR(ncid ,varid ,field(imthalf1+1:imt,:), start2D, count2D )
          ELSE
              ierr=NF90_GET_VAR(ncid ,varid ,field, start2D, count2D )
          END IF
          IF(ierr .NE. 0) CALL error_getfieldNC(3,fieldFile,varName)

          ierr = NF90_GET_ATT(ncid, varid,"scale_factor", scale_factor)
          IF(ierr .NE. 0) scale_factor = 1.0
          ierr = NF90_GET_ATT(ncid, varid,"add_offset", add_offset)
          IF(ierr .NE. 0) add_offset = 0.0

          ierr=NF90_CLOSE(ncid)
          IF(ierr .NE. 0) CALL error_getfieldNC(4,fieldFile,varName)

          get2DfieldNC(:,:) = field(:,:)*scale_factor + add_offset

       END FUNCTION get2DfieldNC

       FUNCTION get3DfieldNC(fieldFile ,varName, start3D, count3D, stcase, cextend)
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

           CHARACTER (len=6), OPTIONAL               :: cextend
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

           IF ( PRESENT(cextend)) THEN
                 ALLOCATE(get3DfieldNC(1:imt, 1:jmt+1, 1:km))
           ELSE
                 ALLOCATE(get3DfieldNC(1:imt, 1:jmt, 1:km))
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
                  ierr=NF90_GET_VAR(ncid, varid, field(:,:,1:imtdom), ss, cc)
                  ss(3) = 1; cc(3) = imthalf2
                  ierr=NF90_GET_VAR(ncid ,varid ,field(:,:,imtdom+1:imt), ss, cc )
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
              DO ii = 1, imt
                 DO kk = 1, km
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
                  PRINT*,'Could not find the following file:',TRIM(fieldFile)
                  STOP
              CASE(2)
                  PRINT*,'ERROR:'
                  PRINT*,'Could not find the following variable',TRIM(varName)
                  PRINT*,'in file', TRIM(fieldFile)
                  STOP
              CASE(3)
                  PRINT*,'ERROR:'
                  PRINT*,'The dimensions of variable',TRIM(varName),' do not match'
                  STOP
              CASE(4)
                  PRINT*,'ERROR:'
                  PRINT*,'Could not close the following file:',TRIM(fieldFile)
                  STOP
              CASE DEFAULT
                RETURN

          END SELECT

        END SUBROUTINE error_getfieldNC

#endif
END MODULE mod_getfile
