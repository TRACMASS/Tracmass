MODULE mod_getfile
  !!------------------------------------------------------------------------------
  !!
  !!       MODULE: mod_getfile
  !!
  !!          Reads data from netCDF files
  !!
  !!          Subroutines included:
  !!------------------------------------------------------------------------------

#ifndef no_netcdf

  USE mod_grid
  USE mod_log
  USE netcdf

  IMPLICIT NONE

  INTEGER                                    :: ierr, varid,ncid

  CONTAINS


  FUNCTION get2DfieldNC(fieldFile ,varName, start2D, count2D)
  ! --------------------------------------------------
  !
  ! Purpose:
  ! Get 2D field data
  !
  ! --------------------------------------------------

      REAL, ALLOCATABLE,   DIMENSION(:,:)     :: get2DfieldNC
      CHARACTER (len=*)                       :: fieldFile ,varName
      INTEGER, DIMENSION(4)                   :: start2D  ,count2D

      REAL, ALLOCATABLE,   DIMENSION(:,:)     :: field

      ALLOCATE(field(count2D(1)-start2D(1)+1, count2D(2)-start2D(2)+1))
      ALLOCATE(get2DfieldNC(1:imt, 1:jmt))

      ierr=NF90_OPEN(TRIM(fieldFile) ,NF90_NOWRITE ,ncid)
      IF(ierr .NE. 0) STOP 1
      ierr=NF90_INQ_VARID(ncid ,varName ,varid)
      IF(ierr .NE. 0) STOP 2
      ierr=NF90_GET_VAR(ncid ,varid ,field, start2D, count2D )
      IF(ierr .NE. 0) STOP 3
      ierr=NF90_CLOSE(ncid)
      IF(ierr .NE. 0) STOP 4

      get2DfieldNC(:,:) = field(:,:)

   END FUNCTION get2DfieldNC

   FUNCTION get3DfieldNC(fieldFile ,varName, start3D, count3D, stcase)
   ! --------------------------------------------------
   !
   ! Purpose:
   ! Get 3D field data
   !
   ! --------------------------------------------------

       REAL, ALLOCATABLE,   DIMENSION(:,:,:)     :: get3DfieldNC
       CHARACTER (len=*)                         :: fieldFile ,varName, stcase
       INTEGER, DIMENSION(4)                     :: start3D,count3D, ss, cc
       INTEGER                                   :: ii, kk

       REAL, ALLOCATABLE,   DIMENSION(:,:,:)       :: field
       REAL, ALLOCATABLE,   DIMENSION(:,:,:,:)     :: field4

       IF (stcase == 'st')  THEN
            ALLOCATE(field(count3D(1)-start3D(1)+1, count3D(2)-start3D(2)+1,count3D(3)-start3D(3)+1))
       ELSE IF (stcase == 'ts')  THEN
            ALLOCATE(field4(1,count3D(1)-start3D(1)+1, count3D(2)-start3D(2)+1,count3D(3)-start3D(3)+1))
       ELSE IF (stcase == 'st_r') THEN
            ALLOCATE(field(count3D(3)-start3D(3)+1, count3D(2)-start3D(2)+1,count3D(1)-start3D(1)+1))
       ELSE IF (stcase == 'ts_r') THEN
            ALLOCATE(field4(1,count3D(3)-start3D(3)+1, count3D(2)-start3D(2)+1,count3D(1)-start3D(1)+1))
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

       ALLOCATE(get3DfieldNC(1:imt, 1:jmt, 1:km))

       ierr=NF90_OPEN(TRIM(fieldFile), NF90_NOWRITE, ncid)
       IF(ierr .NE. 0) STOP 1
       ierr=NF90_INQ_VARID(ncid, varName, varid)
       IF(ierr .NE. 0) STOP 2
       IF (stcase == 'st' .OR. stcase == 'st_r') ierr=NF90_GET_VAR(ncid, varid, field, ss, cc)
       IF (stcase == 'ts' .OR. stcase == 'ts_r') ierr=NF90_GET_VAR(ncid, varid, field4, ss, cc)
       IF(ierr .NE. 0) STOP 3
       ierr=NF90_CLOSE(ncid)
       IF(ierr .NE. 0) STOP 4

       IF (stcase == 'st') THEN
          get3DfieldNC(:,:,:) = field(:,:,:)
       ELSE IF (stcase == 'ts') THEN
          get3DfieldNC(:,:,:) = field4(1,:,:,:)
       ELSE IF (stcase == 'st_r' .OR. stcase == 'ts_r') THEN
          DO ii = 1, imt
             DO kk = 1, km
               IF (stcase == 'st_r') get3DfieldNC(ii,:,kk) = field(kk,:,ii)
               IF (stcase == 'ts_r') get3DfieldNC(ii,:,kk) = field4(1,kk,:,ii)
             END DO
          END DO
       END IF

    end function get3DfieldNC

#endif
END MODULE mod_getfile
