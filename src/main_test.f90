PROGRAM main
! This is the man program

        use mod_test

        IMPLICIT NONE

        INTEGER :: x ! Argument 1
        INTEGER :: y ! Argument 2

        INTEGER :: ierr ! Error argument

        ! Define arguments
        x = 0; y = 1

        CALL test1(x,y,ierr)
        PRINT*, test2(ierr)

        ! Change arguments
        x = 0; y = 0

        CALL test1(x,y,ierr)
        PRINT*, test2(ierr)


END PROGRAM main
