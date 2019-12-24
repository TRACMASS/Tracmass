MODULE mod_test

      IMPLICIT NONE

      CONTAINS

      SUBROUTINE test1(x,y,ierr)
      ! This is a subroutine that tests x and y

          INTEGER, INTENT(IN) :: x ! Input number 1
          INTEGER, INTENT(IN) :: y ! Input number 2

          INTEGER, INTENT(OUT) :: ierr ! Output


          ierr = 0
          IF (x /= y) ierr = 1

      END SUBROUTINE

      FUNCTION test2 (ierr)
      ! This is a test function

          IMPLICIT NONE

          ! Arguments
          INTEGER, INTENT(IN)   :: ierr ! Argument
          CHARACTER(len=40)     :: test2 ! result

          test2 = '============ NO PASS =============='
          IF (ierr == 0) test2 = '============= PASS ================'

      END FUNCTION test2

END MODULE mod_test
