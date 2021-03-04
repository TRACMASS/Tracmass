test_suite mod_pos

! test_suite for mod_pos_tanalytical
! Contains  tests
!  5 --> azer
!  3 --> aneg
!  3 --> apos
!  1 --> daw
!  1 --> fun_erf
!  1 --> fun_erfc

SETUP

  ! Grid information
  imt = 10; jmt = 10; km = 10

  ! Velocities
  dsn = 0.; dss = 0.; dsu = 0.; dsd = 0.

  ! No stream functions
  l_psi     = .FALSE.
  l_offline = .TRUE.

  ! jperio
  jperio = 0

  ! dstep
  dstep = 1.

  ! time setting
  ts = 0.d0; tseas = 1.d0; tt = 0.d0

  ! Volume
  dxyz = 1.d0

END SETUP

TEARDOWN
  ! This code runs immediately after each test

END TEARDOWN

TEST test_azer_1

   ! Test for azer
   ! alfa = 0, beta = 0, gamma = 0

   REAL(DP) :: rw, sw

   PRINT *, ' * Test azer    :  alfa = 0, beta = 0, gamma = 0'

   CALL azer(2,1,1.d0,1.d0,1.d0,1.d0,1.5d0,rw,0.d0,0.d0,sw)

   ASSERT_EQUAL(rw, 2)
   ASSERT_EQUAL(sw, 0.5)

END TEST

TEST test_azer_2

   ! Test for azer
   ! alfa = 0, beta = 0, gamma /= 0

   REAL(DP) :: rw, sw

   PRINT *, ' * Test azer    :  alfa = 0, beta = 0, gamma /= 0'

   CALL azer(2,1,1.d0,1.d0,0.25d0,0.25d0,1.5d0,rw,0.d0,0.d0,sw)

   ASSERT_EQUAL(rw, 2)
   ASSERT_REAL_EQUAL(sw, 2./3.)

END TEST

TEST test_azer_3

    ! Test for azer
    ! alfa = 0, beta /= 0, gamma = 0

    REAL(DP) :: rw, sw

    PRINT *, ' * Test azer    :  alfa = 0, beta /= 0, gamma = 0'

    CALL azer(2,1,0.25d0,1.d0,0.25d0,1.d0,1.5d0,rw,0.d0,0.d0,sw)

    ASSERT_EQUAL(rw, 2)
    ASSERT_REAL_EQUAL(sw,-LOG(0.4)/0.75)

END TEST

TEST test_azer_4

    ! Test for azer
    ! alfa = 0, beta /= 0, gamma /= 0

    REAL(DP) :: rw, sw

    PRINT *, ' * Test azer    :  alfa = 0, beta /= 0, gamma /= 0'

    CALL azer(2,1,0.6d0,0.8d0,0.4d0,0.6d0,1.5d0,rw,0.d0,0.d0,sw)

    ASSERT_EQUAL(rw, 2)
    ASSERT_EQUAL_WITHIN(sw,0.899,1e-2)

END TEST

TEST test_azer_5

    ! Test for azer
    ! alfa = 0, beta /= 0, gamma /= 0

    REAL(DP) :: rw, sw

    PRINT *, ' * Test azer    :  alfa = 0, beta /= 0, gamma /= 0 (no solution)'

    CALL azer(2,1,0.5d0,0.75d0,0.25d0,0.5d0,1.5d0,rw,0.d0,0.d0,sw)

    ASSERT_EQUAL(rw, -99)
    ASSERT_EQUAL(sw,UNDEF)

END TEST

TEST test_aneg_1

    ! Test for aneg
    ! alfa < 0 (land point at ii)

    REAL(DP) :: rw, sw

    PRINT *, ' * Test aneg    :  alfa < 0 (land point at ii)'

    CALL aneg(2,1,0.d0,0.1d0,0.d0,-1.5d0,1.5d0,rw,0.d0,0.d0,sw)

    ASSERT_EQUAL(rw, 1)
    ASSERT_EQUAL_WITHIN(sw,0.99,1e-2)

END TEST

TEST test_aneg_2

    ! Test for aneg
    ! alfa < 0 (land point at iim)

    REAL(DP) :: rw, sw

    PRINT *, ' * Test aneg    :  alfa < 0 (land point at iim)'

    CALL aneg(2,1,-0.5d0,0.0d0,2.d0,0.d0,1.5d0,rw,0.d0,0.d0,sw)

    ASSERT_EQUAL(rw, 2)
    ASSERT_EQUAL_WITHIN(sw,0.97,1e-2)

END TEST

TEST test_aneg_3

    ! Test for aneg
    ! alfa < 0

    REAL(DP) :: rw, sw

    PRINT *, ' * Test aneg    :  alfa < 0'

    CALL aneg(2,1,-0.1d0,0.1d0,2.d0,1.d0,1.5d0,rw,0.d0,0.d0,sw)

    ASSERT_EQUAL(rw, 2)
    ASSERT_EQUAL_WITHIN(sw,0.76,1e-2)

    PRINT *, ' * Test aneg    :  alfa < 0 (no solution)'

    CALL aneg(2,1,-0.1d0,0.1d0,0.5d0,0.1d0,1.5d0,rw,0.d0,0.d0,sw)

    ASSERT_EQUAL(rw, -99)
    ASSERT_EQUAL(sw,UNDEF)

END TEST

TEST test_apos_1

    ! Test for apos
    ! alfa > 0 (land point at ii)

    REAL(DP) :: rw, sw

    PRINT *, ' * Test apos    :  alfa > 0 (land point at ii)'

    CALL apos(2,1,0.d0,-1.5d0,0.d0,0.1d0,1.5d0,rw,0.d0,0.d0,sw)

    ASSERT_EQUAL(rw, 1)
    ASSERT_EQUAL_WITHIN(sw,0.82,1e-2)

END TEST

TEST test_apos_2

    ! Test for apos
    ! alfa > 0 (land point at iim)

    REAL(DP) :: rw, sw

    PRINT *, ' * Test apos    :  alfa > 0 (land point at iim)'

    CALL apos(2,1,2.d0,0.0d0,-.5d0,0.d0,1.5d0,rw,0.d0,0.d0,sw)

    ASSERT_EQUAL(rw, 2)
    ASSERT_EQUAL_WITHIN(sw,0.50,1e-2)

END TEST

TEST test_apos_3

    ! Test for apos
    ! alfa > 0

    REAL(DP) :: rw, sw

    PRINT *, ' * Test apos    :  alfa > 0'

    CALL apos(2,1,2.d0,1.0d0,-0.1d0,0.1d0,1.5d0,rw,0.d0,0.d0,sw)

    ASSERT_EQUAL(rw, 2)
    ASSERT_EQUAL_WITHIN(sw,0.35,1e-2)

    PRINT *, ' * Test apos    :  alfa > 0 (no solution)'

    CALL apos(2,1,0.5d0,0.1d0,-0.1d0,0.1d0,1.5d0,rw,0.d0,0.d0,sw)

    ASSERT_EQUAL(rw, -99)
    ASSERT_EQUAL(sw,UNDEF)

END TEST

TEST test_daw

    ! Test for daw
    ! Dawson function

    PRINT *, ' * Test daw     :  Dawson function'

    ASSERT_EQUAL_WITHIN(daw(0.d0),0.d0,1e-2)
    ASSERT_EQUAL_WITHIN(daw(1.d0),0.538d0,1e-2)
    ASSERT_EQUAL_WITHIN(daw(10.d0),0.050d0,1e-2)
    ASSERT_EQUAL_WITHIN(daw(5.5d0),0.092d0,1e-2)

END TEST

TEST test_fun_erf

    ! Test for fun_erf
    ! erf function

    PRINT *, ' * Test fun_erf :  Erf function'

    ASSERT_EQUAL_WITHIN(fun_erf(0.d0),ERF(0.d0),1e-2)
    ASSERT_EQUAL_WITHIN(fun_erf(1.d0),ERF(1.d0),1e-2)
    ASSERT_EQUAL_WITHIN(fun_erf(10.d0),ERF(10.d0),1e-2)
    ASSERT_EQUAL_WITHIN(fun_erf(5.5d0),ERF(5.5d0),1e-2)
    ASSERT_EQUAL_WITHIN(fun_erf(1.2d0),ERF(1.2d0),1e-2)

END TEST

TEST test_fun_erfc

    ! Test for fun_erfc
    ! erfc function

    PRINT *, ' * Test fun_erfc:  Erfc function'

    ASSERT_EQUAL_WITHIN(fun_erfc(0.d0),ERFC(0.d0),1e-2)
    ASSERT_EQUAL_WITHIN(fun_erfc(1.d0),ERFC(1.d0),1e-2)
    ASSERT_EQUAL_WITHIN(fun_erfc(10.d0),ERFC(10.d0),1e-2)
    ASSERT_EQUAL_WITHIN(fun_erfc(5.5d0),ERFC(5.5d0),1e-2)
    ASSERT_EQUAL_WITHIN(fun_erfc(1.2d0),ERFC(1.2d0),1e-2)

END TEST

end test_suite
