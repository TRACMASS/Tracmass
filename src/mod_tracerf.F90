MODULE mod_tracerf

  !!------------------------------------------------------------------------------
  !!
  !!       MODULE: mod_tracerf
  !!
  !!       This module includes functions and subroutines to compute tracers
  !!       - thermo_dens0
  !!
  !!------------------------------------------------------------------------------

  IMPLICIT NONE

  CONTAINS

  FUNCTION thermo_dens0(T,S)
  ! --------------------------------------------------
  !
  ! Purpose:
  ! Compute sigma 0 density
  !
  ! Method:
  ! Compute the sigma 0 densitybased on the 1980
  ! equation of state (EOS-80)
  !
  ! --------------------------------------------------

      IMPLICIT NONE

      REAL, INTENT(IN)                         :: T(:,:,:)      ! Potential T [degC]
      REAL, INTENT(IN)                         :: S(:,:,:)      ! Practical S [PSU]

      REAL, ALLOCATABLE, DIMENSION (:,:,:)     :: T68

      REAL, ALLOCATABLE, DIMENSION (:,:,:)     :: thermo_dens0

      REAL, ALLOCATABLE, DIMENSION (:,:,:)     :: dens_temp

      INTEGER                                  :: nx, ny, nz

      REAL, PARAMETER                          :: a0 = 999.842594
      REAL, PARAMETER                          :: a1 =   6.793952e-2
      REAL, PARAMETER                          :: a2 =  -9.095290e-3
      REAL, PARAMETER                          :: a3 =   1.001685e-4
      REAL, PARAMETER                          :: a4 =  -1.120083e-6
      REAL, PARAMETER                          :: a5 =   6.536332e-9

      REAL, PARAMETER                          :: b0 =  8.24493e-1
      REAL, PARAMETER                          :: b1 = -4.0899e-3
      REAL, PARAMETER                          :: b2 =  7.6438e-5
      REAL, PARAMETER                          :: b3 = -8.2467e-7
      REAL, PARAMETER                          :: b4 =  5.3875e-9

      REAL, PARAMETER                          :: c0 = -5.72466e-3
      REAL, PARAMETER                          :: c1 = +1.0227e-4
      REAL, PARAMETER                          :: c2 = -1.6546e-6

      REAL, PARAMETER                          :: d0 = 4.8314e-4

      ! Size of array
      nx = SIZE(S,1); ny = SIZE(S,2); nz = SIZE(S,3);

      ALLOCATE( dens_temp(nx,ny,nz) ,thermo_dens0(nx,ny,nz), T68(nx,ny,nz) )

      ! Correct T to IPTS-68 standard
      T68 = 1.00024 * T

      dens_temp = a0+(a1+(a2+(a3+(a4+a5*T68)*T68)*T68)*T68)*T68
      thermo_dens0  = dens_temp &
           + (b0+(b1+(b2+(b3+b4*T68)*T68)*T68)*T68)*S &
           + (c0+(c1+c2*T68)*T68)*S*SQRT(S) + d0*S**2

  END FUNCTION thermo_dens0

  FUNCTION thermo_pt2ct(T,S)
  ! --------------------------------------------------
  !
  ! Purpose:
  ! Compute conservative temperature from Potential
  ! temperature
  !
  ! Method:
  ! TEOS_10 method gsw_ct_fron_pt
  !
  ! --------------------------------------------------

      IMPLICIT NONE

      REAL, INTENT(IN)                         :: T(:,:,:)      ! Potential T [degC]
      REAL, INTENT(IN)                         :: S(:,:,:)      ! Absolut S   [g/kg]

      REAL, ALLOCATABLE, DIMENSION (:,:,:)     :: thermo_pt2ct

      REAL, ALLOCATABLE, DIMENSION (:,:,:)     :: ct_temp, xS, xS2, yT

      REAL, PARAMETER                          :: gsw_sfac = 0.0248826675584615
      REAL, PARAMETER                          :: gsw_cp0  = 3991.86795711963

      INTEGER                                  :: nx, ny, nz

      ! Size of array
      nx = SIZE(S,1); ny = SIZE(S,2); nz = SIZE(S,3);

      ALLOCATE(thermo_pt2ct(nx,ny,nz), ct_temp(nx,ny,nz), xS(nx,ny,nz), xS2(nx,ny,nz), yT(nx,ny,nz))

      ! Calculation
      xS  = gsw_sfac*S
      xS2 = SQRT(xS)
      yT  = T*0.025        ! normalize for F03 and F08

      ct_temp =  61.01362420681071 + yT*(168776.46138048015 + &
                     yT*(-2735.2785605119625 + yT*(2574.2164453821433 + &
                     yT*(-1536.6644434977543 + yT*(545.7340497931629 + &
                     (-50.91091728474331 - 18.30489878927802*yT)*yT))))) + &
                     xS2*(268.5520265845071 + yT*(-12019.028203559312 + &
                     yT*(3734.858026725145 + yT*(-2046.7671145057618 + &
                     yT*(465.28655623826234 + (-0.6370820302376359 - &
                     10.650848542359153*yT)*yT)))) + &
                     xS*(937.2099110620707 + yT*(588.1802812170108 + &
                     yT*(248.39476522971285 + (-3.871557904936333 - &
                     2.6268019854268356*yT)*yT)) + &
                     xS*(-1687.914374187449 + xS*(246.9598888781377 + &
                     xS*(123.59576582457964 - 48.5891069025409*xS)) + &
                     yT*(936.3206544460336 + &
                     yT*(-942.7827304544439 + yT*(369.4389437509002 + &
                     (-33.83664947895248 - 9.987880382780322*yT)*yT))))))

      thermo_pt2ct = ct_temp/gsw_cp0

  END FUNCTION thermo_pt2ct

END MODULE mod_tracerf
