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

END MODULE mod_tracerf
