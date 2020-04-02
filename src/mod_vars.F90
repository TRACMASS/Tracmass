!!---------------------------------------------------------------------------
!!
!!       MODULE mod_vars:
!!
!!          Collection of different modules that define variables
!!
!!          Subroutines included:
!!              - mod_precdef
!!              - mod_log
!!              - mod_param
!!              - mod_trajdef
!!              - mod_loopvars
!!              - mod_traj
!!              - mod_grid
!!              - mod_time
!!              - mod_domain
!!              - mod_vel
!!              - mod_psi
!!
!!---------------------------------------------------------------------------


! Precision definitions
MODULE mod_precdef
   INTEGER, PARAMETER                       :: PP = selected_real_kind(6 ,  37)
   INTEGER, PARAMETER                       :: DP = selected_real_kind(15, 307)
   INTEGER, PARAMETER                       :: QP = selected_real_kind(33, 4931)
ENDMODULE mod_precdef

! Verbose options
MODULE mod_log
  USE mod_precdef

  INTEGER                                   :: log_level = 0
END MODULE

! General parameters
MODULE mod_param
  USE mod_precdef

  INTEGER                                   :: write_frec ! Writing frecuency
  INTEGER                                   :: iter       ! Number of subcycles
  INTEGER                                   :: ngcm       ! Time step in hours
  INTEGER                                   :: ngcm_step  ! Size of the time step
  INTEGER                                   :: ngcm_unit  ! Unit of the time step

  REAL(DP), PARAMETER                       :: UNDEF=1.d20
  REAL(DP), PARAMETER                       :: EPS=1.d-10

  REAL(DP), PARAMETER                       :: grav = 9.81
  REAL(DP), PARAMETER                       :: PI = 4.d0*ATAN(1.d0)
  REAL(DP), PARAMETER                       :: radius = 6371229.d0
  REAL(DP), PARAMETER                       :: radian = pi/180.d0
  REAL(DP), PARAMETER                       :: deg = radius*radian
  REAL(DP), PARAMETER                       :: tday = 24.d0 * 3600.d0
ENDMODULE mod_param

! Define derived type "trajectory"
MODULE mod_trajdef
   USE mod_precdef

   TYPE trajectory
      INTEGER                               :: ia,ja,ka,ib,jb,kb  !! grid indices
      INTEGER                               :: nts                !! time step
      INTEGER                               :: niter              !! trajectory iterations
      INTEGER                               :: icycle             !! 0=keep advecting particle
                                                                  !! 1=stop and update model fields
      INTEGER                               :: lbas               !! boundary flag

      REAL(DP)                              :: x0,y0,z0,x1,y1,z1  !! positions
      REAL(DP)                              :: tt,t0              !! time
      REAL(DP)                              :: subvol             !! volume (or mass for atm.)

      LOGICAL                               :: active             !! particle active or not
   END TYPE trajectory
END MODULE mod_trajdef

! Loop variables
MODULE mod_loopvars
  USE mod_precdef

  REAL(DP)                                  :: ds         ! Volume normalised time (VNT)
  REAL(DP)                                  :: dsmin      ! VNT subcycles
  REAL(DP)                                  :: dse, dsw   ! VNT to east/west crossing
  REAL(DP)                                  :: dsn, dss   ! VNT to north/south crossing
  REAL(DP)                                  :: dsu, dsd   ! VNT to up/dow crossing
  REAL(DP)                                  :: dsc        ! VNT convergence
  REAL(DP)                                  :: dts        ! VNT step
  REAL(DP)                                  :: subvol     ! Transported mass/vol

  INTEGER                                   :: niter      ! number of iterations of a trajectory

  LOGICAL                                   :: scrivi     ! Writing flag
ENDMODULE mod_loopvars

! Trajectory variables
MODULE mod_traj
  USE mod_precdef
  USE mod_trajdef

  ! Variables connected to particle positions
  INTEGER                                   :: nend=0       ! Killing zone flag
  INTEGER                                   :: ntrac        ! Particle iteration variable
  INTEGER                                   :: ntractot=0   ! Total number trajectories

  ! Particle counters
  INTEGER                                   :: nout=0       ! Number of killed trajectories
  INTEGER                                   :: nloop=0      ! Number of particles trapped in a infinite loop
  INTEGER                                   :: nerror=0     ! Number of trajectories that have an error

  ! Error index
  INTEGER                                   :: errCode

  ! Particle positions
  INTEGER                                   :: ia, ja, ka, iam
  INTEGER                                   :: ib, jb, kb, ibm
  REAL(DP)                                  :: x0, y0, z0
  REAL(DP)                                  :: x1, y1, z1

  ! Particle arrays
  TYPE(trajectory), ALLOCATABLE, DIMENSION(:) :: trajectories
ENDMODULE mod_traj


! Grid variables
MODULE mod_grid
  USE mod_param, only: pi, undef, iter
  USE mod_precdef

  IMPLICIT NONE

  ! Regular size
  INTEGER                                   :: imt, jmt, km   ! Size of domain
  INTEGER                                   :: nst=2          ! Number of time steps stored in memory
  INTEGER                                   :: nsm=1,  nsp=2  ! Past/Present time step

  ! Boundary conditions
  INTEGER                                   :: iperio=1       ! East/West cyclic boundary condition
                                                              ! 0 - False / 1 - True
  INTEGER                                   :: jperio=1       ! North Folding

  ! Horizontal grid size
  REAL(DP)                                    :: dx, dy       ! dx/dy size (T point)
  REAL(DP)                                    :: dxdeg,dydeg
  REAL(DP), ALLOCATABLE, DIMENSION(:,:)       :: dxv, dyu     ! dx in V points, dy in U points
  REAL(DP), ALLOCATABLE, DIMENSION(:,:)       :: dxdy         ! Area of the grid (T point)
  REAL(DP)                                    :: dxyz         ! Grid volume
  INTEGER, ALLOCATABLE, DIMENSION(:,:)        :: mask         ! Land-sea mask

  ! Vertical grid
  REAL(DP), ALLOCATABLE, DIMENSION(:,:,:,:)   :: dzt, dzu, dzv
  REAL(PP), ALLOCATABLE, DIMENSION(:,:,:)     :: botbox

  INTEGER, ALLOCATABLE, DIMENSION(:,:)        :: kmt, kmu, kmv


  ! Sea level
  REAL(PP), ALLOCATABLE, DIMENSION(:,:,:)     :: hs

  ! Info about input data
  CHARACTER(LEN=50)                         :: RunID, tGridName, uGridName, vGridName, &
                                               fileSuffix,  ssh_name, ueul_name, veul_name, &
                                               usgs_name, vsgs_name

  CHARACTER(LEN=50)                         :: hgridFile, dy_name, dyu_name, dx_name, dxv_name, &
                                               zgridFile, dzt_name, dzu_name, dzv_name, &
                                               bathyFile, kmt_name

  CHARACTER(LEN=200)                        :: physDataDir, physPrefixForm,  &
                                               bioDataDir, bioPrefixForm, topoDataDir
  CHARACTER(LEN=50), DIMENSION(20)          :: physTracerNames, bioTracerNames

  CHARACTER (LEN=23)                        :: Project, Case
  CHARACTER (LEN=200)                       :: projdir="", ormdir=""
ENDMODULE mod_grid

! Time variables
MODULE mod_time
  USE mod_precdef
  ! Variables and routines for timekeeping

  !Timesteps
  INTEGER                                   :: ints=0    ! Time iteration variables
  INTEGER                                   :: intrun    ! Number maximum of time steps
  INTEGER                                   :: nff=1     ! Time arrow 1-forward / -1 backward


  ! Date variables
  LOGICAL                                   :: noleap=.TRUE.  ! Leap year flag
  INTEGER                                   :: startYear, startMon, startDay
  INTEGER                                   :: startHour, startMin
  REAL(DP)                                  :: startSec
  INTEGER                                   :: endYear=0, endMon,   endDay
  INTEGER                                   :: endHour,   endMin
  REAL(DP)                                  :: endSec
  ! Current date
  INTEGER                                   :: currYear, currMon, currDay
  INTEGER                                   :: currHour, currMin
  REAL(DP)                                  :: currSec
  INTEGER                                   :: dateYear, dateMon, dateDay
  INTEGER                                   :: dateHour, dateMin
  REAL(DP)                                  :: dateSec

  !Looping time
  INTEGER                                   :: loopIndex = 0               ! Counter of calendar loops
  INTEGER                                   :: loopStartYear, loopEndYear  ! Start/end year loop
  LOGICAL                                   :: loopYears =.FALSE.          ! Loop over years

  ! Other time variables
  INTEGER                                   :: iyear              ! Simulation year
  INTEGER                                   :: imon ,iday ,ihour  ! Dummies

  ! Time array
  REAL(DP)                                  :: tseas     ! GCM time step in seconds
  REAL(DP)                                  :: t0        ! Seeding time step in seconds
  REAL(DP)                                  :: tt        ! Time vector since the beginnig of the simulation in seconds
  REAL(DP)                                  :: tf        ! Future time vector
  REAL(DP)                                  :: ts        ! Normalised time
  REAL(DP)                                  :: tss       ! Normalised time between two time steps
  REAL(DP)                                  :: dt        ! Time step in second
  REAL(DP)                                  :: dtreg     ! Time step to the next time subcycle
  REAL(DP)                                  :: dtmin     ! Time step between time subcycles
  REAL(DP)                                  :: dstep     ! Normalised time step

  ! Time interpolation
  REAL(DP)                                  :: intrpr, intrpg
ENDMODULE mod_time

! Killing zones
MODULE mod_domain
  USE mod_precdef

  LOGICAL                                 :: l_rerun = .FALSE.      ! Rerun logics

  INTEGER                                 :: exitType
  INTEGER, DIMENSION(10)                  :: ienw ,iene, jens ,jenn ! Horizontal killing zones

  REAL(PP)                                :: timax                  ! Maximum time to run a trajectory

ENDMODULE mod_domain

! Velocity and fluxes
MODULE mod_vel
  USE mod_grid, only: nsm, nsp, dzt
  USE mod_precdef

  ! Velocity fields
  REAL(DP),   ALLOCATABLE, DIMENSION(:,:,:)      :: uvel ,vvel ,wvel
  REAL(DP)                                   :: uu, um, vv, vm

  ! Mass/volume fluxes
  REAL(DP), ALLOCATABLE, DIMENSION(:,:,:,:)    :: uflux, vflux
#if defined w_3dim || full_wflux
  REAL(DP), ALLOCATABLE, DIMENSION(:,:,:,:)    :: wflux
#else
  REAL(DP), ALLOCATABLE, DIMENSION(:,:)        :: wflux
#endif
ENDMODULE mod_vel

! Streamfunctions
MODULE mod_psi
  USE mod_precdef

  LOGICAL    :: l_psi = .FALSE.

  INTEGER , DIMENSION(9)   :: dirpsi = 0

  ! Barotropic streamfunction
  REAL(DP), ALLOCATABLE, DIMENSION(:,:,:) :: fluxes_xy
  REAL(DP), ALLOCATABLE, DIMENSION(:,:)   :: psi_xy

  ! Meridional streamfunction
  REAL(DP), ALLOCATABLE, DIMENSION(:,:,:) :: fluxes_yz
  REAL(DP), ALLOCATABLE, DIMENSION(:,:)   :: psi_yz

END MODULE mod_psi
