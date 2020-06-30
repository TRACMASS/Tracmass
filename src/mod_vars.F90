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
!!              - mod_seedvars
!!              - mod_trajdef
!!              - mod_loopvars
!!              - mod_traj
!!              - mod_grid
!!              - mod_time
!!              - mod_domain
!!              - mod_vel
!!              - mod_tracerdef
!!              - mod_tracervars
!!              - mod_psi
!!              - mod_postprocessvars
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

! Seed Variables
MODULE mod_seedvars
  USE mod_precdef

  INTEGER                                    :: isec         ! section for seeding
  INTEGER                                    :: idir         ! spatial direction of seeding
  INTEGER                                    :: nqua         ! type of seeding
  INTEGER                                    :: loneparticle ! ntrac of the lone particle

  REAL(DP)                                   :: partQuant    ! number of particles per grid to seed

  INTEGER                                    :: ist1, ist2   ! Zonal seeding region
  INTEGER                                    :: jst1, jst2   ! Meridional seeding region
  INTEGER                                    :: kst1, kst2   ! Vertical seeding region
  INTEGER                                    :: tst1, tst2   ! Seeding time

  INTEGER                                    :: seedType, seedTime

  CHARACTER(LEN=200)                         :: seedDir      ! Directory where seed files are stored
  CHARACTER(LEN=200)                         :: seedFile     ! space seed file
  CHARACTER(LEN=200)                         :: timeFile     ! time seed file

END MODULE mod_seedvars

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
      REAL(DP),DIMENSION(:), ALLOCATABLE    :: tracerval

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
  INTEGER                                   :: iloop      ! tracer loop dummy

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

  ! Trajectory direction
  INTEGER                                   :: trajdir = 1

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

  ! Grid description
  INTEGER, DIMENSION(3)                     :: griddir = 1
  LOGICAL                                   :: zeroindx

  ! Subdomain grid
  LOGICAL                                   :: l_subdom = .FALSE.
  INTEGER                                   :: imindom = 1,  imaxdom, jmindom = 1, jmaxdom

  ! Regular size
  INTEGER                                   :: imt, jmt, km   ! Size of domain
  INTEGER                                   :: imtdom, jmtdom ! Size of the original domain
  INTEGER                                   :: imthalf1, imthalf2, imtjump
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
  REAL(DP), ALLOCATABLE, DIMENSION(:,:,:,:)   :: dzt, dzu, dzv, dzdt
  REAL(DP), ALLOCATABLE, DIMENSION(:,:)       :: zstou, zstov
  REAL(DP), ALLOCATABLE, DIMENSION(:,:,:)     :: zstot
  REAL(DP), ALLOCATABLE, DIMENSION(:,:,:)     :: botbox
  REAL(DP), ALLOCATABLE, DIMENSION(:)         :: aa, bb

  INTEGER, ALLOCATABLE, DIMENSION(:,:)        :: kmt, kmu, kmv


  ! Sea level
  REAL(DP), ALLOCATABLE, DIMENSION(:,:,:)     :: hs

  ! Info about input data
  CHARACTER(LEN=50)                         :: RunID, tGridName, uGridName, vGridName, &
                                               fileSuffix,  hs_name, ueul_name, veul_name, &
                                               usgs_name = '', vsgs_name = ''

  CHARACTER(LEN=50)                         :: hgridFile, dy_name, dyu_name, dx_name, dxv_name, &
                                               zgridFile, dzt_name, dzu_name, dzv_name, &
                                               bathyFile, kmt_name

  CHARACTER(LEN=200)                        :: physDataDir, physPrefixForm,  &
                                               dateFormat, &
                                               bioDataDir, bioPrefixForm, topoDataDir
  CHARACTER(LEN=50), DIMENSION(20)          :: physTracerNames, bioTracerNames

  CHARACTER (LEN=23)                        :: Project, Case
  CHARACTER (LEN=200)                       :: projdir="", ormdir=""

  LOGICAL                                   :: l_onestep = .FALSE.
ENDMODULE mod_grid

! Time variables
MODULE mod_time
  USE mod_precdef
  ! Variables and routines for timekeeping

  !Timesteps
  INTEGER                                   :: ints=0    ! Time iteration variables
  INTEGER                                   :: intrun    ! Number maximum of time steps
  INTEGER                                   :: nff=1     ! Time arrow 1-forward / -1 backward
  INTEGER                                   :: nctstep

  ! Date variables
  LOGICAL                                   :: noleap=.TRUE.  ! Leap year flag
  INTEGER                                   :: startYear, startMon, startDay
  INTEGER                                   :: startHour, startMin
  REAL(DP)                                  :: startSec
  INTEGER                                   :: endYear=0, endMon,   endDay
  INTEGER                                   :: endHour,   endMin
  REAL(DP)                                  :: endSec

  ! Previous date
  INTEGER                                   :: prevYear, prevMon, prevDay
  INTEGER                                   :: prevHour, prevMin
  REAL(DP)                                  :: prevSec

  ! Current date
  INTEGER                                   :: currYear, currMon, currDay
  INTEGER                                   :: currHour, currMin
  REAL(DP)                                  :: currSec

  ! Next date
  INTEGER                                   :: nextYear, nextMon, nextDay
  INTEGER                                   :: nextHour, nextMin
  REAL(DP)                                  :: nextSec

  ! Temporary or calendar dates
  INTEGER                                   :: tempYear, tempMon, tempDay
  INTEGER                                   :: tempHour, tempMin
  REAL(DP)                                  :: tempSec
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
  REAL(DP)                                  :: tf        ! Next time vector
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
  REAL(DP),   ALLOCATABLE, DIMENSION(:,:,:)    :: uvel ,vvel ,wvel
  REAL(DP)                                     :: uu, um, vv, vm

  ! Mass/volume fluxes
  REAL(DP), ALLOCATABLE, DIMENSION(:,:,:,:)    :: uflux, vflux
#if defined w_explicit
  REAL(DP), ALLOCATABLE, DIMENSION(:,:,:,:)    :: wflux
#else
  REAL(DP), ALLOCATABLE, DIMENSION(:,:)        :: wflux
#endif
ENDMODULE mod_vel

! Define derived type "tracers"
MODULE mod_tracerdef
   USE mod_precdef

   TYPE tracer
      REAL(DP)                              :: minimum, maximum   !! minimum and maximum value of tracer

      CHARACTER(len=100)                    :: name               !! Description of the tracer
      CHARACTER(len=100)                    :: unit               !! Unit of the tracer
      CHARACTER(len=100)                    :: action             !! Read/Compute action
      CHARACTER(len=100)                    :: varname            !! Name of the variable
      CHARACTER(len=2)                      :: dimension          !! 2D/3D tracer

      REAL(DP), DIMENSION(:,:,:,:), ALLOCATABLE :: data           !! Data tracer
    END TYPE tracer
END MODULE mod_tracerdef

! Tracers
MODULE mod_tracervars
  USE mod_precdef
  USE mod_tracerdef

  LOGICAL    :: l_tracers = .FALSE.

  ! Number of tracers
  INTEGER                             :: numtracers = 0

  ! Tracer choice
  INTEGER, DIMENSION(20)              :: tracerchoice = 999, maxormin = 1

  ! Tracer characteristics
  CHARACTER(len=100), DIMENSION(20)   :: tracername = '', tracerunit, &
                                         tracervarname,traceraction

  CHARACTER(len=2), DIMENSION(20)     :: tracerdimension = '3D'

  REAL(DP), DIMENSION(20)             :: tracermin, tracermax, &
                                         tracer0min=-9999.d0, tracer0max=9999.d0, &
                                         tracere
  REAL(DP), DIMENSION(:), ALLOCATABLE :: tracervalue

  ! Tracer resolution
  INTEGER     :: resolution = 501

  REAL(DP), DIMENSION(:), ALLOCATABLE   :: dtracervalue
  INTEGER, DIMENSION(:,:), ALLOCATABLE  :: tracerbinvalue

  ! Particle arrays
  TYPE(tracer), ALLOCATABLE, DIMENSION(:) :: tracers


END MODULE mod_tracervars

! Streamfunctions
MODULE mod_psi
  USE mod_precdef

  ! Streamfunctions on/off
  LOGICAL    :: l_psi     = .FALSE.
  LOGICAL    :: l_offline = .TRUE.

  INTEGER , DIMENSION(9)   :: dirpsi = 0

  ! Barotropic streamfunction
  REAL(DP), ALLOCATABLE, DIMENSION(:,:,:) :: fluxes_xy
  REAL(DP), ALLOCATABLE, DIMENSION(:,:)   :: psi_xy

  ! Meridional streamfunction
  REAL(DP), ALLOCATABLE, DIMENSION(:,:,:) :: fluxes_yz
  REAL(DP), ALLOCATABLE, DIMENSION(:,:)   :: psi_yz

  ! Meridional-tracer streamfunction
  REAL(DP), ALLOCATABLE, DIMENSION(:,:,:,:) :: fluxes_yr
  REAL(DP), ALLOCATABLE, DIMENSION(:,:,:)   :: psi_yr

  ! Meridional-tracer streamfunction
  REAL(DP), ALLOCATABLE, DIMENSION(:,:,:) :: fluxes_rr
  REAL(DP), ALLOCATABLE, DIMENSION(:,:)   :: psi_rr

END MODULE mod_psi

MODULE mod_postprocessvars

  USE mod_precdef

  LOGICAL                                 :: l_norun = .FALSE.      ! Run only postprocessing
  LOGICAL                                 :: l_summary = .FALSE.    ! Extended information about killing zones

  INTEGER, DIMENSION(:),ALLOCATABLE       :: nsavewrite
  INTEGER                                 :: nsave

  INTEGER, DIMENSION(10)                  :: ntrajout = 0
  INTEGER                                 :: ntrajtot = 0
  INTEGER                                 :: maxlbas = 0

  REAL(DP), DIMENSION(10)                 :: volout = 0
  REAL(DP)                                :: voltot = 0

  ! Temporary trajectory information
  REAL(DP), DIMENSION(:,:,:), ALLOCATABLE   :: traj_t
  REAL(DP), DIMENSION(:,:), ALLOCATABLE     :: traj_x, traj_y, traj_z
  REAL(DP), DIMENSION(:), ALLOCATABLE       :: traj_subvol
  INTEGER, DIMENSION(:), ALLOCATABLE        :: traj_out
  INTEGER, DIMENSION(:), ALLOCATABLE        :: counter

END MODULE mod_postprocessvars
