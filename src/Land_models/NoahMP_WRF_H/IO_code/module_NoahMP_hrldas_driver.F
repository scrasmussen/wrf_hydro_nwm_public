!  Program Name:
!  Author(s)/Contact(s):
!  Abstract:
!  History Log:
!
!  Usage:
!  Parameters: <Specify typical arguments passed>
!  Input Files:
!        <list file names and briefly describe the data they include>
!  Output Files:
!        <list file names and briefly describe the information they include>
!
!  Condition codes:
!        <list exit condition or error codes returned >
!        If appropriate, descriptive troubleshooting instructions or
!        likely causes for failures could be mentioned here with the
!        appropriate error code
!
!  User controllable options: <if applicable>

module module_NoahMP_hrldas_driver

  use module_hrldas_netcdf_io ! artless add only
  use NoahmpIOVarType, only: NoahmpIO_type
  use GroundWaterMmfMod, only: WTABLE_mmf_noahmp
  use NoahmpInitMainMod, only: NoahmpInitMain
  use NoahmpDriverMainMod, only: NoahmpDriverMain
  use module_date_utilities ! artless add only
  use orchestrator_base ! artless add only
  use config_base, only: wrf_hydro, noah_lsm
  use modi_ini_csts, only: ini_csts
#ifdef MPP_LAND
  use module_mpp_land, only: MPP_LAND_PAR_INI, mpp_land_init, getLocalXY, mpp_land_bcast_char, mpp_land_sync
  use module_mpp_land, only: check_land, node_info, numprocs
  use module_cpl_land, only: cpl_land_init, fatal_error_stop
  !use mpi
#endif
#ifdef WRF_HYDRO
  use module_NWM_io, only: output_NoahMP_NWM
#endif

  implicit none

!#ifdef MPP_LAND
!  use mpi
!#endif

#ifdef WRF_HYDRO
   REAL,    allocatable, DIMENSION(:,:)   :: infxsrt,sfcheadrt, soldrain
   !LRK - Remove HRLDAS_ini_typ for WRF-Hydro
   integer :: snow_assim, HRLDAS_ini_typ
   REAL,    allocatable, DIMENSION(:,:)   :: etpnd, greenfrac, prcp0
   real :: etpnd1
   character(len=19) :: forcDate
   ! LRK - Remove GEO_STATIC_FLNM
   !character(len = 256):: GEO_STATIC_FLNM
   real, allocatable, dimension(:) :: zsoil
   integer :: kk
 ! INOUT (new accumulator variables for output water balance)
   REAL,    ALLOCATABLE, DIMENSION(:,:)    ::  ACCPRCP ! accumulated precip [mm]
   REAL,    ALLOCATABLE, DIMENSION(:,:)    ::  ACCECAN  ! accumulated canopy evap [mm]
   REAL,    ALLOCATABLE, DIMENSION(:,:)    ::  ACCETRAN ! accumulated transpiration [mm]
   REAL,    ALLOCATABLE, DIMENSION(:,:)    ::  ACCEDIR ! accumulated direct soil evap [mm]
   integer :: io_config_outputs=0
   integer :: t0OutputFlag=0
   REAL,    ALLOCATABLE, DIMENSION(:,:)    ::  SOILSAT_TOP ! top 2 layer soil saturation [fraction]
   REAL,    ALLOCATABLE, DIMENSION(:,:)    ::  SOILSAT ! column integrated soil saturation [fraction]
   REAL,    ALLOCATABLE, DIMENSION(:,:)    ::  SOILICE ! fraction of soil moisture that is ice [fraction]
   REAL,    ALLOCATABLE, DIMENSION(:,:)    ::  SNOWT_AVG ! snowpack average temperature (by layer mass) [K] ** DIAGNOSTIC VARIABLE
   integer, parameter                                      ::  max_ioc_num_vars = 200
   integer, parameter                                      ::  ioc_var_len = 20
   character(len=ioc_var_len), DIMENSION(max_ioc_num_vars) ::  IOCVARS
#endif

  character(len=9), parameter :: version = "v20150506"
  integer :: LDASIN_VERSION

!------------------------------------------------------------------------
! Begin exact copy of declaration section from driver (substitute allocatable, remove intent)
!------------------------------------------------------------------------

! IN only (as defined in WRF)
  ! artless note xlat_urb2d is xlat in noahmpr
  INTEGER                                 ::  ITIMESTEP ! timestep number
  INTEGER                                 ::  YR        ! 4-digit year
  REAL                                    ::  DTBL      ! timestep [s]
  INTEGER                                 ::  NSOIL     ! number of soil layers
  INTEGER                                 ::  NUM_SOIL_LAYERS     ! number of soil layers
  INTEGER                                 :: crocus_opt
  INTEGER                                 :: act_lev
  REAL                                    ::  DX        ! horizontal grid spacing [m]
  INTEGER, ALLOCATABLE, DIMENSION(:,:)    ::  GLACINFO  ! location of  glacier
  REAL,    ALLOCATABLE, DIMENSION(:,:)    ::  GLACT     ! glacier thickness
  REAL,    ALLOCATABLE, DIMENSION(:,:)    ::  VIS_ICEALB! map of visible ice albedo
  REAL                                    ::  XICE_THRESHOLD! fraction of grid determining seaice
  INTEGER                                 ::  ISICE     ! land cover category for ice
  INTEGER                                 ::  ISURBAN   ! land cover category for urban
  INTEGER                                 ::  ISLAKE    ! land cover category for lake
  INTEGER                                 ::  ISWATER   ! land cover category for water
  INTEGER                                 ::  IDVEG     ! dynamic vegetation (1 -> off ; 2 -> on) with opt_crs = 1
  INTEGER                                 ::  IOPT_CRS  ! canopy stomatal resistance (1-> Ball-Berry; 2->Jarvis)
  INTEGER                                 ::  IOPT_BTR  ! soil moisture factor for stomatal resistance (1-> Noah; 2-> CLM; 3-> SSiB)
  INTEGER                                 ::  IOPT_RUN  ! runoff and groundwater (1->SIMGM; 2->SIMTOP; 3->Schaake96; 4->BATS)
  INTEGER                                 ::  IOPT_SFC  ! surface layer drag coeff (CH & CM) (1->M-O; 2->Chen97)
  INTEGER                                 ::  IOPT_FRZ  ! supercooled liquid water (1-> NY06; 2->Koren99)
  INTEGER                                 ::  IOPT_INF  ! frozen soil permeability (1-> NY06; 2->Koren99)
  INTEGER                                 ::  IOPT_RAD  ! radiation transfer (1->gap=F(3D,cosz); 2->gap=0; 3->gap=1-Fveg)
  INTEGER                                 ::  IOPT_ALB  ! snow surface albedo (1->BATS; 2->CLASS)
  INTEGER                                 ::  IOPT_SNF  ! rainfall & snowfall (1-Jordan91; 2->BATS; 3->Noah)
  INTEGER                                 ::  IOPT_TBOT ! lower boundary of soil temperature (1->zero-flux; 2->Noah)
  INTEGER                                 ::  IOPT_STC  ! snow/soil temperature time scheme
  INTEGER                                 ::  IOPT_GLA  ! glacier option (1->phase change; 2->simple)
  INTEGER                                 ::  IOPT_RSF  ! surface resistance option (1->Zeng; 2->simple)
  INTEGER                                 ::  IZ0TLND   ! option of Chen adjustment of Czil (not used)
  INTEGER                                 ::  IOPT_SOIL ! soil configuration option
  INTEGER                                 ::  IOPT_PEDO ! soil pedotransfer function option
  INTEGER                                 ::  IOPT_CROP ! crop model option (0->none; 1->Liu et al.)
  INTEGER                                 ::  IOPT_IMPERV !imperviousness infiltration adjustment (0->none; 1->total;
                                                                                        !2->Alley&Veenhuis; 9->old)
! New spatially varying fields

  CHARACTER(LEN = 256)                    ::  spatial_filename
#ifdef SPATIAL_SOIL
  REAL, ALLOCATABLE, DIMENSION(:,:)       ::  slope_2D   ! Soil Drainage Parameter ! artless what
  ! is this in noahmpr
  REAL, ALLOCATABLE, DIMENSION(:,:)       ::  cwpvt_2D   ! Canopy wind parameter
  REAL, ALLOCATABLE, DIMENSION(:,:)       ::  vcmx25_2D  ! VCmax at 25C
  REAL, ALLOCATABLE, DIMENSION(:,:)       ::  mp_2D      ! Slope of Ball-Berry rs-P relationship
  REAL, ALLOCATABLE, DIMENSION(:,:)       ::  hvt_2D     ! Canopy Height
  REAL, ALLOCATABLE, DIMENSION(:,:)       ::  mfsno_2D   ! Snow cover m parameter
  REAL, ALLOCATABLE, DIMENSION(:,:)       ::  rsurfexp_2D! exponent in the shape parameter for soil resistance option 1
  REAL, ALLOCATABLE, DIMENSION(:,:)       ::  axaj_2D    ! Tension water distribution inflection parameter [-]
  REAL, ALLOCATABLE, DIMENSION(:,:)       ::  bxaj_2D    ! Tension water distribution shape parameter [-]
  REAL, ALLOCATABLE, DIMENSION(:,:)       ::  xxaj_2D    ! Free water distribution shape parameter [-]
  REAL, ALLOCATABLE, DIMENSION(:,:)       ::  imperv_2D  ! impervious fraction
  REAL, ALLOCATABLE, DIMENSION(:,:)       ::  ssi_2D     ! liquid water holding capacity for snowpack (m3/m3)
  REAL, ALLOCATABLE, DIMENSION(:,:)       ::  snowretfac_2D   ! snowpack water release timescale factor (1/s)
  REAL, ALLOCATABLE, DIMENSION(:,:)       ::  tau0_2D         ! tau0 from Yang97 eqn. 10a
  REAL, ALLOCATABLE, DIMENSION(:,:)       ::  rsurfsnow_2D    ! surface resistence for snow [s/m]
  REAL, ALLOCATABLE, DIMENSION(:,:)       ::  scamax_2D       ! maximum fractional snow covered area (0.0-1.0)
#endif
! ARTLESS WHY ARE THESE COMMENTED OUT??
!  REAL,    ALLOCATABLE, DIMENSION(:,:)    ::  SNOW      ! snow water equivalent [mm] ** (sometime) PROGNOSTIC VARIABLE
!  REAL,    ALLOCATABLE, DIMENSION(:,:)    ::  SNOWH     ! physical snow depth [m] ** (sometime) PROGNOSTIC VARIABLE

! INOUT (with no Noah LSM equivalent) (as defined in WRF)
!  REAL,    ALLOCATABLE, DIMENSION(:,:,:)  ::  TSNOXY    ! snow temperature [K] ** REFACTOR THIS!
!  REAL,    ALLOCATABLE, DIMENSION(:,:,:)  ::  ZSNSOXY   ! snow layer depth [m] ** REFACTOR THIS!
!  REAL,    ALLOCATABLE, DIMENSION(:,:,:)  ::  SNICEXY   ! snow layer ice [mm] ** REFACTOR THIS!
!  REAL,    ALLOCATABLE, DIMENSION(:,:,:)  ::  SNLIQXY   ! snow layer liquid water [mm] ** REFACTOR THIS!

! OUT (with no Noah LSM equivalent) (as defined in WRF)
  REAL,    ALLOCATABLE, DIMENSION(:,:,:)  ::  ALBSNDXY  ! snow albedo (direct)
  REAL,    ALLOCATABLE, DIMENSION(:,:,:)  ::  ALBSNIXY  ! snow albedo (diffuse)
  INTEGER   ::  ids,ide, jds,jde, kds,kde,  &  ! d -> domain
   &            ims,ime, jms,jme, kms,kme,  &  ! m -> memory
   &            its,ite, jts,jte, kts,kte      ! t -> tile

!------------------------------------------------------------------------
! Needed for NoahMP init
!------------------------------------------------------------------------

  LOGICAL                                 ::  FNDSOILW    ! soil water present in input
  LOGICAL                                 ::  FNDSNOWH    ! snow depth present in input
  REAL,    ALLOCATABLE, DIMENSION(:,:)    ::  CHSTARXY    ! for consistency with MP_init; delete later
  REAL,    ALLOCATABLE, DIMENSION(:,:)    ::  SEAICE      ! seaice fraction

!------------------------------------------------------------------------
! Needed for MMF_RUNOFF (IOPT_RUN = 5); not part of MP driver in WRF
!------------------------------------------------------------------------

  REAL,    ALLOCATABLE, DIMENSION(:,:)    ::  MSFTX
  REAL,    ALLOCATABLE, DIMENSION(:,:)    ::  MSFTY
  REAL,    ALLOCATABLE, DIMENSION(:,:)    ::  EQZWT
  REAL,    ALLOCATABLE, DIMENSION(:,:)    ::  RIVERBEDXY
  REAL,    ALLOCATABLE, DIMENSION(:,:)    ::  RIVERCONDXY
  REAL,    ALLOCATABLE, DIMENSION(:,:)    ::  FDEPTHXY
  REAL,    ALLOCATABLE, DIMENSION(:,:)    ::  AREAXY
  REAL,    ALLOCATABLE, DIMENSION(:,:)    ::  QRFSXY
  REAL,    ALLOCATABLE, DIMENSION(:,:)    ::  QSPRINGSXY
  REAL,    ALLOCATABLE, DIMENSION(:,:)    ::  QRFXY
  REAL,    ALLOCATABLE, DIMENSION(:,:)    ::  QSPRINGXY
  REAL,    ALLOCATABLE, DIMENSION(:,:)    ::  QSLATXY
  REAL                                    ::  WTDDT  = 30.0    ! frequency of groundwater call [minutes]
  INTEGER                                 ::  STEPWTD = 1      ! step of groundwater call

!------------------------------------------------------------------------
! Crocus
!------------------------------------------------------------------------

  REAL,       ALLOCATABLE, DIMENSION(:,:)     ::  PSNOWTHRUFALXY
  REAL,       ALLOCATABLE, DIMENSION(:,:)     ::  PSNOWALBXY
  REAL,       ALLOCATABLE, DIMENSION(:,:)     ::  PSNOWHEIGHTXY
  REAL,       ALLOCATABLE, DIMENSION(:,:)     ::  PSNOWTOTSWEXY
  REAL,       ALLOCATABLE, DIMENSION(:,:)     ::  FLOW_SNOW
  REAL,       ALLOCATABLE, DIMENSION(:,:)     ::  FLOW_ICE
  REAL,       ALLOCATABLE, DIMENSION(:,:,:)   ::  PSNOWHEATXY
  REAL,       ALLOCATABLE, DIMENSION(:,:,:)   ::  PSNOWRHOXY
  REAL,       ALLOCATABLE, DIMENSION(:,:,:)   ::  PSNOWSWEXY
  REAL,       ALLOCATABLE, DIMENSION(:,:,:)   ::  PSNOWGRAN1XY
  REAL,       ALLOCATABLE, DIMENSION(:,:,:)   ::  PSNOWGRAN2XY
  REAL,       ALLOCATABLE, DIMENSION(:,:,:)   ::  PSNOWAGEXY
  REAL,       ALLOCATABLE, DIMENSION(:,:,:)   ::  PSNOWLIQXY
  REAL,       ALLOCATABLE, DIMENSION(:,:,:)   ::  PSNOWTEMPXY
  REAL,       ALLOCATABLE, DIMENSION(:,:,:)   ::  PSNOWDZXY
  REAL,       ALLOCATABLE, DIMENSION(:,:,:)   ::  PSNOWHISTXY

!------------------------------------------------------------------------
! End 2D variables not used in WRF
!------------------------------------------------------------------------

  CHARACTER(LEN=256) :: MMINSL  = 'STAS'  ! soil classification
  CHARACTER(LEN=256) :: LLANDUSE          ! (=USGS, using USGS landuse classification)

!------------------------------------------------------------------------
! Timing:
!------------------------------------------------------------------------

  INTEGER :: ITIME          ! LSM time step

!---------------------------------------------------------------------
!  DECLARE/Initialize constants
!---------------------------------------------------------------------

    INTEGER                             :: I
    INTEGER                             :: J
    INTEGER                             :: SLOPETYP
    INTEGER                             :: YEARLEN
    INTEGER, PARAMETER                  :: NSNOW = 3    ! number of snow layers fixed to 3
    REAL, PARAMETER                     :: undefined_real = 9.9692099683868690E36 ! NetCDF float   FillValue
    INTEGER, PARAMETER                  :: undefined_int = -2147483647            ! NetCDF integer FillValue
    LOGICAL                             :: update_lai, update_veg

!---------------------------------------------------------------------
!  File naming, parallel
!---------------------------------------------------------------------

  character          :: hgrid_hydro
  integer            :: igrid, igrid_hydro
  logical            :: lexist
  integer            :: imode, iimode
  integer            :: ixfull
  integer            :: jxfull
  integer            :: ixpar
  integer            :: jxpar
  integer            :: xstartpar
  integer            :: ystartpar
  integer            :: rank = 0
  logical            :: restart_flag
  character(len=256) :: restart_flnm
  integer            :: ierr

!---------------------------------------------------------------------
! Attributes from LDASIN input file (or HRLDAS_SETUP_FILE, as the case may be)
!---------------------------------------------------------------------

  INTEGER           :: IX
  INTEGER           :: JX
  REAL              :: DY
  REAL              :: TRUELAT1
  REAL              :: TRUELAT2
  REAL              :: CEN_LON
  INTEGER           :: MAPPROJ
  REAL              :: LAT1
  REAL              :: LON1

#ifdef MPP_LAND
  integer ix_tmp, jx_tmp
#endif

#ifdef WRF_HYDRO
  character(len=19)  :: tmpdate
  character(len=1000) :: VARLIST
  integer :: brkflag = 0
  integer :: varind = 1
#endif
  ! ARTLESS  remove these if they disappear
  integer :: xstart, xend, ystart, yend !convenience variables

  contains

  subroutine land_driver_ini(NTIME_out, state, wrfits,wrfite,wrfjts,wrfjte)
     use module_HYDRO_drv, only: HYDRO_ini
     use config_base, only: nlst
     use module_rt_data, only: rt_domain
     use module_hrldas_HYDRO, only: open_print_mpp
     use NoahmpIOVarType, only: NoahmpIO_type
     implicit  none
     integer, intent(out) :: NTIME_out
     integer, parameter :: did=1
     integer :: khour
     type(NoahmpIO_type), intent(out) :: state

    ! initilization for stand alone parallel code.
    integer, optional, intent(in) :: wrfits,wrfite,wrfjts,wrfjte

#ifdef MPP_LAND
    call read_dim(noah_lsm%hrldas_setup_file,ix_tmp,jx_tmp)
    call MPP_LAND_INIT(ix_tmp,jx_tmp)
#endif

#ifdef WRF_HYDRO
    ! forc_typ = wrf_hydro%forc_typ ! artless
#endif

  dtbl = real(noah_lsm%noah_timestep)
  num_soil_layers = noah_lsm%nsoil      ! because surface driver uses the long form
  act_lev    = noah_lsm%act_lev
  crocus_opt = noah_lsm%crocus_opt
  IDVEG      = noah_lsm%dynamic_veg_option ! transfer from namelist to driver format
  IOPT_CRS   = noah_lsm%canopy_stomatal_resistance_option
  IOPT_BTR   = noah_lsm%btr_option
  IOPT_RUN   = noah_lsm%runoff_option
  IOPT_SFC   = noah_lsm%surface_drag_option
  IOPT_FRZ   = noah_lsm%supercooled_water_option
  IOPT_INF   = noah_lsm%frozen_soil_option
  IOPT_RAD   = noah_lsm%radiative_transfer_option
  IOPT_ALB   = noah_lsm%snow_albedo_option
  IOPT_SNF   = noah_lsm%pcp_partition_option
  IOPT_TBOT  = noah_lsm%tbot_option
  IOPT_STC   = noah_lsm%temp_time_scheme_option
  IOPT_GLA   = noah_lsm%glacier_option
  IOPT_RSF   = noah_lsm%surface_resistance_option
  IOPT_SOIL  = noah_lsm%soil_data_option
  IOPT_PEDO  = noah_lsm%pedotransfer_option
  IOPT_CROP  = noah_lsm%crop_option
  IOPT_IMPERV = noah_lsm%imperv_option

  khour = state%khour

  !!----------------------------------------------------------------------------
  !! channel_only
  call updateNameList("channel_only",0)
  if(state%forc_typ .eq. 9)  call updateNameList("channel_only",1)
  call updateNameList("channelBucket_only",0)
  if(state%forc_typ .eq. 10) call updateNameList("channelBucket_only",1)

  if(state%forc_typ .eq. 9 .or. state%forc_typ .eq. 10) then
     write(state%olddate,'(I4.4,"-",I2.2,"-",I2.2,"_",I2.2,":",I2.2,":",I2.2)') &
          noah_lsm%start_year, noah_lsm%start_month, noah_lsm%start_day, noah_lsm%start_hour, noah_lsm%start_min, 0
     forcdate = state%olddate
     if ((khour < 0) .and. (noah_lsm%kday < 0)) then
        write(*, '("FATAL ERROR: In module_NoahMP_hrldas_driver.F land_driver_ini() - "// &
             "Namelist error: Either KHOUR or KDAY must be defined.")')
        stop
     else if (( khour < 0 ) .and. (noah_lsm%kday > 0)) then
        khour = noah_lsm%kday * 24
     else if ((khour > 0) .and. (noah_lsm%kday > 0)) then
        write(*, '("WARNING: In land_driver_ini() - Check Namelist: KHOUR and KDAY both defined.")')
        stop
     end if
     state%NTIME = khour*3600./nint(dtbl)
     NTIME_out = state%NTIME
      ! artless how to handle nlst
     if(.not. RT_DOMAIN(did)%initialized) then
        nlst(did)%dt = real(noah_lsm%noah_timestep)
        nlst(did)%khour = khour ! Adding kHOUR to be used in the NWM output routines.
        nlst(did)%olddate(1:19) = state%olddate(1:19)
        nlst(did)%startdate(1:19) = state%olddate(1:19)
        nlst(did)%nsoil = -999999
#ifdef MPP_LAND
        call mpp_land_bcast_int1(nlst(did)%nsoil)
#endif
        allocate(nlst(did)%zsoil8(nlst(did)%nsoil))
        nlst(did)%zsoil8(1:nlst(did)%nsoil) = zsoil(1:nlst(did)%nsoil) ! artless double check zsoil
        ! is allocated??
        call HYDRO_ini(state%ntime,did,ix0=1,jx0=1)
        RT_DOMAIN(did)%initialized = .true.
     end if ! if .not. RT_DOMAIN(did)%initialized

     call open_print_mpp(6)

     return  !! no more init necessary if channel_only or channelBucket_only

  end if
  !!----------------------------------------------------------------------------
  !! channel_only


!---------------------------------------------------------------------
!  NAMELIST end
!---------------------------------------------------------------------

!---------------------------------------------------------------------
!  NAMELIST check begin
!---------------------------------------------------------------------

  update_lai = .true.   ! default: use LAI if present in forcing file
  if (noah_lsm%dynamic_veg_option == 2 .or. noah_lsm%dynamic_veg_option == 5 .or. noah_lsm%dynamic_veg_option == 6) &
    update_lai = .false.

  update_veg = .false.  ! default: don't use VEGFRA if present in forcing file
  if (noah_lsm%dynamic_veg_option == 1 .or. noah_lsm%dynamic_veg_option == 6 .or. noah_lsm%dynamic_veg_option == 7) &
    update_veg = .true.

  if (noah_lsm%nsoil < 0) then
     stop "FATAL ERROR: In module_NoahMP_hrldas_driver.F land_driver_ini()"// &
          " - NSOIL must be set in the namelist."
  end if

  if ((khour < 0) .and. (noah_lsm%kday < 0)) then
#ifdef HYDRO_D
     write(*, '("FATAL ERROR: In module_NoahMP_hrldas_driver.F land_driver_ini() - "// &
                "Namelist error.")')
     write(*, '(" ***** ")')
     write(*, '(" *****      Either KHOUR or KDAY must be defined.")')
     write(*, '(" ***** ")')
#endif
     stop
  else if (( khour < 0 ) .and. (noah_lsm%kday > 0)) then
     khour = noah_lsm%kday * 24
     write(*, '("WARNING: In land_driver_ini() - KHOUR < 0. DEFINED USING KDAY.")')
  else if ((khour > 0) .and. (noah_lsm%kday > 0)) then
     write(*, '("WARNING: In land_driver_ini() - Check Namelist: KHOUR and KDAY both defined.")')
  else
     ! all is well.  KHOUR defined
  end if

  if (noah_lsm%forcing_timestep < 0) then
        write(*, *)
        write(*, '("FATAL ERROR: In module_NoahMP_hrldas_driver.F land_driver_ini()- "// &
                   "Namelist error.")')
        write(*, '(" ***** ")')
        write(*, '(" *****       FORCING_TIMESTEP needs to be set greater than zero.")')
        write(*, '(" ***** ")')
        write(*, *)
        stop
  end if

  if (noah_lsm%noah_timestep < 0) then
        write(*, *)
        write(*, '("FATAL ERROR: In module_NoahMP_hrldas_driver.F land_driver_ini()"// &
                   " - Namelist error.")')
        write(*, '(" ***** ")')
        write(*, '(" *****       NOAH_TIMESTEP needs to be set greater than zero.")')
        write(*, '(" *****                     900 seconds is recommended.       ")')
        write(*, '(" ***** ")')
        write(*, *)
        stop
  end if

  !
  ! Check that OUTPUT_TIMESTEP fits into NOAH_TIMESTEP:
  !
  if (noah_lsm%output_timestep /= 0) then
     if (mod(noah_lsm%output_timestep, noah_lsm%noah_timestep) > 0) then
        write(*, *)
        write(*, '("FATAL ERROR: In module_NoahMP_hrldas_driver.F land_driver_ini() - "// &
                   "Namelist error.")')
        write(*, '(" ***** ")')
        write(*, '(" *****       OUTPUT_TIMESTEP should set to an integer multiple of NOAH_TIMESTEP.")')
        write(*, '(" *****            OUTPUT_TIMESTEP = ", I12, " seconds")') noah_lsm%output_timestep
        write(*, '(" *****            NOAH_TIMESTEP   = ", I12, " seconds")') noah_lsm%noah_timestep
        write(*, '(" ***** ")')
        write(*, *)
        stop
     end if
  end if

  !
  ! Check that RESTART_FREQUENCY_HOURS fits into NOAH_TIMESTEP:
  !
  if (noah_lsm%restart_frequency_hours /= 0) then
     if (mod(noah_lsm%restart_frequency_hours*3600, noah_lsm%noah_timestep) > 0) then
        write(*, *)
        write(*, '("FATAL ERROR: In module_NoahMP_hrldas_driver.F land_driver_ini() - "// &
                   "Namelist error.")')
        write(*, '(" *****       RESTART_FREQUENCY_HOURS (converted to seconds) should set to an ")')
        write(*, '(" *****       integer multiple of NOAH_TIMESTEP.")')
        write(*, '(" *****            RESTART_FREQUENCY_HOURS = ", I12, " hours:  ", I12, " seconds")') &
             noah_lsm%restart_frequency_hours, noah_lsm%restart_frequency_hours*3600
        write(*, '(" *****            NOAH_TIMESTEP           = ", I12, " seconds")') noah_lsm%noah_timestep
        write(*, '(" ***** ")')
        write(*, *)
        stop
     end if
  end if

  if (noah_lsm%dynamic_veg_option == 2 .or. noah_lsm%dynamic_veg_option == 5 .or. noah_lsm%dynamic_veg_option == 6) then
     if ( noah_lsm%canopy_stomatal_resistance_option /= 1) then
        write(*, *)
        write(*, '("FATAL ERROR: In module_NoahMP_hrldas_driver.F land_driver_ini() - "// &
                   "Namelist error.")')
        write(*, '(" ***** ")')
        write(*, '(" *****       CANOPY_STOMATAL_RESISTANCE_OPTION must be 1 when DYNAMIC_VEG_OPTION == 2/5/6")')
        write(*, *)
        stop
     end if
  end if

!---------------------------------------------------------------------
!  NAMELIST check end
!---------------------------------------------------------------------

!----------------------------------------------------------------------
! Initialize gridded domain
!----------------------------------------------------------------------

#ifdef MPP_LAND
#ifdef WRF_HYDRO
  if(wrf_hydro%finemesh .ne. 0) then
     write(*, '("WARNING: In module_NoahMP_hrldas_driver.F land_driver_ini() - "// &
          "noah_lsm%x and y start and noah_lsm%x and yend calculated using wrf_hydro%finemesh_factor")')

     xstart = (wrfits-1) * wrf_hydro%finemesh_factor + 1
     xend = (wrfite-1) * wrf_hydro%finemesh_factor
     ystart = (wrfjts-1) * wrf_hydro%finemesh_factor + 1
     yend = (wrfjte-1) * wrf_hydro%finemesh_factor
     call CPL_LAND_INIT(xstart,xend, ystart,yend)
     ix_tmp = xend - xstart + 1
     jx_tmp = yend - ystart + 1

    else
#endif
       call read_dim(noah_lsm%hrldas_setup_file,ix_tmp,jx_tmp)
       call MPP_LAND_PAR_INI(1,ix_tmp,jx_tmp,1)
       call getLocalXY(ix_tmp,jx_tmp,noah_lsm%xstart,noah_lsm%ystart,noah_lsm%xend,noah_lsm%yend)
#ifdef WRF_HYDRO
    end if
#endif
#endif

    call read_hrldas_hdrinfo(noah_lsm%hrldas_setup_file, ix, jx, &
         noah_lsm%xstart, noah_lsm%xend, noah_lsm%ystart, noah_lsm%yend, &
         iswater, islake, isurban, isice, llanduse, dx, dy, truelat1, truelat2, cen_lon, lat1, lon1, &
         igrid, mapproj)
  write(state%hgrid,'(I1)') igrid

  write(state%olddate,'(I4.4,"-",I2.2,"-",I2.2,"_",I2.2,":",I2.2,":",I2.2)') &
       noah_lsm%start_year, noah_lsm%start_month, noah_lsm%start_day, noah_lsm%start_hour, noah_lsm%start_min, 0

  state%startdate = state%olddate

#ifdef MPP_LAND
  ix = ix_tmp
  jx = jx_tmp
#endif

#ifdef WRF_HYDRO
  forcdate = state%olddate
#endif

  ! Convenience variables
  xstart = noah_lsm%xstart
  ystart = noah_lsm%ystart
  xend = noah_lsm%xend
  yend = noah_lsm%yend
  nsoil = noah_lsm%nsoil
  act_lev = noah_lsm%act_lev
  crocus_opt = noah_lsm%crocus_opt
  ids = xstart
  ide = xend
  jds = ystart
  jde = yend
  kds = 1
  kde = 2
  its = xstart
  ite = xend
  jts = ystart
  jte = yend
  kts = 1
  kte = 2
  ims = xstart
  ime = xend
  jms = ystart
  jme = yend
  kms = 1
  kme = 2

!---------------------------------------------------------------------
!  Allocate multi-dimension fields for subwindow calculation
!---------------------------------------------------------------------

  ixfull = xend-xstart+1
  jxfull = yend-ystart+1

  ixpar = ixfull
  jxpar = jxfull
  xstartpar = 1
  ystartpar = 1

!---------------------------------------------------------------------
!  initialize NoahmpIOVarType data type and variables
!---------------------------------------------------------------------

  call NoahmpIOVarInitDefault(state) ! ARTLESS NEED TO FIND WERE noahmpiovarinitmod.f90:1603 occurs

!   ------- from here, these need to be sorted, these are just in ours
  ! NAME CHANGEES
  ! ALLOCATE ( XLAT(XSTART:XEND,YSTART:YEND) )    ! latitude [rad]
  ! ARTLESS CROCUS needs to be handled, added to state varible??
  if (crocus_opt /= 0) then
     ALLOCATE (GLACINFO(XSTART:XEND,YSTART:YEND) )    ! loacation of glacier
     ALLOCATE (GLACT   (XSTART:XEND,YSTART:YEND) )    ! glacier thickness [m]
     ALLOCATE (VIS_ICEALB(XSTART:XEND,YSTART:YEND))
  end if

  ! also allocating swddir, swddif, snowbl, rainncv
  ! also allocating soilcomp, soilcl{1,2,3,4}, irr_{frac,har,mad}
    ! soilcl4     irr_frac_2D      irr_har_2D       irr_lai_2D
    ! irr_mad_2D       filoss_2D        sprir_rate_2D    micir_rate_2D
  ! firtfac_2D       ir_rain_2D       bvic_2D  bbvic_2D?
  ! NoahmpIO%KLAT_FAC       NoahmpIO%TDSMC_FAC      NoahmpIO%TD_DC          NoahmpIO%TD_DCOEF
  ! NoahmpIO%TD_DDRAIN      NoahmpIO%TD_RADI        NoahmpIO%TD_SPAC

#ifdef SPATIAL_SOIL  ! ARTLESS, this is happening either way!
  ALLOCATE ( slope_2D   (XSTART:XEND,YSTART:YEND) )            ! Soil Drainage Parameter
  ALLOCATE ( cwpvt_2D   (XSTART:XEND,YSTART:YEND) )            ! Canopy wind parameter
  ALLOCATE ( vcmx25_2D  (XSTART:XEND,YSTART:YEND) )            ! VCmax at 25C
  ALLOCATE ( mp_2D      (XSTART:XEND,YSTART:YEND) )            ! Slope of Ball-Berry rs-P relationship
  ALLOCATE ( hvt_2D     (XSTART:XEND,YSTART:YEND) )            ! Canopy Height
  ALLOCATE ( mfsno_2D   (XSTART:XEND,YSTART:YEND) )            ! Snow cover m parameter
  ALLOCATE ( rsurfexp_2D(XSTART:XEND,YSTART:YEND) )            ! exponent in the shape parameter for soil resistance option 1
  ! ARTLESS, this is now not happening
  ALLOCATE ( imperv_2D  (XSTART:XEND,YSTART:YEND) )            ! impervious fraction
  ALLOCATE ( ssi_2D     (XSTART:XEND,YSTART:YEND) )            ! liquid water holding capacity for snowpack (m3/m3)
  ALLOCATE ( snowretfac_2D (XSTART:XEND,YSTART:YEND) )         ! snowpack water release timescale factor (1/s)
  ALLOCATE ( tau0_2D    (XSTART:XEND,YSTART:YEND) )            ! tau0 from Yang97 eqn. 10a
  ALLOCATE ( rsurfsnow_2D (XSTART:XEND,YSTART:YEND) )          ! surface resistence for snow [s/m]
  ALLOCATE ( scamax_2D  (XSTART:XEND,YSTART:YEND) )            ! maximum fractional snow covered area (0.0-1.0)
#endif
  ! ARTLESS WHY ARE THESE NOT ALLOCATED HERE?? they are in nmpiovarinit
  !ALLOCATE ( TSNOXY    (XSTART:XEND,-NSNOW+1:0,    YSTART:YEND) )  ! snow temperature [K]
  !ALLOCATE ( ZSNSOXY   (XSTART:XEND,-NSNOW+1:NSOIL,YSTART:YEND) )  ! snow layer depth [m]
  !ALLOCATE ( SNICEXY   (XSTART:XEND,-NSNOW+1:0,    YSTART:YEND) )  ! snow layer ice [mm]
  !ALLOCATE ( SNLIQXY   (XSTART:XEND,-NSNOW+1:0,    YSTART:YEND) )  ! snow layer liquid water [mm]
  ! noahmpd also allocate granxy ggdxy
  !                      lai_tmp
  ! noahmpd also allocates irrigation
    ! NoahmpIO%IRFRACT    NoahmpIO%SIFRACT    NoahmpIO%MIFRACT    NoahmpIO%FIFRACT
    ! NoahmpIO%IRNUMSI    NoahmpIO%IRNUMMI    NoahmpIO%IRNUMFI    NoahmpIO%IRWATSI
    ! NoahmpIO%IRWATMI    NoahmpIO%IRWATFI    NoahmpIO%IRELOSS    NoahmpIO%IRSIVOL
    ! NoahmpIO%IRMIVOL    NoahmpIO%IRFIVOL    NoahmpIO%IRRSPLH    NoahmpIO%LOCTIM
  ALLOCATE ( ALBSNDXY  (XSTART:XEND,1:2,YSTART:YEND) )  ! snow albedo (direct)
  ALLOCATE ( ALBSNIXY  (XSTART:XEND,1:2,YSTART:YEND) )  ! snow albedo (diffuse)
  ! noahmpd also allocates RS total stomatal resistance
  !          and QTDRAIN TD_FRACTION
  !  name switch from XLONIN to XLONG
  ! PAHXY PAHGXY PAHBXY PAHVXY QINTSXY QINTRXY QDRIPSXY QDRIPRXY QTHROSXY
  ! QTHRORXY QSNSUBXY QSNFROXY QSUBCXY QFROCXY QEVACXY QDEWCXY QFRZCXY QMELTCXY
  ! QSNBOTXY QMELTXY PONDINGXY FPICEXY RAINLSM SNOWLSM FORCTLSM FORCQLSM FORCPLSM
  ! FORCZLSM FORCWLSM ACC_SSOILXY ACC_QINSURXY ACC_QSEVAXY ACC_ETRANIXY EFLXBXY SOILENERGY SNOWENERGY
  ! CANHSXY ACC_DWATERXY ACC_PRCPXY ACC_ECANXY ACC_ETRANXY ACC_EDIRXY
  ! crop model noahmpd stuff
  ! PGSXY CROPCAT PLANTING HARVEST SEASON_GDD CROPTYPE
  ! they have urban physics, but doesnt get allocated

  if (crocus_opt /= 0) then
     ALLOCATE ( PSNOWTHRUFALXY (XSTART:XEND,YSTART:YEND) )
     ALLOCATE ( PSNOWALBXY     (XSTART:XEND,YSTART:YEND) )
     ALLOCATE ( PSNOWHEIGHTXY  (XSTART:XEND,YSTART:YEND) )
     ALLOCATE ( PSNOWTOTSWEXY  (XSTART:XEND,YSTART:YEND) )
     ALLOCATE ( FLOW_SNOW      (XSTART:XEND,YSTART:YEND) )
     ALLOCATE ( FLOW_ICE       (XSTART:XEND,YSTART:YEND) )
     ALLOCATE ( PSNOWHEATXY    (XSTART:XEND,1:act_lev,YSTART:YEND) )
     ALLOCATE ( PSNOWRHOXY     (XSTART:XEND,1:act_lev,YSTART:YEND) )
     ALLOCATE ( PSNOWSWEXY     (XSTART:XEND,1:act_lev,YSTART:YEND) )
     ALLOCATE ( PSNOWGRAN1XY   (XSTART:XEND,1:act_lev,YSTART:YEND) )
     ALLOCATE ( PSNOWGRAN2XY   (XSTART:XEND,1:act_lev,YSTART:YEND) )
     ALLOCATE ( PSNOWAGEXY     (XSTART:XEND,1:act_lev,YSTART:YEND) )
     ALLOCATE ( PSNOWLIQXY     (XSTART:XEND,1:act_lev,YSTART:YEND) )
     ALLOCATE ( PSNOWTEMPXY    (XSTART:XEND,1:act_lev,YSTART:YEND) )
     ALLOCATE ( PSNOWDZXY      (XSTART:XEND,1:act_lev,YSTART:YEND) )
     ALLOCATE ( PSNOWHISTXY    (XSTART:XEND,1:act_lev,YSTART:YEND) )
   end if ! crocus_opt /= 0
   ! artless
#ifdef WRF_HYDRO
   ! noahmpr allocating qtiledrain and zwatble2d
   ALLOCATE ( ACCPRCP  (XSTART:XEND,YSTART:YEND) )  ! accumulated precip [mm]
   ALLOCATE ( ACCECAN  (XSTART:XEND,YSTART:YEND) )  ! accumulated canopy evap [mm]
   ALLOCATE ( ACCETRAN  (XSTART:XEND,YSTART:YEND) )  ! accumulated transpiration [mm]
   ALLOCATE ( ACCEDIR  (XSTART:XEND,YSTART:YEND) )  ! accumulated direct soil evap [mm]

   ALLOCATE ( SOILSAT_TOP  (XSTART:XEND,YSTART:YEND) )  ! top 2 layer soil saturation [fraction]
   ALLOCATE ( SOILSAT  (XSTART:XEND,YSTART:YEND) )  ! column integrated soil saturation [fraction]
   ALLOCATE ( SOILICE  (XSTART:XEND,YSTART:YEND) )  ! fraction of soil moisture that is ice [fraction]
   ALLOCATE ( SNOWT_AVG  (XSTART:XEND,YSTART:YEND) )  ! snowpack average temperature (by layer mass) [K]

 ! Initialize accumulator variables to 0
   ACCPRCP = 0.0
   ACCECAN = 0.0
   ACCETRAN = 0.0
   ACCEDIR = 0.0
#endif
  ! noahmpr has ICE var
  if (crocus_opt /= 0) then
     GLACINFO   = undefined_int
     GLACT      = undefined_real
     VIS_ICEALB = undefined_real
  end if
  !SNOWH      = undefined_real
  !TSNOXY     = undefined_real
  !SNICEXY    = undefined_real
  !SNLIQXY    = undefined_real
  !ZSNSOXY    = undefined_real
  ALBSNDXY   = undefined_real
  ALBSNIXY   = undefined_real

#ifdef WRF_HYDRO
  SOILSAT_TOP = undefined_real
  SOILSAT     = undefined_real
  SOILICE     = undefined_real
  SNOWT_AVG   = undefined_real
#endif
! ------- to here, these need to be sorted

!----------------------------------------------------------------------
! Read Landuse Type and Soil Texture and Other Information
!----------------------------------------------------------------------

  call readland_hrldas(state%HRLDAS_SETUP_FILE, state%XSTART, state%XEND, state%YSTART, &
       state%YEND, state%ISWATER, state%ISLAKE, state%IVGTYP, &
       state%ISLTYP, state%TERRAIN, state%TMN, state%XLAT, &
       state%XLONG, state%XLAND, state%SEAICE, state%MSFTX, &
       state%MSFTY,  crocus_opt, GLACINFO, GLACT, &
       VIS_ICEALB)

  where (state%SEAICE > 0.0) state%XICE = 1.0

!----------------------------------------------------------------------
! Crocus: Initialize glacier
!----------------------------------------------------------------------
  if (crocus_opt /= 0) then
     !PSNOWLIQXY     = undefined_real
     !PSNOWTEMPXY    = undefined_real
     !PSNOWDZXY      = undefined_real
     !PSNOWHEATXY    = undefined_real
     !PSNOWRHOXY     = undefined_real
     !PSNOWSWEXY     = undefined_real
     !PSNOWGRAN1XY   = undefined_real
     !PSNOWGRAN2XY   = undefined_real
     !PSNOWHISTXY    = undefined_real
     !PSNOWALBXY     = undefined_real
     !PSNOWTHRUFALXY = undefined_real
     !PSNOWHEIGHTXY  = undefined_real
     !PSNOWTOTSWEXY  = undefined_real
     !FLOW_SNOW      = undefined_real
     !FLOW_ICE       = undefined_real
     !PSNOWAGEXY     = undefined_real

! These should probably be kept initialized with an undefined value (above)
! to allow tracking of cells that fall through the cracks in value updates
! (also worth noting that 0 is not a valid value for all of these variables).
! However, since restarts are not currently water-masking 2d reals, this makes
! for odd value ranges in the LSM restarts so leaving as 0s for now.
! On quick tests, the only one of these that changes answers if NOT initialized
! at 0 is PSNOWHISTXY so making sure that one is covered in value initialization
! loop below in case we go back to undefined_real for these.
     PSNOWLIQXY     = 0.
     PSNOWTEMPXY    = 0.
     PSNOWDZXY      = 0.
     PSNOWHEATXY    = 0.
     PSNOWRHOXY     = 0.
     PSNOWSWEXY     = 0.
     PSNOWGRAN1XY   = 0.
     PSNOWGRAN2XY   = 0.
     PSNOWHISTXY    = 0.
     PSNOWALBXY     = 0.
     PSNOWTHRUFALXY = 0.
     PSNOWHEIGHTXY  = 0.
     PSNOWTOTSWEXY  = 0.
     FLOW_ICE       = 0.
     FLOW_SNOW      = 0.
     PSNOWAGEXY     = 0.

     WHERE(GLACINFO > 0.0) PSNOWALBXY=0.35

     do kk = 1, act_lev
        WHERE(GLACINFO > 0.0) PSNOWRHOXY  (:,kk,:) = 900.
        WHERE(GLACINFO > 0.0) PSNOWDZXY   (:,kk,:) = GLACT/act_lev
        WHERE(GLACINFO > 0.0) PSNOWSWEXY  (:,kk,:) = PSNOWDZXY(:,kk,:)*PSNOWRHOXY(:,KK,:)
        WHERE(GLACINFO > 0.0) PSNOWHEATXY (:,kk,:) = -PSNOWSWEXY(:,kk,:)*333231.05
        WHERE(GLACINFO > 0.0) PSNOWGRAN1XY(:,kk,:) = 0.0009
        WHERE(GLACINFO > 0.0) PSNOWGRAN2XY(:,kk,:) = 99    ! 1
        WHERE(GLACINFO > 0.0) PSNOWAGEXY  (:,kk,:) = 20000
        WHERE(GLACINFO > 0.0) PSNOWHISTXY  (:,kk,:) = 0.
     enddo

     call INI_CSTS
  end if ! crocus_opt /= 0


!------------------------------------------------------------------------
! For spatially-varying soil parameters, read in necessary extra fields
!------------------------------------------------------------------------

#ifdef SPATIAL_SOIL ! artless this could be an iopt_soil option like in noahmpr??
    call READ_3D_SOIL(state%SPATIAL_FILENAME, state%XSTART, state%XEND, state%YSTART, &
         state%YEND, state%NSOIL, state%BEXP_3D, state%SMCDRY_3D, &
         state%SMCWLT_3D, state%SMCREF_3D, state%SMCMAX_3D, state%DKSAT_3D,&
         state%DWSAT_3D, state%PSISAT_3D, state%QUARTZ_3D, state%REFDK_2D, &
         state%REFKDT_2D, state%SLOPE_2D, state%CWPVT_2D, state%VCMX25_2D, &
         state%MP_2D, state%HVT_2D, state%MFSNO_2D, state%RSURFEXP_2D, &
         state%SSI_2D, state%SNOWRETFAC_2D, state%TAU0_2D, state%RSURFSNOW_2D,&
         state%SCAMAX_2D)

    if (noah_lsm%runoff_option == 7) then ! artless
       call READ_XAJ_RUNOFF(state%SPATIAL_FILENAME, state%XSTART, state%XEND, state%YSTART, &
            YEND, state%AXAJ_2D, state%BXAJ_2D, state%XXAJ_2D)
    end if

    if (noah_lsm%imperv_option > 0 .and. noah_lsm%imperv_option < 9) then ! artless
        call READ_IMPERV(noah_lsm%SPATIAL_FILENAME,XSTART, XEND, YSTART, YEND, &
                             IMPERV_2D)
    end if

#endif

!------------------------------------------------------------------------
! For IOPT_RUN = 5 (MMF groundwater), read in necessary extra fields
! This option is not tested for parallel use in the offline driver
!------------------------------------------------------------------------

  if (noah_lsm%runoff_option == 5) then
     call READ_MMF_RUNOFF(noah_lsm%MMF_RUNOFF_FILE, state%XSTART, state%XEND, state%YSTART, &
          state%YEND, state%ZWTXY, state%EQZWT, state%RIVERBEDXY, &
          state%RIVERCONDXY, state%PEXPXY, state%FDEPTHXY)
  end if

!----------------------------------------------------------------------
! Initialize Model state
!----------------------------------------------------------------------

  state%SLOPETYP = 2 ! artless: noahmpr message ! it was 2 here and 1 in the noahmpdrv- pvk
  state%DZS = state%SOIL_THICK_INPUT(1:state%NSOIL)

  state%ITIMESTEP = 1

  xstart = State%xstart
  xend   = State%xend
  ystart = State%ystart
  yend   = State%yend
  ixfull = State%ixfull
  jxfull = State%jxfull


  if (state%restart_filename_requested /= " ") then
     state%restart_flag = .TRUE.

  if (noah_lsm%rst_bi_in .eq. 0) then

#ifdef WRF_HYDRO
     tmpdate = state%olddate
#endif

     call find_restart_file(state%rank, trim(state%restart_filename_requested), state%startdate, &
          khour, state%olddate, state%restart_flnm)  ! artless check khour

     call read_restart(trim(state%restart_flnm), xstart, xend, xstart, &
          ixfull, jxfull, state%nsoil, state%olddate)

#ifdef WRF_HYDRO
      state%olddate = tmpdate
#endif

       ITIMESTEP = 2

#ifdef MPP_LAND
     call mpp_land_bcast_char(19, state%olddate(1:19))
#endif

     call get_from_restart(xstart, xend, xstart, ixfull, jxfull, "SOIL_T"  , state%TSLB     )
     call get_from_restart(xstart, xend, xstart, ixfull, jxfull, "SNOW_T"  , state%TSNOXY   )
     call get_from_restart(xstart, xend, xstart, ixfull, jxfull, "SMC"     , state%SMOIS    )
     call get_from_restart(xstart, xend, xstart, ixfull, jxfull, "SH2O"    , state%SH2O     )
     call get_from_restart(xstart, xend, xstart, ixfull, jxfull, "ZSNSO"   , state%ZSNSOXY  )
     call get_from_restart(xstart, xend, xstart, ixfull, jxfull, "SNICE"   , state%SNICEXY  )
     call get_from_restart(xstart, xend, xstart, ixfull, jxfull, "SNLIQ"   , state%SNLIQXY  )
     call get_from_restart(xstart, xend, xstart, ixfull, jxfull, "QSNOW"   , state%QSNOWXY  )
     call get_from_restart(xstart, xend, xstart, ixfull, jxfull, "FWET"    , state%FWETXY   )
     call get_from_restart(xstart, xend, xstart, ixfull, jxfull, "SNEQVO"  , state%SNEQVOXY )
     call get_from_restart(xstart, xend, xstart, ixfull, jxfull, "EAH"     , state%EAHXY    )
     call get_from_restart(xstart, xend, xstart, ixfull, jxfull, "TAH"     , state%TAHXY    )
     call get_from_restart(xstart, xend, xstart, ixfull, jxfull, "ALBOLD"  , state%ALBOLDXY )
     call get_from_restart(xstart, xend, xstart, ixfull, jxfull, "CM"      , state%CMXY     )
     call get_from_restart(xstart, xend, xstart, ixfull, jxfull, "CH"      , state%CHXY     )
     call get_from_restart(xstart, xend, xstart, ixfull, jxfull, "ISNOW"   , state%ISNOWXY  )
     call get_from_restart(xstart, xend, xstart, ixfull, jxfull, "CANLIQ"  , state%CANLIQXY )
     call get_from_restart(xstart, xend, xstart, ixfull, jxfull, "CANICE"  , state%CANICEXY )
     call get_from_restart(xstart, xend, xstart, ixfull, jxfull, "SNEQV"   , state%SNOW     )
     call get_from_restart(xstart, xend, xstart, ixfull, jxfull, "SNOWH"   , state%SNOWH    )
     call get_from_restart(xstart, xend, xstart, ixfull, jxfull, "TV"      , state%TVXY     )
     call get_from_restart(xstart, xend, xstart, ixfull, jxfull, "TG"      , state%TGXY     )
     call get_from_restart(xstart, xend, xstart, ixfull, jxfull, "ZWT"     , state%ZWTXY    )
     call get_from_restart(xstart, xend, xstart, ixfull, jxfull, "WA"      , state%WAXY     )
     call get_from_restart(xstart, xend, xstart, ixfull, jxfull, "WT"      , state%WTXY     )
     call get_from_restart(xstart, xend, xstart, ixfull, jxfull, "WSLAKE"  , state%WSLAKEXY )
     call get_from_restart(xstart, xend, xstart, ixfull, jxfull, "LFMASS"  , state%LFMASSXY )
     call get_from_restart(xstart, xend, xstart, ixfull, jxfull, "RTMASS"  , state%RTMASSXY )
     call get_from_restart(xstart, xend, xstart, ixfull, jxfull, "STMASS"  , state%STMASSXY )
     call get_from_restart(xstart, xend, xstart, ixfull, jxfull, "WOOD"    , state%WOODXY   )
     call get_from_restart(xstart, xend, xstart, ixfull, jxfull, "STBLCP"  , state%STBLCPXY )
     call get_from_restart(xstart, xend, xstart, ixfull, jxfull, "FASTCP"  , state%FASTCPXY )
     call get_from_restart(xstart, xend, xstart, ixfull, jxfull, "LAI"     , state%LAI      )
     call get_from_restart(xstart, xend, xstart, ixfull, jxfull, "SAI"     , state%XSAIXY   )
     call get_from_restart(xstart, xend, xstart, ixfull, jxfull, "VEGFRA"  , state%VEGFRA   )
     call get_from_restart(xstart, xend, xstart, ixfull, jxfull, "GVFMIN"  , state%GVFMIN   )
     call get_from_restart(xstart, xend, xstart, ixfull, jxfull, "GVFMAX"  , state%GVFMAX   )
     call get_from_restart(xstart, xend, xstart, ixfull, jxfull, "ACMELT"  , state%ACSNOM   )
     call get_from_restart(xstart, xend, xstart, ixfull, jxfull, "ACSNOW"  , state%ACSNOW   )
     call get_from_restart(xstart, xend, xstart, ixfull, jxfull, "TAUSS"   , state%TAUSSXY  )
     call get_from_restart(xstart, xend, xstart, ixfull, jxfull, "QSFC"    , state%QSFC     )
     call get_from_restart(xstart, xend, xstart, ixfull, jxfull, "SFCRUNOFF",state%SFCRUNOFF   )
     call get_from_restart(xstart, xend, xstart, ixfull, jxfull, "UDRUNOFF" ,state%UDRUNOFF    )
     if (crocus_opt /= 0) then
        call get_from_restart(xstart, xend, xstart, ixfull, jxfull, "PSNOWAGE",     PSNOWAGEXY    )
        call get_from_restart(xstart, xend, xstart, ixfull, jxfull, "PSNOWDZ",      PSNOWDZXY     )
        call get_from_restart(xstart, xend, xstart, ixfull, jxfull, "PSNOWGRAN1",   PSNOWGRAN1XY  )
        call get_from_restart(xstart, xend, xstart, ixfull, jxfull, "PSNOWGRAN2",   PSNOWGRAN2XY  )
        call get_from_restart(xstart, xend, xstart, ixfull, jxfull, "PSNOWHEAT",    PSNOWHEATXY   )
        call get_from_restart(xstart, xend, xstart, ixfull, jxfull, "PSNOWHIST",    PSNOWHISTXY   )
        call get_from_restart(xstart, xend, xstart, ixfull, jxfull, "PSNOWLIQ",     PSNOWLIQXY    )
        call get_from_restart(xstart, xend, xstart, ixfull, jxfull, "PSNOWRHO",     PSNOWRHOXY    )
        call get_from_restart(xstart, xend, xstart, ixfull, jxfull, "PSNOWSWE",     PSNOWSWEXY    )
        call get_from_restart(xstart, xend, xstart, ixfull, jxfull, "PSNOWTEMP",    PSNOWTEMPXY   )
        call get_from_restart(xstart, xend, xstart, ixfull, jxfull, "PSNOWALB",     PSNOWALBXY    )
        call get_from_restart(xstart, xend, xstart, ixfull, jxfull, "PSNOWHEIGHT",  PSNOWHEIGHTXY )
        call get_from_restart(xstart, xend, xstart, ixfull, jxfull, "PSNOWTHRUFAL", PSNOWTHRUFALXY)
        call get_from_restart(xstart, xend, xstart, ixfull, jxfull, "PSNOWTOTSWE",  PSNOWTOTSWEXY )
        call get_from_restart(xstart, xend, xstart, ixfull, jxfull, "FLOW_SNOW",    FLOW_SNOW     )
        call get_from_restart(xstart, xend, xstart, ixfull, jxfull, "FLOW_ICE",     FLOW_ICE      )
     end if ! crocus_opt /= 0
#ifdef WRF_HYDRO
     if(checkRstV("ACCPRCP") .eq. 0) call get_from_restart(xstart, xend, xstart, ixfull, jxfull, "ACCPRCP"  ,ACCPRCP    )
     if(checkRstV("ACCECAN") .eq. 0) call get_from_restart(xstart, xend, xstart, ixfull, jxfull, "ACCECAN"  ,ACCECAN    )
     if(checkRstV("ACCEDIR") .eq. 0) call get_from_restart(xstart, xend, xstart, ixfull, jxfull, "ACCEDIR"  ,ACCEDIR    )
     if(checkRstV("ACCETRAN") .eq. 0) call get_from_restart(xstart, xend, xstart, ixfull, jxfull, "ACCETRAN" ,ACCETRAN    )
#endif
! below for opt_run = 5
     call get_from_restart(xstart, xend, xstart, ixfull, jxfull, "SMOISEQ"   , state%SMOISEQ    )
     call get_from_restart(xstart, xend, xstart, ixfull, jxfull, "AREAXY"    , state%AREAXY     )
     call get_from_restart(xstart, xend, xstart, ixfull, jxfull, "SMCWTDXY"  , state%SMCWTDXY   )
     call get_from_restart(xstart, xend, xstart, ixfull, jxfull, "QRFXY"     , state%QRFXY      )
     call get_from_restart(xstart, xend, xstart, ixfull, jxfull, "DEEPRECHXY", state%DEEPRECHXY )
     call get_from_restart(xstart, xend, xstart, ixfull, jxfull, "QSPRINGXY" , state%QSPRINGXY  )
     call get_from_restart(xstart, xend, xstart, ixfull, jxfull, "QSLATXY"   , state%QSLATXY    )
     call get_from_restart(xstart, xend, xstart, ixfull, jxfull, "QRFSXY"    , state%QRFSXY     )
     call get_from_restart(xstart, xend, xstart, ixfull, jxfull, "QSPRINGSXY", state%QSPRINGSXY )
     call get_from_restart(xstart, xend, xstart, ixfull, jxfull, "RECHXY"    , state%RECHXY     )
     call get_from_restart(xstart, xend, xstart, ixfull, jxfull, "FDEPTHXY"   ,state%FDEPTHXY   )
     call get_from_restart(xstart, xend, xstart, ixfull, jxfull, "RIVERCONDXY",state%RIVERCONDXY)
     call get_from_restart(xstart, xend, xstart, ixfull, jxfull, "RIVERBEDXY" ,state%RIVERBEDXY )
     call get_from_restart(xstart, xend, xstart, ixfull, jxfull, "EQZWT"      ,state%EQZWT      )
     call get_from_restart(xstart, xend, xstart, ixfull, jxfull, "PEXPXY"     ,state%PEXPXY     )
  else  !  rst_bi_in
#ifdef MPP_LAND
     call mpp_land_bcast_char(19,state%olddate(1:19))  ! artless, not in noahmpr
#endif
     ITIMESTEP = 2
     call lsm_rst_bi_in(state)
  end if

     state%STEPWTD = nint(state%WTDDT*60. / state%DTBL)
     state%STEPWTD = max(state%STEPWTD, 1)

! Must still call NOAHMPINITMAIN even in restart to set up parameter arrays (also done in WRF)
     call NoahmpInitMain(state)
     ! artless, need to check if our noahmpinitmain does something more for us?
     ! call NOAHMPINITMAIN(    LLANDUSE,     state%SNOW,    state%SNOWH,   CANWAT,   ISLTYP,   IVGTYP, &   ! call from WRF phys_init
     !                TSLB,    SMOIS,     SH2O,      DZS, FNDSOILW, FNDSNOWH, &
     !                 TSK,  ISNOWXY,     TVXY,     TGXY, CANICEXY,      TMN,     XICE, &
     !            CANLIQXY,    EAHXY,    TAHXY,     CMXY,     CHXY,                     &
     !              FWETXY, SNEQVOXY, ALBOLDXY,  QSNOWXY, WSLAKEXY,    ZWTXY,     WAXY, &
     !                WTXY,   state%TSNOXY,  state%ZSNSOXY,  state%SNICEXY,  state%SNLIQXY, LFMASSXY, RTMASSXY, &
     !            STMASSXY,   WOODXY, STBLCPXY, FASTCPXY,   XSAIXY, LAI,                    &
     !              T2MVXY,   T2MBXY, CHSTARXY,                                         &
     !               NSOIL,  .true.,                                                   &
     !              .true.,noah_lsm%runoff_option,noah_lsm%crop_option,noah_lsm%pedotransfer_option, &
     !              ids,ide+1, jds,jde+1, kds,kde,                &  ! domain
     !              ims,ime, jms,jme, kms,kme,                &  ! memory
     !              its,ite, jts,jte, kts,kte                 &  ! tile
     !                 ,smoiseq  ,smcwtdxy ,rechxy   ,deeprechxy, areaxy ,dx, dy, msftx, msfty,&
     !                 wtddt    ,stepwtd  ,dtbl  ,qrfsxy ,qspringsxy  ,qslatxy,                  &
     !                 fdepthxy ,terrain ,riverbedxy ,eqzwt ,rivercondxy ,pexpxy              &
     !                 )
  else  ! for none restart

     restart_flag = .FALSE.

     state%SMOIS      =  undefined_real
     state%TSLB       =  undefined_real
     state%SH2O       =  undefined_real
     state%CANLIQXY   =  undefined_real
     state%TSK        =  undefined_real
     state%RAINBL_tmp =  undefined_real
     !SNOW      =  undefined_real  ! ARTLESS
     !SNOWH     =  undefined_real

! LRK - Remove HRLDAS_ini_typ for WRF-Hydro. Originally, there was a fork
!       for difference forcing types, or if the user desired a different
!       reading from wrfinput. However, for WRF-Hydro uses, we only use
!       option 1 at this point, so removing all other potential options,
!       which was 0 before. This will force the user to place SHDMIN/SHDMAX
!       into the wrfinput file now as opposed to allowing it to be read
!       in from the geogrid file.
!#ifdef WRF_HYDRO
!  if((forc_typ .gt. 2) .and. (forc_typ .ne. 6) ) HRLDAS_ini_typ = 0

!  if(HRLDAS_ini_typ .eq. 1) then
!     ! read initial parameters and conditions from the HRLDAS forcing data
!     if(forc_typ .eq. 2) then
!          inflnm = trim(indir)//"/"//&
!          startdate(1:4)//startdate(6:7)//startdate(9:10)//startdate(12:13)//&
!          startdate(15:16)//".LDASIN_DOMAIN"//hgrid
!     else
!          inflnm = trim(indir)//"/"//&
!          startdate(1:4)//startdate(6:7)//startdate(9:10)//startdate(12:13)//&
!          ".LDASIN_DOMAIN"//hgrid
!     end if

!#else

!     inflnm = trim(indir)//"/"//&
!          startdate(1:4)//startdate(6:7)//startdate(9:10)//startdate(12:13)//&
!          ".LDASIN_DOMAIN"//hgrid
!#endif

     ! artless, just use state variable??
     call READINIT_HRLDAS(state%HRLDAS_SETUP_FILE, state%xstart, state%xend, state%ystart, &
          state%yend, state%NSOIL, state%DZS, state%OLDDATE, &
          LDASIN_VERSION, state%SMOIS, state%TSLB, state%CANWAT, &
          state%TSK, state%SNOW, state%SNOWH, state%FNDSNOWH)

!yw     VEGFRA    =  undefined_real
!yw     LAI       =  undefined_real
!yw     GVFMIN    =  undefined_real
!yw     GVFMAX    =  undefined_real

     call READVEG_HRLDAS(state%HRLDAS_SETUP_FILE, state%xstart, state%xend, state%ystart, &
          state%yend, state%OLDDATE, state%IVGTYP, state%VEGFRA, &
          state%LAI, state%GVFMIN, state%GVFMAX)

!#ifdef WRF_HYDRO
!  else   !  HRLDAS_ini_typ

!#ifdef MPP_LAND
!    call HYDRO_HRLDAS_ini_mpp   &
!#else
!    call HYDRO_HRLDAS_ini   &
!#endif
!       (trim(hrldas_setup_file), xend-xstart+1,yend-ystart+1, &
!       nsoil,SMOIS(:,1:NSOIL,:),TSLB(:,1:NSOIL,:),SH2O(:,1:NSOIL,:), CANWAT, TSK,SNOW,SNOWH,lai,VEGFRA,IVGTYP,FNDSNOWH)
!       if(maxval(VEGFRA) .le. 1)  VEGFRA = VEGFRA * 100

!       greenfrac = 0.0
!#ifdef MPP_LAND
!        call get_greenfrac_mpp &
!#else
!      call get_greenfrac &
!#endif
!         (trim(GEO_STATIC_FLNM),greenfrac, ix, jx, olddate, GVFMAX)
!        !yw GVFMAX = maxval(greenfrac)
!	if(maxval(GVFMAX) .le. 1)  GVFMAX = GVFMAX * 100
!  end if   ! initialization type
!#endif

!     SNOW = SNOW * 1000.    ! Convert snow water equivalent to mm. MB: remove v3.7

     FNDSOILW = .FALSE.
     call NoahmpInitMain(state)
     ! artless: make sure these subs are the same
     ! call NOAHMPINITMAIN(    LLANDUSE,     state%SNOW,    state%SNOWH,   CANWAT,   ISLTYP,   IVGTYP, &   ! call from WRF phys_init
     !                TSLB,    SMOIS,     SH2O,      DZS, FNDSOILW, FNDSNOWH, &
     !                 TSK,  ISNOWXY,     TVXY,     TGXY, CANICEXY,      TMN,     XICE, &
     !            CANLIQXY,    EAHXY,    TAHXY,     CMXY,     CHXY,                     &
     !              FWETXY, SNEQVOXY, ALBOLDXY,  QSNOWXY, WSLAKEXY,    ZWTXY,     WAXY, &
     !                WTXY,   state%TSNOXY,  state%ZSNSOXY,  state%SNICEXY,  state%SNLIQXY, LFMASSXY, RTMASSXY, &
     !            STMASSXY,   WOODXY, STBLCPXY, FASTCPXY,   XSAIXY, LAI,                    &
     !              T2MVXY,   T2MBXY, CHSTARXY,                                         &
     !               NSOIL,  .false.,                                                   &
     !              .true.,noah_lsm%runoff_option,noah_lsm%crop_option,noah_lsm%pedotransfer_option,  &
     !              ids,ide+1, jds,jde+1, kds,kde,                &  ! domain
     !              ims,ime, jms,jme, kms,kme,                &  ! memory
     !              its,ite, jts,jte, kts,kte                 &  ! tile
     !                 ,smoiseq  ,smcwtdxy ,rechxy   ,deeprechxy, areaxy ,dx, dy, msftx, msfty,&
     !                 wtddt    ,stepwtd  ,dtbl  ,qrfsxy ,qspringsxy  ,qslatxy,                  &
     !                 fdepthxy ,terrain ,riverbedxy ,eqzwt ,rivercondxy ,pexpxy              &
     !                 )

      state%TAUSSXY = 0.0   ! Need to be added to _INIT later

  end if  ! end of restart if block

  state%NTIME = (KHOUR)*3600./nint(state%dtbl) !*(NoahmpIO%spinup_loops+1)  !artless noahmpr addition

#ifdef HYDRO_D
  print*, "NTIME = ", NTIME , "KHOUR=",KHOUR,"dtbl = ", dtbl
#endif

  ! assinging the KHOUR to be used in the NWM output routing for global metadata
  nlst(did)%khour = khour

  call system_clock(count=state%clock_count_1)   ! Start a timer

  if (crocus_opt /= 0) &
     PSNOWTHRUFALXY(:,:) = 0.

#ifdef WRF_HYDRO
   allocate( etpnd     (ix,jx) )
   allocate( prcp0     (ix,jx) )

   prcp0     = 0   !artless are these qtiledrain or zwatble2d??
   etpnd     = 0.0

   allocate(zsoil(state%NSOIL))
   zsoil = 0

   zsoil(1) = -1 * state%soil_thick_input(1)
   do kk = 2, state%NSOIL
      zsoil(kk) = zsoil(kk-1)-state%soil_thick_input(kk)
   end do
#ifdef HYDRO_D
   print*, "zsoil/soil_thick_input = ", state%soil_thick_input(1:state%NSOIL)
#endif

   call hrldas_drv_HYDRO_ini(state%TSLB(:,1:state%NSOIL,:), &
        state%SMOIS(:,1:state%NSOIL,:), state%SH2O(:,1:state%NSOIL,:), &
        state%infxsrt, state%sfcheadrt, state%soldrain, &
        ! they have added state%qtiledrain,state%ZWATBLE2D,                     & ! ARTLESS
        state%ix, state%jx, state%NSOIL, state%SMOIS, &
        real(state%noah_timestep), state%olddate, zsoil(1:state%NSOIL))

call get_iocflag(1, io_config_outputs)

#ifdef WRF_HYDRO
if (io_config_outputs .gt. 0) then
     ACCPRCP = 0.0
     ACCECAN = 0.0
     ACCEDIR = 0.0
     ACCETRAN = 0.0
     state%SFCRUNOFF = 0.0
     state%UDRUNOFF = 0.0
     state%ACSNOM = 0.0
     state%ACSNOW = 0.0
end if
#endif

   !!--- Setup variable list. Change as needed; rest of parsing should adapt automatically.
   if (io_config_outputs .eq. 0) then
        VARLIST = 'IVGTYP,ISLTYP,FVEG,LAI,SAI,SWFORC,COSZ,LWFORC,RAINRATE,EMISS,FSA,FIRA,GRDFLX,HFX,LH,ECAN,EDIR,ALBEDO,' // &
                  'ETRAN,UGDRNOFF,SFCRNOFF,CANLIQ,CANICE,ZWT,WA,WT,ACCPRCP,ACCECAN,ACCEDIR,ACCETRAN,SAV,TR,EVC,IRC,SHC,' // &
                  'IRG,SHG,EVG,GHV,SAG,IRB,SHB,EVB,GHB,TRAD,TG,TV,TAH,TGV,TGB,T2MV,T2MB,Q2MV,Q2MB,EAH,FWET,ZSNSO_SN,SNICE,' // &
                  'SNLIQ,SOIL_T,SOIL_W,SNOW_T,SOIL_M,SNOWH,SNEQV,QSNOW,ISNOW,FSNO,ACSNOW,ACSNOM,CM,CH,CHV,CHB,CHLEAF,CHUC,' // &
                  'CHV2,CHB2,LFMASS,RTMASS,STMASS,WOOD,STBLCP,FASTCP,NEE,GPP,NPP,PSN,APAR,ACCET,CANWAT,SOILICE,SOILSAT_TOP,'// &
                  'SOILSAT,SNOWT_AVG,QRAIN,glacier,glacier_thickness,PSNOWALB,PSNOWTHRUFAL,PSNOWHEIGHT,PSNOWTOTSWE,'// &
                  'PSNOWGRAN1,PSNOWGRAN2,PSNOWAGE,PSNOWTEMP,PSNOWDZ,PSNOWHIST,PSNOWLIQ,PSNOWHEAT,PSNOWRHO,PSNOWSWE,'// &
                  'FLOW_ICE,FLOW_SNOW'

   end if
   if (io_config_outputs .eq. 1) then
        VARLIST = 'SNOWH,SNEQV,FSNO,SOILSAT_TOP,SNOWT_AVG,ACCET'
   end if
   if (io_config_outputs .eq. 2) then
        VARLIST = 'SNOWH,SNEQV,FSNO,SOILSAT_TOP,SNOWT_AVG,ACCET'
   end if
   if (io_config_outputs .eq. 3) then
        VARLIST = 'UGDRNOFF,ACSNOM,SNOWH,SNEQV,ACCECAN,ACCETRAN,ACCEDIR,SNLIQ,ISNOW,SOIL_T,FSNO,SOIL_M,GRDFLX,HFX,LH,FIRA,FSA,TRAD,SOILSAT_TOP,SNOWT_AVG,SOILICE,ACCET,CANWAT'
   end if
   if (io_config_outputs .eq. 4) then
        VARLIST = 'UGDRNOFF,SFCRNOFF,ACSNOM,SNEQV,SOILSAT_TOP,SOILSAT,ACCET,CANWAT,PET'
   end if
   if (io_config_outputs .eq. 5) then
        VARLIST = 'UGDRNOFF,SFCRNOFF,ACCET,SNEQV,SNOWH,FSNO,SOIL_M,SOIL_W,TRAD,FIRA,FSA,LH,HFX'
   end if
   if (io_config_outputs .eq. 6) then
        VARLIST = 'UGDRNOFF,SFCRNOFF,ACSNOM,SNOWH,SNEQV,ACCECAN,ACCETRAN,ACCEDIR,SNLIQ,ISNOW,SOIL_T,FSNO,SOIL_M,GRDFLX,HFX,LH,FIRA,FSA,TRAD,SOILSAT_TOP,SOILSAT,SNOWT_AVG,SOILICE,ACCET,CANWAT'
   end if

   !!--- Parse into character array. Constructor not valid with uneven
   !!--- strings in f90 so using brute force.

   do while (brkflag .eq. 0)
      if (index(VARLIST, ',') .eq. 0) then
          IOCVARS(varind) = adjustl(VARLIST)
          brkflag = 1
          if (varind > max_ioc_num_vars) then
#ifdef MPP_LAND
             call fatal_error_stop("ERROR: number of vars is greater than current max_num_vars")
#else
             stop "Error: number of vars is greater than current max_num_vars"
#endif
          end if
       else
#ifdef HYDRO_D
          if (len(adjustl(VARLIST(1:(index(VARLIST, ',')-1)))) > ioc_var_len) then
             print *,"WARNING: length of ", adjustl(VARLIST(1:(index(VARLIST, ',')-1))), &
                  " is longer than ioc_var_len"
          end if
#endif
          IOCVARS(varind) = adjustl(VARLIST(1:(index(VARLIST, ',')-1)))
          VARLIST = VARLIST((index(VARLIST, ',')+1):)
          varind = varind + 1
       end if
   end do

   if(state%finemesh .ne. 0 ) then ! finemesh was wrf_hydro
       if(state%restart_flag) then
          NTIME_out =  10
       else
          NTIME_out =  1
       end if
       return
   end if
#endif

   NTIME_out = state%NTIME

#ifdef WRF_HYDRO
   call get_t0OutputFlag(1, t0OutputFlag)
#ifdef MPP_LAND
   if(my_id .eq. io_id) &
       print*, "t0OutputFlag: ", t0OutputFlag
#endif

! ldas_output subroutine will be called when
! the t0OutputFlag is 1 in the hydro.namelist
! the ldas_output requires one variables ITIME
! which is the LSM timestep, we declare it here
! since it does not exist at this point
  ITIME = 0
  if(t0OutputFlag .eq. 1) call ldas_output(ITIME,state)
!#else
!  if (restart_filename_requested == " ") then
!     if(t0OutputFlag .eq. 1) call ldas_output()
!  end if
!#endif
#endif

end subroutine land_driver_ini

!===============================================================================
  subroutine land_driver_exe(itime, state)
    use module_hydro_io, only: read_channel_only
    use NoahmpIOVarType, only: NoahmpIO_type
    implicit  none
    type(NoahmpIO_type) :: state
    integer :: itime          ! timestep loop

!---------------------------------------------------------------------------------
! Read the forcing data.
!---------------------------------------------------------------------------------

#ifdef MPP_LAND
     call mpp_land_bcast_char(19,state%OLDDATE(1:19))
#endif

#ifdef WRF_HYDRO
!      if(forc_typ .eq. 8) then
!          call read_forc_ldasout(olddate,hgrid, indir, dtbl,ix,jx,infxsrt,soldrain)
!          call hrldas_drv_HYDRO(TSLB(:,1:NSOIL,:),SMOIS(:,1:NSOIL,:),SH2O(:,1:NSOIL,:),infxsrt,sfcheadrt,soldrain,ix,jx,NSOIL)
!          return
!      end if
#endif

#ifdef WRF_HYDRO
      if(state%forc_typ .eq. 8) then
         call read_forc_ldasout(state%olddate, state%hgrid, state%indir, state%dtbl, &
              state%ix, state%jx, state%infxsrt, state%soldrain)
          call hrldas_drv_HYDRO(state%TSLB(:,1:state%NSOIL,:), &
               state%SMOIS(:,1:state%NSOIL,:), state%SH2O(:,1:state%NSOIL,:), &
               state%infxsrt, state%sfcheadrt, state%soldrain, state%ix, &
               state%jx, state%NSOIL)
          call geth_newdate(state%newdate, state%olddate, nint(dtbl))
          state%olddate = state%newdate
          return
      end if
      if(state%forc_typ .eq. 9 .or. state%forc_typ .eq. 10) then
         !! JLM:: fix hgrid: This becomes 1 eventhough 3 is specified in hydro.namelist
         !! JLM: This is initalized by read_hrldas_hdrinfo
         !! JLM: Appears that we should differentiate the LSM and HYDRO igrids, define a local
         !! JLM: igrid for this purpose.
         !! JLM: ?*?* hrldas_drv_HYDRO.F should be made a module *?*?
         !! JLM:  Simple modification which forces type, rank and kind checking...
          call getNameList('igrid', igrid_hydro)  !! get hydro namelist info :: case sensitive
          write(hgrid_hydro,'(I1)') igrid_hydro
          call read_channel_only(state%olddate, hgrid_hydro, noah_lsm%indir, noah_lsm%forcing_timestep)
          call hrldas_drv_HYDRO(state%TSLB(:,1:state%NSOIL,:),&
               state%SMOIS(:,1:state%NSOIL,:),state%SH2O(:,1:state%NSOIL,:), &
               state%infxsrt, state%sfcheadrt, state%soldrain, state%ix, &
               state%jx, state%NSOIL)
          call geth_newdate(state%newdate, state%olddate, nint(state%dtbl))
          state%olddate = state%newdate
          return
      end if
#endif

! For HRLDAS, we're assuming (for now) that each time period is in a
! separate file.  So we can open a new one right now.

     state%inflnm = trim(noah_lsm%indir)//"/"//&
          state%olddate(1:4)//state%olddate(6:7)//state%olddate(9:10)//state%olddate(12:13)//&
          ".LDASIN_DOMAIN"//state%hgrid

     ! Build a filename template
     state%inflnm_template = trim(noah_lsm%indir)//"/<date>.LDASIN_DOMAIN"//state%hgrid


#ifdef WRF_HYDRO

     if(wrf_hydro%finemesh .ne. 0) goto 991

     if(state%forc_typ .eq. 0) then
        call READFORC_HRLDAS(state%INFLNM_TEMPLATE, state%FORCING_TIMESTEP, &
             state%OLDDATE, state%XSTART, state%XEND, state%YSTART, &
             state%YEND, state%T_PHY(:,1,:), state%QV_CURR(:,1,:), state%U_PHY(:,1,:), &
             state%V_PHY(:,1,:), state%P8W(:,1,:), state%GLW, state%SWDOWN, &
             state%RAINBL_tmp, state%VEGFRA, state%update_veg, state%LAI, &
             state%update_lai)
        ! REFACTOR, they have this! what to do with this?
        ! state%VEGFRA = state%VEGFRA * 100.
     else
        if(state%olddate == forcDate) then
           call HYDRO_frocing_drv(trim(state%indir), state%forc_typ, state%snow_assim, &
                state%olddate, state%xstart, state%xend, state%ystart, state%yend,    &
                noah_lsm%forcing_name_T ,noah_lsm%forcing_name_Q ,noah_lsm%forcing_name_U , &
                noah_lsm%forcing_name_V ,noah_lsm%forcing_name_P ,noah_lsm%forcing_name_LW, &
                noah_lsm%forcing_name_SW,noah_lsm%forcing_name_PR,noah_lsm%forcing_name_SN, &
                noah_lsm%forcing_name_LF, &
                state%T_PHY(:,1,:), state%QV_CURR(:,1,:), state%U_PHY(:,1,:), state%V_PHY(:,1,:), &
                state%P8W(:,1,:), state%GLW, state%SWDOWN, state%RAINBL_tmp, &
                state%LAI, state%VEGFRA, state%SNOWH, ITIME, &
                state%FORCING_TIMESTEP, prcp0)

               if(maxval(state%VEGFRA) .le. 1) state%VEGFRA = state%VEGFRA * 100

           call geth_newdate(state%newdate, forcDate, state%FORCING_TIMESTEP)
           forcDate = state%newdate
        end if
     end if

#else
     call READFORC_HRLDAS(state%INFLNM_TEMPLATE, state%FORCING_TIMESTEP, state%OLDDATE, &
          state%XSTART, state%XEND, state%YSTART, state%YEND, &
          noah_lsm%forcing_name_T ,noah_lsm%forcing_name_Q ,noah_lsm%forcing_name_U , &
	  noah_lsm%forcing_name_V ,noah_lsm%forcing_name_P ,noah_lsm%forcing_name_LW, &
	  noah_lsm%forcing_name_SW,noah_lsm%forcing_name_PR,noah_lsm%forcing_name_SN, &
	  noah_lsm%forcing_name_LF, &
          state%T_PHY(:,1,:), state%QV_CURR(:,1,:), state%U_PHY(:,1,:), state%V_PHY(:,1,:), &
	  state%P8W(:,1,:), state%GLW, state%SWDOWN, state%RAINBL_tmp, &
          state%VEGFRA, state%update_veg, state%LAI, state%update_lai)

     ! REFACTOR TODO: THIS WAS IN NOAHMPR
     ! if(maxval(state%VEGFRA) .le. 1) state%VEGFRA = state%VEGFRA * 100
#endif

991  continue

     where(state%XLAND > 1.5) state%T_PHY(:,1,:)   = 0.0  ! Prevent some overflow problems with ifort compiler [MB:20150812]
     where(state%XLAND > 1.5) state%U_PHY(:,1,:)   = 0.0
     where(state%XLAND > 1.5) state%V_PHY(:,1,:)   = 0.0
     where(state%XLAND > 1.5) state%QV_CURR(:,1,:) = 0.0
     where(state%XLAND > 1.5) state%P8W(:,1,:)     = 0.0
     where(state%XLAND > 1.5) state%GLW            = 0.0
     where(state%XLAND > 1.5) state%SWDOWN         = 0.0
     where(state%XLAND > 1.5) state%RAINBL_tmp     = 0.0
     where(state%XLAND > 1.5) state%SNOWBL         = 0.0

     state%QV_CURR(:,1,:) = state%QV_CURR(:,1,:) / &
          (1.0 - state%QV_CURR(:,1,:))  ! Assuming input forcing are specific hum.;
                                        ! WRF wants mixing ratio at driver level
     state%P8W(:,2,:)     = state%P8W(:,1,:) ! WRF uses lowest two layers
     state%T_PHY(:,2,:)   = state%T_PHY(:,1,:) ! Only pressure is needed in two layer but fill the rest
     state%U_PHY(:,2,:)   = state%U_PHY(:,1,:)
     state%V_PHY(:,2,:)   = state%V_PHY(:,1,:)
     state%QV_CURR(:,2,:) = state%QV_CURR(:,1,:)
     state%RAINBL = state%RAINBL_tmp * state%DTBL ! RAINBL in WRF is [mm]
     state%SNOWBL = state%SNOWBL * state%DTBL
     ! REFACTOR TODO: SR was commented out in commit, should be in?
     state%SR         = 0.0 ! Will only use component if opt_snf=4
     state%RAINCV     = 0.0
     state%RAINNCV    = state%RAINBL
     state%RAINSHV    = 0.0
     state%SNOWNCV    = state%SNOWBL  ! REFACTOR TODO: in noahmpr it is NoahmpIO%SNOWBL, changing to this
     state%GRAUPELNCV = 0.0
     state%HAILNCV    = 0.0
     state%DZ8W = 2 * state%ZLVL ! 2* to be consistent with WRF model level

!------------------------------------------------------------------------
! Noah-MP updates we can do before spatial loop.
!------------------------------------------------------------------------

   ! create a few fields that are IN in WRF - coszen, julian,yr

    do J = state%YSTART, state%YEND
    do I = state%XSTART, state%XEND
       call CALC_DECLIN(state%OLDDATE(1:19), state%XLAT(I,J), state%XLONG(I,J), state%COSZEN(I,J), &
            state%JULIAN)
    end do
    end do

    read(state%OLDDATE(1:4),*)  state%YR
    state%YEARLEN = 365 ! find length of year for phenology (also S Hemisphere)
    if (mod(state%YR,4) == 0) then
       state%YEARLEN = 366
       if (mod(state%YR,100) == 0) then
          state%YEARLEN = 365
          if (mod(state%YR,400) == 0) then
             state%YEARLEN = 366
          end if
       end if
    end if

    if (ITIME == 1 .and. .not. state%RESTART_FLAG ) then
       state%EAHXY = (state%P8W(:,1,:)*state%QV_CURR(:,1,:)) / &
            (0.622+state%QV_CURR(:,1,:)) ! Initial guess only.
       state%TAHXY = state%T_PHY(:,1,:) ! Initial guess only.
       state%CHXY = 0.1
       state%CMXY = 0.1
    end if

!------------------------------------------------------------------------
! Skip model call at t=1 since initial conditions are at start time; First model time is +1
!------------------------------------------------------------------------

   if (ITIME > 0) then

!------------------------------------------------------------------------
! Call to Noah-MP driver same as surface_driver
!------------------------------------------------------------------------
      state%sflx_count_sum = 0 ! Timing

      ! Timing information for SFLX:

      call system_clock(count=state%count_before_sflx, count_rate=state%clock_rate)

      call NoahmpDriverMain(state)
! artless how to handle spatial_soil??
! #ifdef SPATIAL_SOIL
!       BEXP_3D,SMCDRY_3D,SMCWLT_3D,SMCREF_3D,SMCMAX_3D,             &
!            DKSAT_3D,DWSAT_3D,PSISAT_3D,QUARTZ_3D,                       &
!            REFDK_2D,REFKDT_2D,SLOPE_2D,                                 &
!            CWPVT_2D,VCMX25_2D,MP_2D,HVT_2D,MFSNO_2D,RSURFEXP_2D,        &
!            AXAJ_2D,BXAJ_2D,XXAJ_2D,                                     &
!            IMPERV_2D,                                                   &
!            SSI_2D,SNOWRETFAC_2D,TAU0_2D,RSURFSNOW_2D,SCAMAX_2D,         &
! #endif
! #ifdef WRF_HYDRO
!            sfcheadrt,INFXSRT,soldrain,                          &    !O
! #endif
!            ! variables below are optional
!            MP_RAINC =  RAINCV, MP_RAINNC =    RAINNCV, MP_SHCV = RAINSHV,&
!            MP_SNOW  = SNOWNCV, MP_GRAUP  = GRAUPELNCV, MP_HAIL = HAILNCV, &
!            VIS_ICEALB=VIS_ICEALB &
! #ifdef WRF_HYDRO
!            , ACCPRCP=ACCPRCP,  ACCECAN=ACCECAN, ACCETRAN=ACCETRAN,  ACCEDIR=ACCEDIR  &
!            , SOILSAT_TOP=SOILSAT_TOP, SOILSAT=SOILSAT, SOILICE=SOILICE, SNOWT_AVG=SNOWT_AVG      &
! #endif

      call system_clock(count=state%count_after_sflx, count_rate=state%clock_rate)
      state%sflx_count_sum = state%sflx_count_sum + &
           (state%count_after_sflx - state%count_before_sflx)

  if (noah_lsm%RUNOFF_OPTION == 5 .and. mod(ITIME,STEPWTD) == 0) then
           call wrf_message('calling WTABLE' )

!gmm update wtable from lateral flow and shed water to rivers
           CALL WTABLE_MMF_NOAHMP(state, &
                state%NUM_SOIL_LAYERS, state%XLAND, state%XICE, state%XICE_THRESHOLD, &
                state%ISICE, state%ISLTYP, state%SMOISEQ, state%DZS, &
                state%WTDDT, state%FDEPTHXY, state%AREAXY, state%TERRAIN, &
                state%ISURBAN, state%IVGTYP, state%RIVERCONDXY, state%RIVERBEDXY, &
                state%EQZWT, state%PEXPXY, state%SMOIS, state%SH2O, &
                state%SMCWTDXY, state%ZWTXY, state%QRFXY, state%DEEPRECHXY, &
                state%QSPRINGXY, state%QSLATXY, state%QRFSXY, state%QSPRINGSXY, &
                state%RECHXY, state%IDS, state%IDE, state%JDS, &
                state%JDE, state%KDS, state%KDE, state%IMS, &
                state%IME, state%JMS, state%JME, state%KMS, &
                state%KME, state%ITS, state%ITE, state%JTS, &
                state%JTE, state%KTS,state%KTE)

 end if

!------------------------------------------------------------------------
! END of surface_driver consistent code
!------------------------------------------------------------------------

 end if   ! SKIP FIRST TIMESTEP

#ifdef WRF_HYDRO
     call geth_newdate(state%newdate, state%olddate, nint(state%dtbl))
     state%olddate = state%newdate
     call hrldas_drv_HYDRO(state%TSLB(:,1:state%NSOIL,:), state%SMOIS(:,1:state%NSOIL,:), &
          state%SH2O(:,1:state%NSOIL,:), state%infxsrt, state%sfcheadrt, state%soldrain, &
          state%ix, state%jx, state%NSOIL)
#endif

! Output for history
     OUTPUT_FOR_HISTORY: if (state%output_timestep > 0) then
        if (mod(ITIME * state%noah_timestep, state%output_timestep) == 0) then
           ! convert RAINRATE back to mm/s for output
           state%RAINBL = state%RAINBL_tmp
           call ldas_output(ITIME,state)
        end if
     end if OUTPUT_FOR_HISTORY

     if (state%IVGTYP(state%xstart, state%ystart) == state%ISWATER) then
        write(*,'(" ***DATE=", A19)', advance="NO") state%olddate
     else
        write(*,'(" ***DATE=", A19, 6F10.5)', advance="NO") state%olddate, &
             state%TSLB(state%xstart,1,state%ystart), &
             state%LAI(state%xstart,state%ystart)
     end if

!------------------------------------------------------------------------
! Write Restart - timestamp equal to output will have same states
!------------------------------------------------------------------------

      if ( (state%restart_frequency_hours .gt. 0) .and. &
           (mod(ITIME, int(state%restart_frequency_hours*3600./nint(state%dtbl))) == 0)) then
       if(noah_lsm%rst_bi_out .eq. 0) then ! artless, rst_bi_out, why not in noahmpr
           call lsm_restart(state)
       else
           call lsm_rst_bi_out(state)
       end if
#ifdef WRF_HYDRO
      else
       if (state%restart_frequency_hours <= 0) then
          if ( (state%olddate( 9:10) == "01") .and. (state%olddate(12:13) == "00") .and. &
               (state%olddate(15:16) == "00") .and. (state%olddate(18:19) == "00") ) then
               if(noah_lsm%rst_bi_out .eq. 0) then
                   call lsm_restart(state)  ! jlm - i moved all the restart code to a subroutine.
               else
                   call lsm_rst_bi_out(state)
               end if
          end if
       end if
#endif
      end if

!------------------------------------------------------------------------
! Advance the time
!------------------------------------------------------------------------
#ifndef WRF_HYDRO
     call geth_newdate(state%newdate, state%olddate, nint(state%dtbl))
     state%olddate = state%newdate
#endif

! update the timer
     call system_clock(count=state%clock_count_2, count_rate=state%clock_rate)
     state%timing_sum = state%timing_sum + &
          float(state%clock_count_2-state%clock_count_1)/float(state%clock_rate)
     write(*,'("    Timing: ",f6.2," Cumulative:  ", f10.2, "  SFLX: ", f6.2 )') &
          float(state%clock_count_2-state%clock_count_1)/float(state%clock_rate), &
          state%timing_sum, real(state%sflx_count_sum) / real(state%clock_rate)
     state%clock_count_1 = state%clock_count_2

     state%ITIMESTEP = state%ITIMESTEP + 1
end subroutine land_driver_exe

!!===============================================================================
subroutine  ldas_output(itime, state)
  use NoahmpIOVarType, only: NoahmpIO_type
  type(NoahmpIO_type) :: state
  integer, intent(in)  :: itime ! time step of the LSM

!#ifdef WRF_HYDRO
!if ( (io_config_outputs .eq. 0) ) then
!#endif
!#ifndef WRF_HYDRO
!           call prepare_output_file (trim(outdir), version, &
!                igrid, output_timestep, llanduse, split_output_count, hgrid,                &
!                ixfull, jxfull, ixpar, jxpar, xstartpar, ystartpar,                         &
!                iswater, mapproj, lat1, lon1, dx, dy, truelat1, truelat2, cen_lon,          &
!                nsoil, nsnow, dzs, startdate, olddate, IVGTYP, ISLTYP)
!
!           DEFINE_MODE_LOOP : do imode = 1, 2
!
!              call set_output_define_mode(imode)
!
!              ! For 3D arrays, we need to know whether the Z dimension is snow layers, or soil layers.
!
!        ! Properties - Assigned or predicted
!              call add_to_output(IVGTYP     , "IVGTYP"  , "Dominant vegetation category"         , "category"              )
!              call add_to_output(ISLTYP     , "ISLTYP"  , "Dominant soil category"               , "category"              )
!              call add_to_output(FVEGXY     , "FVEG"    , "Green Vegetation Fraction"              , "-"                   )
!              call add_to_output(LAI        , "LAI"     , "Leaf area index"                      , "-"                     )
!              call add_to_output(XSAIXY     , "SAI"     , "Stem area index"                      , "-"                     )
!        ! Forcing
!              call add_to_output(SWDOWN     , "SWFORC"  , "Shortwave forcing"                    , "W m{-2}"               )
!              call add_to_output(COSZEN     , "COSZ"    , "Cosine of zenith angle"                    , "W m{-2}"               )
!              call add_to_output(GLW        , "LWFORC"  , "Longwave forcing"                    , "W m{-2}"               )
!              call add_to_output(RAINBL     , "RAINRATE", "Precipitation rate"                   , "kg m{-2} s{-1}"        )
!        ! Grid energy budget terms
!              call add_to_output(EMISS      , "EMISS"   , "Grid emissivity"                    , ""               )
!              call add_to_output(FSAXY      , "FSA"     , "Total absorbed SW radiation"          , "W m{-2}"               )
!              call add_to_output(FIRAXY     , "FIRA"    , "Total net LW radiation to atmosphere" , "W m{-2}"               )
!              call add_to_output(GRDFLX     , "GRDFLX"  , "Heat flux into the soil"              , "W m{-2}"               )
!              call add_to_output(HFX        , "HFX"     , "Total sensible heat to atmosphere"    , "W m{-2}"               )
!              call add_to_output(LH         , "LH"      , "Total latent heat to atmosphere"    , "W m{-2}"               )
!              call add_to_output(ECANXY     , "ECAN"    , "Canopy water evaporation rate"        , "kg m{-2} s{-1}"        )
!              call add_to_output(EDIRXY     , "EDIR"    , "Direct from soil evaporation rate"    , "kg m{-2} s{-1}"        )
!              call add_to_output(ALBEDO     , "ALBEDO"  , "Surface albedo"                         , "-"                   )
!              call add_to_output(ETRANXY    , "ETRAN"   , "Transpiration rate"                   , "kg m{-2} s{-1}"        )
!        ! Grid water budget terms - in addition to above
!              call add_to_output(UDRUNOFF   , "UGDRNOFF", "Accumulated underground runoff"       , "mm"                    )
!              call add_to_output(SFCRUNOFF  , "SFCRNOFF", "Accumulatetd surface runoff"          , "mm"                    )
!              call add_to_output(CANLIQXY   , "CANLIQ"  , "Canopy liquid water content"          , "mm"                    )
!              call add_to_output(CANICEXY   , "CANICE"  , "Canopy ice water content"             , "mm"                    )
!              call add_to_output(ZWTXY      , "ZWT"     , "Depth to water table"                 , "m"                     )
!              call add_to_output(WAXY       , "WA"      , "Water in aquifer"                     , "kg m{-2}"              )
!              call add_to_output(WTXY       , "WT"      , "Water in aquifer and saturated soil"  , "kg m{-2}"              )
!        ! Additional needed to close the canopy energy budget
!              call add_to_output(SAVXY      , "SAV"     , "Solar radiative heat flux absorbed by vegetation", "W m{-2}"    )
!              call add_to_output(TRXY       , "TR"      , "Transpiration heat"                     , "W m{-2}"             )
!              call add_to_output(EVCXY      , "EVC"     , "Canopy evap heat"                       , "W m{-2}"             )
!              call add_to_output(IRCXY      , "IRC"     , "Canopy net LW rad"                      , "W m{-2}"             )
!              call add_to_output(SHCXY      , "SHC"     , "Canopy sensible heat"                   , "W m{-2}"             )
!        ! Additional needed to close the under canopy ground energy budget
!              call add_to_output(IRGXY      , "IRG"     , "Ground net LW rad"                      , "W m{-2}"             )
!              call add_to_output(SHGXY      , "SHG"     , "Ground sensible heat"                   , "W m{-2}"             )
!              call add_to_output(EVGXY      , "EVG"     , "Ground evap heat"                       , "W m{-2}"             )
!              call add_to_output(GHVXY      , "GHV"     , "Ground heat flux + to soil vegetated"   , "W m{-2}"             )
!        ! Needed to close the bare ground energy budget
!              call add_to_output(SAGXY      , "SAG"     , "Solar radiative heat flux absorbed by ground", "W m{-2}"        )
!              call add_to_output(IRBXY      , "IRB"     , "Net LW rad to atm bare"                 , "W m{-2}"             )
!              call add_to_output(SHBXY      , "SHB"     , "Sensible heat to atm bare"              , "W m{-2}"             )
!              call add_to_output(EVBXY      , "EVB"     , "Evaporation heat to atm bare"           , "W m{-2}"             )
!              call add_to_output(GHBXY      , "GHB"     , "Ground heat flux + to soil bare"        , "W m{-2}"             )
!        ! Above-soil temperatures
!              call add_to_output(TRADXY     , "TRAD"    , "Surface radiative temperature"        , "K"                     )
!              call add_to_output(TGXY       , "TG"      , "Ground temperature"                   , "K"                     )
!              call add_to_output(TVXY       , "TV"      , "Vegetation temperature"               , "K"                     )
!              call add_to_output(TAHXY      , "TAH"     , "Canopy air temperature"               , "K"                     )
!              call add_to_output(TGVXY      , "TGV"     , "Ground surface Temp vegetated"          , "K"                   )
!              call add_to_output(TGBXY      , "TGB"     , "Ground surface Temp bare"               , "K"                   )
!              call add_to_output(T2MVXY     , "T2MV"    , "2m Air Temp vegetated"                  , "K"                   )
!              call add_to_output(T2MBXY     , "T2MB"    , "2m Air Temp bare"                       , "K"                   )
!        ! Above-soil moisture
!              call add_to_output(Q2MVXY     , "Q2MV"    , "2m mixing ratio vegetated"              , "kg/kg"               )
!              call add_to_output(Q2MBXY     , "Q2MB"    , "2m mixing ratio bare"                   , "kg/kg"               )
!              call add_to_output(EAHXY      , "EAH"     , "Canopy air vapor pressure"            , "Pa"                    )
!              call add_to_output(FWETXY     , "FWET"    , "Wetted or snowed fraction of canopy"  , "fraction"              )
!        ! Snow and soil - 3D terms
!              call add_to_output(ZSNSOXY(:,-nsnow+1:0,:),  "ZSNSO_SN" , "Snow layer depths from snow surface", "m", "SNOW")
!              call add_to_output(SNICEXY    , "SNICE"   , "Snow layer ice"                       , "mm"             , "SNOW")
!              call add_to_output(SNLIQXY    , "SNLIQ"   , "Snow layer liquid water"              , "mm"             , "SNOW")
!              call add_to_output(TSLB       , "SOIL_T"  , "soil temperature"                     , "K"              , "SOIL")
!              call add_to_output(SH2O       , "SOIL_W"  , "liquid volumetric soil moisture"      , "m3 m-3"         , "SOIL")
!              call add_to_output(TSNOXY     , "SNOW_T"  , "snow temperature"                     , "K"              , "SNOW")
!              call add_to_output(SMOIS      , "SOIL_M"  , "volumetric soil moisture"             , "m{3} m{-3}"     , "SOIL")
!        ! Snow - 2D terms
!              call add_to_output(SNOWH      , "SNOWH"   , "Snow depth"                           , "m"                     )
!              call add_to_output(SNOW       , "SNEQV"   , "Snow water equivalent"                , "kg m{-2}"              )
!              call add_to_output(QSNOWXY    , "QSNOW"   , "Snowfall rate"                        , "mm s{-1}"              )
!              call add_to_output(ISNOWXY    , "ISNOW"   , "Number of snow layers"                , "count"                 )
!              call add_to_output(SNOWC      , "FSNO"    , "Snow-cover fraction on the ground"      , ""                    )
!              call add_to_output(ACSNOW     , "ACSNOW"  , "accumulated snow fall"                  , "mm"                  )
!              call add_to_output(ACSNOM     , "ACSNOM"  , "accumulated melting water out of snow bottom" , "mm"            )
!        ! Exchange coefficients
!              call add_to_output(CMXY       , "CM"      , "Momentum drag coefficient"            , ""                      )
!              call add_to_output(CHXY       , "CH"      , "Sensible heat exchange coefficient"   , ""                      )
!              call add_to_output(CHVXY      , "CHV"     , "Exchange coefficient vegetated"         , "m s{-1}"             )
!              call add_to_output(CHBXY      , "CHB"     , "Exchange coefficient bare"              , "m s{-1}"             )
!              call add_to_output(CHLEAFXY   , "CHLEAF"  , "Exchange coefficient leaf"              , "m s{-1}"             )
!              call add_to_output(CHUCXY     , "CHUC"    , "Exchange coefficient bare"              , "m s{-1}"             )
!              call add_to_output(CHV2XY     , "CHV2"    , "Exchange coefficient 2-meter vegetated" , "m s{-1}"             )
!              call add_to_output(CHB2XY     , "CHB2"    , "Exchange coefficient 2-meter bare"      , "m s{-1}"             )
!        ! Carbon allocation model
!              call add_to_output(LFMASSXY   , "LFMASS"  , "Leaf mass"                            , "g m{-2}"               )
!              call add_to_output(RTMASSXY   , "RTMASS"  , "Mass of fine roots"                   , "g m{-2}"               )
!              call add_to_output(STMASSXY   , "STMASS"  , "Stem mass"                            , "g m{-2}"               )
!              call add_to_output(WOODXY     , "WOOD"    , "Mass of wood and woody roots"         , "g m{-2}"               )
!              call add_to_output(STBLCPXY   , "STBLCP"  , "Stable carbon in deep soil"           , "g m{-2}"               )
!              call add_to_output(FASTCPXY   , "FASTCP"  , "Short-lived carbon in shallow soil"   , "g m{-2}"               )
!              call add_to_output(NEEXY      , "NEE"     , "Net ecosystem exchange"                 , "g m{-2} s{-1} CO2"   )
!              call add_to_output(GPPXY      , "GPP"     , "Net instantaneous assimilation"         , "g m{-2} s{-1} C"     )
!              call add_to_output(NPPXY      , "NPP"     , "Net primary productivity"               , "g m{-2} s{-1} C"     )
!              call add_to_output(PSNXY      , "PSN"     , "Total photosynthesis"                   , "umol CO@ m{-2} s{-1}")
!              call add_to_output(APARXY     , "APAR"    , "Photosynthesis active energy by canopy" , "W m{-2}"             )
!
!        ! Carbon allocation model
!	    if(RUNOFF_OPTION == 5) THEN
!              call add_to_output(SMCWTDXY   , "SMCWTD"   , "Leaf mass"                            , "g m{-2}"               )
!              call add_to_output(RECHXY     , "RECH"     , "Mass of fine roots"                   , "g m{-2}"               )
!              call add_to_output(QRFSXY     , "QRFS"     , "Stem mass"                            , "g m{-2}"               )
!              call add_to_output(QSPRINGSXY , "QSPRINGS" , "Mass of wood and woody roots"         , "g m{-2}"               )
!              call add_to_output(QSLATXY    , "QSLAT"    , "Stable carbon in deep soil"           , "g m{-2}"               )
!	    end if
!
!           enddo DEFINE_MODE_LOOP
!
!           call finalize_output_file(split_output_count)
!#ifdef WRF_HYDRO
!else
!#endif
!#endif /* WRF_HYDRO */

#ifdef WRF_HYDRO
   ! Logan add begin
   ! Go through each variable. For the first time the NWM output routine is
   ! called, the file is created and all output variables (desired per flags),
   ! are created in define mode.

   call output_NoahMP_NWM(trim(noah_lsm%outdir),igrid,noah_lsm%output_timestep,ITIME,startdate,olddate,ixpar,jxpar,1,float(IVGTYP),IVGTYP,1)
   call output_NoahMP_NWM(trim(noah_lsm%outdir),igrid,noah_lsm%output_timestep,itime,startdate,olddate,ixpar,jxpar,1,float(ISLTYP),IVGTYP,2)
   call output_NoahMP_NWM(trim(noah_lsm%outdir),igrid,noah_lsm%output_timestep,itime,startdate,olddate,ixpar,jxpar,1,FVEGXY,IVGTYP,3)
   call output_NoahMP_NWM(trim(noah_lsm%outdir),igrid,noah_lsm%output_timestep,itime,startdate,olddate,ixpar,jxpar,1,LAI,IVGTYP,4)
   call output_NoahMP_NWM(trim(noah_lsm%outdir),igrid,noah_lsm%output_timestep,itime,startdate,olddate,ixpar,jxpar,1,XSAIXY,IVGTYP,5)
   call output_NoahMP_NWM(trim(noah_lsm%outdir),igrid,noah_lsm%output_timestep,itime,startdate,olddate,ixpar,jxpar,1,SWDOWN,IVGTYP,6)
   call output_NoahMP_NWM(trim(noah_lsm%outdir),igrid,noah_lsm%output_timestep,itime,startdate,olddate,ixpar,jxpar,1,COSZEN,IVGTYP,7)
   call output_NoahMP_NWM(trim(noah_lsm%outdir),igrid,noah_lsm%output_timestep,itime,startdate,olddate,ixpar,jxpar,1,GLW,IVGTYP,8)
   call output_NoahMP_NWM(trim(noah_lsm%outdir),igrid,noah_lsm%output_timestep,itime,startdate,olddate,ixpar,jxpar,1,RAINBL,IVGTYP,9)
   call output_NoahMP_NWM(trim(noah_lsm%outdir),igrid,noah_lsm%output_timestep,itime,startdate,olddate,ixpar,jxpar,1,EMISS,IVGTYP,10)
   call output_NoahMP_NWM(trim(noah_lsm%outdir),igrid,noah_lsm%output_timestep,itime,startdate,olddate,ixpar,jxpar,1,FSAXY,IVGTYP,11)
   call output_NoahMP_NWM(trim(noah_lsm%outdir),igrid,noah_lsm%output_timestep,itime,startdate,olddate,ixpar,jxpar,1,FIRAXY,IVGTYP,12)
   call output_NoahMP_NWM(trim(noah_lsm%outdir),igrid,noah_lsm%output_timestep,itime,startdate,olddate,ixpar,jxpar,1,GRDFLX,IVGTYP,13)
   call output_NoahMP_NWM(trim(noah_lsm%outdir),igrid,noah_lsm%output_timestep,itime,startdate,olddate,ixpar,jxpar,1,HFX,IVGTYP,14)
   call output_NoahMP_NWM(trim(noah_lsm%outdir),igrid,noah_lsm%output_timestep,itime,startdate,olddate,ixpar,jxpar,1,LH,IVGTYP,15)
   call output_NoahMP_NWM(trim(noah_lsm%outdir),igrid,noah_lsm%output_timestep,itime,startdate,olddate,ixpar,jxpar,1,ECANXY,IVGTYP,16)
   call output_NoahMP_NWM(trim(noah_lsm%outdir),igrid,noah_lsm%output_timestep,itime,startdate,olddate,ixpar,jxpar,1,EDIRXY,IVGTYP,17)
   call output_NoahMP_NWM(trim(noah_lsm%outdir),igrid,noah_lsm%output_timestep,itime,startdate,olddate,ixpar,jxpar,1,ALBEDO,IVGTYP,18)
   call output_NoahMP_NWM(trim(noah_lsm%outdir),igrid,noah_lsm%output_timestep,itime,startdate,olddate,ixpar,jxpar,1,ETRANXY,IVGTYP,19)
   call output_NoahMP_NWM(trim(noah_lsm%outdir),igrid,noah_lsm%output_timestep,itime,startdate,olddate,ixpar,jxpar,1,UDRUNOFF,IVGTYP,20)
   call output_NoahMP_NWM(trim(noah_lsm%outdir),igrid,noah_lsm%output_timestep,itime,startdate,olddate,ixpar,jxpar,1,SFCRUNOFF,IVGTYP,21)
   call output_NoahMP_NWM(trim(noah_lsm%outdir),igrid,noah_lsm%output_timestep,itime,startdate,olddate,ixpar,jxpar,1,CANLIQXY,IVGTYP,22)
   call output_NoahMP_NWM(trim(noah_lsm%outdir),igrid,noah_lsm%output_timestep,itime,startdate,olddate,ixpar,jxpar,1,CANICEXY,IVGTYP,23)
   call output_NoahMP_NWM(trim(noah_lsm%outdir),igrid,noah_lsm%output_timestep,itime,startdate,olddate,ixpar,jxpar,1,ZWTXY,IVGTYP,24)
   call output_NoahMP_NWM(trim(noah_lsm%outdir),igrid,noah_lsm%output_timestep,itime,startdate,olddate,ixpar,jxpar,1,WAXY,IVGTYP,25)
   call output_NoahMP_NWM(trim(noah_lsm%outdir),igrid,noah_lsm%output_timestep,itime,startdate,olddate,ixpar,jxpar,1,WTXY,IVGTYP,26)
   call output_NoahMP_NWM(trim(noah_lsm%outdir),igrid,noah_lsm%output_timestep,itime,startdate,olddate,ixpar,jxpar,1,ACCPRCP,IVGTYP,27)
   call output_NoahMP_NWM(trim(noah_lsm%outdir),igrid,noah_lsm%output_timestep,itime,startdate,olddate,ixpar,jxpar,1,ACCECAN,IVGTYP,28)
   call output_NoahMP_NWM(trim(noah_lsm%outdir),igrid,noah_lsm%output_timestep,itime,startdate,olddate,ixpar,jxpar,1,ACCEDIR,IVGTYP,29)
   call output_NoahMP_NWM(trim(noah_lsm%outdir),igrid,noah_lsm%output_timestep,itime,startdate,olddate,ixpar,jxpar,1,ACCETRAN,IVGTYP,30)
   call output_NoahMP_NWM(trim(noah_lsm%outdir),igrid,noah_lsm%output_timestep,itime,startdate,olddate,ixpar,jxpar,1,SAVXY,IVGTYP,31)
   call output_NoahMP_NWM(trim(noah_lsm%outdir),igrid,noah_lsm%output_timestep,itime,startdate,olddate,ixpar,jxpar,1,TRXY,IVGTYP,32)
   call output_NoahMP_NWM(trim(noah_lsm%outdir),igrid,noah_lsm%output_timestep,itime,startdate,olddate,ixpar,jxpar,1,EVCXY,IVGTYP,33)
   call output_NoahMP_NWM(trim(noah_lsm%outdir),igrid,noah_lsm%output_timestep,itime,startdate,olddate,ixpar,jxpar,1,IRCXY,IVGTYP,34)
   call output_NoahMP_NWM(trim(noah_lsm%outdir),igrid,noah_lsm%output_timestep,itime,startdate,olddate,ixpar,jxpar,1,SHCXY,IVGTYP,35)
   call output_NoahMP_NWM(trim(noah_lsm%outdir),igrid,noah_lsm%output_timestep,itime,startdate,olddate,ixpar,jxpar,1,IRGXY,IVGTYP,36)
   call output_NoahMP_NWM(trim(noah_lsm%outdir),igrid,noah_lsm%output_timestep,itime,startdate,olddate,ixpar,jxpar,1,SHGXY,IVGTYP,37)
   call output_NoahMP_NWM(trim(noah_lsm%outdir),igrid,noah_lsm%output_timestep,itime,startdate,olddate,ixpar,jxpar,1,EVGXY,IVGTYP,38)
   call output_NoahMP_NWM(trim(noah_lsm%outdir),igrid,noah_lsm%output_timestep,itime,startdate,olddate,ixpar,jxpar,1,GHVXY,IVGTYP,39)
   call output_NoahMP_NWM(trim(noah_lsm%outdir),igrid,noah_lsm%output_timestep,itime,startdate,olddate,ixpar,jxpar,1,SAGXY,IVGTYP,40)
   call output_NoahMP_NWM(trim(noah_lsm%outdir),igrid,noah_lsm%output_timestep,itime,startdate,olddate,ixpar,jxpar,1,IRBXY,IVGTYP,41)
   call output_NoahMP_NWM(trim(noah_lsm%outdir),igrid,noah_lsm%output_timestep,itime,startdate,olddate,ixpar,jxpar,1,SHBXY,IVGTYP,42)
   call output_NoahMP_NWM(trim(noah_lsm%outdir),igrid,noah_lsm%output_timestep,itime,startdate,olddate,ixpar,jxpar,1,EVBXY,IVGTYP,43)
   call output_NoahMP_NWM(trim(noah_lsm%outdir),igrid,noah_lsm%output_timestep,itime,startdate,olddate,ixpar,jxpar,1,GHBXY,IVGTYP,44)
   call output_NoahMP_NWM(trim(noah_lsm%outdir),igrid,noah_lsm%output_timestep,itime,startdate,olddate,ixpar,jxpar,1,TRADXY,IVGTYP,45)
   call output_NoahMP_NWM(trim(noah_lsm%outdir),igrid,noah_lsm%output_timestep,itime,startdate,olddate,ixpar,jxpar,1,TGXY,IVGTYP,46)
   call output_NoahMP_NWM(trim(noah_lsm%outdir),igrid,noah_lsm%output_timestep,itime,startdate,olddate,ixpar,jxpar,1,TVXY,IVGTYP,47)
   call output_NoahMP_NWM(trim(noah_lsm%outdir),igrid,noah_lsm%output_timestep,itime,startdate,olddate,ixpar,jxpar,1,TAHXY,IVGTYP,48)
   call output_NoahMP_NWM(trim(noah_lsm%outdir),igrid,noah_lsm%output_timestep,itime,startdate,olddate,ixpar,jxpar,1,TGVXY,IVGTYP,49)
   call output_NoahMP_NWM(trim(noah_lsm%outdir),igrid,noah_lsm%output_timestep,itime,startdate,olddate,ixpar,jxpar,1,TGBXY,IVGTYP,50)
   call output_NoahMP_NWM(trim(noah_lsm%outdir),igrid,noah_lsm%output_timestep,itime,startdate,olddate,ixpar,jxpar,1,T2MVXY,IVGTYP,51)
   call output_NoahMP_NWM(trim(noah_lsm%outdir),igrid,noah_lsm%output_timestep,itime,startdate,olddate,ixpar,jxpar,1,T2MBXY,IVGTYP,52)
   call output_NoahMP_NWM(trim(noah_lsm%outdir),igrid,noah_lsm%output_timestep,itime,startdate,olddate,ixpar,jxpar,1,Q2MVXY,IVGTYP,53)
   call output_NoahMP_NWM(trim(noah_lsm%outdir),igrid,noah_lsm%output_timestep,itime,startdate,olddate,ixpar,jxpar,1,Q2MBXY,IVGTYP,54)
   call output_NoahMP_NWM(trim(noah_lsm%outdir),igrid,noah_lsm%output_timestep,itime,startdate,olddate,ixpar,jxpar,1,EAHXY,IVGTYP,55)
   call output_NoahMP_NWM(trim(noah_lsm%outdir),igrid,noah_lsm%output_timestep,itime,startdate,olddate,ixpar,jxpar,1,FWETXY,IVGTYP,56)
   !call output_NoahMP_NWM(trim(noah_lsm%outdir),igrid,noah_lsm%output_timestep,itime,startdate,olddate,ixpar,jxpar,1,float(ZSNOXY(:,-nsnow+1:0,:)),IVGTYP,57)
   call output_NoahMP_NWM(trim(noah_lsm%outdir),igrid,noah_lsm%output_timestep,itime,startdate,olddate,ixpar,jxpar,3,state%SNICEXY,IVGTYP,58)
   call output_NoahMP_NWM(trim(noah_lsm%outdir),igrid,noah_lsm%output_timestep,itime,startdate,olddate,ixpar,jxpar,3,state%SNLIQXY,IVGTYP,59)
   call output_NoahMP_NWM(trim(noah_lsm%outdir),igrid,noah_lsm%output_timestep,itime,startdate,olddate,ixpar,jxpar,4,TSLB,IVGTYP,60)
   call output_NoahMP_NWM(trim(noah_lsm%outdir),igrid,noah_lsm%output_timestep,itime,startdate,olddate,ixpar,jxpar,4,SH2O,IVGTYP,61)
   call output_NoahMP_NWM(trim(noah_lsm%outdir),igrid,noah_lsm%output_timestep,itime,startdate,olddate,ixpar,jxpar,3,state%TSNOXY,IVGTYP,62)
   call output_NoahMP_NWM(trim(noah_lsm%outdir),igrid,noah_lsm%output_timestep,itime,startdate,olddate,ixpar,jxpar,4,SMOIS,IVGTYP,63)
   call output_NoahMP_NWM(trim(noah_lsm%outdir),igrid,noah_lsm%output_timestep,itime,startdate,olddate,ixpar,jxpar,1,state%SNOWH,IVGTYP,64)
   call output_NoahMP_NWM(trim(noah_lsm%outdir),igrid,noah_lsm%output_timestep,itime,startdate,olddate,ixpar,jxpar,1,state%SNOW,IVGTYP,65)
   call output_NoahMP_NWM(trim(noah_lsm%outdir),igrid,noah_lsm%output_timestep,itime,startdate,olddate,ixpar,jxpar,1,QSNOWXY,IVGTYP,66)
   call output_NoahMP_NWM(trim(noah_lsm%outdir),igrid,noah_lsm%output_timestep,itime,startdate,olddate,ixpar,jxpar,1,float(ISNOWXY),IVGTYP,67)
   call output_NoahMP_NWM(trim(noah_lsm%outdir),igrid,noah_lsm%output_timestep,itime,startdate,olddate,ixpar,jxpar,1,SNOWC,IVGTYP,68)
   call output_NoahMP_NWM(trim(noah_lsm%outdir),igrid,noah_lsm%output_timestep,itime,startdate,olddate,ixpar,jxpar,1,ACSNOW,IVGTYP,69)
   call output_NoahMP_NWM(trim(noah_lsm%outdir),igrid,noah_lsm%output_timestep,itime,startdate,olddate,ixpar,jxpar,1,ACSNOM,IVGTYP,70)
   call output_NoahMP_NWM(trim(noah_lsm%outdir),igrid,noah_lsm%output_timestep,itime,startdate,olddate,ixpar,jxpar,1,CMXY,IVGTYP,71)
   call output_NoahMP_NWM(trim(noah_lsm%outdir),igrid,noah_lsm%output_timestep,itime,startdate,olddate,ixpar,jxpar,1,CHXY,IVGTYP,72)
   call output_NoahMP_NWM(trim(noah_lsm%outdir),igrid,noah_lsm%output_timestep,itime,startdate,olddate,ixpar,jxpar,1,CHVXY,IVGTYP,73)
   call output_NoahMP_NWM(trim(noah_lsm%outdir),igrid,noah_lsm%output_timestep,itime,startdate,olddate,ixpar,jxpar,1,CHBXY,IVGTYP,74)
   call output_NoahMP_NWM(trim(noah_lsm%outdir),igrid,noah_lsm%output_timestep,itime,startdate,olddate,ixpar,jxpar,1,CHLEAFXY,IVGTYP,75)
   call output_NoahMP_NWM(trim(noah_lsm%outdir),igrid,noah_lsm%output_timestep,itime,startdate,olddate,ixpar,jxpar,1,CHUCXY,IVGTYP,76)
   call output_NoahMP_NWM(trim(noah_lsm%outdir),igrid,noah_lsm%output_timestep,itime,startdate,olddate,ixpar,jxpar,1,CHV2XY,IVGTYP,77)
   call output_NoahMP_NWM(trim(noah_lsm%outdir),igrid,noah_lsm%output_timestep,itime,startdate,olddate,ixpar,jxpar,1,CHB2XY,IVGTYP,78)
   call output_NoahMP_NWM(trim(noah_lsm%outdir),igrid,noah_lsm%output_timestep,itime,startdate,olddate,ixpar,jxpar,1,LFMASSXY,IVGTYP,79)
   call output_NoahMP_NWM(trim(noah_lsm%outdir),igrid,noah_lsm%output_timestep,itime,startdate,olddate,ixpar,jxpar,1,RTMASSXY,IVGTYP,80)
   call output_NoahMP_NWM(trim(noah_lsm%outdir),igrid,noah_lsm%output_timestep,itime,startdate,olddate,ixpar,jxpar,1,STMASSXY,IVGTYP,81)
   call output_NoahMP_NWM(trim(noah_lsm%outdir),igrid,noah_lsm%output_timestep,itime,startdate,olddate,ixpar,jxpar,1,WOODXY,IVGTYP,82)
   call output_NoahMP_NWM(trim(noah_lsm%outdir),igrid,noah_lsm%output_timestep,itime,startdate,olddate,ixpar,jxpar,1,STBLCPXY,IVGTYP,83)
   call output_NoahMP_NWM(trim(noah_lsm%outdir),igrid,noah_lsm%output_timestep,itime,startdate,olddate,ixpar,jxpar,1,FASTCPXY,IVGTYP,84)
   call output_NoahMP_NWM(trim(noah_lsm%outdir),igrid,noah_lsm%output_timestep,itime,startdate,olddate,ixpar,jxpar,1,NEEXY,IVGTYP,85)
   call output_NoahMP_NWM(trim(noah_lsm%outdir),igrid,noah_lsm%output_timestep,itime,startdate,olddate,ixpar,jxpar,1,GPPXY,IVGTYP,86)
   call output_NoahMP_NWM(trim(noah_lsm%outdir),igrid,noah_lsm%output_timestep,itime,startdate,olddate,ixpar,jxpar,1,NPPXY,IVGTYP,87)
   call output_NoahMP_NWM(trim(noah_lsm%outdir),igrid,noah_lsm%output_timestep,itime,startdate,olddate,ixpar,jxpar,1,PSNXY,IVGTYP,88)
   call output_NoahMP_NWM(trim(noah_lsm%outdir),igrid,noah_lsm%output_timestep,itime,startdate,olddate,ixpar,jxpar,1,APARXY,IVGTYP,89)
   call output_NoahMP_NWM(trim(noah_lsm%outdir),igrid,noah_lsm%output_timestep,itime,startdate,olddate,ixpar,jxpar,1,(ACCECAN+ACCEDIR+ACCETRAN),IVGTYP,90)
   call output_NoahMP_NWM(trim(noah_lsm%outdir),igrid,noah_lsm%output_timestep,itime,startdate,olddate,ixpar,jxpar,1,CANWAT,IVGTYP,91)
   call output_NoahMP_NWM(trim(noah_lsm%outdir),igrid,noah_lsm%output_timestep,itime,startdate,olddate,ixpar,jxpar,1,(SOILICE),IVGTYP,92)
   call output_NoahMP_NWM(trim(noah_lsm%outdir),igrid,noah_lsm%output_timestep,itime,startdate,olddate,ixpar,jxpar,1,(SOILSAT_TOP),IVGTYP,93)
   call output_NoahMP_NWM(trim(noah_lsm%outdir),igrid,noah_lsm%output_timestep,itime,startdate,olddate,ixpar,jxpar,1,(SOILSAT),IVGTYP,94)
   call output_NoahMP_NWM(trim(noah_lsm%outdir),igrid,noah_lsm%output_timestep,itime,startdate,olddate,ixpar,jxpar,1,(SNOWT_AVG),IVGTYP,95)
   call output_NoahMP_NWM(trim(noah_lsm%outdir),igrid,noah_lsm%output_timestep,itime,startdate,olddate,ixpar,jxpar,2,ALBSNDXY,IVGTYP,96)
   call output_NoahMP_NWM(trim(noah_lsm%outdir),igrid,noah_lsm%output_timestep,itime,startdate,olddate,ixpar,jxpar,2,ALBSNIXY,IVGTYP,97)
   call output_NoahMP_NWM(trim(noah_lsm%outdir),igrid,noah_lsm%output_timestep,itime,startdate,olddate,ixpar,jxpar,1,QRAINXY,IVGTYP,98)
   if (crocus_opt /= 0) then
      call output_NoahMP_NWM(trim(noah_lsm%outdir),igrid,noah_lsm%output_timestep,itime, &
           state%startdate,state%olddate,ixpar,jxpar,1,float(GLACINFO),state%IVGTYP,99)
      call output_NoahMP_NWM(trim(noah_lsm%outdir),igrid,noah_lsm%output_timestep,itime, &
           state%startdate,state%olddate,ixpar,jxpar,1,(GLACT),state%IVGTYP,100)
      call output_NoahMP_NWM(trim(noah_lsm%outdir),igrid,noah_lsm%output_timestep,itime, &
           state%startdate,state%olddate,ixpar,jxpar,1,PSNOWALBXY,state%IVGTYP,101)
      call output_NoahMP_NWM(trim(noah_lsm%outdir),igrid,noah_lsm%output_timestep,itime, &
           state%startdate,state%olddate,ixpar,jxpar,1,PSNOWTHRUFALXY,state%IVGTYP,102)
      call output_NoahMP_NWM(trim(noah_lsm%outdir),igrid,noah_lsm%output_timestep,itime, &
           state%startdate,state%olddate,ixpar,jxpar,1,PSNOWHEIGHTXY,state%IVGTYP,103)
      call output_NoahMP_NWM(trim(noah_lsm%outdir),igrid,noah_lsm%output_timestep,itime, &
           state%startdate,state%olddate,ixpar,jxpar,1,PSNOWTOTSWEXY,state%IVGTYP,104)
      call output_NoahMP_NWM(trim(noah_lsm%outdir),igrid,noah_lsm%output_timestep,itime, &
           state%startdate,state%olddate,ixpar,jxpar,noah_lsm%act_lev,PSNOWGRAN1XY,state%IVGTYP,105)
      call output_NoahMP_NWM(trim(noah_lsm%outdir),igrid,noah_lsm%output_timestep,itime, &
           state%startdate,state%olddate,ixpar,jxpar,noah_lsm%act_lev,PSNOWGRAN2XY,state%IVGTYP,106)
      call output_NoahMP_NWM(trim(noah_lsm%outdir),igrid,noah_lsm%output_timestep,itime, &
           state%startdate,state%olddate,ixpar,jxpar,noah_lsm%act_lev,PSNOWAGEXY,state%IVGTYP,107)
      call output_NoahMP_NWM(trim(noah_lsm%outdir),igrid,noah_lsm%output_timestep,itime, &
           state%startdate,state%olddate,ixpar,jxpar,noah_lsm%act_lev,PSNOWTEMPXY,state%IVGTYP,108)
      call output_NoahMP_NWM(trim(noah_lsm%outdir),igrid,noah_lsm%output_timestep,itime, &
           state%startdate,state%olddate,ixpar,jxpar,noah_lsm%act_lev,PSNOWDZXY,state%IVGTYP,109)
      call output_NoahMP_NWM(trim(noah_lsm%outdir),igrid,noah_lsm%output_timestep,itime, &
           state%startdate,state%olddate,ixpar,jxpar,noah_lsm%act_lev,PSNOWHISTXY,state%IVGTYP,110)
      call output_NoahMP_NWM(trim(noah_lsm%outdir),igrid,noah_lsm%output_timestep,itime, &
           state%startdate,state%olddate,ixpar,jxpar,noah_lsm%act_lev,(PSNOWLIQXY),state%IVGTYP,111)
      call output_NoahMP_NWM(trim(noah_lsm%outdir),igrid,noah_lsm%output_timestep,itime, &
           state%startdate,state%olddate,ixpar,jxpar,noah_lsm%act_lev,PSNOWHEATXY,state%IVGTYP,112)
      call output_NoahMP_NWM(trim(noah_lsm%outdir),igrid,noah_lsm%output_timestep,itime, &
           state%startdate,state%olddate,ixpar,jxpar,noah_lsm%act_lev,PSNOWRHOXY ,state%IVGTYP,113)
      call output_NoahMP_NWM(trim(noah_lsm%outdir),igrid,noah_lsm%output_timestep,itime, &
           state%startdate,state%olddate,ixpar,jxpar,noah_lsm%act_lev,PSNOWSWEXY,state%IVGTYP,114)
      call output_NoahMP_NWM(trim(noah_lsm%outdir),igrid,noah_lsm%output_timestep,itime, &
           state%startdate,state%olddate,ixpar,jxpar,1,FLOW_ICE,state%IVGTYP,115)
      call output_NoahMP_NWM(trim(noah_lsm%outdir),igrid,noah_lsm%output_timestep,itime, &
           state%startdate,state%olddate,ixpar,jxpar,1,FLOW_SNOW,state%IVGTYP,116)
   end if ! crocus_opt /= 0
#endif /* WRF_HYDRO */

!#ifdef WRF_HYDRO
!endif
!#endif

end subroutine  ldas_output

!!===============================================================================
subroutine lsm_restart(state)
  use NoahmpIOVarType, only: NoahmpIO_type
  implicit none
  type(NoahmpIO_type) :: state
  character(len=256):: tmpStr
  integer :: ncid

#ifdef HYDRO_D
  print*, 'Write restart at '//state%olddate(1:13)
#endif

  call prepare_restart_file (trim(state%outdir), version, state%igrid, llanduse,  &
       state%olddate, state%startdate, state%ixfull, state%jxfull,    &
       state%ixpar, state%jxpar, state%xstartpar, state%ystartpar, &
       state%nsoil, state%nsnow, state%dx, state%dy,        &
       state%truelat1, state%truelat2, state%mapproj, state%lat1,      &
       state%lon1, state%cen_lon, state%iswater, state%ivgtyp,    &
       glacinfo, glact, act_lev,   VIS_ICEALB)

  write(tmpStr, '(A,"/RESTART.",A10,"_DOMAIN",I1)') trim(state%outdir), &
       state%olddate(1:4)//state%olddate(6:7)//state%olddate(9:10)//state%olddate(12:13), &
       state%igrid


#ifdef MPP_LAND
  if(my_id .eq. io_id) then
#endif
     ierr = nf90_open(trim(tmpStr),  NF90_WRITE, ncid)
     call define_rst_variables(ncid,state)
#ifdef MPP_LAND
  end if
#endif

  call add_to_restart(ncid, state%TSLB      , "SOIL_T", layers="SOIL")
  call add_to_restart(ncid, state%TSNOXY    , "SNOW_T", layers="SNOW")
  call add_to_restart(ncid, state%SMOIS     , "SMC"   , layers="SOIL")
  call add_to_restart(ncid, state%SH2O      , "SH2O"  , layers="SOIL")
  call add_to_restart(ncid, state%ZSNSOXY   , "ZSNSO" , layers="SOSN")
  call add_to_restart(ncid, state%SNICEXY   , "SNICE" , layers="SNOW")
  call add_to_restart(ncid, state%SNLIQXY   , "SNLIQ" , layers="SNOW")
  call add_to_restart(ncid, state%QSNOWXY   , "QSNOW" )
  call add_to_restart(ncid, state%FWETXY    , "FWET"  )
  call add_to_restart(ncid, state%SNEQVOXY  , "SNEQVO")
  call add_to_restart(ncid, state%EAHXY     , "EAH"   )
  call add_to_restart(ncid, state%TAHXY     , "TAH"   )
  call add_to_restart(ncid, state%ALBOLDXY  , "ALBOLD")
  call add_to_restart(ncid, state%CMXY      , "CM"    )
  call add_to_restart(ncid, state%CHXY      , "CH"    )
  call add_to_restart(ncid, state%ISNOWXY   , "ISNOW" )
  call add_to_restart(ncid, state%CANLIQXY  , "CANLIQ")
  call add_to_restart(ncid, state%CANICEXY  , "CANICE")
  call add_to_restart(ncid, state%SNOW      , "SNEQV" )
  call add_to_restart(ncid, state%SNOWH     , "SNOWH" )
  call add_to_restart(ncid, state%TVXY      , "TV"    )
  call add_to_restart(ncid, state%TGXY      , "TG"    )
  call add_to_restart(ncid, state%ZWTXY     , "ZWT"   )
  call add_to_restart(ncid, state%WAXY      , "WA"    )
  call add_to_restart(ncid, state%WTXY      , "WT"    )
  call add_to_restart(ncid, state%WSLAKEXY  , "WSLAKE")
  call add_to_restart(ncid, state%LFMASSXY  , "LFMASS")
  call add_to_restart(ncid, state%RTMASSXY  , "RTMASS")
  call add_to_restart(ncid, state%STMASSXY  , "STMASS")
  call add_to_restart(ncid, state%WOODXY    , "WOOD"  )
  call add_to_restart(ncid, state%STBLCPXY  , "STBLCP")
  call add_to_restart(ncid, state%FASTCPXY  , "FASTCP")
  call add_to_restart(ncid, state%LAI       , "LAI"   )
  call add_to_restart(ncid, state%XSAIXY    , "SAI"   )
  call add_to_restart(ncid, state%VEGFRA    , "VEGFRA")
  call add_to_restart(ncid, state%GVFMIN    , "GVFMIN")
  call add_to_restart(ncid, state%GVFMAX    , "GVFMAX")
  call add_to_restart(ncid, state%ACSNOM    , "ACMELT")
  call add_to_restart(ncid, state%ACSNOW    , "ACSNOW")
  call add_to_restart(ncid, state%TAUSSXY   , "TAUSS" )
  call add_to_restart(ncid, state%QSFC      , "QSFC"  )
  call add_to_restart(ncid, state%SFCRUNOFF , "SFCRUNOFF")
  call add_to_restart(ncid, state%UDRUNOFF  , "UDRUNOFF" )
  if (crocus_opt /= 0) then
     call add_to_restart(ncid, PSNOWAGEXY    , "PSNOWAGE"  , LAYERS="MAXS")
     call add_to_restart(ncid, PSNOWDZXY     , "PSNOWDZ"   , LAYERS="MAXS")
     call add_to_restart(ncid, PSNOWGRAN1XY  , "PSNOWGRAN1", LAYERS="MAXS")
     call add_to_restart(ncid, PSNOWGRAN2XY  , "PSNOWGRAN2", LAYERS="MAXS")
     call add_to_restart(ncid, PSNOWHEATXY   , "PSNOWHEAT" , LAYERS="MAXS")
     call add_to_restart(ncid, PSNOWHISTXY   , "PSNOWHIST" , LAYERS="MAXS")
     call add_to_restart(ncid, PSNOWLIQXY    , "PSNOWLIQ"  , LAYERS="MAXS")
     call add_to_restart(ncid, PSNOWRHOXY    , "PSNOWRHO"  , LAYERS="MAXS")
     call add_to_restart(ncid, PSNOWSWEXY    , "PSNOWSWE"  , LAYERS="MAXS")
     call add_to_restart(ncid, PSNOWTEMPXY   , "PSNOWTEMP" , LAYERS="MAXS")
     call add_to_restart(ncid, PSNOWALBXY    , "PSNOWALB"    )
     call add_to_restart(ncid, PSNOWHEIGHTXY , "PSNOWHEIGHT" )
     call add_to_restart(ncid, PSNOWTHRUFALXY, "PSNOWTHRUFAL")
     call add_to_restart(ncid, PSNOWTOTSWEXY , "PSNOWTOTSWE" )
     call add_to_restart(ncid, FLOW_ICE      , "FLOW_ICE"    )
     call add_to_restart(ncid, FLOW_SNOW     , "FLOW_SNOW"   )
  end if ! crocus_opt /= 0
#ifdef WRF_HYDRO
  call add_to_restart(ncid,ACCPRCP   , "ACCPRCP" )
  call add_to_restart(ncid,ACCECAN   , "ACCECAN" )
  call add_to_restart(ncid,ACCEDIR   , "ACCEDIR" )
  call add_to_restart(ncid,ACCETRAN  , "ACCETRAN" )
#endif
! below for opt_run = 5
  call add_to_restart(ncid, state%SMOISEQ   , "SMOISEQ"  , layers="SOIL"  )
  call add_to_restart(ncid, state%AREAXY    , "AREAXY"     )
  call add_to_restart(ncid, state%SMCWTDXY  , "SMCWTDXY"   )
  call add_to_restart(ncid, state%DEEPRECHXY, "DEEPRECHXY" )
  call add_to_restart(ncid, state%QSLATXY   , "QSLATXY"    )
  call add_to_restart(ncid, state%QRFSXY    , "QRFSXY"     )
  call add_to_restart(ncid, state%QSPRINGSXY, "QSPRINGSXY" )
  call add_to_restart(ncid, state%RECHXY    , "RECHXY"     )
  call add_to_restart(ncid, state%QRFXY     , "QRFXY"      )
  call add_to_restart(ncid, state%QSPRINGXY , "QSPRINGXY"  )
  call add_to_restart(ncid, state%FDEPTHXY  , "FDEPTHXY"   )
  call add_to_restart(ncid, state%RIVERCONDXY, "RIVERCONDXY")
  call add_to_restart(ncid, state%RIVERBEDXY , "RIVERBEDXY" )
  call add_to_restart(ncid, state%EQZWT     , "EQZWT"      )
  call add_to_restart(ncid, state%PEXPXY    , "PEXPXY"     )

#ifdef MPP_LAND
  if(my_id .eq. io_id) then
     ierr = nf90_close(ncid)
  end if
#endif

  call finalize_restart_file()

end subroutine lsm_restart

subroutine lsm_rst_bi_out(state)
  use NoahmpIOVarType, only: NoahmpIO_type
  implicit none
  type(NoahmpIO_type) :: state
  integer :: iunit, ierr
  character(len=256) :: output_flnm, str_tmp
  integer  :: i0,ie, i, istep, mkdirStatus

#ifdef MPP_LAND
  call mpp_land_sync()

 i0 = 0
 istep = 64
 ie = istep
 do i = 0, numprocs,istep
   if(my_id .ge. i0 .and. my_id .lt. ie) then
#endif

      write(output_flnm, '(A,"/RESTART.",A10,"_DOMAIN",I1)') trim(state%outdir), &
           state%olddate(1:4)//state%olddate(6:7)//state%olddate(9:10)//state%olddate(12:13),state%igrid
  iunit = 56

#ifdef MPP_LAND
             if(my_id .lt. 10) then
                  write(str_tmp,'(I1)') my_id
             else if(my_id .lt. 100) then
                  write(str_tmp,'(I2)') my_id
             else if(my_id .lt. 1000) then
                  write(str_tmp,'(I3)') my_id
             else if(my_id .lt. 10000) then
                  write(str_tmp,'(I4)') my_id
             else if(my_id .lt. 100000) then
                  write(str_tmp,'(I5)') my_id
             else
                continue
             end if
  open(iunit,file="restart/"//trim(output_flnm)//"."//trim(str_tmp),form="unformatted",ERR=102, access="sequential")
#else
  open(iunit,file="restart/"//trim(output_flnm),form="unformatted",ERR=102, access="sequential")
#endif

  write(iunit,ERR=101) state%TSLB
  write(iunit,ERR=101) state%TSNOXY
  write(iunit,ERR=101) state%SMOIS
  write(iunit,ERR=101) state%SH2O
  write(iunit,ERR=101) state%ZSNSOXY
  write(iunit,ERR=101) state%SNICEXY
  write(iunit,ERR=101) state%SNLIQXY
  write(iunit,ERR=101) state%QSNOWXY
  write(iunit,ERR=101) state%FWETXY
  write(iunit,ERR=101) state%SNEQVOXY
  write(iunit,ERR=101) state%EAHXY
  write(iunit,ERR=101) state%TAHXY
  write(iunit,ERR=101) state%ALBOLDXY
  write(iunit,ERR=101) state%CMXY
  write(iunit,ERR=101) state%CHXY
  write(iunit,ERR=101) state%ISNOWXY
  write(iunit,ERR=101) state%CANLIQXY
  write(iunit,ERR=101) state%CANICEXY
  write(iunit,ERR=101) state%SNOW
  write(iunit,ERR=101) state%SNOWH
  write(iunit,ERR=101) state%TVXY
  write(iunit,ERR=101) state%TGXY
  write(iunit,ERR=101) state%ZWTXY
  write(iunit,ERR=101) state%WAXY
  write(iunit,ERR=101) state%WTXY
  write(iunit,ERR=101) state%WSLAKEXY
  write(iunit,ERR=101) state%LFMASSXY
  write(iunit,ERR=101) state%RTMASSXY
  write(iunit,ERR=101) state%STMASSXY
  write(iunit,ERR=101) state%WOODXY
  write(iunit,ERR=101) state%STBLCPXY
  write(iunit,ERR=101) state%FASTCPXY
  write(iunit,ERR=101) state%LAI
  write(iunit,ERR=101) state%XSAIXY
  write(iunit,ERR=101) state%VEGFRA
  write(iunit,ERR=101) state%GVFMIN
  write(iunit,ERR=101) state%GVFMAX
  write(iunit,ERR=101) state%ACSNOM
  write(iunit,ERR=101) state%ACSNOW
  write(iunit,ERR=101) state%TAUSSXY
  write(iunit,ERR=101) state%QSFC
  write(iunit,ERR=101) state%SFCRUNOFF
  write(iunit,ERR=101) state%UDRUNOFF
  if (crocus_opt /= 0) then
     write(iunit,ERR=101) PSNOWAGEXY
     write(iunit,ERR=101) PSNOWDZXY
     write(iunit,ERR=101) PSNOWGRAN1XY
     write(iunit,ERR=101) PSNOWGRAN2XY
     write(iunit,ERR=101) PSNOWHEATXY
     write(iunit,ERR=101) PSNOWHISTXY
     write(iunit,ERR=101) PSNOWLIQXY
     write(iunit,ERR=101) PSNOWRHOXY
     write(iunit,ERR=101) PSNOWSWEXY
     write(iunit,ERR=101) PSNOWTEMPXY
     write(iunit,ERR=101) PSNOWALBXY
     write(iunit,ERR=101) PSNOWHEIGHTXY
     write(iunit,ERR=101) PSNOWTHRUFALXY
     write(iunit,ERR=101) PSNOWTOTSWEXY
     write(iunit,ERR=101) FLOW_SNOW
     write(iunit,ERR=101) FLOW_ICE
  end if ! crocus_opt /= 0
! #ifndef REALTIME
! #ifdef WRF_HYDRO
!   write(iunit,ERR=101) ACCPRCP
!   write(iunit,ERR=101) ACCECAN
!   write(iunit,ERR=101) ACCEDIR
!   write(iunit,ERR=101) ACCETRAN
! #endif
! #endif
! ! below for opt_run = 5
!   if(IOPT_RUN .eq. 5) then
!      write(iunit,ERR=101) SMOISEQ
!      write(iunit,ERR=101) AREAXY
!      write(iunit,ERR=101) SMCWTDXY
!      write(iunit,ERR=101) DEEPRECHXY
!      write(iunit,ERR=101) QSLATXY
!      write(iunit,ERR=101) QRFSXY
!      write(iunit,ERR=101) QSPRINGSXY
!      write(iunit,ERR=101) RECHXY
!      write(iunit,ERR=101) QRFXY
!      write(iunit,ERR=101) QSPRINGXY
!      write(iunit,ERR=101) FDEPTHXY
!      write(iunit,ERR=101) RIVERCONDXY
!      write(iunit,ERR=101) RIVERBEDXY
!      write(iunit,ERR=101) EQZWT
!      write(iunit,ERR=101) PEXPXY
!   end if

  close(iunit)

#ifdef MPP_LAND
    end if
    call mpp_land_sync()
    i0 = i0 + istep
    ie = ie + istep
  end do ! end do of i loop
#endif

  return
101  continue
#ifdef MPP_LAND
  call fatal_error_stop("FATAL ERROR: failed to write lsm restartfile")
#else
  stop "failed to write lsm restart file"
#endif
102  continue
#ifdef MPP_LAND
  call fatal_error_stop("FATAL ERROR: failed to open lsm restartfile")
#else
  stop "FATAL ERROR: failed to open restart file"
#endif
end subroutine lsm_rst_bi_out

subroutine lsm_rst_bi_in(state)
  use NoahmpIOVarType, only: NoahmpIO_type
  implicit none
  type(NoahmpIO_type) :: state
  integer :: iunit, ierr
  character(len=256):: str_tmp
  integer  :: i0,ie, i, istep

  iunit = 56
#ifdef MPP_LAND
 i0 = 0
 istep = 64
 ie = istep
 do i = 0, numprocs,istep
   if(my_id .ge. i0 .and. my_id .lt. ie) then

             if(my_id .lt. 10) then
                  write(str_tmp,'(I1)') my_id
             else if(my_id .lt. 100) then
                  write(str_tmp,'(I2)') my_id
             else if(my_id .lt. 1000) then
                  write(str_tmp,'(I3)') my_id
             else if(my_id .lt. 10000) then
                  write(str_tmp,'(I4)') my_id
             else if(my_id .lt. 100000) then
                  write(str_tmp,'(I5)') my_id
             else
                continue
             end if
  open(iunit,file=trim(noah_lsm%restart_filename_requested)//"."//trim(str_tmp),form="unformatted",ERR=101, access="sequential")
#else
  open(iunit,file=trim(noah_lsm%restart_filename_requested),form="unformatted",ERR=101, access="sequential")
#endif


  read(iunit,ERR=101) state%TSLB
  read(iunit,ERR=101) state%TSNOXY
  read(iunit,ERR=101) state%SMOIS
  read(iunit,ERR=101) state%SH2O
  read(iunit,ERR=101) state%ZSNSOXY
  read(iunit,ERR=101) state%SNICEXY
  read(iunit,ERR=101) state%SNLIQXY
  read(iunit,ERR=101) state%QSNOWXY
  read(iunit,ERR=101) state%FWETXY
  read(iunit,ERR=101) state%SNEQVOXY
  read(iunit,ERR=101) state%EAHXY
  read(iunit,ERR=101) state%TAHXY
  read(iunit,ERR=101) state%ALBOLDXY
  read(iunit,ERR=101) state%CMXY
  read(iunit,ERR=101) state%CHXY
  read(iunit,ERR=101) state%ISNOWXY
  read(iunit,ERR=101) state%CANLIQXY
  read(iunit,ERR=101) state%CANICEXY
  read(iunit,ERR=101) state%SNOW
  read(iunit,ERR=101) state%SNOWH
  read(iunit,ERR=101) state%TVXY
  read(iunit,ERR=101) state%TGXY
  read(iunit,ERR=101) state%ZWTXY
  read(iunit,ERR=101) state%WAXY
  read(iunit,ERR=101) state%WTXY
  read(iunit,ERR=101) state%WSLAKEXY
  read(iunit,ERR=101) state%LFMASSXY
  read(iunit,ERR=101) state%RTMASSXY
  read(iunit,ERR=101) state%STMASSXY
  read(iunit,ERR=101) state%WOODXY
  read(iunit,ERR=101) state%STBLCPXY
  read(iunit,ERR=101) state%FASTCPXY
  read(iunit,ERR=101) state%LAI
  read(iunit,ERR=101) state%XSAIXY
  read(iunit,ERR=101) state%VEGFRA
  read(iunit,ERR=101) state%GVFMIN
  read(iunit,ERR=101) state%GVFMAX
  read(iunit,ERR=101) state%ACSNOM
  read(iunit,ERR=101) state%ACSNOW
  read(iunit,ERR=101) state%TAUSSXY
  read(iunit,ERR=101) state%QSFC
  read(iunit,ERR=101) state%SFCRUNOFF
  read(iunit,ERR=101) state%UDRUNOFF
  if (crocus_opt /= 0) then
     read(iunit,ERR=101) PSNOWAGEXY
     read(iunit,ERR=101) PSNOWDZXY
     read(iunit,ERR=101) PSNOWGRAN1XY
     read(iunit,ERR=101) PSNOWGRAN2XY
     read(iunit,ERR=101) PSNOWHEATXY
     read(iunit,ERR=101) PSNOWHISTXY
     read(iunit,ERR=101) PSNOWLIQXY
     read(iunit,ERR=101) PSNOWRHOXY
     read(iunit,ERR=101) PSNOWSWEXY
     read(iunit,ERR=101) PSNOWTEMPXY
     read(iunit,ERR=101) PSNOWALBXY
     read(iunit,ERR=101) PSNOWHEIGHTXY
     read(iunit,ERR=101) PSNOWTHRUFALXY
     read(iunit,ERR=101) PSNOWTOTSWEXY
     read(iunit,ERR=101) FLOW_SNOW
     read(iunit,ERR=101) FLOW_ICE
  end if ! crocus_opt /= 0

  ! read(iunit,ERR=101) PSNOWLIQXY
! #ifndef REALTIME
! #ifdef WRF_HYDRO
!   read(iunit,ERR=101) ACCPRCP
!   read(iunit,ERR=101) ACCECAN
!   read(iunit,ERR=101) ACCEDIR
!   read(iunit,ERR=101) ACCETRAN
! #endif
! #endif
! ! below for opt_run = 5
!   if(IOPT_RUN .eq. 5) then
!       read(iunit,ERR=101) SMOISEQ
!       read(iunit,ERR=101) AREAXY
!       read(iunit,ERR=101) SMCWTDXY
!       read(iunit,ERR=101) DEEPRECHXY
!       read(iunit,ERR=101) QSLATXY
!       read(iunit,ERR=101) QRFSXY
!       read(iunit,ERR=101) QSPRINGSXY
!       read(iunit,ERR=101) RECHXY
!       read(iunit,ERR=101) QRFXY
!       read(iunit,ERR=101) QSPRINGXY
!       read(iunit,ERR=101) FDEPTHXY
!       read(iunit,ERR=101) RIVERCONDXY
!       read(iunit,ERR=101) RIVERBEDXY
!       read(iunit,ERR=101) EQZWT
!       read(iunit,ERR=101) PEXPXY
!   end if
  close(iunit)

#ifdef MPP_LAND
    end if
    call mpp_land_sync()
    i0 = i0 + istep
    ie = ie + istep
  end do ! end do of i loop
#endif

  return

101  continue
#ifdef MPP_LAND
  call fatal_error_stop("FATAL ERROR: failed to read in lsm restartfile "   &
          //trim(state%restart_filename_requested)//"."//trim(str_tmp))
#else
  stop "FATAL ERROR: failed to read lsm restart file"
#endif
end subroutine lsm_rst_bi_in


subroutine define_rst_variables(ncid, state)
  use NoahmpIOVarType, only: NoahmpIO_type
  implicit none
  type(NoahmpIO_type) :: state
  integer ncid
  !      character(len=*) :: tmpStr

  call error_handler(ierr, "In module_hrldas_netcdf_io.F add_to_restart_2d_float() - "// &
       "Problem nf90_open")
  ierr = nf90_redef(ncid)
  call error_handler(ierr, "In module_hrldas_netcdf_io.F add_to_restart_2d_float() - "// &
       "Problem nf90_redef")
! add the variables

  call define_rst_var(ncid, state%TSLB      , "SOIL_T", layers="SOIL")
  call define_rst_var(ncid, state%TSNOXY    , "SNOW_T", layers="SNOW")
  call define_rst_var(ncid, state%SMOIS     , "SMC"   , layers="SOIL")
  call define_rst_var(ncid, state%SH2O      , "SH2O"  , layers="SOIL")
  call define_rst_var(ncid, state%ZSNSOXY   , "ZSNSO" , layers="SOSN")
  call define_rst_var(ncid, state%SNICEXY   , "SNICE" , layers="SNOW")
  call define_rst_var(ncid, state%SNLIQXY   , "SNLIQ" , layers="SNOW")
  call define_rst_var(ncid, state%QSNOWXY   , "QSNOW" )
  call define_rst_var(ncid, state%FWETXY    , "FWET"  )
  call define_rst_var(ncid, state%SNEQVOXY  , "SNEQVO")
  call define_rst_var(ncid, state%EAHXY     , "EAH"   )
  call define_rst_var(ncid, state%TAHXY     , "TAH"   )
  call define_rst_var(ncid, state%ALBOLDXY  , "ALBOLD")
  call define_rst_var(ncid, state%CMXY      , "CM"    )
  call define_rst_var(ncid, state%CHXY      , "CH"    )
  call define_rst_var(ncid, state%ISNOWXY   , "ISNOW" )
  call define_rst_var(ncid, state%CANLIQXY  , "CANLIQ")
  call define_rst_var(ncid, state%CANICEXY  , "CANICE")
  call define_rst_var(ncid, state%SNOW      , "SNEQV" )
  call define_rst_var(ncid, state%SNOWH     , "SNOWH" )
  call define_rst_var(ncid, state%TVXY      , "TV"    )
  call define_rst_var(ncid, state%TGXY      , "TG"    )
  call define_rst_var(ncid, state%ZWTXY     , "ZWT"   )
  call define_rst_var(ncid, state%WAXY      , "WA"    )
  call define_rst_var(ncid, state%WTXY      , "WT"    )
  call define_rst_var(ncid, state%WSLAKEXY  , "WSLAKE")
  call define_rst_var(ncid, state%LFMASSXY  , "LFMASS")
  call define_rst_var(ncid, state%RTMASSXY  , "RTMASS")
  call define_rst_var(ncid, state%STMASSXY  , "STMASS")
  call define_rst_var(ncid, state%WOODXY    , "WOOD"  )
  call define_rst_var(ncid, state%STBLCPXY  , "STBLCP")
  call define_rst_var(ncid, state%FASTCPXY  , "FASTCP")
  call define_rst_var(ncid, state%LAI       , "LAI"   )
  call define_rst_var(ncid, state%XSAIXY    , "SAI"   )
  call define_rst_var(ncid, state%VEGFRA    , "VEGFRA")
  call define_rst_var(ncid, state%GVFMIN    , "GVFMIN")
  call define_rst_var(ncid, state%GVFMAX    , "GVFMAX")
  call define_rst_var(ncid, state%ACSNOM    , "ACMELT")
  call define_rst_var(ncid, state%ACSNOW    , "ACSNOW")
  call define_rst_var(ncid, state%TAUSSXY   , "TAUSS" )
  call define_rst_var(ncid, state%QSFC      , "QSFC"  )
  call define_rst_var(ncid, state%SFCRUNOFF , "SFCRUNOFF")
  call define_rst_var(ncid, state%UDRUNOFF  , "UDRUNOFF" )
  if (crocus_opt /= 0) then
     call define_rst_var(ncid,PSNOWAGEXY    , "PSNOWAGE"  , layers="MAXS")
     call define_rst_var(ncid,PSNOWDZXY     , "PSNOWDZ"   , layers="MAXS")
     call define_rst_var(ncid,PSNOWGRAN1XY  , "PSNOWGRAN1", layers="MAXS")
     call define_rst_var(ncid,PSNOWGRAN2XY  , "PSNOWGRAN2", layers="MAXS")
     call define_rst_var(ncid,PSNOWHEATXY   , "PSNOWHEAT" , layers="MAXS")
     call define_rst_var(ncid,PSNOWHISTXY   , "PSNOWHIST" , layers="MAXS")
     call define_rst_var(ncid,PSNOWLIQXY    , "PSNOWLIQ"  , layers="MAXS")
     call define_rst_var(ncid,PSNOWRHOXY    , "PSNOWRHO"  , layers="MAXS")
     call define_rst_var(ncid,PSNOWSWEXY    , "PSNOWSWE"  , layers="MAXS")
     call define_rst_var(ncid,PSNOWTEMPXY   , "PSNOWTEMP" , layers="MAXS")
     call define_rst_var(ncid,PSNOWALBXY    , "PSNOWALB"    )
     call define_rst_var(ncid,PSNOWHEIGHTXY , "PSNOWHEIGHT" )
     call define_rst_var(ncid,PSNOWTHRUFALXY, "PSNOWTHRUFAL")
     call define_rst_var(ncid,PSNOWTOTSWEXY , "PSNOWTOTSWE" )
     call define_rst_var(ncid,FLOW_SNOW     , "FLOW_SNOW"   )
     call define_rst_var(ncid,FLOW_ICE      , "FLOW_ICE"    )
  end if ! crocus_opt /= 0
#ifdef WRF_HYDRO
  call define_rst_var(ncid,ACCPRCP   , "ACCPRCP" )
  call define_rst_var(ncid,ACCECAN   , "ACCECAN" )
  call define_rst_var(ncid,ACCEDIR   , "ACCEDIR" )
  call define_rst_var(ncid,ACCETRAN  , "ACCETRAN" )
#endif
! below for opt_run = 5
  call define_rst_var(ncid, state%SMOISEQ   , "SMOISEQ"  , layers="SOIL"  )
  call define_rst_var(ncid, state%AREAXY    , "AREAXY"     )
  call define_rst_var(ncid, state%SMCWTDXY  , "SMCWTDXY"   )
  call define_rst_var(ncid, state%DEEPRECHXY, "DEEPRECHXY" )
  call define_rst_var(ncid, state%QSLATXY   , "QSLATXY"    )
  call define_rst_var(ncid, state%QRFSXY    , "QRFSXY"     )
  call define_rst_var(ncid, state%QSPRINGSXY, "QSPRINGSXY" )
  call define_rst_var(ncid, state%RECHXY    , "RECHXY"     )
  call define_rst_var(ncid, state%QRFXY     , "QRFXY"      )
  call define_rst_var(ncid, state%QSPRINGXY , "QSPRINGXY"  )
  call define_rst_var(ncid, state%FDEPTHXY , "FDEPTHXY"  )
  call define_rst_var(ncid, state%RIVERCONDXY , "RIVERCONDXY"  )
  call define_rst_var(ncid, state%RIVERBEDXY , "RIVERBEDXY"  )
  call define_rst_var(ncid, state%EQZWT , "EQZWT"  )
  call define_rst_var(ncid, state%PEXPXY , "PEXPXY"  )
      ierr = nf90_enddef(ncid)
      call error_handler(ierr, "In module_hrldas_netcdf_io.F add_to_restart_2d_float() - "// &
                             "Problem nf90_enddef")
      call error_handler(ierr, "In module_hrldas_netcdf_io.F add_to_restart_3d() - "// &
                             "Problem nf90_close")

   end subroutine define_rst_variables

end module module_NoahMP_hrldas_driver

!subroutine wrf_message(msg)
!  implicit none
!  character(len=*), intent(in) :: msg
!  print*, msg
!end subroutine wrf_message

logical function wrf_dm_on_monitor() result(l)
  l = .TRUE.
  return
end function wrf_dm_on_monitor


!------------------------------------------------------------------------------------
!------------------------------------------------------------------------------------

subroutine CALC_DECLIN ( NOWDATE, LATITUDE, LONGITUDE, COSZ, JULIAN )

  use MODULE_DATE_UTILITIES
!---------------------------------------------------------------------
   implicit none
!---------------------------------------------------------------------

   REAL, PARAMETER :: DEGRAD = 3.14159265/180.
   REAL, PARAMETER :: DPD    = 360./365.
! !ARGUMENTS:
   CHARACTER(LEN=19), INTENT(IN)  :: NOWDATE    ! YYYY-MM-DD_HH:MM:SS
   REAL,              INTENT(IN)  :: LATITUDE
   REAL,              INTENT(IN)  :: LONGITUDE
   REAL,              INTENT(OUT) :: COSZ
   REAL,              INTENT(OUT) :: JULIAN
   REAL                           :: HRANG
   REAL                           :: DECLIN
   REAL                           :: OBECL
   REAL                           :: SINOB
   REAL                           :: SXLONG
   REAL                           :: ARG
   REAL                           :: TLOCTIM
   INTEGER                        :: IDAY
   INTEGER                        :: IHOUR
   INTEGER                        :: IMINUTE
   INTEGER                        :: ISECOND

   call GETH_IDTS(NOWDATE(1:10), NOWDATE(1:4)//"-01-01", IDAY)
   read(NOWDATE(12:13), *) IHOUR
   read(NOWDATE(15:16), *) IMINUTE
   read(NOWDATE(18:19), *) ISECOND
   JULIAN = REAL(IDAY) + REAL(IHOUR)/24.

!
! FOR SHORT WAVE RADIATION

   DECLIN=0.

!-----OBECL : OBLIQUITY = 23.5 DEGREE.

   OBECL=23.5*DEGRAD
   SINOB=SIN(OBECL)

!-----CALCULATE LONGITUDE OF THE SUN FROM VERNAL EQUINOX:

   if (JULIAN.GE.80.) SXLONG=DPD*(JULIAN-80.)*DEGRAD
   if (JULIAN.LT.80.) SXLONG=DPD*(JULIAN+285.)*DEGRAD
   ARG=SINOB*SIN(SXLONG)
   DECLIN=ASIN(ARG)

   TLOCTIM = REAL(IHOUR) + REAL(IMINUTE)/60.0 + REAL(ISECOND)/3600.0 + LONGITUDE/15.0 ! LOCAL TIME IN HOURS
   TLOCTIM = AMOD(TLOCTIM+24.0, 24.0)
   HRANG=15.*(TLOCTIM-12.)*DEGRAD
   COSZ=SIN(LATITUDE*DEGRAD)*SIN(DECLIN)+COS(LATITUDE*DEGRAD)*COS(DECLIN)*COS(HRANG)

 end subroutine CALC_DECLIN

!
!------------------------------------------------------------------------------------------
