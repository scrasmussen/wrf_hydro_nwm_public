#include "WRFHydro_NUOPC_Macros.h"

#define DEBUG

module wrfhydro_nuopc_gluecode
! !MODULE: wrfhydro_nuopc_gluecode
!
! !DESCRIPTION:
!   This module connects NUOPC initialize, advance,
!   and finalize to WRFHYDRO.
!
! !REVISION HISTORY:
!  13Oct15    Dan Rosen  Initial Specification
!
! !USES:
  use ESMF
  use NUOPC
  use mpi
  use WRFHydro_ESMF_Extensions
  use wrfhydro_nuopc_utils, only: array_write_i
  use module_mpp_land, only: &
    HYDRO_COMM_WORLD, &
    numprocs, &
    global_nx, &
    global_ny, &
    startx, &
    starty, &
    local_nx_size, &
    local_ny_size, &
    log_map2d, &
    mpp_land_par_ini, &
    decompose_data_real, &
    write_io_real, my_id, &
    mpp_land_bcast_real1, &
    IO_id, &
    mpp_land_bcast_real, &
    mpp_land_bcast_int1, &
    MPP_LAND_INIT
  use module_HYDRO_drv, only: &
    HYDRO_ini, &
    HYDRO_exe
  use module_HYDRO_io, only: &
    get_file_dimension
  use module_CPL_LAND, only: &
    CPL_LAND_INIT, &
    cpl_outdate
  use module_rt_data, only: &
    rt_domain
  use module_lsm_forcing, only: &
    read_ldasout
  use config_base, only: &
    nlst, &
    init_namelist_rt_field
  use orchestrator_base
  use wrfhydro_nuopc_fields
  use wrfhydro_nuopc_flags
  use wrfhydro_nuopc_utils, only: check, check_nf, printa

  implicit none

  private

  public :: wrfhydro_nuopc_ini
  public :: wrfhydro_nuopc_run
  public :: wrfhydro_nuopc_fin
  public :: wrfhydro_open_mesh
  public :: wrfhydro_regrid_mesh
  public :: wrfhydro_GridCreate
  public :: wrfhydro_get_timestep
  public :: wrfhydro_set_timestep
  public :: wrfhydro_get_hgrid
  public :: wrfhydro_get_restart

  public :: wrfhydro_write_geo_file
  public :: wrfhydro_GridCreate_init
  public :: regrid_import_mesh_to_grid
  public :: regrid_export_grid_to_mesh

  type(ESMF_RouteHandle) :: route_handle

  character(len=ESMF_MAXSTR), parameter :: file = __FILE__
  character(len=24), parameter :: filename = "WRFHydro_NUOPC_Gluecode"
  character(len=27), parameter :: modname = "WRFHydro_NUOPC_Gluecode.F90"

  ! PARAMETERS
  character(len=ESMF_MAXSTR) :: indir = 'WRFHYDRO_FORCING'
  integer                    :: num_nests = UNINITIALIZED
  integer                    :: num_tiles
  integer                    :: nx_global(1)
  integer                    :: ny_global(1)
  integer                    :: x_start
  integer                    :: x_end
  integer                    :: y_start
  integer                    :: y_end
  integer                    :: nx_local
  integer                    :: ny_local
  integer                    :: sf_surface_physics = UNINITIALIZED

  ! added to consider the adaptive time step from driver.
  real                  :: dt0 = UNINITIALIZED
  real                  :: dtrt_ter0 = UNINITIALIZED
  real                  :: dtrt_ch0 = UNINITIALIZED
  integer               :: dt_factor0 = UNINITIALIZED
  integer               :: dt_factor = UNINITIALIZED
  ! added for check soil moisture and soiltype
  integer               :: checkSOIL_flag = UNINITIALIZED
  ! added to track the driver clock
  character(len=19)     :: startTimeStr = "0000-00-00_00:00:00"

  type(ESMF_DistGrid)   :: wrfhydro_distgrid ! One DistGrid created with ConfigFile dimensions
  logical :: wrfhydro_distgrid_initialized = .false.
  character(len=512) :: logMsg


  interface read_mesh_var_and_regrid
     module procedure read_mesh_var_and_regrid_i
     module procedure read_mesh_var_and_regrid_r
  end interface read_mesh_var_and_regrid


  !-----------------------------------------------------------------------------
  ! Model Glue Code
  !-----------------------------------------------------------------------------
contains

#undef METHOD
#define METHOD "wrfhydro_nuopc_ini"

  subroutine wrfhydro_nuopc_ini(did,vm,clock,forcingDir,rc)
    integer, intent(in)                     :: did
    type(ESMF_VM),intent(in)                :: vm
    type(ESMF_Clock),intent(in)             :: clock
    character(len=*)                        :: forcingDir
    integer, intent(out)                    :: rc

    ! local variables
    integer                     :: localPet
    integer                     :: stat
    integer, allocatable        :: deBlockList(:,:,:)
    type(ESMF_DistGridConnection), allocatable :: connectionList(:)
    integer                     :: i
    type(ESMF_Time)             :: startTime
    type(ESMF_TimeInterval)     :: timeStep
    real(ESMF_KIND_R8)          :: dt
#ifdef DEBUG
    character(ESMF_MAXSTR)      :: logMsg
#endif

#ifdef DEBUG
    call ESMF_LogWrite(MODNAME//": entered "//METHOD, ESMF_LOGMSG_INFO)
#endif

    rc = ESMF_SUCCESS

    ! Set mpiCommunicator for WRFHYDRO
    call ESMF_VMGet(vm, localPet=localPet, &
      mpiCommunicator=HYDRO_COMM_WORLD, rc=rc)
    if(ESMF_STDERRORCHECK(rc)) return ! bail out

    ! Set focing directory
    indir=forcingDir

    ! Get the models timestep
    call ESMF_ClockGet(clock,timestep=timestep,startTime=startTime,rc=rc)
    if(ESMF_STDERRORCHECK(rc)) return ! bail out
    call ESMF_TimeIntervalGet(timestep,s_r8=dt,rc=rc)
    if(ESMF_STDERRORCHECK(rc)) return ! bail out
    call WRFHYDRO_TimeToString(startTime,timestr=startTimeStr,rc=rc)
    if(ESMF_STDERRORCHECK(rc)) return ! bail out

    call orchestrator%init()


    ! Set default namelist values
    read (startTimeStr(1:4),"(I4)")   nlst(did)%START_YEAR
    read (startTimeStr(6:7),"(I2)")   nlst(did)%START_MONTH
    read (startTimeStr(9:10),"(I2)")  nlst(did)%START_DAY
    read (startTimeStr(12:13),"(I2)") nlst(did)%START_HOUR
    read (startTimeStr(15:16),"(I2)") nlst(did)%START_MIN
    nlst(did)%startdate(1:19) = startTimeStr(1:19)
    nlst(did)%olddate(1:19)   = startTimeStr(1:19)
    nlst(did)%dt = dt
    cpl_outdate = startTimeStr(1:19)
    nlst(did)%nsoil=4
    allocate(nlst(did)%zsoil8(4),stat=stat)
    if (ESMF_LogFoundAllocError(statusToCheck=stat, &
      msg=METHOD//': Allocation of model soil depths memory failed.', &
      file=FILENAME, rcToReturn=rc)) return ! bail out
    nlst(did)%zsoil8(1:4)=(/-0.1,-0.4,-1.0,-2.0/)
    ! nlst(did)%geo_static_flnm = "geo_em.d01.nc"
    ! nlst(did)%geo_static_flnm = "fixed/frontrange.init.fixed.nc"
    ! geo_static_flnm is read in init_namelist_rt_field call
    nlst(did)%geo_finegrid_flnm = "fulldom_hires_hydrofile.d01.nc"
    nlst(did)%sys_cpl = 2
    nlst(did)%sys_cpl = 5 ! added to couple to MPAS
    print *, "---FIX THIS TO NAMELIST OPTION OR SOMETHING ELSE---"

    nlst(did)%IGRID = did
    write(nlst(did)%hgrid,'(I1)') did

    if(nlst(did)%dt .le. 0) then
      call ESMF_LogSetError(ESMF_FAILURE, &
        msg=METHOD//": Timestep less than 1 is not supported!", &
        file=FILENAME,rcToReturn=rc)
      return  ! bail out
    endif

     ! Read information from hydro.namelist config file
     call init_namelist_rt_field(did)

#ifdef DEBUG
    call WRFHYDRO_nlstLog(did,MODNAME,rc=rc)
    if(ESMF_STDERRORCHECK(rc)) return ! bail out
#endif

    if(nlst(did)%nsoil .gt. 4) then
      call ESMF_LogSetError(ESMF_FAILURE, &
        msg=METHOD//": Maximum soil levels supported is 4.", &
        file=FILENAME,rcToReturn=rc)
      return  ! bail out
    endif

    print *, "FIX: getting dimension from ", trim(nlst(did)%geo_static_flnm)
    ! gets - west_east, south_north,
    call get_file_dimension(fileName=nlst(did)%geo_static_flnm, &
         ix=nx_global(1), jx=ny_global(1)) !, &
         ! x_dim_var="x", y_dim_var="y")


    print *, "nx_global(1),ny_global(1) =", nx_global(1),ny_global(1)
    call MPP_LAND_INIT(nx_global(1), ny_global(1))


#ifdef DEBUG
    write (logMsg,"(A,2(I0,A))") MODNAME//": Global Dimensions = (", &
      nx_global(1),",",ny_global(1),")"
    call ESMF_LogWrite(trim(logMsg), ESMF_LOGMSG_INFO)
#endif

    call log_map2d()

    call ESMF_VMBroadcast(vm, nx_global, count=1, rootPet=IO_id, rc=rc)
    if(ESMF_STDERRORCHECK(rc)) return ! bail out
    call ESMF_VMBroadcast(vm, ny_global, count=1, rootPet=IO_id, rc=rc)
    if(ESMF_STDERRORCHECK(rc)) return ! bail out

    rt_domain(did)%ix = nx_global(1)
    rt_domain(did)%jx = ny_global(1)

    print *, "DEBUGGING: rt_domain(did)%ix, rt_domain(did)%jx", &
         rt_domain(did)%ix, rt_domain(did)%jx
    print*, "DEBUGGING: nlst(did)%AGGFACTRT =", nlst(did)%AGGFACTRT
    ! stop "DEBUGGING GLUE"


    call MPP_LAND_PAR_INI(1,rt_domain(did)%ix,rt_domain(did)%jx,&
         nlst(did)%AGGFACTRT)

    call ESMF_VMBroadcast(vm, startx, count=numprocs, rootPet=IO_id, rc=rc)
    if(ESMF_STDERRORCHECK(rc)) return ! bail out
    call ESMF_VMBroadcast(vm, starty, count=numprocs, rootPet=IO_id, rc=rc)
    if(ESMF_STDERRORCHECK(rc)) return ! bail out
    call ESMF_VMBroadcast(vm, local_nx_size, count=numprocs, rootPet=IO_id, rc=rc)
    if(ESMF_STDERRORCHECK(rc)) return ! bail out
    call ESMF_VMBroadcast(vm, local_ny_size, count=numprocs, rootPet=IO_id, rc=rc)
    if(ESMF_STDERRORCHECK(rc)) return ! bail out

    allocate(deBlockList(2,2,numprocs))
    do i = 1, numprocs
      deBlockList(:,1,i) = (/startx(i),starty(i)/)
      deBlockList(:,2,i) = (/startx(i)+local_nx_size(i)-1, &
                             starty(i)+local_ny_size(i)-1/)
!      write (logMsg,"(A,I0,A,4(I0,A))") MODNAME//": deBlockList ", i, " = (", &
!        deBlockList(1,1,i),":",deBlockList(1,2,i),",", &
!        deBlockList(2,1,i),":",deBlockList(2,2,i),")"
!      call ESMF_LogWrite(trim(logMsg), ESMF_LOGMSG_INFO)
    enddo

!    allocate(connectionList(1),stat=stat)
!    if (ESMF_LogFoundAllocError(statusToCheck=stat, &
!      msg=METHOD//': Allocation of connection list memory failed.', &
!      file=FILENAME, rcToReturn=rc)) return ! bail out
!    call ESMF_DistGridConnectionSet(connectionList(1), tileIndexA=1, &
!      tileIndexB=1, positionVector=(/nx_global(1), 0/), rc=rc)
!    if (ESMF_STDERRORCHECK(rc)) return  ! bail out


    ! Create DistGrid based on WRFHDYRO Config NX,NY
    wrfhydro_distgrid = ESMF_DistGridCreate( &
      minIndex=(/1,1/), maxIndex=(/nx_global(1),ny_global(1)/), &
!     indexflag = ESMF_INDEX_DELOCAL, &
     deBlockList=deBlockList, &
!     deLabelList=deLabelList, &
!     delayout=delayout, &
!     connectionList=connectionList, &
      rc=rc)
    if(ESMF_STDERRORCHECK(rc)) return ! bail out
    wrfhydro_distgrid_initialized = .true.


    deallocate(deBlockList)

!   deallocate(connectionList,stat=stat)
!   if (ESMF_LogFoundDeallocError(statusToCheck=stat, &
!     msg=METHOD//': Deallocation of connection list memory failed.', &
!     file=FILENAME,rcToReturn=rc)) return ! bail out

    ! Get the Local Decomp Incides
    call set_local_indices(rc)
    if(ESMF_STDERRORCHECK(rc)) return ! bail out

#ifdef DEBUG
    call ESMF_LogWrite(MODNAME//": Enter CPL_LAND_INIT", ESMF_LOGMSG_INFO)
#endif
    ! Initialize the internal Land <-> Hydro Coupling
    call CPL_LAND_INIT(x_start, x_end, y_start, y_end)
#ifdef DEBUG
    call ESMF_LogWrite(MODNAME//": Exit CPL_LAND_INIT", ESMF_LOGMSG_INFO)
#endif

    ! Routing timestep set in HYDRO_ini
#ifdef DEBUG
    call ESMF_LogWrite(MODNAME//": Enter HYDRO_ini", ESMF_LOGMSG_INFO)
#endif

    if(sf_surface_physics .eq. 5) then
      ! clm4
      ! Use wrfinput vegetation type and soil type
      call HYDRO_ini(ntime=1,did=did,ix0=1,jx0=1)
    else
      ! Use wrfinput vegetation type and soil type
      call HYDRO_ini(ntime=1,did=did,ix0=nx_local,jx0=ny_local)
    endif
#ifdef DEBUG
    call ESMF_LogWrite(MODNAME//": Exit HYDRO_ini", ESMF_LOGMSG_INFO)
    call WRFHYDRO_domainLog(did,MODNAME,rc=rc)
    if(ESMF_STDERRORCHECK(rc)) return ! bail out
#endif

    ! Override the clock configuration in hyro.namelist
    read (startTimeStr(1:4),"(I4)")   nlst(did)%START_YEAR
    read (startTimeStr(6:7),"(I2)")   nlst(did)%START_MONTH
    read (startTimeStr(9:10),"(I2)")  nlst(did)%START_DAY
    read (startTimeStr(12:13),"(I2)") nlst(did)%START_HOUR
    read (startTimeStr(15:16),"(I2)") nlst(did)%START_MIN
    nlst(did)%startdate(1:19) = startTimeStr(1:19)
    nlst(did)%olddate(1:19)   = startTimeStr(1:19)
    nlst(did)%dt = dt
    nlst(did)%nsoil=4
    cpl_outdate = startTimeStr(1:19)

    if(nlst(did)%dt .le. 0) then
      call ESMF_LogSetError(ESMF_FAILURE, &
        msg=METHOD//": Timestep less than 1 is not supported!", &
        file=FILENAME,rcToReturn=rc)
      return  ! bail out
    endif

    ! Adjust the routing timestep and factor
    ! At this point the coupling driver timestep is unknown
    ! and uses WRFHYDRO Config as best guess
    if(nlst(did)%dtrt_ter .ge. nlst(did)%dt) then
       nlst(did)%dtrt_ter = nlst(did)%dt
       dt_factor0 = 1
    else
       dt_factor = nlst(did)%dt/nlst(did)%dtrt_ter
       if (dt_factor*nlst(did)%dtrt_ter .lt. nlst(did)%dt) &
         nlst(did)%dtrt_ter = nlst(did)%dt/dt_factor
       dt_factor0 = dt_factor
    endif

    if(nlst(did)%dtrt_ch .ge. nlst(did)%dt) then
      nlst(did)%dtrt_ch = nlst(did)%dt
      dt_factor0 = 1
    else
      dt_factor = nlst(did)%dt/nlst(did)%dtrt_ch
      if(dt_factor*nlst(did)%dtrt_ch .lt. nlst(did)%dt) &
        nlst(did)%dtrt_ch = nlst(did)%dt/dt_factor
      dt_factor0 = dt_factor
    endif

    dt0 = nlst(did)%dt
    dtrt_ter0 = nlst(did)%dtrt_ter
    dtrt_ch0 = nlst(did)%dtrt_ch

    RT_DOMAIN(did)%initialized = .true.

    num_nests = num_nests + 1

#ifdef DEBUG
    call ESMF_LogWrite(MODNAME//": leaving "//METHOD, ESMF_LOGMSG_INFO)
#endif

  end subroutine

  !-----------------------------------------------------------------------------

#undef METHOD
#define METHOD "wrfhydro_nuopc_run"

  subroutine wrfhydro_nuopc_run(did,lsm_forcings,clock,importState,&
  exportState,rc)
    integer, intent(in)                     :: did
    logical, intent(in)                     :: lsm_forcings
    type(ESMF_Clock),intent(in)             :: clock
    type(ESMF_State),intent(inout)          :: importState
    type(ESMF_State),intent(inout)          :: exportState
    integer, intent(out)                    :: rc

    ! local variables
    type(ESMF_TimeInterval)     :: timeStep

#ifdef DEBUG
    call ESMF_LogWrite(MODNAME//": entered "//METHOD, ESMF_LOGMSG_INFO)
#endif

    rc = ESMF_SUCCESS

    if(.not. RT_DOMAIN(did)%initialized) then
      call ESMF_LogSetError(ESMF_FAILURE, &
        msg="WRHYDRO: Model has not been initialized!", &
        file=FILENAME,rcToReturn=rc)
      return  ! bail out
    endif

    call ESMF_ClockGet(clock, timeStep=timeStep, rc=rc)
    if(ESMF_STDERRORCHECK(rc)) return ! bail out

    call WRFHYDRO_ClockToString(clock,timestr=cpl_outdate,rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return
    nlst(did)%olddate(1:19) = cpl_outdate(1:19) ! Current time is the

    nlst(did)%dt = WRFHYDRO_TimeIntervalGetReal(timeInterval=timeStep,rc=rc)
    if(ESMF_STDERRORCHECK(rc)) return ! bail out

    if(nlst(did)%dt .le. 0) then
      call ESMF_LogSetError(ESMF_FAILURE, &
        msg=METHOD//": Timestep less than 1 is not supported!", &
        file=FILENAME,rcToReturn=rc)
      return  ! bail out
    endif

    if((dt_factor0*nlst(did)%dtrt_ter) .ne. nlst(did)%dt) then   ! NUOPC driver time step changed.
      call ESMF_LogWrite(METHOD//": Driver timestep changed.",ESMF_LOGMSG_INFO)
      if(dtrt_ter0 .ge. nlst(did)%dt) then
        nlst(did)%dtrt_ter = nlst(did)%dt
        dt_factor0 = 1
      else
        dt_factor = nlst(did)%dt / dtrt_ter0
        if(dt_factor*dtrt_ter0 .lt. nlst(did)%dt) &
          nlst(did)%dtrt_ter = nlst(did)%dt / dt_factor
        dt_factor0 = dt_factor
      endif
    endif

    if((dt_factor0*nlst(did)%dtrt_ch) .ne. nlst(did)%dt) then   ! NUOPD driver time step changed.
      call ESMF_LogWrite(METHOD//": Driver timestep changed.",ESMF_LOGMSG_INFO)
      if(dtrt_ch0 .ge. nlst(did)%dt) then
        nlst(did)%dtrt_ch = nlst(did)%dt
        dt_factor0 = 1
      else
        dt_factor = nlst(did)%dt / dtrt_ch0
        if(dt_factor*dtrt_ch0 .lt. nlst(did)%dt) &
          nlst(did)%dtrt_ch = nlst(did)%dt / dt_factor
        dt_factor0 = dt_factor
      endif
    endif

    if(nlst(did)%SUBRTSWCRT .eq.0  .and. &
      nlst(did)%OVRTSWCRT .eq. 0 .and. &
      nlst(did)%GWBASESWCRT .eq. 0) then
       call ESMF_LogWrite(METHOD//": SUBRTSWCRT,OVRTSWCRT,GWBASESWCRT are zero!", &
            ESMF_LOGMSG_WARNING)
      !call ESMF_LogSetError(ESMF_FAILURE, &
      !  msg=METHOD//": SUBRTSWCRT,OVRTSWCRT,GWBASESWCRT are zero!", &
      !  file=FILENAME,rcToReturn=rc)
      !return  ! bail out
    endif

    print *, "WRF-H GLUE: lsm_forcings =", lsm_forcings
    if((.not. RT_DOMAIN(did)%initialized) .and. (nlst(did)%rst_typ .eq. 1) ) then
      call ESMF_LogWrite(METHOD//": Restart initial data from offline file.", &
        ESMF_LOGMSG_INFO)
    else
      if (.not. lsm_forcings) then
        call read_ldasout(olddate=nlst(did)%olddate(1:19), &
          hgrid=nlst(did)%hgrid, &
          indir=trim(indir), dt=nlst(did)%dt, &
          ix=rt_domain(did)%ix,jx=rt_domain(did)%jx, &
          infxsrt=rt_domain(did)%infxsrt,soldrain=rt_domain(did)%soldrain)
      endif
    endif
    print *, "DEBUGGING: COMPLETE"

    ! Call the WRF-HYDRO run routine
    call HYDRO_exe(did=did)

    ! provide groundwater soil flux to WRF for fully coupled simulations (FERSCH 09/2014)
    !if(nlst(did)%GWBASESWCRT .eq. 3 ) then
      !Wei Yu: comment the following two lines. Not ready
    !yw     qsgw(x_start(1):x_end(1),y_start(1):y_end(1)) = gw2d(did)%qsgw
    !yw     config_flags%gwsoilcpl = nlst(did)%gwsoilcpl
    !end if

#ifdef DEBUG
    call ESMF_LogWrite(MODNAME//": leaving "//METHOD, ESMF_LOGMSG_INFO)
#endif

  end subroutine wrfhydro_nuopc_run

  !-----------------------------------------------------------------------------

#undef METHOD
#define METHOD "wrfhydro_nuopc_fin"

  subroutine wrfhydro_nuopc_fin(did,rc)
    ! ARGUMENTES
    integer, intent(inout)      :: did
    integer, intent(out)        :: rc

    ! LOCAL VARIABLES
    integer                     :: stat

#ifdef DEBUG
    call ESMF_LogWrite(MODNAME//": entered "//METHOD, ESMF_LOGMSG_INFO)
#endif

    rc = ESMF_SUCCESS

    ! WRF-Hydro finish routine cannot be called because it stops MPI

!    DCR - Turned off to let the model deallocate memory
!    deallocate(nlst(did)%zsoil8)
!    if (ESMF_LogFoundDeallocError(statusToCheck=stat, &
!      msg=METHOD//': Deallocation of model soil depth memory failed.', &
!      file=FILENAME,rcToReturn=rc)) return ! bail out

    RT_DOMAIN(did)%initialized = .false.

#ifdef DEBUG
    call ESMF_LogWrite(MODNAME//": leaving "//METHOD, ESMF_LOGMSG_INFO)
#endif

  end subroutine


  subroutine write_netcdf_geo_file(dx, dy, hgt, soil_cat, lat, lon, veg, &
       lu_index, landmask)
    integer, intent(in) :: dx, dy
    real, dimension(:,:), intent(in) :: hgt, lat, lon
    integer, dimension(:,:), intent(in) :: soil_cat, veg, lu_index, landmask

    ! Constants
    character(len=*), parameter :: default_file = 'frontrange.d01.nc'
    character(len=*), parameter :: times_value = '0000-00-00_00:00:00'
    integer, parameter :: DateStrLen = len(times_value)

    ! Locals
    integer :: stat, ncid
    integer :: dim_time, dim_we, dim_sn, dim_dsl
    integer :: dim_soil_cat, dim_land_cat
    integer :: var_times, var_hgt, var_soil_cat, var_lat, var_lon, &
         var_veg, var_lu_index, var_landmask

    ! Local parameters
    character(len=*), parameter :: timestr = "0000-00-00_00:00:00"
    integer, parameter :: soil_cat_n = 16
    integer, parameter :: land_cat_n = 24

    print *, "=== entering write_netcdf_geo_file ==="
    ! Begin work
    ! Create/clobber file
    call check_nf(nf90_create(default_file, NF90_CLOBBER, ncid))

    ! Define Dimensions
    call check_nf(nf90_def_dim(ncid, 'Time', NF90_UNLIMITED, dim_time))
    call check_nf(nf90_def_dim(ncid, 'west_east', dx, dim_we))
    call check_nf(nf90_def_dim(ncid, 'south_north', dy, dim_sn))
    call check_nf(nf90_def_dim(ncid, 'soil_cat', soil_cat_n, dim_soil_cat))
    call check_nf(nf90_def_dim(ncid, 'land_cat', land_cat_n, dim_land_cat))
    call check_nf(nf90_def_dim(ncid, 'DateStrLen', DateStrLen, dim_dsl))

    ! Define Variables
    call check_nf(nf90_def_var(ncid, 'Times', NF90_CHAR, [dim_dsl, dim_time], var_times))
    call check_nf(nf90_def_var(ncid, 'HGT_M', NF90_FLOAT, [dim_we, dim_sn, dim_time], var_hgt))
    call check_nf(nf90_def_var(ncid, 'SCT_DOM', NF90_INT, [dim_we, dim_sn, dim_time], var_soil_cat))
    call check_nf(nf90_def_var(ncid, 'XLAT_M', NF90_FLOAT, [dim_we, dim_sn, dim_time], var_lat))
    call check_nf(nf90_def_var(ncid, 'XLONG_M', NF90_FLOAT, [dim_we, dim_sn, dim_time], var_lon))
    call check_nf(nf90_def_var(ncid, 'VEGTYP', NF90_INT, [dim_we, dim_sn, dim_time], var_veg))
    call check_nf(nf90_def_var(ncid, 'LU_INDEX', NF90_INT, [dim_we, dim_sn, dim_time], var_lu_index))
    call check_nf(nf90_def_var(ncid, 'LANDMASK', NF90_INT, [dim_we, dim_sn, dim_time], var_landmask))

    ! Attributes
    call check_nf(nf90_put_att(ncid, NF90_GLOBAL, 'ISWATER', 16))
    call check_nf(nf90_put_att(ncid, NF90_GLOBAL, 'ISLAKE', -1))
    call check_nf(nf90_put_att(ncid, NF90_GLOBAL, 'ISICE', 24))
    call check_nf(nf90_put_att(ncid, NF90_GLOBAL, 'ISURBAN', 1))
    call check_nf(nf90_put_att(ncid, NF90_GLOBAL, 'ISOILWATER', 14))

    ! call check_nf(nf90_put_att(ncid, var_hgt, 'description', 'topography height'))

    ! End define mode before writing data
    call check_nf(nf90_enddef(ncid))

    ! Write Variables
    call check_nf(nf90_put_var(ncid, var_times, timestr, start=[1,1], count=[DateStrLen,1]))
    call check_nf(nf90_put_var(ncid, var_hgt, hgt, start=[1,1,1], count=[dx,dy,1]))
    call check_nf(nf90_put_var(ncid, var_soil_cat, soil_cat, start=[1,1,1], count=[dx,dy,1]))
    call check_nf(nf90_put_var(ncid, var_lat, lat, start=[1,1,1], count=[dx,dy,1]))
    call check_nf(nf90_put_var(ncid, var_lon, lon, start=[1,1,1], count=[dx,dy,1]))
    call check_nf(nf90_put_var(ncid, var_veg, veg, start=[1,1,1], count=[dx,dy,1]))
    call check_nf(nf90_put_var(ncid, var_lu_index, lu_index, start=[1,1,1], count=[dx,dy,1]))
    call check_nf(nf90_put_var(ncid, var_landmask, landmask, start=[1,1,1], count=[dx,dy,1]))
    ! call check_nf(nf90_put_var(ncid, var_, , start=[1,1,1], count=[dx,dy,1]))



    call check_nf(nf90_close(ncid))
    print *, "=== exiting write_netcdf_geo_file ==="
  end subroutine write_netcdf_geo_file

  subroutine read_mesh_var_and_regrid_r(var_name, outvar, &
       ncid, nCells, &
       regrid_handle, srcPtr, dstPtr, f_src, f_dst)
    character(len=*), intent(in) :: var_name
    real, allocatable, intent(inout) :: outvar(:,:)
    integer, intent(in) :: ncid, nCells
    real(ESMF_KIND_R8), pointer, intent(inout) :: srcPtr(:), dstPtr(:,:)
    type(ESMF_Field) :: f_src, f_dst
    type(ESMF_RouteHandle), intent(inout) :: regrid_handle

    ! local
    real, allocatable :: var(:)
    integer :: rc, stat, varid
    print *, "Regridding ", trim(var_name)

    ! read in NetCDF Vars
    stat = nf90_inq_varid(ncid, var_name, varid)
    call check_nf(stat)
    allocate(var(nCells))
    stat = nf90_get_var(ncid, varid, var)
    call check_nf(stat)

    ! print *, "ncells =", ncells
    ! print *, "var_r(1:10) =", var(1:10)

    if (size(srcPtr) /= size(var)) then
       rc = ESMF_RC_ARG_SIZE
       stop 'var_r length does not match local mesh ELEMENT count.'
    end if
    ! if (size(dstPtr,1) /= size(outvar,1) .or. &
    !     size(dstPtr,2) /= size(outvar,2)) then
    !    rc = ESMF_RC_ARG_SIZE
    !     stop 'outvar shape does not match local grid CENTER size.'
    ! end if

    srcPtr = real(var, kind=ESMF_KIND_R8)
    dstPtr = 0.0_ESMF_KIND_R8

    call ESMF_FieldRegrid(srcField=f_src, dstField=f_dst, &
         routehandle=regrid_handle, &
         rc=rc)
    call check(rc, __LINE__, file)

    outvar = dstPtr

    print *, "  outvar_r(1:2,1:2) =", outvar(1:2,1:2)
    print *, "  outvar_r shape =", shape(outvar)
  end subroutine read_mesh_var_and_regrid_r


  subroutine read_mesh_var_and_regrid_i(var_name, outvar, &
       ncid, nCells, &
       regrid_handle, srcPtr, dstPtr, f_src, f_dst)
    character(len=*), intent(in) :: var_name
    integer, allocatable, intent(inout) :: outvar(:,:)
    integer, intent(in) :: ncid, nCells
    real(ESMF_KIND_R8), pointer, intent(inout) :: srcPtr(:), dstPtr(:,:)
    type(ESMF_Field) :: f_src, f_dst
    type(ESMF_RouteHandle), intent(inout) :: regrid_handle

    ! local
    integer, allocatable :: var_i(:)
    integer :: rc, stat, varid
    print *, "Regridding ", trim(var_name)

    ! read in NetCDF Vars
    stat = nf90_inq_varid(ncid, var_name, varid)
    call check_nf(stat)
    allocate(var_i(nCells))
    stat = nf90_get_var(ncid, varid, var_i)
    call check_nf(stat)

    if (size(srcPtr) /= size(var_i)) then
       rc = ESMF_RC_ARG_SIZE
       print *, "size(srcPtr) =", size(srcPtr), "size(var_i)) =", size(var_i)
       error stop 'var_i length does not match local mesh ELEMENT count.'
    end if
    ! if (size(dstPtr,1) /= size(outvar,1) .or. &
    !     size(dstPtr,2) /= size(outvar,2)) then
    !    rc = ESMF_RC_ARG_SIZE
    !     stop 'outvar shape does not match local grid CENTER size.'
    ! end if

    srcPtr = real(var_i, kind=ESMF_KIND_R8)
    dstPtr = 0.0_ESMF_KIND_R8

    call ESMF_FieldRegrid(srcField=f_src, dstField=f_dst, &
         routehandle=regrid_handle, &
         rc=rc)
    call check(rc, __LINE__, file)

    ! print*, trim(var_name), "dstPtr =", dstPtr
    outvar = nint(dstPtr)

    print *, "  outvar_i(1:2,1:2) =", outvar(1:2,1:2)
    print *, "  outvar_i shape =", shape(outvar)
  end subroutine read_mesh_var_and_regrid_i

  subroutine wrfhydro_write_geo_file(wrfhydro_grid, wrfhydro_mesh, &
       regrid_handle_bl, regrid_handle_nn_stod, regrid_handle_nn_dtos)
    use netcdf
    type(ESMF_Grid), intent(in) :: wrfhydro_grid
    type(ESMF_Mesh), intent(in) :: wrfhydro_mesh
    type(ESMF_RouteHandle), intent(inout) :: regrid_handle_bl
    type(ESMF_RouteHandle), intent(inout) :: regrid_handle_nn_stod
    type(ESMF_RouteHandle), intent(inout) :: regrid_handle_nn_dtos
    ! type(ESMF_Mesh)            :: wrfhydro_mesh
    character(:), allocatable :: mpas_file

    ! NetCDF Variables
    integer :: stat, ncid, dimid, nCells, varid
    integer :: ncid_out
    integer, allocatable :: var_i(:)
    real, allocatable :: var_r(:)

    ! ESMF Variables
    type(ESMF_Field) :: f_src, f_dst
    real(ESMF_KIND_R8), pointer :: srcPtr(:) => null()
    real(ESMF_KIND_R8), pointer :: dstPtr(:,:) => null()
    integer :: rc

    ! result
    character(len=*), parameter :: items(2) = [ "foo", "bar" ]
    ! integer, allocatable :: outvar_i(:,:)
    ! real, allocatable :: outvar_r(:,:)

    ! vars to write
    real, dimension(:,:), allocatable :: hgt, lat, lon
    integer, dimension(:,:), allocatable :: soil_cat, veg, lu_index, landmask

    ! locals
    integer :: nx, ny
    ! type(ESMF_RouteHandle) :: regrid_handle_tmp

    print *, "=== entering wrfhydro_write_geo_file ==="
    ! initialize values
    ! regrid_handle_tmp = regrid_handle

    ! setup MPAS NetCDF variables
    mpas_file = "frontrange.static.nc"
    print *, "TODO: mpas_file for geo is hardcoded to: ", mpas_file

    stat = nf90_open(trim(mpas_file), NF90_NOWRITE, ncid)
    call check_nf(stat)

    stat = nf90_inq_dimid(ncid, 'nCells', dimid)
    call check_nf(stat)

    stat = nf90_inquire_dimension(ncid, dimid, len=nCells)
    call check_nf(stat)

    ! setup MPAS regridding
    !   regrid MPAS mesh to WRF-Hydro hi-res grid
    !   Create source and destination field on grid (CENTER)
    f_src = ESMF_FieldCreate(mesh=wrfhydro_mesh, &
         typekind=ESMF_TYPEKIND_R8, &
         meshloc=ESMF_MESHLOC_ELEMENT, &
         name='var_src', rc=rc)
    call check(rc, __LINE__, file)
    f_dst = ESMF_FieldCreate(grid=wrfhydro_grid, &
         typekind=ESMF_TYPEKIND_R8, &
         indexflag=ESMF_INDEX_DELOCAL, &
         name='var_dst', rc=rc)
    call check(rc, __LINE__, file)

    call ESMF_FieldGet(f_src, farrayPtr=srcPtr, rc=rc)
    call check(rc, __LINE__, file)
    call ESMF_FieldGet(f_dst, farrayPtr=dstPtr, rc=rc)
    call check(rc, __LINE__, file)

    ! print *, "prencells =", ncells
    ! print *, "prelat(1:2,1:2) =", lat(1:2,1:2)
    ! print *, "prelon(1:2,1:2) =", lon(1:2,1:2)
    ! print *, "prehgt(1:2,1:2) =", hgt(1:2,1:2)
    ! print *, "presoil_cat(1:2,1:2) =", soil_cat(1:2,1:2)
    ! print *, "preveg(1:2,1:2) =", veg(1:2,1:2)
    ! print *, "prelu_index(1:2,1:2) =", lu_index(1:2,1:2)
    ! print *, "prelandmask(1:2,1:2) =", landmask(1:2,1:2)
    ! print *, "=================="


    ! nearest neighbor regridding
    call read_mesh_var_and_regrid('isltyp', soil_cat, ncid, nCells, &
         regrid_handle_nn_stod, srcPtr, dstPtr, f_src, f_dst)
    call read_mesh_var_and_regrid('ivgtyp', veg, ncid, nCells, &
         regrid_handle_nn_stod, srcPtr, dstPtr, f_src, f_dst) ! this might be overkill
    call read_mesh_var_and_regrid('ivgtyp', lu_index, ncid, nCells, &
         regrid_handle_nn_stod, srcPtr, dstPtr, f_src, f_dst)
    call read_mesh_var_and_regrid('landmask', landmask, ncid, nCells, &
         regrid_handle_nn_stod, srcPtr, dstPtr, f_src, f_dst)
    ! bilinear regridding
    call read_mesh_var_and_regrid('ter', hgt, ncid, nCells, &
         regrid_handle_bl, srcPtr, dstPtr, f_src, f_dst)
    call read_mesh_var_and_regrid('latCell', lat, ncid, nCells, &
         regrid_handle_bl, srcPtr, dstPtr, f_src, f_dst)
    call read_mesh_var_and_regrid('lonCell', lon, ncid, nCells, &
         regrid_handle_bl, srcPtr, dstPtr, f_src, f_dst)
    ! try turning off terrain and veg
    ! bilinear wont work for integer variables/categorical,
    !  - use nearest neightbor?? Majority would be the best choice if possible

    stat = nf90_close(ncid)
    call check_nf(stat)

    print *, "ncells =", ncells
    print *, "lat(1:2,1:2) =", lat(1:2,1:2)
    print *, "lon(1:2,1:2) =", lon(1:2,1:2)
    print *, "hgt(1:2,1:2) =", hgt(1:2,1:2)
    print *, "soil_cat(1:2,1:2) =", soil_cat(1:2,1:2)
    print *, "veg(1:2,1:2) =", veg(1:2,1:2)
    print *, "lu_index(1:2,1:2) =", lu_index(1:2,1:2)
    print *, "landmask(1:2,1:2) =", landmask(1:2,1:2)
    print *, "-----------------"
    print *, "writing veg grid"
    call array_write_i(veg, "veg_dtos.nc")
    call array_write_i(lu_index, "veg_stod.nc")


    nx = size(hgt, dim=1)
    ny = size(hgt, dim=2)
    call write_netcdf_geo_file(nx, ny, hgt, soil_cat, lat, lon, veg, &
         lu_index, landmask)


    print *, "=== exiting wrfhydro_write_geo_file ==="
  end subroutine wrfhydro_write_geo_file

  function wrfhydro_open_mesh(rc) result(mesh)
    integer, intent(out) :: rc
    type(ESMF_Mesh) :: mesh
    type(ESMF_DistGrid) :: distgrid
    type(ESMF_VM)       :: vm
    integer, allocatable :: gindex(:)
    character(:), allocatable :: mesh_file
    character(len=256) :: iomsg
    integer :: unit, iostat, irank, localCount
    integer :: rank, np
    integer :: idx, inode

    rc = ESMF_SUCCESS

    ! add the following to make generic
    call ESMF_VMGetGlobal(vm, rc=rc)
    call check(rc, __LINE__, file)
    call ESMF_VMGet(vm, localPet=rank, petCount=np, rc=rc)
    call check(rc, __LINE__, file)

    ! read
    open(newunit=unit, file='frontrange.graph.info.part.2', &
         status='old', action='read', iostat=iostat, iomsg=iomsg)
    if (iostat /= 0) then
       print *, trim(iomsg)
       stop "Error opening [casename].graph.info.part.[np]"
    end if

    localCount = 0
    do
       read(unit, *, iostat=iostat) irank
       if (iostat /= 0) exit
       if (irank == rank) localCount = localCount + 1
    end do

    allocate(gindex(localCount))

    print *, rank, "/", np, ": with localCount =", localCount

    ! setup grid distribution
    rewind(unit)
    idx   = 0
    inode = 0

    do
       read(unit, *, iostat=iostat) irank
       if (iostat /= 0) exit

       inode = inode + 1 ! inode = line number
       if (irank == rank) then
          idx = idx + 1
          gindex(idx) = inode ! seqIndex = global node id
       end if
    end do
    close(unit)


    distgrid = ESMF_DistGridCreate(arbSeqIndexList=gindex, rc=rc)
    call check(rc, __LINE__, file)

    mesh_file = "frontrange.scrip.nc"
    print *, "todo: read mesh_file name from namelist, currently ", trim(mesh_file)
    mesh = ESMF_MeshCreate(filename=mesh_file, &
         elementDistgrid=distgrid, &
         fileformat=ESMF_FILEFORMAT_SCRIP, rc=rc)
    call check(rc, __LINE__, file)

  end function wrfhydro_open_mesh

  function wrfhydro_regrid_mesh(input_grid, mesh, regrid_method, &
       did, importState, rc) result(handle)
    type(ESMF_Grid), intent(in) :: input_grid
    type(ESMF_Mesh), intent(in) :: mesh
    integer, intent(in) :: did
    type(ESMF_State), intent(inout) :: importState
    type(ESMF_RegridMethod_Flag), intent(in) :: regrid_method
    integer, intent(out) :: rc
    type(ESMF_RouteHandle) :: handle
    type(ESMF_Grid) :: grid

    type(ESMF_Field) :: import_field, new_field
    character(:), allocatable :: file

    ! testing variables
    logical :: realizeImport, connected
    character(:), allocatable :: st_name

    logical :: route_handle_initialized = .false.


    route_handle_initialized = .false.
    file = __FILE__
    rc = ESMF_SUCCESS
    print *, "enter: wrfhydro_regrid_mesh"
    call ESMF_LogWrite("WRFH: enter wrfhydro_regrid_mesh", &
         ESMF_LOGMSG_INFO, rc=rc)

    if (regrid_method == ESMF_REGRIDMETHOD_BILINEAR) then
       st_name = "bilinear"
    end if
    if (regrid_method == ESMF_REGRIDMETHOD_NEAREST_STOD) then
       st_name = "nn_stod"
       ! stop "GOOOD stod"
    end if
    if (regrid_method == ESMF_REGRIDMETHOD_NEAREST_DTOS) then
       st_name = "nn_dtos"
       ! stop "GOOOD dtos"
    end if
    ! check for import field

    print *, "========", trim(st_name), "========"

    call ESMF_StateLog(importState, logMsgFlag=ESMF_LOGMSG_INFO, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return

    import_field = ESMF_FieldCreate(name=st_name, &
         mesh=mesh, typekind=ESMF_TYPEKIND_R8, &
         meshloc=ESMF_MESHLOC_ELEMENT, & ! ESMF_MESHLOC_ELEMENT for cell-center vars
         rc=rc)
    if(ESMF_STDERRORCHECK(rc)) return
    new_field = ESMF_FieldCreate(name=st_name, &
         staggerloc=ESMF_STAGGERLOC_CENTER,         &
         grid=input_grid, typekind=ESMF_TYPEKIND_R8, rc=rc)
    if(ESMF_STDERRORCHECK(rc)) return

    ! THIS DOESN'T WORK BECAUSE IT IS A REDISTRIBUTION, WHEN AN INTERPOLATION
    !   IS NEEDED
    ! call ESMF_FieldRedistStore(srcField=import_field, dstField=new_field, &
    !      routehandle=route_handle, rc=rc)

    ! Build the interpolation operator once
    if (route_handle_initialized .eqv. .false.) then
       print *, "Initialized route_handle"
       call ESMF_LogWrite("Initialized route_handle", &
         ESMF_LOGMSG_INFO, rc=rc)

       ! generate regrid weights file and read in to handle
       call ESMF_RegridWeightGen(&
            srcFile='frontrange.scrip.nc', &
            dstFile='fulldom_hires_hydrofile.d01.nc', &
            weightFile='weights/'//st_name//'.nc', &
            regridmethod=regrid_method, & ! FOOBAR: THIS SHOULD REGEN EVERYTIME
! ESMF_REGRIDMETHOD_NEAREST_STOD, &
            rc=rc)
       ! Precompute Field sparse matrix multiplication with local factors
       call ESMF_FieldSMMStore(srcField=import_field, dstField=new_field, &
            filename='weights/'//st_name//'.nc', &
            routehandle=route_handle, rc=rc)

       ! generate regrid weights in to handle
       ! call ESMF_FieldRegridStore(srcfield=import_field, dstfield=new_field, &
       !      routehandle=route_handle, rc=rc, &
       !      regridmethod=ESMF_REGRIDMETHOD_BILINEAR, & ! or CONSERVE / PATCH
       !      lineType=ESMF_LINETYPE_GREAT_CIRCLE, &
       !      unmappedaction=ESMF_UNMAPPEDACTION_IGNORE)
       ! write handle to binary file
       ! call ESMF_RouteHandleWrite(route_handle, fileName="mpas_hydro.RH", rc=rc)
       ! if(ESMF_STDERRORCHECK(rc)) return
       if(ESMF_STDERRORCHECK(rc)) return
       route_handle_initialized = .true.

    end if


    ! each step (after src has values):
    print *, "Regrid field, applying weights"
    call ESMF_LogWrite("Regrid field, applying weights", &
         ESMF_LOGMSG_INFO, rc=rc)
    ! Compute a regridding operation
    call ESMF_FieldRegrid(import_field, new_field, route_handle, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return



    ! if (ESMF_STDERRORCHECK(rc)) return
    ! call NUOPC_Realize(importState, field=field_import, rc=rc)
    ! if (ESMF_STDERRORCHECK(rc)) return
    ! connected = NUOPC_IsConnected(importState, &
    !      fieldName=trim(st_name),rc=rc)


    ! ! get import field
    ! ! field_import=field_create(fld_name=fieldList(n)%st_name, &
    ! !       mesh=mesh, did=did, memflg=memr_import, rc=rc)

    ! ! field = ESMF_FieldCreate(name="sss", mesh=meshIn, &
    ! !   typekind=ESMF_TYPEKIND_R8, rc=rc)
    ! ! call NUOPC_Realize(importState, field=field, rc=rc)

    ! connected = NUOPC_IsConnected(importState, &
    !      fieldName=trim(st_name),rc=rc)

    ! print *, st_name, " variable connected? ", connected

    handle = route_handle ! return variable
    call ESMF_LogWrite("WRFH: exit wrfhydro_regrid_mesh", &
         ESMF_LOGMSG_INFO, rc=rc)
    print *, "WRFH: exit wrfhydro_regrid_mesh"
    ! error stop "where is this being called?"
  end function wrfhydro_regrid_mesh

  subroutine regrid_import_mesh_to_grid(grid, mesh, state, &
       did, memflg)
    type(ESMF_Grid), intent(in) :: grid
    type(ESMF_Mesh), intent(in) :: mesh
    type(ESMF_State),intent(in) :: state
    integer, intent(in) :: did
    type(memory_flag), intent(in) :: memflg
    type(ESMF_Field) :: meshField, gridField
    integer :: n, rc, itemCount
    character(len=64), allocatable :: itemNameList(:)
    logical :: imported

    ! debugging
    integer :: unit, i
    logical :: exists
    integer :: rank, ierr
    character(len=3) :: rank_s

    print *, 'WRFH: enter regrid_import_mesh_to_grid'
    call ESMF_LogWrite('WRFH: enter regrid_import_mesh_to_grid', ESMF_LOGMSG_INFO)
    call ESMF_LogWrite('--- WRFH: IMPORT STATE DEBUG: ', ESMF_LOGMSG_INFO)
    call ESMF_StateLog(state, rc=rc)
    call ESMF_LogWrite('--- WRFH: POST STATE PRINT: ', ESMF_LOGMSG_INFO)

    call ESMF_StateGet(state,itemCount=itemCount, rc=rc)
    call check(rc, __LINE__, file)
    allocate(itemNameList(itemCount), stat=rc)

    call ESMF_StateGet(state,itemNameList=itemNameList, rc=rc)
    call check(rc, __LINE__, file)
    do i=1,itemCount
       print *, "WRFH: import itemnamelist: ", itemNameList(i)
       call ESMF_LogWrite("WRFH: import itemnamelist: "// itemNameList(i), ESMF_LOGMSG_INFO, rc=rc)
    end do

    call handle_rank_string(rank, rank_s)

    do n=lbound(cap_fld_list,1),ubound(cap_fld_list,1)
       if (cap_fld_list(n)%ad_import) then
          imported = NUOPC_IsConnected(state, &
            fieldName=trim(cap_fld_list(n)%st_name), rc=rc)
          call check(rc, __LINE__, file)

          print *, "WRFH: imported = ", imported,&
               ", name: ", cap_fld_list(n)%st_name
          call ESMF_LogWrite("WRFH: imported "//cap_fld_list(n)%st_name, &
               ESMF_LOGMSG_INFO, rc=rc)

          call ESMF_StateGet(state, itemName=trim(cap_fld_list(n)%st_name), &
               field=meshField, rc=rc)
          call check(rc, __LINE__, file)

          ! gridField = field_create(cap_fld_list(n)%st_name, grid, did, &
          !      memflg, rc)
          call ESMF_GridValidate(grid, rc=rc)
          call check(rc, __LINE__, file)

          gridField = field_create(cap_fld_list(n)%st_name, grid, did, &
               memflg, rc)
          ! CREAT LOCAL COPY, NOT CONNECTED TO WRF-H MODEL MEMORY
          ! gridField = ESMF_FieldCreate(grid=grid, &
          !      typekind=ESMF_TYPEKIND_R8, &
          !      indexflag=ESMF_INDEX_DELOCAL, &
          !      name=cap_fld_list(n)%st_name, rc=rc)
          call check(rc, __LINE__, file)


          call check(rc, __LINE__, file)

          ! --- Debugging: write import vars before regrid
          call ESMF_FieldWrite(gridField, &
               "vars_in/"//trim(cap_fld_list(n)%st_name)//&
               ! "_grid_pre_"//trim(rank_s)//".nc", &
               "_grid_pre.nc", &
               overwrite=.true., &
               variableName=trim(cap_fld_list(n)%st_name), rc=rc)
          call ESMF_FieldWrite(meshField, &
               "vars_in/"//trim(cap_fld_list(n)%st_name)//&
               "_mesh_pre.nc", &
               overwrite=.true., &
               ! "_mesh_pre_"//trim(rank_s)//".nc", &
               variableName=trim(cap_fld_list(n)%st_name), rc=rc)
          call check(rc, __LINE__, file)
          ! --- end debug ---

          if (cap_fld_list(n)%import_handle_init .eqv. .false.) then
             ! call ESMF_FieldRegridStore(srcField=meshField, &
             !      dstField=gridField, &
             !      regridmethod=ESMF_REGRIDMETHOD_BILINEAR, &
             !      ! unmappedaction=ESMF_UNMAPPEDACTION_IGNORE, &
             !      routehandle=cap_fld_list(n)%import_handle, rc=rc)

             ! unmappedDstList=unmapped, &  ! get failing ids
             ! dstStatusField= dstStatus, & ! optional per-point codes
             ! extrapMethod  = ESMF_EXTRAPMETHOD_NEAREST_STOD, &! fill stragglers
             ! extrapNumSrcPnts=8, &        ! conservative default
             call check(rc, __LINE__, file)

             call ESMF_RegridWeightGen(&
                  srcFile='frontrange.scrip.nc', &
                  dstFile='fulldom_hires_hydrofile.d01.nc', &
                  weightFile='weights/'//trim(cap_fld_list(n)%st_name)//'_import.nc', &
                  regridmethod=cap_fld_list(n)%regrid_method, &
                  rc=rc)
             call check(rc, __LINE__, file)
             ! Precompute Field sparse matrix multiplication with local factors
             call ESMF_FieldSMMStore(srcField=meshfield, dstField=gridfield, &
                  filename='weights/'//trim(cap_fld_list(n)%st_name)//'_import.nc', &
                  routehandle=cap_fld_list(n)%import_handle, &
                  transposeRoutehandle=cap_fld_list(n)%export_handle, &
                  rc=rc)
             call check(rc, __LINE__, file)

             cap_fld_list(n)%import_handle_init = .true.
             cap_fld_list(n)%export_handle_init = .true.
          end if

          print*, "DEBUGGING: using var's handle to regrid field ", &
               trim(cap_fld_list(n)%st_name)
          call ESMF_FieldRegrid(meshField, gridField, &
               cap_fld_list(n)%import_handle, rc=rc)
          call check(rc, __LINE__, file)

          ! --- Debugging: write import mesh before regrid
          call ESMF_FieldWrite(meshField, &
               "vars_in/"//trim(cap_fld_list(n)%st_name)// &
               "_mesh_post.nc", &
               overwrite=.true., &
               ! "_mesh_post_"//trim(rank_s)//".nc", &
               variableName=trim(cap_fld_list(n)%st_name), rc=rc)
          call check(rc, __LINE__, file)
          call ESMF_FieldWrite(gridField, &
               "vars_in/"//trim(cap_fld_list(n)%st_name)// &
               "_grid_post.nc", &
               overwrite=.true., &
               ! "_grid_post_"//trim(rank_s)//".nc", &
               variableName=trim(cap_fld_list(n)%st_name), rc=rc)
          call check(rc, __LINE__, file)
          ! --- end debug ---

       end if
    end do
  end subroutine regrid_import_mesh_to_grid

  subroutine regrid_export_grid_to_mesh(grid, mesh, state, did, memflg)
    use MPI
    type(ESMF_Grid), intent(in) :: grid
    type(ESMF_Mesh), intent(in) :: mesh
    type(ESMF_State),intent(in) :: state
    integer, intent(in) :: did
    type(memory_flag), intent(in) :: memflg
    type(ESMF_Field) :: meshField, gridField
    integer :: n, rc, itemCount
    character(len=64), allocatable :: itemNameList(:)
    logical :: exported

    real(ESMF_KIND_R8), pointer :: gridPtr(:,:) => null()
    type(ESMF_Array) :: gridarray
    real(ESMF_KIND_R8), pointer :: r82d(:,:) => null()
    real(ESMF_KIND_R4), pointer :: r42d(:,:) => null()
    real(ESMF_KIND_FIELD), pointer :: farrayPtr2d(:,:)

    ! debugging
    integer :: unit
    logical :: exists

    integer :: rank, localDECount, localDE
    type(ESMF_TypeKind_Flag) :: tk


    integer :: ierr


    ! stop "GETTING TO REGRID EXPORT GRID TO MESH"
    call ESMF_LogWrite('_____ EXPORT STATE DEBUG: ', ESMF_LOGMSG_INFO)
    call ESMF_StatePrint(state, rc=rc)

    call ESMF_StateGet(state,itemCount=itemCount, rc=rc)
    call check(rc, __LINE__, file)
    allocate(itemNameList(itemCount), stat=rc)

    call ESMF_StateGet(state,itemNameList=itemNameList, rc=rc)
    call check(rc, __LINE__, file)
    print *, "============ Export itemnamelist: ", itemNameList

    do n=lbound(cap_fld_list,1),ubound(cap_fld_list,1)
       if (cap_fld_list(n)%ad_export) then
          exported = NUOPC_IsConnected(state, &
            fieldName=trim(cap_fld_list(n)%st_name), rc=rc)
          call check(rc, __LINE__, file)

          print *, "exported = ", exported,", name: ", cap_fld_list(n)%st_name

          ! meshField = field_create(cap_fld_list(n)%st_name, mesh, did, &
          !      memflg, rc)
          ! call check(rc, __LINE__, file)
          ! will this work better??
          call ESMF_StateGet(state, itemName=trim(cap_fld_list(n)%st_name), &
               field=meshField, rc=rc)
          call check(rc, __LINE__, file)


          ! should probably be called field_get?
          gridField = field_create(cap_fld_list(n)%st_name, grid, did, &
               memflg, rc)
          call check(rc, __LINE__, file)



          ! MPI: CHECK FOR GRID DECOMPOSITION
          ! if (trim(cap_fld_list(n)%st_name) == "sh2ox1") then
          !    call ESMF_FieldGet(gridField, array=gridarray, rc=rc)
          !    call check(rc, __LINE__, file)

          !    call ESMF_ArrayGet(gridarray, typekind=tk, rank=rank, &
          !         localDECount=localDECount, rc=rc)
          !    call check(rc, __LINE__, file)
          !    print *, ""
          !    print *, "tk=", tk
          !    print *, "rank, decount", rank, localDECount
          !    call ESMF_ArrayGet(gridarray, farrayPtr=r42d, rc=rc)
          !    call check(rc, __LINE__, file)

          !    call MPI_Comm_rank(MPI_COMM_WORLD, rank, ierr)
          !    r42d = rank

          !    print* , trim(cap_fld_list(n)%st_name), ": TRY GRID ARRAY =", &
          !         r42d(1:2, 1:2)
          !    print *, "rt_domain(did)%sh2ox(1:2,1:2,1)= ", rt_domain(did)%sh2ox(1:2,1:2,1)
          ! end if

          ! Debugging: write export mesh before regrid
          call ESMF_FieldWrite(meshField, &
               "vars_out/"//trim(cap_fld_list(n)%st_name)// &
               "_mesh_pre.nc", &
               overwrite=.true., &
               variableName=trim(cap_fld_list(n)%st_name), rc=rc)
          call check(rc, __LINE__, file)
          call ESMF_FieldWrite(gridField, &
               "vars_out/"//trim(cap_fld_list(n)%st_name)// &
               "_grid_pre.nc", &
               overwrite=.true., &
               variableName=trim(cap_fld_list(n)%st_name), rc=rc)
          call check(rc, __LINE__, file)
          ! stop "ARTLESS"

          if (cap_fld_list(n)%export_handle_init .eqv. .false.) then
             call printa("Regridding export variables")
             stop "TESTING transposeRouteHandle: THIS SHOULD NOT BE REACHED"
             ! call ESMF_FieldRegridStore(srcField=gridField, &
             !      dstField=meshField, &
             !      regridmethod=ESMF_REGRIDMETHOD_BILINEAR, &
             !      unmappedaction=ESMF_UNMAPPEDACTION_IGNORE, &
             !      routehandle=cap_fld_list(n)%export_handle, rc=rc)
             ! call check(rc, __LINE__, file)
             stop "switch over to regrid method, using inverse though?"
             call ESMF_RegridWeightGen(&
                  srcFile='fulldom_hires_hydrofile.d01.nc', &
                  dstFile='frontrange.scrip.nc', &
                  weightFile='weights/'//trim(cap_fld_list(n)%st_name)//'_export.nc', &
                  regridmethod=ESMF_REGRIDMETHOD_NEAREST_STOD, &  ! <- no polygons required
                  ! regridmethod=ESMF_REGRIDMETHOD_BILINEAR, &
                  rc=rc)
             call check(rc, __LINE__, file)
             ! Precompute Field sparse matrix multiplication with local factors
             call ESMF_FieldSMMStore(srcField=gridField, dstField=meshField, &
                  filename='weights/'//trim(cap_fld_list(n)%st_name)//'_export.nc', &
                  routehandle=cap_fld_list(n)%export_handle, rc=rc)
             call check(rc, __LINE__, file)
             cap_fld_list(n)%export_handle_init = .true.
          end if

          print*, "DEBUGGING: using var's handle to regrid field ", &
               trim(cap_fld_list(n)%st_name)
          call ESMF_FieldRegrid(gridField, meshField, &
               cap_fld_list(n)%export_handle, rc=rc)
          call check(rc, __LINE__, file)

          ! Debugging: write export vars after regrid
          call ESMF_FieldWrite(meshField, &
               "vars_out/"//trim(cap_fld_list(n)%st_name)// &
               "_mesh_post.nc", &
               overwrite=.true., &
               variableName=trim(cap_fld_list(n)%st_name), rc=rc)
          call check(rc, __LINE__, file)
          call ESMF_FieldWrite(gridField, &
               "vars_out/"//trim(cap_fld_list(n)%st_name)// &
               "_grid_post.nc", &
               overwrite=.true., &
               variableName=trim(cap_fld_list(n)%st_name), rc=rc)
          call check(rc, __LINE__, file)
       end if
    end do
    ! stop "CHECKING IF REGRID EXPORT GRID TO MESH WORKS"
  end subroutine regrid_export_grid_to_mesh


  subroutine dump_state(state, label)
    use ESMF
    implicit none
    type(ESMF_State), intent(in) :: state
    character(len=*), intent(in) :: label

    integer :: rc, n, i, pet
    type(ESMF_VM) :: vm
    type(ESMF_StateItem_Flag), allocatable :: itemType(:)
    character(len=ESMF_MAXSTR), allocatable :: itemName(:)
    type(ESMF_StateItem_Flag) :: case
    type(ESMF_Field) :: f
    character(len=ESMF_MAXSTR) :: nm, stdn
    type(ESMF_State) :: sub

    call ESMF_VMGetCurrent(vm, rc=rc)
    call ESMF_VMGet(vm, localPet=pet, rc=rc)
    if (pet /= 0) return   ! print once

    call ESMF_StateGet(state, itemCount=n, rc=rc)
    if (n <= 0) then
       call ESMF_LogWrite(trim(label)//" state: (empty)", ESMF_LOGMSG_INFO)
       return
    endif

    allocate(itemType(n), itemName(n))
    call ESMF_StateGet(state, itemTypeList=itemType, itemNameList=itemName, rc=rc)

    do i = 1, n
       print*, "itemType(i) =", itemType(i)
       case = itemType(i)
       if (case == ESMF_STATEITEM_FIELD) then
          call ESMF_StateGet(state, itemName=itemName(i), field=f, rc=rc)
          call ESMF_FieldGet(f, name=nm, rc=rc)
          stdn = ""
          call ESMF_AttributeGet(f, name="standard_name", value=stdn, defaultValue="", rc=rc)
          call ESMF_LogWrite("Import Field: "//trim(nm)// &
               merge(" ["//trim(stdn)//"]","",len_trim(stdn)>0), ESMF_LOGMSG_INFO)

          ! case (ESMF_STATEITEM_FIELDBUNDLE)
          !   type(ESMF_FieldBundle) :: fb
          !   integer :: k, nf
          !   type(ESMF_Field), allocatable :: flist(:)
          !   character(len=ESMF_MAXSTR) :: bname, fname
          !   call ESMF_StateGet(state, itemName=itemName(i), fieldbundle=fb, rc=rc)
          !   call ESMF_FieldBundleGet(fb, name=bname, fieldCount=nf, rc=rc)
          !   allocate(flist(nf))
          !   call ESMF_FieldBundleGet(fb, fieldList=flist, rc=rc)
          !   call ESMF_LogWrite("Import FieldBundle: "//trim(bname), ESMF_LOGMSG_INFO)
          !   do k=1,nf
          !     call ESMF_FieldGet(flist(k), name=fname, rc=rc)
          !     call ESMF_LogWrite("  - "//trim(fname), ESMF_LOGMSG_INFO)
          !   enddo
          !   deallocate(flist)

       ! else if (case == ESMF_STATEITEM_STATE) then
          ! call ESMF_StateGet(state, itemName=itemName(i), state=sub, rc=rc)
          ! call ESMF_LogWrite("Nested State: "//trim(itemName(i)), ESMF_LOGMSG_INFO)
          ! call dump_state(sub, "  "//trim(label))  ! recurse
       else
          call ESMF_LogWrite("Unknown item: "//trim(itemName(i)), ESMF_LOGMSG_INFO)
       end if
    enddo
  end subroutine dump_state

  function wrfhydro_GridCreate_init(vm, did,rc) &
       result(grid)
    type(ESMF_VM), intent(in) :: vm
    type(ESMF_Grid) :: grid
    integer, intent(in) :: did
    integer, intent(out) :: rc
    integer :: nx, ny
    real :: lon0, lat0, dlon, dlat
    real, allocatable :: lats(:), lons(:)
    real :: lat_start, lat_end, lon_start, lon_end
    real(ESMF_KIND_R8), pointer :: lon(:,:), lat(:,:)
    integer :: i,j

    integer :: ncid, dim_xid, dim_yid
    integer :: var_lat_id, var_lon_id
    integer :: stat
    character(len=:), allocatable :: hires_file
    real, allocatable :: latitude(:,:), longitude(:,:)

    print *, "enter: TMP WRFHYDRO GRID CREATE, FUTURE REPLACEMENT"
    call ESMF_LogWrite("enter TMP WRFHYDRO GRID CREATE, FUTURE REPLACEMENT", &
         ESMF_LOGMSG_INFO)

    hires_file = "Fulldom_hires.nc"
    ! get lat/lon and nx/ny from Fulldom.nc
    print *, "WRFH: Opening high-resolution file ", hires_file
    print *, "move wrfhydro grid file to namelist: ", hires_file
    stat = nf90_open(trim(hires_file), NF90_NOWRITE, ncid)
    call check_nf(stat)


    ! Dimension IDs and sizes
    stat = nf90_inq_dimid(ncid, 'x', dim_xid)
    call check_nf(stat)
    stat = nf90_inq_dimid(ncid, 'y', dim_yid)
    call check_nf(stat)
    stat = nf90_inquire_dimension(ncid, dim_xid, len=nx)
    call check_nf(stat)
    stat = nf90_inquire_dimension(ncid, dim_yid, len=ny)
    call check_nf(stat)

    if (nx <= 0 .or. ny <= 0) then
       stat = NF90_EBADID ! why: surface a clear failure if dims are invalid
       error stop "Fulldom.nc nx <=0 or ny <= 0"
    end if

    print *, "Fulldom.nc nx, ny:", nx, ny


    ! Variable IDs
    stat = nf90_inq_varid(ncid, 'LATITUDE', var_lat_id)
    call check_nf(stat)
    stat = nf90_inq_varid(ncid, 'LONGITUDE', var_lon_id)
    call check_nf(stat)

    ! Allocate outputs [y,x]
    allocate(latitude(nx, ny), longitude(nx, ny))
    ! Read variables (library converts to real64 as needed)
    stat = nf90_get_var(ncid, var_lat_id, latitude)
    call check_nf(stat)
    stat = nf90_get_var(ncid, var_lon_id, longitude)
    call check_nf(stat)


    stat = nf90_get_var(ncid, var_lat_id, latitude)
    call check_nf(stat)
    stat = nf90_get_var(ncid, var_lon_id, longitude)
    call check_nf(stat)

    print *, "lat shape =", shape(latitude)
    print *, "lat(1:2,1:2) =", latitude(1:2,1:2)


    ! ! hard-coded from geo_em.d01.nc
    ! lats = [39.07154, 40.916, 40.916, 39.07154, 39.07148, 40.91594, 40.91594, 39.07148, 39.06692, 40.92064, 40.92064, 39.06692, 39.06685, 40.92057, 40.92057, 39.06685]
    ! lons = [-106.1862, -106.2224, -103.7776, -103.8138, -106.1921, -106.2285, -103.7715, -103.8079, -106.1861, -106.2225, -103.7775, -103.8139, -106.192, -106.2286, -103.7714, -103.808]

    ! nx=200
    ! ny=200

    ! lon_start = minval(lons)
    ! lon_end = maxval(lons)
    ! lon0 = lon_start

    ! lat_start=minval(lats)
    ! lat_end = maxval(lats)
    ! lat0 = lat_start

    ! dlon = (lon_end - lon_start) / nx
    ! dlat = (lat_end - lat_start) / ny
    ! ! dlon=0.003
    ! ! dlat=.00001
    ! print *, "Coordinates:"
    ! print *, "  lon:", lon_start, lon_end, dlon
    ! print *, "  lat:", lat_start, lat_end, dlat

    ! rc = ESMF_SUCCESS

    ! FOO ADD/REMOVE?
    ! wrfhydro_distgrid = init_distgrid(vm)
    ! print *, "create distgrid with nx, ny =", nx, ny
    ! WRFHYDRO_distgrid = ESMF_DistGridCreate( &
    !      minIndex=[1,1], maxIndex=[nx,ny], &
    !      deBlockList=deBlockList, &
    !      rc=rc)

    if (wrfhydro_distgrid_initialized .eqv. .false.) then
       error stop "Make sure wrfhydro_distgrid_initalized is initialized"
    end if


    ! Single-tile grid (easy for testing); spherical coords in degrees.
    ! and it works
    grid = ESMF_GridCreate(&
         regDecomp=[1, 1], &
         decompflag=[ESMF_DECOMP_BALANCED, ESMF_DECOMP_BALANCED], &
         maxIndex=[nx,ny], &
         rc=rc)
    call check(rc, __LINE__, file)

    ! testing method in other wrfhydro_gridcreate()
    ! grid = ESMF_GridCreate(name=hires_file, &
    !      distgrid=wrfhydro_distGrid, &
    !      coordSys=ESMF_COORDSYS_SPH_DEG, &
    !      coordTypeKind=ESMF_TYPEKIND_COORD, &
    !      ! gridEdgeLWidth=(/0,0/), gridEdgeUWidth=(/0,1/), &
    !      rc = rc)

    call check(rc, __LINE__, file)

   call ESMF_GridAddCoord(grid, staggerLoc=ESMF_STAGGERLOC_CENTER, rc=rc)
    call check(rc, __LINE__, file)

    ! Get writable pointers to the coordinate arrays and fill a regular lat/lon
    call ESMF_GridGetCoord(grid, staggerLoc=ESMF_STAGGERLOC_CENTER, &
         coordDim=1, farrayPtr=lon, rc=rc)
    call check(rc, __LINE__, file)
    call ESMF_GridGetCoord(grid, staggerLoc=ESMF_STAGGERLOC_CENTER, &
         coordDim=2, farrayPtr=lat, rc=rc)
    call check(rc, __LINE__, file)

    lat(:,:) = latitude(:,:)
    lon(:,:) = longitude(:,:)
    ! do j=1,ny
    !    lat(:,j) = lat0 + (j-1)*dlat
    ! enddo
    ! do i=1,nx
    !    lon(i,:) = lon0 + (i-1)*dlon
    ! enddo

    call ESMF_LogWrite("exit: TMP WRFHYDRO GRID CREATE, FUTURE REPLACEMENT", &
         ESMF_LOGMSG_INFO)
    print *, "exit: TMP WRFHYDRO GRID CREATE, FUTURE REPLACEMENT"
    ! stop "DON'T WANT TO CALL THIS?"
  end function wrfhydro_GridCreate_init

#undef METHOD
#define METHOD "wrfhydro_GridCreate"

  function wrfhydro_GridCreate(did,rc) result(wrfhydro_grid)
    type(ESMF_Grid) :: wrfhydro_grid
    ! ARGUMENTS
    integer, intent(in)                     :: did
    integer, intent(out)                    :: rc
    ! LOCAL VARIABLES
    integer                     :: stat
    real                        :: min_lat, max_lat, min_lon, max_lon
    real, allocatable           :: latitude(:,:), longitude(:,:)
    integer, allocatable        :: mask(:,:)
    integer                     :: lbnd(2),ubnd(2)
    real(ESMF_KIND_COORD), pointer :: coordXcenter(:,:)
    real(ESMF_KIND_COORD), pointer :: coordYcenter(:,:)
    real(ESMF_KIND_COORD), pointer :: coordXcorner(:,:)
    real(ESMF_KIND_COORD), pointer :: coordYcorner(:,:)
    integer(ESMF_KIND_I4), pointer :: gridmask(:,:)
    integer                     :: i,j, i1,j1
    character(len=16)           :: xlat_corner_name, xlon_corner_name
#ifdef DEBUG
    character(ESMF_MAXSTR)      :: logMsg
#endif
    character(:), allocatable :: grid_file

    integer :: ierr, rank


#ifdef DEBUG
    call ESMF_LogWrite(MODNAME//": entered "//METHOD, ESMF_LOGMSG_INFO)
#endif

    rc = ESMF_SUCCESS

    ! grid_file = 'WRFHYDRO_Grid_'//trim(nlst(did)%hgrid)
    ! grid_file = 'fulldom_hires_hydrofile.d01.nc'
    grid_file = 'Fulldom_hires.nc'
    print *, "move wrfhydro grid file to namelist: ", grid_file

    ! print *, "wrfhydro_distGrid =", wrfhydro_distGrid
    ! stop "pink panther"
    if (wrfhydro_distgrid_initialized .eqv. .false.) then
       error stop "Make sure wrfhydro_distgrid_initalized is initialized"
    end if
    wrfhydro_grid = ESMF_GridCreate(name=grid_file, &
         distgrid=wrfhydro_distGrid, &
         coordSys=ESMF_COORDSYS_SPH_DEG, &
         coordTypeKind=ESMF_TYPEKIND_COORD, &
         ! gridEdgeLWidth=(/0,0/), gridEdgeUWidth=(/0,1/), &
         rc = rc)
    call check(rc, __LINE__, file)

    ! CENTERS

    ! Get Local Latitude (lat)
    allocate(latitude(nx_local,ny_local),stat=stat)
    if (ESMF_LogFoundAllocError(statusToCheck=stat, &
      msg=METHOD//': Allocation of latitude memory failed.', &
      file=FILENAME, rcToReturn=rc)) return ! bail out
    call WRFHYDRO_ESMF_NetcdfReadIXJX("XLAT_M",nlst(did)%geo_static_flnm, &
      (/x_start,y_start/),latitude,rc=rc)
    call check(rc, __LINE__, file)

    ! Get Local Longitude (lon)
    allocate(longitude(nx_local,ny_local),stat=stat)
    if (ESMF_LogFoundAllocError(statusToCheck=stat, &
      msg=METHOD//': Allocation of longitude memory failed.', &
      file=FILENAME, rcToReturn=rc)) return ! bail out
    call WRFHYDRO_ESMF_NetcdfReadIXJX("XLONG_M",nlst(did)%geo_static_flnm, &
      (/x_start,y_start/),longitude,rc=rc)
    call check(rc, __LINE__, file)

! #ifdef DEBUG
    ! Print Local Lat Lon Lower Left / Upper Right Centers
    write(logMsg,"(A,4(F0.3,A))") MODNAME//": Center Coordinates = (", &
      longitude(1,1),":",longitude(nx_local,ny_local),",", &
      latitude(1,1),":",latitude(nx_local,ny_local),")"
    call ESMF_LogWrite(trim(logMsg), ESMF_LOGMSG_INFO)
! #endif

    ! call MPI_Comm_rank(MPI_COMM_WORLD, rank, ierr)
    ! print *, rank, ": nx_local, ny_local =", nx_local, ny_local
    ! print *, rank, ": max/min lat = ", maxval(latitude), minval(latitude)
    ! print *, rank, ": max/min lon = ", maxval(longitude), minval(longitude)

    ! Add Center Coordinates to Grid
    call ESMF_GridAddCoord(wrfhydro_grid, staggerLoc=ESMF_STAGGERLOC_CENTER, rc=rc)
    call check(rc, __LINE__, file)

    call ESMF_GridGetCoord(wrfhydro_grid, coordDim=1, localDE=0, &
      staggerloc=ESMF_STAGGERLOC_CENTER, &
      computationalLBound=lbnd, computationalUBound=ubnd, &
      farrayPtr=coordXcenter, rc=rc)
    call check(rc, __LINE__, file)
    call ESMF_GridGetCoord(wrfhydro_grid, coordDim=2, localDE=0, &
      staggerloc=ESMF_STAGGERLOC_CENTER, farrayPtr=coordYcenter, rc=rc)
    call check(rc, __LINE__, file)

    do j = lbnd(2),ubnd(2)
    do i = lbnd(1),ubnd(1)
      coordXcenter(i,j) = longitude(i,j)
      coordYcenter(i,j) = latitude(i,j)
    enddo
    enddo

    deallocate(latitude,longitude,stat=stat)
    if (ESMF_LogFoundDeallocError(statusToCheck=stat, &
      msg=METHOD//': Deallocation of longitude and latitude memory failed.', &
      file=FILENAME,rcToReturn=rc)) return ! bail out

    ! Get Local Mask
    allocate(mask(nx_local,ny_local),stat=stat)
    if (ESMF_LogFoundAllocError(statusToCheck=stat, &
      msg=METHOD//': Allocation of mask memory failed.', &
      file=FILENAME, rcToReturn=rc)) return ! bail out
    call WRFHYDRO_ESMF_NetcdfReadIXJX("LANDMASK",nlst(did)%geo_static_flnm, &
      (/x_start,y_start/),mask,rc=rc)
    call check(rc, __LINE__, file)

    ! Add Grid Mask
    call ESMF_GridAddItem(wrfhydro_grid, itemFlag=ESMF_GRIDITEM_MASK, &
      staggerLoc=ESMF_STAGGERLOC_CENTER, rc=rc)
    call check(rc, __LINE__, file)
    ! Get pointer to Grid Mask array
    call ESMF_GridGetItem(wrfhydro_grid, itemflag=ESMF_GRIDITEM_MASK, &
      localDE=0, &
      staggerloc=ESMF_STAGGERLOC_CENTER, &
      farrayPtr=gridmask, rc=rc)
    call check(rc, __LINE__, file)

    do j = lbnd(2),ubnd(2)
    do i = lbnd(1),ubnd(1)
      gridmask(i,j) = mask(i,j)
      gridmask(i,j) = mask(i,j)
    enddo
    enddo

    deallocate(mask,stat=stat)
    if (ESMF_LogFoundDeallocError(statusToCheck=stat, &
      msg=METHOD//': Deallocation of mask memory failed.', &
      file=FILENAME,rcToReturn=rc)) return ! bail out

    ! CORNERS
    ! The original WPS implementation used the _CORNER names
    ! but it was then changes to the _C names.  Support both
    ! options.
    if (WRFHYDRO_ESMF_NetcdfIsPresent("XLAT_CORNER",nlst(did)%geo_static_flnm) .AND. &
         WRFHYDRO_ESMF_NetcdfIsPresent("XLONG_CORNER",nlst(did)%geo_static_flnm)) then
       xlat_corner_name = "XLAT_CORNER"
       xlon_corner_name = "XLONG_CORNER"
    else if (WRFHYDRO_ESMF_NetcdfIsPresent("XLAT_C",nlst(did)%geo_static_flnm) .AND. &
         WRFHYDRO_ESMF_NetcdfIsPresent("XLONG_C",nlst(did)%geo_static_flnm)) then
       xlat_corner_name = "XLAT_C"
       xlon_corner_name = "XLONG_C"
    else
       xlat_corner_name = ""
       xlon_corner_name = ""
    endif

    if (trim(xlat_corner_name) /= "") then
      ! Get Local Latitude (lat)
      allocate(latitude(nx_local+1,ny_local+1),stat=stat)
      if (ESMF_LogFoundAllocError(statusToCheck=stat, &
        msg=METHOD//': Allocation of corner latitude memory failed.', &
        file=FILENAME, rcToReturn=rc)) return ! bail out
      call WRFHYDRO_ESMF_NetcdfReadIXJX(trim(xlat_corner_name),nlst(did)%geo_static_flnm, &
        (/x_start,y_start/),latitude,rc=rc)
      if(ESMF_STDERRORCHECK(rc)) return ! bail out

      ! Get Local Longitude (lon)
      allocate(longitude(nx_local+1,ny_local+1),stat=stat)
      if (ESMF_LogFoundAllocError(statusToCheck=stat, &
       msg=METHOD//': Allocation of corner longitude memory failed.', &
       file=FILENAME, rcToReturn=rc)) return ! bail out
      call WRFHYDRO_ESMF_NetcdfReadIXJX(trim(xlon_corner_name),nlst(did)%geo_static_flnm, &
        (/x_start,y_start/),longitude,rc=rc)
      if(ESMF_STDERRORCHECK(rc)) return ! bail out

#ifdef DEBUG
      ! Print Local Lat Lon Lower Left / Upper Right Corners
      write(logMsg,"(A,4(F0.3,A))") MODNAME//": Corner Coordinates = (", &
        longitude(1,1),":",longitude(nx_local+1,ny_local+1),",", &
        latitude(1,1),":",latitude(nx_local+1,ny_local+1),")"
      call ESMF_LogWrite(trim(logMsg), ESMF_LOGMSG_INFO)
#endif

      ! Add Corner Coordinates to Grid
      call ESMF_GridAddCoord(wrfhydro_grid, staggerLoc=ESMF_STAGGERLOC_CORNER, rc=rc)
      call check(rc, __LINE__, file)

      call ESMF_GridGetCoord(wrfhydro_grid, coordDim=1, localDE=0, &
        staggerloc=ESMF_STAGGERLOC_CORNER, &
        computationalLBound=lbnd, computationalUBound=ubnd, &
        farrayPtr=coordXcorner, rc=rc)
      call check(rc, __LINE__, file)
      call ESMF_GridGetCoord(wrfhydro_grid, coordDim=2, localDE=0, &
        staggerloc=ESMF_STAGGERLOC_CORNER, farrayPtr=coordYcorner, rc=rc)
      call check(rc, __LINE__, file)

      do j = lbnd(2),ubnd(2)
      do i = lbnd(1),ubnd(1)
        coordXcorner(i,j) = longitude(i,j)
        coordYcorner(i,j) = latitude(i,j)
      enddo
      enddo

      deallocate(latitude,longitude,stat=stat)
      if (ESMF_LogFoundDeallocError(statusToCheck=stat, &
        msg=METHOD//': Deallocation of corner longitude and latitude memory failed.', &
        file=FILENAME,rcToReturn=rc)) return ! bail out

      call add_area(wrfhydro_grid, rc=rc)
      call check(rc, __LINE__, file)

    else
#ifdef DEBUG
      ! Warning no corners in domain file
      call ESMF_LogWrite(MODNAME//": No Corner Coordinates.", ESMF_LOGMSG_WARNING)
#endif
    endif

#ifdef DEBUG
    call ESMF_LogWrite(MODNAME//": leaving "//METHOD, ESMF_LOGMSG_INFO)
#endif



  end function wrfhydro_GridCreate


  !-----------------------------------------------------------------------------

#undef METHOD
#define METHOD "add_area"

  subroutine add_area(grid,rc)
    type(ESMF_Grid), intent(inout)          :: grid
    integer, intent(out)                    :: rc

    ! Local Variables
    integer(ESMF_KIND_I4), PARAMETER :: R = 6376000 ! metres
    type(ESMF_Field)                 :: fieldArea
    type(ESMF_Array)                 :: areaArray
    integer                          :: i,j
    integer                          :: lbnd(2),ubnd(2)
    real(ESMF_KIND_R8), pointer      :: radianarea(:,:)
    real(ESMF_KIND_R8), pointer      :: gridarea(:,:)

#ifdef DEBUG
    call ESMF_LogWrite(MODNAME//": entered "//METHOD, ESMF_LOGMSG_INFO)
#endif

    rc = ESMF_SUCCESS

    fieldArea = ESMF_FieldCreate(grid=grid, typekind=ESMF_TYPEKIND_R8, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return

    call ESMF_FieldRegridGetArea(fieldArea, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return

    call ESMF_FieldGet(fieldArea, localDE=0, &
      farrayPtr=radianarea, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return

    call ESMF_GridAddItem(grid, itemFlag=ESMF_GRIDITEM_AREA, itemTypeKind=ESMF_TYPEKIND_R8, &
      staggerLoc=ESMF_STAGGERLOC_CENTER, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return

    call ESMF_GridGetItem(grid, itemflag=ESMF_GRIDITEM_AREA, localDE=0, &
      staggerloc=ESMF_STAGGERLOC_CENTER, &
      computationalLBound=lbnd, computationalUBound=ubnd, &
      farrayPtr=gridarea, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return

     do j = lbnd(2),ubnd(2)
     do i = lbnd(1),ubnd(1)
       gridarea(i,j) = radianarea(i,j) * R * R
     enddo
     enddo

#ifdef DEBUG
    call ESMF_LogWrite(MODNAME//": leaving "//METHOD, ESMF_LOGMSG_INFO)
#endif

  end subroutine

  !-----------------------------------------------------------------------------

#undef METHOD
#define METHOD "set_local_indices"

  subroutine set_local_indices(rc)
    ! ARGUMENTS
    integer, intent(out)                    :: rc

    ! LOCAL VARIABLES
    integer                     :: stat
    type(ESMF_VM)               :: currentVM
    integer                     :: localPet
    integer                     :: petCount
    type(ESMF_DELayout)         :: delayout
    integer, allocatable        :: dimExtent(:,:)
    integer, allocatable        :: iIndexList(:), jIndexList(:)
#ifdef DEBUG
    character(ESMF_MAXSTR)      :: logMsg
#endif

#ifdef DEBUG
    call ESMF_LogWrite(MODNAME//": entered "//METHOD, ESMF_LOGMSG_INFO)
#endif

    rc = ESMF_SUCCESS

    !! Get VM Info to see if this will give me the PET info I need
    call ESMF_VMGetCurrent(currentVM, rc=rc)
    if(ESMF_STDERRORCHECK(rc)) return ! bail out
    call ESMF_VMGet(currentVM, localPet=localPet, petCount=petCount, rc=rc)
    if(ESMF_STDERRORCHECK(rc)) return ! bail out

    !! Get the grid distribution for this pet
    allocate(dimExtent(2, 0:(petCount - 1)),stat=stat) ! (dimCount, deCount)
    if (ESMF_LogFoundAllocError(statusToCheck=stat, &
      msg=METHOD//': Allocation of indexCountPDe memory failed.', &
      file=FILENAME, rcToReturn=rc)) return ! bail out
    call ESMF_DistGridGet(wrfhydro_distGrid, delayout=delayout, &
      indexCountPDe=dimExtent, rc=rc)
    if(ESMF_STDERRORCHECK(rc)) return ! bail out

    allocate(iIndexList(dimExtent(1, localPet)),stat=stat)
    if (ESMF_LogFoundAllocError(statusToCheck=stat, &
      msg=METHOD//': Allocation of iIndexList memory failed.', &
      file=FILENAME, rcToReturn=rc)) return ! bail out
    call ESMF_DistGridGet(wrfhydro_distGrid, localDe=0, dim=1, &
      indexList=iIndexList, rc=rc)
    if(ESMF_STDERRORCHECK(rc)) return ! bail out

    allocate(jIndexList(dimExtent(2, localPet)),stat=stat)
    if (ESMF_LogFoundAllocError(statusToCheck=stat, &
      msg=METHOD//': Allocation of jIndexList memory failed.', &
      file=FILENAME, rcToReturn=rc)) return ! bail out
    call ESMF_DistGridGet(wrfhydro_distGrid, localDe=0, dim=2, &
      indexList=jIndexList, rc=rc)
    if(ESMF_STDERRORCHECK(rc)) return ! bail out

    x_start = minVal(iIndexList)
    x_end   = maxVal(iIndexList)
    y_start = minVal(jIndexList)
    y_end   = maxVal(jIndexList)

    nx_local = x_end - x_start + 1
    ny_local = y_end - y_start + 1

#ifdef DEBUG
    write (logMsg,"(A,6(I0,A))") MODNAME//": Local Indices = (", &
      x_start,":",x_end,",",y_start,":",y_end,") Local Size = (", &
      nx_local,"x",ny_local,")"
    call ESMF_LogWrite(trim(logMsg), ESMF_LOGMSG_INFO)
#endif

    deallocate(iIndexList,jIndexList,stat=stat)
    if (ESMF_LogFoundDeallocError(statusToCheck=stat, &
      msg=METHOD//': Deallocation of IndexList memory failed.', &
      file=FILENAME,rcToReturn=rc)) return ! bail out

    deallocate(dimExtent,stat=stat)
    if (ESMF_LogFoundDeallocError(statusToCheck=stat, &
      msg=METHOD//': Deallocation of indexCountPDeo memory failed.', &
      file=FILENAME,rcToReturn=rc)) return ! bail out

#ifdef DEBUG
    call ESMF_LogWrite(MODNAME//": leaving "//METHOD, ESMF_LOGMSG_INFO)
#endif

  end subroutine

  !-----------------------------------------------------------------------------

#undef METHOD
#define METHOD "WRFHYDRO_get_timestep"

  function WRFHYDRO_get_timestep(did,rc)
    ! RETURN VALUE
    real :: WRFHYDRO_get_timestep
    ! ARGUMENTS
    integer, intent(in)         :: did
    integer, intent(out)        :: rc

#ifdef DEBUG
    call ESMF_LogWrite(MODNAME//": entered "//METHOD, ESMF_LOGMSG_INFO)
#endif

    rc = ESMF_SUCCESS

    WRFHYDRO_get_timestep = nlst(did)%dt

#ifdef DEBUG
    call ESMF_LogWrite(MODNAME//": leaving "//METHOD, ESMF_LOGMSG_INFO)
#endif

  end function

  !-----------------------------------------------------------------------------

#undef METHOD
#define METHOD "WRFHYDRO_set_timestep"

  subroutine WRFHYDRO_set_timestep(did,dt,rc)
    ! ARGUMENTS
    integer, intent(in)           :: did
    real                          :: dt
    integer, intent(out)          :: rc

#ifdef DEBUG
    call ESMF_LogWrite(MODNAME//": entered "//METHOD, ESMF_LOGMSG_INFO)
#endif

    rc = ESMF_SUCCESS

    nlst(did)%dt = dt

#ifdef DEBUG
    call ESMF_LogWrite(MODNAME//": leaving "//METHOD, ESMF_LOGMSG_INFO)
#endif

  end subroutine

  !-----------------------------------------------------------------------------

#undef METHOD
#define METHOD "WRFHYDRO_get_hgrid"

  subroutine WRFHYDRO_get_hgrid(did,hgrid,rc)
    ! ARGUMENTS
    integer, intent(in)         :: did
    character, intent(out)      :: hgrid
    integer, intent(out)        :: rc

#ifdef DEBUG
    call ESMF_LogWrite(MODNAME//": entered "//METHOD, ESMF_LOGMSG_INFO)
#endif

    rc = ESMF_SUCCESS

    hgrid = nlst(did)%hgrid

#ifdef DEBUG
    call ESMF_LogWrite(MODNAME//": leaving "//METHOD, ESMF_LOGMSG_INFO)
#endif

  end subroutine

  !-----------------------------------------------------------------------------

#undef METHOD
#define METHOD "WRFHYDRO_get_restart"

  subroutine WRFHYDRO_get_restart(did,restart,rc)
    ! ARGUMENTS
    integer, intent(in)         :: did
    logical, intent(out)        :: restart
    integer, intent(out)        :: rc

#ifdef DEBUG
    call ESMF_LogWrite(MODNAME//": entered "//METHOD, ESMF_LOGMSG_INFO)
#endif

    rc = ESMF_SUCCESS

    if (nlst(did)%rst_typ .eq. 0) then
      restart = .FALSE.
    else
      restart = .TRUE.
    endif

#ifdef DEBUG
    call ESMF_LogWrite(MODNAME//": leaving "//METHOD, ESMF_LOGMSG_INFO)
#endif

  end subroutine

  !-----------------------------------------------------------------------------
  ! Conversion Utilities
  !-----------------------------------------------------------------------------

#undef METHOD
#define METHOD "WRFHYDRO_ClockToString"

  subroutine WRFHYDRO_ClockToString(clock, timestr, rc)
    ! ARGUMENTS
    type(ESMF_Clock)                :: clock
    integer, intent(out),optional   :: rc
    character (len=*), intent(out)  :: timestr

    ! LOCAL VARIABLES
    type(ESMF_Time)            :: currTime

#ifdef DEBUG
    call ESMF_LogWrite(MODNAME//": entered "//METHOD, ESMF_LOGMSG_INFO)
#endif

    if(present(rc)) rc = ESMF_SUCCESS  ! Initialize

    ! Get the current time from the clock
    call ESMF_ClockGet(clock=clock,currTime=currTime,rc=rc)
    if(ESMF_STDERRORCHECK(rc)) return ! bail out

    call WRFHYDRO_TimeToString(currTime,timestr,rc=rc)
    if(ESMF_STDERRORCHECK(rc)) return ! bail out

#ifdef DEBUG
    call ESMF_LogWrite(MODNAME//": leaving "//METHOD, ESMF_LOGMSG_INFO)
#endif

  end subroutine

!-----------------------------------------------------------------------------

#undef METHOD
#define METHOD "WRFHYDRO_TimeToString"

  subroutine WRFHYDRO_TimeToString(time, timestr, rc)
    ! ARGUMENTS
    type(ESMF_Time)                 :: time
    integer, intent(out),optional   :: rc
    character (len=*), intent(out)  :: timestr

    ! LOCAL VARIABLES
    character (len=256)        :: tmpstr = ''
    integer                    :: strlen

#ifdef DEBUG
    call ESMF_LogWrite(MODNAME//": entered "//METHOD, ESMF_LOGMSG_INFO)
#endif

    if(present(rc)) rc = ESMF_SUCCESS  ! Initialize

    timestr = '' ! clear string

    if (len(timestr) < 19) then
      call ESMF_LogSetError(ESMF_FAILURE, &
        msg=METHOD//": Time string is too short!", &
        file=FILENAME,rcToReturn=rc)
      return  ! bail out
    endif

    CALL ESMF_TimeGet(time,timeString=tmpstr,rc=rc )
    if(ESMF_STDERRORCHECK(rc)) return ! bail out

    strlen = min(len(timestr),len_trim(tmpstr))
    timestr(1:strlen) = tmpstr(1:strlen)
    timestr(11:11) = '_'

#ifdef DEBUG
    call ESMF_LogWrite(MODNAME//": leaving "//METHOD, ESMF_LOGMSG_INFO)
#endif

  end subroutine

  !-----------------------------------------------------------------------------

#undef METHOD
#define METHOD "WRFHYDRO_TimeIntervalGetReal"

  function WRFHYDRO_TimeIntervalGetReal(timeInterval,rc)
    ! RETURN VALUE:
    real                                :: WRFHYDRO_TimeIntervalGetReal
    ! ARGUMENTS
    type(ESMF_TimeInterval),intent(in)  :: timeInterval
    integer, intent(out), optional      :: rc

    ! LOCAL VARIABLES
    real(ESMF_KIND_R8)                  :: s_r8

#ifdef DEBUG
    call ESMF_LogWrite(MODNAME//": entered "//METHOD, ESMF_LOGMSG_INFO)
#endif

    if(present(rc)) rc = ESMF_SUCCESS

    WRFHYDRO_TimeIntervalGetReal = -9999

    call ESMF_TimeIntervalGet(timeInterval,s_r8=s_r8,rc=rc)
    if(ESMF_STDERRORCHECK(rc)) return ! bail out
    WRFHYDRO_TimeIntervalGetReal = s_r8

#ifdef DEBUG
    call ESMF_LogWrite(MODNAME//": leaving "//METHOD, ESMF_LOGMSG_INFO)
#endif

  end function

  !-----------------------------------------------------------------------------
  ! Log Utilities
  !-----------------------------------------------------------------------------

#undef METHOD
#define METHOD "WRFHYDRO_nlstLog"

  subroutine WRFHYDRO_nlstLog(did,label,rc)
    ! ARGUMENTS
    integer,intent(in)                   :: did
    character(len=*),intent(in),optional :: label
    integer,intent(out)                  :: rc

    ! LOCAL VARIABLES
    integer                     :: layerIndex
    character(len=64)           :: l_label

#ifdef DEBUG
    call ESMF_LogWrite(MODNAME//": entered "//METHOD, ESMF_LOGMSG_INFO)
#endif

    rc = ESMF_SUCCESS

    if (present(label)) then
      l_label = label
    else
      l_label = METHOD
    endif

    write (logMsg,"(A,I0)") ": Domain ID      = ",did
    call ESMF_LogWrite(trim(l_label)//logMsg,ESMF_LOGMSG_INFO)

    write (logMsg,"(A,5(I0,A))") ": Start Date     = ", &
      nlst(did)%START_YEAR,"-",nlst(did)%START_MONTH,"-", &
      nlst(did)%START_DAY,"_",nlst(did)%START_HOUR,":", &
      nlst(did)%START_MIN
    call ESMF_LogWrite(trim(l_label)//logMsg,ESMF_LOGMSG_INFO)
    write (logMsg,"(A,F0.3)") ": Timestep       = ",nlst(did)%dt
    call ESMF_LogWrite(trim(l_label)//logMsg,ESMF_LOGMSG_INFO)
    write (logMsg,"(A,F0.3)") ": Output Step    = ",nlst(did)%out_dt
    call ESMF_LogWrite(trim(l_label)//logMsg,ESMF_LOGMSG_INFO)
    write (logMsg,"(A,F0.3)") ": Restart Step   = ",nlst(did)%rst_dt
    call ESMF_LogWrite(trim(l_label)//logMsg,ESMF_LOGMSG_INFO)
    write (logMsg,"(A,F0.3)") ": Ter Routing Step   = ",nlst(did)%dtrt_ter
    call ESMF_LogWrite(trim(l_label)//logMsg,ESMF_LOGMSG_INFO)
    write (logMsg,"(A,F0.3)") ": Ch Routing Step   = ",nlst(did)%dtrt_ch
    call ESMF_LogWrite(trim(l_label)//logMsg,ESMF_LOGMSG_INFO)

    write (logMsg,"(A,I0)") ": Grid ID        = ",nlst(did)%igrid
    call ESMF_LogWrite(trim(l_label)//logMsg,ESMF_LOGMSG_INFO)
    write (logMsg,"(A,A)") ": Hydro Grid     = ",nlst(did)%hgrid
    call ESMF_LogWrite(trim(l_label)//logMsg,ESMF_LOGMSG_INFO)
    write (logMsg,"(A,A)") ": Geo Grid File  = ",nlst(did)%geo_static_flnm
    call ESMF_LogWrite(trim(l_label)//logMsg,ESMF_LOGMSG_INFO)
    write (logMsg,"(A,A)") ": Fine Grid File = ",nlst(did)%geo_finegrid_flnm
    call ESMF_LogWrite(trim(l_label)//logMsg,ESMF_LOGMSG_INFO)
    write (logMsg,"(A,A)") ": GW Basin File  = ",nlst(did)%gwbasmskfil
    call ESMF_LogWrite(trim(l_label)//logMsg,ESMF_LOGMSG_INFO)

    write (logMsg,"(A,I0)") ": Restart Type   = ",nlst(did)%rst_typ
    call ESMF_LogWrite(trim(l_label)//logMsg,ESMF_LOGMSG_INFO)
    write (logMsg,"(A,A)") ": Restart file   = ",nlst(did)%restart_file
    call ESMF_LogWrite(trim(l_label)//logMsg,ESMF_LOGMSG_INFO)
    write (logMsg,"(A,I0)") ": Coupling       = ",nlst(did)%sys_cpl
    call ESMF_LogWrite(trim(l_label)//logMsg,ESMF_LOGMSG_INFO)

    write (logMsg,"(A,I0)") ": Channel RT     = ",nlst(did)%CHANRTSWCRT
    call ESMF_LogWrite(trim(l_label)//logMsg,ESMF_LOGMSG_INFO)
    write (logMsg,"(A,I0)") ": Subsurface RT  = ",nlst(did)%SUBRTSWCRT
    call ESMF_LogWrite(trim(l_label)//logMsg,ESMF_LOGMSG_INFO)
    write (logMsg,"(A,I0)") ": Overland RT    = ",nlst(did)%OVRTSWCRT
    call ESMF_LogWrite(trim(l_label)//logMsg,ESMF_LOGMSG_INFO)
    write (logMsg,"(A,I0)") ": GW Baseflow RT = ",nlst(did)%GWBASESWCRT
    call ESMF_LogWrite(trim(l_label)//logMsg,ESMF_LOGMSG_INFO)
    write (logMsg,"(A,I0)") ": Routing Option = ",nlst(did)%RT_OPTION
    call ESMF_LogWrite(trim(l_label)//logMsg,ESMF_LOGMSG_INFO)
    write (logMsg,"(A,I0)") ": Channel Option = ",nlst(did)%channel_option
    call ESMF_LogWrite(trim(l_label)//logMsg,ESMF_LOGMSG_INFO)
    write (logMsg,"(A,I0)") ": Aggr Factor    = ",nlst(did)%AGGFACTRT
    call ESMF_LogWrite(trim(l_label)//logMsg,ESMF_LOGMSG_INFO)
    write (logMsg,"(A,I0)") ": GW Restart     = ",nlst(did)%GW_RESTART
    call ESMF_LogWrite(trim(l_label)//logMsg,ESMF_LOGMSG_INFO)
    write (logMsg,"(A,I0)") ": SWC Restart    = ",nlst(did)%RSTRT_SWC
    call ESMF_LogWrite(trim(l_label)//logMsg,ESMF_LOGMSG_INFO)

    write (logMsg,"(A,I0)") ": Soil Layers    = ",nlst(did)%nsoil
    call ESMF_LogWrite(trim(l_label)//logMsg,ESMF_LOGMSG_INFO)
    do layerIndex=1,nlst(did)%nsoil
      write (logMsg,"(A,I0,A,F0.3)") ": Soil layer depth (", &
        layerIndex,") = ",nlst(did)%ZSOIL8(layerIndex)
      call ESMF_LogWrite(trim(l_label)//logMsg,ESMF_LOGMSG_INFO)
    enddo

#ifdef DEBUG
    call ESMF_LogWrite(MODNAME//": leaving "//METHOD, ESMF_LOGMSG_INFO)
#endif

  end subroutine

  !-----------------------------------------------------------------------------

#undef METHOD
#define METHOD "WRFHYDRO_domainLog"

  subroutine WRFHYDRO_domainLog(did,label,rc)
    ! ARGUMENTS
    integer,intent(in)                   :: did
    character(len=*),intent(in),optional :: label
    integer,intent(out)                  :: rc

    ! LOCAL VARIABLES
    character(len=64)           :: l_label

#ifdef DEBUG
    call ESMF_LogWrite(MODNAME//": entered "//METHOD, ESMF_LOGMSG_INFO)
#endif

    rc = ESMF_SUCCESS

    if (present(label)) then
      l_label = label
    else
      l_label = METHOD
    endif

    write (logMsg,"(A,I0)") ": Domain ID      = ",did
    call ESMF_LogWrite(trim(l_label)//logMsg,ESMF_LOGMSG_INFO)

    write (logMsg,"(A,L1)") ": Domain Init    = ",rt_domain(did)%initialized
    call ESMF_LogWrite(trim(l_label)//logMsg,ESMF_LOGMSG_INFO)
    write (logMsg,"(A,I0)") ": Domain IX      = ",rt_domain(did)%IX
    call ESMF_LogWrite(trim(l_label)//logMsg,ESMF_LOGMSG_INFO)
    write (logMsg,"(A,I0)") ": Domain JX      = ",rt_domain(did)%JX
    call ESMF_LogWrite(trim(l_label)//logMsg,ESMF_LOGMSG_INFO)
    write (logMsg,"(A,I0)") ": Domain IXRT    = ",rt_domain(did)%IXRT
    call ESMF_LogWrite(trim(l_label)//logMsg,ESMF_LOGMSG_INFO)
    write (logMsg,"(A,I0)") ": Domain JXRT    = ",rt_domain(did)%JXRT
    call ESMF_LogWrite(trim(l_label)//logMsg,ESMF_LOGMSG_INFO)
    write (logMsg,"(A,I0)") ": Domain Forc    = ",rt_domain(did)%FORC_TYP
    call ESMF_LogWrite(trim(l_label)//logMsg,ESMF_LOGMSG_INFO)
    write (logMsg,"(A,I0)") ": Max Links      = ",rt_domain(did)%NLINKS
    call ESMF_LogWrite(trim(l_label)//logMsg,ESMF_LOGMSG_INFO)
    write (logMsg,"(A,I0)") ": Num Lakes      = ",rt_domain(did)%NLAKES
    call ESMF_LogWrite(trim(l_label)//logMsg,ESMF_LOGMSG_INFO)
    write (logMsg,"(A,I0)") ": Num Basins     = ",rt_domain(did)%numbasns
    call ESMF_LogWrite(trim(l_label)//logMsg,ESMF_LOGMSG_INFO)

#ifdef DEBUG
    call ESMF_LogWrite(MODNAME//": leaving "//METHOD, ESMF_LOGMSG_INFO)
#endif

  end subroutine


  ! TODO: remove this function
  function init_distgrid(vm) result(distGrid)
    type(ESMF_VM), intent(in) :: vm
    type(ESMF_DistGrid) :: distGrid

    integer :: localPet, rc
    call ESMF_VMGet(vm, localPet=localPet, &
      mpiCommunicator=HYDRO_COMM_WORLD, rc=rc)
!     if(ESMF_STDERRORCHECK(rc)) return ! bail out

!     call ESMF_VMBroadcast(vm, startx, count=numprocs, rootPet=IO_id, rc=rc)
!     if(ESMF_STDERRORCHECK(rc)) return ! bail out
!     call ESMF_VMBroadcast(vm, starty, count=numprocs, rootPet=IO_id, rc=rc)
!     if(ESMF_STDERRORCHECK(rc)) return ! bail out
!     call ESMF_VMBroadcast(vm, local_nx_size, count=numprocs, rootPet=IO_id, rc=rc)
!     if(ESMF_STDERRORCHECK(rc)) return ! bail out
!     call ESMF_VMBroadcast(vm, local_ny_size, count=numprocs, rootPet=IO_id, rc=rc)
!     if(ESMF_STDERRORCHECK(rc)) return ! bail out

!     allocate(deBlockList(2,2,numprocs))
!     do i = 1, numprocs
!       deBlockList(:,1,i) = (/startx(i),starty(i)/)
!       deBlockList(:,2,i) = (/startx(i)+local_nx_size(i)-1, &
!                              starty(i)+local_ny_size(i)-1/)
! !      write (logMsg,"(A,I0,A,4(I0,A))") MODNAME//": deBlockList ", i, " = (", &
! !        deBlockList(1,1,i),":",deBlockList(1,2,i),",", &
! !        deBlockList(2,1,i),":",deBlockList(2,2,i),")"
! !      call ESMF_LogWrite(trim(logMsg), ESMF_LOGMSG_INFO)
!     enddo

! !    allocate(connectionList(1),stat=stat)
! !    if (ESMF_LogFoundAllocError(statusToCheck=stat, &
! !      msg=METHOD//': Allocation of connection list memory failed.', &
! !      file=FILENAME, rcToReturn=rc)) return ! bail out
! !    call ESMF_DistGridConnectionSet(connectionList(1), tileIndexA=1, &
! !      tileIndexB=1, positionVector=(/nx_global(1), 0/), rc=rc)
! !    if (ESMF_STDERRORCHECK(rc)) return  ! bail out


!     ! Create DistGrid based on WRFHDYRO Config NX,NY
!     WRFHYDRO_distgrid = ESMF_DistGridCreate( &
!       minIndex=(/1,1/), maxIndex=(/nx_global(1),ny_global(1)/), &
! !     indexflag = ESMF_INDEX_DELOCAL, &
!      deBlockList=deBlockList, &
! !     deLabelList=deLabelList, &
! !     delayout=delayout, &
! !     connectionList=connectionList, &
!       rc=rc)
!     if(ESMF_STDERRORCHECK(rc)) return ! bail out

!     deallocate(deBlockList)

! !   deallocate(connectionList,stat=stat)
! !   if (ESMF_LogFoundDeallocError(statusToCheck=stat, &
! !     msg=METHOD//': Deallocation of connection list memory failed.', &
! !     file=FILENAME,rcToReturn=rc)) return ! bail out

!     ! Get the Local Decomp Incides
!     call set_local_indices(rc)
!     if(ESMF_STDERRORCHECK(rc)) return ! bail out
  end function init_distgrid

  subroutine handle_rank_string(rank, rank_s)
    use mpi
    integer, intent(inout) :: rank
    character(len=3), intent(inout) :: rank_s
    integer :: ierr
    call MPI_Comm_rank(MPI_COMM_WORLD, rank, ierr)
    ! print *, "rank =", rank
    ! write(rank_s, '(I0)') rank
    if (rank == 0) rank_s = "0"
    if (rank == 1) rank_s = "1"
  end subroutine handle_rank_string


!-----------------------------------------------------------------------------

end module
