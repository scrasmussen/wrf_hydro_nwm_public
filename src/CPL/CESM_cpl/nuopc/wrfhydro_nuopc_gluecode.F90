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
  use wrfhydro_esmf_extensions
  use wrfhydro_nuopc_macros
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

  implicit none

  private

  public :: wrfhydro_nuopc_ini
  public :: wrfhydro_nuopc_run
  public :: wrfhydro_nuopc_fin
  public :: wrfhydro_gridcreate
  public :: wrfhydro_get_timestep
  public :: wrfhydro_set_timestep
  public :: wrfhydro_get_hgrid
  public :: wrfhydro_get_restart
  public :: wrfhydro_create_geogrid_file

  character(len=*), parameter :: filename = "wrfhydro_nuopc_cap.F90"
  ! land-surface grid file
  character(len=*), parameter :: geogrid_file = 'geo_em.d01.nc'
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

  type(ESMF_DistGrid)   :: WRFHYDRO_DistGrid ! One DistGrid created with ConfigFile dimensions
  character(len=512)  :: logMsg

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
    nlst(did)%geo_static_flnm = "geo_em.d01.nc"
    nlst(did)%geo_finegrid_flnm = "fulldom_hires_hydrofile.d01.nc"
    nlst(did)%sys_cpl = 2
    nlst(did)%IGRID = did
    write(nlst(did)%hgrid,'(I1)') did

    if(nlst(did)%dt .le. 0) then
      call ESMF_LogSetError(ESMF_FAILURE, &
        msg=METHOD//": Timestep less than 1 is not supported!", &
        file=FILENAME,rcToReturn=rc)
      return  ! bail out
    endif

!    ! Read information from hydro.namelist config file
     call init_namelist_rt_field(did)

#if DEBUG
    call WRFHYDRO_nlstLog(did,MODNAME,rc=rc)
    if(ESMF_STDERRORCHECK(rc)) return ! bail out
#endif

    if(nlst(did)%nsoil .gt. 4) then
      call ESMF_LogSetError(ESMF_FAILURE, &
        msg=METHOD//": Maximum soil levels supported is 4.", &
        file=FILENAME,rcToReturn=rc)
      return  ! bail out
    endif

    call get_file_dimension(fileName=nlst(did)%geo_static_flnm,&
      ix=nx_global(1),jx=ny_global(1))
    call MPP_LAND_INIT(nx_global(1),ny_global(1))

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
    WRFHYDRO_distgrid = ESMF_DistGridCreate( &
      minIndex=(/1,1/), maxIndex=(/nx_global(1),ny_global(1)/), &
!     indexflag = ESMF_INDEX_DELOCAL, &
     deBlockList=deBlockList, &
!     deLabelList=deLabelList, &
!     delayout=delayout, &
!     connectionList=connectionList, &
      rc=rc)
    if(ESMF_STDERRORCHECK(rc)) return ! bail out

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
      else
        ! CMEPS supplies the time-averaged infiltration-excess rate in
        ! kg m-2 s-1. For liquid water this is numerically mm s-1. WRF-Hydro
        ! expects the accumulated depth in mm for the current coupling step.
        rt_domain(did)%infxsrt = rt_domain(did)%infxsrt * nlst(did)%dt
      endif
    endif

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

  end subroutine

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

#undef METHOD
#define METHOD "WRFHYDRO_GridCreate"

  function WRFHYDRO_GridCreate(did,rc)
    ! RETURN VALUE
    type(ESMF_Grid) :: WRFHYDRO_GridCreate
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


#ifdef DEBUG
    call ESMF_LogWrite(MODNAME//": entered "//METHOD, ESMF_LOGMSG_INFO)
#endif

    rc = ESMF_SUCCESS

    WRFHYDRO_GridCreate = ESMF_GridCreate(name='WRFHYDRO_Grid_'//trim(nlst(did)%hgrid), &
      distgrid=WRFHYDRO_DistGrid, coordSys = ESMF_COORDSYS_SPH_DEG, &
      coordTypeKind=ESMF_TYPEKIND_COORD, &
!      gridEdgeLWidth=(/0,0/), gridEdgeUWidth=(/0,1/), &
      rc = rc)
    if(ESMF_STDERRORCHECK(rc)) return ! bail out

    ! CENTERS

    ! Get Local Latitude (lat)
    allocate(latitude(nx_local,ny_local),stat=stat)
    if (ESMF_LogFoundAllocError(statusToCheck=stat, &
      msg=METHOD//': Allocation of latitude memory failed.', &
      file=FILENAME, rcToReturn=rc)) return ! bail out
    call WRFHYDRO_ESMF_NetcdfReadIXJX("XLAT_M",nlst(did)%geo_static_flnm, &
      (/x_start,y_start/),latitude,rc=rc)
    if(ESMF_STDERRORCHECK(rc)) return ! bail out

    ! Get Local Longitude (lon)
    allocate(longitude(nx_local,ny_local),stat=stat)
    if (ESMF_LogFoundAllocError(statusToCheck=stat, &
      msg=METHOD//': Allocation of longitude memory failed.', &
      file=FILENAME, rcToReturn=rc)) return ! bail out
    call WRFHYDRO_ESMF_NetcdfReadIXJX("XLONG_M",nlst(did)%geo_static_flnm, &
      (/x_start,y_start/),longitude,rc=rc)
    if(ESMF_STDERRORCHECK(rc)) return ! bail out

#ifdef DEBUG
    ! Print Local Lat Lon Lower Left / Upper Right Centers
    write(logMsg,"(A,4(F0.3,A))") MODNAME//": Center Coordinates = (", &
      longitude(1,1),":",longitude(nx_local,ny_local),",", &
      latitude(1,1),":",latitude(nx_local,ny_local),")"
    call ESMF_LogWrite(trim(logMsg), ESMF_LOGMSG_INFO)
#endif

    ! Add Center Coordinates to Grid
    call ESMF_GridAddCoord(WRFHYDRO_GridCreate, staggerLoc=ESMF_STAGGERLOC_CENTER, rc=rc)
    if(ESMF_STDERRORCHECK(rc)) return ! bail out

    call ESMF_GridGetCoord(WRFHYDRO_GridCreate, coordDim=1, localDE=0, &
      staggerloc=ESMF_STAGGERLOC_CENTER, &
      computationalLBound=lbnd, computationalUBound=ubnd, &
      farrayPtr=coordXcenter, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return
    call ESMF_GridGetCoord(WRFHYDRO_GridCreate, coordDim=2, localDE=0, &
      staggerloc=ESMF_STAGGERLOC_CENTER, farrayPtr=coordYcenter, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return

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
    if(ESMF_STDERRORCHECK(rc)) return ! bail out

    ! Add Grid Mask
    call ESMF_GridAddItem(WRFHYDRO_GridCreate, itemFlag=ESMF_GRIDITEM_MASK, &
      staggerLoc=ESMF_STAGGERLOC_CENTER, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out
    ! Get pointer to Grid Mask array
    call ESMF_GridGetItem(WRFHYDRO_GridCreate, itemflag=ESMF_GRIDITEM_MASK, &
      localDE=0, &
      staggerloc=ESMF_STAGGERLOC_CENTER, &
      farrayPtr=gridmask, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out

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
      call ESMF_GridAddCoord(WRFHYDRO_GridCreate, staggerLoc=ESMF_STAGGERLOC_CORNER, rc=rc)
      if(ESMF_STDERRORCHECK(rc)) return ! bail out

      call ESMF_GridGetCoord(WRFHYDRO_GridCreate, coordDim=1, localDE=0, &
        staggerloc=ESMF_STAGGERLOC_CORNER, &
        computationalLBound=lbnd, computationalUBound=ubnd, &
        farrayPtr=coordXcorner, rc=rc)
      if (ESMF_STDERRORCHECK(rc)) return
      call ESMF_GridGetCoord(WRFHYDRO_GridCreate, coordDim=2, localDE=0, &
        staggerloc=ESMF_STAGGERLOC_CORNER, farrayPtr=coordYcorner, rc=rc)
      if (ESMF_STDERRORCHECK(rc)) return

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

      call add_area(WRFHYDRO_GridCreate, rc=rc)
      if (ESMF_STDERRORCHECK(rc)) return

    else
#ifdef DEBUG
      ! Warning no corners in domain file
      call ESMF_LogWrite(MODNAME//": No Corner Coordinates.", ESMF_LOGMSG_WARNING)
#endif
    endif

#ifdef DEBUG
    call ESMF_LogWrite(MODNAME//": leaving "//METHOD, ESMF_LOGMSG_INFO)
#endif

  end function

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
    call ESMF_DistGridGet(WRFHYDRO_DistGrid, delayout=delayout, &
      indexCountPDe=dimExtent, rc=rc)
    if(ESMF_STDERRORCHECK(rc)) return ! bail out

    allocate(iIndexList(dimExtent(1, localPet)),stat=stat)
    if (ESMF_LogFoundAllocError(statusToCheck=stat, &
      msg=METHOD//': Allocation of iIndexList memory failed.', &
      file=FILENAME, rcToReturn=rc)) return ! bail out
    call ESMF_DistGridGet(WRFHYDRO_DistGrid, localDe=0, dim=1, &
      indexList=iIndexList, rc=rc)
    if(ESMF_STDERRORCHECK(rc)) return ! bail out

    allocate(jIndexList(dimExtent(2, localPet)),stat=stat)
    if (ESMF_LogFoundAllocError(statusToCheck=stat, &
      msg=METHOD//': Allocation of jIndexList memory failed.', &
      file=FILENAME, rcToReturn=rc)) return ! bail out
    call ESMF_DistGridGet(WRFHYDRO_DistGrid, localDe=0, dim=2, &
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

  !-----------------------------------------------------------------------------
  function esmf_stderrorcheck(rc) result(res)
    integer, intent(in) :: rc
    logical :: res
    ! call ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, file=filename)
    if (rc == 0) then
       res = .false.
    else
       res = .true.
    end if
  end function esmf_stderrorcheck


  subroutine wrfhydro_create_geogrid_file()
    use netcdf

    character(len=*), parameter :: method = 'wrfhydro_create_geogrid_file'
    character(len=*), parameter :: default_geogrid = './DOMAIN/geo_em.d01.nc'
    character(len=*), parameter :: default_metadata = &
      './DOMAIN/GEOGRID_LDASOUT_Spatial_Metadata.nc'
    character(len=*), parameter :: default_fulldom = './DOMAIN/Fulldom_hires.nc'

    character(len=2048) :: fsurdat_file, mesh_file
    character(len=2048) :: geogrid_path, metadata_file, fulldom_file
    logical :: found, exists

    integer :: ncid, crs_varid
    integer :: nx, ny, nx_src, ny_src
    integer :: natpft_count, numurbl_count, nlevsoi_count
    integer :: i, j, rc

    real(ESMF_KIND_R8) :: standard_parallel(2)
    real(ESMF_KIND_R8) :: central_lon, origin_lat
    real(ESMF_KIND_R8) :: false_easting, false_northing, earth_radius
    real(ESMF_KIND_R8), allocatable :: x(:), y(:), x_corner(:), y_corner(:)
    real(ESMF_KIND_R8), allocatable :: latitude(:,:), longitude(:,:)
    real(ESMF_KIND_R8), allocatable :: latitude_corner(:,:), longitude_corner(:,:)
    real(ESMF_KIND_R8), allocatable :: hgt(:,:), sand(:,:), clay(:,:)
    real(ESMF_KIND_R8), allocatable :: sand_src(:,:,:), clay_src(:,:,:)
    real(ESMF_KIND_R8), allocatable :: landfrac_src(:,:), natveg_src(:,:)
    real(ESMF_KIND_R8), allocatable :: crop_src(:,:), wetland_src(:,:)
    real(ESMF_KIND_R8), allocatable :: lake_src(:,:), glacier_src(:,:)
    real(ESMF_KIND_R8), allocatable :: pft_src(:,:,:), urban_src(:,:,:)
    real(ESMF_KIND_R8), allocatable :: work_src(:,:), work_dst(:,:), max_fraction(:,:)
    integer, allocatable :: lu_index(:,:), soil_category(:,:), landmask(:,:)

    type(ESMF_Mesh) :: source_mesh
    type(ESMF_Grid) :: target_grid
    type(ESMF_Field) :: source_field, target_field
    type(ESMF_RouteHandle) :: bilinear_handle
    real(ESMF_KIND_R8), pointer :: source_ptr(:) => null()
    real(ESMF_KIND_R8), pointer :: target_ptr(:,:) => null()
    real(ESMF_KIND_R8), pointer :: grid_lon(:,:) => null()
    real(ESMF_KIND_R8), pointer :: grid_lat(:,:) => null()
    real(ESMF_KIND_R8), pointer :: grid_lon_corner(:,:) => null()
    real(ESMF_KIND_R8), pointer :: grid_lat_corner(:,:) => null()

    geogrid_path = default_geogrid
    metadata_file = default_metadata
    fulldom_file = default_fulldom

    call read_case_value('hydro.namelist', 'geo_static_flnm', geogrid_path, found)
    if (.not. found) geogrid_path = default_geogrid
    call read_case_value('hydro.namelist', 'land_spatial_meta_flnm', metadata_file, found)
    if (.not. found) metadata_file = default_metadata
    call read_case_value('hydro.namelist', 'geo_finegrid_flnm', fulldom_file, found)
    if (.not. found) fulldom_file = default_fulldom

    inquire(file=trim(geogrid_path), exist=exists)
    if (exists) then
      call ESMF_LogWrite(method//': using existing '//trim(geogrid_path), &
        ESMF_LOGMSG_INFO)
      return
    end if

    call read_case_value('lnd_in', 'fsurdat', fsurdat_file, found)
    if (.not. found) call fatal('Could not find fsurdat in lnd_in')
    call read_case_value('nuopc.runconfig', 'mesh_lnd', mesh_file, found)
    if (.not. found) call fatal('Could not find mesh_lnd in nuopc.runconfig')

    call require_file(trim(fsurdat_file))
    call require_file(trim(mesh_file))
    call require_file(trim(metadata_file))
    call require_file(trim(fulldom_file))

    call ESMF_LogWrite(method//': creating '//trim(geogrid_path), ESMF_LOGMSG_INFO)
    call ESMF_LogWrite(method//': CTSM surface data: '//trim(fsurdat_file), &
      ESMF_LOGMSG_INFO)
    call ESMF_LogWrite(method//': CTSM mesh: '//trim(mesh_file), ESMF_LOGMSG_INFO)

    ! The routing-stack metadata defines the destination grid.  Its x and y
    ! coordinates are cell centers in a spherical Lambert conformal projection.
    call check_nf(nf90_open(trim(metadata_file), NF90_NOWRITE, ncid), &
      'opening land spatial metadata')
    call get_dimension(ncid, 'x', nx)
    call get_dimension(ncid, 'y', ny)
    allocate(x(nx), y(ny), x_corner(nx+1), y_corner(ny+1))
    call read_variable_1d(ncid, 'x', x)
    call read_variable_1d(ncid, 'y', y)
    call check_nf(nf90_inq_varid(ncid, 'crs', crs_varid), 'finding metadata crs')
    call check_nf(nf90_get_att(ncid, crs_varid, 'standard_parallel', &
      standard_parallel), 'reading standard_parallel')
    call check_nf(nf90_get_att(ncid, crs_varid, &
      'longitude_of_central_meridian', central_lon), 'reading central meridian')
    call check_nf(nf90_get_att(ncid, crs_varid, &
      'latitude_of_projection_origin', origin_lat), 'reading projection origin')
    call check_nf(nf90_get_att(ncid, crs_varid, 'false_easting', false_easting), &
      'reading false_easting')
    call check_nf(nf90_get_att(ncid, crs_varid, 'false_northing', false_northing), &
      'reading false_northing')
    call check_nf(nf90_get_att(ncid, crs_varid, 'earth_radius', earth_radius), &
      'reading earth_radius')
    call check_nf(nf90_close(ncid), 'closing land spatial metadata')

    call centers_to_corners(x, x_corner)
    call centers_to_corners(y, y_corner)
    allocate(latitude(nx,ny), longitude(nx,ny))
    allocate(latitude_corner(nx+1,ny+1), longitude_corner(nx+1,ny+1))
    do j = 1, ny
      do i = 1, nx
        call inverse_lambert(x(i), y(j), standard_parallel, central_lon, &
          origin_lat, false_easting, false_northing, earth_radius, &
          latitude(i,j), longitude(i,j))
      end do
    end do
    do j = 1, ny+1
      do i = 1, nx+1
        call inverse_lambert(x_corner(i), y_corner(j), standard_parallel, &
          central_lon, origin_lat, false_easting, false_northing, earth_radius, &
          latitude_corner(i,j), longitude_corner(i,j))
      end do
    end do

    ! Read CTSM surface fields.  The ESMF mesh elements and the flattened
    ! (lsmlon,lsmlat) surface arrays have the same ordering.
    call check_nf(nf90_open(trim(fsurdat_file), NF90_NOWRITE, ncid), &
      'opening CTSM surface dataset')
    call get_dimension(ncid, 'lsmlon', nx_src)
    call get_dimension(ncid, 'lsmlat', ny_src)
    call get_dimension(ncid, 'natpft', natpft_count)
    call get_dimension(ncid, 'numurbl', numurbl_count)
    call get_dimension(ncid, 'nlevsoi', nlevsoi_count)
    if (natpft_count < 15) call fatal('CTSM surface dataset has fewer than 15 natural PFTs')
    if (nlevsoi_count < 1) call fatal('CTSM surface dataset has no soil levels')

    allocate(landfrac_src(nx_src,ny_src), natveg_src(nx_src,ny_src))
    allocate(crop_src(nx_src,ny_src), wetland_src(nx_src,ny_src))
    allocate(lake_src(nx_src,ny_src), glacier_src(nx_src,ny_src))
    allocate(pft_src(nx_src,ny_src,natpft_count))
    allocate(urban_src(nx_src,ny_src,numurbl_count))
    allocate(sand_src(nx_src,ny_src,nlevsoi_count))
    allocate(clay_src(nx_src,ny_src,nlevsoi_count))
    call read_variable_2d(ncid, 'LANDFRAC_MKSURFDATA', landfrac_src)
    call read_variable_2d(ncid, 'PCT_NATVEG', natveg_src)
    call read_variable_2d(ncid, 'PCT_CROP', crop_src)
    call read_variable_2d(ncid, 'PCT_WETLAND', wetland_src)
    call read_variable_2d(ncid, 'PCT_LAKE', lake_src)
    call read_variable_2d(ncid, 'PCT_GLACIER', glacier_src)
    call read_variable_3d(ncid, 'PCT_NAT_PFT', pft_src)
    call read_variable_3d(ncid, 'PCT_URBAN', urban_src)
    call read_variable_3d(ncid, 'PCT_SAND', sand_src)
    call read_variable_3d(ncid, 'PCT_CLAY', clay_src)
    call check_nf(nf90_close(ncid), 'closing CTSM surface dataset')

    ! Build a serial ESMF destination grid and a reusable bilinear route from
    ! the CTSM land mesh.  This routine is called during the one-PET setup run.
    source_mesh = ESMF_MeshCreate(filename=trim(mesh_file), &
      fileformat=ESMF_FILEFORMAT_ESMFMESH, rc=rc)
    call check_esmf(rc, 'creating CTSM source mesh')
    target_grid = ESMF_GridCreate(maxIndex=(/nx,ny/), regDecomp=(/1,1/), &
      decompflag=(/ESMF_DECOMP_BALANCED,ESMF_DECOMP_BALANCED/), &
      coordSys=ESMF_COORDSYS_SPH_DEG, coordTypeKind=ESMF_TYPEKIND_R8, rc=rc)
    call check_esmf(rc, 'creating geogrid destination grid')
    call ESMF_GridAddCoord(target_grid, staggerLoc=ESMF_STAGGERLOC_CENTER, rc=rc)
    call check_esmf(rc, 'adding destination center coordinates')
    call ESMF_GridGetCoord(target_grid, coordDim=1, &
      staggerLoc=ESMF_STAGGERLOC_CENTER, farrayPtr=grid_lon, rc=rc)
    call check_esmf(rc, 'getting destination center longitudes')
    call ESMF_GridGetCoord(target_grid, coordDim=2, &
      staggerLoc=ESMF_STAGGERLOC_CENTER, farrayPtr=grid_lat, rc=rc)
    call check_esmf(rc, 'getting destination center latitudes')
    grid_lon = longitude
    grid_lat = latitude
    call ESMF_GridAddCoord(target_grid, staggerLoc=ESMF_STAGGERLOC_CORNER, rc=rc)
    call check_esmf(rc, 'adding destination corner coordinates')
    call ESMF_GridGetCoord(target_grid, coordDim=1, &
      staggerLoc=ESMF_STAGGERLOC_CORNER, farrayPtr=grid_lon_corner, rc=rc)
    call check_esmf(rc, 'getting destination corner longitudes')
    call ESMF_GridGetCoord(target_grid, coordDim=2, &
      staggerLoc=ESMF_STAGGERLOC_CORNER, farrayPtr=grid_lat_corner, rc=rc)
    call check_esmf(rc, 'getting destination corner latitudes')
    grid_lon_corner = longitude_corner
    grid_lat_corner = latitude_corner

    source_field = ESMF_FieldCreate(mesh=source_mesh, &
      typekind=ESMF_TYPEKIND_R8, meshloc=ESMF_MESHLOC_ELEMENT, &
      name='ctsm_geogrid_source', rc=rc)
    call check_esmf(rc, 'creating CTSM source field')
    target_field = ESMF_FieldCreate(grid=target_grid, &
      typekind=ESMF_TYPEKIND_R8, staggerloc=ESMF_STAGGERLOC_CENTER, &
      name='wrfhydro_geogrid_target', rc=rc)
    call check_esmf(rc, 'creating geogrid destination field')
    call ESMF_FieldGet(source_field, farrayPtr=source_ptr, rc=rc)
    call check_esmf(rc, 'getting CTSM source field storage')
    call ESMF_FieldGet(target_field, farrayPtr=target_ptr, rc=rc)
    call check_esmf(rc, 'getting geogrid destination field storage')
    if (size(source_ptr) /= nx_src*ny_src) then
      call fatal('CTSM mesh element count does not match fsurdat lsmlon*lsmlat')
    end if
    if (size(target_ptr,1) /= nx .or. size(target_ptr,2) /= ny) then
      call fatal('ESMF target field shape does not match routing-stack metadata')
    end if
    call ESMF_FieldRegridStore(source_field, target_field, &
      routehandle=bilinear_handle, regridmethod=ESMF_REGRIDMETHOD_BILINEAR, &
      lineType=ESMF_LINETYPE_GREAT_CIRCLE, &
      unmappedaction=ESMF_UNMAPPEDACTION_IGNORE, rc=rc)
    call check_esmf(rc, 'building CTSM-to-geogrid bilinear weights')

    allocate(work_src(nx_src,ny_src), work_dst(nx,ny), max_fraction(nx,ny))
    allocate(sand(nx,ny), clay(nx,ny), lu_index(nx,ny))
    allocate(soil_category(nx,ny), landmask(nx,ny), hgt(nx,ny))

    call apply_regrid(sand_src(:,:,1), sand)
    call apply_regrid(clay_src(:,:,1), clay)

    ! Convert CTSM landunit/PFT fractions to dominant USGS categories.  CTSM
    ! natural-PFT indices 0:14 are the first 15 entries in PCT_NAT_PFT.
    max_fraction = -huge(1.0_ESMF_KIND_R8)
    lu_index = 19
    work_src = landfrac_src * sum(urban_src, dim=3)
    call consider_usgs_category(1, work_src)
    work_src = landfrac_src * crop_src
    call consider_usgs_category(2, work_src)
    work_src = landfrac_src * natveg_src * sum(pft_src(:,:,13:15), dim=3) / 100.0_ESMF_KIND_R8
    call consider_usgs_category(7, work_src)
    work_src = landfrac_src * natveg_src * sum(pft_src(:,:,10:12), dim=3) / 100.0_ESMF_KIND_R8
    call consider_usgs_category(8, work_src)
    work_src = landfrac_src * natveg_src * sum(pft_src(:,:,7:9), dim=3) / 100.0_ESMF_KIND_R8
    call consider_usgs_category(11, work_src)
    work_src = landfrac_src * natveg_src * pft_src(:,:,4) / 100.0_ESMF_KIND_R8
    call consider_usgs_category(12, work_src)
    work_src = landfrac_src * natveg_src * sum(pft_src(:,:,5:6), dim=3) / 100.0_ESMF_KIND_R8
    call consider_usgs_category(13, work_src)
    work_src = landfrac_src * natveg_src * sum(pft_src(:,:,2:3), dim=3) / 100.0_ESMF_KIND_R8
    call consider_usgs_category(14, work_src)
    work_src = landfrac_src * wetland_src
    call consider_usgs_category(17, work_src)
    work_src = landfrac_src * natveg_src * pft_src(:,:,1) / 100.0_ESMF_KIND_R8
    call consider_usgs_category(19, work_src)
    work_src = landfrac_src * glacier_src
    call consider_usgs_category(24, work_src)
    work_src = 100.0_ESMF_KIND_R8 * max(0.0_ESMF_KIND_R8, &
      1.0_ESMF_KIND_R8-landfrac_src) + landfrac_src * lake_src
    call consider_usgs_category(16, work_src)

    landmask = 1
    where (lu_index == 16) landmask = 0
    do j = 1, ny
      do i = 1, nx
        if (landmask(i,j) == 0) then
          soil_category(i,j) = 14
        else
          soil_category(i,j) = usda_soil_category(sand(i,j), clay(i,j))
        end if
      end do
    end do

    ! Fulldom_hires is projection-aligned with the target geogrid.  Block
    ! averaging preserves the available 100-m terrain information at 1 km.
    call aggregate_fulldom_topography(trim(fulldom_file), x, y, hgt)

    call write_geogrid(trim(geogrid_path))

    call ESMF_RouteHandleDestroy(bilinear_handle, rc=rc)
    call check_esmf(rc, 'destroying geogrid route handle')
    call ESMF_FieldDestroy(source_field, rc=rc)
    call check_esmf(rc, 'destroying CTSM source field')
    call ESMF_FieldDestroy(target_field, rc=rc)
    call check_esmf(rc, 'destroying geogrid destination field')
    call ESMF_MeshDestroy(source_mesh, rc=rc)
    call check_esmf(rc, 'destroying CTSM source mesh')
    call ESMF_GridDestroy(target_grid, rc=rc)
    call check_esmf(rc, 'destroying geogrid destination grid')

    call ESMF_LogWrite(method//': created '//trim(geogrid_path), ESMF_LOGMSG_INFO)

  contains

    subroutine fatal(message)
      character(len=*), intent(in) :: message
      call ESMF_LogWrite(method//': '//trim(message), ESMF_LOGMSG_ERROR)
      error stop trim(method)//': '//trim(message)
    end subroutine fatal

    subroutine check_nf(status, context)
      integer, intent(in) :: status
      character(len=*), intent(in) :: context
      if (status /= NF90_NOERR) then
        call fatal(trim(context)//': '//trim(nf90_strerror(status)))
      end if
    end subroutine check_nf

    subroutine check_esmf(status, context)
      integer, intent(in) :: status
      character(len=*), intent(in) :: context
      character(len=32) :: status_string
      if (status /= ESMF_SUCCESS) then
        write(status_string,'(I0)') status
        call fatal(trim(context)//' (ESMF rc='//trim(status_string)//')')
      end if
    end subroutine check_esmf

    subroutine require_file(path)
      character(len=*), intent(in) :: path
      logical :: file_exists
      inquire(file=trim(path), exist=file_exists)
      if (.not. file_exists) call fatal('Required file does not exist: '//trim(path))
    end subroutine require_file

    subroutine read_case_value(path, key, value, was_found)
      character(len=*), intent(in) :: path, key
      character(len=*), intent(out) :: value
      logical, intent(out) :: was_found
      character(len=4096) :: line, lower_line, rhs
      integer :: unit_number, ios, key_position, equal_position
      integer :: first_quote, second_quote, stop_position

      value = ''
      was_found = .false.
      open(newunit=unit_number, file=trim(path), status='old', action='read', iostat=ios)
      if (ios /= 0) return
      do
        read(unit_number,'(A)',iostat=ios) line
        if (ios /= 0) exit
        lower_line = lower_string(line)
        key_position = index(lower_line, lower_string(trim(key)))
        if (key_position == 0) cycle
        equal_position = index(line(key_position:), '=')
        if (equal_position == 0) cycle
        rhs = adjustl(line(key_position+equal_position:))
        first_quote = index(rhs, "'")
        if (first_quote == 0) first_quote = index(rhs, '"')
        if (first_quote > 0) then
          second_quote = index(rhs(first_quote+1:), rhs(first_quote:first_quote))
          if (second_quote > 0) then
            value = rhs(first_quote+1:first_quote+second_quote-1)
            was_found = .true.
            exit
          end if
        else
          stop_position = scan(rhs, ' ,!')
          if (stop_position == 0) stop_position = len_trim(rhs)+1
          value = rhs(1:stop_position-1)
          was_found = len_trim(value) > 0
          if (was_found) exit
        end if
      end do
      close(unit_number)
    end subroutine read_case_value

    pure function lower_string(input) result(output)
      character(len=*), intent(in) :: input
      character(len=len(input)) :: output
      integer :: k, code
      do k = 1, len(input)
        code = iachar(input(k:k))
        if (code >= iachar('A') .and. code <= iachar('Z')) then
          output(k:k) = achar(code + iachar('a') - iachar('A'))
        else
          output(k:k) = input(k:k)
        end if
      end do
    end function lower_string

    subroutine get_dimension(file_id, name, length)
      integer, intent(in) :: file_id
      character(len=*), intent(in) :: name
      integer, intent(out) :: length
      integer :: dimension_id
      call check_nf(nf90_inq_dimid(file_id, trim(name), dimension_id), &
        'finding dimension '//trim(name))
      call check_nf(nf90_inquire_dimension(file_id, dimension_id, len=length), &
        'reading dimension '//trim(name))
    end subroutine get_dimension

    subroutine read_variable_1d(file_id, name, values)
      integer, intent(in) :: file_id
      character(len=*), intent(in) :: name
      real(ESMF_KIND_R8), intent(out) :: values(:)
      integer :: variable_id
      call check_nf(nf90_inq_varid(file_id, trim(name), variable_id), &
        'finding variable '//trim(name))
      call check_nf(nf90_get_var(file_id, variable_id, values), &
        'reading variable '//trim(name))
    end subroutine read_variable_1d

    subroutine read_variable_2d(file_id, name, values)
      integer, intent(in) :: file_id
      character(len=*), intent(in) :: name
      real(ESMF_KIND_R8), intent(out) :: values(:,:)
      integer :: variable_id
      call check_nf(nf90_inq_varid(file_id, trim(name), variable_id), &
        'finding variable '//trim(name))
      call check_nf(nf90_get_var(file_id, variable_id, values), &
        'reading variable '//trim(name))
    end subroutine read_variable_2d

    subroutine read_variable_3d(file_id, name, values)
      integer, intent(in) :: file_id
      character(len=*), intent(in) :: name
      real(ESMF_KIND_R8), intent(out) :: values(:,:,:)
      integer :: variable_id
      call check_nf(nf90_inq_varid(file_id, trim(name), variable_id), &
        'finding variable '//trim(name))
      call check_nf(nf90_get_var(file_id, variable_id, values), &
        'reading variable '//trim(name))
    end subroutine read_variable_3d

    subroutine centers_to_corners(center, corner)
      real(ESMF_KIND_R8), intent(in) :: center(:)
      real(ESMF_KIND_R8), intent(out) :: corner(:)
      integer :: k, count
      count = size(center)
      if (count < 2 .or. size(corner) /= count+1) then
        call fatal('Cannot derive grid corners from center coordinate array')
      end if
      corner(1) = center(1) - 0.5_ESMF_KIND_R8*(center(2)-center(1))
      do k = 2, count
        corner(k) = 0.5_ESMF_KIND_R8*(center(k-1)+center(k))
      end do
      corner(count+1) = center(count) + &
        0.5_ESMF_KIND_R8*(center(count)-center(count-1))
    end subroutine centers_to_corners

    subroutine inverse_lambert(x_coordinate, y_coordinate, parallels, lon0_degrees, &
        lat0_degrees, easting, northing, radius, lat_degrees, lon_degrees)
      real(ESMF_KIND_R8), intent(in) :: x_coordinate, y_coordinate
      real(ESMF_KIND_R8), intent(in) :: parallels(2), lon0_degrees, lat0_degrees
      real(ESMF_KIND_R8), intent(in) :: easting, northing, radius
      real(ESMF_KIND_R8), intent(out) :: lat_degrees, lon_degrees
      real(ESMF_KIND_R8), parameter :: pi = acos(-1.0_ESMF_KIND_R8)
      real(ESMF_KIND_R8) :: phi1, phi2, phi0, lambda0
      real(ESMF_KIND_R8) :: cone, factor, rho0, rho, theta, dx, dy

      phi1 = parallels(1)*pi/180.0_ESMF_KIND_R8
      phi2 = parallels(2)*pi/180.0_ESMF_KIND_R8
      phi0 = lat0_degrees*pi/180.0_ESMF_KIND_R8
      lambda0 = lon0_degrees*pi/180.0_ESMF_KIND_R8
      if (abs(phi1-phi2) < 1.0e-12_ESMF_KIND_R8) then
        cone = sin(phi1)
      else
        cone = log(cos(phi1)/cos(phi2)) / &
          log(tan(pi/4.0_ESMF_KIND_R8+phi2/2.0_ESMF_KIND_R8) / &
              tan(pi/4.0_ESMF_KIND_R8+phi1/2.0_ESMF_KIND_R8))
      end if
      factor = cos(phi1) * &
        tan(pi/4.0_ESMF_KIND_R8+phi1/2.0_ESMF_KIND_R8)**cone / cone
      rho0 = radius*factor / &
        tan(pi/4.0_ESMF_KIND_R8+phi0/2.0_ESMF_KIND_R8)**cone
      dx = x_coordinate-easting
      dy = rho0-(y_coordinate-northing)
      rho = sign(sqrt(dx*dx+dy*dy), cone)
      if (abs(rho) < tiny(rho)) then
        lat_degrees = sign(90.0_ESMF_KIND_R8, cone)
        lon_degrees = lon0_degrees
      else
        theta = atan2(dx,dy)
        lat_degrees = (2.0_ESMF_KIND_R8*atan((radius*factor/rho)** &
          (1.0_ESMF_KIND_R8/cone))-pi/2.0_ESMF_KIND_R8)*180.0_ESMF_KIND_R8/pi
        lon_degrees = (lambda0+theta/cone)*180.0_ESMF_KIND_R8/pi
      end if
      lon_degrees = modulo(lon_degrees+180.0_ESMF_KIND_R8, &
        360.0_ESMF_KIND_R8)-180.0_ESMF_KIND_R8
    end subroutine inverse_lambert

    subroutine apply_regrid(source_values, target_values)
      real(ESMF_KIND_R8), intent(in) :: source_values(:,:)
      real(ESMF_KIND_R8), intent(out) :: target_values(:,:)
      integer :: local_rc
      source_ptr = reshape(source_values, (/size(source_ptr)/))
      target_ptr = 0.0_ESMF_KIND_R8
      call ESMF_FieldRegrid(source_field, target_field, bilinear_handle, rc=local_rc)
      call check_esmf(local_rc, 'applying CTSM-to-geogrid weights')
      target_values = target_ptr
    end subroutine apply_regrid

    subroutine consider_usgs_category(category, source_fraction)
      integer, intent(in) :: category
      real(ESMF_KIND_R8), intent(in) :: source_fraction(:,:)
      call apply_regrid(source_fraction, work_dst)
      where (work_dst > max_fraction)
        max_fraction = work_dst
        lu_index = category
      end where
    end subroutine consider_usgs_category

    integer function usda_soil_category(sand_percent, clay_percent) result(category)
      real(ESMF_KIND_R8), intent(in) :: sand_percent, clay_percent
      real(ESMF_KIND_R8) :: sand_value, clay_value, silt_value, total
      real(ESMF_KIND_R8) :: distance, best_distance
      real(ESMF_KIND_R8), parameter :: reference_sand(12) = &
        (/92.0,82.0,60.0,25.0,10.0,40.0,60.0,10.0,35.0,52.0,10.0,25.0/)
      real(ESMF_KIND_R8), parameter :: reference_clay(12) = &
        (/3.0,6.0,10.0,13.0,5.0,20.0,27.0,34.0,34.0,42.0,47.0,48.0/)
      integer :: k

      sand_value = max(0.0_ESMF_KIND_R8, sand_percent)
      clay_value = max(0.0_ESMF_KIND_R8, clay_percent)
      total = sand_value+clay_value
      if (total > 100.0_ESMF_KIND_R8) then
        sand_value = 100.0_ESMF_KIND_R8*sand_value/total
        clay_value = 100.0_ESMF_KIND_R8*clay_value/total
      end if
      silt_value = max(0.0_ESMF_KIND_R8, &
        100.0_ESMF_KIND_R8-sand_value-clay_value)

      if (sand_value >= 85.0_ESMF_KIND_R8 .and. &
          silt_value+1.5_ESMF_KIND_R8*clay_value < 15.0_ESMF_KIND_R8) then
        category = 1
      else if (sand_value >= 70.0_ESMF_KIND_R8 .and. sand_value < 90.0_ESMF_KIND_R8 .and. &
          silt_value+1.5_ESMF_KIND_R8*clay_value >= 15.0_ESMF_KIND_R8 .and. &
          silt_value+2.0_ESMF_KIND_R8*clay_value < 30.0_ESMF_KIND_R8) then
        category = 2
      else if ((clay_value >= 7.0_ESMF_KIND_R8 .and. clay_value < 20.0_ESMF_KIND_R8 .and. &
          sand_value > 52.0_ESMF_KIND_R8 .and. &
          silt_value+2.0_ESMF_KIND_R8*clay_value >= 30.0_ESMF_KIND_R8) .or. &
          (clay_value < 7.0_ESMF_KIND_R8 .and. silt_value < 50.0_ESMF_KIND_R8 .and. &
          silt_value+2.0_ESMF_KIND_R8*clay_value >= 30.0_ESMF_KIND_R8)) then
        category = 3
      else if ((silt_value >= 50.0_ESMF_KIND_R8 .and. clay_value >= 12.0_ESMF_KIND_R8 .and. &
          clay_value < 27.0_ESMF_KIND_R8) .or. &
          (silt_value >= 50.0_ESMF_KIND_R8 .and. silt_value < 80.0_ESMF_KIND_R8 .and. &
          clay_value < 12.0_ESMF_KIND_R8)) then
        category = 4
      else if (silt_value >= 80.0_ESMF_KIND_R8 .and. clay_value < 12.0_ESMF_KIND_R8) then
        category = 5
      else if (clay_value >= 7.0_ESMF_KIND_R8 .and. clay_value < 27.0_ESMF_KIND_R8 .and. &
          silt_value >= 28.0_ESMF_KIND_R8 .and. silt_value < 50.0_ESMF_KIND_R8 .and. &
          sand_value <= 52.0_ESMF_KIND_R8) then
        category = 6
      else if (clay_value >= 20.0_ESMF_KIND_R8 .and. clay_value < 35.0_ESMF_KIND_R8 .and. &
          silt_value < 28.0_ESMF_KIND_R8 .and. sand_value > 45.0_ESMF_KIND_R8) then
        category = 7
      else if (clay_value >= 27.0_ESMF_KIND_R8 .and. clay_value < 40.0_ESMF_KIND_R8 .and. &
          sand_value <= 20.0_ESMF_KIND_R8) then
        category = 8
      else if (clay_value >= 27.0_ESMF_KIND_R8 .and. clay_value < 40.0_ESMF_KIND_R8 .and. &
          sand_value > 20.0_ESMF_KIND_R8 .and. sand_value <= 45.0_ESMF_KIND_R8) then
        category = 9
      else if (clay_value >= 35.0_ESMF_KIND_R8 .and. sand_value > 45.0_ESMF_KIND_R8) then
        category = 10
      else if (clay_value >= 40.0_ESMF_KIND_R8 .and. silt_value >= 40.0_ESMF_KIND_R8) then
        category = 11
      else if (clay_value >= 40.0_ESMF_KIND_R8 .and. sand_value <= 45.0_ESMF_KIND_R8 .and. &
          silt_value < 40.0_ESMF_KIND_R8) then
        category = 12
      else
        category = 1
        best_distance = huge(1.0_ESMF_KIND_R8)
        do k = 1, 12
          distance = (sand_value-reference_sand(k))**2 + &
            (clay_value-reference_clay(k))**2
          if (distance < best_distance) then
            best_distance = distance
            category = k
          end if
        end do
      end if
    end function usda_soil_category

    subroutine aggregate_fulldom_topography(path, target_x, target_y, target_hgt)
      character(len=*), intent(in) :: path
      real(ESMF_KIND_R8), intent(in) :: target_x(:), target_y(:)
      real(ESMF_KIND_R8), intent(out) :: target_hgt(:,:)
      integer :: file_id, full_nx, full_ny, ratio_x, ratio_y
      integer :: ii, jj, i0, i1, j0, j1, variable_id
      real, allocatable :: full_topography(:,:)
      real(ESMF_KIND_R8), allocatable :: full_x(:), full_y(:)
      real(ESMF_KIND_R8) :: center_value, tolerance

      call check_nf(nf90_open(trim(path), NF90_NOWRITE, file_id), &
        'opening Fulldom_hires')
      call get_dimension(file_id, 'x', full_nx)
      call get_dimension(file_id, 'y', full_ny)
      if (mod(full_nx,size(target_x)) /= 0 .or. &
          mod(full_ny,size(target_y)) /= 0) then
        call fatal('Fulldom_hires dimensions are not integer multiples of the geogrid')
      end if
      ratio_x = full_nx/size(target_x)
      ratio_y = full_ny/size(target_y)
      allocate(full_x(full_nx), full_y(full_ny), full_topography(full_nx,full_ny))
      call read_variable_1d(file_id, 'x', full_x)
      call read_variable_1d(file_id, 'y', full_y)
      call check_nf(nf90_inq_varid(file_id, 'TOPOGRAPHY', variable_id), &
        'finding Fulldom_hires TOPOGRAPHY')
      call check_nf(nf90_get_var(file_id, variable_id, full_topography), &
        'reading Fulldom_hires TOPOGRAPHY')
      call check_nf(nf90_close(file_id), 'closing Fulldom_hires')

      tolerance = 1.0e-5_ESMF_KIND_R8 * max(1.0_ESMF_KIND_R8, &
        max(maxval(abs(target_x)),maxval(abs(target_y))))
      do ii = 1, size(target_x)
        i0 = (ii-1)*ratio_x+1
        i1 = ii*ratio_x
        center_value = sum(full_x(i0:i1))/real(ratio_x,ESMF_KIND_R8)
        if (abs(center_value-target_x(ii)) > tolerance) then
          call fatal('Fulldom_hires x coordinates are not aligned with the geogrid')
        end if
      end do
      do jj = 1, size(target_y)
        j0 = (jj-1)*ratio_y+1
        j1 = jj*ratio_y
        center_value = sum(full_y(j0:j1))/real(ratio_y,ESMF_KIND_R8)
        if (abs(center_value-target_y(jj)) > tolerance) then
          call fatal('Fulldom_hires y coordinates are not aligned with the geogrid')
        end if
      end do
      do jj = 1, size(target_y)
        j0 = (jj-1)*ratio_y+1
        j1 = jj*ratio_y
        do ii = 1, size(target_x)
          i0 = (ii-1)*ratio_x+1
          i1 = ii*ratio_x
          target_hgt(ii,jj) = sum(real(full_topography(i0:i1,j0:j1), &
            ESMF_KIND_R8))/real(ratio_x*ratio_y,ESMF_KIND_R8)
        end do
      end do
    end subroutine aggregate_fulldom_topography

    subroutine write_geogrid(path)
      character(len=*), intent(in) :: path
      character(len=*), parameter :: time_string = '0000-00-00_00:00:00'
      integer :: file_id
      integer :: dim_time, dim_we, dim_sn, dim_wes, dim_sns
      integer :: dim_soil, dim_land, dim_datestr
      integer :: var_times, var_hgt, var_hgt_m, var_lat, var_lat_m
      integer :: var_lon, var_lon_m, var_lat_c, var_lon_c
      integer :: var_lu, var_soil, var_mask
      integer :: dimensions_3d(3), corner_dimensions_3d(3)
      real(ESMF_KIND_R8) :: dx, dy, center_latitude, center_longitude
      real(ESMF_KIND_R8) :: normalized_central_lon

      dx = abs(x(2)-x(1))
      dy = abs(y(2)-y(1))
      center_latitude = latitude((nx+1)/2,(ny+1)/2)
      center_longitude = longitude((nx+1)/2,(ny+1)/2)
      normalized_central_lon = modulo(central_lon+180.0_ESMF_KIND_R8, &
        360.0_ESMF_KIND_R8)-180.0_ESMF_KIND_R8

      call check_nf(nf90_create(trim(path), NF90_CLOBBER, file_id), &
        'creating geogrid file')
      call check_nf(nf90_def_dim(file_id, 'Time', NF90_UNLIMITED, dim_time), &
        'defining Time')
      call check_nf(nf90_def_dim(file_id, 'west_east', nx, dim_we), &
        'defining west_east')
      call check_nf(nf90_def_dim(file_id, 'south_north', ny, dim_sn), &
        'defining south_north')
      call check_nf(nf90_def_dim(file_id, 'west_east_stag', nx+1, dim_wes), &
        'defining west_east_stag')
      call check_nf(nf90_def_dim(file_id, 'south_north_stag', ny+1, dim_sns), &
        'defining south_north_stag')
      call check_nf(nf90_def_dim(file_id, 'soil_cat', 19, dim_soil), &
        'defining soil_cat')
      call check_nf(nf90_def_dim(file_id, 'land_cat', 27, dim_land), &
        'defining land_cat')
      call check_nf(nf90_def_dim(file_id, 'DateStrLen', len(time_string), dim_datestr), &
        'defining DateStrLen')

      dimensions_3d = (/dim_we,dim_sn,dim_time/)
      corner_dimensions_3d = (/dim_wes,dim_sns,dim_time/)
      call check_nf(nf90_def_var(file_id, 'Times', NF90_CHAR, &
        (/dim_datestr,dim_time/), var_times), 'defining Times')
      call check_nf(nf90_def_var(file_id, 'HGT', NF90_FLOAT, dimensions_3d, var_hgt), &
        'defining HGT')
      call check_nf(nf90_def_var(file_id, 'HGT_M', NF90_FLOAT, dimensions_3d, var_hgt_m), &
        'defining HGT_M')
      call check_nf(nf90_def_var(file_id, 'XLAT', NF90_FLOAT, dimensions_3d, var_lat), &
        'defining XLAT')
      call check_nf(nf90_def_var(file_id, 'XLAT_M', NF90_FLOAT, dimensions_3d, var_lat_m), &
        'defining XLAT_M')
      call check_nf(nf90_def_var(file_id, 'XLONG', NF90_FLOAT, dimensions_3d, var_lon), &
        'defining XLONG')
      call check_nf(nf90_def_var(file_id, 'XLONG_M', NF90_FLOAT, dimensions_3d, var_lon_m), &
        'defining XLONG_M')
      call check_nf(nf90_def_var(file_id, 'XLAT_C', NF90_FLOAT, &
        corner_dimensions_3d, var_lat_c), 'defining XLAT_C')
      call check_nf(nf90_def_var(file_id, 'XLONG_C', NF90_FLOAT, &
        corner_dimensions_3d, var_lon_c), 'defining XLONG_C')
      call check_nf(nf90_def_var(file_id, 'LU_INDEX', NF90_INT, dimensions_3d, var_lu), &
        'defining LU_INDEX')
      call check_nf(nf90_def_var(file_id, 'SCT_DOM', NF90_INT, dimensions_3d, var_soil), &
        'defining SCT_DOM')
      call check_nf(nf90_def_var(file_id, 'LANDMASK', NF90_INT, dimensions_3d, var_mask), &
        'defining LANDMASK')

      call put_wps_variable_attributes(file_id, var_hgt, 'm', 'Terrain height')
      call put_wps_variable_attributes(file_id, var_hgt_m, 'm', 'Terrain height')
      call put_wps_variable_attributes(file_id, var_lat, 'degrees_north', 'Latitude')
      call put_wps_variable_attributes(file_id, var_lat_m, 'degrees_north', 'Latitude')
      call put_wps_variable_attributes(file_id, var_lon, 'degrees_east', 'Longitude')
      call put_wps_variable_attributes(file_id, var_lon_m, 'degrees_east', 'Longitude')
      call put_wps_variable_attributes(file_id, var_lat_c, 'degrees_north', &
        'Latitude at cell corners')
      call put_wps_variable_attributes(file_id, var_lon_c, 'degrees_east', &
        'Longitude at cell corners')
      call put_wps_variable_attributes(file_id, var_lu, 'category', &
        'Dominant USGS land-use category derived from CTSM surface fractions')
      call put_wps_variable_attributes(file_id, var_soil, 'category', &
        'Dominant top-layer USDA soil texture category derived from CTSM')
      call put_wps_variable_attributes(file_id, var_mask, 'none', &
        'Land mask: 1=land, 0=water')

      call check_nf(nf90_put_att(file_id, NF90_GLOBAL, 'TITLE', &
        'WRF-Hydro geogrid generated from CTSM surface data'), 'writing TITLE')
      call check_nf(nf90_put_att(file_id, NF90_GLOBAL, 'MMINLU', 'USGS'), &
        'writing MMINLU')
      call check_nf(nf90_put_att(file_id, NF90_GLOBAL, 'MMINSL', 'STAS'), &
        'writing MMINSL')
      call check_nf(nf90_put_att(file_id, NF90_GLOBAL, 'ISWATER', 16), &
        'writing ISWATER')
      call check_nf(nf90_put_att(file_id, NF90_GLOBAL, 'ISLAKE', -1), &
        'writing ISLAKE')
      call check_nf(nf90_put_att(file_id, NF90_GLOBAL, 'ISICE', 24), &
        'writing ISICE')
      call check_nf(nf90_put_att(file_id, NF90_GLOBAL, 'ISURBAN', 1), &
        'writing ISURBAN')
      call check_nf(nf90_put_att(file_id, NF90_GLOBAL, 'ISOILWATER', 14), &
        'writing ISOILWATER')
      call check_nf(nf90_put_att(file_id, NF90_GLOBAL, 'MAP_PROJ', 1), &
        'writing MAP_PROJ')
      call check_nf(nf90_put_att(file_id, NF90_GLOBAL, 'MAP_PROJ_CHAR', &
        'Lambert Conformal'), 'writing MAP_PROJ_CHAR')
      call check_nf(nf90_put_att(file_id, NF90_GLOBAL, 'DX', dx), 'writing DX')
      call check_nf(nf90_put_att(file_id, NF90_GLOBAL, 'DY', dy), 'writing DY')
      call check_nf(nf90_put_att(file_id, NF90_GLOBAL, 'TRUELAT1', &
        standard_parallel(1)), 'writing TRUELAT1')
      call check_nf(nf90_put_att(file_id, NF90_GLOBAL, 'TRUELAT2', &
        standard_parallel(2)), 'writing TRUELAT2')
      call check_nf(nf90_put_att(file_id, NF90_GLOBAL, 'STAND_LON', &
        normalized_central_lon), 'writing STAND_LON')
      call check_nf(nf90_put_att(file_id, NF90_GLOBAL, 'MOAD_CEN_LAT', &
        origin_lat), 'writing MOAD_CEN_LAT')
      call check_nf(nf90_put_att(file_id, NF90_GLOBAL, 'CEN_LAT', &
        center_latitude), 'writing CEN_LAT')
      call check_nf(nf90_put_att(file_id, NF90_GLOBAL, 'CEN_LON', &
        center_longitude), 'writing CEN_LON')
      call check_nf(nf90_put_att(file_id, NF90_GLOBAL, &
        'WEST-EAST_GRID_DIMENSION', nx+1), 'writing WEST-EAST_GRID_DIMENSION')
      call check_nf(nf90_put_att(file_id, NF90_GLOBAL, &
        'SOUTH-NORTH_GRID_DIMENSION', ny+1), 'writing SOUTH-NORTH_GRID_DIMENSION')

      call check_nf(nf90_enddef(file_id), 'ending geogrid define mode')
      call check_nf(nf90_put_var(file_id, var_times, time_string, &
        start=(/1,1/), count=(/len(time_string),1/)), 'writing Times')
      call check_nf(nf90_put_var(file_id, var_hgt, hgt, &
        start=(/1,1,1/), count=(/nx,ny,1/)), 'writing HGT')
      call check_nf(nf90_put_var(file_id, var_hgt_m, hgt, &
        start=(/1,1,1/), count=(/nx,ny,1/)), 'writing HGT_M')
      call check_nf(nf90_put_var(file_id, var_lat, latitude, &
        start=(/1,1,1/), count=(/nx,ny,1/)), 'writing XLAT')
      call check_nf(nf90_put_var(file_id, var_lat_m, latitude, &
        start=(/1,1,1/), count=(/nx,ny,1/)), 'writing XLAT_M')
      call check_nf(nf90_put_var(file_id, var_lon, longitude, &
        start=(/1,1,1/), count=(/nx,ny,1/)), 'writing XLONG')
      call check_nf(nf90_put_var(file_id, var_lon_m, longitude, &
        start=(/1,1,1/), count=(/nx,ny,1/)), 'writing XLONG_M')
      call check_nf(nf90_put_var(file_id, var_lat_c, latitude_corner, &
        start=(/1,1,1/), count=(/nx+1,ny+1,1/)), 'writing XLAT_C')
      call check_nf(nf90_put_var(file_id, var_lon_c, longitude_corner, &
        start=(/1,1,1/), count=(/nx+1,ny+1,1/)), 'writing XLONG_C')
      call check_nf(nf90_put_var(file_id, var_lu, lu_index, &
        start=(/1,1,1/), count=(/nx,ny,1/)), 'writing LU_INDEX')
      call check_nf(nf90_put_var(file_id, var_soil, soil_category, &
        start=(/1,1,1/), count=(/nx,ny,1/)), 'writing SCT_DOM')
      call check_nf(nf90_put_var(file_id, var_mask, landmask, &
        start=(/1,1,1/), count=(/nx,ny,1/)), 'writing LANDMASK')
      call check_nf(nf90_close(file_id), 'closing geogrid file')
    end subroutine write_geogrid

    subroutine put_wps_variable_attributes(file_id, variable_id, units, description)
      integer, intent(in) :: file_id, variable_id
      character(len=*), intent(in) :: units, description
      call check_nf(nf90_put_att(file_id, variable_id, 'FieldType', 104), &
        'writing FieldType attribute')
      call check_nf(nf90_put_att(file_id, variable_id, 'MemoryOrder', 'XY '), &
        'writing MemoryOrder attribute')
      call check_nf(nf90_put_att(file_id, variable_id, 'description', &
        trim(description)), 'writing description attribute')
      call check_nf(nf90_put_att(file_id, variable_id, 'units', trim(units)), &
        'writing units attribute')
      call check_nf(nf90_put_att(file_id, variable_id, 'stagger', 'M'), &
        'writing stagger attribute')
      call check_nf(nf90_put_att(file_id, variable_id, 'sr_x', 1), &
        'writing sr_x attribute')
      call check_nf(nf90_put_att(file_id, variable_id, 'sr_y', 1), &
        'writing sr_y attribute')
    end subroutine put_wps_variable_attributes

  end subroutine wrfhydro_create_geogrid_file

end module wrfhydro_nuopc_gluecode
