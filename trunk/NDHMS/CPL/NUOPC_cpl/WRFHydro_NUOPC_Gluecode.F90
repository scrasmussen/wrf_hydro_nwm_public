#include "WRFHydro_NUOPC_Macros.h"

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
  use WRFHydro_ESMF_Extensions
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
  use wrfhydro_nuopc_time
  use wrfhydro_nuopc_flags

  implicit none

  private

  public :: wrfhydro_nuopc_ini
  public :: wrfhydro_nuopc_run
  public :: wrfhydro_nuopc_fin
  public :: WRFHYDRO_GridCreate
  public :: WRFHYDRO_grid_get
  public :: WRFHYDRO_isRestart

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

  interface WRFHYDRO_grid_get
    module procedure WRFHYDRO_hgrid_get
    module procedure WRFHYDRO_igrid_get
  end interface


  !-----------------------------------------------------------------------------
  ! Model Glue Code
  !-----------------------------------------------------------------------------
contains

  subroutine wrfhydro_nuopc_ini(did,vm,clock,forcingDir,verbosity,rc)
    integer, intent(in)                     :: did
    type(ESMF_VM),intent(in)                :: vm
    type(ESMF_Clock),intent(in)             :: clock
    character(len=*)                        :: forcingDir
    integer, intent(in)                     :: verbosity
    integer, intent(out)                    :: rc

    ! local variables
    character(*), parameter     :: rname="wrfhydro_nuopc_ini"
    integer                     :: localPet
    integer                     :: stat
    integer, allocatable        :: deBlockList(:,:,:)
    type(ESMF_DistGridConnection), allocatable :: connectionList(:)
    integer                     :: i
    type(ESMF_Time)             :: startTime
    type(ESMF_TimeInterval)     :: timeStep
    real(ESMF_KIND_R8)          :: dt
    character(ESMF_MAXSTR)      :: logMsg

    rc = ESMF_SUCCESS

    ! Set mpiCommunicator for WRFHYDRO
    call ESMF_VMGet(vm, localPet=localPet, &
      mpiCommunicator=HYDRO_COMM_WORLD, rc=rc)
    if(ESMF_STDERRORCHECK(rc)) return

    ! Set focing directory
    indir=forcingDir

    ! Get the models timestep
    call ESMF_ClockGet(clock, timeStep=timeStep, startTime=startTime, rc=rc)
    if(ESMF_STDERRORCHECK(rc)) return
    call ESMF_TimeIntervalGet(timeStep, s_r8=dt, rc=rc)
    if(ESMF_STDERRORCHECK(rc)) return
    call WRFHYDRO_time_toString(startTime, timestr=startTimeStr, rc=rc)
    if(ESMF_STDERRORCHECK(rc)) return

    call orchestrator%init()

    ! Set default namelist values
    read (startTimeStr(1:4),"(I)")   nlst(did)%START_YEAR
    read (startTimeStr(6:7),"(I)")   nlst(did)%START_MONTH
    read (startTimeStr(9:10),"(I)")  nlst(did)%START_DAY
    read (startTimeStr(12:13),"(I)") nlst(did)%START_HOUR
    read (startTimeStr(15:16),"(I)") nlst(did)%START_MIN
    nlst(did)%startdate(1:19) = startTimeStr(1:19)
    nlst(did)%olddate(1:19)   = startTimeStr(1:19)
    nlst(did)%dt = dt
    cpl_outdate = startTimeStr(1:19)
    nlst(did)%nsoil=4
    allocate(nlst(did)%zsoil8(4),stat=stat)
    if (ESMF_LogFoundAllocError(statusToCheck=stat, &
      msg=rname//': Allocation of model soil depths memory failed.', &
      line=__LINE__, file=__FILE__, rcToReturn=rc)) return
    nlst(did)%zsoil8(1:4)=(/-0.1,-0.4,-1.0,-2.0/)
    nlst(did)%geo_static_flnm = "geo_em.d01.nc"
    nlst(did)%geo_finegrid_flnm = "fulldom_hires_hydrofile.d01.nc"
    nlst(did)%sys_cpl = 2
    nlst(did)%IGRID = did
    write(nlst(did)%hgrid,'(I1)') did

    if(nlst(did)%dt .le. 0) then
      call ESMF_LogSetError(ESMF_FAILURE, &
        msg=rname//": Timestep less than 1 is not supported!", &
        line=__LINE__, file=__FILE__, rcToReturn=rc)
      return
    endif

!    ! Read information from hydro.namelist config file
     call init_namelist_rt_field(did)

    if(nlst(did)%nsoil .gt. 4) then
      call ESMF_LogSetError(ESMF_FAILURE, &
        msg=rname//": Maximum soil levels supported is 4.", &
        line=__LINE__, file=__FILE__, rcToReturn=rc)
      return
    endif

    call get_file_dimension(fileName=nlst(did)%geo_static_flnm,&
      ix=nx_global(1),jx=ny_global(1))
    call MPP_LAND_INIT(nx_global(1),ny_global(1))

    if (btest(verbosity,16)) then
      write (logMsg,"(A,2(I0,A))") rname//": Global Dimensions = (", &
        nx_global(1),",",ny_global(1),")"
      call ESMF_LogWrite(trim(logMsg), ESMF_LOGMSG_INFO)
    endif

    call log_map2d()

    call ESMF_VMBroadcast(vm, nx_global, count=1, rootPet=IO_id, rc=rc)
    if(ESMF_STDERRORCHECK(rc)) return
    call ESMF_VMBroadcast(vm, ny_global, count=1, rootPet=IO_id, rc=rc)
    if(ESMF_STDERRORCHECK(rc)) return

    rt_domain(did)%ix = nx_global(1)
    rt_domain(did)%jx = ny_global(1)

    call MPP_LAND_PAR_INI(1,rt_domain(did)%ix,rt_domain(did)%jx,&
         nlst(did)%AGGFACTRT)

    call ESMF_VMBroadcast(vm, startx, count=numprocs, rootPet=IO_id, rc=rc)
    if(ESMF_STDERRORCHECK(rc)) return
    call ESMF_VMBroadcast(vm, starty, count=numprocs, rootPet=IO_id, rc=rc)
    if(ESMF_STDERRORCHECK(rc)) return
    call ESMF_VMBroadcast(vm, local_nx_size, count=numprocs, rootPet=IO_id, rc=rc)
    if(ESMF_STDERRORCHECK(rc)) return
    call ESMF_VMBroadcast(vm, local_ny_size, count=numprocs, rootPet=IO_id, rc=rc)
    if(ESMF_STDERRORCHECK(rc)) return

    allocate(deBlockList(2,2,numprocs))
    do i = 1, numprocs
      deBlockList(:,1,i) = (/startx(i),starty(i)/)
      deBlockList(:,2,i) = (/startx(i)+local_nx_size(i)-1, &
                             starty(i)+local_ny_size(i)-1/)
!      write (logMsg,"(A,I0,A,4(I0,A))") rname//": deBlockList ", i, " = (", &
!        deBlockList(1,1,i),":",deBlockList(1,2,i),",", &
!        deBlockList(2,1,i),":",deBlockList(2,2,i),")"
!      call ESMF_LogWrite(trim(logMsg), ESMF_LOGMSG_INFO)
    enddo

!    allocate(connectionList(1),stat=stat)
!    if (ESMF_LogFoundAllocError(statusToCheck=stat, &
!      msg=rname//': Allocation of connection list memory failed.', &
!      line=__LINE__, file=__FILE__, rcToReturn=rc)) return
!    call ESMF_DistGridConnectionSet(connectionList(1), tileIndexA=1, &
!      tileIndexB=1, positionVector=(/nx_global(1), 0/), rc=rc)
!    if (ESMF_STDERRORCHECK(rc)) return


    ! Create DistGrid based on WRFHDYRO Config NX,NY
    WRFHYDRO_distgrid = ESMF_DistGridCreate( &
      minIndex=(/1,1/), maxIndex=(/nx_global(1),ny_global(1)/), &
!     indexflag = ESMF_INDEX_DELOCAL, &
     deBlockList=deBlockList, &
!     deLabelList=deLabelList, &
!     delayout=delayout, &
!     connectionList=connectionList, &
      rc=rc)
    if(ESMF_STDERRORCHECK(rc)) return

    deallocate(deBlockList)

!   deallocate(connectionList,stat=stat)
!   if (ESMF_LogFoundDeallocError(statusToCheck=stat, &
!     msg=rname//': Deallocation of connection list memory failed.', &
!     line=__LINE__, file=__FILE__, rcToReturn=rc)) return

    ! Get the Local Decomp Incides
    call set_local_indices(verbosity, rc)
    if(ESMF_STDERRORCHECK(rc)) return

    ! Initialize the internal Land <-> Hydro Coupling
    call CPL_LAND_INIT(x_start, x_end, y_start, y_end)

    ! Routing timestep set in HYDRO_ini
    if(sf_surface_physics .eq. 5) then
      ! clm4
      ! Use wrfinput vegetation type and soil type
      call HYDRO_ini(ntime=1,did=did,ix0=1,jx0=1)
    else
      ! Use wrfinput vegetation type and soil type
      call HYDRO_ini(ntime=1,did=did,ix0=nx_local,jx0=ny_local)
    endif

    ! Override the clock configuration in hyro.namelist
    read (startTimeStr(1:4),"(I)")   nlst(did)%START_YEAR
    read (startTimeStr(6:7),"(I)")   nlst(did)%START_MONTH
    read (startTimeStr(9:10),"(I)")  nlst(did)%START_DAY
    read (startTimeStr(12:13),"(I)") nlst(did)%START_HOUR
    read (startTimeStr(15:16),"(I)") nlst(did)%START_MIN
    nlst(did)%startdate(1:19) = startTimeStr(1:19)
    nlst(did)%olddate(1:19)   = startTimeStr(1:19)
    nlst(did)%dt = dt
    nlst(did)%nsoil=4
    cpl_outdate = startTimeStr(1:19)

    if(nlst(did)%dt .le. 0) then
      call ESMF_LogSetError(ESMF_FAILURE, &
        msg=rname//": Timestep less than 1 is not supported!", &
        line=__LINE__, file=__FILE__, rcToReturn=rc)
      return
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

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine wrfhydro_nuopc_run(did,lsm_forcings,clock,importState,&
  exportState,rc)
    integer, intent(in)                     :: did
    logical, intent(in)                     :: lsm_forcings
    type(ESMF_Clock),intent(in)             :: clock
    type(ESMF_State),intent(inout)          :: importState
    type(ESMF_State),intent(inout)          :: exportState
    integer, intent(out)                    :: rc

    ! local variables
    character(*), parameter     :: rname="wrfhydro_nuopc_run"
    type(ESMF_TimeInterval)     :: timeStep

    rc = ESMF_SUCCESS

    if(.not. RT_DOMAIN(did)%initialized) then
      call ESMF_LogSetError(ESMF_FAILURE, &
        msg="WRHYDRO: Model has not been initialized!", &
        line=__LINE__, file=__FILE__, rcToReturn=rc)
      return
    endif

    call ESMF_ClockGet(clock, timeStep=timeStep, rc=rc)
    if(ESMF_STDERRORCHECK(rc)) return

    call WRFHYDRO_time_toString(clock, timestr=cpl_outdate, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return
    nlst(did)%olddate(1:19) = cpl_outdate(1:19) ! Current time is the

    nlst(did)%dt = WRFHYDRO_interval_toReal(timeStep,rc=rc)
    if(ESMF_STDERRORCHECK(rc)) return

    if(nlst(did)%dt .le. 0) then
      call ESMF_LogSetError(ESMF_FAILURE, &
        msg=rname//": Timestep less than 1 is not supported!", &
        line=__LINE__, file=__FILE__, rcToReturn=rc)
      return
    endif

    if((dt_factor0*nlst(did)%dtrt_ter) .ne. nlst(did)%dt) then   ! NUOPC driver time step changed.
      call ESMF_LogWrite(rname//": Driver timestep changed.",ESMF_LOGMSG_INFO)
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
      call ESMF_LogWrite(rname//": Driver timestep changed.",ESMF_LOGMSG_INFO)
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
       call ESMF_LogWrite(rname//": SUBRTSWCRT,OVRTSWCRT,GWBASESWCRT are zero!", &
            ESMF_LOGMSG_WARNING)
      !call ESMF_LogSetError(ESMF_FAILURE, &
      !  msg=rname//": SUBRTSWCRT,OVRTSWCRT,GWBASESWCRT are zero!", &
      !  line=__LINE__, file=__FILE__, rcToReturn=rc)
      !return
    endif

    if((.not. RT_DOMAIN(did)%initialized) .and. (nlst(did)%rst_typ .eq. 1) ) then
      call ESMF_LogWrite(rname//": Restart initial data from offline file.", &
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

    ! Call the WRF-HYDRO run routine
    call HYDRO_exe(did=did)

    ! provide groundwater soil flux to WRF for fully coupled simulations (FERSCH 09/2014)
    !if(nlst(did)%GWBASESWCRT .eq. 3 ) then
      !Wei Yu: comment the following two lines. Not ready
    !yw     qsgw(x_start(1):x_end(1),y_start(1):y_end(1)) = gw2d(did)%qsgw
    !yw     config_flags%gwsoilcpl = nlst(did)%gwsoilcpl
    !end if

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine wrfhydro_nuopc_fin(did,rc)
    ! ARGUMENTES
    integer, intent(inout)      :: did
    integer, intent(out)        :: rc

    ! LOCAL VARIABLES
    character(*), parameter     :: rname="wrfhydro_nuopc_fin"
    integer                     :: stat

    rc = ESMF_SUCCESS

    ! WRF-Hydro finish routine cannot be called because it stops MPI

!    DCR - Turned off to let the model deallocate memory
!    deallocate(nlst(did)%zsoil8)
!    if (ESMF_LogFoundDeallocError(statusToCheck=stat, &
!      msg=rname//': Deallocation of model soil depth memory failed.', &
!      line=__LINE__, file=__FILE__, rcToReturn=rc)) return

    RT_DOMAIN(did)%initialized = .false.

  end subroutine

  function WRFHYDRO_GridCreate(did,verbosity,rc) result(grid)
    ! RETURN VALUE
    type(ESMF_Grid) :: grid
    ! ARGUMENTS
    integer, intent(in)                     :: did
    integer, intent(in)                     :: verbosity
    integer, intent(out)                    :: rc
    ! LOCAL VARIABLES
    character(*), parameter     :: rname="WRFHYDRO_GridCreate"
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
    character(ESMF_MAXSTR)      :: logMsg

    rc = ESMF_SUCCESS

    grid = ESMF_GridCreate(name='WRFHYDRO_Grid_'//trim(nlst(did)%hgrid), &
      distgrid=WRFHYDRO_DistGrid, coordSys = ESMF_COORDSYS_SPH_DEG, &
      coordTypeKind=ESMF_TYPEKIND_COORD, &
!      gridEdgeLWidth=(/0,0/), gridEdgeUWidth=(/0,1/), &
      rc = rc)
    if(ESMF_STDERRORCHECK(rc)) return

    ! CENTERS

    ! Get Local Latitude (lat)
    allocate(latitude(nx_local,ny_local),stat=stat)
    if (ESMF_LogFoundAllocError(statusToCheck=stat, &
      msg=rname//': Allocation of latitude memory failed.', &
      line=__LINE__, file=__FILE__, rcToReturn=rc)) return
    call WRFHYDRO_ESMF_NetcdfReadIXJX("XLAT_M",nlst(did)%geo_static_flnm, &
      (/x_start,y_start/),latitude,rc=rc)
    if(ESMF_STDERRORCHECK(rc)) return

    ! Get Local Longitude (lon)
    allocate(longitude(nx_local,ny_local),stat=stat)
    if (ESMF_LogFoundAllocError(statusToCheck=stat, &
      msg=rname//': Allocation of longitude memory failed.', &
      line=__LINE__, file=__FILE__, rcToReturn=rc)) return
    call WRFHYDRO_ESMF_NetcdfReadIXJX("XLONG_M",nlst(did)%geo_static_flnm, &
      (/x_start,y_start/),longitude,rc=rc)
    if(ESMF_STDERRORCHECK(rc)) return

    if (btest(verbosity,16)) then
      ! Print Local Lat Lon Lower Left / Upper Right Centers
      write(logMsg,"(A,4(F0.3,A))") rname//": Center Coordinates = (", &
        longitude(1,1),":",longitude(nx_local,ny_local),",", &
        latitude(1,1),":",latitude(nx_local,ny_local),")"
      call ESMF_LogWrite(trim(logMsg), ESMF_LOGMSG_INFO)
    endif

    ! Add Center Coordinates to Grid
    call ESMF_GridAddCoord(grid, staggerLoc=ESMF_STAGGERLOC_CENTER, rc=rc)
    if(ESMF_STDERRORCHECK(rc)) return

    call ESMF_GridGetCoord(grid, coordDim=1, localDE=0, &
      staggerloc=ESMF_STAGGERLOC_CENTER, &
      computationalLBound=lbnd, computationalUBound=ubnd, &
      farrayPtr=coordXcenter, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return
    call ESMF_GridGetCoord(grid, coordDim=2, localDE=0, &
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
      msg=rname//': Deallocation of longitude and latitude memory failed.', &
      line=__LINE__, file=__FILE__, rcToReturn=rc)) return

    ! Get Local Mask
    allocate(mask(nx_local,ny_local),stat=stat)
    if (ESMF_LogFoundAllocError(statusToCheck=stat, &
      msg=rname//': Allocation of mask memory failed.', &
      line=__LINE__, file=__FILE__, rcToReturn=rc)) return
    call WRFHYDRO_ESMF_NetcdfReadIXJX("LANDMASK",nlst(did)%geo_static_flnm, &
      (/x_start,y_start/),mask,rc=rc)
    if(ESMF_STDERRORCHECK(rc)) return

    ! Add Grid Mask
    call ESMF_GridAddItem(grid, itemFlag=ESMF_GRIDITEM_MASK, &
      staggerLoc=ESMF_STAGGERLOC_CENTER, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return
    ! Get pointer to Grid Mask array
    call ESMF_GridGetItem(grid, itemflag=ESMF_GRIDITEM_MASK, &
      localDE=0, &
      staggerloc=ESMF_STAGGERLOC_CENTER, &
      farrayPtr=gridmask, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return

    do j = lbnd(2),ubnd(2)
    do i = lbnd(1),ubnd(1)
      gridmask(i,j) = mask(i,j)
      gridmask(i,j) = mask(i,j)
    enddo
    enddo

    deallocate(mask,stat=stat)
    if (ESMF_LogFoundDeallocError(statusToCheck=stat, &
      msg=rname//': Deallocation of mask memory failed.', &
      line=__LINE__, file=__FILE__, rcToReturn=rc)) return

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
        msg=rname//': Allocation of corner latitude memory failed.', &
        line=__LINE__, file=__FILE__, rcToReturn=rc)) return
      call WRFHYDRO_ESMF_NetcdfReadIXJX(trim(xlat_corner_name),nlst(did)%geo_static_flnm, &
        (/x_start,y_start/),latitude,rc=rc)
      if(ESMF_STDERRORCHECK(rc)) return

      ! Get Local Longitude (lon)
      allocate(longitude(nx_local+1,ny_local+1),stat=stat)
      if (ESMF_LogFoundAllocError(statusToCheck=stat, &
       msg=rname//': Allocation of corner longitude memory failed.', &
       line=__LINE__, file=__FILE__, rcToReturn=rc)) return
      call WRFHYDRO_ESMF_NetcdfReadIXJX(trim(xlon_corner_name),nlst(did)%geo_static_flnm, &
        (/x_start,y_start/),longitude,rc=rc)
      if(ESMF_STDERRORCHECK(rc)) return

      if (btest(verbosity,16)) then
        ! Print Local Lat Lon Lower Left / Upper Right Corners
        write(logMsg,"(A,4(F0.3,A))") rname//": Corner Coordinates = (", &
          longitude(1,1),":",longitude(nx_local+1,ny_local+1),",", &
          latitude(1,1),":",latitude(nx_local+1,ny_local+1),")"
        call ESMF_LogWrite(trim(logMsg), ESMF_LOGMSG_INFO)
      endif

      ! Add Corner Coordinates to Grid
      call ESMF_GridAddCoord(grid, staggerLoc=ESMF_STAGGERLOC_CORNER, rc=rc)
      if(ESMF_STDERRORCHECK(rc)) return

      call ESMF_GridGetCoord(grid, coordDim=1, localDE=0, &
        staggerloc=ESMF_STAGGERLOC_CORNER, &
        computationalLBound=lbnd, computationalUBound=ubnd, &
        farrayPtr=coordXcorner, rc=rc)
      if (ESMF_STDERRORCHECK(rc)) return
      call ESMF_GridGetCoord(grid, coordDim=2, localDE=0, &
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
        msg=rname//': Deallocation of corner longitude and latitude memory failed.', &
        line=__LINE__, file=__FILE__, rcToReturn=rc)) return

      call add_area()
      if (ESMF_STDERRORCHECK(rc)) return

    else
      ! Warning no corners in domain file
      call ESMF_LogWrite(rname//": No Corner Coordinates.", ESMF_LOGMSG_WARNING)
    endif

    contains

    !---------------------------------------------------------------------------

    subroutine add_area()
      ! local variables
      integer(ESMF_KIND_I4), PARAMETER :: R = 6376000 ! metres
      type(ESMF_Field)                 :: fieldArea
      type(ESMF_Array)                 :: areaArray
      integer                          :: i,j
      integer                          :: lbnd(2),ubnd(2)
      real(ESMF_KIND_R8), pointer      :: radianarea(:,:)
      real(ESMF_KIND_R8), pointer      :: gridarea(:,:)

      fieldArea = ESMF_FieldCreate(grid=grid, typekind=ESMF_TYPEKIND_R8, rc=rc)
      if (ESMF_STDERRORCHECK(rc)) return

      call ESMF_FieldRegridGetArea(fieldArea, rc=rc)
      if (ESMF_STDERRORCHECK(rc)) return

      call ESMF_FieldGet(fieldArea, localDE=0, &
        farrayPtr=radianarea, rc=rc)
      if (ESMF_STDERRORCHECK(rc)) return

      call ESMF_GridAddItem(grid, itemFlag=ESMF_GRIDITEM_AREA, &
        itemTypeKind=ESMF_TYPEKIND_R8, staggerLoc=ESMF_STAGGERLOC_CENTER, rc=rc)
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

    end subroutine

    !---------------------------------------------------------------------------

  end function

  !-----------------------------------------------------------------------------

  subroutine set_local_indices(verbosity,rc)
    ! ARGUMENTS
    integer, intent(in)                     :: verbosity
    integer, intent(out)                    :: rc

    ! LOCAL VARIABLES
    character(*), parameter     :: rname="set_local_indices"
    integer                     :: stat
    type(ESMF_VM)               :: currentVM
    integer                     :: localPet
    integer                     :: petCount
    type(ESMF_DELayout)         :: delayout
    integer, allocatable        :: dimExtent(:,:)
    integer, allocatable        :: iIndexList(:), jIndexList(:)
    character(ESMF_MAXSTR)      :: logMsg

    rc = ESMF_SUCCESS

    !! Get VM Info to see if this will give me the PET info I need
    call ESMF_VMGetCurrent(currentVM, rc=rc)
    if(ESMF_STDERRORCHECK(rc)) return
    call ESMF_VMGet(currentVM, localPet=localPet, petCount=petCount, rc=rc)
    if(ESMF_STDERRORCHECK(rc)) return

    !! Get the grid distribution for this pet
    allocate(dimExtent(2, 0:(petCount - 1)),stat=stat) ! (dimCount, deCount)
    if (ESMF_LogFoundAllocError(statusToCheck=stat, &
      msg=rname//': Allocation of indexCountPDe memory failed.', &
      line=__LINE__, file=__FILE__, rcToReturn=rc)) return
    call ESMF_DistGridGet(WRFHYDRO_DistGrid, delayout=delayout, &
      indexCountPDe=dimExtent, rc=rc)
    if(ESMF_STDERRORCHECK(rc)) return

    allocate(iIndexList(dimExtent(1, localPet)),stat=stat)
    if (ESMF_LogFoundAllocError(statusToCheck=stat, &
      msg=rname//': Allocation of iIndexList memory failed.', &
      line=__LINE__, file=__FILE__, rcToReturn=rc)) return
    call ESMF_DistGridGet(WRFHYDRO_DistGrid, localDe=0, dim=1, &
      indexList=iIndexList, rc=rc)
    if(ESMF_STDERRORCHECK(rc)) return

    allocate(jIndexList(dimExtent(2, localPet)),stat=stat)
    if (ESMF_LogFoundAllocError(statusToCheck=stat, &
      msg=rname//': Allocation of jIndexList memory failed.', &
      line=__LINE__, file=__FILE__, rcToReturn=rc)) return
    call ESMF_DistGridGet(WRFHYDRO_DistGrid, localDe=0, dim=2, &
      indexList=jIndexList, rc=rc)
    if(ESMF_STDERRORCHECK(rc)) return

    x_start = minVal(iIndexList)
    x_end   = maxVal(iIndexList)
    y_start = minVal(jIndexList)
    y_end   = maxVal(jIndexList)

    nx_local = x_end - x_start + 1
    ny_local = y_end - y_start + 1

    if (btest(verbosity,16)) then
      write (logMsg,"(A,6(I0,A))") rname//": Local Indices = (", &
        x_start,":",x_end,",",y_start,":",y_end,") Local Size = (", &
        nx_local,"x",ny_local,")"
      call ESMF_LogWrite(trim(logMsg), ESMF_LOGMSG_INFO)
    endif

    deallocate(iIndexList,jIndexList,stat=stat)
    if (ESMF_LogFoundDeallocError(statusToCheck=stat, &
      msg=rname//': Deallocation of IndexList memory failed.', &
      line=__LINE__, file=__FILE__, rcToReturn=rc)) return

    deallocate(dimExtent,stat=stat)
    if (ESMF_LogFoundDeallocError(statusToCheck=stat, &
      msg=rname//': Deallocation of indexCountPDeo memory failed.', &
      line=__LINE__, file=__FILE__, rcToReturn=rc)) return

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine WRFHYDRO_hgrid_get(did,grid,rc)
    ! arguments
    integer, intent(in)    :: did
    character, intent(out) :: grid
    integer,   intent(out) :: rc
    rc = ESMF_SUCCESS
    grid = nlst(did)%hgrid
  end subroutine

  !-----------------------------------------------------------------------------

  subroutine WRFHYDRO_igrid_get(did,grid,rc)
    ! arguments
    integer, intent(in)  :: did
    integer, intent(out) :: grid
    integer, intent(out) :: rc
    rc = ESMF_SUCCESS
    grid = nlst(did)%igrid
  end subroutine

  !-----------------------------------------------------------------------------

  function WRFHYDRO_isRestart(did) result(restart)
    ! return value
    logical :: restart
    ! arguments
    integer, intent(in) :: did

    if (nlst(did)%rst_typ .le. 0) then
      restart = .FALSE.
    else
      restart = .TRUE.
    endif
  end function

  !-----------------------------------------------------------------------------

end module
