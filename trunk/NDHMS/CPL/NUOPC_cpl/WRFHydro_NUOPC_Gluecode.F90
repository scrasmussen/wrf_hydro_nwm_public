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
  use wrfhydro_nuopc_domain
  use wrfhydro_nuopc_flags

  implicit none

  private

  public :: wrfhydro_nuopc_ini
  public :: wrfhydro_nuopc_run
  public :: wrfhydro_nuopc_fin
  public :: WRFHYDRO_isRestart

  ! PARAMETERS
  character(len=ESMF_MAXSTR) :: indir = 'WRFHYDRO_FORCING'
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

  !-----------------------------------------------------------------------------
  ! Model Glue Code
  !-----------------------------------------------------------------------------
contains

  subroutine wrfhydro_nuopc_ini(did,vm,clock,forcingDir,domain,rc)
    integer, intent(in)                     :: did
    type(ESMF_VM),intent(in)                :: vm
    type(ESMF_Clock),intent(in)             :: clock
    character(len=*)                        :: forcingDir
    type(cap_domain_type),intent(inout)     :: domain
    integer, intent(out)                    :: rc

    ! local variables
    character(*), parameter     :: rname="wrfhydro_nuopc_ini"
    integer                     :: nx_global(1)
    integer                     :: ny_global(1)
    integer                     :: localPet
    integer                     :: petCount
    integer                     :: stat
    integer                     :: i
    type(ESMF_Time)             :: startTime
    type(ESMF_TimeInterval)     :: timeStep
    real(ESMF_KIND_R8)          :: dt
    character(ESMF_MAXSTR)      :: logMsg

    rc = ESMF_SUCCESS

    ! Set mpiCommunicator for WRFHYDRO
    call ESMF_VMGet(vm, localPet=localPet, petCount=petCount, &
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

    ! Initialize the internal Land <-> Hydro Coupling
    call CPL_LAND_INIT(startx(my_id+1), &
      startx(my_id+1)+local_nx_size(my_id+1)-1, &
      starty(my_id+1), &
      starty(my_id+1)+local_ny_size(my_id+1)-1)

    ! Routing timestep set in HYDRO_ini
    if(sf_surface_physics .eq. 5) then
      ! clm4
      ! Use wrfinput vegetation type and soil type
      call HYDRO_ini(ntime=1,did=did,ix0=1,jx0=1)
    else
      ! Use wrfinput vegetation type and soil type
      call HYDRO_ini(ntime=1, did=did, &
        ix0=local_nx_size(my_id+1), &
        jx0=local_ny_size(my_id+1))
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
