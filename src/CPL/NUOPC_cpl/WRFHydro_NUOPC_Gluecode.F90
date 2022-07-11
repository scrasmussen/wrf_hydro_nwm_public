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
    CPL_LAND_INIT
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

  !-----------------------------------------------------------------------------
  ! Model Glue Code
  !-----------------------------------------------------------------------------
contains

  subroutine wrfhydro_nuopc_ini(did,vm,forcingDir,domain,rc)
    integer, intent(in)                     :: did
    type(ESMF_VM),intent(in)                :: vm
    character(len=*)                        :: forcingDir
    type(cap_domain_type),intent(inout)     :: domain
    integer, intent(out)                    :: rc

    ! local variables
    character(*), parameter     :: rname="wrfhydro_nuopc_ini"
    character(len=19)           :: olddate
    integer                     :: khour
    integer                     :: nx_global(1)
    integer                     :: ny_global(1)
    integer                     :: stat
    integer                     :: i
    character(ESMF_MAXSTR)      :: logMsg

    rc = ESMF_SUCCESS

    ! Set focing directory
    indir=forcingDir

    if(.not. rt_domain(did)%initialized) then

      ! override system coupling option
      nlst(did)%sys_cpl = 2

      ! Set time (NUOPC driver will reset start time and run duration)
      write(olddate,'(I4.4,"-",I2.2,"-",I2.2,"_",I2.2,":",I2.2,":",I2.2)') &
        noah_lsm%start_year, noah_lsm%start_month, noah_lsm%start_day, &
        noah_lsm%start_hour, noah_lsm%start_min, 0
      khour = noah_lsm%khour
      if ((khour < 0) .and. (noah_lsm%kday < 0)) then
        call ESMF_LogSetError(ESMF_FAILURE, &
          msg=rname//" Namelist error: Either KHOUR or KDAY must be defined.", &
          line=__LINE__, file=__FILE__, rcToReturn=rc)
        return
      else if (( noah_lsm%khour < 0 ) .and. (noah_lsm%kday > 0)) then
        khour = noah_lsm%kday * 24
      endif
      nlst(did)%olddate(1:19) = olddate(1:19)
      nlst(did)%startdate(1:19) = olddate(1:19)
      nlst(did)%khour = khour
      nlst(did)%dt = real(noah_lsm%noah_timestep)
      if(nlst(did)%dt .le. 0) then
        call ESMF_LogSetError(ESMF_FAILURE, &
          msg=rname//": Timestep less than 1 is not supported!", &
          line=__LINE__, file=__FILE__, rcToReturn=rc)
        return
      endif

      ! Set soil thickness in nlst
      if (noah_lsm%nsoil < 0) then
        call ESMF_LogSetError(ESMF_FAILURE, &
          msg=rname//" Namelist error. NSOIL must be set in the namelist.", &
          line=__LINE__, file=__FILE__, rcToReturn=rc)
        return
      endif
      nlst(did)%nsoil = noah_lsm%nsoil
      allocate(nlst(did)%zsoil8(noah_lsm%nsoil))
      nlst(did)%zsoil8(1) = -1 * noah_lsm%soil_thick_input(1)
      do i=2, noah_lsm%nsoil
        nlst(did)%zsoil8(i) = nlst(did)%zsoil8(i-1)-noah_lsm%soil_thick_input(i)
      end do

      ! read nlst data from hydro.namelist config file
      call init_namelist_rt_field(did)

      ! get lsm grid dimensions
      call get_file_dimension(fileName=nlst(did)%geo_static_flnm, &
        ix=nx_global(1), jx=ny_global(1))
      call MPP_LAND_INIT(nx_global(1),ny_global(1))

      call log_map2d()

      call ESMF_VMBroadcast(vm, nx_global, count=1, rootPet=IO_id, rc=rc)
      if(ESMF_STDERRORCHECK(rc)) return
      call ESMF_VMBroadcast(vm, ny_global, count=1, rootPet=IO_id, rc=rc)
      if(ESMF_STDERRORCHECK(rc)) return

      rt_domain(did)%ix = nx_global(1)
      rt_domain(did)%jx = ny_global(1)

      ! Initialize decomposition
      call MPP_LAND_PAR_INI(1, rt_domain(did)%ix, rt_domain(did)%jx, &
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

      rt_domain(did)%initialized = .true.
    else
      call ESMF_LogSetError(ESMF_FAILURE, &
        msg=rname//" ERROR rt_domain has already been initialized.", &
        line=__LINE__, file=__FILE__, rcToReturn=rc)
      return
    endif

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
