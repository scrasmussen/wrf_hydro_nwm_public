#include "WRFHydro_NUOPC_Macros.h"

module wrfhydro_nuopc_domain
! !MODULE: wrfhydro_nuopc_domain
!
! !DESCRIPTION:
!   This module provides domain utilities
!
! !REVISION HISTORY:
!  13Oct15    Dan Rosen  Initial Specification
!  06Jun22    Dan Rosen  Split from gluecode
!
! !USES:
  use ESMF
  use NUOPC
  use WRFHydro_ESMF_Extensions
  use module_mpp_land, only: &
    numprocs, &
    my_id, &
    global_nx, &
    global_ny, &
    startx, &
    starty, &
    local_nx_size, &
    local_ny_size, &
    IO_id
  use config_base, only: &
    nlst
  use orchestrator_base

  implicit none

  private

  public :: cap_domain_type
  public :: WRFHYDRO_DomainInit
  public :: WRFHYDRO_DomainLog
  public :: WRFHYDRO_DistGridCreate
  public :: WRFHYDRO_GridCreate

  type cap_domain_type
    integer               :: id    =  0
    character             :: label = '0'
    type(ESMF_DistGrid)   :: distgrid
    type(ESMF_Grid)       :: grid
    integer, dimension(2) :: lcl_bnds(2,2)     = UNINITIALIZED
    integer, dimension(1) :: lcl_exts(2)       = UNINITIALIZED
    real                  :: lcl_crds_ctr(2,2) = UNINITIALIZED
    real                  :: lcl_crds_edg(2,2) = UNINITIALIZED
    integer, dimension(1) :: gbl_exts(2)       = UNINITIALIZED
  end type cap_domain_type

  !-----------------------------------------------------------------------------
  ! Model Grid utilities
  !-----------------------------------------------------------------------------

contains

  subroutine WRFHYDRO_DomainInit(did,domain,rc)
    ! arguments
    integer, intent(in)                :: did
    type(cap_domain_type), intent(out) :: domain
    integer, intent(out)               :: rc

    rc = ESMF_SUCCESS

    domain%id    = did
    domain%label = nlst(domain%id)%hgrid
    domain%lcl_crds_ctr(2,2) = UNINITIALIZED
    domain%lcl_crds_edg(2,2) = UNINITIALIZED
    call WRFHYDRO_DistGridCreate(domain, rc=rc)
    if(ESMF_STDERRORCHECK(rc)) return
    call WRFHYDRO_GridCreate(domain, rc=rc)
    if(ESMF_STDERRORCHECK(rc)) return
  end subroutine

  !-------------------------------------------------------------------------

  subroutine WRFHYDRO_DistGridCreate(domain,rc)
    ! arguments
    type(cap_domain_type), intent(inout) :: domain
    integer, intent(out)                 :: rc
    ! local variables
    integer              :: stat
    integer, allocatable :: deBlockList(:,:,:)
    integer              :: i
    type(ESMF_VM)        :: currentVM
    integer              :: localPet
    integer              :: petCount
    integer, allocatable :: dimExtent(:,:)
    integer, allocatable :: iIndexList(:), jIndexList(:)

    rc = ESMF_SUCCESS

    ! create DistGrid based on WRFHDYRO decomposition
    allocate(deBlockList(2,2,numprocs), stat=stat)
    if (ESMF_LogFoundAllocError(statusToCheck=stat, &
      msg='Allocation of deBlockList memory failed.', &
      line=__LINE__, file=__FILE__, rcToReturn=rc)) return
    do i = 1, numprocs
      deBlockList(:,1,i) = (/startx(i),starty(i)/)
      deBlockList(:,2,i) = (/startx(i)+local_nx_size(i)-1, &
                             starty(i)+local_ny_size(i)-1/)
    enddo
    domain%distgrid = ESMF_DistGridCreate( &
      minIndex=(/1,1/), maxIndex=(/global_nx,global_ny/), &
      deBlockList=deBlockList, rc=rc)
    if(ESMF_STDERRORCHECK(rc)) return
    deallocate(deBlockList, stat=stat)
    if (ESMF_LogFoundDeallocError(statusToCheck=stat, &
      msg='Deallocation of deBlockList memory failed.', &
      line=__LINE__, file=__FILE__, rcToReturn=rc)) return

    ! get the grid decomposition for this pet
    call ESMF_VMGetCurrent(currentVM, rc=rc)
    if(ESMF_STDERRORCHECK(rc)) return
    call ESMF_VMGet(currentVM, localPet=localPet, petCount=petCount, rc=rc)
    if(ESMF_STDERRORCHECK(rc)) return
    allocate(dimExtent(2, 0:(petCount - 1)), stat=stat)
    if (ESMF_LogFoundAllocError(statusToCheck=stat, &
      msg='Allocation of dimExtent memory failed.', &
      line=__LINE__, file=__FILE__, rcToReturn=rc)) return
    call ESMF_DistGridGet(domain%distgrid, indexCountPDe=dimExtent, rc=rc)
    if(ESMF_STDERRORCHECK(rc)) return
    if (dimExtent(1,localPet) .gt. 0) then
      allocate(iIndexList(dimExtent(1, localPet)), stat=stat)
      if (ESMF_LogFoundAllocError(statusToCheck=stat, &
        msg='Allocation of iIndexList memory failed.', &
        line=__LINE__, file=__FILE__, rcToReturn=rc)) return
      call ESMF_DistGridGet(domain%distgrid, localDe=0, dim=1, &
        indexList=iIndexList, rc=rc)
      if(ESMF_STDERRORCHECK(rc)) return
      domain%lcl_bnds(1,1) = minVal(iIndexList)
      domain%lcl_bnds(1,2) = maxVal(iIndexList)
      domain%lcl_exts(1)   = size(iIndexList)
      deallocate(iIndexList,stat=stat)
      if (ESMF_LogFoundDeallocError(statusToCheck=stat, &
        msg='Deallocation of iIndexList memory failed.', &
        line=__LINE__, file=__FILE__, rcToReturn=rc)) return
    endif
    if (dimExtent(2,localPet) .gt. 0) then
      allocate(jIndexList(dimExtent(2, localPet)),stat=stat)
      if (ESMF_LogFoundAllocError(statusToCheck=stat, &
        msg='Allocation of jIndexList memory failed.', &
        line=__LINE__, file=__FILE__, rcToReturn=rc)) return
      call ESMF_DistGridGet(domain%distgrid, localDe=0, dim=2, &
        indexList=jIndexList, rc=rc)
      if(ESMF_STDERRORCHECK(rc)) return
      domain%lcl_bnds(2,1) = minVal(jIndexList)
      domain%lcl_bnds(2,2) = maxVal(jIndexList)
      domain%lcl_exts(2)   = size(jIndexList)
      deallocate(jIndexList,stat=stat)
      if (ESMF_LogFoundDeallocError(statusToCheck=stat, &
        msg='Deallocation of jIndexList memory failed.', &
        line=__LINE__, file=__FILE__, rcToReturn=rc)) return
    endif
    deallocate(dimExtent,stat=stat)
    if (ESMF_LogFoundDeallocError(statusToCheck=stat, &
      msg='Deallocation of dimExtent memory failed.', &
      line=__LINE__, file=__FILE__, rcToReturn=rc)) return

    ! store the global extents
    domain%gbl_exts(1)   = global_nx
    domain%gbl_exts(2)   = global_ny
  end subroutine

  !-------------------------------------------------------------------------

  subroutine WRFHYDRO_GridCreate(domain,rc)
    ! arguments
    type(cap_domain_type), intent(inout) :: domain
    integer, intent(out)                 :: rc
    ! local variables
    integer                        :: stat
    real                           :: min_lat, max_lat, min_lon, max_lon
    real, allocatable              :: latitude(:,:), longitude(:,:)
    integer, allocatable           :: mask(:,:)
    integer                        :: lbnd(2),ubnd(2)
    real(ESMF_KIND_COORD), pointer :: coordXcenter(:,:)
    real(ESMF_KIND_COORD), pointer :: coordYcenter(:,:)
    real(ESMF_KIND_COORD), pointer :: coordXcorner(:,:)
    real(ESMF_KIND_COORD), pointer :: coordYcorner(:,:)
    integer(ESMF_KIND_I4), pointer :: gridmask(:,:)
    integer                        :: i, j, i1, j1
    character(len=16)              :: xlat_corner_name, xlon_corner_name

    rc = ESMF_SUCCESS

    domain%grid = ESMF_GridCreate(name='WRFHYDRO_LSM_'//domain%label, &
      distgrid=domain%distgrid, coordSys = ESMF_COORDSYS_SPH_DEG, &
      coordTypeKind=ESMF_TYPEKIND_COORD, &
!      gridEdgeLWidth=(/0,0/), gridEdgeUWidth=(/0,1/), &
      rc = rc)
    if(ESMF_STDERRORCHECK(rc)) return

    ! LANDMASK

    ! Get Local Mask from File
    allocate(mask(domain%lcl_exts(1),domain%lcl_exts(2)),stat=stat)
    if (ESMF_LogFoundAllocError(statusToCheck=stat, &
      msg='Allocation of mask memory failed.', &
      line=__LINE__, file=__FILE__, rcToReturn=rc)) return
    call WRFHYDRO_ESMF_NetcdfReadIXJX("LANDMASK", &
      nlst(domain%id)%geo_static_flnm, &
      (/domain%lcl_bnds(1,1),domain%lcl_bnds(2,1)/), mask, rc=rc)
    if(ESMF_STDERRORCHECK(rc)) return

    ! Add Grid Mask
    call ESMF_GridAddItem(domain%grid, itemFlag=ESMF_GRIDITEM_MASK, &
      staggerLoc=ESMF_STAGGERLOC_CENTER, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return
    call ESMF_GridGetItem(domain%grid, itemflag=ESMF_GRIDITEM_MASK, &
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
      msg='Deallocation of mask memory failed.', &
      line=__LINE__, file=__FILE__, rcToReturn=rc)) return

    ! CENTERS

    ! Get Local Latitude from File
    allocate(latitude(domain%lcl_exts(1),domain%lcl_exts(2)),stat=stat)
    if (ESMF_LogFoundAllocError(statusToCheck=stat, &
      msg='Allocation of latitude memory failed.', &
      line=__LINE__, file=__FILE__, rcToReturn=rc)) return
    call WRFHYDRO_ESMF_NetcdfReadIXJX("XLAT_M", &
      nlst(domain%id)%geo_static_flnm, &
      (/domain%lcl_bnds(1,1),domain%lcl_bnds(2,1)/), latitude, rc=rc)
    if(ESMF_STDERRORCHECK(rc)) return
    domain%lcl_crds_ctr(1,1) = minval(latitude)
    domain%lcl_crds_ctr(1,2) = maxval(latitude)

    ! Get Local Longitude (lon)
    allocate(longitude(domain%lcl_exts(1),domain%lcl_exts(2)),stat=stat)
    if (ESMF_LogFoundAllocError(statusToCheck=stat, &
      msg='Allocation of longitude memory failed.', &
      line=__LINE__, file=__FILE__, rcToReturn=rc)) return
    call WRFHYDRO_ESMF_NetcdfReadIXJX("XLONG_M", &
      nlst(domain%id)%geo_static_flnm, &
      (/domain%lcl_bnds(1,1),domain%lcl_bnds(2,1)/), longitude, rc=rc)
    if(ESMF_STDERRORCHECK(rc)) return
    domain%lcl_crds_ctr(2,1) = minval(longitude)
    domain%lcl_crds_ctr(2,2) = maxval(longitude)

    ! Add Center Coordinates to Grid
    call ESMF_GridAddCoord(domain%grid, staggerLoc=ESMF_STAGGERLOC_CENTER, rc=rc)
    if(ESMF_STDERRORCHECK(rc)) return
    call ESMF_GridGetCoord(domain%grid, coordDim=1, localDE=0, &
      staggerloc=ESMF_STAGGERLOC_CENTER, &
      computationalLBound=lbnd, computationalUBound=ubnd, &
      farrayPtr=coordXcenter, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return
    call ESMF_GridGetCoord(domain%grid, coordDim=2, localDE=0, &
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
      msg='Deallocation of longitude and latitude memory failed.', &
      line=__LINE__, file=__FILE__, rcToReturn=rc)) return

    ! CORNERS

    ! The original WPS implementation used the _CORNER names
    ! but it was then changes to the _C names.  Support both
    ! options.
    if (WRFHYDRO_ESMF_NetcdfIsPresent("XLAT_CORNER", &
         nlst(domain%id)%geo_static_flnm)) then
       xlat_corner_name = "XLAT_CORNER"
       xlon_corner_name = "XLONG_CORNER"
    else if (WRFHYDRO_ESMF_NetcdfIsPresent("XLAT_C", &
               nlst(domain%id)%geo_static_flnm)) then
       xlat_corner_name = "XLAT_C"
       xlon_corner_name = "XLONG_C"
    else
       xlat_corner_name = ""
       xlon_corner_name = ""
    endif

    if (trim(xlat_corner_name) /= "") then
      ! Get Local Latitude from File
      allocate(latitude(domain%lcl_exts(1)+1,domain%lcl_exts(2)+1),stat=stat)
      if (ESMF_LogFoundAllocError(statusToCheck=stat, &
        msg='Allocation of corner latitude memory failed.', &
        line=__LINE__, file=__FILE__, rcToReturn=rc)) return
      call WRFHYDRO_ESMF_NetcdfReadIXJX(trim(xlat_corner_name), &
        nlst(domain%id)%geo_static_flnm, &
      (/domain%lcl_bnds(1,1),domain%lcl_bnds(2,1)/), latitude, rc=rc)
      if(ESMF_STDERRORCHECK(rc)) return
      domain%lcl_crds_edg(1,1) = minval(latitude)
      domain%lcl_crds_edg(1,2) = maxval(latitude)

      ! Get Local Longitude from File
      allocate(longitude(domain%lcl_exts(1)+1,domain%lcl_exts(2)+1),stat=stat)
      if (ESMF_LogFoundAllocError(statusToCheck=stat, &
       msg='Allocation of corner longitude memory failed.', &
       line=__LINE__, file=__FILE__, rcToReturn=rc)) return
      call WRFHYDRO_ESMF_NetcdfReadIXJX(trim(xlon_corner_name), &
        nlst(domain%id)%geo_static_flnm, &
      (/domain%lcl_bnds(1,1),domain%lcl_bnds(2,1)/), longitude, rc=rc)
      if(ESMF_STDERRORCHECK(rc)) return
      domain%lcl_crds_edg(2,1) = minval(longitude)
      domain%lcl_crds_edg(2,2) = maxval(longitude)

      ! Add Corner Coordinates to Grid
      call ESMF_GridAddCoord(domain%grid, staggerLoc=ESMF_STAGGERLOC_CORNER, rc=rc)
      if(ESMF_STDERRORCHECK(rc)) return
      call ESMF_GridGetCoord(domain%grid, coordDim=1, localDE=0, &
        staggerloc=ESMF_STAGGERLOC_CORNER, &
        computationalLBound=lbnd, computationalUBound=ubnd, &
        farrayPtr=coordXcorner, rc=rc)
      if (ESMF_STDERRORCHECK(rc)) return
      call ESMF_GridGetCoord(domain%grid, coordDim=2, localDE=0, &
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
        msg='Deallocation of corner longitude and latitude memory failed.', &
        line=__LINE__, file=__FILE__, rcToReturn=rc)) return

      call add_area()
      if (ESMF_STDERRORCHECK(rc)) return

    else
      ! Warning no corners in domain file
      call ESMF_LogWrite("No Corner Coordinates.", ESMF_LOGMSG_WARNING)
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

      fieldArea = ESMF_FieldCreate(grid=domain%grid, &
        typekind=ESMF_TYPEKIND_R8, rc=rc)
      if (ESMF_STDERRORCHECK(rc)) return

      call ESMF_FieldRegridGetArea(fieldArea, rc=rc)
      if (ESMF_STDERRORCHECK(rc)) return

      call ESMF_FieldGet(fieldArea, localDE=0, &
        farrayPtr=radianarea, rc=rc)
      if (ESMF_STDERRORCHECK(rc)) return

      call ESMF_GridAddItem(domain%grid, itemFlag=ESMF_GRIDITEM_AREA, &
        itemTypeKind=ESMF_TYPEKIND_R8, staggerLoc=ESMF_STAGGERLOC_CENTER, rc=rc)
      if (ESMF_STDERRORCHECK(rc)) return

      call ESMF_GridGetItem(domain%grid, itemflag=ESMF_GRIDITEM_AREA, localDE=0, &
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

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine WRFHYDRO_DomainLog(cname,domain,rc)
    ! arguments
    character(len=*),intent(in)       :: cname
    type(cap_domain_type), intent(in) :: domain
    integer,intent(out)               :: rc
    ! local variables
    character(ESMF_MAXSTR) :: logMsg

    rc = ESMF_SUCCESS

    write (logMsg, "(A,I0,A)") trim(cname)//': Settings domain(',domain%id,')'
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO)
    write (logMsg, "(A,(A,A))") trim(cname)//': ', &
      '  Label               = ',domain%label
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO)
    write (logMsg, "(A,(A,4(I0,A)))") trim(cname)//': ', &
      '  Local Bounds        = (', &
      domain%lcl_bnds(1,1), ":", domain%lcl_bnds(1,2), ",", &
      domain%lcl_bnds(2,1), ":", domain%lcl_bnds(2,2), ")"
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO)
    write (logMsg, "(A,(A,2(I0,A)))") trim(cname)//': ', &
      '  Local Extents       = (', &
      domain%lcl_exts(1), ",", domain%lcl_exts(2), ")"
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO)
    write (logMsg, "(A,(A,2(I0,A)))") trim(cname)//': ', &
      '  Global Extents      = (', &
      domain%gbl_exts(1), ",", domain%gbl_exts(2), ")"
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO)
    write (logMsg, "(A,(A,4(F0.3,A)))") trim(cname)//': ', &
      '  Local Center Coords = (', &
      domain%lcl_crds_ctr(1,1), ":", domain%lcl_crds_ctr(1,2), ",", &
      domain%lcl_crds_ctr(2,1), ":", domain%lcl_crds_ctr(2,2), ")"
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO)
    write (logMsg, "(A,(A,4(F0.3,A)))") trim(cname)//': ', &
      '  Local Edge Coords   = (', &
      domain%lcl_crds_edg(1,1), ":", domain%lcl_crds_edg(1,2), ",", &
      domain%lcl_crds_edg(2,1), ":", domain%lcl_crds_edg(2,2), ")"
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO)
  end subroutine

  !-----------------------------------------------------------------------------

end module
