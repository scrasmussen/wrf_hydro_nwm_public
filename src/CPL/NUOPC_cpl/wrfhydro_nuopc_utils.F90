module wrfhydro_nuopc_utils
  use ESMF
  use NUOPC
  implicit none

  character(len=ESMF_MAXSTR), parameter :: file = __FILE__

contains

  subroutine check(rc, line, err_file, did)
    integer, intent(in) :: rc
    integer, intent(in) :: line
    character(len=ESMF_MAXSTR), intent(in) :: err_file
    integer, intent(in), optional :: did
    logical :: res

    res = ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=line, file=err_file)
    if (res .eqv. .true.) then
       if (present(did)) then
          print *, did, ": Bad Check"
       end if
       error stop "Bad Check, msg = " // ESMF_LOGERR_PASSTHRU
    end if
  end subroutine check

  subroutine check_nf(stat)
    use netcdf, only: nf90_strerror, NF90_NOERR
    integer, intent(in) :: stat
    if (stat /= NF90_NOERR) then
       error stop nf90_strerror(stat)
    end if
  end subroutine check_nf

  subroutine printa(msg)
    character(len=*), intent(in) :: msg
    integer :: rc
    print *, "WRFH: ", trim(msg)
    call ESMF_LogWrite("WRFH: "//trim(msg), ESMF_LOGMSG_INFO, rc=rc)
    call check(rc, __LINE__, file)
  end subroutine printa

  subroutine probe_connected_pair(expState, impState, name, rc)
    type(ESMF_State), intent(inout) :: expState, impState
    character(*), intent(in) :: name
    integer, intent(out) :: rc
    type(ESMF_Field) :: fe, fi
    logical :: ce, ci
    character(:), allocatable :: file
    file = __FILE__
    rc = ESMF_SUCCESS

    ce = NUOPC_IsConnected(expState, fieldName=trim(name), rc=rc)
    ci = NUOPC_IsConnected(impState, fieldName=trim(name), rc=rc)
    write(*,'(A,1X,A,1X,L1,1X,L1)') 'Connected(exp,imp):', trim(name), ce, ci
    if (ce) then
       call ESMF_StateGet(expState, itemName=trim(name), field=fe, rc=rc)
       call check(rc, __LINE__, file)
       call ESMF_FieldValidate(fe, rc=rc)
       call check(rc, __LINE__, file)
    end if
    if (ci) then
       call ESMF_StateGet(impState, itemName=trim(name), field=fi, rc=rc)
       call check(rc, __LINE__, file)
       call ESMF_FieldValidate(fi, rc=rc)
       call check(rc, __LINE__, file)
    end if
  end subroutine probe_connected_pair


  subroutine array_write_i(grid, fileName)
    use netcdf
    implicit none
    integer, intent(in)            :: grid(:,:)
    character(len=*), intent(in)   :: fileName
    integer :: u, ios, i, j
    integer :: nrow, ncol
    integer :: ncid, varid, dimids(2)
    integer :: ny, nx, status


    ny = size(grid, 1)   ! first index in Fortran is "row"/y
    nx = size(grid, 2)   ! second index is "col"/x

  ! Create (overwrite) the file
  status = nf90_create(trim(fileName), NF90_CLOBBER, ncid);
  call check_nf(status)

  ! Define dimensions
  status = nf90_def_dim(ncid, 'y', ny, dimids(1));
  call check_nf(status)
  status = nf90_def_dim(ncid, 'x', nx, dimids(2));
  call check_nf(status)

  ! Define the 2-D integer variable on (y, x)
  status = nf90_def_var(ncid, 'grid', NF90_INT, dimids, varid);
  call check_nf(status)

  ! End define mode
  status = nf90_enddef(ncid);
  call check_nf(status)
  ! Write the array
  status = nf90_put_var(ncid, varid, grid);
  call check_nf(status)

  ! Close
  status = nf90_close(ncid);
  call check_nf(status)

  end subroutine array_write_i





  subroutine grid_write(grid, fileName, rc)
    use MPI
    use ESMF, only : ESMF_Grid, ESMF_Array, ESMF_ArrayBundle
    use ESMF, only : ESMF_ArrayBundleCreate, ESMF_GridGet
    use ESMF, only : ESMF_GridGetCoord, ESMF_ArraySet, ESMF_ArrayBundleAdd
    use ESMF, only : ESMF_GridGetItem, ESMF_ArrayBundleWrite, ESMF_ArrayBundleDestroy
    use ESMF, only : ESMF_STAGGERLOC_CENTER, ESMF_STAGGERLOC_CORNER
    use ESMF, only : ESMF_SUCCESS, ESMF_GRIDITEM_MASK, ESMF_GRIDITEM_AREA

    ! input/output variables
    type(ESMF_Grid) , intent(in)  :: grid
    character(len=*), intent(in)  :: fileName
    integer         , intent(out) :: rc

    ! local variables
    type(ESMF_Array)       :: array
    type(ESMF_ArrayBundle) :: arrayBundle
    integer                :: tileCount
    logical                :: isPresent
    !-------------------------------------------------------------------------------

    rc = ESMF_SUCCESS

    ! Check that the grid has tiles or not
    ! Currently only supports single tile
    call ESMF_GridGet(grid, tileCount=tileCount, rc=rc)
    call check(rc, __LINE__, file)

    if (tileCount .eq. 1) then
      ! Create arraybundle to store grid information
      arrayBundle = ESMF_ArrayBundleCreate(rc=rc)
      call check(rc, __LINE__, file)

      ! Query grid for center stagger
      ! Coordinates
      call ESMF_GridGetCoord(grid, staggerLoc=ESMF_STAGGERLOC_CENTER, &
            isPresent=isPresent, rc=rc)
      call check(rc, __LINE__, file)
      if (isPresent) then
        call ESMF_GridGetCoord(grid, coordDim=1, &
             staggerLoc=ESMF_STAGGERLOC_CENTER, array=array, rc=rc)
        call check(rc, __LINE__, file)
        call ESMF_ArraySet(array, name="lon_center", rc=rc)
        call check(rc, __LINE__, file)
        call ESMF_ArrayBundleAdd(arrayBundle, (/array/), rc=rc)
        call check(rc, __LINE__, file)
        call ESMF_GridGetCoord(grid, coordDim=2, &
             staggerLoc=ESMF_STAGGERLOC_CENTER, array=array, rc=rc)
        call check(rc, __LINE__, file)
        call ESMF_ArraySet(array, name="lat_center", rc=rc)
        call check(rc, __LINE__, file)
        call ESMF_ArrayBundleAdd(arrayBundle, (/array/), rc=rc)
        call check(rc, __LINE__, file)
      endif

      ! Mask
      call ESMF_GridGetItem(grid, itemflag=ESMF_GRIDITEM_MASK, &
           staggerLoc=ESMF_STAGGERLOC_CENTER, isPresent=isPresent, rc=rc)
      call check(rc, __LINE__, file)
      if (isPresent) then
        call ESMF_GridGetItem(grid, staggerLoc=ESMF_STAGGERLOC_CENTER, &
             itemflag=ESMF_GRIDITEM_MASK, array=array, rc=rc)
        call check(rc, __LINE__, file)
        call ESMF_ArraySet(array, name="mask_center", rc=rc)
        call check(rc, __LINE__, file)
        call ESMF_ArrayBundleAdd(arrayBundle, (/array/), rc=rc)
        call check(rc, __LINE__, file)
      endif

      ! Area
      call ESMF_GridGetItem(grid, itemflag=ESMF_GRIDITEM_AREA, &
           staggerLoc=ESMF_STAGGERLOC_CENTER, isPresent=isPresent, rc=rc)
      call check(rc, __LINE__, file)
      if (isPresent) then
        call ESMF_GridGetItem(grid, staggerLoc=ESMF_STAGGERLOC_CENTER, &
             itemflag=ESMF_GRIDITEM_AREA, array=array, rc=rc)
        call check(rc, __LINE__, file)
        call ESMF_ArraySet(array, name="area_center", rc=rc)
        call check(rc, __LINE__, file)
        call ESMF_ArrayBundleAdd(arrayBundle, (/array/), rc=rc)
        call check(rc, __LINE__, file)
      endif

      ! Query grid for corner stagger
      ! Coordinates
      call ESMF_GridGetCoord(grid, staggerLoc=ESMF_STAGGERLOC_CORNER, &
            isPresent=isPresent, rc=rc)
      call check(rc, __LINE__, file)

      if (isPresent) then
        call ESMF_GridGetCoord(grid, coordDim=1, &
             staggerLoc=ESMF_STAGGERLOC_CORNER, array=array, rc=rc)
        call check(rc, __LINE__, file)
        call ESMF_ArraySet(array, name="lon_corner", rc=rc)
        call check(rc, __LINE__, file)
        call ESMF_ArrayBundleAdd(arrayBundle, (/array/), rc=rc)
        call check(rc, __LINE__, file)
        call ESMF_GridGetCoord(grid, coordDim=2, &
             staggerLoc=ESMF_STAGGERLOC_CORNER, array=array, rc=rc)
        call check(rc, __LINE__, file)
        call ESMF_ArraySet(array, name="lat_corner", rc=rc)
        call check(rc, __LINE__, file)
        call ESMF_ArrayBundleAdd(arrayBundle, (/array/), rc=rc)
        call check(rc, __LINE__, file)
      endif

      ! Mask
      call ESMF_GridGetItem(grid, itemflag=ESMF_GRIDITEM_MASK, &
           staggerLoc=ESMF_STAGGERLOC_CORNER, isPresent=isPresent, rc=rc)
      call check(rc, __LINE__, file)
      if (isPresent) then
        call ESMF_GridGetItem(grid, staggerLoc=ESMF_STAGGERLOC_CORNER, &
             itemflag=ESMF_GRIDITEM_MASK, array=array, rc=rc)
        call check(rc, __LINE__, file)
        call ESMF_ArraySet(array, name="mask_corner", rc=rc)
        call check(rc, __LINE__, file)
        call ESMF_ArrayBundleAdd(arrayBundle, (/array/), rc=rc)
        call check(rc, __LINE__, file)
      endif

      ! Area
      call ESMF_GridGetItem(grid, itemflag=ESMF_GRIDITEM_AREA, &
           staggerLoc=ESMF_STAGGERLOC_CORNER, isPresent=isPresent, rc=rc)
      call check(rc, __LINE__, file)
      if (isPresent) then
        call ESMF_GridGetItem(grid, staggerLoc=ESMF_STAGGERLOC_CORNER, &
             itemflag=ESMF_GRIDITEM_AREA, array=array, rc=rc)
        call check(rc, __LINE__, file)
        call ESMF_ArraySet(array, name="area_corner", rc=rc)
        call check(rc, __LINE__, file)
        call ESMF_ArrayBundleAdd(arrayBundle, (/array/), rc=rc)
        call check(rc, __LINE__, file)
      endif

      ! Write arraybundle to file
      call ESMF_ArrayBundleWrite(arrayBundle, &
           fileName=trim(fileName), overwrite=.true., rc=rc)
      call check(rc, __LINE__, file)

      ! Destroy arraybundle
      call ESMF_ArrayBundleDestroy(arrayBundle, rc=rc)
      call check(rc, __LINE__, file)
    end if

  end subroutine grid_write




end module wrfhydro_nuopc_utils
