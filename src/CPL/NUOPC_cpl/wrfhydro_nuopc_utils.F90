module wrfhydro_nuopc_utils
  use ESMF
  use NUOPC
  implicit none

  character(len=ESMF_MAXSTR), parameter :: file = __FILE__

contains

  subroutine check(rc, line, err_file)
    integer, intent(in) :: rc
    integer, intent(in) :: line
    character(len=ESMF_MAXSTR), intent(in) :: err_file
    logical :: res

    res = ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=line, file=err_file)
    if (res .eqv. .true.) error stop "Bad Check, msg = " // ESMF_LOGERR_PASSTHRU
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


end module wrfhydro_nuopc_utils
