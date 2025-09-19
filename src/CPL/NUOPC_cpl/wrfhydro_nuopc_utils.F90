module wrfhydro_nuopc_uttils
  use ESMF
  use NUOPC
  implicit none

contains

  subroutine check(rc, line)
    integer, intent(in) :: rc
    integer, intent(in) :: line
    logical :: res
    character(len=ESMF_MAXSTR), parameter :: file = __FILE__

    res = ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=line, file=file)
    if (res .eqv. .true.) error stop "Bad Check, msg = " // ESMF_LOGERR_PASSTHRU
  end subroutine check



end module wrfhydro_nuopc_uttils
