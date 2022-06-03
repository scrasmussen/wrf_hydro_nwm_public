#include "WRFHydro_NUOPC_Macros.h"

module wrfhydro_nuopc_time
! !MODULE: wrfhydro_nuopc_time
!
! !DESCRIPTION:
!   This module provides time utilities
!
! !REVISION HISTORY:
!  13Oct15    Dan Rosen  Initial Specification
!  06Jun22    Dan Rosen  Split from gluecode
!
! !USES:
  use ESMF
  use NUOPC
  use config_base, only: &
    nlst, noah_lsm

  implicit none

  private

  public :: WRFHYDRO_timestep_get
  public :: WRFHYDRO_timestep_set
  public :: WRFHYDRO_time_toString
  public :: WRFHYDRO_interval_toReal
  public :: NLST_IsAtTime
  public :: LSM_IsAtTime

  interface WRFHYDRO_time_toString
    module procedure WRFHYDRO_esmfclock_toString
    module procedure WRFHYDRO_esmftime_toString
  end interface

  interface WRFHYDRO_interval_toReal
    module procedure WRFHYDRO_esmfinterval_toReal
  end interface

  !-----------------------------------------------------------------------------
  ! Model Time Utilities
  !-----------------------------------------------------------------------------

contains

  !-----------------------------------------------------------------------------

  function WRFHYDRO_timestep_get(did) result(timestep)
    ! return value
    real :: timestep
    ! arguments
    integer, intent(in)         :: did

    timestep = nlst(did)%dt

  end function

  !-----------------------------------------------------------------------------

  subroutine WRFHYDRO_timestep_set(did,dt)
    ! arguments
    integer, intent(in)           :: did
    real                          :: dt

    nlst(did)%dt = dt

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine WRFHYDRO_esmfclock_toString(clock,timestr,rc)
    ! arguments
    type(ESMF_Clock)               :: clock
    character (len=*), intent(out) :: timestr
    integer, intent(out)           :: rc
    ! local variables
    type(ESMF_Time)     :: currTime
    character (len=256) :: tmpstr = ''
    integer             :: strlen

    rc = ESMF_SUCCESS
    timestr = ''

    if (len(timestr) < 19) then
      call ESMF_LogSetError(ESMF_FAILURE, &
        msg="Time string is too short!", &
        line=__LINE__, file=__FILE__, rcToReturn=rc)
      return
    endif

    call ESMF_ClockGet(clock=clock, currTime=currTime, rc=rc)
    if(ESMF_STDERRORCHECK(rc)) return

    call ESMF_TimeGet(currTime,timeString=tmpstr,rc=rc )
    if(ESMF_STDERRORCHECK(rc)) return

    strlen = min(len(timestr),len_trim(tmpstr))
    timestr(1:strlen) = tmpstr(1:strlen)
    timestr(11:11) = '_'

  end subroutine

!-----------------------------------------------------------------------------

  subroutine WRFHYDRO_esmftime_toString(time,timestr,rc)
    ! arguments
    type(ESMF_Time)                :: time
    character (len=*), intent(out) :: timestr
    integer, intent(out)           :: rc
    ! local variables
    character (len=256)        :: tmpstr = ''
    integer                    :: strlen

    rc = ESMF_SUCCESS
    timestr = ''

    if (len(timestr) < 19) then
      call ESMF_LogSetError(ESMF_FAILURE, &
        msg="Time string is too short!", &
        line=__LINE__, file=__FILE__, rcToReturn=rc)
      return
    endif

    call ESMF_TimeGet(time, timeString=tmpstr, rc=rc)
    if(ESMF_STDERRORCHECK(rc)) return

    strlen = min(len(timestr),len_trim(tmpstr))
    timestr(1:strlen) = tmpstr(1:strlen)
    timestr(11:11) = '_'

  end subroutine

  !-----------------------------------------------------------------------------

  function WRFHYDRO_esmfinterval_toReal(interval,rc) result(s_r)
    ! return value
    real :: s_r
    ! arguments
    type(ESMF_TimeInterval) :: interval
    integer, intent(out)    :: rc
    ! local variables
    real(ESMF_KIND_R8)   :: s_r8

    rc = ESMF_SUCCESS
    call ESMF_TimeIntervalGet(interval, s_r8=s_r8, rc=rc)
    if(ESMF_STDERRORCHECK(rc)) return
    s_r = real(s_r8)
  end function

  !-----------------------------------------------------------------------------

  function NLST_IsAtTime(time, did, rc) result(check)
    ! return value
    logical :: check
    ! arguments
    type(ESMF_Time), intent(in) :: time
    integer, intent(in)         :: did
    integer, intent(out)        :: rc
    ! local variables
    type(ESMF_Time) :: htime

    call ESMF_TimeSet(time=htime, &
      yy=int(nlst(did)%START_YEAR, ESMF_KIND_I4), &
      mm=nlst(did)%START_MONTH, &
      dd=nlst(did)%START_DAY, &
      h=int(nlst(did)%START_HOUR, ESMF_KIND_I4), &
      m=int(nlst(did)%START_MIN, ESMF_KIND_I4), &
      rc=rc)
    if(ESMF_STDERRORCHECK(rc)) return
    check = (htime .eq. time)
  end function

  !-----------------------------------------------------------------------------

  function LSM_IsAtTime(time, rc) result(check)
    ! return value
    logical :: check
    ! arguments
    type(ESMF_Time), intent(in) :: time
    integer, intent(out)        :: rc
    ! local variables
    type(ESMF_Time) :: htime

    call ESMF_TimeSet(time=htime, &
      yy=int(noah_lsm%start_year, ESMF_KIND_I4), &
      mm=noah_lsm%start_month, &
      dd=noah_lsm%start_day, &
      h=int(noah_lsm%start_hour, ESMF_KIND_I4), &
      m=int(noah_lsm%start_min, ESMF_KIND_I4), &
      rc=rc)
    if(ESMF_STDERRORCHECK(rc)) return
    check = (htime .eq. time)
  end function

  !-----------------------------------------------------------------------------

end module
