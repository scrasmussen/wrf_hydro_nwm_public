#include "WRFHydro_NUOPC_Macros.h"

module wrfhydro_esmf_logging
! !MODULE: wrfhydro_esmf_logging
!
! !DESCRIPTION:
!   This module provides logging utilities
!
! !REVISION HISTORY:
!  13Oct15    Dan Rosen  Initial Specification
!  06Jun22    Dan Rosen  Split from gluecode
!
! !USES:
  use ESMF
  use module_rt_data, only: &
    rt_domain
  use config_base, only: &
    nlst
  use wrfhydro_nuopc_flags

  implicit none

  private

  public :: WRFHYDRO_log_nlst
  public :: WRFHYDRO_log_rtdomain

contains

  subroutine WRFHYDRO_log_nlst(cname,did,rc)
    ! arguments
    character(len=*),intent(in) :: cname
    integer,intent(in)          :: did
    integer,intent(out)         :: rc
    ! local variables
    character(ESMF_MAXSTR+1024) :: logMsg
    integer                     :: i

    rc = ESMF_SUCCESS

    write (logMsg, "(A,I0,A)") trim(cname)//': Settings nlst(',did,')'
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO)
    write (logMsg, "(A,(A,I0))") trim(cname)//': ', &
      '  Start Year            = ',nlst(did)%START_YEAR
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO)
    write (logMsg, "(A,(A,I0))") trim(cname)//': ', &
      '  Start Month           = ',nlst(did)%START_MONTH
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO)
    write (logMsg, "(A,(A,I0))") trim(cname)//': ', &
      '  Start Day             = ',nlst(did)%START_DAY
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO)
    write (logMsg, "(A,(A,I0))") trim(cname)//': ', &
      '  Start Hour            = ',nlst(did)%START_HOUR
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO)
    write (logMsg, "(A,(A,I0))") trim(cname)//': ', &
      '  Start Minute          = ',nlst(did)%START_MIN
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO)
    write (logMsg, "(A,(A,F0.3))") trim(cname)//': ', &
      '  Timestep(dt)          = ',nlst(did)%dt
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO)
    write (logMsg, "(A,(A,F0.3))") trim(cname)//': ', &
      '  Output Timestep       = ',nlst(did)%out_dt
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO)
    write (logMsg, "(A,(A,F0.3))") trim(cname)//': ', &
      '  Restart Step          = ',nlst(did)%rst_dt
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO)
    write (logMsg, "(A,(A,F0.3))") trim(cname)//': ', &
      '  Ter Routing Step      = ',nlst(did)%dtrt_ter
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO)
    write (logMsg, "(A,(A,F0.3))") trim(cname)//': ', &
      '  Ch Routing Step       = ',nlst(did)%dtrt_ch
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO)
    write (logMsg, "(A,(A,I0))") trim(cname)//': ', &
      '  Grid ID               = ',nlst(did)%igrid
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO)
    write (logMsg, "(A,(A,A))") trim(cname)//': ', &
      '  Hydro Grid            = ',nlst(did)%hgrid
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO)
    write (logMsg, "(A,(A,A))") trim(cname)//': ', &
      '  Geo Grid File         = ',nlst(did)%geo_static_flnm
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO)
    write (logMsg, "(A,(A,A))") trim(cname)//': ', &
      '  Fine Grid File        = ',nlst(did)%geo_finegrid_flnm
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO)
    write (logMsg, "(A,(A,A))") trim(cname)//': ', &
      '  GW Basin File         = ',nlst(did)%gwbasmskfil
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO)
    write (logMsg, "(A,(A,I0))") trim(cname)//': ', &
      '  Restart Type          = ',nlst(did)%rst_typ
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO)
    write (logMsg, "(A,(A,A))") trim(cname)//': ', &
      '  Restart file          = ',nlst(did)%restart_file
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO)
    write (logMsg, "(A,(A,I0))") trim(cname)//': ', &
      '  Coupling              = ',nlst(did)%sys_cpl
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO)
    write (logMsg, "(A,(A,I0))") trim(cname)//': ', &
      '  Channel RT            = ',nlst(did)%CHANRTSWCRT
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO)
    write (logMsg, "(A,(A,I0))") trim(cname)//': ', &
      '  Subsurface RT         = ',nlst(did)%SUBRTSWCRT
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO)
    write (logMsg, "(A,(A,I0))") trim(cname)//': ', &
      '  Overland RT           = ',nlst(did)%OVRTSWCRT
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO)
    write (logMsg, "(A,(A,I0))") trim(cname)//': ', &
      '  GW Baseflow RT        = ',nlst(did)%GWBASESWCRT
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO)
    write (logMsg, "(A,(A,I0))") trim(cname)//': ', &
      '  Routing Option        = ',nlst(did)%RT_OPTION
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO)
    write (logMsg, "(A,(A,I0))") trim(cname)//': ', &
      '  Channel Option        = ',nlst(did)%channel_option
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO)
    write (logMsg, "(A,(A,I0))") trim(cname)//': ', &
      '  Aggr Factor           = ',nlst(did)%AGGFACTRT
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO)
    write (logMsg, "(A,(A,I0))") trim(cname)//': ', &
      '  GW Restart            = ',nlst(did)%GW_RESTART
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO)
    write (logMsg, "(A,(A,I0))") trim(cname)//': ', &
      '  SWC Restart           = ',nlst(did)%RSTRT_SWC
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO)
    write (logMsg, "(A,(A,I0))") trim(cname)//': ', &
      '  Soil Layers           = ',nlst(did)%nsoil
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO)
    if (allocated(nlst(did)%ZSOIL8)) then
      do i=1,nlst(did)%nsoil
        write (logMsg, "(A,(A,I2,A,F0.3))") trim(cname)//': ', &
          '  Soil layer depth (',i,') =',nlst(did)%ZSOIL8(i)
        call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO)
      enddo
    endif

  end subroutine WRFHYDRO_log_nlst

  !-----------------------------------------------------------------------------

  subroutine WRFHYDRO_log_rtdomain(cname,did,rc)
    ! arguments
    character(len=*),intent(in) :: cname
    integer,intent(in)          :: did
    integer,intent(out)         :: rc
    ! local variables
    character(ESMF_MAXSTR)      :: logMsg

    rc = ESMF_SUCCESS

    write (logMsg, "(A,I0,A)") trim(cname)//': Settings rt_domain(',did,')'
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO)
    write (logMsg, "(A,(A,L1))") trim(cname)//': ', &
      '  Initialized     = ',rt_domain(did)%initialized
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO)
    write (logMsg, "(A,(A,I0))") trim(cname)//': ', &
      '  IX              = ',rt_domain(did)%IX
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO)
    write (logMsg, "(A,(A,I0))") trim(cname)//': ', &
      '  JX              = ',rt_domain(did)%JX
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO)
    write (logMsg, "(A,(A,I0))") trim(cname)//': ', &
      '  IXRT            = ',rt_domain(did)%IXRT
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO)
    write (logMsg, "(A,(A,I0))") trim(cname)//': ', &
      '  JXRT            = ',rt_domain(did)%JXRT
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO)
    write (logMsg, "(A,(A,I0))") trim(cname)//': ', &
      '  Forcing Type    = ',rt_domain(did)%FORC_TYP
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO)
    write (logMsg, "(A,(A,I0))") trim(cname)//': ', &
      '  Max Links       = ',rt_domain(did)%NLINKS
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO)
    write (logMsg, "(A,(A,I0))") trim(cname)//': ', &
      '  Num Lakes       = ',rt_domain(did)%NLAKES
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO)
    write (logMsg, "(A,(A,I0))") trim(cname)//': ', &
      '  Num Basins      = ',rt_domain(did)%numbasns
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO)

  end subroutine WRFHYDRO_log_rtdomain

  !-----------------------------------------------------------------------------

end module
