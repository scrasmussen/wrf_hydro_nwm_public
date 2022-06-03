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
    nlst, noah_lsm

  implicit none

  private

  public :: WRFHYDRO_log_nlst
  public :: WRFHYDRO_log_rtdomain
  public :: WRFHYDRO_log_noahlsm

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

  subroutine WRFHYDRO_log_noahlsm(cname,rc)
    ! arguments
    character(len=*),intent(in) :: cname
    integer,intent(out)         :: rc
    ! local variables
    integer                     :: i
    character(ESMF_MAXSTR+256)  :: logMsg

    rc = ESMF_SUCCESS

    write (logMsg, "(A,I0,A)") trim(cname)//': Settings noah_lsm'
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO)
    write (logMsg, "(A,(A,A))") trim(cname)//': ', &
      '  indir                             = ',noah_lsm%indir
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO)
    write (logMsg, "(A,(A,I0))") trim(cname)//': ', &
      '  nsoil                             = ',noah_lsm%nsoil
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO)
    write (logMsg, "(A,(A,I0))") trim(cname)//': ', &
      '  crocus_opt                        = ',noah_lsm%crocus_opt
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO)
    write (logMsg, "(A,(A,I0))") trim(cname)//': ', &
      '  act_lev                           = ',noah_lsm%act_lev
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO)
    write (logMsg, "(A,(A,I0))") trim(cname)//': ', &
      '  forcing_timestep                  = ',noah_lsm%forcing_timestep
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO)
    write (logMsg, "(A,(A,I0))") trim(cname)//': ', &
      '  noah_timestep                     = ',noah_lsm%noah_timestep
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO)
    write (logMsg, "(A,(A,I0))") trim(cname)//': ', &
      '  start_year                        = ',noah_lsm%start_year
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO)
    write (logMsg, "(A,(A,I0))") trim(cname)//': ', &
      '  start_month                       = ',noah_lsm%start_month
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO)
    write (logMsg, "(A,(A,I0))") trim(cname)//': ', &
      '  start_day                         = ',noah_lsm%start_day
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO)
    write (logMsg, "(A,(A,I0))") trim(cname)//': ', &
      '  start_hour                        = ',noah_lsm%start_hour
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO)
    write (logMsg, "(A,(A,I0))") trim(cname)//': ', &
      '  start_min                         = ',noah_lsm%start_min
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO)
    write (logMsg, "(A,(A,A))") trim(cname)//': ', &
      '  outdir                            = ',noah_lsm%outdir
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO)
    write (logMsg, "(A,(A,A))") trim(cname)//': ', &
      '  restart_filename_requested        = ',noah_lsm%restart_filename_requested
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO)
    write (logMsg, "(A,(A,I0))") trim(cname)//': ', &
      '  restart_frequency_hours           = ',noah_lsm%restart_frequency_hours
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO)
    write (logMsg, "(A,(A,I0))") trim(cname)//': ', &
      '  output_timestep                   = ',noah_lsm%output_timestep
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO)
    write (logMsg, "(A,(A,I0))") trim(cname)//': ', &
      '  dynamic_veg_option                = ',noah_lsm%dynamic_veg_option
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO)
    write (logMsg, "(A,(A,I0))") trim(cname)//': ', &
      '  canopy_stomatal_resistance_option = ',noah_lsm%canopy_stomatal_resistance_option
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO)
    write (logMsg, "(A,(A,I0))") trim(cname)//': ', &
      '  btr_option                        = ',noah_lsm%btr_option
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO)
    write (logMsg, "(A,(A,I0))") trim(cname)//': ', &
      '  runoff_option                     = ',noah_lsm%runoff_option
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO)
    write (logMsg, "(A,(A,I0))") trim(cname)//': ', &
      '  surface_drag_option               = ',noah_lsm%surface_drag_option
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO)
    write (logMsg, "(A,(A,I0))") trim(cname)//': ', &
      '  supercooled_water_option          = ',noah_lsm%supercooled_water_option
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO)
    write (logMsg, "(A,(A,I0))") trim(cname)//': ', &
      '  frozen_soil_option                = ',noah_lsm%frozen_soil_option
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO)
    write (logMsg, "(A,(A,I0))") trim(cname)//': ', &
      '  radiative_transfer_option         = ',noah_lsm%radiative_transfer_option
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO)
    write (logMsg, "(A,(A,I0))") trim(cname)//': ', &
      '  snow_albedo_option                = ',noah_lsm%snow_albedo_option
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO)
    write (logMsg, "(A,(A,I0))") trim(cname)//': ', &
      '  pcp_partition_option              = ',noah_lsm%pcp_partition_option
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO)
    write (logMsg, "(A,(A,I0))") trim(cname)//': ', &
      '  tbot_option                       = ',noah_lsm%tbot_option
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO)
    write (logMsg, "(A,(A,I0))") trim(cname)//': ', &
      '  temp_time_scheme_option           = ',noah_lsm%temp_time_scheme_option
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO)
    write (logMsg, "(A,(A,I0))") trim(cname)//': ', &
      '  glacier_option                    = ',noah_lsm%glacier_option
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO)
    write (logMsg, "(A,(A,I0))") trim(cname)//': ', &
      '  surface_resistance_option         = ',noah_lsm%surface_resistance_option
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO)
    write (logMsg, "(A,(A,I0))") trim(cname)//': ', &
      '  soil_data_option                  = ',noah_lsm%soil_data_option
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO)
    write (logMsg, "(A,(A,I0))") trim(cname)//': ', &
      '  pedotransfer_option               = ',noah_lsm%pedotransfer_option
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO)
    write (logMsg, "(A,(A,I0))") trim(cname)//': ', &
      '  crop_option                       = ',noah_lsm%crop_option
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO)
    write (logMsg, "(A,(A,I0))") trim(cname)//': ', &
      '  imperv_option                     = ',noah_lsm%imperv_option
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO)
    write (logMsg, "(A,(A,I0))") trim(cname)//': ', &
      '  split_output_count                = ',noah_lsm%split_output_count
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO)
    write (logMsg, "(A,(A,I0))") trim(cname)//': ', &
      '  khour                             = ',noah_lsm%khour
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO)
    write (logMsg, "(A,(A,I0))") trim(cname)//': ', &
      '  kday                              = ',noah_lsm%kday
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO)
    write (logMsg, "(A,(A,F0.3))") trim(cname)//': ', &
      '  zlvl                              = ',noah_lsm%zlvl
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO)
    write (logMsg, "(A,(A,A))") trim(cname)//': ', &
      '  hrldas_setup_file                 = ',noah_lsm%hrldas_setup_file
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO)
    write (logMsg, "(A,(A,A))") trim(cname)//': ', &
      '  mmf_runoff_file                   = ',noah_lsm%mmf_runoff_file
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO)
    write (logMsg, "(A,(A,A))") trim(cname)//': ', &
      '  external_veg_filename_template    = ',noah_lsm%external_veg_filename_template
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO)
    write (logMsg, "(A,(A,A))") trim(cname)//': ', &
      '  external_lai_filename_template    = ',noah_lsm%external_lai_filename_template
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO)
    write (logMsg, "(A,(A,I0))") trim(cname)//': ', &
      '  xstart                            = ',noah_lsm%xstart
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO)
    write (logMsg, "(A,(A,I0))") trim(cname)//': ', &
      '  ystart                            = ',noah_lsm%ystart
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO)
    write (logMsg, "(A,(A,I0))") trim(cname)//': ', &
      '  xend                              = ',noah_lsm%xend
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO)
    write (logMsg, "(A,(A,I0))") trim(cname)//': ', &
      '  yend                              = ',noah_lsm%yend
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO)
    do i=1,noah_lsm%nsoil
      write (logMsg, "(A,(A,I2,A,F0.3))") trim(cname)//': ', &
        '  soil_thick_input(',i,')              = ',noah_lsm%soil_thick_input(i)
      call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO)
    enddo
    write (logMsg, "(A,(A,I0))") trim(cname)//': ', &
      '  rst_bi_out                        = ',noah_lsm%rst_bi_out
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO)
    write (logMsg, "(A,(A,I0))") trim(cname)//': ', &
      '  rst_bi_in                         = ',noah_lsm%rst_bi_in
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO)
    write (logMsg, "(A,(A,A))") trim(cname)//': ', &
      '  spatial_filename                  = ',noah_lsm%spatial_filename
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO)

  end subroutine WRFHYDRO_log_noahlsm

  !------------------------------------
end module
