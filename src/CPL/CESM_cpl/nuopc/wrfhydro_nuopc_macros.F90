module wrfhydro_nuopc_macros
  use ESMF, only : ESMF_KIND_R4, ESMF_KIND_R8, &
       ESMF_TYPEKIND_R4, ESMF_TYPEKIND_R8, ESMF_TypeKind_Flag, ESMF_TypeKind_Flag
  implicit none
!-------------------------------------------------------------------------------
! NUOPC CPP Macros
!-------------------------------------------------------------------------------
#ifndef FILENAME
#define FILENAME __FILE__
#endif
#define CONTEXT  line=__LINE__,file=FILENAME
#define PASSTHRU msg=ESMF_LOGERR_PASSTHRU,CONTEXT

!-------------------------------------------------------------------------------
! Define ESMF real kind to match Appplications single/double precision
!-------------------------------------------------------------------------------
#if defined(REAL4)
  integer, parameter :: ESMF_KIND_FIELD = ESMF_KIND_R4
  integer, parameter :: ESMF_KIND_COORD = ESMF_KIND_R4
  type(esmf_typekind_flag), parameter :: ESMF_TYPEKIND_FIELD = ESMF_TYPEKIND_R4
  type(esmf_typekind_flag), parameter :: ESMF_TYPEKIND_COORD = ESMF_TYPEKIND_R4
#elif defined(REAL8)
  integer, parameter :: ESMF_KIND_FIELD = ESMF_KIND_R8
  integer, parameter :: ESMF_KIND_COORD = ESMF_KIND_R8
  type(esmf_typekind_flag), parameter :: ESMF_TYPEKIND_FIELD = ESMF_TYPEKIND_R8
  type(esmf_typekind_flag), parameter :: ESMF_TYPEKIND_COORD = ESMF_TYPEKIND_R8
#else
  integer, parameter :: ESMF_KIND_FIELD = ESMF_KIND_R4
  integer, parameter :: ESMF_KIND_COORD = ESMF_KIND_R8
  type(esmf_typekind_flag), parameter :: ESMF_TYPEKIND_FIELD = ESMF_TYPEKIND_R4
  type(esmf_typekind_flag), parameter :: ESMF_TYPEKIND_COORD = ESMF_TYPEKIND_R8
#endif

!-------------------------------------------------------------------------------
! Define Missing Value
!-------------------------------------------------------------------------------

  real(ESMF_KIND_R8), parameter::  ESMF_MISSING_VALUE = 9.99e20_ESMF_KIND_R8
  integer, parameter :: UNINITIALIZED = -9999

!-------------------------------------------------------------------------------
! Define Output Levels
!-------------------------------------------------------------------------------

  integer, parameter :: VERBOSITY_LV0 = 0
  integer, parameter :: VERBOSITY_LV1 = 1
  integer, parameter :: VERBOSITY_LV2 = 255
  integer, parameter :: VERBOSITY_LV3 = 1023

end module wrfhydro_nuopc_macros
