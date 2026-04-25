# Include ESMFMKFILE fragment
ifneq ($(origin ESMFMKFILE), environment)
$(error Environment variable ESMFMKFILE was not set.)
endif
include $(ESMFMKFILE)

# Determine Build Precision
ifeq ($(BUILD_PREC),r4)
override ESMF_F90COMPILECPPFLAGS += -DREAL4
else ifeq ($(BUILD_PREC),r8)
override ESMF_F90COMPILECPPFLAGS += -DREAL8
else
override ESMF_F90COMPILECPPFLAGS += -DREAL4
endif

# Compile with Debugging Directives
ifeq ($(DEBUG),on)
override ESMF_F90COMPILECPPFLAGS += -DDEBUG
override ESMF_CXXCOMPILECPPFLAGS += -DDEBUG
endif

ifdef DBGBUILD
ESMF_F90COMPILEOPTS += -g -traceback
ESMF_CXXCOMPILEOPTS += -g -traceback
endif

# Determine Installation Path
ifndef DESTDIR
DESTDIR  := $(DIR)
endif

ifndef INSTDIR
INSTDIR  := WRFHYDRO_$(shell date '+%Y-%m-%d-%H-%M-%S')
endif

ifndef INSTPATH
INSTPATH := $(abspath $(DESTDIR)/$(INSTDIR))
endif

# ###############
# Model Variables
# ###############

MODEL_DIR    := $(abspath $(PWD)/../../../)
MODEL_MKDIR  := $(abspath $(MODEL_DIR))
MODEL_LIBDIR := $(abspath $(MODEL_DIR)/lib)
MODEL_MODDIR := $(abspath $(MODEL_DIR)/mod)
MODEL_MPPDIR := $(abspath $(MODEL_DIR)/MPP)
MODEL_RTGDIR := $(abspath $(MODEL_DIR)/Routing)

MODEL_MK     := $(abspath $(MODEL_MKDIR)/Makefile.comm)
MODEL_MKINC  := $(abspath $(MODEL_DIR)/macros)
MODEL_LIB    := $(abspath $(MODEL_LIBDIR)/libHYDRO.a)

MODEL_MODS   := $(abspath $(MODEL_MPPDIR)/module_mpp_land.mod)
MODEL_MODS   += $(abspath $(MODEL_MPPDIR)/module_cpl_land.mod)
MODEL_MODS   += $(abspath $(MODEL_MODDIR)/module_hydro_drv.mod)
MODEL_MODS   += $(abspath $(MODEL_MODDIR)/module_rt_data.mod)
MODEL_MODS   += $(abspath $(MODEL_MODDIR)/module_namelist.mod)
MODEL_MODS   += $(abspath $(MODEL_MODDIR)/module_hydro_io.mod)
MODEL_MODS   += $(abspath $(MODEL_MODDIR)/module_lsm_forcing.mod)
MODEL_MODS   += $(abspath $(MODEL_RTGDIR)/Overland/overland_data.mod)
MODEL_MODS   += $(abspath $(MODEL_RTGDIR)/Overland/overland_control.mod)

MODEL_FILES  := $(MODEL_LIB) $(MODEL_MODS)

# #############
# Cap Variables
# #############

CAP_DIR       := $(abspath $(DIR))
CAP_LIB       := libwrfhydro_nuopc.a
CAP_DEP_FRONT := wrfhydro_nuopc
CAP_VERS      := VERSION

CAP_OBJS      := wrfhydro_nuopc_cap.o
CAP_OBJS      += wrfhydro_nuopc_gluecode.o
CAP_OBJS      += wrfhydro_nuopc_fields.o
CAP_OBJS      += wrfhydro_nuopc_flags.o
CAP_OBJS      += wrfhydro_esmf_extensions.o

CAP_MODS      := wrfhydro_nuopc.mod
CAP_MODS      += wrfhydro_nuopc_gluecode.mod
CAP_MODS      += wrfhydro_nuopc_fields.mod
CAP_MODS      += wrfhydro_nuopc_flags.mod
CAP_MODS      += wrfhydro_esmf_extensions.mod

CAP_FILES     := $(CAP_OBJS) $(CAP_LIB)
# CAP_FILES     := $(CAP_OBJS) $(CAP_MODS) $(CAP_LIB) $(CAP_VERS) $(CAP_MK)

# ###############################
# Include Model Makefile Fragment
# ###############################

include $(MODEL_MKINC)
override ESMF_F90COMPILEPATHS    += -I$(MODEL_MODDIR)
override ESMF_F90COMPILEPATHS    += -I$(MODEL_MPPDIR)
override ESMF_F90COMPILECPPFLAGS += $(HYDRO_D)

# #######################
# Primary Makefile Target
# #######################
.PHONY: nuopc nuopcinstall nuopcdistclean nuopcclean install_mk

nuopc: $(CAP_FILES)

nuopcinstall: $(CAP_LIB) $(CAP_MODS) \
 $(addprefix $(INSTPATH)/,$(CAP_LIB)) \
 $(addprefix $(INSTPATH)/,$(CAP_MODS)) \
 install_mk

# ############
# Dependencies
# ############

wrfhydro_nuopc_cap.o: wrfhydro_nuopc_macros.o \
 wrfhydro_nuopc_gluecode.o wrfhydro_nuopc_fields.o \
 wrfhydro_nuopc_flags.o wrfhydro_esmf_extensions.o

wrfhydro_nuopc_gluecode.o: wrfhydro_nuopc_macros.o \
 wrfhydro_nuopc_fields.o wrfhydro_nuopc_flags.o \
 wrfhydro_esmf_extensions.o

wrfhydro_nuopc_fields.o: wrfhydro_nuopc_macros.o \
 wrfhydro_nuopc_flags.o wrfhydro_esmf_extensions.o

wrfhydro_nuopc_macros.o:

wrfhydro_nuopc.mod: wrfhydro_nuopc_cap.o
wrfhydro_nuopc_gluecode.mod: wrfhydro_nuopc_gluecode.o
wrfhydro_nuopc_fields.mod: wrfhydro_nuopc_fields.o
wrfhydro_nuopc_flags.mod: wrfhydro_nuopc_flags.o
wrfhydro_esmf_extensions.mod: wrfhydro_esmf_extensions.o

libwrfhydro_nuopc.a: $(CAP_OBJS)
	ar rcs $@ $(CAP_OBJS)

fflags= \
  -I/glade/u/apps/derecho/25.10/spack/opt/spack/esmf/8.9.1/cray-mpich/8.1.32/gcc/14.3.0/zri7/include \
  -I../../../mod \
  -I./ \
  -I$(NCAR_INC_NETCDF) \
  -I../../../MPP


%.o: %.F90
	$(FC) $(fflags) $(CPPDEFS) -c $< -o $@

# Build Model

build: nuopc

clean:
	rm -f *.o *.mod *.a *~
