# Makefile
#
include ../user_build_options

SRCS := $(wildcard *.F)
OBJS := $(SRCS:%.F=%.o)
SUR_SRCS := $(wildcard surfex/*.F)
SUR_OBJS := $(SUR_SRCS:%.F=%.o)
CPPHRLDAS = -D_HRLDAS_OFFLINE_

all: $(OBJS)

module_sf_noahmplsm.o: ../noahmp/src/module_sf_noahmplsm.F
	@echo ""
	$(COMPILERF90) $(CPPINVOKE) $(CPPFLAGS) $(CPPHRLDAS) -o $(@) -c $(F90FLAGS) $(LDFLAGS) $(FREESOURCE) ../noahmp/src/module_sf_noahmplsm.F
	@echo ""

%.o:%.F
	@echo ""
	$(COMPILERF90) $(CPPINVOKE) $(CPPFLAGS) $(CPPHRLDAS) -o $(@) -c -I../Utility_routines -Isurfex $(F90FLAGS) $(LDFLAGS) $(FREESOURCE) $(*).F
	@echo ""

surfex/%.o:
	$(MAKE) --directory=surfex/

#
# Dependencies:
#
module_sf_noahmpdrv.o:	module_sf_noahmplsm.o module_sf_noahmp_glacier.o module_sf_noahmp_groundwater.o module_snowcro.o
module_sf_noahmp_groundwater.o: module_sf_noahmplsm.o
module_snowcro.o: $(SUR_OBJS)

#
# This command cleans up object (etc) files:
#
clean:
	$(MAKE) clean --directory=surfex
	$(RM) *.o *.mod *.stb *~
