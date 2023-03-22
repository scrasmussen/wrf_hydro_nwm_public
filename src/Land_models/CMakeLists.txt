# NoahMP is a submodule project that does not currently have CMakeLists files
#   thus the submodule CMake is setup from here
# --- noahmp directory ---
set(noahmp_dir NoahMP/src)
set(noahmp_utility NoahMP/utility)
set(noahmp_drivers_hrldas_dir NoahMP/drivers/hrldas)
# set(noahmp_hrldas NoahMP/hrldas/Utility_routines)
set(noahmp_wrf NoahMP/wrf)

# drivers/hrldas/
add_library(noahmp_hrldas_driver
  ${noahmp_drivers_hrldas_dir}/NoahmpDriverMainMod.F90
  )

add_library(noahmp_hrldas_lib
  ${noahmp_drivers_hrldas_dir}/BiochemVarInTransferMod.F90
  ${noahmp_drivers_hrldas_dir}/BiochemVarOutTransferMod.F90
  ${noahmp_drivers_hrldas_dir}/ConfigVarInTransferMod.F90
  ${noahmp_drivers_hrldas_dir}/ConfigVarOutTransferMod.F90
  ${noahmp_drivers_hrldas_dir}/EnergyVarInTransferMod.F90
  ${noahmp_drivers_hrldas_dir}/EnergyVarOutTransferMod.F90
  ${noahmp_drivers_hrldas_dir}/ForcingVarInTransferMod.F90
  ${noahmp_drivers_hrldas_dir}/ForcingVarOutTransferMod.F90
  ${noahmp_drivers_hrldas_dir}/module_domain.F90
  ${noahmp_drivers_hrldas_dir}/module_ra_gfdleta.F90
  ${noahmp_drivers_hrldas_dir}/NoahmpGroundwaterInitMod.F90
  ${noahmp_drivers_hrldas_dir}/NoahmpInitMainMod.F90
  ${noahmp_drivers_hrldas_dir}/NoahmpIOVarInitMod.F90
  ${noahmp_drivers_hrldas_dir}/NoahmpReadNamelistMod.F90
  ${noahmp_drivers_hrldas_dir}/NoahmpReadTableMod.F90
  ${noahmp_drivers_hrldas_dir}/NoahmpSnowInitMod.F90
  ${noahmp_drivers_hrldas_dir}/PedoTransferSR2006Mod.F90
  ${noahmp_drivers_hrldas_dir}/WaterVarInTransferMod.F90
  ${noahmp_drivers_hrldas_dir}/WaterVarOutTransferMod.F90
  )

add_library(noahmp_utility
  ${noahmp_utility}/CheckNanMod.F90
  ${noahmp_utility}/ErrorHandleMod.F90
  ${noahmp_utility}/Machine.F90
  )

add_library(noahmp_var_types
  ${noahmp_drivers_hrldas_dir}/NoahmpIOVarType.F90
  ${noahmp_dir}/NoahmpVarType.F90
  ${noahmp_dir}/BiochemVarType.F90
  ${noahmp_dir}/ConfigVarType.F90
  ${noahmp_dir}/EnergyVarType.F90
  ${noahmp_dir}/ForcingVarType.F90
  ${noahmp_dir}/WaterVarType.F90
  )

add_library(noahmp_lib
  ${noahmp_dir}/AtmosForcingMod.F90
  ${noahmp_dir}/BalanceErrorCheckGlacierMod.F90
  ${noahmp_dir}/BalanceErrorCheckMod.F90
  ${noahmp_dir}/BiochemCropMainMod.F90
  ${noahmp_dir}/BiochemNatureVegMainMod.F90
  ${noahmp_dir}/BiochemVarInitMod.F90
  ${noahmp_dir}/CanopyHydrologyMod.F90
  ${noahmp_dir}/CanopyRadiationTwoStreamMod.F90
  ${noahmp_dir}/CanopyWaterInterceptMod.F90
  ${noahmp_dir}/CarbonFluxCropMod.F90
  ${noahmp_dir}/CarbonFluxNatureVegMod.F90
  ${noahmp_dir}/ConfigVarInitMod.F90
  ${noahmp_dir}/ConstantDefineMod.F90
  ${noahmp_dir}/CropGrowDegreeDayMod.F90
  ${noahmp_dir}/CropPhotosynthesisMod.F90
  ${noahmp_dir}/EnergyMainGlacierMod.F90
  ${noahmp_dir}/EnergyMainMod.F90
  ${noahmp_dir}/EnergyVarInitMod.F90
  ${noahmp_dir}/ForcingVarInitMod.F90
  ${noahmp_dir}/GeneralInitGlacierMod.F90
  ${noahmp_dir}/GeneralInitMod.F90
  ${noahmp_dir}/GlacierIceThermalPropertyMod.F90
  ${noahmp_dir}/GlacierPhaseChangeMod.F90
  ${noahmp_dir}/GlacierTemperatureMainMod.F90
  ${noahmp_dir}/GlacierTemperatureSolverMod.F90
  ${noahmp_dir}/GlacierThermalDiffusionMod.F90
  ${noahmp_dir}/GroundAlbedoGlacierMod.F90
  ${noahmp_dir}/GroundAlbedoMod.F90
  ${noahmp_dir}/GroundRoughnessPropertyGlacierMod.F90
  ${noahmp_dir}/GroundRoughnessPropertyMod.F90
  ${noahmp_dir}/GroundThermalPropertyGlacierMod.F90
  ${noahmp_dir}/GroundThermalPropertyMod.F90
  ${noahmp_dir}/GroundWaterMmfMod.F90
  ${noahmp_dir}/GroundWaterTopModelMod.F90
  ${noahmp_dir}/HumiditySaturationMod.F90
  ${noahmp_dir}/IrrigationFloodMod.F90
  ${noahmp_dir}/IrrigationInfilPhilipMod.F90
  ${noahmp_dir}/IrrigationMicroMod.F90
  ${noahmp_dir}/IrrigationPrepareMod.F90
  ${noahmp_dir}/IrrigationSprinklerMod.F90
  ${noahmp_dir}/IrrigationTriggerMod.F90
  ${noahmp_dir}/MatrixSolverTriDiagonalMod.F90
  ${noahmp_dir}/NoahmpMainGlacierMod.F90
  ${noahmp_dir}/NoahmpMainMod.F90
  ${noahmp_dir}/PhenologyMainMod.F90
  ${noahmp_dir}/PrecipitationHeatAdvectGlacierMod.F90
  ${noahmp_dir}/PrecipitationHeatAdvectMod.F90
  ${noahmp_dir}/PsychrometricVariableGlacierMod.F90
  ${noahmp_dir}/PsychrometricVariableMod.F90
  ${noahmp_dir}/ResistanceAboveCanopyChen97Mod.F90
  ${noahmp_dir}/ResistanceAboveCanopyMostMod.F90
  ${noahmp_dir}/ResistanceBareGroundChen97Mod.F90
  ${noahmp_dir}/ResistanceBareGroundMostMod.F90
  ${noahmp_dir}/ResistanceCanopyStomataBallBerryMod.F90
  ${noahmp_dir}/ResistanceCanopyStomataJarvisMod.F90
  ${noahmp_dir}/ResistanceGroundEvaporationGlacierMod.F90
  ${noahmp_dir}/ResistanceGroundEvaporationMod.F90
  ${noahmp_dir}/ResistanceLeafToGroundMod.F90
  ${noahmp_dir}/RunoffSubSurfaceDrainageMod.F90
  ${noahmp_dir}/RunoffSubSurfaceEquiWaterTableMod.F90
  ${noahmp_dir}/RunoffSubSurfaceGroundWaterMod.F90
  ${noahmp_dir}/RunoffSubSurfaceShallowMmfMod.F90
  ${noahmp_dir}/RunoffSurfaceBatsMod.F90
  ${noahmp_dir}/RunoffSurfaceDynamicVicMod.F90
  ${noahmp_dir}/RunoffSurfaceExcessDynamicVicMod.F90
  ${noahmp_dir}/RunoffSurfaceFreeDrainMod.F90
  ${noahmp_dir}/RunoffSurfaceTopModelEquiMod.F90
  ${noahmp_dir}/RunoffSurfaceTopModelGrdMod.F90
  ${noahmp_dir}/RunoffSurfaceTopModelMmfMod.F90
  ${noahmp_dir}/RunoffSurfaceVicMod.F90
  ${noahmp_dir}/RunoffSurfaceXinAnJiangMod.F90
  ${noahmp_dir}/ShallowWaterTableMmfMod.F90
  ${noahmp_dir}/SnowAgingBatsMod.F90
  ${noahmp_dir}/SnowAlbedoBatsMod.F90
  ${noahmp_dir}/SnowAlbedoClassMod.F90
  ${noahmp_dir}/SnowCoverGlacierMod.F90
  ${noahmp_dir}/SnowCoverGroundNiu07Mod.F90
  ${noahmp_dir}/SnowfallBelowCanopyMod.F90
  ${noahmp_dir}/SnowLayerCombineMod.F90
  ${noahmp_dir}/SnowLayerDivideMod.F90
  ${noahmp_dir}/SnowLayerWaterComboMod.F90
  ${noahmp_dir}/SnowpackCompactionMod.F90
  ${noahmp_dir}/SnowpackHydrologyGlacierMod.F90
  ${noahmp_dir}/SnowpackHydrologyMod.F90
  ${noahmp_dir}/SnowThermalPropertyMod.F90
  ${noahmp_dir}/SnowWaterMainGlacierMod.F90
  ${noahmp_dir}/SnowWaterMainMod.F90
  ${noahmp_dir}/SoilHydraulicPropertyMod.F90
  ${noahmp_dir}/SoilMoistureSolverMod.F90
  ${noahmp_dir}/SoilSnowTemperatureMainMod.F90
  ${noahmp_dir}/SoilSnowTemperatureSolverMod.F90
  ${noahmp_dir}/SoilSnowThermalDiffusionMod.F90
  ${noahmp_dir}/SoilSnowWaterPhaseChangeMod.F90
  ${noahmp_dir}/SoilThermalPropertyMod.F90
  ${noahmp_dir}/SoilWaterDiffusionRichardsMod.F90
  ${noahmp_dir}/SoilWaterInfilGreenAmptMod.F90
  ${noahmp_dir}/SoilWaterInfilPhilipMod.F90
  ${noahmp_dir}/SoilWaterInfilSmithParlangeMod.F90
  ${noahmp_dir}/SoilWaterMainMod.F90
  ${noahmp_dir}/SoilWaterSupercoolKoren99Mod.F90
  ${noahmp_dir}/SoilWaterSupercoolNiu06Mod.F90
  ${noahmp_dir}/SoilWaterTranspirationMod.F90
  ${noahmp_dir}/SurfaceAlbedoGlacierMod.F90
  ${noahmp_dir}/SurfaceAlbedoMod.F90
  ${noahmp_dir}/SurfaceEmissivityGlacierMod.F90
  ${noahmp_dir}/SurfaceEmissivityMod.F90
  ${noahmp_dir}/SurfaceEnergyFluxBareGroundMod.F90
  ${noahmp_dir}/SurfaceEnergyFluxGlacierMod.F90
  ${noahmp_dir}/SurfaceEnergyFluxVegetatedMod.F90
  ${noahmp_dir}/SurfaceRadiationGlacierMod.F90
  ${noahmp_dir}/SurfaceRadiationMod.F90
  ${noahmp_dir}/TileDrainageEquiDepthMod.F90
  ${noahmp_dir}/TileDrainageHooghoudtMod.F90
  ${noahmp_dir}/TileDrainageSimpleMod.F90
  ${noahmp_dir}/VaporPressureSaturationMod.F90
  ${noahmp_dir}/WaterMainGlacierMod.F90
  ${noahmp_dir}/WaterMainMod.F90
  ${noahmp_dir}/WaterTableDepthSearchMod.F90
  ${noahmp_dir}/WaterTableEquilibriumMod.F90
  ${noahmp_dir}/WaterVarInitMod.F90
  ) # end noahmp_lib

# REFACTOR TODO: after build works, NoahMP_WRF_H and these CMake path
#                directories can be put in it's new dir structure
add_library(wrf_hydro_noahmp
  NoahMP_WRF_H/phys/surfex/ini_csts.F
  NoahMP_WRF_H/phys/surfex/modd_csts.F
  )

add_library(noahmp_util # REFACTOR TODO: can these modules be removed?
  NoahMP_WRF_H/Utility_routines/kwm_string_utilities.F
  NoahMP_WRF_H/Utility_routines/module_date_utilities.F
  )

target_link_libraries(
  noahmp_hrldas_lib
  noahmp_var_types
  noahmp_lib
  )

target_link_libraries(
  noahmp_hrldas_driver
  noahmp_var_types
  noahmp_utility
  noahmp_lib
  noahmp_hrldas_lib
  )

target_link_libraries(
  noahmp_hrldas_lib
  noahmp_var_types
  )

target_link_libraries(
  noahmp_var_types
  noahmp_utility
  )

target_link_libraries(
  noahmp_lib
  noahmp_var_types
  noahmp_utility
  )

add_library(noahmp INTERFACE)
target_link_libraries(noahmp
  INTERFACE
  noahmp_lib
  noahmp_hrldas_driver
  noahmp_hrldas_lib
  wrf_hydro_noahmp
  )
