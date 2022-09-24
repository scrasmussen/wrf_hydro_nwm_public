# NoahMP is a submodule project that does not currently have CMakeLists files
#   thus the submodule CMake is setup from here
# --- noahmp directory ---
set(noahmp_dir NoahMP/noahmp/src)
set(noahmp_utility NoahMP/noahmp/utility)
set(noahmp_drivers NoahMP/noahmp/drivers)
set(noahmp_hrldas NoahMP/hrldas/Utility_routines)
set(noahmp_wrf NoahMP/wrf)

add_library(noahmp_refactor_hrldas
# add_library(noahmp_refactor
  ${noahmp_hrldas}/kwm_string_utilities.F
  ${noahmp_hrldas}/module_date_utilities.F
  ${noahmp_hrldas}/module_domain.F
  ${noahmp_hrldas}/module_model_constants.F
  ${noahmp_hrldas}/module_ra_gfdleta.F
  ${noahmp_hrldas}/module_wrf_error.F
  ${noahmp_hrldas}/module_wrf_utilities.F
  )
add_library(noahmp_refactor_noahmp
  ${noahmp_drivers}/NoahmpDriverMainMod.F90
  ${noahmp_drivers}/NoahmpGroundwaterInitMod.F90
  ${noahmp_drivers}/NoahmpInitMainMod.F90
  ${noahmp_drivers}/NoahmpIOVarType.F90
  ${noahmp_drivers}/NoahmpIOVarInitMod.F90
  ${noahmp_drivers}/NoahmpSnowInitMod.F90
  ${noahmp_utility}/CheckNanMod.F90
  ${noahmp_utility}/ErrorHandleMod.F90
  ${noahmp_utility}/Machine.F90
  ${noahmp_dir}/AtmosForcingMod.F90
  ${noahmp_dir}/BalanceErrorCheckGlacierMod.F90
  ${noahmp_dir}/BalanceErrorCheckMod.F90
  ${noahmp_dir}/BiochemCropMainMod.F90
  ${noahmp_dir}/BiochemNatureVegMainMod.F90
  ${noahmp_dir}/BiochemVarInitMod.F90
  ${noahmp_dir}/BiochemVarOutMod.F90
  ${noahmp_dir}/BiochemVarType.F90
  ${noahmp_dir}/CanopyHydrologyMod.F90
  ${noahmp_dir}/CanopyRadiationTwoStreamMod.F90
  ${noahmp_dir}/CanopyWaterInterceptMod.F90
  ${noahmp_dir}/CarbonFluxCropMod.F90
  ${noahmp_dir}/CarbonFluxNatureVegMod.F90
  ${noahmp_dir}/ConfigVarInitMod.F90
  ${noahmp_dir}/ConfigVarOutMod.F90
  ${noahmp_dir}/ConfigVarType.F90
  ${noahmp_dir}/ConstantDefineMod.F90
  ${noahmp_dir}/CropGrowDegreeDayMod.F90
  ${noahmp_dir}/CropPhotosynthesisMod.F90
  ${noahmp_dir}/EnergyMainGlacierMod.F90
  ${noahmp_dir}/EnergyMainMod.F90
  ${noahmp_dir}/EnergyVarInitMod.F90
  ${noahmp_dir}/EnergyVarOutMod.F90
  ${noahmp_dir}/EnergyVarType.F90
  ${noahmp_dir}/ForcingVarInitMod.F90
  ${noahmp_dir}/ForcingVarType.F90
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
  ${noahmp_dir}/NoahmpVarType.F90
  ${noahmp_dir}/PedoTransferSR2006.F90
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
  ${noahmp_dir}/SnowfallGlacierMod.F90
  ${noahmp_dir}/SnowLayerCombineGlacierMod.F90
  ${noahmp_dir}/SnowLayerCombineMod.F90
  ${noahmp_dir}/SnowLayerDivideGlacierMod.F90
  ${noahmp_dir}/SnowLayerDivideMod.F90
  ${noahmp_dir}/SnowLayerWaterComboMod.F90
  ${noahmp_dir}/SnowpackCompactionGlacierMod.F90
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
  ${noahmp_dir}/SoilWaterSupercoolLiquidMod.F90
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
  ${noahmp_dir}/WaterVarOutMod.F90
  ${noahmp_dir}/WaterVarType.F90) # end noahmp_refactor add_library

add_library(noahmp_refactor_wrf
  ${noahmp_wrf}/module_bep_bem_helper.F
  ${noahmp_wrf}/module_sf_bem.F
  ${noahmp_wrf}/module_sf_bep_bem.F
  ${noahmp_wrf}/module_sf_bep.F
  ${noahmp_wrf}/module_sf_urban.F
  ${noahmp_wrf}/NoahmpUrbanDriverMainMod.F)

add_dependencies(
  noahmp_refactor_noahmp
  noahmp_refactor_hrldas
  )

add_dependencies(
  noahmp_refactor_wrf
  noahmp_refactor_noahmp
  )


add_library(noahmp_refactor INTERFACE)
target_link_libraries(noahmp_refactor
  INTERFACE
  noahmp_refactor_hrldas
  noahmp_refactor_noahmp
  noahmp_refactor_wrf)


# add_library(noahmp_refactor
#   noahmp_refactor_hrldas
#   noahmp_refactor_noahmp)

# add_executable(jedi_snow_increment jedi_snow_incr.f90)

# target_link_libraries(jedi_snow_increment
#   PUBLIC jedi_disag var_names NetCDF::NetCDF_Fortran MPI::MPI_Fortran)


# add_subdirectory("NoahMP_WRF_H")
