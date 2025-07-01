#' @title makeViews
#'
#' @description Internal function that runs within importData that creates the views for each
#' NGPN-specific sampling protocols of FFI. Views are Cover_Points_Metric, Cover_Species_Composition,
#' Density_Belts_Metric, Density_Quadrats_Metric, Disturbance_History, MacroPlot_SampleEvents,
#' Surface_Fuels_1000Hr, Surface_Fuels_Fine, Surface_Fuels_Duff, Taxa_Table, Trees_Metric.
#' Function only compiles data for plots with "PCM", "FPCM", "LPCM", or "RCM" in the MacroPlot_Name,
#' and starts at 2011, the year NGPN-specific monitoring began. If new_env = TRUE in importData(),
#' views will be saved to VIEWS_NGPN environment. If new_env = F, views will be saved to global environment.
#'
#' @importFrom dplyr filter left_join right_join
#'
#' @examples
#' \dontrun{
#'
#' # makeViews() runs within importData().
#' library(plantcomNGPN)
#' importData(type = 'local',
#'   dbname = c("FFI_RA_AGFO", "FFI_RA_BADL", "FFI_RA_DETO", "FFI_RA_FOLA",
#'              "FFI_RA_FOUS", "FFI_RA_JECA", "FFI_RA_KNRI", "FFI_RA_MNRR",
#'              "FFI_RA_MORU", "FFI_RA_SCBL", "FFI_RA_THRO", "FFI_RA_WICA"))
#'
#' }
#'
#' @return Returns flattened views of NGPN fire effects data
#'
#' @export

makeViews <- function(){

  env <- if(exists("NGPN_tables")){NGPN_tables} else {.GlobalEnv}

  #---- MacroPlot_SampleEvent View -----
  #### Compile MacroPlot data ####
  tryCatch(
    macro_orig <- get("MacroPlot", envir = env),
    error = function(e){stop("MacroPlot table not found. Please import NGPN FFI data.")})
  tryCatch(
    mm_projunit <- get("MM_ProjectUnit_MacroPlot", envir = env),
    error = function(e){stop("MM_ProjectUnit_MacroPlot table not found. Please import NGPN FFI data tables.")})
  tryCatch(
    projunit <- get("ProjectUnit", envir = env),
    error = function(e){stop("ProjectUnit table not found. Please import NGPN FFI data tables.")})
  tryCatch(
    regunit <- get("RegistrationUnit", envir = env),
    error = function(e){stop("RegistrationUnit table not found. Please import NGPN FFI data tables.")})

  # cleanup project and projectunit data
  projunit$ProjectUnit_Agency <- "NPS"
  NGPN_plots <- macro_orig$MacroPlot_Name[grepl("_PCM_|_LPCM_|_FPCM_|_RCM_", macro_orig$MacroPlot_Name)]
  macro <- macro_orig[macro_orig$MacroPlot_Name %in% NGPN_plots,]

  # Joining macroplot-relevant tables
  macro1 <- left_join(macro, mm_projunit,
                      by = c("MacroPlot_GUID" = "MM_MacroPlot_GUID", "datasource"))
  macro2 <- left_join(macro1, regunit, by = c("MacroPlot_RegistrationUnit_GUID" = "RegistrationUnit_GUID", "datasource"))
  macro3 <- left_join(macro2, projunit,
                      by = c("MacroPlot_RegistrationUnit_GUID" = "ProjectUnit_RegistrationUnitGUID",
                             "MM_ProjectUnit_GUID" = "ProjectUnit_GUID",
                             "datasource"))

  # hacky way to keep tblname_UV1 as is
  names(macro3)[names(macro3) == "MacroPlot_UV1"] <- "MacroPlotUV1"
  names(macro3)[names(macro3) == "MacroPlot_UV2"] <- "MacroPlotUV2"
  names(macro3)[names(macro3) == "MacroPlot_UV3"] <- "MacroPlotUV3"
  names(macro3)[names(macro3) == "MacroPlot_UV4"] <- "MacroPlotUV4"
  names(macro3)[names(macro3) == "MacroPlot_UV5"] <- "MacroPlotUV5"
  names(macro3)[names(macro3) == "MacroPlot_UV6"] <- "MacroPlotUV6"
  names(macro3)[names(macro3) == "MacroPlot_UV7"] <- "MacroPlotUV7"
  names(macro3)[names(macro3) == "MacroPlot_UV8"] <- "MacroPlotUV8"
  names(macro3)[names(macro3) == "MacroPlot_GUID"] <- "MacroPlotGUID"
  names(macro3)[names(macro3) == "MacroPlot_Comment"] <- "MacroPlotComment"
  names(macro3)[names(macro3) == "MacroPlot_Name"] <- "MacroPlotName"
  names(macro3)[names(macro3) == "MacroPlot_Purpose"] <- "MacroPlotPurpose"
  names(macro3)[names(macro3) == "MacroPlot_Type"] <- "MacroPlotType"
  names(macro3)[names(macro3) == "ProjectUnit_Name"] <- "ProjectUnitName"
  names(macro3)[names(macro3) == "MM_ProjectUnit_GUID"] <- "MM_ProjectUnitGUID"
  names(macro3)[names(macro3) == "RegistrationUnit_GUID"] <- "RegUnitGUID"

  # Drop table names from most column names for easier coding
  names(macro3) <-
    gsub("^MacroPlot_|^ProjectUnit_|^Registration", "", names(macro3))

  # Add the _ back
  names(macro3) <- gsub("MacroPlot", "MacroPlot_", names(macro3))
  names(macro3) <- gsub("ProjectUnit", "ProjectUnit_", names(macro3))
  names(macro3) <- gsub("RegUnit", "RegistrationUnit_", names(macro3))

  # Compile final dataset
  keep_cols_macro <-
    c("MacroPlot_Name", "Unit_Name", "MacroPlot_Purpose", "MacroPlot_Type",
      "ProjectUnit_Name", "Agency", "UTM_X", "UTM_Y", "UTMzone", "Datum",
      "DD_Lat", "DD_Long", "Elevation", "ElevationUnits", "Azimuth", "Aspect",
      "SlopeHill", "SlopeTransect", "MacroPlot_UV1", "MacroPlot_UV2", "MacroPlot_UV3",
      "MacroPlot_UV4", "MacroPlot_UV5", "MacroPlot_UV6", "MacroPlot_UV7", "MacroPlot_UV8",
      "Metadata", "StartPoint", "Directions", "MacroPlot_Comment","Unit_Comment", "Description",
      "MacroPlot_GUID", "RegistrationUnit_GUID", "MM_ProjectUnit_GUID", "datasource")

  macroplot <- macro3[,keep_cols_macro]

  #### Compile Sample Event Data ####
  tryCatch(
    monstat <- get("MonitoringStatus", envir = env) |> select(-datasource),
    error = function(e){stop("MonitoringStatus table not found. Please import NGPN FFI data tables.")})
  tryCatch(
    mm_monstat_se <- get("MM_MonitoringStatus_SampleEvent", envir = env) |> select(-datasource),
    error = function(e){stop("MM_MonitoringStatus_SampleEvent table not found. Please import NGPN FFI data tables.")})
  tryCatch(
    sampev <- get("SampleEvent", envir = env) |> select(-datasource),
    error = function(e){stop("SampleEvent table not found. Please import NGPN FFI data tables.")})

  # Use to make some tables smaller before join
  macro_guids <- unique(macroplot$MacroPlot_GUID)

  # Fix typos in MonitoringStatus_Name and MonitoringStatus_Base
  monstat$MonitoringStatus_Name[monstat$MonitoringStatus_Name == "2009_Plant Community"] <- "2009_PlantCommunity"
  monstat$MonitoringStatus_Name[monstat$MonitoringStatus_Name == "2018_Plant Community"] <- "2018_PlantCommunity"
  monstat$MonitoringStatus_Name[monstat$MonitoringStatus_Name == "2024_Plant Community"] <- "2024_PlantCommunity"

  monstat$MonitoringStatus_Base[monstat$MonitoringStatus_Base == "2016_"] <- "PlantCommunity"
  monstat$MonitoringStatus_Base[monstat$MonitoringStatus_Base %in% c("Plant Community", "_PlantCommunity")] <- "PlantCommunity"
  monstat$MonitoringStatus_Base[monstat$MonitoringStatus_Base %in% c("2018_PlantCommunity", "2019_PlantCommunity")] <- "PlantCommunity"
  monstat$MonitoringStatus_Base[monstat$MonitoringStatus_Base %in% c("_Riparian")] <- "Riparian"
  monstat$MonitoringStatus_Base[monstat$MonitoringStatus_Base %in% c("_ForestStructure")] <- "ForestStructure"
  monstat$MonitoringStatus_Base[monstat$MonitoringStatus_Base %in% c("_FirePlantCommunity")] <- "FirePlantCommunity"

  sampev2 <- left_join(macroplot, sampev, by = c("MacroPlot_GUID" = "SampleEvent_Plot_GUID"),
                        relationship = 'many-to-many') #MM b/c plots are used for multiple projects
  sampev3 <- left_join(sampev2, mm_monstat_se, by = c("SampleEvent_GUID" = "MM_SampleEvent_GUID"),
                           relationship = 'many-to-many')
  sampev4 <- left_join(sampev3, monstat,
                                by = c("MM_MonitoringStatus_GUID" = "MonitoringStatus_GUID",
                                       "MM_ProjectUnit_GUID" = "MonitoringStatus_ProjectUnit_GUID"))
  sampev4$SampleEvent_Date <-
    format(as.Date(sampev4$SampleEvent_Date, format = "%Y-%m-%d %H:%m:%s"),
           "%Y-%m-%d")
  sampev4$year <- format(as.Date(sampev4$SampleEvent_Date, format = "%Y-%m-%d"), "%Y")
  sampev4$month <- format(as.Date(sampev4$SampleEvent_Date, format = "%Y-%m-%d"), "%m")
  sampev4$doy <- format(as.Date(sampev4$SampleEvent_Date, format = "%Y-%m-%d"), "%j")

  # drop plots with no associated sample events
  # unique(sampev4$MacroPlot_Name[is.na(sampev4$SampleEvent_GUID)]) # Plots with no sample events
  sampev5 <- sampev4[!is.na(sampev4$SampleEvent_GUID),]

  names(sampev5)[names(sampev5) == "SampleEvent_GUID"] <- "SampleEventGUID"
  names(sampev5)[names(sampev5) == "SampleEvent_Date"] <- "SampleEventDate"
  names(sampev5)[names(sampev5) == "SampleEvent_UV1"] <- "SampleEventUV1"
  names(sampev5)[names(sampev5) == "SampleEvent_Comment"] <- "SampleEventComment"

  # Drop table names from most column names for easier coding
  names(sampev5) <-
    gsub("^SampleEvent_", "", names(sampev5))

  # Add the _ back
  names(sampev5) <- gsub("SampleEvent", "SampleEvent_", names(sampev5))

  # Drop data before 2011
  sampev6 <- sampev5[sampev5$year >= 2011,]

  keep_cols_samp <- c("SampleEvent_Date", "year", "month", "doy",
                      "SampleEvent_UV1", "DefaultMonitoringStatus",
                      "TreatmentUnit", "MonitoringStatus_Prefix", "MonitoringStatus_Base",
                      "MonitoringStatus_Suffix", "MonitoringStatus_Name",
                      "MonitoringStatus_Comment", "SampleEvent_Comment",
                      "SampleEvent_GUID", "MM_MonitoringStatus_GUID")

  MacroPlot_SampleEvents <- sampev6[order(sampev6$MacroPlot_Name, sampev6$SampleEvent_Date),
                                    c(keep_cols_macro[1:32], keep_cols_samp,
                                      keep_cols_macro[33:length(keep_cols_macro)])] # puts GUIDs last

  #---- Taxa_Table View----
  tryCatch(localspp <- get("LocalSpecies", envir = env),
           error = function(e){stop("LocalSpecies table not found. Please import NGPN FFI data tables.")})
  tryCatch(mastspp <- get("MasterSpecies", envir = env),
           error = function(e){stop("MasterSpecies table not found. Please import NGPN FFI data tables.")})
  tryCatch(auxspp <- get("AuxSpecies", envir = env),
           error = function(e){stop("AuxSpecies table not found. Please import NGPN FFI data tables.")})
  tryCatch(lifeform <- get("LU_LifeForm", envir = env),
           error = function(e){stop("LU_LifeForm table not found. Please import NGPN FFI data tables.")})
  # NGPN does not appear to use the SpeciesPickList, so not including here.
  # lifecycle doesn't appear to be used much by NGPN, so not including it here.

  # Table joins
  locspp_reg <- left_join(localspp, regunit,
                          by = c("LocalSpecies_RegistrationUnitGUID" = "RegistrationUnit_GUID", "datasource"))

  spp1 <- left_join(locspp_reg, auxspp,
                    by = c("LocalSpecies_AuxSpeciesGUID" = "AuxSpecies_GUID",
                           "LocalSpecies_RegistrationUnitGUID" = "AuxSpecies_RegistrationUnitGUID",
                           "datasource"))

  spp2 <- left_join(spp1, mastspp,
                    by = c("LocalSpecies_MasterSpeciesGUID" = "MasterSpecies_GUID", 'datasource'))

  spp3 <- left_join(spp2, lifeform,
                    by = c("LocalSpecies_PreferedLifeForm_GUID" = "LU_LifeForm_GUID", "datasource"))

  # Merge Master and Aux species list to return complete species list.
  spp3$ScientificName <- ifelse(is.na(spp3$MasterSpecies_ScientificName), spp3$AuxSpecies_ScientificName,
                                spp3$MasterSpecies_ScientificName)
  spp3$ITIS_TSN <- ifelse(is.na(spp3$MasterSpecies_ITIS_TSN), spp3$AuxSpecies_ITIS_TSN,
                          spp3$MasterSpecies_ITIS_TSN)

  spp3$Family <- ifelse(is.na(spp3$MasterSpecies_Family), spp3$AuxSpecies_Family,
                        spp3$MasterSpecies_Family)

  spp3$Genus <- ifelse(is.na(spp3$MasterSpecies_Genus), spp3$AuxSpecies_Genus,
                       spp3$MasterSpecies_Genus)

  spp3$Symbol <- ifelse(is.na(spp3$MasterSpecies_Symbol), spp3$AuxSpecies_Symbol,
                        spp3$MasterSpecies_Symbol)

  spp3$NotBiological <- ifelse(is.na(spp3$MasterSpecies_NotBiological), spp3$AuxSpecies_NotBiological,
                               spp3$MasterSpecies_NotBiological)

  spp3$Nativity <- ifelse(is.na(spp3$LocalSpecies_Nativity), spp3$MasterSpecies_Nativity,
                          spp3$LocalSpecies_Nativity)

  spp3$CommonName <- ifelse(is.na(spp3$LocalSpecies_CommonName), spp3$MasterSpecies_CommonName,
                            spp3$LocalSpecies_CommonName)

  spp4 <- spp3 |> select(-MasterSpecies_ScientificName, -AuxSpecies_ScientificName,
                         -MasterSpecies_ITIS_TSN, -AuxSpecies_ITIS_TSN,
                         -MasterSpecies_Family, -AuxSpecies_Family,
                         -MasterSpecies_Genus, -AuxSpecies_Genus,
                         -MasterSpecies_Symbol, -LocalSpecies_Symbol, -AuxSpecies_Symbol,
                         -MasterSpecies_NotBiological, -AuxSpecies_NotBiological,
                         -LocalSpecies_Nativity, -MasterSpecies_Nativity,
                         -LocalSpecies_CommonName, -MasterSpecies_CommonName)

  # hacky way to keep LocalSpecies_UV1 as is
  names(spp4)[names(spp4) == "LocalSpecies_GUID"] <- "Spp_GUID"
  names(spp4)[names(spp4) == "LocalSpecies_UV1"] <- "Species_UV1"
  names(spp4)[names(spp4) == "LocalSpecies_Description"] <- "Species_Description"
  names(spp4)[names(spp4) == "LocalSpecies_Comment"] <- "Species_Comment"

  # Drop table names from column names for easier coding
  names(spp4) <-
    gsub("^MacroPlot_|^LocalSpecies_|^MasterSpecies_|^LU_|^AuxSpecies_|^Registration", "", names(spp4))

  names(spp4)[names(spp4) == "UnitGUID"] <- "RegistrationUnitGUID"

  keep_cols_taxa <- c("Symbol", "ITIS_TSN", "ScientificName", "CommonName", "Family", "Genus",
                      "Nativity", "Invasive", "Cultural", "Concern", "LifeCycle",
                      "LifeForm_Name", "NotBiological", "UserAdded", "Species_UV1",
                      "IsUnknown", "IsUnlisted", "Species_Description", "Species_Comment", "Unit_Name",
                      "SymbolKey", "Synonym_SymbolKey", "Spp_GUID", "RegistrationUnitGUID",
                      "MasterSpeciesGUID", "AuxSpeciesGUID", "PLANTS_GUID")

  Taxa_Table <- spp4[order(spp4$Symbol, spp4$Unit_Name), keep_cols_taxa]

  #---- Cover_Points_Metric View ----
  covpts_samp1 <-   tryCatch(get("Cover_Points_metric_Sample", envir = env),
                             error = function(e){
                               stop("Cover_Points_metric_Sample table not found. Please import NGPN FFI data tables.")})
  covpts_attr1 <- tryCatch(get("Cover_Points_metric_Attribute", envir = env),
                           error = function(e){
                             stop("Cover_Points_metric_Attribute table not found. Please import NGPN FFI data tables.")})

  # Making tables smaller before joins
  sampev_guids <- unique(MacroPlot_SampleEvents$SampleEvent_GUID)
  covpts_samp <- covpts_samp1[covpts_samp1$SampleData_SampleEvent_GUID %in% sampev_guids,]
  samprow_guids <- unique(covpts_samp$SampleData_SampleRow_GUID)
  covpts_attr2 <- covpts_attr1[covpts_attr1$AttributeData_SampleRow_GUID %in% samprow_guids,]
  # Drop records where Index is blank b/c causes issues in the join
  #covpts_attr <- covpts_attr2[!is.na(covpts_attr2$Index),]

  samp_covs <- left_join(MacroPlot_SampleEvents, covpts_samp,
                         by = c("SampleEvent_GUID" = "SampleData_SampleEvent_GUID", "datasource"))

  samp_cova <- left_join(samp_covs, covpts_attr2,
                         by = c("SampleData_SampleRow_GUID" = "AttributeData_SampleRow_GUID",
                                "datasource"),
                         relationship = 'many-to-many') # b/c multiple projects/macroplot

  samp_cov_spp <- left_join(samp_cova, Taxa_Table,
                            by = c("Spp_GUID", "Unit_Name", "RegistrationUnit_GUID" = "RegistrationUnitGUID"))

  cols_view_start <- c("MacroPlot_Name", "Unit_Name", "MacroPlot_Purpose", "ProjectUnit_Name", "Elevation",
                       "Azimuth", "Aspect", "SlopeHill", "SlopeTransect", "SampleEvent_Date", "year")
  cols_view_end <- c("UV1Desc", "UV2Desc", "UV3Desc", "SaComment",
                     "MacroPlot_GUID", "SampleEvent_GUID", "MM_MonitoringStatus_GUID", "RegistrationUnit_GUID",
                     "MM_ProjectUnit_GUID", "Spp_GUID")
  cols_taxa_start <- c("Symbol", "ITIS_TSN", "ScientificName", "CommonName")
  cols_taxa_end <- c("Nativity", "Invasive", "Cultural", "Concern", "LifeCycle", "LifeForm_Name",
                     "NotBiological", "Species_Description", "Species_Comment")
  cols_covpt <- c("Visited", "NumTran", "TranLen", 'NumPtsTran', "Offset",
                  "Index", "Transect", "Point", "Tape", "Order", "Height", "Status")

  Cover_Points_Metric <- samp_cov_spp[order(samp_cov_spp$MacroPlot_Name, samp_cov_spp$year,
                                            samp_cov_spp$Index, samp_cov_spp$ScientificName),
                                      c(cols_view_start, cols_taxa_start,
                                        cols_covpt,
                                        cols_taxa_end, cols_view_end)]

  #---- Cover_Species_Composition View ----
  covcomp_samp1 <-   tryCatch(get("Cover_SpeciesComposition_metric_Sample", envir = env),
                             error = function(e){
                               stop("Cover_SpeciesComposition_metric_Sample table not found. Please import NGPN FFI data tables.")})
  covcomp_attr1 <- tryCatch(get("Cover_SpeciesComposition_metric_Attribute", envir = env),
                           error = function(e){
                             stop("Cover_Points_metric_Attribute table not found. Please import NGPN FFI data tables.")})

  # Making tables smaller before joins
  covcomp_samp <- covcomp_samp1[covcomp_samp1$SampleData_SampleEvent_GUID %in% sampev_guids,]
  samprow_guids <- unique(covcomp_samp$SampleData_SampleRow_GUID)
  covcomp_attr2 <- covcomp_attr1[covcomp_attr1$AttributeData_SampleRow_GUID %in% samprow_guids,]

  samp_comps <- left_join(MacroPlot_SampleEvents, covcomp_samp,
                          by = c("SampleEvent_GUID" = "SampleData_SampleEvent_GUID", "datasource"))

  samp_compa <- left_join(samp_comps, covcomp_attr2,
                         by = c("SampleData_SampleRow_GUID" = "AttributeData_SampleRow_GUID",
                                "datasource"),
                         relationship = 'many-to-many') # b/c multiple projects/macroplot

  samp_comp_spp <- left_join(samp_compa, Taxa_Table,
                             by = c("Spp_GUID", "Unit_Name", "RegistrationUnit_GUID" = "RegistrationUnitGUID"))

  cols_covcomp <- c("Index", "Status", "SizeCl", "AgeCl", "Cover", "Height", "Comment", "UV1", "UV2", "UV3")

  Cover_Species_Composition <-
    samp_comp_spp[order(samp_comp_spp$MacroPlot_Name, samp_comp_spp$year,
                        samp_comp_spp$Index, samp_comp_spp$ScientificName),
                 c(cols_view_start, cols_taxa_start,
                   cols_covcomp,
                   cols_taxa_end, cols_view_end)]

  #---- Density_Belts_Metric View ----
  densbelt_samp1 <-   tryCatch(get("Density_Belts_metric_Sample", envir = env),
                              error = function(e){
                                stop("Density_Belts_metric_Sample table not found. Please import NGPN FFI data tables.")})
  densbelt_attr1 <- tryCatch(get("Density_Belts_metric_Attribute", envir = env),
                            error = function(e){
                              stop("Density_Belts_metric_Attribute table not found. Please import NGPN FFI data tables.")})

  # Making tables smaller before joins
  densbelt_samp <- densbelt_samp1[densbelt_samp1$SampleData_SampleEvent_GUID %in% sampev_guids,]
  samprow_guids <- unique(densbelt_samp$SampleData_SampleRow_GUID)
  densbelt_attr2 <- densbelt_attr1[densbelt_attr1$AttributeData_SampleRow_GUID %in% samprow_guids,]

  samp_densbs <- left_join(MacroPlot_SampleEvents, densbelt_samp,
                          by = c("SampleEvent_GUID" = "SampleData_SampleEvent_GUID", "datasource"))

  samp_densba <- left_join(samp_densbs, densbelt_attr2,
                          by = c("SampleData_SampleRow_GUID" = "AttributeData_SampleRow_GUID",
                                 "datasource"),
                          relationship = 'many-to-many') # b/c multiple projects/macroplot

  samp_densb_spp <- left_join(samp_densba, Taxa_Table,
                             by = c("Spp_GUID", "Unit_Name", "RegistrationUnit_GUID" = "RegistrationUnitGUID"))

  cols_densbelt <- c("Visited", "NumTran", "NumSubbelt", "TranLen", "TranWid", "Area",
                     "Index", "Transect", "Subbelt", "Status", "SizeCl", "AgeCl",
                     "Count", "Height", "SubFrac", "Comment", "UV1", "UV2", "UV3")

  Density_Belt_Transect <-
    samp_densb_spp[order(samp_densb_spp$MacroPlot_Name, samp_densb_spp$year,
                         samp_densb_spp$Index, samp_densb_spp$ScientificName),
                  c(cols_view_start, cols_taxa_start,
                    cols_densbelt,
                    cols_taxa_end, cols_view_end)]

  #---- Density_Quadrats_Metric View ----
  densquad_samp1 <-   tryCatch(get("Density_Quadrats_metric_Sample", envir = env),
                               error = function(e){
                                 stop("Density_Quadrats_metric_Sample table not found. Please import NGPN FFI data tables.")})
  densquad_attr1 <- tryCatch(get("Density_Quadrats_metric_Attribute", envir = env),
                             error = function(e){
                               stop("Density_Quadrats_metric_Attribute table not found. Please import NGPN FFI data tables.")})

  # Making tables smaller before joins
  densquad_samp <- densquad_samp1[densquad_samp1$SampleData_SampleEvent_GUID %in% sampev_guids,]
  samprow_guids <- unique(densquad_samp$SampleData_SampleRow_GUID)
  densquad_attr2 <- densquad_attr1[densquad_attr1$AttributeData_SampleRow_GUID %in% samprow_guids,]

  samp_densqs <- left_join(MacroPlot_SampleEvents, densquad_samp,
                           by = c("SampleEvent_GUID" = "SampleData_SampleEvent_GUID", "datasource"))

  samp_densqa <- left_join(samp_densqs, densquad_attr2,
                           by = c("SampleData_SampleRow_GUID" = "AttributeData_SampleRow_GUID",
                                  "datasource"),
                           relationship = 'many-to-many') # b/c multiple projects/macroplot

  samp_densq_spp <- left_join(samp_densqa, Taxa_Table,
                              by = c("Spp_GUID", "Unit_Name", "RegistrationUnit_GUID" = "RegistrationUnitGUID"))

  cols_densquad <- c("Visited", "NumTran", "NumQuadTran", "QuadLen", "QuadWid", "Area",
                     "Index", "Transect", "Quadrat", "Status", "SizeCl", "AgeCl",
                     "Count", "Height", "SubFrac", "Comment", "UV1", "UV2", "UV3")

  Density_quad_Transect <-
    samp_densq_spp[order(samp_densq_spp$MacroPlot_Name, samp_densq_spp$year,
                         samp_densq_spp$Index, samp_densq_spp$ScientificName),
                   c(cols_view_start, cols_taxa_start,
                     cols_densquad,
                     cols_taxa_end, cols_view_end)]

  #---- Disturbance_History View ----
  disthist_samp1 <-   tryCatch(get("DisturbanceHistory_Sample", envir = env),
                               error = function(e){
                                 stop("DisturbanceHistory_Sample table not found. Please import NGPN FFI data tables.")})
  disthist_attr1 <- tryCatch(get("DisturbanceHistory_Attribute", envir = env),
                             error = function(e){
                               stop("DisturbanceHistory_Attribute table not found. Please import NGPN FFI data tables.")})

  # Making tables smaller before joins
  disthist_samp <- disthist_samp1[disthist_samp1$SampleData_SampleEvent_GUID %in% sampev_guids,]
  samprow_guids <- unique(disthist_samp$SampleData_SampleRow_GUID)
  disthist_attr2 <- disthist_attr1[disthist_attr1$AttributeData_SampleRow_GUID %in% samprow_guids,]

  samp_dists <- left_join(MacroPlot_SampleEvents, disthist_samp,
                           by = c("SampleEvent_GUID" = "SampleData_SampleEvent_GUID", "datasource"))

  samp_dista <- left_join(samp_dists, disthist_attr2,
                           by = c("SampleData_SampleRow_GUID" = "AttributeData_SampleRow_GUID",
                                  "datasource"),
                           relationship = 'many-to-many') # b/c multiple projects/macroplot

  cols_dist <- c("Index", "ChAgent", "SevCode", "StartYr", "StartMo", "StartDy",
                 "EndYr", "EndMo", "EndDy", "DatePrec", "ChgDesc", "Comment",
                 "UV1", "UV2", "UV3")

  cols_view_end_nospp <- cols_view_end[!cols_view_end %in% "Spp_GUID"]

  Disturbance_History <-
    samp_dista[order(samp_dista$MacroPlot_Name, samp_dista$year,
                     samp_dista$Index),
                   c(cols_view_start, cols_dist, cols_view_end_nospp)]


  #---- Surface_Fuels_1000Hr View ----
  surf1000_samp1 <- tryCatch(get("SurfaceFuels_1000Hr_Sample", envir = env),
                             error = function(e){
                               stop("SurfaceFuels_1000Hr_Sample table not found. Please import NGPN FFI data tables.")})
  surf1000_attr1 <- tryCatch(get("SurfaceFuels_1000Hr_Attribute", envir = env),
                             error = function(e){
                               stop("SurfaceFuels_1000Hr_Attribute table not found. Please import NGPN FFI data tables.")})

  # Making tables smaller before joins
  surf1000_samp <- surf1000_samp1[surf1000_samp1$SampleData_SampleEvent_GUID %in% sampev_guids,]
  samprow_guids <- unique(surf1000_samp$SampleData_SampleRow_GUID)
  surf1000_attr2 <- surf1000_attr1[surf1000_attr1$AttributeData_SampleRow_GUID %in% samprow_guids,]

  samp_surf1000s <- left_join(MacroPlot_SampleEvents, surf1000_samp,
                          by = c("SampleEvent_GUID" = "SampleData_SampleEvent_GUID", "datasource"))

  samp_surf1000a <- left_join(samp_surf1000s, surf1000_attr2,
                          by = c("SampleData_SampleRow_GUID" = "AttributeData_SampleRow_GUID",
                                 "datasource"),
                          relationship = 'many-to-many') # b/c multiple projects/macroplot

  cols_surf1000 <- c("Visited", "NumTran", "TranLen", "Index", "Transect", "Slope", "LogNum", "Dia",
                     "DecayCl", "CWDFuConSt", "Comment", "UV1", "UV2", "UV3")

  Surface_Fuels_1000Hr <-
    samp_surf1000a[order(samp_surf1000a$MacroPlot_Name, samp_surf1000a$year,
                     samp_surf1000a$Index),
               c(cols_view_start, cols_surf1000, cols_view_end_nospp)]

  #---- Surface_Fuels_Fine View ----
  surffine_samp1 <-   tryCatch(get("SurfaceFuels_Fine_Sample", envir = env),
                               error = function(e){
                                 stop("SurfaceFuels_Fine_Sample table not found. Please import NGPN FFI data tables.")})
  surffine_attr1 <- tryCatch(get("SurfaceFuels_Fine_Attribute", envir = env),
                             error = function(e){
                               stop("SurfaceFuels_Fine_Attribute table not found. Please import NGPN FFI data tables.")})

  # Making tables smaller before joins
  surffine_samp <- surffine_samp1[surffine_samp1$SampleData_SampleEvent_GUID %in% sampev_guids,]
  samprow_guids <- unique(surffine_samp$SampleData_SampleRow_GUID)
  surffine_attr2 <- surffine_attr1[surffine_attr1$AttributeData_SampleRow_GUID %in% samprow_guids,]

  samp_surffines <- left_join(MacroPlot_SampleEvents, surffine_samp,
                              by = c("SampleEvent_GUID" = "SampleData_SampleEvent_GUID", "datasource"))

  samp_surffinea <- left_join(samp_surffines, surffine_attr2,
                              by = c("SampleData_SampleRow_GUID" = "AttributeData_SampleRow_GUID",
                                     "datasource"),
                              relationship = 'many-to-many') # b/c multiple projects/macroplot
  names(samp_surffinea)[names(samp_surffinea) == "Azimuth.x"] <- "Azimuth"
  names(samp_surffinea)[names(samp_surffinea) == "Azimuth.y"] <- "Azimuth_Fuels"

  cols_surffine <- c("Visited", "NumTran", "OneHrTranLen", "TenHrTranLen", "HunHrTranLen",
                     "Index", "Transect", "Azimuth_Fuels", "Slope", "OneHr", "TenHr", "HunHr", "FWDFuConSt",
                     "Comment", "UV1", "UV2", "UV3")

  Surface_Fuels_Fine <-
    samp_surffinea[order(samp_surffinea$MacroPlot_Name, samp_surffinea$year,
                         samp_surffinea$Index),
                   c(cols_view_start, cols_surffine, cols_view_end_nospp)]

  #---- Surface_Fuels_Duff View ----
  surfduff_samp1 <-   tryCatch(get("SurfaceFuels_Duff_Litter_Sample", envir = env),
                               error = function(e){
                                 stop("SurfaceFuels_Duff_Litter_Sample table not found. Please import NGPN FFI data tables.")})
  surfduff_attr1 <- tryCatch(get("SurfaceFuels_Duff_Litter_Attribute", envir = env),
                             error = function(e){
                               stop("SurfaceFuels_Duff_Litter_Attribute table not found. Please import NGPN FFI data tables.")})

  # Making tables smaller before joins
  surfduff_samp <- surfduff_samp1[surfduff_samp1$SampleData_SampleEvent_GUID %in% sampev_guids,]
  samprow_guids <- unique(surfduff_samp$SampleData_SampleRow_GUID)
  surfduff_attr2 <- surfduff_attr1[surfduff_attr1$AttributeData_SampleRow_GUID %in% samprow_guids,]

  samp_surfduffs <- left_join(MacroPlot_SampleEvents, surfduff_samp,
                              by = c("SampleEvent_GUID" = "SampleData_SampleEvent_GUID", "datasource"))

  samp_surfduffa <- left_join(samp_surfduffs, surfduff_attr2,
                              by = c("SampleData_SampleRow_GUID" = "AttributeData_SampleRow_GUID",
                                     "datasource"),
                              relationship = 'many-to-many') # b/c multiple projects/macroplot

  cols_surfduff <- c("Visited", "NumTran", "Index", "Transect", "SampLoc", "OffSet", "LittDep",
                     "DuffDep", "FuelbedDep", "DLFuConSt", "Comment", "UV1", "UV2", "UV3")

  Surface_Fuels_Duff <-
    samp_surfduffa[order(samp_surfduffa$MacroPlot_Name, samp_surfduffa$year,
                         samp_surfduffa$Index),
                   c(cols_view_start, cols_surfduff, cols_view_end_nospp)]

  #---- Trees_Metric ----
  tree_samp1 <-   tryCatch(get("Trees_Individuals_metric_Sample", envir = env),
                           error = function(e){
                             stop("Trees_Individuals_metric_Sample table not found. Please import NGPN FFI data tables.")})
  tree_attr1 <- tryCatch(get("Trees_Individuals_metric_Attribute", envir = env),
                         error = function(e){
                           stop("Trees_Individuals_metric_Attribute table not found. Please import NGPN FFI data tables.")})

  # Making tables smaller before joins
  tree_samp <- tree_samp1[tree_samp1$SampleData_SampleEvent_GUID %in% sampev_guids,]
  samprow_guids <- unique(tree_samp$SampleData_SampleRow_GUID)
  tree_attr <- tree_attr1[tree_attr1$AttributeData_SampleRow_GUID %in% samprow_guids,]

  # Not all parks/plots have tree data associated, making the left_joins bring in a bunch of blank rows.
  # Using all plots with a tree recorded in the tree_samp1 to filter out non-tree plots
  samp_treesrj <- right_join(MacroPlot_SampleEvents, tree_samp,
                             by = c("SampleEvent_GUID" = "SampleData_SampleEvent_GUID", "datasource"))
  samp_treearj <- right_join(samp_treesrj, tree_attr2,
                          by = c("SampleData_SampleRow_GUID" = "AttributeData_SampleRow_GUID",
                                 "datasource"),
                          relationship = 'many-to-many') # b/c multiple projects/macroplot
  tree_samp_plots <- sort(unique(samp_treearj$MacroPlot_Name))

  samp_trees <- left_join(MacroPlot_SampleEvents, tree_samp,
                          by = c("SampleEvent_GUID" = "SampleData_SampleEvent_GUID", "datasource")) |>
    filter(MacroPlot_Name %in% tree_samp_plots)

  samp_treea <- left_join(samp_trees, tree_attr2,
                           by = c("SampleData_SampleRow_GUID" = "AttributeData_SampleRow_GUID",
                                  "datasource"),
                           relationship = 'many-to-many') # b/c multiple projects/macroplot

  samp_tree_spp <- left_join(samp_treea, Taxa_Table,
                              by = c("Spp_GUID", "Unit_Name", "RegistrationUnit_GUID" = "RegistrationUnitGUID"))

  cols_tree <- c("QTR", "SubFrac", "TagNo", "Status", "DBH", "CrwnCl", "LiCrBHt",
                 "CrwnRad", "DRC", "Comment", "UV1", "UV2", "UV3")

  # tree columns not used by NGPN
  #c("Ht", "CrwnRto", "CrFuBHt", "Age", "GrwthRt", "Mort", "DecayCl", "LaddBaseHt", "LaddMaxHt",
  # "NuLiStems", "NuDeStems", "EqDia", "XCoord", "YCoord", "CKR", "CharHt",
  # "ScorchHt", "CrScPct", "DamCd1", "DamSev1", "DamCd2", "DamSev2", "DamCd3",
  # "DamSev3", "DamCd4", "DamSev4", "DamCd5", "DamSev5")

  Trees_Metric <-
    samp_tree_spp[order(samp_densq_spp$MacroPlot_Name, samp_densq_spp$year,
                         samp_densq_spp$Index, samp_densq_spp$ScientificName),
                   c(cols_view_start, cols_taxa_start,
                     cols_tree,
                     cols_taxa_end, cols_view_end)]

    #CBI is Composite_Burn_Index

  #---- Add views to VIEWS_NGPN ----
  view_names <- c("Cover_Points_Metric", "Cover_Species_Composition", "Density_Belts_Metric",
                 "Density_Quadrats_Metric", "Disturbance_History", "MacroPlot_SampleEvents",
                 "Surface_Fuels_1000Hr", "Surface_Fuels_Fine", "Surface_Fuels_Duff", "Taxa_Table",
                 "Trees_Metric")

  if(new_env == TRUE){VIEWS_NGPN <<- new.env()}
  env_views <- if(new_env == TRUE){VIEWS_NGPN} else {.GlobalEnv}

  assign("Cover_Points_Metric", Cover_Points_Metric, envir = env_views)
  assign("Cover_Species_Composition", Cover_Species_Composition, envir = env_views)
  assign("Density_Belts_Metric", Density_Belts_Metric, envir = env_views)
  assign("Density_Quadrats_Metric", Disturbance_History, envir = env_views)
  assign("MacroPlot_SampleEvents", MacroPlot_SampleEvents, envir = env_views)
  assign("Surface_Fuels_1000Hr", Surface_Fuels_1000Hr, envir = env_views)
  assign("Surface_Fuels_Fine", Surface_Fuels_Fine, envir = env_views)
  assign("Surface_Fuels_Duff", Surface_Fuels_Duff, envir = env_views)
  assign("Taxa_Table", Taxa_Table, envir = env_views)
  assign("Trees_Metric", Trees_Metric, envir = env_views)

  if(export_views == TRUE){
    dir.create(tmp <- tempfile())

    invisible(lapply(seq_along(view_names), function(x){
      temp_tbl = get(view_names[x], envir = env_views)
      write.csv(temp_tbl,
                paste0(tmp, "\\", view_names[x], ".csv"),
                row.names = FALSE)
    }))

    view_list <- list.files(tmp)
    park <- substr(dbname, nchar(dbname)-3, nchar(dbname))

    zip_name = paste0("NGPN_FFI_views_", format(Sys.Date(), "%Y%m%d"), ".zip")

    zip::zipr(zipfile = paste0(export_pathn, "\\", zip_name),
              root = tmp,
              files = view_list)
  }

  noquote(paste0("Export of views complete and saved to ", export_pathn, "\\", zip_name))

  return(env_views)

  }

