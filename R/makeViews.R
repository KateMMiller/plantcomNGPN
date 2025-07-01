#' @title makeViews
#'
#' @description Internal function that runs within importData that creates the views for each
#' NGPN-specific sampling protocols of FFI. Views are Cover_Points_Metric, Cover_Species_Composition,
#' Density_Belts_Metric, Density_Quadrats_Metric, Disturbance_History, MacroPlot_SampleEvents,
#' Surface_Fuels, Taxa_Table, Trees_Metric. Function only compiles data for plots with "PCM",
#' "FPCM", "LPCM", or "RCM" in the MacroPlot_Name, and start at 2011 when NGPN-specific monitoring began.
#'
#' @importFrom dplyr arrange left_join
#'
#' @examples
#' \dontrun{
#'
#' # makeViews() runs within importData().
#' library(plantcomNGPN)
#' importData(type = 'local',
#'   dbname = c("FFI_RA_AGFO", "FFI_RA_BADL", "FFI_RA_DETO", "FFI_RA_FOLA",
#'              "FFI_RA_FOUS", "FFI_RA_JECA", "FFI_RA_KNRI", "FFI_RA_MNRR",
#'              "FFI_RA_MORU", "FFI_RA_SCBL", "FFI_RA_THRO", "FFI_RA_WICA"),
#'   export = F)
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
  cols_view_end <- c("MacroPlot_GUID", "SampleEvent_GUID", "MM_MonitoringStatus_GUID", "RegistrationUnit_GUID",
                     "MM_ProjectUnit_GUID", "Spp_GUID")
  cols_taxa_start <- c("Symbol", "ITIS_TSN", "ScientificName", "CommonName")
  cols_taxa_end <- c("Nativity", "Invasive", "Cultural", "Concern", "LifeCycle", "LifeForm_Name",
                     "NotBiological", "Species_Description", "Species_Comment")
  cols_covpt <- c("Index", "Transect", "Point", "Tape", "Order", "Height", "Status")

  Cover_Points_Metric <- samp_cov_spp[order(samp_cov_spp$MacroPlot_Name, samp_cov_spp$year,
                                            samp_cov_spp$Index, samp_cov_spp$ScientificName),
                                      c(cols_view_start, cols_taxa_start,
                                        cols_covpt,
                                        cols_taxa_end, cols_view_end)]



  }

