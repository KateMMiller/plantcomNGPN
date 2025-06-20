#' @include getMacroPlot.R
#'
#' @title getSampleEvent
#'
#' @description This function filters FFI sample event data by park, plot name, project, purpose,
#' and year. This function was primarily developed to pull out NGPN plant community
#' monitoring plots and NGPN PCM vital signs sampling events. Using combinations of plot names,
#' projects or purposes that are outside NGPN PCM plots hasn't been tested as thoroughly, and may
#' not return intended results in every case. Note that plots in the MacroPlot table that don't have
#' a corresponding record in the SampleEvent table are not returned. Note that this is more
#' of an internal function that other data-related getter functions source to correctly link table and
#' filter on records.
#'
#' @importFrom dplyr left_join select
#'
#' @param park Filter on park code (aka RegistrationUnit_Name). Can select more than one. Valid inputs:
#' \itemize{
#' \item{"all":} {Include all NGPN parks with FFI data}
#' \item{"AGFO":} {Agate Fossil Beds National Monument}
#' \item{"BADL":} {Badlands National Park}
#' \item{"DETO":} {Devils Tower National Monument}
#' \item{"FOLA":} {Fort Laramie National Historic Site}
#' \item{"FOUS":} {Fort Union Trading Post National Historic Site}
#' \item{"JECA":} {Jewel Cave National Monument}
#' \item{"KNRI":} {Knife River Indian Villages National Historic Sites}
#' \item{"MORU":} {Mount Rushmore National Monument}
#' \item{"SCBL":} {Scotts Bluff National Monument}
#' \item{"THRO":} {Theodore Roosevelt National Park}
#' \item{"WICA":} {Wind Cave National Park}
#'}
#'
#' @param plot_name Quoted string to return a particular plot based on name. Default is "all", which if
#' purpose is set to "NGPN_VS" (default), and project is set to "Park" (default), then only NGPN Plant Community
#' Monitoring plots (e.g.,macroplots with "_PCM_", "_FPCM_", "_LPCM_", and "_RCM_" in their names) will be included.
#' Can select multiple plots. If a plot name is specified that does not occur in the imported data,
#' function will error out with a list of unmatched plot names.
#'
#' @param project Quoted string to return plots of a particular project, based on ProjectUnit_Name. In NGPN, this
#' typically is the strata a given plot belongs to. By default, selects "NGPN_VS" plots, which are plots with
#' c("_PCM_", "_FPCM_", "_LPCM_", and "_RCM_") in their name and the "Park" stratum for those plots. Note that some
#' plots fall in multiple stratum, such as Park and Native Prairie in AGFO. In those cases, the Park strata is
#' selected by default. If a user wants a different strata than "Park", that can be specified using the codes below.
#' Valid inputs:
#' \itemize{
#' \item{'all':} {Pull in all project types.}
#' \item{"Park":} {Default. *NGPN VS* stratum covering whole park.}
#' \item{"ABAM":} {*NGPN VS* stratum in WICA.}
#' \item{"Bodmer":} {*NGPN VS* stratum in FOUS.}
#' \item{"Cedar Removal Study":} {*NGPN VS* in MNRR.}
#' \item{"Deciduous Woodland":} {*NGPN VS* covers KNRI (2 plots) and THROS (1 plot).}
#' \item{"Fort":} {*NGPN VS* stratum in FOUS.}
#' \item{"Monitoring"} {*NGPN VS* stratum in MNRR.}
#' \item{"Native Prairie":} {*NGPN VS* stratum in AGFO.}
#' \item{"North Riparian":} {*NGPN VS* stratum in THRO.}
#' \item{"North Upland":} {*NGPN VS* stratum in THRO.}
#' \item{"North Unit":} {*NGPN VS* stratum in BADL.}
#' \item{"Pine Forest":} {*NGPN VS* stratum in DETO, JECA, MORU, and WICA.}
#' \item{"Prairie":} {*NGPN VS* stratum in BADL, DETO, FOUS, KNRI, SCBL, THRO, and WICA.}
#' \item{"Riparian":} {*NGPN VS* stratum in AGFO, DETO, and FOLA.}
#' \item{"Shrubland":} {*NGPN VS* stratum in THRO.}
#' \item{"South Riparian":} {*NGPN VS* stratum in THRO.}
#' \item{"South Upland":} {*NGPN VS* stratum in THRO.}
#' \item{"Upland":} {*NGPN VS* stratum in DETO and FOLA.}
#' }
#' Other options include c("ABAM Supplemental", "AnnualBrome_Research",
#'                         "American Elk Invasive Research", "Archaeology JFSP",
#'                         "Belle Fourche Invasive Research", "CBI",
#'                         "Centennial Invasive Research", "Control Invasive Research",
#'                         "FFI TESTING", "Highland Creek TH Herbicide Trial",
#'                         "IN-ACTIVE", "Juniper Woodland", "Lithograph Invasive Research",
#'                         "Pringle Dog Town Herbicide Trial", "Woodland")
#'
#' @param purpose Quoted string to return plots with a particular purpose, which typically refers to a characteristic
#' of the plot's sample design in NGPN (e.g., Panel1). Note that purpose is not standard across parks. This function
#' standardizes some purposes (eg "FX" and "Fire Effects" are both called "FX monitoring"). The following purposes
#' that can be specified are below. By default, "NGPN_VS" plots are selected, which includes all plots with c("_PCM_",
#' "_FPCM_", "_LPCM_", and "_RCM_") in their name.
#' \itemize{
#' \item{"all":} {All plots in imported FFI database}
#' \item{"NGPN_VS":} {Default. NGPN Plant Community Monitoring Plots with c("_PCM_", "_FPCM_", "_LPCM_", and, "_RCM_") in their name}
#' \item{"Panel1":} {NGPN PCM Panel 1}
#' \item{"Panel2":} {NGPN PCM Panel 2}
#' \item{"Panel3":} {NGPN PCM Panel 3}
#' \item{"Panel4":} {NGPN PCM Panel 4}
#' \item{"Panel5":} {NGPN PCM Panel 5}
#' \item{"Panel6":} {NGPN PCM Panel 6}
#' \item{"Panel7":} {NGPN PCM Panel 7}
#' \item{"Panel8":} {NGPN PCM Panel 8}
#' \item{"Panel9":} {NGPN PCM Panel 9}
#' \item{"Panel10":} {NGPN PCM Panel 10}
#' \item{"PanelE":} {NGPN PCM Extensive. Found in DETO, FOLA, JECA, MORU, SCBL, and THRO.}
#' \item{"ABAM Supplemental":} {Supplemental plots related to ABAM. Only found in BADL, FOLA, and WICA}
#' \item{"AnnualBromeResearch":} {Annual Brome Research in BADL and SCBL}
#' }
#' Other options include c("CBI plot monitoring" (WICA), "Control" (MNRR), "Daubenmire Plot" (KNRI),
#'                         "Early Detection" (DETO), "FIRE" (JECA, KNRI),
#'                         "Fire/I&M Veg Monitoring Plot" (DETO, KNRI, SCBL, and THRO),
#'                         "Fire/IM Pilot Study Plot" (DETO), "FIRE_Dual" (WICA),
#'                         "FMH Forest Plot" (BADL, DETO, JECA, KNRI, MORU, SCBL, THRO, WICA),
#'                         "FMH Grass Plot" (AGFO, BADL, DETO, KNRI, SCBL, THRO, WICA),
#'                         "FMH Shrub Plot" (BADL, SCBL, and THRO), "Forest and Fuels" (MORU, SCBL, and WICA.),
#'                         "Forest Fuels and Vegetation" (WICA), "Forest Plot" (WICA), "ForestStructure" (KNRI and WICA),
#'                         "FPCM Grassland plot" (DETO), "FX Dual" (DETO and WICA), "FX Extensive" (WICA),
#'                         "FX Intensive" (BADL, KNRI, THRO), "FX Monitoring" (AGFO, BADL, DETO, FOUS, KNRI, MORU, SCBL, THRO, and WICA),
#'                         "HTLN Legacy" (AGFO and SCBL), "I&M_tower_vegetation (Found in DETO),
#'                         "IM_FX_Dual" (DETO), "IM_Intensive" (AGFO, FOUS, and THRO), "IM_veg" (THRO),
#'                         "Invasives Research" (DETO, JECA, and WICA), "Lafferty Plot" (MORU), "LTEM/FMH" (AGFO),
#'                         "Modified Forest Plot" (THRO), "Modified Shrub Plot" (THRO),
#'                         "NGP Fire Forest Fuel Veg Protcol" (DETO), "NGP Grassland Plot - Interior Burn Unit" (BADL),
#'                         "Pre- and Post-treatment of fuels" (JECA), "Research" (WICA), "Treatment" (MNRR))
#'
#' @param mon_status Quoted string. Allows you to select different sampling event status types. Default is "NGPN_VS",
#' which will pull in sample events coded a NGPN Plant Community Monitoring (see description for NGPN_VS below). Note
#' that in the data, the status name starts with year. For simplicity, the years argument pulls out specific years,
#' and mon_status to pull out different status types without considering year. Valid inputs:
#' \itemize{
#' \item{"NGPN_VS":} {Default. Pulls in records with status of "####_PlantCommunity", "####_FirePlantCommunity", "####_ForestStructure", #### representing year.}
#' \item{"PlantCommunity":} {####_PlantCommunity only records}
#' \item{"FirePlantCommunity":} {####_FirePlantCommunity only records}
#' \item{"ForestStructure":} {####_ForestStructure only records}
#' }
#' Other options include c("00Pre", "00Pre2", "01Burn", "01Post", "01Pre", "01yr01", "01yr02",
#'                         "01yr10", "FireOther_1", "Dual", "FPCM_Other_01", "FPCM_Other_02",
#'                         "Other", "PCM_Other", "FireOther_2", "FPCM_Other", "FireOther",
#'                         "FireOther_FuelReduction", "FPCM_Other_03", "FireOther_3", "Fire_Other",
#'                         "PCM_Fire", "Plant Community", "Fire", "Ext", "00Pre02")
#'
#'
#' @param years Numeric. Filter on years. Accepted values start at 1997. Default is 2011 to current year,
#' which represents the time NGPN plant community monitoring began using latest protocol and sample design.
#'
#' @param output Quoted string. Options are "short" (default), which only returns most important columns;
#' "verbose" returns all columns in the SampleEvent-related tables.
#'
#' @examples
#' \dontrun{
#'
#' library(vegcomNPGN)
#' importData(type = 'local',
#'   dbname = c("FFI_RA_AGFO", "FFI_RA_BADL", "FFI_RA_DETO", "FFI_RA_FOLA",
#'              "FFI_RA_FOUS", "FFI_RA_JECA", "FFI_RA_KNRI", "FFI_RA_MNRR",
#'              "FFI_RA_MORU", "FFI_RA_SCBL", "FFI_RA_THRO", "FFI_RA_WICA"),
#'   export = F)
#'
#' # return all samples of NGPN Plant Community Monitoring plots (ie vital signs plots),
#' # for the Park stratum and all purposes used by NGPN from 2011 and later
#' samp_vs <- getSampleEvent()
#'
#' # return Prairie stratum for AGFO and SCBL for NGPN_VS plots
#' samp_pr_vs <- getSampleEvent(park = c("AGFO", "SCBL"), purpose = "NGPN_VS",
#'   project = c("Native Prairie", "Prairie"))
#'
#' # query all sites, all years
#' samp_all <- getSampleEvent(purpose = "all", project = "all", years = 1997:2024)
#'
#' # query NGPN only plots from North Dakota from 2020:2024
#' samp_nd <- getSampleEvent(park = c("FOUS", "KNRI", "THRO"), years = 2020:2024)
#'
#' # query only North and South Upland for THRO with all columns
#' thro_up <- getSampleEvent(park = "THRO", project = c("North Upland", "South Upland"),
#'   output = "verbose")
#' }
#'
#' @return Returns a data frame of sample event data
#'
#' @export

getSampleEvent <- function(park = 'all', plot_name = "all", project = "Park", purpose = "NGPN_VS",
                           mon_status = "NGPN_VS", years = 2011:format(Sys.Date(), "%Y"),
                           output = "short"){
  #---- Bug handling ----
  park <- match.arg(park, several.ok = TRUE,
                    c("all", "AGFO", "BADL", "DETO", "FOLA", "FOUS",
                      "JECA", "KNRI", "MORU", "SCBL", "THRO", "WICA"))
  if(any(park == "all")){park = c("AGFO", "BADL", "DETO", "FOLA", "FOUS",
                                  "JECA", "KNRI", "MORU", "SCBL", "THRO", "WICA")} else {park}
  purpose <- match.arg(purpose, c("all", "NGPN_VS", "Panel1", "Panel2", "Panel3", "Panel4", "Panel5",
                                   "Panel6", "Panel7", "Panel8", "Panel9", "Panel10", "PanelE",
                                   "ABAM Supplemental", "AnnualBromeResearch", "CBI plot monitoring",
                                   "Control", "Daubenmire Plot", "Early Detection", "FIRE",
                                   "Fire/I&M Veg Monitoring Plot", "Fire/IM Pilot Study Plot",
                                   "FIRE_Dual", "FMH Forest Plot", "FMH Grass Plot", "FMH Shrub Plot",
                                   "Forest and Fuels", "Forest Fuels and Vegetation", "Forest Plot",
                                   "ForestStructure", "FPCM Grassland plot", "FX Dual", "FX Extensive",
                                   "FX Intensive", "FX Monitoring", "HTLN Legacy", "I&M_tower_vegetation",
                                   "IM_FX_Dual", "IM_Intensive", "IM_veg", "Invasives Research", "Lafferty Plot",
                                   "LTEM/FMH", "Modified Forest Plot", "Modified Shrub Plot",
                                   "NGP Fire Forest Fuel Veg Protcol", "NGP Grassland Plot - Interior Burn Unit",
                                   "Pre- and Post-treatment of fuels", "Research", "Treatment"), several.ok = TRUE)
  project <- match.arg(project, c('all', "Park", "ABAM", "Bodmer", "Cedar Removal Study", "Deciduous Woodland",
                                  "Fort", "Monitoring", "Native Prairie", "North Riparian", "North Upland", "North Unit",
                                  "Pine Forest", "Prairie", "Riparian", "Shrubland", "South Riparian", "South Upland",
                                  "Upland", "ABAM Supplemental", "AnnualBrome_Research", "American Elk Invasive Research",
                                  "Archaeology JFSP", "Belle Fourche Invasive Research", "CBI", "Centennial Invasive Research",
                                  "Control Invasive Research", "FFI TESTING", "Highland Creek TH Herbicide Trial",
                                  "IN-ACTIVE", "Juniper Woodland", "Lithograph Invasive Research", "Pringle Dog Town Herbicide Trial",
                                  "Woodland"), several.ok = T)

  mon_status <- match.arg(mon_status, c("NGPN_VS", "PlantCommunity", "FirePlantCommunity",
                                        "ForestStructure", "00Pre", "00Pre2", "01Burn", "01Post",
                                        "01Pre", "01yr01", "01yr02", "01yr10", "FireOther_1",
                                        "Dual", "FPCM_Other_01", "FPCM_Other_02", "Other",
                                        "PCM_Other", "FireOther_2", "FPCM_Other", "FireOther",
                                        "FireOther_FuelReduction", "FPCM_Other_03", "FireOther_3", "Fire_Other",
                                        "PCM_Fire", "Plant Community", "Fire", "Ext", "00Pre02"),
                          several.ok = TRUE)

  mon_status <- if(all(mon_status %in% "NGPN_VS")){c("PlantCommunity", "FirePlantCommunity", "ForestStructure")
  } else if(length(mon_status) > 1 & any(mon_status %in% "NGPN_VS")){
    c("PlantCommunity", "FirePlantCommunity", "ForestStructure", mon_status[!mon_status %in% "NGPN_VS"])
  } else {mon_status}

  stopifnot(class(years) %in% c("numeric", "integer"), years >= 1997)
  output <- match.arg(output, c("short", "verbose"))

  #---- Compile data ----
  env <- if(exists("NGPN_tables")){NGPN_tables} else {.GlobalEnv}
  macro <- getMacroPlot(park = park, plot_name = plot_name, project = project, purpose = purpose,
                        output = "short")|>
    select(MacroPlot_Name, RegistrationUnit_Name, MacroPlot_Purpose, MacroPlot_Type,
           ProjectUnit_Name, MacroPlot_UTM_X, MacroPlot_UTM_Y, MacroPlot_DD_Lat, MacroPlot_DD_Long,
           MacroPlot_Elevation, MacroPlot_Aspect, MacroPlot_Azimuth, MacroPlot_SlopeHill,
           MacroPlot_SlopeTransect, MacroPlot_GUID, MM_ProjectUnit_GUID)

  tryCatch(
    monstat1 <- get("MonitoringStatus", envir = env),
    error = function(e){stop("MonitoringStatus table not found. Please import data.")})

  tryCatch(
    mm_monstat_se <- get("MM_MonitoringStatus_SampleEvent", envir = env),
    error = function(e){stop("MM_MonitoringStatus_SampleEvent table not found. Please import data.")})

  tryCatch(
    sampev1 <- get("SampleEvent", envir = env),
    error = function(e){stop("SampleEvent table not found. Please import data.")})

  # Use to make some tables smaller before join
  macro_guids <- unique(macro$MacroPlot_GUID)
  macro_proj_guids <- unique(macro$MM_ProjectUnit_GUID)

  # Fix typo in MonitoringStatus_Name
  monstat1$MonitoringStatus_Name[monstat1$MonitoringStatus_Name == "2009_Plant Community"] <- "2009_PlantCommunity"
  monstat1$MonitoringStatus_Name[monstat1$MonitoringStatus_Name == "2018_Plant Community"] <- "2018_PlantCommunity"
  monstat1$MonitoringStatus_Name[monstat1$MonitoringStatus_Name == "2024_Plant Community"] <- "2024_PlantCommunity"

  # filter on monitoring status with grepl
  mon_stat_list <- paste(mon_status, collapse = "|")
  monstat2 <- monstat1[grepl(mon_stat_list, monstat1$MonitoringStatus_Name),]

  sampev <- sampev1[sampev1$SampleEvent_Plot_GUID %in% macro_guids,]
  monstat <- monstat2[monstat2$MonitoringStatus_ProjectUnit_GUID %in% macro_proj_guids,]

  mac_samp <- left_join(macro, sampev, by = c("MacroPlot_GUID" = "SampleEvent_Plot_GUID"))
  mac_samp_mm <- left_join(mac_samp, mm_monstat_se, by = c("SampleEvent_GUID" = "MM_SampleEvent_GUID",
                                                           "datasource"))
  mac_samp_monstat <- left_join(mac_samp_mm, monstat,
                                by = c("MM_MonitoringStatus_GUID" = "MonitoringStatus_GUID",
                                       "MM_ProjectUnit_GUID" = "MonitoringStatus_ProjectUnit_GUID",
                                       "datasource"))

  mac_samp_monstat$SampleEvent_Date <-
    format(as.Date(mac_samp_monstat$SampleEvent_Date, format = "%Y-%m-%d %H:%m:%s"),
           "%Y-%m-%d")
  mac_samp_monstat$year <- format(as.Date(mac_samp_monstat$SampleEvent_Date, format = "%Y-%m-%d"),
                             "%Y")
  mac_samp_monstat$month <- format(as.Date(mac_samp_monstat$SampleEvent_Date, format = "%Y-%m-%d"),
                                  "%m")
  mac_samp_monstat$doy <- format(as.Date(mac_samp_monstat$SampleEvent_Date, format = "%Y-%m-%d"),
                                  "%j")

  keep_cols <- c("MacroPlot_Name", "RegistrationUnit_Name", "MacroPlot_Purpose", "MacroPlot_Type",
                 "ProjectUnit_Name", "MacroPlot_UTM_X", "MacroPlot_UTM_Y",
                 "MacroPlot_DD_Lat", "MacroPlot_DD_Long", "MacroPlot_Elevation",
                 "MacroPlot_Aspect", "MacroPlot_Azimuth", "MacroPlot_SlopeHill", "MacroPlot_SlopeTransect",
                 "SampleEvent_Date", "year", "month", "doy", "SampleEvent_DefaultMonitoringStatus",
                 "MonitoringStatus_Name", "MonitoringStatus_UV1",
                 "MacroPlot_GUID", "MM_ProjectUnit_GUID", "SampleEvent_GUID", "MM_MonitoringStatus_GUID")

  full_cols <- c(keep_cols, setdiff(names(mac_samp_monstat), keep_cols)) # for logical col order

  # Drop plots with no sample events
  sampev2 <- mac_samp_monstat[!is.na(mac_samp_monstat$SampleEvent_GUID),]
  sampev3 <- sampev2[grepl(mon_stat_list, sampev2$MonitoringStatus_Name),] # dropping blanks

  # Filter on years
  sampev4 <- sampev3[sampev3$year %in% years, ]

  sampev_final <- if(output == "short"){
    sampev4[,keep_cols]
  } else {sampev4[,full_cols]}

  return(sampev_final)
  }
