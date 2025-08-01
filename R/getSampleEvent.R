#' @include getMacroPlot.R
#'
#' @title getSampleEvent
#'
#' @description This function filters FFI sample event data by park, plot name, project, purpose, and year.
#' This function was primarily developed to pull out NGPN plant community monitoring plots. Using
#' combinations of plot names, projects or purposes that are outside NGPN PCM plots hasn't been
#' tested as thoroughly, and may not return intended results in every case. Note that this is more
#' of an internal function that other data-related getter functions source to efficiently
#' filter on records. Note that currently, the MonitoringStatus_Comment column is not included, as it
#' results in duplicate returns, where one row is blank and another includes a comment for the sample
#' sample event.
#'
#' @param park Filter on park code (aka Unit_Name). Can select more than one. Valid inputs:
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
#' @param plot_name Quoted string to return a particular plot based on MacroPlot_Name. Default is "all", which if
#' purpose is set to "NGPN_PCM" (default), and project is set to "Park" (default), then only NGPN Plant Community
#' Monitoring plots (e.g.,macroplots with "_PCM_", "_FPCM_", "_LPCM_", and "_RCM_" in their names) will be included.
#' Can select multiple plots. If a plot name is specified that does not occur in the imported data,
#' function will error out with a list of unmatched plot names.
#'
#' @param project Quoted string to return plots of a particular project, based on ProjectUnit_Name. In NGPN, this
#' typically is the strata a given plot belongs to. By default, selects NGPN_PCM plots, which are plots with
#' c("_PCM_", "_FPCM_", "_LPCM_", and "_RCM_") in their name and the "Park" stratum for those plots. Note that some
#' plots fall in multiple stratum, such as Park and Native Prairie in AGFO. In those cases, the "Park" strata is
#' selected by default. If a user wants a different strata than "Park", that can be specified using the codes below.
#' Only one project can be specified at a time. Current valid inputs:
#' \itemize{
#' \item{"Park":} {Default. *NGPN_PCM* stratum covering whole park (same.}
#' \item{"ABAM":} {*NGPN_PCM* stratum in WICA.}
#' \item{"Bodmer":} {*NGPN_PCM* stratum in FOUS.}
#' \item{"Fort":} {*NGPN_PCM* stratum in FOUS.}
#' \item{"Native Prairie":} {*NGPN_PCM* stratum in AGFO.}
#' \item{"North Riparian":} {*NGPN_PCM* stratum in THRO.}
#' \item{"North Upland":} {*NGPN_PCM* stratum in THRO.}
#' \item{"North Unit":} {*NGPN_PCM* stratum in BADL.}
#' \item{"Pine Forest":} {*NGPN_PCM* stratum in DETO, JECA, MORU, and WICA.}
#' \item{"Prairie":} {*NGPN_PCM* stratum in BADL, DETO, FOUS, KNRI, SCBL, THRO, and WICA.}
#' \item{"Riparian":} {*NGPN_PCM* stratum in AGFO, DETO, and FOLA.}
#' \item{"Shrubland":} {*NGPN_PCM* stratum in THRO.}
#' \item{"South Riparian":} {*NGPN_PCM* stratum in THRO.}
#' \item{"South Upland":} {*NGPN_PCM* stratum in THRO.}
#' \item{"Upland":} {*NGPN_PCM* stratum in DETO and FOLA.}
#' }
#' Other options include c("ABAM Supplemental", "AnnualBrome_Research",
#'                         "American Elk Invasive Research", "Archaeology JFSP",
#'                         "Belle Fourche Invasive Research", "CBI",
#'                         "Cedar Removal Study", "Centennial Invasive Research",
#'                         "Control Invasive Research", "Deciduous Woodland",#'
#'                         "FFI TESTING", "Highland Creek TH Herbicide Trial",
#'                         "INACTIVE", "Juniper Woodland", "Lithograph Invasive Research",
#'                         "Monitoring", "Pringle Dog Town Herbicide Trial", "Woodland")
#'
#' @param purpose Quoted string to return plots with a particular purpose, which typically refers to a characteristic
#' of the plot's sample design in NGPN (e.g., Panel1). Note that purpose is not standard across parks. This function
#' standardizes some purposes (eg "FX" and "Fire Effects" are both called "FX monitoring"). The following purposes
#' that can be specified are below. By default, "NGPN_PCM" plots are selected, which includes all plots with c("_PCM_",
#' "_FPCM_", "_LPCM_", and "_RCM_") in their name. If new purposes are added in the future, they will need to be added
#' to the bug handling code in the function. Valid inputs:
#' \itemize{
#' \item{"all":} {All plots in imported FFI database}
#' \item{"NGPN_PCM":} {Default. NGPN Plant Community Monitoring Plots with c("_PCM_", "_FPCM_", "_LPCM_", and, "_RCM_") in their name, and all of the purposes defined below.}
#' \item{"ForestStructure":}{NGPN Forest Structure plot. Found in KNRI and WICA.}
#' \item{"IM_Intensive"}{NGPN intensive monitoring plot. Found in AGFO, FOUS, and THRO.}
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
#' \item(""){A number of PCM plots have a blank purpose. Until this is resolved, or we know these can be dropped, they are included in the NGPN_PCM purpose}
#' }
#' Other non NPGN_PCM options include:
#'    c("AnnualBromeResearch" (BADL, SCBL)
#'      "ABAM Supplemental" (BADL, FOLA, WICA),
#'      "CBI plot monitoring" (WICA), "Control" (MNRR), "Daubenmire Plot" (KNRI),
#'      "Early Detection" (DETO), "FIRE" (JECA, KNRI),
#'      "Fire/I&M Veg Monitoring Plot" (DETO, KNRI, SCBL, and THRO),
#'      "Fire/IM Pilot Study Plot" (DETO), "FIRE_Dual" (WICA),
#'      "FMH Forest Plot" (BADL, DETO, JECA, KNRI, MORU, SCBL, THRO, WICA),
#'      "FMH Grass Plot" (AGFO, BADL, DETO, KNRI, SCBL, THRO, WICA),
#'      "FMH Shrub Plot" (BADL, SCBL, and THRO),
#'      "Forest and Fuels" (MORU, SCBL, and WICA.),
#'      "Forest Fuels and Vegetation" (WICA), "Forest Plot" (WICA),
#'      "FPCM Grassland plot" (DETO), "FX Dual" (DETO and WICA),
#'      "FX Extensive" (WICA), "FX Intensive" (BADL, KNRI, THRO),
#'      "FX Monitoring" (AGFO, BADL, DETO, FOUS, KNRI, MORU, SCBL, THRO, and WICA),
#'      "HTLN Legacy" (AGFO and SCBL), "I&M_tower_vegetation (Found in DETO),
#'      "IM_FX_Dual" (DETO), "IM_veg" (THRO),
#'      "Invasives Research" (DETO, JECA, and WICA), "Lafferty Plot" (MORU), "LTEM/FMH" (AGFO),
#'      "Modified Forest Plot" (THRO), "Modified Shrub Plot" (THRO),
#'      "NGP Fire Forest Fuel Veg Protcol" (DETO),
#'      "NGP Grassland Plot - Interior Burn Unit" (BADL),
#'      "Pre- and Post-treatment of fuels" (JECA),
#'      "Research" (WICA), "Treatment" (MNRR))
#'
#' @param mon_status Quoted string. Allows you to select different MonitoringStatus$MonitoringStatus_Base types. Default is "NGPN_PCM",
#' which will pull in sample events coded a NGPN Plant Community Monitoring (see description for NGPN_PCM below).
#' Current valid inputs:
#' \itemize{
#' \item{"NGPN_PCM":} {Default. Pulls in records with monitoring status base of "PlantCommunity", "FirePlantCommunity", "ForestStructure".
#' Note that some base names have _, spaces, or years in them. These are cleaned up in the function until they're fixed in the database.}
#' \item{"PlantCommunity":} {PlantCommunity only records}
#' \item{"FirePlantCommunity":} {FirePlantCommunity only records}
#' \item{"ForestStructure":} {ForestStructure only records}
#' }
#'
#' Other options include c("00Pre", "00Pre2", "01Burn", "01Post", "01Pre", "01yr01", "01yr02",
#'                         "01yr10", "FireOther_1", "Dual", "FPCM_Other_01", "FPCM_Other_02",
#'                         "Other", "PCM_Other", "FireOther_2", "FPCM_Other", "FireOther",
#'                         "FireOther_FuelReduction", "FPCM_Other_03", "FireOther_3", "Fire_Other",
#'                         "PCM_Fire", "Fire", "Ext", "00Pre02")
#'
#' @param years Numeric. Filter on years. Accepted values start at 2011. Default is 2011 to current year,
#' which represents the time NGPN plant community monitoring began using latest protocol and sample design.
#'
#' @param complete_events Logical. If TRUE (Default) only returns sample events with associated sample data
#' (eg Cover Point Data). If FALSE, returns all sample events with a record in the SampleEvent table.
#' +++++ NOT YET ENABLED +++++
#'
#' @examples
#' \dontrun{
#'
#' library(plantcomNGPN)
#' importViews(import_path = "C:/temp/NGPN_FFI_views_20250708.zip")
#'
#' # Default return all samples of NGPN Plant Community Monitoring plots (ie vital signs plots),
#' # for the Park stratum and all purposes used by NGPN from 2011 and later
#' samp_vs <- getSampleEvent()
#'
#' # return data for AGFO and SCBL for NGPN_PCM plots
#' samp_AS <- getSampleEvent(park = c("AGFO", "SCBL"), purpose = "NGPN_PCM")
#' table(samp_AS$Unit_Name)
#'
#' # query all NGPN_PCM sites JECA PlantCommunity only
#' samp_jeca <- getSampleEvent(park = "JECA", mon_status = "PlantCommunity")
#' table(samp_jeca$Unit_Name, samp_jeca$MonitoringStatus_Base)
#'
#' # query NGPN only plots from North Dakota from 2020:2024
#' samp_nd <- getSampleEvent(park = c("FOUS", "KNRI", "THRO"), years = 2020:2024)
#' table(samp_nd$Unit_Name, samp_nd$year)
#'
#' # query and combine North and South Upland for THRO
#' thro_upn <- getSampleEvent(park = "THRO", project = "North Upland")
#' thro_upn$ProjectUnit <- "North_Upland"
#'
#' thro_ups <- getSampleEvent(park = "THRO", project = "South Upland")
#' thro_ups$ProjectUnit <- "South_Upland"
#'
#' thro_up <- rbind(thro_upn, thro_ups)
#' table(thro_up$ProjectUnit, thro_up$year)
#'
#' # return results for three plots in KNRI for PlantCommunity monitoring status only
#' KNRI_123 <- getSampleEvent(plot_name = c("KNRI_PCM_001", "KNRI_PCM_002", "KNRI_PCM_003"),
#'  mon_stat = "PlantCommunity")
#' table(KNRI_123$MacroPlot_Name, KNRI_123$MonitoringStatus_Base)
#'
#' }
#'
#' @return Returns a data frame of sample events
#' @export
#'

getSampleEvent <- function(park = 'all', plot_name = "all", project = "Park", purpose = "NGPN_PCM",
                           mon_status = "NGPN_PCM", years = 2011:format(Sys.Date(), "%Y"),
                           complete_events = TRUE){

  #---- Bug handling ----
  stopifnot(class(years) %in% c("numeric", "integer"), years >= 2011)
  stopifnot(class(complete_events) == "logical")

  env <- if(exists("VIEWS_NGPN")){VIEWS_NGPN} else {.GlobalEnv}
  tryCatch(
    sampev <- get("SampleEvents", envir = env),
    error = function(e){stop("SampleEvents view not found. Please import NGPN FFI data.")}
  )
  # Update monitoring status for all or NGPN_PCM
  mon_status <- if(any(mon_status %in% 'all')){
    sort(unique(sampev$MonitoringStatus_Base))
  } else if(any(mon_status %in% "NGPN_PCM")){
    c("PlantCommunity", "FirePlantCommunity", "ForestStructure", "Dual", "Riparian",
      "Panel1", "Panel2", "Panel3", "Panel4", "Panel5", "PanelE")
  } else {mon_status}

  # check monitoring status is in the view
  se_monstat <- sort(unique(sampev$MonitoringStatus_Base))
  bad_monstat1 <- setdiff(mon_status, se_monstat)
  bad_monstat <- bad_monstat1[!grepl("PlantCommunity|FirePlantCommunity|ForestStructure|Dual|Riparian|Panel1|Panel2|Panel3|Panel4|Panel5|PanelE",
                                     bad_monstat1)]

  if(length(bad_monstat) > 0){stop("Specified mon_status not found in data: ",
                                   paste0(bad_monstat))}

  macro_guids <- getMacroPlot(park = park, plot_name = plot_name, project = project,
                              purpose = purpose, output = 'short')$MacroPlot_GUID

  sampev2 <- sampev[sampev$MacroPlot_GUID %in% macro_guids,]
  sampev3 <- sampev2[sampev2$year %in% years,]
  sampev4 <- sampev3[sampev3$MonitoringStatus_Base %in% mon_status,]

  # drop MonitoringStatus_Comment, which is how multiple records turn up
  sampev4$ProjectUnit_Name <- project
  keep_cols <- c("MacroPlot_Name", "Unit_Name", "MacroPlot_Purpose", "MacroPlot_Type", "ProjectUnit_Name",
                 "SampleEvent_Date", "year", "month", "doy", "UTM_X", "UTM_Y", "UTMzone", "Elevation",
                 "Azimuth", "Aspect", "SlopeHill", "SlopeTransect", "SampleEvent_UV1", "DefaultMonitoringStatus",
                 "TreatmentUnit", "MonitoringStatus_Prefix", "MonitoringStatus_Base", "MonitoringStatus_Suffix",
                 "MonitoringStatus_Name", "SampleEvent_Comment", "SampleEvent_GUID", "RegistrationUnit_GUID",
                 "MacroPlot_GUID")

  keep_cols <- names(sampev4[!grepl("MonitoringStatus_Comment|MM_MonitoringStatus_GUID", names(sampev4))])
  sampev5 <- unique(sampev4[,keep_cols])

  if(nrow(sampev5) == 0){stop(paste0("Specified function arguments returned an empty data frame. ",
                                     "Check that the combination of plot_name, purpose, project, etc.
                                      arguments will return records."))}

  return(data.frame(sampev5))

  }
