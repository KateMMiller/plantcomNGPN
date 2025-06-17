#' @title getMacroPlot
#'
#' @description This function filters macroplot data by plot name and purpose from FFI data.
#'
#' @importFrom dplyr filter full_join select
#'
#' @param park Filter on park code (aka RegistrationUnit_Name). Can select more than one. Valid inputs:
#' \describe{
#' \item{"all"}{Include all NGPN parks with FFI data}
#' \item{"AGFO"}{Agate Fossil Beds National Monument}
#' \item{"BADL"}{Badlands National Park}
#' \item{"DETO"}{Devils Tower National Monument}
#' \item{"FOLA"}{Fort Laramie National Historic Site}
#' \item{"FOUS"}{Fort Union Trading Post National Historic Site}
#' \item{"JECA"}{Jewel Cave National Monument}
#' \item{"KNRI"}{Knife River Indian Villages National Historic Sites}
#' \item{"MORU"}{Mount Rushmore National Monument}
#' \item{"SCBL"}{Scotts Bluff National Monument}
#' \item{"THRO"}{Theodore Roosevelt National Park}
#' \item{"WICA"}{Wind Cave National Park}
#'}
#'
#' @param plot_name Quoted string to return a particular plot based on name. Default is "all".
#' Can select multiple plots. If a plot name is specified that does not occur in the imported data,
#' function will error out with a list of unmatched plot names.
#'
#' @param purpose Quoted string to return plots with a particular purpose. Note that purpose
#' is not standard across parks. This function standardizes some purposes (eg "FX" and "Fire Effects" are both
#' called "FX monitoring". The following purposes that can be specified are below. By default, Panels 1-10 are
#' selected.
#' \describe{
#' \item{"all"}{All plots in imported FFI database}
#' \item{"NGPN_VS"}{NGPN Vegetation Monitoring Panels 1:10}
#' \item{"Panel1"}{NGPN Vegetation Monitoring Panel 1}
#' \item{"Panel2"}{NGPN Vegetation Monitoring Panel 2}
#' \item{"Panel3"}{NGPN Vegetation Monitoring Panel 3}
#' \item{"Panel4"}{NGPN Vegetation Monitoring Panel 4}
#' \item{"Panel5"}{NGPN Vegetation Monitoring Panel 5}
#' \item{"Panel6"}{NGPN Vegetation Monitoring Panel 6}
#' \item{"Panel7"}{NGPN Vegetation Monitoring Panel 7}
#' \item{"Panel8"}{NGPN Vegetation Monitoring Panel 8}
#' \item{"Panel9"}{NGPN Vegetation Monitoring Panel 9}
#' \item{"Panel10"}{NGPN Vegetation Monitoring Panel 10}
#' \item{"ABAM Supplemental"}{Supplemental plots related to ABAM. Only found in BADL, FOLA, and WICA}
#' \item{"AnnualBromeResearch"}{Annual Brome Research in BADL and SCBL}
#' \item{"CBI plot monitoring"}{Unknown use. Only in WICA.}
#' \item{"Control"}{Unknown use. Only in MNRR.}
#' \item{"Daubenmire Plot"}{Unknown use. Only in KNRI.}
#' \item{"Early Detection"}{Early detection of invasive species. Found in DETO}
#' \item{"FIRE"}{Fire-related monitoring, but unclear of fire effects. Found in JECA and KNRI.}
#' \item{"Fire/I&M Veg Monitoring Plot"}{Dual NGPN}
#' \item{"Fire/IM Pilot Study Plot"}{}
#' \item{"FIRE_Dual"}{}
#' \item{"FMH Forest Plot"}{}
#' \item{"FMH Grass Plot"}{}
#' \item{"FMH Shrub Plot"}{}
#' \item{"Forest and Fuels"}{}
#' \item{"Forest Fuels and Vegetation"}{}
#' \item{"Forest Plot"}{}
#' \item{"ForestStructure"}{}
#' \item{"FPCM Grassland plot"}{}
#' \item{"FS"}{}
#' \item{"FX Dual"}{}
#' \item{"FX Extensive"}{}
#' \item{"FX Monitoring"}{}
#' \item{"FX Intensive"}{}
#' \item{"HTLN Legacy"}{}
#' \item{"I&M_tower_vegetation"}{}
#' \item{"IM_FX_Dual"}{}
#' \item{"IM_Intensive"}{}
#' \item{"IM_veg"}{}
#' \item{"Invasives Research"}{}
#' \item{"Lafferty Plot"}{}
#' \item{"LTEM/FMH"}{}
#' \item{"Modified Forest Plot"}{}
#' \item{"Modified Shrub Plot"}{}
#' \item{"NGP Fire Forest Fuel Veg Protcol"}{}
#' \item{"NGP Grassland Plot - Interior Burn Unit"}{}
#' \item{"PanelE"}{}
#' \item{"Pre- and Post-treatment of fuels"}{}
#' \item{"Research"}{}
#' \item{"Treatment"}{}
#' }
#'
#' @param output Quoted string. Options are "short" (default), which only returns most important columns.
#' "verbose" returns all columns in the MacroPlot database table.
#'
#' @examples
#' \dontrun{
#'
#' library(vegcomNPGN)
#' importData(type = 'local',
#' dbname = c("FFI_RA_AGFO", "FFI_RA_BADL", "FFI_RA_DETO", "FFI_RA_FOLA",
#'            "FFI_RA_FOUS", "FFI_RA_JECA", "FFI_RA_KNRI", "FFI_RA_MNRR",
#'            "FFI_RA_MORU", "FFI_RA_SCBL", "FFI_RA_THRO", "FFI_RA_WICA"),
#' export = F)
#'
#' # query all parks, plots and purposes, and only return main columns
#' macro <- getMacroPlot()
#'
#' # return only plots with NGPN purposes listed as: Panels 1-10
#' macro_vs <- getMacroPlot(purpose = "NGPN_VS")
#'
#' # return NGPN only plots from North Dakota
#' macro_nd <- getMacroPlot(park = c("FOUS", "KNRI", "THRO"), purpose = "NGPN_VS")
#' table(macro_nd$RegistrationUnit_Name, macro_nd$MacroPlot_Purpose)
#' }
#'
#' @return Returns a data frame of macroplots
#'
#' @export

getMacroPlot <- function(park = 'all', plot_name = "all", purpose = "NGPN_VS", output = "short"){
  #---- Bug handling ----
  park <- match.arg(park, several.ok = TRUE,
                    c("all", "AGFO", "BADL", "DETO", "FOLA", "FOUS",
                      "JECA", "KNRI", "MORU", "SCBL", "THRO", "WICA"))
  purpose <- match.arg(purpose, c("all", "NGPN_VS",
                                  "ABAM Supplemental", "AnnualBromeResearch",
                                  "CBI plot monitoring", "Control", "Daubenmire Plot",
                                  "Determine strategies for efficient early detection",
                                  "Determine Strategies for efficient early detection",
                                  "Early Invasives Detection",
                                  "FIRE", "Fire Effects Monitoring", "Fire/I&M Veg Monitoring Plot",
                                  "Fire/IM Pilot Study Plot", "FIRE_Dual",
                                  "FIRE_Extensive", "FIRE_Intensive", "FIRE_intesive",
                                  "FS", "Fx", "FX","FX Dual", "FX monitoring", "FX Monitoring", "FX_ Intensive",
                                  "FX_Dual", "FX_Intensive", "I&M_tower_vegetation", "IM_FX_Dual",
                                  "IM_Intensive", "IM_veg",
                                  "FMH Forest Plot", "FMH Grass Plot", "FMH Grass Plot ",
                                  "FMH Shrub Plot", "Forest and Fuels", "Forest Fuels and Vegetation",
                                  "LTEM/FMH",
                                  "Forest Plot", "ForestStructure", "FPCM Grassland plot",
                                  "HTLN Legacy",
                                  "invasive research", "Invasives Research", "Invasvies Research",
                                  "Lafferty Plot", "Lafferty Plot ",
                                  "Modified Forest Plot", "Modified Shrub Plot", "Modified Shrub Plot ",
                                  "NGP Fire Forest Fuel Veg Protcol", "NGP Grassland Plot - Interior Burn Unit",
                                  "Panel1", "Panel2", "Panel3", "Panel4", "Panel5",
                                  "Panel6", "Panel7", "Panel8", "Panel9", "Panel 9", "Panel10",
                                  "PanelE",
                                  "pre- and post-treatment forest and fuels", "Pre- and Post-treatment of fuels",
                                  "research", "Research", "Treatment"), several.ok = TRUE)

  output <- match.arg(output, c("short", "verbose"))

  #---- Compile data ----
  env <- if(exists("VIEWS_NGPN")){VIEWS_NGPN} else {.GlobalEnv}
  tryCatch(
    macro_orig <- get("MacroPlot", envir = env),
    error = function(e){stop("MacroPlot table not found. Please import data.")})
  tryCatch(
    mm_projunit_orig <- get("MM_ProjectUnit_MacroPlot", envir = env),
    error = function(e){stop("MM_ProjectUnit_MacroPlot table not found. Please import data.")})
  tryCatch(
    regunit_orig <- get("RegistrationUnit", envir = env),
    error = function(e){stop("RegistrationUnit table not found. Please import data.")})
  tryCatch(
    projunit_orig <- get("ProjectUnit", envir = env),
    error = function(e){stop("ProjectUnit table not found. Please import data.")})

  # Standardize purpose across dataset
  macro_orig$MacroPlot_Purpose[macro_orig$MacroPlot_Purpose == "Panel 9"] <- "Panel9"
  macro_orig$MacroPlot_Purpose[macro_orig$MacroPlot_Purpose %in% c("FX", "Fx", "FX Monitoring", "FX monitoring", "Fire Effects Monitoring")] <-
    "FX Monitoring"
  macro_orig$MacroPlot_Purpose[macro_orig$MacroPlot_Purpose %in% c("FX Dual", "FX_Dual")] <- "FX Dual"

  macro_orig$MacroPlot_Purpose[macro_orig$MacroPlot_Purpose %in% c("Research", "research")] <- "Research"
  macro_orig$MacroPlot_Purpose[macro_orig$MacroPlot_Purpose %in% c("Determine Strategies for efficient early detection",
                                                           "Determine strategies for efficient early detection",
                                                           "Early Invasives Detection")] <- "Early Detection"
  macro_orig$MacroPlot_Purpose[macro_orig$MacroPlot_Purpose %in% c("FMH Grass Plot", "FMH Grass Plot ")] <- "FMH Grass Plot"
  macro_orig$MacroPlot_Purpose[macro_orig$MacroPlot_Purpose %in% c("FIRE_Intensive", "FIRE_intensive", "FX_Intensive",
                                                           "FX_ Intensive", "FIRE_intesive")] <-
    "FX Intensive"
  macro_orig$MacroPlot_Purpose[macro_orig$MacroPlot_Purpose %in% c("FIRE_Extensive")] <- "FX Extensive"
  macro_orig$MacroPlot_Purpose[macro_orig$MacroPlot_Purpose %in% c("Lafferty Plot", "Lafferty Plot ")] <- "Lafferty Plot"
  macro_orig$MacroPlot_Purpose[macro_orig$MacroPlot_Purpose %in% c("invasive research", "Invasives Research",
                                                           "Invasvies Research")] <- "Invasives Research"
  macro_orig$MacroPlot_Purpose[macro_orig$MacroPlot_Purpose %in% c("Modified Shrub Plot", "Modified Shrub Plot ")] <- "Modified Shrub Plot"
  macro_orig$MacroPlot_Purpose[macro_orig$MacroPlot_Purpose %in% c("Pre- and Post- treatment of fuels",
                                                           "pre- and post-treatment forest and fuels")] <- "Pre and post fuels treatment"
  # cleanup project and projectunit data
  projunit_orig$ProjectUnit_Agency <- "NPS"

  # Check that specified plot_name matches at least one record in the macroplot table
  plot_names <- sort(unique(macro_orig$MacroPlot_Name))
  typo_names <- plot_name[!plot_name %in% c(plot_names, "all")]
  if(length(typo_names) > 0){
      stop(paste0("The following specified plot names do not have matching records in the MacroPlot table: "),
           paste0(typo_names, collapse = ", "))}
  plot_list <- if(plot_name == "all"){plot_names} else {plot_name}

  # Make tables smaller before joins, so faster performance
  macro_a <- macro_orig[macro_orig$MacroPlot_Name %in% plot_list,]
  # Set purpose list if all or NGPN_VS selected
  purpose_list <- if(any(purpose == "all")){
    unique(macro_a$MacroPlot_Purpose)
  } else if(all(purpose == "NGPN_VS")){
    c(paste0("Panel", 1:10))
  } else {unique(purpose)}

  # filter macro table on purpose
  macro_b <- macro_a[macro_a$MacroPlot_Purpose %in% purpose_list, ]
  # extract guids from table above for further filtering
  guids_macro <- unique(macro_b$MacroPlot_GUID)
  guids_regunits <- unique(macro_b$MacroPlot_RegistrationUnit_GUID)
  # filter mm_projunit on macroplot guids
  mm_projunit <- mm_projunit_orig[mm_projunit_orig$MM_MacroPlot_GUID %in% guids_macro,]
  # filter regunit on registration_guids
  regunit <- regunit_orig[regunit_orig$RegistrationUnit_GUID %in% guids_regunits,]

  # Joining macroplot-relevant tables
  macro1 <- full_join(macro_b, mm_projunit,
                      by = c("MacroPlot_GUID" = "MM_MacroPlot_GUID", "datasource"))
  macro2 <- full_join(macro1, regunit, by = c("MacroPlot_RegistrationUnit_GUID" = "RegistrationUnit_GUID", "datasource"))
  macro3 <- full_join(macro2, projunit,
                      by = c("MacroPlot_RegistrationUnit_GUID" = "ProjectUnit_RegistrationUnitGUID",
                             "MM_ProjectUnit_GUID" = "ProjectUnit_GUID",
                             "datasource"))

  # Compile final dataset
  keep_cols <- c("MacroPlot_Name", "RegistrationUnit_Name",
                 "MacroPlot_Purpose", "MacroPlot_Type", "ProjectUnit_Name", "ProjectUnit_Agency",
                 "ProjectUnit_Description",
                 "MacroPlot_UTM_X", "MacroPlot_UTM_Y", "MacroPlot_UTMzone", "MacroPlot_Datum",
                 "MacroPlot_Elevation", "MacroPlot_ElevationUnits", "MacroPlot_Azimuth", "MacroPlot_Aspect",
                 "MacroPlot_SlopeHill", "MacroPlot_SlopeTransect",
                 "MacroPlot_StartPoint", "MacroPlot_Directions", "MacroPlot_Comment",
                 "MacroPlot_UV1", "MacroPlot_UV2", "MacroPlot_UV3", "MacroPlot_UV4", "MacroPlot_UV5",
                 "MacroPlot_UV6", "MacroPlot_UV7", "MacroPlot_UV8", "MacroPlot_Metadata",
                 "MacroPlot_GUID", "MacroPlot_Original_GUID", "MM_ProjectUnit_GUID",
                 "ProjectUnit_Original_GUID", "datasource")

  macro4 <-
  if(output == "short"){macro3[,keep_cols]
  } else {macro3}

  if(nrow(macro4) == 0){stop(paste0("Specified function arguments returned an empty data frame. ",
                                    "Check that the combination of plot_name and purpose arguments will return records."))}

  return(macro4)
  }
