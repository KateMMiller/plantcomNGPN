#' @title getMacroPlot
#'
#' @description This function filters macroplot data by plot name and purpose from FFI data.
#'
#' @importFrom dplyr filter full_join select
#'
#' @param plot_name Quoted string to return a particular plot based on name. Default is "all".
#' Can select multiple plots.
#'
#' @param purpose Quoted string to return plots with a particular purpose. Note that purpose
#' is not standard across parks. This function standardizes some purposes (eg "FX" and "Fire Effects" are both
#' called "FX monitoring". The following purposes that can be specified are below.
#' \describe{
#' \item{"Panel1"}{}
#' \item{"Panel2"}{}
#' \item{"Panel3"}{}
#' \item{"Panel4"}{}
#' \item{"Panel5"}{}
#' \item{"Panel6"}{}
#' \item{"Panel7"}{}
#' \item{"Panel8"}{}
#' \item{"Panel9"}{}
#' \item{"Panel10"}{}
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
#'
#'
#' @param output Quoted string. Options are "short" (default), which only returns most important columns.
#' "verbose" returns all columns in the MacroPlot database table.
#'
#' @examples
#' \dontrun{
#'
#' #+++ ADD EXAMPELS +++
#'
#' }
#'
#' @return Returns a data frame of macroplots
#'
#' @export

getMacroPlot <- function(plot_name = "all", purpose = "all"){

  #---- Bug handling ----
  # Can't do plot_name matching. Will at least check that specified plot names are included in the data.
  purpose <- match.arg(purpose, c("ABAM Supplemental", "AnnualBromeResearch",
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

  #---- Compile data ----
  env <- if(exists("VIEWS_NGPN")){VIEWS_NGPN} else {.GlobalEnv}

  tryCatch(
    macro <- get("MacroPlot", envir = env),
    error = function(e){stop("MacroPlot table not found. Please import data.")}
  )

  tryCatch(
    mm_projunit <- get("MM_ProjectUnit_MacroPlot", envir = env),
    error = function(e){stop("MM_ProjectUnit_MacroPlot table not found. Please import data.")}
  )

  tryCatch(
    regunit <- get("RegistrationUnit", envir = env),
    error = function(e){stop("RegistrationUnit table not found. Please import data.")}
  )

  tryCatch(
    projunit <- get("ProjectUnit", envir = env),
    error = function(e){stop("ProjectUnit table not found. Please import data.")}
  )

  # Joining macroplot-relevant tables
  macro1 <- full_join(macro, mm_projunit,
                      by = c("MacroPlot_GUID" = "MM_MacroPlot_GUID", "datasource"))
  macro2 <- full_join(macro1, regunit, by = c("MacroPlot_RegistrationUnit_GUID" = "RegistrationUnit_GUID", "datasource"))
  macro3 <- full_join(macro2, projunit,
                      by = c("MacroPlot_RegistrationUnit_GUID" = "ProjectUnit_RegistrationUnitGUID",
                             "MM_ProjectUnit_GUID" = "ProjectUnit_GUID",
                             "datasource"))

  # Standardize purpose across dataset
  macro3$MacroPlot_Purpose[macro3$MacroPlot_Purpose == "Panel 9"] <- "Panel9"
  macro3$MacroPlot_Purpose[macro3$MacroPlot_Purpose %in% c("FX", "Fx", "FX Monitoring", "FX monitoring", "Fire Effects Monitoring")] <-
    "FX Monitoring"
  macro3$MacroPlot_Purpose[macro3$MacroPlot_Purpose %in% c("FX Dual", "FX_Dual")] <- "FX Dual"

  macro3$MacroPlot_Purpose[macro3$MacroPlot_Purpose %in% c("Research", "research")] <- "Research"
  macro3$MacroPlot_Purpose[macro3$MacroPlot_Purpose %in% c("Determine Strategies for efficient early detection",
                                                         "Determine strategies for efficient early detection",
                                                         "Early Invasives Detection")] <- "Early Detection"
  macro3$MacroPlot_Purpose[macro3$MacroPlot_Purpose %in% c("FMH Grass Plot", "FMH Grass Plot ")] <- "FMH Grass Plot"
  macro3$MacroPlot_Purpose[macro3$MacroPlot_Purpose %in% c("FIRE_Intensive", "FIRE_intensive", "FX_Intensive",
                                                         "FX_ Intensive", "FIRE_intesive")] <-
    "FX Intensive"
  macro3$MacroPlot_Purpose[macro3$MacroPlot_Purpose %in% c("FIRE_Extensive")] <- "FX Extensive"
  macro3$MacroPlot_Purpose[macro3$MacroPlot_Purpose %in% c("Lafferty Plot", "Lafferty Plot ")] <- "Lafferty Plot"
  macro3$MacroPlot_Purpose[macro3$MacroPlot_Purpose %in% c("invasive research", "Invasives Research",
                                                         "Invasvies Research")] <- "Invasives Research"
  macro3$MacroPlot_Purpose[macro3$MacroPlot_Purpose %in% c("Modified Shrub Plot", "Modified Shrub Plot ")] <- "Modified Shrub Plot"
  macro3$MacroPlot_Purpose[macro3$MacroPlot_Purpose %in% c("Pre- and Post- treatment of fuels",
                                                         "pre- and post-treatment forest and fuels")] <- "Pre and post fuels treatment"
  # cleanup project and projectunit
}
