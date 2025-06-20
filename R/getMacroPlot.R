#' @title getMacroPlot
#'
#' @description This function filters FFI macroplot data by park, plot name, project, and purpose.
#' This function was primarily developed to pull out NGPN plant community monitoring plots. Using
#' combinations of plot names, projects or purposes that are outside NGPN PCM plots hasn't been
#' tested as thoroughly, and may not return intended results in every case. Note that this is more
#' of an internal function that other data-related getter functions source to correctly link table and
#' filter on records.
#'
#' @importFrom dplyr left_join
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
#' \describe{
#' \item{'all'}{Pull in all project types.}
#' \item{"Park"}{Default. *NGPN VS* stratum covering whole park.}
#' \item{"ABAM"}{*NGPN VS* stratum in WICA.}
#' \item{"Bodmer"}{*NGPN VS* stratum in FOUS.}
#' \item{"Cedar Removal Study"}{*NGPN VS* in MNRR.}
#' \item{"Deciduous Woodland"}{*NGPN VS* covers KNRI (2 plots) and THROS (1 plot).}
#' \item{"Fort"}{*NGPN VS* stratum in FOUS.}
#' \item{"Monitoring"}{*NGPN VS* stratum in MNRR.}
#' \item{"Native Prairie"}{*NGPN VS* stratum in AGFO.}
#' \item{"North Riparian"}{*NGPN VS* stratum in THRO.}
#' \item{"North Upland"}{*NGPN VS* stratum in THRO.}
#' \item{"North Unit"}{*NGPN VS* stratum in BADL.}
#' \item{"Pine Forest"}{*NGPN VS* stratum in DETO, JECA, MORU, and WICA.}
#' \item{"Prairie"}{*NGPN VS* stratum in BADL, DETO, FOUS, KNRI, SCBL, THRO, and WICA.}
#' \item{"Riparian"}{*NGPN VS* stratum in AGFO, DETO, and FOLA.}
#' \item{"Shrubland"}{*NGPN VS* stratum in THRO.}
#' \item{"South Riparian"}{*NGPN VS* stratum in THRO.}
#' \item{"South Upland"}{*NGPN VS* stratum in THRO.}
#' \item{"Upland"}{*NGPN VS* stratum in DETO and FOLA.}
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
#' \describe{
#' \item{"all"}{All plots in imported FFI database}
#' \item{"NGPN_VS"}{Default. NGPN Plant Community Monitoring Plots with c("_PCM_", "_FPCM_", "_LPCM_", and, "_RCM_") in their name}
#' \item{"Panel1"}{NGPN PCM Panel 1}
#' \item{"Panel2"}{NGPN PCM Panel 2}
#' \item{"Panel3"}{NGPN PCM Panel 3}
#' \item{"Panel4"}{NGPN PCM Panel 4}
#' \item{"Panel5"}{NGPN PCM Panel 5}
#' \item{"Panel6"}{NGPN PCM Panel 6}
#' \item{"Panel7"}{NGPN PCM Panel 7}
#' \item{"Panel8"}{NGPN PCM Panel 8}
#' \item{"Panel9"}{NGPN PCM Panel 9}
#' \item{"Panel10"}{NGPN PCM Panel 10}
#' \item{"PanelE"}{NGPN PCM Extensive. Found in DETO, FOLA, JECA, MORU, SCBL, and THRO.}
#' \item{"ABAM Supplemental"}{Supplemental plots related to ABAM. Only found in BADL, FOLA, and WICA}
#' \item{"AnnualBromeResearch"}{Annual Brome Research in BADL and SCBL}
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
#' @param output Quoted string. Options are "short" (default), which only returns most important columns;
#' "verbose" returns all columns in the MacroPlot database table.
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
#' # return all NGPN Plant Community Monitoring plots (ie vital signs plots),
#' # for the Park stratum and all purposes used by NGPN
#' macro_vs <- getMacroPlot()
#' table(macro_vs$RegistrationUnit_Name, macro_vs$ProjectUnit_Name)
#'
#' # return Prairie stratum for AGFO and SCBL for NGPN_VS plots
#' macro_pr_vs <- getMacroPlot(park = c("AGFO", "SCBL"), purpose = "NGPN_VS",
#'   project = c("Native Prairie", "Prairie"))
#' table(macro_pr_vs$RegistrationUnit_Name, macro_pr_vs$ProjectUnit_Name)
#'
#' # query all parks, plots and purposes, and only return main columns
#' macro <- getMacroPlot(purpose = "all", project = "all")
#' table(macro$RegistrationUnit_Name, macro$ProjectUnit_Name)
#'
#' # query NGPN only plots from North Dakota
#' macro_nd <- getMacroPlot(park = c("FOUS", "KNRI", "THRO"))
#' table(macro_nd$RegistrationUnit_Name, macro_nd$ProjectUnit_Name)
#'
#' # query only North and South Upland for HTRO
#' thro_up <- getMacroPlot(park = "THRO", project = c("North Upland", "South Upland"))
#'
#' }
#'
#' @return Returns a data frame of macroplots
#'
#' @export

getMacroPlot <- function(park = 'all', plot_name = "all", project = "Park", purpose = "NGPN_VS",
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

  output <- match.arg(output, c("short", "verbose"))

  #---- Compile data ----
  env <- if(exists("NGPN_tables")){NGPN_tables} else {.GlobalEnv}
  tryCatch(
    macro_orig <- get("MacroPlot", envir = env),
    error = function(e){stop("MacroPlot table not found. Please import data.")})
  tryCatch(
    mm_projunit <- get("MM_ProjectUnit_MacroPlot", envir = env),
    error = function(e){stop("MM_ProjectUnit_MacroPlot table not found. Please import data.")})
  tryCatch(
    projunit <- get("ProjectUnit", envir = env),
    error = function(e){stop("ProjectUnit table not found. Please import data.")})
  tryCatch(
    regunit <- get("RegistrationUnit", envir = env),
    error = function(e){stop("RegistrationUnit table not found. Please import data.")})

  # Standardize purpose and project across dataset
  macro_orig$MacroPlot_Purpose[macro_orig$MacroPlot_Purpose == "Panel 9"] <- "Panel9"
  macro_orig$MacroPlot_Purpose[macro_orig$MacroPlot_Purpose %in%
                                 c("FX", "Fx", "FX Monitoring", "FX monitoring", "Fire Effects Monitoring")] <- "FX Monitoring"
  macro_orig$MacroPlot_Purpose[macro_orig$MacroPlot_Purpose %in% c("FX Dual", "FX_Dual")] <- "FX Dual"
  macro_orig$MacroPlot_Purpose[macro_orig$MacroPlot_Purpose %in% c("Research", "research")] <- "Research"
  macro_orig$MacroPlot_Purpose[macro_orig$MacroPlot_Purpose %in% c("Determine Strategies for efficient early detection",
                                                           "Determine strategies for efficient early detection",
                                                           "Early Invasives Detection")] <- "Early Detection"
  macro_orig$MacroPlot_Purpose[macro_orig$MacroPlot_Purpose %in% c("FMH Grass Plot", "FMH Grass Plot ")] <- "FMH Grass Plot"
  macro_orig$MacroPlot_Purpose[macro_orig$MacroPlot_Purpose %in% c("FIRE_Intensive", "FIRE_intensive", "FX_Intensive",
                                                           "FX_ Intensive", "FIRE_intesive")] <- "FX Intensive"
  macro_orig$MacroPlot_Purpose[macro_orig$MacroPlot_Purpose %in% c("FIRE_Extensive")] <- "FX Extensive"
  macro_orig$MacroPlot_Purpose[macro_orig$MacroPlot_Purpose %in% c("Lafferty Plot", "Lafferty Plot ")] <- "Lafferty Plot"
  macro_orig$MacroPlot_Purpose[macro_orig$MacroPlot_Purpose %in% c("invasive research", "Invasives Research",
                                                           "Invasvies Research")] <- "Invasives Research"
  macro_orig$MacroPlot_Purpose[macro_orig$MacroPlot_Purpose %in% c("Modified Shrub Plot", "Modified Shrub Plot ")] <- "Modified Shrub Plot"
  macro_orig$MacroPlot_Purpose[macro_orig$MacroPlot_Purpose %in% c("Pre- and Post- treatment of fuels",
                                                           "pre- and post-treatment forest and fuels")] <- "Pre and post fuels treatment"
  macro_orig$MacroPlot_Purpose[macro_orig$MacroPlot_Purpose %in% c("FS")] <- "ForestStructure" # KNRI_PCM_038

  projunit$ProjectUnit_Name[projunit$ProjectUnit_Name %in% c("IN-ACTIVE", "In-Active", "Inactive")] <- "IN-ACTIVE"

  # cleanup project and projectunit data
  projunit$ProjectUnit_Agency <- "NPS"
  NGPN_plots <- macro_orig$MacroPlot_Name[grepl("_PCM_|_LPCM_|_FPCM_|_RCM_", macro_orig$MacroPlot_Name)]

  # Check that specified plot_name matches at least one record in the macroplot table
  plot_names <- sort(unique(macro_orig$MacroPlot_Name))
  typo_names <- plot_name[!plot_name %in% c(plot_names, "all")]
  if(length(typo_names) > 0){
      stop(paste0("The following specified plot names do not have matching records in the MacroPlot table: "),
           paste0(typo_names, collapse = ", "))}

  # Create plot list to filter on [may need to tweak this depending on if needs outside NGPN_VS]
  plot_list1 <- if(any(plot_name == "all")){plot_names} else {plot_name}
  plot_list <- if(all(purpose == "NGPN_VS") & any(plot_name == "all")){NGPN_plots
  } else if(!any(plot_name %in% "all") & any(purpose == "NGPN_VS")){intersect(plot_list1, NGPN_plots)
  } else if(any(plot_name %in% "all") & any(purpose %in% "all") & any(project %in% "all")){plot_list1
  } else {plot_name}

  # Make tables smaller before joins, so faster performance
  macro_a <- macro_orig[macro_orig$MacroPlot_Name %in% plot_list,]

  # Set purpose list if all is selected
  purpose_list <- if(any(purpose %in% c("all", "NGPN_VS"))){
    unique(macro_a$MacroPlot_Purpose)
  } else {unique(purpose)}

  # filter macro table on purpose
  macro_b <- macro_a[macro_a$MacroPlot_Purpose %in% purpose_list, ]

  # Joining macroplot-relevant tables
  macro1 <- left_join(macro_b, mm_projunit,
                      by = c("MacroPlot_GUID" = "MM_MacroPlot_GUID", "datasource"))
  macro2 <- left_join(macro1, regunit, by = c("MacroPlot_RegistrationUnit_GUID" = "RegistrationUnit_GUID", "datasource"))
  macro3 <- left_join(macro2, projunit,
                      by = c("MacroPlot_RegistrationUnit_GUID" = "ProjectUnit_RegistrationUnitGUID",
                             "MM_ProjectUnit_GUID" = "ProjectUnit_GUID",
                             "datasource"))
  # filter on park
  macro4 <- macro3[macro3$RegistrationUnit_Name %in% park,]

  # Set project list if all or NGPN_VS selected
  project_list <- if(any(project %in% c("all", "NGPN_VS"))){
    unique(macro3$ProjectUnit_Name)
  } else {unique(project)}

  macro5 <- macro4[macro4$ProjectUnit_Name %in% project_list,]

  #macro5 <- macro5[,keep_cols]
  # Return only park project for plots found in multiple strata, so only 1 record per plot returned
  # dup_plots <- macro5 |> group_by(MacroPlot_Name) |>
  #   summarize(num_recs = sum(!is.na(ProjectUnit_Name)), .groups = 'drop')
  #
  # macro6 <- left_join(macro5, dup_plots, by = "MacroPlot_Name") |>
  #   mutate(keep = ifelse(!is.na(num_recs) & ProjectUnit_Name == "Park", 1,
  #                        ifelse(num_recs == 1, 1, 0))) |>
  #   filter(keep == 1) |> select(-keep)


  # Compile final dataset
  keep_cols <- c("MacroPlot_Name", "RegistrationUnit_Name",
                 "MacroPlot_Purpose", "MacroPlot_Type", "ProjectUnit_Name", "ProjectUnit_Agency",
                 "ProjectUnit_Description",
                 "MacroPlot_UTM_X", "MacroPlot_UTM_Y", "MacroPlot_UTMzone", "MacroPlot_Datum",
                 "MacroPlot_DD_Lat", "MacroPlot_DD_Long",
                 "MacroPlot_Elevation", "MacroPlot_ElevationUnits", "MacroPlot_Azimuth", "MacroPlot_Aspect",
                 "MacroPlot_SlopeHill", "MacroPlot_SlopeTransect",
                 "MacroPlot_StartPoint", "MacroPlot_Directions", "MacroPlot_Comment",
                 "MacroPlot_UV1", "MacroPlot_UV2", "MacroPlot_UV3", "MacroPlot_UV4", "MacroPlot_UV5",
                 "MacroPlot_UV6", "MacroPlot_UV7", "MacroPlot_UV8", "MacroPlot_Metadata",
                 "MacroPlot_GUID", "MacroPlot_Original_GUID", "MM_ProjectUnit_GUID",
                 "ProjectUnit_Original_GUID", "datasource")

  macro6 <-
  if(output == "short"){macro5[,keep_cols]
  } else {macro5}

  if(nrow(macro6) == 0){stop(paste0("Specified function arguments returned an empty data frame. ",
                                    "Check that the combination of plot_name and purpose arguments will return records."))}

  return(macro6)
  }
