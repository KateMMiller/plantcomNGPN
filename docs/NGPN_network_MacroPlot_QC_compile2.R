# Params for troubleshooting ----
## These parameters come from NGPM_network_MacroPlot_Qc_Checks.Rmd

# library(plantcomNGPN)
# library(tidyverse) # dplyr, purrr, tidyr
# library(knitr) # for kable and include_graphic()
# library(kableExtra) # for custom kable features
# library(sf)
# library(data.table)
# library(DT)

# importData(type = 'local',
#            dbname = c("FFI_RA_AGFO", "FFI_RA_BADL", "FFI_RA_DETO", "FFI_RA_FOLA",
#                       "FFI_RA_FOUS", "FFI_RA_JECA", "FFI_RA_KNRI", #"FFI_RA_MNRR",
#                       "FFI_RA_MORU", "FFI_RA_SCBL", "FFI_RA_THRO", "FFI_RA_WICA"),
#            keep_tables = T)

# importData(type = 'csv', import_path = "./docs/data/NGPN_FFI_table_export_20260408.zip", keep_tables = T)
#
# all_years <- TRUE
# year_curr <- 2024
# year_range <- if(all_years == TRUE){2011:year_curr} else {year_curr}
# year_hist <- 2011:(year_curr - 1)

# Start of source code ----
## Loading data ----
# Species list
# tab4_spp <- read.csv("https://raw.githubusercontent.com/KateMMiller/plantcomNGPN/refs/heads/main/data/NGPN_PCM_Table_4_Tree_shrub_species_list.csv")
# or UPDATE PATH
tab4_spp <- read.csv("C:/Users/kbailey/Documents/Development/plantcomNGPN/data/NGPN_PCM_Table_4_Tree_shrub_species_list.csv")

# PCM Panel sampling schedule
# Path will need to be updated
panel_sch_wide <- read.csv("C:/Users/kbailey/Documents/Development/plantcomNGPN/data/panel_schedule.csv",
                           na.strings = "")

# pivot to longer
panel_sch <- panel_sch_wide |>
  pivot_longer(!Year,
               names_to = "Panel") |>
  drop_na() |>
  # filtering to current date (will update every year)
  filter(Year <= as.integer(format(Sys.Date(), "%Y"))) |>
  select(Year,
         Panel)

#THRO Panel sch
panel_sch_wide_thro <- read.csv("C:/Users/kbailey/Documents/Development/plantcomNGPN/data/THRO_panel_schedule.csv",
                           na.strings = "")

# pivot to longer
panel_sch_thro <- panel_sch_wide_thro |>
  pivot_longer(!Year,
               names_to = "Panel") |>
  drop_na() |>
  # filtering to current date (will update every year)
  filter(Year <= as.integer(format(Sys.Date(), "%Y"))) |>
  select(Year,
         Panel)

# Forest panel sampling schedule
# forest_sch_wide <- read.csv("C:/Users/kbailey/Documents/Development/plantcomNGPN/data/forest_panel_schedule.csv",
#                            na.strings = "")
#
# # pivot to linger
# forest_sch <- forest_sch_wide |>
#   pivot_longer(!Year,
#                names_to = "Panel") |>
#   drop_na()|>
#   # filtering to current date (will update every year)
#   filter(Year <= as.integer(format(Sys.Date(), "%Y"))) |>
#   select(Year,
#          Panel)

## Functions ----
### Summarize results of QC check
QC_check <- function(df, meas_type, tab, check, chk_type = "error"){
  result <- data.frame("Type" = meas_type, "Data" = tab,
                       "Description" = check, "Num_Records" = nrow(df), "check_type" = chk_type)
}

### function to make tables via kable
make_kable <- function(df, cap){
  QC_table <- if(nrow(df) > 0){
    if(nrow(df) > 1){
      kable(df, format = 'html', align = 'c', caption = cap)  |>
        kable_styling(fixed_thead = TRUE, bootstrap_options = c('condensed'),
                      full_width = TRUE, position = 'left', font_size = 12) |>
        row_spec(0, extra_css = "border-top: 1px solid #000000; border-bottom: 1px solid #000000;") |>
        collapse_rows(1, valign = 'top') |>
        row_spec(nrow(df), extra_css = 'border-bottom: 1px solid #000000;')
    } else if(nrow(df) == 1){
      kable(df, format = 'html', align = 'c', caption = cap)  |>
        kable_styling(fixed_thead = TRUE, bootstrap_options = c('condensed'),
                      full_width = TRUE, position = 'left', font_size = 12) |>
        row_spec(0, extra_css = "border-top: 1px solid #000000; border-bottom: 1px solid #000000;") |>
        row_spec(nrow(df), extra_css = 'border-bottom: 1px solid #000000;')
    }
  } else NULL

}

### function to make data tables
make_dt <- function(df, cap){
  datatable(df,
            class = 'cell-border stripe', rownames = FALSE,
            caption = cap,
            extensions = c("FixedColumns", "Buttons"),
            options = list(
              initComplete = htmlwidgets::JS(
                "function(settings, json) {",
                "$('body').css({'font-size': '11px'});",
                "$('body').css({'font-family': 'Arial'});",
                "$(this.api().table().header()).css({'font-size': '11px'});",
                "$(this.api().table().header()).css({'font-family': 'Arial'});",
                "}"),
              pageLength = nrow(df),
              autoWidth = FALSE, scrollX = '850px',
              scrollY = '600px', scrollCollapse = TRUE,
              fixedColumns = list(leftColumns = 1),
              dom = "Blfrtip", buttons = c('copy', 'csv', 'print')
            ),
            filter = list(position = c('top'), clear = FALSE))
}

### Determine whether to include/drop tab in rmd output
tab_include <- function(df){ifelse(nrow(df) > 0, TRUE, FALSE)}

### Determine if table exists or is null used in eval for rmd
check_null <- function(table){
  if(!is.null(table)){table}
}

check_null_print <- function(table, tab_level = 4, tab_title){
  if(!is.null(table)){cat(paste0(rep("#", tab_level), collapse = ""), " ", tab_title, " {.tabset} ", "\n\n")}
  check_null(table)
}

## Macroplot Checks ----
### Merging tables ----

#### Plot Matrix List
macro <- NGPN_tables$MacroPlot # list of plots and purpose
samp <- NGPN_tables$SampleEvent # list of sample events

#### formatting date
samp$SampleEvent_Date <-
  as.Date(substr(samp$SampleEvent_Date, 1, 11), format = "%Y-%m-%d")

#### selecting only pcm type plots
plots <- macro$MacroPlot_Name[grepl("_PCM_|_LPCM_|_FPCM_|_RCM_", macro$MacroPlot_Name)]
# plots <- macro$MacroPlot_Name[grepl("_PCM_|_RCM_", macro$MacroPlot_Name)]

### Macro plot df ----
macro_plots <- macro |>
  # filtering pcm plots
  filter(MacroPlot_Name %in% plots)|>
  # cleaning up columns
  select(MacroPlot_Name,
         MacroPlot_Purpose,
         MacroPlot_Type,
         MacroPlot_RegistrationUnit_GUID,
         MacroPlot_UTM_X,
         MacroPlot_UTM_Y,
         MacroPlot_UTMzone,
         MacroPlot_Datum,
         MacroPlot_DD_Lat,
         MacroPlot_DD_Long,
         MacroPlot_Elevation,
         MacroPlot_Aspect,
         MacroPlot_Azimuth,
         MacroPlot_SlopeHill,
         MacroPlot_SlopeTransect,
         MacroPlot_GUID) |>
  # naming parks
  mutate(park = substr(MacroPlot_Name, 1, 4)) |>
  # dropping duplicate plots
  distinct() #|>
  # removes HTLN legacy, treatment/control, and samples that don't have purpose or monitoring status from MacroPlot_Purpose
  # mutate(keep = ifelse(grepl("Panel|ForestStructure|FS", MacroPlot_Purpose)|
  #                        grepl("RCM", MacroPlot_Name) |
  #                        grepl("FPCM", MacroPlot_Name), 1, 0)) |>
  # filter(keep == 1) |> select(-keep)

### Macro plot/sample events ----
# joining macro plots with sample events
macro_samp <- left_join(macro_plots, samp, by = c("MacroPlot_GUID" = "SampleEvent_Plot_GUID")) |>
  # cleaning up columns
  select(park,
         MacroPlot_Name,
         MacroPlot_GUID,
         MacroPlot_Purpose,
         SampleEvent_GUID,
         SampleEvent_Date,
         # SampleEvent_DefaultMonitoringStatus
         ) |>
  # removing duplicates
  distinct()

# adding year
macro_samp$year <- as.numeric(format(as.Date(macro_samp$SampleEvent_Date,
                                             format = "%Y-%m-%d"), "%Y"))

# NA to blanks
# macro_samp$SampleEvent_DefaultMonitoringStatus[is.na(macro_samp$SampleEvent_DefaultMonitoringStatus)] <- "blank"

### Add monitoringstatus_base ----
monstat <- NGPN_tables$MonitoringStatus # monitoringstatus_base
mm_monstat_se <- NGPN_tables$MM_MonitoringStatus_SampleEvent # needed for GUID matching

# merge for correct GUID
macro_samp_ms1 <- left_join(macro_samp,
                            mm_monstat_se,
                            by = c("SampleEvent_GUID" = "MM_SampleEvent_GUID"))

# merge for monitoringstatus_base
macro_samp_ms2 <- left_join(macro_samp_ms1,
                            monstat,
                            by = c("MM_MonitoringStatus_GUID" = "MonitoringStatus_GUID",
                                   "datasource"))


macro_samp_ms <- macro_samp_ms2 |>
  # removing anything before 2011
  filter(year >= 2011) |>
  select(park,
         MacroPlot_Name,
         # MonitoringStatus_Base,
         MonitoringStatus_Name,
         MacroPlot_Purpose,
         # MacroPlot_GUID,
         # SampleEvent_DefaultMonitoringStatus,
         SampleEvent_Date,
         year)

# Fire Filtering ----
samp_events_fire <- macro_samp_ms |>
  mutate(keep = ifelse(grepl("Pre|Burn|Post|yr",
                             MonitoringStatus_Name), 1, 0)) |>
  filter(keep == 1) |>  select(-keep) |>
  arrange(MacroPlot_Name, year)

# getting MonStat_Name plot
samp_events_count_fire <- samp_events_fire |>
  # distinct() |>
  summarize(count = n(),
            .by = c(park,
                    MonitoringStatus_Name,
                    year)) |>
  arrange(year) |>
  pivot_wider(names_from = year,
              values_from = count) |>
  arrange(park, MonitoringStatus_Name)

## Panel Filter ----
# removing sample events from macro_samp_ms that don't have a panel_yr match (keeping all observations for panel_sch)
# samp_events_all <- right_join(macro_samp_ms,
#                               panel_sch,
#                               by = c("MacroPlot_Purpose" = "Panel",
#                                      "year" = "Year"))

# THRO filtering
samp_event_thro <- macro_samp_ms |>
  filter(park == "THRO") |>
  # semi joining with panel sch
  semi_join(panel_sch_thro,
            by = c("MacroPlot_Purpose" = "Panel",
                   "year" = "Year"))

# Other PCM filtering
samp_events_all <- macro_samp_ms |>
  filter(!park == "THRO") |>
  semi_join(panel_sch,
            by = c("MacroPlot_Purpose" = "Panel",
                   "year" = "Year")) |>
  bind_rows(samp_event_thro)

# filtering plots that NEED a monitoring status name
samp_events_name <- samp_events_all |>
  filter(is.na(MonitoringStatus_Name)) |>
  select(park,
         MacroPlot_Name,
         SampleEvent_Date,
         # MacroPlot_GUID,
         # MonitoringStatus_Base,
         MonitoringStatus_Name) |>
  arrange(SampleEvent_Date, MacroPlot_Name)

# getting count for each panel-year
samp_events <- samp_events_all |>
  # filtering anything that isn't plant community samples
  mutate(keep = case_when(!grepl("Fire",
                                 MonitoringStatus_Name) &
                          grepl("PlantCommunity",
                                MonitoringStatus_Name) ~ 1,
                          grepl("Dual",
                                MonitoringStatus_Name) ~ 1,
                          TRUE ~ 0)) |>
  filter(keep == 1) |> select(-keep) |>
  arrange(MacroPlot_Name, year)

samp_events_indv <- samp_events |>
  distinct()

# samp_events_b <- samp_events_all |>
#   # filtering anything that isn't plant community samples
#   mutate(keep = ifelse(grepl("PlantCommunity",
#                              MonitoringStatus_Base), 1, 0)) |>
#   filter(keep == 1) |> select(-keep) |>
#   arrange(MacroPlot_Name, year)

# checking duplicate rows
sampe_events_dup <- samp_events[duplicated(samp_events) | duplicated(samp_events, fromLast = TRUE), ]

## Sample event counts ----
samp_events_count <- samp_events |>
  distinct() |>
  summarize(count = n(),
            .by = c(park,
                    MacroPlot_Purpose,
                    year)) |>
  pivot_wider(names_from = MacroPlot_Purpose,
              values_from = count) |>
  arrange(park, year)


#   group_by(park, MacroPlot_Name, MonitoringStatus_Base, MacroPlot_Purpose, year, type) |>
#   summarize(num_recs = sum(!is.na(year)), .groups = 'drop') |>
#   arrange(year, MonitoringStatus_Base) |>
#   pivot_wider(names_from = year, values_from = num_recs, names_prefix = 'yr') |>
#   # mutate(plotnum = as.numeric(gsub("\\D", "", MacroPlot_Name)),
#   #        plottype = ifelse(grepl("_LPCM", MacroPlot_Name), 1, 0)) |>
#   arrange(MacroPlot_Name, MacroPlot_Purpose, MonitoringStatus_Base, type)
#
# # Fire Plots
# macro_samp_ms_fpcm <- macro_samp_ms |>
#   filter(type == "fire") |>
#   select(park, MacroPlot_Name, MacroPlot_Purpose, MonitoringStatus_Base, yr2011:last_col())
#
# # NGPN Plots
# macro_samp_ms_pcm <- macro_samp_ms |>
#   filter(type == "ngpn") |>
#   select(park, MacroPlot_Name, MacroPlot_Purpose, MonitoringStatus_Base, yr2011:last_col())

## Creating Park List ----
park_list <- sort(unique(macro_plots$park))
park_list_name <- sort(unique(samp_events_name$park))
park_list_samp <- sort(unique(samp_events_count$park))
park_list_samp_fire <- sort(unique(samp_events_count_fire$park))
# park_list2 <- sort(unique(macro_samp_ms_fpcm$park))

# NGPN dups
# macro_samp_ms_pcm_dups <- macro_samp_ms_pcm |>
#   group_by(MacroPlot_Name) |>
#   summarize(num_monstat = sum(!is.na(MonitoringStatus_Base))) |>
#   filter(num_monstat > 1) |> select(MacroPlot_Name)
#
# # macro_samp_ms$dup_ms <- ifelse(macro_samp_ms$MacroPlot_Name %in% macro_samp_ms_dups$MacroPlot_Name, 1, 0)
#
# # writing for plot checks from NGPN staff
# # writexl::write_xlsx()
#
#
# #### Purpose ---------
# macro2 <- left_join(macro_plots, NGPN_tables$MM_ProjectUnit_MacroPlot,
#                     by = c("MacroPlot_GUID" = "MM_MacroPlot_GUID"))
# macroproj <- left_join(macro2, NGPN_tables$ProjectUnit,
#                        by = c("MM_ProjectUnit_GUID" = "ProjectUnit_GUID", "datasource"),
#                        relationship = "many-to-many")
#
# macroproj2 <- macroproj |>
#   # mutate(park = substr(datasource, nchar(datasource)-3, nchar(datasource))) |>
#   select(park, MacroPlot_Name, MacroPlot_Purpose, MacroPlot_Type,
#          ProjectUnit_Name, ProjectUnit_Agency, MacroPlot_GUID) |>
#   arrange(MacroPlot_Name)
#
# macroproj_dups <- macroproj2 |>
#   group_by(MacroPlot_Name, MacroPlot_GUID,
#            MacroPlot_Purpose, ProjectUnit_Name) |>
#   summarize(num_rows = sum(!is.na(park)), .groups = 'drop') |>
#   arrange(ProjectUnit_Name) |>
#   select(-MacroPlot_GUID) |>
#   pivot_wider(names_from = ProjectUnit_Name,
#               values_from = num_rows) |>
#   data.frame() |>
#   #select(MacroPlot_Name, MacroPlot_Purpose, park, everything()) |>
#   arrange(MacroPlot_Name) |>
#   mutate(park = substr(MacroPlot_Name, 1, 4))
#
# start_cols <- c("MacroPlot_Name", "MacroPlot_Purpose", "Park")
# macroproj_dups <- macroproj_dups[, c(start_cols, sort(setdiff(names(macroproj_dups), start_cols)))]
#
# macroproj_dups$num_recs <- apply(macroproj_dups[,3:ncol(macroproj_dups)], 1, function(x) sum(!is.na(x)))
# macroproj_dups$nonvs <- grepl("Panel|IM_Intensive", macroproj_dups$MacroPlot_Purpose)
#
#
# macro_purp1 <- macro_plots |>
#   select(MacroPlot_Name, MacroPlot_Purpose) |>
#   #unique() |>
#   mutate(pres = 1,
#          MacroPlot_Purpose = ifelse(is.na(MacroPlot_Purpose) |
#                                       MacroPlot_Purpose == "", "Unknown", MacroPlot_Purpose)) |>
#   arrange(MacroPlot_Purpose) |>
#   pivot_wider(names_from = MacroPlot_Purpose, values_from = pres) |>
#   arrange(MacroPlot_Name)
#
# start_cols <- c("MacroPlot_Name", "Panel1", "Panel2", "Panel3", "Panel4", "Panel5", "Panel6",
#                 "Panel7", "Panel8", "Panel9", "Panel10", "PanelE")#, "IM_Intensive",
#                 #"IM_veg", "IM_FX_Dual")
# other_cols <- sort(setdiff(names(macro_purp1), start_cols))
#
# macro_purp <- macro_purp1[,c(start_cols, other_cols)]
#
### Sample Event Checks {.tabset} ---------
macro_guids <- unique(macro_plots$MacroPlot_GUID) # NGPN macroplot_guids for filter
mm_projunit <- NGPN_tables$MM_ProjectUnit_MacroPlot
regunit <- NGPN_tables$RegistrationUnit
projunit <- NGPN_tables$ProjectUnit
monstat <- NGPN_tables$MonitoringStatus
mm_monstat_se = NGPN_tables$MM_MonitoringStatus_SampleEvent
sampev <- NGPN_tables$SampleEvent |> filter(SampleEvent_Plot_GUID %in% macro_guids)

macro1 <- left_join(macro_plots, mm_projunit,
                    by = c("MacroPlot_GUID" = "MM_MacroPlot_GUID"))
macro2 <- left_join(macro1, regunit, by = c("MacroPlot_RegistrationUnit_GUID" = "RegistrationUnit_GUID", "datasource"))
macro3 <- left_join(macro2, projunit,
                    by = c("MacroPlot_RegistrationUnit_GUID" = "ProjectUnit_RegistrationUnitGUID",
                           "MM_ProjectUnit_GUID" = "ProjectUnit_GUID",
                           "datasource")) |> unique()

mac_samp <- left_join(macro3, sampev, by = c("MacroPlot_GUID" = "SampleEvent_Plot_GUID", "datasource"),
                      relationship = "many-to-many")

# Plots in MacroPlot table that don't have a corresponding SampleEvent:
miss_samp <- mac_samp |> filter(is.na(SampleEvent_GUID)) |>
  select(MacroPlot_Name, MacroPlot_Purpose, ProjectUnit_Name)

QC_table <- QC_check(df = miss_samp, meas_type = "SampleEvent", tab = "No SampleEvents",
                     check = "NGPN PCM MacroPlots no accompanying SampleEvents.",
                     chk_type = 'error')

kbl_miss_samp <- make_kable(miss_samp, cap = "NGPN PCM MacroPlots with no accompanying SampleEvents.")

# check if Sample Event - General checks returned at least 1 record to determine whether to include that tab in report
sampev_gen_check <- QC_table |> filter(Type %in% "SampleEvent" & Data %in% "General" & Num_Records > 0)
sampev_gen_include <- tab_include(sampev_gen_check)

# Checks on monitoring status.
mac_samp_mm <- left_join(mac_samp, mm_monstat_se, by= c("SampleEvent_GUID" = "MM_SampleEvent_GUID", "datasource"),
                         relationship = 'many-to-many')
mac_samp_monstat <- left_join(mac_samp_mm, monstat,
                              by = c("MM_MonitoringStatus_GUID" = "MonitoringStatus_GUID",
                                     "datasource"))

mac_samp_monstat$SampleEvent_Date <-
  as.Date(substr(mac_samp_monstat$SampleEvent_Date, 1, 11), format = "%Y-%m-%d")
mac_samp_monstat$year <- format(as.Date(mac_samp_monstat$SampleEvent_Date, format = "%Y-%m-%d"), "%Y")
mac_samp_monstat$month <- format(as.Date(mac_samp_monstat$SampleEvent_Date, format = "%Y-%m-%d"), "%m")
mac_samp_monstat$doy <- format(as.Date(mac_samp_monstat$SampleEvent_Date, format = "%Y-%m-%d"), "%j")

keep_cols <- c("MacroPlot_Name", "RegistrationUnit_Name", "MacroPlot_Purpose", "MacroPlot_Type",
               #"ProjectUnit_Name",
               "MacroPlot_UTM_X", "MacroPlot_UTM_Y",
               "MacroPlot_DD_Lat", "MacroPlot_DD_Long", "MacroPlot_Elevation",
               "MacroPlot_Aspect", "MacroPlot_Azimuth", "MacroPlot_SlopeHill", "MacroPlot_SlopeTransect",
               "SampleEvent_Date", "year", "month", "doy", #"SampleEvent_DefaultMonitoringStatus",
               "MonitoringStatus_Name", "MonitoringStatus_UV1",
               "MacroPlot_GUID", "SampleEvent_GUID", "MM_MonitoringStatus_GUID")

mac_samp_monstat2 <- mac_samp_monstat[,keep_cols]
mac_samp_monstat3 <- mac_samp_monstat2 |> filter(year >= 2011) |>
  select(MacroPlot_Name, SampleEvent_Date, year, MonitoringStatus_Name) |> unique() |>
  group_by(MacroPlot_Name, SampleEvent_Date, year, #ProjectUnit_Name,
           MonitoringStatus_Name, #SampleEvent_DefaultMonitoringStatus,
  ) |>
  summarize(num_samps = sum(!is.na(SampleEvent_Date)), .groups = 'drop')

mac_samp_monstat3$year_match <-
  ifelse(substr(mac_samp_monstat3$MonitoringStatus_Name, 1, 4) %in% 2010:2024, 1, 0)

mac_samp_monstat4 <- mac_samp_monstat3 |>
  filter(year_match == 1) |>
  filter(nchar(MonitoringStatus_Name) > 4) |>
  filter(!grepl("Other", MonitoringStatus_Name)) |>
  group_by(MacroPlot_Name, SampleEvent_Date, year, MonitoringStatus_Name) |>
  summarize(num_samps = sum(!is.na(year)), .groups = 'drop')

mac_samp_monstat4$year_mismatch <- ifelse(
  mac_samp_monstat4$year != substr(mac_samp_monstat4$MonitoringStatus_Name, 1, 4), 1, 0)

monstat_yr_mismatch <- mac_samp_monstat4 |> filter(year_mismatch == 1) |> select(-num_samps, -year_mismatch)

QC_table <- rbind(QC_table,
                  QC_check(df = monstat_yr_mismatch, meas_type = "SampleEvent", tab = "Year Mismatch",
                           check = "NGPN PCM plots with mismatch in year of SampleEvent_Date, and MonitoringStatus_Name.",
                           chk_type = 'error'))

kbl_monstat_yr_mismatch <- kable(monstat_yr_mismatch, format = "html", align = 'c',
                                 caption = "NGPN PCM plots with mismatch in year of SampleEvent_Date, and MonitoringStatus_Name.") |>
  kable_styling(fixed_thead = T, bootstrap_options = c("condensed", "striped"),
                full_width = T, position = 'left', font_size = 10) |>
  column_spec(1:ncol(monstat_yr_mismatch), border_left = "1px solid grey", border_right = "1px solid grey")

mac_samp_monstat4$monstat <-
  substr(mac_samp_monstat4$MonitoringStatus_Name, 6, nchar(mac_samp_monstat4$MonitoringStatus_Name))

monstat_typo <- mac_samp_monstat4 |> group_by(MacroPlot_Name, monstat) |>
  summarize(years = paste0(year, collapse = ", "),
            .groups = 'drop') |>
  pivot_wider(names_from = monstat, values_from = years)

monstat_typo$inconsist <- rowSums(!is.na(monstat_typo[,2:ncol(monstat_typo)]))
monstat_incon <- monstat_typo |> filter(inconsist > 1) |> arrange(MacroPlot_Name) |>
  filter(inconsist > 1 | !is.na(`Plant Community`) | !is.na(Dual) | !is.na(PCM_Fire) |
           !is.na(Dual))

monstat_incon2 <- monstat_incon[,c("MacroPlot_Name", sort(names(monstat_incon[,2:(ncol(monstat_incon)-1)])))]

QC_table <- rbind(QC_table,
                  QC_check(df = monstat_incon2, meas_type = "SampleEvent", tab = "Name Inconsistencies",
                           check = "NGPN PCM plots with inconsistently labeled MonitoringStatus_Name.",
                           chk_type = 'error')
)

# kbl_monstat_incon <- kable(monstat_incon2, format = "html", align = 'c',
#                            caption = "NGPN PCM plots with inconsistently labeled MonitoringStatus_Name.
#                            Plots may be used for different monitoring purposes, but also seems some are incorrect.
#                            The years in the cell are years that a given monitoring status was recored (eg 2013 in
#                            PlantCommunity means there's a monitoring status name for that plot called '2013_PlantCommunity').
#                            Note that the first Plant Community column has a space between the words.") |>
#   kable_styling(fixed_thead = T, bootstrap_options = c("condensed", "striped"),
#                 full_width = T, position = 'left', font_size = 10) |>
#   column_spec(1:ncol(monstat_incon2), border_left = "1px solid grey", border_right = "1px solid grey")

dt_monstat_incon <- make_dt(monstat_incon2, "NGPN PCM plots with inconsistently labeled MonitoringStatus_Name.
                           Plots may be used for different monitoring purposes, but also seems some are incorrect.
                           The years in the cell are years that a given monitoring status was recored (eg 2013 in
                           PlantCommunity means there's a monitoring status name for that plot called '2013_PlantCommunity').
                           Note that the first Plant Community column has a space between the words.")

# check if Sample Event - Monitoring Status checks returned at least 1 record to determine whether to include that tab in report
sampev_ms_check <- QC_table |> filter(Type %in% "SampleEvent" & Data %in% "Monitoring Status" & Num_Records > 0)
sampev_ms_include <- tab_include(sampev_ms_check)

# check if Sample Event checks returned at least 1 record to determine whether to include that tab in report
sampev_check <- QC_table |> filter(Type %in% "SampleEvent" & Num_Records > 0)
sampev_include <- tab_include(sampev_check)

###### Compile final QC Table ######
# revise for different color combos for checks (99 vs 90)? Drop for checks vs. errors?
QC_cap <- "The table below documents Quality Control checks performed on NGPN Plant Community Monitoring data
that are stored in the FFI database. This report primarily checks data that are entered previously, and that
once fixed are less likely to introduce errors again, compared with the 'Data Entry QC' report, which is designed
to check data that are collected annually. If records are returned for a given check, the row is highlighted yellow
for errors and blue for records that aren't necessarily errors, but need further review (e.g., large DBH measurements).
A separate tab corresponding to each check that returned results by protocol module."

QC_check_table <- kable(QC_table, format = 'html', align = 'c', caption = QC_cap,
                        col.names = c("Type", "Data Tab", "Check Description", "Number of Records", "Check Type")) |>
  kable_styling(fixed_thead = TRUE, bootstrap_options = c('condensed'),
                full_width = TRUE, position = 'left', font_size = 12) |>
  row_spec(0, extra_css = "border-top: 1px solid #000000; border-bottom: 1px solid #000000;") |>
  column_spec(3, width = "300px") |>
  column_spec(2:ncol(QC_table), background =
                ifelse(QC_table$Num_Records > 0 & QC_table$check_type == "error", "#F2F2A0",
                       ifelse(QC_table$Num_Records > 0 & QC_table$check_type == "check", "#b7d8ef", "#ffffff"))) |>
  collapse_rows(1, valign = 'top') |>
  row_spec(nrow(QC_table), extra_css = 'border-bottom: 1px solid #000000;')

