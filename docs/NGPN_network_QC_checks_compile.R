# library(plantcomNGPN)
# library(tidyverse) # dplyr, purrr, tidyr
# library(knitr) # for kable and include_graphic()
# library(kableExtra) # for custom kable features
# library(sf)
# importData(type = 'local',
#            dbname = c("FFI_RA_AGFO", "FFI_RA_BADL", "FFI_RA_DETO", "FFI_RA_FOLA",
#                       "FFI_RA_FOUS", "FFI_RA_JECA", "FFI_RA_KNRI", #"FFI_RA_MNRR",
#                       "FFI_RA_MORU", "FFI_RA_SCBL", "FFI_RA_THRO", "FFI_RA_WICA"),
#            export = F)

### Functions
# Summarize results of QC check
QC_check <- function(df, meas_type, tab, check, chk_type){
  result <- data.frame("Type" = meas_type, "Data" = tab,
                       "Description" = check, "Num_Records" = nrow(df), "check_type" = chk_type)
}

# function to make tables via kable
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

# Determine whether to include/drop tab in rmd output
tab_include <- function(df){ifelse(nrow(df) > 0, TRUE, FALSE)}

# Determine if table exists or is null used in eval for rmd
check_null <- function(table){
  if(!is.null(table)){table}
}

check_null_print <- function(table, tab_level = 4, tab_title){
  if(!is.null(table)){cat(paste0(rep("#", tab_level), collapse = ""), " ", tab_title, " {.tabset} ", "\n\n")}
  check_null(table)
}

### Macroplot Checks
#### Plot Matrix List
macro <- NGPN_tables$MacroPlot
samp <- NGPN_tables$SampleEvent
monstat <- NGPN_tables$MonitoringStatus
mm_monstat <- NGPN_tables$MM_MonitoringStatus_SampleEvent

plots <- macro$MacroPlot_Name[grepl("_PCM_|_LPCM_|_FPCM_|_RCM_", macro$MacroPlot_Name)]

macro_plots <- macro |> mutate(park = substr(datasource, 8, 11)) |>
  filter(MacroPlot_Name %in% plots)|>
  select(MacroPlot_Name, MacroPlot_Purpose, MacroPlot_Type, MacroPlot_RegistrationUnit_GUID,
         MacroPlot_UTM_X, MacroPlot_UTM_Y, MacroPlot_UTMzone, MacroPlot_DD_Lat, MacroPlot_DD_Long,
         MacroPlot_Elevation, MacroPlot_Aspect, MacroPlot_Azimuth, MacroPlot_SlopeHill,
         MacroPlot_SlopeTransect, MacroPlot_GUID) |>
  mutate(park = substr(MacroPlot_Name, 1, 4)) |>
  unique()

macro_samp <- left_join(macro_plots, samp, by = c("MacroPlot_GUID" = "SampleEvent_Plot_GUID")) |>
  select(MacroPlot_Name, MacroPlot_GUID, MacroPlot_Purpose,
         SampleEvent_GUID, SampleEvent_Date, SampleEvent_DefaultMonitoringStatus) |>
  unique()

macro_samp$SampleEvent_Date <-
  format(as.Date(macro_samp$SampleEvent_Date, format = "%Y-%m-%d %H:%m:%s"), "%Y-%m-%d")
macro_samp$year <- format(as.Date(macro_samp$SampleEvent_Date, format = "%Y-%m-%d"), "%Y")
macro_samp$SampleEvent_DefaultMonitoringStatus[is.na(macro_samp$SampleEvent_DefaultMonitoringStatus)] <- "blank"

macro_samp2 <- macro_samp |> filter(year >= 2011) |>
  mutate(park = substr(MacroPlot_Name, 1, 4),
         SE_DefaultMonStatus_base = sub(".*\\_", "", SampleEvent_DefaultMonitoringStatus)) |>
  select(park, MacroPlot_Name, SE_DefaultMonStatus_base, MacroPlot_Purpose, year) |> unique() |>
  group_by(park, MacroPlot_Name, SE_DefaultMonStatus_base, MacroPlot_Purpose, year) |>
  summarize(num_recs = sum(!is.na(year)), .groups = 'drop') |>
  arrange(year, SE_DefaultMonStatus_base) |>
  pivot_wider(names_from = year, values_from = num_recs, names_prefix = 'yr') |>
  filter(grepl("blank|Dual|Fire|FirePlantCommunity|ForestStructure|PCM_Fire|Plant Community|PlantCommunity|Riparian",
               SE_DefaultMonStatus_base)) |>
  mutate(plotnum = as.numeric(gsub("\\D", "", MacroPlot_Name)),
         plottype = ifelse(grepl("_LPCM", MacroPlot_Name), 1, 0)) |>
  arrange(plottype, plotnum, SE_DefaultMonStatus_base) |>
  select(park, MacroPlot_Name, MacroPlot_Purpose, SE_DefaultMonStatus_base, yr2011:last_col()) |>
  select(-plotnum, -plottype)

park_list <- sort(unique(macro_plots$park))

macro_samp_dups <- macro_samp2 |> group_by(MacroPlot_Name) |> summarize(num_monstat = sum(!is.na(SE_DefaultMonStatus_base))) |>
  filter(num_monstat > 1) |> select(MacroPlot_Name)

macro_samp2$dup_ms <- ifelse(macro_samp2$MacroPlot_Name %in% macro_samp_dups$MacroPlot_Name, 1, 0)

# Add in monitoringstatus_base from monitoring status table
monstat <- NGPN_tables$MonitoringStatus
mm_monstat_se <- NGPN_tables$MM_MonitoringStatus_SampleEvent

macro_samp_ms1 <- left_join(macro_samp, mm_monstat_se, by = c("SampleEvent_GUID" = "MM_SampleEvent_GUID"))

macro_samp_ms2 <- left_join(macro_samp_ms1, monstat,
                            by = c("MM_MonitoringStatus_GUID" = "MonitoringStatus_GUID",
                                   "datasource"))

macro_samp_ms <- macro_samp_ms2 |> filter(year >= 2011) |>
  mutate(park = substr(MacroPlot_Name, 1, 4)) |>
  filter(grepl("blank|Dual|^Fire$|^Fire_$|FirePlantCommunity|ForestStructure|PCM_Fire|Plant Community|PlantCommunity|Riparian",
               MonitoringStatus_Base)) |>
  select(park, MacroPlot_Name, MonitoringStatus_Base, MacroPlot_Purpose, year) |> unique() |>
  group_by(park, MacroPlot_Name, MonitoringStatus_Base, MacroPlot_Purpose, year) |>
  summarize(num_recs = sum(!is.na(year)), .groups = 'drop') |>
  arrange(year, MonitoringStatus_Base) |>
  pivot_wider(names_from = year, values_from = num_recs, names_prefix = 'yr') |>
  mutate(plotnum = as.numeric(gsub("\\D", "", MacroPlot_Name)),
         plottype = ifelse(grepl("_LPCM", MacroPlot_Name), 1, 0)) |>
  arrange(plottype, plotnum, MonitoringStatus_Base) |>
  select(park, MacroPlot_Name, MacroPlot_Purpose, MonitoringStatus_Base, yr2011:last_col()) |>
  select(-plotnum, -plottype)

park_list <- sort(unique(macro_plots$park))

macro_samp_ms_dups <- macro_samp_ms |> group_by(MacroPlot_Name) |> summarize(num_monstat = sum(!is.na(MonitoringStatus_Base))) |>
  filter(num_monstat > 1) |> select(MacroPlot_Name)

macro_samp_ms$dup_ms <- ifelse(macro_samp_ms$MacroPlot_Name %in% macro_samp_ms_dups$MacroPlot_Name, 1, 0)


#### Purpose
macro2 <- left_join(macro_plots, NGPN_tables$MM_ProjectUnit_MacroPlot,
                    by = c("MacroPlot_GUID" = "MM_MacroPlot_GUID"))
macroproj <- left_join(macro2, NGPN_tables$ProjectUnit,
                       by = c("MM_ProjectUnit_GUID" = "ProjectUnit_GUID", "datasource"))

macroproj2 <- macroproj |>
  mutate(park = substr(datasource, nchar(datasource)-3, nchar(datasource))) |>
  select(park, MacroPlot_Name, MacroPlot_Purpose, MacroPlot_Type, ProjectUnit_Name, ProjectUnit_Agency,
         MacroPlot_GUID) |> arrange(MacroPlot_Name)

macroproj_dups <- macroproj2 |> group_by(MacroPlot_Name, MacroPlot_GUID, MacroPlot_Purpose, ProjectUnit_Name) |>
  summarize(num_rows = sum(!is.na(park)), .groups = 'drop') |>
  arrange(ProjectUnit_Name) |> select(-MacroPlot_GUID) |>
  pivot_wider(names_from = ProjectUnit_Name, values_from = num_rows) |> data.frame() |>
  #select(MacroPlot_Name, MacroPlot_Purpose, park, everything()) |>
  arrange(MacroPlot_Name) |> mutate(park = substr(MacroPlot_Name, 1, 4))

start_cols <- c("MacroPlot_Name", "MacroPlot_Purpose", "Park")
macroproj_dups <- macroproj_dups[, c(start_cols, sort(setdiff(names(macroproj_dups), start_cols)))]

macroproj_dups$num_recs <- apply(macroproj_dups[,3:ncol(macroproj_dups)], 1, function(x) sum(!is.na(x)))
macroproj_dups$nonvs <- grepl("Panel|IM_Intensive", macroproj_dups$MacroPlot_Purpose)


macro_purp1 <- macro_plots |> select(MacroPlot_Name, MacroPlot_Purpose) |>
  #unique() |>
  mutate(pres = 1,
         MacroPlot_Purpose = ifelse(is.na(MacroPlot_Purpose) |
                                      MacroPlot_Purpose == "", "Unknown", MacroPlot_Purpose)) |>
  arrange(MacroPlot_Purpose) |>
  pivot_wider(names_from = MacroPlot_Purpose, values_from = pres) |>
  arrange(MacroPlot_Name)

start_cols <- c("MacroPlot_Name", "Panel1", "Panel2", "Panel3", "Panel4", "Panel5", "Panel6",
                "Panel7", "Panel8", "Panel9", "Panel10", "PanelE", "IM_Intensive",
                "IM_veg", "IM_FX_Dual")
other_cols <- sort(setdiff(names(macro_purp1), start_cols))

macro_purp <- macro_purp1[,c(start_cols, other_cols)]


# MacroPlot Checks
# NGPN plots missing X/Y Coordinates
macro_miss_utm <- macro_plots |> filter(is.na(MacroPlot_UTM_X) | is.na(MacroPlot_UTM_Y) | is.na(MacroPlot_UTMzone)) |>
  select(MacroPlot_Name, MacroPlot_UTM_X, MacroPlot_UTM_Y, MacroPlot_UTMzone, MacroPlot_DD_Lat, MacroPlot_DD_Long)

QC_table <- QC_check(df = macro_miss_utm, meas_type = "MacroPlot", tab = "Plot Info",
                     check = "NGPN PCM plots missing UTM X, Y, and/or UTM Zone data.",
                     chk_type = 'error')

kbl_macro_miss_utm <- make_kable(macro_miss_utm, "NGPN PCM plots missing UTM X, Y, and/or Zone data. ") |>
  column_spec(2, background = ifelse(is.na(macro_miss_utm$MacroPlot_UTM_X), "#F2F2A0", "white")) |>
  column_spec(3, background = ifelse(is.na(macro_miss_utm$MacroPlot_UTM_Y), "#F2F2A0", "white")) |>
  column_spec(4, background = ifelse(is.na(macro_miss_utm$MacroPlot_UTMzone), "#F2F2A0", "white"))

park_list
# Set bounding box for each park and check UTMs and/or lat/long against them:
# First downloaded NPS Administrative Boundaries from: https://irma.nps.gov/DataStore/Reference/Profile/2309935

tryCatch(nps_bounds <- read_sf("./docs/www/Administrative Boundaries of National Park System Units.shp"), error = function(e){})
tryCatch(nps_bounds <- read_sf("./www/Administrative Boundaries of National Park System Units.shp"), error = function(e){})
st_crs(nps_bounds) # EPSG 4269
ngpn_poly <- nps_bounds |> filter(UNIT_CODE %in% park_list) |> arrange(UNIT_CODE)

AGFO_poly <- st_transform(ngpn_poly |> filter(UNIT_CODE == "AGFO"), crs = 26913)
BADL_poly <- st_transform(ngpn_poly |> filter(UNIT_CODE == "BADL"), crs = 26913) # far east is 14N, not sure if I need to accommodate that.
DETO_poly <- st_transform(ngpn_poly |> filter(UNIT_CODE == "DETO"), crs = 26913)
FOLA_poly <- st_transform(ngpn_poly |> filter(UNIT_CODE == "FOLA"), crs = 26913)
FOUS_poly <- st_transform(ngpn_poly |> filter(UNIT_CODE == "FOUS"), crs = 26913)
JECA_poly <- st_transform(ngpn_poly |> filter(UNIT_CODE == "JECA"), crs = 26913)
KNRI_poly <- st_transform(ngpn_poly |> filter(UNIT_CODE == "KNRI"), crs = 26914)
MORU_poly <- st_transform(ngpn_poly |> filter(UNIT_CODE == "MORU"), crs = 26913)
SCBL_poly <- st_transform(ngpn_poly |> filter(UNIT_CODE == "SCBL"), crs = 26913)
THRO_poly <- st_transform(ngpn_poly |> filter(UNIT_CODE == "THRO"), crs = 26913)
WICA_poly <- st_transform(ngpn_poly |> filter(UNIT_CODE == "WICA"), crs = 26913)

macro_plots_gps <- macro_plots |> select(MacroPlot_Name, MacroPlot_UTM_X, MacroPlot_UTM_Y) |>
  mutate(park = substr(MacroPlot_Name, 1, 4)) |>
  filter(!is.na(MacroPlot_UTM_X))

AGFO_pts <- st_as_sf(macro_plots_gps |> filter(park == "AGFO"), coords = c("MacroPlot_UTM_X", "MacroPlot_UTM_Y"), crs = 26913)
BADL_pts <- st_as_sf(macro_plots_gps |> filter(park == "BADL"), coords = c("MacroPlot_UTM_X", "MacroPlot_UTM_Y"), crs = 26913)
DETO_pts <- st_as_sf(macro_plots_gps |> filter(park == "DETO"), coords = c("MacroPlot_UTM_X", "MacroPlot_UTM_Y"), crs = 26913)
FOLA_pts <- st_as_sf(macro_plots_gps |> filter(park == "FOLA"), coords = c("MacroPlot_UTM_X", "MacroPlot_UTM_Y"), crs = 26913)
FOUS_pts <- st_as_sf(macro_plots_gps |> filter(park == "FOUS"), coords = c("MacroPlot_UTM_X", "MacroPlot_UTM_Y"), crs = 26913)
JECA_pts <- st_as_sf(macro_plots_gps |> filter(park == "JECA"), coords = c("MacroPlot_UTM_X", "MacroPlot_UTM_Y"), crs = 26913)
KNRI_pts <- st_as_sf(macro_plots_gps |> filter(park == "KNRI"), coords = c("MacroPlot_UTM_X", "MacroPlot_UTM_Y"), crs = 26914)
MORU_pts <- st_as_sf(macro_plots_gps |> filter(park == "MORU"), coords = c("MacroPlot_UTM_X", "MacroPlot_UTM_Y"), crs = 26913)
SCBL_pts <- st_as_sf(macro_plots_gps |> filter(park == "SCBL"), coords = c("MacroPlot_UTM_X", "MacroPlot_UTM_Y"), crs = 26913)
THRO_pts <- st_as_sf(macro_plots_gps |> filter(park == "THRO"), coords = c("MacroPlot_UTM_X", "MacroPlot_UTM_Y"), crs = 26913)
WICA_pts <- st_as_sf(macro_plots_gps |> filter(park == "WICA"), coords = c("MacroPlot_UTM_X", "MacroPlot_UTM_Y"), crs = 26913)

out_pts1 <- rbind(as.data.frame(AGFO_pts[!st_intersects(AGFO_pts, AGFO_poly, sparse = F),]),
                  as.data.frame(BADL_pts[!st_intersects(BADL_pts, BADL_poly, sparse = F),]),
                  as.data.frame(DETO_pts[!st_intersects(DETO_pts, DETO_poly, sparse = F),]),
                  as.data.frame(FOLA_pts[!st_intersects(FOLA_pts, FOLA_poly, sparse = F),]),
                  as.data.frame(FOUS_pts[!st_intersects(FOUS_pts, FOUS_poly, sparse = F),]),
                  as.data.frame(JECA_pts[!st_intersects(JECA_pts, JECA_poly, sparse = F),]),
                  as.data.frame(KNRI_pts[!st_intersects(KNRI_pts, KNRI_poly, sparse = F),]),
                  as.data.frame(MORU_pts[!st_intersects(MORU_pts, MORU_poly, sparse = F),]),
                  as.data.frame(SCBL_pts[!st_intersects(SCBL_pts, SCBL_poly, sparse = F),]),
                  as.data.frame(THRO_pts[!st_intersects(THRO_pts, THRO_poly, sparse = F),]),
                  as.data.frame(WICA_pts[!st_intersects(WICA_pts, WICA_poly, sparse = F),])
                 )

out_pts_utm <- cbind(MacroPlot_Name = out_pts1$MacroPlot_Name,
                     MacroPlot_UTM_X = as.numeric(st_coordinates(st_as_sf(out_pts1))[,1]),
                     MacroPlot_UTM_Y = as.numeric(st_coordinates(st_as_sf(out_pts1))[,2])
                     )

QC_table <- rbind(QC_table,
                  QC_check(df = out_pts_utm, meas_type = "MacroPlot", tab = "Plot Info",
                           check = "NGPN PCM MacroPlot UTM coordinates that are not within the park boundary.",
                           chk_type = 'error'))

kbl_out_pts_utm <- make_kable(out_pts_utm, cap = "NGPN PCM MacroPlot UTM coordinates that are not within the park boundary.")

# For plots with lat/long only, check if they're within the park bounds. This helps if we need to use the lat/longs
# to generate the UTMs. I'm not proud that I didn't iterate on this.
macro_plots_DD <- macro_plots |> select(MacroPlot_Name, MacroPlot_DD_Lat, MacroPlot_DD_Long) |>
  mutate(park = substr(MacroPlot_Name, 1, 4)) |>
  filter(!is.na(MacroPlot_DD_Lat))

AGFO_polydd <- ngpn_poly |> filter(UNIT_CODE == "AGFO")
BADL_polydd <- ngpn_poly |> filter(UNIT_CODE == "BADL")
DETO_polydd <- ngpn_poly |> filter(UNIT_CODE == "DETO")
FOLA_polydd <- ngpn_poly |> filter(UNIT_CODE == "FOLA")
FOUS_polydd <- ngpn_poly |> filter(UNIT_CODE == "FOUS")
JECA_polydd <- ngpn_poly |> filter(UNIT_CODE == "JECA")
KNRI_polydd <- ngpn_poly |> filter(UNIT_CODE == "KNRI")
MORU_polydd <- ngpn_poly |> filter(UNIT_CODE == "MORU")
SCBL_polydd <- ngpn_poly |> filter(UNIT_CODE == "SCBL")
THRO_polydd <- ngpn_poly |> filter(UNIT_CODE == "THRO")
WICA_polydd <- ngpn_poly |> filter(UNIT_CODE == "WICA")

AGFO_ptsdd <- st_as_sf(macro_plots_DD |> filter(park == "AGFO"), coords = c("MacroPlot_DD_Long", "MacroPlot_DD_Lat"), crs = 4269)
BADL_ptsdd <- st_as_sf(macro_plots_DD |> filter(park == "BADL"), coords = c("MacroPlot_DD_Long", "MacroPlot_DD_Lat"), crs = 4269)
DETO_ptsdd <- st_as_sf(macro_plots_DD |> filter(park == "DETO"), coords = c("MacroPlot_DD_Long", "MacroPlot_DD_Lat"), crs = 4269)
FOLA_ptsdd <- st_as_sf(macro_plots_DD |> filter(park == "FOLA"), coords = c("MacroPlot_DD_Long", "MacroPlot_DD_Lat"), crs = 4269)
FOUS_ptsdd <- st_as_sf(macro_plots_DD |> filter(park == "FOUS"), coords = c("MacroPlot_DD_Long", "MacroPlot_DD_Lat"), crs = 4269)
JECA_ptsdd <- st_as_sf(macro_plots_DD |> filter(park == "JECA"), coords = c("MacroPlot_DD_Long", "MacroPlot_DD_Lat"), crs = 4269)
KNRI_ptsdd <- st_as_sf(macro_plots_DD |> filter(park == "KNRI"), coords = c("MacroPlot_DD_Long", "MacroPlot_DD_Lat"), crs = 4269)
MORU_ptsdd <- st_as_sf(macro_plots_DD |> filter(park == "MORU"), coords = c("MacroPlot_DD_Long", "MacroPlot_DD_Lat"), crs = 4269)
SCBL_ptsdd <- st_as_sf(macro_plots_DD |> filter(park == "SCBL"), coords = c("MacroPlot_DD_Long", "MacroPlot_DD_Lat"), crs = 4269)
THRO_ptsdd <- st_as_sf(macro_plots_DD |> filter(park == "THRO"), coords = c("MacroPlot_DD_Long", "MacroPlot_DD_Lat"), crs = 4269)
WICA_ptsdd <- st_as_sf(macro_plots_DD |> filter(park == "WICA"), coords = c("MacroPlot_DD_Long", "MacroPlot_DD_Lat"), crs = 4269)

out_pts1dd <- rbind(if(nrow(AGFO_ptsdd) > 0){as.data.frame(AGFO_ptsdd[!st_intersects(AGFO_ptsdd, AGFO_polydd, sparse = F),])},
                    if(nrow(BADL_ptsdd) > 0){as.data.frame(BADL_ptsdd[!st_intersects(BADL_ptsdd, BADL_polydd, sparse = F),])},
                    if(nrow(DETO_ptsdd) > 0){as.data.frame(DETO_ptsdd[!st_intersects(DETO_ptsdd, DETO_polydd, sparse = F),])},
                    if(nrow(FOLA_ptsdd) > 0){as.data.frame(FOLA_ptsdd[!st_intersects(FOLA_ptsdd, FOLA_polydd, sparse = F),])},
                    if(nrow(FOUS_ptsdd) > 0){as.data.frame(FOUS_ptsdd[!st_intersects(FOUS_ptsdd, FOUS_polydd, sparse = F),])},
                    if(nrow(JECA_ptsdd) > 0){as.data.frame(JECA_ptsdd[!st_intersects(JECA_ptsdd, JECA_polydd, sparse = F),])},
                    if(nrow(KNRI_ptsdd) > 0){as.data.frame(KNRI_ptsdd[!st_intersects(KNRI_ptsdd, KNRI_polydd, sparse = F),])},
                    if(nrow(MORU_ptsdd) > 0){as.data.frame(MORU_ptsdd[!st_intersects(MORU_ptsdd, MORU_polydd, sparse = F),])},
                    if(nrow(SCBL_ptsdd) > 0){as.data.frame(SCBL_ptsdd[!st_intersects(SCBL_ptsdd, SCBL_polydd, sparse = F),])},
                    if(nrow(THRO_ptsdd) > 0){as.data.frame(THRO_ptsdd[!st_intersects(THRO_ptsdd, THRO_polydd, sparse = F),])},
                    if(nrow(WICA_ptsdd) > 0){as.data.frame(WICA_ptsdd[!st_intersects(WICA_ptsdd, WICA_polydd, sparse = F),])}
)

out_pts_dd <- cbind(MacroPlot_Name = out_pts1dd$MacroPlot_Name,
                    MacroPlot_DD_Long = as.numeric(st_coordinates(st_as_sf(out_pts1dd))[,1]),
                    MacroPlot_DD_Lat = as.numeric(st_coordinates(st_as_sf(out_pts1dd))[,2])
)

QC_table <- rbind(QC_table,
                  QC_check(df = out_pts_dd, meas_type = "MacroPlot", tab = "Plot Info",
                           check = "NGPN PCM MacroPlot DD coordinates that are not within the park boundary for plots missing UTM X,Y.",
                           chk_type = 'error'))

kbl_out_pts_dd <- make_kable(out_pts_dd, cap = "NGPN PCM MacroPlot DD coordinates that are not within the park boundary for plots missing UTM X,Y.")

# THROS_PCM_0069 Macroplot_SlopeHill and _SlopeTransect has some 9999

# check if Macroplot-Plot Info checks returned at least 1 record to determine whether to include that tab in report
macro_pi_check <- QC_table |> filter(Type %in% "MacroPlot" & Data %in% "Plot Info" & Num_Records > 0)
macro_pi_include <- tab_include(macro_pi_check)

# check if MacroPlot checks returned at least 1 record to determine whether to include that tab in report
macro_check <- QC_table |> filter(Type %in% "MacroPlot" & Num_Records > 0)
macro_include <- tab_include(macro_check)

### Sample Event Checks {.tabset}
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
  select(MacroPlot_Name, MacroPlot_Purpose, ProjectUnit_Name, ProjectUnit_Agency)

QC_table <- rbind(QC_table,
                  QC_check(df = miss_samp, meas_type = "SampleEvent", tab = "General",
                           check = "NGPN PCM MacroPlots no accompanying SampleEvents.",
                           chk_type = 'error'))

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
  format(as.Date(mac_samp_monstat$SampleEvent_Date, format = "%Y-%m-%d %H:%m:%s"),
         "%Y-%m-%d")
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
            QC_check(df = monstat_yr_mismatch, meas_type = "SampleEvent", tab = "Monitoring Status",
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
             QC_check(df = monstat_incon2, meas_type = "SampleEvent", tab = "Monitoring Status",
                     check = "NGPN PCM plots with inconsistently labeled MonitoringStatus_Name.",
                     chk_type = 'error')
)

kbl_monstat_incon <- kable(monstat_incon2, format = "html", align = 'c',
                           caption = "NGPN PCM plots with inconsistently labeled MonitoringStatus_Name.
                           Plots may be used for different monitoring purposes, but also seems some are incorrect.
                           The years in the cell are years that a given monitoring status was recored (eg 2013 in
                           PlantCommunity means there's a monitoring status name for that plot called '2013_PlantCommunity').
                           Note that the first Plant Community column has a space between the words.") |>
  kable_styling(fixed_thead = T, bootstrap_options = c("condensed", "striped"),
                full_width = T, position = 'left', font_size = 10) |>
  column_spec(1:ncol(monstat_incon2), border_left = "1px solid grey", border_right = "1px solid grey")

# check if Sample Event - Monitoring Status checks returned at least 1 record to determine whether to include that tab in report
sampev_ms_check <- QC_table |> filter(Type %in% "SampleEvent" & Data %in% "Monitoring Status" & Num_Records > 0)
sampev_ms_include <- tab_include(sampev_ms_check)

# check if Sample Event checks returned at least 1 record to determine whether to include that tab in report
sampev_check <- QC_table |> filter(Type %in% "SampleEvent" & Num_Records > 0)
sampev_include <- tab_include(sampev_check)

###### Compile final QC Table ######
# revise for different color combos for checks (99 vs 90)? Drop for checks vs. errors?
QC_check_table <- kable(QC_table, format = 'html', align = 'c', caption = "QC checking results",
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
# ++++ OTHER CHECKS ++++
# Check the different module outputs in the package against the WICA data Dan sent.

# Check covpts_attr$Index has a bunch of blanks in it.

# the join to cover point attributes to cover point sample then sample events, causes many-to-many join in the sample events table. Need to figure out why.

# JECA_PCM_134 in 2016; 2016-07-05; 2016-09-13; 2016_FirePlantCommunity; 2016_ForestStructure; 01yr02; 2016_ForestStructure; Based on other results from 2016 in JECA, 2016_ForestStructure appears to be the VS sample.
# The FFI Data Depot returns 2 sets of CoverPoint data, and they're identical.

# JECA_PCM_038 in 2016; 2016-07-05; 2016-09-13; 2016_FirePlantCommunity; 2016_ForestStructure; 01yr02; 2016_ForestStructure;
# Also looks like sometimes MonitoringStatus_Name isn't consistent between ForestStructure or PlantCommunity across years, but usually within for the same plots. Then there's FirePlantCommunity, which appears to be different.



