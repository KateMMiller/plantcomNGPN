# ---- Params for troubleshooting ----
# library(plantcomNGPN)
# library(tidyverse) # dplyr, purrr, tidyr
# library(knitr) # for kable and include_graphic()
# library(kableExtra) # for custom kable features
# library(sf)
# library(data.table)
# library(DT)
#
# importData(type = 'local',
#            dbname = c("FFI_RA_AGFO", "FFI_RA_BADL", "FFI_RA_DETO", "FFI_RA_FOLA",
#                       "FFI_RA_FOUS", "FFI_RA_JECA", "FFI_RA_KNRI", #"FFI_RA_MNRR",
#                       "FFI_RA_MORU", "FFI_RA_SCBL", "FFI_RA_THRO", "FFI_RA_WICA"),
#            keep_tables = T)
#
# importData(type = 'local',
#            dbname = c("FFI_RA_AGFO", "FFI_RA_BADL", "FFI_RA_DETO", "FFI_RA_FOLA",
#                       "FFI_RA_FOUS", "FFI_RA_JECA", "FFI_RA_KNRI", #"FFI_RA_MNRR",
#                       "FFI_RA_MORU", "FFI_RA_SCBL", "FFI_RA_THRO", "FFI_RA_WICA"),
#            keep_tables = T, export_tables = TRUE, export_views = TRUE,
#            export_path = "./docs/data")#
# all_years <- TRUE
# year_curr <- 2024
# year_range <- if(all_years == TRUE){2011:year_curr} else {year_curr}
# year_hist <- 2011:(year_curr - 1)

# Start of source code ----
## Functions ----
# Summarize results of QC check
QC_check <- function(df, meas_type, tab, check, chk_type = "error"){
  result <- data.frame("Type" = meas_type,
                       "Data" = tab,
                       "Description" = check,
                       "Num_Records" = nrow(df),
                       "check_type" = chk_type)
}

# function to make tables via kable
make_kable <- function(df, cap){
  QC_table <- if(nrow(df) > 0){
    if(nrow(df) > 1){
      kable(df,
            format = 'html',
            align = 'c',
            caption = cap)  |>
        kable_styling(fixed_thead = TRUE,
                      bootstrap_options = c('condensed'),
                      full_width = TRUE,
                      position = 'left',
                      font_size = 12) |>
        row_spec(0,
                 extra_css = "border-top: 1px solid #000000; border-bottom: 1px solid #000000;") |>
        collapse_rows(1,
                      valign = 'top') |>
        row_spec(nrow(df),
                 extra_css = 'border-bottom: 1px solid #000000;')
    } else if(nrow(df) == 1){
      kable(df,
            format = 'html',
            align = 'c',
            caption = cap)  |>
        kable_styling(fixed_thead = TRUE,
                      bootstrap_options = c('condensed'),
                      full_width = TRUE,
                      position = 'left',
                      font_size = 12) |>
        row_spec(0,
                 extra_css = "border-top: 1px solid #000000; border-bottom: 1px solid #000000;") |>
        row_spec(nrow(df),
                 extra_css = 'border-bottom: 1px solid #000000;')
    }
  } else NULL
}

make_dt <- function(df, cap){
  datatable(df,
            class = 'cell-border stripe',
            rownames = FALSE,
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
              autoWidth = FALSE,
              scrollX = '850px',
              scrollY = '600px',
              scrollCollapse = TRUE,
              fixedColumns = list(leftColumns = 1),
              dom = "Blfrtip",
              buttons = c('copy', 'csv', 'print')
            ),
            filter = list(position = c('top'),
                          clear = FALSE))
}

# Determine whether to include/drop tab in rmd output
tab_include <- function(df){ifelse(nrow(df) > 0, TRUE, FALSE)}

# Determine if table exists or is null used in eval for rmd
check_null <- function(table){
  if(!is.null(table)){table}
}

check_null_print <- function(table, tab_level = 4, tab_title){
  if(!is.null(table)){cat(paste0(rep("#", tab_level),
                                 collapse = ""),
                          " ",
                          tab_title,
                          " {.tabset} ", "\n\n")}
  check_null(table)
}

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

## Macroplot Checks ----
macro_plots_all <- getMacroPlot(purpose = "NGPN_PCM") |>
  mutate(park = substr(MacroPlot_Name, 1, 4))

macro_plots <- bind_rows(macro_plots_all |>
                           filter(!park == "THRO") |>
                           semi_join(panel_sch,
                                     by = c("MacroPlot_Purpose" = "Panel")),
                         macro_plots_all |>
                           filter(park == "THRO") |>
                           semi_join(panel_sch_thro,
                                     by = c("MacroPlot_Purpose" = "Panel"))) |>
  distinct()

park_list <- sort(unique(macro_plots$park))

### Coordinates Checks ----
# names(macro_plots)
names(macro_plots)

### Missing Coordinates ----
macro_miss_utm <- macro_plots |>
  filter(is.na(UTM_X) | is.na(UTM_Y) | is.na(UTMzone)) |>
  select(MacroPlot_Name,
         UTM_X,
         UTM_Y,
         UTMzone,
         DD_Lat,
         DD_Long) |>
  arrange(MacroPlot_Name)

# Adding to initial table
QC_table <- QC_check(df = macro_miss_utm, meas_type = "MacroPlot", tab = "Plot Info",
                     check = "NGPN PCM plots missing UTM X, Y, and/or UTM Zone data.",
                     chk_type = 'error')

# Individual table
kbl_macro_miss_utm <- make_kable(macro_miss_utm,
                                 "NGPN PCM plots missing UTM X, Y, and/or Zone data. ") |>
  column_spec(2, background = ifelse(is.na(macro_miss_utm$UTM_X),
                                     "#F2F2A0", "white")) |>
  column_spec(3, background = ifelse(is.na(macro_miss_utm$UTM_Y),
                                     "#F2F2A0", "white")) |>
  column_spec(4, background = ifelse(is.na(macro_miss_utm$UTMzone),
                                     "#F2F2A0", "white"))

### Plots outside NPS Bounds ----
# Set bounding box for each park and check UTMs and/or lat/long against them:
# First downloaded NPS Administrative Boundaries from:
# https://irma.nps.gov/DataStore/Reference/Profile/2309935
tryCatch(nps_bounds <- read_sf("C:/Users/kbailey/Documents/Development/plantcomNGPN/docs/www/Administrative Boundaries of National Park System Units.shp"),
         error = function(e){})

st_crs(nps_bounds) # EPSG 4269

# lookup for each park
crs_lookup <- tibble::tribble(~UNIT_CODE, ~crs,
                              "AGFO", 26913,
                              "BADL", 26913,
                              "DETO", 26913,
                              "FOLA", 26913,
                              "FOUS", 26913,
                              "JECA", 26913,
                              "KNRI", 26914,
                              "MORU", 26913,
                              "SCBL", 26913,
                              "THRO", 26913,
                              "WICA", 26913)

# Creating polygons for NGPN
ngpn_poly <- nps_bounds |>
  dplyr::filter(UNIT_CODE %in% park_list) |>
  left_join(crs_lookup,
            by ="UNIT_CODE")

# splitting into parks
poly_list <- ngpn_poly |>
  split(ngpn_poly$UNIT_CODE) |>
  map(~ st_as_sf(.x))

# Getting Plot locations
macro_plots_gps <- macro_plots |>
  select(Unit_Name,
         MacroPlot_Name,
         UTM_X, UTM_Y) |>
  rename(UNIT_CODE = Unit_Name) |>
  filter(!is.na(UTM_X)) |>
  left_join(crs_lookup, by = "UNIT_CODE")

# convert each park to sf object
pts_list <- macro_plots_gps |>
  split(macro_plots_gps$UNIT_CODE) |>
  map(~ st_as_sf(.x, coords = c("UTM_X", "UTM_Y"), crs = .x$crs[1]))

# Identify points outside polygons
out_pts_list <- map(names(pts_list), function(p) {
  pts <- pts_list[[p]]
  poly <- poly_list[[p]]

  # Matching CRS
  pts <- st_transform(pts, st_crs(poly))

  # getting points that don't intersect polys
  pts[!st_intersects(pts, poly, sparse = FALSE), ]
})

out_pts <- bind_rows(out_pts_list)

out_pts_utm <- out_pts |>
  mutate(UTM_X = st_coordinates(geometry)[,1],
         UTM_Y = st_coordinates(geometry)[,2]) |>
  st_drop_geometry() |>
  select(MacroPlot_Name, UTM_X, UTM_Y) |>
  arrange(MacroPlot_Name)

# Adding values to QC_table
QC_table <- rbind(QC_table,
                  QC_check(df = out_pts_utm,
                           meas_type = "MacroPlot",
                           tab = "Plot Info",
                           check = "NGPN PCM MacroPlot UTM coordinates that are not within the park boundary.",
                           chk_type = 'error'))

kbl_out_pts_utm <- make_kable(out_pts_utm,
                              cap = "NGPN PCM MacroPlot UTM coordinates that are not within the park boundary.")

### NPGN plots with non-standard datum ----
datum <- macro_plots |>
  select(MacroPlot_Name, UTM_X, UTM_Y,
         UTMzone, Datum) |>
  filter(!Datum %in% "NAD83") |>
  arrange(MacroPlot_Name)

QC_table <- rbind(QC_table,
                  QC_check(df = datum,
                           meas_type = "MacroPlot",
                           tab = "Plot Info",
                           check = "NGPN PCM plot coordinates with datum not matching 'NAD83'.",
                           chk_type = 'error'))

kbl_datum <- make_kable(datum,
                        cap = "NGPN PCM plot coordinates with datum not matching 'NAD83'")

# For plots with lat/long only, check if they're within the park bounds.
# This helps if we need to use the lat/longs


# Getting Plot locations
macro_plots_DD <- macro_plots |>
  select(Unit_Name,
         MacroPlot_Name,
         DD_Lat,
         DD_Long) |>
  rename(UNIT_CODE = Unit_Name) |>
  filter(!is.na(DD_Lat))

# convert each park to sf object
ptsdd_list <- macro_plots_DD |>
  split(macro_plots_DD$UNIT_CODE) |>
  map(~ st_as_sf(.x, coords = c("DD_Long", "DD_Lat"), crs = 4269))

# Identify points outside polygons
out_ptsdd_list <- map(names(ptsdd_list), function(p) {
  pts <- pts_list[[p]]
  poly <- poly_list[[p]]

  # Matching CRS
  pts <- st_transform(pts, st_crs(poly))

  # getting points that don't intersect polys
  pts[!st_intersects(pts, poly, sparse = FALSE), ]
})

out_ptsdd <- bind_rows(out_ptsdd_list)

out_pts_dd <- out_ptsdd |>
  mutate(DD_Long = st_coordinates(geometry)[,1],
         DD_Lat = st_coordinates(geometry)[,2]) |>
  st_drop_geometry() |>
  select(MacroPlot_Name,
         DD_Long,
         DD_Lat) |>
  arrange(MacroPlot_Name)

QC_table <- rbind(QC_table,
                  QC_check(df = out_pts_dd,
                           meas_type = "MacroPlot",
                           tab = "Plot Info",
                           check = "NGPN PCM MacroPlot DD coordinates that are not within the park boundary for plots missing UTM X,Y.",
                           chk_type = 'error'))

kbl_out_pts_dd <- make_kable(out_pts_dd,
                             cap = "NGPN PCM MacroPlot DD coordinates that are not within the park boundary for plots missing UTM X,Y.")

### Check for blank macro data ----
macro_blanks <- macro_plots |>
  select(MacroPlot_Name,
         Elevation,
         ElevationUnits,
         Azimuth,
         Aspect,
         SlopeHill) |>
  filter(is.na(Elevation) | is.na(ElevationUnits) | is.na(Azimuth) |
           is.na(Aspect) | is.na(SlopeHill)) |>
  arrange(MacroPlot_Name)

QC_table <- rbind(QC_table,
                  QC_check(df = macro_blanks,
                           meas_type = "MacroPlot",
                           tab = "Blank Loc. Values",
                           check = "Macroplots missing location information",
                           chk_type = 'error'))

kbl_macro_blanks <- make_kable(macro_blanks,
                               cap = "Macroplots missing location information")

### Check for impossible macro data ----
macro_imp <- macro_plots |>
  select(MacroPlot_Name,
         Elevation,
         ElevationUnits,
         Azimuth,
         Aspect,
         SlopeHill,
         SlopeTransect) |>  # slopes should be <= 100%
  filter(SlopeTransect != 9999,
         Azimuth > 360 | Azimuth < 0 | Aspect > 360 | Aspect < 0 |
         SlopeHill > 100 | SlopeTransect > 100) |>
  arrange(MacroPlot_Name)

QC_table <- rbind(QC_table,
                  QC_check(df = macro_imp,
                           meas_type = "MacroPlot",
                           tab = "Impossible Values",
                           check = "Macroplot location data with impossible values",
                           chk_type = 'error'))

kbl_macro_imp <- make_kable(macro_imp,
                            cap = "Macroplot location data with impossible values")

### Check on UV values ----
# UV1 = Topographic position; UV2 = Surface water; UV3 = Hydrologic Regime; UV4 = Vegetation Type
macro_uv <- macro_plots |>
  select(MacroPlot_Name,
         MacroPlot_UV1,
         MacroPlot_UV2,
         MacroPlot_UV3,
         MacroPlot_UV4)

#### UV 1 ----
# check topo positions that aren't CR, DR, LV, LS, MS, RO, SB, US
macro_topo <- macro_uv |>
  filter(!MacroPlot_UV1 %in%
         c('CR', 'DR', 'LV', 'LS', 'MS', 'RO', 'SB', 'US')) |>
  select(MacroPlot_Name,
         MacroPlot_UV1) |>
  arrange(MacroPlot_Name)

QC_table <- rbind(QC_table,
                  QC_check(df = macro_topo,
                           meas_type = "MacroPlot",
                           tab = "UV1 Topo Positions",
                           check = "Macroplot topographic positions (UV1) that don't identically match c('CR', 'DR', 'LV', 'LS', 'MS', 'RO', 'SB', 'US')",
                           chk_type = "check"))

kbl_macro_topo <- make_kable(macro_topo,
                             cap = "Macroplot topographic positions (UV1) that don't identically match c('CR', 'DR', 'LV', 'LS', 'MS', 'RO', 'SB', 'US').")

#### UV 2 ----
# UV2 = Surface water; check Surface water that aren't <50m or >50m or in plot
# macro_surf <- macro_uv |>
#   filter(!MacroPlot_UV2 %in% c("<50m", ">50m", "in plot")) |>
#   select(MacroPlot_Name,
#          MacroPlot_UV2)
#
# QC_table <- rbind(QC_table,
#                   QC_check(df = macro_surf, meas_type = "MacroPlot", tab = "UV2 Surface Water",
#                            check = "Macroplot surface water (UV2) that doesn't identically match '<50m', '>50m', or 'in plot'",
#                            chk_type = "error"))
#
# kbl_macro_surf <- make_kable(macro_surf, cap = "Macroplot surface water (UV2) that doesn't identically match '<50m', '>50m', or 'in plot'")

#### UV 3 ----
# UV3 = Hydrologic Region
# macro_hydro <- macro_uv |>
#   filter(!MacroPlot_UV3 %in% c("IF", "PF", "SF", "SP", "TF", "UP")) |>
#   select(MacroPlot_Name,
#          MacroPlot_UV3)
#
# QC_table <- rbind(QC_table,
#                   QC_check(df = macro_hydro, meas_type = "MacroPlot", tab = "UV3 Hydro Regime",
#                            check = "Macroplot hydrologic regime (UV3) that doesn't identically match c('IF', 'PF', 'SF', 'SP', 'TF', 'UP')",
#                            chk_type = "error"))
#
# kbl_macro_hydro <- make_kable(macro_hydro, cap = "Macroplot hydrologic regime (UV3) that doesn't identically match c('IF', 'PF', 'SF', 'SP', 'TF', 'UP')")

#### UV 4 ----
# UV4 = Veg type; check vegetation type
macro_veg <- macro_uv |>
  filter(!MacroPlot_UV4 %in% c("BS", "HR", "PP", "RW", "SH", "UG", "WD")) |>
  select(MacroPlot_Name,
         MacroPlot_UV4) |>
  arrange(MacroPlot_Name)

QC_table <- rbind(QC_table,
                  QC_check(df = macro_veg,
                           meas_type = "MacroPlot",
                           tab = "UV4 Veg. Type",
                           check = "Macroplot Vegetation Type (UV4) that doesn't identically match c('BS', 'HR', 'PP', 'RW', 'SH', 'UG', 'WD')",
                           chk_type = "error"))

kbl_macro_veg <- make_kable(macro_veg,
                            cap = "Macroplot Vegetation Type (UV4) that doesn't identically match c('BS', 'HR', 'PP', 'RW', 'SH', 'UG', 'WD')")


# check if MacroPlot checks returned at least 1 record to determine whether to include that tab in report
macro_check <- QC_table |> filter(Type %in% "MacroPlot" & Num_Records > 0)
macro_include <- tab_include(macro_check)

## Sample Event -----
mac_samp_all <- getSampleEvent(years = year_range,
                               purpose = "NGPN_PCM") |>
  mutate(park = substr(MacroPlot_Name, 1, 4),
         year = as.numeric(year))

mac_samp <- bind_rows(mac_samp_all |>
                        filter(!park == "THRO") |>
                        semi_join(panel_sch,
                                  by = c("MacroPlot_Purpose" = "Panel",
                                         "year" = "Year")),
                      mac_samp_all |>
                        filter(park == "THRO") |>
                        semi_join(panel_sch_thro,
                                  by = c("MacroPlot_Purpose" = "Panel",
                                         "year" = "Year")))

### Check for sample events with no data ----
miss_samp <- mac_samp |> filter(is.na(SampleEvent_GUID)) |>
  arrange(MacroPlot_Name,
          SampleEvent_Date)

QC_table <- rbind(QC_table,
                  QC_check(df = miss_samp,
                           meas_type = "SampleEvent",
                           tab = "No SampleEvents",
                           check = "NGPN PCM MacroPlots with no accompanying SampleEvents.",
                           chk_type = 'error'))

kbl_miss_samp <- make_kable(miss_samp,
                            cap = "NGPN PCM MacroPlots with no accompanying SampleEvents.")

### Check for year in MonitoringStatus_Name that differs from sample year ----
# mac_samp$monstat_year <- as.numeric(substr(mac_samp$MonitoringStatus_Name, 1, 4))
# mac_samp$year_match <- ifelse(mac_samp$year == mac_samp$monstat_year, 1, 0)
#
# mac_samp2 <- mac_samp |> filter(year_match == 0) |>
#   select(MacroPlot_Name, SampleEvent_Date, year, MonitoringStatus_Name)
#
# QC_table <- QC_check(df = mac_samp2, meas_type = "SampleEvent", tab = "MonStat Year Mismatch",
#                      check = "NGPN PCM plots with mismatch in year of SampleEvent_Date, and MonitoringStatus_Name.",
#                      chk_type = 'error')
#
# kbl_mac_samp2 <- make_kable(mac_samp2, cap = "NGPN PCM plots with mismatch in year of SampleEvent_Date, and MonitoringStatus_Name.")

### Check if Sample Event checks returned at least 1 record to determine whether to include tab----
samp_check <- QC_table |>
  filter(Type %in% "SampleEvent" & Num_Records > 0)

samp_include <- tab_include(samp_check)

## Taxa ----
# Does not remove "only active" plots

### Missing Values ----
# Check for species with inconsistent scientific names (eg genus only, genus spp.) but the same symbol
taxa <- unique(VIEWS_NGPN$Taxa_Table[,c("Symbol", "ITIS_TSN", "ScientificName",
                                        "Unit_Name", "NotBiological")])

taxa_wide <- taxa |>
  mutate(present = 1) |>
  arrange(Unit_Name) |>
  pivot_wider(names_from = Unit_Name,
              values_from = present,
              values_fill = 0)

#### Blanks in ScientificName ----
taxa_miss_sci <- taxa_wide |>
  filter(is.na(ScientificName)) |>
  arrange(Symbol)

QC_table <- rbind(QC_table,
                  QC_check(df = taxa_miss_sci,
                           meas_type = "Taxa",
                           tab = "Blank SciName",
                           check = "Records that have a blank ScientificName column.",
                           chk_type = 'check'))

dt_taxa_miss_sci <- make_dt(df = taxa_miss_sci,
                            cap = "Records that have a blank ScientificName column")

#### Blank symbols ----
taxa_miss_sym <- taxa_wide |>
  filter(is.na(Symbol)) |>
  arrange(Symbol)

QC_table <- rbind(QC_table,
                  QC_check(df = taxa_miss_sym,
                           meas_type = "Taxa",
                           tab = "Blank Symbol",
                           check = "Records that have a blank Symbol column.",
                           chk_type = 'check'))

kbl_taxa_miss_sym <- make_kable(df = taxa_miss_sym,
                                cap = "Records that have a blank Symbol column")

#### Blank TSN ----
taxa_miss_tsn <- taxa_wide |>
  filter(is.na(ITIS_TSN)) |>
  arrange(Symbol)

QC_table <- rbind(QC_table,
                  QC_check(df = taxa_miss_tsn,
                           meas_type = "Taxa",
                           tab = "Blank ITIS_TSN",
                           check = "Records that have a blank TSN column.",
                           chk_type = 'check'))

dt_taxa_miss_tsn <- make_dt(df = taxa_miss_tsn,
                            cap = "Records that have a blank TSN column")

#### NotBiological blank ----
taxa_miss_nb <- taxa_wide |>
  filter(is.na(NotBiological)) |>
  arrange(Symbol) |>
  filter(!is.na(Symbol)) # drops record that's all NAs across species columns and picked up in other checks

QC_table <- rbind(QC_table,
                  QC_check(df = taxa_miss_nb,
                           meas_type = "Taxa",
                           tab = "Blank NotBio",
                           check = "Records that have a blank NotBiological column.",
                           chk_type = 'check'))

kbl_taxa_miss_nb <- make_kable(df = taxa_miss_nb,
                               cap = "Records that have a blank NotBiological column")

### Inconsistent names ----

#### Different scientific names, common names or TSNs ----
taxa2 <- unique(VIEWS_NGPN$Taxa_Table[,c("Symbol", "ITIS_TSN",
                                         "ScientificName", "CommonName",
                                         "Unit_Name", "NotBiological")])

taxa_dups <- taxa2 |>
  mutate(present = 1) |>
  arrange(Unit_Name) |>
  pivot_wider(names_from = Unit_Name,
              values_from = present,
              values_fill = 0) |>
  group_by(Symbol) |>
  mutate(num_tsn = sum(!is.na(ITIS_TSN)),
         num_sci = sum(!is.na(ScientificName)),
         num_com = sum(!is.na(CommonName)),
         num_dup = num_tsn + num_sci + num_com) |>
  filter(num_dup > 3) |>
  arrange(Symbol,
          ScientificName) |>
  data.frame()

taxa_dup_notbio <- taxa_dups |>
  filter(NotBiological == TRUE) |>
  select(Symbol:CommonName, AGFO:WICA) |>
  arrange(Symbol)

QC_table <- rbind(QC_table,
                  QC_check(df = taxa_dup_notbio,
                           meas_type = "Taxa",
                           tab = "Inconsistent NotBio",
                           check = "Symbols with inconsistent TSN, ScientificName, or CommonName values across parks where NotBiological = T",
                           chk_type = 'check'))

dt_taxa_dup_notbio <- make_dt(taxa_dup_notbio,
                              cap = "Symbols with inconsistent TSN, ScientificName, or CommonName values across parks where NotBiological = T")

# kbl_taxa_dup_notbio <-
#   kable(taxa_dup_notbio, format = 'html', align = 'c',
#         caption = "Symbols with inconsistent TSN, ScientificName, or CommonName values across parks where NotBiological = T")  |>
#   kable_styling(fixed_thead = TRUE, bootstrap_options = c('condensed'),
#                 full_width = TRUE, position = 'left', font_size = 12) |>
#   row_spec(0, extra_css = "border-top: 1px solid #000000; border-bottom: 1px solid #000000;") |>
#   row_spec(nrow(taxa_dup_notbio), extra_css = 'border-bottom: 1px solid #000000;') |>
#   collapse_rows(1:4, valign = 'top')

#### Inconsistent Species ----
taxa_dup_bio <- taxa_dups |>
  filter(NotBiological == FALSE) |>
  select(Symbol:CommonName, AGFO:WICA) |>
  arrange(Symbol)

QC_table <- rbind(QC_table,
                  QC_check(df = taxa_dup_bio,
                           meas_type = "Taxa",
                           tab = "Inconsistent Species",
                           check = "Symbols with inconsistent TSN, ScientificName, or CommonName values across parks where NotBiological = F",
                           chk_type = 'check'))

dt_taxa_dup_bio <- make_dt(taxa_dup_bio, cap = "Symbols with inconsistent TSN, ScientificName, or CommonName values across parks where NotBiological = F")

# kbl_taxa_dup_bio <-
#   kable(taxa_dup_bio, format = 'html', align = 'c',
#         caption = "Symbols with inconsistent TSN, ScientificName, or CommonName values across parks where NotBiological = F")  |>
#   kable_styling(fixed_thead = TRUE, bootstrap_options = c('condensed'),
#                 full_width = TRUE, position = 'left', font_size = 12) |>
#   row_spec(0, extra_css = "border-top: 1px solid #000000; border-bottom: 1px solid #000000;") |>
#   row_spec(nrow(taxa_dup_bio), extra_css = 'border-bottom: 1px solid #000000;') |>
#   collapse_rows(1:4, valign = 'top')

# check if Taxa - missing checks returned at least 1 record to determine whether to include tab
taxa_check <- QC_table |>
  filter(Type %in% "Taxa" & Num_Records > 0)

taxa_include <- tab_include(taxa_check)

## Cover Point Data ----
point_int1 <- getCoverPoints(years = year_range,
                             purpose = "NGPN_PCM") |>
  mutate(park = substr(MacroPlot_Name, 1, 4),
         year = as.numeric(year))

point_int <- bind_rows(point_int1 |>
                        filter(!park == "THRO") |>
                        semi_join(panel_sch,
                                  by = c("MacroPlot_Purpose" = "Panel",
                                         "year" = "Year")),
                      point_int1 |>
                        filter(park == "THRO") |>
                        semi_join(panel_sch_thro,
                                  by = c("MacroPlot_Purpose" = "Panel",
                                         "year" = "Year"))) |>
  select(MacroPlot_Name,
         Unit_Name,
         SampleEvent_Date,
         year,
         month,
         TranLen,
         NumPtsTran,
         Transect,
         Point,
         Tape,
         Order,
         Height,
         ScientificName,
         Status,
         NotBiological) #|> unique()

#### Ground Hits ----
num_ground <- point_int |>
  filter(Order == 0) |>
  # group_by(MacroPlot_Name,
  #          Unit_Name,
  #          SampleEvent_Date,
  #          year,
  #          NumPtsTran,
  #          Transect) |>
  distinct() |>
  summarize(num_ground = n(),
            .by = c(MacroPlot_Name,
                    Unit_Name,
                    SampleEvent_Date,
                    year,
                    NumPtsTran,
                    Transect)) |>
  filter(NumPtsTran != num_ground)|>
  select(MacroPlot_Name,
         SampleEvent_Date,
         year,
         Transect,
         NumPtsTran,
         num_ground) |>
  arrange(MacroPlot_Name,
          SampleEvent_Date,
          Transect)

QC_table <- rbind(QC_table,
                  QC_check(df = num_ground,
                           tab = "Incorrect Ground",
                           meas_type = "Point Intercept",
                           check = "Transects where number of ground hits doesn't match number of points sampled.",
                           chk_type = "error"))

dt_num_ground <- make_dt(num_ground,
                         cap = "Transects where number of ground hits doesn't match number of points sampled.")

#### Order > 0 have heights ----
ht_check <- point_int |>
  distinct() |>
  summarize(num_orders = n(),
            Height = sum(Height, na.rm = T), # sum much faster than max
            .by = c(MacroPlot_Name,
                    Unit_Name,
                    SampleEvent_Date,
                    year,
                    Transect,
                    Point,
                    Tape)) |>
  filter(num_orders > 1 & Height == 0) |>
  arrange(MacroPlot_Name,
          SampleEvent_Date)

ht_check$Height[ht_check$Height == 0] <- NA_real_ # Adding NA back in due to sum

QC_table <- rbind(QC_table,
                  QC_check(df = ht_check,
                           tab = "Missing Height",
                           meas_type = "Point Intercept",
                           check = "Points with more than 1 order missing a height for the top hit.",
                           chk_type = "error"))

dt_ht_check <- make_dt(ht_check,
                       cap = "Points with more than 1 order missing a height for the top hit.")

# Check that heights are only recorded for Hit = 1 (top hit)
# hit1_ht <- point_int |>
#   mutate(hit1 = ifelse(Order == 1, T, F),
#          ht = ifelse(!is.na(Height), T, F),
#          hitblank = ifelse(hit1 == F & ht == T, T, F)) |>
#   filter(hitblank == T)
#
# QC_table <- rbind(QC_table,
#                   QC_check(df = hit1_ht, tab = "Heights on Order != 1", meas_type = "Point Intercept",
#                            check = "Heights recorded for Orders not equal to 1, the top hit.",
#                            chk_type = "error"))
#
# kbl_hit1_ht <- make_kable(hit1_ht, cap = "Heights recorded for Orders not equal to 1, the top hit.")
# returns >8k of records.


### Duplicate orders within a transect ----
dup_order <- point_int |>
  distinct() |>
  summarize(num_hits = sum(!is.na(ScientificName)),
            .by = c(MacroPlot_Name,
                    Unit_Name,
                    SampleEvent_Date,
                    year,
                    Transect,
                    Point,
                    Tape,
                    Order)) |>
  filter(num_hits > 1) |>
  arrange(MacroPlot_Name,
          SampleEvent_Date)

QC_table <- rbind(QC_table,
                  QC_check(df = dup_order,
                           tab = "Duplicate Order",
                           meas_type = "Point Intercept",
                           check = "Points with duplicate orders on the same transect.",
                           chk_type = "error"))

kbl_dup_order <- make_kable(dup_order,
                            cap = "Points with Order = 1 and blank Height value.")

### Check for heights > 2m ----
ht_oor <- point_int |>
  filter(Height > 2.0) |>
  select(MacroPlot_Name,
         SampleEvent_Date,
         year,
         month,
         Transect,
         Point,
         Tape,
         Order,
         Height,
         ScientificName) |>
  distinct()

QC_table <- rbind(QC_table,
                  QC_check(df = ht_oor,
                           tab = "Height over 2m",
                           meas_type = "Point Intercept",
                           check = "Points with a Height > 2.0m.",
                           chk_type = "error"))

kbl_ht_oor <- make_kable(ht_oor, cap = "Points with a Height > 2.0m.")

### Find heights > 99% ever recorded ----
point_ht99 <- quantile(point_int$Height,
                       probs = 0.998,
                       na.rm = T)

point99 <- point_int |>
  filter(Height > point_ht99) |>
  mutate(Height99 = point_ht99) |>
  arrange()

QC_table <- rbind(QC_table,
                  QC_check(point99,
                           tab = "Heights over 99.8pct",
                           meas_type = "Point Intercept",
                           check = "Heights greater than 99.8pct ever recorded.",
                           chk_type = 'check'))

kbl_point99 <- make_dt(point99,
                       cap = "Heights greater than 99.8pct ever recorded.")

### Find inconsistent Status Codes ----
stat_code <- point_int |>
  filter(!Status %in% c("D", "L")) |>
  filter(!is.na(Status)) |>
  filter(!Status == "") |>
  select(MacroPlot_Name,
         Unit_Name,
         SampleEvent_Date,
         year,
         month,
         Transect,
         Point,
         Tape,
         Order,
         ScientificName,
         Status,
         NotBiological)

QC_table <- rbind(QC_table,
                  QC_check(df = stat_code,
                           tab = "Status Typos",
                           meas_type = "Point Intercept",
                           check = "Status codes that are not L or D",
                           chk_type = "error"))

kbl_stat_code <- make_kable(stat_code,
                            cap = "Status codes that are not L or D")

### Find blank status codes ----
stat_blank <- point_int |>
  filter(Status == "") |>
  select(MacroPlot_Name,
         Unit_Name,
         SampleEvent_Date,
         year,
         month,
         Transect,
         Point,
         Tape,
         Order,
         ScientificName,
         Status,
         NotBiological)

QC_table <- rbind(QC_table,
                  QC_check(df = stat_blank,
                           tab = "Status Blanks",
                           meas_type = "Point Intercept",
                           check = "Status codes that are blank",
                           chk_type = "error"))

dt_stat_blank <- make_dt(stat_blank,
                         cap = "Status codes that are blank")

### Incorrect NotBiological and Status combos ----
nb_l_stat <- point_int |>
  filter(NotBiological == TRUE & Status == "L")

QC_table <- rbind(QC_table,
                  QC_check(df = nb_l_stat,
                           tab = "Live NotBiological",
                           meas_type = "Point Intercept",
                           check = "Status = Live and NotBiological = TRUE",
                           chk_type = "error"))

dt_nb_l_stat <- make_dt(nb_l_stat,
                        cap = "Status = Live and NotBiological = TRUE")

# Check for incorrect NotBiological and Status combos
# nb_dm_stat <- point_int |> filter((NotBiological == FALSE & Status == "D") |
#                                    (NotBiological == FALSE & is.na(Status)))
#
# QC_table <- rbind(QC_table,
#                   QC_check(nb_dm_stat, tab = "Dead NotBiological", meas_type = "Point Intercept",
#                            check = "Status = Dead or Blank and NotBiological = FALSE",
#                            chk_type = "error"))
#
# kbl_nb_l_stat <- make_kable(nb_dm_stat, cap = "Status = Live and NotBiological = TRUE")

#### Transect numbers that aren't 1 or 2 ----
trans12 <- point_int |>
  filter(!Transect %in% c(1, 2)) |>
  select(MacroPlot_Name,
         Unit_Name,
         SampleEvent_Date,
         year,
         month,
         Transect)

QC_table <- rbind(QC_table,
                  QC_check(df = trans12,
                           tab = "Odd Transects",
                           meas_type = "Point Intercept",
                           check = "Transects not numbered 1 or 2",
                           chk_type = 'error'))

kbl_trans12 <- make_kable(trans12, cap = "Transects not numbered 1 or 2")

# transects != 50m
# trans50 <- point_int |> filter(TranLen != 50) |>
#   select(MacroPlot_Name, Unit_Name, SampleEvent_Date, year, month, TranLen, NumPtsTran, Transect)
#
# QC_table <- rbind(QC_table,
#                   QC_check(df = trans50, tab = "Transects not 50m", meas_type = "Point Intercept",
#                            check = "Transects that are more or less than 50m",
#                            chk_type = 'error'))
#
# kbl_trans50 <- make_kable(trans50, cap = "Transects that are more or less than 50m")

# check if Point Intercept checks returned at least 1 record to determine whether to include tab
pint_check <- QC_table |>
  filter(Type %in% "Point Intercept" & Num_Records > 0)

pint_include <- tab_include(pint_check)


## Quadrats/Density Belts ----
densb1 <- getDensityBelts(years = year_range, purpose = "NGPN_PCM") |>
  mutate(year = as.numeric(year)) |>
  select(MacroPlot_Name,
         Unit_Name,
         SampleEvent_Date,
         MacroPlot_Purpose,
         year,
         month,
         NumTran,
         TranLen,
         TranWid,
         Area,
         Transect,
         Subbelt,
         SubFrac,
         Status,
         Count,
         Symbol,
         ITIS_TSN,
         ScientificName,
         UV1,
         UV2,
         UV3)

densb <- bind_rows(densb1 |>
                     filter(!Unit_Name == "THRO") |>
                     semi_join(panel_sch,
                               by = c("MacroPlot_Purpose" = "Panel",
                                      "year" = "Year")),
                   densb1 |>
                     filter(Unit_Name == "THRO") |>
                     semi_join(panel_sch_thro,
                               by = c("MacroPlot_Purpose" = "Panel",
                                      "year" = "Year")))


### Transect numbers that aren't 1 or 2 ----
dtrans12 <- densb |>
  filter(!Transect %in% c(1, 2)) |>
  select(MacroPlot_Name,
         Unit_Name,
         SampleEvent_Date,
         year,
         month,
         Transect)

QC_table <- rbind(QC_table,
                  QC_check(df = dtrans12,
                           tab = "Odd Transects",
                           meas_type = "Quadrats",
                           check = "Transects not numbered 1 or 2",
                           chk_type = 'error'))

kbl_dtrans12 <- make_kable(dtrans12,
                           cap = "Transects not numbered 1 or 2")

# Transect areas not 1 or 10
# trans1_10 <- densb |> filter(!Area %in% c(1, 10)) |>
#   select(MacroPlot_Name, Unit_Name, SampleEvent_Date, year, month, Transect, Area)
#
# QC_table <- rbind(QC_table,
#                   QC_check(df = trans1_10, tab = "Odd Areas", meas_type = "Nested Quadrats",
#                            check = "Areas that are either blank or not 1 or 10",
#                            chk_type = 'error'))
# kbl_trans1_10 <- make_kable(trans1_10, cap = "Areas that are either blank or not 1 or 10")
# Returns ~ 500 records

### Transects != 1m ----
dtrans1 <- densb |>
  filter(TranLen != 1) |>
  select(MacroPlot_Name,
         Unit_Name,
         SampleEvent_Date,
         year,
         month,
         TranLen,
         Transect)

QC_table <- rbind(QC_table,
                  QC_check(df = dtrans1,
                           tab = "Transect Lengths not = 1",
                           meas_type = "Quadrats",
                           check = "Transects that are not = 1",
                           chk_type = 'error'))

kbl_dtrans1 <- make_kable(dtrans1, cap = "Transect lengths that are not = 1")

# Transect width != 1
# transwid <- densb |> filter(TranWid != 1) |>
#   select(MacroPlot_Name, Unit_Name, SampleEvent_Date, year, month, TranWid, Transect)
#
# QC_table <- rbind(QC_table,
#                   QC_check(df = transwid, tab = "Transect width != 1m", meas_type = "Nested Quadrats",
#                            check = "Transect widths that are different from 1m",
#                            chk_type = "error"))
#
# kbl_transwid <- make_kable(transwid, cap = "Transect widths that are different from 1m")

### Subfractions that don't match ----
subfracs <- c(0.01, 0.1, 1, 10)

subfrac_imp <- densb |>
  filter(!SubFrac %in% subfracs) |>
  select(MacroPlot_Name,
         Unit_Name,
         SampleEvent_Date,
         year,
         month,
         Transect,
         SubFrac)

QC_table <- rbind(QC_table,
                  QC_check(df = subfrac_imp,
                           tab = "SubFrac Typo",
                           meas_type = "Quadrats",
                           check = "Subfractions that are outside acceptible values",
                           chk_type = 'error'))

kbl_subfrac_imp <- make_kable(subfrac_imp,
                              "Subfractions that are outside acceptible values")

# Check that Subbelts are 1-5
# subbelt <- densb |> filter(!Subbelt %in% c(1, 2, 3, 4, 5)) |>
#   select(MacroPlot_Name, Unit_Name, SampleEvent_Date, year, month, Transect, Subbelt)
#
# QC_table <- rbind(QC_table,
#                   QC_check(df = subbelt, tab = "Subbelt Typo", meas_type = "Nested Quadrats",
#                            check = "Subbelts that are outside acceptible values",
#                            chk_type = 'error'))
#
# kbl_subbelt <- make_kable(subbelt, "Subbelts that are outside acceptible values")

### Species Counts != 1 ----
spp_count <- densb |>
  filter(Count != 1) |>
  select(MacroPlot_Name,
         SampleEvent_Date,
         year,
         Transect,
         Subbelt,
         SubFrac,
         Symbol,
         ScientificName,
         Count)

QC_table <- rbind(QC_table,
                  QC_check(df = spp_count,
                           tab = "Count != 1",
                           meas_type = "Quadrats",
                           check = "Recorded count value != 1",
                           chk_type = "error"))

kbl_spp_count <- make_kable(spp_count, cap = "Recorded count value != 1")

### Species count > 1 ----
spp_dup <- densb |>
  filter(Count == 1) |>
  select(MacroPlot_Name,
         SampleEvent_Date,
         year,
         Transect,
         Subbelt,
         SubFrac,
         Symbol,
         ScientificName,
         Count) |>
  unique() |>
  pivot_wider(names_from = SubFrac,
              values_from = Count,
              names_prefix = "SubFrac_",
              values_fill = 0)

subfrac_cols <- c("SubFrac_0.01",
                  "SubFrac_0.1",
                  "SubFrac_1",
                  "SubFrac_10")

missing <- setdiff(subfrac_cols, names(spp_dup))

spp_dup[missing] <- 0

spp_dup$Count <- spp_dup$SubFrac_0.01 + spp_dup$SubFrac_0.1 + spp_dup$SubFrac_1 + spp_dup$SubFrac_10

spp_dup2 <- spp_dup |> filter(Count > 1)

QC_table <- rbind(QC_table,
                  QC_check(spp_dup2, tab = "Duplicate Spp.",
                           meas_type = "Quadrats",
                           check = "Species with more than one count per subbelt",
                           chk_type = "error"))

kbl_spp_dup2 <- make_kable(spp_dup2, cap = "Species with more than one count per subbelt.")

### Transect Area checks ----
# UV1_text <- densb |>
#   filter(!UV1 %in% c(0, 1, 2, 3)) |>
#   filter(!is.na(UV1))
#
# QC_table <- rbind(QC_table,
#                   QC_check(UV1_text, tab = "UV1 non-numeric",
#                            meas_type = "Quadrats",
#                            check = "Values in UV1 that are not 0, 1, 2, or 3 to indicate SubPlot fraction."))
#
# kbl_UV1_text <- make_kable(UV1_text, cap = "Values in UV1 that are not 0, 1, 2, or 3 to indicate SubPlot fraction.
#                            Note that there's little consistency in the UV1 column being used to indicate the
#                            SubPlot fraction. Instead of fixing >100,000 records that are either NA or have text in them,
#                            use the SubFrac column to distinguish among the sizes of the nested quadrats.")

# The queries below return A LOT of records. It doesn't appear the UV1 column has consistently
# been used the way the USGS QC check suggests.
# trans10_sub1 <- densb |> filter(Area == 10) |> filter(SubFrac == 1) |> filter(!UV1 %in% 0 | is.na(UV1))
# trans10_sub01 <- densb |> filter(Area == 10) |> filter(SubFrac == 0.1) |> filter(!UV1 %in% 1 | is.na(UV1))
# trans10_sub001 <- densb |> filter(Area == 10) |> filter(SubFrac == 0.01) |> filter(!UV1 %in% 2 | is.na(UV1))

# trans1_sub1 <- densb |> filter(Area == 1) |> filter(SubFrac == 1) |> filter(!UV1 %in% 1 | is.na(UV1))
# trans1_sub01 <- densb |> filter(Area == 1) |> filter(SubFrac == 0.1) |> filter(!UV1 %in% 2 | is.na(UV1))
# trans1_sub001 <- densb |> filter(Area == 1) |> filter(SubFrac == 0.01) |> filter(!UV1 %in% 3 | is.na(UV1))

# check if Nested Quadrats checks returned at least 1 record to determine whether to include tab
densb_check <- QC_table |>
  filter(Type %in% "Quadrats" & Num_Records > 0)

densb_include <- tab_include(densb_check)

## Trees and Poles ----
trees1 <- getTrees(years = year_range, purpose = "NGPN_PCM") |>
  select(MacroPlot_Name,
         Unit_Name,
         SampleEvent_Date,
         year,
         month,
         MacroPlotSize,
         SnagPlotSize,
         BrkPntDia,
         QTR,
         SubFrac,
         TagNo,
         Symbol,
         ScientificName,
         Status,
         DBH,
         CrwnCl,
         LiCrBHt,
         CrwnRad,
         DRC,
         UV1,
         UV2)

trees <- left_join(trees1,
                   tab4_spp,
                   by = c("ScientificName",
                          "Symbol")) |>
 mutate(diam = pmax(DBH,
                    DRC,
                    na.rm = T))

# Check that species that are DRC are correctly sampled as DRC (taken from Table 4 of NGPN PCM SOPs (p94))
drc_spp <- tab4_spp$Symbol[tab4_spp$Diam_Loc == "Root Collar"]

### Wrong DRC vs DBH ----
trees_wrong_drc <- trees |>
  filter(Symbol %in% drc_spp & !is.na(DBH)) |>
  select(MacroPlot_Name,
         SampleEvent_Date,
         year,
         QTR,
         SubFrac,
         TagNo,
         Symbol,
         ScientificName,
         Status,
         DBH,
         DRC)

QC_table <- rbind(QC_table,
                  QC_check(trees_wrong_drc, tab = "Wrong Diam - DRC",
                           meas_type = "Trees and Poles",
                           check = "Trees that should have had DRC instead of DBH measured.",
                           chk_type = 'error'))

dt_trees_wrong_drc <- make_dt(trees_wrong_drc,
                              cap = "Trees that should have had DRC instead of DBH measured, based on Table 4 in SOPs.")

### Wrong DBH vs DRC ----
trees_wrong_dbh <- trees |>
  filter((!Symbol %in% drc_spp) & !is.na(DRC))|>
  select(MacroPlot_Name,
         SampleEvent_Date,
         year,
         QTR,
         SubFrac,
         TagNo,
         Symbol,
         ScientificName,
         Status,
         DBH,
         DRC)

QC_table <- rbind(QC_table,
                  QC_check(trees_wrong_dbh,
                           tab = "Wrong Diam - DBH",
                           meas_type = "Trees and Poles",
                           check = "Trees that should have had DBH instead of DRC measured.",
                           chk_type = 'error'))

dt_trees_wrong_dbh <- make_dt(trees_wrong_dbh,
                                  cap = "Trees that should have had DBH instead of DRC measured, based on Table 4 in SOPs.")

### Check trees > 80 cm for possible errors ----
bigt <- trees |>
  filter(diam > 80) |>
  select(MacroPlot_Name,
         SampleEvent_Date,
         year,
         QTR,
         SubFrac,
         TagNo,
         Symbol,
         ScientificName,
         DBH,
         DRC,
         diam)

QC_table <- rbind(QC_table,
                  QC_check(bigt, tab = "Big Trees", meas_type = "Trees and Poles",
                           check = "Trees greater than 80cm DBH or DRC to check for possible errors.",
                           chk_type = "check"))

#hist(bigt$diam, main = "Distribution of DBH/DRC > 60cm", xlab = "DBH/DRC class")
dt_bigt <- make_dt(bigt |>
                     select(-diam),
                   cap = "Trees greater than 60cm DBH or DRC to check for possible errors")

### Check tree Diam >99% of ever recorded ----
tree_dbh99 <- quantile(trees$DBH, probs = 0.99, na.rm = T)

trees99 <- trees |>
  filter(DBH > tree_dbh99)

QC_table <- rbind(QC_table,
                  QC_check(trees99,
                           tab = "DBH over 99pct",
                           meas_type = "Trees and Poles",
                           check = "Tree DBH measurements greater than 99pct ever recorded.",
                           chk_type = 'check'))

dt_trees99 <- make_dt(trees99,
                      cap = "Tree DBH measurements greather than 99pct ever recorded. An alternate way of checking for out of range values.")

### Check trees > 30 cm DRC for possible errors ----
bigdrc <- trees |>
  filter(DRC > 30) |>
  select(MacroPlot_Name,
         SampleEvent_Date,
         year,
         QTR,
         SubFrac,
         TagNo,
         Symbol,
         ScientificName,
         DRC)

QC_table <- rbind(QC_table,
                  QC_check(bigdrc,
                           tab = "Big DRCs",
                           meas_type = "Trees and Poles",
                           check = "Trees greater than 30cm DRC to check for possible errors.",
                           chk_type = "check"))

dt_bigdrc <- make_dt(bigdrc,
                     cap = "Trees greater than 30cm DRC to check for possible errors.")


### Check tree Diam >99% of ever recorded ---
tree_drc99 <- quantile(trees$DRC, probs = 0.99, na.rm = T)

trees_drc99 <- trees |>
  filter(DRC > tree_drc99)

QC_table <- rbind(QC_table,
                  QC_check(trees_drc99,
                           tab = "DRC over 99pct",
                           meas_type = "Trees and Poles",
                           check = "Tree DRC measurements greater than 99pct ever recorded.",
                           chk_type = 'check'))

kbl_trees_drc99 <- make_kable(trees_drc99,
                              "Tree DRC measurements greather than 99pct ever recorded. An alternate way of checking for out of range values.")

## Check for missing tree data ----
### Trees >15cm DBH should have UV1 In/Out and UV2 Condition Code column and that are Trees (not shrubs) ----
tree_miss_uv <- trees |>
  filter(diam > 15) |>
  filter(GrowthForm == "Tree") |>
  filter(is.na(UV1) | is.na(UV2)) |>
  select(MacroPlot_Name,
         SampleEvent_Date,
         year,
         QTR,
         SubFrac,
         TagNo,
         Symbol,
         ScientificName,
         GrowthForm,
         DBH,
         DRC,
         UV1,
         UV2)

QC_table <- rbind(QC_table,
                  QC_check(tree_miss_uv, tab = "Trees missing UV",
                           meas_type = "Trees and Poles",
                           check = "Trees > 15cm DBH or DRC missing UV1 and or UV2 values.",
                           chk_type = "check"))

dt_tree_miss_uv <- make_dt(tree_miss_uv, cap = "Trees > 15cm DBH or DRC missing UV1 and or UV2 values.")

### Inconsistent IN/OUT in UV1 ----
tree_incon_UV1 <- trees |>
  filter(diam > 15) |>
  filter(GrowthForm == "Tree") |>
  filter(!UV1 %in% c("IN", "OUT") & !is.na(UV1)) |>
  select(MacroPlot_Name,
         SampleEvent_Date,
         year,
         QTR,
         SubFrac,
         TagNo,
         Symbol,
         ScientificName,
         GrowthForm,
         DBH,
         DRC,
         UV1)

QC_table <- rbind(QC_table,
                  QC_check(tree_incon_UV1,
                           tab = "Trees incon. UV1",
                           meas_type = "Trees and Poles",
                           check = "Trees > 15cm DBH or DRC with UV1 values that don't perfectly match 'IN' or 'OUT'.",
                           chk_type = "check"))

dt_tree_incon_UV1 <- make_dt(tree_incon_UV1, cap = "Trees > 15cm DBH or DRC with UV1 values that don't perfectly match 'IN' or 'OUT'.")

### Check that UV2 tree conditions match SOP conditions (page 85/103 of SOP) ----
tree_cond_list <- c( "BKN", "CAMB", "DBK", "DEC", "DIS", "INS", "MPBB", "MPBG", "ROOT",
                     "SCAR", "SCORCH1", "SCORCH2", "SCORCH3", "SCORCH4", "SND")

tree_conds_UV2 <- trees |>
  filter(diam > 15) |>
  filter(GrowthForm == "Tree") |>
  filter(Status == "L") |>
  filter(!UV2 %in% tree_cond_list & !is.na(UV2)) |>
  select(MacroPlot_Name,
         SampleEvent_Date,
         year,
         QTR,
         SubFrac,
         TagNo,
         Symbol,
         ScientificName,
         GrowthForm,
         DBH,
         DRC,
         UV2)

QC_table <- rbind(QC_table,
                  QC_check(tree_conds_UV2,
                           tab = "Trees incon. UV2",
                           meas_type = "Trees and Poles",
                           check =
                           paste0(
                           "Live trees > 15cm DBH or DRC with UV2 values that don't match codes in SOP: ",
                           paste0(tree_cond_list, collapse = ", "),
                           "."),
                           chk_type = "check"))

dt_tree_conds_UV2 <- make_dt(tree_conds_UV2, cap = "Live trees > 15cm DBH or DRC with UV1 values that don't perfectly match 'IN' or 'OUT'.")

### Check if poles have UV1 or UV2 entered ----
pole_uv <- trees |>
  filter(diam <= 15) |>
  filter(!is.na(UV1) | !is.na(UV2))|>
  select(MacroPlot_Name,
         SampleEvent_Date,
         year,
         QTR,
         SubFrac,
         TagNo,
         Symbol,
         ScientificName,
         GrowthForm,
         DBH,
         DRC,
         UV1,
         UV2)

QC_table <- rbind(QC_table,
                  QC_check(pole_uv, tab = "Poles with UV",
                           meas_type = "Trees and Poles",
                           check = "Poles <= 15cm DBH or DRC with UV1 or UV2 values.",
                           chk_type = "check"))

dt_pole_uv <- make_dt(pole_uv, cap = "Poles <= 15cm DBH or DRC with UV1 or UV2 values.")

# Check for Trees < 2.54 DBH
small_dbh <- trees |>
  filter(diam <= 2.54) |>
  select(MacroPlot_Name,
         SampleEvent_Date,
         year,
         QTR,
         SubFrac,
         TagNo,
         Symbol,
         ScientificName,
         GrowthForm,
         DBH,
         DRC)

QC_table <- rbind(QC_table,
                  QC_check(small_dbh,
                           tab = "DBH under 2.54",
                           meas_type = "Trees and Poles",
                           check = "Stems with DBH or DRC <= 2.54cm",
                           chk_type = 'error'))

kbl_small_dbh <- make_kable(small_dbh, cap = "Stems with DBH or DRC <= 2.54cm")

# Check if MacroPlot purpose and subfrac match
# mac_trees <- inner_join(mac_samp |> select(MacroPlot_Name, MacroPlot_Purpose) |> distinct(),
#                         trees,
#                         by = "MacroPlot_Name")
# table(mac_trees$MacroPlot_Purpose, mac_trees$SubFrac)
# Not sure how to make this check, given that the Purposes don't match what the check suggests (intensive vs extensive).

### Inconsistent tree status codes ---
tree_statcodes <- trees |>
  filter(!Status %in% c("L", "D") & !is.na(Status)) |>
  select(MacroPlot_Name,
         SampleEvent_Date,
         year,
         QTR,
         SubFrac,
         TagNo,
         Symbol,
         ScientificName,
         GrowthForm,
         DBH,
         DRC)

QC_table <- rbind(QC_table,
                  QC_check(tree_statcodes,
                           tab = "Incons. Status Codes",
                           meas_type = "Trees and Poles",
                           check = "Status codes that don't exactly match L or D",
                           chk_type = 'error'))

kbl_tree_statcodes <- make_kable(tree_statcodes, cap = "Status codes that don't exactly match L or D")

### Check UV2 for dead trees ----
dead_tree_codes <- c("CS", "LS", "RS")

dead_con <- trees |>
  filter(diam > 15) |>
  filter(GrowthForm == "Tree") |>
  filter(Status == "D") |>
  filter(!UV2 %in% dead_tree_codes & !is.na(UV2))

QC_table <- rbind(QC_table,
                  QC_check(dead_con,
                           tab = "Incons. Dead UV2",
                           meas_type = "Trees and Poles",
                           check = "Status codes that don't exactly match dead condition codes : 'CS', 'LS', 'RS'",
                           chk_type = 'error'))

dt_dead_con <- make_dt(dead_con,
                       cap = "Status codes that don't exactly match dead condition codes : 'CS', 'LS', 'RS'")

### Check for missing UV2 for dead trees ----
dead_miss_uv2 <- trees |>
  filter(diam > 15) |>
  filter(GrowthForm == "Tree") |>
  filter(Status == "D") |>
  filter(is.na(UV2))

QC_table <- rbind(QC_table,
                  QC_check(dead_miss_uv2,
                           tab = "Dead missing UV2",
                           meas_type = "Trees and Poles",
                           check = "Dead trees > 15cm DBH or DRC missing UV2 values.",
                           chk_type = 'error'))

kbl_dead_miss_uv2 <- make_kable(dead_miss_uv2,
                           cap = "Dead trees > 15cm DBH or DRC missing UV2 values.")

# check if tree checks returned at least 1 record to determine whether to include tab
tree_check <- QC_table |>
  filter(Type %in% "Trees and Poles" & Num_Records > 0)

tree_include <- tab_include(tree_check)

## Density Quadrats (seedlings) ----
# seeds <- getDensityQuadrats(years = year_range, purpose = "NGPN_PCM") |>
#   select(MacroPlot_Name,
#          SampleEvent_Date,
#          year,
#          month,
#          NumTran,
#          NumQuadTran,
#          QuadLen,
#          QuadWid,
#          Area,
#          Transect,
#          Quadrat,
#          Status,
#          SizeCl,
#          AgeCl,
#          Count,
#          SubFrac,
#          Symbol,
#          ScientificName)

### Check that if counts are < 100, SubFrac == 1 (page 86/105 in SOP) ----
# subfrac100 <- seeds |>
#   summarize(Count = sum(Count),
#             .by = c(MacroPlot_Name,
#                     SampleEvent_Date,
#                     year,
#                     month,
#                     Area,
#                     Transect,
#                     SizeCl,
#                     Symbol,
#                     SubFrac,
#                     ScientificName)) |>
#   filter(Count < 100) |>
#   filter(SubFrac != 1)
#
# QC_table <- rbind(QC_table,
#                   QC_check(subfrac100,
#                            tab = "Under 100 Count",
#                            meas_type = "Seedlings",
#                            check = "Counts < 100 for a species on a plot with a SubFrac < 1.",
#                            chk_type = 'error'))
#
# dt_subfrac100 <- make_dt(subfrac100,
#                          "Counts < 100 for a species on a plot with a SubFrac < 1.")


### Check that if counts are > 100, SubFrac != 1 (page 86/105 in SOP) ----
# subfrac100b <- seeds |>
#   group_by(MacroPlot_Name,
#            SampleEvent_Date,
#            year,
#            month,
#            Area,
#            Transect,
#            SizeCl,
#            Symbol,
#            SubFrac,
#            ScientificName) |>
#   summarize(Count = sum(Count),
#             .by = ) |>
#   filter(Count > 100) |> filter(SubFrac == 1)
#
# QC_table <- rbind(QC_table,
#                   QC_check(subfrac100b, tab = "Over 100 Count", meas_type = "Seedlings",
#                            check = "Counts > 100 for a species on a plot with a SubFrac = 1.",
#                            chk_type = 'error'))
#
# dt_subfrac100b <- make_dt(subfrac100b, "Counts > 100 for a species on a plot with a SubFrac = 1.")

# check if seedlings checks returned at least 1 record to determine whether to include tab
# seed_check <- QC_table |> filter(Type %in% "Seedlings" & Num_Records > 0)
# seed_include <- tab_include(seed_check)

## CWD Checks ----

cwd1 <- getFuels1000(years = year_range, purpose = "NGPN_PCM") |>
  select(MacroPlot_Name,
         Unit_Name,
         SampleEvent_Date,
         year,
         NumTran,
         TranLen,
         Transect,
         Slope,
         LogNum,
         Dia,
         DecayCl,
         CWDFuConSt,
         Comment,
         SaComment)

# add veg type from MacroPlot table to help understand why non-standard data shows up
macroveg <- getMacroPlot(purpose = "NGPN_PCM") |>
  select(MacroPlot_Name,
         veg_type = MacroPlot_UV4)

cwd <- left_join(cwd1,
                 macroveg,
                 by = "MacroPlot_Name")

### Check that cwd is only on Ponderosa Pine forests ----
# macro_PP <- macroveg |>
#   filter(grepl("PP", veg_type))
#
# pp_plots <- unique(macro_PP$MacroPlot_Name)
#
# cwd_non_pp <- cwd |>
#   filter(!MacroPlot_Name %in% pp_plots) |>
#   select(MacroPlot_Name,
#          Unit_Name,
#          SampleEvent_Date,
#          year,
#          Transect) |>
#   distinct()
#
# QC_table <- rbind(QC_table,
#                   QC_check(cwd_non_pp,
#                            tab = "CWD on non-PP plots",
#                            meas_type = "Coarse Woody Debris",
#                            check = "Coarse Woody Debris record on plots not classified as Ponderosa Pine,
#                            based on MacroPlot_UV4 column.",
#                            chk_type = 'error'))
#
# dt_cwd_non_pp <- make_dt(cwd_non_pp,
#                          cap = "Coarse Woody Debris record on plots not classified as Ponderosa Pine, based on MacroPlot_UV4 column. Returns a lot of records, so may not be a worthwhile check")

### Plots with blank transects ----
# cwd_na <- cwd |>
#   filter(is.na(Transect))
#
# QC_table <- rbind(QC_table,
#                   QC_check(cwd_na,
#                            tab = "Blank Transect data",
#                            meas_type = "Coarse Woody Debris",
#                            check = "Coarse Woody Debris records with blank Transect data.",
#                            chk_type = 'error'))
#
# dt_cwd_na <- make_dt(cwd_na,
#                      "Coarse Woody Debris records with blank Transect data. Returns a lot of records, so may not be a worthwhile check.")

### Check that slopes are < abs(100) ----
# cwd_slope100 <- cwd |>
#   filter(abs(Slope) >= 100)
#
# QC_table <- rbind(QC_table,
#                   QC_check(cwd_slope100,
#                            tab = "CWD slope over 100",
#                            meas_type = "Coarse Woody Debris",
#                            check = "Coarse Woody Debris slopes that are >= 100.",
#                            chk_type = 'error'))
#
# kbl_cwd_slope100 <- make_kable(cwd_slope100,
#                                "Coarse Woody Debris slopes that are >= 100")

### Check Trans length is 100 ---
# cwd_translen <- cwd |>
#   filter(TranLen != 100)
#
# QC_table <- rbind(QC_table,
#                   QC_check(cwd_translen, tab = "CWD TransLen not 100", meas_type = "Coarse Woody Debris",
#                            "Coarse Woody Debris transect lengths that are not equal to 100ft.",
#                            chk_type = 'error'))
# kbl_cwd_translen <- make_kable(cwd_translen, "Coarse Woody Debris transect lengths that are not equal to 100ft.")

### Check that 2 transects are sampled ----
# cwd_2trans <- cwd |>
#   filter(NumTran != 2)
#
# QC_table <- rbind(QC_table,
#                   QC_check(cwd_2trans,
#                            tab = "Missing CWD Transects",
#                            meas_type = "Coarse Woody Debris",
#                            "Coarse Woody Debris records where NumTran column does not equal 2",
#                            chk_type = 'error'))
#
# kbl_cwd_2trans <- make_kable(cwd_2trans, "Coarse Woody Debris records where NumTran column does not equal 2")

### Check that CWD diam > 3" ----
# cwd_small <- cwd |> filter(Dia <= 3)
#
# QC_table <- rbind(QC_table,
#                   QC_check(cwd_small, tab = "CWD less than 3in", meas_type = "Coarse Woody Debris",
#                            check = "Coarse Woody Debris less than or equal to 3in.",
#                            chk_type = 'error'))
#
# kbl_cwd_small <- make_kable(cwd_small, cap = "Coarse Woody Debris less than or equal to 3in.")

### Check coarse fuel constant if not listed as Ponderosa Pine ----
#table(cwd$CWDFuConSt)
# cwdfuconst <- cwd |> filter(!CWDFuConSt %in% "Ponderosa pine" & !is.na(CWDFuConSt))
#
# QC_table <- rbind(QC_table,
#                   QC_check(cwdfuconst, tab = "CWDFuConSt not PP", meas_type = "Coarse Woody Debris",
#                   check = "Coarse Woody Debris CWDFuConSt records that are not Ponderosa pine.",
#                   chk_type = "error"))
#
# dt_cwdfuconst <- make_dt(cwdfuconst, "Coarse Woody Debris CWDFuConSt records that are not Ponderosa pine.
#                          Check taken from FFI_QAQC_UserGuide_20130930.docx.")

### Check Dia that are >99% of recorded Dia ---
# dia99 = quantile(cwd$Dia, probs = 0.99, na.rm = T)
#
# cwd99 <- cwd |> filter(Dia > dia99)
#
# QC_table <- rbind(QC_table,
#                   QC_check(cwd99, tab = "CWD diam over 99pct", meas_type = "Coarse Woody Debris",
#                            check = "Coarse Woody Debris diameters > 99% of diameters ever recorded",
#                            chk_type = 'check'))
#
# dt_cwd99 <- make_dt(cwd99, "Coarse Woody Debris diameters > 99% of diameters ever recorded")

# check if cwd checks returned at least 1 record to determine whether to include tab
# cwd_check <- QC_table |> filter(Type %in% "Coarse Woody Debris" & Num_Records > 0)
#
# cwd_include <- tab_include(cwd_check)

## FWD Checks ----
# fwd1 <- getFuelsFine(years = year_range, purpose = "NGPN_PCM") |>
#   select(MacroPlot_Name, Unit_Name, SampleEvent_Date, year, NumTran, OneHrTranLen, TenHrTranLen,
#          HunHrTranLen, Transect, Azimuth_Fuels, Slope, OneHr, TenHr, HunHr,
#          FWDFuConSt, UV1Desc)
#
# fwd <- left_join(fwd1, macroveg, by = "MacroPlot_Name")

### Plots not classified as Ponderosa Pine ----
# fwd_non_pp <- fwd |> filter(!MacroPlot_Name %in% pp_plots) |>
#   select(MacroPlot_Name, Unit_Name, SampleEvent_Date, year, Transect) |>
#   distinct()
#
# QC_table <- rbind(QC_table,
#                   QC_check(fwd_non_pp, tab = "FWD on non-PP plots", meas_type = "Fine Woody Debris",
#                            check = "Fine Woody Debris record on plots not classified as Ponderosa Pine,
#                            based on MacroPlot_UV4 column.",
#                            chk_type = 'error'))
#
# dt_fwd_non_pp <- make_dt(fwd_non_pp, "Fine Woody Debris record on plots not classified as Ponderosa Pine,
#                            based on MacroPlot_UV4 column. Returns a lot of records, so may not be
#                            a worthwhile check.")

### Plots with blank transects ----
# fwd_na <- fwd |> filter(is.na(Transect))
#
# QC_table <- rbind(QC_table,
#                   QC_check(fwd_na, tab = "Blank Transect data", meas_type = "Fine Woody Debris",
#                            check = "Fine Woody Debris records with blank Transect data.",
#                            chk_type = 'error'))
#
# dt_fwd_na <- make_dt(fwd_na, "Fine Woody Debris records with blank Transect data. Returns a
#                            lot of records, so may not be a worthwhile check.")

### Check that slopes are < abs(100) ---
# fwd_slope100 <- fwd |> filter(abs(Slope) >= 100)
#
# QC_table <- rbind(QC_table,
#                   QC_check(fwd_slope100, tab = "FWD slope over 100", meas_type = "Fine Woody Debris",
#                            check = "Fine Woody Debris slopes that are >= 100.",
#                            chk_type = 'error'))
#
# kbl_fwd_slope100 <- make_kable(fwd_slope100, "Fine Woody Debris slopes that are >= 100")

### Check that Azimuth_Fuels <= 360 ----
# fwd_az360 <- fwd |> filter(Azimuth_Fuels > 360 | Azimuth_Fuels < 0)
#
# QC_table <- rbind(QC_table,
#                   QC_check(fwd_az360, tab = "FWD Azimuth over 360", meas_type = "Fine Woody Debris",
#                            check = "Fine Woody Debris Azimuth_Fuels > 360 or < 0.",
#                            chk_type = 'error'))
#
# kbl_fwd_az360 <- make_kable(fwd_slope100, "Fine Woody Debris Azimuth_Fuels > 360 or < 0.")

### Check plots with non-standard transect lengths ----
#### One & 10 hour != 6----
# fwd_1_10hr <- fwd |> filter(OneHrTranLen != 6 | TenHrTranLen != 6)
#
# QC_table <- rbind(QC_table,
#                   QC_check(fwd_1_10hr, tab = "1-10hr transect not 6ft", meas_type = "Fine Woody Debris",
#                            check = "One or 10-hour transect lengths not equal to 6ft.",
#                            chk_type = "error"))
#
# kbl_fwd_1_10hr <- make_kable(fwd_1_10hr, "One or 10-hour transect lengths not equal to 6ft.")

#### 100 hour != 12 ----
# fwd_100hr <- fwd |> filter(HunHrTranLen != 12)
#
# QC_table <- rbind(QC_table,
#                   QC_check(fwd_100hr, tab = "100hr transect not 12ft", meas_type = "Fine Woody Debris",
#                            check = "100-hour transect lengths not equal to 12ft.",
#                            chk_type = "error"))
#
# kbl_fwd_100hr <- make_kable(fwd_100hr, "100-hour transect lengths not equal to 12ft.")

### missing transects ----
# fwd_2trans <- fwd |> filter(NumTran != 2 & !is.na(NumTran))
#
# QC_table <- rbind(QC_table,
#                   QC_check(fwd_2trans, tab = "Missing FWD Transects", meas_type = "Fine Woody Debris",
#                            "Fine Woody Debris records where NumTran column does not equal 2",
#                            chk_type = 'error'))
#
# kbl_fwd_2trans <- make_kable(fwd_2trans, "Fine Woody Debris records where NumTran column does not equal 2")

### Check coarse fuel constant if not listed as Ponderosa Pine ----
# table(fwd$FWDFuConSt)
# fwdfuconst <- fwd |> filter(!FWDFuConSt %in% "Ponderosa pine" & !is.na(FWDFuConSt))
#
# QC_table <- rbind(QC_table,
#                   QC_check(fwdfuconst, tab = "FWDFuConSt not PP", meas_type = "Fine Woody Debris",
#                            check = "Fine Woody Debris FWDFuConSt records that are not Ponderosa pine.",
#                            chk_type = "error"))
#
# dt_fwdfuconst <- make_dt(fwdfuconst, "Fine Woody Debris CWDFuConSt records that are not Ponderosa pine.
#                          Check taken from FFI_QAQC_UserGuide_20130930.docx.")

### Check counts that are >99% of recorded for the 3 fuels ----
#### 1 hr ----
# onehr99 <- quantile(fwd$OneHr, probs = 0.99, na.rm = T)
#
# fwd_1hr99 <- fwd |> filter(OneHr > onehr99)
#
# QC_table <- rbind(QC_table,
#                   QC_check(fwd_1hr99, tab = "One hour counts over 99pct", meas_type = "Fine Woody Debris",
#                            check = "Fine Woody Debris One-hour counts > 99% of counts ever recorded",
#                            chk_type = 'check'))
#
# dt_fwd_1hr99 <- make_dt(fwd_1hr99, "Fine Woody Debris One-hour counts > 99% of counts ever recorded")

#### 10 hr ----
# tenhr99 <- quantile(fwd$TenHr, probs = 0.99, na.rm = T)
#
# fwd_10hr99 <- fwd |> filter(TenHr > tenhr99)
#
# QC_table <- rbind(QC_table,
#                   QC_check(fwd_10hr99, tab = "Ten hour counts over 99pct", meas_type = "Fine Woody Debris",
#                            check = "Fine Woody Debris 10-hour counts > 99% of counts ever recorded",
#                            chk_type = 'check'))
#
# dt_fwd_10hr99 <- make_dt(fwd_10hr99, "Fine Woody Debris 10-hour counts > 99% of counts ever recorded")
#
# hunhr99 <- quantile(fwd$HunHr, probs = 0.99, na.rm = T)

#### 100 hr ----
# fwd_100hr99 <- fwd |> filter(HunHr > hunhr99)
#
# QC_table <- rbind(QC_table,
#                   QC_check(fwd_100hr99, tab = "Hundred hour counts over 99pct", meas_type = "Fine Woody Debris",
#                            check = "Fine Woody Debris 100-hour counts > 99% of counts ever recorded",
#                            chk_type = 'check'))
#
# dt_fwd_100hr99 <- make_dt(fwd_100hr99, "Fine Woody Debris One-hour counts > 99% of counts ever recorded")

# check if fwd checks returned at least 1 record to determine whether to include tab
fwd_check <- QC_table |>
  filter(Type %in% "Fine Woody Debris" & Num_Records > 0)

fwd_include <- tab_include(fwd_check)

## Duff Checks ----
# duff1 <- getFuelsDuff(years = year_range, purpose = "NGPN_PCM") |>
#   select(MacroPlot_Name, Unit_Name, SampleEvent_Date, year, NumTran,
#          Transect, SampLoc, OffSet, LittDep, DuffDep,
#          FuelbedDep, DLFuConSt, Comment, UV1Desc)
# duff <- left_join(duff1, macroveg, by = "MacroPlot_Name")

### check on duff not on pp ----
# duff_non_pp <- duff |> filter(!MacroPlot_Name %in% pp_plots) |>
#   select(MacroPlot_Name, Unit_Name, SampleEvent_Date, year, Transect) |>
#   distinct()
#
# QC_table <- rbind(QC_table,
#                   QC_check(duff_non_pp, tab = "Duff on non-PP plots", meas_type = "Duff and Litter",
#                            check = "Duff record on plots not classified as Ponderosa Pine,
#                            based on MacroPlot_UV4 column.",
#                            chk_type = 'error'))
#
# dt_duff_non_pp <- make_dt(duff_non_pp, "Duff record on plots not classified as Ponderosa Pine,
#                            based on MacroPlot_UV4 column. Returns a lot of records, so may not be
#                            a worthwhile check.")

### Plots with blank transects ----
# duff_na <- duff |> filter(is.na(Transect))
#
# QC_table <- rbind(QC_table,
#                   QC_check(duff_na, tab = "Blank Transect data", meas_type = "Duff and Litter",
#                            check = "Duff records with blank Transect data.",
#                            chk_type = 'error'))
#
# dt_duff_na <- make_dt(duff_na, "Duff records with blank Transect data. Returns a
#                            lot of records, so may not be a worthwhile check.")

### missing transects -----
# duff_2trans <- duff |> filter(NumTran != 2 & !is.na(NumTran))
#
# QC_table <- rbind(QC_table,
#                   QC_check(duff_2trans, tab = "Missing Duff Transects", meas_type = "Duff and Litter",
#                            check = "Duff records where NumTran column does not equal 2",
#                            chk_type = 'error'))
#
# kbl_duff_2trans <- make_kable(duff_2trans, "Duff records where NumTran column does not equal 2")

### Duff or litter > 3 ----
# duff3 <- duff |> filter(DuffDep >= 3 | LittDep >= 3)
#
# QC_table <- rbind(QC_table,
#                   QC_check(duff3, tab = "Depths over 3in", meas_type = "Duff and Litter",
#                            check = "Duff or Litter depths over 3in.",
#                            chk_type = "check"))
#
# dt_duff3 <- make_dt(duff3, "Duff or Litter depths over 3in.")

### Duff over 99% recorded ----
# duff99 <- quantile(duff$DuffDep, probs = 0.99, na.rm = T)
#
# duff_depth99 <- duff |> filter(DuffDep > duff99)
#
# QC_table <- rbind(QC_table,
#                   QC_check(duff_depth99, tab = "Duff depths over 99pct", meas_type = "Duff and Litter",
#                            check = "Duff depths > 99% of depths ever recorded.",
#                            chk_type = "check"))
#
# dt_duff_depth99 <- make_dt(duff_depth99, "Duff depths > 99% of depths ever recorded.")

### Litter over 99% recorded ----
# litter99 <- quantile(duff$LittDep, probs = 0.99, na.rm = T)
#
# litt_depth99 <- duff |> filter(LittDep > litter99)
#
# QC_table <- rbind(QC_table,
#                   QC_check(litt_depth99, tab = "Litter depths over 99pct", meas_type = "Duff and Litter",
#                            check = "Litter depths > 99% of depths ever recorded.",
#                            chk_type = "check"))
#
# dt_litt_depth99 <- make_dt(litt_depth99, "Litter depths > 99% of depths ever recorded.")

### Check duff fuel constant if not listed as Ponderosa Pine ----
# table(duff$DLFuConSt)
# dufffuconst <- duff |> filter(!DLFuConSt %in% "Ponderosa pine" & !is.na(DLFuConSt))
#
# QC_table <- rbind(QC_table,
#                   QC_check(dufffuconst, tab = "DLFuConSt not PP", meas_type = "Duff and Litter",
#                            check = "Duff DLFuConSt records that are not Ponderosa pine.",
#                            chk_type = "error"))
#
# dt_dufffuconst <- make_dt(dufffuconst, "Duff DLFuConSt records that are not Ponderosa pine.
#                          Check taken from FFI_QAQC_UserGuide_20130930.docx.")

### Check number of SampLocs = 19 ----
# duff_samp <- duff |> filter(!is.na(Transect)) |>
#   select(MacroPlot_Name, Unit_Name, SampleEvent_Date, year, Transect, SampLoc) |>
#   distinct() |>
#   group_by(MacroPlot_Name, Unit_Name, SampleEvent_Date, year, Transect) |>
#   summarize(num_samps = sum(!is.na(SampLoc)), .groups = 'drop') |>
#   filter(num_samps != 10)
#
# QC_table <- rbind(QC_table,
#                   QC_check(duff_samp, tab = "Sample Locations not 10", meas_type = "Duff and Litter",
#                            check = "Number of unique duff SampLoc does not equal 10.",
#                            chk_type = 'error'))
#
# kbl_duff_samp <- make_kable(duff_samp, "Number of unique duff SampLoc does not equal 10.")

# check if duff checks returned at least 1 record to determine whether to include tab
# duff_check <- QC_table |> filter(Type %in% "Duff and Litter" & Num_Records > 0)
# duff_include <- tab_include(duff_check)

## Cover Spp Comp/Target Species ----
# KEEP THIS SECTION LAST

covspp1 <- getCoverSpeciesComp(years = year_range,
                              purpose = "NGPN_PCM") |>
  mutate(year = as.numeric(year))

covspp <- bind_rows(covspp1 |>
                      filter(!Unit_Name == "THRO") |>
                      semi_join(panel_sch,
                                by = c("MacroPlot_Purpose" = "Panel",
                                       "year" = "Year")),
                    covspp1 |>
                      filter(Unit_Name == "THRO") |>
                      semi_join(panel_sch_thro,
                                by = c("MacroPlot_Purpose" = "Panel",
                                       "year" = "Year"))) |>
  select(MacroPlot_Name,
         Unit_Name,
         SampleEvent_Date,
         year,
         month,
         SaComment,
         Cover,
         UV1,
         Symbol,
         ScientificName,
         CommonName,
         Nativity,
         Invasive,
         Cultural,
         Concern,
         LifeCycle,
         LifeForm_Name)

### Target species lists by park ----
inv_targ <- covspp |>
  filter(Invasive == TRUE)

QC_table <- rbind(QC_table,
                  QC_check(df = inv_targ,
                           tab = "Invasive Species",
                           meas_type = "Target Species Detections",
                           check = "Target invasive species detections = TRUE",
                           chk_type = "check"))

dt_inv_targ <- make_dt(inv_targ, cap = "Target invasive species detections")

### Invasive = FALSE ----
oth_targ <- covspp |>
  filter(Invasive == FALSE)

QC_table <- rbind(QC_table,
                  QC_check(df = oth_targ,
                           tab = "Other Species",
                           meas_type = "Target Species Detections",
                           check = "Target species detections that aren't classified as invasive (Invasive = FALSE)",
                           chk_type = "check"))

dt_oth_targ <- make_dt(oth_targ,
                       cap = "Target species detections that aren't classified as invasive")

# check if Cover Species Composition checks returned at least 1 record to determine whether to include tab
targ_check <- QC_table |>
  filter(Type %in% "Target Species Detections" & Num_Records > 0)

targ_include <- tab_include(targ_check)

# Compile final QC Table ----
# revise for different color combos for checks (99 vs 90)? Drop for checks vs. errors?
QC_cap <- "The table below documents Quality Control checks performed on NGPN
Plant Community Monitoring data that are stored in the FFI database. This report
primarily checks data that are entered annually, compared with the 'MacroPlot
and SampleEvent checks' report, which checks data that once fixed, are unlikely
to produce errors again. If records are returned for a given check, the row is
highlighted yellow for errors and blue for records that aren't necessarily
errors, but need further review (e.g., large DBH measurements). A separate tab
corresponding to each check that returned results by protocol module (e.g.
Point Intercept, Quadrats, etc.) is printed to the right of this tab. Only
MacroPlots with a MacroPlot_Purpose with Panel or ForestStructure in the name,
or plots with RCM in the name are included in this check."

QC_check_table <- kable(QC_table,
                        format = 'html',
                        align = 'c',
                        caption = QC_cap,
                        col.names = c("Type",
                                      "Data Tab",
                                      "Check Description",
                                      "Number of Records", "Check Type")) |>
  kable_styling(fixed_thead = TRUE,
                bootstrap_options = c('condensed'),
                full_width = TRUE,
                position = 'left',
                font_size = 12) |>
  row_spec(0,
           extra_css = "border-top: 1px solid #000000; border-bottom: 1px solid #000000;") |>
  column_spec(3,
              width = "300px") |>
  column_spec(2:ncol(QC_table),
              background =
                ifelse(QC_table$Num_Records > 0 & QC_table$check_type == "error",
                       "#F2F2A0",
                       ifelse(QC_table$Num_Records > 0 & QC_table$check_type == "check",
                              "#b7d8ef",
                              "#ffffff"))) |>
  collapse_rows(1,
                valign = 'top') |>
  row_spec(nrow(QC_table),
           extra_css = 'border-bottom: 1px solid #000000;')



