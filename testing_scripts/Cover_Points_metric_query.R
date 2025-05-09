#-------------------------------------------------------------------
# Scripts to start piecing together the FFI database for NGPN parks
#-------------------------------------------------------------------

library(tidyverse)

source("./testing_scripts/NGPN_FFI_database_tables.R")

importData(park = "BADL")

env <- if(exists("VIEWS_NGPN")){VIEWS_NGPN} else {.GlobalEnv}

sort(names(VIEWS_NGPN))

# Macroplot Table
macroplot1 <- get("MacroPlot", envir = env)
# Drop table name from columns for faster coding
names(macroplot1) <- gsub("MacroPlot_", "", names(macroplot1))
# Select only columns I want and rename columns I want macroplot as start
macroplot <- macroplot1 |> select(MacroPlot_Name = Name, MacroPlot_GUID = GUID, Purpose,
                                  UTM_X, UTM_Y, UTMzone, Elevation, Azimuth, Aspect,
                                  SlopeHill, SlopeTransect)
head(macroplot)

# Sample Event Table
sampev <- get("SampleEvent", envir = env) |>
  select(SampleEvent_GUID, MacroPlot_GUID = SampleEvent_Plot_GUID,
         Date = SampleEvent_Date, DefaultMonitoringStatus = SampleEvent_DefaultMonitoringStatus,
         TreatmentUnit = SampleEvent_TreatmentUnit)

head(sampev)
# Cover Points metric Sample
covpts_samp <- get("Cover_Points_metric_Sample", envir = env) |>
  select(SampleEvent_GUID = SampleData_SampleEvent_GUID, SampleRow_GUID = SampleData_SampleRow_GUID,
         Visited, NumTran, TranLen, NumPtsTran, Offset)

head(covpts_samp)

# Cover Points metric Attribute
covpts_att <- get("Cover_Points_metric_Attribute", envir = env) |>
  select(SampleRow_GUID = AttributeData_SampleRow_GUID, Index,
         Transect, Point, Tape, Order, Height, CanopyLayer, Status, Comment, Spp_GUID)


spp_table <- left_join(VIEWS_NGPN$LocalSpecies, VIEWS_NGPN$MasterSpecies,
                       by = c("LocalSpecies_MasterSpeciesGUID" = "MasterSpecies_GUID")) |>
  select(Spp_GUID = LocalSpecies_GUID, Symbol = LocalSpecies_Symbol, CommonName = LocalSpecies_CommonName,
         LifeCycle = LocalSpecies_LifeCycle, Nativity = LocalSpecies_Nativity, Invasive = LocalSpecies_Invasive,
         Cultural = LocalSpecies_Cultural, Concern = LocalSpecies_Concern, Retired = LocalSpecies_Retired,
         LocSpp_Comment = LocalSpecies_Comment, ScientificName = MasterSpecies_ScientificName, TSN = MasterSpecies_ITIS_TSN,
         Genus = MasterSpecies_Genus, Family = MasterSpecies_Family)





mplot <- full_join(VIEWS_NGPN$MacroPlot, VIEWS_NGPN$SampleEvent,
                  by = c("MacroPlot_GUID" = "SampleEvent_Plot_GUID"))

samp <- full_join(mplot, VIEWS_NGPN$Cover_Points_metric_Sample,
                  by = c("SampleEvent_GUID" = "SampleData_SampleEvent_GUID"))

cover <- full_join(samp, VIEWS_NGPN$Cover_Points_metric_Attribute,
                  by = c("SampleData_SampleRow_GUID" = "AttributeData_SampleRow_GUID"))

head(cover)

covptspp <- left_join(cover, VIEWS_NGPN$LocalSpecies, by = c("Spp_GUID" = "LocalSpecies_GUID"))
covptsp2 <- left_join(covptspp, VIEWS_NGPN$MasterSpecies, by = c("LocalSpecies_MasterSpeciesGUID" = "MasterSpecies_GUID"))

names(covptsp2) <- gsub("MacroPlot_", "", names(covptsp2))
names(covptsp2) <- gsub("SampleEvent_", "", names(covptsp2))
names(covptsp2) <- gsub("LocalSpecies_", "", names(covptsp2))
names(covptsp2) <- gsub("MasterSpecies_", "", names(covptsp2))
names(covptsp2)

cov_points <- covptsp2 |> select(MacroPlot_Name, UTM_X = MacrotPlot_UTM_X, UTM_Y = MacroPlot_UTM_Y, UTMzone = MacroPlot_UTMzone,
                                 Azimuth = MacroPlot_Azimuth, Aspect = MacroPlot_Aspect, SlopeHill = MacroPlot_SlopeHill,
                                 )

# Project$Project_GUID = MM_Project_Protocol$MM_Project_GUID
# Method$Method_GUID = MM_Protocol_Method$MM_Method_GUID
# Protocol$Protocol_GUID = MM_Protocol_Method$MM_Protocol_GUID
# SampleAttributeCode$SampleAttributeCode_SampleAttribute_GUID = SampleAttribute$SampleAtt_GUID
  # Useful to get Forest, Grassland, Shrub, Not assessed
# ProjectUnit$ProjectUnit_RegistrationUnitGUID = MacroPlot$MacroPlot_RegistrationUnit_GUID
  # has project units like ABAM, Park, etc.
# MonitoirngStatus$MonitoringStatus_ProjectUnit_GUID = ProjectUnit$ProjectUnit_GUID
# PlotDescription_metric_Sample$SampleData_SampleEvent_GUID = SampleEvent$SampleEvent_GUID
# PlotDescription_metric_Attribute$AttributeData_SampleRow_GUID =
  # PlotDescription_metric_Sample$SampleData_SampleRow_GUID
# MasterSpecies$MasterSpecies_GUID = LocalSpecies$LocalSpecies_MasterSpeciesGUID

