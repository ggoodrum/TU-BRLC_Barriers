# Title: Input data processing
# Author: Greg Goodrum
# Last update: 8/6/2025
# Contact: goodrum.greg@gmail.com
# Description: Processing code to prepare input data for estimating connectivity for 
#              TU/BRLC barrier removal assessments

# NOTES:

# References:

# --------------------------------------------------------------------- #
# 00. Set up workspace
# ---------------------------------------------------------------------

# Summary: Set up workspace, load data, and load relevant packages.

# Clean workspace
rm(list = ls())

# Load Packages
# Data/stats
if(!require("rstudioapi")){
  install.packages("rstudioapi", dependencies = TRUE); library(rstudioapi)}
if(!require("tidyverse")){
  install.packages("tidyverse", dependencies = TRUE); library(tidyverse)}
if(!require("rio")){
  install.packages("rio", dependencies = TRUE); library(rio)}
if(!require("lubridate")){
  install.packages("lubridate", dependencies = TRUE); library(lubridate)}

# Plotting

# Spatial

# Plotting

# Declare working directory
pwd <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(pwd)

# ---------------------------------------------------------------------

# --------------------------------------------------------------------- #
# 01. Barrier Data (currently only Bear Lake)
# ---------------------------------------------------------------------

# Summary: Combine NABD and TU barrier data into single dataset for analysis.
# NABD = National Aquatic Barrier Dataset (https://www.aquaticbarriers.org)
# NOTE: Currently preliminary TU dataset that only includes Bear Lake barriers

# Declare data
file.data.tu <- paste0(getwd(), '/TU/TU_BearLake.xlsx')
file.data.nabd <- paste0(getwd(), '/NABD/NABD_SnakeBear.csv')

# Load data
data.tu <- import_list(file.data.tu)
data.nabd <- read.csv(file = file.data.nabd, header = TRUE)

# ---------------------------------------------------------------------------- #

# Format Data - NABD
data.nabd.join <- data.nabd %>%
  select(Name,BarrierType,
         River,
         lat, lon,
         SARPID,
         Passability,
         Removed,
         YearRemoved) %>%
  rename(BarrierName = Name,
         StreamName = River,
         Lat = lat,
         Lon = lon,
         SourceID = SARPID,
         Mitigated = Removed,
         YearMitigated = YearRemoved) %>%
  mutate(Source = 'National Aquatic Barrier Dataset (SARP)',
         Pass_NABD = Passability) %>%
  # Passability from ratings
  mutate(Pass_Before = ifelse(Pass_NABD == 'Complete barrier', 0.0,
                              ifelse(Pass_NABD == 'Unknown', NA,
                                     ifelse(Pass_NABD %in% c('Partial passability', 'Partial passability - salmonid'),
                                            0.5, 1.0)))) %>%
  # Passability unknown - estimated from barrier type
  mutate(Pass_Before = ifelse(Pass_NABD == 'Unknown' & BarrierType == 'Dam', 0,
                              ifelse(Pass_NABD == 'Unknown' & BarrierType == 'Assessed road-related barrier',
                                     0.5, Pass_Before))) %>%
  mutate(Pass_After = Pass_Before) %>%
  select(BarrierName, BarrierType, StreamName, Source, SourceID,
         Lat, Lon,
         Mitigated, YearMitigated, Pass_NABD, Pass_Before, Pass_After) %>%
  as.data.frame

# ---------------------------------------------------------------------------- #

# NOTE: Currently ignoring diversion screens, not sure these should be included in connectivity assessment.

# Format TU Data - Culverts
data.tu.culverts <- data.tu[['Culverts']] %>%
  rename(BarrierName = `Culvert/Road Name`,
         StreamName = `Waterway Name`,
         Lat = `Culvert LAT`,
         Lon = `Culvert LONG`,
         YearMitigated = `Year Replaced`,
         Pass_Before = `Culvert Passage before`,
         Pass_After = `Culvert Passage after`) %>%
  mutate(BarrierType = 'Culvert',
         Source = 'Trout Unlimited (TU)',
         SourceID = paste0('TU_CUL_', CUL_ID),
         Mitigated = 'yes',
         Pass_NABD = NA) %>%
  select(BarrierName, BarrierType, StreamName, Source, SourceID,
         Lat, Lon,
         Mitigated, YearMitigated, Pass_NABD, Pass_Before, Pass_After) %>%
  as.data.frame

# Format TU Data - Diversions
data.tu.diversions <- data.tu[['Diversions']] %>%
  rename(BarrierName = `Div Name`,
         StreamName = `Waterway Name`,
         Lat = `Div LAT`,
         Lon = `Div LONG`,
         Mitigated = `Div struct Rebuild     (Y/N)`,
         YearMitigated = `Div Rebuild Year      (YYYY)`,
         Pass_Before = `Diversion Passability Before`,
         Pass_After = `Diversion Passability After`) %>%
  mutate(BarrierType = 'Diversion',
         Source = 'Trout Unlimited (TU)',
         SourceID = paste0('TU_DIV_', `Div #`),
         Pass_NABD = NA,
         Pass_After = as.numeric(Pass_After)) %>%
  mutate(YearMitigated = as.numeric(YearMitigated)) %>%
  select(BarrierName, BarrierType, StreamName, Source, SourceID,
         Lat, Lon,
         Mitigated, YearMitigated, Pass_NABD, Pass_Before, Pass_After) %>%
  as.data.frame

# ---------------------------------------------------------------------------- #

# Combine and export datasets
data.barriers <- do.call(rbind, list(data.nabd.join, data.tu.culverts, data.tu.diversions))

# ---------------------------------------------------------------------------- #

# Write output
write.csv(data.barriers, file = 'Barriers_TU_NADB.csv', row.names = FALSE)
# ---------------------------------------------------------------------