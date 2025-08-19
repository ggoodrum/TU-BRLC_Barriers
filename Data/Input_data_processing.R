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
# 01. Barrier Data
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
         Pass_Before = Passability,
         Pass_After = Passability) %>%
  select(BarrierName, BarrierType, StreamName, Source, SourceID,
         Lat, Lon,
         Mitigated, YearMitigated, Pass_Before, Pass_After) %>%
  as.data.frame

# ---------------------------------------------------------------------