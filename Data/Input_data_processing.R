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