##################
# file name: main_file.R
# created by: Anne Meyer
# created on: 2024-01-20
##################

## packages
library(ggplot2)
library(tidyverse)
library(scales)
library(plotly)
library(flextable)
library(freedom)
library(ggrepel)
library(readxl)
library(openxlsx)
library(meta)
library(sf)
library(officer)

rm(list=ls())
set.seed(123)
setwd(choose.dir()) # choose "feedlot_model" directory location
dir.create("outputs")
dir.create("figures")

# Load model code
source("code/1_ahle_model_growout_v6.R")

## Run model for each scenario
scenario_file <- "data/example_params_feedlot.xlsx"
source("code/2_run_model.R")

## Process outputs and create tables and plots
source("code/3_process_ahle_data.R")
