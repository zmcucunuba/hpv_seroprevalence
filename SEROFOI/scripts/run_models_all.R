# SEROFOI #
# library(pak)
# pak::pkg_install("epiverse-trace/serofoi@full-refac-test")


library(serofoi)
library(openxlsx)
library(dplyr)
library(ggplot2)
options(mc.cores=4)

source("SEROFOI/scripts/fRun_And_Save_4FOI_models.R")

dat <- readRDS('data/data_for_models/clean_data_total_models.RDS')
surveys_of_interest <- c("CRI-009-01", "CRI-009-02", "COL_024_02", 
                         "COL_024_03", "COL_024_04", "BRA-017-01",
                         "PRI-001-02", "USA-011-03", "USA-026-04", 
                         "USA-026-03", "TWN-025-01", "NGA-002-01")
dat %>% filter(survey %in% surveys_of_interest)

run_and_save_models(dat, survey_name = "COL_024_02")
run_and_save_models(dat, survey_name = "COL_024_02")
run_and_save_models(dat, survey_name = "COL_024_02")
run_and_save_models(dat, survey_name = "COL_024_02")
run_and_save_models(dat, survey_name = "COL_024_02")
run_and_save_models(dat, survey_name = "COL_024_02")
run_and_save_models(dat, survey_name = "COL_024_02")
run_and_save_models(dat, survey_name = "COL_024_02")
run_and_save_models(dat, survey_name = "COL_024_02")



