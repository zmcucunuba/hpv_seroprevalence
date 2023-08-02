## ---- Data Wrangling Data Published

rm(list= ls())

library(tidyverse)
library(readxl)

source('fun/FormatDatatotalForModels.R')

#------------------------------
#---------PUERTO RICO----------
#------------------------------
PRI <- read_excel("data/raw_data/PRI.xlsx")
unique(PRI$source_id)
unique(PRI$source_ss)
PRIf <- read_and_bind_data(PRI)

# Esto debe repetirse para cada país

#-------------------------
#------CONSOLIDADO--------
#-------------------------
tot_data <- rbind(PRIf)  # Aquí se debe incluir todos los países que se vayan extrayendo. Ej: MEXf, COLf, ECUf, etc..

# Esto es sólo una tabla descriptiva que muestra cuántas serosurveys se ha extraído por cada país
surveys <- tot_data %>% group_by(country_iso3, dataset_id) %>% dplyr::summarise(n_surveys = n())

surveys_total <- surveys %>% group_by(country_iso3) %>% 
  dplyr::summarise(dataset_id = n())

# Guardar los datos en RDS
saveRDS(tot_data, "data/data_for_models/total_data_entered.RDS")

