## ---- Data Wrangling Data Published

rm(list= ls())

library(tidyverse)
library(readxl)

source('fun/FormatDatatotalForModels.R')

#------------------------------
#---------Datos seroprevalencia HPV global----------
#------------------------------
dati <- read_excel("data/raw_data/HPV-seroprev-extraction-2023-09-15.xlsx")
unique(dati$source_id)
length(unique(dati$source_id))
unique(dati$source_ss)
length(unique(dati$source_ss))
datif <- read_and_bind_data(dati)

# Esto debe repetirse para cada país

#-------------------------
#------CONSOLIDADO--------
#-------------------------
tot_data <- rbind(datif)  # Aquí se debe incluir todos los países que se vayan extrayendo. Ej: MEXf, COLf, ECUf, etc..

# Esto es sólo una tabla descriptiva que muestra cuántas serosurveys se ha extraído por cada país
surveys <- tot_data %>% group_by(country_iso3, dataset_id) %>% dplyr::summarise(n_surveys = n())

surveys_total <- surveys %>% group_by(country_iso3) %>% 
  dplyr::summarise(dataset_id = n())

# Guardar los datos en RDS
saveRDS(tot_data, "data/data_for_models/total_data_entered.RDS")

