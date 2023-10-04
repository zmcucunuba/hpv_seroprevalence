## ---- Data Wrangling Data Published

rm(list= ls())

library(tidyverse)
library(readxl)

source('fun/FormatDatatotalForModels.R')

#------------------------------
#---------Datos seroprevalencia HPV global----------
#------------------------------
dati <- read_excel("data/raw_data/HPV-seroprev-extraction-2023-09-29.xlsx")
unique(dati$source_id)
length(unique(dati$source_id))
unique(dati$source_ss)
length(unique(dati$source_ss))
unique(dati$survey)
length(unique(dati$survey))
datif <- read_and_bind_data(dati)



#-------------------------
#------CONSOLIDADO--------
#-------------------------

# Esto es sólo una tabla descriptiva que muestra cuántas serosurveys se ha extraído por cada país
surveys <- datif %>% group_by(country_iso3, survey) %>% dplyr::summarise(n_ages = n())

surveys_total <- surveys %>% group_by(country_iso3) %>% 
  dplyr::summarise(dataset_id = n())

# Guardar los datos en RDS
saveRDS(tot_data, "data/data_for_models/total_data_entered.RDS")

