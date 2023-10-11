## ---- Data Wrangling Data Published

rm(list= ls())

library(tidyverse)
library(readxl)

source('fun/f_FormatDatatotalForModels.R')

#------------------------------
#---------Datos seroprevalencia HPV global----------
#------------------------------
dati <- read_excel("data/raw_data/HPV-seroprev-extraction-2023-10-09.xlsx")
unique(dati$paper_id)
length(unique(dati$paper_id))
unique(dati$survey_id)
length(unique(dati$survey_id))
unique(dati$survey_cons)
length(unique(dati$survey_cons))

# Ejecutar la función de lectura y organización de datos
datif <- read_and_bind_data(dati)


#-------------------------
#------CONSOLIDADO--------
#-------------------------

# Revisar el total de surveys y el número de age classes
surveys <- datif %>% group_by(country_iso3, survey) %>% dplyr::summarise(n_ages = n())

# Guardar los datos en RDS
saveRDS(datif, "data/data_for_models/total_data_entered.RDS")

