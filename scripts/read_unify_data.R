## ---- Data Wrangling Data Published

rm(list= ls())

library(tidyverse)
library(readxl)

source('fun/FormatDatatotalForModels.R')

#------------------------------
#---------Datos seroprevalencia HPV global----------
#------------------------------
dati <- read_excel("data/raw_data/HPV-seroprev-extraction-2023-10-09.xlsx")
unique(dati$source_id)
length(unique(dati$source_id))
unique(dati$source_ss)
length(unique(dati$source_ss))
unique(dati$survey)
length(unique(dati$survey))

# Ejecutar la función de lectura y organización de datos

datif <- read_and_bind_data(dati)




#-------------------------
#------CONSOLIDADO--------
#-------------------------

# Revisar el total de surveys y el número de age classes
surveys <- datif %>% group_by(country_iso3, survey) %>% dplyr::summarise(n_ages = n())

# Guardar los datos en RDS
saveRDS(datif, "data/data_for_models/total_data_entered.RDS")

