## ---- Data Wrangling Data Published

rm(list= ls())

library(tidyverse)
library(readxl)

source('fun/f_read_and_bind_data.R')

#------------------------------
#---------Datos seroprevalencia HPV global----------
#------------------------------
dati <- read_excel("data/raw_data/HPV-seroprev-extraction-2023-11-01.xlsx")
unique(dati$paper_id)
length(unique(dati$paper_id))
unique(dati$survey_id)
length(unique(dati$survey_id))
unique(dati$survey_cons)
length(unique(dati$survey_cons))

# cambiar character a nueric en age_min, age_max##

dati$age_min <- as.numeric(dati$age_min)
dati$age_max <- as.numeric(dati$age_max)
dati$prev_obs <- as.numeric(dati$prev_obs)
dati$lat_dec <- as.numeric(dati$lat_dec)
sdati$long_dec <- as.numeric(dati$long_dec)

# Ejecutar la función de lectura y organización de datos
datif <- read_and_bind_data(dati)

# Revisar el total de surveys y el número de age classes

surveys <- datif %>% group_by(country_iso3, survey_id,pathogen) %>% 
  dplyr::summarise(n_ages = n()) %>% filter(n_ages > 2)

table(surveys$pathogen)


# Guardar los datos en RDS
saveRDS(datif, "data/data_for_models/total_data_entered.RDS")

