
rm(list=ls())
library(tidyverse)

source('fun/f_FormatDatatotalForModels.R')

dati <- readRDS('data/data_for_models/total_data_entered.RDS')
dat  <- format_data_tot(dati)

# Guarda los datos en RDS
saveRDS(dat, 'data/data_for_models/clean_data_total.RDS') 

# Estos datos incluyen las siguientes variables, por si necesitan mapear o hacer uso de esas variables relevantes:
# survey, total, counts, age_mean_f, tsur, country,
# test, antibody, setting, pop_sample, published,
# loc_type, ADM1, ADM2, ADM3, lat_dec, long_dec, gen_sample,
# pathogen,sexual_debut_percent, 
# age_min, age_max, source_type, year_init, year_end, prev_obs,
# prev_obs_lower, pre_obs_upper, n_ages, sample_size, quality


##--Para los modelos filtrar así, de acuerdo a lo que necesita serofoi:

dat0 <- dat %>%
  select (survey, country, ADM1, ADM2, setting, 
          sexual_debut_percent,sexual_debut_age_under,
          tsur, age_min, age_max, age_mean_f,
          total, counts, prev_obs, prev_obs_lower, prev_obs_upper,
          pathogen,risk_class,test, antibody, int_vaccine, gender_sample, 
          citation, quality_NC) # agregar la variable "calidad_otawa_nc"




rownames(dat0) <- NULL

# Guarda los datos finales para los modelos en RDS
saveRDS(dat0, 'data/data_for_models/clean_data_total_models.RDS') 
