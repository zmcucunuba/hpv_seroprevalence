
rm(list=ls())
library(tidyverse)

source('fun/FormatDatatotalForModels.R')

dat0 <- readRDS('data/data_for_models/total_data_entered.RDS')
dat  <- format_data_tot(dat0)

# Guarda los datos en RDS
saveRDS(dat, 'data/data_for_models/clean_data_total.RDS') 

# Estos datos incluyen las siguientes variables, por si necesitan mapear o hacer uso de esas variables relevantes:
# survey, total, counts, age_mean_f, tsur, country,
# test, antibody, setting, pop_sample, published,
# loc_type, ADM1, ADM2, ADM3, lat_dec, long_dec, gen_sample,
# pathogen,sexual_debut_percent, 
# age_min, age_max, source_type, year_init, year_end, prev_obs,
# prev_obs_lower, pre_obs_upper, n_ages, sample_size


##--Para los modelos filtrar así, de acuerdo a lo que necesita serofoi:

dat <- dat %>%
  select (survey, country, ADM1, ADM2,  setting, 
          pop_type, gender_sampled, sexual_debut_percent_by15,
          tsur,
          age_min, age_max, age_mean_f,
          total, counts, prev_obs, prev_obs_lower, prev_obs_upper,
          pathogen, test, antibody,  int_vaccine, gender_sampled, pop_type, 
          sexual_debut_percent_by15,
          citation) 



rownames(dat) <- NULL

# Guarda los datos finales para los modelos en RDS
saveRDS(dat, 'data/data_for_models/clean_data_total_models.RDS') 
