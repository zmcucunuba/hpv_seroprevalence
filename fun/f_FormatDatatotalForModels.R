format_data_tot <- function (dat0)
{
  
  dati <- dat0
  
  dati$test[dati$test=='NA'] <- '.'
  dati$age_max[is.na(dati$age_max)] <- dati$age_min[is.na(dati$age_max)] # Reemplaza NA en edad máxima con el valor de la edad mínima
  dati$age_min[is.na(dati$age_min)] <- dati$age_max[is.na(dati$age_min)] # Reemplaza NA en edad máxima con el valor de la edad mínima
  dati$year_end[is.na(dati$year_end)] <- dati$year_init[is.na(dati$year_end)] # Idem para año final
  dati$year_init[is.na(dati$year_end)] <- dati$year_end[is.na(dati$year_init)] # Idem para año inicial
  
  
  dat <- dati %>% 
    mutate(counts = pos, 
           setting = area_type,
           age_mean_f = floor((age_min + age_max)/2),
           tsur = floor((year_init + year_end)/2),
           country = country_iso3,
           antibody = 'IgG') %>%
    mutate(birth = NA) %>%
    mutate(survey = str_replace(survey_id, "_search_", "-"))
  
  
  # Esto es en tal caso que se ingresen los datos en español
 
  dat$setting[dat$setting=='NA'] <- 'no data'
  
  
  dat %>% group_by(setting) %>% summarise(number = n())
  
  dat$age_mean_f [dat$age_mean_f == 0] <- 1
  dat$tsur[is.na(dat$tsur)] <- dat$year_pub[is.na(dat$tsur)]
  
  
  dat <- filter(dat, !is.na(counts))
  dat <- filter(dat, !is.na(total))
  
  
  dat$published <- dat$source_type
 
  
  # unique(dat$published)
  
  dat <- dat %>%
    select (survey, total, counts, age_mean_f, tsur, country,
            test, antibody, setting, published,
            loc_type, ADM1, ADM2, ADM3, lat_dec, long_dec, 
            pathogen, gender_sample,	pop_sample, sexual_debut_percent,sexual_debut_age_under,int_vaccine,
            age_min, age_max, source_type, year_init, year_end, citation)
  
  
  conf <- data.frame(Hmisc::binconf(dat$counts, dat$total,method="exact"))
  dat  <- cbind(dat, conf) %>%
    rename (prev_obs = PointEst,
            prev_obs_lower = Lower,
            prev_obs_upper = Upper
    )
  
  
  dat <- filter(dat, age_mean_f <81)
  
  dat$n_ages <- NA
  datasets <- unique(as.character(dat$survey))
  for (i in datasets)
  {
    dat$n_ages[dat$survey==i] <- length(dat$survey[dat$survey==i]) 
    dat$sample_size[dat$survey==i] <- sum(dat$total[dat$survey==i])
  }
  
  # remueve los datos con una observación 
  dat_one_age_class <-  filter(dat, n_ages <= 1) 
  
  datf <- filter(dat, sample_size > 29) 
  
  
  return(datf)
  
}

