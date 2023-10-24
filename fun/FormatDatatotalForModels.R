format_data_tot <- function (dat)
{
  
  dati <- dat0
  dati$test[dati$test=='NA'] <- '.'
  dati <- dati %>%
    mutate(year_init  = as.numeric(year_init),
           year_end   = as.numeric(year_end),
           age_min    = as.numeric(age_min),
           age_max    = as.numeric(age_max),
           year_pub   = as.numeric(year_pub)) 
  dati$age_max[is.na(dati$age_max)] <- dati$age_min[is.na(dati$age_max)]
  dati$age_min[is.na(dati$age_min)] <- dati$age_max[is.na(dati$age_min)]
  dati$year_end[is.na(dati$year_end)] <- dati$year_init[is.na(dati$year_end)]
  dati$year_init[is.na(dati$year_end)] <- dati$year_end[is.na(dati$year_init)]
  dati$source_ss[dati$source_ss < 10] <- paste0(0, dati$source_ss[dati$source_ss < 10])
  
  dat <- dati %>% 
      mutate(counts = pos, 
             survey = paste0(survey, '-', source_ss),
             setting = area_type,
             age_mean_f = floor((age_min + age_max)/2),
             tsur = floor((year_init + year_end)/2),
             country = country_iso3,
             antibody = 'IgG',
             test = paste(test1, test2)) %>%
      mutate(birth = NA) %>%
      mutate(survey = str_replace(survey, "_search_", "-")) %>%
      mutate(survey = str_replace(survey, "_search_", "-")) 
  
  
  # Esto es en tal caso que se ingresen los datos en español
  dat$setting[dat$setting=='rural'] <- 'rural'
  dat$setting[dat$setting=='urbano'] <- 'urban'
  dat$setting[dat$setting=='periurbano'] <- 'periurban'
  dat$setting[dat$setting=='mixto'] <- 'mixed'
  dat$setting[dat$setting=='NA'] <- 'no data'
  
  
  dat %>% group_by(setting) %>% summarise(number = n())
  
  dat$age_mean_f [dat$age_mean_f == 0] <- 1
  dat$tsur[is.na(dat$tsur)] <- dat$year_pub[is.na(dat$tsur)]
  
  problems <- which(dat$pop_type == 'pregnant' & (dat$age_mean_f <12 | dat$age_mean_f > 55)) 
  
  if (length(problems>0)) {
    print('Carolina!, hay un problema con la edad de las maternas')
  }
  
  dat <- filter(dat, !is.na(counts))
  dat <- filter(dat, !is.na(total))
  
  
  dat$published <- dat$source_type
  dat$published[dat$published=='Publicado'] <- 'published'
  
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



read_and_bind_data <- function(dat)
  
{
  
  dat <- dat %>% mutate(dataset_id = paste0(source_id, '-', source_ss))
  
  ### --------- Repeat metadata  for same paper source
  
  new_dat1 <- data.frame()
  (unique_papers <- unique(dat$source_id))
  
  for (j in unique_papers)
  {
    
    temp_dat <- filter(dat, source_id == j)
    temp_dat[,3:15] <- temp_dat[1,3:15]
  for (k in unique(temp_dat$survey))
  {
    
    # Aquí hay que hacer el mismo proceso pero para cada survey con las columnas
    # equivalentes a 16 hasta antes de ns por edad (pendiente revisar bien una vez se mofifique el excel)
    temp_dat[,16:15] <- temp_dat[1,3:15]
    
    
  }
    new_dat1 <- rbind(new_dat1, temp_dat)
    rm(temp_dat)
  }
  
  
  
  ### --------- Repeat metadata  for same survey
  unique_datasets <- unique(dat$dataset_id)
  new_dat2 <- data.frame()
  
  for (i in unique_datasets)
    
  {
    
    temp_dat2 <- filter(new_dat1, dataset_id == i)
    temp_dat2[,13:52] <- temp_dat2[1,13:52]
    new_dat2 <- rbind(new_dat2, temp_dat2)
    rm(temp_dat2)
    
  }
  
  
  
  ### --------- Return completed data
  return(new_dat2)
  
}

