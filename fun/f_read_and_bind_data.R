

# Funcion leer y pegar datos
read_and_bind_data <- function(dat){
  
  dat$survey_cons[dat$survey_cons < 10] <- paste0("0", dat$survey_cons[dat$survey_cons < 10])
  dat <- dat %>% mutate(survey_id_check = paste0(paper_id, '-', survey_cons))
  check <- dat %>% select(survey_id_check, survey_id)
  dat <- dat %>% mutate(survey_id = survey_id_check)
  ### --------- Repeat metadata  for same paper source
  
  binded_data_paper <- data.frame()
  unique_papers <- unique(dat$paper_id)
  for (j in unique_papers)
  {
    temp_dat <- filter(dat, paper_id == j)
    
    vars_papers <- c ("citation","source_type", "authors", "data_provider","year_pub", "year_rec_pub", "year_report", 
                      "n_ss_extracted", "lang" )
    temp_dat[,vars_papers] <- temp_dat[1,vars_papers]
    binded_data_paper <- rbind(binded_data_paper, temp_dat)
    rm(temp_dat)
  }
  
  dat <- binded_data_paper
  
  binded_surveys <- data.frame()
  unique_surveys <- unique(dat$survey_id)
  
  for (k in unique_surveys)
  {
    temp_dat <- filter(dat, survey_id == k)
    vars_surveys <- c(
      "year_init", "year_end", "loc_details", 
      "country_iso3", "ADM1", "ADM2",
      "ADM3", "loc_type", "loc_name_given",
      "loc_name_found", "lat_dec", "long_dec",
      "latlong_source", "lat_deg", "lat_min",
      "lat_sec", "long_deg", "long_min",
      "long_sec", "loc_notes", "area_type",
      "gender_sample", "pop_sample", "other_type_sample",
      "note_pop", "sex_f_percent", "sample",
      "setting_notes", "test", "antibody",
      "n_pos_for_pos", "specific_tests_for_pos", "diag_notes",
      "int_vaccine", "vaccine_details", "pathogen_art", "pathogen",
      "int_other", "int_notes", "sexual_debut_percent",
      "sexual_debut_age_under"
    )
    temp_dat[,vars_surveys] <- temp_dat[1,vars_surveys]
    binded_surveys <- rbind(binded_surveys, temp_dat)
    rm(temp_dat)
  }
  
  dat <- binded_surveys
  ### --------- Return completed data
  return(dat)
  
}

