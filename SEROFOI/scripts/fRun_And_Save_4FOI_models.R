


run_and_save_models <- function(dat, survey_name) {
  
  dat0 <- dat %>% filter(
    survey == survey_name) %>% 
    select(survey, tsur, age_min, age_max, counts, total) %>%
    rename(n_seropositive = counts,
           sample_size = total)
  
  # # --------------------  Modelo 1.
  
  time_constant_no_seroreversion <- fit_seromodel(
    serosurvey = dat0,
    model_type = "constant",
    foi_prior = sf_uniform(),
    is_seroreversion = TRUE,
    seroreversion_prior = sf_uniform(0.0, 2.0)
  )
  
  model1_summary <- summarise_seromodel(
    seromodel = time_constant_no_seroreversion,
    serosurvey = dat0 ) %>% as.data.frame()
  
  # -------------------- Modelo 2.
  
  foi_index <- get_foi_index(
    serosurvey = dat0,
    group_size = 10
  )
  init <- function() {
    list(foi_vector = rep(0.01, max(dat0$age_max)))
  }
  time_varying_no_seroreversion <- fit_seromodel(
    serosurvey = dat0,
    model_type = "time",
    foi_prior = sf_normal(),
    is_seroreversion = FALSE,
    init = init,
    iter = 5000,
    thin = 2
  )
  
  model2_summary <- summarise_seromodel(
    seromodel = time_varying_no_seroreversion,
    serosurvey = dat0 ) %>% as.data.frame()
  
  
  #   --------------------  Modelo 3
  
  foi_index <- c(
    rep(1, 15),
    16:max(dat0$age_max)
  )
  init <- function() {
    list(foi_vector = rep(0.01, max(foi_index)))
  }
  
  age_no_seroreversion <- fit_seromodel(
    serosurvey = dat0,
    model_type = "age",
    foi_prior = sf_normal(1e-4, 1e-5),
    foi_index = foi_index,
    is_seroreversion = FALSE,
    init = init,
    iter = 5000,
    thin = 2
  )
  
  model3_summary <- summarise_seromodel(
    seromodel = age_no_seroreversion,
    serosurvey = dat0 ) %>% as.data.frame()
  
  
  # #   --------------------  Modelo 4
  
  foi_index <- c(
    rep(1, 15),
    16:max(dat0$age_max)
  )
  
  init <- function() {
    list(foi_vector = rep(0.01, max(foi_index)))
  }
  
  age_seroreversion <- fit_seromodel(
    serosurvey = dat0,
    model_type = "age",
    foi_prior = sf_normal(1e-4, 1e-5),
    foi_index = foi_index,
    is_seroreversion = TRUE,
    seroreversion_prior = sf_normal(1, 0.5),
    iter = 5000,
    thin = 2
  )
  
  model4_summary <- summarise_seromodel(
    seromodel = age_seroreversion,
    serosurvey = dat0 ) %>% as.data.frame()
  
  

  
  list_outputs <- list(dat0= dat0, 
                       survey_name = survey_name,
                       model1 = time_constant_no_seroreversion,
                       model2 = time_varying_no_seroreversion,
                       model3 = age_no_seroreversion,
                       model4 = age_seroreversion,
                       model1_summary = model1_summary,
                       model2_summary = model2_summary,
                       model3_summary = model3_summary,
                       model4_summary = model4_summary)
  
  name_pathogen <- unique(dat0$pathogen)
  
  name_file <- paste0("SEROFOI/results_RDS/", survey_name, "_", name_pathogen,".RDS")
  saveRDS(list_outputs, name_file)
  
  print(paste("finished____",  name_file))
  
}




