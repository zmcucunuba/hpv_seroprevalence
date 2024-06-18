


run_and_save_models <- function(dat, survey_name, output_excel_file = "summary_models.xlsx"){
  
  dat0 <- dat %>% filter(
    survey == survey_name) %>% 
    select(survey, tsur, age_min, age_max, counts, total) %>%
    rename(n_seropositive = counts,
           sample_size = total)
  
  # # --------------------  Modelo 1.
  
  time_constant_no_seroreversion <- fit_seromodel(
    serosurvey = dat0,
    model_type = "constant",
    foi_prior = sf_uniform()
     )
  
  model1_summary <- summarise_seromodel(
    seromodel = time_constant_no_seroreversion,
    serosurvey = dat0 ) %>% as.data.frame()
   model1_summary$model <- "model1" 
   

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
    iter = 5500,
    thin = 2
  )
  
  model2_summary <- summarise_seromodel(
    seromodel = time_varying_no_seroreversion,
    serosurvey = dat0 ) %>% as.data.frame()
  model2_summary$model <- "model2"
  
  
  
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
  model3_summary$model <- "model3"
  
  
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
  model4_summary$model <- "model4"
  
  
  # combinar resumen de data frame
  
  df_summary <- dplyr::bind_rows(
    model1_summary,
    model2_summary,
    model3_summary,
    model4_summary
  )
  

  write.xlsx(df_summary, output_excel_file)
  

  
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
  
  write.xlsx(df_summary, paste0("SEROFOI/summary_models/", survey_name, "_dat0.xlsx"))
  
  print(paste("SEROFOI/summary_models/", output_excel_file))
  

  
  print(paste("finished____",  name_file))
  
}




