library(tidyverse)
rm(list=ls())

dat <- readRDS('data/data_for_models/clean_data_total_models.RDS') 
dat$survey_adm1 <- paste0(dat$survey, "_", dat$ADM1)

survey_ages <- dat %>% group_by(survey) %>% summarise(n = n())
survey_ages_mult <- survey_ages %>% filter(n>1)

dat_mult_ages <- dat %>% filter(survey %in% survey_ages_mult$survey)

# Plot todos los surveys multiage
ggplot(data = dat_mult_ages) + # data
  geom_line(aes(x = age_mean_f, y = prev_obs)) + # aesthesic 
  geom_point(aes(x = age_mean_f, y = prev_obs, ), size = 2, pch = 21, fill = "red") + 
  facet_wrap(~ survey)
  
# Plot todos los surveys multiage con fill patogeno
ggplot(data = dat_mult_ages) + # data
  geom_line(aes(x = age_mean_f, y = prev_obs)) + # aesthesic 
  geom_point(aes(x = age_mean_f, y = prev_obs, fill = pathogen), size = 2, pch = 21) + 
  facet_wrap(~ survey)

# Plot todos los surveys multiages con fill risk_class

ggplot(data = dat_mult_ages) + # data
  geom_line(aes(x = age_mean_f, y = prev_obs)) + # aesthesic 
  geom_point(aes(x = age_mean_f, y = prev_obs, fill = risk_class), size = 2, pch = 21) + 
  facet_wrap(~ survey)

# Plot todos los surveys multiages con fill risk_class, quality 

ggplot(data = dat_mult_ages) + # data
  geom_line(aes(x = age_mean_f, y = prev_obs)) + # aesthesic 
  geom_point(aes(x = age_mean_f, y = prev_obs, fill = quality_NC), size = 2, pch = 21) + 
  facet_wrap(~ survey)
# Plot todos lso surveys high quality on fill risk_class

dat_quality_study_high <- filter(dat_mult_ages, quality_NC == "high")

ggplot(data = dat_quality_study_high) + # data
  geom_line(aes(x = age_mean_f, y = prev_obs)) + # aesthesic 
  geom_point(aes(x = age_mean_f, y = prev_obs, fill = risk_class), size = 2, pch = 21) + 
  facet_wrap(~ survey)


# Plot todos los surveys multiage con fill patogeno para un unico país
ggplot(data = dat_mult_ages %>% filter (country == "BES", age_mean_f < 65)) + # data
  geom_line(aes(x = age_mean_f, y = prev_obs)) + # aesthesic 
  geom_point(aes(x = age_mean_f, y = prev_obs, fill = pathogen), size = 2, pch = 21) + 
  geom_errorbar(aes(x = age_mean_f, ymin = prev_obs_lower, ymax = prev_obs_upper)) +
  facet_wrap(pathogen ~ survey_adm1 ) +
  coord_cartesian(xlim = c(0, 65))

ggplot(data = dat_mult_ages %>% filter (country == "COL", age_mean_f < 65)) + # data
  geom_line(aes(x = age_mean_f, y = prev_obs)) + # aesthesic 
  geom_point(aes(x = age_mean_f, y = prev_obs, fill = pathogen), size = 2, pch = 21) + 
  geom_errorbar(aes(x = age_mean_f, ymin = prev_obs_lower, ymax = prev_obs_upper)) +
  facet_wrap(pathogen ~ survey_adm1 ) +
  coord_cartesian(xlim = c(0, 65))


ggplot(data = dat_mult_ages %>% filter (pathogen == "HPV 16")) + # data
  geom_line(aes(x = age_mean_f, y = prev_obs)) + # aesthesic 
  geom_point(aes(x = age_mean_f, y = prev_obs, fill = pathogen), size = 2, pch = 21) + 
  geom_errorbar(aes(x = age_mean_f, ymin = prev_obs_lower, ymax = prev_obs_upper)) +
  facet_wrap(country ~ survey_adm1 ) 

ggplot(data = dat_mult_ages %>% filter (pathogen == "HPV 18")) + # data
  geom_line(aes(x = age_mean_f, y = prev_obs)) + # aesthesic 
  geom_point(aes(x = age_mean_f, y = prev_obs, fill = pathogen), size = 2, pch = 21) + 
  geom_errorbar(aes(x = age_mean_f, ymin = prev_obs_lower, ymax = prev_obs_upper)) +
  facet_wrap(country ~ survey_adm1 ) 


ggplot(data = dat_mult_ages %>% filter (pathogen == "HPV")) + # data
  geom_line(aes(x = age_mean_f, y = prev_obs)) + # aesthesic 
  geom_point(aes(x = age_mean_f, y = prev_obs, fill = pathogen), size = 2, pch = 21) + 
  geom_errorbar(aes(x = age_mean_f, ymin = prev_obs_lower, ymax = prev_obs_upper)) +
  facet_wrap(country ~ survey_adm1 ) 



