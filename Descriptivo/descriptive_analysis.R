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

dat_mult_ages$country_year <- paste(dat_mult_ages$country, dat_mult_ages$tsur)

ggplot(data = dat_mult_ages) + # data
  geom_line(aes(x = age_mean_f, y = prev_obs)) + # aesthesic 
  geom_point(aes(x = age_mean_f, y = prev_obs, fill = quality_NC), size = 2, pch = 21) + 
  facet_wrap(~ survey) +
  

ggplot(data = dat_mult_ages) + # data
  geom_line(aes(x = age_mean_f, y = prev_obs)) + # aesthesic 
  geom_errorbar(aes(x = age_mean_f, ymin = prev_obs_lower, ymax = prev_obs_upper, colour = country_year), width = 0) +
  geom_point(aes(x = age_mean_f, y = prev_obs, fill = quality_NC), size = 2, pch = 21) + 
  facet_wrap(~ survey) +
 


# Plot todos loS surveys high quality on fill risk_class

dat_Qhigh <- filter(dat_mult_ages, quality_NC == "high")

ggplot(data = dat_Qhigh) + # data
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

ggplot(data = dat_mult_ages %>% filter (country == "USA", age_mean_f < 65)) + # data
  geom_line(aes(x = age_mean_f, y = prev_obs)) + # aesthesic 
  geom_point(aes(x = age_mean_f, y = prev_obs, fill = pathogen), size = 2, pch = 21) + 
  geom_errorbar(aes(x = age_mean_f, ymin = prev_obs_lower, ymax = prev_obs_upper)) +
  facet_wrap(pathogen ~ survey_adm1 ) +
  coord_cartesian(xlim = c(0, 65))

ggplot(data = dat_mult_ages %>% filter (country == "PRI", age_mean_f < 65)) + # data
  geom_line(aes(x = age_mean_f, y = prev_obs)) + # aesthesic 
  geom_point(aes(x = age_mean_f, y = prev_obs, fill = pathogen), size = 2, pch = 21) + 
  geom_errorbar(aes(x = age_mean_f, ymin = prev_obs_lower, ymax = prev_obs_upper)) +
  facet_wrap(pathogen ~ survey_adm1 ) +
  coord_cartesian(xlim = c(0, 65), ylim = c(0,1)) 

ggplot(data = dat_mult_ages %>% filter (country == "NGA", age_mean_f < 65)) + # data
  geom_line(aes(x = age_mean_f, y = prev_obs)) + # aesthesic 
  geom_point(aes(x = age_mean_f, y = prev_obs, fill = pathogen), size = 2, pch = 21) + 
  geom_errorbar(aes(x = age_mean_f, ymin = prev_obs_lower, ymax = prev_obs_upper)) +
  facet_wrap(pathogen ~ survey_adm1 ) +
  coord_cartesian(xlim = c(0, 65))

ggplot(data = dat_mult_ages %>% filter (country == "CRI", age_mean_f < 65)) + # data
  geom_line(aes(x = age_mean_f, y = prev_obs)) + # aesthesic 
  geom_point(aes(x = age_mean_f, y = prev_obs, fill = pathogen), size = 2, pch = 21) + 
  geom_errorbar(aes(x = age_mean_f, ymin = prev_obs_lower, ymax = prev_obs_upper)) +
  facet_wrap(pathogen ~ survey_adm1 ) +
  coord_cartesian(xlim = c(0, 65))


ggplot(data = dat_mult_ages %>% filter (country == "BRA", age_mean_f < 65)) + # data
  geom_line(aes(x = age_mean_f, y = prev_obs)) + # aesthesic 
  geom_point(aes(x = age_mean_f, y = prev_obs, fill = pathogen), size = 2, pch = 21) + 
  geom_errorbar(aes(x = age_mean_f, ymin = prev_obs_lower, ymax = prev_obs_upper)) +
  facet_wrap(pathogen ~ survey_adm1 ) +
  coord_cartesian(xlim = c(0, 65))


ggplot(data = dat_mult_ages %>% filter (country == "IRN", age_mean_f < 65)) + # data
  geom_line(aes(x = age_mean_f, y = prev_obs)) + # aesthesic 
  geom_point(aes(x = age_mean_f, y = prev_obs, fill = pathogen), size = 2, pch = 21) + 
  geom_errorbar(aes(x = age_mean_f, ymin = prev_obs_lower, ymax = prev_obs_upper)) +
  facet_wrap(pathogen ~ survey_adm1 ) +
  coord_cartesian(xlim = c(0, 65))

#Plot por patogeno 

ggplot(data = dat_Qhigh %>% filter (pathogen == "HPV 16")) + # data
  geom_line(aes(x = age_mean_f, y = prev_obs)) + # aesthesic 
  geom_point(aes(x = age_mean_f, y = prev_obs, fill = pathogen), size = 2, pch = 21) + 
  geom_errorbar(aes(x = age_mean_f, ymin = prev_obs_lower, ymax = prev_obs_upper)) +
  facet_wrap(country ~ survey_adm1 ) +
  coord_cartesian(xlim = c(0, 65), ylim = c(0,1)) 

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

ggplot(data = dat_mult_ages %>% filter (pathogen == "HPV 16/18")) + # data
  geom_line(aes(x = age_mean_f, y = prev_obs)) + # aesthesic 
  geom_point(aes(x = age_mean_f, y = prev_obs, fill = pathogen), size = 2, pch = 21) + 
  geom_errorbar(aes(x = age_mean_f, ymin = prev_obs_lower, ymax = prev_obs_upper)) +
  facet_wrap(country ~ survey_adm1 ) 

# Plot HPV16 alta calidad#
dat_Qhigh <- filter(dat_mult_ages, quality_NC == "high")

dat_Qhigh$country_year <- paste(dat_Qhigh$country, dat_Qhigh$tsur)
hpv16_hq <- dat_Qhigh %>% filter (pathogen == "HPV 16") %>% filter(gender_sample== "female")

# plot panel VPH 16 HQ -  FEMALE 

ggplot(data = hpv16_hq) + # data
  geom_line(aes(x = age_mean_f, y = prev_obs)) + # aesthesic 
  geom_point(aes(x = age_mean_f, y = prev_obs, fill = gender_sample), size = 2, pch = 21) + 
  geom_errorbar(aes(x = age_mean_f, ymin = prev_obs_lower, ymax = prev_obs_upper)) +
  facet_wrap(country_year ~ survey_adm1 ) + 
  labs(title = "HPV 16_HQ")  +
  coord_cartesian(xlim = c(0, 65), ylim = c(0,1)) 

hpv18_hq <- dat_Qhigh %>% filter (pathogen == "HPV 18") %>% filter(gender_sample== "female")


hpv_16.18_hq <- dat_Qhigh %>% filter(pathogen == "HPV 16/18")

# plot panel VPH 18 HQ -  FEMALE 

ggplot(data = hpv18_hq) + # dat
  geom_line(aes(x = age_mean_f, y = prev_obs)) + # aesthesic 
  geom_point(aes(x = age_mean_f, y = prev_obs, fill = gender_sample), size = 2, pch = 21) + 
  geom_errorbar(aes(x = age_mean_f, ymin = prev_obs_lower, ymax = prev_obs_upper)) +
  facet_wrap(country_year ~ survey_adm1 ) + 
  labs(title = "HPV 18_HQ") +
  coord_cartesian(xlim = c(0, 65), ylim = c(0,1)) 
  

unique(dat_Qhigh$survey)
unique(hpv16_hq$survey)

## plot Alto riesgo BES - No aplica hombres y mujeres

ggplot((data = hpv18_hq)  %>% filter (country == "BES", age_mean_f < 65)) + # data
  geom_line(aes(x = age_mean_f, y = prev_obs)) + # aesthesic 
  geom_point(aes(x = age_mean_f, y = prev_obs, fill = gender_sample), size = 2, pch = 21) + 
  geom_errorbar(aes(x = age_mean_f, ymin = prev_obs_lower, ymax = prev_obs_upper)) +
  facet_wrap(country_year ~ survey_adm1 ) + 
  labs(title = "HPV 18_HQ BES")

ggplot((data = hpv16_hq)  %>% filter (country == "BES", age_mean_f < 65)) + # data
  geom_line(aes(x = age_mean_f, y = prev_obs)) + # aesthesic 
  geom_point(aes(x = age_mean_f, y = prev_obs, fill = gender_sample), size = 2, pch = 21) + 
  geom_errorbar(aes(x = age_mean_f, ymin = prev_obs_lower, ymax = prev_obs_upper)) +
  facet_wrap(country_year ~ survey_adm1 ) + 
  labs(title = "HPV 16_HQ BES")

## plot Alto riesgo PRI  -  HPV 16 - NO aplica es hombres y mujeres

ggplot((data = hpv16_hq)  %>% filter (country == "PRI", age_mean_f < 65)) + # data
  geom_line(aes(x = age_mean_f, y = prev_obs)) + # aesthesic 
  geom_point(aes(x = age_mean_f, y = prev_obs, fill = gender_sample), size = 2, pch = 21) + 
  geom_errorbar(aes(x = age_mean_f, ymin = prev_obs_lower, ymax = prev_obs_upper)) +
  facet_wrap(country_year ~ survey_adm1 ) + 
  labs(title = "HPV16_HQ PRI") +
  coord_cartesian(xlim = c(0, 65), ylim = c(0,1)) 



# Plot Alto riesgo NGA

ggplot((data = hpv16_hq)  %>% filter (country == "NGA", age_mean_f < 65)) + # data
  geom_line(aes(x = age_mean_f, y = prev_obs)) + # aesthesic 
  geom_point(aes(x = age_mean_f, y = prev_obs, fill = gender_sample), size = 2, pch = 21) + 
  geom_errorbar(aes(x = age_mean_f, ymin = prev_obs_lower, ymax = prev_obs_upper)) +
  facet_wrap(country_year ~ survey_adm1 ) + 
  labs(title = "HPV 16_HQ NGA") +
  coord_cartesian(xlim = c(0, 65), ylim = c(0,1)) 


# Plot alto riesgo CRI

ggplot((data = hpv18_hq)  %>% filter (country == "CRI", age_mean_f < 65)) + # data
  geom_line(aes(x = age_mean_f, y = prev_obs)) + # aesthesic 
  geom_point(aes(x = age_mean_f, y = prev_obs, fill = gender_sample), size = 2, pch = 21) + 
  geom_errorbar(aes(x = age_mean_f, ymin = prev_obs_lower, ymax = prev_obs_upper)) +
  facet_wrap(country_year ~ survey_adm1 ) + 
  labs(title = "HPV 18_HQ CRI") +
  coord_cartesian(xlim = c(0, 65), ylim = c(0,1)) 

ggplot((data = hpv16_hq)  %>% filter (country == "CRI", age_mean_f < 65)) + # data
  geom_line(aes(x = age_mean_f, y = prev_obs)) + # aesthesic 
  geom_point(aes(x = age_mean_f, y = prev_obs, fill = gender_sample), size = 2, pch = 21) + 
  geom_errorbar(aes(x = age_mean_f, ymin = prev_obs_lower, ymax = prev_obs_upper)) +
  facet_wrap(country_year ~ survey_adm1 ) + 
  labs(title = "HPV 16_HQ CRI") + 
  coord_cartesian(xlim = c(0, 65), ylim = c(0,1)) 


# Plot alto riesgo USA

ggplot((data = hpv18_hq)  %>% filter (country == "USA", age_mean_f < 65)) + # data
  geom_line(aes(x = age_mean_f, y = prev_obs)) + # aesthesic 
  geom_point(aes(x = age_mean_f, y = prev_obs, fill = gender_sample), size = 2, pch = 21) + 
  geom_errorbar(aes(x = age_mean_f, ymin = prev_obs_lower, ymax = prev_obs_upper)) +
  facet_wrap(country_year ~ survey_adm1 ) + 
  labs(title = "HPV 18_HQ USA") +
  coord_cartesian(xlim = c(0, 65), ylim = c(0,1)) 


ggplot((data = hpv16_hq)  %>% filter (country == "USA", age_mean_f < 65)) + # data
  geom_line(aes(x = age_mean_f, y = prev_obs)) + # aesthesic 
  geom_point(aes(x = age_mean_f, y = prev_obs, fill = gender_sample), size = 2, pch = 21) + 
  geom_errorbar(aes(x = age_mean_f, ymin = prev_obs_lower, ymax = prev_obs_upper)) +
  facet_wrap(country_year ~ survey_adm1 ) + 
  labs(title = "HPV 16_HQ USA") +
coord_cartesian(xlim = c(0, 65), ylim = c(0,1)) 

# Plot alto riesgo BRA

ggplot((data = hpv16_hq)  %>% filter (country == "BRA", age_mean_f < 65)) + # data
  geom_line(aes(x = age_mean_f, y = prev_obs)) + # aesthesic 
  geom_point(aes(x = age_mean_f, y = prev_obs, fill = gender_sample), size = 2, pch = 21) + 
  geom_errorbar(aes(x = age_mean_f, ymin = prev_obs_lower, ymax = prev_obs_upper)) +
  facet_wrap(country_year ~ survey_adm1 ) + 
  labs(title = "HPV 16_HQ BRA") +
  coord_cartesian(xlim = c(0, 65), ylim = c(0,1)) 
 

# Plot alto riesgo COL - pendiente definir

ggplot((data = hpv18_hq)  %>% filter (country == "COL", age_mean_f < 65)) + # data
  geom_line(aes(x = age_mean_f, y = prev_obs)) + # aesthesic 
  geom_point(aes(x = age_mean_f, y = prev_obs, fill = gender_sample), size = 2, pch = 21) + 
  geom_errorbar(aes(x = age_mean_f, ymin = prev_obs_lower, ymax = prev_obs_upper)) +
  facet_wrap(country_year ~ survey_adm1 ) + 
  labs(title = "HPV 18_HQ COL") +
coord_cartesian(xlim = c(0, 65), ylim = c(0,1)) 


ggplot((data = hpv16_hq)  %>% filter (country == "COL", age_mean_f < 65)) + # data
  geom_line(aes(x = age_mean_f, y = prev_obs)) + # aesthesic 
  geom_point(aes(x = age_mean_f, y = prev_obs, fill = gender_sample), size = 2, pch = 21) + 
  geom_errorbar(aes(x = age_mean_f, ymin = prev_obs_lower, ymax = prev_obs_upper)) +
  facet_wrap(country_year ~ survey_adm1 ) + 
  labs(title = "HPV 16_HQ COL") +
  coord_cartesian(xlim = c(0, 65), ylim = c(0,1)) 

# quitar
ggplot((data = hpv_16.18_hq)  %>% filter (country == "COL", age_mean_f < 65)) + # data
  geom_line(aes(x = age_mean_f, y = prev_obs)) + # aesthesic 
  geom_point(aes(x = age_mean_f, y = prev_obs, fill = gender_sample), size = 2, pch = 21) + 
  geom_errorbar(aes(x = age_mean_f, ymin = prev_obs_lower, ymax = prev_obs_upper)) +
  facet_wrap(country_year ~ survey_adm1 ) + 
  labs(title = "HPV 16/18_HQ COL")

# Plot alto riesgo TWN

ggplot((data = hpv18_hq)  %>% filter (country == "TWN", age_mean_f < 65)) + # data
  geom_line(aes(x = age_mean_f, y = prev_obs)) + # aesthesic 
  geom_point(aes(x = age_mean_f, y = prev_obs, fill = gender_sample), size = 2, pch = 21) + 
  geom_errorbar(aes(x = age_mean_f, ymin = prev_obs_lower, ymax = prev_obs_upper)) +
  facet_wrap(country_year ~ survey_adm1 ) + 
  labs(title = "HPV 18_HQ TWN") +
  coord_cartesian(xlim = c(0, 65), ylim = c(0,1)) 
  


ggplot((data = hpv16_hq)  %>% filter (country == "TWN", age_mean_f < 65)) + # data
  geom_line(aes(x = age_mean_f, y = prev_obs)) + # aesthesic 
  geom_point(aes(x = age_mean_f, y = prev_obs, fill = gender_sample), size = 2, pch = 21) + 
  geom_errorbar(aes(x = age_mean_f, ymin = prev_obs_lower, ymax = prev_obs_upper)) +
  facet_wrap(country_year ~ survey_adm1 ) + 
  labs(title = "HPV 16_HQ TWN") +
  coord_cartesian(xlim = c(0, 65), ylim = c(0,1)) 





