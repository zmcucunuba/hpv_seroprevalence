library(tidyverse)
rm(list=ls())

dat <- readRDS('data/data_for_models/clean_data_total_models.RDS') 
dat$survey_adm1 <- paste0(dat$survey, "_", dat$ADM1)

survey_ages <- dat %>% group_by(survey) %>% summarise(n = n())
survey_ages_mult <- survey_ages %>% filter(n>1)
dat_mult_ages <- dat %>% filter(survey %in% survey_ages_mult$survey)

# Only High Quality (HQ) Studies
dat_Qhigh <- filter(dat_mult_ages, quality_NC == "high")
dat_Qhigh$country_year <- paste(dat_Qhigh$country, dat_Qhigh$tsur)

##====== HQ + HPV16 + Female
hpv16_hqF <- dat_Qhigh %>% filter (pathogen == "HPV 16") %>% filter(gender_sample== "female")
ggplot(data = hpv16_hqF) + # data
  geom_line(aes(x = age_mean_f, y = prev_obs)) + # aesthesic 
  geom_point(aes(x = age_mean_f, y = prev_obs, fill = gender_sample), size = 2, pch = 21) + 
  geom_errorbar(aes(x = age_mean_f, ymin = prev_obs_lower, ymax = prev_obs_upper)) +
  facet_wrap(country_year ~ survey_adm1 ) + 
  labs(title = " 16_HQ")  +
  coord_cartesian(xlim = c(0, 65), ylim = c(0,1)) 

##====== HQ + HPV18 + Female
hpv18_hqF <- dat_Qhigh %>% filter (pathogen == "HPV 18") %>% filter(gender_sample== "female")
ggplot(data = hpv18_hqF) + # dat
  geom_line(aes(x = age_mean_f, y = prev_obs)) + # aesthesic 
  geom_point(aes(x = age_mean_f, y = prev_obs, fill = gender_sample), size = 2, pch = 21) + 
  geom_errorbar(aes(x = age_mean_f, ymin = prev_obs_lower, ymax = prev_obs_upper)) +
  facet_wrap(country_year ~ survey_adm1 ) + 
  labs(title = "HPV 18_HQ") +
  coord_cartesian(xlim = c(0, 65), ylim = c(0,1)) 


unique(dat_Qhigh$survey)
unique(hpv16_hqF$survey)
unique(hpv18_hqF$survey)


colour_hpv16 <- c("#8dd3c7", "#bebada", "#ffffb3", "#fb8072", 
                  "#80b1d3", "#fdb462", "#b3de69")
p_hpv16 <- 
  ggplot(data = hpv16_hqF) + 
  geom_line(aes(x = age_mean_f, y = prev_obs, colour = country_year), size = 1) + # aesthesic 
  geom_errorbar(aes(x = age_mean_f, ymin = prev_obs_lower, ymax = prev_obs_upper, colour = country_year), width = 0) +
  geom_point(aes(x = age_mean_f, y = prev_obs, fill = country_year, size = total),pch = 21, colour = "black") + 
  theme_light() + 
  scale_y_log10() +
  coord_cartesian(ylim = c(0.001, 1)) +
  scale_x_continuous(breaks = seq(0, 65, by = 5), limits = c(0, 70)) +
  scale_fill_manual(values = colour_hpv16) +
  scale_colour_manual(values = colour_hpv16) +
  labs(title = "HPV 16s", x = "Age", y = "Seoprevalence") +
  theme(legend.position = "none")


colour_hpv18 <- c( "#bebada", "#ffffb3","#b3de69")
p_hpv18 <- 
  ggplot(data = hpv18_hqF) + 
  geom_line(aes(x = age_mean_f, y = prev_obs, colour = country_year), size = 1) + # aesthesic 
  geom_errorbar(aes(x = age_mean_f, ymin = prev_obs_lower, ymax = prev_obs_upper, colour = country_year), width = 0) +
  geom_point(aes(x = age_mean_f, y = prev_obs, fill = country_year, size = total),pch = 21, colour = "black") + 
  theme_light() + 
  scale_y_log10() +
  coord_cartesian(ylim = c(0.001, 1)) +
  scale_x_continuous(breaks = seq(0, 65, by = 5), limits = c(5, 65)) +
  scale_fill_manual(values = colour_hpv18) +
  scale_colour_manual(values = colour_hpv18) +
  labs(title = "HPV 18", x = "Age", y = "Seoprevalence") +
  theme(legend.position = "none")


hpv_16.18_hq <- dat_Qhigh %>% filter(pathogen == "HPV 16/18")
colour_hpv16.18 <- c( "#bebada", "grey30")
p_hpv16.18 <- 
  ggplot(data = hpv_16.18_hq) + 
  geom_line(aes(x = age_mean_f, y = prev_obs, colour = country_year), size = 1) + # aesthesic 
  geom_errorbar(aes(x = age_mean_f, ymin = prev_obs_lower, ymax = prev_obs_upper, colour = country_year), width = 0) +
  geom_point(aes(x = age_mean_f, y = prev_obs, fill = country_year, size = total),pch = 21, colour = "black") + 
  theme_light() + 
  scale_y_log10() +
  coord_cartesian(ylim = c(0.001, 1)) +
  scale_x_continuous(breaks = seq(0, 65, by = 5), limits = c(5, 65)) +
  scale_fill_manual(values = colour_hpv16.18) +
  scale_colour_manual(values = colour_hpv16.18) +
  labs(title = "HPV 16 or 18", x = "Age", y = "Seoprevalence")+
  theme(legend.position = "none")


cowplot::plot_grid(p_hpv16, p_hpv18, p_hpv16.18, nrow = 1)




colour_hpv_all <- c("#8dd3c7", "#bebada", "#ffffb3", "#fb8072", 
                    "#80b1d3", "#fdb462", "#b3de69", "grey50")
hpvHQ_all <- rbind(hpv16_hqF, hpv18_hqF, hpv_16.18_hq)
hpvHQ_all$pathogen <- factor(hpvHQ_all$pathogen, 
                             levels = unique(hpvHQ_all$pathogen))
ggplot(data = hpvHQ_all) + 
  geom_line(aes(x = age_mean_f, y = prev_obs, colour = country_year), size = 1) + # aesthesic 
  geom_errorbar(aes(x = age_mean_f, ymin = prev_obs_lower, ymax = prev_obs_upper, colour = country_year), width = 0) +
  geom_point(aes(x = age_mean_f, y = prev_obs, fill = country_year, size = total),pch = 21, colour = "black") + 
  # geom_point(aes(x = age_mean_f, y = sexual_young_prop),pch = 22, colour = "black", fill = "red") + 
  theme_linedraw() + 
  scale_y_log10() +
  coord_cartesian(ylim = c(0.001, 1)) +
  scale_x_continuous(breaks = seq(0, 65, by = 10), limits = c(5, 65)) +
  scale_fill_manual(values = colour_hpv_all) +
  scale_colour_manual(values = colour_hpv_all) +
  facet_wrap(~  pathogen) + 
  labs(title = "Estudios seroprevalencia VPH-IgG de alta calidad", x = "Edad", 
       y = "Seroprevalencia", size = "Tamaño muestra", 
       fill = "Country, year", colour = "Country, year")


hpvHQ_all$sexual_young_prop  <- as.numeric(hpvHQ_all$sexual_debut_percent) /100
ggplot(data = hpvHQ_all) + 
  geom_line(aes(x = age_mean_f, y = prev_obs, colour = country_year), size = 1) + # aesthesic 
  geom_errorbar(aes(x = age_mean_f, ymin = prev_obs_lower, ymax = prev_obs_upper, colour = country_year), width = 0) +
  geom_point(aes(x = age_mean_f, y = prev_obs, fill = country_year, size = total),pch = 21, colour = "black") + 
  theme_linedraw() + 
  scale_y_log10() +
  coord_cartesian(ylim = c(0.001, 1)) +
  scale_x_continuous(breaks = seq(0, 65, by = 10), limits = c(5, 65)) +
  scale_fill_manual(values = colour_hpv_all) +
  scale_colour_manual(values = colour_hpv_all) +
  facet_wrap( country_year ~  pathogen) + 
  labs(title = "Estudios seroprevalencia VPH-IgG de alta calidad", x = "Edad", 
       y = "Seroprevalencia", size = "Sample size", 
       fill = "Country, year", colour = "Country, year")



# Plot todos los surveys multiages con fill risk_class

## Plot Base

ggplot(data = dat_mult_ages) + # data
  geom_line(aes(x = age_mean_f, y = prev_obs)) + # aesthesic 
  geom_point(aes(x = age_mean_f, y = prev_obs, fill = risk_class), size = 2, pch = 21) + 
  facet_wrap(~ survey)

#Plot modificado respecto a los otros del descriptivo

colour_hpv_risk <- c("#8dd3c7","#b3de69", "#fb8072")
                   

ggplot(data = dat_mult_ages) + # data
  geom_line(aes(x = age_mean_f, y = prev_obs)) + # aesthesic 
  geom_point(aes(x = age_mean_f, y = prev_obs, fill = risk_class), size = 2, pch = 21, colour = "black") + 
  theme_linedraw() + 
    scale_x_continuous(breaks = seq(0, 65, by = 10), limits = c(5, 65)) +
  scale_fill_manual(values = colour_hpv_risk) +
  scale_colour_manual(values = colour_hpv_risk) +
  facet_wrap(~ survey) +
  labs(title = "Seroprevalencia VPH-IgG", x = "Edad", 
       y = "Seroprevalencia",size = "Sample size", 
       fill = "Riesgo VPH")






