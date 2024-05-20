library(tidyverse)
rm(list=ls())

dat <- readRDS('data/data_for_models/clean_data_total_models.RDS') 
dat$survey_adm1 <- paste0(dat$survey, "_", dat$ADM1)

survey_ages <- dat %>% group_by(survey) %>% summarise(n = n())
survey_ages_mult <- survey_ages %>% filter(n>1)

dat_mult_ages <- dat %>% filter(survey %in% survey_ages_mult$survey)

## crear colimna para año y pais##

dat_mult_ages$country_year <- paste(dat_mult_ages$country, dat_mult_ages$tsur) 



## DESCRIPTIVO, TODAS LAS SEROENECUESTAS CON MAS DE UN GRUPO DE EDAD DIFRENCIADAS POR EL RIESGO DE VPH ## 

x <- ggplot(data = dat_mult_ages) + # data
  geom_line(aes(x = age_mean_f, y = prev_obs)) + # aesthesic 
  geom_point(aes(x = age_mean_f, y = prev_obs, fill = risk_class), size = 2, pch = 21) + 
  facet_wrap(~ survey)+
  theme_linedraw() +
  labs( x = "Edad (años)", 
        y = "Seroprevalencia", fill = "Riesgo VPH")


##### ggsave(filename = "Descriptivo/plots/grafico_alta_total.png", plot = x, dpi = 400, width = 16, height = 9)

png(filename = "Descriptivo/plots/grafico_allHVP3G.png", 
    width = 480*2, height = 480*2) 
x
dev.off()


## DESCRIPTIVO, SEROENCUESTAS VPH 16-18-16/18 , SOLO EN MUJERES Y ALTA CALIDAD ###

### HPV 16 ##

hpv16_hqF <- dat_mult_ages %>% filter (pathogen == "HPV 16") %>% filter(gender_sample== "female")%>%
            filter(quality_NC == "high")


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
  labs(title = "VPH 16", x = "Edad", y = "Seroprevalencia") +
  theme(legend.position = "none")
 


 #### HPV 18 ##
hpv18_hqF <- dat_mult_ages %>% filter (pathogen == "HPV 18") %>% filter(gender_sample== "female")%>%
  filter(quality_NC == "high")


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
  labs(title = "VPH 18", x = "Edad", y = "Seroprevalencia") +
  theme(legend.position = "none")



 ### HPV 16/18 ##

hpv16.18_hqF <- dat_mult_ages %>% filter (pathogen == "HPV 16/18") %>% filter(gender_sample== "female")%>%
  filter(quality_NC == "high")

colour_hpv16.18 <- c( "#bebada", "grey30")

p_hpv16.18 <- 
  ggplot(data = hpv16.18_hqF) + 
  geom_line(aes(x = age_mean_f, y = prev_obs, colour = country_year), size = 1) + # aesthesic 
  geom_errorbar(aes(x = age_mean_f, ymin = prev_obs_lower, ymax = prev_obs_upper, colour = country_year), width = 0) +
  geom_point(aes(x = age_mean_f, y = prev_obs, fill = country_year, size = total),pch = 21, colour = "black") + 
  theme_light() + 
  scale_y_log10() +
  coord_cartesian(ylim = c(0.001, 1)) +
  scale_x_continuous(breaks = seq(0, 65, by = 5), limits = c(5, 65)) +
  scale_fill_manual(values = colour_hpv16.18) +
  scale_colour_manual(values = colour_hpv16.18) +
  labs(title = "VPH 16 or 18", x = "Edad", y = "Seroprevalencia")+
  theme(legend.position = "none")

#cowplot::plot_grid(p_hpv16, p_hpv18, p_hpv16.18, nrow = 1)


colour_hpv_all <- c("#8dd3c7", "#bebada", "#ffffb3", "#fb8072", 
                    "#80b1d3", "#fdb462", "#b3de69", "grey50")

hpvHQ_all <- rbind(hpv16_hqF, hpv18_hqF, hpv16.18_hqF)
hpvHQ_all$pathogen <- factor(hpvHQ_all$pathogen, 
                             levels = unique(hpvHQ_all$pathogen))

### GRAFICA FINAL EN PANELES INDIVIDUALES ##

p <- ggplot(data = hpvHQ_all) + 
  geom_line(aes(x = age_mean_f, y = prev_obs, colour = country_year), size = 1) + 
  geom_errorbar(aes(x = age_mean_f, ymin = prev_obs_lower, ymax = prev_obs_upper, colour = country_year), width = 0) +
  geom_point(aes(x = age_mean_f, y = prev_obs, fill = country_year, size = total), pch = 21, colour = "black") + 
  theme_linedraw(20) + 
  scale_y_log10() +
  coord_cartesian(ylim = c(0.001, 1)) +
  scale_x_continuous(breaks = seq(0, 65, by = 10), limits = c(5, 65)) +
  scale_fill_manual(values = colour_hpv_all) +
  scale_colour_manual(values = colour_hpv_all) +
  facet_wrap(country_year ~ pathogen) + 
  labs(x = "Edad (años)", 
       y = "Seroprevalencia", 
       size = "", 
       fill = "", 
       colour = ""
       # size = "Tamaño de muestra", 
       # fill = "País,año", 
       # colour = "País,año"
       )




jpeg(filename = "Descriptivo/plots/HPVALLPI.jpeg", width = 480*2, height = 480*2) 
p
dev.off()


## GRAFICA FINAL PANELES AGRUPADOS##

y <- ggplot(data = hpvHQ_all) + 
  geom_line(aes(x = age_mean_f, y = prev_obs, colour = country_year), size = 1) + 
  geom_errorbar(aes(x = age_mean_f, ymin = prev_obs_lower, ymax = prev_obs_upper, colour = country_year), width = 0) +
  geom_point(aes(x = age_mean_f, y = prev_obs, fill = country_year, size = total), pch = 21, colour = "black") + 
  theme_linedraw() + 
  scale_y_log10() +
  coord_cartesian(ylim = c(0.001, 1)) +
  scale_x_continuous(breaks = seq(0, 65, by = 10), limits = c(5, 65)) +
  scale_fill_manual(values = colour_hpv_all) +
  scale_colour_manual(values = colour_hpv_all) +
  facet_wrap(~ pathogen) + 
  labs(x = "Edad", 
       y = "Seroprevalencia", 
       size = "Tamaño de muestra", 
       fill = "País, año", 
       colour = "País, año") +
  theme(
    axis.title.x = element_text(size = 16),  # Ajusta el tamaño del título del eje x
    axis.title.y = element_text(size = 16),  # Ajusta el tamaño del título del eje y
    axis.text.x = element_text(size = 16),   # Ajusta el tamaño del texto de los ejes x
    axis.text.y = element_text(size = 16),   # Ajusta el tamaño del texto de los ejes y
    legend.title = element_text(size = 16),  # Ajusta el tamaño del título de la leyenda
    legend.text = element_text(size = 16),   # Ajusta el tamaño del texto de la leyenda
    strip.text.x = element_text(size = 18) )   # Ajusta el tamaño del texto de las facetas en el eje x




jpeg(filename = "Descriptivo/plots/grafico_allHVP3G.jpeg", width = 480*2, height = 480*2) 
y
dev.off()


