# SEROFOI ## pak::pkg_install("epiverse-trace/serofoi@full-refac-test")


library(serofoi)
library(openxlsx)
library(dplyr)
library(ggplot2)
options(mc.cores=4)




## CARGA BASE LIMPIA DE MODELOS ##

dat <- readRDS('data/data_for_models/clean_data_total_models.RDS')



### SEROENCUESTA COLOMBIA VPH 18###

## CREAR DATA.FRAME DE SEROENCUESTA ## 

col_HPV18 <- dat %>% filter(
  country == "COL" & pathogen == "HPV 18"
) %>%
  
## SELECCCION DE DATOS QUE SE REQUIEREN PARA LOS MODELOS ##
 
   select(survey, tsur, age_min, age_max, counts, total) %>%
  rename(
    n_seropositive = counts,
    sample_size = total)

plot_serosurvey(col_HPV18)


# Modelo constante sin seroreversion

col_HPV18_constant_no_seroreversion <- fit_seromodel(
  serosurvey = col_HPV18,
  model_type = "constant",
  foi_prior = sf_uniform(),
  is_seroreversion = TRUE,
  seroreversion_prior = sf_uniform(0.0, 2.0)
)


# Modelo constante con seroreversion

col_HPV18_constant_seroreversion <- fit_seromodel(
  serosurvey = col_HPV18,
  model_type = "constant",
  foi_prior = sf_uniform(),
  is_seroreversion = TRUE,
  seroreversion_prior = sf_uniform(0.0, 2.0)
)


# Modelo dependiente del tiempo sin seroreversion

foi_index <- get_foi_index(
  serosurvey = col_HPV18,
  group_size = 10
)
init <- function() {
  list(foi_vector = rep(0.01, max(col_HPV18$age_max)))
}
col_HPV18_time_no_seroreversion <- fit_seromodel(
  serosurvey = col_HPV18,
  model_type = "time",
  foi_prior = sf_normal(),
  # foi_index = foi_index,
  is_seroreversion = FALSE,
  init = init,
  iter = 5000,
  thin = 2
)

# Modelo dependiente de la edad sin seroreversion

foi_index <- c(
  rep(1, 15),
  16:max(col_HPV18$age_max)
)
init <- function() {
  list(foi_vector = rep(0.01, max(foi_index)))
}

col_HPV18_age_no_seroreversion <- fit_seromodel(
  serosurvey = col_HPV18,
  model_type = "age",
  foi_prior = sf_normal(1e-4, 1e-5),
  foi_index = foi_index,
  is_seroreversion = FALSE,
  init = init,
  iter = 5000,
  thin = 2
)
# Modelo dependiente de la edad con seroreversion

foi_index <- c(
  rep(1, 15),
  16:max(col_HPV18$age_max)
)

init <- function() {
  list(foi_vector = rep(0.01, max(foi_index)))
}

col_HPV18_age_seroreversion <- fit_seromodel(
  serosurvey = col_HPV18,
  model_type = "age",
  foi_prior = sf_normal(1e-4, 1e-5),
  foi_index = foi_index,
  is_seroreversion = TRUE,
  seroreversion_prior = sf_normal(1, 0.5),
  iter = 5000,
  thin = 2
)

# Gráficas de todos los modelos

size_text <- 8
# foi_max <-

## Constante - no seroreversion

col_HPV18_constant_no_seroreversion_plot <- plot_seromodel(
  seromodel = col_HPV18_constant_no_seroreversion,
  serosurvey = col_HPV18,
  size_text = size_text
)
plot(col_HPV18_constant_no_seroreversion_plot)


## Constante - seroreversion

col_HPV18_constant_seroreversion_plot <- plot_seromodel(
  seromodel = col_HPV18_constant_seroreversion,
  serosurvey = col_HPV18,
  size_text = size_text
)
plot(col_HPV18_constant_seroreversion_plot)

## Tiempo - no seroreversion

col_HPV18_time_no_seroreversion_plot <- plot_seromodel(
  seromodel = col_HPV18_time_no_seroreversion,
  serosurvey = col_HPV18,
  size_text = size_text
)
plot(col_HPV18_time_no_seroreversion_plot)


## Edad - no seroreversion

col_HPV18_age_no_seroreversion_plot <- plot_seromodel(
  seromodel = col_HPV18_age_no_seroreversion,
  serosurvey = col_HPV18,
  size_text = size_text
)
plot(col_HPV18_age_no_seroreversion_plot)

## Edad - seroreversion

col_HPV18_age_seroreversion_plot <- plot_seromodel(
  seromodel = col_HPV18_age_seroreversion,
  serosurvey = col_HPV18,
  size_text = size_text
)
plot(col_HPV18_age_seroreversion_plot)


# Gráfica conjunta de todos los modelos


col_HPV18_all_models_plot <- cowplot::plot_grid(
  col_HPV18_constant_no_seroreversion_plot,
  col_HPV18_constant_seroreversion_plot,
  col_HPV18_time_no_seroreversion_plot,
  col_HPV18_age_no_seroreversion_plot,
  col_HPV18_age_seroreversion_plot,
  ncol = 5
)

plot(col_HPV18_all_models_plot)
jpeg(filename = "SEROFOI/plots/col_hpv18_all.jpeg", width = 480*2, height = 480*2) 
col_HPV18_all_models_plot
dev.off()


col_HPV18_all_models_plot <- cowplot::plot_grid(
  col_HPV18_time_no_seroreversion_plot,
  col_HPV18_age_no_seroreversion_plot,
  col_HPV18_age_seroreversion_plot,
  ncol = 3
)
plot(col_HPV18_all_models_plot)
jpeg(filename = "SEROFOI/plots/col_hpv18.jpeg", width = 480*2, height = 480*2) 
col_HPV18_all_models_plot
dev.off()

col_HPV18_constant_models_plot <- cowplot::plot_grid(
  col_HPV18_constant_no_seroreversion_plot,
  col_HPV18_constant_seroreversion_plot,
  ncol = 2)

plot(col_HPV18_constant_models_plot)
jpeg(filename = "SEROFOI/plots/col_hpv18_conts.jpeg", width = 480*2, height = 480*2) 
col_HPV18_constant_models_plot
dev.off()



## Tabla para cada modelo

col_HPV18_constant_no_seroreversion_summary <- summarise_seromodel(
  seromodel = col_HPV18_constant_no_seroreversion,
  serosurvey = col_HPV18
) %>% as.data.frame()

col_HPV18_constant_seroreversion_summary <- summarise_seromodel(
  seromodel = col_HPV18_constant_seroreversion,
  serosurvey = col_HPV18
) %>% as.data.frame()

col_HPV18_time_no_seroreversion_summary <- summarise_seromodel(
  seromodel = col_HPV18_time_no_seroreversion,
  serosurvey = col_HPV18
) %>% as.data.frame()

col_HPV18_age_no_seroreversion_summary <- summarise_seromodel(
  seromodel = col_HPV18_age_no_seroreversion,
  serosurvey = col_HPV18
) %>% as.data.frame()

col_HPV18_age_seroreversion_summary <- summarise_seromodel(
  seromodel = col_HPV18_age_seroreversion,
  serosurvey = col_HPV18
) %>% as.data.frame()

## Tabla de todos los modelos
col_HPV18_summary <- dplyr::bind_rows(
  col_HPV18_constant_no_seroreversion_summary,
  col_HPV18_constant_seroreversion_summary,
  col_HPV18_time_no_seroreversion_summary,
  col_HPV18_age_no_seroreversion_summary,
  col_HPV18_age_seroreversion_summary
)

write.xlsx(col_HPV18_summary, file = "SEROFOI/data frame/col_HPV18_summary.xlsx")

# Gráfica de seroprevalencia
size_text <- 8
## Constante - no seroreversion

col_HPV18_constant_no_seroreversion_plot_seroprev <- plot_seroprevalence_estimates(
  seromodel = col_HPV18_constant_no_seroreversion,
  serosurvey = col_HPV18,
  size_text = size_text
) +
  ggtitle("constant - no seroreversion")

## Constante - seroreversion

col_HPV18_constant_seroreversion_plot_seroprev <- plot_seroprevalence_estimates(
  seromodel = col_HPV18_constant_seroreversion,
  serosurvey = col_HPV18,
  size_text = size_text
) +
  ggtitle("constant - seroreversion")

## Tiempo - no seroreversion

col_HPV18_time_no_seroreversion_plot_seroprev <- plot_seroprevalence_estimates(
  seromodel = col_HPV18_time_no_seroreversion,
  serosurvey = col_HPV18,
  size_text = size_text
) +
  ggtitle("time - no seroreversion")

## Edad - no seroreversion

col_HPV18_age_no_seroreversion_plot_seroprev <- plot_seroprevalence_estimates(
  seromodel = col_HPV18_age_no_seroreversion,
  serosurvey = col_HPV18,
  size_text = size_text
) +
  ggtitle("age - no seroreversion")

## Edad - seroreversion

col_HPV18_age_seroreversion_plot_seroprev <- plot_seroprevalence_estimates(
  seromodel = col_HPV18_age_seroreversion,
  serosurvey = col_HPV18,
  size_text = size_text
)+
  ggtitle("age - seroreversion")

## Grafica de seroprevalencia conjunta

col_HPV18_plot_seroprev <- cowplot::plot_grid(
  col_HPV18_constant_no_seroreversion_plot_seroprev,
  col_HPV18_constant_seroreversion_plot_seroprev,
  col_HPV18_time_no_seroreversion_plot_seroprev,
  col_HPV18_age_no_seroreversion_plot_seroprev,
  col_HPV18_age_seroreversion_plot_seroprev,
  nrow = 1,
  align = "hv"
)

# Gráfica de foi
foi_max <- 0.3
## Tiempo - no seroreversion

col_HPV18_time_no_seroreversion_plot_foi <- plot_foi_estimates(
  seromodel = col_HPV18_time_no_seroreversion,
  serosurvey = col_HPV18,
  size_text = size_text,
  foi_max = foi_max
 )

## Edad - no seroreversion

col_HPV18_age_no_seroreversion_plot_foi <- plot_foi_estimates(
  seromodel = col_HPV18_age_no_seroreversion,
  serosurvey = col_HPV18,
  size_text = size_text,
  foi_max = foi_max
)

## Edad - seroreversion

col_HPV18_age_seroreversion_plot_foi <- plot_foi_estimates(
  seromodel = col_HPV18_age_seroreversion,
  serosurvey = col_HPV18,
  size_text = size_text,
  foi_max = foi_max
)

## Grafica conjunta foi

empty_plot <- ggplot() + theme_void()

col_HPV18_plot_foi <- cowplot::plot_grid(
  empty_plot,
  empty_plot,
  col_HPV18_time_no_seroreversion_plot_foi,
  col_HPV18_age_no_seroreversion_plot_foi,
  col_HPV18_age_seroreversion_plot_foi,
  ncol = 5,
  nrow = 1,
  align = "hv"
)

# Gráfica de rhats

## Tiempo - no seroreversion

col_HPV18_time_no_seroreversion_plot_rhats <- serofoi:::plot_rhats(
  seromodel = col_HPV18_time_no_seroreversion,
  serosurvey = col_HPV18,
  size_text = size_text
)

## Edad - no seroreversion

col_HPV18_age_no_seroreversion_plot_rhats <- serofoi:::plot_rhats(
  seromodel = col_HPV18_age_no_seroreversion,
  serosurvey = col_HPV18,
  size_text = size_text
)

## Edad - seroreversion

col_HPV18_age_seroreversion_plot_rhats <- serofoi:::plot_rhats(
  seromodel = col_HPV18_age_seroreversion,
  serosurvey = col_HPV18,
  size_text = size_text
)

## Grafica conjunta foi

empty_plot <- ggplot() + theme_void()

col_HPV18_plot_rhats <- cowplot::plot_grid(
  empty_plot,
  empty_plot,
  col_HPV18_time_no_seroreversion_plot_rhats,
  col_HPV18_age_no_seroreversion_plot_rhats,
  col_HPV18_age_seroreversion_plot_rhats,
  ncol = 5,
  nrow = 1,
  align = "hv"
)



# Gráfica conjunta col HPV18

col_HPV18_plot <- cowplot::plot_grid(
  col_HPV18_plot_seroprev,
  col_HPV18_plot_foi,
  col_HPV18_plot_rhats,
  nrow = 3,
  align = "hv"
)

plot(col_HPV18_plot)
jpeg(filename = "SEROFOI/plots/all_col_hpv18_.jpeg", width = 11, height = 8, units = "in", res = 300) 
col_HPV18_plot
dev.off()




### SEROENCUESTA COLOMBIA VPH 16###



## CREAR DATA.FRAME DE SEROENCUESTA ## 

col_HPV16 <- dat %>% filter(
  country == "COL" & pathogen == "HPV 16"
) %>%
  
  ## SELECCCION DE DATOS QUE SE REQUIEREN PARA LOS MODELOS ##
  
  select(survey, tsur, age_min, age_max, counts, total) %>%
  rename(
    n_seropositive = counts,
    sample_size = total)

plot_serosurvey(col_HPV16)




# Modelo constante sin seroreversion


col_HPV16_constant_no_seroreversion <- fit_seromodel(
  serosurvey = col_HPV16,
  model_type = "constant",
  foi_prior = sf_normal()
)


# Modelo constante con seroreversion

col_HPV16_constant_seroreversion <- fit_seromodel(
  serosurvey = col_HPV16,
  model_type = "constant",
  foi_prior = sf_uniform(),
  is_seroreversion = TRUE,
  seroreversion_prior = sf_uniform(0.0, 2.0)
)


# Modelo dependiente del tiempo sin seroreversion

foi_index <- get_foi_index(
  serosurvey = col_HPV16,
  group_size = 10
)
init <- function() {
  list(foi_vector = rep(0.01, max(col_HPV16$age_max)))
}
col_HPV16_time_no_seroreversion <- fit_seromodel(
  serosurvey = col_HPV16,
  model_type = "time",
  foi_prior = sf_normal(),
  # foi_index = foi_index,
  is_seroreversion = FALSE,
  init = init,
  iter = 5000,
  thin = 2
)



# Modelo dependiente de la edad sin seroreversion

foi_index <- c(
  rep(1, 15),
  16:max(col_HPV16$age_max)
)
init <- function() {
  list(foi_vector = rep(0.01, max(foi_index)))
}

col_HPV16_age_no_seroreversion <- fit_seromodel(
  serosurvey = col_HPV16,
  model_type = "age",
  foi_prior = sf_normal(1e-4, 1e-5),
  foi_index = foi_index,
  is_seroreversion = FALSE,
  init = init
)


# Modelo dependiente de la edad con seroreversion

foi_index <- c(
  rep(1, 15),
  16:max(col_HPV16$age_max)
)

init <- function() {
  list(foi_vector = rep(0.01, max(foi_index)))
}

col_HPV16_age_seroreversion <- fit_seromodel(
  serosurvey = col_HPV16,
  model_type = "age",
  foi_prior = sf_normal(1e-4, 1e-5),
  foi_index = foi_index,
  is_seroreversion = TRUE,
  seroreversion_prior = sf_normal(1, 0.5),
  iter = 5000,
  thin = 2
)

# Gráficas de todos los modelos

size_text <- 8

## Constante - no seroreversion

col_HPV16_constant_no_seroreversion_plot <- plot_seromodel(
  seromodel = col_HPV16_constant_no_seroreversion,
  serosurvey = col_HPV16,
  size_text = size_text
)
plot(col_HPV16_constant_no_seroreversion_plot)

## Constante - seroreversion

col_HPV16_constant_seroreversion_plot <- plot_seromodel(
  seromodel = col_HPV16_constant_seroreversion,
  serosurvey = col_HPV16,
  size_text = size_text
)
plot(col_HPV16_constant_seroreversion_plot)

## Tiempo - no seroreversion
col_HPV16_time_no_seroreversion_plot <- plot_seromodel(
  seromodel = col_HPV16_time_no_seroreversion,
  serosurvey = col_HPV16,
  size_text = size_text
)
plot(col_HPV16_time_no_seroreversion_plot)

## Edad - no seroreversion

col_HPV16_age_no_seroreversion_plot <- plot_seromodel(
  seromodel = col_HPV16_age_no_seroreversion,
  serosurvey = col_HPV16,
  size_text = size_text
)
plot(col_HPV16_age_no_seroreversion_plot)

## Edad - seroreversion

col_HPV16_age_seroreversion_plot <- plot_seromodel(
  seromodel = col_HPV16_age_seroreversion,
  serosurvey = col_HPV16,
  size_text = size_text
)
plot(col_HPV16_age_seroreversion_plot)


# Gráfica conjunta de todos los modelos


col_HPV16_all_models_plot <- cowplot::plot_grid(
  col_HPV16_constant_no_seroreversion_plot,
  col_HPV16_constant_seroreversion_plot,
  col_HPV16_time_no_seroreversion_plot,
  col_HPV16_age_no_seroreversion_plot,
  col_HPV16_age_seroreversion_plot,
  ncol = 5
)

plot(col_HPV16_all_models_plot)
jpeg(filename = "SEROFOI/plots/col_hpv16_all.jpeg", width = 480*2, height = 480*2) 
col_HPV16_all_models_plot
dev.off()


col_HPV16_all_models_plot <- cowplot::plot_grid(
  col_HPV16_time_no_seroreversion_plot,
  col_HPV16_age_no_seroreversion_plot,
  col_HPV16_age_seroreversion_plot,
  ncol = 3
)
plot(col_HPV16_all_models_plot)
jpeg(filename = "SEROFOI/plots/col_hpv16.jpeg", width = 480*2, height = 480*2) 
col_HPV16_all_models_plot
dev.off()

col_HPV16_constant_models_plot <- cowplot::plot_grid(
  col_HPV16_constant_no_seroreversion_plot,
  col_HPV16_constant_seroreversion_plot,
  ncol = 2)

plot(col_HPV16_constant_models_plot)
jpeg(filename = "SEROFOI/plots/col_hpv16_conts.jpeg", width = 480*2, height = 480*2) 
col_HPV16_constant_models_plot
dev.off()

# Tabla de resumen

## Tabla para cada modelo

col_HPV16_constant_no_seroreversion_summary <- summarise_seromodel(
  seromodel = col_HPV16_constant_no_seroreversion,
  serosurvey = col_HPV16
) %>% as.data.frame()

col_HPV16_constant_seroreversion_summary <- summarise_seromodel(
  seromodel = col_HPV16_constant_seroreversion,
  serosurvey = col_HPV16
) %>% as.data.frame()

col_HPV16_time_no_seroreversion_summary <- summarise_seromodel(
  seromodel = col_HPV16_time_no_seroreversion,
  serosurvey = col_HPV16
) %>% as.data.frame()

col_HPV16_age_no_seroreversion_summary <- summarise_seromodel(
  seromodel = col_HPV16_age_no_seroreversion,
  serosurvey = col_HPV16
) %>% as.data.frame()

col_HPV16_age_seroreversion_summary <- summarise_seromodel(
  seromodel = col_HPV16_age_seroreversion,
  serosurvey = col_HPV16
) %>% as.data.frame()

## Tabla de todos los modelos
col_HPV16_summary <- dplyr::bind_rows(
  col_HPV16_constant_no_seroreversion_summary,
  col_HPV16_constant_seroreversion_summary,
  col_HPV16_time_no_seroreversion_summary,
  col_HPV16_age_no_seroreversion_summary,
  col_HPV16_age_seroreversion_summary
)

write.xlsx(col_HPV16_summary, file = "SEROFOI/data frame/col_HPV16_summary.xlsx")

# Gráfica de seroprevalencia

size_text <- 8

## Constante - no seroreversion

col_HPV16_constant_no_seroreversion_plot_seroprev <- plot_seroprevalence_estimates(
  seromodel = col_HPV16_constant_no_seroreversion,
  serosurvey = col_HPV16,
  size_text = size_text
) +
  ggtitle("constant - no seroreversion")

## Constante - seroreversion

col_HPV16_constant_seroreversion_plot_seroprev <- plot_seroprevalence_estimates(
  seromodel = col_HPV16_constant_seroreversion,
  serosurvey = col_HPV16,
  size_text = size_text
) +
  ggtitle("constant - seroreversion")

## Tiempo - no seroreversion

col_HPV16_time_no_seroreversion_plot_seroprev <- plot_seroprevalence_estimates(
  seromodel = col_HPV16_time_no_seroreversion,
  serosurvey = col_HPV16,
  size_text = size_text
) +
  ggtitle("time - no seroreversion")

## Edad - no seroreversion

col_HPV16_age_no_seroreversion_plot_seroprev <- plot_seroprevalence_estimates(
  seromodel = col_HPV16_age_no_seroreversion,
  serosurvey = col_HPV16,
  size_text = size_text
) +
  ggtitle("age - no seroreversion")

## Edad - seroreversion

col_HPV16_age_seroreversion_plot_seroprev <- plot_seroprevalence_estimates(
  seromodel = col_HPV16_age_seroreversion,
  serosurvey = col_HPV16,
  size_text = size_text
)+
  ggtitle("age - seroreversion")

## Grafica de seroprevalencia conjunta

col_HPV16_plot_seroprev <- cowplot::plot_grid(
  col_HPV16_constant_no_seroreversion_plot_seroprev,
  col_HPV16_constant_seroreversion_plot_seroprev,
  col_HPV16_time_no_seroreversion_plot_seroprev,
  col_HPV16_age_no_seroreversion_plot_seroprev,
  col_HPV16_age_seroreversion_plot_seroprev,
  nrow = 1,
  align = "hv"
)

# Gráfica de foi

foi_max <- 0.5

## Tiempo - no seroreversion

col_HPV16_time_no_seroreversion_plot_foi <- plot_foi_estimates(
  seromodel = col_HPV16_time_no_seroreversion,
  serosurvey = col_HPV16,
  size_text = size_text,
  foi_max = foi_max
)

plot(col_HPV16_time_no_seroreversion_plot_foi)


## Edad - no seroreversion

col_HPV16_age_no_seroreversion_plot_foi <- plot_foi_estimates(
  seromodel = col_HPV16_age_no_seroreversion,
  serosurvey = col_HPV16,
  size_text = size_text,
  foi_max = foi_max
)

plot(col_HPV16_age_no_seroreversion_plot_foi)

## Edad - seroreversion

col_HPV16_age_seroreversion_plot_foi <- plot_foi_estimates(
  seromodel = col_HPV16_age_seroreversion,
  serosurvey = col_HPV16,
  size_text = size_text,
  foi_max = foi_max
 )

plot(col_HPV16_age_seroreversion_plot_foi)

## Grafica conjunta foi

empty_plot <- ggplot() + theme_void()

col_HPV16_plot_foi <- cowplot::plot_grid(
  empty_plot,
  empty_plot,
  col_HPV16_time_no_seroreversion_plot_foi,
  col_HPV16_age_no_seroreversion_plot_foi,
  col_HPV16_age_seroreversion_plot_foi,
  ncol = 5,
  nrow = 1,
  align = "hv"
)

plot(col_HPV16_plot_foi)

# Gráfica de rhats

## Tiempo - no seroreversion

col_HPV16_time_no_seroreversion_plot_rhats <- serofoi:::plot_rhats(
  seromodel = col_HPV16_time_no_seroreversion,
  serosurvey = col_HPV16,
  size_text = size_text
)

## Edad - no seroreversion

col_HPV16_age_no_seroreversion_plot_rhats <- serofoi:::plot_rhats(
  seromodel = col_HPV16_age_no_seroreversion,
  serosurvey = col_HPV16,
  size_text = size_text
)

## Edad - seroreversion

col_HPV16_age_seroreversion_plot_rhats <- serofoi:::plot_rhats(
  seromodel = col_HPV16_age_seroreversion,
  serosurvey = col_HPV16,
  size_text = size_text
)

## Grafica conjunta foi

empty_plot <- ggplot() + theme_void()

col_HPV16_plot_rhats <- cowplot::plot_grid(
  empty_plot,
  empty_plot,
  col_HPV16_time_no_seroreversion_plot_rhats,
  col_HPV16_age_no_seroreversion_plot_rhats,
  col_HPV16_age_seroreversion_plot_rhats,
  ncol = 5,
  nrow = 1,
  align = "hv"
)

# Gráfica conjunta col HPV16

col_HPV16_plot <- cowplot::plot_grid(
  col_HPV16_plot_seroprev,
  col_HPV16_plot_foi,
  col_HPV16_plot_rhats,
  nrow = 3,
  align = "hv"
)

plot(col_HPV16_plot)
jpeg(filename = "SEROFOI/plots/all_col_hpv16_.jpeg", width = 11, height = 8, units = "in", res = 300) 
col_HPV16_plot
dev.off()

png(filename = "SEROFOI/plots/all_col_hpv16_.png", width = 500*2, height = 500*2) 
col_HPV16_plot
dev.off()


install.packages("gridExtra")






### SEROENCUESTA COLOMBIA VPH 16/18 ###


## CREAR DATA.FRAME DE SEROENCUESTA ## 

col_HPVHr<- dat %>% filter(
  country == "COL" & pathogen == "HPV 16/18"
) %>%
  
  ## SELECCCION DE DATOS QUE SE REQUIEREN PARA LOS MODELOS ##
  
  select(survey, tsur, age_min, age_max, counts, total) %>%
  rename(
    n_seropositive = counts,
    sample_size = total)

plot_serosurvey(col_HPVHr)



# Modelo constante sin seroreversion

col_HPVHr_constant_no_seroreversion <- fit_seromodel(
  serosurvey = col_HPVHr,
  model_type = "constant",
  foi_prior = sf_uniform() ,
 )



# Modelo constante con seroreversion

col_HPVHr_constant_seroreversion <- fit_seromodel(
  serosurvey = col_HPVHr,
  model_type = "constant",
  foi_prior = sf_uniform(),
  is_seroreversion = TRUE,
  seroreversion_prior = sf_uniform(0.0, 2.0)
  )



# Modelo dependiente del tiempo sin seroreversion

foi_index <- get_foi_index(
  serosurvey = col_HPVHr,
  group_size = 10
)


init <- function() {
  list(foi_vector = rep(0.01, max(col_HPVHr$age_max)))
}
col_HPVHr_time_no_seroreversion <- fit_seromodel(
  serosurvey = col_HPVHr,
  model_type = "time",
  foi_prior = sf_normal(),
  is_seroreversion = FALSE,
  init = init,
  iter = 5000,
  thin = 2
)



# Modelo dependiente de la edad sin seroreversion

foi_index <- c(
  rep(1, 15),
  16:max(col_HPVHr$age_max)
)
init <- function() {
  list(foi_vector = rep(0.01, max(foi_index)))
}
col_HPVHr_age_no_seroreversion <- fit_seromodel(
  serosurvey = col_HPVHr,
  model_type = "age",
  foi_prior = sf_normal(1e-4, 1e-5),
  foi_index = foi_index,
  is_seroreversion = FALSE,
  init = init
)


# Modelo dependiente de la edad con seroreversion

foi_index <- c(
  rep(1, 15),
  16:max(col_HPVHr$age_max)
)

init <- function() {
  list(foi_vector = rep(0.01, max(foi_index)))
}

col_HPVHr_age_seroreversion <- fit_seromodel(
  serosurvey = col_HPVHr,
  model_type = "age",
  foi_prior = sf_normal(1e-4, 1e-5),
  foi_index = foi_index,
  is_seroreversion = TRUE,
  seroreversion_prior = sf_normal(1, 0.5),
  iter = 5000,
  thin = 2
)



# Gráfica conjunta de todos los modelos

size_text <- 8

## Constante - no seroreversion

col_HPVHr_constant_no_seroreversion_plot <- plot_seromodel(
  seromodel = col_HPVHr_constant_no_seroreversion,
  serosurvey = col_HPVHr,
  size_text = size_text
)
plot(col_HPVHr_constant_no_seroreversion_plot)

## Constante - seroreversion

col_HPVHr_constant_seroreversion_plot <- plot_seromodel(
  seromodel = col_HPVHr_constant_seroreversion,
  serosurvey = col_HPVHr,
  size_text = size_text
)
plot(col_HPVHr_constant_seroreversion_plot)

## Tiempo - no seroreversion

col_HPVHr_time_no_seroreversion_plot <- plot_seromodel(
  seromodel = col_HPVHr_time_no_seroreversion,
  serosurvey = col_HPVHr,
  size_text = size_text
)
plot(col_HPVHr_time_no_seroreversion_plot)

## Edad - no seroreversion

col_HPVHr_age_no_seroreversion_plot <- plot_seromodel(
  seromodel = col_HPVHr_age_no_seroreversion,
  serosurvey = col_HPVHr,
  size_text = size_text
)
plot(col_HPVHr_age_no_seroreversion_plot)

## Edad - seroreversion

col_HPVHr_age_seroreversion_plot <- plot_seromodel(
  seromodel = col_HPVHr_age_seroreversion,
  serosurvey = col_HPVHr,
  size_text = size_text
)
plot(col_HPVHr_age_seroreversion_plot)

# Grafica conjunta de todos los modelos



col_HPVHr_models_plot <- cowplot::plot_grid(
  col_HPVHr_constant_no_seroreversion_plot,
  col_HPVHr_constant_seroreversion_plot,
  col_HPVHr_time_no_seroreversion_plot,
  col_HPVHr_age_no_seroreversion_plot,
  col_HPVHr_age_seroreversion_plot,
  ncol = 5
)
plot(col_HPVHr_models_plot)


col_HPVHr_all_models_plot <- cowplot::plot_grid(
  col_HPVHr_time_no_seroreversion_plot,
  col_HPVHr_age_no_seroreversion_plot,
  col_HPVHr_age_seroreversion_plot,
  ncol = 3
)
plot(col_HPVHr_all_models_plot)
jpeg(filename = "SEROFOI/plots/col_HPVHr.jpeg", width = 480*2, height = 480*2) 
col_HPV16_all_models_plot
dev.off()

col_HPVHr_constant_models_plot <- cowplot::plot_grid(
  col_HPVHr_constant_no_seroreversion_plot,
  col_HPVHr_constant_seroreversion_plot,
  ncol = 2)

plot(col_HPVHr_constant_models_plot)
jpeg(filename = "SEROFOI/plots/col_hpv16_conts.jpeg", width = 480*2, height = 480*2) 
col_HPVHr_constant_models_plot
dev.off()



## Tabla para cada modelo

col_HPVHr_constant_no_seroreversion_summary <- summarise_seromodel(
  seromodel = col_HPVHr_constant_no_seroreversion,
  serosurvey = col_HPVHr
) %>% as.data.frame()

col_HPVHr_constant_seroreversion_summary <- summarise_seromodel(
  seromodel = col_HPVHr_constant_seroreversion,
  serosurvey = col_HPVHr
) %>% as.data.frame()

col_HPVHr_time_no_seroreversion_summary <- summarise_seromodel(
  seromodel = col_HPVHr_time_no_seroreversion,
  serosurvey = col_HPVHr
) %>% as.data.frame()

col_HPVHr_age_no_seroreversion_summary <- summarise_seromodel(
  seromodel = col_HPVHr_age_no_seroreversion,
  serosurvey = col_HPVHr
) %>% as.data.frame()

col_HPVHr_age_seroreversion_summary <- summarise_seromodel(
  seromodel = col_HPVHr_age_seroreversion,
  serosurvey = col_HPVHr
) %>% as.data.frame()

## Tabla de todos los modelos

col_HPVHr_summary <- dplyr::bind_rows(
  col_HPVHr_constant_no_seroreversion_summary,
  col_HPVHr_constant_seroreversion_summary,
  col_HPVHr_time_no_seroreversion_summary,
  col_HPVHr_age_no_seroreversion_summary,
  col_HPVHr_age_seroreversion_summary
)

write.xlsx(col_HPVHr_summary, file = "SEROFOI/data frame/col_HPVHr_summary.xlsx")

# Gráfica de seroprevalencia
size_text <- 8
## Constante - no seroreversion

col_HPVHr_constant_no_seroreversion_plot_seroprev <- plot_seroprevalence_estimates(
  seromodel = col_HPVHr_constant_no_seroreversion,
  serosurvey = col_HPVHr,
  size_text = size_text
) +
  ggtitle("constant - no seroreversion")

## Constante - seroreversion

col_HPVHr_constant_seroreversion_plot_seroprev <- plot_seroprevalence_estimates(
  seromodel = col_HPVHr_constant_seroreversion,
  serosurvey = col_HPVHr,
  size_text = size_text
) +
  ggtitle("constant - seroreversion")

## Tiempo - no seroreversion

col_HPVHr_time_no_seroreversion_plot_seroprev <- plot_seroprevalence_estimates(
  seromodel = col_HPVHr_time_no_seroreversion,
  serosurvey = col_HPVHr,
  size_text = size_text
) +
  ggtitle("time - no seroreversion")

## Edad - no seroreversion

col_HPVHr_age_no_seroreversion_plot_seroprev <- plot_seroprevalence_estimates(
  seromodel = col_HPVHr_age_no_seroreversion,
  serosurvey = col_HPVHr,
  size_text = size_text
) +
  ggtitle("age - no seroreversion")

## Edad - seroreversion

col_HPVHr_age_seroreversion_plot_seroprev <- plot_seroprevalence_estimates(
  seromodel = col_HPVHr_age_seroreversion,
  serosurvey = col_HPVHr,
  size_text = size_text
)+
  ggtitle("age - seroreversion")

## Grafica de seroprevalencia conjunta

col_HPVHr_plot_seroprev <- cowplot::plot_grid(
  col_HPVHr_constant_no_seroreversion_plot_seroprev,
  col_HPVHr_constant_seroreversion_plot_seroprev,
  col_HPVHr_time_no_seroreversion_plot_seroprev,
  col_HPVHr_age_no_seroreversion_plot_seroprev,
  col_HPVHr_age_seroreversion_plot_seroprev,
  nrow = 1,
  align = "hv"
)

# Gráfica de foi
foi_max <- 0.6
## Tiempo - no seroreversion

col_HPVHr_time_no_seroreversion_plot_foi <- plot_foi_estimates(
  seromodel = col_HPVHr_time_no_seroreversion,
  serosurvey = col_HPVHr,
  size_text = size_text,
  foi_max = foi_max
)

## Edad - no seroreversion

col_HPVHr_age_no_seroreversion_plot_foi <- plot_foi_estimates(
  seromodel = col_HPVHr_age_no_seroreversion,
  serosurvey = col_HPVHr,
  size_text = size_text,
  foi_max = foi_max
)

## Edad - seroreversion

col_HPVHr_age_seroreversion_plot_foi <- plot_foi_estimates(
  seromodel = col_HPVHr_age_seroreversion,
  serosurvey = col_HPVHr,
  size_text = size_text,
  foi_max = foi_max
)

## Grafica conjunta foi


empty_plot <- ggplot() + theme_void()

col_HPVHr_plot_foi <- cowplot::plot_grid(
  empty_plot,
  empty_plot,
  col_HPVHr_time_no_seroreversion_plot_foi,
  col_HPVHr_age_no_seroreversion_plot_foi,
  col_HPVHr_age_seroreversion_plot_foi,
  ncol = 5,
  nrow = 1,
  align = "hv"
)

# Gráfica de rhats

## Tiempo - no seroreversion

col_HPVHr_time_no_seroreversion_plot_rhats <- serofoi:::plot_rhats(
  seromodel = col_HPVHr_time_no_seroreversion,
  serosurvey = col_HPVHr,
  size_text = size_text
)

## Edad - no seroreversion

col_HPVHr_age_no_seroreversion_plot_rhats <- serofoi:::plot_rhats(
  seromodel = col_HPVHr_age_no_seroreversion,
  serosurvey = col_HPVHr,
  size_text = size_text
)

## Edad - seroreversion

col_HPVHr_age_seroreversion_plot_rhats <- serofoi:::plot_rhats(
  seromodel = col_HPVHr_age_seroreversion,
  serosurvey = col_HPVHr,
  size_text = size_text
)

## Grafica conjunta foi

empty_plot <- ggplot() + theme_void()

col_HPVHr_plot_rhats <- cowplot::plot_grid(
  empty_plot,
  empty_plot,
  col_HPVHr_time_no_seroreversion_plot_rhats,
  col_HPVHr_age_no_seroreversion_plot_rhats,
  col_HPVHr_age_seroreversion_plot_rhats,
  ncol = 5,
  nrow = 1,
  align = "hv"
)



# Gráfica conjunta col HPVHt

col_HPVHr_plot <- cowplot::plot_grid(
  col_HPVHr_plot_seroprev,
  col_HPVHr_plot_foi,
  col_HPVHr_plot_rhats,
  nrow = 3,
  align = "hv"
)

plot(col_HPVHr_plot)
jpeg(filename = "SEROFOI/plots/all_col_hpvHr_.jpeg", width = 11, height = 8, units = "in", res = 300) 
col_HPV18_plot
dev.off()



### SEROENCUESTA COSTA RICA VPH 18 ###

##CREAR DATA.FRAME DE SEROENCUESTA ## 

CRI_HPV18 <- dat %>% filter(
  country == "CRI" & pathogen == "HPV 18"
) %>%

  ## SELECCCION DE DATOS QUE SE REQUIEREN PARA LOS MODELOS ##
  select(survey, tsur, age_min, age_max, counts, total) %>%
  rename(
    n_seropositive = counts,
    sample_size = total)

plot_serosurvey(CRI_HPV18)



# Modelo constante sin seroreversion

CRI_HPV18_constant_no_seroreversion <- fit_seromodel(
  serosurvey = CRI_HPV18,
  model_type = "constant",
  foi_prior = sf_uniform() ,
)



# Modelo constante con seroreversion


CRI_HPV18_constant_seroreversion <- fit_seromodel(
  serosurvey = CRI_HPV18,
  model_type = "constant",
  foi_prior = sf_uniform(),
  is_seroreversion = TRUE,
  seroreversion_prior = sf_uniform(0.0, 2.0)
)



# Modelo dependiente del tiempo sin seroreversion

foi_index <- get_foi_index(
  serosurvey = CRI_HPV18,
  group_size = 10
)


init <- function() {
  list(foi_vector = rep(0.01, max(CRI_HPV18$age_max)))
}
CRI_HPV18_time_no_seroreversion <- fit_seromodel(
  serosurvey = CRI_HPV18,
  model_type = "time",
  foi_prior = sf_normal(),
  is_seroreversion = FALSE,
  init = init,
  iter = 5000,
  thin = 2
)


# Modelo dependiente de la edad sin seroreversion

foi_index <- c(
  rep(1, 15),
  16:max(CRI_HPV18$age_max)
)
init <- function() {
  list(foi_vector = rep(0.01, max(foi_index)))
}
CRI_HPV18_age_no_seroreversion <- fit_seromodel(
  serosurvey = CRI_HPV18,
  model_type = "age",
  foi_prior = sf_normal(1e-4, 1e-5),
  foi_index = foi_index,
  is_seroreversion = FALSE,
  init = init
)



# Modelo dependiente de la edad con seroreversion

foi_index <- c(
  rep(1, 15),
  16:max(CRI_HPV18$age_max)
)

init <- function() {
  list(foi_vector = rep(0.01, max(foi_index)))
}

CRI_HPV18_age_seroreversion <- fit_seromodel(
  serosurvey = CRI_HPV18,
  model_type = "age",
  foi_prior = sf_normal(1e-4, 1e-5),
  foi_index = foi_index,
  is_seroreversion = TRUE,
  seroreversion_prior = sf_normal(1, 0.5),
  iter = 5000,
  thin = 2
)




# Gráfica conjunta de todos los modelos

size_text <- 8

## Constante - no seroreversion

CRI_HPV18_constant_no_seroreversion_plot <- plot_seromodel(
  seromodel = CRI_HPV18_constant_no_seroreversion,
  serosurvey = CRI_HPV18,
  size_text = size_text
)
plot(CRI_HPV18_constant_no_seroreversion_plot)

## Constante - seroreversion

CRI_HPV18_constant_seroreversion_plot <- plot_seromodel(
  seromodel = CRI_HPV18_constant_seroreversion,
  serosurvey = CRI_HPV18,
  size_text = size_text
)
plot(CRI_HPV18_constant_seroreversion_plot)

## Tiempo - no seroreversion

CRI_HPV18_time_no_seroreversion_plot <- plot_seromodel(
  seromodel = CRI_HPV18_time_no_seroreversion,
  serosurvey = CRI_HPV18,
  size_text = size_text
)

plot(CRI_HPV18_time_no_seroreversion_plot)

## Edad - no seroreversion

CRI_HPV18_age_no_seroreversion_plot <- plot_seromodel(
  seromodel = CRI_HPV18_age_no_seroreversion,
  serosurvey = CRI_HPV18,
  size_text = size_text
)
plot(CRI_HPV18_age_no_seroreversion_plot)

## Edad - seroreversion

CRI_HPV18_age_seroreversion_plot <- plot_seromodel(
  seromodel = CRI_HPV18_age_seroreversion,
  serosurvey = CRI_HPV18,
  size_text = size_text
)

plot(CRI_HPV18_age_seroreversion_plot)




# Gráfica conjunta de todos los modelos

CRI_HPV18_models_plot <- cowplot::plot_grid(
  CRI_HPV18_constant_no_seroreversion_plot,
  CRI_HPV18_constant_seroreversion_plot,
  CRI_HPV18_time_no_seroreversion_plot,
  CRI_HPV18_age_no_seroreversion_plot,
  CRI_HPV18_age_seroreversion_plot,
  ncol = 5
)
plot(CRI_HPV18_models_plot)


CRI_HPV18_all_models_plot <- cowplot::plot_grid(
  CRI_HPV18_time_no_seroreversion_plot,
  CRI_HPV18_age_no_seroreversion_plot,
  CRI_HPV18_age_seroreversion_plot,
  ncol = 3
)
plot(CRI_HPV18_all_models_plot)
jpeg(filename = "SEROFOI/plots/col_HPVHr.jpeg", width = 480*2, height = 480*2) 
CRI_HPV18_all_models_plot
dev.off()

CRI_HPV18_constant_models_plot <- cowplot::plot_grid(
  CRI_HPV18_constant_no_seroreversion_plot,
  CRI_HPV18_constant_seroreversion_plot,
  ncol = 2)

plot(CRI_HPV18_constant_models_plot)
jpeg(filename = "SEROFOI/plots/col_hpv16_conts.jpeg", width = 480*2, height = 480*2) 
CRI_HPV18_constant_models_plot
dev.off()



# Tabla de resumen

## Tabla para cada modelo
CRI_HPV18_constant_no_seroreversion_summary <- summarise_seromodel(
  seromodel = CRI_HPV18_constant_no_seroreversion,
  serosurvey = CRI_HPV18
) %>% as.data.frame()

CRI_HPV18_constant_seroreversion_summary <- summarise_seromodel(
  seromodel = CRI_HPV18_constant_seroreversion,
  serosurvey = CRI_HPV18
) %>% as.data.frame()

CRI_HPV18_time_no_seroreversion_summary <- summarise_seromodel(
  seromodel = CRI_HPV18_time_no_seroreversion,
  serosurvey = CRI_HPV18
) %>% as.data.frame()

CRI_HPV18_age_no_seroreversion_summary <- summarise_seromodel(
  seromodel = CRI_HPV18_age_no_seroreversion,
  serosurvey = CRI_HPV18
) %>% as.data.frame()

CRI_HPV18_age_seroreversion_summary <- summarise_seromodel(
  seromodel = CRI_HPV18_age_seroreversion,
  serosurvey = CRI_HPV18
) %>% as.data.frame()

## Tabla de todos los modelos
CRI_HPV18_summary <- dplyr::bind_rows(
  CRI_HPV18_constant_no_seroreversion_summary,
  CRI_HPV18_constant_seroreversion_summary,
  CRI_HPV18_time_no_seroreversion_summary,
  CRI_HPV18_age_no_seroreversion_summary,
  CRI_HPV18_age_seroreversion_summary
)


write.xlsx(CRI_HPV18_summary, file = "SEROFOI/data frame/CRI_HPV18_summary.xlsx")

# Gráfica de seroprevalencia
size_text <- 8

## Constante - no seroreversion

CRI_HPV18_constant_no_seroreversion_plot_seroprev <- plot_seroprevalence_estimates(
  seromodel = CRI_HPV18_constant_no_seroreversion,
  serosurvey = CRI_HPV18,
  size_text = size_text
) +
  ggtitle("constant - no seroreversion")


## Constante - seroreversion

CRI_HPV18_constant_seroreversion_plot_seroprev <- plot_seroprevalence_estimates(
  seromodel = CRI_HPV18_constant_seroreversion,
  serosurvey = CRI_HPV18,
  size_text = size_text
) +
  ggtitle("constant - seroreversion")

## Tiempo - no seroreversion

CRI_HPV18_time_no_seroreversion_plot_seroprev <- plot_seroprevalence_estimates(
  seromodel = CRI_HPV18_time_no_seroreversion,
  serosurvey = CRI_HPV18,
  size_text = size_text
) +
  ggtitle("time - no seroreversion")

## Edad - no seroreversion

CRI_HPV18_age_no_seroreversion_plot_seroprev <- plot_seroprevalence_estimates(
  seromodel = CRI_HPV18_age_no_seroreversion,
  serosurvey = CRI_HPV18,
  size_text = size_text
) +
  ggtitle("age - no seroreversion")

## Edad - seroreversion

CRI_HPV18_age_seroreversion_plot_seroprev <- plot_seroprevalence_estimates(
  seromodel = CRI_HPV18_age_seroreversion,
  serosurvey = CRI_HPV18,
  size_text = size_text
)+
  ggtitle("age - seroreversion")

## Grafica de seroprevalencia conjunta

CRI_HPV18_plot_seroprev <- cowplot::plot_grid(
  CRI_HPV18_constant_no_seroreversion_plot_seroprev,
  CRI_HPV18_constant_seroreversion_plot_seroprev,
  CRI_HPV18_time_no_seroreversion_plot_seroprev,
  CRI_HPV18_age_no_seroreversion_plot_seroprev,
  CRI_HPV18_age_seroreversion_plot_seroprev,
  nrow = 1,
  align = "hv"
)

# Gráfica de foi
foi_max <- 1.00

# Tiempo - no seroreversion

CRI_HPV18_time_no_seroreversion_plot_foi <- plot_foi_estimates(
  seromodel = CRI_HPV18_time_no_seroreversion,
  serosurvey = CRI_HPV18,
  size_text = size_text,
  foi_max = foi_max
)

## Edad - no seroreversion
CRI_HPV18_age_no_seroreversion_plot_foi <- plot_foi_estimates(
  seromodel = CRI_HPV18_age_no_seroreversion,
  serosurvey = CRI_HPV18,
  size_text = size_text,
  foi_max = foi_max
)

## Edad - seroreversion
CRI_HPV18_age_seroreversion_plot_foi <- plot_foi_estimates(
  seromodel = CRI_HPV18_age_seroreversion,
  serosurvey = CRI_HPV18,
  size_text = size_text,
  foi_max = foi_max
)

## Grafica conjunta foi
empty_plot <- ggplot() + theme_void()

CRI_HPV18_plot_foi <- cowplot::plot_grid(
  empty_plot,
  empty_plot,
  CRI_HPV18_time_no_seroreversion_plot_foi,
  CRI_HPV18_age_no_seroreversion_plot_foi,
  CRI_HPV18_age_seroreversion_plot_foi,
  ncol = 5,
  nrow = 1,
  align = "hv"
)

# Gráfica de rhats

## Tiempo - no seroreversion
CRI_HPV18_time_no_seroreversion_plot_rhats <- serofoi:::plot_rhats(
  seromodel = CRI_HPV18_time_no_seroreversion,
  serosurvey = CRI_HPV18,
  size_text = size_text
)

## Edad - no seroreversion
CRI_HPV18_age_no_seroreversion_plot_rhats <- serofoi:::plot_rhats(
  seromodel = CRI_HPV18_age_no_seroreversion,
  serosurvey = CRI_HPV18,
  size_text = size_text
)

## Edad - seroreversion
CRI_HPV18_age_seroreversion_plot_rhats <- serofoi:::plot_rhats(
  seromodel = CRI_HPV18_age_seroreversion,
  serosurvey = CRI_HPV18,
  size_text = size_text
)

## Grafica conjunta foi
empty_plot <- ggplot() + theme_void()

CRI_HPV18_plot_rhats <- cowplot::plot_grid(
  empty_plot,
  empty_plot,
  CRI_HPV18_time_no_seroreversion_plot_rhats,
  CRI_HPV18_age_no_seroreversion_plot_rhats,
  CRI_HPV18_age_seroreversion_plot_rhats,
  ncol = 5,
  nrow = 1,
  align = "hv"
)

# Gráfica conjunta col CRI_HPV18

CRI_HPV18_plot <- cowplot::plot_grid(
  CRI_HPV18_plot_seroprev,
  CRI_HPV18_plot_foi,
  CRI_HPV18_plot_rhats,
  nrow = 3,
  align = "hv"
)

plot(CRI_HPV18_plot)
jpeg(filename = "SEROFOI/plots/all_CRI_HPV18_.jpeg", width = 11, height = 8, units = "in", res = 300) 
CRI_HPV18_plot
dev.off()



### SEROENCUESTA COSTA RICA VPH 16 ###

CRI_HPV16 <- dat %>% filter(
  country == "CRI" & pathogen == "HPV 16"
) %>%
  
  ## SELECCCION DE DATOS QUE SE REQUIEREN PARA LOS MODELOS ##
  select(survey, tsur, age_min, age_max, counts, total) %>%
  rename(
    n_seropositive = counts,
    sample_size = total)

plot_serosurvey(CRI_HPV16)

## Modelo constante sin seroreversion

CRI_HPV16_constant_no_seroreversion <- fit_seromodel(
  serosurvey = CRI_HPV16,
  model_type = "constant",
  foi_prior = sf_uniform() ,
)



# Modelo constante con seroreversion


CRI_HPV16_constant_seroreversion <- fit_seromodel(
  serosurvey = CRI_HPV16,
  model_type = "constant",
  foi_prior = sf_uniform(),
  is_seroreversion = TRUE,
  seroreversion_prior = sf_uniform(0.0, 2.0),
  iter =1500
)



# Modelo dependiente del tiempo sin seroreversion

foi_index <- get_foi_index(
  serosurvey = CRI_HPV16,
  group_size = 10
)


init <- function() {
  list(foi_vector = rep(0.01, max(CRI_HPV16$age_max)))
}
CRI_HPV16_time_no_seroreversion <- fit_seromodel(
  serosurvey = CRI_HPV16,
  model_type = "time",
  foi_prior = sf_normal(),
  is_seroreversion = FALSE,
  init = init,
  iter = 5000,
  thin = 2
)


# Modelo dependiente de la edad sin seroreversion

foi_index <- c(
  rep(1, 15),
  16:max(CRI_HPV16$age_max)
)
init <- function() {
  list(foi_vector = rep(0.01, max(foi_index)))
}
CRI_HPV16_age_no_seroreversion <- fit_seromodel(
  serosurvey = CRI_HPV16,
  model_type = "age",
  foi_prior = sf_normal(1e-4, 1e-5),
  foi_index = foi_index,
  is_seroreversion = FALSE,
  init = init
)



# Modelo dependiente de la edad con seroreversion

foi_index <- c(
  rep(1, 15),
  16:max(CRI_HPV16$age_max)
)

init <- function() {
  list(foi_vector = rep(0.01, max(foi_index)))
}

CRI_HPV16_age_seroreversion <- fit_seromodel(
  serosurvey = CRI_HPV16,
  model_type = "age",
  foi_prior = sf_normal(1e-4, 1e-5),
  foi_index = foi_index,
  is_seroreversion = TRUE,
  seroreversion_prior = sf_normal(1, 0.5),
  iter = 5000,
  thin = 2
)


# Gráfica conjunta de todos los modelos

size_text <- 8

## Constante - no seroreversion

CRI_HPV16_constant_no_seroreversion_plot <- plot_seromodel(
  seromodel = CRI_HPV16_constant_no_seroreversion,
  serosurvey = CRI_HPV16,
  size_text = size_text
)
plot(CRI_HPV16_constant_no_seroreversion_plot)

## Constante - seroreversion

CRI_HPV16_constant_seroreversion_plot <- plot_seromodel(
  seromodel = CRI_HPV16_constant_seroreversion,
  serosurvey = CRI_HPV16,
  size_text = size_text
)
plot(CRI_HPV16_constant_seroreversion_plot)

## Tiempo - no seroreversion

CRI_HPV16_time_no_seroreversion_plot <- plot_seromodel(
  seromodel = CRI_HPV16_time_no_seroreversion,
  serosurvey = CRI_HPV16,
  size_text = size_text
)

plot(CRI_HPV16_time_no_seroreversion_plot)

## Edad - no seroreversion

CRI_HPV16_age_no_seroreversion_plot <- plot_seromodel(
  seromodel = CRI_HPV16_age_no_seroreversion,
  serosurvey = CRI_HPV16,
  size_text = size_text
)
plot(CRI_HPV16_age_no_seroreversion_plot)

## Edad - seroreversion

CRI_HPV16_age_seroreversion_plot <- plot_seromodel(
  seromodel = CRI_HPV16_age_seroreversion,
  serosurvey = CRI_HPV16,
  size_text = size_text
)

plot(CRI_HPV16_age_seroreversion_plot)

# Tabla de resumen

## Tabla para cada modelo
CRI_HPV16_constant_no_seroreversion_summary <- summarise_seromodel(
  seromodel = CRI_HPV16_constant_no_seroreversion,
  serosurvey = CRI_HPV16
) %>% as.data.frame()

CRI_HPV16_constant_seroreversion_summary <- summarise_seromodel(
  seromodel = CRI_HPV16_constant_seroreversion,
  serosurvey = CRI_HPV16
) %>% as.data.frame()

CRI_HPV16_time_no_seroreversion_summary <- summarise_seromodel(
  seromodel = CRI_HPV16_time_no_seroreversion,
  serosurvey = CRI_HPV16
) %>% as.data.frame()

CRI_HPV16_age_no_seroreversion_summary <- summarise_seromodel(
  seromodel = CRI_HPV16_age_no_seroreversion,
  serosurvey = CRI_HPV16
) %>% as.data.frame()

CRI_HPV16_age_seroreversion_summary <- summarise_seromodel(
  seromodel = CRI_HPV16_age_seroreversion,
  serosurvey = CRI_HPV16
) %>% as.data.frame()

## Tabla de todos los modelos
CRI_HPV16_summary <- dplyr::bind_rows(
  CRI_HPV16_constant_no_seroreversion_summary,
  CRI_HPV16_constant_seroreversion_summary,
  CRI_HPV16_time_no_seroreversion_summary,
  CRI_HPV16_age_no_seroreversion_summary,
  CRI_HPV16_age_seroreversion_summary
)


write.xlsx(CRI_HPV16_summary, file = "SEROFOI/data frame/CRI_HPV16_summary.xlsx")

# Gráfica de seroprevalencia
size_text <- 8

## Constante - no seroreversion

CRI_HPV16_constant_no_seroreversion_plot_seroprev <- plot_seroprevalence_estimates(
  seromodel = CRI_HPV16_constant_no_seroreversion,
  serosurvey = CRI_HPV16,
  size_text = size_text
) +
  ggtitle("constant - no seroreversion")


## Constante - seroreversion

CRI_HPV16_constant_seroreversion_plot_seroprev <- plot_seroprevalence_estimates(
  seromodel = CRI_HPV16_constant_seroreversion,
  serosurvey = CRI_HPV16,
  size_text = size_text
) +
  ggtitle("constant - seroreversion")

## Tiempo - no seroreversion

CRI_HPV16_time_no_seroreversion_plot_seroprev <- plot_seroprevalence_estimates(
  seromodel = CRI_HPV16_time_no_seroreversion,
  serosurvey = CRI_HPV16,
  size_text = size_text
) +
  ggtitle("time - no seroreversion")

## Edad - no seroreversion

CRI_HPV16_age_no_seroreversion_plot_seroprev <- plot_seroprevalence_estimates(
  seromodel = CRI_HPV16_age_no_seroreversion,
  serosurvey = CRI_HPV16,
  size_text = size_text
) +
  ggtitle("age - no seroreversion")

## Edad - seroreversion

CRI_HPV16_age_seroreversion_plot_seroprev <- plot_seroprevalence_estimates(
  seromodel = CRI_HPV16_age_seroreversion,
  serosurvey = CRI_HPV16,
  size_text = size_text
)+
  ggtitle("age - seroreversion")

## Grafica de seroprevalencia conjunta

CRI_HPV16_plot_seroprev <- cowplot::plot_grid(
  CRI_HPV16_constant_no_seroreversion_plot_seroprev,
  CRI_HPV16_constant_seroreversion_plot_seroprev,
  CRI_HPV16_time_no_seroreversion_plot_seroprev,
  CRI_HPV16_age_no_seroreversion_plot_seroprev,
  CRI_HPV16_age_seroreversion_plot_seroprev,
  nrow = 1,
  align = "hv"
)

# Gráfica de foi
foi_max <- 0.9

# Tiempo - no seroreversion

CRI_HPV16_time_no_seroreversion_plot_foi <- plot_foi_estimates(
  seromodel = CRI_HPV16_time_no_seroreversion,
  serosurvey = CRI_HPV16,
  size_text = size_text,
  foi_max = foi_max
)

## Edad - no seroreversion
CRI_HPV16_age_no_seroreversion_plot_foi <- plot_foi_estimates(
  seromodel = CRI_HPV16_age_no_seroreversion,
  serosurvey = CRI_HPV16,
  size_text = size_text,
  foi_max = foi_max
)

## Edad - seroreversion
CRI_HPV16_age_seroreversion_plot_foi <- plot_foi_estimates(
  seromodel = CRI_HPV16_age_seroreversion,
  serosurvey = CRI_HPV16,
  size_text = size_text,
  foi_max = foi_max
)

## Grafica conjunta foi
empty_plot <- ggplot() + theme_void()

CRI_HPV16_plot_foi <- cowplot::plot_grid(
  empty_plot,
  empty_plot,
  CRI_HPV16_time_no_seroreversion_plot_foi,
  CRI_HPV16_age_no_seroreversion_plot_foi,
  CRI_HPV16_age_seroreversion_plot_foi,
  ncol = 5,
  nrow = 1,
  align = "hv"
)

# Gráfica de rhats

## Tiempo - no seroreversion
CRI_HPV16_time_no_seroreversion_plot_rhats <- serofoi:::plot_rhats(
  seromodel = CRI_HPV16_time_no_seroreversion,
  serosurvey = CRI_HPV16,
  size_text = size_text
)

## Edad - no seroreversion
CRI_HPV16_age_no_seroreversion_plot_rhats <- serofoi:::plot_rhats(
  seromodel = CRI_HPV16_age_no_seroreversion,
  serosurvey = CRI_HPV16,
  size_text = size_text
)

## Edad - seroreversion
CRI_HPV16_age_seroreversion_plot_rhats <- serofoi:::plot_rhats(
  seromodel = CRI_HPV16_age_seroreversion,
  serosurvey = CRI_HPV16,
  size_text = size_text
)

## Grafica conjunta foi
empty_plot <- ggplot() + theme_void()

CRI_HPV16_plot_rhats <- cowplot::plot_grid(
  empty_plot,
  empty_plot,
  CRI_HPV16_time_no_seroreversion_plot_rhats,
  CRI_HPV16_age_no_seroreversion_plot_rhats,
  CRI_HPV16_age_seroreversion_plot_rhats,
  ncol = 5,
  nrow = 1,
  align = "hv"
)

# Gráfica conjunta col CRI_HPV16

CRI_HPV16_plot <- cowplot::plot_grid(
  CRI_HPV16_plot_seroprev,
  CRI_HPV16_plot_foi,
  CRI_HPV16_plot_rhats,
  nrow = 3,
  align = "hv"
)

plot(CRI_HPV16_plot)
jpeg(filename = "SEROFOI/plots/all_CRI_HPV16_.jpeg", width = 11, height = 8, units = "in", res = 300) 
CRI_HPV16_plot
dev.off()



# Gráfica conjunta de todos los modelos

CRI_HPV16_models_plot <- cowplot::plot_grid(
  CRI_HPV16_constant_no_seroreversion_plot,
  CRI_HPV16_constant_seroreversion_plot,
  CRI_HPV16_time_no_seroreversion_plot,
  CRI_HPV16_age_no_seroreversion_plot,
  CRI_HPV16_age_seroreversion_plot,
  ncol = 5
)
plot(CRI_HPV16_models_plot)


CRI_HPV16_all_models_plot <- cowplot::plot_grid(
  CRI_HPV16_time_no_seroreversion_plot,
  CRI_HPV16_age_no_seroreversion_plot,
  CRI_HPV16_age_seroreversion_plot,
  ncol = 3
)
plot(CRI_HPV16_all_models_plot)
jpeg(filename = "SEROFOI/plots/col_HPVHr.jpeg", width = 480*2, height = 480*2) 
CRI_HPV16_all_models_plot
dev.off()

CRI_HPV16_constant_models_plot <- cowplot::plot_grid(
  CRI_HPV16_constant_no_seroreversion_plot,
  CRI_HPV16_constant_seroreversion_plot,
  ncol = 2)

plot(CRI_HPV16_constant_models_plot)
jpeg(filename = "SEROFOI/plots/col_hpv16_conts.jpeg", width = 480*2, height = 480*2) 
CRI_HPV16_constant_models_plot
dev.off()






### SEROENCUESTA BRASIL HPV 16  ###

##CREAR DATA.FRAME DE SEROENCUESTA ## 

BRA_HPV16 <- dat %>% filter( survey == "BRA-017-01"
 ) %>%
  
  ## SELECCCION DE DATOS QUE SE REQUIEREN PARA LOS MODELOS ##
  select(survey, tsur, age_min, age_max, counts, total) %>%
  rename(
    n_seropositive = counts,
    sample_size = total)

plot_serosurvey(BRA_HPV16)



## Modelo constante sin seroreversion

BRA_HPV16_constant_no_seroreversion <- fit_seromodel(
  serosurvey = BRA_HPV16,
  model_type = "constant",
  foi_prior = sf_uniform() ,
)

# Modelo constante con seroreversion

BRA_HPV16_constant_seroreversion <- fit_seromodel(
  serosurvey = BRA_HPV16,
  model_type = "constant",
  foi_prior = sf_uniform(),
  is_seroreversion = TRUE,
  seroreversion_prior = sf_uniform(0.0, 2.0),
  iter =1500
)

# Modelo dependiente del tiempo sin seroreversion

foi_index <- get_foi_index(
  serosurvey = BRA_HPV16,
  group_size = 10
)

init <- function() {
  list(foi_vector = rep(0.01, max(BRA_HPV16$age_max)))
}
BRA_HPV16_time_no_seroreversion <- fit_seromodel(
  serosurvey = BRA_HPV16,
  model_type = "time",
  foi_prior = sf_normal(),
  is_seroreversion = FALSE,
  init = init,
  iter = 5000,
  thin = 2
)

# Modelo dependiente de la edad sin seroreversion

foi_index <- c(
  rep(1, 15),
  16:max(BRA_HPV16$age_max)
)
init <- function() {
  list(foi_vector = rep(0.01, max(foi_index)))
}
BRA_HPV16_age_no_seroreversion <- fit_seromodel(
  serosurvey = BRA_HPV16,
  model_type = "age",
  foi_prior = sf_normal(1e-4, 1e-5),
  foi_index = foi_index,
  is_seroreversion = FALSE,
  init = init, 
  iter = 5000,
  thin = 2
)

# Modelo dependiente de la edad con seroreversion

foi_index <- c(
  rep(1, 15),
  16:max(BRA_HPV16$age_max)
)

init <- function() {
  list(foi_vector = rep(0.01, max(foi_index)))
}

BRA_HPV16_age_seroreversion <- fit_seromodel(
  serosurvey = BRA_HPV16,
  model_type = "age",
  foi_prior = sf_normal(1e-4, 1e-5),
  foi_index = foi_index,
  is_seroreversion = TRUE,
  seroreversion_prior = sf_normal(1, 0.5),
  iter = 5000,
  thin = 2
)


# Gráfica conjunta de todos los modelos

size_text <- 8

## Constante - no seroreversion

BRA_HPV16_constant_no_seroreversion_plot <- plot_seromodel(
  seromodel = BRA_HPV16_constant_no_seroreversion,
  serosurvey = BRA_HPV16,
  size_text = size_text
)
plot(BRA_HPV16_constant_no_seroreversion_plot)

## Constante - seroreversion

BRA_HPV16_constant_seroreversion_plot <- plot_seromodel(
  seromodel = BRA_HPV16_constant_seroreversion,
  serosurvey = BRA_HPV16,
  size_text = size_text
)
plot(BRA_HPV16_constant_seroreversion_plot)

## Tiempo - no seroreversion

BRA_HPV16_time_no_seroreversion_plot <- plot_seromodel(
  seromodel = BRA_HPV16_time_no_seroreversion,
  serosurvey = BRA_HPV16,
  size_text = size_text
)

plot(BRA_HPV16_time_no_seroreversion_plot)

## Edad - no seroreversion

BRA_HPV16_age_no_seroreversion_plot <- plot_seromodel(
  seromodel = BRA_HPV16_age_no_seroreversion,
  serosurvey = BRA_HPV16,
  size_text = size_text
)
plot(BRA_HPV16_age_no_seroreversion_plot)

## Edad - seroreversion

BRA_HPV16_age_seroreversion_plot <- plot_seromodel(
  seromodel = BRA_HPV16_age_seroreversion,
  serosurvey = BRA_HPV16,
  size_text = size_text
)

plot(BRA_HPV16_age_seroreversion_plot)

# Tabla de resumen

## Tabla para cada modelo

BRA_HPV16_constant_no_seroreversion_summary <- summarise_seromodel(
  seromodel = BRA_HPV16_constant_no_seroreversion,
  serosurvey = BRA_HPV16
) %>% as.data.frame()

BRA_HPV16_constant_seroreversion_summary <- summarise_seromodel(
  seromodel = BRA_HPV16_constant_seroreversion,
  serosurvey = BRA_HPV16
) %>% as.data.frame()

BRA_HPV16_time_no_seroreversion_summary <- summarise_seromodel(
  seromodel = BRA_HPV16_time_no_seroreversion,
  serosurvey = BRA_HPV16
) %>% as.data.frame()

BRA_HPV16_age_no_seroreversion_summary <- summarise_seromodel(
  seromodel = BRA_HPV16_age_no_seroreversion,
  serosurvey = BRA_HPV16
) %>% as.data.frame()

BRA_HPV16_age_seroreversion_summary <- summarise_seromodel(
  seromodel = BRA_HPV16_age_seroreversion,
  serosurvey = BRA_HPV16
) %>% as.data.frame()

## Tabla de todos los modelos

BRA_HPV16_summary <- dplyr::bind_rows(
  BRA_HPV16_constant_no_seroreversion_summary,
  BRA_HPV16_constant_seroreversion_summary,
  BRA_HPV16_time_no_seroreversion_summary,
  BRA_HPV16_age_no_seroreversion_summary,
  BRA_HPV16_age_seroreversion_summary
)

write.xlsx(BRA_HPV16_summary, file = "SEROFOI/data frame/BRA_HPV16_summary.xlsx")

# Gráfica de seroprevalencia
size_text <- 8

## Constante - no seroreversion

BRA_HPV16_constant_no_seroreversion_plot_seroprev <- plot_seroprevalence_estimates(
  seromodel = BRA_HPV16_constant_no_seroreversion,
  serosurvey = BRA_HPV16,
  size_text = size_text
) +
  ggtitle("constant - no seroreversion")


## Constante - seroreversion

BRA_HPV16_constant_seroreversion_plot_seroprev <- plot_seroprevalence_estimates(
  seromodel = BRA_HPV16_constant_seroreversion,
  serosurvey = BRA_HPV16,
  size_text = size_text
) +
  ggtitle("constant - seroreversion")

## Tiempo - no seroreversion

BRA_HPV16_time_no_seroreversion_plot_seroprev <- plot_seroprevalence_estimates(
  seromodel = BRA_HPV16_time_no_seroreversion,
  serosurvey = BRA_HPV16,
  size_text = size_text
) +
  ggtitle("time - no seroreversion")

## Edad - no seroreversion

BRA_HPV16_age_no_seroreversion_plot_seroprev <- plot_seroprevalence_estimates(
  seromodel = BRA_HPV16_age_no_seroreversion,
  serosurvey = BRA_HPV16,
  size_text = size_text
) +
  ggtitle("age - no seroreversion")

## Edad - seroreversion

BRA_HPV16_age_seroreversion_plot_seroprev <- plot_seroprevalence_estimates(
  seromodel = BRA_HPV16_age_seroreversion,
  serosurvey = BRA_HPV16,
  size_text = size_text
)+
  ggtitle("age - seroreversion")

## Grafica de seroprevalencia conjunta

BRA_HPV16_plot_seroprev <- cowplot::plot_grid(
  BRA_HPV16_constant_no_seroreversion_plot_seroprev,
  BRA_HPV16_constant_seroreversion_plot_seroprev,
  BRA_HPV16_time_no_seroreversion_plot_seroprev,
  BRA_HPV16_age_no_seroreversion_plot_seroprev,
  BRA_HPV16_age_seroreversion_plot_seroprev,
  nrow = 1,
  align = "hv"
)

# Gráfica de foi

foi_max <- 1.2

# Tiempo - no seroreversion

BRA_HPV16_time_no_seroreversion_plot_foi <- plot_foi_estimates(
  seromodel = BRA_HPV16_time_no_seroreversion,
  serosurvey = BRA_HPV16,
  size_text = size_text,
  foi_max = foi_max
)

## Edad - no seroreversion
BRA_HPV16_age_no_seroreversion_plot_foi <- plot_foi_estimates(
  seromodel = BRA_HPV16_age_no_seroreversion,
  serosurvey = BRA_HPV16,
  size_text = size_text,
  foi_max = foi_max
)

## Edad - seroreversion
BRA_HPV16_age_seroreversion_plot_foi <- plot_foi_estimates(
  seromodel = BRA_HPV16_age_seroreversion,
  serosurvey = BRA_HPV16,
  size_text = size_text,
  foi_max = foi_max
)

## Grafica conjunta foi
empty_plot <- ggplot() + theme_void()

BRA_HPV16_plot_foi <- cowplot::plot_grid(
  empty_plot,
  empty_plot,
  BRA_HPV16_time_no_seroreversion_plot_foi,
  BRA_HPV16_age_no_seroreversion_plot_foi,
  BRA_HPV16_age_seroreversion_plot_foi,
  ncol = 5,
  nrow = 1,
  align = "hv"
)

# Gráfica de rhats

## Tiempo - no seroreversion
BRA_HPV16_time_no_seroreversion_plot_rhats <- serofoi:::plot_rhats(
  seromodel = BRA_HPV16_time_no_seroreversion,
  serosurvey = BRA_HPV16,
  size_text = size_text
)

## Edad - no seroreversion
BRA_HPV16_age_no_seroreversion_plot_rhats <- serofoi:::plot_rhats(
  seromodel = BRA_HPV16_age_no_seroreversion,
  serosurvey = BRA_HPV16,
  size_text = size_text
)

## Edad - seroreversion
BRA_HPV16_age_seroreversion_plot_rhats <- serofoi:::plot_rhats(
  seromodel = BRA_HPV16_age_seroreversion,
  serosurvey = BRA_HPV16,
  size_text = size_text
)

## Grafica conjunta foi
empty_plot <- ggplot() + theme_void()

BRA_HPV16_plot_rhats <- cowplot::plot_grid(
  empty_plot,
  empty_plot,
  BRA_HPV16_time_no_seroreversion_plot_rhats,
  BRA_HPV16_age_no_seroreversion_plot_rhats,
  BRA_HPV16_age_seroreversion_plot_rhats,
  ncol = 5,
  nrow = 1,
  align = "hv"
)

# Gráfica conjunta  BRA_HPV16

BRA_HPV16_plot <- cowplot::plot_grid(
  BRA_HPV16_plot_seroprev,
  BRA_HPV16_plot_foi,
  BRA_HPV16_plot_rhats,
  nrow = 3,
  align = "hv"
)

plot(BRA_HPV16_plot)
jpeg(filename = "SEROFOI/plots/all_BRA_HPV16_.jpeg", width = 11, height = 8, units = "in", res = 300) 
BRA_HPV16_plot
dev.off()




### SEROENCUESTA PUERTO RICO HPV 16/18  ###


#CREAR DATA.FRAME DE SEROENCUESTA ## 

PRI_HPVHr<- dat %>% filter( survey == "PRI-001-02"
) %>%
  
  ## SELECCCION DE DATOS QUE SE REQUIEREN PARA LOS MODELOS ##
  select(survey, tsur, age_min, age_max, counts, total) %>%
  rename(
    n_seropositive = counts,
    sample_size = total)

plot_serosurvey(PRI_HPVHr)


# Modelo constante sin seroreversion

PRI_HPVHr_constant_no_seroreversion <- fit_seromodel(
  serosurvey = PRI_HPVHr,
  model_type = "constant",
  foi_prior = sf_uniform() ,
)

# Modelo constante con seroreversion

PRI_HPVHr_constant_seroreversion <- fit_seromodel(
  serosurvey = PRI_HPVHr,
  model_type = "constant",
  foi_prior = sf_uniform(),
  is_seroreversion = TRUE,
  seroreversion_prior = sf_uniform(0.0, 2.0),
  iter = 1500
)

# Modelo dependiente del tiempo sin seroreversion

foi_index <- get_foi_index(
  serosurvey = PRI_HPVHr,
  group_size = 10
)

init <- function() {
  list(foi_vector = rep(0.01, max(PRI_HPVHr$age_max)))
}
PRI_HPVHr_time_no_seroreversion <- fit_seromodel(
  serosurvey = PRI_HPVHr,
  model_type = "time",
  foi_prior = sf_normal(),
  is_seroreversion = FALSE,
  init = init,
  iter = 5000,
  thin = 2
)

# Modelo dependiente de la edad sin seroreversion

foi_index <- c(
  rep(1, 15),
  16:max(PRI_HPVHr$age_max)
)
init <- function() {
  list(foi_vector = rep(0.01, max(foi_index)))
}
PRI_HPVHr_age_no_seroreversion <- fit_seromodel(
  serosurvey = PRI_HPVHr,
  model_type = "age",
  foi_prior = sf_normal(1e-4, 1e-5),
  foi_index = foi_index,
  is_seroreversion = FALSE,
  init = init
)

# Modelo dependiente de la edad con seroreversion

foi_index <- c(
  rep(1, 15),
  16:max(PRI_HPVHr$age_max)
)

init <- function() {
  list(foi_vector = rep(0.01, max(foi_index)))
}

PRI_HPVHr_age_seroreversion <- fit_seromodel(
  serosurvey = PRI_HPVHr,
  model_type = "age",
  foi_prior = sf_normal(1e-4, 1e-5),
  foi_index = foi_index,
  is_seroreversion = TRUE,
  seroreversion_prior = sf_normal(1, 0.5),
  iter = 5000,
  thin = 2
)

# Gráfica conjunta de todos los modelos

size_text <- 8

## Constante - no seroreversion

PRI_HPVHr_constant_no_seroreversion_plot <- plot_seromodel(
  seromodel = PRI_HPVHr_constant_no_seroreversion,
  serosurvey = PRI_HPVHr,
  size_text = size_text
)
plot(PRI_HPVHr_constant_no_seroreversion_plot)

## Constante - seroreversion

PRI_HPVHr_constant_seroreversion_plot <- plot_seromodel(
  seromodel = PRI_HPVHr_constant_seroreversion,
  serosurvey = PRI_HPVHr,
  size_text = size_text
)
plot(PRI_HPVHr_constant_seroreversion_plot)

## Tiempo - no seroreversion

PRI_HPVHr_time_no_seroreversion_plot <- plot_seromodel(
  seromodel = PRI_HPVHr_time_no_seroreversion,
  serosurvey = PRI_HPVHr,
  size_text = size_text
)
plot(PRI_HPVHr_time_no_seroreversion_plot)

## Edad - no seroreversion

PRI_HPVHr_age_no_seroreversion_plot <- plot_seromodel(
  seromodel = PRI_HPVHr_age_no_seroreversion,
  serosurvey = PRI_HPVHr,
  size_text = size_text
)
plot(PRI_HPVHr_age_no_seroreversion_plot)

## Edad - seroreversion

PRI_HPVHr_age_seroreversion_plot <- plot_seromodel(
  seromodel = PRI_HPVHr_age_seroreversion,
  serosurvey = PRI_HPVHr,
  size_text = size_text
)
plot(PRI_HPVHr_age_seroreversion_plot)


# Tabla de resumen

## Tabla para cada modelo
PRI_HPVHr_constant_no_seroreversion_summary <- summarise_seromodel(
  seromodel = PRI_HPVHr_constant_no_seroreversion,
  serosurvey = PRI_HPVHr
) %>% as.data.frame()

PRI_HPVHr_constant_seroreversion_summary <- summarise_seromodel(
  seromodel = PRI_HPVHr_constant_seroreversion,
  serosurvey = PRI_HPVHr
) %>% as.data.frame()

PRI_HPVHr_time_no_seroreversion_summary <- summarise_seromodel(
  seromodel = PRI_HPVHr_time_no_seroreversion,
  serosurvey = PRI_HPVHr
) %>% as.data.frame()

PRI_HPVHr_age_no_seroreversion_summary <- summarise_seromodel(
  seromodel = PRI_HPVHr_age_no_seroreversion,
  serosurvey = PRI_HPVHr
) %>% as.data.frame()

PRI_HPVHr_age_seroreversion_summary <- summarise_seromodel(
  seromodel = PRI_HPVHr_age_seroreversion,
  serosurvey = PRI_HPVHr
) %>% as.data.frame()

## Tabla de todos los modelos

PRI_HPVHr_summary <- dplyr::bind_rows(
  PRI_HPVHr_constant_no_seroreversion_summary,
  PRI_HPVHr_constant_seroreversion_summary,
  PRI_HPVHr_time_no_seroreversion_summary,
  PRI_HPVHr_age_no_seroreversion_summary,
  PRI_HPVHr_age_seroreversion_summary
)

write.xlsx(PRI_HPVHr_summary, file = "SEROFOI/data frame/PRI_HPVHr_summary.xlsx")
# Gráfica de seroprevalencia
size_text <- 8

## Constante - no seroreversion

PRI_HPVHr_constant_no_seroreversion_plot_seroprev <- plot_seroprevalence_estimates(
  seromodel = PRI_HPVHr_constant_no_seroreversion,
  serosurvey = PRI_HPVHr,
  size_text = size_text
) +
  ggtitle("constant - no seroreversion")


## Constante - seroreversion

PRI_HPVHr_constant_seroreversion_plot_seroprev <- plot_seroprevalence_estimates(
  seromodel = PRI_HPVHr_constant_seroreversion,
  serosurvey = PRI_HPVHr,
  size_text = size_text
) +
  ggtitle("constant - seroreversion")

## Tiempo - no seroreversion

PRI_HPVHr_time_no_seroreversion_plot_seroprev <- plot_seroprevalence_estimates(
  seromodel = PRI_HPVHr_time_no_seroreversion,
  serosurvey = PRI_HPVHr,
  size_text = size_text
) +
  ggtitle("time - no seroreversion")

## Edad - no seroreversion

PRI_HPVHr_age_no_seroreversion_plot_seroprev <- plot_seroprevalence_estimates(
  seromodel = PRI_HPVHr_age_no_seroreversion,
  serosurvey = PRI_HPVHr,
  size_text = size_text
) +
  ggtitle("age - no seroreversion")

## Edad - seroreversion

PRI_HPVHr_age_seroreversion_plot_seroprev <- plot_seroprevalence_estimates(
  seromodel = PRI_HPVHr_age_seroreversion,
  serosurvey = PRI_HPVHr,
  size_text = size_text
)+
  ggtitle("age - seroreversion")

## Grafica de seroprevalencia conjunta

PRI_HPVHr_plot_seroprev <- cowplot::plot_grid(
  PRI_HPVHr_constant_no_seroreversion_plot_seroprev,
  PRI_HPVHr_constant_seroreversion_plot_seroprev,
  PRI_HPVHr_time_no_seroreversion_plot_seroprev,
  PRI_HPVHr_age_no_seroreversion_plot_seroprev,
  PRI_HPVHr_age_seroreversion_plot_seroprev,
  nrow = 1,
  align = "hv"
)

# Gráfica de foi
foi_max <- 1.0

# Tiempo - no seroreversion

PRI_HPVHr_time_no_seroreversion_plot_foi <- plot_foi_estimates(
  seromodel = PRI_HPVHr_time_no_seroreversion,
  serosurvey = PRI_HPVHr,
  size_text = size_text,
  foi_max = foi_max
)

## Edad - no seroreversion
PRI_HPVHr_age_no_seroreversion_plot_foi <- plot_foi_estimates(
  seromodel = PRI_HPVHr_age_no_seroreversion,
  serosurvey = PRI_HPVHr,
  size_text = size_text,
  foi_max = foi_max
)

## Edad - seroreversion
PRI_HPVHr_age_seroreversion_plot_foi <- plot_foi_estimates(
  seromodel = PRI_HPVHr_age_seroreversion,
  serosurvey = PRI_HPVHr,
  size_text = size_text,
  foi_max = foi_max
)

## Grafica conjunta foi
empty_plot <- ggplot() + theme_void()

PRI_HPVHr_plot_foi <- cowplot::plot_grid(
  empty_plot,
  empty_plot,
  PRI_HPVHr_time_no_seroreversion_plot_foi,
  PRI_HPVHr_age_no_seroreversion_plot_foi,
  PRI_HPVHr_age_seroreversion_plot_foi,
  ncol = 5,
  nrow = 1,
  align = "hv"
)

# Gráfica de rhats

## Tiempo - no seroreversion
PRI_HPVHr_time_no_seroreversion_plot_rhats <- serofoi:::plot_rhats(
  seromodel = PRI_HPVHr_time_no_seroreversion,
  serosurvey = PRI_HPVHr,
  size_text = size_text
)

## Edad - no seroreversion
PRI_HPVHr_age_no_seroreversion_plot_rhats <- serofoi:::plot_rhats(
  seromodel = PRI_HPVHr_age_no_seroreversion,
  serosurvey = PRI_HPVHr,
  size_text = size_text
)

## Edad - seroreversion
PRI_HPVHr_age_seroreversion_plot_rhats <- serofoi:::plot_rhats(
  seromodel = PRI_HPVHr_age_seroreversion,
  serosurvey = PRI_HPVHr,
  size_text = size_text
)

## Grafica conjunta foi
empty_plot <- ggplot() + theme_void()

PRI_HPVHr_plot_rhats <- cowplot::plot_grid(
  empty_plot,
  empty_plot,
  PRI_HPVHr_time_no_seroreversion_plot_rhats,
  PRI_HPVHr_age_no_seroreversion_plot_rhats,
  PRI_HPVHr_age_seroreversion_plot_rhats,
  ncol = 5,
  nrow = 1,
  align = "hv"
)

# Gráfica conjunta col PRI_HPVHr

PRI_HPVHr_plot <- cowplot::plot_grid(
  PRI_HPVHr_plot_seroprev,
  PRI_HPVHr_plot_foi,
  PRI_HPVHr_plot_rhats,
  nrow = 3,
  align = "hv"
)

plot(PRI_HPVHr_plot)
jpeg(filename = "SEROFOI/plots/all_PRI_HPVHr_.jpeg", width = 11, height = 8, units = "in", res = 300) 
PRI_HPVHr_plot
dev.off()




### SEROENCUESTA ESTADOS UNIDOS HPV 16 ###

#CREAR DATA.FRAME DE SEROENCUESTA ## 

USA92_HPV16 <- dat %>% filter( survey == "USA-011-03"
) %>%
  
## SELECCCION DE DATOS QUE SE REQUIEREN PARA LOS MODELOS ##
  select(survey, tsur, age_min, age_max, counts, total) %>%
  rename(
    n_seropositive = counts,
    sample_size = total)

plot_serosurvey(USA92_HPV16)

## Modelo constante sin seroreversion

USA92_HPV16_constant_no_seroreversion <- fit_seromodel(
  serosurvey = USA92_HPV16,
  model_type = "constant",
  foi_prior = sf_uniform() ,
)

# Modelo constante con seroreversion

USA92_HPV16_constant_seroreversion <- fit_seromodel(
  serosurvey = USA92_HPV16,
  model_type = "constant",
  foi_prior = sf_uniform(),
  is_seroreversion = TRUE,
  seroreversion_prior = sf_uniform(0.0, 2.0),
  iter =2000
)

# Modelo dependiente del tiempo sin seroreversion

foi_index <- get_foi_index(
  serosurvey = USA92_HPV16,
  group_size = 10
)

init <- function() {
  list(foi_vector = rep(0.01, max(USA92_HPV16$age_max)))
}
USA92_HPV16_time_no_seroreversion <- fit_seromodel(
  serosurvey = USA92_HPV16,
  model_type = "time",
  foi_prior = sf_normal(),
  is_seroreversion = FALSE,
  init = init,
  iter = 5000,
  thin = 2
)

# Modelo dependiente de la edad sin seroreversion

foi_index <- c(
  rep(1, 15),
  16:max(USA92_HPV16$age_max)
)
init <- function() {
  list(foi_vector = rep(0.01, max(foi_index)))
}
USA92_HPV16_age_no_seroreversion <- fit_seromodel(
  serosurvey = USA92_HPV16,
  model_type = "age",
  foi_prior = sf_normal(1e-4, 1e-5),
  foi_index = foi_index,
  is_seroreversion = FALSE,
  init = init
)

# Modelo dependiente de la edad con seroreversion

foi_index <- c(
  rep(1, 15),
  16:max(USA92_HPV16$age_max)
)

init <- function() {
  list(foi_vector = rep(0.01, max(foi_index)))
}

USA92_HPV16_age_seroreversion <- fit_seromodel(
  serosurvey = USA92_HPV16,
  model_type = "age",
  foi_prior = sf_normal(1e-4, 1e-5),
  foi_index = foi_index,
  is_seroreversion = TRUE,
  seroreversion_prior = sf_normal(1, 0.5),
  iter = 5000,
  thin = 2
)

# Gráfica conjunta de todos los modelos

size_text <- 8

## Constante - no seroreversion

USA92_HPV16_constant_no_seroreversion_plot <- plot_seromodel(
  seromodel = USA92_HPV16_constant_no_seroreversion,
  serosurvey = USA92_HPV16,
  size_text = size_text
)
plot(USA92_HPV16_constant_no_seroreversion_plot)

## Constante - seroreversion

USA92_HPV16_constant_seroreversion_plot <- plot_seromodel(
  seromodel = USA92_HPV16_constant_seroreversion,
  serosurvey = USA92_HPV16,
  size_text = size_text
)
plot(USA92_HPV16_constant_seroreversion_plot)

## Tiempo - no seroreversion

USA92_HPV16_time_no_seroreversion_plot <- plot_seromodel(
  seromodel = USA92_HPV16_time_no_seroreversion,
  serosurvey = USA92_HPV16,
  size_text = size_text
)

plot(USA92_HPV16_time_no_seroreversion_plot)

## Edad - no seroreversion

USA92_HPV16_age_no_seroreversion_plot <- plot_seromodel(
  seromodel = USA92_HPV16_age_no_seroreversion,
  serosurvey = USA92_HPV16,
  size_text = size_text
)
plot(USA92_HPV16_age_no_seroreversion_plot)

## Edad - seroreversion

USA92_HPV16_age_seroreversion_plot <- plot_seromodel(
  seromodel = USA92_HPV16_age_seroreversion,
  serosurvey = USA92_HPV16,
  size_text = size_text
)

plot(USA92_HPV16_age_seroreversion_plot)

# Tabla de resumen

## Tabla para cada modelo
USA92_HPV16_constant_no_seroreversion_summary <- summarise_seromodel(
  seromodel = USA92_HPV16_constant_no_seroreversion,
  serosurvey = USA92_HPV16
) %>% as.data.frame()

USA92_HPV16_constant_seroreversion_summary <- summarise_seromodel(
  seromodel = USA92_HPV16_constant_seroreversion,
  serosurvey = USA92_HPV16
) %>% as.data.frame()

USA92_HPV16_time_no_seroreversion_summary <- summarise_seromodel(
  seromodel = USA92_HPV16_time_no_seroreversion,
  serosurvey = USA92_HPV16
) %>% as.data.frame()

USA92_HPV16_age_no_seroreversion_summary <- summarise_seromodel(
  seromodel = USA92_HPV16_age_no_seroreversion,
  serosurvey = USA92_HPV16
) %>% as.data.frame()

USA92_HPV16_age_seroreversion_summary <- summarise_seromodel(
  seromodel = USA92_HPV16_age_seroreversion,
  serosurvey = USA92_HPV16
) %>% as.data.frame()

## Tabla de todos los modelos

USA92_HPV16_summary <- dplyr::bind_rows(
  USA92_HPV16_constant_no_seroreversion_summary,
  USA92_HPV16_constant_seroreversion_summary,
  USA92_HPV16_time_no_seroreversion_summary,
  USA92_HPV16_age_no_seroreversion_summary,
  USA92_HPV16_age_seroreversion_summary
)

write.xlsx(USA92_HPV16_summary, file = "SEROFOI/data frame/USA92_HPV16_summary.xlsx")

# Gráfica de seroprevalencia
size_text <- 8

## Constante - no seroreversion

USA92_HPV16_constant_no_seroreversion_plot_seroprev <- plot_seroprevalence_estimates(
  seromodel = USA92_HPV16_constant_no_seroreversion,
  serosurvey = USA92_HPV16,
  size_text = size_text
) +
  ggtitle("constant - no seroreversion")


## Constante - seroreversion

USA92_HPV16_constant_seroreversion_plot_seroprev <- plot_seroprevalence_estimates(
  seromodel = USA92_HPV16_constant_seroreversion,
  serosurvey = USA92_HPV16,
  size_text = size_text
) +
  ggtitle("constant - seroreversion")

## Tiempo - no seroreversion

USA92_HPV16_time_no_seroreversion_plot_seroprev <- plot_seroprevalence_estimates(
  seromodel = USA92_HPV16_time_no_seroreversion,
  serosurvey = USA92_HPV16,
  size_text = size_text
) +
  ggtitle("time - no seroreversion")

## Edad - no seroreversion

USA92_HPV16_age_no_seroreversion_plot_seroprev <- plot_seroprevalence_estimates(
  seromodel = USA92_HPV16_age_no_seroreversion,
  serosurvey = USA92_HPV16,
  size_text = size_text
) +
  ggtitle("age - no seroreversion")

## Edad - seroreversion

USA92_HPV16_age_seroreversion_plot_seroprev <- plot_seroprevalence_estimates(
  seromodel = USA92_HPV16_age_seroreversion,
  serosurvey = USA92_HPV16,
  size_text = size_text
)+
  ggtitle("age - seroreversion")

## Grafica de seroprevalencia conjunta

USA92_HPV16_plot_seroprev <- cowplot::plot_grid(
  USA92_HPV16_constant_no_seroreversion_plot_seroprev,
  USA92_HPV16_constant_seroreversion_plot_seroprev,
  USA92_HPV16_time_no_seroreversion_plot_seroprev,
  USA92_HPV16_age_no_seroreversion_plot_seroprev,
  USA92_HPV16_age_seroreversion_plot_seroprev,
  nrow = 1,
  align = "hv"
)

# Gráfica de foi
foi_max <- 0.05

# Tiempo - no seroreversion

USA92_HPV16_time_no_seroreversion_plot_foi <- plot_foi_estimates(
  seromodel = USA92_HPV16_time_no_seroreversion,
  serosurvey = USA92_HPV16,
  size_text = size_text,
  foi_max = foi_max
)

## Edad - no seroreversion
USA92_HPV16_age_no_seroreversion_plot_foi <- plot_foi_estimates(
  seromodel = USA92_HPV16_age_no_seroreversion,
  serosurvey = USA92_HPV16,
  size_text = size_text,
  foi_max = foi_max
)

## Edad - seroreversion
USA92_HPV16_age_seroreversion_plot_foi <- plot_foi_estimates(
  seromodel = USA92_HPV16_age_seroreversion,
  serosurvey = USA92_HPV16,
  size_text = size_text,
  foi_max = foi_max
)

## Grafica conjunta foi
empty_plot <- ggplot() + theme_void()

USA92_HPV16_plot_foi <- cowplot::plot_grid(
  empty_plot,
  empty_plot,
  USA92_HPV16_time_no_seroreversion_plot_foi,
  USA92_HPV16_age_no_seroreversion_plot_foi,
  USA92_HPV16_age_seroreversion_plot_foi,
  ncol = 5,
  nrow = 1,
  align = "hv"
)

# Gráfica de rhats

## Tiempo - no seroreversion
USA92_HPV16_time_no_seroreversion_plot_rhats <- serofoi:::plot_rhats(
  seromodel = USA92_HPV16_time_no_seroreversion,
  serosurvey = USA92_HPV16,
  size_text = size_text
)

## Edad - no seroreversion
USA92_HPV16_age_no_seroreversion_plot_rhats <- serofoi:::plot_rhats(
  seromodel = USA92_HPV16_age_no_seroreversion,
  serosurvey = USA92_HPV16,
  size_text = size_text
)

## Edad - seroreversion
USA92_HPV16_age_seroreversion_plot_rhats <- serofoi:::plot_rhats(
  seromodel = USA92_HPV16_age_seroreversion,
  serosurvey = USA92_HPV16,
  size_text = size_text
)

## Grafica conjunta foi
empty_plot <- ggplot() + theme_void()

USA92_HPV16_plot_rhats <- cowplot::plot_grid(
  empty_plot,
  empty_plot,
  USA92_HPV16_time_no_seroreversion_plot_rhats,
  USA92_HPV16_age_no_seroreversion_plot_rhats,
  USA92_HPV16_age_seroreversion_plot_rhats,
  ncol = 5,
  nrow = 1,
  align = "hv"
)

# Gráfica conjunta col USA92_HPV16

USA92_HPV16_plot <- cowplot::plot_grid(
  USA92_HPV16_plot_seroprev,
  USA92_HPV16_plot_foi,
  USA92_HPV16_plot_rhats,
  nrow = 3,
  align = "hv"
)

plot(USA92_HPV16_plot)
jpeg(filename = "SEROFOI/plots/all_USA92_HPV16_.jpeg", width = 11, height = 8, units = "in", res = 300) 
USA92_HPV16_plot
dev.off()





### SEROENCUESTA ESTADO UNIDOS (2003) HPV 18  ###

#CREAR DATA.FRAME DE SEROENCUESTA ## 

USA_HPV18<- dat %>% filter( survey == "USA-026-04"
) %>%
  
  ## SELECCCION DE DATOS QUE SE REQUIEREN PARA LOS MODELOS ##
  select(survey, tsur, age_min, age_max, counts, total) %>%
  rename(
    n_seropositive = counts,
    sample_size = total)

plot_serosurvey(USA_HPV18)


# Modelo constante sin seroreversion

USA_HPV18_constant_no_seroreversion <- fit_seromodel(
  serosurvey = USA_HPV18,
  model_type = "constant",
  foi_prior = sf_uniform() ,
)

# Modelo constante con seroreversion

USA_HPV18_constant_seroreversion <- fit_seromodel(
  serosurvey = USA_HPV18,
  model_type = "constant",
  foi_prior = sf_uniform(),
  is_seroreversion = TRUE,
  seroreversion_prior = sf_uniform(0.0, 2.0),
  iter = 1500
)

# Modelo dependiente del tiempo sin seroreversion

foi_index <- get_foi_index(
  serosurvey = USA_HPV18,
  group_size = 10
)

init <- function() {
  list(foi_vector = rep(0.01, max(USA_HPV18$age_max)))
}
USA_HPV18_time_no_seroreversion <- fit_seromodel(
  serosurvey = USA_HPV18,
  model_type = "time",
  foi_prior = sf_normal(),
  is_seroreversion = FALSE,
  init = init,
  iter = 5000,
  thin = 2
)

# Modelo dependiente de la edad sin seroreversion

foi_index <- c(
  rep(1, 15),
  16:max(USA_HPV18$age_max)
)
init <- function() {
  list(foi_vector = rep(0.01, max(foi_index)))
}
USA_HPV18_age_no_seroreversion <- fit_seromodel(
  serosurvey = USA_HPV18,
  model_type = "age",
  foi_prior = sf_normal(1e-4, 1e-5),
  foi_index = foi_index,
  is_seroreversion = FALSE,
  init = init
)

# Modelo dependiente de la edad con seroreversion

foi_index <- c(
  rep(1, 15),
  16:max(USA_HPV18$age_max)
)

init <- function() {
  list(foi_vector = rep(0.01, max(foi_index)))
}

USA_HPV18_age_seroreversion <- fit_seromodel(
  serosurvey = USA_HPV18,
  model_type = "age",
  foi_prior = sf_normal(1e-4, 1e-5),
  foi_index = foi_index,
  is_seroreversion = TRUE,
  seroreversion_prior = sf_normal(1, 0.5),
  iter = 6000,
  thin = 2
)


# Gráfica conjunta de todos los modelos

size_text <- 8

## Constante - no seroreversion

USA_HPV18_constant_no_seroreversion_plot <- plot_seromodel(
  seromodel = USA_HPV18_constant_no_seroreversion,
  serosurvey = USA_HPV18,
  size_text = size_text
)
plot(USA_HPV18_constant_no_seroreversion_plot)

## Constante - seroreversion

USA_HPV18_constant_seroreversion_plot <- plot_seromodel(
  seromodel = USA_HPV18_constant_seroreversion,
  serosurvey = USA_HPV18,
  size_text = size_text
)
plot(USA_HPV18_constant_seroreversion_plot)

## Tiempo - no seroreversion

USA_HPV18_time_no_seroreversion_plot <- plot_seromodel(
  seromodel = USA_HPV18_time_no_seroreversion,
  serosurvey = USA_HPV18,
  size_text = size_text
)
plot(USA_HPV18_time_no_seroreversion_plot)

## Edad - no seroreversion

USA_HPV18_age_no_seroreversion_plot <- plot_seromodel(
  seromodel = USA_HPV18_age_no_seroreversion,
  serosurvey = USA_HPV18,
  size_text = size_text
)
plot(USA_HPV18_age_no_seroreversion_plot)

## Edad - seroreversion

USA_HPV18_age_seroreversion_plot <- plot_seromodel(
  seromodel = USA_HPV18_age_seroreversion,
  serosurvey = USA_HPV18,
  size_text = size_text
)
plot(USA_HPV18_age_seroreversion_plot)


# Tabla de resumen

## Tabla para cada modelo
USA_HPV18_constant_no_seroreversion_summary <- summarise_seromodel(
  seromodel = USA_HPV18_constant_no_seroreversion,
  serosurvey = USA_HPV18
) %>% as.data.frame()

USA_HPV18_constant_seroreversion_summary <- summarise_seromodel(
  seromodel = USA_HPV18_constant_seroreversion,
  serosurvey = USA_HPV18
) %>% as.data.frame()

USA_HPV18_time_no_seroreversion_summary <- summarise_seromodel(
  seromodel = USA_HPV18_time_no_seroreversion,
  serosurvey = USA_HPV18
) %>% as.data.frame()

USA_HPV18_age_no_seroreversion_summary <- summarise_seromodel(
  seromodel = USA_HPV18_age_no_seroreversion,
  serosurvey = USA_HPV18
) %>% as.data.frame()

USA_HPV18_age_seroreversion_summary <- summarise_seromodel(
  seromodel = USA_HPV18_age_seroreversion,
  serosurvey = USA_HPV18
) %>% as.data.frame()

## Tabla de todos los modelos

USA_HPV18_summary <- dplyr::bind_rows(
  USA_HPV18_constant_no_seroreversion_summary,
  USA_HPV18_constant_seroreversion_summary,
  USA_HPV18_time_no_seroreversion_summary,
  USA_HPV18_age_no_seroreversion_summary,
  USA_HPV18_age_seroreversion_summary
)

write.xlsx(USA_HPV18_summary, file = "SEROFOI/data frame/USA_HPV18_summary.xlsx")

# Gráfica de seroprevalencia
size_text <- 8

## Constante - no seroreversion

USA_HPV18_constant_no_seroreversion_plot_seroprev <- plot_seroprevalence_estimates(
  seromodel = USA_HPV18_constant_no_seroreversion,
  serosurvey = USA_HPV18,
  size_text = size_text
) +
  ggtitle("constant - no seroreversion")


## Constante - seroreversion

USA_HPV18_constant_seroreversion_plot_seroprev <- plot_seroprevalence_estimates(
  seromodel = USA_HPV18_constant_seroreversion,
  serosurvey = USA_HPV18,
  size_text = size_text
) +
  ggtitle("constant - seroreversion")

## Tiempo - no seroreversion

USA_HPV18_time_no_seroreversion_plot_seroprev <- plot_seroprevalence_estimates(
  seromodel = USA_HPV18_time_no_seroreversion,
  serosurvey = USA_HPV18,
  size_text = size_text
) +
  ggtitle("time - no seroreversion")

## Edad - no seroreversion

USA_HPV18_age_no_seroreversion_plot_seroprev <- plot_seroprevalence_estimates(
  seromodel = USA_HPV18_age_no_seroreversion,
  serosurvey = USA_HPV18,
  size_text = size_text
) +
  ggtitle("age - no seroreversion")

## Edad - seroreversion

USA_HPV18_age_seroreversion_plot_seroprev <- plot_seroprevalence_estimates(
  seromodel = USA_HPV18_age_seroreversion,
  serosurvey = USA_HPV18,
  size_text = size_text
)+
  ggtitle("age - seroreversion")

## Grafica de seroprevalencia conjunta

USA_HPV18_plot_seroprev <- cowplot::plot_grid(
  USA_HPV18_constant_no_seroreversion_plot_seroprev,
  USA_HPV18_constant_seroreversion_plot_seroprev,
  USA_HPV18_time_no_seroreversion_plot_seroprev,
  USA_HPV18_age_no_seroreversion_plot_seroprev,
  USA_HPV18_age_seroreversion_plot_seroprev,
  nrow = 1,
  align = "hv"
)

# Gráfica de foi
foi_max <- 0.12

# Tiempo - no seroreversion

USA_HPV18_time_no_seroreversion_plot_foi <- plot_foi_estimates(
  seromodel = USA_HPV18_time_no_seroreversion,
  serosurvey = USA_HPV18,
  size_text = size_text,
  foi_max = foi_max
)

## Edad - no seroreversion
USA_HPV18_age_no_seroreversion_plot_foi <- plot_foi_estimates(
  seromodel = USA_HPV18_age_no_seroreversion,
  serosurvey = USA_HPV18,
  size_text = size_text,
  foi_max = foi_max
)

## Edad - seroreversion
USA_HPV18_age_seroreversion_plot_foi <- plot_foi_estimates(
  seromodel = USA_HPV18_age_seroreversion,
  serosurvey = USA_HPV18,
  size_text = size_text,
  foi_max = foi_max
)

## Grafica conjunta foi
empty_plot <- ggplot() + theme_void()

USA_HPV18_plot_foi <- cowplot::plot_grid(
  empty_plot,
  empty_plot,
  USA_HPV18_time_no_seroreversion_plot_foi,
  USA_HPV18_age_no_seroreversion_plot_foi,
  USA_HPV18_age_seroreversion_plot_foi,
  ncol = 5,
  nrow = 1,
  align = "hv"
)

# Gráfica de rhats

## Tiempo - no seroreversion
USA_HPV18_time_no_seroreversion_plot_rhats <- serofoi:::plot_rhats(
  seromodel = USA_HPV18_time_no_seroreversion,
  serosurvey = USA_HPV18,
  size_text = size_text
)

## Edad - no seroreversion
USA_HPV18_age_no_seroreversion_plot_rhats <- serofoi:::plot_rhats(
  seromodel = USA_HPV18_age_no_seroreversion,
  serosurvey = USA_HPV18,
  size_text = size_text
)

## Edad - seroreversion
USA_HPV18_age_seroreversion_plot_rhats <- serofoi:::plot_rhats(
  seromodel = USA_HPV18_age_seroreversion,
  serosurvey = USA_HPV18,
  size_text = size_text
)

## Grafica conjunta foi
empty_plot <- ggplot() + theme_void()

USA_HPV18_plot_rhats <- cowplot::plot_grid(
  empty_plot,
  empty_plot,
  USA_HPV18_time_no_seroreversion_plot_rhats,
  USA_HPV18_age_no_seroreversion_plot_rhats,
  USA_HPV18_age_seroreversion_plot_rhats,
  ncol = 5,
  nrow = 1,
  align = "hv"
)

# Gráfica conjunta col USA_HPV18

USA_HPV18_plot <- cowplot::plot_grid(
  USA_HPV18_plot_seroprev,
  USA_HPV18_plot_foi,
  USA_HPV18_plot_rhats,
  nrow = 3,
  align = "hv"
)

plot(USA_HPV18_plot)
jpeg(filename = "SEROFOI/plots/all_USA_HPV18_.jpeg", width = 11, height = 8, units = "in", res = 300) 
USA_HPV18_plot
dev.off()





### SEROENCUESTA ESTADO UNIDOS (2003) HPV 16  ###


#CREAR DATA.FRAME DE SEROENCUESTA ## 

USA_HPV16<- dat %>% filter( survey == "USA-026-03"
) %>%
  
  ## SELECCCION DE DATOS QUE SE REQUIEREN PARA LOS MODELOS ##
  select(survey, tsur, age_min, age_max, counts, total) %>%
  rename(
    n_seropositive = counts,
    sample_size = total)

plot_serosurvey(USA_HPV16)

# Modelo constante sin seroreversion

USA_HPV16_constant_no_seroreversion <- fit_seromodel(
  serosurvey = USA_HPV16,
  model_type = "constant",
  foi_prior = sf_uniform() ,
)

# Modelo constante con seroreversion

USA_HPV16_constant_seroreversion <- fit_seromodel(
  serosurvey = USA_HPV16,
  model_type = "constant",
  foi_prior = sf_uniform(),
  is_seroreversion = TRUE,
  seroreversion_prior = sf_uniform(0.0, 2.0),
  iter = 1500
)

# Modelo dependiente del tiempo sin seroreversion

foi_index <- get_foi_index(
  serosurvey = USA_HPV16,
  group_size = 10
)

init <- function() {
  list(foi_vector = rep(0.01, max(USA_HPV16$age_max)))
}
USA_HPV16_time_no_seroreversion <- fit_seromodel(
  serosurvey = USA_HPV16,
  model_type = "time",
  foi_prior = sf_normal(),
  is_seroreversion = FALSE,
  init = init,
  iter = 5000,
  thin = 2
)

# Modelo dependiente de la edad sin seroreversion

foi_index <- c(
  rep(1, 15),
  16:max(USA_HPV16$age_max)
)
init <- function() {
  list(foi_vector = rep(0.01, max(foi_index)))
}
USA_HPV16_age_no_seroreversion <- fit_seromodel(
  serosurvey = USA_HPV16,
  model_type = "age",
  foi_prior = sf_normal(1e-4, 1e-5),
  foi_index = foi_index,
  is_seroreversion = FALSE,
  init = init
)

# Modelo dependiente de la edad con seroreversion

foi_index <- c(
  rep(1, 15),
  16:max(USA_HPV16$age_max)
)

init <- function() {
  list(foi_vector = rep(0.01, max(foi_index)))
}

USA_HPV16_age_seroreversion <- fit_seromodel(
  serosurvey = USA_HPV16,
  model_type = "age",
  foi_prior = sf_normal(1e-4, 1e-5),
  foi_index = foi_index,
  is_seroreversion = TRUE,
  seroreversion_prior = sf_normal(1, 0.5),
  iter = 5000,
  thin = 2
)

# Gráfica conjunta de todos los modelos

size_text <- 8

## Constante - no seroreversion

USA_HPV16_constant_no_seroreversion_plot <- plot_seromodel(
  seromodel = USA_HPV16_constant_no_seroreversion,
  serosurvey = USA_HPV16,
  size_text = size_text
)
plot(USA_HPV16_constant_no_seroreversion_plot)

## Constante - seroreversion

USA_HPV16_constant_seroreversion_plot <- plot_seromodel(
  seromodel = USA_HPV16_constant_seroreversion,
  serosurvey = USA_HPV16,
  size_text = size_text
)
plot(USA_HPV16_constant_seroreversion_plot)

## Tiempo - no seroreversion

USA_HPV16_time_no_seroreversion_plot <- plot_seromodel(
  seromodel = USA_HPV16_time_no_seroreversion,
  serosurvey = USA_HPV16,
  size_text = size_text
)
plot(USA_HPV16_time_no_seroreversion_plot)

## Edad - no seroreversion

USA_HPV16_age_no_seroreversion_plot <- plot_seromodel(
  seromodel = USA_HPV16_age_no_seroreversion,
  serosurvey = USA_HPV16,
  size_text = size_text
)
plot(USA_HPV16_age_no_seroreversion_plot)

## Edad - seroreversion

USA_HPV16_age_seroreversion_plot <- plot_seromodel(
  seromodel = USA_HPV16_age_seroreversion,
  serosurvey = USA_HPV16,
  size_text = size_text
)
plot(USA_HPV16_age_seroreversion_plot)

# Tabla de resumen

## Tabla para cada modelo
USA_HPV16_constant_no_seroreversion_summary <- summarise_seromodel(
  seromodel = USA_HPV16_constant_no_seroreversion,
  serosurvey = USA_HPV16
) %>% as.data.frame()

USA_HPV16_constant_seroreversion_summary <- summarise_seromodel(
  seromodel = USA_HPV16_constant_seroreversion,
  serosurvey = USA_HPV16
) %>% as.data.frame()

USA_HPV16_time_no_seroreversion_summary <- summarise_seromodel(
  seromodel = USA_HPV16_time_no_seroreversion,
  serosurvey = USA_HPV16
) %>% as.data.frame()

USA_HPV16_age_no_seroreversion_summary <- summarise_seromodel(
  seromodel = USA_HPV16_age_no_seroreversion,
  serosurvey = USA_HPV16
) %>% as.data.frame()

USA_HPV16_age_seroreversion_summary <- summarise_seromodel(
  seromodel = USA_HPV16_age_seroreversion,
  serosurvey = USA_HPV16
) %>% as.data.frame()

## Tabla de todos los modelos

USA_HPV16_summary <- dplyr::bind_rows(
  USA_HPV16_constant_no_seroreversion_summary,
  USA_HPV16_constant_seroreversion_summary,
  USA_HPV16_time_no_seroreversion_summary,
  USA_HPV16_age_no_seroreversion_summary,
  USA_HPV16_age_seroreversion_summary
)

write.xlsx(USA_HPV16_summary, file = "SEROFOI/data frame/USA_HPV16_summary.xlsx")


# Gráfica de seroprevalencia
size_text <- 8

## Constante - no seroreversion

USA_HPV16_constant_no_seroreversion_plot_seroprev <- plot_seroprevalence_estimates(
  seromodel = USA_HPV16_constant_no_seroreversion,
  serosurvey = USA_HPV16,
  size_text = size_text
) +
  ggtitle("constant - no seroreversion")


## Constante - seroreversion

USA_HPV16_constant_seroreversion_plot_seroprev <- plot_seroprevalence_estimates(
  seromodel = USA_HPV16_constant_seroreversion,
  serosurvey = USA_HPV16,
  size_text = size_text
) +
  ggtitle("constant - seroreversion")

## Tiempo - no seroreversion

USA_HPV16_time_no_seroreversion_plot_seroprev <- plot_seroprevalence_estimates(
  seromodel = USA_HPV16_time_no_seroreversion,
  serosurvey = USA_HPV16,
  size_text = size_text
) +
  ggtitle("time - no seroreversion")

## Edad - no seroreversion

USA_HPV16_age_no_seroreversion_plot_seroprev <- plot_seroprevalence_estimates(
  seromodel = USA_HPV16_age_no_seroreversion,
  serosurvey = USA_HPV16,
  size_text = size_text
) +
  ggtitle("age - no seroreversion")

## Edad - seroreversion

USA_HPV16_age_seroreversion_plot_seroprev <- plot_seroprevalence_estimates(
  seromodel = USA_HPV16_age_seroreversion,
  serosurvey = USA_HPV16,
  size_text = size_text
)+
  ggtitle("age - seroreversion")

## Grafica de seroprevalencia conjunta

USA_HPV16_plot_seroprev <- cowplot::plot_grid(
  USA_HPV16_constant_no_seroreversion_plot_seroprev,
  USA_HPV16_constant_seroreversion_plot_seroprev,
  USA_HPV16_time_no_seroreversion_plot_seroprev,
  USA_HPV16_age_no_seroreversion_plot_seroprev,
  USA_HPV16_age_seroreversion_plot_seroprev,
  nrow = 1,
  align = "hv"
)

# Gráfica de foi
foi_max <- 0.3

# Tiempo - no seroreversion

USA_HPV16_time_no_seroreversion_plot_foi <- plot_foi_estimates(
  seromodel = USA_HPV16_time_no_seroreversion,
  serosurvey = USA_HPV16,
  size_text = size_text,
  foi_max = foi_max
)

## Edad - no seroreversion
USA_HPV16_age_no_seroreversion_plot_foi <- plot_foi_estimates(
  seromodel = USA_HPV16_age_no_seroreversion,
  serosurvey = USA_HPV16,
  size_text = size_text,
  foi_max = foi_max
)

## Edad - seroreversion
USA_HPV16_age_seroreversion_plot_foi <- plot_foi_estimates(
  seromodel = USA_HPV16_age_seroreversion,
  serosurvey = USA_HPV16,
  size_text = size_text,
  foi_max = foi_max
)

## Grafica conjunta foi
empty_plot <- ggplot() + theme_void()

USA_HPV16_plot_foi <- cowplot::plot_grid(
  empty_plot,
  empty_plot,
  USA_HPV16_time_no_seroreversion_plot_foi,
  USA_HPV16_age_no_seroreversion_plot_foi,
  USA_HPV16_age_seroreversion_plot_foi,
  ncol = 5,
  nrow = 1,
  align = "hv"
)

# Gráfica de rhats

## Tiempo - no seroreversion
USA_HPV16_time_no_seroreversion_plot_rhats <- serofoi:::plot_rhats(
  seromodel = USA_HPV16_time_no_seroreversion,
  serosurvey = USA_HPV16,
  size_text = size_text
)

## Edad - no seroreversion
USA_HPV16_age_no_seroreversion_plot_rhats <- serofoi:::plot_rhats(
  seromodel = USA_HPV16_age_no_seroreversion,
  serosurvey = USA_HPV16,
  size_text = size_text
)

## Edad - seroreversion
USA_HPV16_age_seroreversion_plot_rhats <- serofoi:::plot_rhats(
  seromodel = USA_HPV16_age_seroreversion,
  serosurvey = USA_HPV16,
  size_text = size_text
)

## Grafica conjunta foi
empty_plot <- ggplot() + theme_void()

USA_HPV16_plot_rhats <- cowplot::plot_grid(
  empty_plot,
  empty_plot,
  USA_HPV16_time_no_seroreversion_plot_rhats,
  USA_HPV16_age_no_seroreversion_plot_rhats,
  USA_HPV16_age_seroreversion_plot_rhats,
  ncol = 5,
  nrow = 1,
  align = "hv"
)

# Gráfica conjunta  USA_HPV16

USA_HPV16_plot <- cowplot::plot_grid(
  USA_HPV16_plot_seroprev,
  USA_HPV16_plot_foi,
  USA_HPV16_plot_rhats,
  nrow = 3,
  align = "hv"
)

plot(USA_HPV16_plot)
jpeg(filename = "SEROFOI/plots/all_USA_HPV16_.jpeg", width = 11, height = 8, units = "in", res = 300) 
USA_HPV16_plot
dev.off()








### SEROENCUESTA  TAIWAN HPV 18 ###

## crear data frame de seroenuest 

TWN_HPV16<- dat %>% filter( survey == "TWN-025-01"
) %>%
  
  ## SELECCCION DE DATOS QUE SE REQUIEREN PARA LOS MODELOS ##
  select(survey, tsur, age_min, age_max, counts, total) %>%
  rename(
    n_seropositive = counts,
    sample_size = total)

plot_serosurvey(TWN_HPV16)

# Modelo constante sin seroreversion

TWN_HPV16_constant_no_seroreversion <- fit_seromodel(
  serosurvey = TWN_HPV16,
  model_type = "constant",
  foi_prior = sf_uniform() ,
)

# Modelo constante con seroreversion

TWN_HPV16_constant_seroreversion <- fit_seromodel(
  serosurvey = TWN_HPV16,
  model_type = "constant",
  foi_prior = sf_uniform(),
  is_seroreversion = TRUE,
  seroreversion_prior = sf_uniform(0.0, 2.0),
  iter = 1500
)

# Modelo dependiente del tiempo sin seroreversion

foi_index <- get_foi_index(
  serosurvey = TWN_HPV16,
  group_size = 10
)

init <- function() {
  list(foi_vector = rep(0.01, max(TWN_HPV16$age_max)))
}
TWN_HPV16_time_no_seroreversion <- fit_seromodel(
  serosurvey = TWN_HPV16,
  model_type = "time",
  foi_prior = sf_normal(),
  is_seroreversion = FALSE,
  init = init,
  iter = 5000,
  thin = 2
)

# Modelo dependiente de la edad sin seroreversion

foi_index <- c(
  rep(1, 15),
  16:max(TWN_HPV16$age_max)
)
init <- function() {
  list(foi_vector = rep(0.01, max(foi_index)))
}
TWN_HPV16_age_no_seroreversion <- fit_seromodel(
  serosurvey = TWN_HPV16,
  model_type = "age",
  foi_prior = sf_normal(1e-4, 1e-5),
  foi_index = foi_index,
  is_seroreversion = FALSE,
  init = init
)

# Modelo dependiente de la edad con seroreversion

foi_index <- c(
  rep(1, 15),
  16:max(TWN_HPV16$age_max)
)

init <- function() {
  list(foi_vector = rep(0.01, max(foi_index)))
}

TWN_HPV16_age_seroreversion <- fit_seromodel(
  serosurvey = TWN_HPV16,
  model_type = "age",
  foi_prior = sf_normal(1e-4, 1e-5),
  foi_index = foi_index,
  is_seroreversion = TRUE,
  seroreversion_prior = sf_normal(1, 0.5),
  iter = 5000,
  thin = 2
)

# Gráfica conjunta de todos los modelos

size_text <- 8

## Constante - no seroreversion

TWN_HPV16_constant_no_seroreversion_plot <- plot_seromodel(
  seromodel = TWN_HPV16_constant_no_seroreversion,
  serosurvey = TWN_HPV16,
  size_text = size_text
)
plot(TWN_HPV16_constant_no_seroreversion_plot)

## Constante - seroreversion

TWN_HPV16_constant_seroreversion_plot <- plot_seromodel(
  seromodel = TWN_HPV16_constant_seroreversion,
  serosurvey = TWN_HPV16,
  size_text = size_text
)
plot(TWN_HPV16_constant_seroreversion_plot)

## Tiempo - no seroreversion

TWN_HPV16_time_no_seroreversion_plot <- plot_seromodel(
  seromodel = TWN_HPV16_time_no_seroreversion,
  serosurvey = TWN_HPV16,
  size_text = size_text
)
plot(TWN_HPV16_time_no_seroreversion_plot)

## Edad - no seroreversion

TWN_HPV16_age_no_seroreversion_plot <- plot_seromodel(
  seromodel = TWN_HPV16_age_no_seroreversion,
  serosurvey = TWN_HPV16,
  size_text = size_text
)
plot(TWN_HPV16_age_no_seroreversion_plot)

## Edad - seroreversion

TWN_HPV16_age_seroreversion_plot <- plot_seromodel(
  seromodel = TWN_HPV16_age_seroreversion,
  serosurvey = TWN_HPV16,
  size_text = size_text
)
plot(TWN_HPV16_age_seroreversion_plot)

# Tabla de resumen

## Tabla para cada modelo
TWN_HPV16_constant_no_seroreversion_summary <- summarise_seromodel(
  seromodel = TWN_HPV16_constant_no_seroreversion,
  serosurvey = TWN_HPV16
) %>% as.data.frame()

TWN_HPV16_constant_seroreversion_summary <- summarise_seromodel(
  seromodel = TWN_HPV16_constant_seroreversion,
  serosurvey = TWN_HPV16
) %>% as.data.frame()

TWN_HPV16_time_no_seroreversion_summary <- summarise_seromodel(
  seromodel = TWN_HPV16_time_no_seroreversion,
  serosurvey = TWN_HPV16
) %>% as.data.frame()

TWN_HPV16_age_no_seroreversion_summary <- summarise_seromodel(
  seromodel = TWN_HPV16_age_no_seroreversion,
  serosurvey = TWN_HPV16
) %>% as.data.frame()

TWN_HPV16_age_seroreversion_summary <- summarise_seromodel(
  seromodel = TWN_HPV16_age_seroreversion,
  serosurvey = TWN_HPV16
) %>% as.data.frame()

## Tabla de todos los modelos

TWN_HPV16_summary <- dplyr::bind_rows(
  TWN_HPV16_constant_no_seroreversion_summary,
  TWN_HPV16_constant_seroreversion_summary,
  TWN_HPV16_time_no_seroreversion_summary,
  TWN_HPV16_age_no_seroreversion_summary,
  TWN_HPV16_age_seroreversion_summary
)

write.xlsx(TWN_HPV16_summary, file = "SEROFOI/data frame/TWN_HPV16_summary.xlsx")

# Gráfica de seroprevalencia
size_text <- 8

## Constante - no seroreversion

TWN_HPV16_constant_no_seroreversion_plot_seroprev <- plot_seroprevalence_estimates(
  seromodel = TWN_HPV16_constant_no_seroreversion,
  serosurvey = TWN_HPV16,
  size_text = size_text
) +
  ggtitle("constant - no seroreversion")


## Constante - seroreversion

TWN_HPV16_constant_seroreversion_plot_seroprev <- plot_seroprevalence_estimates(
  seromodel = TWN_HPV16_constant_seroreversion,
  serosurvey = TWN_HPV16,
  size_text = size_text
) +
  ggtitle("constant - seroreversion")

## Tiempo - no seroreversion

TWN_HPV16_time_no_seroreversion_plot_seroprev <- plot_seroprevalence_estimates(
  seromodel = TWN_HPV16_time_no_seroreversion,
  serosurvey = TWN_HPV16,
  size_text = size_text
) +
  ggtitle("time - no seroreversion")

## Edad - no seroreversion

TWN_HPV16_age_no_seroreversion_plot_seroprev <- plot_seroprevalence_estimates(
  seromodel = TWN_HPV16_age_no_seroreversion,
  serosurvey = TWN_HPV16,
  size_text = size_text
) +
  ggtitle("age - no seroreversion")

## Edad - seroreversion

TWN_HPV16_age_seroreversion_plot_seroprev <- plot_seroprevalence_estimates(
  seromodel = TWN_HPV16_age_seroreversion,
  serosurvey = TWN_HPV16,
  size_text = size_text
)+
  ggtitle("age - seroreversion")

## Grafica de seroprevalencia conjunta

TWN_HPV16_plot_seroprev <- cowplot::plot_grid(
  TWN_HPV16_constant_no_seroreversion_plot_seroprev,
  TWN_HPV16_constant_seroreversion_plot_seroprev,
  TWN_HPV16_time_no_seroreversion_plot_seroprev,
  TWN_HPV16_age_no_seroreversion_plot_seroprev,
  TWN_HPV16_age_seroreversion_plot_seroprev,
  nrow = 1,
  align = "hv"
)

# Gráfica de foi
foi_max <- 0.015

# Tiempo - no seroreversion

TWN_HPV16_time_no_seroreversion_plot_foi <- plot_foi_estimates(
  seromodel = TWN_HPV16_time_no_seroreversion,
  serosurvey = TWN_HPV16,
  size_text = size_text,
  foi_max = foi_max
)

## Edad - no seroreversion
TWN_HPV16_age_no_seroreversion_plot_foi <- plot_foi_estimates(
  seromodel = TWN_HPV16_age_no_seroreversion,
  serosurvey = TWN_HPV16,
  size_text = size_text,
  foi_max = foi_max
)

## Edad - seroreversion
TWN_HPV16_age_seroreversion_plot_foi <- plot_foi_estimates(
  seromodel = TWN_HPV16_age_seroreversion,
  serosurvey = TWN_HPV16,
  size_text = size_text,
  foi_max = foi_max
)

## Grafica conjunta foi
empty_plot <- ggplot() + theme_void()

TWN_HPV16_plot_foi <- cowplot::plot_grid(
  empty_plot,
  empty_plot,
  TWN_HPV16_time_no_seroreversion_plot_foi,
  TWN_HPV16_age_no_seroreversion_plot_foi,
  TWN_HPV16_age_seroreversion_plot_foi,
  ncol = 5,
  nrow = 1,
  align = "hv"
)

# Gráfica de rhats

## Tiempo - no seroreversion
TWN_HPV16_time_no_seroreversion_plot_rhats <- serofoi:::plot_rhats(
  seromodel = TWN_HPV16_time_no_seroreversion,
  serosurvey = TWN_HPV16,
  size_text = size_text
)

## Edad - no seroreversion
TWN_HPV16_age_no_seroreversion_plot_rhats <- serofoi:::plot_rhats(
  seromodel = TWN_HPV16_age_no_seroreversion,
  serosurvey = TWN_HPV16,
  size_text = size_text
)

## Edad - seroreversion
TWN_HPV16_age_seroreversion_plot_rhats <- serofoi:::plot_rhats(
  seromodel = TWN_HPV16_age_seroreversion,
  serosurvey = TWN_HPV16,
  size_text = size_text
)

## Grafica conjunta foi
empty_plot <- ggplot() + theme_void()

TWN_HPV16_plot_rhats <- cowplot::plot_grid(
  empty_plot,
  empty_plot,
  TWN_HPV16_time_no_seroreversion_plot_rhats,
  TWN_HPV16_age_no_seroreversion_plot_rhats,
  TWN_HPV16_age_seroreversion_plot_rhats,
  ncol = 5,
  nrow = 1,
  align = "hv"
)

# Gráfica conjunta  TWN_HPV16

TWN_HPV16_plot <- cowplot::plot_grid(
  TWN_HPV16_plot_seroprev,
  TWN_HPV16_plot_foi,
  TWN_HPV16_plot_rhats,
  nrow = 3,
  align = "hv"
)

plot(TWN_HPV16_plot)
jpeg(filename = "SEROFOI/plots/all_TWN_HPV16_.jpeg", width = 11, height = 8, units = "in", res = 300) 
TWN_HPV16_plot
dev.off()



### SEROENCUESTA  NIGERIA HPV 16 ###


## crear data frame de seroenuest 

NGA_HPV16<- dat %>% filter( survey == "NGA-002-01"
) %>%
  
   ## SELECCCION DE DATOS QUE SE REQUIEREN PARA LOS MODELOS ##
  select(survey, tsur, age_min, age_max, counts, total) %>%
  rename(
    n_seropositive = counts,
    sample_size = total)

plot_serosurvey(NGA_HPV16)


# Modelo constante sin seroreversion

NGA_HPV16_constant_no_seroreversion <- fit_seromodel(
  serosurvey = NGA_HPV16,
  model_type = "constant",
  foi_prior = sf_uniform() ,
)

# Modelo constante con seroreversion

NGA_HPV16_constant_seroreversion <- fit_seromodel(
  serosurvey = NGA_HPV16,
  model_type = "constant",
  foi_prior = sf_uniform(),
  is_seroreversion = TRUE,
  seroreversion_prior = sf_uniform(0.0, 2.0),
  iter = 1500
)

# Modelo dependiente del tiempo sin seroreversion

foi_index <- get_foi_index(
  serosurvey = NGA_HPV16,
  group_size = 10
)

init <- function() {
  list(foi_vector = rep(0.01, max(NGA_HPV16$age_max)))
}
NGA_HPV16_time_no_seroreversion <- fit_seromodel(
  serosurvey = NGA_HPV16,
  model_type = "time",
  foi_prior = sf_normal(),
  is_seroreversion = FALSE,
  init = init,
  iter = 5000,
  thin = 2
)

# Modelo dependiente de la edad sin seroreversion

foi_index <- c(
  rep(1, 15),
  16:max(NGA_HPV16$age_max)
)
init <- function() {
  list(foi_vector = rep(0.01, max(foi_index)))
}
NGA_HPV16_age_no_seroreversion <- fit_seromodel(
  serosurvey = NGA_HPV16,
  model_type = "age",
  foi_prior = sf_normal(1e-4, 1e-5),
  foi_index = foi_index,
  is_seroreversion = FALSE,
  init = init
)

# Modelo dependiente de la edad con seroreversion

foi_index <- c(
  rep(1, 15),
  16:max(NGA_HPV16$age_max)
)

init <- function() {
  list(foi_vector = rep(0.01, max(foi_index)))
}

NGA_HPV16_age_seroreversion <- fit_seromodel(
  serosurvey = NGA_HPV16,
  model_type = "age",
  foi_prior = sf_normal(1e-4, 1e-5),
  foi_index = foi_index,
  is_seroreversion = TRUE,
  seroreversion_prior = sf_normal(1, 0.5),
  iter = 5000,
  thin = 2
)

# Gráfica conjunta de todos los modelos

size_text <- 8

## Constante - no seroreversion

NGA_HPV16_constant_no_seroreversion_plot <- plot_seromodel(
  seromodel = NGA_HPV16_constant_no_seroreversion,
  serosurvey = NGA_HPV16,
  size_text = size_text
)
plot(NGA_HPV16_constant_no_seroreversion_plot)

## Constante - seroreversion

NGA_HPV16_constant_seroreversion_plot <- plot_seromodel(
  seromodel = NGA_HPV16_constant_seroreversion,
  serosurvey = NGA_HPV16,
  size_text = size_text
)
plot(NGA_HPV16_constant_seroreversion_plot)

## Tiempo - no seroreversion

NGA_HPV16_time_no_seroreversion_plot <- plot_seromodel(
  seromodel = NGA_HPV16_time_no_seroreversion,
  serosurvey = NGA_HPV16,
  size_text = size_text
)
plot(NGA_HPV16_time_no_seroreversion_plot)

## Edad - no seroreversion

NGA_HPV16_age_no_seroreversion_plot <- plot_seromodel(
  seromodel = NGA_HPV16_age_no_seroreversion,
  serosurvey = NGA_HPV16,
  size_text = size_text
)
plot(NGA_HPV16_age_no_seroreversion_plot)

## Edad - seroreversion

NGA_HPV16_age_seroreversion_plot <- plot_seromodel(
  seromodel = NGA_HPV16_age_seroreversion,
  serosurvey = NGA_HPV16,
  size_text = size_text
)
plot(NGA_HPV16_age_seroreversion_plot)


# Tabla de resumen

## Tabla para cada modelo
NGA_HPV16_constant_no_seroreversion_summary <- summarise_seromodel(
  seromodel = NGA_HPV16_constant_no_seroreversion,
  serosurvey = NGA_HPV16
) %>% as.data.frame()

NGA_HPV16_constant_seroreversion_summary <- summarise_seromodel(
  seromodel = NGA_HPV16_constant_seroreversion,
  serosurvey = NGA_HPV16
) %>% as.data.frame()

NGA_HPV16_time_no_seroreversion_summary <- summarise_seromodel(
  seromodel = NGA_HPV16_time_no_seroreversion,
  serosurvey = NGA_HPV16
) %>% as.data.frame()

NGA_HPV16_age_no_seroreversion_summary <- summarise_seromodel(
  seromodel = NGA_HPV16_age_no_seroreversion,
  serosurvey = NGA_HPV16
) %>% as.data.frame()

NGA_HPV16_age_seroreversion_summary <- summarise_seromodel(
  seromodel = NGA_HPV16_age_seroreversion,
  serosurvey = NGA_HPV16
) %>% as.data.frame()

## Tabla de todos los modelos

NGA_HPV16_summary <- dplyr::bind_rows(
  NGA_HPV16_constant_no_seroreversion_summary,
  NGA_HPV16_constant_seroreversion_summary,
  NGA_HPV16_time_no_seroreversion_summary,
  NGA_HPV16_age_no_seroreversion_summary,
  NGA_HPV16_age_seroreversion_summary
)

write.xlsx(NGA_HPV16_summary, file = "SEROFOI/data frame/NGA_HPV16_summary.xlsx")

# Gráfica de seroprevalencia
size_text <- 8

## Constante - no seroreversion

NGA_HPV16_constant_no_seroreversion_plot_seroprev <- plot_seroprevalence_estimates(
  seromodel = NGA_HPV16_constant_no_seroreversion,
  serosurvey = NGA_HPV16,
  size_text = size_text
) +
  ggtitle("constant - no seroreversion")


## Constante - seroreversion

NGA_HPV16_constant_seroreversion_plot_seroprev <- plot_seroprevalence_estimates(
  seromodel = NGA_HPV16_constant_seroreversion,
  serosurvey = NGA_HPV16,
  size_text = size_text
) +
  ggtitle("constant - seroreversion")

## Tiempo - no seroreversion

NGA_HPV16_time_no_seroreversion_plot_seroprev <- plot_seroprevalence_estimates(
  seromodel = NGA_HPV16_time_no_seroreversion,
  serosurvey = NGA_HPV16,
  size_text = size_text
) +
  ggtitle("time - no seroreversion")

## Edad - no seroreversion

NGA_HPV16_age_no_seroreversion_plot_seroprev <- plot_seroprevalence_estimates(
  seromodel = NGA_HPV16_age_no_seroreversion,
  serosurvey = NGA_HPV16,
  size_text = size_text
) +
  ggtitle("age - no seroreversion")

## Edad - seroreversion

NGA_HPV16_age_seroreversion_plot_seroprev <- plot_seroprevalence_estimates(
  seromodel = NGA_HPV16_age_seroreversion,
  serosurvey = NGA_HPV16,
  size_text = size_text
)+
  ggtitle("age - seroreversion")

## Grafica de seroprevalencia conjunta

NGA_HPV16_plot_seroprev <- cowplot::plot_grid(
  NGA_HPV16_constant_no_seroreversion_plot_seroprev,
  NGA_HPV16_constant_seroreversion_plot_seroprev,
  NGA_HPV16_time_no_seroreversion_plot_seroprev,
  NGA_HPV16_age_no_seroreversion_plot_seroprev,
  NGA_HPV16_age_seroreversion_plot_seroprev,
  nrow = 1,
  align = "hv"
)

# Gráfica de foi
foi_max <- 1.5

# Tiempo - no seroreversion

NGA_HPV16_time_no_seroreversion_plot_foi <- plot_foi_estimates(
  seromodel = NGA_HPV16_time_no_seroreversion,
  serosurvey = NGA_HPV16,
  size_text = size_text,
  foi_max = foi_max
)

## Edad - no seroreversion
NGA_HPV16_age_no_seroreversion_plot_foi <- plot_foi_estimates(
  seromodel = NGA_HPV16_age_no_seroreversion,
  serosurvey = NGA_HPV16,
  size_text = size_text,
  foi_max = foi_max
)

## Edad - seroreversion
NGA_HPV16_age_seroreversion_plot_foi <- plot_foi_estimates(
  seromodel = NGA_HPV16_age_seroreversion,
  serosurvey = NGA_HPV16,
  size_text = size_text,
  foi_max = foi_max
)

## Grafica conjunta foi
empty_plot <- ggplot() + theme_void()

NGA_HPV16_plot_foi <- cowplot::plot_grid(
  empty_plot,
  empty_plot,
  NGA_HPV16_time_no_seroreversion_plot_foi,
  NGA_HPV16_age_no_seroreversion_plot_foi,
  NGA_HPV16_age_seroreversion_plot_foi,
  ncol = 5,
  nrow = 1,
  align = "hv"
)

# Gráfica de rhats

## Tiempo - no seroreversion
NGA_HPV16_time_no_seroreversion_plot_rhats <- serofoi:::plot_rhats(
  seromodel = NGA_HPV16_time_no_seroreversion,
  serosurvey = NGA_HPV16,
  size_text = size_text
)

## Edad - no seroreversion
NGA_HPV16_age_no_seroreversion_plot_rhats <- serofoi:::plot_rhats(
  seromodel = NGA_HPV16_age_no_seroreversion,
  serosurvey = NGA_HPV16,
  size_text = size_text
)

## Edad - seroreversion
NGA_HPV16_age_seroreversion_plot_rhats <- serofoi:::plot_rhats(
  seromodel = NGA_HPV16_age_seroreversion,
  serosurvey = NGA_HPV16,
  size_text = size_text
)

## Grafica conjunta foi
empty_plot <- ggplot() + theme_void()

NGA_HPV16_plot_rhats <- cowplot::plot_grid(
  empty_plot,
  empty_plot,
  NGA_HPV16_time_no_seroreversion_plot_rhats,
  NGA_HPV16_age_no_seroreversion_plot_rhats,
  NGA_HPV16_age_seroreversion_plot_rhats,
  ncol = 5,
  nrow = 1,
  align = "hv"
)

# Gráfica conjunta  NGA_HPV16

NGA_HPV16_plot <- cowplot::plot_grid(
  NGA_HPV16_plot_seroprev,
  NGA_HPV16_plot_foi,
  NGA_HPV16_plot_rhats,
  nrow = 3,
  align = "hv"
)

plot(NGA_HPV16_plot)
jpeg(filename = "SEROFOI/plots/all_NGA_HPV16_.jpeg", width = 11, height = 8, units = "in", res = 300) 
NGA_HPV16_plot
dev.off()
