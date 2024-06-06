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

col_HPV18_constant_seroreversion <- fit_seromodel(
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
foi_max <- 0.4
## Tiempo - no seroreversion

col_HPV18_time_no_seroreversion_plot_foi <- plot_foi_estimates(
  seromodel = col_HPV18_time_no_seroreversion,
  serosurvey = col_HPV18,
  size_text = size_text,
 )

## Edad - no seroreversion

col_HPV18_age_no_seroreversion_plot_foi <- plot_foi_estimates(
  seromodel = col_HPV18_age_no_seroreversion,
  serosurvey = col_HPV18,
  size_text = size_text,
)

## Edad - seroreversion

col_HPV18_age_seroreversion_plot_foi <- plot_foi_estimates(
  seromodel = col_HPV18_age_seroreversion,
  serosurvey = col_HPV18,
  size_text = size_text,
  max_lambda =0.015
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
jpeg(filename = "SEROFOI/plots/all_col_hpv18_.jpeg", width = 500*2, height = 500*2) 
col_HPV18_plot
dev.off()

png(filename = "SEROFOI/plots/all_col_hpv18_.png", width = 500*2, height = 500*2) 
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

foi_max <- 0.004

## Tiempo - no seroreversion

col_HPV16_time_no_seroreversion_plot_foi <- plot_foi_estimates(
  seromodel = col_HPV16_time_no_seroreversion,
  serosurvey = col_HPV16,
  size_text = size_text,
  foi_max = 0.4
)

plot(col_HPV16_time_no_seroreversion_plot_foi)


## Edad - no seroreversion

col_HPV16_age_no_seroreversion_plot_foi <- plot_foi_estimates(
  seromodel = col_HPV16_age_no_seroreversion,
  serosurvey = col_HPV16,
  size_text = size_text,
  foi_max = 0.4
)

plot(col_HPV16_age_no_seroreversion_plot_foi)

## Edad - seroreversion

col_HPV16_age_seroreversion_plot_foi <- plot_foi_estimates(
  seromodel = col_HPV16_age_seroreversion,
  serosurvey = col_HPV16,
  size_text = size_text,
  foi_max = 0.4
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

dat_col_HPVHr <- data.frame (filter( dat, country == "COL")  %>% filter (pathogen == "HPV 16/18"))

## SELECCCION DE DATOS QUE SE REQUIEREN PARA LOS MODELOS ##

col_HPVHr <- dat_col_HPVHr %>%
  mutate(
    survey = paste(survey, pathogen)
  ) %>%
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
  is_seroreversion = FALSE
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
init <- function() {
  list(foi_vector = rep(0.01, max(col_HPVHr$age_max)))
}
col_HPVHr_time_no_seroreversion <- fit_seromodel(
  serosurvey = col_HPVHr,
  model_type = "time",
  foi_prior = sf_normal(),
  is_seroreversion = FALSE,
  init = init
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
  seroreversion_prior = sf_normal(1, 0.5)
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



### SEROENCUESTA COSTA RICA VPH 18 ###

## CREAR DATA.FRAME DE SEROENCUESTA ## 

dat_CRI_HPV18 <- data.frame (filter( dat, country == "CRI")  %>% filter (pathogen == "HPV 18"))

## SELECCCION DE DATOS QUE SE REQUIEREN PARA LOS MODELOS ##

CRI_HPV18  <- dat_CRI_HPV18 %>%
  mutate(
    survey = paste(survey, pathogen)
  ) %>%
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
  is_seroreversion = FALSE
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

init <- function() {
  list(foi_vector = rep(0.01, max(CRI_HPV18$age_max)))
}
CRI_HPV18_time_no_seroreversion <- fit_seromodel(
  serosurvey = CRI_HPV18,
  model_type = "time",
  foi_prior = sf_normal(),
  is_seroreversion = FALSE,
  init = init
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
  seroreversion_prior = sf_normal(1, 0.5)
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



### SEROENCUESTA COSTA RICA VPH 16 ###

## CREAR DATA.FRAME DE SEROENCUESTA ## 

dat_CRI_HPV16 <- data.frame (filter( dat, country == "CRI")  %>% filter (pathogen == "HPV 16"))

## SELECCCION DE DATOS QUE SE REQUIEREN PARA LOS MODELOS ##

CRI_HPV16  <- dat_CRI_HPV16 %>%
  mutate(
    survey = paste(survey, pathogen)
  ) %>%
  select(survey, tsur, age_min, age_max, counts, total) %>%
  rename(
    n_seropositive = counts,
    sample_size = total)

plot_serosurvey(CRI_HPV16)


# Modelo constante sin seroreversion

CRI_HPV16_constant_no_seroreversion <- fit_seromodel(
  serosurvey = CRI_HPV16,
  model_type = "constant",
  foi_prior = sf_uniform() ,
  is_seroreversion = FALSE
)

CRI_HPV16_constant_no_seroreversion_plot <- serofoi:::plot_seromodel(
  seromodel = CRI_HPV16_constant_no_seroreversion,
  serosurvey = CRI_HPV16,
)

plot(CRI_HPV16_constant_no_seroreversion_plot)

# Modelo constante con seroreversion


CRI_HPV16_constant_seroreversion <- fit_seromodel(
  serosurvey = CRI_HPV16,
  model_type = "constant",
  foi_prior = sf_uniform(),
  is_seroreversion = TRUE
)

CRI_HPV16_constant_seroreversion_plot <- plot_seromodel(
  seromodel = CRI_HPV16_constant_seroreversion,
  serosurvey = CRI_HPV16
)
plot(CRI_HPV16_constant_seroreversion_plot)

# Modelo dependiente del tiempo sin seroreversion

init <- function() {
  list(foi_vector = rep(0.01, max(CRI_HPV16$age_max)))
}
CRI_HPV16_time_no_seroreversion <- fit_seromodel(
  serosurvey = CRI_HPV16,
  model_type = "time",
  foi_prior = sf_normal(),
  is_seroreversion = FALSE,
  init = init
)

CRI_HPV16_time_no_seroreversion_plot <- plot_seromodel(
  seromodel = CRI_HPV16_time_no_seroreversion,
  serosurvey = CRI_HPV16
)
plot(CRI_HPV16_time_no_seroreversion_plot)

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

CRI_HPV16_age_no_seroreversion_plot <- plot_seromodel(
  seromodel = CRI_HPV16_age_no_seroreversion,
  serosurvey = CRI_HPV16
)
plot(CRI_HPV16_age_no_seroreversion_plot)

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
  seroreversion_prior = sf_normal(1, 0.5)
)

CRI_HPV16_age_seroreversion_plot <- plot_seromodel(
  seromodel = CRI_HPV16_age_seroreversion,
  serosurvey = CRI_HPV16
)
plot(CRI_HPV16_age_seroreversion_plot)


# Gráfica conjunta de todos los modelos

CRI_HPV16_models_plot <- cowplot::plot_grid(
  CRI_HPV16_constant_no_seroreversion_plot,
  CRI_HPV16_constant_seroreversion_plot,
  CRI_HPV16_time_no_seroreversion_plot,
  CRI_HPV16_age_no_seroreversion_plot,
  CRI_HPV16_age_seroreversion_plot,
  ncol = 5
)
plot(CRI_HPV16_all_models_plot)


CRI_HPV16_all_models_plot <- cowplot::plot_grid(
  CRI_HPV16_time_no_seroreversion_plot,
  CRI_HPV16_age_no_seroreversion_plot,
  CRI_HPV16_age_seroreversion_plot,
  ncol = 3
)
plot( CRI_HPV16_all_models_plot)
jpeg(filename = "SEROFOI/plots/CRI_HPV16.jpeg", width = 480*2, height = 480*2) 
CRI_HPV16_all_models_plot
dev.off()

CRI_HPV16_constant_models_plot <- cowplot::plot_grid(
  CRI_HPV16_constant_no_seroreversion_plot,
  CRI_HPV16_constant_seroreversion_plot,
  ncol = 2)

plot( CRI_HPV16_constant_models_plot)
jpeg(filename = "SEROFOI/plots/CRI_HPV16_conts.jpeg", width = 480*2, height = 480*2) 
CRI_HPV16_constant_models_plot
dev.off()



### SEROENCUESTA BRASIL HPV 16  ###

## CREAR DATA.FRAME DE SEROENCUESTA ## 

dat_BRA_HPV16 <- data.frame (filter( dat, survey == "BRA-017-01"))

## SELECCCION DE DATOS QUE SE REQUIEREN PARA LOS MODELOS ##

BRA_HPV16  <- dat_BRA_HPV16 %>%
  mutate(
    survey = paste(survey, pathogen)
  ) %>%
  select(survey, tsur, age_min, age_max, counts, total) %>%
  rename(
    n_seropositive = counts,
    sample_size = total)

plot_serosurvey(BRA_HPV16)


# Modelo constante sin seroreversion

BRA_HPV16_constant_no_seroreversion <- fit_seromodel(
  serosurvey = BRA_HPV16,
  model_type = "constant",
  foi_prior = sf_uniform() ,
  is_seroreversion = FALSE
)

BRA_HPV16_constant_no_seroreversion_plot <- serofoi:::plot_seromodel(
  seromodel = BRA_HPV16_constant_no_seroreversion,
  serosurvey = BRA_HPV16,
)

plot(BRA_HPV16_constant_no_seroreversion_plot)

# Modelo constante con seroreversion


BRA_HPV16_constant_seroreversion <- fit_seromodel(
  serosurvey = BRA_HPV16,
  model_type = "constant",
  foi_prior = sf_uniform(),
  is_seroreversion = TRUE
)

BRA_HPV16_constant_seroreversion_plot <- plot_seromodel(
  seromodel = BRA_HPV16_constant_seroreversion,
  serosurvey = BRA_HPV16
)
plot(BRA_HPV16_constant_seroreversion_plot)

# Modelo dependiente del tiempo sin seroreversion

init <- function() {
  list(foi_vector = rep(0.01, max(BRA_HPV16$age_max)))
}
BRA_HPV16_time_no_seroreversion <- fit_seromodel(
  serosurvey = BRA_HPV16,
  model_type = "time",
  foi_prior = sf_normal(),
  is_seroreversion = FALSE,
  init = init
)

BRA_HPV16_time_no_seroreversion_plot <- plot_seromodel(
  seromodel = BRA_HPV16_time_no_seroreversion,
  serosurvey = BRA_HPV16
)
plot(BRA_HPV16_time_no_seroreversion_plot)

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
  init = init
)

BRA_HPV16_age_no_seroreversion_plot <- plot_seromodel(
  seromodel = BRA_HPV16_age_no_seroreversion,
  serosurvey = BRA_HPV16
)
plot(BRA_HPV16_age_no_seroreversion_plot)

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
  seroreversion_prior = sf_normal(1, 0.5)
)

BRA_HPV16_age_seroreversion_plot <- plot_seromodel(
  seromodel = BRA_HPV16_age_seroreversion,
  serosurvey = BRA_HPV16
)
plot(BRA_HPV16_age_seroreversion_plot)


# Gráfica conjunta de todos los modelos

BRA_HPV16_models_plot <- cowplot::plot_grid(
  BRA_HPV16_constant_no_seroreversion_plot,
  BRA_HPV16_constant_seroreversion_plot,
  BRA_HPV16_time_no_seroreversion_plot,
  BRA_HPV16_age_no_seroreversion_plot,
  BRA_HPV16_age_seroreversion_plot,
  ncol = 5
)
plot(BRA_HPV16_all_models_plot)


BRA_HPV16_all_models_plot <- cowplot::plot_grid(
  BRA_HPV16_time_no_seroreversion_plot,
  BRA_HPV16_age_no_seroreversion_plot,
  BRA_HPV16_age_seroreversion_plot,
  ncol = 3
)
plot( BRA_HPV16_all_models_plot)
jpeg(filename = "SEROFOI/plots/BRA_HPV16.jpeg", width = 480*2, height = 480*2) 
BRA_HPV16_all_models_plot
dev.off()

BRA_HPV16_constant_models_plot <- cowplot::plot_grid(
  BRA_HPV16_constant_no_seroreversion_plot,
  BRA_HPV16_constant_seroreversion_plot,
  ncol = 2)

plot( BRA_HPV16_constant_models_plot)
jpeg(filename = "SEROFOI/plots/BRA_HPV16_conts.jpeg", width = 480*2, height = 480*2) 
CRI_HPV16_constant_models_plot
dev.off()


### SEROENCUESTA ESTADOS UNIDOS HPV 16 ###

## CREAR DATA.FRAME DE SEROENCUESTA ## 

dat_USA92_HPV16 <- data.frame (filter( dat, survey == "USA-011-03"))

## SELECCCION DE DATOS QUE SE REQUIEREN PARA LOS MODELOS ##

USA92_HPV16 <- dat_USA92_HPV16 %>%
  mutate(
    survey = paste(survey, pathogen)
  ) %>%
  select(survey, tsur, age_min, age_max, counts, total) %>%
  rename(
    n_seropositive = counts,
    sample_size = total)

plot_serosurvey(USA92_HPV16)


# Modelo constante sin seroreversion

USA92_HPV16_constant_no_seroreversion <- fit_seromodel(
  serosurvey = USA92_HPV16,
  model_type = "constant",
  foi_prior = sf_uniform() ,
  is_seroreversion = FALSE
)

USA92_HPV16_constant_no_seroreversion_plot <- serofoi:::plot_seromodel(
  seromodel = USA92_HPV16_constant_no_seroreversion,
  serosurvey = USA92_HPV16,
)

plot(USA92_HPV16_constant_no_seroreversion_plot)

# Modelo constante con seroreversion


USA92_HPV16_constant_seroreversion <- fit_seromodel(
  serosurvey = USA92_HPV16,
  model_type = "constant",
  foi_prior = sf_uniform(),
  is_seroreversion = TRUE
)

USA92_HPV16_constant_seroreversion_plot <- plot_seromodel(
  seromodel = USA92_HPV16_constant_seroreversion,
  serosurvey = USA92_HPV16
)
plot(USA92_HPV16_constant_seroreversion_plot)

# Modelo dependiente del tiempo sin seroreversion

init <- function() {
  list(foi_vector = rep(0.01, max(USA92_HPV16$age_max)))
}
USA92_HPV16_time_no_seroreversion <- fit_seromodel(
  serosurvey = USA92_HPV16,
  model_type = "time",
  foi_prior = sf_normal(),
  is_seroreversion = FALSE,
  init = init
)

USA92_HPV16_time_no_seroreversion_plot <- plot_seromodel(
  seromodel = USA92_HPV16_time_no_seroreversion,
  serosurvey = USA92_HPV16
)
plot(USA92_HPV16_time_no_seroreversion_plot)

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

USA92_HPV16_age_no_seroreversion_plot <- plot_seromodel(
  seromodel = USA92_HPV16_age_no_seroreversion,
  serosurvey = USA92_HPV16
)
plot(USA92_HPV16_age_no_seroreversion_plot)

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
  seroreversion_prior = sf_normal(1, 0.5)
)

USA92_HPV16_age_seroreversion_plot <- plot_seromodel(
  seromodel = USA92_HPV16_age_seroreversion,
  serosurvey = USA92_HPV16
)
plot(USA92_HPV16_age_seroreversion_plot)


# Gráfica conjunta de todos los modelos

USA92_HPV16_models_plot <- cowplot::plot_grid(
  USA92_HPV16_constant_no_seroreversion_plot,
  USA92_HPV16_constant_seroreversion_plot,
  USA92_HPV16_time_no_seroreversion_plot,
  USA92_HPV16_age_no_seroreversion_plot,
  USA92_HPV16_age_seroreversion_plot,
  ncol = 5
)
plot(USA92_HPV16_all_models_plot)


USA92_HPV16_all_models_plot <- cowplot::plot_grid(
  USA92_HPV16_time_no_seroreversion_plot,
  USA92_HPV16_age_no_seroreversion_plot,
  USA92_HPV16_age_seroreversion_plot,
  ncol = 3
)
plot( USA92_HPV16_all_models_plot)
jpeg(filename = "SEROFOI/plots/USA92_HPV16.jpeg", width = 480*2, height = 480*2) 
USA92_HPV16_all_models_plot
dev.off()

USA92_HPV16_constant_models_plot <- cowplot::plot_grid(
  USA92_HPV16_constant_no_seroreversion_plot,
  USA92_HPV16_constant_seroreversion_plot,
  ncol = 2)

plot( USA92_HPV16_constant_models_plot)
jpeg(filename = "SEROFOI/plots/USA92_HPV16_conts.jpeg", width = 480*2, height = 480*2) 
USA92_HPV16_constant_models_plot
dev.off()




### SEROENCUESTA PUERTO RICO HPV 16/18  ###

## CREAR DATA.FRAME DE SEROENCUESTA ## 

dat_PRI_HPVHr <- data.frame (filter( dat, survey == "PRI-001-02"))

## SELECCCION DE DATOS QUE SE REQUIEREN PARA LOS MODELOS ##

PRI_HPVHr <- dat_PRI_HPVHr %>%
  mutate(
    survey = paste(survey, pathogen)
  ) %>%
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
  is_seroreversion = FALSE
)

PRI_HPVHr_constant_no_seroreversion_plot <- serofoi:::plot_seromodel(
  seromodel = PRI_HPVHr_constant_no_seroreversion,
  serosurvey = PRI_HPVHr,
)

plot(PRI_HPVHr_constant_no_seroreversion_plot)

# Modelo constante con seroreversion


PRI_HPVHr_constant_seroreversion <- fit_seromodel(
  serosurvey = PRI_HPVHr,
  model_type = "constant",
  foi_prior = sf_uniform(),
  is_seroreversion = TRUE
)

PRI_HPVHr_constant_seroreversion_plot <- plot_seromodel(
  seromodel = PRI_HPVHr_constant_seroreversion,
  serosurvey = PRI_HPVHr
)
plot(PRI_HPVHr_constant_seroreversion_plot)

# Modelo dependiente del tiempo sin seroreversion

init <- function() {
  list(foi_vector = rep(0.01, max(PRI_HPVHr$age_max)))
}
PRI_HPVHr_time_no_seroreversion <- fit_seromodel(
  serosurvey = PRI_HPVHr,
  model_type = "time",
  foi_prior = sf_normal(),
  is_seroreversion = FALSE,
  init = init
)

PRI_HPVHr_time_no_seroreversion_plot <- plot_seromodel(
  seromodel = PRI_HPVHr_time_no_seroreversion,
  serosurvey = PRI_HPVHr
)
plot(PRI_HPVHr_time_no_seroreversion_plot)

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

PRI_HPVHr_age_no_seroreversion_plot <- plot_seromodel(
  seromodel = PRI_HPVHr_age_no_seroreversion,
  serosurvey = PRI_HPVHr
)
plot(PRI_HPVHr_age_no_seroreversion_plot)

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
  seroreversion_prior = sf_normal(1, 0.5)
)

PRI_HPVHr_age_seroreversion_plot <- plot_seromodel(
  seromodel = PRI_HPVHr_age_seroreversion,
  serosurvey = PRI_HPVHr
)
plot(PRI_HPVHr_age_seroreversion_plot)


# Gráfica conjunta de todos los modelos

PRI_HPVHr_models_plot <- cowplot::plot_grid(
  PRI_HPVHr_constant_no_seroreversion_plot,
  PRI_HPVHr_constant_seroreversion_plot,
  PRI_HPVHr_time_no_seroreversion_plot,
  PRI_HPVHr_age_no_seroreversion_plot,
  PRI_HPVHr_age_seroreversion_plot,
  ncol = 5
)
plot(PRI_HPVHr_all_models_plot)


PRI_HPVHr_all_models_plot <- cowplot::plot_grid(
  PRI_HPVHr_time_no_seroreversion_plot,
  PRI_HPVHr_age_no_seroreversion_plot,
  PRI_HPVHr_age_seroreversion_plot,
  ncol = 3
)
plot( PRI_HPVHr_all_models_plot)
jpeg(filename = "SEROFOI/plots/PRI_HPVHr.jpeg", width = 480*2, height = 480*2) 
PRI_HPVHr_all_models_plot
dev.off()

PRI_HPVHr_constant_models_plot <- cowplot::plot_grid(
  PRI_HPVHr_constant_no_seroreversion_plot,
  PRI_HPVHr_constant_seroreversion_plot,
  ncol = 2)

plot( PRI_HPVHr_constant_models_plot)
jpeg(filename = "SEROFOI/plots/PRI_HPVHr_conts.jpeg", width = 480*2, height = 480*2) 
PRI_HPVHr_constant_models_plot
dev.off()







### SEROENCUESTA ESTADOS UNIDOS (1992) HPV 16 ###

## CREAR DATA.FRAME DE SEROENCUESTA ## 

dat_USA92_HPV16 <- data.frame (filter( dat, survey == "USA-011-03"))

## SELECCCION DE DATOS QUE SE REQUIEREN PARA LOS MODELOS ##

USA92_HPV16 <- dat_USA92_HPV16 %>%
  mutate(
    survey = paste(survey, pathogen)
  ) %>%
  select(survey, tsur, age_min, age_max, counts, total) %>%
  rename(
    n_seropositive = counts,
    sample_size = total)

plot_serosurvey(USA92_HPV16)


# Modelo constante sin seroreversion

USA92_HPV16_constant_no_seroreversion <- fit_seromodel(
  serosurvey = USA92_HPV16,
  model_type = "constant",
  foi_prior = sf_uniform() ,
  is_seroreversion = FALSE
)

USA92_HPV16_constant_no_seroreversion_plot <- serofoi:::plot_seromodel(
  seromodel = USA92_HPV16_constant_no_seroreversion,
  serosurvey = USA92_HPV16,
)

plot(USA92_HPV16_constant_no_seroreversion_plot)

# Modelo constante con seroreversion


USA92_HPV16_constant_seroreversion <- fit_seromodel(
  serosurvey = USA92_HPV16,
  model_type = "constant",
  foi_prior = sf_uniform(),
  is_seroreversion = TRUE
)

USA92_HPV16_constant_seroreversion_plot <- plot_seromodel(
  seromodel = USA92_HPV16_constant_seroreversion,
  serosurvey = USA92_HPV16
)
plot(USA92_HPV16_constant_seroreversion_plot)

# Modelo dependiente del tiempo sin seroreversion

init <- function() {
  list(foi_vector = rep(0.01, max(USA92_HPV16$age_max)))
}
USA92_HPV16_time_no_seroreversion <- fit_seromodel(
  serosurvey = USA92_HPV16,
  model_type = "time",
  foi_prior = sf_normal(),
  is_seroreversion = FALSE,
  init = init
)

USA92_HPV16_time_no_seroreversion_plot <- plot_seromodel(
  seromodel = USA92_HPV16_time_no_seroreversion,
  serosurvey = USA92_HPV16
)
plot(USA92_HPV16_time_no_seroreversion_plot)

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

USA92_HPV16_age_no_seroreversion_plot <- plot_seromodel(
  seromodel = USA92_HPV16_age_no_seroreversion,
  serosurvey = USA92_HPV16
)
plot(USA92_HPV16_age_no_seroreversion_plot)

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
  seroreversion_prior = sf_normal(1, 0.5)
)

USA92_HPV16_age_seroreversion_plot <- plot_seromodel(
  seromodel = USA92_HPV16_age_seroreversion,
  serosurvey = USA92_HPV16
)
plot(USA92_HPV16_age_seroreversion_plot)


# Gráfica conjunta de todos los modelos

USA92_HPV16_models_plot <- cowplot::plot_grid(
  USA92_HPV16_constant_no_seroreversion_plot,
  USA92_HPV16_constant_seroreversion_plot,
  USA92_HPV16_time_no_seroreversion_plot,
  USA92_HPV16_age_no_seroreversion_plot,
  USA92_HPV16_age_seroreversion_plot,
  ncol = 5
)
plot(USA92_HPV16_all_models_plot)



USA92_HPV16_all_models_plot <- cowplot::plot_grid(
  USA92_HPV16_time_no_seroreversion_plot,
  USA92_HPV16_age_no_seroreversion_plot,
  USA92_HPV16_age_seroreversion_plot,
  ncol = 3
)
plot( USA92_HPV16_all_models_plot)
jpeg(filename = "SEROFOI/plots/USA92_HPV16.jpeg", width = 480*2, height = 480*2) 
USA92_HPV16_all_models_plot
dev.off()

USA92_HPV16_constant_models_plot <- cowplot::plot_grid(
  USA92_HPV16_constant_no_seroreversion_plot,
  USA92_HPV16_constant_seroreversion_plot,
  ncol = 2)

plot(USA92_HPV16_constant_models_plot)
jpeg(filename = "SEROFOI/plots/USA92_HPV16_conts.jpeg", width = 480*2, height = 480*2) 
USA92_HPV16_constant_models_plot
dev.off()


### SEROENCUESTA ESTADO UNIDOS (2003) HPV 18  ###

## CREAR DATA.FRAME DE SEROENCUESTA ## 

dat_USA_HPV18 <- data.frame (filter( dat, survey == "USA-026-04"))

## SELECCCION DE DATOS QUE SE REQUIEREN PARA LOS MODELOS ##

USA_HPV18  <- dat_USA_HPV18 %>%
  mutate(
    survey = paste(survey, pathogen)
  ) %>%
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
  is_seroreversion = FALSE
)

USA_HPV18_constant_no_seroreversion_plot <- serofoi:::plot_seromodel(
  seromodel = USA_HPV18_constant_no_seroreversion,
  serosurvey = USA_HPV18,
)

plot(USA_HPV18_constant_no_seroreversion_plot)

# Modelo constante con seroreversion


USA_HPV18_constant_seroreversion <- fit_seromodel(
  serosurvey = USA_HPV18,
  model_type = "constant",
  foi_prior = sf_uniform(),
  is_seroreversion = TRUE
)

USA_HPV18_constant_seroreversion_plot <- plot_seromodel(
  seromodel = USA_HPV18_constant_seroreversion,
  serosurvey = USA_HPV18
)
plot(USA_HPV18_constant_seroreversion_plot)

# Modelo dependiente del tiempo sin seroreversion

init <- function() {
  list(foi_vector = rep(0.01, max(USA_HPV18$age_max)))
}
USA_HPV18_time_no_seroreversion <- fit_seromodel(
  serosurvey = USA_HPV18,
  model_type = "time",
  foi_prior = sf_normal(),
  is_seroreversion = FALSE,
  init = init
)

USA_HPV18_time_no_seroreversion_plot <- plot_seromodel(
  seromodel = USA_HPV18_time_no_seroreversion,
  serosurvey = USA_HPV18
)
plot(USA_HPV18_time_no_seroreversion_plot)

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

USA_HPV18_age_no_seroreversion_plot <- plot_seromodel(
  seromodel = USA_HPV18_age_no_seroreversion,
  serosurvey = USA_HPV18
)
plot(USA_HPV18_age_no_seroreversion_plot)

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
  seroreversion_prior = sf_normal(1, 0.5)
)

USA_HPV18_age_seroreversion_plot <- plot_seromodel(
  seromodel = USA_HPV18_age_seroreversion,
  serosurvey = USA_HPV18
)
plot(USA_HPV18_age_seroreversion_plot)


# Gráfica conjunta de todos los modelos

USA_HPV18_models_plot <- cowplot::plot_grid(
  USA_HPV18_constant_no_seroreversion_plot,
  USA_HPV18_constant_seroreversion_plot,
  USA_HPV18_time_no_seroreversion_plot,
  USA_HPV18_age_no_seroreversion_plot,
  USA_HPV18_age_seroreversion_plot,
  ncol = 5
)
plot(USA_HPV18_all_models_plot)



USA_HPV186_all_models_plot <- cowplot::plot_grid(
  USA_HPV18_time_no_seroreversion_plot,
  USA_HPV18_age_no_seroreversion_plot,
  USA_HPV18_age_seroreversion_plot,
  ncol = 3
)
plot( USA_HPV18_all_models_plot)
jpeg(filename = "SEROFOI/plots/USA_HPV18.jpeg", width = 480*2, height = 480*2) 
USA_HPV18_all_models_plot
dev.off()

USA_HPV18_constant_models_plot <- cowplot::plot_grid(
  USA_HPV18_constant_no_seroreversion_plot,
  USA_HPV18_constant_seroreversion_plot,
  ncol = 2)

plot(USA_HPV18_constant_models_plot)
jpeg(filename = "SEROFOI/plots/USA_HPV18_conts.jpeg", width = 480*2, height = 480*2) 
USA_HPV18_constant_models_plot
dev.off()



### SEROENCUESTA ESTADO UNIDOS (2003) HPV 16  ###

## CREAR DATA.FRAME DE SEROENCUESTA ## 

dat_USA_HPV16 <- data.frame (filter( dat, survey == "USA-026-03"))

## SELECCCION DE DATOS QUE SE REQUIEREN PARA LOS MODELOS ##

USA_HPV16  <- dat_USA_HPV16 %>%
  mutate(
    survey = paste(survey, pathogen)
  ) %>%
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
  is_seroreversion = FALSE
)


# Modelo constante con seroreversion


USA_HPV16_constant_seroreversion <- fit_seromodel(
  serosurvey = USA_HPV16,
  model_type = "constant",
  foi_prior = sf_uniform(),
  is_seroreversion = TRUE, 
  seroreversion_prior = sf_uniform(0.0, 2.0)
)



# Modelo dependiente del tiempo sin seroreversion

init <- function() {
  list(foi_vector = rep(0.01, max(USA_HPV18$age_max)))
}
USA_HPV16_time_no_seroreversion <- fit_seromodel(
  serosurvey = USA_HPV16,
  model_type = "time",
  foi_prior = sf_normal(),
  is_seroreversion = FALSE,
  init = init
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
  seroreversion_prior = sf_normal(1, 0.5)
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
  seromodel = CRI_HPV18_constant_seroreversion,
  serosurvey = CRI_HPV18,
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

# Gráfica conjunta de todos los modelos

USA_HPV16_models_plot <- cowplot::plot_grid(
  USA_HPV16_constant_no_seroreversion_plot,
  USA_HPV16_constant_seroreversion_plot,
  USA_HPV16_time_no_seroreversion_plot,
  USA_HPV16_age_no_seroreversion_plot,
  USA_HPV16_age_seroreversion_plot,
  ncol = 5
)
plot(USA_HPV16_all_models_plot)

USA_HPV16_all_models_plot <- cowplot::plot_grid(
  USA_HPV16_time_no_seroreversion_plot,
  USA_HPV16_age_no_seroreversion_plot,
  USA_HPV16_age_seroreversion_plot,
  ncol = 3
)
plot( USA_HPV16_all_models_plot)
jpeg(filename = "SEROFOI/plots/USA_HPV16.jpeg", width = 480*2, height = 480*2) 
USA_HPV16_all_models_plot
dev.off()

USA_HPV16_constant_models_plot <- cowplot::plot_grid(
  USA_HPV16_constant_no_seroreversion_plot,
  USA_HPV16_constant_seroreversion_plot,
  ncol = 2)

plot(USA_HPV16_constant_models_plot)
jpeg(filename = "SEROFOI/plots/USA_HPV16_conts.jpeg", width = 480*2, height = 480*2) 
USA_HPV16_constant_models_plot
dev.off()



### SEROENCUESTA  TAIWAN HPV 18 ###

## CREAR DATA.FRAME DE SEROENCUESTA ## 

dat_TWN_HPV16 <- data.frame (filter( dat, survey == "TWN-025-01"))

## SELECCCION DE DATOS QUE SE REQUIEREN PARA LOS MODELOS ##

TWN_HPV16 <- dat_TWN_HPV18 %>%
  mutate(
    survey = paste(survey, pathogen)
  ) %>%
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
  is_seroreversion = FALSE
)

TWN_HPV16_constant_no_seroreversion_plot <- serofoi:::plot_seromodel(
  seromodel = TWN_HPV16_constant_no_seroreversion,
  serosurvey = TWN_HPV16,
)

plot(TWN_HPV16_constant_no_seroreversion_plot)

# Modelo constante con seroreversion


TWN_HPV16_constant_seroreversion <- fit_seromodel(
  serosurvey = TWN_HPV16,
  model_type = "constant",
  foi_prior = sf_uniform(),
  is_seroreversion = TRUE
)

TWN_HPV16_constant_seroreversion_plot <- plot_seromodel(
  seromodel = TWN_HPV16_constant_seroreversion,
  serosurvey = TWN_HPV16
)
plot(TWN_HPV18_constant_seroreversion_plot)

# Modelo dependiente del tiempo sin seroreversion

init <- function() {
  list(foi_vector = rep(0.01, max(TWN_HPV16$age_max)))
}
TWN_HPV16_time_no_seroreversion <- fit_seromodel(
  serosurvey = TWN_HPV16,
  model_type = "time",
  foi_prior = sf_normal(),
  is_seroreversion = FALSE,
  init = init
)

TWN_HPV16_time_no_seroreversion_plot <- plot_seromodel(
  seromodel = TWN_HPV16_time_no_seroreversion,
  serosurvey = TWN_HPV16
)
plot(TWN_HPV16_time_no_seroreversion_plot)

# Modelo dependiente de la edad sin seroreversion

foi_index <- c(
  rep(1, 15),
  16:max(TWN_HPV18$age_max)
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

TWN_HPV16_age_no_seroreversion_plot <- plot_seromodel(
  seromodel = TWN_HPV16_age_no_seroreversion,
  serosurvey = TWN_HPV16
)
plot(TWN_HPV18_age_no_seroreversion_plot)

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
  seroreversion_prior = sf_normal(1, 0.5)
)

TWN_HPV16_age_seroreversion_plot <- plot_seromodel(
  seromodel = TWN_HPV16_age_seroreversion,
  serosurvey = TWN_HPV16
)
plot(TWN_HPV16_age_seroreversion_plot)


# Gráfica conjunta de todos los modelos

all_models_plot <- cowplot::plot_grid(
  TWN_HPV16_constant_no_seroreversion_plot,
  TWN_HPV16constant_seroreversion_plot,
  TWN_HPV16_time_no_seroreversion_plot,
  TWN_HPV16_age_no_seroreversion_plot,
  TWN_HPV16_age_seroreversion_plot,
  ncol = 5
)
plot(TWN_HPV16_all_models_plot)


### SEROENCUESTA  NIGERIA HPV 16 ###

## CREAR DATA.FRAME DE SEROENCUESTA ## 

dat_NGA_HPV16 <- data.frame (filter( dat, survey == "NGA-002-01"))

## SELECCCION DE DATOS QUE SE REQUIEREN PARA LOS MODELOS ##

NGA_HPV16 <- dat_NGA_HPV16  %>%
  mutate(
    survey = paste(survey, pathogen)
  ) %>%
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
  is_seroreversion = FALSE
)

NGA_HPV16_constant_no_seroreversion_plot <- serofoi:::plot_seromodel(
  seromodel = NGA_HPV16_constant_no_seroreversion,
  serosurvey = NGA_HPV16,
)

plot(NGA_HPV16_constant_no_seroreversion_plot)

# Modelo constante con seroreversion


NGA_HPV16_constant_seroreversion <- fit_seromodel(
  serosurvey = NGA_HPV16,
  model_type = "constant",
  foi_prior = sf_uniform(),
  is_seroreversion = TRUE
)

NGA_HPV16_constant_seroreversion_plot <- plot_seromodel(
  seromodel = NGA_HPV16_constant_seroreversion,
  serosurvey = NGA_HPV16
)
plot(TNGA_HPV16_constant_seroreversion_plot)

# Modelo dependiente del tiempo sin seroreversion

init <- function() {
  list(foi_vector = rep(0.01, max(NGA_HPV16$age_max)))
}
NGA_HPV16_time_no_seroreversion <- fit_seromodel(
  serosurvey = NGA_HPV16,
  model_type = "time",
  foi_prior = sf_normal(),
  is_seroreversion = FALSE,
  init = init
)

NGA_HPV16_time_no_seroreversion_plot <- plot_seromodel(
  seromodel = NGA_HPV16_time_no_seroreversion,
  serosurvey = NGA_HPV16
)
plot(NGA_HPV16_time_no_seroreversion_plot)

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

NGA_HPV16_age_no_seroreversion_plot <- plot_seromodel(
  seromodel = NGA_HPV16_age_no_seroreversion,
  serosurvey = NGA_HPV16
)
plot(NGA_HPV16_age_no_seroreversion_plot)

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
  seroreversion_prior = sf_normal(1, 0.5)
)

NGA_HPV16_age_seroreversion_plot <- plot_seromodel(
  seromodel = NGA_HPV16_age_seroreversion,
  serosurvey = NGA_HPV16
)
plot(NGA_HPV16_age_seroreversion_plot)


# Gráfica conjunta de todos los modelos

NGA_HPV16_models_plot <- cowplot::plot_grid(
  NGA_HPV16_constant_no_seroreversion_plot,
  NGA_HPV16constant_seroreversion_plot,
  NGA_HPV16_time_no_seroreversion_plot,
  NGA_HPV16_age_no_seroreversion_plot,
  NGA_HPV16_age_seroreversion_plot,
  ncol = 5
)
plot(NGA_HPV16_all_models_plot)


NGA_HPV16_all_models_plot <- cowplot::plot_grid(
  NGA_HPV16_time_no_seroreversion_plot,
  NGA_HPV16_age_no_seroreversion_plot,
  NGA_HPV16_age_seroreversion_plot,
  ncol = 3
)
plot( NGA_HPV16_all_models_plot)
jpeg(filename = "SEROFOI/plots/NGA_HPV16.jpeg", width = 480*2, height = 480*2) 
NGA_HPV16_all_models_plot
dev.off()

NGA_HPV16_constant_models_plot <- cowplot::plot_grid(
  NGA_HPV16_constant_no_seroreversion_plot,
  NGA_HPV16_constant_seroreversion_plot,
  ncol = 2)

plot(NGA_HPV16_constant_models_plot)
jpeg(filename = "SEROFOI/plots/NGA_HPV16_conts.jpeg", width = 480*2, height = 480*2) 
NGA_HPV16_constant_models_plot
dev.off()




