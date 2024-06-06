## pak::pkg_install("epiverse-trace/serofoi@full-refac-test")## instalacion ##

library(serofoi)
# library(openxlsx)
library(dplyr)
library(ggplot2)
options(mc.cores=4)

# Cargando todos los datos
all_serosurveys <- readRDS('inst/extdata/clean_data_total_models.RDS')

# VPH 18 Colombia
col_HPV18 <- all_serosurveys %>% filter(
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
  foi_prior = sf_normal()
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
  init = init
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

## Gráficas conjuntas
col_HPV18_all_models_plot <- cowplot::plot_grid(
  col_HPV18_constant_no_seroreversion_plot,
  col_HPV18_constant_seroreversion_plot,
  col_HPV18_time_no_seroreversion_plot,
  col_HPV18_age_no_seroreversion_plot,
  col_HPV18_age_seroreversion_plot,
  ncol = 5
)
plot(col_HPV18_all_models_plot)

col_HPV18_all_models_plot <- cowplot::plot_grid(
  col_HPV18_time_no_seroreversion_plot,
  col_HPV18_age_no_seroreversion_plot,
  col_HPV18_age_seroreversion_plot,
  ncol = 3
)
plot(col_HPV18_all_models_plot)

col_HPV18_constant_models_plot <- cowplot::plot_grid(
  col_HPV18_constant_no_seroreversion_plot,
  col_HPV18_constant_seroreversion_plot,
  ncol = 2)

plot(col_HPV18_constant_models_plot)

# Tabla de resumen

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
  size_text = size_text
)

## Edad - no seroreversion
col_HPV18_age_no_seroreversion_plot_foi <- plot_foi_estimates(
  seromodel = col_HPV18_age_no_seroreversion,
  serosurvey = col_HPV18,
  size_text = size_text
)

## Edad - seroreversion
col_HPV18_age_seroreversion_plot_foi <- plot_foi_estimates(
  seromodel = col_HPV18_age_seroreversion,
  serosurvey = col_HPV18,
  size_text = size_text
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
