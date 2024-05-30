# SEROFOI ##

library(serofoi)
library(cowplot)
library(lubridate)
library(loo)
library(openxlsx)
library(ggplot2)
library(dplyr)
library(patchwork)

## CARGA BASE LIMPIA DE MODELOS ##

dat <- readRDS('data/data_for_models/clean_data_total_models.RDS')

## CREAR COLUMNA DE AÑO DE NACIMIENTO ##
dat0 <- dat %>%  mutate ( birth_year = tsur - age_mean_f)




options(mc.cores=4)

### SEROENCUESTA COLOMBIA VPH 18###

## CREAR DATA.FRAME DE SEROENCUESTA ## 

dat_col_HPV18 <- data.frame (filter( dat0, country == "COL")  %>% filter (pathogen == "HPV 18"))

## SELECCCION DE DATOS QUE SE REQUIEREN PARA LOS MODELOS ##

col_HPV18 <- dat_col_HPV18 %>%
  mutate(
    survey = paste(survey, pathogen)
  ) %>%
  select(survey, tsur, age_min, age_max, counts, total) %>%
  rename(
    n_seropositive = counts,
    sample_size = total)

plot_serosurvey(col_HPV18)


# Modelo constante sin seroreversion

col_HPV18_constant_no_seroreversion <- fit_seromodel(
  serosurvey = col_HPV18,
  model_type = "constant",
  foi_prior = sf_uniform() ,
  is_seroreversion = FALSE
)

col_HPV18_constant_no_seroreversion_plot <- serofoi:::plot_seromodel(
  seromodel = col_HPV18_constant_no_seroreversion,
  serosurvey = col_HPV18,
  )

plot(col_HPV18_constant_no_seroreversion_plot)

# Modelo constante con seroreversion

col_HPV18_constant_seroreversion <- fit_seromodel(
  serosurvey = col_HPV18,
  model_type = "constant",
  foi_prior = sf_uniform(),
  is_seroreversion = TRUE
)

col_HPV18_constant_seroreversion_plot <- plot_seromodel(
  seromodel = col_HPV18_constant_seroreversion,
  serosurvey = col_HPV18
)
plot(col_HPV18_constant_seroreversion_plot)

# Modelo dependiente del tiempo sin seroreversion
init <- function() {
  list(foi_vector = rep(0.01, max(col_HPV18$age_max)))
}
col_HPV18_time_no_seroreversion <- fit_seromodel(
  serosurvey = col_HPV18,
  model_type = "time",
  foi_prior = sf_normal(),
  is_seroreversion = FALSE,
  init = init
)

col_HPV18_time_no_seroreversion_plot <- plot_seromodel(
  seromodel = col_HPV18_time_no_seroreversion,
  serosurvey = col_HPV18
)
plot(col_HPV18_time_no_seroreversion_plot)

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

col_HPV18_age_no_seroreversion_plot <- plot_seromodel(
  seromodel = col_HPV18_age_no_seroreversion,
  serosurvey = col_HPV18
)
plot(col_HPV18_age_no_seroreversion_plot)

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
  seroreversion_prior = sf_normal(1, 0.5)
)

col_HPV18l_age_seroreversion_plot <- plot_seromodel(
  seromodel = col_HPV18_age_seroreversion,
  serosurvey = col_HPV18
)
plot(col_HPV18_age_seroreversion_plot)


# Gráfica conjunta de todos los modelos

all_models_plot <- cowplot::plot_grid(
  col_HPV18_constant_no_seroreversion_plot,
  col_HPV18_constant_seroreversion_plot,
  col_HPV18_time_no_seroreversion_plot,
  col_HPV18_age_no_seroreversion_plot,
  col_HPV18_age_seroreversion_plot,
  ncol = 5
)
plot(col_HPV18_all_models_plot)

### SEROENCUESTA COLOMBIA VPH 16###

## CREAR DATA.FRAME DE SEROENCUESTA ## 

dat_col_HPV16 <- data.frame (filter( dat0, country == "COL")  %>% filter (pathogen == "HPV 16"))

## SELECCCION DE DATOS QUE SE REQUIEREN PARA LOS MODELOS ##

col_HPV16 <- dat_col_HPV16 %>%
  mutate(
    survey = paste(survey, pathogen)
  ) %>%
  select(survey, tsur, age_min, age_max, counts, total) %>%
  rename(
    n_seropositive = counts,
    sample_size = total)

plot_serosurvey(col_HPV16)


# Modelo constante sin seroreversion

col_HPV16_constant_no_seroreversion <- fit_seromodel(
  serosurvey = col_HPV16,
  model_type = "constant",
  foi_prior = sf_uniform() ,
  is_seroreversion = FALSE
)

col_HPV16_constant_no_seroreversion_plot <- serofoi:::plot_seromodel(
  seromodel = col_HPV16_constant_no_seroreversion,
  serosurvey = col_HPV16,
)

plot(col_HPV16_constant_no_seroreversion_plot)

# Modelo constante con seroreversion

col_HPV16_constant_seroreversion <- fit_seromodel(
  serosurvey = col_HPV16,
  model_type = "constant",
  foi_prior = sf_uniform(),
  is_seroreversion = TRUE
)

col_HPV16_constant_seroreversion_plot <- plot_seromodel(
  seromodel = col_HPV16_constant_seroreversion,
  serosurvey = col_HPV16
)
plot(col_HPV16_constant_seroreversion_plot)

# Modelo dependiente del tiempo sin seroreversion
init <- function() {
  list(foi_vector = rep(0.01, max(col_HPV16$age_max)))
}
col_HPV16_time_no_seroreversion <- fit_seromodel(
  serosurvey = col_HPV16,
  model_type = "time",
  foi_prior = sf_normal(),
  is_seroreversion = FALSE,
  init = init
)

col_HPV16_time_no_seroreversion_plot <- plot_seromodel(
  seromodel = col_HPV16_time_no_seroreversion,
  serosurvey = col_HPV16
)
plot(col_HPV16_time_no_seroreversion_plot)

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

col_HPV16_age_no_seroreversion_plot <- plot_seromodel(
  seromodel = col_HPV16_age_no_seroreversion,
  serosurvey = col_HPV16
)
plot(col_HPV16_age_no_seroreversion_plot)

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
  seroreversion_prior = sf_normal(1, 0.5)
)

col_HPV16_age_seroreversion_plot <- plot_seromodel(
  seromodel = col_HPV16_age_seroreversion,
  serosurvey = col_HPV16
)
plot(col_HPV16_age_seroreversion_plot)


# Gráfica conjunta de todos los modelos

all_models_plot <- cowplot::plot_grid(
  col_HPV16_constant_no_seroreversion_plot,
  col_HPV16_constant_seroreversion_plot,
  col_HPV16_time_no_seroreversion_plot,
  col_HPV16_age_no_seroreversion_plot,
  col_HPV16_age_seroreversion_plot,
  ncol = 5
)
plot(col_HPV16_all_models_plot)






### SEROENCUESTA COLOMBIA VPH 16/18 ###

## CREAR DATA.FRAME DE SEROENCUESTA ## 

dat_col_HPVHr <- data.frame (filter( dat0, country == "COL")  %>% filter (pathogen == "HPV 16/18"))

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

col_HPVHr_constant_no_seroreversion_plot <- serofoi:::plot_seromodel(
  seromodel = col_HPVHr_constant_no_seroreversion,
  serosurvey = col_HPVHr,
)

plot(col_HPVHr_constant_no_seroreversion_plot)

# Modelo constante con seroreversion

col_HPVHr_constant_seroreversion <- fit_seromodel(
  serosurvey = col_HPVHr,
  model_type = "constant",
  foi_prior = sf_uniform(),
  is_seroreversion = TRUE
)

col_HPVHr_constant_seroreversion_plot <- plot_seromodel(
  seromodel = col_HPVHr_constant_seroreversion,
  serosurvey = col_HPVHr
)
plot(col_HPVHr_constant_seroreversion_plot)

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

col_HPVHr_time_no_seroreversion_plot <- plot_seromodel(
  seromodel = col_HPVHr_time_no_seroreversion,
  serosurvey = col_HPV16
)
plot(col_HPVHr_time_no_seroreversion_plot)

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

col_HPVHr_age_no_seroreversion_plot <- plot_seromodel(
  seromodel = col_HPVHr_age_no_seroreversion,
  serosurvey = col_HPVHr
)
plot(col_HPVHr_age_no_seroreversion_plot)

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

col_HPVHr_age_seroreversion_plot <- plot_seromodel(
  seromodel = col_HPVHr_age_seroreversion,
  serosurvey = col_HPVHr
)
plot(col_HPVHr_age_seroreversion_plot)


# Gráfica conjunta de todos los modelos

all_models_plot <- cowplot::plot_grid(
  col_HPVHr_constant_no_seroreversion_plot,
  col_HPVHr_constant_seroreversion_plot,
  col_HPVHr_time_no_seroreversion_plot,
  col_HPVHr_age_no_seroreversion_plot,
  col_HPVHr_age_seroreversion_plot,
  ncol = 5
)
plot(col_HPV16_all_models_plot)


### SEROENCUESTA COSTA RICA VPH 18 ###

## CREAR DATA.FRAME DE SEROENCUESTA ## 

dat_CRI_HPV18 <- data.frame (filter( dat0, country == "CRI")  %>% filter (pathogen == "HPV 18"))

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

CRI_HPV18_constant_no_seroreversion_plot <- serofoi:::plot_seromodel(
  seromodel = CRI_HPV18_constant_no_seroreversion,
  serosurvey = CRI_HPV18,
)

plot(CRI_HPV18_constant_no_seroreversion_plot)

# Modelo constante con seroreversion


CRI_HPV18_constant_seroreversion <- fit_seromodel(
  serosurvey = CRI_HPV18,
  model_type = "constant",
  foi_prior = sf_uniform(),
  is_seroreversion = TRUE
)

CRI_HPV18_constant_seroreversion_plot <- plot_seromodel(
  seromodel = CRI_HPV18_constant_seroreversion,
  serosurvey = CRI_HPV18
)
plot(CRI_HPV18_constant_seroreversion_plot)

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

CRI_HPV18_time_no_seroreversion_plot <- plot_seromodel(
  seromodel = CRI_HPV18_time_no_seroreversion,
  serosurvey = CRI_HPV18
)
plot(CRI_HPV18_time_no_seroreversion_plot)

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

CRI_HPV18_age_no_seroreversion_plot <- plot_seromodel(
  seromodel = CRI_HPV18_age_no_seroreversion,
  serosurvey = CRI_HPV18
)
plot(CRI_HPV18_age_no_seroreversion_plot)

# Modelo dependiente de la edad con seroreversion

foi_index <- c(
  rep(1, 15),
  16:max(CRI_HPV18r$age_max)
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

CRI_HPV18_age_seroreversion_plot <- plot_seromodel(
  seromodel = CRI_HPV18_age_seroreversion,
  serosurvey = CRI_HPV18
)
plot(CRI_HPV18_age_seroreversion_plot)


# Gráfica conjunta de todos los modelos

all_models_plot <- cowplot::plot_grid(
  CRI_HPV18_constant_no_seroreversion_plot,
  CRI_HPV18_constant_seroreversion_plot,
  CRI_HPV18_time_no_seroreversion_plot,
  CRI_HPV18_age_no_seroreversion_plot,
  CRI_HPV18_age_seroreversion_plot,
  ncol = 5
)
plot(CRI_HPV18_all_models_plot)


### SEROENCUESTA COSTA RICA VPH 16 ###

## CREAR DATA.FRAME DE SEROENCUESTA ## 

dat_CRI_HPV16 <- data.frame (filter( dat0, country == "CRI")  %>% filter (pathogen == "HPV 16"))

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

all_models_plot <- cowplot::plot_grid(
  CRI_HPV16_constant_no_seroreversion_plot,
  CRI_HPV16_constant_seroreversion_plot,
  CRI_HPV16_time_no_seroreversion_plot,
  CRI_HPV16_age_no_seroreversion_plot,
  CRI_HPV16_age_seroreversion_plot,
  ncol = 5
)
plot(CRI_HPV16_all_models_plot)


### SEROENCUESTA BRASIL HPV 16  ###

## CREAR DATA.FRAME DE SEROENCUESTA ## 

dat_BRA_HPV16 <- data.frame (filter( dat0, survey == "BRA-017-01"))

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

all_models_plot <- cowplot::plot_grid(
  BRA_HPV16_constant_no_seroreversion_plot,
  BRA_HPV16_constant_seroreversion_plot,
  BRA_HPV16_time_no_seroreversion_plot,
  BRA_HPV16_age_no_seroreversion_plot,
  BRA_HPV16_age_seroreversion_plot,
  ncol = 5
)
plot(BRA_HPV16_all_models_plot)

### SEROENCUESTA ESTADOS UNIDOS HPV 16 ###

## CREAR DATA.FRAME DE SEROENCUESTA ## 

dat_USA92_HPV16 <- data.frame (filter( dat0, survey == "USA-011-03"))

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

all_models_plot <- cowplot::plot_grid(
  USA92_HPV16_constant_no_seroreversion_plot,
  USA92_HPV16_constant_seroreversion_plot,
  USA92_HPV16_time_no_seroreversion_plot,
  USA92_HPV16_age_no_seroreversion_plot,
  USA92_HPV16_age_seroreversion_plot,
  ncol = 5
)
plot(PRI_HPV16_all_models_plot)


### SEROENCUESTA PUERTO RICO HPV 16/18  ###

## CREAR DATA.FRAME DE SEROENCUESTA ## 

dat_PRI_HPVHr <- data.frame (filter( dat0, survey == "PRI-001-02"))

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

all_models_plot <- cowplot::plot_grid(
  PRI_HPVHr_constant_no_seroreversion_plot,
  PRI_HPVHr_constant_seroreversion_plot,
  PRI_HPVHr_time_no_seroreversion_plot,
  PRI_HPVHr_age_no_seroreversion_plot,
  PRI_HPVHr_age_seroreversion_plot,
  ncol = 5
)
plot(PRI_HPVHr_all_models_plot)






### SEROENCUESTA ESTADOS UNIDOS (1992) HPV 16 ###

## CREAR DATA.FRAME DE SEROENCUESTA ## 

dat_USA92_HPV16 <- data.frame (filter( dat0, survey == "USA-011-03"))

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

all_models_plot <- cowplot::plot_grid(
  USA92_HPV16_constant_no_seroreversion_plot,
  USA92_HPV16_constant_seroreversion_plot,
  USA92_HPV16_time_no_seroreversion_plot,
  USA92_HPV16_age_no_seroreversion_plot,
  USA92_HPV16_age_seroreversion_plot,
  ncol = 5
)
plot(PRI_HPV16_all_models_plot)


### SEROENCUESTA ESTADO UNIDOS (2003) HPV 18  ###

## CREAR DATA.FRAME DE SEROENCUESTA ## 

dat_USA_HPV18 <- data.frame (filter( dat0, survey == "USA-026-04"))

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

all_models_plot <- cowplot::plot_grid(
  USA_HPV18_constant_no_seroreversion_plot,
  USA_HPV18_constant_seroreversion_plot,
  USA_HPV18_time_no_seroreversion_plot,
  USA_HPV18_age_no_seroreversion_plot,
  USA_HPV18_age_seroreversion_plot,
  ncol = 5
)
plot(USA_HPV18_all_models_plot)




### SEROENCUESTA ESTADO UNIDOS (2003) HPV 16  ###

## CREAR DATA.FRAME DE SEROENCUESTA ## 

dat_USA_HPV16 <- data.frame (filter( dat0, survey == "USA-026-03"))

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

USA_HPV16_constant_no_seroreversion_plot <- serofoi:::plot_seromodel(
  seromodel = USA_HPV16_constant_no_seroreversion,
  serosurvey = USA_HPV16,
)

plot(USA_HPV18_constant_no_seroreversion_plot)

# Modelo constante con seroreversion


USA_HPV16_constant_seroreversion <- fit_seromodel(
  serosurvey = USA_HPV16,
  model_type = "constant",
  foi_prior = sf_uniform(),
  is_seroreversion = TRUE
)

USA_HPV16_constant_seroreversion_plot <- plot_seromodel(
  seromodel = USA_HPV16_constant_seroreversion,
  serosurvey = USA_HPV16
)
plot(USA_HPV18_constant_seroreversion_plot)

# Modelo dependiente del tiempo sin seroreversion

init <- function() {
  list(foi_vector = rep(0.01, max(USA_HPV18$age_max)))
}
USA_HPV16_time_no_seroreversion <- fit_seromodel(
  serosurvey = USA_HPV18,
  model_type = "time",
  foi_prior = sf_normal(),
  is_seroreversion = FALSE,
  init = init
)

USA_HPV16_time_no_seroreversion_plot <- plot_seromodel(
  seromodel = USA_HPV18_time_no_seroreversion,
  serosurvey = USA_HPV18
)
plot(USA_HPV16_time_no_seroreversion_plot)

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

USA_HPV16_age_no_seroreversion_plot <- plot_seromodel(
  seromodel = USA_HPV16_age_no_seroreversion,
  serosurvey = USA_HPV16
)
plot(USA_HPV16_age_no_seroreversion_plot)

# Modelo dependiente de la edad con seroreversion

foi_index <- c(
  rep(1, 15),
  16:max(USA_HPV16$age_max)
)


init <- function() {
  list(foi_vector = rep(0.01, max(foi_index)))
}

USA_HPV16_age_seroreversion <- fit_seromodel(
  serosurvey = USA_HPV18,
  model_type = "age",
  foi_prior = sf_normal(1e-4, 1e-5),
  foi_index = foi_index,
  is_seroreversion = TRUE,
  seroreversion_prior = sf_normal(1, 0.5)
)

USA_HPV16_age_seroreversion_plot <- plot_seromodel(
  seromodel = USA_HPV16_age_seroreversion,
  serosurvey = USA_HPV16
)
plot(USA_HPV16_age_seroreversion_plot)


# Gráfica conjunta de todos los modelos

all_models_plot <- cowplot::plot_grid(
  USA_HPV16_constant_no_seroreversion_plot,
  USA_HPV16_constant_seroreversion_plot,
  USA_HPV16_time_no_seroreversion_plot,
  USA_HPV16_age_no_seroreversion_plot,
  USA_HPV16_age_seroreversion_plot,
  ncol = 5
)
plot(USA_HPV16_all_models_plot)



### SEROENCUESTA  TAIWAN HPV 18 ###

## CREAR DATA.FRAME DE SEROENCUESTA ## 

dat_TWN_HPV16 <- data.frame (filter( dat0, survey == "TWN-025-01"))

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

dat_NGA_HPV16 <- data.frame (filter( dat0, survey == "NGA-002-01"))

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

all_models_plot <- cowplot::plot_grid(
  NGA_HPV16_constant_no_seroreversion_plot,
  NGA_HPV16constant_seroreversion_plot,
  NGA_HPV16_time_no_seroreversion_plot,
  NGA_HPV16_age_no_seroreversion_plot,
  NGA_HPV16_age_seroreversion_plot,
  ncol = 5
)
plot(NGA_HPV16_all_models_plot)




