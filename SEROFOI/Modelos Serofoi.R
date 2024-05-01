# SEROFOI ##

library(serofoi)
library(cowplot)
library(lubridate)
library(loo)
library(openxlsx)
library(ggplot2)
library(dplyr)
library(patchwork)


#### instalacion de SEROFOI en caso de cambios ###

pak::pkg_install("epiverse-trace/serofoi@test-av-models")

## CARGA BASE LIMPIA DE MODELOS ##

dat <- readRDS('data/data_for_models/clean_data_total_models.RDS') 


#### SEROENCUESTAS COLOMBIA #####

## CREAR DATA.FRAME DE SEROENCUESTA ## 

dat_col <- data.frame (filter( dat, country == "COL"))


# crear columna birth_year para todos los serotipos - preparacion de datos SEROFOI VPH 18 ##

dat_colp <- dat_col %>%  mutate ( birth_year = tsur - age_mean_f)

dat_col_HPV18 <- data.frame (filter (dat_colp, pathogen == "HPV 18"))

## CREAR VARIABLE LAMBA PARA  ESCALA EJE X DE GRAFICAS DE LOS MODELOS ##

max_lambda <- 0.02


#FOI constate  COLOMBIA HPV 18 ##

HPV_constant_HPV18col <- fit_seromodel(serodata = dat_col_HPV18,
                               foi_model = "constant",
                               iter = 1000)


HPV_constant_plot_HPV18col <- plot_seromodel(HPV_constant_HPV18col, 
                                     serodata = dat_col_HPV18, 
                                     size_text = 12, max_lambda = max_lambda, 
                                     ylim_prev = c(0.0, 0.3))


## normal - dependiente del tiempo  COLOMBIA HPV 18 ##

HPV_time_HPV18col <- fit_seromodel(serodata = dat_col_HPV18,
                             foi_model = "tv_normal",
                             # foi_parameters = list(foi_location = 0, foi_scale = 1),
                             iter = 1500,
                             chunk_size = 5)

HPV_time_plot_HPV18col <- plot_seromodel(HPV_time_HPV18col, 
                                   serodata = dat_col_HPV18, 
                                   size_text = 12,max_lambda = max_lambda,
                                   ylim_prev = c(0.0, 0.3))

# Dependiente de la edad  COLOMBIA HPV 18 ## 

HPV_age_HPV18col <- fit_seromodel(serodata = dat_col_HPV18,
                                     foi_model = "av_normal",
                                     # foi_parameters = list(foi_location = 0, foi_scale = 1),
                                     iter = 1500,
                                  chunk_size = 5)

HPV_age_plot_HPV18col <- plot_seromodel(HPV_age_HPV18col, 
                                           serodata = dat_col_HPV18, 
                                           size_text = 12,max_lambda = max_lambda,
                                            ylim_prev = c(0.0, 0.3))


cowplot::plot_grid(HPV_constant_plot_HPV18col, HPV_time_plot_HPV18col,HPV_age_plot_HPV18col ,ncol = 3)

col_hpv18 <- cowplot::plot_grid(HPV_constant_plot_HPV18col, HPV_time_plot_HPV18col,HPV_age_plot_HPV18col ,ncol = 3) 
jpeg(filename = "SEROFOI/plots/col_hpv18.jpeg", width = 480*2, height = 480*2) 
col_hpv18 
dev.off()


rm(list = ls(pattern = "_HPV18col"))


## AJUSTE DE LOS MODELOS COLOMBIA VPH 18 ### 


HPV_constant_HPV18col <- run_seromodel(serodata = dat_col_HPV18, foi_model = "constant", n_iters = 1000)
HPV_time_HPV18col <- run_seromodel(serodata = dat_col_HPV18, foi_model = "tv_normal", n_iters = 1500)
HPV_age_HPV18col <- run_seromodel(serodata = dat_col_HPV18, foi_model = "tv_normal", n_iters = 1500)



fit1 <- list(constant = HPV_constant_HPV18col, normal = HPV_normal_HPV18col)
waic1 <- mapply(function(z) loo::waic(loo::extract_log_lik(z, parameter_name = "logLikelihood"))$estimates[3, ], fit1)
waic1 <- as.data.frame(waic1)


openxlsx::write.xlsx(waic1, "WAIC_colHPV18.xlsx")
 

loo_fit_constant <- loo::loo(fit1$constant, save_psis = FALSE, pars = c(parameter_name = "logLikelihood"))
loo_fit_constant <- loo_fit_constant$estimates[3, , drop=FALSE]

loo_fit_normal <- loo::loo(fit1$normal, save_psis = FALSE, pars = c(parameter_name = "logLikelihood"))
loo_fit_normal <- loo_fit_normal$estimates[3, , drop=FALSE]


loo <- rbind(loo_fit_constant, 
             loo_fit_normal,
             )

loocolVPH18col <- data.frame(loo)


openxlsx::write.xlsx(loocolVPH18col, "LOO_HPV18col.xlsx")


#####  MODELOS  HPV 16 COLOMBIA #######

dat_col_HPV16 <- data.frame (filter (dat_colp, pathogen == "HPV 16"))


#FOI constate  COLOMBIA HPV 16 ##

HPV_constant_HPV16col <- fit_seromodel(serodata = dat_col_HPV16,
                              foi_model = "constant",
                              iter = 1000)
                              

HPV_constant_plot_HPV16col <- plot_seromodel(HPV_constant_HPV16col, 
                                    serodata = dat_col_HPV16, 
                                    size_text = 12, max_lambda = max_lambda,   
                                    ylim_prev = c(0.0, 0.4))
                                 

## Dependiete del tiempo ##

HPV_time_HPV16col <- fit_seromodel(serodata = dat_col_HPV16,
                            foi_model = "tv_normal",
                            # foi_parameters = list(foi_location = 0, foi_scale = 1),
                            iter = 1500,
                            chunk_size = 5)

HPV_time_plot_HPV16col<- plot_seromodel(HPV_time_HPV16col, 
                                  serodata = dat_col_HPV16, 
                                  size_text = 12, max_lambda = max_lambda,
                                  ylim_prev = c(0.0, 0.4))

#### Dependiente de la edad  ####

HPV_age_HPV16col <- fit_seromodel(serodata = dat_col_HPV16,
                                  foi_model = "av_normal",
                                  # foi_parameters = list(foi_location = 0, foi_scale = 1),
                                  iter = 1500,
                                  chunk_size = 5)

HPV_age_plot_HPV16col <- plot_seromodel(HPV_age_HPV16col, 
                                        serodata = dat_col_HPV16, 
                                        size_text = 12,max_lambda = max_lambda,
                                        ylim_prev = c(0.0, 0.4))




cowplot::plot_grid(HPV_constant_plot_HPV16col , HPV_time_plot_HPV16col, HPV_age_plot_HPV16col, ncol = 3)

col_hpv16 <-cowplot::plot_grid(HPV_constant_plot_HPV16col , HPV_time_plot_HPV16col, HPV_age_plot_HPV16col, ncol = 3)
jpeg(filename = "SEROFOI/plots/col_hpv16.jpeg", width = 480*2, height = 480*2) 
col_hpv16 
dev.off()

rm(list = ls(pattern = "_HPV16col"))


## AJUSTE DE LOS MODELOS VPH16### 

constant_HPV16col <- run_seromodel(serodata = dat_col_HPV16, foi_model = "constant", n_iters = 1000)
normal_HPV16col <- run_seromodel(serodata = dat_col_HPV16, foi_model = "tv_normal", n_iters = 1500)
normal_log_HPV16col <- run_seromodel(serodata = dat_col_HPV16, foi_model = "tv_normal_log", n_iters = 1500)

## revisar ajustes
fit2 <- list(constant = HPV_constant, normal = HPV_normal, normal_log = HPV_normal_log)
waic2 <- mapply(function(z) loo::waic(loo::extract_log_lik(z, parameter_name = "logLikelihood"))$estimates[3, ], fit2)
waic2 <- as.data.frame(waic2)


openxlsx::write.xlsx(waic2, "WAIC_VPH16.xlsx")


### MODELOS COLOMBIA HPV 16-18 ###

# crear columna birth_year para HPV 16-18 para datos preparados

dat_col_HPVhr <- data.frame (filter (dat_colp, pathogen == "HPV 16/18"))


#FOI constate  COLOMBIA HPV 16-18####

HPV_constant_HPVhrcol <- fit_seromodel(serodata = dat_col_HPVhr,
                                foi_model = "constant",
                                iter = 1000)

HPV_constant_plot_HPVhrcol <- plot_seromodel(HPV_constant_HPVhrcol, 
                                      serodata = dat_col_HPVhr, 
                                    size_text = 10, max_lambda = max_lambda,
                                    ylim_prev = c(0.0, 0.5))

## Dependiente del tiempo ##
HPV_time_HPVhrcol <- fit_seromodel(serodata = dat_col_HPVhr,
                                   foi_model = "tv_normal",
                                   # foi_parameters = list(foi_location = 0, foi_scale = 1),
                                   iter = 1500,
                                   chunk_size = 5)

HPV_time_plot_HPVhrcol<- plot_seromodel(HPV_time_HPVhrcol, 
                                        serodata = dat_col_HPVhr, 
                                        size_text = 10, max_lambda = max_lambda,
                                        ylim_prev = c(0.0, 0.5))


#### Dependiente de la edad  ####

HPV_age_HPVhrcol <- fit_seromodel(serodata = dat_col_HPVhr,
                                  foi_model = "av_normal",
                                  # foi_parameters = list(foi_location = 0, foi_scale = 1),
                                  iter = 1500,
                                  chunk_size = 5)

HPV_age_plot_HPVhrcol <- plot_seromodel(HPV_age_HPVhrcol, 
                                        serodata = dat_col_HPVhr, 
                                        size_text = 10,max_lambda = max_lambda, 
                                        ylim_prev = c(0.0, 0.5))



cowplot::plot_grid(HPV_constant_plot_HPVhrcol, HPV_time_plot_HPVhrcol,HPV_age_plot_HPVhrcol,ncol = 3)
col_HPVhrcol <- cowplot::plot_grid(HPV_constant_plot_HPVhrcol, HPV_time_plot_HPVhrcol,HPV_age_plot_HPVhrcol,ncol = 3)
jpeg(filename = "SEROFOI/plots/col_hr.jpeg", width = 480*2, height = 480*2) 
col_HPVhrcol
dev.off()

rm(list = ls(pattern = "_HPVhrcol"))



## AJUSTE DE LOS MODELOS VPH16/18### 

constant_HPV16 <- run_seromodel(serodata = dat_col_HPVhr, foi_model = "constant", n_iters = 1000)
normal_HPV16 <- run_seromodel(serodata = dat_col_HPVhr, foi_model = "tv_normal", n_iters = 1500)
normal_log_HPV16 <- run_seromodel(serodata = dat_col_HPVhr, foi_model = "tv_normal_log", n_iters = 1500)

fit3 <- list(constant = constant_plot_HPVhr, normal = normal_plot_HPVhr, normal_log = normal_log_plot_HPVhr)
waic3 <- mapply(function(z) loo::waic(loo::extract_log_lik(z, parameter_name = "logLikelihood"))$estimates[3, ], fit3)
waic3 <- as.data.frame(waic3)


openxlsx::write.xlsx(waic3, "WAIC_VPHhr.xlsx")



## FOI para Costa Rica ##

dat_cri <- data.frame (filter( dat, country == "CRI"))



# crear columna birth_year para todos los serotipos - preparacion de datos SEROFOI

dat_crip <- dat_cri %>%  mutate ( birth_year = tsur - age_mean_f)

dat_cri_HPV18 <- data.frame (filter (dat_crip, pathogen == "HPV 18"))


#FOI constate  HPV 18 COSTA RICA##
HPV_constant_HPV18cri <- fit_seromodel(serodata = dat_cri_HPV18,
                                    foi_model = "constant",
                                   iter = 1000)

HPV_constant_plot_HPV18cri <- plot_seromodel(HPV_constant_HPV18cri, 
                                          serodata = dat_cri_HPV18, 
                                          size_text = 12,max_lambda = 0.006,
                                          ylim_prev = c(0.0, 0.5))

## FOI dependiente de tiempo HPV 18 COSTA RICA##

HPV_time_HPV18cri <- fit_seromodel(serodata = dat_cri_HPV18,
                                  foi_model = "tv_normal",
                                  # foi_parameters = list(foi_location = 0, foi_scale =1),
                                  iter = 1500,
                                  chunk_size = 5)

HPV_time_plot_HPV18cri <- plot_seromodel(HPV_time_HPV18cri, 
                                        serodata = dat_cri_HPV18, 
                                        size_text = 12, max_lambda = 0.006,
                                        ylim_prev = c(0.0, 0.5))




## FOI dependiente de edad HPV 18 COSTA RICA##

HPV_age_HPV18cri <- fit_seromodel(serodata = dat_cri_HPV18,
                                  foi_model = "av_normal",
                                  foi_parameters = list(foi_location = 0, foi_scale = 1),
                                  iter = 1500)

HPV_age_plot_HPV18cri <- plot_seromodel(HPV_age_HPV18cri , 
                                        serodata = dat_cri_HPV18, 
                                        size_text = 12,max_lambda = 0.006,
                                        ylim_prev = c(0.0, 0.5))


cowplot::plot_grid(HPV_constant_plot_HPV18cri, HPV_time_plot_HPV18cri, HPV_age_plot_HPV18cri, ncol = 3)




## AJUSTE DE LOS MODELOS  CRI VPH18 ### 

constant_HPV16 <- run_seromodel(serodata = dat_col_HPVhr, foi_model = "constant", n_iters = 1000)
normal_HPV16 <- run_seromodel(serodata = dat_col_HPVhr, foi_model = "tv_normal", n_iters = 1500)
normal_log_HPV16 <- run_seromodel(serodata = dat_col_HPVhr, foi_model = "tv_normal_log", n_iters = 1500)

fit3 <- list(constant = constant_plot_HPVhr, normal = normal_plot_HPVhr, normal_log = normal_log_plot_HPVhr)
waic3 <- mapply(function(z) loo::waic(loo::extract_log_lik(z, parameter_name = "logLikelihood"))$estimates[3, ], fit3)
waic3 <- as.data.frame(waic3)


openxlsx::write.xlsx(waic3, "WAIC_VPHhr.xlsx")

## MODELO COSTA RICA HPV 16 ##

dat_cri <- data.frame (filter( dat, country == "CRI"))

# preparacion de datos SEROFOI para VPH16

dat_cri_HPV16 <- data.frame (filter (dat_crip, pathogen == "HPV 16"))


#FOI constate  HPV 16 COSTA RICA ##

HPV_constant_HPV16cri <- fit_seromodel(serodata = dat_cri_HPV16,
                                       foi_model = "constant",
                                       iter = 1000)

HPV_constant_plot_HPV16cri <- plot_seromodel(HPV_constant_HPV16cri, 
                                             serodata = dat_cri_HPV16, 
                                             size_text = 12, max_lambda = 0.06)

## FOI DEPENDIENTE DE TIEMPO COSTA RICA VPH 16#

HPV_time_HPV16cri <- fit_seromodel(serodata = dat_cri_HPV16,
                                   foi_parameters = list(foi_location = 0, foi_scale = 1),
                                    foi_model = "tv_normal",
                                     iter = 1500)

HPV_time_plot_HPV16cri <- plot_seromodel(HPV_time_HPV16cri, 
                                           serodata = dat_cri_HPV16, 
                                           size_text = 12, max_lambda = 0.06)

## FOI dependiente de edad HPV 16 COSTA RICA##

HPV_age_HPV16cri <- fit_seromodel(serodata = dat_cri_HPV16,
                                  foi_model = "av_normal",
                                  foi_parameters = list(foi_location = 0, foi_scale = 1),
                                  iter = 1500)

HPV_age_plot_HPV16cri <- plot_seromodel(HPV_age_HPV16cri , 
                                        serodata = dat_cri_HPV16, 
                                        size_text = 12,max_lambda = 0.06)


cowplot::plot_grid(HPV_constant_plot_HPV16cri, HPV_time_plot_HPV16cri, HPV_age_plot_HPV16cri, ncol = 3)







## MODELO BRASIL HPV 16 ##

dat_bra_HPV16<- data.frame (filter( dat, survey == "BRA-017-01"))

# crear columna birth_year para todos los serotipos - preparacion de datos SEROFOI

dat_bra_HPV16 <- dat_bra_HPV16 %>%  mutate ( birth_year = tsur - age_mean_f)


#FOI constate   Brasil HPV 16
HPV_constant_HPV16bra <- fit_seromodel(serodata = dat_bra_HPV16,
                                       foi_model = "constant",
                                       iter = 1000)

HPV_constant_plot_HPV16bra <- plot_seromodel(HPV_constant_HPV16bra, 
                                             serodata = dat_bra_HPV16, 
                                             size_text = 12, max_lambda = 0.06,
                                             ylim_prev = c(0.0, 0.5))

## dependiet del tiempo Brasil VPH 16#
HPV_time_HPV16bra <- fit_seromodel(serodata = dat_bra_HPV16,
                                     foi_model = "tv_normal",
                                     # foi_parameters = list(foi_location =0, foi_scale=1),
                                     iter = 1500,
                                   chunk_size = 5)


HPV_time_plot_HPV16bra <- plot_seromodel(HPV_time_HPV16bra, 
                                           serodata = dat_bra_HPV16, 
                                           size_text = 12, max_lambda = 0.06,
                                         ylim_prev = c(0.0, 0.5))

# FOI dependiente de edad HPV 16 Brasil##

HPV_age_HPV16bra <- fit_seromodel(serodata = dat_bra_HPV16,
                                  foi_model = "av_normal",
                                  # foi_parameters = list(foi_location = 0, foi_scale = 1),
                                  iter = 1500,
                                  chunk_size = 5)

HPV_age_plot_HPV16bra <- plot_seromodel(HPV_age_HPV16bra , 
                                        serodata = dat_bra_HPV16, 
                                        size_text = 12,max_lambda = 0.06,
                                        ylim_prev = c(0.0, 0.5))


HPV16bra <- cowplot::plot_grid(HPV_constant_plot_HPV16bra, HPV_time_plot_HPV16bra,HPV_age_plot_HPV16bra, ncol = 3)
jpeg(filename = "SEROFOI/plots/HPV16bra.jpeg", width = 480*2, height = 480*2) 
HPV16bra 
dev.off()

rm(list = ls(pattern = "_HPV16bra"))

## MODELO PUERTO RICO  HPV 16/18 ##

dat_PRI_HPVHr<- data.frame (filter( dat, survey == "PRI-001-02"))

# crear columna birth_year para todos los serotipos - preparacion de datos SEROFOI

dat_PRI_HPVHr <- dat_PRI_HPVHr %>%  mutate ( birth_year = tsur - age_mean_f)


#FOI constate  HPV 16 PUERTO RICO 

HPV_constant_PRI_HPVHr <- fit_seromodel(serodata = dat_PRI_HPVHr,
                                        foi_model = "constant",
                                        iter = 1000)

HPV_constant_plot_PRI_HPVHr <- plot_seromodel(HPV_constant_PRI_HPVHr, 
                                              serodata = dat_PRI_HPVHr, 
                                              size_text = 12, max_lambda = max_lambda)

## FOI DEPENDINTE DEL TIMEPO VPH 16-18 PUERTO RICO##
HPV_time_PRI_HPVHr<- fit_seromodel(serodata = dat_PRI_HPVHr,
                                     foi_model = "tv_normal",
                                    foi_parameters = list(foi_location = 0, foi_scale = 1),
                                    iter = 1500)


HPV_time_plot_PRI_HPVHr <- plot_seromodel(HPV_time_PRI_HPVHr, 
                                         serodata = dat_PRI_HPVHr, 
                                         size_text = 12, max_lambda = max_lambda)



# FOI dependiente de edad HPV 16-18 PUERTO RICO ##

HPV_age_PRI_HPVHr <- fit_seromodel(serodata = dat_PRI_HPVHr,
                                  foi_model = "av_normal",
                                  foi_parameters = list(foi_location = 0, foi_scale = 1),
                                  iter = 1500)

HPV_age_plot_PRI_HPVHr <- plot_seromodel(HPV_age_PRI_HPVHr , 
                                        serodata = dat_PRI_HPVHr, 
                                        size_text = 12,max_lambda = max_lambda)


cowplot::plot_grid(HPV_constant_plot_PRI_HPVHr, HPV_time_plot_PRI_HPVHr,HPV_age_plot_PRI_HPVHr, ncol = 3)


## MODELO ESTADOS UNIDOS (1992) HPV 16##

dat_USA92_HPV16<- data.frame (filter( dat, survey == "USA-011-03"))

# crear columna birth_year para todos los serotipos - preparacion de datos SEROFOI

dat_USA92_HPV16 <- dat_USA92_HPV16 %>%  mutate ( birth_year = tsur - age_mean_f)


#FOI constate  HPV 16 ESTADOS UNIDOS (1992)
HPV_constant_USA92_HPV16 <- fit_seromodel(serodata = dat_USA92_HPV16,
                                          foi_model = "constant",
                                          iter = 1500)

HPV_constant_plot_USA92_HPV16 <- plot_seromodel(HPV_constant_USA92_HPV16, 
                                                serodata = dat_USA92_HPV16, 
                                                size_text = 12,max_lambda = max_lambda )

## FOI DEPENDIeNTE DEL TIEMPO ESTADOS UNIDOS (1992) HPV 16 ##
HPV_time_USA92_HPV16<- fit_seromodel(serodata = dat_USA92_HPV16,
                                       foi_model = "tv_normal",
                                       foi_parameters = list( foi_location= 0, foi_scale = 1),
                                       iter = 1500)

HPV_time_plot_USA92_HPV16 <- plot_seromodel(HPV_time_USA92_HPV16, 
                                              serodata = dat_USA92_HPV16, 
                                              size_text = 12, max_lambda = max_lambda)

# FOI dependiente de edad HPV 16 ESTADOS UNIDOS (1992)  ##

HPV_age_USA92_HPV16<- fit_seromodel(serodata = dat_USA92_HPV16,
                                   foi_model = "av_normal",
                                   foi_parameters = list(foi_location = 0, foi_scale = 1),
                                   iter = 1500)

HPV_age_plot_USA92_HPV16 <- plot_seromodel(HPV_age_USA92_HPV16, 
                                         serodata = dat_USA92_HPV16, 
                                         size_text = 12,max_lambda = max_lambda)



cowplot::plot_grid(HPV_constant_plot_USA92_HPV16, HPV_time_plot_USA92_HPV16,HPV_age_plot_USA92_HPV16, ncol = 3)


## MODELO ESTADOS UNIDOS (2003) HPV 18##

dat_USA_HPV18<- data.frame (filter( dat, survey == "USA-026-04"))

# crear columna birth_year para todos los serotipos - preparacion de datos SEROFOI

dat_USA_HPV18 <- dat_USA_HPV18 %>%  mutate ( birth_year = tsur - age_mean_f)


#FOI constate  HPV 18 ESTADOS UNIDOS (2003) ##
HPV_constant_USA_HPV18 <- fit_seromodel(serodata = dat_USA_HPV18,
                                        foi_model = "constant",
                                        iter = 1000)

HPV_constant_plot_USA_HPV18 <- plot_seromodel(HPV_constant_USA_HPV18, 
                                              serodata = dat_USA_HPV18, 
                                              size_text = 12,max_lambda = max_lambda )
## FOI DEPENDINET DE TIEMPO ESTADOS UNIDOS (2003) ##
HPV_time_USA_HPV18<- fit_seromodel(serodata = dat_USA_HPV18,
                                        foi_model = "tv_normal",
                                        foi_parameters = list( foi_location = 0, 
                                                               foi_scale = 1),
                                        iter = 2000 )

HPV_time_plot_USA_HPV18 <- plot_seromodel(HPV_time_USA_HPV18, 
                                               serodata = dat_USA_HPV18, 
                                               size_text = 12, max_lambda = max_lambda )

## dependiente de la edad  ESTADOS UNIDOS (2003) vph 18 ##

HPV_age_USA_HPV18<- fit_seromodel(serodata = dat_USA_HPV18,
                                        foi_model = "av_normal",
                                        foi_parameters = list( foi_location = 0, 
                                                               foi_scale = 1),
                                        iter = 2000 )

HPV_age_plot_USA_HPV18 <- plot_seromodel(HPV_age_USA_HPV18, 
                                               serodata = dat_USA_HPV18, 
                                               size_text = 12, max_lambda = max_lambda )




cowplot::plot_grid(HPV_constant_plot_USA_HPV18, HPV_time_plot_USA_HPV18,HPV_age_plot_USA_HPV18, ncol = 3)


## MODELO ESTADOS UNIDOS (2003) HPV 16##

dat_USA_HPV16<- data.frame (filter( dat, survey == "USA-026-03"))

# crear columna birth_year para todos los serotipos - preparacion de datos SEROFOI

dat_USA_HPV16 <- dat_USA_HPV16 %>%  mutate ( birth_year = tsur - age_mean_f)


#FOI constate  HPV 16 Estado Unidos 2003##
HPV_constant_USA_HPV16 <- fit_seromodel(serodata = dat_USA_HPV16,
                                        foi_model = "constant",
                                        iter = 1000)

HPV_constant_plot_USA_HPV16 <- plot_seromodel(HPV_constant_USA_HPV16, 
                                              serodata = dat_USA_HPV16, 
                                              size_text = 12, max_lambda = max_lambda )

## FOI DEPENDINTE DE TIEMPO ESTADOS UNIDOS (2003) HPV 16 ##

HPV_time_USA_HPV16<- fit_seromodel(serodata = dat_USA_HPV16,
                                     foi_model = "tv_normal",
                                     foi_parameters = list( foi_location = 0,
                                                            foi_scale = 1),
                                     iter = 1500)

HPV_time_plot_USA_HPV16 <- plot_seromodel(HPV_time_USA_HPV16, 
                                            serodata = dat_USA_HPV16, 
                                            size_text = 12, max_lambda = max_lambda )



## dependiente de la edad  ESTADOS UNIDOS (2003) vph 18 ##

HPV_age_USA_HPV16<- fit_seromodel(serodata = dat_USA_HPV16,
                                  foi_model = "av_normal",
                                  foi_parameters = list( foi_location = 0, 
                                                         foi_scale = 1),
                                  iter = 2000 )

HPV_age_plot_USA_HPV16 <- plot_seromodel(HPV_age_USA_HPV16, 
                                         serodata = dat_USA_HPV16, 
                                         size_text = 12, max_lambda = max_lambda )




cowplot::plot_grid(HPV_constant_plot_USA_HPV16, HPV_time_plot_USA_HPV16,HPV_age_plot_USA_HPV16, ncol = 3)


## MODELO Taiwan  (1992) HPV 18##

dat_TWN_HPV18<- data.frame (filter( dat, survey == "TWN-027-02"))

# crear columna birth_year para todos los serotipos - preparacion de datos SEROFOI

dat_TWN_HPV18 <- dat_TWN_HPV18 %>%  mutate ( birth_year = tsur - age_mean_f)


#FOI constate  HPV 18
HPV_constant_TWN_HPV18 <- fit_seromodel(serodata = dat_TWN_HPV18,
                                        foi_model = "constant",
                                       iter = 1000)

HPV_constant_plot_TWN_HPV18 <- plot_seromodel(HPV_constant_TWN_HPV18, 
                                              serodata = dat_TWN_HPV18, 
                                              size_text = 12, max_lambda = max_lambda)
## FOI dependiente de tiempo HPV 18 Taiwan##

HPV_time_TWN_HPV18<- fit_seromodel(serodata = dat_TWN_HPV18,
                                     foi_model = "tv_normal",
                                    foi_parameters = list(foi_location = 0, foi_scale= 1),
                                     iter = 1500)

HPV_time_plot_TWN_HPV18 <- plot_seromodel(HPV_time_TWN_HPV18, 
                                            serodata = dat_TWN_HPV18, 
                                            size_text = 12, max_lambda = max_lambda)

## dependiente de la edad  TAIWAN vph 18 ##

HPV_age_TWN_HPV18<- fit_seromodel(serodata = dat_TWN_HPV18,
                                  foi_model = "av_normal",
                                  foi_parameters = list( foi_location = 0, 
                                                         foi_scale = 1),
                                  iter = 1500 )

HPV_age_plot_TWN_HPV18 <- plot_seromodel(HPV_age_TWN_HPV18, 
                                         serodata = dat_TWN_HPV18, 
                                         size_text = 12, max_lambda = max_lambda )



cowplot::plot_grid(HPV_constant_plot_TWN_HPV18, HPV_time_plot_TWN_HPV18, HPV_age_plot_TWN_HPV18, ncol = 3)



## MODELO NIGERIA HPV 16 ##

dat_NGA_HPV16<- data.frame (filter( dat, survey == "NGA-002-01"))

# crear columna birth_year para todos los serotipos - preparacion de datos SEROFOI ##

dat_NGA_HPV16 <- dat_NGA_HPV16 %>%  mutate ( birth_year = tsur - age_mean_f)


#FOI constate  HPV 16
HPV_constant_HPV16NGA <- fit_seromodel(serodata = dat_NGA_HPV16,
                                       foi_model = "constant",
                                      iter = 1000)

HPV_constant_plot_HPV16NGA <- plot_seromodel(HPV_constant_HPV16NGA, 
                                             serodata = dat_NGA_HPV16, 
                                             size_text = 12, max_lambda = max_lambda)
## Modelos dependiente del tiempo##
HPV_time_HPV16NGA <- fit_seromodel(serodata = dat_NGA_HPV16,
                                     foi_model = "tv_normal",
                                   foi_parameters = list( foi_location = 0, 
                                                          foi_scale = 1),
                                     iter = 1500)

HPV_time_plot_HPV16NGA <- plot_seromodel(HPV_time_HPV16NGA, 
                                           serodata = dat_NGA_HPV16, 
                                           size_text = 12, max_lambda = max_lambda)


# dependiente de la edad  TAIWAN vph 18 ##

HPV_age_HPV16NGA <- fit_seromodel(serodata = dat_NGA_HPV16,
                                  foi_model = "av_normal",
                                  foi_parameters = list( foi_location = 0, 
                                                         foi_scale = 1),
                                  iter = 1500 )

HPV_age_plot_HPV16NGA <- plot_seromodel(HPV_age_HPV16NGA, 
                                         serodata = dat_NGA_HPV16, 
                                         size_text = 12, max_lambda = max_lambda )



cowplot::plot_grid(HPV_constant_plot_HPV16NGA, HPV_time_plot_HPV16NGA,HPV_age_plot_HPV16NGA, ncol = 3)

  


