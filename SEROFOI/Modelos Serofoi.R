# SEROFOI ##
library(serofoi)
library(cowplot)
library(lubridate)
library(loo)
library(openxlsx)
library(ggplot2)
library(dplyr)
library(patchwork)


dat <- readRDS('data/data_for_models/clean_data_total_models.RDS') 

dat_col <- data.frame (filter( dat, country == "COL"))


# crear columna birth_year para todos los serotipos - preparacion de datos SEROFOI

dat_colp <- dat_col %>%  mutate ( birth_year = tsur - age_mean_f)

dat_col_HPV18 <- data.frame (filter (dat_colp, pathogen == "HPV 18"))



#FOI constate  HPV 18
HPV_constant_HPV18 <- run_seromodel(serodata = dat_col_HPV18,
                               foi_model = "constant",
                               n_iters = 1000)

HPV_constant_plot_HPV18 <- plot_seromodel(HPV_constant_HPV18, 
                                     serodata = dat_col_HPV18, 
                                     size_text = 12)
## normal##
HPV_normal_HPV18 <- run_seromodel(serodata = dat_col_HPV18,
                             foi_model = "tv_normal",
                             n_iters = 1500)

HPV_normal_plot_HPV18 <- plot_seromodel(HPV_normal_HPV18, 
                                   serodata = dat_col_HPV18, 
                                   size_text = 10)
## normal log##

HPV_normal_log_HPV18 <- run_seromodel(serodata = dat_col_HPV18,
                                 foi_model = "tv_normal_log",
                                 n_iters = 1500)

HPV_normal_log_plot_HPV18 <- plot_seromodel(HPV_normal_log_HPV18, 
                                       serodata = dat_col_HPV18, 
                                       size_text = 10)

cowplot::plot_grid(HPV_constant_plot_HPV18, HPV_normal_plot_HPV18 , HPV_normal_log_plot_HPV18,ncol = 3)


## AJUSTE DE LOS MODELOS ### 

HPV_constant <- run_seromodel(serodata = dat_col_HPV18, foi_model = "constant", n_iters = 1000)
HPV_normal <- run_seromodel(serodata = dat_col_HPV18, foi_model = "tv_normal", n_iters = 1500)
HPV_normal_log <- run_seromodel(serodata = dat_col_HPV18, foi_model = "tv_normal_log", n_iters = 1500)

fit1 <- list(constant = HPV_constant_HPV18, normal = HPV_normal_HPV18, normal_log_HPV18 = HPV_normal_log)
waic1 <- mapply(function(z) loo::waic(loo::extract_log_lik(z, parameter_name = "logLikelihood"))$estimates[3, ], fit1)
waic1 <- as.data.frame(waic1)


openxlsx::write.xlsx(waic1, "WAIC_V1.xlsx")


loo_fit_constant <- loo::loo(fit1$constant, save_psis = FALSE, pars = c(parameter_name = "logLikelihood"))
loo_fit_constant <- loo_fit_constant$estimates[3, , drop=FALSE]

loo_fit_normal <- loo::loo(fit1$normal, save_psis = FALSE, pars = c(parameter_name = "logLikelihood"))
loo_fit_normal <- loo_fit_normal$estimates[3, , drop=FALSE]

loo_fit_normal_log <- loo::loo(fit1$normal_log, save_psis = FALSE, pars = c(parameter_name = "logLikelihood"))
loo_fit_normal_log <- loo_fit_normal_log$estimates[3, , drop=FALSE]

loo <- rbind(loo_fit_constant, 
             loo_fit_normal,
             loo_fit_normal_log 
)

loo1 <- data.frame(loo)


openxlsx::write.xlsx(loo1, "LOO_HPV18.xlsx")


# HPV 16 COL

dat_col_HPV16 <- data.frame (filter (dat_colp, pathogen == "HPV 16"))


#FOI constate  HPV 16

constant_HPV16 <- run_seromodel(serodata = dat_col_HPV16,
                              foi_model = "constant",
                              n_iters = 1000)

constant_plot_HPV16 <- plot_seromodel(constant_HPV16, 
                                    serodata = dat_col_HPV16, 
                                    size_text = 12)
## normal##
normal_HPV16 <- run_seromodel(serodata = dat_col_HPV16,
                            foi_model = "tv_normal",
                            n_iters = 1500)

normal_plot_HPV16<- plot_seromodel(normal_HPV16, 
                                  serodata = dat_col_HPV16, 
                                  size_text = 10)
## normal log##

normal_log_HPV16 <- run_seromodel(serodata = dat_col_HPV16,
                                foi_model = "tv_normal_log",
                                n_iters = 1500)

normal_log_plot_HPV16 <- plot_seromodel(normal_log_HPV16, 
                                      serodata = dat_col_HPV16, 
                                      size_text = 10)

cowplot::plot_grid(constant_plot_HPV16, normal_plot_HPV16 , normal_log_plot_HPV16,ncol = 3) 


## AJUSTE DE LOS MODELOS VPH16### 

constant_HPV16 <- run_seromodel(serodata = dat_col_HPV16, foi_model = "constant", n_iters = 1000)
normal_HPV16 <- run_seromodel(serodata = dat_col_HPV16, foi_model = "tv_normal", n_iters = 1500)
normal_log_HPV16 <- run_seromodel(serodata = dat_col_HPV16, foi_model = "tv_normal_log", n_iters = 1500)

fit2 <- list(constant = HPV_constant, normal = HPV_normal, normal_log = HPV_normal_log)
waic2 <- mapply(function(z) loo::waic(loo::extract_log_lik(z, parameter_name = "logLikelihood"))$estimates[3, ], fit2)
waic2 <- as.data.frame(waic2)


openxlsx::write.xlsx(waic1, "WAIC_VPH16.xlsx")

# crear columna birth_year para HPV 16-18 para datos preparados

dat_col_HPVhr <- data.frame (filter (dat_colp, pathogen == "HPV 16/18"))


#FOI constate  HPV 16-18

constant_HPVhr <- run_seromodel(serodata = dat_col_HPVhr,
                                foi_model = "constant",
                                n_iters = 1000)

constant_plot_HPVhr <- plot_seromodel(constant_HPVhr, 
                                      serodata = dat_col_HPVhr, 
                                      size_text = 12)
## normal##
normal_HPVhr <- run_seromodel(serodata = dat_col_HPVhr,
                              foi_model = "tv_normal",
                              n_iters = 1500)

normal_plot_HPVhr<- plot_seromodel(normal_HPV16, 
                                   serodata = dat_col_HPVhr, 
                                   size_text = 10)
## normal log##

normal_log_HPVhr <- run_seromodel(serodata = dat_col_HPVhr,
                                  foi_model = "tv_normal_log",
                                  n_iters = 1500)

normal_log_plot_HPVhr <- plot_seromodel(normal_log_HPVhr, 
                                        serodata = dat_col_HPVhr, 
                                        size_text = 10)

cowplot::plot_grid(constant_plot_HPVhr, normal_plot_HPVhr , normal_log_plot_HPVhr,ncol = 3)




## AJUSTE DE LOS MODELOS VPH16/18### 

constant_HPV16 <- run_seromodel(serodata = dat_col_HPVhr, foi_model = "constant", n_iters = 1000)
normal_HPV16 <- run_seromodel(serodata = dat_col_HPVhr, foi_model = "tv_normal", n_iters = 1500)
normal_log_HPV16 <- run_seromodel(serodata = dat_col_HPVhr, foi_model = "tv_normal_log", n_iters = 1500)

fit2 <- list(constant = HPV_constant, normal = HPV_normal, normal_log = HPV_normal_log)
waic2 <- mapply(function(z) loo::waic(loo::extract_log_lik(z, parameter_name = "logLikelihood"))$estimates[3, ], fit2)
waic2 <- as.data.frame(waic2)


openxlsx::write.xlsx(waic1, "WAIC_VPH16.xlsx")



# FOI para Costa Rica ##

dat_cri <- data.frame (filter( dat, country == "CRI"))



# crear columna birth_year para todos los serotipos - preparacion de datos SEROFOI

dat_crip <- dat_cri %>%  mutate ( birth_year = tsur - age_mean_f)

dat_cri_HPV18 <- data.frame (filter (dat_crip, pathogen == "HPV 18"))


#FOI constate  HPV 18
HPV_constant_HPV18 <- run_seromodel(serodata = dat_cri_HPV18,
                                    foi_model = "constant",
                                    n_iters = 1000)

HPV_constant_plot_HPV18 <- plot_seromodel(HPV_constant_HPV18, 
                                          serodata = dat_cri_HPV18, 
                                          size_text = 12)
## normal##
HPV_normal_HPV18 <- run_seromodel(serodata = dat_cri_HPV18,
                                  foi_model = "tv_normal",
                                  n_iters = 1500)

HPV_normal_plot_HPV18 <- plot_seromodel(HPV_normal_HPV18, 
                                        serodata = dat_cri_HPV18, 
                                        size_text = 10)
## normal log##

HPV_normal_log_HPV18 <- run_seromodel(serodata = dat_cri_HPV18,
                                      foi_model = "tv_normal_log",
                                      n_iters = 1500)

HPV_normal_log_plot_HPV18 <- plot_seromodel(HPV_normal_log_HPV18, 
                                            serodata = dat_cri_HPV18, 
                                            size_text = 10)

cowplot::plot_grid(HPV_constant_plot_HPV18, HPV_normal_plot_HPV18 , HPV_normal_log_plot_HPV18,ncol = 3)








