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
dat_col_HPV18 <- data.frame (filter (dat_col, pathogen == "HPV 18"))



# crear columna birth_year para todos los serotipos

dat_colp <- dat_col %>%  mutate ( birth_year = tsur - age_mean_f)

# crear columna birth_year para  hpv18 para datos preparados

dat_col_HPV18p <- dat_col_HPV18 %>% mutate ( birth_year = tsur - age_mean_f)

#FOI constate  HPV 18
HPV_constant <- run_seromodel(serodata = dat_col_HPV18p,
                               foi_model = "constant",
                               n_iters = 1000)

HPV_constant_plot <- plot_seromodel(HPV_constant, 
                                     serodata = dat_col_HPV18p, 
                                     size_text = 12)
## normal##
HPV_normal <- run_seromodel(serodata = dat_col_HPV18p,
                             foi_model = "tv_normal",
                             n_iters = 1500)

HPV_normal_plot <- plot_seromodel(HPV_normal, 
                                   serodata = dat_col_HPV18p, 
                                   size_text = 10)
## normal log##

HPV_normal_log <- run_seromodel(serodata = dat_col_HPV18p,
                                 foi_model = "tv_normal_log",
                                 n_iters = 1500)

HPV_normal_log_plot <- plot_seromodel(HPV_normal_log, 
                                       serodata = dat_col_HPV18p, 
                                       size_text = 10)

cowplot::plot_grid(HPV_constant_plot, HPV_normal_plot , HPV_normal_log_plot,ncol = 3)

## AJUSTE DE LOS MODELOS ### 

HPV_constant <- run_seromodel(serodata = dat_col_HPV18p, foi_model = "constant", n_iters = 1000)
HPV_normal <- run_seromodel(serodata = dat_col_HPV18p, foi_model = "tv_normal", n_iters = 1500)
HPV_normal_log <- run_seromodel(serodata = dat_col_HPV18p, foi_model = "tv_normal_log", n_iters = 1500)

fit1 <- list(constant = HPV_constant, normal = HPV_normal, normal_log = HPV_normal_log)
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


openxlsx::write.xlsx(loo1, "LOO_V1.xlsx")


# HPV 18 COL

dat_col_HPV16 <- data.frame (filter (dat_col, pathogen == "HPV 16"))
dat_col_HPV16p <- dat_col_HPV16 %>% mutate ( birth_year = tsur - age_mean_f)

#FOI constate  HPV 18
HPV_constant16 <- run_seromodel(serodata = dat_col_HPV16p,
                              foi_model = "constant",
                              n_iters = 1000)

HPV_constant_plot16 <- plot_seromodel(HPV_constant16, 
                                    serodata = dat_col_HPV16p, 
                                    size_text = 12)
## normal##
HPV_normal16 <- run_seromodel(serodata = dat_col_HPV16p,
                            foi_model = "tv_normal",
                            n_iters = 1500)

HPV_normal_plot16 <- plot_seromodel(HPV_normal16, 
                                  serodata = dat_col_HPV16p, 
                                  size_text = 10)
## normal log##

HPV_normal_log16 <- run_seromodel(serodata = dat_col_HPV16p,
                                foi_model = "tv_normal_log",
                                n_iters = 1500)

HPV_normal_log_plot16 <- plot_seromodel(HPV_normal_log16, 
                                      serodata = dat_col_HPV18p, 
                                      size_text = 10)

cowplot::plot_grid(HPV_constant_plot16, HPV_normal_plot16 , HPV_normal_log_plot16,ncol = 3)


## AJUSTE DE LOS MODELOS VPH16### 

HPV_constant16 <- run_seromodel(serodata = dat_col_HPV16p, foi_model = "constant", n_iters = 1000)
HPV_normal16 <- run_seromodel(serodata = dat_col_HPV16p, foi_model = "tv_normal", n_iters = 1500)
HPV_normal_log16 <- run_seromodel(serodata = dat_col_HPV16p, foi_model = "tv_normal_log", n_iters = 1500)

fit2 <- list(constant = HPV_constant, normal = HPV_normal, normal_log = HPV_normal_log)
waic2 <- mapply(function(z) loo::waic(loo::extract_log_lik(z, parameter_name = "logLikelihood"))$estimates[3, ], fit2)
waic2 <- as.data.frame(waic2)


openxlsx::write.xlsx(waic1, "WAIC_VPH16.xlsx")



