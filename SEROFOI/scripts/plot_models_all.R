library(serofoi)

source("SEROFOI/scripts/plot_foi_estimates_constant_model.R")

fPlotAllModels <- function(res_models, foi_max, foi_max_m4, size_text)
  
{
  survey <- unique(res_models$dat0$survey)
  
  S1 <- 
    plot_seroprevalence_estimates(
      seromodel = res_models$model1,
      serosurvey = res_models$dat0,
      size_text = size_text) +
      ggplot2::coord_cartesian(
      xlim = c(0, 60)
    ) +
    ggtitle(paste0(survey, "\n M1-Constante sin serorevesión"))
  
   F1 <- 
     plot_foi_estimates_constant_model(
     seromodel = res_models$model1,
     serosurvey = res_models$dat0,
     size_text = size_text,
     foi_max = foi_max,
     model_type = "time")
  
  
  S2 <- 
    plot_seroprevalence_estimates(
      seromodel = res_models$model2,
      serosurvey = res_models$dat0,
      size_text = size_text) +
    ggplot2::coord_cartesian(
      xlim = c(0, 60)
    ) +
    ggtitle(paste0(survey, "\nM2-Tiempo sin seroreversión"))
  
  
  F2 <- plot_foi_estimates(
    seromodel = res_models$model2,
    serosurvey = res_models$dat0,
    size_text = size_text,
    foi_max = foi_max)
  
  
  S3 <- 
    plot_seroprevalence_estimates(
      seromodel = res_models$model3,
      serosurvey = res_models$dat0,
      size_text = size_text) +
    ggplot2::coord_cartesian(
      xlim = c(0, 60)
    ) +
    ggtitle(paste0(survey, "\nM3-Edad sin seroreversión"))
  
  
  F3 <- plot_foi_estimates(
    seromodel = res_models$model3,
    serosurvey = res_models$dat0,
    size_text = size_text,
    foi_max = foi_max)  + ggplot2::coord_cartesian(
      xlim = c(0, 60)
    ) 
  
  S4 <- 
    plot_seroprevalence_estimates(
      seromodel = res_models$model4,
      serosurvey = res_models$dat0,
      size_text = size_text) +
    ggplot2::coord_cartesian(
      xlim = c(0, 60)
    ) +
    ggtitle(paste0(survey, "\nM4-Edad con seroreversión"))
  
  
  F4 <- plot_foi_estimates(
    seromodel = res_models$model4,
    serosurvey = res_models$dat0,
    size_text = size_text,
    foi_max = foi_max_m4) + ggplot2::coord_cartesian(
      xlim = c(0, 60)
    ) 
  
  #empty_plot <- ggplot() + theme_void()

  Ss <- cowplot::plot_grid(S1, S2, S3, S4, nrow = 1, align = "hv") 
  Fs <- cowplot::plot_grid(F1, F2, F3, F4, nrow = 1, align = "hv") 
  
  final_plot <- cowplot::plot_grid(Ss, Fs, nrow=2, align = "hv", rel_heights = c(1,1, 0.9))
  return(final_plot)
}


COL_HPVHr <- readRDS ("SEROFOI/results_RDS/COL-024-02_.RDS")
plot_COL_HPVHr <- fPlotAllModels(res_models = COL_HPV18, 
                                 foi_max = 0.6, foi_max_m4 = 0.6, size_text = 8)
jpeg(filename = "SEROFOI/plots/COL_HPVHr.jpeg",width = 11, height = 8, units = "in", res = 300)
plot_COL_HPVHr 
dev.off()


COL_HPV16 <- readRDS ("SEROFOI/results_RDS/COL-024-03_.RDS")
plot_COL_HPV16 <- fPlotAllModels(res_models = COL_HPV16, 
                                 foi_max = 0.4, foi_max_m4 = 0.4, size_text = 8)
jpeg(filename = "SEROFOI/plots/COL_HPV16.jpeg",width = 11, height = 8, units = "in", res = 300)
plot_COL_HPV16
dev.off()


COL_HPV18 <- readRDS ("SEROFOI/results_RDS/COL-024-04_.RDS")
plot_COL_HPV18 <- fPlotAllModels(res_models = COL_HPV18, 
                                 foi_max = 0.3, foi_max_m4 = 0.3, size_text = 8)
jpeg(filename = "SEROFOI/plots/COL_HPV18.jpeg",width = 11, height = 8, units = "in", res = 300)
plot_COL_HPV18
dev.off()

CRI_HPV16 <- readRDS ("SEROFOI/results_RDS/CRI-009-01_.RDS")
plot_CRI_HPV16 <- fPlotAllModels(res_models = CRI_HPV16, 
                                 foi_max = 0.1, foi_max_m4 = 1, size_text = 8)
jpeg(filename = "SEROFOI/plots/CRI_HPV16.jpeg",width = 11, height = 8, units = "in", res = 300)
plot_CRI_HPV16
dev.off()


CRI_HPV18 <- readRDS ("SEROFOI/results_RDS/CRI-009-02_.RDS")
plot_CRIL_HPV18 <- fPlotAllModels(res_models = COL_HPV18, 
                                 fox_max = 0.1, foi_max_m4 = 1, size_text = 8)
jpeg(filename = "SEROFOI/plots/CRI_HPV18.jpeg",width = 11, height = 8, units = "in", res = 300)

