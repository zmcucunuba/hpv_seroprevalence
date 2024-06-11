library(serofoi)


fPlotAllModels <- function(res_models, fox_max, foi_max_m4, size_text)
  
{
  survey <- unique(res_models$dat0$survey)
  
  S1 <- 
    plot_seroprevalence_estimates(
      seromodel = res_models$model1,
      serosurvey = res_models$dat0,
      size_text = size_text) +
    ggtitle(paste0(survey, "/n M1-Constante sin serorevesión"))
  
  # No existe por ahora
  # F1 <- 
  #   plot_foi_estimates(
  #   seromodel = res_models$model1,
  #   serosurvey = res_models$dat0,
  #   size_text = size_text,
  #   foi_max = fox_max)
  
  
  S2 <- 
    plot_seroprevalence_estimates(
      seromodel = res_models$model2,
      serosurvey = res_models$dat0,
      size_text = size_text) +
    ggtitle(paste0(survey, "/n M2-Tiempo sin seroreversión"))
  
  
  F2 <- plot_foi_estimates(
    seromodel = res_models$model2,
    serosurvey = res_models$dat0,
    size_text = size_text,
    foi_max = foi_max)
  
  
  S3 <- 
    plot_seroprevalence_estimates(
      seromodel = res_models$model3,
      serosurvey = res_models$res_models,
      size_text = size_text) +
    ggtitle(paste0(survey, "/n M3-Edad sin seroreversión"))
  
  
  F3 <- plot_foi_estimates(
    seromodel = res_models$model3,
    serosurvey = res_models$dat0,
    size_text = size_text,
    foi_max = foi_max)
  
  S4 <- 
    plot_seroprevalence_estimates(
      seromodel = res_models$model3,
      serosurvey = res_models$dat0,
      size_text = size_text) +
    ggtitle(paste0(survey, "/n M4-Edad con seroreversión"))
  
  
  F4 <- plot_foi_estimates(
    seromodel = res_models$model3,
    serosurvey = res_models$dat0,
    size_text = size_text,
    foi_max = foi_max_m4)
  
  empty_plot <- ggplot() + theme_void()
  
  Ss <- cowplot::plot_grid(S1, S2, S3, S4, nrow = 1, align = "hv") 
  Fs <- cowplot::plot_grid(empty_plot, F2, F3, F4, nrow = 1, align = "hv") 
  
  final_plot <- cowplot::plot_grid(Ss, Fs, nrow=2, align = "hv", rel_heights = c(1,1, 0.9))
  return(final_plot)
}


COL_HPV18 <- readRDS ("SEROFOI/results_RDS/COL-024-02_.RDS")
plot_COL_HPV18 <- fPlotAllModels(res_models = COL_HPV18, 
                                 fox_max = 0.1, foi_max_m4 = 1, size_text = 8)
jpeg(filename = "SEROFOI/plots/COL_HPV18.jpeg",width = 480*3, height = 480)


COL_HPV18 <- readRDS ("SEROFOI/results_RDS/COL-024-02_.RDS")
plot_COL_HPV18 <- fPlotAllModels(res_models = COL_HPV18, 
                                 fox_max = 0.1, foi_max_m4 = 1, size_text = 8)
jpeg(filename = "SEROFOI/plots/COL_HPV18.jpeg",width = 480*3, height = 480)

