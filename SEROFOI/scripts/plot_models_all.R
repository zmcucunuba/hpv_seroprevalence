library(serofoi)
library(ggplot2)
library(dplyr)

source("SEROFOI/scripts/plot_foi_estimates_constant_model.R")

fPlotAllModels <- function(res_models, foi_max, foi_max_m4, size_text)
  
{
  survey <- unique(res_models$dat0$survey)
  

  S1 <- plot_seroprevalence_estimates(
    seromodel = res_models$model1,
    serosurvey = res_models$dat0,
    size_text = size_text
  ) + 
    ggplot2::coord_cartesian(xlim = c(0, 60)) +
    ggplot2::xlab("Edad") +
    ggplot2::ylab("Seroprevalencia") +
    ggplot2::ggtitle(paste0(survey, "\nM1-Dependiente de tiempo
    (constante)"))
  
   
  
   F1 <- plot_foi_estimates_constant_model(
     seromodel = res_models$model1,
     serosurvey = res_models$dat0,
     size_text = size_text,
     foi_max = foi_max,
     model_type = "time"
   ) + ggplot2::ylab("Fuerza de infección") +
     ggplot2::xlab("Año")
   
  
   
 
  S2 <- 
    plot_seroprevalence_estimates(
      seromodel = res_models$model2,
      serosurvey = res_models$dat0,
      size_text = size_text) +
     ggplot2::coord_cartesian(
       xlim = c(0, 60)
     ) +
    ggplot2::xlab("Edad") +
    ggplot2::ylab("Seroprevalencia") +
    ggplot2::ggtitle(paste0(survey, "\nM2-Dependiente de tiempo 
    (variante)"))
  
  
  F2 <- plot_foi_estimates(
    seromodel = res_models$model2,
    serosurvey = res_models$dat0,
    size_text = size_text,
    foi_max = foi_max
    ) +
    ggplot2::ylab("Fuerza de infección") +
    ggplot2::xlab("Año")
    

  
  
  S3 <- 
    plot_seroprevalence_estimates(
      seromodel = res_models$model3,
      serosurvey = res_models$dat0,
      size_text = size_text)    +
    ggplot2::coord_cartesian(
      xlim = c(0, 60)
    ) +
    ggplot2::xlab("Edad") +
    ggplot2::ylab("Seroprevalencia") +
    ggplot2::ggtitle(paste0(survey, "\nM3-Dependiente de edad "))
  
  F3 <- plot_foi_estimates(
    seromodel = res_models$model3,
    serosurvey = res_models$dat0,
    size_text = size_text,
    foi_max = foi_max
  ) + ggplot2::scale_x_continuous(limits = c(0, 60)) +
    ggplot2::ylab("Fuerza de infección") +
    ggplot2::xlab("Edad")
  
  S4 <- 
    plot_seroprevalence_estimates(
      seromodel = res_models$model4,
      serosurvey = res_models$dat0,
      size_text = size_text) +
    ggplot2::coord_cartesian(
      xlim = c(0, 60)
    ) +
    ggplot2::xlab("Edad") +
    ggplot2::ylab("Seroprevalencia") +
    ggplot2::ggtitle(paste0(survey, "\nM4-Dependiente de edad 
    con serorreversión"))
  
  F4 <- plot_foi_estimates(
    seromodel = res_models$model4,
    serosurvey = res_models$dat0,
    size_text = size_text,
    foi_max = foi_max_m4
  ) + ggplot2::scale_x_continuous(limits = c(0, 60)) +
    ggplot2::ylab("Fuerza de infección") +
    ggplot2::xlab("Edad")
  
  #empty_plot <- ggplot() + theme_void()

  Ss <- cowplot::plot_grid(S1, S2, S3, S4, nrow = 1, align = "hv") 
  Fs <- cowplot::plot_grid(F1, F2, F3, F4, nrow = 1, align = "hv") 
  
  final_plot <- cowplot::plot_grid(Ss, Fs, nrow=2, align = "hv", rel_heights = c(1,1, 0.9))
  return(final_plot)
}

COL_HPV18 <- readRDS ("SEROFOI/results_RDS/COL-024-04_.RDS")
plot_COL_HPV18 <- fPlotAllModels(res_models = COL_HPV18, 
                                 foi_max = 0.020, foi_max_m4 = 0.4, size_text = 10)
jpeg(filename = "SEROFOI/plots/COL_HPV18.jpeg",width = 11, height = 8, units = "in", res = 300)
plot_COL_HPV18
dev.off()

COL_HPV16 <- readRDS ("SEROFOI/results_RDS/COL-024-03_.RDS")
plot_COL_HPV16 <- fPlotAllModels(res_models = COL_HPV16, 
                                 foi_max = 0.040, foi_max_m4 = 0.6, size_text = 10)
jpeg(filename = "SEROFOI/plots/COL_HPV16.jpeg",width = 11, height = 8, units = "in", res = 300)
plot_COL_HPV16
dev.off()


COL_HPVHr <- readRDS ("SEROFOI/results_RDS/COL-024-02_.RDS")
plot_COL_HPVHr <- fPlotAllModels(res_models = COL_HPVHr, 
                                 foi_max = 0.05, foi_max_m4 = 0.9, size_text = 10)
jpeg(filename = "SEROFOI/plots/COL_HPVHr.jpeg",width = 11, height = 8, units = "in", res = 300)
plot_COL_HPVHr 
dev.off()

CRI_HPV18 <- readRDS ("SEROFOI/results_RDS/CRI-009-02_.RDS")
plot_CRI_HPV18 <- fPlotAllModels(res_models = CRI_HPV18, 
                                 foi_max = 0.15 , foi_max_m4 = 1, size_text = 10)
jpeg(filename = "SEROFOI/plots/CRI_HPV18.jpeg",width = 11, height = 8, units = "in", res = 300)
plot_CRI_HPV18
dev.off()


CRI_HPV16 <- readRDS ("SEROFOI/results_RDS/CRI-009-01_.RDS")
plot_CRI_HPV16 <- fPlotAllModels(res_models = CRI_HPV16, 
                                 foi_max = 0.130, foi_max_m4 = 1.20, size_text = 10)
jpeg(filename = "SEROFOI/plots/CRI_HPV16.jpeg",width = 11, height = 8, units = "in", res = 300)
plot_CRI_HPV16
dev.off()


BRA_HPV16 <- readRDS ("SEROFOI/results_RDS/BRA-017-01_.RDS")
plot_BRA_HPV16 <- fPlotAllModels(res_models = BRA_HPV16, 
                                 foi_max = 0.075 , foi_max_m4 = 1.5 , size_text = 10)
jpeg(filename = "SEROFOI/plots/BRA_HPV16.jpeg",width = 11, height = 8, units = "in", res = 300)
plot_BRA_HPV16
dev.off()

PRI_HPVHr <- readRDS ("SEROFOI/results_RDS/PRI-001-02_.RDS")
plot_PRI_HPVHr <- fPlotAllModels(res_models = PRI_HPVHr, 
                                 foi_max = 0.050 , foi_max_m4 = 1.5 , size_text = 10)
jpeg(filename = "SEROFOI/plots/PRI_HPVHr.jpeg",width = 11, height = 8, units = "in", res = 300)
plot_PRI_HPVHr
dev.off()


USA92_HPV16<- readRDS ("SEROFOI/results_RDS/USA-011-03_.RDS")
plot_USA92_HPV16 <- fPlotAllModels(res_models = USA92_HPV16,  
                                   foi_max = 0.05 , foi_max_m4 = 0.6, size_text = 10)
jpeg(filename = "SEROFOI/plots/USA92_HPV16.jpeg",width = 11, height = 8, units = "in", res = 300)
plot_USA92_HPV16 
dev.off()


USA_HPV18<- readRDS ("SEROFOI/results_RDS/USA-026-04_.RDS")
plot_USA_HPV18 <- fPlotAllModels(res_models = USA_HPV18,  
                                 foi_max = 0.006, foi_max_m4 =0.2, size_text = 10)
jpeg(filename = "SEROFOI/plots/USA_HPV18.jpeg",width = 11, height = 8, units = "in", res = 300)
plot_USA_HPV18
dev.off()

USA_HPV16<- readRDS ("SEROFOI/results_RDS/USA-026-03_.RDS")
plot_USA_HPV16 <- fPlotAllModels(res_models = USA_HPV16,  
                                 foi_max = 0.03 , foi_max_m4 =0.5, size_text = 10)
jpeg(filename = "SEROFOI/plots/USA_HPV16.jpeg",width = 11, height = 8, units = "in", res = 300)
plot_USA_HPV16
dev.off()





TWN_HPV16<- readRDS ("SEROFOI/results_RDS/TWN-025-01_.RDS")
plot_TWN_HPV16 <- fPlotAllModels(res_models = TWN_HPV16,  
                                 foi_max = 0.02, foi_max_m4 =0.03, size_text = 10)
jpeg(filename = "SEROFOI/plots/TWN_HPV16.jpeg",width = 11, height = 8, units = "in", res = 300)
plot_TWN_HPV16
dev.off()


NGA_HPV16<- readRDS ("SEROFOI/results_RDS/NGA-002-01_.RDS")
plot_NGA_HPV16 <- fPlotAllModels(res_models = NGA_HPV16,  
                                 foi_max = 0.070, foi_max_m4 =3, size_text = 10)
jpeg(filename = "SEROFOI/plots/NGA_HPV16.jpeg",width = 11, height = 8, units = "in", res = 300)
plot_NGA_HPV16
dev.off()









