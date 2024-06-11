library(serofoi)

COL_HPV18 <- readRDS ("SEROFOI/results_RDS/COL-024-02_.RDS")

# MODELO 1


  plot_seroprevalence_estimates(
  seromodel = COL_HPV18$model1,
  serosurvey = COL_HPV18$dat0,
  size_text = 10
) +
  ggtitle("M1-Constante sin serorevesión")
 
 plot_foi_estimates(
  seromodel = COL_HPV18$model1,
  serosurvey = COL_HPV18$dat0,
  size_text = 10,
  foi_max = 0.04
)
 
 plot_seroprevalence_estimates(
   seromodel = COL_HPV18$model2,
   serosurvey = COL_HPV18$dat0,
   size_text = 10
 ) +
   ggtitle("M2-Tiempo sin seroreversión")
 
 
 plot_foi_estimates(
   seromodel = COL_HPV18$model2,
   serosurvey = COL_HPV18$dat0,
   size_text = 10,
   foi_max = 0.04
 )

 cowplot::plot_grid()
