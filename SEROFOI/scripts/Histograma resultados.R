library(readxl)
library(ggplot2)
library(dplyr)
library(readxl)



VPH16 <- read_excel("SEROFOI/Tabla resumen FOI/VPH16.xlsx", 
                    +     col_types = c("text", "numeric", "numeric", 
                                        +         "text", "numeric"))
View(VPH16)


VPH18 <- read_excel("SEROFOI/Tabla resumen FOI/VPH18.xlsx", 
                    col_types = c("text", "numeric", "numeric", 
                                  "numeric", "numeric"))
View(VPH18)



FOI_MAX_VPH16 <- ggplot(VPH16) +
  geom_histogram(aes(x = `FOI máxima`), fill = "#1f77b4", color = "black") +
  labs(y = "Frecuencia",title = "VPH-16" ) +
  ylim(0, 2)


ANTI_VPH16 <- ggplot(VPH16) +
  geom_histogram(aes(x = (1/Seroreversión)*12), fill = "#1f77b4", color = "black" ) +
  labs(y = "Frecuencia", x = "Duración promedio inmunidad (meses)", title = "VPH-16") +
  ylim(0, 2)



FOI_MAX_VPH18 <-  ggplot(VPH18) +
  geom_histogram(aes(x = `FOI máxima`), fill = "#FF5733", color = "black") +
  labs(y = "Frecuencia", title = "VPH-18") +
  ylim(0, 2)

ANTI_VPH18 <- ggplot(VPH18) +
  geom_histogram(aes(x = (1/Seroreversión)*12), fill = "#FF5733", color = "black" ) +
  labs(y = "Frecuencia", x = "Duración promedio inmunidad (meses)", title = "VPH-18") +
  ylim(0, 2)

# HISTOGRAMA DE DURACIÓN DE ANTICUERCUERPOS##

Histo_all <- cowplot::plot_grid(
  FOI_MAX_VPH16, FOI_MAX_VPH18, 
  ANTI_VPH16,   ANTI_VPH18, 
  nrow = 2, ncol = 2
)

jpeg(filename = "SEROFOI/Tabla resumen FOI/Histo_all.jpeg",width = 11, height = 8, units = "in", res = 300)
Histo_all 
dev.off()

## MEDIA DE FUERZA DE INFECCION##


VPH16$Genotipo <- "VPH16"
VPH18$Genotipo <- "VPH18"
VPH_combined <- bind_rows(
  VPH16 %>% select(País, FOI = `FOI máxima`, Genotipo),
  VPH18 %>% select(País, FOI = `FOI máxima`, Genotipo)
)


FOI_combined_plot <- ggplot(VPH_combined, aes(x = País, y = FOI, fill = Genotipo)) + 
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  labs(x = "País", y = "Fuerza de Infección (per cápita)", title = "Media de fuerza de infección VPH -16 y VPH -18 por País") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 12, face = "bold"),  # Tamaño del título
    axis.title.x = element_text(size = 10),              # Tamaño de la etiqueta del eje X
    axis.title.y = element_text(size = 10),              # Tamaño de la etiqueta del eje Y
    axis.text.x = element_text(size = 10),               # Tamaño del texto de los valores del eje X
    axis.text.y = element_text(size = 10),               # Tamaño del texto de los valores del eje Y
    axis.ticks = element_line(color = "black", size = 0.5), # Color y tamaño de los ticks del eje
    panel.grid.major = element_line(color = "gray85", size = 0.5), # Color y tamaño de las líneas principales de la cuadrícula
    panel.grid.minor = element_line(color = "gray90", size = 0.25),  # Color y tamaño de las líneas secundarias de la cuadrícula
    axis.line.x = element_line(color = "black", size = 0.8), # Color y tamaño de las líneas del borde del eje X
    axis.line.y = element_line(color = "black", size = 0.8)  # Color y tamaño de las líneas del borde del eje Y
  ) +
  ylim(0, 1.5) +
  scale_fill_manual(values = c("VPH16" = "#007BFF", "VPH18" = "#FF5733"))

jpeg(filename = "SEROFOI/Tabla resumen FOI/FOI_combined_plot.jpeg",width = 11, height = 8, units = "in", res = 300)
FOI_combined_plot
dev.off()


   


