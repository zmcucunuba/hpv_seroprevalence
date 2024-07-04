library(readxl)
library(ggplot2)

VPH16 <- read_excel("SEROFOI/Tabla resumen FOI/VPH16.xlsx")
 

ggplot(VPH16, aes(x = País, fill = FOI_max)) +
  geom_histogram(binwidth = 0.05, color = "black", alpha = 0.7) +
  labs(title = "Histograma de FOI máxima (media) por País",
       x = "FOI máxima (media)",
       y = "Frecuencia") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

str(VPH16)

VPH16$FOI_max <- gsub(",", ".", VPH16$FOI_max)


VPH16$FOI_max <- as.numeric(VPH16$FOI_max) 




ggplot(VPH16, aes(x = País, y = FOI_max)) +
  geom_bar(stat = "identity", fill = "#007BFF", color = "#007BFF") +
  labs(x = "País", y = " Fuerza de Infección(%)", title = "Media de fuerza de infección VPH -16") +
  theme_minimal() 

VPH18 <- read_excel("SEROFOI/Tabla resumen FOI/VPH18.xlsx")



VPH18$FOI_max <- gsub(",", ".", VPH18$FOI_max)

VPH18$FOI_max <- as.numeric(VPH18$FOI_max) 


str(VPH18)



ggplot(VPH18, aes(x = País, y = FOI_max)) + 
  geom_bar(stat = "identity", fill = "#007BFF", color = "#007BFF") +
  labs(x = "País", y = "Fuerza de Infección(%)", title = "Media de fuerza de infección VPH -18 por País") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 15, face = "bold"),  # Tamaño del título
    axis.title.x = element_text(size = 12),              # Tamaño de la etiqueta del eje X
    axis.title.y = element_text(size = 12),              # Tamaño de la etiqueta del eje Y
    axis.text.x = element_text(size = 12),               # Tamaño del texto de los valores del eje X
    axis.text.y = element_text(size = 12),               # Tamaño del texto de los valores del eje Y
    axis.ticks = element_line(color = "black", size = 0.5), # Color y tamaño de los ticks del eje
    panel.grid.major = element_line(color = "gray85", size = 0.5), # Color y tamaño de las líneas principales de la cuadrícula
    panel.grid.minor = element_line(color = "gray90", size = 0.25),  # Color y tamaño de las líneas secundarias de la cuadrícula
    axis.line.x = element_line(color = "black", size = 0.8), # Color y tamaño de las líneas del borde del eje X
    axis.line.y = element_line(color = "black", size = 0.8)  # Color y tamaño de las líneas del borde del eje Y
  )


ggplot(VPH18, aes(x = tiempo_meses_seror)) + 
  geom_histogram(binwidth = 0.5, fill = "#00FFFF", color = "black") +
  labs(title = "Histograma de la Duración de Anticuerpos en Meses VPH 18",
       x = "Duración en Meses",
       y = "Frecuencia") +
  theme_minimal() +
  theme(text = element_text(size = 10),  # Cambia el tamaño de todo el texto
        axis.title = element_text(size = 12),  # Cambia el tamaño del título de los ejes
        axis.text = element_text(size = 12),  # Cambia el tamaño del texto en los ejes
        plot.title = element_text(size = 12, face = "bold"))  # Cambia el tamaño y estilo del título del gráfico

