library(readxl)

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


VPH16$FOI_max <- gsub(",", ".", VPH16$FOI_max)


VPH16$FOI_max <- as.numeric(VPH16$FOI_max) 



ggplot(VPH16, aes(x = País, y = FOI_max)) +
  geom_bar(stat = "identity", fill = "#007BFF", color = "#007BFF") +
  labs(x = "País", y = " Fuerza de Infección(%)", title = "Media de fuerza de infección VPH -16") +
  theme_minimal() 
 