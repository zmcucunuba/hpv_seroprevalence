mapas_desc <- readRDS('data/data_for_models/clean_data_total_models.RDS')

library(sf)
library(tidyverse)
library(mapview)

# cambiar variable longitud de caratcer a numerica
mapas_desc$long_dec  <- as.numeric(mapas_desc$long_dec)

# cret columan de gyometry que contenga latitud y longitud

data_geo <- mapas_desc %>%
  filter( !is.na(long_dec),!is.na(lat_dec))%>%
  st_as_sf(coords =  c("long_dec", "lat_dec"), crs=4326)

class(data_geo)

#Mapa general con latitudes y longitudes
# Aqui no aparece la totalidad de lugares de seroencueta porque algunas solo mencionaban lugar
#no coordenadas

mapview(data_geo)

# mapas con ggplot2


