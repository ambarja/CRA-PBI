library(tidyverse)
library(sf)
library(rgee)
library(mapview)
ee_Initialize(user = "antony.barja@upch.pe")

pronostico <- st_read("resources/Tifon/datos_tifon2020.gpkg",layer = "pronostico")
limites <- st_read("resources/Tifon/datos_tifon2020.gpkg",layer = "admin1")

zona_impactada <- function(x){
  # Nivel de pronostico
  nivel <- pronostico[x,]
  # Intersección de capas
  nuevos_limites <- st_intersection(x = limites,y = nivel) %>% 
    dplyr::select(ADM1_EN,value) %>% 
    sf_as_ee()
  # Datos de población de Earth Engine
  poblacion_estimada <- ee$ImageCollection$Dataset$WorldPop_GP_100m_pop$
    select('population')$
    filter(ee$Filter$calendarRange(2020,2020, "year"))$
    mosaic()$
    rename('pop')
 # Estadistica zonal 
  stat_general <- ee_extract(
    x = poblacion_estimada,
    y = nuevos_limites,
    fun = ee$Reducer$sum(),
    scale = 100)
  stat_final <- stat_general %>% 
    mutate(pop = round(pop,0))
  return(stat_final)
}

# Resultados finales en listas
lista_stat <- lapply(X = 1:3,FUN = zona_impactada) 

# Tabla final por valores
datos_finales <- lista_stat %>% 
  map_df(.f = as_tibble) %>% 
  group_by(value) %>% 
  summarise(pop = sum(pop,na.rm = TRUE))
# write.csv(datos_finales,"resources/nieveles_pop.csv")
