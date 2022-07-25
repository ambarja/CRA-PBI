# **Introducción a datos e información de pronósticos para construir mapas de PBI**

## Ejemplos:
 - 🌧️ Niño Costero Perú 2017
 - 🌀 Tifón Molave Vietnam 2020

## Intento de automatización usando el ecosistema espacial de R
- Requerimientos:
   - `rgee`
   - `tidyverse`
   - `sf`
   - `mapview`

#### Activación de paquetes necesarios

```r
library(tidyverse)
library(sf)
library(mapview)
library(rgee)
ee_Initialize(user = "antony.barja@upch.pe)
```

#### Lectura de datos 

```r
pronostico <- st_read("resources/Tifon/datos_tifon2020.gpkg",layer = "pronostico")
limites <- st_read("resources/Tifon/datos_tifon2020.gpkg",layer = "admin1")
```
#### Función para calcular la posible afectacción 

```r
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

```
#### Cuadro estadístico 

```r
datos_finales <- lista_stat %>% 
  map_df(.f = as_tibble) %>% 
  group_by(value) %>% 
  summarise(pop = sum(pop,na.rm = TRUE))
```
```r
# A tibble: 3 × 3
   ...1 value       pop
  <dbl> <chr>     <dbl>
1     1 < 5     6520944
2     2 > 90    5314314
3     3 30 - 40 2175340
```