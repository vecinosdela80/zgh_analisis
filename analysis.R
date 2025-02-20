library(tidyverse)
library(mapdeck)
library(sf)
library(geojsonsf)

## Mapdeck uses Mapbox maps, and to use Mapbox you need an access token.
## https://docs.mapbox.com/help/how-mapbox-works/access-tokens/
key <- "YOUR_MAPBOX_ACCESS_TOKEN"

## sf <- geojson_sf("https://github.com/vecinosdela80/lotesanuncio/raw/master/LotesAreaAnuncio.geojson")
## sf <- geojson_sf("https://github.com/vecinosdela80/zgh_analisis/raw/master/ZGH_2020.geojson")

normalize <- function(x) {
    return ((x - min(x)) / (max(x) - min(x)))
}

x <- sf$VALOR_M2 %>% as.numeric

mean(sf$VALOR_M2)

quantiles <- normalize(x) %>% quantile(seq(0,1,0.2))
x <- normalize(x)
sf$color <- dplyr::case_when(
                       x >= quantiles[[1]] &  x < quantiles[[2]] ~ sprintf("%s - %s", names(quantiles)[1],names(quantiles)[2]),
                       x >= quantiles[[2]] &  x < quantiles[[3]] ~ sprintf("%s - %s", names(quantiles)[2],names(quantiles)[3]),
                       x >= quantiles[[3]] &  x < quantiles[[4]] ~ sprintf("%s - %s", names(quantiles)[3],names(quantiles)[4]),
                       x >= quantiles[[4]] &  x < quantiles[[5]] ~ sprintf("%s - %s", names(quantiles)[4],names(quantiles)[5]),
                       x >= quantiles[[5]] &  x <= quantiles[[6]] ~ sprintf("%s - %s", names(quantiles)[5],names(quantiles)[6])
       )

e <- factor(sf$color)
levels(e) <- exp(1:5)
sf$e <- (e %>% as.character %>% as.numeric) * 3

## si no tienen un navegador por defecto
## options(browser="/usr/sbin/brave")

mapdeck(token = key) %>%
    add_polygon(
        data = sf
      , layer = "polygon_layer"
      , fill_colour = "color"
      , fill_opacity = 255
      , elevation = "e"
      , legend = TRUE
      , tooltip = "info"
      , auto_highlight = TRUE
    )

sf_80_100 <- sf[ sf$color %in% "80% - 100%", ]

x <- sf_80_100$VALOR_M2 %>% as.numeric
quantiles <- normalize(x) %>% quantile(seq(0,1,0.2))
x <- normalize(x)
sf_80_100$e <- x * 1000
sf_80_100$color <- dplyr::case_when(
                       x >= quantiles[[1]] &  x < quantiles[[2]] ~ sprintf("%s - %s", names(quantiles)[1],names(quantiles)[2]),
                       x >= quantiles[[2]] &  x < quantiles[[3]] ~ sprintf("%s - %s", names(quantiles)[2],names(quantiles)[3]),
                       x >= quantiles[[3]] &  x < quantiles[[4]] ~ sprintf("%s - %s", names(quantiles)[3],names(quantiles)[4]),
                       x >= quantiles[[4]] &  x < quantiles[[5]] ~ sprintf("%s - %s", names(quantiles)[4],names(quantiles)[5]),
                       x >= quantiles[[5]] &  x <= quantiles[[6]] ~ sprintf("%s - %s", names(quantiles)[5],names(quantiles)[6])
          ##,
          ##  x >= quantiles[[6]] &  x < quantiles[[7]] ~ names(quantiles)[6],
          ##  x >= quantiles[[7]] &  x < quantiles[[8]] ~ names(quantiles)[7],
          ##  x >= quantiles[[8]] &  x < quantiles[[9]] ~ names(quantiles)[8],
          ##  x >= quantiles[[9]] &  x < quantiles[[10]] ~ names(quantiles)[9]
       )

mapdeck(token = key) %>%
    add_polygon(
        data = sf_80_100
      , layer = "polygon_layer"
      , fill_colour = "color"
      , fill_opacity = 190
      , elevation = "e"
      , legend = TRUE
      , tooltip = "info"
      , auto_highlight = TRUE
    )

#ANUNCIO
areaanuncio <- geojson_sf("./area_anuncio.geojson")
areaanuncio$e <- 100

mapdeck(token = key) %>%
    add_polygon(
        data = areaanuncio
      , layer = "polygon_layer"
      , fill_colour = "color"
      , fill_opacity = 180
      , elevation = "e"
    )

##
sf <- geojson_sf("https://github.com/vecinosdela80/zgh_analisis/raw/master/lotes_anuncio_zgh.geojson")
colnames(sf)

## Indexación desde 2016 a hoy
## Lo que pagarían hoy por metro cuadrado de terreno en la compra de predios
porcentaje_inc_ivp <- 0.151670758326767 + 1
sf <- sf %>% mutate(valor_hoy=ZGH2016*porcentaje_inc_ivp)

quantile(sf$valor_hoy,na.rm = TRUE)

options(browser="/usr/sbin/brave")
library(mapview)
sf %>%
    mapview(zcol = "valor_hoy")


## References
## https://github.com/cydalytics/HK_Properties_Price_Distribution
## https://wcmbishop.github.io/rayshader-demo/
