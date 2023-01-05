
# Bibliotecas 

library(raster)
library(tmap)
library(sf)
library(dplyr)
library(spData)
library(spDataLarge)
library(ggplot2)
library(leaflet)
library(rgdal)
library(ggsn)
library(gridExtra)
library(tidyverse)

# Identificando Biblioteca 

getwd ( )
.libPaths ("C:/Program Files/R/R-4.1.2/library")

# Carregando o arquivo

shapepe=shapefile("E:/Economia Ambiental/Producao Cientifica/Criminalidade e Seca/Database/Data Espacial/Shapefile/26MUE250GC_SIR.shp")

# Tratamento dos dados

database <- data.frame(Dados.Espaciais)
database <- database %>% rename(ID = id)
database <- database %>% rename(id= CODIBGE)
mapape=merge(mapape1,database,by="id", all.x=T)
names(mapape)
mapape1 <- fortify(model=mapape, region="GEOCODM")

df <- merge(mapape, df1, by="id")

# Mapa de criminalidade

df2  <- df1 %>% mutate_jenks_brks(TXCRIME,  n = 6)

A <- ggplot(df) + geom_sf(aes(fill = TXCRIME), colour = "black", size = 0.1) +
  coord_sf(xlim = c(-41.2, -34.8), ylim = c(-10, -6.5)) + 
  scale_fill_distiller(palette = "Greys", direction = 1, ) + 
  labs(fill = " ") + 
  annotation_north_arrow(style = north_arrow_nautical,  location = "te", height = unit (1.2, "cm"))  + 
  annotation_scale(location = "br", height = unit (0.15, "cm")) + 
  labs(x = "Longitude", 
       y = "Latitude", 
       title = "A. Todos os crimes", 
       subtitle = "Taxa média por 100 mil habitantes")

B <- ggplot(df) + geom_sf(aes(fill = TXCVLI), colour = "black", size = 0.1) +
  coord_sf(xlim = c(-41.2, -34.8), ylim = c(-10, -6.5)) + 
  scale_fill_distiller(palette = "Greys", direction = 1) + 
  labs(fill = " ") + 
  annotation_north_arrow(style = north_arrow_nautical,  location = "te", height = unit (1.2, "cm"))  + 
  annotation_scale(location = "br", height = unit (0.15, "cm")) + 
  labs(x = "Longitude", 
       y = "Latitude", 
       title = "B. Crimes violentos e letais", 
       subtitle = "Taxa média por 100 mil habitantes")

C <- ggplot(df) + geom_sf(aes(fill = TXCVP), colour = "black", size = 0.1) +
  coord_sf(xlim = c(-41.2, -34.8), ylim = c(-10, -6.5)) + 
  scale_fill_distiller(palette = "Greys", direction = 1) + 
  labs(fill = " ") + 
  annotation_north_arrow(style = north_arrow_nautical,  location = "te", height = unit (1.2, "cm"))  + 
  annotation_scale(location = "br", height = unit (0.15, "cm")) + 
  labs(x = "Longitude", 
       y = "Latitude", 
       title = "C. Crimes contra o patrimônio", 
       subtitle = "Taxa média por 100 mil habitantes")

D <- ggplot(df) + geom_sf(aes(fill = TXESTUPRO), colour = "black", size = 0.1) +
  coord_sf(xlim = c(-41.2, -34.8), ylim = c(-10, -6.5)) + 
  scale_fill_distiller(palette = "Greys", direction = 1) + 
  labs(fill = " ") + 
  annotation_north_arrow(style = north_arrow_nautical,  location = "te", height = unit (1.2, "cm"))  + 
  annotation_scale(location = "br", height = unit (0.15, "cm")) + 
  labs(x = "Longitude", 
       y = "Latitude", 
       title = "D. Estupro", 
       subtitle = "Taxa média por 100 mil habitantes")

E <- ggplot(df) + geom_sf(aes(fill = TXVDFCM), colour = "black", size = 0.1) +
  coord_sf(xlim = c(-41.2, -34.8), ylim = c(-10, -6.5)) + 
  scale_fill_distiller(palette = "Greys", direction = 1) + 
  labs(fill = " ") + 
  annotation_north_arrow(style = north_arrow_nautical,  location = "te", height = unit (1.2, "cm"))  + 
  annotation_scale(location = "br", height = unit (0.15, "cm")) + 
  labs(x = "Longitude", 
       y = "Latitude", 
       title = "E. Violência doméstica e familiar contra a mulher", 
       subtitle = "Taxa média por 100 mil habitantes")

F <- ggplot(df) + geom_sf(aes(fill = TXESTVDM), colour = "black", size = 0.1) +
  coord_sf(xlim = c(-41.2, -34.8), ylim = c(-10, -6.5)) + 
  scale_fill_distiller(palette = "Greys", direction = 1) + 
  labs(fill = " ") + 
  annotation_north_arrow(style = north_arrow_nautical,  location = "te", height = unit (1.2, "cm"))  + 
  annotation_scale(location = "br", height = unit (0.15, "cm")) + 
  labs(x = "Longitude", 
       y = "Latitude", 
       title = "F. Estupro e violência doméstica e familiar contra a mulher", 
       subtitle = "Taxa média por 100 mil habitantes")


SECA <- ggplot(df) + geom_sf(aes(fill = DCSeca), colour = "black", size = 0.1) +
  coord_sf(xlim = c(-41.2, -34.8), ylim = c(-10, -6.5)) + 
  scale_fill_distiller(palette = "Greys", direction = 1) + 
  labs(fill = " ") + 
  annotation_north_arrow(style = north_arrow_nautical,  location = "te", height = unit (1.2, "cm"))  + 
  annotation_scale(location = "br", height = unit (0.15, "cm")) + 
  labs(x = "Longitude", 
       y = "Latitude", 
       title = "Eventos extremos de seca", 
       subtitle = "Média de choques de seca")

grid.arrange(SECA, SECA, SECA, SECA , ncol= 2)

# Mapa de Clusters 

scale_color_manual(breaks = c("4", "3", "2", "1", "0"), 
                   values=c("red", "orange","yellow","cadetblue2","dodgerblue"),
                   labels = c("Alto-Alto", "Baixo-Baixo", "Baixo-Alto", "Alto-Baixo",
                              "Não significativo"))

values=c("#ff0000", "#0000bf", "#7070bf", "#bf7070", "#ffffff")

# CRIANDO MAPAS TEMÁTICOS DE VIOLÊNCIA 
# Exemplo: https://blog.curso-r.com/posts/2019-02-10-sf-miojo/
library(brazilmaps)
library(tidyverse)

mapape <- get_brmap("City", geo.filter = list(State = 26))
mapape$id <- mapape$City
mapape$id <- mapape$City



# JUNTANDO BASES 

mapape=merge(mapape,df1,by="id", all.x=T)

# CRIANDO MAPAS TEMÁTICOS

# Visualizando mapa de Pernambuco

mapape %>% 
  ggplot() +
  geom_sf(aes(fill = TXCRIME))

Mesoregion <- mapape$MesoRegion

# Criando mapas de criminalidade de Pernambuco 

 A <- ggplot(data=mapape) + 
  geom_sf(aes(fill = TXCRIME), colour = "black", size = 0.1) +
  coord_sf(xlim = c(-41.2, -34.8), ylim = c(-10, -6.5)) + 
  scale_fill_distiller(palette = "Spectral") + 
  labs(fill = " ") + 
   annotation_north_arrow(style = north_arrow_nautical, location = "te",  height = unit (1.2, "cm")) + 
   annotation_scale(location = "br", height = unit (0.15, "cm")) + 
   labs(x = "Longitude", 
        y = "Latitude", 
        title = "A. Todos os crimes", 
        subtitle = "Taxa média por 100 mil habitantes")
 
B <- ggplot(data=mapape) + 
   geom_sf(aes(fill = TXCVLI), colour = "black", size = 0.1) +
   coord_sf(xlim = c(-41.2, -34.8), ylim = c(-10, -6.5)) + 
   scale_fill_distiller(palette = "Spectral") + 
   labs(fill = " ") + 
  annotation_north_arrow(style = north_arrow_nautical, location = "te", height = unit (1.2, "cm"))  + 
  annotation_scale(location = "br", height = unit (0.15, "cm")) + 
   labs(x = "Longitude", 
        y = "Latitude", 
        title = "B. Crimes violentos e letais", 
        subtitle = "Taxa média por 100 mil habitantes")
 
C <- ggplot(data=mapape) + 
  geom_sf(aes(fill = TXCVP), colour = "black", size = 0.1) +
  coord_sf(xlim = c(-41.2, -34.8), ylim = c(-10, -6.5)) + 
  scale_fill_distiller(palette = "Spectral") + 
  labs(fill = " ") + 
  annotation_north_arrow(style = north_arrow_nautical,  location = "te",  height = unit (1.2, "cm"))  + 
  annotation_scale(location = "br", height = unit (0.15, "cm")) + 
  labs(x = "Longitude", 
       y = "Latitude", 
       title = "C. Crimes contra o patrimônio", 
       subtitle = "Taxa média por 100 mil habitantes")

D <- ggplot(data=mapape) + 
  geom_sf(aes(fill = TXVDFCM), colour = "black", size = 0.1) +
  coord_sf(xlim = c(-41.2, -34.8), ylim = c(-10, -6.5)) + 
  scale_fill_distiller(palette = "Spectral") + 
  labs(fill = " ") + 
  annotation_north_arrow(style = north_arrow_nautical,  location = "te", height = unit (1.2, "cm"))  + 
  annotation_scale(location = "br", height = unit (0.15, "cm")) + 
  labs(x = "Longitude", 
       y = "Latitude", 
       title = "D. Violência doméstica e familiar contra a mulher", 
       subtitle = "Taxa média por 100 mil habitantes")

E <- ggplot(data=mapape) + 
  geom_sf(aes(fill = TXE_UPRO), colour = "black", size = 0.1) +
  coord_sf(xlim = c(-41.2, -34.8), ylim = c(-10, -6.5)) + 
  scale_fill_distiller(palette = "Spectral") + 
  labs(fill = " ") + 
  annotation_north_arrow(style = north_arrow_nautical,  location = "te", height = unit (1.2, "cm"))  + 
  annotation_scale(location = "br", height = unit (0.15, "cm")) + 
  labs(x = "Longitude", 
       y = "Latitude",  
       title = "E. Estupros", 
       subtitle = "Taxa média por 100 mil habitantes")

F <- ggplot(data=mapape) + 
  geom_sf(aes(fill = TXESTVDM), colour = "black", size = 0.1) +
  coord_sf(xlim = c(-41.2, -34.8), ylim = c(-10, -6.5)) + 
  scale_fill_distiller(palette = "Spectral") + 
  labs(fill = " ") + 
  annotation_north_arrow(style = north_arrow_nautical,  location = "te", height = unit (1.2, "cm"))  + 
  annotation_scale(location = "br", height = unit (0.15, "cm")) + 
  labs(x = "Longitude", 
       y = "Latitude", 
       title = "F. Estupros e violência doméstica e familiar contra a mulher", 
       subtitle = "Taxa média por 100 mil habitantes" )

grid.arrange(A, B, C, D, E, F, ncol= 2)

Seca <- ggplot(data=mapape) + 
  geom_sf(aes(fill = DCSeca), colour = "black", size = 0.1) +
  coord_sf(xlim = c(-41.2, -34.8), ylim = c(-10, -6.5)) + 
  scale_fill_distiller(palette = "Spectral") + 
  labs(fill = " ") + 
  annotation_north_arrow(style = north_arrow_nautical, location = "te",  height = unit (1.2, "cm")) + 
  annotation_scale(location = "br", height = unit (0.15, "cm")) + 
  labs(x = "Longitude", 
       y = "Latitude", 
       title = "Eventos extremos de seca", 
       subtitle = "Média de choques de seca")

head(df1)



# Agregando mapas temáticos

grid.arrange( A, B, C, D, E, F, ncol= 2)

# Mapa de clusters 

mapape$cluster1 <- as.factor(mapape$CTXCRIME)
mapape$cluster2 <- as.factor(mapape$CTXCVLI)
mapape$cluster3 <- as.factor(mapape$CTXCVP)
mapape$cluster4 <- as.factor(mapape$CTXVDFCM)
mapape$cluster5 <- as.factor(mapape$CTXESTUPRO)
mapape$cluster6 <- as.factor(mapape$CTXESTVDM)

CA <- ggplot(data=mapape) +
  geom_sf(aes(fill= cluster1), colour = "Black", size=0.5) + 
  coord_sf(xlim = c(-41.2, -34.8), ylim = c(-10, -6.5)) +
  scale_fill_manual(values = c("0" = "#ffffff", "1" = "#ff0000",
                               "2" = "#0000bf", "3" = "#7070bf", "4"= "#f5a49a"),
                    labels = c("Não significativo", "Alto-Alto", "Baixo-Baixo", "Baixo-Alto", 
                               "Alto-Baixo")) + labs(fill = " ") + 
  annotation_north_arrow(style = north_arrow_nautical,  location = "te", height = unit (1.2, "cm"))  + 
  annotation_scale(location = "br", height = unit (0.15, "cm")) + 
  labs(x = "Longitude", 
       y = "Latitude", 
       title = "A. Todos os crimes", 
       subtitle = "Taxa média por 100 mil habitantes" ) 

CB <- ggplot(data=mapape) +
  geom_sf(aes(fill= cluster2), colour = "Black", size=0.5) + 
  coord_sf(xlim = c(-41.2, -34.8), ylim = c(-10, -6.5)) +
  scale_fill_manual(values = c("0" = "#ffffff", "1" = "#ff0000",
                               "2" = "#0000bf", "3" = "#7070bf", "4"= "#f5a49a"),
                    labels = c("Não significativo", "Alto-Alto", "Baixo-Baixo", "Baixo-Alto", 
                               "Alto-Baixo")) + labs(fill = " ") + 
  annotation_north_arrow(style = north_arrow_nautical,  location = "te", height = unit (1.2, "cm"))  + 
  annotation_scale(location = "br", height = unit (0.15, "cm")) + 
  labs(x = "Longitude", 
       y = "Latitude", 
       title = "B. Crimes violentos e letais", 
       subtitle = "Taxa média por 100 mil habitantes")

CC <- ggplot(data=mapape) +
  geom_sf(aes(fill= cluster3), colour = "Black", size=0.5) + 
  coord_sf(xlim = c(-41.2, -34.8), ylim = c(-10, -6.5)) +
  scale_fill_manual(values = c("0" = "#ffffff", "1" = "#ff0000",
                               "2" = "#0000bf", "3" = "#7070bf", "4"= "#f5a49a"),
                    labels = c("Não significativo", "Alto-Alto", "Baixo-Baixo", "Baixo-Alto", 
                               "Alto-Baixo")) + labs(fill = " ") + 
  annotation_north_arrow(style = north_arrow_nautical,  location = "te", height = unit (1.2, "cm"))  + 
  annotation_scale(location = "br", height = unit (0.15, "cm")) + 
  labs(x = "Longitude", 
       y = "Latitude", 
       title = "C. Crimes contra o patrimônio", 
       subtitle = "Taxa média por 100 mil habitantes")

CD <- ggplot(data=mapape) +
  geom_sf(aes(fill= cluster4), colour = "Black", size=0.5) + 
  coord_sf(xlim = c(-41.2, -34.8), ylim = c(-10, -6.5)) +
  scale_fill_manual(values = c("0" = "#ffffff", "1" = "#ff0000",
                               "2" = "#0000bf", "3" = "#7070bf", "4"= "#f5a49a"),
                    labels = c("Não significativo", "Alto-Alto", "Baixo-Baixo", "Baixo-Alto", 
                               "Alto-Baixo")) + labs(fill = " ") + 
  annotation_north_arrow(style = north_arrow_nautical,  location = "te", height = unit (1.2, "cm"))  + 
  annotation_scale(location = "br", height = unit (0.15, "cm")) + 
  labs(x = "Longitude", 
       y = "Latitude", 
       title = "D. Violência doméstica e familiar contra a mulher", 
       subtitle = "Taxa média por 100 mil habitantes")


CE <- ggplot(data=mapape) +
  geom_sf(aes(fill= cluster5), colour = "Black", size=0.5) + 
  coord_sf(xlim = c(-41.2, -34.8), ylim = c(-10, -6.5)) +
  scale_fill_manual(values = c("0" = "#ffffff", "1" = "#ff0000",
                               "2" = "#0000bf", "3" = "#7070bf", "4"= "#f5a49a"),
                    labels = c("Não significativo", "Alto-Alto", "Baixo-Baixo", "Baixo-Alto", 
                               "Alto-Baixo")) + labs(fill = " ") + 
  annotation_north_arrow(style = north_arrow_nautical,  location = "te", height = unit (1.2, "cm"))  + 
  annotation_scale(location = "br", height = unit (0.15, "cm")) + 
  labs(x = "Longitude", 
       y = "Latitude",  
       title = "E. Estupros", 
       subtitle = "Taxa média por 100 mil habitantes")

CF <- ggplot(data=mapape) +
  geom_sf(aes(fill= cluster6), colour = "Black", size=0.5) + 
  coord_sf(xlim = c(-41.2, -34.8), ylim = c(-10, -6.5)) +
  scale_fill_manual(values = c("0" = "#ffffff", "1" = "#ff0000",
                               "2" = "#0000bf", "3" = "#7070bf", "4"= "#f5a49a"),
                    labels = c("Não significativo", "Alto-Alto", "Baixo-Baixo", "Baixo-Alto", 
                               "Alto-Baixo")) + labs(fill = " ") + 
  annotation_north_arrow(style = north_arrow_nautical,  location = "te", height = unit (1.2, "cm"))  + 
  annotation_scale(location = "br", height = unit (0.15, "cm")) + 
  labs(x = "Longitude", 
       y = "Latitude", 
       title = "F. Estupros e violência doméstica e familiar contra a mulher", 
       subtitle = "Taxa média por 100 mil habitantes" )


grid.arrange( CA, CB, CC, CD, CE, CF, ncol= 2)
