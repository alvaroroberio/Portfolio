########################################################################################################################
# Rotina {R} : Producao de Mapas e Figuras
# Data: 2022 
# Autor: Alvaro Roberio
# Website: alvaroroberio.github.io
########################################################################################################################

# Pacotes do R 

library("raster")
library("tmap")
library("sf")
library("dplyr")
library("spData")
library("spDataLarge")
library("ggplot2")
library("leaflet")
library("rgdal")
library("ggsn")
library("gridExtra")
library("tidyverse")
library("readr")
library("readxl")
library("foreign")
library("haven")
library("tmap")
library("ggspatial")

# Definindo Diretório 

setwd("D:/Microdados de Agricultura/Data_Tratado/Nordeste/Analise Espacial")
dir()

# Importando Dados 

NE   <- read_stata("DATA_ESP_INFRA.dta")
BRA  <- st_read("BRA.shp")

# Recortando Shapefile 

MNE <- subset(BRA, GRANDES_RE=="Nordeste")
head(MNE)

# Padronizando o Database ao Shapefile 

NE <- rename(NE, CODIGO_MUN = CODIBGE2)

# Tratamento dos dados

DNE = merge(MNE, NE, by = c("CODIGO_MUN"), all.x = T)
names(DNE)

DNE1 <- fortify(model=DNE, region="CODIGO_MUN")
head(DNE1)

### Delimitações regionais 

# Delimitação regional do "Semiárido

DNE1%>% 
  group_by(SEMIARIDO) %>%
  summarise(geometry = sf::st_union(geometry)) %>%
  ungroup() -> DNE2
    
# Delimitação regional do "estado"

DNE1%>% 
  group_by(NOME_UF) %>%
  summarise(geometry = sf::st_union(geometry)) %>%
  ungroup() -> DNE3

DNE1$NE = 1

DNE1%>% 
  group_by(NE) %>%
  summarise(geometry = sf::st_union(geometry)) %>%
  ungroup() -> DNE4

### Produção de Gráficos

# Gráfico 1: Açude

 Acude <- ggplot(DNE1) + 
  geom_sf(aes(fill = as.factor(DACUDE)), colour=NA, size=0.2)  + 
  scale_fill_manual(values = c("#0000CD", "#F4F2EB"), breaks = c("1", "0"), labels=c("Yes", "No")) + 
  guides(fill=guide_legend(title="Agreement concluded")) + 
  annotation_north_arrow(location = "tr", which_north = "true", pad_x = unit(0.1, "in"), pad_y = unit(0.1, "in"),
                          style = north_arrow_nautical(fill = c("grey40", "white"), line_col = "grey20",
                          text_family = "ArcherPro Book")) + 
  annotation_scale(location = "br", height = unit (0.10, "cm"), which_north = "true") +
  labs(x = " ",  y = " ", title = " ", subtitle = " ", caption = "Construction of at least one weir") + 
  geom_sf(data=DNE2, aes(color = SEMIARIDO), fill = "transparent", size=1) +
  geom_sf(data=DNE4, aes(color = NE), fill = "transparent", color= "black", size=0.7) +
  guides(color=guide_legend(title = " ")) +
  scale_color_manual(values = c("red", "black"), breaks = c("Sim"), labels = c("Northeast Semiarid")) + 
  theme(plot.caption = element_text(hjust = 0.5, size = 12, face = "bold"))

Acude

# Gráfico 2: Barragem 


Barragem <- ggplot(DNE1) + 
  geom_sf(aes(fill = as.factor(DBARRAGEM)), colour=NA, size=0.2)  + 
  scale_fill_manual(values = c("#0000CD", "#F4F2EB"), breaks = c("1", "0"), labels=c("Yes", "No")) + 
  guides(fill=guide_legend(title="Agreement concluded")) + 
  annotation_north_arrow(location = "tr", which_north = "true", pad_x = unit(0.1, "in"), pad_y = unit(0.1, "in"),
                         style = north_arrow_nautical(fill = c("grey40", "white"), line_col = "grey20",
                                                      text_family = "ArcherPro Book")) + 
  annotation_scale(location = "br", height = unit (0.10, "cm"), which_north = "true") +
  labs(x = " ",  y = " ", title = " ", subtitle = " ", caption = "Construction of at least one weir") + 
  geom_sf(data=DNE2, aes(color = SEMIARIDO), fill = "transparent", size=1) +
  geom_sf(data=DNE4, aes(color = NE), fill = "transparent", color= "black", size=0.7) +
  guides(color=guide_legend(title = " ")) +
  scale_color_manual(values = c("red", "black"), breaks = c("Sim"), labels = c("Northeast Semiarid")) + 
  theme(plot.caption = element_text(hjust = 0.5, size = 12, face = "bold"))

Barragem

# Gráfico 3: Infraestrutura Hídrica 


Infraestrutura <- ggplot(DNE1) + 
  geom_sf(aes(fill = as.factor(DINFRAHIDRICA)), colour=NA, size=0.2)  + 
  scale_fill_manual(values = c("#0000CD", "#F4F2EB"), breaks = c("1", "0"), labels=c("Yes", "No")) + 
  guides(fill=guide_legend(title="Agreement concluded")) + 
  annotation_north_arrow(location = "tr", which_north = "true", pad_x = unit(0.1, "in"), pad_y = unit(0.1, "in"),
                         style = north_arrow_nautical(fill = c("grey40", "white"), line_col = "grey20",
                                                      text_family = "ArcherPro Book")) + 
  annotation_scale(location = "br", height = unit (0.10, "cm"), which_north = "true") +
  labs(x = " ",  y = " ", title = " ", subtitle = " ", caption = "Construction of at least one weir or dam") + 
  geom_sf(data=DNE2, aes(color = SEMIARIDO), fill = "transparent", size=1) +
  geom_sf(data=DNE4, aes(color = NE), fill = "transparent", color= "black", size=0.7) +
  guides(color=guide_legend(title = " ")) +
  scale_color_manual(values = c("red", "black"), breaks = c("Sim"), labels = c("Northeast Semiarid")) + 
  theme(plot.caption = element_text(hjust = 0.5, size = 12, face = "bold"))

Infraestrutura

grid.arrange(Acude, Barragem, Infraestrutura, ncol= 2)

rm(Barragem, Acude, Infraestrutura, MNE, NE)

# DNE1 (Delimitação da Região do Nordeste)
# DNE2 (Delimitação da Região do Semiárido)

########################################################################################################################
# Produção de mapas da seca                                                                                            #
########################################################################################################################

# Banco de Dados de seca 

dir()
DSDI   <- read_stata("DATA_ESP_SDI.dta")

# Construindo paleta de cores 

pal <- c("#F4F2EB", "#FBFF03", "#FFD374", "#ff3b22", "#df0000")


# Gráfico de 1981 

CSDI1981 <- subset(DSDI, DSDI$ANO=="1981")
CSDI1981 <- rename(CSDI1981, CSDI1981 = CSDI)
CSDI1981$ANO = NULL
DNE1 = merge(DNE1, CSDI1981, by = c("CODIGO_MUN"), all.x = T)


GCSDI1981 = ggplot(data=DNE1) +  geom_sf(aes(fill = CSDI1981), colour = NA, size = 0.01) +
  scale_fill_gradientn(colours =  pal, limits=c(0, 4), 
                       labels = c("Almost Normal", "Mild Drought", "Moderate Drought", "Grave Drought", "Severe Drought")) + 
  labs(fill = " ") + guides(fill=guide_legend(title="Intensity")) + 
  labs(x = " ", 
       y = " ", 
       title = " ", 
       caption = "1981") + guides(color=guide_legend(title = " ")) + 
  geom_sf(data=DNE4, aes(color = NE), fill = "transparent", color= "black", size=0.7) +
  geom_sf(data=DNE3, aes(color = NE), fill = "transparent", color= "black", size=0.7) +
  guides(color=guide_legend(title = " ")) + 
  theme(plot.caption = element_text(hjust = 0.5, size = 12, face = "bold"))


# Gráfico de 1985 

CSDI1985 <- subset(DSDI, DSDI$ANO=="1985")
CSDI1985 <- rename(CSDI1985, CSDI1985 = CSDI)
CSDI1985$ANO = NULL
DNE1 = merge(DNE1, CSDI1985, by = c("CODIGO_MUN"), all.x = T)


GCSDI1985 = ggplot(data=DNE1) +  geom_sf(aes(fill = CSDI1985), colour = NA, size = 0.01) +
  scale_fill_gradientn(colours =  pal, limits=c(0, 4), 
                       labels = c("Almost Normal", "Mild Drought", "Moderate Drought", "Grave Drought", "Severe Drought")) + 
  labs(fill = " ") + guides(fill=guide_legend(title="Intensity")) + 
  labs(x = " ", 
       y = " ", 
       title = " ", 
       caption = "1985") + guides(color=guide_legend(title = " ")) + 
  geom_sf(data=DNE4, aes(color = NE), fill = "transparent", color= "black", size=0.7) +
  geom_sf(data=DNE3, aes(color = NE), fill = "transparent", color= "black", size=0.7) +
  guides(color=guide_legend(title = " ")) + 
  theme(plot.caption = element_text(hjust = 0.5, size = 12, face = "bold"))


# Gráfico de 1990

CSDI1990 <- subset(DSDI, DSDI$ANO=="1990")
CSDI1990 <- rename(CSDI1990, CSDI1990 = CSDI)
CSDI1990$ANO = NULL
DNE1 = merge(DNE1, CSDI1990, by = c("CODIGO_MUN"), all.x = T)


GCSDI1990 = ggplot(data=DNE1) +  geom_sf(aes(fill = CSDI1990), colour = NA, size = 0.01) +
  scale_fill_gradientn(colours =  pal, limits=c(0, 4), 
                       labels = c("Almost Normal", "Mild Drought", "Moderate Drought", "Grave Drought", "Severe Drought")) + 
  labs(fill = " ") + guides(fill=guide_legend(title="Intensity")) +
  labs(x = " ", 
       y = " ", 
       title = " ", 
       caption = "1990") + guides(color=guide_legend(title = " ")) + 
  geom_sf(data=DNE4, aes(color = NE), fill = "transparent", color= "black", size=0.7) +
  geom_sf(data=DNE3, aes(color = NE), fill = "transparent", color= "black", size=0.7) +
  guides(color=guide_legend(title = " ")) + 
  theme(plot.caption = element_text(hjust = 0.5, size = 12, face = "bold"))

# Gráfico de 1995

CSDI1995 <- subset(DSDI, DSDI$ANO=="1995")
CSDI1995 <- rename(CSDI1995, CSDI1995 = CSDI)
CSDI1995$ANO = NULL
DNE1 = merge(DNE1, CSDI1995, by = c("CODIGO_MUN"), all.x = T)


GCSDI1995 = ggplot(data=DNE1) +  geom_sf(aes(fill = CSDI1995), colour = NA, size = 0.01) +
  scale_fill_gradientn(colours =  pal, limits=c(0, 4), 
                       labels = c("Almost Normal", "Mild Drought", "Moderate Drought", "Grave Drought", "Severe Drought")) + 
  labs(fill = " ") + guides(fill=guide_legend(title="Intensity")) + 
  labs(x = " ", 
       y = " ", 
       title = " ", 
       caption = "1995") + guides(color=guide_legend(title = " ")) + 
  geom_sf(data=DNE4, aes(color = NE), fill = "transparent", color= "black", size=0.7) +
  geom_sf(data=DNE3, aes(color = NE), fill = "transparent", color= "black", size=0.7) +
  guides(color=guide_legend(title = " ")) + 
  theme(plot.caption = element_text(hjust = 0.5, size = 12, face = "bold"))


# Gráfico de 2000

CSDI2000 <- subset(DSDI, DSDI$ANO=="2000")
CSDI2000 <- rename(CSDI2000, CSDI2000 = CSDI)
CSDI2000$ANO = NULL
DNE1 = merge(DNE1, CSDI2000, by = c("CODIGO_MUN"), all.x = T)

GCSDI2000 = ggplot(data=DNE1) +  geom_sf(aes(fill = CSDI2000), colour = NA, size = 0.01) +
  scale_fill_gradientn(colours =  pal, limits=c(0, 4), 
                       labels = c("Almost Normal", "Mild Drought", "Moderate Drought", "Grave Drought", "Severe Drought")) + 
  labs(fill = " ") + guides(fill=guide_legend(title="Intensity")) + 
  labs(x = " ", 
       y = " ", 
       title = " ", 
       caption = "2000") + guides(color=guide_legend(title = " ")) + 
  geom_sf(data=DNE4, aes(color = NE), fill = "transparent", color= "black", size=0.7) +
  geom_sf(data=DNE3, aes(color = NE), fill = "transparent", color= "black", size=0.7) +
  guides(color=guide_legend(title = " ")) + 
  theme(plot.caption = element_text(hjust = 0.5, size = 12, face = "bold"))


CSDI2005 <- subset(DSDI, DSDI$ANO=="2005")
CSDI2005 <- rename(CSDI2005, CSDI2005 = CSDI)
CSDI2005$ANO = NULL
DNE1 = merge(DNE1, CSDI2005, by = c("CODIGO_MUN"), all.x = T)


GCSDI2005 = ggplot(data=DNE1) +  geom_sf(aes(fill = CSDI2005), colour = NA, size = 0.01) +
  scale_fill_gradientn(colours =  pal, limits=c(0, 4), 
                       labels = c("Almost Normal", "Mild Drought", "Moderate Drought", "Grave Drought", "Severe Drought")) + 
  labs(fill = " ") + guides(fill=guide_legend(title="Intensity")) + 
  labs(x = " ", 
       y = " ", 
       title = " ", 
       caption = "2005") + guides(color=guide_legend(title = " ")) + 
  geom_sf(data=DNE4, aes(color = NE), fill = "transparent", color= "black", size=0.7) +
  geom_sf(data=DNE3, aes(color = NE), fill = "transparent", color= "black", size=0.7) +
  guides(color=guide_legend(title = " ")) + 
  theme(plot.caption = element_text(hjust = 0.5, size = 12, face = "bold"))

CSDI2010 <- subset(DSDI, DSDI$ANO=="2010")
CSDI2010 <- rename(CSDI2010, CSDI2010 = CSDI)
CSDI2010$ANO = NULL
DNE1 = merge(DNE1, CSDI2010, by = c("CODIGO_MUN"), all.x = T)


GCSDI2010 = ggplot(data=DNE1) +  geom_sf(aes(fill = CSDI2010), colour = NA, size = 0.01) +
  scale_fill_gradientn(colours =  pal, limits=c(0, 4), 
                       labels = c("Almost Normal", "Mild Drought", "Moderate Drought", "Grave Drought", "Severe Drought")) + 
  labs(fill = " ") + guides(fill=guide_legend(title="Intensity")) + 
  labs(x = " ", 
       y = " ", 
       title = " ", 
       caption = "2010") + guides(color=guide_legend(title = " ")) + 
  geom_sf(data=DNE4, aes(color = NE), fill = "transparent", color= "black", size=0.7) +
  geom_sf(data=DNE3, aes(color = NE), fill = "transparent", color= "black", size=0.7) +
  guides(color=guide_legend(title = " ")) + 
  theme(plot.caption = element_text(hjust = 0.5, size = 12, face = "bold"))

CSDI2015 <- subset(DSDI, DSDI$ANO=="2015")
CSDI2015 <- rename(CSDI2015, CSDI2015 = CSDI)
CSDI2015$ANO = NULL
DNE1 = merge(DNE1, CSDI2015, by = c("CODIGO_MUN"), all.x = T)


GCSDI2015 = ggplot(data=DNE1) +  geom_sf(aes(fill = CSDI2015), colour = NA, size = 0.01) +
  scale_fill_gradientn(colours =  pal, limits=c(0, 4), 
                       labels = c("Almost Normal", "Mild Drought", "Moderate Drought", "Grave Drought", "Severe Drought")) + 
  labs(fill = " ") + guides(fill=guide_legend(title="Intensity")) + 
  labs(x = " ", 
       y = " ", 
       title = " ", 
       caption = "2015") + guides(color=guide_legend(title = " ")) + 
  geom_sf(data=DNE4, aes(color = NE), fill = "transparent", color= "black", size=0.7) +
  geom_sf(data=DNE3, aes(color = NE), fill = "transparent", color= "black", size=0.7) +
  guides(color=guide_legend(title = " ")) + 
  theme(plot.caption = element_text(hjust = 0.5, size = 12, face = "bold"))

CSDI2020 <- subset(DSDI, DSDI$ANO=="2020")
CSDI2020 <- rename(CSDI2020, CSDI2020 = CSDI)
CSDI2020$ANO = NULL
DNE1 = merge(DNE1, CSDI2020, by = c("CODIGO_MUN"), all.x = T)

GCSDI2020 = ggplot(data=DNE1) +  geom_sf(aes(fill = CSDI2020), colour = NA, size = 0.01) +
  scale_fill_gradientn(colours =  pal, limits=c(0, 4), 
                       labels = c("Almost Normal", "Mild Drought", "Moderate Drought", "Grave Drought", "Severe Drought")) + 
  labs(fill = " ") + guides(fill=guide_legend(title="Intensity")) + 
  labs(x = " ", 
       y = " ", 
       title = " ", 
       caption = "2020") + guides(color=guide_legend(title = " ")) + 
  geom_sf(data=DNE4, aes(color = NE), fill = "transparent", color= "black", size=0.7) +
  geom_sf(data=DNE3, aes(color = NE), fill = "transparent", color= "black", size=0.7) +
  guides(color=guide_legend(title = " ")) + 
  theme(plot.caption = element_text(hjust = 0.5, size = 12, face = "bold"))


# Exportando mapas de 1981-2020

grid.arrange (GCSDI1981, GCSDI1985, GCSDI1990,
              GCSDI1995, GCSDI2000, GCSDI2005,
              GCSDI2010, GCSDI2015, GCSDI2020, ncol= 3)


########################################################################################################################
# Gráfico de 2000-2020                                                                                                 #
########################################################################################################################

CSDI2000 <- subset(DSDI, DSDI$ANO=="2000")
CSDI2000 <- rename(CSDI2000, CSDI2000 = CSDI)
CSDI2000$ANO = NULL
DNE1 = merge(DNE1, CSDI2000, by = c("CODIGO_MUN"), all.x = T)


GCSDI2000 = ggplot(data=DNE1) +  geom_sf(aes(fill = CSDI2000), colour = NA, size = 0.01) +
  scale_fill_gradientn(colours =  pal, limits=c(0, 4), 
                       labels = c("Almost Normal", "Mild Drought", "Moderate Drought", "Grave Drought", "Severe Drought")) + 
  labs(fill = " ") + guides(fill=guide_legend(title="Intensity")) + 
  labs(x = " ", 
       y = " ", 
       title = " ", 
       caption = "2000") + guides(color=guide_legend(title = " ")) + 
  geom_sf(data=DNE4, aes(color = NE), fill = "transparent", color= "black", size=0.7) +
  geom_sf(data=DNE3, aes(color = NE), fill = "transparent", color= "black", size=0.7) +
  guides(color=guide_legend(title = " ")) + 
  theme(plot.caption = element_text(hjust = 0.5, size = 12, face = "bold"))


CSDI2001 <- subset(DSDI, DSDI$ANO=="2001")
CSDI2001 <- rename(CSDI2001, CSDI2001 = CSDI)
CSDI2001$ANO = NULL
DNE1 = merge(DNE1, CSDI2001, by = c("CODIGO_MUN"), all.x = T)


GCSDI2001 = ggplot(data=DNE1) +  geom_sf(aes(fill = CSDI2001), colour = NA, size = 0.01) +
  scale_fill_gradientn(colours =  pal, limits=c(0, 4), 
                       labels = c("Almost Normal", "Mild Drought", "Moderate Drought", "Grave Drought", "Severe Drought")) + 
  labs(fill = " ") + guides(fill=guide_legend(title="Intensity")) + 
  labs(x = " ", 
       y = " ", 
       title = " ", 
       caption = "2001") + guides(color=guide_legend(title = " ")) + 
  geom_sf(data=DNE4, aes(color = NE), fill = "transparent", color= "black", size=0.7) +
  geom_sf(data=DNE3, aes(color = NE), fill = "transparent", color= "black", size=0.7) +
  guides(color=guide_legend(title = " ")) + 
  theme(plot.caption = element_text(hjust = 0.5, size = 12, face = "bold"))


CSDI2002 <- subset(DSDI, DSDI$ANO=="2002")
CSDI2002 <- rename(CSDI2002, CSDI2002 = CSDI)
CSDI2002$ANO = NULL
DNE1 = merge(DNE1, CSDI2002, by = c("CODIGO_MUN"), all.x = T)


GCSDI2002 = ggplot(data=DNE1) +  geom_sf(aes(fill = CSDI2002), colour = NA, size = 0.01) +
  scale_fill_gradientn(colours =  pal, limits=c(0, 4), 
                       labels = c("Almost Normal", "Mild Drought", "Moderate Drought", "Grave Drought", "Severe Drought")) + 
  labs(fill = " ") + guides(fill=guide_legend(title="Intensity")) + 
  labs(x = " ", 
       y = " ", 
       title = " ", 
       caption = "2002") + guides(color=guide_legend(title = " ")) + 
  geom_sf(data=DNE4, aes(color = NE), fill = "transparent", color= "black", size=0.7) +
  geom_sf(data=DNE3, aes(color = NE), fill = "transparent", color= "black", size=0.7) +
  guides(color=guide_legend(title = " ")) + 
  theme(plot.caption = element_text(hjust = 0.5, size = 12, face = "bold"))


CSDI2003 <- subset(DSDI, DSDI$ANO=="2003")
CSDI2003 <- rename(CSDI2003, CSDI2003 = CSDI)
CSDI2003$ANO = NULL
DNE1 = merge(DNE1, CSDI2003, by = c("CODIGO_MUN"), all.x = T)


GCSDI2003 = ggplot(data=DNE1) +  geom_sf(aes(fill = CSDI2003), colour = NA, size = 0.01) +
  scale_fill_gradientn(colours =  pal, limits=c(0, 4), 
                       labels = c("Almost Normal", "Mild Drought", "Moderate Drought", "Grave Drought", "Severe Drought")) + 
  labs(fill = " ") + guides(fill=guide_legend(title="Intensity")) + 
  labs(x = " ", 
       y = " ", 
       title = " ", 
       caption = "2003") + guides(color=guide_legend(title = " ")) + 
  geom_sf(data=DNE4, aes(color = NE), fill = "transparent", color= "black", size=0.7) +
  geom_sf(data=DNE3, aes(color = NE), fill = "transparent", color= "black", size=0.7) +
  guides(color=guide_legend(title = " ")) + 
  theme(plot.caption = element_text(hjust = 0.5, size = 12, face = "bold"))


CSDI2004 <- subset(DSDI, DSDI$ANO=="2004")
CSDI2004 <- rename(CSDI2004, CSDI2004 = CSDI)
CSDI2004$ANO = NULL
DNE1 = merge(DNE1, CSDI2004, by = c("CODIGO_MUN"), all.x = T)


GCSDI2004 = ggplot(data=DNE1) +  geom_sf(aes(fill = CSDI2004), colour = NA, size = 0.01) +
  scale_fill_gradientn(colours =  pal, limits=c(0, 4), 
                       labels = c("Almost Normal", "Mild Drought", "Moderate Drought", "Grave Drought", "Severe Drought")) + 
  labs(fill = " ") + guides(fill=guide_legend(title="Intensity")) + 
  labs(x = " ", 
       y = " ", 
       title = " ", 
       caption = "2004") + guides(color=guide_legend(title = " ")) + 
  geom_sf(data=DNE4, aes(color = NE), fill = "transparent", color= "black", size=0.7) +
  geom_sf(data=DNE3, aes(color = NE), fill = "transparent", color= "black", size=0.7) +
  guides(color=guide_legend(title = " ")) + 
  theme(plot.caption = element_text(hjust = 0.5, size = 12, face = "bold"))


CSDI2005 <- subset(DSDI, DSDI$ANO=="2005")
CSDI2005 <- rename(CSDI2005, CSDI2005 = CSDI)
CSDI2005$ANO = NULL
DNE1 = merge(DNE1, CSDI2005, by = c("CODIGO_MUN"), all.x = T)


GCSDI2005 = ggplot(data=DNE1) +  geom_sf(aes(fill = CSDI2005), colour = NA, size = 0.01) +
  scale_fill_gradientn(colours =  pal, limits=c(0, 4), 
                       labels = c("Almost Normal", "Mild Drought", "Moderate Drought", "Grave Drought", "Severe Drought")) + 
  labs(fill = " ") + guides(fill=guide_legend(title="Intensity")) + 
  labs(x = " ", 
       y = " ", 
       title = " ", 
       caption = "2005") + guides(color=guide_legend(title = " ")) + 
  geom_sf(data=DNE4, aes(color = NE), fill = "transparent", color= "black", size=0.7) +
  geom_sf(data=DNE3, aes(color = NE), fill = "transparent", color= "black", size=0.7) +
  guides(color=guide_legend(title = " ")) + 
  theme(plot.caption = element_text(hjust = 0.5, size = 12, face = "bold"))


CSDI2006 <- subset(DSDI, DSDI$ANO=="2006")
CSDI2006 <- rename(CSDI2006, CSDI2006 = CSDI)
CSDI2006$ANO = NULL
DNE1 = merge(DNE1, CSDI2006, by = c("CODIGO_MUN"), all.x = T)


GCSDI2006 = ggplot(data=DNE1) +  geom_sf(aes(fill = CSDI2006), colour = NA, size = 0.01) +
  scale_fill_gradientn(colours =  pal, limits=c(0, 4), 
                       labels = c("Almost Normal", "Mild Drought", "Moderate Drought", "Grave Drought", "Severe Drought")) + 
  labs(fill = " ") + guides(fill=guide_legend(title="Intensity")) + 
  labs(x = " ", 
       y = " ", 
       title = " ", 
       caption = "2006") + guides(color=guide_legend(title = " ")) + 
  geom_sf(data=DNE4, aes(color = NE), fill = "transparent", color= "black", size=0.7) +
  geom_sf(data=DNE3, aes(color = NE), fill = "transparent", color= "black", size=0.7) +
  guides(color=guide_legend(title = " ")) + 
  theme(plot.caption = element_text(hjust = 0.5, size = 12, face = "bold"))


CSDI2007 <- subset(DSDI, DSDI$ANO=="2007")
CSDI2007 <- rename(CSDI2007, CSDI2007 = CSDI)
CSDI2007$ANO = NULL
DNE1 = merge(DNE1, CSDI2007, by = c("CODIGO_MUN"), all.x = T)


GCSDI2007 = ggplot(data=DNE1) +  geom_sf(aes(fill = CSDI2007), colour = NA, size = 0.01) +
  scale_fill_gradientn(colours =  pal, limits=c(0, 4), 
                       labels = c("Almost Normal", "Mild Drought", "Moderate Drought", "Grave Drought", "Severe Drought")) + 
  labs(fill = " ") + guides(fill=guide_legend(title="Intensity")) + 
  labs(x = " ", 
       y = " ", 
       title = " ", 
       caption = "2007") + guides(color=guide_legend(title = " ")) + 
  geom_sf(data=DNE4, aes(color = NE), fill = "transparent", color= "black", size=0.7) +
  geom_sf(data=DNE3, aes(color = NE), fill = "transparent", color= "black", size=0.7) +
  guides(color=guide_legend(title = " ")) + 
  theme(plot.caption = element_text(hjust = 0.5, size = 12, face = "bold"))

CSDI2008 <- subset(DSDI, DSDI$ANO=="2008")
CSDI2008 <- rename(CSDI2008, CSDI2008 = CSDI)
CSDI2008$ANO = NULL
DNE1 = merge(DNE1, CSDI2008, by = c("CODIGO_MUN"), all.x = T)


GCSDI2008 = ggplot(data=DNE1) +  geom_sf(aes(fill = CSDI2008), colour = NA, size = 0.01) +
  scale_fill_gradientn(colours =  pal, limits=c(0, 4), 
                       labels = c("Almost Normal", "Mild Drought", "Moderate Drought", "Grave Drought", "Severe Drought")) + 
  labs(fill = " ") + guides(fill=guide_legend(title="Intensity")) + 
  labs(x = " ", 
       y = " ", 
       title = " ", 
       caption = "2008") + guides(color=guide_legend(title = " ")) + 
  geom_sf(data=DNE4, aes(color = NE), fill = "transparent", color= "black", size=0.7) +
  geom_sf(data=DNE3, aes(color = NE), fill = "transparent", color= "black", size=0.7) +
  guides(color=guide_legend(title = " ")) + 
  theme(plot.caption = element_text(hjust = 0.5, size = 12, face = "bold"))


CSDI2009 <- subset(DSDI, DSDI$ANO=="2009")
CSDI2009 <- rename(CSDI2009, CSDI2009 = CSDI)
CSDI2009$ANO = NULL
DNE1 = merge(DNE1, CSDI2009, by = c("CODIGO_MUN"), all.x = T)

GCSDI2009 = ggplot(data=DNE1) +  geom_sf(aes(fill = CSDI2009), colour = NA, size = 0.01) +
  scale_fill_gradientn(colours =  pal, limits=c(0, 4), 
                       labels = c("Almost Normal", "Mild Drought", "Moderate Drought", "Grave Drought", "Severe Drought")) + 
  labs(fill = " ") + guides(fill=guide_legend(title="Intensity")) + 
  labs(x = " ", 
       y = " ", 
       title = " ", 
       caption = "2009") + guides(color=guide_legend(title = " ")) + 
  geom_sf(data=DNE4, aes(color = NE), fill = "transparent", color= "black", size=0.7) +
  geom_sf(data=DNE3, aes(color = NE), fill = "transparent", color= "black", size=0.7) +
  guides(color=guide_legend(title = " ")) + 
  theme(plot.caption = element_text(hjust = 0.5, size = 12, face = "bold"))


CSDI2010 <- subset(DSDI, DSDI$ANO=="2010")
CSDI2010 <- rename(CSDI2010, CSDI2010 = CSDI)
CSDI2010$ANO = NULL
DNE1 = merge(DNE1, CSDI2010, by = c("CODIGO_MUN"), all.x = T)


GCSDI2010 = ggplot(data=DNE1) +  geom_sf(aes(fill = CSDI2010), colour = NA, size = 0.01) +
  scale_fill_gradientn(colours =  pal, limits=c(0, 4), 
                       labels = c("Almost Normal", "Mild Drought", "Moderate Drought", "Grave Drought", "Severe Drought")) + 
  labs(fill = " ") + guides(fill=guide_legend(title="Intensity")) + 
  labs(x = " ", 
       y = " ", 
       title = " ", 
       caption = "2010") + guides(color=guide_legend(title = " ")) + 
  geom_sf(data=DNE4, aes(color = NE), fill = "transparent", color= "black", size=0.7) +
  geom_sf(data=DNE3, aes(color = NE), fill = "transparent", color= "black", size=0.7) +
  guides(color=guide_legend(title = " ")) + 
  theme(plot.caption = element_text(hjust = 0.5, size = 12, face = "bold"))

CSDI2011 <- subset(DSDI, DSDI$ANO=="2011")
CSDI2011 <- rename(CSDI2011, CSDI2011 = CSDI)
CSDI2011$ANO = NULL
DNE1 = merge(DNE1, CSDI2011, by = c("CODIGO_MUN"), all.x = T)

GCSDI2011 = ggplot(data=DNE1) +  geom_sf(aes(fill = CSDI2011), colour = NA, size = 0.01) +
  scale_fill_gradientn(colours =  pal, limits=c(0, 4), 
                       labels = c("Almost Normal", "Mild Drought", "Moderate Drought", "Grave Drought", "Severe Drought")) + 
  labs(fill = " ") + guides(fill=guide_legend(title="Intensity")) + 
  labs(x = " ", 
       y = " ", 
       title = " ", 
       caption = "2011") + guides(color=guide_legend(title = " ")) + 
  geom_sf(data=DNE4, aes(color = NE), fill = "transparent", color= "black", size=0.7) +
  geom_sf(data=DNE3, aes(color = NE), fill = "transparent", color= "black", size=0.7) +
  guides(color=guide_legend(title = " ")) + 
  theme(plot.caption = element_text(hjust = 0.5, size = 12, face = "bold"))


CSDI2012 <- subset(DSDI, DSDI$ANO=="2012")
CSDI2012 <- rename(CSDI2012, CSDI2012 = CSDI)
CSDI2012$ANO = NULL
DNE1 = merge(DNE1, CSDI2012, by = c("CODIGO_MUN"), all.x = T)


GCSDI2012 = ggplot(data=DNE1) +  geom_sf(aes(fill = CSDI2012), colour = NA, size = 0.01) +
  scale_fill_gradientn(colours =  pal, limits=c(0, 4), 
                       labels = c("Almost Normal", "Mild Drought", "Moderate Drought", "Grave Drought", "Severe Drought")) + 
  labs(fill = " ") + guides(fill=guide_legend(title="Intensity")) + 
  labs(x = " ", 
       y = " ", 
       title = " ", 
       caption = "2012") + guides(color=guide_legend(title = " ")) + 
  geom_sf(data=DNE4, aes(color = NE), fill = "transparent", color= "black", size=0.7) +
  geom_sf(data=DNE3, aes(color = NE), fill = "transparent", color= "black", size=0.7) +
  guides(color=guide_legend(title = " ")) + 
  theme(plot.caption = element_text(hjust = 0.5, size = 12, face = "bold"))

CSDI2013 <- subset(DSDI, DSDI$ANO=="2013")
CSDI2013 <- rename(CSDI2013, CSDI2013 = CSDI)
CSDI2013$ANO = NULL
DNE1 = merge(DNE1, CSDI2013, by = c("CODIGO_MUN"), all.x = T)


GCSDI2013 = ggplot(data=DNE1) +  geom_sf(aes(fill = CSDI2013), colour = NA, size = 0.01) +
  scale_fill_gradientn(colours =  pal, limits=c(0, 4), 
                       labels = c("Almost Normal", "Mild Drought", "Moderate Drought", "Grave Drought", "Severe Drought")) + 
  labs(fill = " ") + guides(fill=guide_legend(title="Intensity")) + 
  labs(x = " ", 
       y = " ", 
       title = " ", 
       caption = "2013") + guides(color=guide_legend(title = " ")) + 
  geom_sf(data=DNE4, aes(color = NE), fill = "transparent", color= "black", size=0.7) +
  geom_sf(data=DNE3, aes(color = NE), fill = "transparent", color= "black", size=0.7) +
  guides(color=guide_legend(title = " ")) + 
  theme(plot.caption = element_text(hjust = 0.5, size = 12, face = "bold"))

CSDI2014 <- subset(DSDI, DSDI$ANO=="2014")
CSDI2014 <- rename(CSDI2014, CSDI2014 = CSDI)
CSDI2014$ANO = NULL
DNE1 = merge(DNE1, CSDI2014, by = c("CODIGO_MUN"), all.x = T)


GCSDI2014 = ggplot(data=DNE1) +  geom_sf(aes(fill = CSDI2014), colour = NA, size = 0.01) +
  scale_fill_gradientn(colours =  pal, limits=c(0, 4), 
                       labels = c("Almost Normal", "Mild Drought", "Moderate Drought", "Grave Drought", "Severe Drought")) + 
  labs(fill = " ") + guides(fill=guide_legend(title="Intensity")) + 
  labs(x = " ", 
       y = " ", 
       title = " ", 
       caption = "2014") + guides(color=guide_legend(title = " ")) + 
  geom_sf(data=DNE4, aes(color = NE), fill = "transparent", color= "black", size=0.7) +
  geom_sf(data=DNE3, aes(color = NE), fill = "transparent", color= "black", size=0.7) +
  guides(color=guide_legend(title = " ")) + 
  theme(plot.caption = element_text(hjust = 0.5, size = 12, face = "bold"))

CSDI2015 <- subset(DSDI, DSDI$ANO=="2015")
CSDI2015 <- rename(CSDI2015, CSDI2015 = CSDI)
CSDI2015$ANO = NULL
DNE1 = merge(DNE1, CSDI2015, by = c("CODIGO_MUN"), all.x = T)


GCSDI2015 = ggplot(data=DNE1) +  geom_sf(aes(fill = CSDI2015), colour = NA, size = 0.01) +
  scale_fill_gradientn(colours =  pal, limits=c(0, 4), 
                       labels = c("Almost Normal", "Mild Drought", "Moderate Drought", "Grave Drought", "Severe Drought")) + 
  labs(fill = " ") + guides(fill=guide_legend(title="Intensity")) + 
  labs(x = " ", 
       y = " ", 
       title = " ", 
       caption = "2015") + guides(color=guide_legend(title = " ")) + 
  geom_sf(data=DNE4, aes(color = NE), fill = "transparent", color= "black", size=0.7) +
  geom_sf(data=DNE3, aes(color = NE), fill = "transparent", color= "black", size=0.7) +
  guides(color=guide_legend(title = " ")) + 
  theme(plot.caption = element_text(hjust = 0.5, size = 12, face = "bold"))

CSDI2016 <- subset(DSDI, DSDI$ANO=="2016")
CSDI2016 <- rename(CSDI2016, CSDI2016 = CSDI)
CSDI2016$ANO = NULL
DNE1 = merge(DNE1, CSDI2016, by = c("CODIGO_MUN"), all.x = T)


GCSDI2016 = ggplot(data=DNE1) +  geom_sf(aes(fill = CSDI2016), colour = NA, size = 0.01) +
  scale_fill_gradientn(colours =  pal, limits=c(0, 4), 
                       labels = c("Almost Normal", "Mild Drought", "Moderate Drought", "Grave Drought", "Severe Drought")) + 
  labs(fill = " ") + guides(fill=guide_legend(title="Intensity")) + 
  labs(x = " ", 
       y = " ", 
       title = " ", 
       caption = "2016") + guides(color=guide_legend(title = " ")) + 
  geom_sf(data=DNE4, aes(color = NE), fill = "transparent", color= "black", size=0.7) +
  geom_sf(data=DNE3, aes(color = NE), fill = "transparent", color= "black", size=0.7) +
  guides(color=guide_legend(title = " ")) + 
  theme(plot.caption = element_text(hjust = 0.5, size = 12, face = "bold"))

CSDI2017 <- subset(DSDI, DSDI$ANO=="2017")
CSDI2017 <- rename(CSDI2017, CSDI2017 = CSDI)
CSDI2017$ANO = NULL
DNE1 = merge(DNE1, CSDI2017, by = c("CODIGO_MUN"), all.x = T)


GCSDI2017 = ggplot(data=DNE1) +  geom_sf(aes(fill = CSDI2017), colour = NA, size = 0.01) +
  scale_fill_gradientn(colours =  pal, limits=c(0, 4), 
                       labels = c("Almost Normal", "Mild Drought", "Moderate Drought", "Grave Drought", "Severe Drought")) + 
  labs(fill = " ") + guides(fill=guide_legend(title="Intensity")) + 
  labs(x = " ", 
       y = " ", 
       title = " ", 
       caption = "2017") + guides(color=guide_legend(title = " ")) + 
  geom_sf(data=DNE4, aes(color = NE), fill = "transparent", color= "black", size=0.7) +
  geom_sf(data=DNE3, aes(color = NE), fill = "transparent", color= "black", size=0.7) +
  guides(color=guide_legend(title = " ")) + 
  theme(plot.caption = element_text(hjust = 0.5, size = 12, face = "bold"))

CSDI2018 <- subset(DSDI, DSDI$ANO=="2018")
CSDI2018 <- rename(CSDI2018, CSDI2018 = CSDI)
CSDI2018$ANO = NULL
DNE1 = merge(DNE1, CSDI2018, by = c("CODIGO_MUN"), all.x = T)


GCSDI2018 = ggplot(data=DNE1) +  geom_sf(aes(fill = CSDI2018), colour = NA, size = 0.01) +
  scale_fill_gradientn(colours =  pal, limits=c(0, 4), 
                       labels = c("Almost Normal", "Mild Drought", "Moderate Drought", "Grave Drought", "Severe Drought")) + 
  labs(fill = " ") + guides(fill=guide_legend(title="Intensity")) + 
  labs(x = " ", 
       y = " ", 
       title = " ", 
       caption = "2018") + guides(color=guide_legend(title = " ")) + 
  geom_sf(data=DNE4, aes(color = NE), fill = "transparent", color= "black", size=0.7) +
  geom_sf(data=DNE3, aes(color = NE), fill = "transparent", color= "black", size=0.7) +
  guides(color=guide_legend(title = " ")) + 
  theme(plot.caption = element_text(hjust = 0.5, size = 12, face = "bold"))

CSDI2019 <- subset(DSDI, DSDI$ANO=="2019")
CSDI2019 <- rename(CSDI2019, CSDI2019 = CSDI)
CSDI2019$ANO = NULL
DNE1 = merge(DNE1, CSDI2019, by = c("CODIGO_MUN"), all.x = T)


GCSDI2019 = ggplot(data=DNE1) +  geom_sf(aes(fill = CSDI2019), colour = NA, size = 0.01) +
  scale_fill_gradientn(colours =  pal, limits=c(0, 4), 
                       labels = c("Almost Normal", "Mild Drought", "Moderate Drought", "Grave Drought", "Severe Drought")) + 
  labs(fill = " ") + guides(fill=guide_legend(title="Intensity")) + 
  labs(x = " ", 
       y = " ", 
       title = " ", 
       caption = "2019") + guides(color=guide_legend(title = " ")) + 
  geom_sf(data=DNE4, aes(color = NE), fill = "transparent", color= "black", size=0.7) +
  geom_sf(data=DNE3, aes(color = NE), fill = "transparent", color= "black", size=0.7) +
  guides(color=guide_legend(title = " ")) + 
  theme(plot.caption = element_text(hjust = 0.5, size = 12, face = "bold"))

CSDI2020 <- subset(DSDI, DSDI$ANO=="2020")
CSDI2020 <- rename(CSDI2020, CSDI2020 = CSDI)
CSDI2020$ANO = NULL
DNE1 = merge(DNE1, CSDI2020, by = c("CODIGO_MUN"), all.x = T)

GCSDI2020 = ggplot(data=DNE1) +  geom_sf(aes(fill = CSDI2020), colour = NA, size = 0.01) +
  scale_fill_gradientn(colours =  pal, limits=c(0, 4), 
                       labels = c("Almost Normal", "Mild Drought", "Moderate Drought", "Grave Drought", "Severe Drought")) + 
  labs(fill = " ") + guides(fill=guide_legend(title="Intensity")) + 
  labs(x = " ", 
       y = " ", 
       title = " ", 
       caption = "2020") + guides(color=guide_legend(title = " ")) + 
  geom_sf(data=DNE4, aes(color = NE), fill = "transparent", color= "black", size=0.7) +
  geom_sf(data=DNE3, aes(color = NE), fill = "transparent", color= "black", size=0.7) +
  guides(color=guide_legend(title = " ")) + 
  theme(plot.caption = element_text(hjust = 0.5, size = 12, face = "bold"))


# Exportando mapas de 1981-2020

grid.arrange (GCSDI2000, GCSDI2001, GCSDI2002, GCSDI2003,
              GCSDI2004, GCSDI2005, GCSDI2006, GCSDI2007,
              GCSDI2008, GCSDI2009, GCSDI2010,GCSDI2011,  ncol= 3)

grid.arrange (GCSDI2012, GCSDI2013, GCSDI2014,
              GCSDI2015, GCSDI2016, GCSDI2017, GCSDI2018,
              GCSDI2019, GCSDI2020,  GCSDI2000, GCSDI2001, GCSDI2002,
              ncol= 3)

