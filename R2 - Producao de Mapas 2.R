########################################################################################################################
# Rotina {R} : Producao de Mapas 
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
library("ggspatial")
library("geobr")

# Definindo Diretório 

setwd("D:/Microdados de Agricultura/Data_Tratado/Nordeste/Analise Espacial")
dir()

# Importando Dados 

NE  <- read_stata("Data_Mapa.dta")
UF <- c("AL", "BA", "CE", "PB", "PE", "MA", "RN", "SE", "PI")

# Recortando Shapefile p/ Municípios do Nordeste 

BRA <- read_municipality(code_muni = "all", year=2019)
MNE <- subset(BRA, name_region =="Nordeste")
NE  <- rename(NE, code_muni = codibge)

NE2000 <- subset(NE, NE$ano=="2000")
MNE = merge(MNE, NE2000, by = c("code_muni"), all.x = T)

### Delimitações regionais 

# Delimitação regional do "estado" e "região"

DELUF <- fortify(model=BRA, region="code_state")
DELSEM <- fortify(model=BRA, region="semiarido")

########################################################################################################################
# PRODUCAO DE GRAFICOS DE DIVERSIFICAÇÃO                                                                               #                 
########################################################################################################################

# Figura dos Clubes de Convergência 

Figura1 <- ggplot(MNE) + 
  geom_sf(aes(fill = as.factor(clubs)), colour=NA, size=0.2)  + 
  scale_fill_manual(values = c("#0000CD", "#ff0000"), breaks = c("club1", "club2"), labels=c("Club 1", "Club 2")) + 
  guides(fill=guide_legend(title="Convergence")) + 
  annotation_north_arrow(location = "tr", which_north = "true", pad_x = unit(0.1, "in"), pad_y = unit(0.1, "in"),
                         style = north_arrow_nautical(fill = c("grey40", "white"), line_col = "grey20",
                                                      text_family = "ArcherPro Book")) + 
  annotation_scale(location = "br", height = unit (0.10, "cm"), which_north = "true") +
  labs(x = " ",  y = " ", title = " ", subtitle = " ", caption = " ") + 
  guides(color=guide_legend(title = " ")) +
  geom_sf(data=DELSEM, aes(color = semiaridp), fill = "transparent", size=1) +
  geom_sf(data=DELUF, aes(color = code_state), fill = "transparent", color= "black", size=0.7) +
  scale_color_manual(values = c("red", "black"), breaks = c("Sim"), labels = c("Northeast Semiarid")) + 
  theme(plot.caption = element_text(hjust = 0.5, size = 12, face = "bold"))

# Figuras do MSI de 2000, 2005, 2010, 2015, 2020 

CSI2000 <- subset(NE, NE$ANO=="2000")
CSI2000 <- rename(CSI2000, SI2000 = SI)

DNE1 = merge(DNE1, CSI2000, by = c("CODIGO_MUN"), all.x = T)

GCSI2000 = ggplot(data=DNE1) +  geom_sf(aes(fill = SI2000), colour = NA, size = 0.01) +
  scale_fill_gradient(low = "#FFAE00", high = "#00963D" , breaks=c(0, 0.35, 0.65, 1), limits=c(0,1),
                      labels=c("Very specialized", "Specialized", "Diversified", "Very diversified"),
                      na.value = "black") + 
  geom_area(inherit.aes = F, mapping = aes(x = -50, y =-20, color = "No production"), data = data.frame()) +
  scale_colour_manual(name = element_blank(), values="black")  + 
  guides(fill=guide_legend(title="SDI")) + 
  labs(x = " ", 
       y = " ", 
       title = " ", 
       caption = "2000") + guides(color=guide_legend(title = " ")) + 
  geom_sf(data=DNE4, aes(color = NE), fill = "transparent", colour= "black", size=0.7) +
  geom_sf(data=DNE3, aes(color = NE), fill = "transparent", colour= "black", size=0.7) + 
  guides(color=guide_legend(title = " ")) + 
  theme(plot.caption = element_text(hjust = 0.5, size = 12, face = "bold"))
        
        CSI2001 <- subset(NE, NE$ANO=="2001")
        CSI2001 <- rename(CSI2001, SI2001 = SI)
        
        DNE1 = merge(DNE1, CSI2001, by = c("CODIGO_MUN"), all.x = T)
        
        GCSI2001 = ggplot(data=DNE1) +  geom_sf(aes(fill = SI2001), colour = NA, size = 0.01) +
          scale_fill_gradient(low = "#FFAE00", high = "#00963D" , breaks=c(0, 0.35, 0.65, 1), limits=c(0,1),
                              labels=c("Very specialized", "Specialized", "Diversified", "Very diversified"),
                              na.value = "black") + 
          geom_area(inherit.aes = F, mapping = aes(x = -50, y =-20, color = "No production"), data = data.frame()) +
          scale_colour_manual(name = element_blank(), values="black")  + 
          guides(fill=guide_legend(title="SDI")) + 
          labs(x = " ", 
               y = " ", 
               title = " ", 
               caption = "2001") + guides(color=guide_legend(title = " ")) + 
          geom_sf(data=DNE4, aes(color = NE), fill = "transparent", colour= "black", size=0.7) +
          geom_sf(data=DNE3, aes(color = NE), fill = "transparent", colour= "black", size=0.7) + 
          guides(color=guide_legend(title = " ")) + 
          theme(plot.caption = element_text(hjust = 0.5, size = 12, face = "bold"))
        
        CSI2002 <- subset(NE, NE$ANO=="2002")
        CSI2002 <- rename(CSI2002, SI2002 = SI)
        
        DNE1 = merge(DNE1, CSI2002, by = c("CODIGO_MUN"), all.x = T)
        
        GCSI2002 = ggplot(data=DNE1) +  geom_sf(aes(fill = SI2002), colour = NA, size = 0.01) +
          scale_fill_gradient(low = "#FFAE00", high = "#00963D" , breaks=c(0, 0.35, 0.65, 1), limits=c(0,1),
                              labels=c("Very specialized", "Specialized", "Diversified", "Very diversified"),
                              na.value = "black") + 
          geom_area(inherit.aes = F, mapping = aes(x = -50, y =-20, color = "No production"), data = data.frame()) +
          scale_colour_manual(name = element_blank(), values="black")  + 
          guides(fill=guide_legend(title="SDI")) + 
          labs(x = " ", 
               y = " ", 
               title = " ", 
               caption = "2002") + guides(color=guide_legend(title = " ")) + 
          geom_sf(data=DNE4, aes(color = NE), fill = "transparent", colour= "black", size=0.7) +
          geom_sf(data=DNE3, aes(color = NE), fill = "transparent", colour= "black", size=0.7) + 
          guides(color=guide_legend(title = " ")) + 
          theme(plot.caption = element_text(hjust = 0.5, size = 12, face = "bold"))
        
        CSI2003 <- subset(NE, NE$ANO=="2003")
        CSI2003 <- rename(CSI2003, SI2003 = SI)
        
        DNE1 = merge(DNE1, CSI2003, by = c("CODIGO_MUN"), all.x = T)
        
        GCSI2003 = ggplot(data=DNE1) +  geom_sf(aes(fill = SI2003), colour = NA, size = 0.01) +
          scale_fill_gradient(low = "#FFAE00", high = "#00963D" , breaks=c(0, 0.35, 0.65, 1), limits=c(0,1),
                              labels=c("Very specialized", "Specialized", "Diversified", "Very diversified"),
                              na.value = "black") + 
          geom_area(inherit.aes = F, mapping = aes(x = -50, y =-20, color = "No production"), data = data.frame()) +
          scale_colour_manual(name = element_blank(), values="black")  + 
          guides(fill=guide_legend(title="SDI")) + 
          labs(x = " ", 
               y = " ", 
               title = " ", 
               caption = "2003") + guides(color=guide_legend(title = " ")) + 
          geom_sf(data=DNE4, aes(color = NE), fill = "transparent", colour= "black", size=0.7) +
          geom_sf(data=DNE3, aes(color = NE), fill = "transparent", colour= "black", size=0.7) + 
          guides(color=guide_legend(title = " ")) + 
          theme(plot.caption = element_text(hjust = 0.5, size = 12, face = "bold"))
        
        CSI2004 <- subset(NE, NE$ANO=="2004")
        CSI2004 <- rename(CSI2004, SI2004 = SI)
        
        DNE1 = merge(DNE1, CSI2004, by = c("CODIGO_MUN"), all.x = T)
        
        GCSI2004 = ggplot(data=DNE1) +  geom_sf(aes(fill = SI2004), colour = NA, size = 0.01) +
          scale_fill_gradient(low = "#FFAE00", high = "#00963D" , breaks=c(0, 0.35, 0.65, 1), limits=c(0,1),
                              labels=c("Very specialized", "Specialized", "Diversified", "Very diversified"),
                              na.value = "black") + 
          geom_area(inherit.aes = F, mapping = aes(x = -50, y =-20, color = "No production"), data = data.frame()) +
          scale_colour_manual(name = element_blank(), values="black")  + 
          guides(fill=guide_legend(title="SDI")) + 
          labs(x = " ", 
               y = " ", 
               title = " ", 
               caption = "2004") + guides(color=guide_legend(title = " ")) + 
          geom_sf(data=DNE4, aes(color = NE), fill = "transparent", colour= "black", size=0.7) +
          geom_sf(data=DNE3, aes(color = NE), fill = "transparent", colour= "black", size=0.7) + 
          guides(color=guide_legend(title = " ")) + 
          theme(plot.caption = element_text(hjust = 0.5, size = 12, face = "bold"))
        
        CSI2005 <- subset(NE, NE$ANO=="2005")
        CSI2005 <- rename(CSI2005, SI2005 = SI)
        
        DNE1 = merge(DNE1, CSI2005, by = c("CODIGO_MUN"), all.x = T)
        
        GCSI2005 = ggplot(data=DNE1) +  geom_sf(aes(fill = SI2005), colour = NA, size = 0.01) +
          scale_fill_gradient(low = "#FFAE00", high = "#00963D" , breaks=c(0, 0.35, 0.65, 1), limits=c(0,1),
                              labels=c("Very specialized", "Specialized", "Diversified", "Very diversified"),
                              na.value = "black") + 
          geom_area(inherit.aes = F, mapping = aes(x = -50, y =-20, color = "No production"), data = data.frame()) +
          scale_colour_manual(name = element_blank(), values="black")  + 
          guides(fill=guide_legend(title="SDI")) + 
          labs(x = " ", 
               y = " ", 
               title = " ", 
               caption = "2005") + guides(color=guide_legend(title = " ")) + 
          geom_sf(data=DNE4, aes(color = NE), fill = "transparent", colour= "black", size=0.7) +
          geom_sf(data=DNE3, aes(color = NE), fill = "transparent", colour= "black", size=0.7) + 
          guides(color=guide_legend(title = " ")) + 
          theme(plot.caption = element_text(hjust = 0.5, size = 12, face = "bold"))
        
        CSI2006 <- subset(NE, NE$ANO=="2006")
        CSI2006 <- rename(CSI2006, SI2006 = SI)
        
        DNE1 = merge(DNE1, CSI2006, by = c("CODIGO_MUN"), all.x = T)
        
        GCSI2006 = ggplot(data=DNE1) +  geom_sf(aes(fill = SI2006), colour = NA, size = 0.01) +
          scale_fill_gradient(low = "#FFAE00", high = "#00963D" , breaks=c(0, 0.35, 0.65, 1), limits=c(0,1),
                              labels=c("Very specialized", "Specialized", "Diversified", "Very diversified"),
                              na.value = "black") + 
          geom_area(inherit.aes = F, mapping = aes(x = -50, y =-20, color = "No production"), data = data.frame()) +
          scale_colour_manual(name = element_blank(), values="black")  + 
          guides(fill=guide_legend(title="SDI")) + 
          labs(x = " ", 
               y = " ", 
               title = " ", 
               caption = "2006") + guides(color=guide_legend(title = " ")) + 
          geom_sf(data=DNE4, aes(color = NE), fill = "transparent", colour= "black", size=0.7) +
          geom_sf(data=DNE3, aes(color = NE), fill = "transparent", colour= "black", size=0.7) + 
          guides(color=guide_legend(title = " ")) + 
          theme(plot.caption = element_text(hjust = 0.5, size = 12, face = "bold"))
        
        CSI2007 <- subset(NE, NE$ANO=="2007")
        CSI2007 <- rename(CSI2007, SI2007 = SI)
        
        DNE1 = merge(DNE1, CSI2007, by = c("CODIGO_MUN"), all.x = T)
        
        GCSI2007 = ggplot(data=DNE1) +  geom_sf(aes(fill = SI2007), colour = NA, size = 0.01) +
          scale_fill_gradient(low = "#FFAE00", high = "#00963D" , breaks=c(0, 0.35, 0.65, 1), limits=c(0,1),
                              labels=c("Very specialized", "Specialized", "Diversified", "Very diversified"),
                              na.value = "black") + 
          geom_area(inherit.aes = F, mapping = aes(x = -50, y =-20, color = "No production"), data = data.frame()) +
          scale_colour_manual(name = element_blank(), values="black")  + 
          guides(fill=guide_legend(title="SDI")) + 
          labs(x = " ", 
               y = " ", 
               title = " ", 
               caption = "2007") + guides(color=guide_legend(title = " ")) + 
          geom_sf(data=DNE4, aes(color = NE), fill = "transparent", colour= "black", size=0.7) +
          geom_sf(data=DNE3, aes(color = NE), fill = "transparent", colour= "black", size=0.7) + 
          guides(color=guide_legend(title = " ")) + 
          theme(plot.caption = element_text(hjust = 0.5, size = 12, face = "bold"))
        
        CSI2008 <- subset(NE, NE$ANO=="2008")
        CSI2008 <- rename(CSI2008, SI2008 = SI)
        
        DNE1 = merge(DNE1, CSI2008, by = c("CODIGO_MUN"), all.x = T)
        
        GCSI2008 = ggplot(data=DNE1) +  geom_sf(aes(fill = SI2008), colour = NA, size = 0.01) +
          scale_fill_gradient(low = "#FFAE00", high = "#00963D" , breaks=c(0, 0.35, 0.65, 1), limits=c(0,1),
                              labels=c("Very specialized", "Specialized", "Diversified", "Very diversified"),
                              na.value = "black") + 
          geom_area(inherit.aes = F, mapping = aes(x = -50, y =-20, color = "No production"), data = data.frame()) +
          scale_colour_manual(name = element_blank(), values="black")  + 
          guides(fill=guide_legend(title="SDI")) + 
          labs(x = " ", 
               y = " ", 
               title = " ", 
               caption = "2008") + guides(color=guide_legend(title = " ")) + 
          geom_sf(data=DNE4, aes(color = NE), fill = "transparent", colour= "black", size=0.7) +
          geom_sf(data=DNE3, aes(color = NE), fill = "transparent", colour= "black", size=0.7) + 
          guides(color=guide_legend(title = " ")) + 
          theme(plot.caption = element_text(hjust = 0.5, size = 12, face = "bold"))
        
        CSI2009 <- subset(NE, NE$ANO=="2009")
        CSI2009 <- rename(CSI2009, SI2009 = SI)
        
        DNE1 = merge(DNE1, CSI2009, by = c("CODIGO_MUN"), all.x = T)
        
        GCSI2009 = ggplot(data=DNE1) +  geom_sf(aes(fill = SI2009), colour = NA, size = 0.01) +
          scale_fill_gradient(low = "#FFAE00", high = "#00963D" , breaks=c(0, 0.35, 0.65, 1), limits=c(0,1),
                              labels=c("Very specialized", "Specialized", "Diversified", "Very diversified"),
                              na.value = "black") + 
          geom_area(inherit.aes = F, mapping = aes(x = -50, y =-20, color = "No production"), data = data.frame()) +
          scale_colour_manual(name = element_blank(), values="black")  + 
          guides(fill=guide_legend(title="SDI")) + 
          labs(x = " ", 
               y = " ", 
               title = " ", 
               caption = "2009") + guides(color=guide_legend(title = " ")) + 
          geom_sf(data=DNE4, aes(color = NE), fill = "transparent", colour= "black", size=0.7) +
          geom_sf(data=DNE3, aes(color = NE), fill = "transparent", colour= "black", size=0.7) + 
          guides(color=guide_legend(title = " ")) + 
          theme(plot.caption = element_text(hjust = 0.5, size = 12, face = "bold"))
        
        CSI2010 <- subset(NE, NE$ANO=="2010")
        CSI2010 <- rename(CSI2010, SI2010 = SI)
        
        DNE1 = merge(DNE1, CSI2010, by = c("CODIGO_MUN"), all.x = T)
        
        GCSI2010 = ggplot(data=DNE1) +  geom_sf(aes(fill = SI2010), colour = NA, size = 0.01) +
          scale_fill_gradient(low = "#FFAE00", high = "#00963D" , breaks=c(0, 0.35, 0.65, 1), limits=c(0,1),
                              labels=c("Very specialized", "Specialized", "Diversified", "Very diversified"),
                              na.value = "black") + 
          geom_area(inherit.aes = F, mapping = aes(x = -50, y =-20, color = "No production"), data = data.frame()) +
          scale_colour_manual(name = element_blank(), values="black")  + 
          guides(fill=guide_legend(title="SDI")) + 
          labs(x = " ", 
               y = " ", 
               title = " ", 
               caption = "2010") + guides(color=guide_legend(title = " ")) + 
          geom_sf(data=DNE4, aes(color = NE), fill = "transparent", colour= "black", size=0.7) +
          geom_sf(data=DNE3, aes(color = NE), fill = "transparent", colour= "black", size=0.7) + 
          guides(color=guide_legend(title = " ")) + 
          theme(plot.caption = element_text(hjust = 0.5, size = 12, face = "bold"))
        
        CSI2011 <- subset(NE, NE$ANO=="2011")
        CSI2011 <- rename(CSI2011, SI2011 = SI)
        
        DNE1 = merge(DNE1, CSI2011, by = c("CODIGO_MUN"), all.x = T)
        
        GCSI2011 = ggplot(data=DNE1) +  geom_sf(aes(fill = SI2011), colour = NA, size = 0.01) +
          scale_fill_gradient(low = "#FFAE00", high = "#00963D" , breaks=c(0, 0.35, 0.65, 1), limits=c(0,1),
                              labels=c("Very specialized", "Specialized", "Diversified", "Very diversified"),
                              na.value = "black") + 
          geom_area(inherit.aes = F, mapping = aes(x = -50, y =-20, color = "No production"), data = data.frame()) +
          scale_colour_manual(name = element_blank(), values="black")  + 
          guides(fill=guide_legend(title="SDI")) + 
          labs(x = " ", 
               y = " ", 
               title = " ", 
               caption = "2011") + guides(color=guide_legend(title = " ")) + 
          geom_sf(data=DNE4, aes(color = NE), fill = "transparent", colour= "black", size=0.7) +
          geom_sf(data=DNE3, aes(color = NE), fill = "transparent", colour= "black", size=0.7) + 
          guides(color=guide_legend(title = " ")) + 
          theme(plot.caption = element_text(hjust = 0.5, size = 12, face = "bold"))
        
        CSI2012 <- subset(NE, NE$ANO=="2012")
        CSI2012 <- rename(CSI2012, SI2012 = SI)
        
        DNE1 = merge(DNE1, CSI2012, by = c("CODIGO_MUN"), all.x = T)
        
        GCSI2012 = ggplot(data=DNE1) +  geom_sf(aes(fill = SI2012), colour = NA, size = 0.01) +
          scale_fill_gradient(low = "#FFAE00", high = "#00963D" , breaks=c(0, 0.35, 0.65, 1), limits=c(0,1),
                              labels=c("Very specialized", "Specialized", "Diversified", "Very diversified"),
                              na.value = "black") + 
          geom_area(inherit.aes = F, mapping = aes(x = -50, y =-20, color = "No production"), data = data.frame()) +
          scale_colour_manual(name = element_blank(), values="black")  + 
          guides(fill=guide_legend(title="SDI")) + 
          labs(x = " ", 
               y = " ", 
               title = " ", 
               caption = "2012") + guides(color=guide_legend(title = " ")) + 
          geom_sf(data=DNE4, aes(color = NE), fill = "transparent", colour= "black", size=0.7) +
          geom_sf(data=DNE3, aes(color = NE), fill = "transparent", colour= "black", size=0.7) + 
          guides(color=guide_legend(title = " ")) + 
          theme(plot.caption = element_text(hjust = 0.5, size = 12, face = "bold"))
        
        CSI2013 <- subset(NE, NE$ANO=="2013")
        CSI2013 <- rename(CSI2013, SI2013 = SI)
        
        DNE1 = merge(DNE1, CSI2013, by = c("CODIGO_MUN"), all.x = T)
        
        GCSI2013 = ggplot(data=DNE1) +  geom_sf(aes(fill = SI2013), colour = NA, size = 0.01) +
          scale_fill_gradient(low = "#FFAE00", high = "#00963D" , breaks=c(0, 0.35, 0.65, 1), limits=c(0,1),
                              labels=c("Very specialized", "Specialized", "Diversified", "Very diversified"),
                              na.value = "black") + 
          geom_area(inherit.aes = F, mapping = aes(x = -50, y =-20, color = "No production"), data = data.frame()) +
          scale_colour_manual(name = element_blank(), values="black")  + 
          guides(fill=guide_legend(title="SDI")) + 
          labs(x = " ", 
               y = " ", 
               title = " ", 
               caption = "2013") + guides(color=guide_legend(title = " ")) + 
          geom_sf(data=DNE4, aes(color = NE), fill = "transparent", colour= "black", size=0.7) +
          geom_sf(data=DNE3, aes(color = NE), fill = "transparent", colour= "black", size=0.7) + 
          guides(color=guide_legend(title = " ")) + 
          theme(plot.caption = element_text(hjust = 0.5, size = 12, face = "bold"))
        
        CSI2014 <- subset(NE, NE$ANO=="2014")
        CSI2014 <- rename(CSI2014, SI2014 = SI)
        
        DNE1 = merge(DNE1, CSI2014, by = c("CODIGO_MUN"), all.x = T)
        
        GCSI2014 = ggplot(data=DNE1) +  geom_sf(aes(fill = SI2014), colour = NA, size = 0.01) +
          scale_fill_gradient(low = "#FFAE00", high = "#00963D" , breaks=c(0, 0.35, 0.65, 1), limits=c(0,1),
                              labels=c("Very specialized", "Specialized", "Diversified", "Very diversified"),
                              na.value = "black") + 
          geom_area(inherit.aes = F, mapping = aes(x = -50, y =-20, color = "No production"), data = data.frame()) +
          scale_colour_manual(name = element_blank(), values="black")  + 
          guides(fill=guide_legend(title="SDI")) + 
          labs(x = " ", 
               y = " ", 
               title = " ", 
               caption = "2014") + guides(color=guide_legend(title = " ")) + 
          geom_sf(data=DNE4, aes(color = NE), fill = "transparent", colour= "black", size=0.7) +
          geom_sf(data=DNE3, aes(color = NE), fill = "transparent", colour= "black", size=0.7) + 
          guides(color=guide_legend(title = " ")) + 
          theme(plot.caption = element_text(hjust = 0.5, size = 12, face = "bold"))
        
        CSI2015 <- subset(NE, NE$ANO=="2015")
        CSI2015 <- rename(CSI2015, SI2015 = SI)
        
        DNE1 = merge(DNE1, CSI2015, by = c("CODIGO_MUN"), all.x = T)
        
        GCSI2015 = ggplot(data=DNE1) +  geom_sf(aes(fill = SI2015), colour = NA, size = 0.01) +
          scale_fill_gradient(low = "#FFAE00", high = "#00963D" , breaks=c(0, 0.35, 0.65, 1), limits=c(0,1),
                              labels=c("Very specialized", "Specialized", "Diversified", "Very diversified"),
                              na.value = "black") + 
          geom_area(inherit.aes = F, mapping = aes(x = -50, y =-20, color = "No production"), data = data.frame()) +
          scale_colour_manual(name = element_blank(), values="black")  + 
          guides(fill=guide_legend(title="SDI")) + 
          labs(x = " ", 
               y = " ", 
               title = " ", 
               caption = "2015") + guides(color=guide_legend(title = " ")) + 
          geom_sf(data=DNE4, aes(color = NE), fill = "transparent", colour= "black", size=0.7) +
          geom_sf(data=DNE3, aes(color = NE), fill = "transparent", colour= "black", size=0.7) + 
          guides(color=guide_legend(title = " ")) + 
          theme(plot.caption = element_text(hjust = 0.5, size = 12, face = "bold"))
        
        CSI2016 <- subset(NE, NE$ANO=="2016")
        CSI2016 <- rename(CSI2016, SI2016 = SI)
        
        DNE1 = merge(DNE1, CSI2016, by = c("CODIGO_MUN"), all.x = T)
        
        GCSI2016 = ggplot(data=DNE1) +  geom_sf(aes(fill = SI2016), colour = NA, size = 0.01) +
          scale_fill_gradient(low = "#FFAE00", high = "#00963D" , breaks=c(0, 0.35, 0.65, 1), limits=c(0,1),
                              labels=c("Very specialized", "Specialized", "Diversified", "Very diversified"),
                              na.value = "black") + 
          geom_area(inherit.aes = F, mapping = aes(x = -50, y =-20, color = "No production"), data = data.frame()) +
          scale_colour_manual(name = element_blank(), values="black")  + 
          guides(fill=guide_legend(title="SDI")) + 
          labs(x = " ", 
               y = " ", 
               title = " ", 
               caption = "2016") + guides(color=guide_legend(title = " ")) + 
          geom_sf(data=DNE4, aes(color = NE), fill = "transparent", colour= "black", size=0.7) +
          geom_sf(data=DNE3, aes(color = NE), fill = "transparent", colour= "black", size=0.7) + 
          guides(color=guide_legend(title = " ")) + 
          theme(plot.caption = element_text(hjust = 0.5, size = 12, face = "bold"))
        
        CSI2017 <- subset(NE, NE$ANO=="2017")
        CSI2017 <- rename(CSI2017, SI2017 = SI)
        
        DNE1 = merge(DNE1, CSI2017, by = c("CODIGO_MUN"), all.x = T)
        
        GCSI2017 = ggplot(data=DNE1) +  geom_sf(aes(fill = SI2017), colour = NA, size = 0.01) +
          scale_fill_gradient(low = "#FFAE00", high = "#00963D" , breaks=c(0, 0.35, 0.65, 1), limits=c(0,1),
                              labels=c("Very specialized", "Specialized", "Diversified", "Very diversified"),
                              na.value = "black") + 
          geom_area(inherit.aes = F, mapping = aes(x = -50, y =-20, color = "No production"), data = data.frame()) +
          scale_colour_manual(name = element_blank(), values="black")  + 
          guides(fill=guide_legend(title="SDI")) + 
          labs(x = " ", 
               y = " ", 
               title = " ", 
               caption = "2017") + guides(color=guide_legend(title = " ")) + 
          geom_sf(data=DNE4, aes(color = NE), fill = "transparent", colour= "black", size=0.7) +
          geom_sf(data=DNE3, aes(color = NE), fill = "transparent", colour= "black", size=0.7) + 
          guides(color=guide_legend(title = " ")) + 
          theme(plot.caption = element_text(hjust = 0.5, size = 12, face = "bold"))
        
        CSI2018 <- subset(NE, NE$ANO=="2018")
        CSI2018 <- rename(CSI2018, SI2018 = SI)
        
        DNE1 = merge(DNE1, CSI2018, by = c("CODIGO_MUN"), all.x = T)
        
        GCSI2018 = ggplot(data=DNE1) +  geom_sf(aes(fill = SI2018), colour = NA, size = 0.01) +
          scale_fill_gradient(low = "#FFAE00", high = "#00963D" , breaks=c(0, 0.35, 0.65, 1), limits=c(0,1),
                              labels=c("Very specialized", "Specialized", "Diversified", "Very diversified"),
                              na.value = "black") + 
          geom_area(inherit.aes = F, mapping = aes(x = -50, y =-20, color = "No production"), data = data.frame()) +
          scale_colour_manual(name = element_blank(), values="black")  + 
          guides(fill=guide_legend(title="SDI")) + 
          labs(x = " ", 
               y = " ", 
               title = " ", 
               caption = "2018") + guides(color=guide_legend(title = " ")) + 
          geom_sf(data=DNE4, aes(color = NE), fill = "transparent", colour= "black", size=0.7) +
          geom_sf(data=DNE3, aes(color = NE), fill = "transparent", colour= "black", size=0.7) + 
          guides(color=guide_legend(title = " ")) + 
          theme(plot.caption = element_text(hjust = 0.5, size = 12, face = "bold"))
        
        CSI2019 <- subset(NE, NE$ANO=="2019")
        CSI2019 <- rename(CSI2019, SI2019 = SI)
        
        DNE1 = merge(DNE1, CSI2019, by = c("CODIGO_MUN"), all.x = T)
        
        GCSI2019 = ggplot(data=DNE1) +  geom_sf(aes(fill = SI2019), colour = NA, size = 0.01) +
          scale_fill_gradient(low = "#FFAE00", high = "#00963D" , breaks=c(0, 0.35, 0.65, 1), limits=c(0,1),
                              labels=c("Very specialized", "Specialized", "Diversified", "Very diversified"),
                              na.value = "black") + 
          geom_area(inherit.aes = F, mapping = aes(x = -50, y =-20, color = "No production"), data = data.frame()) +
          scale_colour_manual(name = element_blank(), values="black")  + 
          guides(fill=guide_legend(title="SDI")) + 
          labs(x = " ", 
               y = " ", 
               title = " ", 
               caption = "2019") + guides(color=guide_legend(title = " ")) + 
          geom_sf(data=DNE4, aes(color = NE), fill = "transparent", colour= "black", size=0.7) +
          geom_sf(data=DNE3, aes(color = NE), fill = "transparent", colour= "black", size=0.7) + 
          guides(color=guide_legend(title = " ")) + 
          theme(plot.caption = element_text(hjust = 0.5, size = 12, face = "bold"))
        
        CSI2020 <- subset(NE, NE$ANO=="2020")
        CSI2020 <- rename(CSI2020, SI2020 = SI)
        
        DNE1 = merge(DNE1, CSI2020, by = c("CODIGO_MUN"), all.x = T)
        
        GCSI2020 = ggplot(data=DNE1) +  geom_sf(aes(fill = SI2020), colour = NA, size = 0.01) +
          scale_fill_gradient(low = "#FFAE00", high = "#00963D" , breaks=c(0, 0.35, 0.65, 1), limits=c(0,1),
                              labels=c("Very specialized", "Specialized", "Diversified", "Very diversified"),
                              na.value = "black") + 
          geom_area(inherit.aes = F, mapping = aes(x = -50, y =-20, color = "No production"), data = data.frame()) +
          scale_colour_manual(name = element_blank(), values="black")  + 
          guides(fill=guide_legend(title="SDI")) + 
          labs(x = " ", 
               y = " ", 
               title = " ", 
               caption = "2020") + guides(color=guide_legend(title = " ")) + 
          geom_sf(data=DNE4, aes(color = NE), fill = "transparent", colour= "black", size=0.7) +
          geom_sf(data=DNE3, aes(color = NE), fill = "transparent", colour= "black", size=0.7) + 
          guides(color=guide_legend(title = " ")) + 
          theme(plot.caption = element_text(hjust = 0.5, size = 12, face = "bold"))


# Exportando mapas de 2000-2020

grid.arrange (GCSI2000, GCSI2001, GCSI2002,
              GCSI2003, GCSI2004, GCSI2005,
              GCSI2006, GCSI2007, GCSI2008,
              GCSI2009, GCSI2010, GCSI2011, ncol= 4)

grid.arrange (GCSI2012, GCSI2013,
              GCSI2014, GCSI2015, GCSI2016,
              GCSI2017, GCSI2018, GCSI2019,
              GCSI2020, GCSI2000, GCSI2001, 
              GCSI2002, ncol= 4)

