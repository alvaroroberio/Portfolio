########################################################################################################################
# Rotina {R} : Tratamento de Dados Ambientais
# Data: 2022 
# Autor: Alvaro Roberio
# Website: alvaroroberio.github.io
########################################################################################################################

# Pacotes do R 

require(readr)
require(readxl)
require(tidyverse)
require(dplyr)
require(foreign)

rm(list = ls())
options(scipen = 999, digits = 6)

setwd("C:/Users/alvar/Desktop/Microdados de Agricultura/Mapbiomas")
dir()

########################################################################################################################
# Mapbiomas // Superf?cie de ?gua 
########################################################################################################################

# Importando e Renomeando Dados 

Agua <- read_excel("MapBio_Agua.xlsx", sheet=4, col_names=TRUE)

head(Agua)
Agua <- rename(Agua, CODIBGE = code, MUNICIPIO = name, ANO = year, AGUA = area_ha)

# Convertando ?rea de superf?cie de ?gua de hectare para Km2 

Agua$AGUA <- Agua$AGUA/100 

# Organizando dados 

Agua %>%
  relocate(ANO, CODIBGE, MUNICIPIO, AGUA) -> Agua
head(Agua)

write.dta(Agua, "Agua.dta")

########################################################################################################################
# Mapbiomas // Floresta 
########################################################################################################################

# Importando Database de Biomas 

Cobertura <- read_excel("MapBio_Vegetacao.xlsx", sheet=3, col_names=TRUE)

# Filtrando a Cobertura de Floresta Natural - Classe: 1.1-5 (MapBiomas)

NFloresta <- filter(Cobertura, level_0 == "Natural" & level_1 == "1. Forest")
head(NFloresta)

# Excluindo colunas desnecess?rias 

Exclusao  <- c("feature_id", "class_id", "level_3", "level_4", "color", "category", "index")
NFloresta <- NFloresta[,!(names(NFloresta) %in% Exclusao)]
rm(Exclusao)
head(NFloresta)

# Renomeando a Database

NFloresta <- rename(NFloresta, UF = state, MUNICIPIO = city, CODIBGE = geo_code, LEVEL0 = level_0,  LEVEL1 = level_1, 
                    LEVEL2 = level_2)

# Atribuindo prefixo para colunas e transpondo database

# CFLORESTA: "Cobertura Florestal Natural"

colnames(NFloresta)[7:42]  <- paste0("X", colnames(NFloresta)[7:42])
head(NFloresta)

NFloresta <- gather(NFloresta, ANO, CFLORESTA, X1985:X2020, factor_key=TRUE)  # Ajustando coluna de tempo 

NFloresta$ANO <- gsub("X","",as.character(NFloresta$ANO)) 

NFloresta[is.na(NFloresta)] <- 0                                               # Tratando 'NAs' do database 

# Agregando dados da classe de Florestal Natural 
# ESTFLORESTAL: 'Estoque de floresta natural' 

NFloresta %>% group_by(UF, MUNICIPIO, CODIBGE, LEVEL0, LEVEL1, ANO) %>%
              summarise(ESTFLORESTAL=sum(CFLORESTA)) -> NFloresta

# Convertando "Estoque Florestal" de hectare para km2 

NFloresta$ESTFLORESTAL = NFloresta$ESTFLORESTAL/100

# Excluindo e Exportando 

NFloresta$LEVEL0 = NULL 
NFloresta$LEVEL1 = NULL

write.dta(NFloresta, "NFloresta.dta")

########################################################################################################################
# Mapbiomas // N?o Floresta Natural 
########################################################################################################################

# Importando e Renomeando Dados 

NNFloresta <- filter(Cobertura, level_0 == "Natural" & level_1 == "2. Non Forest Natural Formation")


# Excluindo colunas desnecess?rias 

Exclusao  <- c("feature_id", "class_id", "level_3", "level_4", "color", "category", "index")
NNFloresta <- NNFloresta[,!(names(NNFloresta) %in% Exclusao)] 
rm(Exclusao)
head(NNFloresta)

# Renomeando a Database

NNFloresta <- rename(NNFloresta, UF = state, MUNICIPIO = city, CODIBGE = geo_code, LEVEL0 = level_0,  LEVEL1 = level_1, 
                    LEVEL2 = level_2)


# Atribuindo prefixo para colunas e transpondo database

# CNFLORESTA: "Cobertura  Natural N?o Florestal"

colnames(NNFloresta)[7:42]  <- paste0("X", colnames(NNFloresta)[7:42])
head(NNFloresta)

NNFloresta <- gather(NNFloresta, ANO, CNFLORESTA, X1985:X2020, factor_key=TRUE)    #Ajustando coluna de tempo 

NNFloresta$ANO <- gsub("X","",as.character(NNFloresta$ANO)) 

NNFloresta[is.na(NNFloresta)] <- 0                                                 #Tratando 'NAs' do database 


# Agregando dados da classe de Florestal Natural 
# ESTNFLORESTAL: 'Estoque natural de n?o florestal' 

NNFloresta %>% group_by(UF, MUNICIPIO, CODIBGE, LEVEL0, LEVEL1, ANO) %>%
  summarise(ESTNFLORESTAL=sum(CNFLORESTA)) -> NNFloresta


# Convertando "Estoque Natural n?o Florestal" de hectare para km2 

NNFloresta$ESTNFLORESTAL = NNFloresta$ESTNFLORESTAL/100

# Excluindo e Exportando

NNFloresta$LEVEL0 = NULL 
NNFloresta$LEVEL1 = NULL

write.dta(NNFloresta, "NNFloresta.dta")

########################################################################################################################
# Extens?o Territorial 
########################################################################################################################
# O MapBiomas apresenta cobertura de uso da terra para todos os munic?pios brasileiros de 1985-2020
# Os dados de extens?o territorial do Brasil s?o fixados nos valores de 2020 para evitar distor??es nas medidas
# Essas distor??es se d?o em fun??o dos processos de mudan?as territoriais no Brasil ao longo das ?ltimas d?cadas

# Importando e renomeando

Territorio <- read_excel("Territorio.xls", sheet=1, col_names=TRUE)

Territorio <- rename(Territorio, UF=Sigla, CODIBGE = Codigo, MUNICIPIO = Munic?pio)

# Expandindo database de territ?rio 

Tempo = expand.grid(CODIBGE = unique(Territorio$CODIBGE), ANO = 1985:2020)
left_join(Territorio, Tempo, by = "CODIBGE") -> Territorio
rm(Tempo)

########################################################################################################################
# Unindo Bases de Dados: Agua, Cobertura e Territ?rio 
########################################################################################################################

DCobertura <- merge(Agua, Territorio, by=c("ANO", "CODIBGE", "MUNICIPIO"))
DCobertura <- merge(DCobertura, NFloresta, by=c("ANO", "CODIBGE", "MUNICIPIO"))
DCobertura <- merge(DCobertura, NNFloresta, by=c("ANO", "CODIBGE", "MUNICIPIO"))

DCobertura$UF.x = NULL
DCobertura$UF.y = NULL 
head(DCobertura)

DCobertura <- rename(DCobertura, TERRITORIO = Territorio)

# Criando vari?veis Ambientais 

head(DCobertura)

## Taxa de Cobertura de ?gua em Km2 (TCA) 

DCobertura$TCA = (DCobertura$AGUA/DCobertura$TERRITORIO)
summary(DCobertura$TCA)

### Taxa de Cobertura de Florestal Natural em km2 (TCF)

DCobertura$TCF = (DCobertura$ESTFLORESTAL/DCobertura$TERRITORIO)
summary(DCobertura$TCF)

#### Taxa de Cobertura Natural N?o Florestal em km2 (TCNF) 

DCobertura$TCNF = (DCobertura$ESTNFLORESTAL/DCobertura$TERRITORIO)
summary(DCobertura$TCNF)

# Exportando Bancos de Dados 

write.dta(Agua, "Data_Agua.dta")
write.dta(NFloresta, "Data_NFloresta.dta")
write.dta(NNFloresta, "Data_NNFloresta.dta")
write.dta(Territorio, "Data_Territorio.dta")
write.dta(DCobertura, "Data_DCobertura.dta")




