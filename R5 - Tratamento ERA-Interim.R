########################################################################################################################
# Rotina {R} : Dados climaticos do ERA-Interim
# Data: 2022 
# Autor: Alvaro Roberio
# Website: alvaroroberio.github.io
########################################################################################################################

# Packages 

library(tidyverse)
library(dplyr)
library(lubridate)
library(foreign)


# Tratamento de Banco de Dados Climáticos 

# Banco de Dados

setwd("D:/Artigos de Economia/Seca e Agropecuária - NE/Data_Recursos_Naturais")
Pluv_1980.2021 <- read.csv("/Pluv_1980-2021.csv")
Temp_1980.2021 <- read.csv("/Temp_1980-2021.csv")

# Renomeando dataframe 

Pluv <- data.frame(Pluv_1980.2021)
Temp <- data.frame(Temp_1980.2021)

Temp <- gather(Temp, DATE, TEMP, X1981.01.01:X2021.07.01, factor_key=TRUE)
Pluv <- gather(Pluv, DATE, PLUV, X1981.01.01:X2021.07.01, factor_key=TRUE)

# Tratando Variável de Tempo 

Temp$DATE <-gsub("X","",as.character(Temp$DATE))
Pluv$DATE <-gsub("X","",as.character(Pluv$DATE))

Temp = Temp %>% 
  mutate(DATE = ymd(DATE)) %>% 
  mutate_at(vars(DATE), funs(year, month, day))

Pluv = Pluv %>% 
  mutate(DATE = ymd(DATE)) %>% 
  mutate_at(vars(DATE), funs(year, month, day))

Temp = Temp %>% rename(ANO=year, MES=month, DIA=day)
Pluv = Pluv %>% rename(ANO=year, MES=month, DIA=day)

Temp = Temp %>% rename(ANO=year, MES=month, DIA=day)
Pluv = Pluv %>% rename(ANO=year, MES=month, DIA=day)


Temp = Temp %>% rename(ID = X_ID, LONGITUDE = X_CX, LATITUDE = X_CY, REGIAO = GRANDES_RE, 
                       UF = NOME_UF, CODIBGE = CODIGO_MUN)

Pluv = Pluv %>% rename(ID = X_ID, LONGITUDE = X_CX, LATITUDE = X_CY, REGIAO = GRANDES_RE, 
                       UF = NOME_UF, CODIBGE = CODIGO_MUN)


# Exportando Banco de Dados 

write.dta(Temp, "DataPluv.dta")
write.dta(Pluv, "DataTemp.dta")

# Tratamento de Banco de Dados MapaBio 

DataAgua = DataAgua %>% rename(CODIBGE = code, MUNICIPIO = name, ANO = year, 
                              AREAAGUA_HA = area_ha)

write.dta(DataAgua, "DataAgua.dta")


DataLandVegetal <- read_excel("DataLandVegetal.xlsx")

DataLandVegetal <- gather(DataLandVegetal, ano, cobuso, 14:49, factor_key=TRUE)

write.dta(DataLandVegetal, "DataLandVegetal.dta")

DataTransVegetal <- read_excel("DataTransVegetal.xlsx")
DataTransVegetal <- gather(DataTransVegetal, ano, cobusotrans, 14:49, factor_key=TRUE)

