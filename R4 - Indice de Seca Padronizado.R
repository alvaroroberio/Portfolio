########################################################################################################################
# Rotina {R} : Indice de Seca Padronizado 
# Data: 2022 
# Autor: Alvaro Roberio
# Website: alvaroroberio.github.io
########################################################################################################################

# Pacotes do R 

require(readr)
require(readxl)
require(tidyverse)
require(haven)
require(dplyr)
require(foreign)

rm(list = ls())
options(scipen = 999, digits = 6)

setwd("C:/Users/alvar/Desktop/Microdados de Agricultura/Data_Tratado")
dir()

# Importando Database 

Pluv  <- read_stata("DataPluv_NE.dta")
Temp  <- read_stata("DataTemp_NE.dta")

# Desvio de Precipitação (TRD)

DT = data.frame(Pluv)
DT$DATE = NULL 
DT$DIA  = NULL 
head(DT)

# Média  e Desvio-Padrão de Longo Prazo Mensal da Pluviometria (TRD)

DT %>% group_by(ID, LONGITUDE, LATITUDE, REGIAO, UF, CODIBGE, ANO) %>%
  summarise(APLUV=sum(PLUV)) -> APLUV 

APLUV %>% group_by(ID, LONGITUDE, LATITUDE, REGIAO, UF, CODIBGE) %>%
  summarise(SDAPLUV=sd(APLUV)) -> SDAPLUV

APLUV %>% group_by(ID, LONGITUDE, LATITUDE, REGIAO, UF, CODIBGE) %>%
  summarise(MAPLUV=mean(APLUV)) -> MAPLUV


DT <- merge(APLUV , SDAPLUV,  by=c("ID", "LONGITUDE", "LATITUDE", "REGIAO", "UF", "CODIBGE"))
DT <- merge(DT, MAPLUV, by=c("ID", "LONGITUDE", "LATITUDE", "REGIAO", "UF", "CODIBGE"))


DT$TRD  = (DT$APLUV - DT$MAPLUV)/DT$SDAPLUV # Z-escore de TRD 

# Excluindo termos desnecessários

rm(MPLUV, SDPLUV)
DT$MAPLUV   = NULL 
DT$SADPLUV  = NULL 

# Desvio de Temperatura (MTD) 

DT1 = data.frame(Temp)
DT1$DATE = NULL 
DT1$DIA  = NULL 
head(DT1)

# Média e Desvio Padrão de Longo Prazo da Temperatura (MTD)

DT1 %>% group_by(ID, LONGITUDE, LATITUDE, REGIAO, UF, CODIBGE, ANO) %>%
  summarise(TEMP=mean(TEMP)) -> TEMP

TEMP %>% group_by(ID, LONGITUDE, LATITUDE, REGIAO, UF, CODIBGE) %>%
  summarise(SDTEMP=sd(TEMP)) -> SDTEMP

TEMP %>% group_by(ID, LONGITUDE, LATITUDE, REGIAO, UF, CODIBGE) %>%
  summarise(MTEMP=mean(TEMP)) -> MTEMP

DT1 <- merge(TEMP, SDTEMP, by=c("ID", "LONGITUDE", "LATITUDE", "REGIAO", "UF", "CODIBGE"))
DT1 <- merge(DT1, MTEMP,   by=c("ID", "LONGITUDE", "LATITUDE", "REGIAO", "UF", "CODIBGE"))

# Z-score de temperatura (MTD) 

DT1$MTD  = (DT1$TEMP - DT1$MTEMP)/DT1$SDTEMP

# Excluindo termos desnecessários

rm(MTEMP, SDTEMP, SDPLUV, MPLUV)

# Merge do Database

DT2 <- merge(DT1, DT, by=c("ID", "LONGITUDE", "LATITUDE", "REGIAO", "UF", "CODIBGE", "ANO"))
rm(DT, DT1, SDAPLUV, MAPLUV, APLUV, TEMP)


# MTD eventos -min(0, TRD)

DT2$TRD1 = DT2$TRD
DT2$TRD1[DT2$TRD > 0] <- 0
DT2$TRD1 = DT2$TRD1*(-1)


# MTD eventos max(0, MTD)

DT2$MTD1 = DT2$MTD
DT2$MTD1[DT2$MTD < 0] <- 0

# Índice de Seca Padronizado (SDI)

DT2$SDI = (DT2$MTD1*DT2$TRD1)


DT2 %>% group_by(ANO) %>%
  summarise(MSDI=mean(SDI)) -> MSDI

DT2 %>% group_by(ANO) %>%
  summarise(SDSDI=sd(SDI)) -> SDSDI

DT2 <- merge(DT2, MSDI, by=c("ANO"))
DT2 <- merge(DT2, SDSDI, by=c("ANO"))

DT2$DP1 = DT2$MSDI - DT2$SDSDI # -1 DP (Inferior)
DT2$DP2 = DT2$MSDI + DT2$SDSDI # +1 DP (Superior)

# Classificação do Índice de Secas (CSDI)
# Seca Leve................................. CSDI = 1 se SDI <= DP1
# Seca Moderada............................. CSDI = 2 se DP1 < SDI <= DP2 
# Seca Severa............................... CSDI = 2 se DP2 < SDI

DT2 <- DT2 %>% mutate(CSDI = case_when(SDI <= DP1               ~ '1',
                                       DP1 < SDI &  SDI <= DP2  ~ '2',
                                       DP2 < SDI                ~ '3'))
head(DT2)
DT2$SDTEMP = NULL
DT2$SDAPLUV = NULL
Clima = DT2
rm(DT2)

# Exportando database 

write.dta(Clima, "Clima.dta")

hist(Clima$SDI)
hist(Clima$TRD1)
hist(Clima$MTD1)

# Gráficos de Seca 

# Pacotes 

require(gridExtra)
require(ggpp)
require(tibble)

Clima  <- read_stata("Clima_NE.dta")

# Figura 1: Histograma do SDI 

ggplot(data=Clima, aes(SDI)) + geom_histogram(aes(y=(..count..)/sum(..count..)),
            bins = 20, fill='cornsilk4', color='black') +  labs(y = " ",
       x = "Índice Padronizado de Seca (SDI)",
       title = "Secas no Nordeste do Brasil",
       subtitle = "Índice Padronizado de Seca (SDI): 1981-2020") + 
  scale_y_continuous(breaks = seq(0, .10, .025),
                     labels = scales::percent,
                     limits = c(0, .10)) +  theme_classic()

ggsave("G1.png", width = 20, height = 15, units="cm")


# Figura 2: MTD1 x TRD1 


 ggplot(Clima, aes(y=TRD, x=MTD)) + 
          labs(y = "Desvio de Precipitação",
          x = "Desvio de Temperatura",
          title = "Secas no Nordeste do Brasil",
          subtitle = "Precipitação e Temperatura: 1981-2020") +
          xlim(-4,4) + ylim(-4,4) + 
          geom_point(size = 2, shape = 1, color="cornsilk4") +
          geom_hline(yintercept=0, color= "cornsilk4", size=1) +
          geom_vline(xintercept=0, color= "cornsilk4", size=1)  + theme_classic() 

 ggsave("G2.png", width = 20, height = 15, units="cm")
 
# Corrigindo Escala do Índice de Seca (CSDI)
 
DT2  <- read_stata("Clima_NE.dta")

head(DT2)

# 0 Clima Normal      = SDI <= 0.1
# 1 Seca Leve         = SDI > 0.1 and SDI  <= 0.5
# 2 Seca Moderada     = SDI > 0.5 and SDI  <= 1
# 3 Seca Severa       = SDI > 1   and SDI  <= 1.5
# 4 Seca Extrema      = SDI > 1.5

DT2 <- DT2 %>% mutate(CSDI2 = case_when(
                      SDI <= 0.1                ~ '0',
                      SDI >  0.1 & SDI <=  0.5  ~ '1',
                      SDI >  0.5 & SDI <=  1    ~ '2',
                      SDI >  1.0 & SDI <=  2    ~ '3',
                      SDI >  2.0                ~ '4'))

write.dta(DT2, "Clima.dta")


# SPI 

Data_SPI_NE$DSeca=0
Data_SPI_NE$SPI1 = Data_SPI_NE$SPI*(-1)
Data_SPI_NE$DSeca[Data_SPI_NE$SPI1 > 1] <- 1


Data_SPI_NE$DESeca=0
Data_SPI_NE$DESeca[Data_SPI_NE$SPI1 > 1.5] <- 1

head(Data_SPI_NE)

Data_SPI_NE %>% group_by(ID, LONGITUDE, LATITUDE, REGIAO, UF, CODIBGE, ANO) %>%
  summarise(DSeca=sum(DSeca)) -> DSeca

Data_SPI_NE %>% group_by(ID, LONGITUDE, LATITUDE, REGIAO, UF, CODIBGE, ANO) %>%
  summarise(DESeca=sum(DESeca)) -> DESeca

Seca <- merge(DSeca,DESeca, by=c("ID", "LONGITUDE", "LATITUDE", "REGIAO", "UF", "CODIBGE", "ANO"))

Seca <- merge(Seca,Data_Clima_NE, by=c("LONGITUDE", "LATITUDE", "REGIAO", "UF", "CODIBGE", "ANO"))

write.dta(Seca, "Seca_NE.dta")


# Modelo de Z-score 

df <- Data_Clima_NE
df %>% group_by(LONGITUDE, LATITUDE, REGIAO, UF, CODIBGE) %>%
  summarise(SDPA=sd(PA)) -> SDPA

df %>% group_by(LONGITUDE, LATITUDE, REGIAO, UF, CODIBGE) %>%
  summarise(MDPA=mean(PA)) -> MDPA

df %>% group_by(LONGITUDE, LATITUDE, REGIAO, UF, CODIBGE) %>%
  summarise(SDTM=sd(TM)) -> SDTM

df %>% group_by(LONGITUDE, LATITUDE, REGIAO, UF, CODIBGE) %>%
  summarise(MDTM=mean(TM)) -> MDTM

df <- merge(df, MDPA, by=c("LONGITUDE", "LATITUDE", "UF", "CODIBGE"))
df <- merge(df, SDPA, by=c("LONGITUDE", "LATITUDE", "UF", "CODIBGE"))
df <- merge(df, MDTM, by=c("LONGITUDE", "LATITUDE", "UF", "CODIBGE"))
df <- merge(df, SDTM, by=c("LONGITUDE", "LATITUDE", "UF", "CODIBGE"))

df$TRD = (df$MDPA - df$PA)/df$SDPA
df$MTD = (df$MDTM - df$TM)/df$SDTM

write.dta(df, "DF_CLIMA.dta")
