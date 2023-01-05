########################################################################################################################
# Rotina {R} : Indicadores e tratamento de dados climaticos
# Data: 2022
# Autor: Alvaro Roberio
# Website: alvaroroberio.github.io
########################################################################################################################

# Carregando pacote de manipulação de dados tabulares

library(tidyverse)
library(dplyr)

# Primeira parte da rotina

## Renomeando dataframe

df <- data.frame(DataclimaPE)

## Calculando médias históricas das variáveis climáticas 

df %>% group_by(ANO, ID, CODIGO, CODIBGE, MESOREGIAO, MICRORREGIAO, GRES, MUNICIPIOS) %>%
  summarise(MAET=mean(AET)) -> MAET

df %>% group_by(ANO, ID, CODIGO, CODIBGE, MESOREGIAO, MICRORREGIAO, GRES, MUNICIPIOS) %>%
  summarise(MTMAX=mean(TMAX)) -> MTMAX

df %>% group_by(ANO, ID, CODIGO, CODIBGE, MESOREGIAO, MICRORREGIAO, GRES, MUNICIPIOS) %>%
  summarise(MTMIN=mean(TMIN)) -> MTMIN

df %>% group_by(ANO, ID, CODIGO, CODIBGE, MESOREGIAO, MICRORREGIAO, GRES, MUNICIPIOS) %>%
  summarise(MPPT=mean(PPT)) -> MPPT

df %>% group_by(ANO, ID, CODIGO, CODIBGE, MESOREGIAO, MICRORREGIAO, GRES, MUNICIPIOS) %>%
  summarise(MDEF=mean(DEF)) -> MDEF

df %>% group_by(ANO, ID, CODIGO, CODIBGE, MESOREGIAO, MICRORREGIAO, GRES, MUNICIPIOS) %>%
  summarise(MPDSI=mean(PDSI)) -> MPDSI

## Unindo primeira database (anual)

df1 <- merge(MAET, MDEF,
             by=c( "ANO", "ID", "CODIGO", "CODIBGE", "MESOREGIAO", "MICRORREGIAO", "GRES", "MUNICIPIOS"))

df1 <- merge(df1, MPDSI,
             by=c( "ANO", "ID", "CODIGO", "CODIBGE", "MESOREGIAO", "MICRORREGIAO", "GRES", "MUNICIPIOS"))

df1 <- merge(df1, MTMAX,
             by=c( "ANO", "ID", "CODIGO", "CODIBGE", "MESOREGIAO", "MICRORREGIAO", "GRES", "MUNICIPIOS"))

df1 <- merge(df1, MTMIN,
             by=c( "ANO", "ID", "CODIGO", "CODIBGE", "MESOREGIAO", "MICRORREGIAO", "GRES", "MUNICIPIOS"))

df1 <- merge(df1, MPPT,
             by=c( "ANO", "ID", "CODIGO", "CODIBGE", "MESOREGIAO", "MICRORREGIAO", "GRES", "MUNICIPIOS"))

## Alterando nome das variáveis no dataframe 

df1 <- df1 %>% rename (PPT = MPPT, TMAX = MTMAX, TMIN = MTMIN, AET = MAET, DEF = MDEF, 
                       PDSI = MPDSI)

rm(MPPT, MTMAX, MTMIN, MAET, MDEF, MPDSI )

## Excluindo variáveis criadas 

# Segunda parte da rotina 

## Calculando médias históricas de pluviometria e evapotranspiração 
## Essas variáveis serão utilizadas para calcular choques de seca e o índice de aridez 

df %>% group_by(ANO, ID, CODIGO, CODIBGE, MESOREGIAO, MICRORREGIAO, GRES, MUNICIPIOS) %>%
  summarise(MPPT=mean(PPT)) -> MPPT

df %>% group_by(ANO, ID, CODIGO, CODIBGE, MESOREGIAO, MICRORREGIAO, GRES, MUNICIPIOS) %>%
  summarise(SDPPT=sd(PPT)) -> SDPP

## Unindo com a base de dados 

df3 <- merge(SDPPT, MPPT,
             by=c( "ANO", "ID", "CODIGO", "CODIBGE", "MESOREGIAO", "MICRORREGIAO", "GRES", "MUNICIPIOS"))

df3 <- merge(df3, SDPPT,
             by=c( "ANO", "ID", "CODIGO", "CODIBGE", "MESOREGIAO", "MICRORREGIAO", "GRES", "MUNICIPIOS"))

df4 <- merge(DataSemiarido, df3,
             by=c( "ANO", "ID", "CODIGO", "CODIBGE", "MESOREGIAO", "MICRORREGIAO", "GRES", "MUNICIPIOS"))

df3 %>% group_by(ANO, ID, CODIGO, CODIBGE, MESOREGIAO, MICRORREGIAO, GRES, MUNICIPIOS) %>%
  summarise(MMPPT=mean(MPPT)) -> MMPPT

df3 %>% group_by(ANO, ID, CODIGO, CODIBGE, MESOREGIAO, MICRORREGIAO, GRES, MUNICIPIOS) %>%
  summarise(MSDPPT=mean(SDPPT)) -> MSDPPT

dfS <- merge(MSDPPT, MMPPT,
             by=c( "ANO", "ID", "CODIGO", "CODIBGE", "MESOREGIAO", "MICRORREGIAO", "GRES", "MUNICIPIOS"))

## Choques de Seca 

df2$CSECA <- 0
df2$CSECA[df2$PPT < df2$MPPT - df2$SDPPT] <-1 

df2$CCHUVA <- 0 
df2$CCHUVA[df2$PPT > df2$MPPT + df2$SDPPT] <- 1 


## Calculando o número de Choques de Seca no Ano

df2 %>% group_by(ANO, ID, CODIGO, CODIBGE, MESOREGIAO, MICRORREGIAO, GRES, MUNICIPIOS) %>%
  summarise(CSECA=sum(CSECA)) -> CSECA

df2 %>% group_by(ANO, ID, CODIGO, CODIBGE, MESOREGIAO, MICRORREGIAO, GRES, MUNICIPIOS) %>%
  summarise(CCHUVA=sum(CCHUVA)) -> CCHUVA

df2 %>% group_by(ANO, ID, CODIGO, CODIBGE, MESOREGIAO, MICRORREGIAO, GRES, MUNICIPIOS) %>%
  summarise(MMPPT=mean(MPPT)) -> MMPPT

df2 %>% group_by(ANO, ID, CODIGO, CODIBGE, MESOREGIAO, MICRORREGIAO, GRES, MUNICIPIOS) %>%
  summarise(MSDPPT=mean(SDPPT)) -> MSDPPT

## Novo merge com variáveis de choques de seca 

df1 <- merge(df1, CSECA,
             by=c( "ANO", "ID", "CODIGO", "CODIBGE", "MESOREGIAO", "MICRORREGIAO", "GRES", "MUNICIPIOS"))

df1 <- merge(df1, CCHUVA,
             by=c( "ANO", "ID", "CODIGO", "CODIBGE", "MESOREGIAO", "MICRORREGIAO", "GRES", "MUNICIPIOS"))

df1 <- merge(df1, MMPPT,
             by=c( "ANO", "ID", "CODIGO", "CODIBGE", "MESOREGIAO", "MICRORREGIAO", "GRES", "MUNICIPIOS"))

df1 <- merge(df1, MSDPPT,
             by=c( "ANO", "ID", "CODIGO", "CODIBGE", "MESOREGIAO", "MICRORREGIAO", "GRES", "MUNICIPIOS"))

plot(df1$CSECA)

## Calculando Indice de Aridez (Balanço Hídrico)

df %>% group_by(ANO, ID, CODIGO, CODIBGE, MESOREGIAO, MICRORREGIAO, GRES, MUNICIPIOS) %>%
  summarise(ACPPT=sum(PPT)) -> ACPPT

df %>% group_by(ANO, ID, CODIGO, CODIBGE, MESOREGIAO, MICRORREGIAO, GRES, MUNICIPIOS) %>%
  summarise(ACAET=sum(AET)) -> ACAET

## Gerando uma estatística descritiva para verificar se não existem zeros 
## O zero pode levar ao resultado "inf" no R. 

summary(ACAET)
summary(ACPPT)

## Juntando bases antes de prosseguir

df1 <- merge(df1, ACAET,
             by=c( "ANO", "ID", "CODIGO", "CODIBGE", "MESOREGIAO", "MICRORREGIAO", "GRES", "MUNICIPIOS"))

df1 <- merge(df1, ACPPT,
             by=c( "ANO", "ID", "CODIGO", "CODIBGE", "MESOREGIAO", "MICRORREGIAO", "GRES", "MUNICIPIOS"))

## Gerando variáveis em logaritmo (Acumulado da Pluviometria e Evapotranspiração)

df1$LNAI = (df1$ACPPT/df1$ACAET)*100 
df1$LNACAET = log(df1$ACAET)
df1$LNACPPT = log(df1$ACPPT)


## Resultado do Ìndice de Aridez

df1 <- subset(df1, select = -c(LNAI))

# Construindo outras variáveis importantes

## Temperatura Média 

df1$TMED = ((df1$TMAX+df1$TMIN)/2)

## Amplitude Térmica 

df1$AMT = (df1$TMAX - df1$TMIN)

## Déficit Hídrico Acumulado 

df %>% group_by(ANO, ID, CODIGO, CODIBGE, MESOREGIAO, MICRORREGIAO, GRES, MUNICIPIOS) %>%
  summarise(ADEF=sum(DEF)) -> ADEF

df1 <- merge(df1, ADEF,
             by=c( "ANO", "ID", "CODIGO", "CODIBGE", "MESOREGIAO", "MICRORREGIAO", "GRES", "MUNICIPIOS"))


## Índice de Porcentagem de Normal 

df %>% group_by(MES, ID, CODIGO, CODIBGE, MESOREGIAO, MICRORREGIAO, GRES, MUNICIPIOS) %>%
  summarise(MPPT=mean(PPT)) -> MPPT

df2 <- merge(df, MPPT,
             by=c( "MES", "ID", "CODIGO", "CODIBGE", "MESOREGIAO", "MICRORREGIAO", "GRES", "MUNICIPIOS"))

df2$IPN = ((df2$PPT/df2$MPPT)-1)*100

summary(df2$IPN)

df2 %>% group_by(ANO, ID, CODIGO, CODIBGE, MESOREGIAO, MICRORREGIAO, GRES, MUNICIPIOS) %>%
  summarise(MIPN=mean(IPN)) -> MIPN

df1 <- merge(df1, MIPN,
             by=c( "ANO", "ID", "CODIGO", "CODIBGE", "MESOREGIAO", "MICRORREGIAO", "GRES", "MUNICIPIOS"))

rm(df2, MIPN, MPPT, ADEF, ACAET, ACPPT)

## Índice de Chuva de Lang (LRI)

df1$LRI = (df1$ACPPT/df1$TMED)
summary(df1$LRI)

# Exportando banco de dados 

library(foreign)

write.dta(dfS, "DataSemiarido4.dta")


# Rainfall Desviation 

df <- data.frame(DataclimaPE)  

df %>% group_by(MES, ID, CODIGO, CODIBGE, MESOREGIAO, MICRORREGIAO, GRES, MUNICIPIOS) %>%
  summarise(MPPT=mean(PPT)) -> MPPT

df %>% group_by(MES, ID, CODIGO, CODIBGE, MESOREGIAO, MICRORREGIAO, GRES, MUNICIPIOS) %>%
  summarise(SDPPT=sd(PPT)) -> SDPPT

df2$RainfallDesv = (df2$MPPT - df2$PPT)/df2$SDPPT

summary(df2$RainfallDesv)

df2$DRainfall <- 0
df2$DRainfall[df2$RainfallDesv < 1 & df2$RainfallDesv > 0 ]<-1 

df2$CDRainfall <- 0
df2$CDRainfall[df2$RainfallDesv > 0]<-1 

df2 %>% group_by(ANO, ID, CODIGO, CODIBGE, MESOREGIAO, MICRORREGIAO, GRES, MUNICIPIOS) %>%
  summarise(DRainfall =sum(DRainfall )) -> DRainfall 

df2 %>% group_by(ANO, ID, CODIGO, CODIBGE, MESOREGIAO, MICRORREGIAO, GRES, MUNICIPIOS) %>%
  summarise(CDRainfall =sum(CDRainfall)) -> CDRainfall 

## Estatística Descritiva da Seca

df %>% group_by(ANO, ID, CODIGO, CODIBGE, MESOREGIAO, MICRORREGIAO, GRES, MUNICIPIOS) %>%
  summarise(APPT=sum(PPT)) -> APPT

df %>% group_by(MES) %>%
  summarise(MPPTM=mean(PPT)) -> MPPTM

write.dta(df3, "DataSemiarido4.dta")

## Dados p/ Gráfico de SECA 

MES =   expand.grid(ANO = unique(DataSemiarido$ANO), MES = 1:12)
left_join(DataSemiarido, MES, by = "ANO")

DataSemiarido %>% mutate(MES = Map(seq, 1, 12)) %>%
  tidyr::unnest(cols = MES) -> DataSemiarido1

df4 <- merge(df, DataSemiarido1,
             by=c( "ANO", "MES", "ID", "CODIGO", "CODIBGE"))


            