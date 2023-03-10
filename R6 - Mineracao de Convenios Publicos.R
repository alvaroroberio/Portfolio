########################################################################################################################
# Rotina {R} : Mineracao de Dados de Convenio (Governo Federal)
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
require(stringr)
require(haven)

rm(list = ls())
options(scipen = 999, digits = 6)

setwd("C:\\Users\\alvar\\Desktop\\Microdados de Agricultura\\Data_Bruto\\Tipos de Conv?nio")
dir()

# Importando Banco de Dados 

DC <- read.csv("Convenios.csv", sep=";", header=TRUE)
head(DC)

########################################################################################################################
# Barragens 
########################################################################################################################

# 1) Filtrando estados do Nordeste Brasileiro 

DCNE <- filter(DC, UF=="MA" | UF=="PI" | UF=="CE" | UF=="RN" | UF=="PB" | UF=="PE" | UF=="AL" | UF=="SE" | UF=="BA")
 
# 2) Filtrando conv?nios conclu?dos 

DCNE <- rename(DCNE, INSTRUMENTO = TIPO.INSTRUMENTO, CONVENIO = SITUA??O.CONV?NIO)
DCNE <- filter(DCNE, INSTRUMENTO == "CONVENIO" & CONVENIO == "CONCLU?DO")


# 3) Filtrando por tipo convenente (Estadual ou Municipal)

DCNE <- filter(DCNE, TIPO.CONVENENTE == "Administra??o P?blica Municipal" | 
                     TIPO.CONVENENTE == "Administra??o P?blica Estadual ou do Distrito Federal")

# 4) Minerando a descri??o do database de conv?nio por n?veis 

DCNE1 <- DCNE
DCNE1$descricao <- str_to_lower(DCNE1$OBJETO)

# N?vel 1: Objetivo do conv?nio (constru??o, execucao e ampliacao)

DCNE1 %>% filter(str_detect(descricao, "construcao | execucao | ampliacao")) -> DCNE2

# N?vel 2: Infraestrura H?drica (Barragem e Acude)

DCNE2%>% filter(str_detect(descricao, "barragem | barragens| acude | acudes")) -> DCNE2

# Nivel 3: Eliminando ministerios que podem distorcer a amostra
# Neste caso, apenas o Minist?rio da Educa??o e do Turismo foram evidenciados como candidatos 

list(DCNE2$NOME.?RG?O.SUPERIOR)

DCNE2 <- filter(DCNE2, NOME.?RG?O.SUPERIOR != "Minist?rio da Educa??o" )
DCNE2 <- filter(DCNE2, NOME.?RG?O.SUPERIOR != "Minist?rio do Turismo" )


# 5) Renomeando e Excluindo dados desnecess?rios 

DCNE2$N?MERO.CONV?NIO = NULL 
DCNE2$N?MERO.PROCESSO.DO.CONV?NIO = NULL 
DCNE2$C?DIGO.?RG?O.CONCEDENTE = NULL 
DCNE2$C?DIGO.?RG?O.SUPERIOR = NULL 
DCNE2$C?DIGO.CONVENENTE = NULL 
DCNE2$DATA.?LTIMA.LIBERA??O = NULL 
DCNE2$VALOR.?LTIMA.LIBERA??O = NULL
DCNE2$OBJETO = NULL
DCNE2$C?DIGO.UG.CONCEDENTE=NULL

head(DCNE2)

DCNE2 <- rename(DCNE2, SIAFI = C?DIGO.SIAFI.MUNIC?PIO, MUNICIPIO = NOME.MUNIC?PIO, 
                ORGAO_SUPERIOR = NOME.?RG?O.SUPERIOR, ORGAO_CONCEDENTE = NOME.?RG?O.CONCEDENTE,
                UG_CONCEDENTE = NOME.UG.CONCEDENTE, TIPO_DE_CONVENIO =  TIPO.CONVENENTE, 
                NOME_CONVENENTE = NOME.CONVENENTE, ENTE_CONVENENTE = TIPO.ENTE.CONVENENTE, 
                VALOR_CONVENIO = VALOR.CONV?NIO, VALOR_LIBERADO = VALOR.LIBERADO, 
                DATA_PUBLICACAO = DATA.PUBLICA??O, DATA_INICIO = DATA.IN?CIO.VIG?NCIA,
                DATA_FINAL =  DATA.FINAL.VIG?NCIA, VALOR_CONTRAPARTIDA = VALOR.CONTRAPARTIDA,
                DESCRICAO = descricao)

DCNE3 <-  DCNE2

# 6) Criando uma nova se??o de filtro para dummies iniciais 

## Barragem e Barragens 

DCNE3$BARRAGEM = 0 
DCNE3$BARRAGEM[str_detect(DCNE3$DESCRICAO, "barragem|barragens")] <- 1

## Acude e Acudes 

DCNE3$ACUDE = 0 
DCNE3$ACUDE[str_detect(DCNE3$DESCRICAO, "acude|acudes")] <- 1

# Constru??o, perfuracacao ou ampliacao

DCNE3$CONSTRUCAO = 0 
DCNE3$CONSTRUCAO[str_detect(DCNE3$DESCRICAO, "construcao | execucao | ampliacao")] <- 1


### Total de Projetos 

DCNE3$PROJETOS = DCNE3$BARRAGEM + DCNE3$ACUDE
DCNE3$DBARRAGEM = DCNE3$BARRAGEM*DCNE3$CONSTRUCAO
DCNE3$DACUDE = DCNE3$ACUDE*DCNE3$CONSTRUCAO

# 8) Excluindo colunas desnecessarias e formatando datas

DCNE3$CONVENIO = NULL 
DCNE3$OBJETO.DO.CONV?NIO = NULL 
DCNE3$INSTRUMENTO = NULL
DCNE3$N?MERO.ORIGINAL = NULL

library(lubridate)

head(DCNE3)

# 8.1) Data de Publica??o 

 DCNE3 %>% 
 mutate(DATA_P = dmy(DATA_PUBLICACAO)) %>% 
 mutate_at(vars(DATA_P), funs(year, month, day)) ->  DCNE3

 DCNE3$month = NULL
 DCNE3$day   = NULL 

 DCNE3 = rename (DCNE3, ANO_DE_PUB = year)


# 8.2 DATA_INICIO

DCNE3 %>% 
  mutate(DATA_I = dmy(DATA_INICIO)) %>% 
  mutate_at(vars(DATA_I), funs(year, month, day)) ->  DCNE3

DCNE3$month = NULL
DCNE3$day   = NULL 

DCNE3 = rename (DCNE3, ANO_DE_INICIO = year)

# 8.3 DATA_FINAL 

DCNE3 %>% 
  mutate(DATA_F = dmy(DATA_FINAL)) %>% 
  mutate_at(vars(DATA_F), funs(year, month, day)) ->  DCNE3

DCNE3$month = NULL
DCNE3$day   = NULL 

DCNE3 = rename (DCNE3, ANO_DE_FIM = year)

# 8) Excluindo e Exportando Database 

DCNE3$DATA_FINAL = NULL 
DCNE3$DATA_INICIO  = NULL 
DCNE3$DATA_PUBLICACAO = NULL 

write.csv(DCNE3, "Data_Barragem_NE.csv")

# 9) Estat?stica Descritiva

head(DCNE3)

DCNE3 %>% group_by(ANO_DE_FIM) %>%
  summarise(TPROJETOS=sum(PROJETOS)) -> TOTAL_PROJETOS

DCNE3 %>% group_by(UF) %>%
  summarise(TPROJETOSUF=sum(PROJETOS)) -> TOTAL_PROJETOSUF

