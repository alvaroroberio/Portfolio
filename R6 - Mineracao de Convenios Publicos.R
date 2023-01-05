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

setwd("C:\\Users\\alvar\\Desktop\\Microdados de Agricultura\\Data_Bruto\\Tipos de Convênio")
dir()

# Importando Banco de Dados 

DC <- read.csv("Convenios.csv", sep=";", header=TRUE)
head(DC)

########################################################################################################################
# Barragens 
########################################################################################################################

# 1) Filtrando estados do Nordeste Brasileiro 

DCNE <- filter(DC, UF=="MA" | UF=="PI" | UF=="CE" | UF=="RN" | UF=="PB" | UF=="PE" | UF=="AL" | UF=="SE" | UF=="BA")
 
# 2) Filtrando convênios concluídos 

DCNE <- rename(DCNE, INSTRUMENTO = TIPO.INSTRUMENTO, CONVENIO = SITUAÇÃO.CONVÊNIO)
DCNE <- filter(DCNE, INSTRUMENTO == "CONVENIO" & CONVENIO == "CONCLUÍDO")


# 3) Filtrando por tipo convenente (Estadual ou Municipal)

DCNE <- filter(DCNE, TIPO.CONVENENTE == "Administração Pública Municipal" | 
                     TIPO.CONVENENTE == "Administração Pública Estadual ou do Distrito Federal")

# 4) Minerando a descrição do database de convênio por níveis 

DCNE1 <- DCNE
DCNE1$descricao <- str_to_lower(DCNE1$OBJETO)

# Nível 1: Objetivo do convênio (construção, execucao e ampliacao)

DCNE1 %>% filter(str_detect(descricao, "construcao | execucao | ampliacao")) -> DCNE2

# Nível 2: Infraestrura Hídrica (Barragem e Acude)

DCNE2%>% filter(str_detect(descricao, "barragem | barragens| acude | acudes")) -> DCNE2

# Nivel 3: Eliminando ministerios que podem distorcer a amostra
# Neste caso, apenas o Ministério da Educação e do Turismo foram evidenciados como candidatos 

list(DCNE2$NOME.ÓRGÃO.SUPERIOR)

DCNE2 <- filter(DCNE2, NOME.ÓRGÃO.SUPERIOR != "Ministério da Educação" )
DCNE2 <- filter(DCNE2, NOME.ÓRGÃO.SUPERIOR != "Ministério do Turismo" )


# 5) Renomeando e Excluindo dados desnecessários 

DCNE2$NÚMERO.CONVÊNIO = NULL 
DCNE2$NÚMERO.PROCESSO.DO.CONVÊNIO = NULL 
DCNE2$CÓDIGO.ÓRGÃO.CONCEDENTE = NULL 
DCNE2$CÓDIGO.ÓRGÃO.SUPERIOR = NULL 
DCNE2$CÓDIGO.CONVENENTE = NULL 
DCNE2$DATA.ÚLTIMA.LIBERAÇÃO = NULL 
DCNE2$VALOR.ÚLTIMA.LIBERAÇÃO = NULL
DCNE2$OBJETO = NULL
DCNE2$CÓDIGO.UG.CONCEDENTE=NULL

head(DCNE2)

DCNE2 <- rename(DCNE2, SIAFI = CÓDIGO.SIAFI.MUNICÍPIO, MUNICIPIO = NOME.MUNICÍPIO, 
                ORGAO_SUPERIOR = NOME.ÓRGÃO.SUPERIOR, ORGAO_CONCEDENTE = NOME.ÓRGÃO.CONCEDENTE,
                UG_CONCEDENTE = NOME.UG.CONCEDENTE, TIPO_DE_CONVENIO =  TIPO.CONVENENTE, 
                NOME_CONVENENTE = NOME.CONVENENTE, ENTE_CONVENENTE = TIPO.ENTE.CONVENENTE, 
                VALOR_CONVENIO = VALOR.CONVÊNIO, VALOR_LIBERADO = VALOR.LIBERADO, 
                DATA_PUBLICACAO = DATA.PUBLICAÇÃO, DATA_INICIO = DATA.INÍCIO.VIGÊNCIA,
                DATA_FINAL =  DATA.FINAL.VIGÊNCIA, VALOR_CONTRAPARTIDA = VALOR.CONTRAPARTIDA,
                DESCRICAO = descricao)

DCNE3 <-  DCNE2

# 6) Criando uma nova seção de filtro para dummies iniciais 

## Barragem e Barragens 

DCNE3$BARRAGEM = 0 
DCNE3$BARRAGEM[str_detect(DCNE3$DESCRICAO, "barragem|barragens")] <- 1

## Acude e Acudes 

DCNE3$ACUDE = 0 
DCNE3$ACUDE[str_detect(DCNE3$DESCRICAO, "acude|acudes")] <- 1

# Construção, perfuracacao ou ampliacao

DCNE3$CONSTRUCAO = 0 
DCNE3$CONSTRUCAO[str_detect(DCNE3$DESCRICAO, "construcao | execucao | ampliacao")] <- 1


### Total de Projetos 

DCNE3$PROJETOS = DCNE3$BARRAGEM + DCNE3$ACUDE
DCNE3$DBARRAGEM = DCNE3$BARRAGEM*DCNE3$CONSTRUCAO
DCNE3$DACUDE = DCNE3$ACUDE*DCNE3$CONSTRUCAO

# 8) Excluindo colunas desnecessarias e formatando datas

DCNE3$CONVENIO = NULL 
DCNE3$OBJETO.DO.CONVÊNIO = NULL 
DCNE3$INSTRUMENTO = NULL
DCNE3$NÚMERO.ORIGINAL = NULL

library(lubridate)

head(DCNE3)

# 8.1) Data de Publicação 

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

# 9) Estatística Descritiva

head(DCNE3)

DCNE3 %>% group_by(ANO_DE_FIM) %>%
  summarise(TPROJETOS=sum(PROJETOS)) -> TOTAL_PROJETOS

DCNE3 %>% group_by(UF) %>%
  summarise(TPROJETOSUF=sum(PROJETOS)) -> TOTAL_PROJETOSUF

