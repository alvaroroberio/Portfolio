########################################################################################################################
# Rotina {R} : Lidando com microdados da Rais
# Data: 2022 
# Autor: Alvaro Roberio
# Website: alvaroroberio.github.io
########################################################################################################################
# Carregando Pacotes

library(reader)
library(data.table)
library(readxl)
library(tidyverse)
library(acid)
library(affluenceIndex)
library(ineq)
library(dineq)
library(REAT)
library(IC2)
library(foreign)

memory.size() ### Verificando o tamanho da memória
memory.limit() ## Verificando o limite definido
memory.limit(size=56000) ### expandindo sua memória _ aqui vai além da sua memória real. Este 56000 é proposto para 64 bits.


#Definindo diretório de trabalho 

setwd("C:/Users/alvar/Desktop/RaisPE/PE2011")

rm(list = ls())
options(scipen = 999, digits = 9)
dir()

# Obtendo dados e tratando (diretório local)        

PE2011 <- fread("PE2011.txt",dec = ",", encoding = "Latin-1", sep = ";", stringsAsFactors = FALSE)

# Reduzindo o tamanho da base de dados 

PE2011[, 1:10] <- list(NULL)
PE2011[, 26:35] <- list(NULL)

# Excluindo trabalhadores inativos (base compostas por ativos até 31/12)

PE2011 <- data.frame(PE2011)
PE2011 <- PE2011[PE2011$"Vínculo.Ativo.31.12"==1, ] 
PE2011$"Vínculo.Ativo.31.12" <- NULL 

# Excluindo trabalhadores fora da faixa de idade (18 a 64 anos)

filter(PE2011, Idade >= 18 & Idade <=64) -> PE2011
summary(PE2011$Idade)

# Renomeando variável de interesse/código e deflacionando 
# INPC // IPEAData - Ano base 2011 | Deflator encontra-se calculado em tabela no Excel 

PE2011 = rename(PE2011, salario = "Vl.Remun.Dezembro.Nom", codigo = "Município")
PE2011$salariodf = PE2011$salario/1

# Excluindo trabalhadores com Salário negativo ou igual a zero  

filter(PE2011, salariodf !="0") -> PE2011
summary(PE2011$salariodf)

# Excluindo observações discrepantes: renda superior a mediana da base x 50 vezes

summary(PE2011$salariodf)
PE2011 %>% group_by(codigo) %>% summarise(msalariodf=mean(salariodf)*10) -> msalariodf
PE2011  <- merge(PE2011, msalariodf, by=c( "codigo"))

filter(PE2011, salariodf <= msalariodf) -> PE2011
summary(PE2011$salariodf)


# Checando tratamento da base de dados 

summary(PE2011$salariodf)
summary(PE2011$Idade)

# Calculando Indices de Desigualdade e Polarização de renda salarial 

## Gini

GINI2011 <- aggregate(salariodf ~ codigo, data = PE2011, FUN="gini")
GINI2011 = rename(GINI2011, Gini = "salariodf")

summary(GINI2011$Gini)
GINI2011$ano = 2011

## Atkinson

ATKINSON12011 <- aggregate(salariodf ~ codigo, data = PE2011, FUN="Atkinson", parameter = 0.5)
ATKINSON22011 <- aggregate(salariodf ~ codigo, data = PE2011, FUN="Atkinson", parameter = 1.0)
ATKINSON32011 <- aggregate(salariodf ~ codigo, data = PE2011, FUN="Atkinson", parameter = 1.5)

ATKINSON12011 = rename(ATKINSON12011, Atkinson1 = "salariodf")
ATKINSON22011 = rename(ATKINSON22011, Atkinson2 = "salariodf")
ATKINSON32011 = rename(ATKINSON32011, Atkinson3 = "salariodf")

ATKINSON12011$ano = 2011
ATKINSON22011$ano = 2011
ATKINSON32011$ano = 2011

## Hoover 

HOOVER2011 <- aggregate(salariodf ~ codigo, data = PE2011, FUN="hoover")

HOOVER2011 = rename(HOOVER2011, Hoover = "salariodf")
HOOVER2011$ano = 2011 


## Indices de Entropia Generalizada (Theil-L(0), Theil-T(1)) 

IEG1 <- aggregate(salariodf ~ codigo, data = PE2011, FUN="entropy", parameter=0)
IEG2 <- aggregate(salariodf ~ codigo, data = PE2011, FUN="entropy", parameter=1)
IEG3 <- aggregate(salariodf ~ codigo, data = PE2011, FUN="entropy", parameter=2)

IEG1 = rename(IEG1, IEG1 = "salariodf")
IEG2 = rename(IEG2, IEG2 = "salariodf")
IEG3 = rename(IEG3, IEG3 = "salariodf")

IEG1$ano = 2011
IEG2$ano = 2011
IEG3$ano = 2011


## Merge das bases

GINI2011 <- data.frame(GINI2011)
ATKINSON12011 <- data.frame(ATKINSON12011)
ATKINSON22011 <- data.frame(ATKINSON22011)
ATKINSON32011 <- data.frame(ATKINSON32011)
HOOVER2011 <- data.frame(HOOVER2011)
IEG1 <- data.frame(IEG1)
IEG2 <- data.frame(IEG2)
IEG3 <- data.frame(IEG3)

RPE2011 <- merge(GINI2011, ATKINSON12011,
             by=c( "codigo", "ano"))

RPE2011 <- merge(RPE2011, ATKINSON22011,
                 by=c( "codigo", "ano"))

RPE2011 <- merge(RPE2011, ATKINSON32011,
                 by=c( "codigo", "ano"))

RPE2011 <- merge(RPE2011, HOOVER2011,
                 by=c( "codigo", "ano"))

RPE2011 <- merge(RPE2011, IEG1,
                 by=c( "codigo", "ano"))

RPE2011 <- merge(RPE2011, IEG2,
                 by=c( "codigo", "ano"))

RPE2011 <- merge(RPE2011, IEG3,
                 by=c( "codigo", "ano"))

#RPE20111 = data.frame(lapply(RPE2011, as.character), stringsAsFactors=FALSE)

# Exportação de resultados 

PE2011 <- data.frame(PE2011)
write.dta(PE2011, 'RPE2017.dta')

# Estatística Descritiva 

## Criando variavel de vínculo ativo (emprego)

PE2011$Emprego=1

## Estatística descritiva de emprego e salário 

PE2011 %>% group_by(codigo) %>% summarise(EMP=sum(Emprego)) -> EMP 
PE2011 %>% group_by(codigo) %>% summarise(ASAL=sum(salariodf)) -> ASAL 
PE2011 %>% group_by(codigo) %>% summarise(MSAL=mean(salariodf)) -> MSAL

## Agregando dados exportação 

RPE2011 <- merge(RPE2011, EMP,
                 by=c( "codigo"))

RPE2011 <- merge(RPE2011, ASAL,
                 by=c( "codigo"))

RPE2011  <- merge(RPE2011, MSAL,
             by=c( "codigo"))

RPE2011  <- data.frame (RPE2011 )

write.csv(RPE2011, 'RPE2011.csv')


