########################################################################################################################
# Rotina {R} : Analise de Clubs de Convergencia na Diversificacao Agricola
# Data: 2022 
# Autor: Alvaro Roberio
# Website: alvaroroberio.github.io
########################################################################################################################

# Package 

library("ConvergenceClubs")
library("foreign")
library("haven")
library("ggplot2")
library("dplyr")
library("panelr")
library("mfilter")
library("funtimes")
library("tidyverse")

install.packages("mFilter")
install.packages("funtimes")

# Importando 

setwd("G:/Meu Drive/DataArtigo/4- Diversificação Agrícola/Banco de Dados")
dir( )

DF   <- read_stata("Data_ClubMSI.dta")
DF <- DF %>% relocate(mcod, ano)

########################################################################################
# Análise para Diversificação Agrícola 
########################################################################################

# Prepagando os dados (Retirando tendências e zonas cíclicas)

Data <- DF[ , c("ano", "mcod", "msi")]
Data <- panel_data(Data, id = mcod, wave = ano)
Data <- widen_panel(Data)
Log <- Data[, -1]


FD <- apply(Log, 1, function(x){mFilter::hpfilter(x, freq=250, type="lambda")$trend})
FD  <- data.frame(mcod = Data[,1], t(FD), stringsAsFactors = FALSE )
colnames(FD) <- colnames(Data)
rm(Log)

# Transição Path 

H <- computeH(FD[,-1], quantity="H")
DH <- data.frame(H)
DH
round(estimateMod(H, time_trim=1/3, HACmethod = "FQSB"), 2)

# Clubes de Convergência 


clubs <- findClubs(FD, dataCols = 2:22, refCol= ncol(FD),
                   time_trim=1/3, cstar=0, HACmethod = "FQSB")
clubs
class(clubs)
str(clubs, give.attr=FALSE)

Figura1 <- plot(clubs, clubs=NULL, avgTP = TRUE, legend=TRUE,
     plot_args = list(xmarks=seq(1,21), 
     xlabs=seq(2000,2020), lty="dotted", lwd=3, col= c("blue", "red", "green")))

Figura1

Figura2 <- plot(clubs, avgTP = TRUE, ncol=2, plot_args = list(xmarks=seq(1,21), 
                                                   xlabs=seq(2000,2020)))
Figura2 

clubs <- findClubs(FD, dataCols = 2:22, unit_names = 1, refCol= ncol(FD),
                   time_trim=1/3, cstar=0, HACmethod = "FQSB")
clubs

class(clubs)
str(clubs, give.attr=FALSE)
summary(clubs)
print(clubs)

# Merge de Clubs Convergente (PS e LT)

clubs_PS <- mergeClubs(clubs, mergeMethod='PS', mergeDivergent=TRUE)
summary(clubs_PS)
clubs_PS

clubs_LT <- mergeClubs(clubs, mergeMethod='vLT', mergeDivergent=TRUE)
summary(clubs_LT)

# Merge de Clubs Divergente (PS e LT)

clubs_DPS <- mergeClubs(clubs, mergeMethod='PS', mergeDivergent=TRUE)
summary(clubs_DPS)

clubs_DLT <- mergeClubs(clubs, mergeMethod='vLT', mergeDivergent=TRUE)
summary(clubs_DLT)

# Exportando Resultados 

DTPS <- transition_paths(clubs_PS)
DTPS <- transition_paths(clubs_PS, output_type = 'data.frame')

DTPS <- gather(DTPS, ano, tp_msi, msi_2000:msi_2020, factor_key=TRUE)
DTPS$ano <- gsub("msi_","",as.character(DTPS$ano))

DTPS <- rename(DTPS, clubeps = club, municipio = unit_name)


DF <- read_stata("Data_Diversificacao.dta")
head(DF)

# DT1 <- DF[ , c("id", "codibge", "semiarido", "coduf", "msi")]
# DT1 <- panel_data(DT1, id = CODIGO, wave = ANO)
# DT1 <- widen_panel(DT1)
# CODIGO <- DT1%>%  select(CODIGO)
# DT2 <- cbind(CODIGO, DT)
# DT2$Municipio = NULL

write.table(DTPS, file="DTPS.csv", )
