########################################################################################################################
# Rotina {R} : Analise de Insumo-Produto
# Data: 2022 
# Autor: Alvaro Roberio
# Website: alvaroroberio.github.io
########################################################################################################################

# Definindo Diretório de Trabalho

setwd("G:/Meu Drive/DataEconomia/Insumo-Produto/Artigo - MIP/MIP-R/MIPs")

# Pacotes 

library(openxlsx)
library(flextable)
library(knitr)
library(kableExtra)
library(dplyr)
library(ggplot2)
library(scales)
library(ggrepel)
library(tibble)
library(gridExtra)


# Consumo Intermediário (CI):
Z = read.xlsx("MIT2018.xlsx", sheet = "Z", colNames = FALSE)

# Demanda Final (DF):
y = read.xlsx("MIT2018.xlsx", sheet = "y", colNames = FALSE)

# Valor Bruto da Produção (VBP):
x = read.xlsx("MIT2018.xlsx", sheet = "x", colNames = FALSE)

# Valor Adicionado (VA):
v = read.xlsx("MIT2018.xlsx", sheet = "v", colNames = FALSE)

# Consumo das Famílias (C):
c = read.xlsx("MIT2018.xlsx", sheet = "c", colNames = FALSE)

# Matriz de Emprego Setorial - IT (e)
e = read.xlsx("MIT2018.xlsx", sheet = "e", colNames = FALSE)

# Matriz de Salário Setorial - IT (w)
# w = read.xlsx("MIT2018.xlsx", sheet = "w", colNames = FALSE)

# Remunerações (r)
r = read.xlsx("MIT2018.xlsx", sheet = "r", colNames = FALSE)

# Setor de Pagamentos (PT-PN)
sp = read.xlsx("MIT2018.xlsx", sheet = "sp", colNames = FALSE)
        
# Setores de Intensidade Tecnológica 
setores = read.xlsx("MIT2018.xlsx", sheet = "set", colNames = FALSE)


Z = data.matrix(Z)     # Consumo intermediário
y = data.matrix(y)     # Demanda final
x = data.matrix(x)     # Valor Bruto da Produção
x = as.vector(x)       # Valor Bruto da Produção
v = data.matrix(v)     # Valor Adicionado
c = data.matrix(c)     # Consumo das famílias
e = data.matrix(e)     # Matrix de Emprego Setorial (IT)
# w = data.matrix(w)     # Salário Setorial (IT)
r = data.matrix(r)     # Remuneração
sp = data.matrix(sp)   # Setor de Pagamentos 

save(Z, y, x, v, c, e, r, sp, setores, file = "MIT2018.RData") 


# 1. Modelo Aberto 

A = Z %*% diag(1 / x)
n = length(x)
I = diag(n) 
B = solve(I - A) 

# 2. Modelo Fechado 

hc = c / sum(r)
hr = r / x     
hr = t(hr) 

AF = matrix(NA, ncol = n + 1, nrow = n + 1) 
AF = rbind(cbind(A, hc), cbind(hr, 0))
IF = diag(n + 1)    
BF = solve(IF - AF)

# 3. Modelo Oferta (Ghosh) 

F = diag(1 / x) %*% Z  
G = solve(I - F)

# 4. Multiplicadores 

MP = colSums(B)              
MPT = colSums(BF[, 1:n])     
MPTT = colSums(BF[1:n, 1:n])
MultProd= cbind(setores, MP, MPT, MPTT)
MultProd = as.data.frame(MultProd)
colnames(MultProd) = c("setores", "MP", "MPT", "MPTT") 
MultProd$MP = as.numeric(as.character(MultProd$MP))
MultProd$MPT = as.numeric(as.character(MultProd$MPT))
MultProd$MPTT = as.numeric(as.character(MultProd$MPTT))
MultProd
MultProd$AnO = 2018

## Multiplicador de Emprego por intensidade tecnológica 

# 1. Multiplicador de Emprego Geral (Baixa)

ce = e[, 1]/x
ce = as.vector(ce)
Cehat = diag(ce) 
E = Cehat %*% B
ME = colSums(E)            
MEI = ME / ce              
MEI = data.matrix(MEI)
EF = Cehat %*% BF[1:n, 1:n]  
MET = colSums(EF)            
MET = data.matrix(MET)
MEII = MET / ce              
MultEmp = cbind(setores, ME, MEI, MET, MEII)
MultEmp = as.data.frame(MultEmp)
colnames(MultEmp) = c("setores", "ME", "MEI", "MET", "MEII")
MultEmp$ME = as.numeric(as.character(MultEmp$ME))
MultEmp$MEI = as.numeric(as.character(MultEmp$MEI))
MultEmp$MET = as.numeric(as.character(MultEmp$MET))
MultEmp$MEII = as.numeric(as.character(MultEmp$MEII))
MultEmp1 = MultEmp
MultEmp1$Nivel = 1 

# 2. Multiplicador de Emprego Geral (Média-Baixa)

ce = e[, 2]/ x
ce = as.vector(ce)
Cehat = diag(ce) 
E = Cehat %*% B
ME = colSums(E)            
MEI = ME / ce              
MEI = data.matrix(MEI)
EF = Cehat %*% BF[1:n, 1:n]  
MET = colSums(EF)            
MET = data.matrix(MET)
MEII = MET / ce              
MultEmp = cbind(setores, ME, MEI, MET, MEII)
MultEmp = as.data.frame(MultEmp)
colnames(MultEmp) = c("setores", "ME", "MEI", "MET", "MEII")
MultEmp$ME = as.numeric(as.character(MultEmp$ME))
MultEmp$MEI = as.numeric(as.character(MultEmp$MEI))
MultEmp$MET = as.numeric(as.character(MultEmp$MET))
MultEmp$MEII = as.numeric(as.character(MultEmp$MEII))
MultEmp2 = MultEmp
MultEmp2$Nivel = 2

# 3. Multiplicador de Emprego Geral (Média)

ce = e[, 3]/ x
ce = as.vector(ce)
Cehat = diag(ce) 
E = Cehat %*% B
ME = colSums(E)            
MEI = ME / ce              
MEI = data.matrix(MEI)
EF = Cehat %*% BF[1:n, 1:n]  
MET = colSums(EF)            
MET = data.matrix(MET)
MEII = MET / ce              
MultEmp = cbind(setores, ME, MEI, MET, MEII)
MultEmp = as.data.frame(MultEmp)
colnames(MultEmp) = c("setores", "ME", "MEI", "MET", "MEII")
MultEmp$ME = as.numeric(as.character(MultEmp$ME))
MultEmp$MEI = as.numeric(as.character(MultEmp$MEI))
MultEmp$MET = as.numeric(as.character(MultEmp$MET))
MultEmp$MEII = as.numeric(as.character(MultEmp$MEII))
MultEmp3 = MultEmp
MultEmp3$Nivel = 3 

# 4. Multiplicador de Emprego Geral (Média-Alta)

ce = e[, 4]/ x
ce = as.vector(ce)
Cehat = diag(ce) 
E = Cehat %*% B
ME = colSums(E)            
MEI = ME / ce              
MEI = data.matrix(MEI)
EF = Cehat %*% BF[1:n, 1:n]  
MET = colSums(EF)            
MET = data.matrix(MET)
MEII = MET / ce              
MultEmp = cbind(setores, ME, MEI, MET, MEII)
MultEmp = as.data.frame(MultEmp)
colnames(MultEmp) = c("setores", "ME", "MEI", "MET", "MEII")
MultEmp$ME = as.numeric(as.character(MultEmp$ME))
MultEmp$MEI = as.numeric(as.character(MultEmp$MEI))
MultEmp$MET = as.numeric(as.character(MultEmp$MET))
MultEmp$MEII = as.numeric(as.character(MultEmp$MEII))
MultEmp4 = MultEmp
MultEmp4$Nivel = 4 

# 4. Multiplicador de Emprego Geral (Alta)

ce = e[, 5]/ x
ce = as.vector(ce)
Cehat = diag(ce) 
E = Cehat %*% B
ME = colSums(E)            
MEI = ME / ce              
MEI = data.matrix(MEI)
EF = Cehat %*% BF[1:n, 1:n]  
MET = colSums(EF)            
MET = data.matrix(MET)
MEII = MET / ce              
MultEmp = cbind(setores, ME, MEI, MET, MEII)
MultEmp = as.data.frame(MultEmp)
colnames(MultEmp) = c("setores", "ME", "MEI", "MET", "MEII")
MultEmp$ME = as.numeric(as.character(MultEmp$ME))
MultEmp$MEI = as.numeric(as.character(MultEmp$MEI))
MultEmp$MET = as.numeric(as.character(MultEmp$MET))
MultEmp$MEII = as.numeric(as.character(MultEmp$MEII))
MultEmp5 = MultEmp
MultEmp5$Nivel = 5

# 5. Multiplicador de Emprego Geral (Total)

ce = e[, 6]/ x
ce = as.vector(ce)
Cehat = diag(ce) 
E = Cehat %*% B
ME = colSums(E)            
MEI = ME / ce            
MEI = data.matrix(MEI)
EF = Cehat %*% BF[1:n, 1:n] 
MET = colSums(EF)            
MET = data.matrix(MET)
MEII = MET / ce             
MultEmp = cbind(setores, ME, MEI, MET, MEII)
MultEmp = as.data.frame(MultEmp)
colnames(MultEmp) = c("setores", "ME", "MEI", "MET", "MEII")
MultEmp$ME = as.numeric(as.character(MultEmp$ME))
MultEmp$MEI = as.numeric(as.character(MultEmp$MEI))
MultEmp$MET = as.numeric(as.character(MultEmp$MET))
MultEmp$MEII = as.numeric(as.character(MultEmp$MEII))
MultEmp6 = MultEmp
MultEmp6$Nivel = 6 


MultEmp = rbind(MultEmp1, MultEmp2, MultEmp3, MultEmp4, MultEmp5, MultEmp6)
MultEmp$Ano = 2018

rm(MultEmp1, MultEmp2, MultEmp3, MultEmp4, MultEmp5, MultEmp6)
MultEmp

## Multiplicador de Salário por intensidade tecnológica 

# 1. Multiplicador de Salário Geral (Baixa)

cs = w[, 1] / x
cs = as.vector(cs)
Cshat = diag(cs) 
S = Cshat %*% B
MS = colSums(S)            
MSI = MS / cs              
MSI = data.matrix(MSI)
SF = Cshat %*% BF[1:n, 1:n]  
MST = colSums(SF)            
MST = data.matrix(MST)
MSII = MST / cs             
MultSal = cbind(setores, MS, MSI, MST, MSII)
MultSal = as.data.frame(MultSal)
colnames(MultSal) = c("setores", "MS", "MSI", "MST", "MSII")
MultSal$MS = as.numeric(as.character(MultSal$MS))
MultSal$MSI = as.numeric(as.character(MultSal$MSI))
MultSal$MST = as.numeric(as.character(MultSal$MST))
MultSal$MSII = as.numeric(as.character(MultSal$MSII))
MultSal1 = MultSal
MultSal1$Nivel = 1 


# 2. Multiplicador de Salário Geral (Média-Baixa)

cs = w[, 2] / x
cs = as.vector(cs)
Cshat = diag(cs) 
S = Cshat %*% B
MS = colSums(S)            
MSI = MS / cs              
MSI = data.matrix(MSI)
SF = Cshat %*% BF[1:n, 1:n]  
MST = colSums(SF)            
MST = data.matrix(MST)
MSII = MST / cs             
MultSal = cbind(setores, MS, MSI, MST, MSII)
MultSal = as.data.frame(MultSal)
colnames(MultSal) = c("setores", "MS", "MSI", "MST", "MSII")
MultSal$MS = as.numeric(as.character(MultSal$MS))
MultSal$MSI = as.numeric(as.character(MultSal$MSI))
MultSal$MST = as.numeric(as.character(MultSal$MST))
MultSal$MSII = as.numeric(as.character(MultSal$MSII))
MultSal2 = MultSal
MultSal2$Nivel = 2 

# 3. Multiplicador de Salário Geral (Média)

cs = w[, 3] / x
cs = as.vector(cs)
Cshat = diag(cs) 
S = Cshat %*% B
MS = colSums(S)            
MSI = MS / cs              
MSI = data.matrix(MSI)
SF = Cshat %*% BF[1:n, 1:n]  
MST = colSums(SF)            
MST = data.matrix(MST)
MSII = MST / cs             
MultSal = cbind(setores, MS, MSI, MST, MSII)
MultSal = as.data.frame(MultSal)
colnames(MultSal) = c("setores", "MS", "MSI", "MST", "MSII")
MultSal$MS = as.numeric(as.character(MultSal$MS))
MultSal$MSI = as.numeric(as.character(MultSal$MSI))
MultSal$MST = as.numeric(as.character(MultSal$MST))
MultSal$MSII = as.numeric(as.character(MultSal$MSII))
MultSal3 = MultSal
MultSal3$Nivel = 3

# 4. Multiplicador de Salário Geral (Média-Alta)

cs = w[, 4] / x
cs = as.vector(cs)
Cshat = diag(cs) 
S = Cshat %*% B
MS = colSums(S)            
MSI = MS / cs              
MSI = data.matrix(MSI)
SF = Cshat %*% BF[1:n, 1:n]  
MST = colSums(SF)            
MST = data.matrix(MST)
MSII = MST / cs             
MultSal = cbind(setores, MS, MSI, MST, MSII)
MultSal = as.data.frame(MultSal)
colnames(MultSal) = c("setores", "MS", "MSI", "MST", "MSII")
MultSal$MS = as.numeric(as.character(MultSal$MS))
MultSal$MSI = as.numeric(as.character(MultSal$MSI))
MultSal$MST = as.numeric(as.character(MultSal$MST))
MultSal$MSII = as.numeric(as.character(MultSal$MSII))
MultSal4 = MultSal
MultSal4$Nivel = 4 

# 5. Multiplicador de SalárioGeral (Alta)

cs = w[, 5] / x
cs = as.vector(cs)
Cshat = diag(cs) 
S = Cshat %*% B
MS = colSums(S)            
MSI = MS / cs              
MSI = data.matrix(MSI)
SF = Cshat %*% BF[1:n, 1:n]  
MST = colSums(SF)            
MST = data.matrix(MST)
MSII = MST / cs             
MultSal = cbind(setores, MS, MSI, MST, MSII)
MultSal = as.data.frame(MultSal)
colnames(MultSal) = c("setores", "MS", "MSI", "MST", "MSII")
MultSal$MS = as.numeric(as.character(MultSal$MS))
MultSal$MSI = as.numeric(as.character(MultSal$MSI))
MultSal$MST = as.numeric(as.character(MultSal$MST))
MultSal$MSII = as.numeric(as.character(MultSal$MSII))
MultSal5 = MultSal
MultSal5$Nivel = 5 

# 6. Multiplicador de Salário Total 

cs = w[, 6] / x
cs = as.vector(cs)
Cshat = diag(cs) 
S = Cshat %*% B
MS = colSums(S)            
MSI = MS / cs              
MSI = data.matrix(MSI)
SF = Cshat %*% BF[1:n, 1:n]  
MST = colSums(SF)          
MST = data.matrix(MST)
MSII = MST / cs             
MultSal = cbind(setores, MS, MSI, MST, MSII)
MultSal = as.data.frame(MultSal)
colnames(MultSal) = c("setores", "MS", "MSI", "MST", "MSII")
MultSal$MS = as.numeric(as.character(MultSal$MS))
MultSal$MSI = as.numeric(as.character(MultSal$MSI))
MultSal$MST = as.numeric(as.character(MultSal$MST))
MultSal$MSII = as.numeric(as.character(MultSal$MSII))
MultSal6 = MultSal
MultSal6$Nivel = 6


MultSal = rbind(MultSal1, MultSal2, MultSal3, MultSal4, MultSal5, MultSal6)
MultSal$Ano = 2018

rm(MultSal1, MultSal2, MultSal3, MultSal4, MultSal5, MultSal6)
rm(MEI, MEII, MET, MSI, MSII, MST, MPTT, MPT, ME, MP)

# 5. Índices de Ligação 

BL = colMeans(B) / mean(B)  
FL = rowMeans(B) / mean(B)  
SLG = rowSums(G)
MLG = SLG / n
Gstar = sum(G) / n ** 2
FLG = MLG / Gstar 
IndLig = cbind(setores, BL, FL, FLG)
IndLig = as.data.frame(IndLig)
colnames(IndLig) = c("setores", "BL", "FL", "FLG") 
IndLig$BL = as.numeric(as.character(IndLig$BL))
IndLig$FL = as.numeric(as.character(IndLig$FL))
IndLig$FLG = as.numeric(as.character(IndLig$FLG))
IndLig
IndLig = mutate(IndLig,
                Setores.Chave = 
                  ifelse(BL > 1 &
                           FLG > 1, "Setor-Chave", "-")) 
IndLig

# Gráfico do Ìndice de Ligação 

G12018 <- ggplot(IndLig, aes(x = FLG, y = BL)) +
  theme_gray () +
  theme(plot.background = element_rect(fill = "#FFFFFF", colour = "#000000")) +
  xlab(expression("Índice de ligação para frente"~"("*U[i]*")")) +
  ylab(expression("Índice de ligação para trás"~"("*U[j]*")")) +
  ggtitle("Índices de Ligação e setores-chave do Brasil - 2018") +
  labs(subtitle = "Setores por intensidade tecnológica",
       caption = "Fonte: MIP-IBGE (2018).
       \nNota: Índices de ligação para frente calculados com a matriz inversa de Ghosh.") +
  theme(plot.title = element_text(hjust = 0),
        plot.subtitle = element_text(hjust = 0),
        plot.caption = element_text(hjust = 0)) +
  geom_text_repel(aes(label = setores), vjust = 0.8, size = 3.5)+
  scale_y_continuous(labels = scales::number_format(accuracy = 0.01),
                     limits = c(0.5,1.5),
                     breaks = seq(from = 0.5, to = 2, by = 0.25)) +
  scale_x_continuous(labels = scales::number_format(accuracy = 0.01),
                     limits = c(0.5,1.5),
                     breaks = seq(from = 0.5, to = 1.5, by = 0.25)) +
  geom_hline(yintercept=1, linetype="dashed", color = "black") +
  geom_vline(xintercept=1, linetype="dashed", color = "black") +
  annotate("text", x=1.42, y=1.5,
           label= "Setor-Chave", colour='black', size=3.5) +
  annotate('text', x=0.65, y=1.5,
           label='Forte encadeamento para trás', colour='black', size=3.5) +
  annotate('text', x=0.60, y=0.5,
           label='Fraco encadeamento', colour='black', size=3.5) +
  annotate('text', x=1.35, y=0.5,
           label='Forte encadeamento para frente', colour='black', size=3.5)

ggsave("G12018.png", width = 20, height = 14, units = "cm")

# Coeficiente de Variação 

SC = colSums(B)
SL = rowSums(B)
MC = SC / n
ML = SL / n 

Vj = (((1 / (n - 1)) * (rowSums((B - MC) ** 2))) ** 0.5) / MC
Vi = (((1 / (n - 1)) * (colSums((B - ML) ** 2))) ** 0.5) / ML


CoefVar = cbind(setores, Vj, Vi)
CoefVar = as.data.frame(CoefVar)
colnames(CoefVar) = c("setores", "Vj", "Vi") 
CoefVar$Vj = as.numeric(as.character(CoefVar$Vj)) 
CoefVar$Vi = as.numeric(as.character(CoefVar$Vi)) 
CoefVar$ANO = 2018

# Índices Puros de Ligação 

IPL = matrix(NA, ncol = 3, nrow = n)

for (s in 1:n) {
  
  for (i in 1:n) {
    for (j in 1:n) {
      
      if (s==i) {
        if (i==j) {
          
          yj = y[i]
          yr = y[-i]
          
          Ajj = A[i,j]
          DJ = solve(1-Ajj)
          
          Ajr = A[i,-j]
          Arj = A[-i,j]
          Arr = A[-i,-j]
          
          DR = solve(diag(n-1)-Arr)
          
          PBL = sum (DR %*% Arj %*% DJ %*% yj)
          PFL = DJ %*% Ajr %*% DR %*% yr
          PTL = PBL + PFL
          
          IPuros = c(PBL,PFL,PTL)
          
        }}}}
  
  IPL[s,] = IPuros
} 


# Índices Puros de Ligação Normalizados

IPLm = (colSums(IPL) / n)
IPLm = as.vector(IPLm)
IPLN = IPL %*% (diag(1 / IPLm))

IPLN = as.data.frame(IPLN)
IPLN = cbind(setores, IPLN)
colnames(IPLN) = c("setores", "PBLN", "PFLN", "PTL") 
IPLN$ANO=2018


# Campo de Influência 

ee = 0.001
E = matrix(0, ncol = n, nrow = n)
SI = matrix(0, ncol = n, nrow = n)

for (i in 1:n) {
  for (j in 1:n) {
    E[i, j] = ee
    AE = A + E
    BE = solve(I - AE)
    FE = (BE - B) / ee
    FEq = FE * FE
    S = sum(FEq)
    SI[i, j] = S
    E[i, j] = 0
  }
} 

sx = setores[1:n, ]
sy = setores[1:n, ]
data = expand.grid(X = sx, Y = sy)


G22018 <- ggplot(data,aes(X, Y, fill = SI)) +
  geom_tile() +
  theme_bw() +
  xlab("Setores por intensidade tecnológica") +
  ylab("Setores por intensidade tecnológica") +
  ggtitle("Campo de Influência dos setores do Brasil - 2018") +
  labs(subtitle = "Setores por intensidade tecnológica",
       caption = "Fonte: MIP-IBGE (2018).") +
  theme(plot.title = element_text(hjust = 0),
        plot.subtitle = element_text(hjust = 0),
        plot.caption = element_text(hjust = 0)) +
  theme(axis.text.x = element_text(angle=35, vjust = 0.7),
        axis.text.y = element_text(angle=35, hjust = 0.7)) +
  theme(legend.position = "none") +
  scale_fill_distiller(palette = "Greys", trans = "reverse")

SI2 = as_tibble(SI) %>%
  mutate_all(funs(case_when(. < mean(SI) ~ 1,
                            . >= mean(SI) & . < (mean(SI) + sd(SI)) ~ 2,
                            . >= (mean(SI) + sd(SI)) & . < (mean(SI) + 2 *
                                                              sd(SI)) ~ 3,
                            . >= (mean(SI) + 2 * sd(SI)) ~ 4)))

SI2 = as.factor(as.matrix(SI2))

G22018 <- ggplot(data,aes(X, Y)) +
  geom_tile(aes(fill = SI2)) +
  theme_bw() +
  xlab("Setores por intensidade tecnológica") +
  ylab("Setores por intensidade tecnológica") +
  ggtitle("Campo de Influência dos setores do Brasil - 2018") +
  labs(subtitle = "Setores por intensidade tecnológica",
       caption = "Fonte: MIP-IBGE (2018).") +
  theme(plot.title = element_text(hjust = 0),
        plot.subtitle = element_text(hjust = 0),
        plot.caption = element_text(hjust = 0)) +
  theme(axis.text.x = element_text(angle=35, vjust = 0.7),
        axis.text.y = element_text(angle=35, hjust = 0.7)) +
  scale_fill_manual(name = "SI",
                    values=c("#e9e9e9", "#a9a9a9", "#3f3f3f", "#191919"),
                    labels = c ("< Média", "< Média + DP", "< Média + 2DP", "> Média + DP"))
ggsave("G22018.png", width = 20, height = 14, units = "cm")

colnames(SI) = c("Baixa", "Média-Baixa", "Média", "Média-Alta", "Alta", "Outros setores")

# Extração Hipótetica 

BLextrac = matrix(NA, ncol=1, nrow=n)
FLextrac = matrix(NA, ncol=1, nrow=n)

for (i in 1:n) {
  for (j in 1:n) {
    ABL = A
    ABL[, j] = 0
    BBL = solve(I - ABL)
    xbl = BBL %*% y
    tbl = sum(x) - sum(xbl)
    BLextrac[j] = tbl
    BLextracp = BLextrac / sum(x) * 100
    
    FFL = F
    FFL[i, ] = 0
    GFL = solve(I - FFL)
    xfl = t(sp) %*% GFL
    tfl = sum(x) - sum(xfl)
    FLextrac[i] = tfl
    FLextracp = FLextrac / sum(x) * 100
    
    Extrac = cbind(BLextrac, FLextrac, BLextracp, FLextracp)
    colnames(Extrac) = c("BL", "FL", "BL%", "FL%")
  }
}

Extrac = cbind(setores, Extrac)
colnames(Extrac) = c("Setores", "BL", "FL", "BL%", "FL%") 

Extrac$ANO = 2018


# Exportando Resultados 

colnames(A) = c("Baixa", "Média-Baixa", "Média", "Média-Alta", "Alta", "Outros setores")
colnames(B) = c("Baixa", "Média-Baixa", "Média", "Média-Alta", "Alta", "Outros setores")
colnames(AF) = c("Baixa", "Média-Baixa", "Média", "Média-Alta", "Alta", "Outros setores", "Consumo da Família")
colnames(BF) = c("Baixa", "Média-Baixa", "Média", "Média-Alta", "Alta", "Outros setores", "Consumo da Família")
colnames(F) = c("Baixa", "Média-Baixa", "Média", "Média-Alta", "Alta", "Outros setores")
colnames(G) = c("Baixa", "Média-Baixa", "Média", "Média-Alta", "Alta", "Outros setores")
A <- data.frame(A)
B <- data.frame(B)
AF <- data.frame(AF)
BF <- data.frame(BF)
F <- data.frame(F)
G <- data.frame(G)


wb2018 = createWorkbook()
addWorksheet(wb2018, "A")
addWorksheet(wb2018, "B")
addWorksheet(wb2018, "AF")
addWorksheet(wb2018, "BF")
addWorksheet(wb2018, "F")
addWorksheet(wb2018, "G")
addWorksheet(wb2018, "MP")
addWorksheet(wb2018, "ME")
#addWorksheet(wb2018, "MS")
addWorksheet(wb2018, "IndHR")
addWorksheet(wb2018, "CV")
addWorksheet(wb2018, "IPLN")
addWorksheet(wb2018, "SI")
addWorksheet(wb2018, "ExtHipo")

writeDataTable(wb2018, "A", x = A)
writeDataTable(wb2018, "B", x = B)
writeDataTable(wb2018, "AF", x = AF)
writeDataTable(wb2018, "BF", x = BF)
writeDataTable(wb2018, "F", x = F)
writeDataTable(wb2018, "G", x = G)
writeDataTable(wb2018, "MP", x = MultProd)
writeDataTable(wb2018, "ME", x = MultEmp)
# writeDataTable(wb2018, "MS", x = MultSal)
writeDataTable(wb2018, "IndHR", x = IndLig)
writeDataTable(wb2018, "CV", x = CoefVar)
writeDataTable(wb2018, "IPLN", x = IPLN)
writeData(wb2018, "SI", x = SI)
writeDataTable(wb2018, "ExtHipo", x = Extrac)
saveWorkbook(wb2018, file = "R2018.xlsx", overwrite = TRUE) 


