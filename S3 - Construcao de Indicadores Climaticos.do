########################################################################################################################
# Rotina {Stata} : Construcao de Indicadores Climaticos 
# Data: 2022 
# Autor: Alvaro Roberio
# Website: alvaroroberio.github.io
########################################################################################################################

*OBSERVACAO!!! 
*Para usar a base de dados em ano utilizar o codigo abaixo: 

drop if mes>1 

*Os indicadores abaixo já foram transformados das suas versoes mensais para anuais

************************************************************************************************************************
* Variaveis climaticas basicas 
************************************************************************************************************************

// Temperatura media anual (temperatura)

gen tmed = (tmax + tmin)/2
bysort codigo ano: egen temperatura=mean(tmed)

// Temperatura maxima media anual (atmax) e temperatura minima anual (atmin)

bysort codigo ano: egen atmax=mean(tmax)
bysort codigo ano: egen atmin=mean(tmin)

// Amplitude termica 

gen amplitude = (atmax-atmin)

// Precipitacao media anual (precipitacao)

bysort codigo ano: egen pluviometria=mean(ppt)

// Precipitacao media acumulada (aprecipitacao)

bysort codigo ano: egen apluviometria=sum(ppt)

// Evapotranspiracao media anual (m_evapotranspiracao) e evapotranspiracao acumulada anual (a_evapotranspiracao)

bysort codigo ano: egen m_evapotranspiracao=mean(aet)
bysort codigo ano: egen a_evapotranspiracao=sum(aet)

// Deficit hidrico permanente medio (m_deficit_hidrico) e deficit hidrico permanente acumulado anual (a_deficit_hidrico)

bysort codigo ano: egen m_deficit_hidrico = mean(def)
bysort codigo ano: egen a_deficit_hidrico = sum(def)

************************************************************************************************************************
* Indicadores climaticos 
************************************************************************************************************************
* Quantificando secas EMBRAPA: https://ainfo.cnptia.embrapa.br/digital/bitstream/CNPAF-2010/29786/1/doc-244.pdf

// Indice Padronizado de Seca (SPI-3 meses) - versao mensal - EMBRAPA pag. 17 

* Nota: escala negativa (seca) e escala positiva (umidade/chuva)

gen LNPLUV = ln(ppt)
bysort codigo mes: egen MR=mean(ppt)
bysort codigo mes: egen Mlnrainfall=mean(LNPLUV)
bysort codigo mes: egen SDR=sd(ppt)
bysort codigo mes: egen SKR=skew(ppt)

gen U=log(MR)-Mlnrainfall
gen shape=(1+sqrt(1+(4*U/3)))/(4*U)
gen scale=MR/shape

gen GAMMA=gammap(shape, ppt/scale)
gen SPI= invnormal(GAMMA)

drop MR Mlnrainfall SDR SKR  LNPLUV U scale GAMMA shape

* SPI anual (media do mensal) - SPI_anual

bysort codigo ano: egen SPI_anual=mean(SPI)

* Nota: SPI de verao e inverno calculado de acordo com estacoes da APAC-PE
* Obs.: Existe uma series de estudos que apontam que no Brasil existem apenas duas estacoes climaticas bem definidas.
* Link: http://old.apac.pe.gov.br/meteorologia/estacoes-do-ano.php?estacao=verao

* SPI verao (media dos meses de dezembro-marco) - SPI_verao

gen verao=0 
replace verao=1 if mes==12 
replace verao=1 if mes>=1 & mes<=3 

gen SPIV = SPI*verao 
replace SPIV=. if SPIV==0 
bysort codigo ano: egen SPI_verao=mean(SPIV)

drop verao SPIV 

* SPI inverno (media dos meses de junho-setembro)

gen inverno=0
replace inverno=1 if mes>=6 & mes<=9

gen SPII = SPI*inverno 
replace SPII=. if SPII==0 
bysort codigo ano: egen SPI_inverno=mean(SPII)

drop inverno SPII 

// Indice de Porcentagem Normal de Chuva Anual (IPN) - EMBRAPA pag. 15 

* Calculado com a precipitacao acumulada de 12 meses 
* PPN (Precipitacao Normal) = precipitacao acumulada anual (media historica)
* apluviometria = precipitacao acumulada atual 
* Obs.:nao ha necessidade de multiplicar por 100 (nesse sentido, reescreva a escala do indice dividindo por 100)
* Nota: escala negativa (seca) e escala positiva (umidade/chuva)

bysort codigo: egen PPN=mean(apluviometria) // média historica anual
gen PNM = (apluviometria/PPN) 
gen IPN = (PNM-1) // normalizacao 

drop PNM

// Desvio de pluviometria acumulada (z_pluviometria)

bysort codigo: egen sd_pluviometria = sd(apluviometria)
bysort codigo: egen mean_pluviometria = mean(apluviometria)

gen z_pluviometria = (apluviometria-mean_pluviometria)/sd_pluviometria
drop sd_pluviometria mean_pluviometria

* Contraparte negativa do z-score de precipitacao multiplicada (-1)

gen z0=0
replace z0=1 if z_pluviometria<=0
gen z0_pluviometria= (z_pluviometria*z0)*(-1)

drop z0 

// Desvio de temperatura (z_temperatura)

bysort codigo: egen sd_temperatura = sd(temperatura)
bysort codigo: egen mean_temperatura  = mean(temperatura)

gen z_temperatura = (temperatura-mean_temperatura)/sd_temperatura

drop sd_temperatura mean_temperatura

* Contraparte positiva da temperatura 

gen z0=0
replace z0=1 if z_temperatura>=0
gen z0_temperatura= (z_temperatura*z0)

drop z0 


// Indice Padronizado de Seca (SDI) - nao estar no livro do EMBRAPA 

* Referencia: YU, T.; BABCOCK, B. A. Are us corn and soybeans becoming more drought tolerant?
* American Journal of Agricultural Economics, Wiley Online Library, v. 92, n. 5, p. 1310–1323, 2010.

*Nota: O indice capta duas questões importantes para o semiárido (secura e quentura, de forma simultaneamente)
*Nota: 0 a 0.1 (clima normal) e acima de <0.1 (seca)

gen SDI = z0_pluviometria*z0_temperatura


// Meses de seca registrados em um ano (mseca)

gen seca=0 
replace seca=1 if SPI<=-1

bysort codigo ano: egen mseca = sum(seca)

drop seca

// Índice de Aridez de Martonne (MIA) - EMBRAPA pag. 41 

gen MIA = (apluviometria)/(temperatura+10)

// Índice de Chuva de Lang (LRI) - EMBRAPA pag. 48 

gen LRI = apluviometria/temperatura 

// Índice de Aridez (clássico) - Thornthwaite (1948) (AI)  media anual - EMBRAPA 
* Nota: Calculado com Evapotranspiração total mensal

gen AIM = (ppt/aet)*100 
bysort codigo ano: egen AI = mean(AIM)

drop AIM 
