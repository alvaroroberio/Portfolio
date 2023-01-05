########################################################################################################################
# Rotina {Stata} : Calculo do SPI 
# Data: 2022 
# Autor: Alvaro Roberio
# Website: alvaroroberio.github.io
########################################################################################################################


*\ Rotina do SPI 

gen LNPLUV = ln(PLUV)
bysort CODIBGE MES: egen MR=mean(PLUV)
bysort CODIBGE MES: egen Mlnrainfall=mean(LNPLUV)
bysort CODIBGE MES: egen SDR=sd(PLUV)
bysort CODIBGE MES: egen SKR=skew(PLUV)

gen U=log(MR)-Mlnrainfall
gen shape=(1+sqrt(1+(4*U/3)))/(4*U)
gen scale=MR/shape

gen GAMMA=gammap(shape, PLUV/scale)
gen SPI= invnormal(GAMMA)

drop MR Mlnrainfall SDR SKR  LNPLUV U scale GAMMA shape

*\ Rotina do STI 

gen LNTEMP = ln(TEMP)
bysort CODIBGE MES: egen MR=mean(TEMP)
bysort CODIBGE MES: egen Mlntemp=mean(LNTEMP)
bysort CODIBGE MES: egen SDR=sd(TEMP)
bysort CODIBGE MES: egen SKR=skew(TEMP)

gen U=log(MR)-Mlntemp
gen shape=(1+sqrt(1+(4*U/3)))/(4*U)
gen scale=MR/shape

gen GAMMA=gammap(shape, PLUV/scale)
gen STI= invnormal(GAMMA)

drop MR Mlntemp SDR SKR LNTEMP U scale GAMMA shape
