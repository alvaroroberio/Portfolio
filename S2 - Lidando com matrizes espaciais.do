########################################################################################################################
# Rotina {Stata} : Lidando com matrizes de peso espacial
# Data: 2022 
# Autor: Alvaro Roberio
# Website: alvaroroberio.github.io
########################################################################################################################

*Install Package 

ssc install spwmatrix
ssc install sppack
ssc install sppack 
ssc install xample
scc install xtmoran 

cd "Defina seu Diretorio de Trabalho (C:\Users...)"

*Obs.: Altere os links abaixo. 

********************************************************************************
* Criando matrizes de pesos espaciais 
********************************************************************************

*Matriz Binária (Rainha)

spwmatrix import using "C:\Users\alvar\Desktop\Database\Data_Spatial\NE\W_Queen.gal", wname(weights) xport(weights, txt)
insheet using "C:\Users\alvar\Desktop\Database\Data_Spatial\NE\weights.txt", delim (" ") clear
save "weights.dta"

gen ID = _n
order ID, first

rename v(#) m(#)
rename v(##) m(##)
rename v(###) m(###)
rename v(####) m(####)

gsort - ID

drop ID 
gen ID = _n
order ID, first

save "W_Queen.dta"
clear 

*Matriz Binária (Torre)

spwmatrix import using "C:\Users\alvar\Desktop\Database\Data_Spatial\NE\W_Torre.gal", wname(weights1) xport(weights1, txt)
insheet using "C:\Users\alvar\Desktop\Database\Data_Spatial\NE\weights1.txt", delim (" ") clear
save "weights1.dta"

gen ID = _n
order ID, first

rename v(#) m(#)
rename v(##) m(##)
rename v(###) m(###)
rename v(####) m(####)

gsort - ID

drop ID 
gen ID = _n
order ID, first

save "W_Torre.dta"
clear 

*Matriz de K-Vizinhos (K-3)

spwmatrix import using "C:\Users\alvar\Desktop\Database\Data_Spatial\NE\W_K3.gal", wname(weights2) xport(weights2, txt)
insheet using "C:\Users\alvar\Desktop\Database\Data_Spatial\NE\weights2.txt", delim (" ") clear
save "weights2.dta"

gen ID = _n
order ID, first

rename v(#) m(#)
rename v(##) m(##)
rename v(###) m(###)
rename v(####) m(####)

gsort - ID

drop ID 
gen ID = _n
order ID, first

save "W_K3.dta"
clear

*Matriz de K-Vizinhos (K-5)

spwmatrix import using "C:\Users\alvar\Desktop\Database\Data_Spatial\NE\W_K5.gal", wname(weights3) xport(weights3, txt)
insheet using "C:\Users\alvar\Desktop\Database\Data_Spatial\NE\weights3.txt", delim (" ") clear
save "weights3.dta"

gen ID = _n
order ID, first

rename v(#) m(#)
rename v(##) m(##)
rename v(###) m(###)
rename v(####) m(####)

gsort - ID

drop ID 
gen ID = _n
order ID, first

save "W_K5.dta"
clear

*Matriz de K-Vizinhos (K-7)

spwmatrix import using "C:\Users\alvar\Desktop\Database\Data_Spatial\NE\W_K7.gal", wname(weights4) xport(weights4, txt)
insheet using "C:\Users\alvar\Desktop\Database\Data_Spatial\NE\weights4.txt", delim (" ") clear
save "weights4.dta"

gen ID = _n
order ID, first

rename v(#) m(#)
rename v(##) m(##)
rename v(###) m(###)
rename v(####) m(####)

gsort - ID

drop ID 
gen ID = _n
order ID, first

save "W_K7.dta"
clear

*Matriz de K-Vizinhos (K-10)

spwmatrix import using "C:\Users\alvar\Desktop\Database\Data_Spatial\NE\W_K10.gal", wname(weights5) xport(weights5, txt)
insheet using "C:\Users\alvar\Desktop\Database\Data_Spatial\NE\weights5.txt", delim (" ") clear
save "weights5.dta"

gen ID = _n
order ID, first

rename v(#) m(#)
rename v(##) m(##)
rename v(###) m(###)
rename v(####) m(####)

gsort - ID

drop ID 
gen ID = _n
order ID, first

save "W_K10.dta"
clear


********************************************************************************
* Importando Matrizes de Pesos Espaciais // Teste de I Moran  
********************************************************************************

*Nota: "Fernando de Noronha" nas matrizes Torre e Rainha possuí interação espacial apenas com o Recife. 
*Nota: Essa interação dar-se via fluxo de pessoas pelo tráfego áereo ou marítmo (necessário fazer alguma escolha). 

clear all
set more off

cd "C:\Users\alvar\Desktop\Database\Data_Spatial\NE"

use W_Queen.dta, clear
spmat dta Queen m*, normalize(row)
spmat summarize Queen
spmat summarize Queen, links
spmat summarize Queen, links detail 

use W_Torre.dta, clear
spmat dta Torre m*, normalize(row)
spmat summarize Torre
spmat summarize Torre, links
spmat summarize Torre, links detail 

use W_K3.dta, clear
spmat dta K3 m*, normalize(row)
spmat summarize K3
spmat summarize K3, links
spmat summarize K3, links detail 

use W_K5.dta, clear
spmat dta K5 m*, normalize(row)
spmat summarize K5
spmat summarize K5, links
spmat summarize K5, links detail 

use W_K7.dta, clear
spmat dta K7 m*, normalize(row)
spmat summarize K7
spmat summarize K7, links
spmat summarize K7, links detail 

use W_K10.dta, clear
spmat dta K10 m*, normalize(row)
spmat summarize K10
spmat summarize K10, links
spmat summarize K10, links detail 

********************************************************************************
* Calculando Indice de Moran para Dados em Painel
********************************************************************************

*** Obs.: Necessário tirar a coluna ID da Matriz para calcular o I Moran com xtmoran 
*** Obs.: Necessário grande capacidade computacional para uso do XTMORAN (Core 7 - 10-12 geração e Ram 8 GB pra cima)

* Example 

cd "C:\Users\alvar\...."
use Database 
xtset ID ANO

xtmoran VAR_DEPENDENTE, wname(Queen1.dta) morani(2006 2007 2008 2009 2010)
