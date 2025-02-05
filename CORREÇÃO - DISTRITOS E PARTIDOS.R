#Vou verificar os dados por distrito para tirar os distritos em que est�o representados nas letras A-I menos de 50% dos votos. 
# � um recorte relativamente baixo, poderia usar 70, 80 e ainda assim boa parte dos pa�ses/elei��es n�o seriam muito afetados.
#Mas por enquanto ficamos assim. 

cses_leg <- cses_leg %>%
  mutate(totalvot = rowSums(select(., pv_dist_A:pv_dist_I), na.rm = T))


leg_summary <- cses_leg %>% group_by(election, district) %>%
  select (election, district, totalvot, pv_dist_A:pv_dist_I) %>%
  summarize_all (.funs = c(mean="mean"), na.rm = T) %>% filter(totalvot_mean > 0)



cses_lh <- cses_leg %>% select (ID, election, district, totalvot, pv_dist_A:pv_dist_I, vote_LH_PL, vote_LH_DC)



#PROCUREI PRIMEIRO EM leg_summary (ORDENADO POR TOTALVOT)

#DEPOIS EM CSES_LH (TAMB�M ORDENADO POR TOTALVOT MAS FILTRANDO "ELECTION")
#O QUE ACONTECE COM ESSES CASOS:


# ZAF_2009 - multiplicar por 100


cses_lh$tot100 <- cses_lh$totalvot * 100

cses_lh <- cses_lh %>% mutate (
  totalvot = case_when(
    election == "ZAF_2009" ~ tot100,
    TRUE          ~ totalvot
  )
)

cses_lh$tot100 <- NULL

# CAN_2008 - s� primeira linha em cada distrito teve pv_dist preenchido, mas tirando district 12007 que tem 29.6
#  apenas, 5 distritos entre 64.2 e 88.9, o resto � 94% at� 100. E 2 �nicas observa��es com missing no distrito 


# POL_2007 - todos distritos de 87.3% para cima, mas algumas linhas n�o preencheram o pv_dist
# #O caso do district 34, por exemplo, s� preencheram a primeira, em outras s� falta uma parte.


# ESSES DOIS CASOS N�O PRECISARIA CORRIGIR PARA FILTRAR ACIMA DE 50%, IA DAR CERTO, MAS J� VOU APLICAR UM C�DIGO PARA CORRIGIR TUDO:
# COM ESSE C�DIGO EU PEGO O VALOR M�XIMO EM TODAS AS LINHAS DE UM GRUPO, E DEPOIS APLICO ELE DE VOLTA NO RESTO DO GRUPO COM "MATCH"

#MAS PRECISEI CRIAR UMA VARI�VEL QUE JUNTA DISTRITO E ELEI��O PORQUE N�O CONSEGUI FAZER ISSO USANDO AS 2 NO GROUP_BY

cses_lh$el_dist <- (paste(cses_lh$election, cses_lh$district, sep = "-"))

totalvotes <- cses_lh %>% 
  group_by(el_dist) %>%
  summarise(pt = max(totalvot, na.rm = TRUE))

cses_lh$pt <- totalvotes$pt[match(cses_lh$el_dist, totalvotes$el_dist)]

cses_lh$el_dist <- NULL

#DAQUI PARA FRENTE N�O TEM NADA A ALTERAR, S� ANOTA��ES DE OUTROS CASOS COM % ABAIXO

#ENT�O VAMOS PASSAR OS DADOS AO BANCO PRINCIPAL


cses_leg$votes_dist <- cses_lh$pt[match(cses_leg$ID, cses_leg$ID)]

#TUDO ISSO FOI S� PARA CORRIGIR �FRICA DO SUL E ESSA MUDAN�A DE INSERIR EM TODAS LINHAS DE UM MESMO DISTRITO A INFORMA��O DE %

# FRA_2007 - alguns distritos parecem ter valores bem baixos em todas linhas mesmo.
#Ou seja, aqui come�amos a ter problema de pouco % de votos dos partidos inclu�dos nas letras mesmo 
#n�o � como CAN_2008 e POL_2007, em que problema de mau preenchimento que deixou m�dia distorcida.


#DISTRICT 8103 -  4.96% 
# 5105 - 29,44%
#9108  com 38.53%
#5912 - 40.17%
#4406 - 41,05%
#4203 - 43,45%


#DISTRITO 2002 tem 53,98% e o resto acima disso


# KOREA 2012

#DISTRITO 53 = 11.40%
#106 = 49.35%
#O RESTO PASSA DE 50%


#ITA_2006 - DISTRITO 27 19,90% RESTO 71,20% PRA CIMA


#THAILAND 2011
#DISTRITO 1802 - 25,79% ; DISTRITO 1303 - 33,42%; DISTRITO 2707 46,72%
#DISTRITO 1506 - 49,86% O RESTO 2406 QUE TEM 50,84% E A� � 58,31% OU MAIS
#DISTRITO 1303 COM 33.42% (E UMA �NICA LINHA)
# LEGISLATIVE - LOWER HOUSE #


# JPN_2007 - DISTRITOS 19 , 21, 1, 3, 6, 8 ABAIXO DE 50%
#KEN_2013 DISTRITO 75 COM 29.27

#AUSTRIA 1996 - DISTRITO 237 COM 30% e 801 com 49.7

# Austria 2004 - 
# 132
# 32.05
# 107
# 32.46
# distrito 802 = 49.25%


#TUR_2011 
# 21
# 32.9 %

# 47
# 35.7 %

# 72
# 43.5%

### TODOS VALORES A PARTIR A� DE FRA2007 ERAM OS VALORES CERTOS, N�O ERA PROBLEMA DE 
#M�DIA PELO QUE VI, ENT�O ASSUMO QUE A PARTIR DA� TAMB�M SER�. Claro que pode ter o problema anterior, mas quando os valores n�o s�o t�o baixos
#n�o parece ser o caso. Ou o problema oposto: repeti��o de valores por serem partidos de uma mesma coaliz�o, e consequentemente
#o total de % de votos fica acima do limiar de 50% (ou qualquer outro que eu venha a escolher) mas na verdade era abaixo. 


#Passam de 100, cerca de 105 distritos em toda amostra (de 8877 tirando os zerados)
#Muitos s�o casos de considerar duas vezes partidos que devem ser de uma mesma
#coaliz�o, como acontece % of votes nacional. Outros parece ser erro 

# E os zerados? A princ�pio vou tirar, mas podem ter alguns que s� est� sem dado, a� precisaria pesquisar os dados eleitorais de cada um. 