#rm(list=ls()[!(ls() %in% c("cses_leg", "cses_pr", "cses"))])

rm(list=ls()[!(ls() %in% c("cses"))])

load("CSES_Manifesto.RData")

library(tidyverse)
#library(descr)

# CORRIGIR PARA ARQUIVO NOVO, EM TESE_CSES: load("C:/Users/livia/OneDrive - usp.br/TESE/PROJETO - CSES/INTRO-EDIT/cses.RData")


#setwd("C:/Users/livia/OneDrive - usp.br/TESE/PROJETO - CSES/PRESIDENTIAL")


##### EDI��ES INICIAIS #####

# FILTRANDO S� OS BANCOS/PA�SES COM ELEI��O PARA PRESIDENTE, ORGANIZANDO ORDEM DAS COLUNAS E SELECIONANDO VARI�VEIS 


cses_pr <- cses %>% filter (type == 20 | type == 12) %>%
  select(ID,	election,	country, module, type, system_PR,  #MAIN DATA ON EACH ELECTION
                        vote_PR_1,	vote_PR_2, ideol_self, starts_with("ideolparty"),  #IDEOLOGY
                        starts_with("ideol_mean"), # Ideology (mean of all voters in each election)
                        starts_with("ex_ideolparty"), starts_with("ideol_leader"),
                        alt_ideol_self, starts_with ("alt_ideolparty"),
                        starts_with ("exp_alt_ideolparty"), starts_with("alt_ideol_leader"),
                        #ideol_voted_PR_1:voted_exp_closest_PR_1, #IDEOLOGY - CREATED 
                        starts_with("family_ideol"), rile_A:rile_I, 
                        rile_other_1:rile_other_4, # a partir de rile_other_5 nenhum dado nesses pa�ses
                        elected_pr, pcv_PR_A:pcv_PR_I, 
                        compulsory, compulsory_dummy, regime_age:CENPP, fh_civil:dalton_pol,freedom_net, # INSTITUTIONS
                        GDP_1:GDP_3, cum_gdp,	abs_growth,	cum_gdp2,	abs_growth2, # ECONOMY
                        education, knowledge, knowledge_adj, efficacy, 
                        age, gender, female, rural, income,   #INDIVIDUAL
                        effic_vote, economy_1:IMD3015_D,
                        numparty_A:numparty_I)  



##### RECODES/ MISSING #####

cses_pr <- cses_pr %>%
  mutate_at(.vars = vars(system_PR), 
            .funs = list(~ifelse(. > 50, NA, .)))



cses_pr <- cses_pr %>%
  mutate_at(.vars = vars(contains("ideol")), 
            .funs = list(~ifelse(. > 50, NA, .)))

cses_pr <- cses_pr %>%
  mutate_at(.vars = vars(contains("vote"), contains("numparty")), 
            .funs = list(~ifelse(. >9999900, NA, .)))



#Transformando class das vari�veis para poder aplicar case_when 

cses_pr <- cses_pr %>%
  mutate_at(vars(starts_with("ex_ideolparty"), starts_with("ideolparty"), starts_with("vote"),
                 starts_with("pcv"), elected_pr), as.numeric) 

# MISSING

#Creio n�o ter nenhuma vari�vel com "ideol" no nome al�m de todas de placement (self, party, leader
# e suas vers�es alternativas/expert) E IDEOL_FAMILY (que tem valores n�o-missing at�
#26 segundo o Codebook IMD). Ent�o uso essa palavra para atribuir todos missings dessas:


##### CORRE��ES DE COALIZ�ES #####

#ESSA SE��O � A DIFEREN�A PARA O ARQUIVO ANTERIOR (QUE EST� NA PASTA "PROJETO_CSES")

# Antes de ideology_voted e ideology_elected, fa�o certas corre��es em pcv, 
#ideology_party e ideology_voted para adequar aos partidos que realmente estavam concorrendo na elei��o

##### ARGENTINA #####

#ARG_2015 est� codificado como tendo eleito a coaliz�o (0320002, ou party B). Que n�o tem em "ideology_party" 
#mas tem em "ex_ideology_party" .

#Esse pa�s � confuso, porque tem v�rias etapas de elei��o (uma esp�cie de prim�rias, com v�rios candidatos de cada
#alian�a, depois a elei��o geral e um poss�vel segundo turno). Tudo aqui parece se referir a essa elei��o geral, 
#os votos s�o para as alian�as participantes. (Mais observa��es na planilha). 

#Estrat�gia: para preservar o que os experts atribu�ram a party B, mas n�o ficar com NA
#nem em "ideology_voted" para muitos entrevistados, nem em "ideology_elected" para todos, 
#vamos transformar ideology_party_B em m�dia de G e H.

cses_pr <- cses_pr %>% mutate (
  ideolparty_B = case_when(
    election == "ARG_2015" ~ (ideolparty_G + ideolparty_H)/2,
       TRUE          ~ ideolparty_B
  )
)

#Com isso perdemos alguns valores onde um dos dois partidos est� como "NA" mas o outro n�o
#(s�o poucos casos, a maioria onde um est� missing outro tamb�m). E tamb�m n�o temos dados para o terceiro
#partido da alian�a, o  Civic Coalition (CC-ARI). 


## PARA PARTY_D(Alian�a "Workers' Left Front") e PARTY_F (Alian�a "Compromiso Federal") n�o temos
# nenhum partido-membro inclu�do nas outras letras (A-F s�o as 6 principais alian�as, depois 2
#partidos da alian�a de PARTY_B e por �ltimo o Partido Justicialista, que � o principal da alian�a
#representada por PARTY_A). Por isso o ideology_party nesses casos fica missing mesmo.

cses_pr <- cses_pr %>% mutate (
  pcv_PR_A = case_when(
    election == "ARG_2015" ~ 37.08 ,
    TRUE          ~ pcv_PR_A
  )
)

cses_pr <- cses_pr %>% mutate (
  pcv_PR_B = case_when(
    election == "ARG_2015" ~ 34.15 ,
    TRUE          ~ pcv_PR_B
  )
)

cses_pr <- cses_pr %>% mutate (
  pcv_PR_C = case_when(
    election == "ARG_2015" ~ 21.39 ,
    TRUE          ~ pcv_PR_C
  )
)

cses_pr <- cses_pr %>% mutate (
  pcv_PR_D = case_when(
    election == "ARG_2015" ~ 3.23 ,
    TRUE          ~ pcv_PR_D
  )
)

cses_pr <- cses_pr %>% mutate (
  pcv_PR_E = case_when(
    election == "ARG_2015" ~ 2.51 ,
    TRUE          ~ pcv_PR_E
  )
)

cses_pr <- cses_pr %>% mutate (
  pcv_PR_F = case_when(
    election == "ARG_2015" ~ 1.64 ,
    TRUE          ~ pcv_PR_F
  )
)

##### BRASIL #####

# 2006 - incluir Helo�sa Helena (ideolparty n�o tem mas ela consta como Leader I):
# � preciso tomar cuidado - se o Partido I constar em alguma outra coisa, essa altera��o de 
#pcv pode distorcer os dados. Mas aparentemente n�o h� partido I pelo menos em ideolparty e ex_ideolparty)

cses_pr <- cses_pr %>% mutate (
  pcv_PR_I = case_when(
    election == "BRA_2006" ~ 6.85 ,
    TRUE          ~ pcv_PR_I
  )
)

#2010 - S� TEMOS PT (party_A) e PSDB (party_C), entre os concorrentes da elei��o, nas letras e respectivos dados de ideologia. 

#Mas eles aparecem repetidos em % votes para outras letras com quem fizeram coaliz�es. Retirando esses valores:

cses_pr$pcv_PR_B[cses_pr$election == "BRA_2010"] <- NA
cses_pr$pcv_PR_D[cses_pr$election == "BRA_2010"] <- NA
cses_pr$pcv_PR_E[cses_pr$election == "BRA_2010"] <- NA
cses_pr$pcv_PR_F[cses_pr$election == "BRA_2010"] <- NA
cses_pr$pcv_PR_G[cses_pr$election == "BRA_2010"] <- NA
cses_pr$pcv_PR_H[cses_pr$election == "BRA_2010"] <- NA
cses_pr$pcv_PR_I[cses_pr$election == "BRA_2010"] <- NA

  

#FRA_2002

# Em todas vari�veis (inclusive VOTO), est� tudo certo, mas o "elected" ficou 2500001, 
# Union for a Popular Movement (UMP), que foi o partido que Chirac criou fundindo o seu com outros logo depois.
#Por isso vamos alterar:

cses_pr <- cses_pr %>% mutate (
  elected_pr = case_when(
    election == "FRA_2002" ~ 2500008 ,
    TRUE          ~ elected_pr
  )
)

#KEN_2013

cses_pr <- cses_pr %>% mutate (
  pcv_PR_G = case_when(
    election == "KEN_2013" ~ 0.36,
    TRUE      ~ pcv_PR_G
  )
)

##### MEXICO #####

#MEX_2000

cses_pr <- cses_pr %>% mutate (
  elected_pr = case_when(
    election == "MEX_2000" ~ 4840022 ,
    TRUE          ~ elected_pr
  )
)

#MEX_2006

cses_pr$pcv_PR_D[cses_pr$election == "MEX_2006"] <- NA

cses_pr$pcv_PR_E[cses_pr$election == "MEX_2006"] <- NA

cses_pr$pcv_PR_F[cses_pr$election == "MEX_2006"] <- NA


#Adequar vote_PR_1(estava com c�digos num�ricos das coaliz�es nos casos 4840029 (PRI � o cabe�a de chapa, 4840001) e 
#(4840028 - PRD, c�digo 4840003)


cses_pr <- cses_pr %>% mutate (
  vote_PR_1 = case_when(
    election == "MEX_2006" & vote_PR_1 == 4840029 ~ 4840001 ,
    TRUE          ~ vote_PR_1
  )
)

cses_pr <- cses_pr %>% mutate (
  vote_PR_1 = case_when(
    election == "MEX_2006" & vote_PR_1 == 4840028 ~ 4840003 ,
    TRUE          ~ vote_PR_1
  )
)


##### PERU #####

#2000 
#corrigir elected: constava 6040028 (VAMOS VECINO), um dos partidos da coaliz�o 6040054 (PERU 2000)

cses_pr <- cses_pr %>% mutate (
  elected_pr = case_when(
    election == "PER_2000" ~ 6040054 ,
    TRUE          ~ elected_pr
  )
)


#PERU 2016
#INSERIR PCVS

# Popular Force (FP) - PARTY A

cses_pr <- cses_pr %>% mutate (
  pcv_PR_A = case_when(
    election == "PER_2016" ~ 39.86 ,
    TRUE          ~ pcv_PR_A
  )
)

#PPK - PARTY B
cses_pr <- cses_pr %>% mutate (
  pcv_PR_B = case_when(
    election == "PER_2016" ~  21.05 ,
    TRUE          ~ pcv_PR_B
  )
)

#Frente Amplio - PARTY C
cses_pr <- cses_pr %>% mutate (
  pcv_PR_C = case_when(
    election == "PER_2016" ~ 18.74 ,
    TRUE          ~ pcv_PR_C
  )
)

#PARTY D n�o participou das Presidenciais 

#Alianza Popular - PARTY E
cses_pr <- cses_pr %>% mutate (
  pcv_PR_E = case_when(
    election == "PER_2016" ~  5.83 ,
    TRUE          ~ pcv_PR_E
  )
)

# ACCION POPULAR - PARTY F
cses_pr <- cses_pr %>% mutate (
  pcv_PR_F = case_when(
    election == "PER_2016" ~  6.97 ,
    TRUE          ~ pcv_PR_F
  )
)

#  Direct Democracy - PARTY G
cses_pr <- cses_pr %>% mutate (
  pcv_PR_G = case_when(
    election == "PER_2016" ~  4 ,
    TRUE          ~ pcv_PR_G
  )
)

# Possible Peru - PARTY H
cses_pr <- cses_pr %>% mutate (
  pcv_PR_H = case_when(
    election == "PER_2016" ~  1.3 ,
    TRUE          ~ pcv_PR_H
  )
)

# Hope Front - PARTY I
cses_pr <- cses_pr %>% mutate (
  pcv_PR_I = case_when(
    election == "PER_2016" ~  1.32 ,
    TRUE          ~ pcv_PR_I
  )
)

#PHILIPPINES - 2010

cses_pr <- cses_pr %>% mutate (
  pcv_PR_I = case_when(
    election == "PHL_2010" ~  0.12 ,
    TRUE          ~ pcv_PR_I
  )
)


# ESSES AQUI S� EST�O EM LEADERS, MAS COMO N�O PARECE TER IDEOL_PARTIES PARA ELES PODEMOS J� INCLUIR AGORA TAMB�M O PCV:

cses_pr <- cses_pr %>% mutate (
  pcv_PR_G = case_when(
    election == "PHL_2010" ~  0.15 ,
    TRUE          ~ pcv_PR_G
  )
)

cses_pr <- cses_pr %>% mutate (
  pcv_PR_H = case_when(
    election == "PHL_2010" ~  0.13 ,
    TRUE          ~ pcv_PR_H
  )
)


##### ROMANIA #####

#ROU_2004

#Voto estava com as alian�as
cses_pr <- cses_pr %>% mutate (
  vote_PR_1 = case_when(
    election == "ROU_2004" & vote_PR_1 == 6420026 ~ 6420019 ,
    TRUE          ~ vote_PR_1
  )
)


cses_pr <- cses_pr %>% mutate (
  vote_PR_1 = case_when(
    election == "ROU_2004" & vote_PR_1 == 6420041 ~ 6420001 ,
    TRUE          ~ vote_PR_1
  )
)

#PCV

cses_pr$pcv_PR_B[cses_pr$election == "ROU_2004"] <- NA
cses_pr$pcv_PR_F[cses_pr$election == "ROU_2004"] <- NA


#ROU_2014

#INSERIR PERCENT VOTES:

# 6420101. PSD-UNPR-PC Electoral Alliance 

cses_pr <- cses_pr %>% mutate (
  pcv_PR_A = case_when(
    election == "ROU_2014" ~  40.44 ,
    TRUE          ~ pcv_PR_A
  )
)

# 6420102. Christian-Liberal Alliance (ACL): 

cses_pr <- cses_pr %>% mutate (
  pcv_PR_B = case_when(
    election == "ROU_2014" ~  30.37 ,
    TRUE          ~ pcv_PR_B
  )
)

# 6420103. Popular Movement Party (PMP) 
cses_pr <- cses_pr %>% mutate (
  pcv_PR_C = case_when(
    election == "ROU_2014" ~  5.20 ,
    TRUE          ~ pcv_PR_C
  )
)

#6420002. People's Party - Dan Diaconescu (PP-DD) 
cses_pr <- cses_pr %>% mutate (
  pcv_PR_D = case_when(
    election == "ROU_2014" ~  4.03 ,
    TRUE          ~ pcv_PR_D
  )
)

# 6420004. Greater Romania Party (PRM)
cses_pr <- cses_pr %>% mutate (
  pcv_PR_E = case_when(
    election == "ROU_2014" ~  3.68 ,
    TRUE          ~ pcv_PR_E
  )
)

# 6420003. Dem. Union of Hungarians in Romania (UDMR)
cses_pr <- cses_pr %>% mutate (
  pcv_PR_F = case_when(
    election == "ROU_2014" ~  3.47 ,
    TRUE          ~ pcv_PR_F
  )
)

#INDEPENDENTES (para serem considerados em "Expert")

#Popescu-Tariceanu Calin-Constantin-Anton
cses_pr <- cses_pr %>% mutate (
  pcv_PR_H = case_when(
    election == "ROU_2014" ~  5.36 ,
    TRUE          ~ pcv_PR_H
  )
)

#Monica Macovei
cses_pr <- cses_pr %>% mutate (
  pcv_PR_I = case_when(
    election == "ROU_2014" ~  4.44 ,
    TRUE          ~ pcv_PR_I
  )
)


##### SRB_2012 #####

#Inserindo Percent of Votes:

#  Tomislav Nikolic - (SNS)	Let's Get Serbia Moving	25.05%

cses_pr <- cses_pr %>% mutate (
  pcv_PR_A = case_when(
    election == "SRB_2012" ~  25.05 ,
    TRUE          ~ pcv_PR_A
  )
)

# Boris Tadic	Choice for a Better Life - 25.31% 


cses_pr <- cses_pr %>% mutate (
  pcv_PR_B = case_when(
    election == "SRB_2012" ~  25.31 ,
    TRUE          ~ pcv_PR_B
  )
)

# Ivica Dacic-	Socialist Party of Serbia (SPS) -	14.23%	

cses_pr <- cses_pr %>% mutate (
  pcv_PR_C = case_when(
    election == "SRB_2012" ~  14.23 ,
    TRUE          ~ pcv_PR_C
  )
)

# Democratic Party of Serbia -	7.44%
cses_pr <- cses_pr %>% mutate (
  pcv_PR_D = case_when(
    election == "SRB_2012" ~  7.44 ,
    TRUE          ~ pcv_PR_D
  )
)

#	U-Turn coalition (ALSO CALLED "TURNOVER")	-	5.03%

cses_pr <- cses_pr %>% mutate (
  pcv_PR_E = case_when(
    election == "SRB_2012" ~  5.03 ,
    TRUE          ~ pcv_PR_E
  )
)

# United Regions of Serbia -	6.58%

cses_pr <- cses_pr %>% mutate (
  pcv_PR_F = case_when(
    election == "SRB_2012" ~  6.58 ,
    TRUE          ~ pcv_PR_F
  )
)

# Alliance of Vojvodina Hungarians - 1.62%

cses_pr <- cses_pr %>% mutate (pcv_PR_H = case_when(election == "SRB_2012" ~  1.62 ,
    TRUE ~ pcv_PR_H)) 
    

##### PARTY_IDEOLOGY - MEAN OF VOTERS PLACEMENT #####

grouped_PR<- cses_pr %>% select (country, election, ideolparty_A:ideolparty_I) %>% 
  group_by (country, election)  %>% 
  summarize_all (.funs = c(mean="mean"), na.rm = T) 


cses_pr$ideol_mean_A <- grouped_PR$ideolparty_A_mean[match(cses_pr$election, grouped_PR$election)]
cses_pr$ideol_mean_B <- grouped_PR$ideolparty_B_mean[match(cses_pr$election, grouped_PR$election)]
cses_pr$ideol_mean_C <- grouped_PR$ideolparty_C_mean[match(cses_pr$election, grouped_PR$election)]
cses_pr$ideol_mean_D <- grouped_PR$ideolparty_D_mean[match(cses_pr$election, grouped_PR$election)]
cses_pr$ideol_mean_E <- grouped_PR$ideolparty_E_mean[match(cses_pr$election, grouped_PR$election)]
cses_pr$ideol_mean_F <- grouped_PR$ideolparty_F_mean[match(cses_pr$election, grouped_PR$election)]
cses_pr$ideol_mean_G <- grouped_PR$ideolparty_G_mean[match(cses_pr$election, grouped_PR$election)]
cses_pr$ideol_mean_H <- grouped_PR$ideolparty_H_mean[match(cses_pr$election, grouped_PR$election)]
cses_pr$ideol_mean_I <- grouped_PR$ideolparty_I_mean[match(cses_pr$election, grouped_PR$election)]



###### IDEOLOGY - PARTY VOTED #####

### PARA PODER USAR O CASE_WHEN, MUDAR TUDO PARA MESMO TIPO (NUMERIC):

# VOTER PERSPECTIVE 

cses_pr <- cses_pr %>% mutate (
  ideol_voted_PR_1 = case_when(
    numparty_A == vote_PR_1 ~ ideolparty_A,
    numparty_B == vote_PR_1 ~ ideolparty_B,
    numparty_C == vote_PR_1 ~ ideolparty_C,
    numparty_D == vote_PR_1 ~ ideolparty_D,
    numparty_E == vote_PR_1 ~ ideolparty_E,
    numparty_F == vote_PR_1 ~ ideolparty_F,
    numparty_G == vote_PR_1 ~ ideolparty_G,
    numparty_H == vote_PR_1 ~ ideolparty_H,
    numparty_I == vote_PR_1 ~ ideolparty_I,
    TRUE                    ~ vote_PR_1
  )
)

## EXPERT PERSPECTIVE

cses_pr <- cses_pr %>% mutate (
  exp_ideol_voted_PR_1 = case_when(
    numparty_A == vote_PR_1 ~ ex_ideolparty_A,
    numparty_B == vote_PR_1 ~ ex_ideolparty_B,
    numparty_C == vote_PR_1 ~ ex_ideolparty_C,
    numparty_D == vote_PR_1 ~ ex_ideolparty_D,
    numparty_E == vote_PR_1 ~ ex_ideolparty_E,
    numparty_F == vote_PR_1 ~ ex_ideolparty_F,
    numparty_G == vote_PR_1 ~ ex_ideolparty_G,
    numparty_H == vote_PR_1 ~ ex_ideolparty_H,
    numparty_I == vote_PR_1 ~ ex_ideolparty_I,
    TRUE                    ~ vote_PR_1
  )
)

# VOTERS MEAN

cses_pr <- cses_pr %>% mutate (
  meanv_ideol_voted_PR_1 = case_when(
    numparty_A == vote_PR_1 ~ ideol_mean_A,
    numparty_B == vote_PR_1 ~ ideol_mean_B,
    numparty_C == vote_PR_1 ~ ideol_mean_C,
    numparty_D == vote_PR_1 ~ ideol_mean_D,
    numparty_E == vote_PR_1 ~ ideol_mean_E,
    numparty_F == vote_PR_1 ~ ideol_mean_F,
    numparty_G == vote_PR_1 ~ ideol_mean_G,
    numparty_H == vote_PR_1 ~ ideol_mean_H,
    numparty_I == vote_PR_1 ~ ideol_mean_I,
    TRUE                    ~ vote_PR_1
  )
)


##### IDEOLOGY - ELECTED #####


# VOTER PERSPECTIVE

cses_pr <- cses_pr %>% mutate (
  ideol_elected_PR_1 = case_when(
    numparty_A == elected_pr ~ ideolparty_A,
    numparty_B == elected_pr ~ ideolparty_B,
    numparty_C == elected_pr ~ ideolparty_C,
    numparty_D == elected_pr ~ ideolparty_D,
    numparty_E == elected_pr ~ ideolparty_E,
    numparty_F == elected_pr ~ ideolparty_F,
    numparty_G == elected_pr ~ ideolparty_G,
    numparty_H == elected_pr ~ ideolparty_H,
    numparty_I == elected_pr ~ ideolparty_I,
    TRUE                    ~ elected_pr
  )
)

# EXPERT
cses_pr <- cses_pr %>% mutate (
  exp_ideol_elected_PR_1 = case_when(
    numparty_A == elected_pr ~ ex_ideolparty_A,
    numparty_B == elected_pr ~ ex_ideolparty_B,
    numparty_C == elected_pr ~ ex_ideolparty_C,
    numparty_D == elected_pr ~ ex_ideolparty_D,
    numparty_E == elected_pr ~ ex_ideolparty_E,
    numparty_F == elected_pr ~ ex_ideolparty_F,
    numparty_G == elected_pr ~ ex_ideolparty_G,
    numparty_H == elected_pr ~ ex_ideolparty_H,
    numparty_I == elected_pr ~ ex_ideolparty_I,
    TRUE                    ~ elected_pr
  )
)


# MEAN VOTER

cses_pr <- cses_pr %>% mutate (
  meanv_ideol_elected_PR_1 = case_when(
    numparty_A == elected_pr ~ ideol_mean_A,
    numparty_B == elected_pr ~ ideol_mean_B,
    numparty_C == elected_pr ~ ideol_mean_C,
    numparty_D == elected_pr ~ ideol_mean_D,
    numparty_E == elected_pr ~ ideol_mean_E,
    numparty_F == elected_pr ~ ideol_mean_F,
    numparty_G == elected_pr ~ ideol_mean_G,
    numparty_H == elected_pr ~ ideol_mean_H,
    numparty_I == elected_pr ~ ideol_mean_I,
    TRUE                    ~ elected_pr
  )
)

##### MISSING #####
#Vou rodar novamente para pegar vari�veis criadas - ideol_voted, ideol_elected
#DEPOIS AS OUTRAS CRIADAS (closest, congruence) se baseiam nessas j� recodificadas. 

cses_pr <- cses_pr %>%
  mutate_at(.vars = vars(contains("ideol")), 
            .funs = list(~ifelse(. > 10, NA, .)))


##### % DE TIPOS DE MISSING EM IDEOLOGIA #####

#PARA ESSA TABELA FUNCIONAR N�O PODE HAVER A TRANSFORMA��O DE MISSING L� EM CIMA 
## IDEOLOGY_VOTED (especificando os tipos de MISSING) ##

# cses_pr$ideol_PR_1_ch <- as.character(cses_pr$ideol_voted_PR_1) 
# cses_pr$ideol_PR_1_ch[cses_pr$ideol_voted_PR_1 > 99 & cses_pr$ideol_voted_PR_1 < 9999988 ] <- "Party not included"
# cses_pr$ideol_PR_1_ch[cses_pr$ideol_voted_PR_1 == 9999988 | cses_pr$ideol_voted_PR_1 == 9999993] <- "Invalid/None"
# cses_pr$ideol_PR_1_ch[cses_pr$ideol_voted_PR_1 > 9999988 & cses_pr$ideol_voted_PR_1< 9999993] <- "Other parties"
# cses_pr$ideol_PR_1_ch[cses_pr$ideol_voted_PR_1 == 9999998] <- "Don't know"
# cses_pr$ideol_PR_1_ch[cses_pr$ideol_voted_PR_1 == 9999997 |cses_pr$ideol_voted_PR_1 == 9999999] <- "Missing - vote"
# cses_pr$ideol_PR_1_ch[cses_pr$ideol_voted_PR_1 == 99 & cses_pr$vote_PR_1 != 9999999] <- "Missing - party ideology"
# cses_pr$ideol_PR_1_ch[cses_pr$ideol_voted_PR_1 == 99 & cses_pr$vote_PR_1 == 9999999] <- "Missing - vote"
# cses_pr$ideol_PR_1_ch[cses_pr$ideol_voted_PR_1 == 98] <- "Don't know where to place"
# cses_pr$ideol_PR_1_ch[cses_pr$ideol_voted_PR_1 == 97] <- "Missing - party ideology"
#cses_pr$ideol_PR_1_ch[cses_pr$ideol_voted_PR_1 == 96] <- "Haven't heard of party"
#cses_pr$ideol_PR_1_ch[cses_pr$ideol_voted_PR_1 == 95] <- "Haven't heard of LR"
#cses_pr$ideol_PR_1_ch[cses_pr$ideol_voted_PR_1 < 95 ] <- "Placed party in LR"

## SELF-PLACEMENT ##

#cses_pr$ideolself_ch <- as.character(cses_pr$ideol_self) 
#cses_pr$ideolself_ch[cses_pr$ideol_self == 98] <- "Don't know where to place"
#cses_pr$ideolself_ch[cses_pr$ideol_self == 97| cses_pr$ideol_self == 99 ] <- "Missing/ Refused"
#cses_pr$ideolself_ch[cses_pr$ideol_self == 95] <- "Haven't heard of LR"


# TabelaS com propor��es de cada valor de "ideol_voted" e "ideol-self" por pa�s-ano:

#table <- with (cses_pr, table (interaction(election), interaction(ideol_PR_1_ch)))
#table2 <- with (cses_pr, table (interaction(election), interaction(ideolself_ch)))
#write.csv(table, file = "TABLES/Ideol_voted_PR1.csv")
#write.csv(table2, file = "TABLES/Ideol_self_PR Subset.csv")


##### CORRE��O PCV - IDEOL #####

#COM FUN��O GREP IDENTIFICAMOS O N� DAS COLUNAS COM AS VARI�VEIS RESPECTIVAS

pcvcols <- grep("^pcv", names(cses_pr))

#RECODIFICANDO - CITIZEN PLACEMENT: 
partycols <- grep("^ideolparty", names(cses_pr))
cses_pr[partycols][is.na(cses_pr[pcvcols])] <- NA

#EXPERT PLACEMENT:
ex_partycols <- grep("^ex_ideolparty", names(cses_pr))
cses_pr[ex_partycols][is.na(cses_pr[pcvcols])] <- NA

#LEADERS
leadercols <- grep("^ideol_leader", names(cses_pr))
cses_pr[leadercols][is.na(cses_pr[pcvcols])] <- NA

#ALTERNATIVE SCALE (TEM 3?):

altpartycols <- grep("^alt_ideolparty", names(cses_pr))
cses_pr[altpartycols][is.na(cses_pr[pcvcols])] <- NA

exp_altcols <- grep("^exp_alt_ideolparty", names(cses_pr))
cses_pr[exp_altcols][is.na(cses_pr[pcvcols])] <- NA

altleadercols <- grep("^alt_ideol_leader", names(cses_pr))
cses_pr[altleadercols][is.na(cses_pr[pcvcols])] <- NA


##### CLOSEST/ CONG_CLOSEST #####

##### Voter Perspective#####

cses_pr <- data.frame(cses_pr)

cols <- grep("^ideolparty", names(cses_pr))
temp_df <- -abs(cses_pr[cols] - cses_pr$ideol_self)
cses_pr$closest <- cses_pr[cols][cbind(1:nrow(cses_pr), 
                                       max.col(replace(temp_df, is.na(temp_df), -Inf)))]

cses_pr <- cses_pr %>% mutate (cong_closest = abs(closest-ideol_self))

##### Expert Perspective #####

cols <- grep("^ex_ideolparty", names(cses_pr))

temp_df <- -abs(cses_pr[cols] - cses_pr$ideol_self)
cses_pr$exp_closest <- cses_pr[cols][cbind(1:nrow(cses_pr), 
                                           max.col(replace(temp_df, is.na(temp_df), -Inf)))]

cses_pr <- cses_pr %>% mutate (cong_closest_exp= abs(exp_closest-ideol_self))


##### MEAN - Voters #####

cols <- grep("^ideol_mean", names(cses_pr))
temp_df <- -abs(cses_pr[cols] - cses_pr$ideol_self)
cses_pr$meanv_closest <- cses_pr[cols][cbind(1:nrow(cses_pr), 
                                       max.col(replace(temp_df, is.na(temp_df), -Inf)))]

cses_pr <- cses_pr %>% mutate (cong_closest_meanv = abs(meanv_closest-ideol_self))

##### ELECTED - CONGRUENCE #####

cses_pr <- cses_pr %>% mutate (cong_elected= abs(ideol_elected_PR_1-ideol_self))
cses_pr <- cses_pr %>% mutate (cong_elected_exp= abs(exp_ideol_elected_PR_1-ideol_self))
cses_pr <- cses_pr %>% mutate (cong_elected_meanv= abs(meanv_ideol_elected_PR_1-ideol_self))


# EXPERT CLOSEST = PERCEIVED CLOSEST? DUMMY (PARA PODER CALCULAR % POR ELEI��O)

cses_pr$closest_cit_eq_exp <- with(cses_pr, closest == exp_closest)

#TABELA: 
#ct_closest <- crosstab(cses_pr$election , cses_pr$closest_cit_eq_exp)


##### CONGRU�NCIA PARTY VOTED (CITIZEN PERSPECTIVE) - SELF #####
cses_pr <- mutate (cses_pr, cong_PR_1 = abs(ideol_self - ideol_voted_PR_1))

##### CONGRU�NCIA PARTY VOTED (EXPERT) - SELF #####
cses_pr <- mutate (cses_pr, exp_cong_PR_1 = abs(ideol_self - exp_ideol_voted_PR_1))

##### CONGRU�NCIA PARTY VOTED (MEAN CITIZENS) - SELF #####
cses_pr <- mutate (cses_pr, meanv_cong_PR_1 = abs(ideol_self - meanv_ideol_voted_PR_1))


##### VOLUNTARY INCONGRUENCE #####
# Diferen�a entre congru�ncia com partido votado e com o closest
cses_pr$dif_cls_PR_1 <- cses_pr$cong_PR_1 - cses_pr$cong_closest

##### DIFEREN�A PARTY VOTED CITIZEN/EXPERT PLACEMENT #####

cses_pr$voter_exp_dif_PR_1 <- abs(cses_pr$exp_ideol_voted_PR_1 - cses_pr$ideol_voted_PR_1)


##### DIFEREN�A PARTY VOTED CITIZEN/MEAN CITIZENS #####

cses_pr$voter_meanv_dif_PR_1 <- abs(cses_pr$meanv_ideol_voted_PR_1 - cses_pr$ideol_voted_PR_1)


# VOTED CLOSEST PERCEIVED? DUMMY (%)
cses_pr$voted_closest_PR_1 <- with(cses_pr, as.numeric (closest == ideol_voted_PR_1))

# VOTED CLOSEST EXPERT? 
cses_pr$voted_exp_closest_PR_1 <- with(cses_pr, as.numeric (exp_closest == ideol_voted_PR_1))



##### PLURALITY #####


#system_pr tem 4 categorias: plurality (Mexico, Philippines e Taiwan), 
#abs. majority (segundo turno tradicional), qualified majority (s� Arg aqui),
#e electoral college (EUA)

#PLURALITY VS MAJORITY

cses_pr$plurality <- 0
cses_pr$plurality[cses_pr$system_PR == 1] <- 1

# OUTRAS DUMMIES PARA CADA CATEGORIA:

cses_pr$elec_college <- 0
cses_pr$elec_college[cses_pr$system_PR == 4] <- 1

cses_pr$quali_maj <- 0
cses_pr$quali_maj[cses_pr$system_PR == 3] <- 1

cses_pr$maj <- 0
cses_pr$maj[cses_pr$system_PR == 2] <- 1


##### 2� TURNO - DUMMY #####

cses_pr$round2_PR <- 0

cses_pr$round2_PR[cses_pr$election =="ARG_2015"] <- 1
cses_pr$round2_PR[cses_pr$election =="BRA_2002"] <- 1
cses_pr$round2_PR[cses_pr$election =="BRA_2006"] <- 1
cses_pr$round2_PR[cses_pr$election =="BRA_2010"] <- 1
cses_pr$round2_PR[cses_pr$election =="BRA_2014"] <- 1
cses_pr$round2_PR[cses_pr$election =="CHL_1999"] <- 1
cses_pr$round2_PR[cses_pr$election =="CHL_2005"] <- 1
cses_pr$round2_PR[cses_pr$election =="CHL_2009"] <- 1
cses_pr$round2_PR[cses_pr$election =="FRA_2002"] <- 1
cses_pr$round2_PR[cses_pr$election =="FRA_2012"] <- 1
cses_pr$round2_PR[cses_pr$election =="LTU_1997"] <- 1
cses_pr$round2_PR[cses_pr$election =="PER_2000"] <- 1
cses_pr$round2_PR[cses_pr$election =="PER_2001"] <- 1
cses_pr$round2_PR[cses_pr$election =="PER_2006"] <- 1
cses_pr$round2_PR[cses_pr$election =="PER_2011"] <- 1
cses_pr$round2_PR[cses_pr$election =="PER_2016"] <- 1
cses_pr$round2_PR[cses_pr$election =="ROU_1996"] <- 1
cses_pr$round2_PR[cses_pr$election =="ROU_2004"] <- 1
cses_pr$round2_PR[cses_pr$election =="ROU_2009"] <- 1
cses_pr$round2_PR[cses_pr$election =="ROU_2014"] <- 1
cses_pr$round2_PR[cses_pr$election =="SRB_2012"] <- 1
cses_pr$round2_PR[cses_pr$election =="URY_2009"] <- 1


##### TRANSFORMA��ES #####

invert <- function(x, na.rm = FALSE) (x * -1)

cses_pr <- cses_pr %>%
    mutate_at(vars(contains("cong")),
              invert) 


#RILE - MANIFESTO (transformando em escala 1 a 10)



##### FUN��O - ESCALA DE VARI�VEIS CONGRU�NCIA 
# scale_per <- function(x, na.rm = FALSE) (1+ (x/-10))
# 
# cses_pr <- cses_pr %>%
#   mutate_at(vars(contains("cong"), dif_cls_PR_1, voter_exp_dif_PR_1),
#             scale_per) 

##### EXPLICA��O #####
#A escala iria de 0 a 10 (sendo 0 total congru�ncia e 10 maior dist�ncia)
#Dividimos por -10 porque queremos dividir por 10 para ir de 0 a 1,
#e multiplicar por -1 para ficar mais intuitivo (maior congru�ncia, maiores 
#n�meros). Assim, o que antes seria 10 (menor congru�ncia) fica como -1, se
#aproximando de 0 quanto maior a congru�ncia. Ent�o adicionamos 1, para ficar
# de 0 (maior dist�ncia, antigo 10) at� 1 (congru�ncia total, antigo 0).


#### EFFECTIVE NUMBER OF PRESIDENTIAL CANDIDATES #####

cses_pr$cname <- substr(cses_pr$election, 1, 3)

des <- read.csv("C:/Users/livia/OneDrive - usp.br/TESE/DATASETS/Democratic Electoral Systems - GOLDER/es_data-v3.csv")
#Source: http://mattgolder.com/elections

des <- des %>% filter (presidential == 1 & year > 1994) # O ano nem precisava filtrar, mas o "presidential" sim sen�o
#vai dar match errado, porque �s vezes tem duas elei��es, uma legislativa outra presidencial, no mesmo ano
#

#Mesmo processo que fiz com o CMP (criar vari�vel "election" a partir de nome do pa�s e ano):
des$cname <- cses_pr$cname[match(des$country, cses_pr$country)]

des$election <- str_c(des$cname, "_", des$year)

#AGORA SIM, O MATCH PARA INSERIR A VARI�VEL NO CSES: 
cses_pr$enpres <- des$enpres[match(cses_pr$election, des$election)]

# PERU_2000 FOI A �NICA QUE FICOU FALTANDO 
#1/(0.4987 ^2) + (0.4024 ^2) + (0.3 ^2) + (0.223 ^2) + (0.18 ^2)  + (0.138^2) + ( 0.072^2) + ( 0.042^2) + ( 0.033^2)
cses_pr$enpres[cses_pr$election =="PER_2000"] <- 4.382017

#Faltou tamb�m BLR_2001 e Russia mas esses j� vou dropar por outros problemas maiores



save(cses_pr, file = "cses_pr.Rdata")
