##### PACOTES EXIGIDOS #####

library(tidyverse)
library(plyr); library(dplyr)
library(emdist)

#### SUMMARIZE ####

# CRIAR UM BANCO AGRUPADO POR PA�S/ELEI��O PARA PODER USAR A IDEOLOGIA DE CADA PARTIDO 
# DE ACORDO COM SEUS VOTOS 

# ***** BOTAR PARTIDO ELEITO, OS QUE FORAM PARA SEGUNDO #####
PR_grouped <- cses_pr %>% 
  select (election, country, contains("ideolparty"), starts_with("pcv_PR"), elected_pr) %>%
  group_by(election, country) %>%
  summarize_all (.funs = c(mean="mean"), na.rm = T) 

## CONFERIR DADOS ELECTED

## PEGAR DADOS DE SEGUNDO TURNO

### FAZER CIT-VOTER, VOTER-VOTES, VOTES-ELECTED, DEPOIS VER O QUE FAZER COM 2? TURNO
#CIT-VOTER 2?, VOTER 1? -VOTER 2?; VOTER-VOTES 2?, VOTES 2? - ELECTED


#### TIDY DATA - GATHER #### - PARA FAZER O BANCO PONDERADO POR VOTES 


# TODAS VARI�VEIS RELACIONADAS A PARTY A-I EST�O EM COLUNAS, PRECISAMOS DELAS EM ROWS,
# PARA PODER CONSIDERAR CADA PARTIDO COMO UMA "OBSERVA��O" (E DEPOIS MULTIPLIC�-LOS DE
#ACORDO COM SEU % DE VOTOS)


##### RENOMEANDO ##### 

#SEGUNDO NUMERAL � O QUE VAI SER CADA COLUNA , CADA VAR. Terceiro n�mero � o loop, o que deve virar row (cada partido)  
#Q3.2.1. mas na minha vers�o Q.3.2_A_mean, etc.

names (PR_grouped) <- gsub("ex_ideolparty", "Q3.1", names(PR_grouped))
names (PR_grouped) <- gsub("ideolparty", "Q3.2", names(PR_grouped))
names (PR_grouped) <- gsub("pcv_PR", "Q3.3", names(PR_grouped))

##### GATHERING #####

long_PR <- PR_grouped %>%
  gather(key, value, -election, -country, -elected_pr_mean) %>%
  extract(key, c("var", "party"), "(Q.\\..)\\_(.\\_mean)") %>%
  spread(var, value)

### FIZ GATHER DE TODAS VARI�VEIS MENOS ELECTION E COUNTRY, E DEPOIS UM SPREAD (CONTR�RIO
#DE GATHER, DE ROW PARA COLUNA DE NOVO) PARA AJUSTAR (PARA N�O FICAR UMA COLUNA PARA CADA VARI�VEL - 8 PARA EX_IDEOLPARTY + 8 PARA IDEOL, ETC., E SIM
#UMA PARA CADA PARTIDO, COMO SE CADA PARTIDO FOSSE UMA OBSERVA��O, COM AS INFORMA��ES RESPECTIVAS DE EX_IDEOL, ETC.)


### RENOMEANDO DE VOLTA:

names (long_PR) <- gsub("Q3.1","ex_ideolparty", names(long_PR))
names (long_PR) <- gsub("Q3.2","ideolparty", names(long_PR))
names (long_PR) <- gsub("Q3.3","pcv_PR", names(long_PR))
names (long_PR) <- gsub("elected_pr_mean","elected", names(long_PR))


long_PR$party <- gsub('_mean', '', long_PR$party)


##### ***** CONTINUAR DAQUI ***** #####

# FAZER A PLANILHA E CONFERIR - T� TUDO CERTO, CONTAR PARTIDOS PELO % VOTOS, ETC. 
#VER POR QUE ARGENTINA SEM NENHUM % ELECTED, E OUTROS CASOS QUE TEREI QUE EDITAR - NA M�O? 

write.csv(long_PR, file = "PRESIDENTIAL- grouped.csv")

##### BANCOS PARA % VOTES (E SEATS) ##### 

# REPETIR OBS DE ACORDO COM PCSEATS E PC VOTES (CRIAR DATASETS PARA CADA UM DESSES):

# Antes um recode de NaN para ZERO para n�o ter erros:
long_PR$pcv_LH[is.nan(long_PR$pcv_LH)]<-0








