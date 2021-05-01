#rm(list=ls()[!(ls() %in% c("cses"))])
#rm(list=ls()[!(ls() %in% c("cses_leg", "cses_pr", "cses"))])

#rm(list=ls())

library(dplyr)
library(descr)

#load("cses.RData")

#setwd("C:/Users/livia/OneDrive - usp.br/TESE/PROJETO - CSES/LEGISLATIVO")

load("CSES_Manifesto.RData")

#####  ELEIÇÕES LEGISLATIVAS #####

## SUBSET - SÓ OS BANCOS/PAÍSES COM ELEIÇÃO PARA LEGISLATIVO  

cses_leg <- cses %>% filter (type == 10 | type == 12 | type == 13) %>%
  select (-starts_with("vote_PR"))


##### PARTY_IDEOLOGY - MEAN OF VOTERS PLACEMENT #####

grouped_LEG<- cses_leg %>% select (country, election, ideolparty_A:ideolparty_I) %>% 
  group_by (country, election)  %>% 
  summarize_all (.funs = c(mean="mean"), na.rm = T) 


cses_leg$ideol_mean_A <- grouped_LEG$ideolparty_A_mean[match(cses_leg$election, grouped_LEG$election)]
cses_leg$ideol_mean_B <- grouped_LEG$ideolparty_B_mean[match(cses_leg$election, grouped_LEG$election)]
cses_leg$ideol_mean_C <- grouped_LEG$ideolparty_C_mean[match(cses_leg$election, grouped_LEG$election)]
cses_leg$ideol_mean_D <- grouped_LEG$ideolparty_D_mean[match(cses_leg$election, grouped_LEG$election)]
cses_leg$ideol_mean_E <- grouped_LEG$ideolparty_E_mean[match(cses_leg$election, grouped_LEG$election)]
cses_leg$ideol_mean_F <- grouped_LEG$ideolparty_F_mean[match(cses_leg$election, grouped_LEG$election)]
cses_leg$ideol_mean_G <- grouped_LEG$ideolparty_G_mean[match(cses_leg$election, grouped_LEG$election)]
cses_leg$ideol_mean_H <- grouped_LEG$ideolparty_H_mean[match(cses_leg$election, grouped_LEG$election)]
cses_leg$ideol_mean_I <- grouped_LEG$ideolparty_I_mean[match(cses_leg$election, grouped_LEG$election)]


##### GALLAGHER INDEX OF DISPROPORTIONALITY (inserindo variável de outro banco) #####
#VOU DEIXAR TUDO ISSO COMENTADO PORQUE JÁ FIZ O PROCESSO:

#disp <- read.csv2("C:/Users/user/Google Drive/BANCOS DE DADOS/Disproportionality/Disproportionality.csv")
## SOURCE: http://christophergandrud.github.io/Disproportionality_Data/

#disp$cname <- countrycode(disp$iso2c, origin = "iso2c", destination = "iso3c")

#Criar uma variável para país e ano 

#disp$cyear <- paste(disp$cname, disp$year) 
#disp$cyear <- gsub(" ", "_", disp$cyear)

### PODERIA USAR A NOSSA VARIÁVEL TRADICIONAL DO CSES ("election") mas em alguns casos
#(Bélgica, Alemanha) tem um 4º caractere ao invés do "_". Portanto faço uma alteração :

#cses_leg$cyear <- cses_leg$election
#substr(cses_leg$cyear, 4, 4) <- "_"

# Agora sim, vamos juntar os dados:

#cses_leg$gallagher <- disp$disproportionality[match(cses_leg$cyear, disp$cyear)]

# AGORA VOU CRIAR UMA PLANILHA PARA inserir/corrigir manualmente alguns dados 
# a partir do pdf original de Gallagher:

#tab_leg <- cses_leg %>% select (election, year, country, gallagher, vote_LH_PL, 
 #                               vote_LH_DC, vote_UH_PL, vote_UH_DC_1) %>%
  #   group_by(election, country) %>%
   # summarize_all (.funs = c(mean="mean"))

#write.csv(dispcses, file = "Gallagher LS.csv")
##### BAIXANDO VERSÃO FINAL DE DISP E JUNTANDO#####

disp <- read.csv("Disproport_new.csv") 
cses_leg$gallagher <- disp$gallagher_mean[match(cses_leg$election, disp$election)]


##### VARIÁVEL country_year diferente (para juntar estudos diferentes em 
#Alemanha e Bélgica):

cses_leg$cyear <- cses_leg$election
substr(cses_leg$cyear, 4, 4) <- "_"

#Como Presidencial não tem esses 2 países-anos, mais fácil fazer só para Legislativo. 
#Mas se for fazer modelos com Leg e Executive elections, aí tenho que incluir isso no CSES.


##### LIMPEZA DE VARIÁVEIS E MISSINGS #####

#LIMPANDO MISSINGS EM IDEOLOGIA (SELF, PARTY VOTED, PARTIDOS):

cses_leg <- cses_leg %>%
  mutate_at(.vars = vars( ideol_self,
                         starts_with("ideolparty"),
                         starts_with("ex_ideol")), 
            .funs = funs(ifelse(. > 90, NA, .)))



##### 1. IDEOLOGY VOTED #####

### PRIMEIRO TRANSFORMAMOS TUDO EM NUMERIC PARA USAR O CASE_WHEN EM IDEOLOGY_VOTED

cses_leg <- cses_leg %>%
  mutate_at(vars(starts_with("ex_ideolparty"), starts_with("ideolparty"), starts_with("ideol_mean"), starts_with("vote")),
            as.numeric) 

##### LOWER CHAMBER ##### 

##### LH - PARTY LIST #####

## VOTER PLACEMENT ## 

cses_leg <- cses_leg %>% mutate (
  ideol_voted_LH_PL = case_when(
    numparty_A == vote_LH_PL ~ ideolparty_A,
    numparty_B == vote_LH_PL ~ ideolparty_B,
    numparty_C == vote_LH_PL ~ ideolparty_C,
    numparty_D == vote_LH_PL ~ ideolparty_D,
    numparty_E == vote_LH_PL ~ ideolparty_E,
    numparty_F == vote_LH_PL ~ ideolparty_F,
    numparty_G == vote_LH_PL ~ ideolparty_G,
    numparty_H == vote_LH_PL ~ ideolparty_H,
    numparty_I == vote_LH_PL ~ ideolparty_I,
    TRUE                    ~ vote_LH_PL
  )
)

## EXPERT PLACEMENT ##

cses_leg <- cses_leg %>% mutate (
  exp_ideol_voted_LH_PL = case_when(
    numparty_A == vote_LH_PL ~ ex_ideolparty_A,
    numparty_B == vote_LH_PL ~ ex_ideolparty_B,
    numparty_C == vote_LH_PL ~ ex_ideolparty_C,
    numparty_D == vote_LH_PL ~ ex_ideolparty_D,
    numparty_E == vote_LH_PL ~ ex_ideolparty_E,
    numparty_F == vote_LH_PL ~ ex_ideolparty_F,
    numparty_G == vote_LH_PL ~ ex_ideolparty_G,
    numparty_H == vote_LH_PL ~ ex_ideolparty_H,
    numparty_I == vote_LH_PL ~ ex_ideolparty_I,
    TRUE                    ~ vote_LH_PL
  )
)

## VOTER MEAN ## 

cses_leg <- cses_leg %>% mutate (
  meanv_ideol_voted_LH_PL = case_when(
    numparty_A == vote_LH_PL ~ ideol_mean_A,
    numparty_B == vote_LH_PL ~ ideol_mean_B,
    numparty_C == vote_LH_PL ~ ideol_mean_C,
    numparty_D == vote_LH_PL ~ ideol_mean_D,
    numparty_E == vote_LH_PL ~ ideol_mean_E,
    numparty_F == vote_LH_PL ~ ideol_mean_F,
    numparty_G == vote_LH_PL ~ ideol_mean_G,
    numparty_H == vote_LH_PL ~ ideol_mean_H,
    numparty_I == vote_LH_PL ~ ideol_mean_I,
    TRUE                    ~ vote_LH_PL
  )
)

##### TIRANDO ALGUMAS VARIÁVEIS QUE NÃO USAREMOS AGORA:
cses_leg <- cses_leg %>% select (-starts_with("prevote"), 
                                 -contains("_PR"), -contains("ch"))


##### 2.CLOSEST PARTY/ CONG_CLOSEST #####

#VOTER PLACEMENT #

cses_leg <- data.frame(cses_leg)


cols <- grep("^ideolparty", names(cses_leg))

temp_df <- -abs(cses_leg[cols] - cses_leg$ideol_self)
cses_leg$closest <- cses_leg[cols][cbind(1:nrow(cses_leg), 
                                         max.col(replace(temp_df, is.na(temp_df), -Inf)))]

# CONGRUÊNCIA COM O PARTIDO MAIS PRÓXIMO (VOTER - CLOSEST PARTY DISTANCE)
cses_leg <- cses_leg %>% mutate (cong_closest = abs(closest-ideol_self))


# EXPERT PLACEMENT #

cols <- grep("^ex_ideolparty", names(cses_leg))

temp_df <- -abs(cses_leg[cols] - cses_leg$ideol_self)
cses_leg$exp_closest <- cses_leg[cols][cbind(1:nrow(cses_leg), 
                                             max.col(replace(temp_df, is.na(temp_df), -Inf)))]

# CONGRUÊNCIA COM O PARTIDO MAIS PRÓXIMO (VOTER - CLOSEST PARTY DISTANCE)

cses_leg <- cses_leg %>% mutate (cong_closest_exp= abs(exp_closest-ideol_self))


#MEAN OF VOTERS PLACEMENT

cols <- grep("^ideol_mean", names(cses_leg))

temp_df <- -abs(cses_leg[cols] - cses_leg$ideol_self)
cses_leg$meanv_closest <- cses_leg[cols][cbind(1:nrow(cses_leg), 
                                         max.col(replace(temp_df, is.na(temp_df), -Inf)))]

# CONGRUÊNCIA COM O PARTIDO MAIS PRÓXIMO (VOTER - CLOSEST PARTY DISTANCE)
cses_leg <- cses_leg %>% mutate (cong_closest_meanv = abs(meanv_closest-ideol_self))



# EXPERT CLOSEST = PERCEIVED CLOSEST? DUMMY (PARA PODER CALCULAR % POR ELEIÇÃO)

cses_leg$closest_cit_exp<- with(cses_leg, closest == exp_closest)

#TABELA: 
# ct_closest <- crosstab(cses_leg$election , cses_leg$closest_cit_exp)


##### CONGRUÊNCIA - PARTY VOTED #####

#CITIZEN PLACEMENT 
cses_leg <- mutate (cses_leg, cong_LH_PL = abs(ideol_self - ideol_voted_LH_PL))


# EXPERT #
cses_leg <- mutate (cses_leg, exp_cong_LH_PL = abs(ideol_self - exp_ideol_voted_LH_PL))

# MEAN OF CITIZENS PLACEMENT #
cses_leg <- mutate (cses_leg, meanv_cong_LH_PL = abs(ideol_self - meanv_ideol_voted_LH_PL))


##### VOLUNTARY INCONGRUENCE #####
# Diferença entre congruência com partido votado e com o closest
cses_leg$dif_cls_LH_PL <- cses_leg$cong_LH_PL - cses_leg$cong_closest

##### DIFERENÇA DE PERCEPÇÃO VOTER/EXPERT #####

cses_leg$voter_exp_dif_LH_PL <- abs(cses_leg$exp_ideol_voted_LH_PL - cses_leg$ideol_voted_LH_PL)

##### DIFERENÇA DE PERCEPÇÃO VOTER/MEAN OF VOTERS #####

cses_leg$voter_meanv_dif_LH_PL <- abs(cses_leg$meanv_ideol_voted_LH_PL - cses_leg$ideol_voted_LH_PL)


# VOTED CLOSEST PERCEIVED? DUMMY (%)
cses_leg$voted_closest_LH_PL <- with(cses_leg, as.numeric (closest == ideol_voted_LH_PL))

# VOTED CLOSEST EXPERT? DUMMY (%)
cses_leg$voted_exp_closest_LH_PL <- with(cses_leg, as.numeric (exp_closest == ideol_voted_LH_PL))


source("2-1-2 - LH_DC.R")


##### FUNÇÃO - ESCALA DE VARIÁVEIS CONGRUÊNCIA #####
invert <- function(x, na.rm = FALSE) (x * -1)

cses_leg <- cses_leg %>%
  mutate_at(vars(contains("cong")),
            invert) 

#source("C:/Users/user/Google Drive/TESE - Congruência/ARQUIVOS - CSES/2-2-1 - UPPER HOUSE - PL.R")
#source("C:/Users/user/Google Drive/TESE - Congruência/ARQUIVOS - CSES/2-2-2-1 - UH_DC_1.R")
#source("C:/Users/user/Google Drive/TESE - Congruência/ARQUIVOS - CSES/2-2-2-2 - UH_DC_2.R")
#source("C:/Users/user/Google Drive/TESE - Congruência/ARQUIVOS - CSES/2-2-2-3 - UH_DC_3.R")
#source("C:/Users/user/Google Drive/TESE - Congruência/ARQUIVOS - CSES/2-2-2-4 - UH_DC_4.R")


# CRIANDO UMA TABELA PARA IDENTIFICAR DIFERENÇA DE TURNOUT NA POPULAÇÃO E NA AMOSTRA:

turnout <- cses_leg %>% group_by(election) %>%
  select (voted_LH, turnout, voted) %>% 
  summarize_all (.funs = c(mean="mean"), na.rm = T) 

turnout$dif <- (turnout$turnout_mean/100) - turnout$voted_mean


#tab_cses <- cses_leg %>% group_by(election) %>%
  # select (starts_with("ideol"), starts_with ("vote_LH"), turnout) %>% 
  # summarize_all (.funs = c(mean="mean"), na.rm = T) 

#write.csv(tab_cses, file = "Legislative")


save(cses_leg, file = "cses_leg.RData")







