#rm(list=ls()[!(ls() %in% c("cses_leg", "cses_pr", "cses"))])

#load("cses.RData")


library(dplyr)
#library(descr)

load("C:/Users/livia/OneDrive - usp.br/TESE/PROJETO - CSES/INTRO-EDIT/cses.RData")


#setwd("C:/Users/livia/OneDrive - usp.br/TESE/PROJETO - CSES/PRESIDENTIAL")


##### EDIÇÕES INICIAIS #####

# FILTRANDO SÓ OS BANCOS/PAÍSES COM ELEIÇÃO PARA PRESIDENTE  

cses_pr <- cses %>% filter (type == 20 | type == 12) %>%
  select (-starts_with("vote_UH"), -starts_with("vote_LH"))

#AGRUPANDO POR PAÍS-ELEIÇÃO PARA VISUALIZAR OS DADOS:

#tab_pr <- cses_pr %>% group_by(election, country) %>%
 # summarize_all (.funs = c(mean="mean"))

###### IDEOLOGY - PARTY VOTED #####

### PARA PODER USAR O CASE_WHEN, MUDAR TUDO PARA MESMO TIPO (NUMERIC):
cses_pr <- cses_pr %>%
  mutate_at(vars(starts_with("ex_ideolparty"), starts_with("ideolparty"), starts_with("vote"),
            elected_pr), as.numeric) 

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



##### % DE TIPOS DE MISSING EM IDEOLOGIA #####

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


# TabelaS com proporções de cada valor de "ideol_voted" e "ideol-self" por país-ano:

#table <- with (cses_pr, table (interaction(election), interaction(ideol_PR_1_ch)))
#table2 <- with (cses_pr, table (interaction(election), interaction(ideolself_ch)))
#write.csv(table, file = "TABLES/Ideol_voted_PR1.csv")
#write.csv(table2, file = "TABLES/Ideol_self_PR Subset.csv")





##### LIMPEZA DE VARIÁVEIS E MISSINGS #####

#### LIMPANDO MISSINGS EM IDEOLOGIA (SELF, PARTY VOTED, PARTIDOS):

cses_pr <- cses_pr %>%
  mutate_at(.vars = vars(contains("ideol")), 
            .funs = funs(ifelse(. > 90, NA, .)))

##### TIRANDO ALGUMAS VARIÁVEIS QUE NÃO USAREMOS AGORA:

cses_pr <- cses_pr %>% select (-starts_with("numparty"), -starts_with("prevote"), 
                               -contains("UH"), - contains("LH"), -contains("ch"))


##### CLOSEST PARTY - Voter Perspective#####

cols <- grep("^ideolparty", names(cses_pr))

temp_df <- -abs(cses_pr[cols] - cses_pr$ideol_self)
cses_pr$closest <- cses_pr[cols][cbind(1:nrow(cses_pr), 
                                       max.col(replace(temp_df, is.na(temp_df), -Inf)))]

cses_pr <- cses_pr %>% mutate (cong_closest = abs(closest-ideol_self))


##### CLOSEST PARTY - Expert Perspective #####

cols <- grep("^ex_ideolparty", names(cses_pr))

temp_df <- -abs(cses_pr[cols] - cses_pr$ideol_self)
cses_pr$exp_closest <- cses_pr[cols][cbind(1:nrow(cses_pr), 
                                           max.col(replace(temp_df, is.na(temp_df), -Inf)))]

##### CONGRUÊNCIA - PARTIDO MAIS PRÓXIMO (VOTER - CLOSEST PARTY DISTANCE) #####

cses_pr <- cses_pr %>% mutate (cong_closest_exp= abs(exp_closest-ideol_self))


##### ELECTED - CONGRUENCE #####

cses_pr <- cses_pr %>% mutate (cong_elected= abs(ideol_elected_PR_1-ideol_self))
cses_pr <- cses_pr %>% mutate (cong_elected_exp= abs(exp_ideol_elected_PR_1-ideol_self))


# EXPERT CLOSEST = PERCEIVED CLOSEST? DUMMY (PARA PODER CALCULAR % POR ELEIÇÃO)

cses_pr$closest_cit_eq_exp <- with(cses_pr, closest == exp_closest)

#TABELA: 
#ct_closest <- crosstab(cses_pr$election , cses_pr$closest_cit_eq_exp)


##### CONGRUÊNCIA PARTY VOTED (CITIZEN PERSPECTIVE) - SELF #####
cses_pr <- mutate (cses_pr, cong_PR_1 = abs(ideol_self - ideol_voted_PR_1))

##### CONGRUÊNCIA PARTY VOTED (EXPERT) - SELF #####
cses_pr <- mutate (cses_pr, exp_cong_PR_1 = abs(ideol_self - exp_ideol_voted_PR_1))

##### VOLUNTARY INCONGRUENCE #####
# Diferença entre congruência com partido votado e com o closest
cses_pr$dif_cls_PR_1 <- cses_pr$cong_PR_1 - cses_pr$cong_closest


##### DIFERENÇA PARTY VOTED PERCEIVED/EXPERT #####

cses_pr$voter_exp_dif_PR_1 <- abs(cses_pr$exp_ideol_voted_PR_1 - cses_pr$ideol_voted_PR_1)


# VOTED CLOSEST PERCEIVED? DUMMY (%)
cses_pr$voted_closest_PR_1 <- with(cses_pr, as.numeric (closest == ideol_voted_PR_1))

# VOTED CLOSEST EXPERT? 
cses_pr$voted_exp_closest_PR_1 <- with(cses_pr, as.numeric (exp_closest == ideol_voted_PR_1))



#### NOVA TABELA COM SUMÁRIOS - MÉDIAS E PORCENTAGENS:
#tab_pr <- cses_pr %>% group_by(election, country) %>%
 # summarize_all (.funs = c(mean="mean"), na.rm = T)

#write.csv(tab_pr, file = "TABLES/Summary - final - Presidential 1.csv")


#write.csv(cses_pr, file = "TABLES/CSES - Presidential 1 - final.csv")


##### FUNÇÃO - ESCALA DE VARIÁVEIS CONGRUÊNCIA #####
scale_per <- function(x, na.rm = FALSE) (1+ (x/-10))

cses_pr <- cses_pr %>%
  mutate_at(vars(contains("cong"), dif_cls_PR_1, voter_exp_dif_PR_1),
            scale_per) 

##### EXPLICAÇÃO #####
#A escala iria de 0 a 10 (sendo 0 total congruência e 10 maior distância)
#Dividimos por -10 porque queremos dividir por 10 para ir de 0 a 1,
#e multiplicar por -1 para ficar mais intuitivo (maior congruência, maiores 
#números). Assim, o que antes seria 10 (menor congruência) fica como -1, se
#aproximando de 0 quanto maior a congruência. Então adicionamos 1, para ficar
# de 0 (maior distância, antigo 10) até 1 (congruência total, antigo 0).







#####

save(cses_pr, file = "cses_pr.Rdata")