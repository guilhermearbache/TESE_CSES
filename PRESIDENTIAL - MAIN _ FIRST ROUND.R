#rm(list=ls()[!(ls() %in% c("cses_leg", "cses_pr", "cses"))])

#rm(list=ls()[!(ls() %in% c("cses"))])

#load("CSES_w_Manifesto.RData")


library(dplyr)
#library(descr)

# CORRIGIR PARA ARQUIVO NOVO, EM TESE_CSES: load("C:/Users/livia/OneDrive - usp.br/TESE/PROJETO - CSES/INTRO-EDIT/cses.RData")


#setwd("C:/Users/livia/OneDrive - usp.br/TESE/PROJETO - CSES/PRESIDENTIAL")


##### EDIÇÕES INICIAIS/ MISSING #####

# FILTRANDO SÓ OS BANCOS/PAÍSES COM ELEIÇÃO PARA PRESIDENTE  

cses_pr <- cses %>% filter (type == 20 | type == 12) %>%
  select (-starts_with("vote_UH"), -starts_with("vote_LH"))


# Transformando class das variáveis para poder aplicar case_when e tirando missings

cses_pr <- cses_pr %>%
  mutate_at(vars(starts_with("ex_ideolparty"), starts_with("ideolparty"), starts_with("vote"),
                 elected_pr), as.numeric) 

# MISSING

#Creio não ter nenhuma variável com "ideol" no nome além de todas de placement (self, party, leader
# e suas versões alternativas/expert). Então uso essa palavra para atribuir todos missings dessas:

cses_pr <- cses_pr %>%
  mutate_at(.vars = vars(contains("ideol")), 
            .funs = list(~ifelse(. > 10, NA, .)))

cses_pr <- cses_pr %>%
  mutate_at(.vars = vars(contains("vote")), 
            .funs = list(~ifelse(. >9999900, NA, .)))


##### CORREÇÕES DE COALIZÕES #####

#ESSA SEÇÃO É A DIFERENÇA PARA O ARQUIVO ANTERIOR (QUE ESTÁ NA PASTA "PROJETO_CSES")

# Antes de ideology_voted e ideology_elected, faço certas correções em pcv, 
#ideology_party e ideology_voted para adequar aos partidos que realmente estavam concorrendo na eleição

##### ARGENTINA #####

#ARG_2015 está codificado como tendo eleito a coalizão (0320002, ou party B). Que não tem em "ideology_party" 
#mas tem em "ex_ideology_party" .

#Esse país é confuso, porque tem várias etapas de eleição (uma espécie de primárias, com vários candidatos de cada
#aliança, depois a eleição geral e um possível segundo turno). Tudo aqui parece se referir a essa eleição geral, 
#os votos são para as alianças participantes. (Mais observações na planilha). 

#Estratégia: para preservar o que os experts atribuíram a party B, mas não ficar com NA
#nem em "ideology_voted" para muitos entrevistados, nem em "ideology_elected" para todos, 
#vamos transformar ideology_party_B em média de G e H.

cses_pr <- cses_pr %>% mutate (
  ideolparty_B = case_when(
    election == "ARG_2015" ~ (ideolparty_G + ideolparty_H)/2,
       TRUE          ~ ideolparty_B
  )
)

#Com isso perdemos alguns valores onde um dos dois partidos está como "NA" mas o outro não
#(são poucos casos, a maioria onde um está missing outro também). E também não temos dados para o terceiro
#partido da aliança, o  Civic Coalition (CC-ARI). 


## PARA PARTY_D(Aliança "Workers' Left Front") e PARTY_F (Aliança "Compromiso Federal") não temos
# nenhum partido-membro incluído nas outras letras (A-F são as 6 principais alianças, depois 2
#partidos da aliança de PARTY_B e por último o Partido Justicialista, que é o principal da aliança
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

# 2006 - incluir Heloísa Helena (ideolparty não tem mas ela consta como Leader I):
# É preciso tomar cuidado - se o Partido I constar em alguma outra coisa, essa alteração de 
#pcv pode distorcer os dados. Mas aparentemente não há partido I pelo menos em ideolparty e ex_ideolparty)

cses_pr <- cses_pr %>% mutate (
  pcv_PR_F = case_when(
    election == "BRA_2006" ~ 6.85 ,
    TRUE          ~ pcv_PR_I
  )
)

#2010

cses_pr <- cses_pr %>%
  mutate(pcv_PR_B = na_if(election, "BRA_2010"))

cses_pr <- cses_pr %>%
  mutate(pcv_PR_D = na_if(election, "BRA_2010"))

cses_pr <- cses_pr %>%
  mutate(pcv_PR_E = na_if(election, "BRA_2010"))

cses_pr <- cses_pr %>%
  mutate(pcv_PR_F = na_if(election, "BRA_2010"))

cses_pr <- cses_pr %>%
  mutate(pcv_PR_G = na_if(election, "BRA_2010"))

cses_pr <- cses_pr %>%
  mutate(pcv_PR_H = na_if(election, "BRA_2010"))

cses_pr <- cses_pr %>%
  mutate(pcv_PR_I = na_if(election, "BRA_2010"))


#FRA_2002

# Em todas variáveis (inclusive VOTO), está tudo certo, mas o "elected" ficou 2500001, 
# Union for a Popular Movement (UMP), que foi o partido que Chirac criou fundindo o seu com outros logo depois.
#Por isso vamos alterar:

cses_pr <- cses_pr %>% mutate (
  elected_pr = case_when(
    election == "FRA_2002" ~ 2500008 ,
    TRUE          ~ elected_pr
  )
)


##DEPOIS PROSSEGUIR NO ARQUIVO "CORREÇÃO PARTIDOS", JUNTA LO AQUI. 

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

#PARA ESSA TABELA FUNCIONAR NÃO PODE HAVER A TRANSFORMAÇÃO DE MISSING LÁ EM CIMA 
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