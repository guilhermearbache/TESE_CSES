##### PACOTES EXIGIDOS #####

library(tidyverse)
library(plyr); library(dplyr)
library(emdist)

#### SUMMARIZE ####

# CRIAR UM BANCO AGRUPADO POR PAÍS/ELEIÇÃO PARA PODER USAR A IDEOLOGIA DE CADA PARTIDO 
# DE ACORDO COM SEUS VOTOS (E SEATS NO CASO DE LEGISLATIVAS)

LH_grouped <- cses_leg %>% group_by(election, country) %>%
  summarize_all (.funs = c(mean="mean"), na.rm = T) %>% 
    select (election, country, starts_with("pcv_LH"), starts_with("pcseats_LH"),
                                                    contains("ideolparty"))
#### TIDY DATA - GATHER ####


# TODAS VARIÁVEIS RELACIONADAS A PARTY A-I ESTÃO EM COLUNAS, PRECISAMOS DELAS EM ROWS,
# PARA PODER CONSIDERAR CADA PARTIDO COMO UMA "OBSERVAÇÃO" (E DEPOIS MULTIPLICÁ-LOS DE
#ACORDO COM SEU % DE VOTOS - E SEATS PARA LEGISLATIVO)


### HÁ DOIS CAMINHOS POSSÍVEIS - PODERIA FAZER UM POR VEZ (COM PCV E CITIZEN PERCEPTION,
#PCV E EXPERT - E PARA LEGISLATIVO AMBOS COM PCVOTES E PCSEATS), MAS AO INVÉS DISSO VOU 
#USAR UMA SOLUÇÃO "SUJA", PEGAR UM CÓDIGO DO STACK OVERFLOW

# https://stackoverflow.com/questions/25925556/gather-multiple-sets-of-columns

# PARA USAR ESSE CÓDIGO QUASE IGUAL, VOU MUDAR OS NOMES DAS VARIÁVEIS PARA Q3.1, ETC. 
#(PODERIA TER NOMES MAIS APROPRIADOS E ADAPTAR O CÓDIGO MAS DE QQ FORMA AS SUGESTÕES FEITAS
#NO STACK OVERFLOW ENVOLVEM TER NOMES DE VARIÁVEIS NUMA SEQUÊNCIA NUMÉRICA - 1,2,3...)


##### RENOMEANDO ##### 

#SEGUNDO NUMERAL É O QUE VAI SER CADA COLUNA , CADA VAR. Terceiro número é o loop, o que deve virar row (cada partido)  
#Q3.2.1. mas na minha versão Q.3.2_A_mean, etc.

names (LH_grouped) <- gsub("ex_ideolparty", "Q3.1", names(LH_grouped))
names (LH_grouped) <- gsub("ideolparty", "Q3.2", names(LH_grouped))
names (LH_grouped) <- gsub("pcv_LH", "Q3.3", names(LH_grouped))
names (LH_grouped) <- gsub("pcseats_LH", "Q3.4", names(LH_grouped))

##### GATHERING #####

long_LH <- LH_grouped %>%
  gather(key, value, -election, -country) %>%
  extract(key, c("var", "party"), "(Q.\\..)\\_(.\\_mean)") %>%
  spread(var, value)

###  EU FIZ UM GATHER DE TODAS VARIÁVEIS MENOS ELECTION E COUNTRY, E DEPOIS UM SPREAD (CONTRÁRIO
#DE GATHER, DE ROW PARA COLUNA DE NOVO) PARA AJUSTAR (PARA NÃO FICAR UMA COLUNA PARA CADA VARIÁVEL - 8 PARA EX_IDEOLPARTY + 8 PARA IDEOL, ETC., E SIM
#UMA PARA CADA PARTIDO, COMO SE CADA PARTIDO FOSSE UMA OBSERVAÇÃO, COM AS INFORMAÇÕES RESPECTIVAS DE EX_IDEOL, ETC.)


### RENOMEANDO DE VOLTA:

names (long_LH) <- gsub("Q3.1","ex_ideolparty", names(long_LH))
names (long_LH) <- gsub("Q3.2","ideolparty", names(long_LH))
names (long_LH) <- gsub("Q3.3","pcv_LH", names(long_LH))
names (long_LH) <- gsub("Q3.4","pcseats_LH", names(long_LH))

long_LH$party <- gsub('_mean', '', long_LH$party)

#write.csv(long_LH, file = "Lower House- grouped.csv")

##### BANCOS PARA % VOTES (E SEATS) ##### 

# REPETIR OBS DE ACORDO COM PCSEATS E PC VOTES (CRIAR DATASETS PARA CADA UM DESSES):

# Antes um recode de NaN para ZERO para não ter erros:
long_LH$pcv_LH[is.nan(long_LH$pcv_LH)]<-0
long_LH$pcseats_LH[is.nan(long_LH$pcseats_LH)]<-0


##### MULTIPLICANDO SEATS E VOTES POR 100 PARA A REPETIÇÃO DE ROWS SER MAIS PRECISA COM 
#OS DADOS QUE TEMOS.
#Afinal não é possível repetir uma linha por números quebrados (não existe "1,5 vezes 
#aquela linha", por ex)

# Também arredondei os dados em uma casa, para não ter nenhuma decimal na versão
#multiplicada por 10, para que fique mais próximo do que seria multiplicando por 100. 

long_LH <- long_LH %>%
  mutate(votes_LH = round(pcv_LH * 10, 0))

long_LH <- long_LH %>%
  mutate(seats_LH = round(pcseats_LH * 10, 0))


# MUITO LENTO COM * 100 (QUE APROVEITARIA TODO O DADO DISPONÍVEL), MUDEI PARA 10 - 
#DEPOIS DE TUDO RESOLVIDO, QUANDO FOR APENAS RODAR, POSSO MUDAR PARA 100 OU 50

#AGORA SIM, REPETINDO ROWS DE ACORDO COM % VOTES:
LH_votes <- long_LH[rep(seq(nrow(long_LH)), long_LH$votes_LH),]

LH_seats <- long_LH[rep(seq(nrow(long_LH)), long_LH$seats_LH),]

#### CALCULANDO EMDS:       

# FUNÇÃO - LUPU:
emd.dis <- function(data, metric = "manhattan", iterations=100000){
  df <- data
  x <- as.matrix(df$samps[which(df$pop == "x")])
  y <- as.matrix(df$samps[which(df$pop == "y")])
  weight.x <- rep(1/nrow(x),nrow(x))
  weight.y <- rep(1/nrow(y),nrow(y))
  emdw(x,weight.x,y,weight.y,dist=metric,max.iter=iterations)
}          

completeFun <- function(data, desiredCols) {
  completeVec <- complete.cases(data[, desiredCols])
  return(data[completeVec, ])
}


##### 1.CITIZENS-TO-VOTERS #####

##### 1.1.VOTERS VS ABSTENTS #####

#CRIAR OS BANCOS VOTER E NON-VOTER

voters_LH <- cses_leg %>% filter(voted_LH == 1)
abstents_LH <- cses_leg %>% filter(voted_LH == 0)

### AJEITAR OS BANCOS PARA A FUNÇÃO DE LUPU

# VARIÁVEL QUE IRÁ IDENTIFICAR OS DOIS GRUPOS DIFERENTES:
voters_LH$pop <- as.factor("x")
abstents_LH$pop <- as.factor("y")

# BIND
LH_abs_voter <- rbind(voters_LH, abstents_LH)

#DAR UMA FILTRADA PARA FICAR SÓ O QUE PRECISA 
LH_abs_voter <- LH_abs_voter %>% select (election, ideol_self, pop) %>%
  dplyr::rename (samps = ideol_self) 

#Tirar os missings:

LH_abs_voter <- completeFun(LH_abs_voter, "samps")

#### DDPLY - EMD! 

emd_LH_abs_voter <- ddply(LH_abs_voter, .(election), emd.dis)

##### 1.2.CITIZENS VS VOTERS  #####

#BANCO DE VOTERS JÁ ESTÁ CRIADO, DE TODOS CIDADÃOS TAMBÉM
#(É O ORIGINAL)


# A partir daqui, para não gerar confusão entre o banco
#original (por exemplo, já ter uma variável chamada "pop"
#com todos valores = "y"), vou criar um novo banco:

allcit_LH <- cses_leg


### AJEITAR OS BANCOS PARA A FUNÇÃO DE LUPU

# VARIÁVEL QUE IRÁ IDENTIFICAR OS DOIS GRUPOS DIFERENTES:
#Também já criada para voters (x), atribuir y a citizens.

allcit_LH$pop <- as.factor("y")

# BIND
LH_cit_voter <- rbind(voters_LH, allcit_LH)

#DAR UMA FILTRADA PARA FICAR SÓ O QUE PRECISA 
LH_cit_voter <- LH_cit_voter %>% select (election, ideol_self, pop) %>%
  dplyr::rename (samps = ideol_self) 

#Tirar os missings:

LH_cit_voter <- completeFun(LH_cit_voter, "samps")

#### DDPLY - EMD! 

emd_LH_cit_voter <- ddply(LH_cit_voter, .(election), emd.dis)


#GRÁFICOS:

ggplot(data=LH_abs_voter, aes(x=samps, group=pop, fill=pop)) +
  geom_density(adjust=10) +
  facet_wrap(~election) +
  theme(
    panel.spacing = unit(0.5, "lines"),
    axis.ticks.x=element_blank() 
  ) + labs (x="Distribuição ideológica - eleitores e ausentes")

#Cidadãos vs eleitores

ggplot(data=LH_cit_voter, aes(x=samps, group=pop, fill=pop)) +
  geom_density(adjust=10) +
  facet_wrap(~election) +
  theme(
    panel.spacing = unit(0.5, "lines"),
    axis.ticks.x=element_blank() 
  ) + labs (x="Distribuição ideológica - eleitores e ausentes")

##### 1.3.SAMPLE ALTERNATIVO #####
# Como há discordâncias entre a variável "voted" (IMD3001) e a variável 
#"Em quem votou", faço versões alternativas. 

cses_leg$alt_voted_LH <- cses_leg$voted_LH

# Esses casos permanecem iguais: 
#Didn't vote for anyone/ declared not voting = 0
#Declared someone voted/ declared voting = 1

#Aqui vamos nos basear na variável de "quem votou":

#Voted for someone / declared not voting" = 2

cses_leg$alt_voted_LH[cses_leg$vote_LH_DC < 9999988 & cses_leg$voted_LH==0 ] <- 1
cses_leg$alt_voted_LH[cses_leg$vote_LH_PL < 9999988 & cses_leg$voted_LH==0 ] <- 1

#Didn't vote for anyone/ declared voting = 3

#Aqui vou usar as que tiverem os seguintes códigos apenas:

# 9999999. MISSING/ABSTAINED (DID NOT VOTE)

cses_leg$alt_voted_LH[cses_leg$vote_LH_DC == 9999999 | cses_leg$vote_LH_PL == 9999999] <- 0

# Outras variáveis se referem a voto inválido, não sabe em quem votou, refused, votou em
#"outros da direita/esquerda", etc.


#SAMPLE ALTERNATIVO DE VOTERS E ABSTENTS

#CRIAR OS BANCOS VOTER E NON-VOTER

voters_LH <- cses_leg %>% filter(alt_voted_LH == 1)
abstents_LH <- cses_leg %>% filter(alt_voted_LH == 0)

### AJEITAR OS BANCOS PARA A FUNÇÃO DE LUPU

# VARIÁVEL QUE IRÁ IDENTIFICAR OS DOIS GRUPOS DIFERENTES:
voters_LH$pop <- as.factor("x")
abstents_LH$pop <- as.factor("y")

# BIND
LH_abs_voter <- rbind(voters_LH, abstents_LH)

#DAR UMA FILTRADA PARA FICAR SÓ O QUE PRECISA 
LH_abs_voter <- LH_abs_voter %>% select (election, ideol_self, pop) %>%
  dplyr::rename (samps = ideol_self) 

#Tirar os missings:

LH_abs_voter <- completeFun(LH_abs_voter, "samps")

#### DDPLY - EMD! 

emd_LH_alt_abs_voter <- ddply(LH_abs_voter, .(election), emd.dis)

##### 2.VOTERS-TO-VOTES #####

### AJEITAR OS BANCOS PARA A FUNÇÃO DE LUPU

# VARIÁVEL QUE IRÁ IDENTIFICAR OS DOIS GRUPOS DIFERENTES:
voters_LH$pop <- as.factor("x")
LH_votes$pop <- as.factor("y")

# LIMPANDO E RENOMEANDO OS DATASETS PARA O BIND

#Citizen placement (votes):
LH_cp_votes <- LH_votes %>% select (election, ideolparty, pop) %>%
  dplyr::rename (samps = ideolparty) 

voters_LH <- voters_LH %>% select (election, ideol_self, pop) %>%
  dplyr::rename (samps = ideol_self) 


# BIND
LH_voter_votes <- dplyr::bind_rows(voters_LH, LH_cp_votes)

#OBS: estava usando o rbind, mas em alguns casos como esse ele cria
#matrix ao invés de dataframe. Melhor usar o dplyr


#Tirar os missings:

LH_voter_votes <- completeFun(LH_voter_votes, "samps")


#### DDPLY - EMD! 

emd_LH_voter_votes <- ddply(LH_voter_votes, .(election), emd.dis)


##### 3.VOTES-TO-SEATS #####

# BANCOS JÁ CRIADOS ACIMA

### AJEITAR OS BANCOS PARA A FUNÇÃO DE LUPU

# VARIÁVEL QUE IRÁ IDENTIFICAR OS DOIS GRUPOS DIFERENTES:
LH_votes$pop <- as.factor("x")
LH_seats$pop <- as.factor("y")

# BIND
LH_votes_seats <- dplyr::bind_rows(LH_votes, LH_seats)

#DAR UMA FILTRADA PARA FICAR SÓ O QUE PRECISA 
LH_votes_seats <- LH_votes_seats %>% select (election, ideolparty, pop) %>%
  dplyr::rename (samps = ideolparty) 

#Tirar os missings:

LH_votes_seats <- completeFun(LH_votes_seats, "samps")

#### DDPLY - EMD! 

emd_LH_votes_seats <- ddply(LH_votes_seats, .(election), emd.dis)


##### 4.CONGRUÊNCIA TOTAL #####

#Já temos os bancos (o CSES original para citizens e LH_SEATS)

### AJEITAR OS BANCOS PARA A FUNÇÃO DE LUPU

# VARIÁVEL QUE IRÁ IDENTIFICAR OS DOIS GRUPOS DIFERENTES:
allcit_LH$pop <- as.factor("x")
LH_seats$pop <- as.factor("y")

#RENOMEANDO E LIMPANDO PARA JUNTAR:
LH_seats <- LH_seats %>% select (election, ideolparty, pop) %>%
  dplyr::rename (samps = ideolparty) 

allcit_LH <- allcit_LH %>% select (election, ideol_self, pop) %>%
  dplyr::rename (samps = ideol_self) 

# BIND
LH_totalcong <- dplyr::bind_rows(allcit_LH, LH_seats)


#Tirar os missings:

LH_totalcong <- completeFun(LH_totalcong, "samps")

#### DDPLY - EMD! 

emd_LH_totalcong <- ddply(LH_totalcong, .(election), emd.dis)


EMDS_LH <- full_join(emd_LH_abs_voter, emd_LH_cit_voter, by = "election") %>%              # Full outer join of multiple data frames
  full_join(., emd_LH_voter_votes, by = "election") %>%
  full_join(., emd_LH_votes_seats, by = "election") %>%
  full_join(., emd_LH_totalcong, by = "election")


colnames(EMDS_LH)<- c("election", "abs_voter", "cit_voter", "voter_votes",
                      "votes_seats", "total")

##### ** EXPERT PLACEMENT ** #####

##### 1. VOTERS-TO-VOTES #####


# LIMPANDO E RENOMEANDO OS DATASETS PARA O BIND

voters_LH <- voters_LH %>% select (election, ideol_self, pop) %>%
  dplyr::rename (samps = ideol_self) 

LH_ex_votes <- LH_votes %>% select (election, ex_ideolparty, pop) %>%
  dplyr::rename (samps = ex_ideolparty) 


voters_LH$pop <- as.factor("x")
LH_ex_votes$pop <- as.factor("y")


# BIND
LH_ex_voter_votes <- dplyr::bind_rows(voters_LH, LH_ex_votes)

#OBS: estava usando o rbind, mas em alguns casos como esse ele cria
#matrix ao invés de dataframe. Melhor usar o dplyr


#Tirar os missings:

LH_ex_voter_votes <- completeFun(LH_ex_voter_votes, "samps")


#### DDPLY - EMD! 

emd_LH_ex_voter_votes <- ddply(LH_ex_voter_votes, .(election), emd.dis)


##### 2.VOTES-TO-SEATS #####

# BANCOS JÁ CRIADOS ACIMA

### AJEITAR OS BANCOS PARA A FUNÇÃO DE LUPU

# VARIÁVEL QUE IRÁ IDENTIFICAR OS DOIS GRUPOS DIFERENTES:
LH_ex_votes$pop <- as.factor("x")
LH_seats$pop <- as.factor("y")

# LH VOTES JÁ ESTÁ TRANSFORMADO, ENTÃO TRANSFORMO O OUTRO
#E FAÇO O BIND:

LH_ex_seats <- LH_seats %>% select (election, ex_ideolparty, pop) %>%
  dplyr::rename (samps = ex_ideolparty)

# BIND
LH_ex_votes_seats <- dplyr::bind_rows(LH_ex_votes, LH_ex_seats)


#Tirar os missings:

LH_ex_votes_seats <- completeFun(LH_ex_votes_seats, "samps")

#### DDPLY - EMD! 

emd_LH_ex_votes_seats <- ddply(LH_ex_votes_seats, .(election), emd.dis)


##### 3.CONGRUÊNCIA TOTAL #####

#Já temos os bancos (o CSES original para citizens e LH_SEATS)

### AJEITAR OS BANCOS PARA A FUNÇÃO DE LUPU

# VARIÁVEL QUE IRÁ IDENTIFICAR OS DOIS GRUPOS DIFERENTES:
allcit_LH$pop <- as.factor("x")
LH_seats$pop <- as.factor("y")

#RENOMEANDO E LIMPANDO PARA JUNTAR:

allcit_LH <- allcit_LH %>% select (election, ideol_self, pop) %>%
  dplyr::rename (samps = ideol_self) 

# BIND
LH_ex_totalcong <- dplyr::bind_rows(allcit_LH, LH_ex_seats)


#Tirar os missings:

LH_ex_totalcong <- completeFun(LH_ex_totalcong, "samps")

#### DDPLY - EMD! 

emd_LH_ex_totalcong <- ddply(LH_ex_totalcong, .(election), emd.dis)


lista <- mget(c("emd_LH_abs_voter", "emd_LH_cit_voter",
                "emd_LH_voter_votes", "emd_LH_ex_voter_votes",
                "emd_LH_votes_seats","emd_LH_ex_votes_seats",
                "emd_LH_totalcong","emd_LH_ex_totalcong"))
                

lista <- map2(lista, names(lista),~{
  names(.x) <- c("ID",.y)
  .x
})

EMDS_LH <- reduce(lista, full_join, by = "ID")


rm(list=ls()[!(ls() %in% c("cses_leg", "cses_pr",
                          "cses", "EMDS_LH", "emd_LH_voter_votes",
                        "emd_LH_abs_voter", "emd_LH_alt_abs_voter", 
                        "emd_LH_cit_voter", "emd_LH_votes_seats",
                       "emd_LH_totalcong", "emd_LH_ex_totalcong",
                       "emd_LH_ex_voter_votes", "emd_LH_ex_votes_seats"))])

