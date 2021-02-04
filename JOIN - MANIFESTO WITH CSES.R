##### CSES #####

#Arrumar tab_cses para ter só essas variáveis de interesse - ELECTION, ID MANIFESTO SÓ? 

tab_cses <- cses %>% group_by(election, country) %>%
  select (country, election, starts_with("manif")) %>%
  summarize_all (.funs = c(mean="mean"), na.rm = T)

names (tab_cses) <- gsub("_mean", "", names(tab_cses))
names (tab_cses) <- gsub("manif", "party", names(tab_cses))


#CRIAR VARIÁVEL PARA PAÍS COM TRÊS LETRAS EM CSES:

tab_cses$cname <- substr(tab_cses$election, 1, 3)

# CORRIGIR BELF1999/BELW1999, DEU12002/DEU22002 - 

# RECODE DEU12002 

tab_cses[tab_cses$election == "DEU12002" , "election"] <- "DEU_2002"


##### OBS ##### 
#EXPLICAÇÃO:se eu apenas criar uma variável sem a quarta letra, de qualquer forma esses 4 casos vão virar 2 casos duplicados. 
# BEL_1999 não tem dados de partidos do Manifesto de qualquer forma, então, esse caso é melhor DROPAR.
#DEU1 e DEU2, por são idênticos nas colunas referentes a party_ID de Manifesto (como esperado). 
#Então só preciso de um deles

#Como ainda não sei fazer rodar o código de Junção abaixo com outras variáveis extras, de character, 
#além da variável que vai ser a chave para o match ("elections" no caso), melhor deixar como está e
#só alterar um dos dois DEU_2002 e dropar o outro. Se tivesse um caso de dois bancos diferentes no mesmo
#país-ano com IDs diferentes para os PARTIDOS DO MANIFESTO, aí a solução talvez fosse rodar os códigos
# de junção em separado para cada um dos bancos, depois juntá-los com o resto (aí poderia fazer ou
#com a quarta letra mas aí teria que adicionar essa quarta letra no banco original de manifesto também -
#primeiro para um dos bancos, tipo BEL_2002 recode para BELF_2002, depois a outra). Ou então
#poderia duplicar no Manifesto todas linhas relacionadas a BEL_2002, e recodificar uma cópia para 
#BELF, outra para BELW.

#tab_cses$election <- tab_cses$election

#substr(tab_cses$election, 4, 4) <- "_"



#"DEU22002", BELF/BELW DEIXEI - DEPOIS VAI SUMIR DO CORPUS LEAN MESMO! 


##### MANIFESTO #####

# Arrumar colunas de MANIFESTO PARA SER SÓ ID E ideology, e ELECTION! # Tem que fazer election!

library(manifestoR)
mp_setapikey("manifesto_apikey.txt")

###### MAIN #####

corpus_original <- mp_maindataset()

# Mudando data para ter só o ano (tirando meses):

corpus_original$date <- substr(corpus_original$date, 1, 4)


# EDIÇÕES PARA MATCH - ELECTIONS #

#DEPOIS, UM MATCH DESSA VARIÁVEL COM MANIFESTO, DE ACORDO COM NOME EXTENSO, PARA TER ELA AO INVÉS DE NOME EXTENSO APENAS 
corpus_original$cname <- tab_cses$cname[match(corpus_original$countryname, tab_cses$country)]


#RETIRANDO DADOS DUPLICADOS (duas eleições no mesmo ano-país, Snap Elections)
#Casos: GRC_2012, GRC_2015, TUR_2015. Vamos deixar só as que são o foco no CSES

##Turkey 2015 - eleições em junho e novembro, CSES focado na de junho (realizado por volta de julho)
#Portanto vamos tirar a "snap election", de novembro:

corpus_original <- corpus_original %>% 
  filter (corpus_original$countryname != "Turkey" | corpus_original$edate != "2015-11-01")

#Grécia - para 2012 a eleição de junho foi a considerada no CSES (2ª eleição, "snap"), tirar a de maio.
#Para 2015 foi a primeira (janeiro de 2015), tirar a de setembro.

corpus_original <- corpus_original %>% 
  filter (corpus_original$countryname != "Greece" | corpus_original$edate != "2012-05-06" &
            corpus_original$edate !="2015-09-20")


##### ADICIONANDO ALGUNS PAÍSES QUE NÃO FUNCIONOU MATCH:
corpus_original$cname[corpus_original$countryname == "United Kingdom"] <- "GBR"
corpus_original$cname[corpus_original$countryname == "United States"] <- "USA"
corpus_original$cname[corpus_original$countryname == "Russia"] <- "RUS"
corpus_original$cname[corpus_original$countryname == "South Korea"] <- "KOR"

#AÍ SÓ JUNTAR COM ANO PARA TER O EQUIVALENTE AO QUE TEMOS EM CSES COMO "ELECTION":
corpus_original$election <- str_c(corpus_original$cname, "_", corpus_original$date)

# LIMPEZA E EDIÇÃO FINAL #

corpus <- corpus_original %>% rename(ID = party, ideology = rile) %>% 
  select (election, ID, ideology)

##### LIMPAR MANIFESTO - SÓ ELEIÇÕES DE CSES #####


#CRIANDO UMA LISTA COM OS VALORES DE "ELECTION" NO CSES:
countries <- table(tab_cses$election)
countries <- as.data.frame(countries)
countries$Freq <- NULL
countries <- unlist(countries,use.names = FALSE)

#Usando essa lista para filtrar o dataset do Manifesto, 
#deixando só as eleições que constam no CSES:
corpus_lean <- corpus[corpus$election %in% countries,]


##### CRIANDO VERSÃO DE TAB_CSES SEM VARIÁVEIS QUE FIZ SÓ PARA
#DAR O MATCH ACIMA, CRIAR "ELECTION" NO MANIFESTO (E COM UNGROUP PARA VARIÁVEL "COUNTRY" NÃO FICAR "VOLTANDO"):

tab_cses2 <- tab_cses %>% ungroup() %>% select (-country, -cname)



##### JUNÇÃO #####


## AGORA SIM, PROCEDENDO PARA O CÓDIGO FORNECIDO NO STACK PARA JUNTAR MANIFESTO E CSES:

mani_cses <- corpus_lean %>% full_join(tab_cses2 %>% pivot_longer(cols=-election, values_to="ID"), by=c("election", "ID")) %>%
  arrange(election, name) %>% nest(data=-election) %>% mutate(data=map(data, function(data){
    data %>% filter(is.na(name)) %>% mutate(name=str_c("other_party_", row_number())) %>%
      bind_rows(data %>% filter(!is.na(name)), .) %>% return()
  })) %>% unnest(cols=data) %>% pivot_longer(cols=c(ID, ideology), names_to="type") %>% arrange(type) %>%
  mutate(name=if_else(type=="ID", name, str_c(type, "_", str_replace(name, "_ID", ""))) %>% as_factor(),
         value=if_else(value==999999, NA_real_, value)) %>%
  select(-type) %>% pivot_wider() %>%
  select(election, starts_with("party"), starts_with("other"),
         starts_with("ideology_party"), starts_with("ideology_other"))



# Não consigo transformar em csv para verificar porque tem class "unknown" (aparentemente tudo está como "list")


#Mudei primeiro para character porque não pode mudar list direto para numeric:
#mani_cses[,2:45] <- lapply(mani_cses[,2:45],unlist)

mani_cses[,2:45] <- lapply(mani_cses[,2:45],as.character)
mani_cses[,2:45] <- lapply(mani_cses[,2:45],as.numeric)


##### ADIÇÕES MANUAIS #####
#Turquia e Grécia ficaram OK tirando as duplicações. 

#Belgium 1999

#FLANDERS:
mani_cses$party_ID_A[mani_cses$election == "BELF1999"] <- 21421
mani_cses$party_ID_B[mani_cses$election == "BELF1999"] <- 21521
mani_cses$party_ID_C[mani_cses$election == "BELF1999"] <- 21321
mani_cses$party_ID_D[mani_cses$election == "BELF1999"] <- 21914
mani_cses$party_ID_E[mani_cses$election == "BELF1999"] <- 21112
mani_cses$party_ID_F[mani_cses$election == "BELF1999"] <- 21915

mani_cses$ideology_party_A[mani_cses$election == "BELF1999"] <- 5.556
mani_cses$ideology_party_B[mani_cses$election == "BELF1999"] <- -4.185
mani_cses$ideology_party_C[mani_cses$election == "BELF1999"] <- -1.429
mani_cses$ideology_party_D[mani_cses$election == "BELF1999"] <- -2.997
mani_cses$ideology_party_E[mani_cses$election == "BELF1999"] <- -16.68
mani_cses$ideology_party_F[mani_cses$election == "BELF1999"] <- -6.612

#WALONIA:
mani_cses$party_ID_A[mani_cses$election == "BELW1999"] <- 21322
mani_cses$party_ID_B[mani_cses$election == "BELW1999"] <- 21425
mani_cses$party_ID_C[mani_cses$election == "BELW1999"] <- 21111
mani_cses$party_ID_D[mani_cses$election == "BELW1999"] <- 21522

mani_cses$ideology_party_A[mani_cses$election == "BELW1999"] <- -14.869
mani_cses$ideology_party_B[mani_cses$election == "BELW1999"] <- -7.805
mani_cses$ideology_party_C[mani_cses$election == "BELW1999"] <- -19.763
mani_cses$ideology_party_D[mani_cses$election == "BELW1999"] <- -11.928




##### LATIN AMERICA #####

##### LATAM #####
manif_latam <- mp_southamerica_dataset()

# Mudando data para ter só o ano (tirando meses):

manif_latam$date <- substr(manif_latam$date, 1, 4)

# EDIÇÕES PARA MATCH - ELECTIONS #

#DEPOIS, UM MATCH DESSA VARIÁVEL COM MANIFESTO, DE ACORDO COM NOME EXTENSO, PARA TER ELA AO INVÉS DE NOME EXTENSO APENAS 
manif_latam$cname <- tab_cses$cname[match(manif_latam$countryname, tab_cses$country)]

#AÍ SÓ JUNTAR COM ANO PARA TER O EQUIVALENTE AO QUE TEMOS EM CSES COMO "ELECTION":
manif_latam$election <- str_c(manif_latam$cname, "_", manif_latam$date)

# CRIANDO BANCO SÓ COM AS "ELECTIONS" EM CSES:
latam <- manif_latam[manif_latam$election %in% countries,] %>% rename(ID = party, ideology = rile) %>% select (election, ID, partyname, ideology, edate, progtype)

##### INSERÇÕES MANUAIS

# ARG_2015:
mani_cses$party_ID_C[mani_cses$election == "ARG_2015"] <- 150021
mani_cses$party_ID_A[mani_cses$election == "ARG_2015"] <- 150025
mani_cses$party_ID_B[mani_cses$election == "ARG_2015"] <- 150062

mani_cses$ideology_party_C[mani_cses$election == "ARG_2015"] <- 8.333
mani_cses$ideology_party_A[mani_cses$election == "ARG_2015"] <- -31.233
mani_cses$ideology_party_B[mani_cses$election == "ARG_2015"] <- -21.354

#Chile 1999:
mani_cses$party_ID_A[mani_cses$election == "CHL_1999"] <- 155021
mani_cses$party_ID_B[mani_cses$election == "CHL_1999"] <- 155061

mani_cses$ideology_party_A[mani_cses$election == "CHL_1999"] <- -13.596
mani_cses$ideology_party_B[mani_cses$election == "CHL_1999"] <- -5.017

#Chile 2005:

mani_cses$party_ID_E[mani_cses$election == "CHL_2005"] <- 155021
mani_cses$other_party_1[mani_cses$election == "CHL_2005"] <- 155025
mani_cses$party_ID_A[mani_cses$election == "CHL_2005"] <- 155601
mani_cses$party_ID_D[mani_cses$election == "CHL_2005"] <- 155602

mani_cses$ideology_party_E[mani_cses$election == "CHL_2005"] <- -19.715
mani_cses$ideology_other_party_1[mani_cses$election == "CHL_2005"] <- -36.323
mani_cses$ideology_party_A[mani_cses$election == "CHL_2005"] <- 16.892
mani_cses$ideology_party_D[mani_cses$election == "CHL_2005"] <- -0.809

#Chile 2009:
mani_cses$party_ID_D[mani_cses$election == "CHL_2009"] <- 155021
mani_cses$party_ID_H[mani_cses$election == "CHL_2009"] <- 155023
mani_cses$party_ID_G[mani_cses$election == "CHL_2009"] <- 155025
mani_cses$party_ID_B[mani_cses$election == "CHL_2009"] <- 155061

mani_cses$ideology_party_D[mani_cses$election == "CHL_2009"] <- -18.857
mani_cses$ideology_party_H[mani_cses$election == "CHL_2009"] <- -28.325
mani_cses$ideology_party_G[mani_cses$election == "CHL_2009"] <- -41.486
mani_cses$ideology_party_B[mani_cses$election == "CHL_2009"] <- -3.029

#Brasil:

#2002
mani_cses$party_ID_A[mani_cses$election == "BRA_2002"] <- 180230
mani_cses$party_ID_F[mani_cses$election == "BRA_2002"] <- 180240
mani_cses$party_ID_B[mani_cses$election == "BRA_2002"] <- 180310
mani_cses$other_party_1[mani_cses$election == "BRA_2002"] <- 180320

mani_cses$ideology_party_A[mani_cses$election == "BRA_2002"] <- -21.955
mani_cses$ideology_party_F[mani_cses$election == "BRA_2002"] <- -12.53
mani_cses$ideology_party_B[mani_cses$election == "BRA_2002"] <- -10.523
mani_cses$ideology_other_party_1[mani_cses$election == "BRA_2002"] <- -20.711


#2006:
mani_cses$party_ID_B[mani_cses$election == "BRA_2006"] <- 180230
mani_cses$other_party_1[mani_cses$election == "BRA_2006"] <- 180231
mani_cses$party_ID_C[mani_cses$election == "BRA_2006"] <- 180310

mani_cses$ideology_party_B[mani_cses$election == "BRA_2006"] <- -19.005
mani_cses$ideology_other_party_1[mani_cses$election == "BRA_2006"] <- -51.128
mani_cses$ideology_party_C[mani_cses$election == "BRA_2006"] <- -9.202

#2010:
mani_cses$other_party_1[mani_cses$election == "BRA_2010"] <- 180110
mani_cses$party_ID_A[mani_cses$election == "BRA_2010"] <- 180230
mani_cses$party_ID_C[mani_cses$election == "BRA_2010"] <- 180310

mani_cses$ideology_other_party_1[mani_cses$election == "BRA_2010"] <- -28.302
mani_cses$ideology_party_A[mani_cses$election == "BRA_2010"] <- -13.636
mani_cses$ideology_party_C[mani_cses$election == "BRA_2010"] <- -11.302

#2014:
mani_cses$party_ID_A[mani_cses$election == "BRA_2014"] <- 180230
mani_cses$party_ID_E[mani_cses$election == "BRA_2014"] <- 180240
mani_cses$party_ID_B[mani_cses$election == "BRA_2014"] <- 180310

mani_cses$ideology_party_A[mani_cses$election == "BRA_2014"] <- -12.048
mani_cses$ideology_party_E[mani_cses$election == "BRA_2014"] <- -5.394
mani_cses$ideology_party_B[mani_cses$election == "BRA_2014"] <- -5.085


##### INSERINDO EM CSES #####

cses$manifesto_ID_A <- mani_cses$party_ID_A[match(cses$election, mani_cses$election)]
cses$manifesto_ID_B <- mani_cses$party_ID_B[match(cses$election, mani_cses$election)]
cses$manifesto_ID_C <- mani_cses$party_ID_C[match(cses$election, mani_cses$election)]
cses$manifesto_ID_D <- mani_cses$party_ID_D[match(cses$election, mani_cses$election)]
cses$manifesto_ID_E <- mani_cses$party_ID_E[match(cses$election, mani_cses$election)]
cses$manifesto_ID_F <- mani_cses$party_ID_F[match(cses$election, mani_cses$election)]
cses$manifesto_ID_G <- mani_cses$party_ID_G[match(cses$election, mani_cses$election)]
cses$manifesto_ID_H <- mani_cses$party_ID_H[match(cses$election, mani_cses$election)]
cses$manifesto_ID_I <- mani_cses$party_ID_I[match(cses$election, mani_cses$election)]


cses$rile_A <- mani_cses$ideology_party_A[match(cses$election, mani_cses$election)]
cses$rile_B <- mani_cses$ideology_party_B[match(cses$election, mani_cses$election)]
cses$rile_C <- mani_cses$ideology_party_C[match(cses$election, mani_cses$election)]
cses$rile_D <- mani_cses$ideology_party_D[match(cses$election, mani_cses$election)]
cses$rile_E <- mani_cses$ideology_party_E[match(cses$election, mani_cses$election)]
cses$rile_F <- mani_cses$ideology_party_F[match(cses$election, mani_cses$election)]
cses$rile_G <- mani_cses$ideology_party_G[match(cses$election, mani_cses$election)]
cses$rile_H <- mani_cses$ideology_party_H[match(cses$election, mani_cses$election)]
cses$rile_I <- mani_cses$ideology_party_I[match(cses$election, mani_cses$election)]

#Agora falta os "other party" - mesma coisa:
cses$mani_ID_1 <- mani_cses$other_party_1[match(cses$election, mani_cses$election)]
cses$mani_ID_2 <- mani_cses$other_party_2[match(cses$election, mani_cses$election)]
cses$mani_ID_3 <- mani_cses$other_party_3[match(cses$election, mani_cses$election)]
cses$mani_ID_4 <- mani_cses$other_party_4[match(cses$election, mani_cses$election)]
cses$mani_ID_5 <- mani_cses$other_party_5[match(cses$election, mani_cses$election)]
cses$mani_ID_6 <- mani_cses$other_party_6[match(cses$election, mani_cses$election)]
cses$mani_ID_7 <- mani_cses$other_party_7[match(cses$election, mani_cses$election)]
cses$mani_ID_8 <- mani_cses$other_party_8[match(cses$election, mani_cses$election)]

cses$mani_ID_9  <- mani_cses$other_party_9[match(cses$election, mani_cses$election)]
cses$mani_ID_10 <- mani_cses$other_party_10[match(cses$election, mani_cses$election)]
cses$mani_ID_11 <- mani_cses$other_party_11[match(cses$election, mani_cses$election)]
cses$mani_ID_12 <- mani_cses$other_party_12[match(cses$election, mani_cses$election)]
cses$mani_ID_13 <- mani_cses$other_party_13[match(cses$election, mani_cses$election)]



cses$rile_other_1 <- mani_cses$ideology_other_party_1[match(cses$election, mani_cses$election)]
cses$rile_other_2 <- mani_cses$ideology_other_party_2[match(cses$election, mani_cses$election)]
cses$rile_other_3 <- mani_cses$ideology_other_party_3[match(cses$election, mani_cses$election)]
cses$rile_other_4 <- mani_cses$ideology_other_party_4[match(cses$election, mani_cses$election)]
cses$rile_other_5 <- mani_cses$ideology_other_party_5[match(cses$election, mani_cses$election)]
cses$rile_other_6 <- mani_cses$ideology_other_party_6[match(cses$election, mani_cses$election)]
cses$rile_other_7 <- mani_cses$ideology_other_party_7[match(cses$election, mani_cses$election)]
cses$rile_other_8 <- mani_cses$ideology_other_party_8[match(cses$election, mani_cses$election)]

cses$rile_other_9  <- mani_cses$ideology_other_party_9[match(cses$election, mani_cses$election)]
cses$rile_other_10 <- mani_cses$ideology_other_party_10[match(cses$election, mani_cses$election)]
cses$rile_other_11 <- mani_cses$ideology_other_party_11[match(cses$election, mani_cses$election)]
cses$rile_other_12 <- mani_cses$ideology_other_party_12[match(cses$election, mani_cses$election)]
cses$rile_other_13 <- mani_cses$ideology_other_party_13[match(cses$election, mani_cses$election)]

