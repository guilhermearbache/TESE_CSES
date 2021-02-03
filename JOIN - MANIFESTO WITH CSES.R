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


##### EXPLICAÇÃO ##### 
#se eu apenas criar uma variável sem a quarta letra, de qualquer forma esses 4 casos vão virar 2 casos duplicados. 
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


##### ADICIONANDO ALGUNS PAÍSES QUE NÃO FUNCIONOU MATCH:
corpus_original$cname[corpus_original$countryname == "United Kingdom"] <- "GBR"
corpus_original$cname[corpus_original$countryname == "United States"] <- "USA"
corpus_original$cname[corpus_original$countryname == "Russia"] <- "RUS"
corpus_original$cname[corpus_original$countryname == "South Korea"] <- "KOR"
 
#AÍ SÓ JUNTAR COM ANO PARA TER O EQUIVALENTE AO QUE TEMOS EM CSES COMO "ELECTION":
corpus_original$election <- str_c(corpus_original$cname, "_", corpus_original$date)

# LIMPEZA E EDIÇÃO FINAL #

corpus <- corpus_original %>% rename(ID = party, ideology = rile) %>% 
  select (election, ID, ideology, partyname, progtype)

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
#DAR O MATCH ACIMA, CRIAR "ELECTION" NO MANIFESTO:

tab_cses2 <- tab_cses %>% ungroup() %>% select (-country, -cname)

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
latam <- manif_latam[manif_latam$election %in% countries,]

# LIMPEZA E EDIÇÃO FINAL #

latam <- latam %>% rename(ID = party, ideology = rile) %>% select (election, ID, ideology)

##### EDIÇÕES FINAIS TAB_CSES - NÃO ESTOU MAIS USANDO ISSO, TEM QUE DAR UNGROUP SENÃO ESSA
#VARIÁVEL "COUNTRY" FICA RETORNANDO: 

#tab_cses$country <- NULL
#tab_cses$cname <- NULL

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


#Warning message:
#Values are not uniquely identified; output will contain list-cols.
#* Use `values_fn = list` to suppress this warning.
#* Use `values_fn = length` to identify where the duplicates arise
#* Use `values_fn = {summary_fun}` to summarise duplicates 




# Não consigo transformar em csv para verificar porque tem class "unknown" (aparentemente tudo está como "list")


#Mudei primeiro para character porque não pode mudar list direto para numeric:
#mani_cses[,2:45] <- lapply(mani_cses[,2:45],unlist)

mani_cses[,2:45] <- lapply(mani_cses[,2:45],as.character)
mani_cses[,2:45] <- lapply(mani_cses[,2:45],as.numeric)


##### CORREÇÕES/ADIÇÕES MANUAIS #####
#Grécia 2012
#Party A  34511	22.727 New Democracy

#Party C 34313	26.531 Panhellenic Socialist Movement

#Party D 34730	-12.258	Independent Greeks
#Party E 34720 -  38.172 Golden Dawn

#Party F - 34213	-6.709	Democratic Left
#Party G - 34210	11.043	Communist Party of Greece

# Other - 34020 - radical - tirar?  Popular Orthodox 	34710 - deixar?

#Grécia 2015 - mesmo problema (duas eleições no ano, diferentes)

#Party A - 34212	-41.722	Coalition of the Radical Left
#Party B  - New Democracy 34511   -8.025
#Party C - Golden Dawn 34720 2.326 
#Party D - The River  - no data!
#Party E - Communist Party of Greece  34210 29.032
#Party F - Independent Greeks 34730 46.154
#Party G - Panhellenic Socialist Movement 34313 -37.842



Corpus_Greece <- corpus_original %>% select (countryname, edate, date, 
                                               partyname, party, rile, progtype) %>%
                    filter (countryname == "Greece" & date > 2000)

