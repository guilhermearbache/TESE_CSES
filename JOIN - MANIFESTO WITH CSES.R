

#Arrumar tab_cses para ter só essas variáveis de interesse - ELECTION, ID MANIFESTO SÓ? 

tab_cses <- cses %>% group_by(country, election) %>%
  select (country, election, starts_with("manif")) %>%
summarize_all (.funs = c(mean="mean"), na.rm = T)


##### MANIFESTO #####
# Arrumar colunas de MANIFESTO PARA SER SÓ ID E ideology, e ELECTION! # Tem que fazer election!

library(manifestoR)
mp_setapikey("manifesto_apikey.txt")

latam <- mp_southamerica_dataset()
corpus <- mp_maindataset()

# Mudando data para ter só o ano (tirando meses):

corpus$date <- substr(corpus$date, 1, 4)

corpus <- corpus %>% filter (date > 1995)


##### MATCH - ELECTIONS #####

#CRIAR VARIÁVEL PARA PAÍS COM TRÊS LETRAS EM CSES:

tab_cses$cname <- substr(tab_cses$election, 1, 3)

#DEPOIS, UM MATCH DESSA VARIÁVEL COM MANIFESTO, DE ACORDO COM NOME EXTENSO, PARA TER ELA AO INVÉS DE NOME EXTENSO APENAS 
corpus$cname <- tab_cses$cname[match(corpus$countryname, tab_cses$country)]

#AÍ SÓ JUNTAR COM ANO PARA TER O EQUIVALENTE AO QUE TEMOS EM CSES COMO "ELECTION":
corpus$election <- str_c(corpus$cname, "_", corpus$date)

##### LIMPEZA E EDIÇÃO FINAL #####

corpus <- corpus %>% rename(ID = party, ideology = rile) %>% select (election, ID, ideology)

tab_cses$country <- NULL
tab_cses$cname <- NULL


names (tab_cses) <- gsub("manif", "party", names(tab_cses))

names (tab_cses) <- gsub ("_mean", "", names(tab_cses))

##### JUNÇÃO #####


#### tentando com left_join (manter tudo de MANIFESTO):
output <- corpus %>% left_join(tab_cses %>% pivot_longer(cols=-election, values_to="ID"), by=c("election", "ID")) %>%
  arrange(election, name) %>% nest(data=-election) %>% mutate(data=map(data, function(data){
    data %>% filter(is.na(name)) %>% mutate(name=str_c("other_party_", row_number())) %>%
      bind_rows(data %>% filter(!is.na(name)), .) %>% return()
  })) %>% unnest(cols=data) %>% pivot_longer(cols=c(ID, ideology), names_to="type") %>% arrange(type) %>%
  mutate(name=if_else(type=="ID", name, str_c(type, "_", str_replace(name, ".ID", ""))) %>% as_factor()) %>%
  select(-type) %>% pivot_wider()


#RIGHT JOIN (manter CSES):

output2 <- corpus %>% right_join(tab_cses %>% pivot_longer(cols=-election, values_to="ID"), by=c("election", "ID")) %>%
  arrange(election, name) %>% nest(data=-election) %>% mutate(data=map(data, function(data){
    data %>% filter(is.na(name)) %>% mutate(name=str_c("other_party_", row_number())) %>%
      bind_rows(data %>% filter(!is.na(name)), .) %>% return()
  })) %>% unnest(cols=data) %>% pivot_longer(cols=c(ID, ideology), names_to="type") %>% arrange(type) %>%
  mutate(name=if_else(type=="ID", name, str_c(type, "_", str_replace(name, ".ID", ""))) %>% as_factor()) %>%
  select(-type) %>% pivot_wider()

#Warning message:
#Values are not uniquely identified; output will contain list-cols.
#* Use `values_fn = list` to suppress this warning.
#* Use `values_fn = length` to identify where the duplicates arise
#* Use `values_fn = {summary_fun}` to summarise duplicates 



# OUTRO CÓDIGO:
output2 <- corpus %>%
  group_by(election) %>%
  mutate(ID = row_number()) %>%
  pivot_wider(names_from = ID, values_from = ideology, names_prefix = 'ideology') %>%
  inner_join(tab_cses, by = 'election') -> result




# Não consigo transformar em csv para verificar porque tem class "unknown" (aparentemente tudo está como "list")


#Mudei primeiro para character porque não pode mudar list direto para numeric:
output2[,2:19] <- lapply(output2[,2:19],as.character)
output2[,2:19] <- lapply(output2[,2:19],as.numeric)


write.csv(corpus, file = "corpus.csv")


#### TESTANDO COM AMÉRICA LATINA - menos países e Brasil com várias 


latam <- %>% right_join(tab_cses %>% pivot_longer(cols=-election, values_to="ID"), by=c("election", "ID")) %>%
  arrange(election, name) %>% nest(data=-election) %>% mutate(data=map(data, function(data){
    data %>% filter(is.na(name)) %>% mutate(name=str_c("other_party_", row_number())) %>%
      bind_rows(data %>% filter(!is.na(name)), .) %>% return()
  })) %>% unnest(cols=data) %>% pivot_longer(cols=c(ID, ideology), names_to="type") %>% arrange(type) %>%
  mutate(name=if_else(type=="ID", name, str_c(type, "_", str_replace(name, ".ID", ""))) %>% as_factor()) %>%
  select(-type) %>% pivot_wider()


