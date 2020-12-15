
#Primeiro : fazer na mão a correção de problemas de coalizão (corrigir pcv), depois ver os casos que faltam
#muito - TENTAR ACHAR EM ALGUM OUTRO BANCO ESSES PARTIDOS PARA EXPERTS, FAZER VERSÕES DO BANCO SEM ELES, 
#ETC. 

cses_pr_adj <- cses_pr

### COMEÇAR COM ARGENTINA - usar eleições gerais, passar das primárias

# Ver como fazer isso no código -  mudar pcv de um partido fazer como abaixo

#Mas e para mudar o "ideol_elected"? Talvez melhor fosse inserir código do partido já em quem 
#foi eleito (portanto, no banco anterior, já antes do código de "ideol_elected")
#Ou ver como faz para "if election ARG_2015 , ideol_elected == ideol_partyG

cses_pr_adj$pcv_PR_G[election == "ARG_2015"] <- "Elder"

0320007 REPUBLICAN PROPOSAL party_G


#INSERIR DADOS PARA SEGUNDO TURNO

#CRIAR cses_pr SÓ PAÍSES COM SEGUNDO TURNO

# FAZER UM SUMMARIZE PARA TRABALHAR COM TABELA MENOR


#INSERIR DADOS - NA MÃO: PCV NO SEGUNDO - COM PCV PRIMEIRO PODERIA CRIAR ALGUM CÓDIGO:
#SCAN COLUMNS PCV_PR, TAKE 2 HIGHER NUMBERS, MATCH COLUMNS IN IDEOLPARTY, EX_IDEOLPARTY corresponding. Aí teremos
#as duas ideologias respectivas, e depois ainda teremos o ganhador. - Pergunta Stack? Já sei fazer com closest, 
# mas e para dois casos? 



#DAR MATCH PARA INSERIR NO BANCO GERAL OS DADOS




#Depois fazer o código abaixo (original do Stack) adaptado para meus dados:

tmp <- mydata %>% 
  mutate(obs = 1:n()) %>% 
  pivot_longer(-obs, 
               names_pattern="(.*)_(.*)", 
               names_to=c("vars", "letter"), 
               values_to="vals") %>% 
  group_by(letter, obs) %>% 
  mutate(val1 = vals[which(vars == "var1")], 
         new = case_when(val1 == 0 | is.na(val1) ~ NA_real_, TRUE ~ vals)) %>% 
  select(-val1) %>% 
  pivot_wider(names_from = c("vars", "letter"), 
              values_from=c("vals", "new"))
names(tmp) <- gsub("vals_", "", names(tmp))



cses_pr_adj <- cses_pr


#Variável base - % vote
#variáveis a criar novas versões: ideol_party, ex_ideol_party, que mais?



### VAMOS TENTAR COM O BANCO REAL:

cses_pr_adj <- cses_pr %>% 
  select(starts_with("pcv_PR"), contains("ideolparty"))

names(cses_pr_adj) <- gsub("pcv_PR", "var1", names(cses_pr_adj))


cses_pr_adj <- cses_pr_adj %>% 
  mutate(obs = 1:n()) %>% 
  pivot_longer(-obs, 
               names_pattern="(.*)_(.*)", 
               names_to=c("vars", "letter"), 
               values_to="vals") %>% 
  group_by(letter, obs) %>% 
  mutate(val1 = vals[which(vars == "var1")], 
         new = case_when(val1 == 0 | is.na(val1) ~ NA_real_, TRUE ~ vals)) %>% 
  select(-val1) %>% 
  pivot_wider(names_from = c("vars", "letter"), 
              values_from=c("vals", "new"))

names(cses_pr_adj) <- gsub("vals_", "", names(cses_pr_adj))



## FUNCIONOU! Terminar de conferir com planilha, 


library(haven)
bcs92 = read_spss("C:/Users/livia/OneDrive - usp.br/TESE/RAW DATASETS/BCS_1992/BCS_1992_Data.por")




##### QUESTION - para achar ideolparty dos 2 no segundo turno
df <- data.frame(a=1:50, b=rnorm(50), c=rpois(50, 10))

set.seed(1234)
df <- data.frame(id=paste0(letters[-1], 1:40), matrix(rnorm(25000L, 5L, 10L), 1000L))

#Temos system_PR para tipo de Presidencial mas isso não ajuda muito, precisamos criar dummy
#para segundo turno e preencher na mão ou só filtrar os países com segundo turno

cses_pr2 <- cses_pr %>% filter (system_PR)

PR_countries <- cses_pr %>%
group_by(election, country) %>%
summarize_all (.funs = c(mean="mean"), na.rm = T)
 

# SELECIONAR AS VARIÁVEIS DE IDEOLOGIA, PCV, E CRIAR COM DPUT- 9 rows mesm?

# fazer a pergunta