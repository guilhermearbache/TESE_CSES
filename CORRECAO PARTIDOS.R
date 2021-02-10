
##### CORREÇÃO PCV - IDEOL #####


 # TESTAR DE NOVO, SE ESTIVER CERTO JOGAR EM PRESIDENTIAL, DEPOIS DE CRIAR AS VARIÁVEIS DE IDEOLOGIA. 

#Depois fazer o código abaixo (original do Stack) adaptado para meus dados, para adaptar ideol_party etc ao que está em pcv:

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


#Variável base - % vote
#variáveis a criar novas versões: ideol_party, ex_ideol_party, que mais? SÓ 



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





##### SEGUNDO TURNO #####


#INSERIR DADOS PARA SEGUNDO TURNO

#CRIAR cses_pr SÓ PAÍSES COM SEGUNDO TURNO

#CRIAR AS VARIÁVEIS - PCV_2???

#SRB_2012
cses_pr <- cses_pr %>% mutate (
  pcv_2_A = case_when(
    election == "SRB_2012" ~  49.54 ,
    TRUE          ~ pcv_2_A 
  )
)

cses_pr <- cses_pr %>% mutate (
  pcv_2_B = case_when(
    election == "SRB_2012" ~  47.31 ,
    TRUE          ~ pcv_2_B
  )
)



#INSERIR DADOS - NA MÃO: PCV NO SEGUNDO - COM PCV PRIMEIRO PODERIA CRIAR ALGUM CÓDIGO:
#SCAN COLUMNS PCV_PR, TAKE 2 HIGHER NUMBERS, MATCH COLUMNS IN IDEOLPARTY, EX_IDEOLPARTY corresponding. Aí teremos
#as duas ideologias respectivas, e depois ainda teremos o ganhador. - Pergunta Stack? Já sei fazer com closest, 
# mas e para dois casos? 

#AÍ DAR MATCH PARA INSERIR NO BANCO GERAL OS DADOS  (DE "TAB CSES PARA CSES") 

#OU SÓ JÁ INSERIR NA MÃO MESMO? CONFORME ACIMA? 







##### QUESTION - para achar ideolparty dos 2 no segundo turno
df <- data.frame(a=1:50, b=rnorm(50), c=rpois(50, 10))

set.seed(1234)
df <- data.frame(id=paste0(letters[-1], 1:40), matrix(rnorm(25000L, 5L, 10L), 1000L))

#Temos system_PR para tipo de Presidencial mas isso não ajuda muito, precisamos criar dummy
#para segundo turno e preencher na mão ou só filtrar os países com segundo turno - JÁ CRIEI ISSO NA MÃO NO EXCEL, VER DE PASSAR
#COM MATCH 

cses_pr2 <- cses_pr %>% filter (system_PR)

PR_countries <- cses_pr %>%
group_by(election, country) %>%
summarize_all (.funs = c(mean="mean"), na.rm = T)
 

# SELECIONAR AS VARIÁVEIS DE IDEOLOGIA, PCV, E CRIAR COM DPUT- 9 rows mesm?

# fazer a pergunta


library(haven)
bcs92 = read_spss("C:/Users/livia/OneDrive - usp.br/TESE/RAW DATASETS/BCS_1992/BCS_1992_Data.por")
