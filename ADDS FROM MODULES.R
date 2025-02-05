#Esse arquivo � para editar o banco inicial e produzir "cses_m",que tem vari�veis que s� encontramos nos m�dulos 
#originais: percentual de votos por distrito, ideology placement de LEADERS 

# MUDANDO O DIRET�RIO PARA UMA SUBPASTA DO ATUAL:

#setwd(paste0(getwd(), "/INTRO-EDIT/"))

#BAIXANDO OS BANCOS:

load("cses_imd.rdata")

load("cses4.rdata")
load("cses3.rdata")
load("cses2.rdata")
load("cses1.rdata")


##### AJUSTE M�DULO 4 - UNIFICANDO D4004 E D4004_N (nationwide district) #####
cses4 <- cses4 %>% mutate (
  pv_dist_A = case_when(
    D4004_A_N > 996 ~ D4004_A,
    TRUE  ~  D4004_A_N
  )
)

cses4 <- cses4 %>% mutate (
  pv_dist_B = case_when(
    D4004_B_N > 996 ~ D4004_B,
    TRUE  ~  D4004_B_N
  )
)

cses4 <- cses4 %>% mutate (
  pv_dist_C = case_when(
    D4004_C_N > 996 ~ D4004_C,
    TRUE  ~  D4004_C_N
  )
)

cses4 <- cses4 %>% mutate (
  pv_dist_D = case_when(
    D4004_D_N > 996 ~ D4004_D,
    TRUE  ~  D4004_D_N
  )
)


cses4 <- cses4 %>% mutate (
  pv_dist_E = case_when(
    D4004_E_N > 996 ~ D4004_E,
    TRUE  ~  D4004_E_N
  )
)

cses4 <- cses4 %>% mutate (
  pv_dist_F = case_when(
    D4004_F_N > 996 ~ D4004_F,
    TRUE  ~  D4004_F_N
  )
)

cses4 <- cses4 %>% mutate (
  pv_dist_G = case_when(
    D4004_G_N > 996 ~ D4004_G,
    TRUE  ~  D4004_G_N
  )
)

cses4 <- cses4 %>% mutate (
  pv_dist_H = case_when(
    D4004_H_N > 996 ~ D4004_H,
    TRUE  ~  D4004_H_N
  )
)

cses4 <- cses4 %>% mutate (
  pv_dist_I = case_when(
    D4004_I_N > 996 ~ D4004_I,
    TRUE  ~  D4004_I_N
  )
)

##### SELECT/RENAME ####
#Selecionando vari�veis de interesse em cada banco e 
#colocando nomes iguais para poder juntar depois

cses1 <- cses1 %>% select (A1001, A1004, A1005, starts_with("A4004"), A2027, 
                           A3033:A3035_F) %>% 
  rename (IMD1005 = A1005, alt_ideol_self = A3033, district = A2027)

cses2 <- cses2 %>% select (B1001, B1004, B1005, starts_with("B4004"), B2031,
                           B3039_A:B3041_I, B3046, B3022:B3026, B5015:B5016,
                           B5020_A:B5020_I) %>% 
  rename (IMD1005 = B1005, alt_ideol_self = B3046,repr_party = B3023,
          repr_party2 = B3024, repr_leader = B3025, repr_leader2 = B3026, district = B2031) 

cses3 <- cses3 %>% select (C1001, C1004, C1005, starts_with("C4004"), C2031, 
                           C3012_A:C3012_I,C3014_A:C3017, C5018_A:C5018_I, 
                           C3007_1:C3008_2) %>% 
  rename (IMD1005 = C1005, alt_ideol_self = C3016, repr_party = C3007_1,
          repr_party2 = C3007_2, repr_leader = C3008_1, repr_leader2 = C3008_2, district = C2031)

cses4 <- cses4 %>% select (D1001, D1004, D1005, starts_with("pv_dist"), D2032,
                           D3015_A:D3015_I, D3016, D5018_A:D5018_I) %>%
  rename (IMD1005 = D1005, alt_ideol_self = D3016, district = D2032)

	 

names (cses2) <- gsub("B3039", "ideol_leader", names(cses2))
names (cses3) <- gsub("C3012", "ideol_leader", names(cses3))

names (cses1) <- gsub("A3034", "alt_ideolparty", names(cses1))
names (cses2) <- gsub("B3040", "alt_ideolparty", names(cses2))
names (cses3) <- gsub("C3014", "alt_ideolparty", names(cses3))
names (cses4) <- gsub("D3015", "alt_ideolparty", names(cses4))

names (cses1) <- gsub("A3035", "alt_ideol_leader", names(cses1))
names (cses2) <- gsub("B3041", "alt_ideol_leader", names(cses2))
names (cses3) <- gsub("C3015", "alt_ideol_leader", names(cses3))

names (cses2) <- gsub("B5020", "exp_alt_ideolparty", names(cses2))
names (cses3) <- gsub("C5018", "exp_alt_ideolparty", names(cses3))
names (cses4) <- gsub("D5018", "exp_alt_ideolparty", names(cses4))


names (cses1) <- gsub("A4004", "pv_dist", names(cses1))
names (cses3) <- gsub("C4004", "pv_dist", names(cses3))
names (cses2) <- gsub("B4004", "pv_dist", names(cses2))

#N�o renomeadas:
#SIGNIFICANT PARTIES NOT REPRESENTED IN PARLIAMENT BEFORE/AFTER THE ELECTION - B5015/B5016
# DIFFERENCES OF CHOICE OPTIONS (among candidates) - C3017

cses_modules <- bind_rows (cses1, cses2, cses3, cses4)

cses_modules <- cses_modules %>% mutate(module = case_when(D1001 == "CSES-MODULE-4" ~ 4, 
                                               C1001 == "CSES-MODULE-3" ~ 3,
                                               B1001 == "CSES-MODULE-2" ~ 2,
                                               TRUE ~ 1)) %>%
          filter (A1004 != "PRT_2002" | module != 1) %>%
          select (-contains("1001"), -contains ("1004"))                                                          


#J� criei a vari�vel para cada m�dulo (juntando as colunas respectivas por m�dulo)
#Depois filtrei Portugal 2002 para um dos m�dulos, porque estava duplicada (Modules 1 e 2)
#A� j� dispensei as vari�veis originais de m�dulo, e tamb�m as de elei��o (pa�s-ano), que j�
#existem no banco IMD que vou juntar. 


##### JOIN #####

cses_add <- left_join(cses_imd, cses_modules, by = "IMD1005")


### VERS�O ANTIGA DESSE SCRIPT TINHA UM C�DIGO PARA CADA M�DULO PARA ESSA CRIA��O DE TOTALSUM(VARI�VEL QUE SOMA TODAS COLUNAS
#DE PV - DISTRICT PARA VER SE EST� MUITO MENOR (OU MAIOR, POR DUPLICA��ES, ERROS) QUE 100%,
#ANTECEDIDO POR ESSE RECODE DE MISSINGS NAS VARI�VEIS PV_DIST. 

# DEPOIS, NA VERS�O "... - new" eu tirei tudo isso e deixei a do "TOTALVOT" geral, no banco grande. 
#At� que me dei conta de que essa a� n�o faz o m�nimo sentido - uma m�dia de total de votos por distrito?
#N�o me diz nada, e os dados est�o tudo muito acima de 100, mesmo tirando NA. 


cses_add <- cses_add %>%
  mutate_at(.vars = vars(starts_with("pv_dist")), 
            .funs = list(~ifelse(. > 996, NA, .)))

save(cses_add,
     file = "cses_add.RData")



