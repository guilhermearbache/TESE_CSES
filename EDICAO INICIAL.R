# PROCEDER PARA TODO RESTO - RODAR O INTRO EDIT, DEPOIS ACERTAR O PRESIDENCIAL, E IR PARA LEGISLATIVO


#Tentativa de proposta para a vers�o ajustada: 

#PArty A ideol  case_when percent votes party A > 0 = partyA ideol orig, else = NA 
### Mas tem que ver que alguns casos o pv_dist parece bem zoado. 




# C�DIGO PARA EDITAR O BANCO ORIGINAL (SELECT/RENAME DE VARI�VEIS, INCLUS�ES DE C�DIGOS - PARTIDOS) 
library(dplyr)
#library(bestNormalize)

load("C:/Users/livia/Desktop/TESE_CSES/cses_add.rdata")

##### SELECT/RENAME #####

  cses <- cses_add %>% 
    rename (ID = IMD1005, election = IMD1004, country = IMD1006_NAM, type = IMD1009, age = IMD2001_1, 
            gender = IMD2002, education = IMD2003, religion = IMD2005, income = IMD2006,
            rural = IMD2007, ideol_self = IMD3006, elected_pr = IMD5009_2, turnout = IMD5006_2,
            efficacy = IMD3011, effic_vote = IMD3012, compulsory = IMD5007,
            votescast_LH1= IMD5016_1, votescast_LH2 = IMD5016_2, votescast_UH1 = IMD5016_3,
            votescast_UH2 = IMD5016_4, party_list_LH1 = IMD5017_1, 
            party_list_LH2 = IMD5017_2, party_list_UH1 = IMD5017_3, party_list_UH2 = IMD5017_4, 
            rounds_LH1 = IMD5018_1, rounds_LH2 = IMD5018_2, rounds_UH1= IMD5018_3, rounds_UH2 = IMD5018_4,
            threshold_LH1 = IMD5022_1, threshold_LH2 = IMD5022_2, threshold_UH1 = IMD5022_3, threshold_UH2 = IMD5022_4,
            direct_election = IMD5026_2, size_LH = IMD5027, 
            regime_age = IMD5049, ENEP = IMD5058_1, 
            CENEP = IMD5058_2, ENPP = IMD5059_1, CENPP = IMD5059_2) %>%
    select(ID, election, country, module, type, age, gender, education, religion, rural, income,
           starts_with("IMD3001"), starts_with("IMD3002"), starts_with ("IMD3004"), ideol_self, 
           elected_pr, starts_with("pv_dist"), IMD5001_A:IMD5005_I, IMD3005_1:IMD3005_4,
           starts_with("IMD3007"), IMD3013_1:IMD3015_D,turnout,
           compulsory, IMD5000_A:IMD5000_I, 
           IMD5011_A:IMD5014, regime_age, IMD5050_1:IMD5051_3, ENEP:CENPP, 
           IMD5052_1:IMD5052_3, efficacy, effic_vote, starts_with("votescast"), starts_with("party_list"), 
           starts_with("rounds"), starts_with("thresholds"), size_LH, direct_election, district:alt_ideol_leader_I,
           starts_with("exp_alt_ideolparty"), IMD5100_A:IMD5100_I) 
 
# S� tem em alguns m�dulos, n�o inclu�das por enquanto (s� vari�veis descritivas): C3017 B5015 B5016 B3022,
# repr_party, repr_leader

###### CODIFICA��O DE VARI�VEIS ##### 

  
# MISSING ATRIBUTION:
  cses <- cses %>%
    mutate_at(.vars = vars(IMD5001_A:IMD5005_I), 
              .funs = list(~ifelse(. > 996, NA, .)))

# RENOMEANDO VARI�VEIS "START_WITH":
  
 names (cses) <- gsub("IMD3001", "voted", names(cses))
 names (cses) <- gsub("IMD3002", "vote", names(cses))
 names (cses) <- gsub("IMD3004", "prevote", names(cses))
 names (cses) <- gsub("IMD3005", "party_ID", names(cses))
 names (cses) <- gsub("IMD3007", "ideolparty", names(cses))
 names (cses) <- gsub("IMD3013", "economy", names(cses))
 names (cses) <- gsub("IMD3014", "gov_eval", names(cses))
 
 names (cses) <- gsub("IMD5000", "numparty", names(cses))

 names (cses) <- gsub("IMD5001", "pcv_LH", names(cses))
 names (cses) <- gsub("IMD5002", "pcseats_LH", names(cses))
 names (cses) <- gsub("IMD5003", "pcv_UH", names(cses))
 names (cses) <- gsub("IMD5004", "pcseats_UH", names(cses))
 names (cses) <- gsub("IMD5005", "pcv_PR", names(cses))
 
 names (cses) <- gsub("IMD5011", "family_ideol", names(cses))
 names (cses) <- gsub("IMD5012", "ex_ideolparty", names(cses))
 names (cses) <- gsub("IMD5013", "system_LH", names(cses))
 names (cses) <- gsub("IMD5014", "system_PR", names(cses))

 names (cses) <- gsub("IMD5050", "freedom_house", names(cses))
 names (cses) <- gsub("IMD5051", "polity", names(cses))
 names (cses) <- gsub("IMD5052", "GDP", names(cses))

 names (cses) <- gsub("IMD5100", "manif_ID", names(cses))
 
 
  
 cses$year <- as.numeric(substr(cses$election, 5, 8))
 
# MAIS MISSINGS e alguns ajustes

 
##### GENDER #####
 cses <- cses %>%
   mutate_at(.vars = vars(gender), 
             .funs = list(~ifelse(. > 2, NA, .)))

 
 cses$female <- cses$gender -1 
 
##### AGE #####
 cses <- cses %>%
   mutate_at(.vars = vars(age), 
             .funs = list(~ifelse(. > 9000, NA, .)))

##### INCOME #####
 cses <- cses %>%
   mutate_at(.vars = vars(income), 
             .funs = list(~ifelse(. > 5, NA, .)))
 
##### RURAL/ EDUCATION #####
 cses <- cses %>%
   mutate_at(.vars = vars(rural, education), 
             .funs = list(~ifelse(. > 4, NA, .)))
 
##### RELIGION #####
 cses <- cses %>%
   mutate_at(.vars = vars(religion), 
             .funs = list(~ifelse(. > 90, NA, .)))
 

# EDUCATION - ATRIBUIR PARA MISSING TODOS VALORES 9,7, 8 e 6 = OTHER (alguns casos de BEL 2003 E DEU12002, pelo que vi eram 9=OTHER 
# no banco original, CSES 2)


##### KNOWLEDGE #####

#Pegar n�mero de corretas e apenas adaptar CSES 4. 

#Antes, codificar missing - VALORES 7 e 9:

cses <- cses %>%
  mutate_at(.vars = vars(IMD3015_1:IMD3015_D), 
            .funs = funs(ifelse(. == 7, NA, .)))

cses <- cses %>%
  mutate_at(.vars = vars(IMD3015_1:IMD3015_D), 
            .funs = funs(ifelse(. == 9, NA, .)))

cses <- cses %>% mutate(knowledge = case_when(module == 4 ~ IMD3015_D/4, 
                                              module == 3 ~ IMD3015_C/3,
                                              module == 2 ~ IMD3015_B/3,
                                              module == 1 ~ IMD3015_A/3))


#Minha vers�o - quando uma das 4 (IMD3015_A:D) for missing (7=refused, 9 = missing),
#apenas n�o conta na m�dia, n�o fica missing a vari�vel que soma tudo.


# Vou recodificar os valores 8 = "don't know" para 0 - vamos perder essa informa��o do banco original
#a partir de agora:

cses <- cses %>%
  mutate_at(.vars = vars(IMD3015_1:IMD3015_4), 
            .funs = funs(ifelse(. == 8, 0, .)))

cses <- cses %>% mutate(knowledge_adj = rowMeans(select(., IMD3015_1:IMD3015_4), na.rm = T))


##### PARTY ID #####
cses <- cses %>%
  mutate_at(.vars = vars(party_ID_1, party_ID_2, party_ID_4), 
            .funs = funs(ifelse(. > 6, NA, .)))


cses <- cses %>%
  mutate_at(.vars = vars(party_ID_1, party_ID_2), 
            .funs = funs(ifelse(. == 5, 0, .)))


##### ENEP & SHARE OF VOTES/SEATS#####

#No primeiro j� aproveitei e inclu� "turnout"

cses <- cses %>%
   mutate_at(.vars = vars(ENEP:CENPP, turnout, starts_with("pcv"), starts_with("pcseats")),  
            .funs = funs(ifelse(. == 999, NA, .)))

cses <- cses %>%
  mutate_at(.vars = vars(ENEP:CENPP,starts_with("pcv"), starts_with("pcseats")), 
            .funs = funs(ifelse(. == 997, NA, .)))

cses <- cses %>%
  mutate_at(.vars = vars(starts_with("pcv"), starts_with("pcseats")), 
            .funs = funs(ifelse(. == 996, NA, .)))



# TURNOUT individual (votou/n�o votou)

cses <- cses %>%
  mutate_at(.vars = vars(starts_with("voted")),  
            .funs = funs(ifelse(. >=9999993 , NA, .)))

##### COMPULSORY VOTE #####

#N�o tem missing! Mas vou fazer uma pequena altera��o e tamb�m criar dummy:

# Para ficar numa ordem l�gica e sem "buracos" (n�o tinha nada com valor 4, era 1-2-3-5), 
#fa�o altera��o para facultativo ser 0 e depois 1,2,3 quanto maiores as san��es. 

cses$compulsory[cses$compulsory == 5] <- 4 

cses$compulsory <- (cses$compulsory - 4) * -1  


#Dummy:

cses$compulsory_dummy <- 0
cses$compulsory_dummy[cses$compulsory >0 ] <- 1


##### GDP #####

### TIRAR MISSING
cses <- cses %>%
  mutate_at(.vars = vars(GDP_1:GDP_3), 
            .funs = funs(ifelse(. == 99, NA, .)))

### FAZER O ACUMULADO:

#T+(T-1):
cses <- cses %>% mutate (cum_gdp =  ((GDP_2/100 + ((GDP_1/100) * (1 + GDP_2/100)))*100))
cses <- cses %>% mutate(abs_growth = abs(cum_gdp))

#T+(T-1)+(T-2):

#cses <- cses %>% mutate (altgdp2 = (((GDP_1/100) * ((1+ (GDP_2/100)) * (1+(GDP_3/100))))+
                      #        ((GDP_2/100) * (1+GDP_3/100)) + GDP_3/100)*100)

cses <- cses %>% mutate (cum_gdp2 = (((GDP_1/100 + 1) *(GDP_2/100 + 1) * (GDP_3/100 + 1))-1)
*100)

cses <- cses %>% mutate(abs_growth2 = abs(cum_gdp2))

##### REGIME AGE #####

cses <- cses %>%
  mutate_at(.vars = vars(regime_age), 
            .funs = funs(ifelse(. == 999, NA, .)))

##### FREEDOM HOUSE #####
# CSES j� incluiu o �ndice geral, mas usarei indicadores desagregados 

#qog<- read.csv (url("http://www.qogdata.pol.gu.se/data/qog_std_ts_jan20.csv"))
qog<- read.csv("C:/Users/livia/Desktop/TESE_CSES/qog_std_ts_jan20.csv")

qog <- qog %>% filter (year > 1995) %>% 
  select (year, ccodealp, cname, starts_with("fh_"), starts_with("fhn_"))

qog$cyear <- paste(qog$ccodealp, qog$year) 
qog$cyear <- gsub(" ", "_", qog$cyear)



#Para acertar BELW, DEU1, etc.
cses$cyear <- cses$election
substr(cses$cyear, 4, 4) <- "_"

#As duas grandes dimens�es: 

cses$fh_civil <- qog$fh_cl[match(cses$cyear, qog$cyear)]
cses$fh_pol <- qog$fh_pr[match(cses$cyear, qog$cyear)]


# INVERTENDO AS ESCALAS, PARA MAIS DEMOCR�TICO TER VALOR MAIOR
#(al�m de mais f�cil de interpretar, fica mais condizente com os sub-indicadores do pr�prio Freedom House, 
#em que valores maiores significa mais democr�tico)

invert <- function(x, na.rm = FALSE) (x * -1)

cses <- cses %>%
  mutate_at(vars(fh_civil, fh_pol, starts_with("freedom_house")),
            invert) 



# Mais espec�ficas (dentro de political)
cses$fh_elec <- qog$fh_ep[match(cses$cyear, qog$cyear)]    #Electoral process
cses$fh_plural <- qog$fh_ppp[match(cses$cyear, qog$cyear)] #Political Pluralism and Participation


cses$fh_aor <- qog$fh_aor[match(cses$cyear, qog$cyear)]
cses$fh_rol <- qog$fh_rol[match(cses$cyear, qog$cyear)] # Rule of Law

cses$fh_fog <- qog$fh_fog[match(cses$cyear, qog$cyear)] # Functioning of Government

cses$fh_expr <- qog$fh_feb[match(cses$cyear, qog$cyear)]  #Freedom of expression and beliefs


cses$freedom_net <- qog$fhn_fotnloc[match(cses$cyear, qog$cyear)] # Freedom on the Net: Limits on content

cses$fh_polity <- qog$fh_ipolity2[match(cses$cyear, qog$cyear)]  #Freeedom House + Polity 

### EXISTEM OUTROS - VER EM QOG ! 


##### POLARIZATION #####

#polariz<- read_excel("C:/Users/livia/OneDrive - usp.br/TESE/PROJETO - CSES/Polarization_Apr2017-2/RJD_PolarizationDatabase1-4.xlsx")
#Desativado porque eu fiz uma altera��o "na m�o", usar a planilha j�
#salva com a altera��o!

# Link para o arquivo original, detalhes e codebook:
# https://cses.org/data-download/download-data-documentation/party-system-polarization-index-for-cses-modules-1-4/
# Criar vers�o resumida do CSES (talvez n�o fosse necess�rio mas
#evita mais vari�veis no CSES, o que al�m de deixar o banco mais 
#dif�cil de lidar sobrecarrega minha mem�ria. 
#Tamb�m n�o sei o comportamento do match com muitos dados 
#da mesma unidade)

tab_cses <- cses %>% group_by(election, country) %>%
  summarize_all (.funs = c(mean="mean")) 

### Criar vari�vel s� com as 3 letras do CSES(tirar o ano de "election")
tab_cses$cname <- substr(tab_cses$election, 1,3) 

#Criar a vari�vel semelhante no banco de Polariza��o 
#polariz$cname <- tab_cses$cname[match(polariz$Country, tab_cses$country)]  

#Criar planilha para resolver alguns casos manualmente(South Korea, USA, 
#UK, Russia)

#write.csv(polariz, file = "polariz.csv")
#Desativado porque eu fiz uma altera��o "na m�o", usar a planilha j�
#salva com a altera��o!


polariz <- read.csv(file="polariz.csv")

#### Criando vari�vel para country + year

polariz$cyear <- paste(polariz$cname, polariz$Year, sep = "_")


#Finalmente, o match para criar a vari�vel no CSES:
cses$dalton_pol <- polariz$Polarization[match(cses$cyear, polariz$cyear)]  

cses <- cses %>%
  mutate_at(.vars = vars(dalton_pol), 
            .funs = list(~ifelse(. > 90, NA, .)))


#tab_cses <- 
 # tab_cses %>%
  #mutate(totalseats = rowSums(select(., IMD5002_A_mean:IMD5002_I_mean), na.rm = T))
