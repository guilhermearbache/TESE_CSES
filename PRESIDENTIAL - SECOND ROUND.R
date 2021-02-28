##### SECOND ROUND  #####


#CRIANDO DUMMY PARA EXISTÊNCIA DE 2º TURNO

cses_pr$round2_PR <- 0

cses_pr$round2_PR[cses_pr$election =="ARG_2015"] <- 1
cses_pr$round2_PR[cses_pr$election =="BRA_2002"] <- 1
cses_pr$round2_PR[cses_pr$election =="BRA_2006"] <- 1
cses_pr$round2_PR[cses_pr$election =="BRA_2010"] <- 1
cses_pr$round2_PR[cses_pr$election =="BRA_2014"] <- 1
cses_pr$round2_PR[cses_pr$election =="CHL_1999"] <- 1
cses_pr$round2_PR[cses_pr$election =="CHL_2005"] <- 1
cses_pr$round2_PR[cses_pr$election =="CHL_2009"] <- 1
cses_pr$round2_PR[cses_pr$election =="FRA_2002"] <- 1
cses_pr$round2_PR[cses_pr$election =="FRA_2012"] <- 1
cses_pr$round2_PR[cses_pr$election =="LTU_1997"] <- 1
cses_pr$round2_PR[cses_pr$election =="PER_2000"] <- 1
cses_pr$round2_PR[cses_pr$election =="PER_2001"] <- 1
cses_pr$round2_PR[cses_pr$election =="PER_2006"] <- 1
cses_pr$round2_PR[cses_pr$election =="PER_2011"] <- 1
cses_pr$round2_PR[cses_pr$election =="PER_2016"] <- 1
cses_pr$round2_PR[cses_pr$election =="ROU_1996"] <- 1
cses_pr$round2_PR[cses_pr$election =="ROU_2004"] <- 1
cses_pr$round2_PR[cses_pr$election =="ROU_2009"] <- 1
cses_pr$round2_PR[cses_pr$election =="ROU_2014"] <- 1
cses_pr$round2_PR[cses_pr$election =="SRB_2012"] <- 1
cses_pr$round2_PR[cses_pr$election =="URY_2009"] <- 1


#####  FILTRAR ELEIÇÕES C SEGUNDO TURNO #####

cses2PR <- cses_pr %>% filter (round2_PR == 1)


##### INSERINDO DADOS DOS 2 PARTIDOS QUE FORAM ##### 

# SÓ PRECISAREMOS CRIAR A,B,C,D e E (NÃO TEM NENHUM CASO QUE SAI DESSAS LETRAS):

cses2PR$pcv_2PR_A <- NA
cses2PR$pcv_2PR_B <- NA
cses2PR$pcv_2PR_C <- NA
cses2PR$pcv_2PR_D <- NA
cses2PR$pcv_2PR_E <- NA

#ARG_2015
cses2PR$pcv_2PR_A[cses2PR$election == "ARG_2015"] <- 48.66
cses2PR$pcv_2PR_B[cses2PR$election == "ARG_2015"] <- 51.34

#BRA_2002
cses2PR$pcv_2PR_A[cses2PR$election == "BRA_2002"] <- 61.27
cses2PR$pcv_2PR_B[cses2PR$election == "BRA_2002"] <- 38.73
  

#BRA_2006
cses2PR$pcv_2PR_B[cses2PR$election == "BRA_2006"] <- 60.83
cses2PR$pcv_2PR_C[cses2PR$election == "BRA_2006"] <- 39.17

#BRA_2010
cses2PR$pcv_2PR_A[cses2PR$election == "BRA_2010"] <- 56.05
cses2PR$pcv_2PR_C[cses2PR$election == "BRA_2010"] <- 43.95

#BRA_2014
cses2PR$pcv_2PR_A[cses2PR$election == "BRA_2014"] <- 51.64
cses2PR$pcv_2PR_B[cses2PR$election == "BRA_2014"] <- 48.36

#CHL_1999
cses2PR$pcv_2PR_A[cses2PR$election == "CHL_1999"] <- 51.31
cses2PR$pcv_2PR_B[cses2PR$election == "CHL_1999"] <- 48.69

#CHL_2005
cses2PR$pcv_2PR_E[cses2PR$election == "CHL_2005"] <- 53.5
cses2PR$pcv_2PR_D[cses2PR$election == "CHL_2005"] <- 46.5


#CHL_2009
cses2PR$pcv_2PR_B[cses2PR$election == "CHL_2009"] <- 51.61
cses2PR$pcv_2PR_C[cses2PR$election == "CHL_2009"] <- 48.39

#FRA_2002
cses2PR$pcv_2PR_A[cses2PR$election == "FRA_2002"] <- 82.21
cses2PR$pcv_2PR_B[cses2PR$election == "FRA_2002"] <- 17.79


#FRA_2012
cses2PR$pcv_2PR_A[cses2PR$election == "FRA_2012"] <- 51.6
cses2PR$pcv_2PR_B[cses2PR$election == "FRA_2012"] <- 48.4


#PER_2000
cses2PR$pcv_2PR_A[cses2PR$election == "PER_2000"] <- 74.33
cses2PR$pcv_2PR_B[cses2PR$election == "PER_2000"] <- 25.67

#PER_2001
cses2PR$pcv_2PR_A[cses2PR$election == "PER_2001"] <- 53.08
cses2PR$pcv_2PR_B[cses2PR$election == "PER_2001"] <- 46.92

# PER_2006
cses2PR$pcv_2PR_A[cses2PR$election == "PER_2006"] <- 47.37
cses2PR$pcv_2PR_B[cses2PR$election == "PER_2006"] <- 52.63


# PER_2011
cses2PR$pcv_2PR_A[cses2PR$election == "PER_2011"] <- 51.45
cses2PR$pcv_2PR_B[cses2PR$election == "PER_2011"] <- 48.55


# PER_2016
cses2PR$pcv_2PR_B[cses2PR$election == "PER_2016"] <- 50.1
cses2PR$pcv_2PR_A[cses2PR$election == "PER_2016"] <- 49.9

# ROU_1996
cses2PR$pcv_2PR_A[cses2PR$election == "ROU_1996"] <- 54.4
cses2PR$pcv_2PR_B[cses2PR$election == "ROU_1996"] <- 45.6

# ROU_2004

# COMO NOS DADOS DE PRIMEIRO TURNO, AQUI TAMBÉM PRECISAMOS ADEQUAR VOTE aos códigos numéricos
#dos PARTIDOS, não das alianças:

cses2PR$vote_PR_2[cses2PR$election == "ROU_2004" & cses2PR$vote_PR_2 == 6420026] <- 6420019
cses2PR$vote_PR_2[cses2PR$election == "ROU_2004" & cses2PR$vote_PR_2 == 6420041] <- 6420001

#Agora sim, % of votes:

cses2PR$pcv_2PR_A[cses2PR$election == "ROU_2004"] <- 48.8  #PSD - Party of Social Democracy 
cses2PR$pcv_2PR_C[cses2PR$election == "ROU_2004"] <- 51.2   #PD - Democratic Party 

# ROU_2009
cses2PR$pcv_2PR_A[cses2PR$election == "ROU_2009"] <- 50.33
cses2PR$pcv_2PR_B[cses2PR$election == "ROU_2009"] <- 49.67

# ROU_2014
cses2PR$pcv_2PR_A[cses2PR$election == "ROU_2014"] <- 45.56
cses2PR$pcv_2PR_B[cses2PR$election == "ROU_2014"] <- 54.43


#SRB_2012
cses2PR <- cses2PR %>% mutate (
  pcv_2PR_A = case_when(
    election == "SRB_2012" ~  49.54 ,
    TRUE          ~ pcv_2PR_A 
  )
)

cses2PR <- cses2PR %>% mutate (
  pcv_2PR_B = case_when(
    election == "SRB_2012" ~  47.31 ,
    TRUE          ~ pcv_2PR_B
  )
)


# URY_2009
cses2PR$pcv_2PR_A[cses2PR$election == "URY_2009"] <- 54.63
cses2PR$pcv_2PR_B[cses2PR$election == "URY_2009"] <- 45.37 


###### IDEOLOGY - PARTY VOTED #####

## VOTER PLACEMENT (PERCEIVED IDEOLOGY)

cses_pr <- cses_pr %>% mutate (
  ideol_voted_PR_2 = case_when(
    numparty_A == vote_PR_2 ~ ideolparty_A,
    numparty_B == vote_PR_2 ~ ideolparty_B,
    numparty_C == vote_PR_2 ~ ideolparty_C,
    numparty_D == vote_PR_2 ~ ideolparty_D,
    numparty_E == vote_PR_2 ~ ideolparty_E,
    numparty_F == vote_PR_2 ~ ideolparty_F,
    numparty_G == vote_PR_2 ~ ideolparty_G,
    numparty_H == vote_PR_2 ~ ideolparty_H,
    numparty_I == vote_PR_2 ~ ideolparty_I,
    TRUE                    ~ vote_PR_2
  )
)

## EXPERT PLACEMENT

# COMO HAVIAM ALGUMAS COLUNAS COM INTEGERS EM EXPERT_IDEOLOGY, TEMOS QUE TRANSFORMAR:

cses_pr <- cses_pr %>%
  mutate_at(vars(vote_PR_2),
            as.numeric) 

cses_pr <- cses_pr %>% mutate (
  exp_ideol_voted_PR_2 = case_when(
    numparty_A == vote_PR_2 ~ ex_ideolparty_A,
    numparty_B == vote_PR_2 ~ ex_ideolparty_B,
    numparty_C == vote_PR_2 ~ ex_ideolparty_C,
    numparty_D == vote_PR_2 ~ ex_ideolparty_D,
    numparty_E == vote_PR_2 ~ ex_ideolparty_E,
    numparty_F == vote_PR_2 ~ ex_ideolparty_F,
    numparty_G == vote_PR_2 ~ ex_ideolparty_G,
    numparty_H == vote_PR_2 ~ ex_ideolparty_H,
    numparty_I == vote_PR_2 ~ ex_ideolparty_I,
    TRUE                    ~ vote_PR_2
  )
)

##### IDEOL1 e IDEOL2 #####
#INSERINDO DUAS COLUNAS (ideol1 e ideol2) para ideologia do partido mais votado e 2º mais votado, no primeiro e segundo turnos. 
# AO INVÉS DE CRIAR UMA COLUNA PARA CADA LETRA, COMO FIZ SEMPRE EM PRIMEIRO TURNO, LEGISLATIVO, OPTEI POR ISSO, AÍ SÓ TIRAR O CLOSEST DESSAS 
#DUAS COLUNAS (E também média ou EMD quando for fazer isso)

# AQUI SERIA O CÓDIGO PARA SEGUNDO TURNO (só mudar "pcv_2PR" para "pcv_PR" para fazer do primeiro)

cses2PR <- cses2PR %>% 
  mutate(row = row_number()) %>%
  pivot_longer(cols = contains("pcv_2PR"),
               names_to = "name", 
               values_to = "value") %>%
  group_by(row) %>%
  arrange(row, desc(value)) %>% 
  mutate(ideol1 = get(paste0("ideolparty_",
                             substr(name[1], nchar(name[1]), nchar(name[1])))),
         ideol2 = get(paste0("ideolparty_",
                             substr(name[2], nchar(name[2]), nchar(name[2]))))) %>%
  ungroup() %>%
  pivot_wider(id_cols = -c(name, value),
              names_from = name, 
              values_from = value)


# FAZER ESSE CÓDIGO ACIMA E O DE ideol_ELECTED, ideol_VOTED PARA ALTERNATE IDEOLOGY, EXP ALTERNATE, LEADER, ALT LEADER

### DEPOIS FAZER COMPARAÇÕES DE PRIMEIRO E SEGUNDO TURNO TIPO "DIFERENÇA CLOSEST PRIMEIRO E CLOSEST SEGUNDO", 
#OU MESMO CONGRUÊNCIA DO VOTO NO PRIMEIRO E SEGUNDO:


pr_cong_exp <- cses_pr %>% group_by(election) %>%
  summarise_at(.funs = mean, na.rm=T,.vars = vars(starts_with("ex_ideol"), exp_cong_voted_PR_1, exp_cong_voted_PR_2))

