##### SECOND ROUND  #####



#####  FILTRAR ELEIÇÕES C SEGUNDO TURNO #####
#VER! JÁ TENHO OS CÓDIGOS ALI PELA PLANILHA, SÓ INSERIR AQUI!
#MAS ANTES INSERINDO % VOTES (ABAIXO) JÁ SERVINDO COMO DOUBLE-CHECK DE QUAIS FORAM PAÍSES COM 2º TURNO!

#cses_pr2 <- cses_pr % filter


##### INSERINDO DADOS DOS 2 PARTIDOS QUE FORAM ##### 

#CRIAR AS VARIÁVEIS - PCV_2???

#ARG_2015
cses_pr$pcv_2PR_A[cses_pr$election == "ARG_2015"] <- 48.66
cses_pr$pcv_2PR_B[cses_pr$election == "ARG_2015"] <- 51.34

#BRA_2002
cses_pr$pcv_2PR_A[cses_pr$election == "BRA_2002"] <- 61.27
cses_pr$pcv_2PR_B[cses_pr$election == "BRA_2002"] <- 38.73
  

#BRA_2006
cses_pr$pcv_2PR_B[cses_pr$election == "BRA_2006"] <- 60.83
cses_pr$pcv_2PR_C[cses_pr$election == "BRA_2006"] <- 39.17

#BRA_2010
cses_pr$pcv_2PR_A[cses_pr$election == "BRA_2010"] <- 56.05
cses_pr$pcv_2PR_C[cses_pr$election == "BRA_2010"] <- 43.95

#BRA_2014
cses_pr$pcv_2PR_A[cses_pr$election == "BRA_2014"] <- 51.64
cses_pr$pcv_2PR_B[cses_pr$election == "BRA_2014"] <- 48.36

#CHL_1999
cses_pr$pcv_2PR_A[cses_pr$election == "CHL_1999"] <- 51.31
cses_pr$pcv_2PR_B[cses_pr$election == "CHL_1999"] <- 48.69

#CHL_2005
cses_pr$pcv_2PR_E[cses_pr$election == "CHL_2005"] <- 53.5
cses_pr$pcv_2PR_D[cses_pr$election == "CHL_2005"] <- 46.5


#CHL_2009
cses_pr$pcv_2PR_B[cses_pr$election == "CHL_2009"] <- 51.61
cses_pr$pcv_2PR_C[cses_pr$election == "CHL_2009"] <- 48.39

#FRA_2002
cses_pr$pcv_2PR_A[cses_pr$election == "FRA_2002"] <- 82.21
cses_pr$pcv_2PR_B[cses_pr$election == "FRA_2002"] <- 17.79


#FRA_2012
cses_pr$pcv_2PR_A[cses_pr$election == "FRA_2012"] <- 51.6
cses_pr$pcv_2PR_B[cses_pr$election == "FRA_2012"] <- 48.4


#PER_2000
cses_pr$pcv_2PR_A[cses_pr$election == "PER_2000"] <- 74.33
cses_pr$pcv_2PR_B[cses_pr$election == "PER_2000"] <- 25.67

#PER_2001
cses_pr$pcv_2PR_A[cses_pr$election == "PER_2001"] <- 53.08
cses_pr$pcv_2PR_B[cses_pr$election == "PER_2001"] <- 46.92

# PER_2006
cses_pr$pcv_2PR_A[cses_pr$election == "PER_2006"] <- 47.37
cses_pr$pcv_2PR_B[cses_pr$election == "PER_2006"] <- 52.63


# PER_2011
cses_pr$pcv_2PR_A[cses_pr$election == "PER_2011"] <- 51.45
cses_pr$pcv_2PR_B[cses_pr$election == "PER_2011"] <- 48.55


# PER_2016
cses_pr$pcv_2PR_B[cses_pr$election == "PER_2016"] <- 50.1
cses_pr$pcv_2PR_A[cses_pr$election == "PER_2016"] <- 49.9

# ROU_1996
cses_pr$pcv_2PR_A[cses_pr$election == "ROU_1996"] <- 54.4
cses_pr$pcv_2PR_B[cses_pr$election == "ROU_1996"] <- 45.6

# ROU_2004

# COMO NOS DADOS DE PRIMEIRO TURNO, AQUI TAMBÉM PRECISAMOS ADEQUAR VOTE aos códigos numéricos
#dos PARTIDOS, não das alianças:

cses_pr$vote_PR_2[cses_pr$election == "ROU_2004" & cses_pr$vote_PR_2 == 6420026] <- 6420019
cses_pr$vote_PR_2[cses_pr$election == "ROU_2004" & cses_pr$vote_PR_2 == 6420041] <- 6420001

#Agora sim, % of votes:

cses_pr$pcv_2PR_A[cses_pr$election == "ROU_2004"] <- 48.8  #PSD - Party of Social Democracy 
cses_pr$pcv_2PR_C[cses_pr$election == "ROU_2004"] <- 51.2   #PD - Democratic Party 

# ROU_2009
cses_pr$pcv_2PR_A[cses_pr$election == "ROU_2009"] <- 50.33
cses_pr$pcv_2PR_B[cses_pr$election == "ROU_2009"] <- 49.67

# ROU_2014
cses_pr$pcv_2PR_A[cses_pr$election == "ROU_2014"] <- 45.56
cses_pr$pcv_2PR_B[cses_pr$election == "ROU_2014"] <- 54.43


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


# URY_2009
cses_pr$pcv_2PR_A[cses_pr$election == "URY_2009"] <- 54.63
cses_pr$pcv_2PR_B[cses_pr$election == "URY_2009"] <- 45.37 









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


#### CONTINUAR IGUAL PRIMEIRO TURNO, MAS AJUSTAR TODA AMOSTRA PARA CLOSEST (SÓ FAZER O ESQUEMA LÁ DE IDEOL POR PCV, MAS PCV DO SEGUNDO!)


### DEPOIS FAZER COMPARAÇÕES DE PRIMEIRO E SEGUNDO TURNO TIPO:


pr_cong_exp <- cses_pr %>% group_by(election) %>%
  summarise_at(.funs = mean, na.rm=T,.vars = vars(starts_with("ex_ideol"), exp_cong_voted_PR_1, exp_cong_voted_PR_2))

