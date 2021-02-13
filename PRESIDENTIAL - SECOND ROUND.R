##### SECOND ROUND  #####



#####  FILTRAR ELEIÇÕES C SEGUNDO TURNO #####
#VER! JÁ TENHO OS CÓDIGOS ALI PELA PLANILHA, SÓ INSERIR AQUI!
#MAS ANTES INSERINDO % VOTES (ABAIXO) JÁ SERVINDO COMO DOUBLE-CHECK DE QUAIS FORAM PAÍSES COM 2º TURNO!

#cses_pr2 <- cses_pr % filter


##### DETERMINAR OS 2 PARTIDOS QUE FORAM ##### 

# COMO PODERIA USAR UM CÓDIGO PARA OS 2 MAIORES % VOTES, OU FAZER NA MÃO O PCV DELES DE SEGUNDO TURNO - MELHOR FAZER ISSO JÁ USAR PARA 
#EMD!!!! 




#CRIAR AS VARIÁVEIS - PCV_2???

#ARG_2015
cses_pr$pcv_PR2_A[cses_pr$election == "ARG_2015"] <- 48.66
cses_pr$pcv_PR2_B[cses_pr$election == "ARG_2015"] <- 51.34

#BRA_2002
cses_pr$pcv_PR2_A[cses_pr$election == "BRA_2002"] <- 61.27
cses_pr$pcv_PR2_B[cses_pr$election == "BRA_2002"] <- 38.73
  

#BRA_2006



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

