###### 2.1.2 - LH - DISTRICT CANDIDATE #####


##### MISSING - variáveis de ideologia #####

cses_leg <- cses_leg %>%
  mutate_at(.vars = vars(ideol_self,
                         starts_with("ideolparty"),
                         starts_with("ex_ideol")), 
            .funs = funs(ifelse(. > 90, NA, .)))


##### IDEOLOGY OF PARTY VOTED #####

# VOTER PLACEMENT ## 

cses_leg <- cses_leg %>% mutate (
  ideol_voted_LH_DC = case_when(
    numparty_A == vote_LH_DC ~ ideolparty_A,
    numparty_B == vote_LH_DC ~ ideolparty_B,
    numparty_C == vote_LH_DC ~ ideolparty_C,
    numparty_D == vote_LH_DC ~ ideolparty_D,
    numparty_E == vote_LH_DC ~ ideolparty_E,
    numparty_F == vote_LH_DC ~ ideolparty_F,
    numparty_G == vote_LH_DC ~ ideolparty_G,
    numparty_H == vote_LH_DC ~ ideolparty_H,
    numparty_I == vote_LH_DC ~ ideolparty_I,
    TRUE                    ~ vote_LH_DC
  )
)

## EXPERT PLACEMENT ##

cses_leg <- cses_leg %>% mutate (
  exp_ideol_voted_LH_DC = case_when(
    numparty_A == vote_LH_DC ~ ex_ideolparty_A,
    numparty_B == vote_LH_DC ~ ex_ideolparty_B,
    numparty_C == vote_LH_DC ~ ex_ideolparty_C,
    numparty_D == vote_LH_DC ~ ex_ideolparty_D,
    numparty_E == vote_LH_DC ~ ex_ideolparty_E,
    numparty_F == vote_LH_DC ~ ex_ideolparty_F,
    numparty_G == vote_LH_DC ~ ex_ideolparty_G,
    numparty_H == vote_LH_DC ~ ex_ideolparty_H,
    numparty_I == vote_LH_DC ~ ex_ideolparty_I,
    TRUE                    ~ vote_LH_DC
  )
)

## VOTERS MEAN ##

cses_leg <- cses_leg %>% mutate (
  meanv_ideol_voted_LH_DC = case_when(
    numparty_A == vote_LH_DC ~ ideol_mean_A,
    numparty_B == vote_LH_DC ~ ideol_mean_B,
    numparty_C == vote_LH_DC ~ ideol_mean_C,
    numparty_D == vote_LH_DC ~ ideol_mean_D,
    numparty_E == vote_LH_DC ~ ideol_mean_E,
    numparty_F == vote_LH_DC ~ ideol_mean_F,
    numparty_G == vote_LH_DC ~ ideol_mean_G,
    numparty_H == vote_LH_DC ~ ideol_mean_H,
    numparty_I == vote_LH_DC ~ ideol_mean_I,
    TRUE                    ~ vote_LH_DC
  )
)

##### CONGRUÊNCIA #####

cses_leg <- mutate (cses_leg, cong_LH_DC = abs(ideol_self - ideol_voted_LH_DC)) # CITIZEN PLACEMENT 
cses_leg <- mutate (cses_leg, exp_cong_LH_DC = abs(ideol_self - exp_ideol_voted_LH_DC)) # EXPERT PLACEMENT
cses_leg <- mutate (cses_leg, meanv_cong_LH_DC = abs(ideol_self - meanv_ideol_voted_LH_DC)) # CITIZENS MEAN 

##### VOLUNTARY INCONGRUENCE #####
# Diferença entre congruência com partido votado e com o closest

cses_leg$dif_cls_LH_DC <- cses_leg$cong_LH_DC - cses_leg$cong_closest


##### DIFERENÇA DE PERCEPÇÃO CITIZEN/EXPERT #####

cses_leg$voter_exp_dif_LH_DC <- abs(cses_leg$exp_ideol_voted_LH_DC - cses_leg$ideol_voted_LH_DC)

##### DIFERENÇA DE PERCEPÇÃO CITIZEN/MEAN OF CITIZENS #####

cses_leg$voter_meanv_dif_LH_DC <- abs(cses_leg$meanv_ideol_voted_LH_DC - cses_leg$ideol_voted_LH_DC)


# VOTED CLOSEST PERCEIVED? DUMMY (%)
cses_leg$voted_closest_LH_DC <- with(cses_leg, as.numeric (closest == ideol_voted_LH_DC))

# VOTED CLOSEST EXPERT? DUMMY(%)
cses_leg$voted_exp_closest_LH_DC <- with(cses_leg, as.numeric (exp_closest == ideol_voted_LH_DC))


##### CRIANDO VARIÁVEIS PARA SOMAR OS DOIS #####

### CALCULAR TODOS INDICADORES PARA LOWER HOUSE - ALL: 

##### CONGRUÊNCIA - alterar esse código ##### 

#CITIZEN PLACEMENT

cses_leg <- cses_leg %>% mutate (
  cong_LH_all = case_when(
    is.na(cong_LH_DC) ~ cong_LH_PL,
    is.na(cong_LH_PL) ~ cong_LH_DC,
    !is.na(cong_LH_DC) && !is.na(cong_LH_PL) ~ (cong_LH_DC+cong_LH_PL)/2
  )
)

#EXPERT PLACEMENT
cses_leg <- cses_leg %>% mutate (
  exp_cong_LH_all = case_when(
    is.na(exp_cong_LH_DC) ~ exp_cong_LH_PL,
    is.na(exp_cong_LH_PL) ~ exp_cong_LH_DC,
    !is.na(exp_cong_LH_DC) && !is.na(exp_cong_LH_PL) ~ (exp_cong_LH_DC+exp_cong_LH_PL)/2
  )
)

#VOTERS MEAN
cses_leg <- cses_leg %>% mutate (
  meanv_cong_LH_all = case_when(
    is.na(exp_cong_LH_DC) ~ meanv_cong_LH_PL,
    is.na(exp_cong_LH_PL) ~ meanv_cong_LH_DC,
    !is.na(exp_cong_LH_DC) && !is.na(meanv_cong_LH_PL) ~ (meanv_cong_LH_DC + meanv_cong_LH_PL)/2
  )
)


##### VOLUNTARY INCONGRUENCE #####
# Diferença entre congruência com partido votado e com o closest
cses_leg <- cses_leg %>% mutate (
  dif_cls_LH_all = case_when(
    is.na(dif_cls_LH_DC) ~ dif_cls_LH_PL,
    is.na(dif_cls_LH_PL) ~ dif_cls_LH_DC,
    !is.na(dif_cls_LH_DC) && !is.na(dif_cls_LH_PL) ~ (dif_cls_LH_DC + dif_cls_LH_PL)/2
  )
)

##### DIFERENÇA PERCEPÇÃO CITIZEN PLACEMENT- EXPERT #####
cses_leg <- cses_leg %>% mutate (
  voter_exp_dif_LH_all = case_when(
    is.na(voter_exp_dif_LH_DC) ~ voter_exp_dif_LH_PL,
    is.na(voter_exp_dif_LH_PL) ~ voter_exp_dif_LH_DC,
    !is.na(voter_exp_dif_LH_DC) && !is.na(voter_exp_dif_LH_PL) ~ (voter_exp_dif_LH_DC + voter_exp_dif_LH_PL)/2
  )
)


##### DIFERENÇA PERCEPÇÃO CITIZEN PLACEMENT- CITIZEN MEAN #####
cses_leg <- cses_leg %>% mutate (
  voter_meanv_dif_LH_all = case_when(
    is.na(voter_meanv_dif_LH_DC) ~ voter_meanv_dif_LH_PL,
    is.na(voter_meanv_dif_LH_PL) ~ voter_meanv_dif_LH_DC,
    !is.na(voter_meanv_dif_LH_DC) && !is.na(voter_meanv_dif_LH_PL) ~ (voter_meanv_dif_LH_DC + voter_meanv_dif_LH_PL)/2
  )
)


### Novamente tabela geral, para ver resultados de todas variáveis criadas: 
tab_leg <- cses_leg %>% group_by(election, country) %>%
  summarize_all (.funs = c(mean="mean"), na.rm = T)

write.csv(tab_leg, file = "Legislative- summary (means).csv")

## PARA TODOS CÓDIGOS: CRIAR TODOS INDICADORES IMPORTANTES/ TIRAR TABELAS - UH, VER ESSE E LH_PL SE ESTÁ OK 
## Mudar os exp_cong_voted para exp_cong apenas
#Criar: cses_leg$dif_cls_UH <- cses_leg$cong_LH_DC - cses_leg$cong_closest


