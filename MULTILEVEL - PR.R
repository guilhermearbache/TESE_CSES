
rm(list=ls()[!(ls() %in% c("cses_exp", "cses_pr"))])

##### PACKAGES #####
library(mlmRev)
library(stargazer)
library(sjPlot)

##### EXPERT CONGRUENCE #####

##### SUBSET ######

expert_drop <- c("BLR_2001", "KGZ_2005", "LTU_1997", "PER_2001", "PHL_2004", "ROU_1996", "RUS_2000", "RUS_2004",
                    "TWN_2012")

'%ni%' <- Negate('%in%')

cses_exp <- cses_pr %>% filter (election %ni% expert_drop)

#, "PHL_2016" - versão com e sem Grace Poe para expert, sem expert nenhuma delas! 
#"TWN_2012" - NÃO TEM EXPERT. 
#"CHL_1999", USA_1996 - NÃO TEM CITIZEN!
#PER_2001 - NÃO TEM EXPERT!

# REGRESSÃO 1 LEVEL # 
EDU1 <- glm(exp_cong_PR_1~ education, data = cses_exp)

KNW1 <- glm(exp_cong_PR_1~ knowledge_adj, data = cses_exp)

VTDIF1 <- glm(exp_cong_PR_1~ voter_exp_dif_PR_1
                        , data = cses_exp)

AGE1 <- glm(exp_cong_PR_1~ age, data = cses_exp) 
AGESQ1 <- glm(exp_cong_PR_1~ age + I(age^2) , data = cses_exp) 
INCOME1 <- glm(exp_cong_PR_1~ income , data = cses_exp) 
CITY1 <- glm(exp_cong_PR_1~ rural , data = cses_exp)
GEND1 <- glm(exp_cong_PR_1~ female , data = cses_exp) 


CLS1 <- glm(exp_cong_PR_1 ~ cong_closest, data = cses_exp) # LIGADA  A INSTITUIÇÕES MAS É NÍVEL MICRO!

# STARGAZE - , APÊNDICE - coeficientes similares, um pouco maiores que multilevel 


##### MULTILEVEL - BIVARIATE MODELS #####

##### COGNITIVES ###### 

EDU <- lmerTest::lmer(exp_cong_PR_1~ education + (1|election), data = cses_exp)

KNW <- lmerTest::lmer(exp_cong_PR_1~ knowledge_adj + (1|election), data = cses_exp)

VTDIF <- lmerTest::lmer(exp_cong_PR_1~ voter_exp_dif_PR_1
                       + (1|election), data = cses_exp)


CLS <- lmerTest::lmer(exp_cong_PR_1 ~ cong_closest + (1|election), data = cses_exp) # LIGADA  A INSTITUIÇÕES MAS É NÍVEL MICRO!

##### OUTRAS DEMOGRÁFICAS #####

AGE <- lmerTest::lmer(exp_cong_PR_1~ age + (1|election), data = cses_exp) # AGE DECREASES CONGRUENCE? nÃO SIGNIFICANTE!
AGESQ <- lmerTest::lmer(exp_cong_PR_1~ age + I(age^2) + (1|election), data = cses_exp) # NADA DE RESULTADO 
INCOME <- lmerTest::lmer(exp_cong_PR_1~ income + (1|election), data = cses_exp) # um pouco mais 
CITY <- lmerTest::lmer(exp_cong_PR_1~ rural + (1|election), data = cses_exp)
GEND <- lmerTest::lmer(exp_cong_PR_1~ female + (1|election), data = cses_exp) 


plot_models(EDU, KNW, VTDIF, AGE, INCOME, CITY, GEND,
           show.legend = FALSE, 
  show.values = TRUE, axis.title = "",   
  ci.lvl =.999, dot.size=2, 
  line.size =1, spacing = .4) + scale_color_sjplot("eight")

##### INSTITUIÇÕES #####


##### OFERTA E SISTEMA ELEITORAL #####

CLS <- lmerTest::lmer(exp_cong_PR_1 ~ cong_closest + (1|election), data = cses_exp) 

CAND <- lmerTest::lmer(exp_cong_PR_1 ~ enpres + (1|election), data = cses_exp)
ENEP <- lmerTest::lmer(exp_cong_PR_1 ~ ENEP + (1|election), data = cses_exp)

PR2 <- lmerTest::lmer(exp_cong_PR_1 ~ round2_PR + (1|election), data = cses_exp)


pr_summary <- cses_exp %>% group_by(election, country) %>%
  select (country, election, enpres) %>%
  summarize_all (.funs = c(mean="mean"), na.rm = T)

##### PR_SYSTEM #####
##### MELHOR USAR system_pr do que round2, porque estamos mais interessados na REGRA ELEITORAL
#Alguns países podem ter a regra mas não ter tido segundo turno naquele caso porque alguém superou a maioria

#system_pr tem 4 categorias: plurality (Mexico, Philippines e Taiwan), abs. majority (segundo turno tradicional), qualified majority (só Arg aqui),
#e electoral college (EUA)


SYS <- lmerTest::lmer(exp_cong_PR_1 ~ plurality + (1|election), data = cses_exp)


# SEM UNITED STATES
plur_maj <- cses_exp %>% filter (country != "United States of America")
  
SYS2 <- lmerTest::lmer(exp_cong_PR_1 ~ plurality + (1|election), data = plur_maj)

plot_models(
  ENEP, CAND, PR2,CLS, SYS, SYS2, axis.labels = c("Sistema pluralista   (com/ sem EUA)",
                                            "Proximidade disponível", "2 turnos","Nº de candidatos", "ENEP"),
  show.legend = FALSE, show.values = TRUE,  axis.title = "", ci.lvl =.99, 
  dot.size=2, line.size =1, spacing = 0.5) 


SYS_3 <- lmerTest::lmer(exp_cong_PR_1 ~ plurality + elec_college + (1|election), data = cses_exp)
SYS_full <- lmerTest::lmer(exp_cong_PR_1 ~ maj + quali_maj + elec_college + (1|election), data = cses_exp) 
#Todas versões do full são parecidas, essa fez mais sentido porque todos tem coeficiente na mesma direção (sugerindo que
#plurality de fato é pior para congruência)


plot_models(SYS_full, show.legend = FALSE,
  axis.labels = c("Maioria absoluta",  "Maioria qualificada", "Colégio eleitoral"),
  show.values = TRUE, axis.title = "", ci.lvl =.95, 
  dot.size=2, line.size =1, spacing = 0.5) 


##### COMPULSORY VOTE #####

CV <- lmerTest::lmer(exp_cong_PR_1 ~ compulsory + (1|election), data = cses_exp)
CV2 <- lmerTest::lmer(exp_cong_PR_1 ~ compulsory_dummy + (1|election), data = cses_exp)

plot_models(
  ENEP, CAND, PR2,CLS, SYS, SYS2, CV, CV2, regime, FH,
  #axis.labels = c("Sistema pluralista   (com/ sem EUA)",
   #                "Proximidade disponível", "2 turnos","Nº de candidatos", "ENEP"),
  show.legend = FALSE, show.values = TRUE,  axis.title = "", ci.lvl =.99, 
  dot.size=2, line.size =1, spacing = 0.5) 


##### REGIME #####

#REGIME AGE
regime <- lmerTest::lmer(exp_cong_PR_1 ~ regime_age + (1|election), data = cses_exp)


#FREEDOM HOUSE 

FH <- lmerTest::lmer(exp_cong_PR_1 ~ freedom_house_1 + (1|election), data = cses_exp)

#Duas grandes dimensões:
FHCIV <- lmerTest::lmer(exp_cong_PR_1 ~ fh_civil + (1|election), data = cses_exp)
FHPOL <- lmerTest::lmer(exp_cong_PR_1 ~ fh_pol + (1|election), data = cses_exp)

# Mais específicas (dentro de political)

FHEL <- lmerTest::lmer(exp_cong_PR_1 ~ fh_elec + (1|election), data = cses_exp)
FHPL <- lmerTest::lmer(exp_cong_PR_1 ~ fh_plural + (1|election), data = cses_exp) #Political Pluralism and Participation

# Outras específicas:

FHEXP <- lmerTest::lmer(exp_cong_PR_1 ~ fh_expr + (1|election), data = cses_exp)  #Freedom of expression and beliefs
FH_ASS <- lmerTest::lmer(exp_cong_PR_1 ~ fh_aor + (1|election), data = cses_exp) #Freedom of association

FH_ROL <- lmerTest::lmer(exp_cong_PR_1 ~ fh_rol + (1|election), data = cses_exp) # Rule of Law
FH_FOG <- lmerTest::lmer(exp_cong_PR_1 ~ fh_fog + (1|election), data = cses_exp)  # Functioning of Government

# Dataset Freedom on the Net: Limits on content 
FNET <- lmerTest::lmer(exp_cong_PR_1 ~ freedom_net + (1|election), data = cses_exp) 

#Freedom House + Polity: 
POL_FH <- lmerTest::lmer(exp_cong_PR_1 ~ fh_polity + (1|election), data = cses_exp) 



plot_models(FH,FHCIV, FHPOL, FHEL, FHPL, FHEXP, FH_ASS, FH_ROL, FH_FOG, FNET, POL_FH,
            axis.labels = c("Polity + Freedom House", "Freedom on the Net: limitação de conteúdo", 
                            "Funcionamento do Governo", "Rule-of-law", "Direito de associação",
                            "Liberdade de expressão", "Pluralismo político e participação",
                            "Processo eleitoral", "Liberdades Políticas", "Liberdades Civis",
                            "Freedom House"),
            show.legend = FALSE, show.values = TRUE,  axis.title = "", ci.lvl =.95, 
            dot.size=2, line.size =1, spacing = 0.5, axis.lim= c(-0.8, 0.8)) + 
  scale_color_sjplot("eight")


## POLITY TEM AQUI? 

#POLARIZATION - mas acho que os dados são em eleição legislativa

POL <- lmerTest::lmer(exp_cong_PR_1 ~ dalton_pol + (1|election), data = cses_exp)

plot_models(POL, 
            show.legend = FALSE, show.values = TRUE,  axis.title = "", ci.lvl =.95, 
            dot.size=2, line.size =1, spacing = 0.5) 


##### VALENCE #####

#GDP_1:GDP_3, cum_gdp,	abs_growth,	cum_gdp2,	abs_growth2, # ECONOMY

ABS_GROWTH <- lmerTest::lmer(exp_cong_PR_1 ~ abs_growth2 + (1|election), data = cses_exp)
GDP_1 <- lmerTest::lmer(exp_cong_PR_1 ~ GDP_1 + (1|election), data = cses_exp)
CUMGDP <- lmerTest::lmer(exp_cong_PR_1 ~ cum_gdp + (1|election), data = cses_exp)


VOLUNT <- lmerTest::lmer(exp_cong_PR_1 ~ dif_cls_PR_1 + (1|election), data = cses_exp)

plot_models(ABS_GROWTH, GDP_1, CUMGDP, VOLUNT,
            show.legend = FALSE, show.values = TRUE,  axis.title = "", ci.lvl =.95, 
            dot.size=2, line.size =1, spacing = 0.5) 

##### FULL MODELS #####

##### TOTALMENTE FULL #####
M1 <- lmerTest::lmer( exp_cong_PR_1 ~ education + knowledge_adj + voter_exp_dif_PR_1
                      + age + income + rural 
                       + cong_closest + + enpres + ENEP +
                        freedom_house_1  + regime_age + 
                        round2_PR + plurality +
                        abs_growth2 + dif_cls_PR_1 + compulsory_dummy 
                      + (1|election), data = cses_exp)

##### TIRAR AS QUE NÃO SÃO SIGNIFICANTES AQUI NEM SOZINHAS #####
#age, rural, abs_growth2, regime, compulsory




##### NESSE MODELO FH QUASE VOLTA AO ÍNDICE NORMAL:


M1.2 <- lmerTest::lmer( exp_cong_PR_1 ~ education + knowledge_adj 
                      + age + income + rural 
                      + cong_closest + + enpres +
                        freedom_house_1  +  
                        round2_PR  +
                         compulsory_dummy 
                      + (1|election), data = cses_exp)



INST <- lmerTest::lmer( exp_cong_PR_1 ~ freedom_house_1 
                           + voter_exp_dif_PR_1  
                      + (1|election), data = cses_exp)


INST <- lmerTest::lmer( exp_cong_PR_1 ~ freedom_house_1 
                        + education + knowledge_adj + regime_age + 
                         plurality
                        + cong_closest + + age + income + rural
                        + (1|election), data = cses_exp)

plot_models(M1, M1.2,
            show.legend = FALSE, show.values = TRUE,  axis.title = "", ci.lvl =.95, 
            dot.size=2, line.size =1, spacing = 0.5) 


M2 <- lmerTest::lmer( exp_cong_PR_1 ~ voter_exp_dif_PR_1 + education + knowledge_adj 
                      + dif_cls_PR_1 + cong_closest + ENEP + compulsory_dummy  +
                        freedom_house_1 +  income  + enpres +
                        round2_PR
                      + (1|election), data = cses_exp)

M3 <- lmerTest::lmer( exp_cong_PR_1 ~ voter_exp_dif_PR_1 + education + knowledge_adj 
                      + dif_cls_PR_1 + cong_closest  +
                        freedom_house_1 +  income  + enpres +
                        plurality
                      + (1|election), data = cses_exp)

plot_models(M2,M3, 
            show.legend = FALSE, show.values = TRUE,  axis.title = "", ci.lvl =.95, 
            dot.size=2, line.size =1, spacing = 0.5) 


##### VARYING SLOPES #####


M1 <- lmerTest::lmer( exp_cong_PR_1 ~ education + knowledge_adj + voter_exp_dif_PR_1
                      + age + income + rural 
                      + cong_closest + + enpres + ENEP +
                        freedom_house_1  + regime_age + 
                        round2_PR + plurality +
                        abs_growth2 + dif_cls_PR_1 + compulsory_dummy 
                      + (1|election), data = cses_exp)

### varying slope para cognitivas, e talvez alguma outra? Closest não sei se faria sentido, 
#por que em um país closest teria MAIS IMPACTO do que em outro? 
#Deve ter relação entre closest para VOTERS e compulsory vote - quem é obrigado a votar tem mais chance de votar mesmo
#não tendo candidatos próximos. Regressão com turnout também para ver isso, closest como VI?


##### NESTED MODELS - ELECTION - COUNTRY 
#Copiar todo script e substituir parte da equação para o nested!

##### NON-NESTED MODELS COUNTRY-YEAR ##### - ACHO QUE SÓ PARA LEGISLATIVE. DISTRICT? (1|DISTRICT|ELECTION)



##### CONGRUÊNCIA SUBJETIVA #####


## OS PRINCIPAIS MODELOS A USAR COM CLOSEST, ETC DEVEM TER EXP_CONG COMO VD!
# SEM EXP_CONG talvez a congruência subjetiva faça mais sentido!


# gráficos de "random effect (re) # ? http://www.strengejacke.de/sjPlot/reference/plot_model.html

##### TABELAS DE REGRESSÃO #####
class(AGE) <- "lmerMod"
class(EC2) <- "lmerMod"
class(EC3) <- "lmerMod"

stargazer(EC1, EC2, EC3)

class(EDU) <- "lmerMod"
class(KNW) <- "lmerMod"
class(VTDIF) <- "lmerMod"


plot_model(SES, type = "pred", terms = "income", "education")


coef(mod1)$Country


#### RESULTADOS DENTRO DAS EXPECTATIVAS - NOS MODELOS COM EXPERT, COGNITIVAS EXPLICAM MAIS,
# NESSA ORDEM: VOTER_EXP_DIF (até demais), KNOWLEDGE, EDUCATION não explica quase nada, 
#Coeficientes muito pequenos, 0.02. No modelo com subjetiva mesmo knowledge já pouco explica 
#Coef de 0.02374

## Mesmo para subjetiva voter cit exp tem o maior coeficiente, o que por um lado contradiz 
#nossos resultados. Por outro, indica que as pessoas que estão votando em candidatos menos próximos
#mesmo em sua própria visão parecem ter menos coerência, percepção no mínimo diferenciada de LR em relação
#a experts, etc.
# 
## Ademais, voter ter maior impacto do que education e knowledge sugere justamente essa questão
#perceptiva que pode ir além de knowledge. 


#Expert e subjetiva muito semelhantes na distribuição por país. Voter exp tbm. Então, por 
#outro lado, isso sugere que não há tanto problema de measurement de ideologia (E principalmente
#de congruência) - junto com o caso de PELA ter tbm distribuições bem parecidas por país
#para LR e issues. 


# com knowledge e principalmente cit_exp_dif como VD. 
IC1 <- lmerTest::lmer(voter_exp_dif_PR_1 ~ education + knowledge_adj + (1|election), data = cses_pr)

IC2 <- lmerTest::lmer(knowledge_adj ~ education + (1|election), data = cses_pr)

#Preferência por variáveis anteriores, mas se elas explicarem
#muito menos que as outras deixar as outras nos modelos principais
#(como controle, em modelos mais complexos, etc.) 


#Regredir closest em ENEP

  IE <- lmerTest::lmer(cong_closest ~ ENEP + (1|election), data = cses_pr)


###Cognitive Interação com compulsory 


# MODEL PLOTS

#Labels ficam em ordem INVERSA dos coeficientes (o primeiro que você coloca ficará acima de tudo), 
#por isso as legendas precisam ficar invertidas para gerar o gráfico correto. 
#PARA CORRIGIR PRECISARIA INSERIR O + guides(color=guide_legend(reverse = TRUE)) mas não deu certo comigo. 

# OUTRAS MELHORIAS - , diminuir distância entre eles, quadrática - COMO PLOTAR O "'PRED"?




#verificar "estrelinhas", coef, etc.

#Começar modelos mais complexos - inserindo variáveis de controle de outras dimensões (preferência pelas que "Explicam mais")
#ou pelas que parecem fazer mais sentido para a questão. Testar alguns modelos mais próximos do "super-full"



