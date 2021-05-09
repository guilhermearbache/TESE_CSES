rm(list=ls()[!(ls() %in% c("cses_leg", "cses_pr"))])

##### PACOTES #####
library(sjPlot)
library(ggplot2)
library(ggrepel)
library(dplyr)
library(stargazer)

##### GENERALIZED MODELS #####
ED <- glm(exp_cong_LH_all~ education, data = cses_leg)

KNW1 <- glm(exp_cong_LH_all~ knowledge_adj, data = cses_leg)

VTDIF1 <- glm(exp_cong_LH_all~ voter_exp_dif_LH_all
              , data = cses_leg)

AGE1 <- glm(exp_cong_LH_all~ age, data = cses_leg) 
AGESQ1 <- glm(exp_cong_LH_all~ age + I(age^2) , data = cses_leg) 
INCOME1 <- glm(exp_cong_LH_all~ income , data = cses_leg) 
CITY1 <- glm(exp_cong_LH_all~ rural , data = cses_leg)
GEND1 <- glm(exp_cong_LH_all~ female , data = cses_leg) 


CLS1 <- glm(exp_cong_LH_all ~ cong_closest_exp, data = cses_leg) # LIGADA  A INSTITUIÇÕES MAS É NÍVEL MICRO!

VAL1 <- glm(exp_cong_LH_all ~ dif_cls_LH_all, data = cses_leg) 




##### MULTILEVEL - BIVARIATE MODELS #####

#DEMOGRAPHICS
AGE <- lmerTest::lmer(exp_cong_LH_all~ age + (1|election), data = cses_leg) 
INCOME <- lmerTest::lmer(exp_cong_LH_all~ income + (1|election), data = cses_leg) 
CITY <- lmerTest::lmer(exp_cong_LH_all~ rural + (1|election) , data = cses_leg)
GEND <- lmerTest::lmer(exp_cong_LH_all~ female + (1|election) , data = cses_leg) 


EDU <- lmerTest::lmer(exp_cong_LH_all~ education + (1|election), data = cses_leg)
KNW <- lmerTest::lmer(exp_cong_LH_all~ knowledge_adj + (1|election), data = cses_leg)

# FRAMEWORK OF 3 

VTDIF <- lmerTest::lmer(exp_cong_LH_all~ voter_exp_dif_LH_all + (1|election), data = cses_leg)
CLS <- lmerTest::lmer(exp_cong_LH_all ~ cong_closest_exp + (1|election), data = cses_leg) 
VAL <- lmerTest::lmer(exp_cong_LH_all ~ dif_cls_LH_all + (1|election), data = cses_leg) 


# MACRO-POLITICAL

CV <- lmerTest::lmer(exp_cong_LH_all ~ compulsory_dummy + (1|election), data = cses_leg)
regime <- lmerTest::lmer(exp_cong_LH_all ~ regime_age + (1|election), data = cses_leg)
FH <- lmerTest::lmer(exp_cong_LH_all ~ freedom_house_1 + (1|election), data = cses_leg)
POL <- lmerTest::lmer(exp_cong_LH_all ~ dalton_pol + (1|election), data = cses_leg)


GALLAGHER <-  lmerTest::lmer(exp_cong_LH_all ~ gallagher + (1|election), data = cses_leg)
ENEP <- lmerTest::lmer(exp_cong_LH_all ~ ENEP + (1|election), data = cses_leg)


ABS_GROWTH <- lmerTest::lmer(exp_cong_LH_all ~ abs_growth2 + (1|election), data = cses_leg)

##### MODELO FULL #####

M1 <- lmerTest::lmer( exp_cong_LH_all ~ voter_exp_dif_LH_all + cong_closest_exp + dif_cls_LH_all + 
                        education + knowledge_adj + age + income + rural + female +
                        gallagher + ENEP + freedom_house_1 +  dalton_pol 
                      + compulsory_dummy + abs_growth2   
                      + (1|election), data = cses_leg)

##### FIGURAS - BIVARIATE, FULL #####
plot_models(VTDIF, CLS, VAL, EDU, KNW, AGE, INCOME, CITY, GEND,
            GALLAGHER, ENEP, FH, POL, CV, ABS_GROWTH,
            axis.labels = c("Variação PIB", "Voto compulsório", "Polarização",
                            "Freedom House", 
                            "ENEP", "Gallagher", "Sexo feminino", "Área rural", "Renda", "Idade", 
                            "Conhecimento", "Escolaridade", "DDVP",
                            "DME", "DPCE"),
            show.legend = FALSE, show.values = TRUE, 
            axis.title = "", ci.lvl =.95, 
            dot.size=1.4, line.size =1, spacing = 0.3,
            value.size = 3.5, colors = "blue", axis.lim= c(-1, 1)) 

plot_models(M1, axis.labels = c("Variação PIB", "Voto compulsório", "Polarização",
                               "Freedom House", "ENEP", "Gallagher", "Sexo feminino", "Área rural",
                               "Renda", "Idade", "Conhecimento", "Escolaridade",
                               "DDVP","DME", "DPCE"),
                    show.legend = FALSE, show.values = TRUE, 
                    axis.title = "", ci.lvl =.95, 
                    dot.size=1.4, line.size =1, spacing = 0.3,
                    value.size = 3.4, colors = "blue", axis.lim= c(-1, 1)) 

class(M1) <- "lmerMod"
class(VTDIF) <- "lmerMod"
class(CLS) <- "lmerMod"
class(VAL) <- "lmerMod"
class(EDU) <- "lmerMod"
class(KNW) <- "lmerMod"
class(AGE) <- "lmerMod"
class(INCOME) <- "lmerMod"
class(CITY) <- "lmerMod"
class(GEND) <- "lmerMod"
class(GALLAGHER) <- "lmerMod"
class(ENEP) <- "lmerMod"
class(FH) <- "lmerMod"
class(POL) <- "lmerMod"
class(CV) <- "lmerMod"
class(ABS_GROWTH) <- "lmerMod"


##### ALTERNANDO KNOWLEDGE E EDUCATION #####
M2 <- lmerTest::lmer( exp_cong_LH_all ~ voter_exp_dif_LH_all + cong_closest_exp + dif_cls_LH_all + 
                        education + knowledge_adj + 
                        gallagher + ENEP + freedom_house_1 + 
                      + compulsory_dummy   
                      + (1|election), data = cses_leg)

M3 <- lmerTest::lmer( exp_cong_LH_all ~ voter_exp_dif_LH_all + cong_closest_exp + dif_cls_LH_all + 
                        education  + 
                        gallagher + ENEP + freedom_house_1 + 
                        + compulsory_dummy   
                      + (1|election), data = cses_leg)

M4 <- lmerTest::lmer( exp_cong_LH_all ~ voter_exp_dif_LH_all + cong_closest_exp + dif_cls_LH_all + 
                        knowledge_adj + 
                        gallagher + ENEP + freedom_house_1 + 
                        + compulsory_dummy   
                      + (1|election), data = cses_leg)

M5 <- lmerTest::lmer( exp_cong_LH_all ~ voter_exp_dif_LH_all + cong_closest_exp + dif_cls_LH_all + 
                        knowledge_adj + dalton_pol +
                         ENEP + freedom_house_1 + 
                        + compulsory_dummy   
                      + (1|election), data = cses_leg)


##### FIGURA 2 - CLEAN MODEL #####
plot_models(M2, M3, M4,M5,
                       show.legend = FALSE, show.values = TRUE,  axis.title = "", ci.lvl =.95, 
            dot.size=2, line.size =1, spacing = 0.5, axis.lim= c(-0.8, 0.8)) + 
  scale_color_sjplot("eight")

class(M1) <- "lmerMod"
class(M2) <- "lmerMod"
class(M3) <- "lmerMod"
class(M4) <- "lmerMod"
class(M5) <- "lmerMod"

stargazer(M1, M2, M3, M4,M5, covariate.labels= c("DPCE", "DME", "DDVP", "Educação", 
                                                 "Conhecimento", "Idade", "Renda", "Rural", 
                                                 "Sexo feminino", "Gallagher", "ENEP", "Freedom House", 
                                                 "Polarização", "Voto compulsório", "Variação do PIB"))

##### DESAGREGANDO FREEDOM HOUSE #####

#Duas grandes dimensões:
  FHCIV <- lmerTest::lmer(exp_cong_LH_all ~ fh_civil + (1|election), data = cses_leg)
  FHPOL <- lmerTest::lmer(exp_cong_LH_all ~ fh_pol + (1|election), data = cses_leg)
  FHBOTH <- lmerTest::lmer(exp_cong_LH_all ~ fh_pol + fh_civil + (1|election), data = cses_leg)
  
  M6 <- lmerTest::lmer( exp_cong_LH_all ~ voter_exp_dif_LH_all + cong_closest_exp + dif_cls_LH_all + 
                          knowledge_adj + 
                          gallagher + ENEP + fh_civil + 
                        + (1|election), data = cses_leg)
  
  M7 <-lmerTest::lmer( exp_cong_LH_all ~ voter_exp_dif_LH_all + cong_closest_exp + dif_cls_LH_all + 
                         knowledge_adj + 
                         gallagher + ENEP + fh_pol + 
                         + (1|election), data = cses_leg) 
  
  M8 <-lmerTest::lmer( exp_cong_LH_all ~ voter_exp_dif_LH_all + cong_closest_exp + dif_cls_LH_all + 
                         knowledge_adj + 
                         gallagher + ENEP + fh_civil + fh_pol +
                          (1|election), data = cses_leg) 
  
  
  p <- plot_models(M6, M7, M8, 
              axis.labels = c( "Freedom House    (dimensão política)", "   Freedom House    (dimensão civil)",
                               "ENEP", "Gallagher", 
                               "Conhecimento político", "Distância voluntária",
                               "DME", "Dif. Cidadão/expert"),
              show.legend = TRUE, legend.title = "Modelos", show.values = TRUE,  axis.title = "", ci.lvl =.95, 
              dot.size=2, value.size = 3, line.size =1, spacing = 0.7, axis.lim= c(-0.8, 0.8))

  p + scale_color_sjplot("social", labels = c("Apenas civil", "Apenas política", "Completo"))
  
  class(M6) <- "lmerMod"
  class(M7) <- "lmerMod"
  class(M8) <- "lmerMod"
  class(FHCIV) <- "lmerMod"
  class(FHPOL) <- "lmerMod"
  class(FHBOTH) <- "lmerMod"
  
  stargazer(M6,M7,M8,FHCIV,FHPOL, FHBOTH, covariate.labels= c("DPCE", "DME", "DDVP",  
                                                     "Conhecimento", "Gallagher", "ENEP", "Direitos civis", 
                                                     "Direitos políticos"))
  
  


###### ALTERNATIVE DEPENDENT VARIABLES #####


##### CONGRUÊNCIA SUBJETIVA #####

##### BIVARIATE #####

AGE <- lmerTest::lmer(cong_LH_all~ age + (1|election), data = cses_leg) 
INCOME <- lmerTest::lmer(cong_LH_all~ income + (1|election), data = cses_leg) 
CITY <- lmerTest::lmer(cong_LH_all~ rural + (1|election) , data = cses_leg)
GEND <- lmerTest::lmer(cong_LH_all~ female + (1|election) , data = cses_leg) 

EDU <- lmerTest::lmer(cong_LH_all~ education + (1|election), data = cses_leg)
KNW <- lmerTest::lmer(cong_LH_all~ knowledge_adj + (1|election), data = cses_leg)

CLS <- lmerTest::lmer(cong_LH_all ~ cong_closest + (1|election), data = cses_leg)  #ELECTORAL SUPPLY
VAL <- lmerTest::lmer(cong_LH_all ~ dif_cls_LH_all + (1|election), data = cses_leg) #VOLUNTARY INCONGRUENCE


##### INSTITUTIONS #####

CV <- lmerTest::lmer(cong_LH_all ~ compulsory_dummy + (1|election), data = cses_leg)

DEMO <- lmerTest::lmer(cong_LH_all ~ regime_age + (1|election), data = cses_leg)


FH <- lmerTest::lmer(cong_LH_all ~ freedom_house_1 + (1|election), data = cses_leg)

POL <- lmerTest::lmer(cong_LH_all ~ dalton_pol + (1|election), data = cses_leg) #Polarization 

GALLAGHER <-  lmerTest::lmer(cong_LH_all ~ gallagher + (1|election), data = cses_leg) 

ENEP <- lmerTest::lmer(cong_LH_all ~ ENEP + (1|election), data = cses_leg) 

ABS_GROWTH <- lmerTest::lmer(cong_LH_all ~ abs_growth2 + (1|election), data = cses_leg) #VALENCE - GDP


#FULL MODEL:

# Só converge sem dif_cls:

M1 <- lmerTest::lmer(cong_LH_all ~ cong_closest + 
                       education + knowledge_adj + age + income + rural + female +
                       gallagher + ENEP + freedom_house_1 +  dalton_pol 
                     + compulsory_dummy + abs_growth2   
                     + (1|election), data = cses_leg)

plotcolors <- c("darkblue", "violetred4", "darkgreen", "turquoise4", "black", "purple")

##### GRÁFICO: BI-FULL #####
plot_models(VAL, CLS, EDU, KNW, AGE, INCOME, CITY, GEND,
            GALLAGHER, ENEP, FH, POL, CV, ABS_GROWTH, M1, 
            axis.labels = c("Variação PIB", "Voto compulsório", "Polarização",
                            "Freedom House", 
                            "ENEP", "Gallagher", "Sexo feminino", "Área rural", "Renda", "Idade", 
                            "Conhecimento político", "Escolaridade",
                            "DME", "Incongruência voluntária"), show.legend = FALSE, show.values = TRUE, 
            axis.title = "", ci.lvl =.95, 
            dot.size=1.2, line.size =0.6, spacing = 0.6, colors = "blue",
            value.size = 3.1, axis.lim= c(-1, 1)) 
 
#show.p = FALSE, p.shape = TRUE,


#CLEAN (sem as demográficas):
SC1 <- lmerTest::lmer(cong_LH_all ~ cong_closest + dif_cls_LH_all + 
                       education + knowledge_adj +
                       gallagher + ENEP + freedom_house_1 +  dalton_pol 
                     + compulsory_dummy + abs_growth2   
                     + (1|election), data = cses_leg)


plot_models(S1, SC2, SC4, show.legend = FALSE, show.values = TRUE,  
            axis.title = "", ci.lvl =.95, 
            value.size = 3,
            dot.size=2, line.size =1,
            spacing = 0.5, axis.lim= c(-0.8, 0.8)) + 
  scale_color_sjplot("eight")


##### MÉDIA DOS CIDADÃOS #####

AGE <- lmerTest::lmer(meanv_cong_LH_all~ age + (1|election), data = cses_leg) 
INCOME <- lmerTest::lmer(meanv_cong_LH_all~ income + (1|election), data = cses_leg) 
CITY <- lmerTest::lmer(meanv_cong_LH_all~ rural + (1|election) , data = cses_leg)
GEND <- lmerTest::lmer(meanv_cong_LH_all~ female + (1|election) , data = cses_leg) 

EDU <- lmerTest::lmer(meanv_cong_LH_all~ education + (1|election), data = cses_leg)
KNW <- lmerTest::lmer(meanv_cong_LH_all~ knowledge_adj + (1|election), data = cses_leg)

VTDIF <- lmerTest::lmer(meanv_cong_LH_all~ voter_meanv_dif_LH_all   # DIFFERENCE OF PERCEPTION
                        + (1|election), data = cses_leg)

CLS <- lmerTest::lmer(meanv_cong_LH_all ~ cong_closest_meanv + (1|election), data = cses_leg)  #ELECTORAL SUPPLY
VAL <- lmerTest::lmer(meanv_cong_LH_all ~ dif_cls_LH_all + (1|election), data = cses_leg) #VOLUNTARY INCONGRUENCE



##### INSTITUTIONS #####

CV <- lmerTest::lmer(meanv_cong_LH_all ~ compulsory_dummy + (1|election), data = cses_leg)

DEMO <- lmerTest::lmer(meanv_cong_LH_all ~ regime_age + (1|election), data = cses_leg)

FH <- lmerTest::lmer(meanv_cong_LH_all ~ freedom_house_1 + (1|election), data = cses_leg)

POL <- lmerTest::lmer(meanv_cong_LH_all ~ dalton_pol + (1|election), data = cses_leg) #Polarization 

GALLAGHER <-  lmerTest::lmer(meanv_cong_LH_all ~ gallagher + (1|election), data = cses_leg) 

ENEP <- lmerTest::lmer(meanv_cong_LH_all ~ ENEP + (1|election), data = cses_leg) 

ABS_GROWTH <- lmerTest::lmer(meanv_cong_LH_all ~ abs_growth2 + (1|election), data = cses_leg) #VALENCE - GDP
#FULL MODEL#

MV1 <- lmerTest::lmer(meanv_cong_LH_all ~ voter_meanv_dif_LH_all + cong_closest_meanv + dif_cls_LH_all + 
                        education + knowledge_adj + age + income + rural + female +
                        gallagher + ENEP + freedom_house_1 +  dalton_pol 
                      + compulsory_dummy + abs_growth2   
                      + (1|election), data = cses_leg)


plot_models(MV1, VTDIF, CLS, VAL, EDU, KNW, AGE, INCOME, CITY, GEND,
            GALLAGHER, ENEP, FH, POL, CV, ABS_GROWTH,
            axis.labels = c("Variação PIB", "Voto compulsório", "Polarização",
                            "Freedom House", 
                            "ENEP", "Gallagher", "Sexo feminino", "Área rural", "Renda", "Idade", 
                            "Conhecimento político", "Escolaridade", "Incongruência voluntária",
                            "DME", "Dif. subjetiva/média"),
            show.legend = FALSE, show.values = TRUE, 
            axis.title = "", ci.lvl =.95, 
            dot.size=1.5, line.size =1, spacing = 0.5, colors = "darkgreen",
            value.size = 3, axis.lim= c(-0.5, 0.5))


##### NESTED MODELS ####

EC1 <- lmerTest::lmer(exp_cong_LH_all~ voter_exp_dif_LH_all +
                        freedom_house_1 
                      + (1|election/country), data = cses_leg)

EC2 <- lmerTest::lmer(exp_cong_LH_all~ voter_exp_dif_LH_all + cong_closest_exp +
                        freedom_house_1 
                      + (1|election/country), data = cses_leg)

EC3<- lmerTest::lmer(exp_cong_LH_all~ voter_exp_dif_LH_all +
                       freedom_house_1 + ENEP
                     + (1|election/country), data = cses_leg)


EC4 <- lmerTest::lmer(exp_cong_LH_all~ voter_exp_dif_LH_all + cong_closest_exp +
                        dif_cls_LH_all + (1|election/country), data = cses_leg)
#NÃO CONVERGE, COMO VÁRIOS OUTROS QUE TENTEI COM DIF_CLS, e outras combinações sem ela também. 

M1 <- lmerTest::lmer( exp_cong_LH_all ~ ENEP + freedom_house_1 +  dalton_pol 
                      + compulsory_dummy + abs_growth2   
                      + (1|election/country), data = cses_leg)



plot_models(EC1,EC2,EC3,EC4, M1, show.legend = FALSE, 
            axis.title = "", ci.lvl =.95, 
            dot.size=1.5, line.size =1.2, spacing = 0.4,
            value.size = 2.5, axis.lim= c(-0.5, 0.5))

class(EC1) <- "lmerMod"
class(EC2) <- "lmerMod"
class(EC3) <- "lmerMod"
class(EC4) <- "lmerMod"

stargazer (EC1,EC2,EC3,EC4, covariate.labels= c("DPCE", "DME", "Freedom House", "ENEP", "Incongruência voluntária"))





##### INTERAÇÕES #####

#Interações de ENEP com Diferença Cidadão-Expert, conhecimento político e nível educacional.

INT1 <- lmerTest::lmer(exp_cong_LH_all ~ voter_exp_dif_LH_all + ENEP + ENEP:voter_exp_dif_LH_all 
                      + (1|election), data = cses_leg)

INT1.2<- lmerTest::lmer(exp_cong_LH_all ~ voter_exp_dif_LH_all + cong_closest_exp + dif_cls_LH_all + 
                          education + knowledge_adj + 
                          gallagher + ENEP +  ENEP:voter_exp_dif_LH_all + freedom_house_1 +  dalton_pol 
                        + compulsory_dummy + abs_growth2   
                        + (1|election), data = cses_leg)

INT2 <- lmerTest::lmer(exp_cong_LH_all ~ knowledge_adj + ENEP + ENEP:knowledge_adj 
                       + (1|election), data = cses_leg)

INT2.2 <- lmerTest::lmer(exp_cong_LH_all ~ voter_exp_dif_LH_all + cong_closest_exp + dif_cls_LH_all + 
                           education + knowledge_adj + 
                           gallagher + ENEP +  ENEP:knowledge_adj + freedom_house_1 +  dalton_pol 
                         + compulsory_dummy + abs_growth2   
                         + (1|election), data = cses_leg)


INT3 <- lmerTest::lmer(exp_cong_LH_all ~ education + ENEP + ENEP:education 
                       + (1|election), data = cses_leg)

INT3.2 <- lmerTest::lmer(exp_cong_LH_all ~ voter_exp_dif_LH_all + cong_closest_exp + dif_cls_LH_all + 
                           education + knowledge_adj + 
                           gallagher + ENEP +  ENEP:education + freedom_house_1 +  dalton_pol 
                         + compulsory_dummy + abs_growth2   
                         + (1|election), data = cses_leg)

##### PLOTS #####

# PLOTS COM NÍVEIS ESPECÍFICOS DE ENEP E AS COGNITIVAS NO EIXO X: 
plot_model(INT1, type = "pred", terms = c("voter_exp_dif_LH_all", "ENEP[1.32, 6, 9, 14.1]"), 
           legend.title ="ENEP" ,
           title="") +  labs(
             x = "DPCE",
             y = "congruência")

plot_model(INT1.2, type = "pred", terms = c("voter_exp_dif_LH_all", "ENEP[1.32,6, 9,14.1]"), 
           legend.title ="ENEP" ,
           title="") +  labs(
             x = "DPCE",
             y = "congruência")

plot_model(INT2, type = "pred", terms = c("knowledge_adj", "ENEP[2.16, 6, 9, 14.1]"), 
           legend.title ="ENEP" ,
           title="") +  labs(
             x = "conhecimento",
             y = "congruência")

plot_model(INT2.2, type = "pred", terms = c("knowledge_adj", "ENEP[2.16, 6, 9, 14.1]"), 
           legend.title ="ENEP" ,
           title="") +  labs(
             x = "conhecimento",
             y = "congruência")


# Agora níveis específicos das Cognitivas e ENEP que fica no eixo X: 

plot_model(INT1, type = "pred", terms = c("ENEP", "voter_exp_dif_LH_all"), 
           legend.title ="DPCE" ,
           title="") +  labs(
             x = "ENEP",
             y = "congruência")

plot_model(INT1.2, type = "pred", terms = c("ENEP", "voter_exp_dif_LH_all"), 
           legend.title ="DPCE" ,
           title="") +  labs(
             x = "ENEP",
             y = "congruência")


plot_model(INT2, type = "pred", terms = c("ENEP","knowledge_adj[0,0.5,1]"), 
           legend.title ="conhecimento" ,
           title="") +  labs(
             x = "ENEP",
             y = "congruência")
#ENEP_know 
plot_model(INT2.2, type = "pred", terms = c("ENEP","knowledge_adj[0,0.5,1]"), 
           legend.title ="conhecimento", 
           title="") + labs(
             x = "ENEP",
             y = "congruência") 

class(INT1) <- "lmerMod"
class(INT1.2) <- "lmerMod"
class(INT2) <- "lmerMod"
class(INT2.2) <- "lmerMod"
class(INT3) <- "lmerMod"
class(INT3.2) <- "lmerMod"



#stargazer(INT1, INT1.2, INT2, INT2.2, INT3, INT3.2, covariate.labels=)



##### EFEITO QUADRÁTICO - ENEP #####

ENEP2 <- lmerTest::lmer(exp_cong_LH_all ~ ENEP + I(ENEP^2)+ (1|election), data = cses_leg)
ENEP2.1 <- lmerTest::lmer(exp_cong_LH_all ~ ENEP + I(ENEP^2)+ voter_exp_dif_LH_all + cong_closest_exp + dif_cls_LH_all + 
                          knowledge_adj + 
                          gallagher + freedom_house_1 +  dalton_pol +
                         compulsory_dummy +
                          (1|election), data = cses_leg)

plot_model(ENEP2, type = "pred", terms = "ENEP [all]",line.color="green", title = "") +  labs(
  x = "ENEP",
  y = "Congruência (experts)") 

plot_model(ENEP2.1, type = "pred", terms = "ENEP [all]",line.color="green", title = "") +  labs(
  x = "ENEP",
  y = "Congruência (experts)") 

class(ENEP2) <- "lmerMod"
class(ENEP2.1) <- "lmerMod"

stargazer(ENEP2, ENEP2.1, covariate.labels=c("ENEP", "ENEPsq", "DPCE", "DME", "DDVP",  
                                             "Conhecimento", "Gallagher",
                                             "Freedom House", "Polarização",
                                             "Voto compulsório")) 


##### VARYING SLOPES #####

VS1 <- lmerTest::lmer(exp_cong_LH_all ~ knowledge_adj + (1 + knowledge_adj|country), data = cses_leg)
VS2 <- lmerTest::lmer(exp_cong_LH_all ~ voter_exp_dif_LH_all + (1 + voter_exp_dif_LH_all|country), data = cses_leg)

VS3 <- lmerTest::lmer(exp_cong_LH_all ~ cong_closest_exp + (1 + cong_closest_exp|country), data = cses_leg)

VS4 <- lmerTest::lmer(exp_cong_LH_all ~ dif_cls_LH_all + (1 + dif_cls_LH_all|country), data = cses_leg)


plot_models(VS1, VS2, VS3, VS4,
            show.legend = FALSE, show.values = TRUE,  axis.title = "", ci.lvl =.95, 
            dot.size=2, line.size =1, spacing = 0.5, axis.lim= c(-0.8, 0.8)) + 
  scale_color_sjplot("eight")


plot_model(VS4, type = "re") +
  scale_color_sjplot("eight") 

plot_model(VS2, type = "re") +
  scale_color_sjplot("eight")

plot_model

pm <- plot_model(VS1, type = "re") +
  scale_color_sjplot("eight") 

pm$data$facet <- factor(pm$data$facet)
levels(pm$data$facet) <- c("pais(Intercept)", "Conhecimento político")


pm2 <- plot_model(VS2, type = "re") +
  scale_color_sjplot("eight")

pm3 <- plot_model(VS3, type = "re") +
  scale_color_sjplot("eight")



##### PLOTS - DESCRITIVAS #####



leg_summary <- cses_leg %>% group_by (country, election) %>%
  summarize_all (.funs = c(mean="mean"), na.rm = T)

ggplot(leg_summmary, aes(x = freedom_house_1_mean, y = exp_cong_LH_all_mean)) +
  geom_point(size = 1, color = "#0099f9") +
  geom_text_repel(label = leg_summary$country,  size=2, max.overlaps = Inf) +  labs(
    x = "Nível de democracia",
    y = "Congruência") 

ggplot(leg_summary, aes(x = ENEP_mean, y = exp_cong_LH_all_mean)) +
  geom_point(size = 1, color = "#0099f9") +
  stat_smooth(method = "loess", col = "red")+
  geom_text_repel(label = leg_summmary$election,  size=2, max.overlaps = Inf) +  labs(
    x = "Número Efetivos de Partidos Eleitorais",
    y = "Congruência") 


ggplot(leg_summary, aes(x = cong_LH_all_mean, y = exp_cong_LH_all_mean)) +
  geom_point(size = 2, color = "blue") +
  geom_text_repel(label = leg_summmary$country,  size=2.5, max.overlaps = Inf) +  labs(
    x = "Medida subjetiva",
    y = "Posicionamento por experts") 

