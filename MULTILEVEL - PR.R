
rm(list=ls()[!(ls() %in% c("cses_exp", "cses_pr"))])



##### PACKAGES #####
library(mlmRev)
library(stargazer)
library(sjPlot)
library(tidyverse)
library(ggplot2)


load("cses_pr.Rdata")


##### EXPERT CONGRUENCE #####

#SUBSET 

expert_drop <- c("BLR_2001", "KGZ_2005", "LTU_1997", "PER_2001", "PHL_2004", "ROU_1996", "RUS_2000", "RUS_2004",
                    "TWN_2012")

'%ni%' <- Negate('%in%')

cses_exp <- cses_pr %>% filter (election %ni% expert_drop)

#, "PHL_2016" - vers�o com e sem Grace Poe para expert, sem expert nenhuma delas! 
#"TWN_2012" - N�O TEM EXPERT. 
#"CHL_1999", USA_1996 - N�O TEM CITIZEN!
#PER_2001 - N�O TEM EXPERT!

# REGRESS�O 1 LEVEL - COMPLETE POOLING # 
EDU1 <- glm(exp_cong_PR_1~ education, data = cses_exp)

KNW1 <- glm(exp_cong_PR_1~ knowledge_adj, data = cses_exp)

VTDIF1 <- glm(exp_cong_PR_1~ voter_exp_dif_PR_1
                        , data = cses_exp)
AGE1 <- glm(exp_cong_PR_1~ age, data = cses_exp) 
AGESQ1 <- glm(exp_cong_PR_1~ age + I(age^2) , data = cses_exp) 
INCOME1 <- glm(exp_cong_PR_1~ income , data = cses_exp) 
CITY1 <- glm(exp_cong_PR_1~ rural , data = cses_exp)
GEND1 <- glm(exp_cong_PR_1~ female , data = cses_exp) 
CLS1 <- glm(exp_cong_PR_1 ~ cong_closest, data = cses_exp) # LIGADA  A INSTITUI��ES MAS � N�VEL MICRO!

# STARGAZE - , AP�NDICE - coeficientes similares, um pouco maiores que multilevel 


##### MULTILEVEL - BIVARIATE MODELS #####

EDU <- lmerTest::lmer(exp_cong_PR_1~ education + (1|election), data = cses_exp)
KNW <- lmerTest::lmer(exp_cong_PR_1~ knowledge_adj + (1|election), data = cses_exp)
VTDIF <- lmerTest::lmer(exp_cong_PR_1~ voter_exp_dif_PR_1 + (1|election), data = cses_exp)
CLS <- lmerTest::lmer(exp_cong_PR_1 ~ cong_closest_exp + (1|election), data = cses_exp) 

# OUTRAS DEMOGR�FICAS
AGE <- lmerTest::lmer(exp_cong_PR_1~ age + (1|election), data = cses_exp) # AGE DECREASES CONGRUENCE? n�O SIGNIFICANTE!
AGESQ <- lmerTest::lmer(exp_cong_PR_1~ age + I(age^2) + (1|election), data = cses_exp) # NADA DE RESULTADO 
INCOME <- lmerTest::lmer(exp_cong_PR_1~ income + (1|election), data = cses_exp) # um pouco mais 
CITY <- lmerTest::lmer(exp_cong_PR_1~ rural + (1|election), data = cses_exp)
GEND <- lmerTest::lmer(exp_cong_PR_1~ female + (1|election), data = cses_exp) 

#INSTITUI��ES 

CV <- lmerTest::lmer(exp_cong_PR_1 ~ compulsory_dummy + (1|election), data = cses_exp) #COMPULSORY
CAND <- lmerTest::lmer(exp_cong_PR_1 ~ enpres + (1|election), data = cses_exp)
PR2 <- lmerTest::lmer(exp_cong_PR_1 ~ round2_PR + (1|election), data = cses_exp)
FH <- lmerTest::lmer(exp_cong_PR_1 ~ freedom_house_1 + (1|election), data = cses_exp) #FREEDOM HOUSE 

#Duas grandes dimens�es:
FHCIV <- lmerTest::lmer(exp_cong_PR_1 ~ fh_civil + (1|election), data = cses_exp)
FHPOL <- lmerTest::lmer(exp_cong_PR_1 ~ fh_pol + (1|election), data = cses_exp)

#Freedom House + Polity: 
POL_FH <- lmerTest::lmer(exp_cong_PR_1 ~ fh_polity + (1|election), data = cses_exp) 

#POLARIZATION - mas acho que os dados s�o em elei��o legislativa
POL <- lmerTest::lmer(exp_cong_PR_1 ~ dalton_pol + (1|election), data = cses_exp)

## VALENCE ##

#GDP_1:GDP_3, cum_gdp,	abs_growth,	cum_gdp2,	abs_growth2, # ECONOMY
VOLUNT <- lmerTest::lmer(exp_cong_PR_1 ~ dif_cls_PR_1 + (1|election), data = cses_exp)
ABS_GROWTH <- lmerTest::lmer(exp_cong_PR_1 ~ abs_growth2 + (1|election), data = cses_exp)

##### PLOT - Bivariadas #####
plot_models(M1,  
            axis.labels = c("Sexo feminino", "Idade", "Rural", "Renda", 
                            "Voto compuls�rio", "Freedom House",
                            "Varia��o do PIB", "DDVP", "N� de candidatos", 
                            "DME", "DPCE", "Conhecimento", "Educa��o"),
            show.legend = TRUE,  
            show.values = TRUE, axis.title = "",value.size = 3.5, 
            ci.lvl =.95, dot.size=1.5, 
            line.size =0.7, spacing = .3, colors = "blue", axis.lim= c(-1, 2), p.shape = TRUE) 


##### PR_SYSTEM #####

#system_pr tem 4 categorias: plurality (Mexico, Philippines e Taiwan), abs. majority (segundo turno tradicional), qualified majority (s� Arg aqui),
#e electoral college (EUA)

SYS <- lmerTest::lmer(exp_cong_PR_1 ~ plurality + (1|election), data = cses_exp)

plur_maj <- cses_exp %>% filter (country != "United States of America") # Tirando EUA
SYS2 <- lmerTest::lmer(exp_cong_PR_1 ~ plurality + (1|election), data = plur_maj) # MODELO SEM EUA
SYS_3 <- lmerTest::lmer(exp_cong_PR_1 ~ plurality + elec_college + (1|election), data = cses_exp)
SYS_full <- lmerTest::lmer(exp_cong_PR_1 ~ maj + quali_maj + elec_college + (1|election), data = cses_exp) 


round_2 <- lmerTest::lmer(exp_cong_PR_1 ~ round2_PR + (1|election), data = cses_exp)

#Todas vers�es do full s�o parecidas, essa fez mais sentido porque todos tem coeficiente na mesma dire��o (sugerindo que
#plurality de fato � pior para congru�ncia)


 

plot_models( SYS, SYS2, SYS_3, SYS_full, round_2, axis.labels = c("2 turnos", "Maioria qualificada",
                                "Maioria absoluta", "Col�gio eleitoral"),
  show.legend = TRUE, show.values = TRUE,  axis.title = "", ci.lvl =.99, 
  dot.size=2, line.size =1, spacing = 0.5) +
  scale_color_brewer(palette = "Set1", labels = c( "2 turnos (bivariada)", "Todos sistemas",
                                                   "Col�gio eleitoral/pluralidade", "Pluralidade(com EUA)",
                                                   "Pluralidade(sem EUA)"))

#TODOS MODELOS COM IC MUITO GRANDE, POUCO CASO, MENCIONAR S� BREVEMENTE O FULL MODEL TOTAL? SIM, 
#INCLUIR ELE PORQUE SE TEM OUTRAS VAR TEM QUE TER ESSAS. A� FAZER PLOT, TABELA E REESCREVER, BIC, ETC.


##### FULL MODEL #####

M1 <- lmerTest::lmer( exp_cong_PR_1 ~ education + knowledge_adj + voter_exp_dif_PR_1
                      +cong_closest_exp + enpres +  dif_cls_PR_1 +
                        abs_growth2 + freedom_house_1 + compulsory_dummy +
                        income + rural + age + female 
                      + (1|election), data = cspr_nomiss) # Tanto faz cses_exp ou o sem missing, 
# o no missing eu tirei os missings de todas vari�veis deste modelo, ser�o automaticamente retiradas. 
#Mas os df precisam ter o mesmo nome para Anova. 

M1.2 <- lmerTest::lmer( exp_cong_PR_1 ~ education + knowledge_adj + voter_exp_dif_PR_1
                      +cong_closest_exp + enpres + maj + quali_maj + elec_college + 
                        + round2_PR + dif_cls_PR_1 +
                        abs_growth2 + freedom_house_1 + compulsory_dummy +
                        income + rural + age + female 
                      + (1|election), data = cspr_nomiss)

# axis.labels = c("Sexo feminino", "Idade", "Rural", "Renda", 
# "Voto compuls�rio", "Freedom House",
# "Varia��o do PIB", "DDVP", "N� de candidatos", 
# "DME", "DPCE", "Conhecimento", "Educa��o"),

plot_models(M1, M1.2, show.legend = FALSE,  
            show.values = TRUE, axis.title = "",value.size = 4, 
            ci.lvl =.95, dot.size=2, 
            line.size =1, spacing = 0.6, colors = "blue", axis.lim= c(-1, 1)) 


##### AJUSTANDO MODELOS #####

# PRIMEIRO VAMOS TIRAR MISSINGS DE TODAS VARI�VEIS DE INTERESSE:

cspr_nomiss<- cses_pr %>% drop_na(exp_cong_PR_1, education, knowledge_adj, voter_exp_dif_PR_1,
                                 cong_closest_exp, enpres, dif_cls_PR_1,
                                    abs_growth2, freedom_house_1, compulsory_dummy,
                                    income, rural, age, female)


##### REDUCED MODELS #####

# VARI�VEIS DE INTERESSE  - TIREI ABS GROWTH QUE N�O � MEDIDA T�O PRECISA, 
#RURAL E IDADE QUE N�O TEM MUITO INTERESSE NEM EFEITO
M2 <- lmerTest::lmer( exp_cong_PR_1 ~ knowledge_adj + voter_exp_dif_PR_1   
                      +cong_closest_exp + enpres +  dif_cls_PR_1 +
                        freedom_house_1 + compulsory_dummy +
                        income + female 
                      + (1|election), data = cspr_nomiss)

# MODELO AINDA MAIS REDUZIDO -  VARI�VEIS DE MUITO INTERESSE E SIGNIFIC�NCIA (ALTERNAR )
M3 <- lmerTest::lmer( exp_cong_PR_1 ~ knowledge_adj + voter_exp_dif_PR_1   
                      +cong_closest_exp + enpres +  dif_cls_PR_1 +
                        freedom_house_1  
                      + (1|election), data = cspr_nomiss)


plot_models(M1, M2, M3, axis.labels = c("Sexo feminino", "Idade", "Rural", "Renda", 
                                "Voto compuls�rio", "Freedom House",
                                "Varia��o do PIB", "DDVP", "N� de candidatos", 
                                "DME", "DPCE", "Conhecimento", "Educa��o"),
            show.legend = TRUE, legend.title = "Modelos",
            show.values = FALSE, axis.title = "",value.size = 3, 
            ci.lvl =.95, dot.size=2, p.shape = TRUE, show.p = FALSE,
            line.size =1, spacing = 1, axis.lim= c(-1, 1))  +  scale_color_brewer(palette = "Set1",   #Forma mais f�cil de manter 3 cores padr�o
   labels = c( "3. Fundamental", "2. Intermedi�rio","1. Completo"))

class(M1) <- "lmerMod"
class(M2) <- "lmerMod"
class(M3) <- "lmerMod"

stargazer(M1, M2, M3,  covariate.labels= c("Educa��o", "Conhecimento", "DPCE", "DME", "N� de candidatos", 
                                           "DDVP", "Varia��o do PIB", "Freedom House", "Voto compuls�rio", 
                                           "Renda", "Rural", "Idade", "Sexo feminino"))

 
#DEPOIS:
anova(M1, M2, M3)


plot_model(M2, type = "diag", show.values = TRUE)

##### VARYING SLOPE/ COUNTRY ##### 

M3.1 <- lmerTest::lmer( exp_cong_PR_1 ~ knowledge_adj + voter_exp_dif_PR_1   
                        +cong_closest_exp + enpres +  dif_cls_PR_1 +
                          freedom_house_1  
                        + (1|election/country), data = cspr_nomiss)

M3.2 <- lmerTest::lmer( exp_cong_PR_1 ~ knowledge_adj + voter_exp_dif_PR_1   
                        +cong_closest_exp + enpres +  dif_cls_PR_1 +
                          freedom_house_1  
                        + (1+cong_closest_exp|election), data = cspr_nomiss) #PARECE O MELHOR PELO ANOVA

M3.2 <- lmerTest::lmer( exp_cong_PR_1 ~ knowledge_adj + voter_exp_dif_PR_1   
                        +cong_closest_exp + enpres +  dif_cls_PR_1 +
                          freedom_house_1  
                        + (1+cong_closest_exp|election), data = cspr_nomiss) #PARECE O MELHOR PELO ANOVA

M3.3 <- lmerTest::lmer( exp_cong_PR_1 ~ knowledge_adj + voter_exp_dif_PR_1   
                        +cong_closest_exp + enpres +  dif_cls_PR_1 +
                          freedom_house_1  
                        + (cong_closest_exp|election) + (knowledge_adj|election), data = cspr_nomiss) 


##### CONGRU�NCIA SUBJETIVA #####





##### SOBRAS #####


# gr�ficos de "random effect (re) # ? http://www.strengejacke.de/sjPlot/reference/plot_model.html


plot_model(SES, type = "pred", terms = "income", "education")



coef(mod1)$Country



#Expert e subjetiva muito semelhantes na distribui��o por pa�s. Voter exp tbm. Ent�o, por 
#outro lado, isso sugere que n�o h� tanto problema de measurement de ideologia (E principalmente
#de congru�ncia) - junto com o caso de PELA ter tbm distribui��es bem parecidas por pa�s
#para LR e issues. 





# PARA PLOTAR UM MODELO DE VARYING SLOPE COM FACET WRAP (1 slope para cada elei��o), tentei v�rias coisas
#Parece que naquela vers�o antiga do sjPlot tinha, sjp.lmer (type=ri.slope) mas na atual n�o achei. Tentei esse 
#abaixo n�o deu certo, algo eu n�o adaptei direito:

ggplot(cses_exp, aes(x = knowledge_adj, y = exp_cong_PR_1)) +
  facet_wrap(~election, nrow=2) +
  geom_point(alpha = 0.5) +
  theme_classic() +
  geom_line(data = cbind(cses_exp, pred = predict(VS1)), aes(y = pred), size = 1) 

#https://ourcodingclub.github.io/tutorials/mixed-models/#ranslopes

#D� ERRO

# A� eu tentei esse aqui, desse site:
#https://yury-zablotski.netlify.app/post/mixed-models/

#fit the model
m_slp <- lmer(Reaction ~ Days + (Days | Subject), sleepstudy)

#the next line put all the estimated intercept and slope per subject into a dataframe
VS_plot <- as.data.frame(t(apply(ranef(VS1)$election, 1,function(x) fixef(VS1) + x)))

#to get the predicted regression lines we need one further step, writing the linear equation: Intercept + Slope*Days with different coefficient for each subject
pred_VS <- melt(apply(VS_plot,1,function(x) x[1] + x[2]*0:9), value.name = "exp_cong_PR_1")

#some re-formatting for the plot
names(pred_VS)[1:2] <- c("knowledge_adj","election")
pred_VS$knowledge_adj <- pred_VS$knowledge_adj -1
pred_VS$election <- as.factor(pred_VS$election)

#plot with actual data 
ggplot(pred_VS,aes(x=knowledge_adj,y=exp_cong_PR_1,color=election))+
  geom_line()+
  geom_point(data=cses_exp,aes(x=knowledge_adj,y=exp_cong_PR_1))+
  facet_wrap(~election,nrow=3) 


#DEU CERTO, MAS FICA NUMA ESCALA MUITO RUIM, A� ADICIONEI O COORD_CARTESIAN, TESTEI COM -10 A 0 
#(QUE � O RANGE DOS MEUS DADOS DESSAS VARI�VEIS), A� FICA SEM A CURVA, S� COM OS DADOS (parece que 
#a curva precisa come�ar no positivo). Eu tinha tentado rodar sem a parte do c�digo ali que subtrai 1
#da vari�vel independente. Seja como for, acho que eles adaptaram para as vari�veis deles, 
#preciso adaptar para as minhas. Tive o mesmo problema com knowledge_adj, acho que preciso ajustar a dependente
#para ficar positiva. PERGUNTAR NO STACK! 



M1 <- lmerTest::lmer( exp_cong_PR_1 ~ education + knowledge_adj + voter_exp_dif_PR_1
                      + age + income + rural 
                      + cong_closest_exp + + enpres + ENEP +
                        freedom_house_1  + regime_age + 
                        round2_PR + plurality +
                        abs_growth2 + dif_cls_PR_1 + compulsory_dummy 
                      + (1|election), data = cses_exp)

### varying slope para cognitivas, e talvez alguma outra? Closest n�o sei se faria sentido, 
#por que em um pa�s closest teria MAIS IMPACTO do que em outro? 
#Deve ter rela��o entre closest para VOTERS e compulsory vote - quem � obrigado a votar tem mais chance de votar mesmo
#n�o tendo candidatos pr�ximos. Regress�o com turnout tamb�m para ver isso, closest como VI?






regime <- lmerTest::lmer(exp_cong_PR_1 ~ regime_age + (1|election), data = cses_exp) #REGIME AGE
CV <- lmerTest::lmer(exp_cong_PR_1 ~ compulsory + (1|election), data = cses_exp) #COMPULSORY (s� fiz com a dummy)



# com knowledge e principalmente cit_exp_dif como VD. 
IC1 <- lmerTest::lmer(voter_exp_dif_PR_1 ~ education + knowledge_adj + (1|election), data = cses_pr)

IC2 <- lmerTest::lmer(knowledge_adj ~ education + (1|election), data = cses_pr)

#Prefer�ncia por vari�veis anteriores, mas se elas explicarem
#muito menos que as outras deixar as outras nos modelos principais
#(como controle, em modelos mais complexos, etc.) 


#Regredir closest em enpres
IE <- lmerTest::lmer(cong_closest ~ ENEP + (1|election), data = cses_pr)


##### QUALIDADE DA DEMOCRACIA - ver se tem dados suficientes para essas espec�ficas
  
# Mais espec�ficas (dentro de political)
  
FHEL <- lmerTest::lmer(exp_cong_PR_1 ~ fh_elec + (1|election), data = cses_exp)
FHPL <- lmerTest::lmer(exp_cong_PR_1 ~ fh_plural + (1|election), data = cses_exp) #Political Pluralism and Participation
  
# Outras espec�ficas:
  
FHEXP <- lmerTest::lmer(exp_cong_PR_1 ~ fh_expr + (1|election), data = cses_exp)  #Freedom of expression and beliefs
FH_ASS <- lmerTest::lmer(exp_cong_PR_1 ~ fh_aor + (1|election), data = cses_exp) #Freedom of association
  
FH_ROL <- lmerTest::lmer(exp_cong_PR_1 ~ fh_rol + (1|election), data = cses_exp) # Rule of Law
FH_FOG <- lmerTest::lmer(exp_cong_PR_1 ~ fh_fog + (1|election), data = cses_exp)  # Functioning of Government
  
# Dataset Freedom on the Net: Limits on content 
FNET <- lmerTest::lmer(exp_cong_PR_1 ~ freedom_net + (1|election), data = cses_exp)   

plot_models(FH,FHCIV, FHPOL, FHEL, FHPL, FHEXP, FH_ASS, FH_ROL, FH_FOG, FNET, POL_FH,
            axis.labels = c("Polity + Freedom House", "Freedom on the Net: limita��o de conte�do", 
                            "Funcionamento do Governo", "Rule-of-law", "Direito de associa��o",
                            "Liberdade de express�o", "Pluralismo pol�tico e participa��o",
                            "Processo eleitoral", "Liberdades Pol�ticas", "Liberdades Civis",
                            "Freedom House"),
            show.legend = FALSE, show.values = TRUE,  axis.title = "", ci.lvl =.95, 
            dot.size=2, line.size =1, spacing = 0.5, axis.lim= c(-0.8, 0.8)) + 
  scale_color_sjplot("eight")
  
###Cognitive Intera��o com compulsory 


# MODEL PLOTS

#Labels ficam em ordem INVERSA dos coeficientes (o primeiro que voc� coloca ficar� acima de tudo), 
#por isso as legendas precisam ficar invertidas para gerar o gr�fico correto. 
#PARA CORRIGIR PRECISARIA INSERIR O + guides(color=guide_legend(reverse = TRUE)) mas n�o deu certo comigo. 

# OUTRAS MELHORIAS - , diminuir dist�ncia entre eles, quadr�tica - COMO PLOTAR O "'PRED"?




#verificar "estrelinhas", coef, etc.

#Come�ar modelos mais complexos - inserindo vari�veis de controle de outras dimens�es (prefer�ncia pelas que "Explicam mais")
#ou pelas que parecem fazer mais sentido para a quest�o. Testar alguns modelos mais pr�ximos do "super-full"



