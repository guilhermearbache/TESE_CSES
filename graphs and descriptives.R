# TIRAR ELEI��ES SEM DADOS

elections_drop <- c("BLR_2001", "KGZ_2005", "LTU_1997", "ROU_1996", "RUS_2000", "RUS_2004")

'%ni%' <- Negate('%in%')

prplot <- cses_pr %>% filter (election %ni% elections_drop)

#"CHL_1999", "PHL_2004", "PHL_2016"
#"TWN_2012" - N�O TEM EXPERT. USA_1996 - N�O TEM CITIZEN!



# CRIAR M�DIA CITIZEN, EXPERT 

prplot <- prplot %>% mutate(mean_ideol = rowMeans(select(., ideolparty_A:ideolparty_I), na.rm = T)) 
prplot <- prplot %>% mutate(mean_exp = rowMeans(select(., ex_ideolparty_A:ex_ideolparty_I), na.rm = T)) 


###### ALGUMAS OP��ES PARA SEPARAR O DATASET EM GRUPOS #####

target <- table(prplot$election)
target<- as.data.frame(target)
target <- as.vector(target$Var1)

target1 <- target[1:10]
target2<- target[11:20]
target3<- target[21:30]
target3<- target[31:37]


cses1 <- prplot %>% filter (election %in% target1) %>% select (election, contains("ideol"), contains("closest"),
                                                               education, knowledge_adj, abs_growth2, ENEP,
                                                               contains("rile"), system_PR,
                                                               compulsory)


##### TABLECONTINUOUS #####

# tableContinuous(vars = list($conc, CO2$uptake, rnorm(1111), runif(2222)), 
#                 nams = c("conc", "uptake", "random1", "random2"), disp.cols = 
#                   c("n", "min", "median", "max", "iqr", "na"), cap = "Table of continuous variables.", lab = 
#                   "tab: descr stat") 

# tableContinuous(vars, weights = NA, subset = NA, group = NA, 
#                 stats = c("n", "min", "q1", "median", "mean", "q3", "max", 
#                           "s", "iqr", "na"), prec = 1, col.tit = NA,
#                 col.tit.font = c("bf", "", "sf", "it", "rm"), print.pval = 
#                   c("none", "anova", "kruskal"), pval.bound = 10^-4, 
#                 declare.zero = 10^-10, cap = "", lab = "", 
#                 font.size = "footnotesize", longtable = TRUE, 
#                 disp.cols = NA, nams = NA, ...)


#### O �NICO PROBLEMA � QUE SE FOR MUITO EXTENSA MESMO A� ESTOURA O ESPA�O DO CONSOLE E N�O D� PARA COPIAR TUDO. E ELA FICA UM POUCO EXTENSA DEMAIS
#PORQUE AS COLUNAS S�O S� PARA AS DIFERENTES STATS, ENT�O UMA LINHA PARA CADA ELECTION (GROUP VARIABLE) E UMA LINHA PARA CADA VARI�VEL

# ESSA TABELA S� PODE SER COM VARI�VEIS NUM�RICAS. Al�m disso preciso tirar UNDERLINE PARA N�O DAR PROBLEMA NO LaTeX:

cses_num <- cses_exp
cses_num$election <- gsub("_", "", cses_num$election)

group <- cses_num[, "election"] #CRIANDO O VETOR DE "GROUP" ANTES DE SELECIONAR S� AS NUM�RICAS PORQUE ESSA VARI�VEL VAI SUMIR

cses_num <- cses_num %>% select(where(is.numeric))

names(cses_num) <- gsub("_", "", names(cses_num)) #Tirando os underlines dos NOMES das vari�veis


cses_num1 <- cses_num %>% select (starts_with("ideol"))

tableContinuous(vars = cses_num1, 
                disp.cols = c("min", "median", "max", "na", "iqr", "s"), prec = 2, 
                group = group, cap = "Estat�sticas descritivas", lab = 
                  "tab: descr stat")


cses_demo <-  cses_num %>% select (age, income, gender, education)

tableContinuous(vars = cses_demo, 
                disp.cols = c("min", "median", "max", "na", "iqr", "s"), prec = 2, 
                group = group, cap = "Estat�sticas descritivas - vari�veis demogr�ficas", lab = 
                  "tab: descr stat")

##### PARA MELHORAR AS TABELAS #####

# MELHORAR NOME DOS PA�SES/ELEI��O (talvez mudar o texto para "Argentina - 2015", etc. a� n�o tem underscore)

# Melhorar alguns labels das colunas (das stats) - op��o "col.tit = "

#Nomes das vari�veis - op��o "nams" 

# Selecionar mais vari�veis no banco total 

# https://rdrr.io/cran/reporttools/man/tableContinuous.html

 ##### GR�FICOS #####
 
 #CRIAR DATASET LONG COM TODAS QUE PRETENDO USAR, DEPOIS IR TIRANDO. 
 
 df1 <- prplot %>% select (election, ideol_self, ideol_elected_PR_1) %>%
  pivot_longer(cols = !election,
  names_to = "variable",
   values_to = "valor")
 

 p <- ggplot(data=df1, aes(x=valor, group=variable, fill=variable)) +
   geom_density(adjust=1.5, alpha=.4) +
   facet_wrap(~election) +
   theme_ipsum()
 p