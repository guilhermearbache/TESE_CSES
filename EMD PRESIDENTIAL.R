##### PACOTES EXIGIDOS #####

library(tidyverse)
library(plyr); library(dplyr)
library(emdist)

#### SUMMARIZE ####

# CRIAR UM BANCO AGRUPADO POR PAÍS/ELEIÇÃO PARA PODER USAR A IDEOLOGIA DE CADA PARTIDO 
# DE ACORDO COM SEUS VOTOS 

# ***** BOTAR PARTIDO ELEITO, OS QUE FORAM PARA SEGUNDO #####
PR_grouped <- cses_pr %>% group_by(election, country) %>%
  summarize_all (.funs = c(mean="mean"), na.rm = T) %>% 
  select (election, country, contains("ideolparty"), starts_with("pcv_PR"), elected)

## DADOS DE ELECTED (acima, está certo?), e segundo turno? Tem algo lá?


### FAZER CIT-VOTER, VOTER-VOTES, VOTES-ELECTED, DEPOIS VER O QUE FAZER COM 2? TURNO
#CIT-VOTER 2?, VOTER 1? -VOTER 2?; VOTER-VOTES 2?, VOTES 2? - ELECTED



