library(tidyverse)
library(hrbrthemes) # theme ggplot

prplot <- cses_pr %>% filter (election != "KGZ_2005")


ggplot(data=prplot, aes(x=age, group=election, fill=election)) +
  geom_histogram( ) +
   facet_wrap(~country) +
  theme(
    legend.position="none",
    #panel.spacing = unit(0.5, "lines"),
    #axis.ticks.x=element_blank() 
    ) + labs (x="Congru�ncia (Elei��es Presidenciais)")


prplot <- cses_pr %>% filter (election != "KGZ_2005" & election != "LTU_1997")

ggplot(data=cses_exp, aes(x=income)) +
  geom_density(adjust=10, fill="lightblue") +
  facet_wrap(~election) +
  theme(
    legend.position="none",
    panel.spacing = unit(0.5, "lines"),
    axis.ticks.x=element_blank() 
  ) + labs (x="Renda familiar") 


cses_gend <- cses_exp %>% select (election, exp_cong_PR_1, ideol_self, gender) %>% rename (sexo = gender) 

cses_gend <- drop_na (cses_gend, sexo)
  
cses_gend$sexo <- recode(cses_gend$sexo, `1`="masculino", `2` ="feminino")


##### GENDER #####

ggplot(data=cses_gend, aes(x=exp_cong_PR_1, fill = sexo, group = sexo)) +
    geom_density( alpha=.3)   +
  facet_wrap(~election, ncol=4) +
theme(strip.text.x = element_text(size=7.5),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      axis.text.x=element_text(size = 5.5),
      axis.text.y=element_text(size = 5),
      legend.title = element_blank(),
      legend.text = element_text(size = 7),
      panel.spacing = unit(0.3, "lines"),
      legend.position="bottom" 
) + labs (x="Congru�ncia com posicionamento por experts (por g�nero)") 

# +  scale_fill_brewer() pode ser bom tamb�m 
ggplot(data=cses_gend, aes(x=exp_cong_PR_1, fill = sexo, group = sexo)) +
  geom_density( )  +   scale_fill_manual(values=c("#69b3a2", "black")) +
  facet_wrap(~election, ncol=4) +
  theme(strip.text.x = element_text(size=7.5),
        axis.text.x=element_text(size = 5.5),
        axis.text.y=element_text(size = 5),
        legend.title = element_blank(),
        legend.text = element_text(size = 7),
        panel.spacing = unit(0.3, "lines"),
        axis.ticks.x=element_blank() 
  ) + labs (x="Congru�ncia com posicionamento por experts (por g�nero)") 



##### N�O CONSEGUI FAZER FUNCIONAR ESCALA DE COR 
# TENTEI V�RIAS DAQUI https://www.r-graph-gallery.com/ggplot2-color.html
#E posicionei em todos locais poss�veis, n�o tive sucesso 

#legend.position="top" (ou bottom, left, right, none)
#axis ticks aqui � "elections"


ggplot(data=cses_gend, aes(x=ideol_self, fill = sexo, group = sexo)) +
  geom_density(adjust=1.5, alpha=.3) +
  facet_wrap(~election, ncol = 4)  +
  theme_ipsum(labs (x="Distribui��o ideol�gica na escala Esquerda-Direita (por g�nero)"))

  theme(
    panel.spacing = unit(0.5, "lines"),
    axis.ticks.x=element_blank() 
  ) + labs (x="Distribui��o ideol�gica na escala Esquerda-Direita (por g�nero)")




ggplot(data=prplot, aes(x=cong_closest, group=election, color=election)) +
  geom_density(adjust=10) +
  facet_wrap(~country) +
  theme(
    legend.position="none",
    panel.spacing = unit(0.5, "lines"),
    axis.ticks.x=element_blank() 
  ) + labs (x="Dist�ncia ideol�gica m�nima por eleitor")



ggplot(data=prplot, aes(x=cong_closest, color=election)) +
  geom_density(adjust=10) +
  facet_wrap(~country) +
  theme(
    legend.position="none",
    panel.spacing = unit(0.5, "lines"),
    axis.ticks.x=element_blank() 
  ) + labs (x="Dist�ncia ideol�gica m�nima por eleitor")


##### IDEOLOGIA DE TODOS PA�SES-ELEI��ES, TANTO LEGISLATIVOS COMO PRESIDENCIAIS #####
cses_ideol <- cses %>%
  mutate_at(.vars = vars(ideol_self,
                         starts_with("ideolparty"),
                         starts_with("ex_ideol")), 
            .funs = funs(ifelse(. > 90, NA, .))) %>% select (election, country, 
                                                             ideol_self, ideol_self,
                                                             starts_with("ideolparty"),
                                                             starts_with("ex_ideol"))
                                                             

ggplot(data=cses_ideol, aes(x=ideol_self, color=election)) +
  geom_density(adjust=10) +
  facet_wrap(~country) +
  theme(
    legend.position="none",
    panel.spacing = unit(0.5, "lines"),
    axis.ticks.x=element_blank() 
  ) + labs (x="Distribui��o ideol�gica por pa�s/elei��o")




ggplot(df, aes(x=weight, color=sex)) +
  geom_density()
# Add mean lines
p<-ggplot(df, aes(x=weight, color=sex)) +
  geom_density()+
  geom_vline(data=mu, aes(xintercept=grp.mean, color=sex),
            linetype="dashed")
  
 
  # COMPARA��O IDEOLOGIA - VER COMO FAZER UMA LINHA PARA AS M�DIAS GERAIS, DE CADA PARTIDO, ETC. PARA COMPARAR TUDO EM UM 
  #PA�S NO MESMO GR�FICO 
  
   
 ##### COUNTRY-LEVEL #####

# FAZER O SUMMARIZE

pr_summary <- cses_exp %>% group_by(election, country) %>%
summarize_all (.funs = c(mean="mean"), na.rm = T)




#PLOTS COM LABEL PARA CADA ELEI��O PODEM SER INTERESSANTES AQUI:
ggplot(pr_summary, aes(x = freedom_house_1_mean, y = exp_cong_PR_1_mean)) + 
  geom_point(size = 5, color = "#0099f9") +
geom_text_repel(label = pr_summary$election,  size=2.5) +  labs(
  x = "Freedom House",
  y = "N�vel de congru�ncia - Presidencial (experts)"
)

#https://appsilon.com/ggplot-scatter-plots/
  
  
##### INSTITUTIONAL VARIABLES #####
#SYSTEM, REGIME_AGE, ETC. (MAPAS SERIA LEGAL TBM)



##### MAPAS #####


library("rnaturalearth")
library("rnaturalearthdata")
library(rgeos)

world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)

#FAZER MATCH COM CSES PELO NOME DE PA�SES DEPOIS CORRIGIR CASOS QUE N�O DER. VER EM CSV QUAL MELHOR VARI�VEL PARA MATCH
#SUBUNIT, ADMIN, NAME, GEOUNIT, name long. Talvez uma das vari�veis de 3 letras pode funcionar melhor (sov_a3, adm0_a3,gu_a3, etc.)

cses$cname <- substr(cses$election, 1, 3)

cses <- cses %>%
  mutate_at(.vars = vars(contains("ideol")), 
            .funs = list(~ifelse(. > 50, NA, .)))


# FAZER DUMMY DE MISSINGS EM IDEOLOGIA EM CSES PARA COMPARAR PA�SES DEPOIS 


grp_cses <- cses %>% group_by(country, cname) %>%
 summarize_all (.funs = c(mean="mean"), na.rm = T)



world$ideol_self <- grp_cses$ideol_self_mean[match(world$adm0_a3, grp_cses$cname)]

ggplot(data = world) +
  geom_sf() +
  xlab("Longitude") + ylab("Latitude") +
  ggtitle("World map", subtitle = paste0("(", length(unique(world$NAME)), " countries)"))


ggplot(data = world) +
  geom_sf(aes(fill = ideol_self)) +
  scale_fill_viridis_c(option = "plasma", trans = "sqrt")




##### AMERICAS #####

america <- world %>% filter (region_un == "Americas")

## DESCOBRIR COMO FAZER PARA TIRAR O RESTO DO GRID NESSES GR�FICOS , SEN�O N�O ADIANTA MUITO TIRAR O RESTO DO MUNDO

# INSERIR DADOS PELA, LAPOP

ggplot(data = america) +
  geom_sf(aes(fill = ideol_self)) +
  scale_fill_viridis_c(option = "plasma", trans = "sqrt")
