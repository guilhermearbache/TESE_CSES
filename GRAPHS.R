prplot <- cses_pr %>% filter (election != "KGZ_2005")


ggplot(data=prplot, aes(x=cong_PR_1, group=election, fill=election)) +
  geom_density(adjust=10) +
   facet_wrap(~country) +
  theme(
    legend.position="none",
    panel.spacing = unit(0.5, "lines"),
    axis.ticks.x=element_blank() 
    ) + labs (x="Congruência (Eleições Presidenciais)")


prplot <- cses_pr %>% filter (election != "KGZ_2005" & election != "LTU_1997")

ggplot(data=prplot, aes(x=exp_cong_PR_1, group=election, fill=election)) +
  geom_density(adjust=10) +
  facet_wrap(~country) +
  theme(
    legend.position="none",
    panel.spacing = unit(0.5, "lines"),
    axis.ticks.x=element_blank() 
  ) + labs (x="Congruência - Eleições Presidenciais (Expert)


ggplot(data=prplot, aes(x=voter_exp_dif_PR_1, group=election, fill=election)) +
  geom_density(adjust=10) +
  facet_wrap(~country) +
  theme(
    legend.position="none",
    panel.spacing = unit(0.5, "lines"),
    axis.ticks.x=element_blank() 
  ) + labs (x="Congruência Subjetiva vs Experts")




ggplot(data=prplot, aes(x=cong_closest, group=election, color=election)) +
  geom_density(adjust=10) +
  facet_wrap(~country) +
  theme(
    legend.position="none",
    panel.spacing = unit(0.5, "lines"),
    axis.ticks.x=element_blank() 
  ) + labs (x="Distância ideológica mínima por eleitor")



ggplot(data=prplot, aes(x=cong_closest, color=election)) +
  geom_density(adjust=10) +
  facet_wrap(~country) +
  theme(
    legend.position="none",
    panel.spacing = unit(0.5, "lines"),
    axis.ticks.x=element_blank() 
  ) + labs (x="Distância ideológica mínima por eleitor")


##### IDEOLOGIA DE TODOS PAÍSES-ELEIÇÕES, TANTO LEGISLATIVOS COMO PRESIDENCIAIS #####
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
  ) + labs (x="Distribuição ideológica por país/eleição")




ggplot(df, aes(x=weight, color=sex)) +
  geom_density()
# Add mean lines
p<-ggplot(df, aes(x=weight, color=sex)) +
  geom_density()+
  #geom_vline(data=mu, aes(xintercept=grp.mean, color=sex),
   #          linetype="dashed")
  
  
  library(plyr)
mu <- ddply(cses_pr, "election", summarise, grp.mean=mean(exp_cong_PR_1))
head(mu)
