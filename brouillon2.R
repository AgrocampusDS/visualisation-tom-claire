library(openxlsx)
library(tidyverse)
library(stringi)
library(gridExtra)

### 2017
abs <- read.csv('In/Presidentielle_2017_Resultats_Département_T1_clean.csv', 
                encoding = 'UTF-8')


abs <- abs %>% select(Département, Abstentions_ins)
abs <- abs[-(97:106),]

# https://www.insee.fr/fr/statistiques/4797606?sommaire=4928952#tableau-figure3
pauv <- read.xlsx('In/pauvrete.xlsx')
pauv <- pauv[-(97:101),]

df <- inner_join(pauv, abs, by=c('Département'='Département'))

mod <- lm(data=df, Abstentions_ins~Taux.de.pauvreté)
summary(mod)

plot <- ggplot(data=df, aes(x=Taux.de.pauvreté, y=Abstentions_ins)) +
  geom_point() +
  geom_smooth(method='lm') +
  xlim(c(10,23)) +
  ylim(c(10,35)) +
  labs(x='Taux de pauvreté en 2017',
       y="Taux d\'absentention aux élections de 2017")
plot


## 2012
# https://www.insee.fr/fr/statistiques/1895078

abs2 <- read.csv('In/France-politique-2012.csv', encoding='UTF-8', sep=';')
abs2 <- abs2[-(97:107),]

abs2 <- abs2 %>%
  mutate(Abstention = 100-Participation) %>% 
  select(1, Abstention) %>% 
  rename(Dep=1)

pauv2 <- read.xlsx('In/base-cc-filosofi-12.xlsx', sheet=4, startRow = 6)

pauv2 <- pauv2 %>% 
  select(2,TP6012) %>% 
  rename(Dep=LIBGEO) %>% 
  rename(Pauvrete=TP6012) %>% 
  mutate(Dep_formatted = str_to_upper(
    stri_trans_general(
      gsub('-',' ',Dep), "Latin-ASCII"))) %>% 
  select(-Dep)


df2 <- inner_join(abs2, pauv2, by=c('Dep'='Dep_formatted'))

mod2 <- lm(data=df2, Abstention~Pauvrete)
summary(mod2)

plot2 <- ggplot(data=df2, aes(x=Pauvrete, y=Abstention)) +
  geom_point() +
  geom_smooth(method='lm') +
  xlim(c(10,23)) +
  ylim(c(10,35)) +
  labs(x='Taux de pauvreté en 2012',
       y="Taux d\'absentention aux élections de 2012")
plot2


grid.arrange(plot,plot2, layout_matrix=rbind(c(1,2)))

## 3ème graphe


