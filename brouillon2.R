library(openxlsx)
library(stringi)
library(gridExtra)
library(sf)
library(viridis)
library(raster)
library(cowplot)

library(tidyverse)


### 2017
abs <- read.csv('In/Presidentielle_2017_Resultats_Département_T1_clean.csv', 
                encoding = 'UTF-8')


abs <- abs %>% select(Département, Abstentions_ins)
abs <- abs[-(97:106),]

# https://www.insee.fr/fr/statistiques/4797606?sommaire=4928952#tableau-figure3
pauv <- read.xlsx('In/pauvrete.xlsx')
pauv <- pauv[-(97:101),]

df <- inner_join(pauv, abs, by=c('Département'='Département'))


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

df2 <- df2 %>% mutate(Dep = replace(Dep, Dep == 'HAUTE CORSE', 'Haute-Corse'))

plot3 <- ggplot() +
  
  geom_point(data=df2, aes(x=Pauvrete, y=Abstention, col='2012')) +
  geom_smooth(data=df2, aes(x=Pauvrete, y=Abstention, col='2012'),
              method='lm', se=F) +
  
  geom_text(data=df2 %>% filter(Abstention>25),
            # nudge_x=-2.3, 
            size=3.5,
            aes(x=Pauvrete, y=Abstention, label=Dep, col='2012')) +
  
  
  geom_point(data=df, aes(x=Taux.de.pauvreté, y=Abstentions_ins, col='2017')) +
  geom_smooth(data=df, aes(x=Taux.de.pauvreté, y=Abstentions_ins, col='2017'), 
              method='lm', se=F) +
  
  geom_text(data=df %>% filter(Abstentions_ins>25, Taux.de.pauvreté<25), 
            # nudge_x=-2.5, 
            size=3.5,
            aes(x=Taux.de.pauvreté, y=Abstentions_ins, 
                label=Département, col='2017')) + 
  
  geom_text(data=df %>% filter(Abstentions_ins>25, Taux.de.pauvreté>25), 
            # nudge_x=-3.3,
            size=3.5,
            aes(x=Taux.de.pauvreté, y=Abstentions_ins, 
                label=Département, col='2017')) + 
  
  
  scale_color_manual(values=list('2012'='darkblue','2017'='darkorange')) +
  labs(x='Taux de pauvreté',
       y="Taux d\'abstention aux élections",
       col='Année',
       title='Comparaison des relations taux de pauvreté - taux 
       d\'abstention entre 2012 et 2017') +
  
  theme_light()

plot3
  

## 3EME 

pauv2018 <- read.csv('In/data_pauv_2018.csv', sep=';', encoding='UTF-8', skip=2)

dfbis <- df %>% 
  mutate(Dep = str_to_upper(
    stri_trans_general(
      gsub('-',' ',Département), "Latin-ASCII"))) %>% 
  select(-Département)



df3 <- inner_join(dfbis, df2, by=c('Dep'='Dep'))

mod <- anova(data=df2)


