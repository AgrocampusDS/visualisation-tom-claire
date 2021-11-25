library(openxlsx)
library(stringi)
library(sf)
library(viridis)
library(raster)
library(tidyverse)
library(sf)



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
  rename(Dep=1) %>% 
  mutate(Dep = replace(Dep, Dep == 'CORSE SUD', 'CORSE DU SUD')) %>% 
  mutate(Dep = replace(Dep, Dep == 'SEINE SAINT-DENIS', 'SEINE SAINT DENIS'))
  

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
pauv2018 <- pauv2018 %>% 
  mutate(Dep = str_to_upper(
  stri_trans_general(
    gsub('-',' ',Libellé), "Latin-ASCII"))) %>% 
  rename(Pauvrete=Taux.de.pauvreté.2018) %>% 
  slice(-c(97:101))%>% 
  mutate(Annee=rep(2021,length(Dep))) %>% 
  select(Annee, Dep, Pauvrete)


dfbis <- df %>% 
  mutate(Dep = str_to_upper(
    stri_trans_general(
      gsub('-',' ',Département), "Latin-ASCII"))) %>% 
  select(-Département) %>% 
  rename(Abstention=Abstentions_ins) %>% 
  rename(Pauvrete=Taux.de.pauvreté) %>% 
  mutate(Annee=rep(2017,length(Dep))) %>% 
  select(Annee,Dep,Abstention,Pauvrete)

df2 <- df2 %>%
  mutate(Annee=rep(2012,length(Dep))) %>% 
  select(Annee,Dep,Abstention,Pauvrete)

df3 <- rbind(df2,dfbis)  
summary(df3)

df3$Annee <- as.numeric(df3$Annee)
mod <- lm(Abstention~Pauvrete*Annee, data=df3)
anova(mod)

pauv2018$abs_pred <- predict(mod, newdata=pauv2018)

# µ <- mod$coefficients[1]
# beta <- mod$coefficients[2]
# alpha2 <- mod$coefficients[3]
# gamma3 <- mod$coefficients[3]*2
  
plot4 <- ggplot() +
  
  geom_smooth(data=df2, aes(x=Pauvrete, y=Abstention, col='2012'),
              method='lm', se=F) +
  
  geom_smooth(data=df, aes(x=Taux.de.pauvreté, y=Abstentions_ins, col='2017'), 
              method='lm', se=F) +
  geom_point(data=df, aes(x=Taux.de.pauvreté, y=Abstentions_ins, col='2017'))+
  
  geom_smooth(data=pauv2018, aes(x=Pauvrete, y=abs_pred, col='2022'),
              method='lm', se=F) +
  geom_point(data=pauv2018, aes(x=Pauvrete, y=abs_pred, col='2022')) +
  scale_color_manual(values=list('2012'='darkblue','2017'='darkorange',
                                 '2022'='darkred')) +
  
  labs(x='Taux de pauvreté',
       y="Taux d\'abstention aux élections",
       col='Année',
       title='Comparaison des relations taux de pauvreté - taux 
       d\'abstention entre 2012 et 2017') +
  
  theme_light()
  
plot4

#Carte

contour <- st_read(dsn = 'In/contour-des-departements.geojson') %>% 
  mutate(Dep = str_to_upper(
  stri_trans_general(
    gsub('-',' ',nom), "Latin-ASCII"))) %>% 
  select(-nom)

tmp <- inner_join(pauv2018, dfbis, by=c('Dep'='Dep')) %>% 
  mutate(DiffAbs = abs_pred-Abstention) %>% 
  mutate(AbsCat = case_when(DiffAbs < 0 ~ "diminution", 
                              DiffAbs > 2.5 ~ "augmentation",
                              TRUE ~ "maintien ou faible augmentation")) %>% 
  select(Dep,DiffAbs, AbsCat)

df5 <- inner_join(contour, tmp, by=c('Dep'='Dep')) 


plot5 <- df5 %>% 
  ggplot() +
  geom_sf(aes(fill= AbsCat)) +
  scale_fill_manual(values=list("augmentation" = "indianred2", 
                                "maintien ou faible augmentation" = "gray88", 
                                "diminution" = "darkseagreen2" ))+
  labs(fill = "Évolution abstention")+
  theme_classic() + 
  theme(axis.line=element_blank(),axis.text.x=element_blank(),
        axis.text.y=element_blank(),axis.ticks=element_blank())+
  ggtitle("Evolution de l'abstention aux élections présidentielles entre 2017 et 2022")+
  theme(plot.title=element_text( hjust=0.5, vjust=0.5, face='bold'))


plot5




