# library
library(sf)
library(tidyverse)
library(tidyr)
library(viridis)
library(ggplot2)
library(raster)
library(cowplot)


###### 2017  #############
ele_17 <- read.csv("In/Presidentielle_2017_Resultats_Département_T1_clean.csv")
ele_17 <- ele_17[-(97:106),]
code_dep <- c(ele_17[,1])
# shapefile

dpt_shape <-  st_read(dsn = 'In/contour-des-departements.geojson')
dpt_shape %>% print(n=10)

dpt_shape %>% 
  ggplot() + geom_sf()

ele_17 <- ele_17 %>%
  rename(code = "CodeDépartement")

dta_map_17 <- dpt_shape %>% 
  inner_join( y = ele_17, by = "code")


carte17 <- dta_map_17 %>% 
  ggplot2::ggplot() +
  geom_sf(aes(fill = Abstentions_ins)) +
  guides(fill=guide_colorbar(title="Pourcentage d'abstention"))+
  scale_fill_gradient(low = "#FCF3CF", 
                      high = "#fb1c05")+
  theme_light() + 
  theme(plot.title=element_text( hjust=0.5, vjust=0.5, face='bold'))

carte17

###### 2012 #############

ele_12 <- read.csv("In/France-politique-2012.csv", sep=";")
ele_12 <- ele_12[-(97:107),]

dpt_shape %>% 
  ggplot() + geom_sf()

ele_12 <- ele_12 %>%
  mutate(code = code_dep)%>%
  mutate(Abstention = 100-Participation)

dta_map_12 <- dpt_shape %>% 
  inner_join( y = ele_12, by = "code")


carte12 <- dta_map_12 %>% 
  ggplot2::ggplot() +
  geom_sf(aes(fill = Abstention)) +
  guides(fill=guide_colorbar(title="Pourcentage d'abstention"))+
  scale_fill_gradient(low = "#FCF3CF", 
                      high = "#fb1c05")+
  theme_light() + 
  theme(plot.title=element_text( hjust=0.5, vjust=0.5, face='bold'))

carte12

## To-do : 
# Mettre les mêmes échelles
plot_grid(carte12, 
          carte17, 
          labels=c("Taux d'abstention par départements aux élections \nprésidentielles de 2012", 
                   "Taux d'abstention par départements aux élections \nprésidentielles de 2017"), 
          ncol = 2, 
          nrow = 1)



## Différence abstention
diff_abst <- ele_17$Abstentions_ins-ele_12$Abstention

new_dta <- data.frame(code=code_dep, diff_abs = diff_abst)

dpt_shape %>% 
  ggplot() + geom_sf()

dta_dif <- dpt_shape %>% 
  inner_join(y = new_dta, by = "code")

cartedif <- dta_dif %>% 
  ggplot2::ggplot() +
  geom_sf(aes(fill = diff_abst)) +
  guides(fill=guide_colorbar(title="Différence pourcentage d'abstention"))+
  scale_fill_gradient2(midpoint = 0, 
                      mid = "white", 
                      low = "#18BFD9", 
                      high = "#fb1c05")+
  theme_light() + 
  ggtitle("Evolution de l'abstention aux élections présidentielles entre 2012 et 2017")
  theme(plot.title=element_text( hjust=0.5, vjust=0.5, face='bold'))
    
    
cartedif
