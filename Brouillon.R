# library
library(sf)
library(tidyverse)
library(tidyr)
library(viridis)
library(ggplot2)
library(raster)
library(cowplot)
library(plotly)
library(stringi)
library(grid)
library(gridExtra)

###### 2017  #############
ele_17 <- read.csv("In/Presidentielle_2017_Resultats_Département_T1_clean.csv")
ele_17 <- ele_17[-(97:106),-(28:49)]
ele_17_t <- as.data.frame(t(as.matrix(ele_17)))

ele_17_t <- ele_17_t%>%
  relocate(20,.after = 6)

ele_17_t <- ele_17_t%>%
  relocate(21,.before = 7)

ele_17 <- as.data.frame(t(as.matrix(ele_17_t)))
ele_17$Abstentions_ins <- as.numeric(ele_17$Abstentions_ins)

#On règle les problèmes de jointure (majuscule et suppression des accents)
# ele_17$Département <- stri_trans_general(ele_17$Département, "Latin-ASCII")
# ele_17$Département <- str_to_upper(ele_17$Département)


# On récupère les noms pour créer un autre jeu de données
code_dep <- c(ele_17[,1])

# shapefile

dpt_shape <-  st_read(dsn = 'In/contour-des-departements.geojson')

# #On règle les problèmes de jointure (majuscule et suppression des accents)
# dpt_shape$nom <- stri_trans_general(dpt_shape$nom, "Latin-ASCII")
# dpt_shape$nom <- str_to_upper(dpt_shape$nom)

# dpt_shape %>% 
#   print(n=10)

ele_17 <- ele_17 %>%
  rename(code = "CodeDépartement")


###### 2012 #############

ele_12 <- read.csv("In/France-politique-2012.csv", sep=";")
ele_12 <- ele_12[-(97:107),]
ele_12_t <- as.data.frame(t(as.matrix(ele_12)))

ele_12_t <- ele_12_t%>%
  relocate(20,.after = 6)

ele_12_t <- ele_12_t%>%
  relocate(21,.before = 7)

ele_12 <- as.data.frame(t(as.matrix(ele_12_t)))
ele_12$Participation <- as.numeric(ele_12$Participation)


ele_12 <- ele_12 %>%
  rename(code = "Département")

ele_12 <- ele_12 %>%
  mutate(code = code_dep)%>%
  mutate(Abstention = 100-Participation)



## Différence abstention
diff_abst <- ele_17$Abstentions_ins-ele_12$Abstention

new_dta <- data.frame(code=code_dep, diff_abs = diff_abst)

new_dta <- new_dta %>%
  mutate(Abst_cat = case_when(diff_abst < 0 ~ "diminution", 
                        diff_abst > 2.5 ~ "augmentation",
                        TRUE ~ "maintien ou faible augmentation"))


dpt_shape %>% 
  ggplot() + geom_sf()

dta_dif <- dpt_shape %>% 
  inner_join(y = new_dta, by = "code")


cartedif <- dta_dif %>% 
  ggplot2::ggplot() +
  geom_sf(aes(fill= Abst_cat)) +
  scale_fill_manual(values=list("augmentation" = "indianred2", 
                                "maintien ou faible augmentation" = "gray88", 
                                "diminution" = "darkseagreen2" ))+
  labs(fill = "Évolution abstention")+
  theme_classic() + 
  theme(axis.line=element_blank(),axis.text.x=element_blank(),
        axis.text.y=element_blank(),axis.ticks=element_blank())+
  ggtitle("Evolution de l'abstention aux élections présidentielles entre 2012 et 2017")+
  theme(plot.title=element_text( hjust=0.5, vjust=0.5, face='bold'))+
  annotate("rect", 
           xmin=c(8.2,1.2,0.8), xmax=c(9.8,3.7,4.2),
           ymin=c(41.2,48.1,45) , ymax=c(43.2,49.2,46.7),
           alpha=0.2, color="black", fill="grey", size=1.5)

cartedif


# ### Carte interactive ###
# cartedif <- ggplotly(cartedif)
# 
# cartedif <- cartedif %>% 
#   layout(dragmode = "pan")
# 
# cartedif

# Changer le nom et mettre en valeur deux exemples : 
# les yvelines et le département en rouge foncé. 

## Sélection des départements à mettre en valeur

# dep_interet <- new_dta %>%
#   filter(diff_abst<=-1.5 | diff_abst>=4)


# 
# annotation_custom(grob = textGrob(label = "Augmentation de l'abstenti", 
#                                   hjust = 0, gp = gpar(cex = 0.65, fontface = "italic")), 
#                   ymin = -4, 
#                   ymax = -2, 
#                   xmin = 20, 
#                   xmax = 80) 

# 
# dta_map_17 <- dpt_shape %>% 
#   inner_join( y = ele_17, 
#               by = "code")
# 
# carte17 <- dta_map_17 %>% 
#   ggplot2::ggplot() +
#   geom_sf(aes(fill = Abstentions_ins)) +
#   guides(fill=guide_colorbar(title="Pourcentage d'abstention"))+
#   scale_fill_gradient(low = "#FCF3CF", 
#                       high = "#fb1c05")+
#   theme_light() + 
#   theme(plot.title=element_text( hjust=0.5, vjust=0.5, face='bold'))
# 
# carte17



# dta_map_12 <- dpt_shape %>% 
#   inner_join(y = ele_12, by = "code")
# 
# carte12 <- dta_map_12 %>% 
#   ggplot2::ggplot() +
#   geom_sf(aes(fill = Abstention)) +
#   guides(fill=guide_colorbar(title="Pourcentage d'abstention"))+
#   scale_fill_gradient(low = "#FCF3CF", 
#                       high = "#fb1c05")+
#   theme_light() + 
#   theme(plot.title=element_text( hjust=0.5, vjust=0.5, face='bold'))
# 
# carte12
# 
# ## To-do : 
# # Mettre les mêmes échelles
# plot_grid(carte12, 
#           carte17, 
#           labels=c("Taux d'abstention par départements aux élections \nprésidentielles de 2012", 
#                    "Taux d'abstention par départements aux élections \nprésidentielles de 2017"), 
#           ncol = 2, 
#           nrow = 1)





# cartedif <- dta_dif %>% 
#   ggplot2::ggplot() +
#   geom_sf(aes(fill = diff_abst)) +
#   guides(fill=guide_colorbar(title="Différence d'abstention"))+
#   scale_fill_gradient2(midpoint = 0, 
#                        mid = "white", 
#                        low = "#18BFD9", 
#                        high = "#fb1c05")+
#   theme_light() + 
#   ggtitle("Evolution de l'abstention aux élections présidentielles entre 2012 et 2017")+
#   theme(plot.title=element_text( hjust=0.5, vjust=0.5, face='bold'))+
#   annotate("rect", 
#            xmin=c(8.2,1.2,0.8), xmax=c(9.8,3.7,4.2),
#            ymin=c(41.2,48.1,45) , ymax=c(43.2,49.2,46.7),
#            alpha=0.2, color="black", fill="grey", size=1.5)+
#   annotate("text", 
#            label = "Augmentation de l'abstention", 
#            x = 7.5, y = 40.9 , 
#            vjust = 0, fontface ="bold", size = 3) +
#   annotate("text", 
#            label = "Diminution de l'abstention", 
#            x = 7.8, y = 50 , 
#            vjust = 0, fontface ="bold", size = 3) +
#   annotate("segment", 
#            x = 3.8, 
#            xend = 4.9, 
#            y = 49.3, 
#            yend = 49.9, 
#            colour = "black", 
#            size=0.8, 
#            arrow=arrow(length=unit(3, "mm")))+
#   annotate("segment", 
#            x = 8, 
#            xend = 6, 
#            y = 42, 
#            yend = 41, 
#            colour = "black", 
#            size=0.8, 
#            arrow=arrow(length=unit(3, "mm")))+
#   annotate("segment", 
#            x = 2.1, 
#            xend = 6, 
#            y = 45, 
#            yend = 41, 
#            colour = "black", 
#            size=0.8, 
#            arrow=arrow(length=unit(3, "mm")))
# 
# cartedif