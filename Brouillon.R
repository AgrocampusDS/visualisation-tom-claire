# library
library(tidyverse)
library(tidyr)
library(viridis)
library(ggplot2)


# Create dataset
food <- read_csv("In/openfoodfacts_Eco.csv")
food <- food[,c(3:10)]
food <- food %>%
    drop_na() #%>%

food <- food  %>%
  group_by(pnns_groups_2, nutrition_grade_fr) %>%
  summarise(nb = n()) %>%
  mutate(percentage = nb/sum(nb))

p <- ggplot(food)+
  geom_bar(aes(x=pnns_groups_2, y=percentage, fill=nutrition_grade_fr), stat = "identity") +
  scale_fill_viridis(discrete = TRUE) +
  coord_polar()
p





# p <- food %>% 
#   group_by(pnns_groups_2)%>%
#   count(nutrition_grade_fr)%>%
#   #summarise(n=n()) %>%
#   #mutate(per=n/sum(n)*100) %>%
#   ggplot()+
#   geom_bar(aes(x=pnns_groups_2, fill=nutrition_grade_fr), stat="count", alpha=0.5) +
#   scale_fill_viridis(discrete = TRUE) +
#   coord_polar()
# p


### Je veux mettre en rouge les mauvais et en vert les meilleurs
#### Changer les légende et les axes 
### changer la police de certains aliments en fonction des autres graphiques


