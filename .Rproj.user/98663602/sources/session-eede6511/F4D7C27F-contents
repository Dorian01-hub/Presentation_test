
if (!require(tidyverse)) install.packages("tidyverse")
if (!require(echarts4r)) install.packages("echarts4r")
if (!require(htmlwidgets)) install.packages("htmlwidgets")


library(htmlwidgets)
library(tidyverse)
library(echarts4r)

d2 = readRDS("ep_merge.rds") %>% filter(année == "2022")




#### graph0 ######

D_graph <- d2 %>%  mutate(acti3 = substr(ape_diff,1,3)) %>% filter(acti3 %in% c(101,102,103,104,105,106,107,108,109,110,462,463)) %>% filter(activite != "agricole") %>% filter(activite != "AC" & stat_etat %in% c(1,6)) %>% mutate(acti3 = case_when(acti3 == 101 ~ "Viande",acti3 == 102 ~ "Poisson",acti3 == 103 ~ "Fruit et légume",acti3 == 104 ~ "Huiles et graisses",acti3 == 105 ~ "Lait",acti3 == 106 ~ "Travail du grain",acti3 == 107 ~ "Boulangerie",acti3 == 108 ~ "Autre produit alimentaire",acti3 == 109 ~ "Aliment pour animaux",acti3 == 110 ~ "Boisson",acti3 == 462 ~ "CGA agricoles bruts",acti3 == 463 ~ "CGA alimentaires")) %>% mutate(acti3 = str_replace_all(acti3, " ", "\n"))  %>%   group_by(acti3,classe) %>% summarise(across(where(is.numeric),\(x) round(sum(x,na.rm = TRUE)/1000000,2),.names = "{col}"),`Nombre d'entreprise` = n()) %>% 
  group_by(acti3) %>% mutate(across(where(is.numeric),\(x) x/sum(x,na.rm=TRUE),.names="{col} en %")) %>% as.data.frame()


D_graph1 <- D_graph %>% select(acti3,classe,`Nombre d'entreprise`) %>%  pivot_wider(names_from = classe,values_from = `Nombre d'entreprise`)
D_graph1[is.na(D_graph1)] <- 0

D_graph1 <- D_graph1 %>%
  mutate(total = `coop indépendante` + `non coop indépendante` + `tête de groupe coop` +
           `tête de groupe non coop` + `tête de groupe étrangère`) %>% arrange(desc(total))

D_graph1 <- as.data.frame(D_graph1)
rownames(D_graph1) <- D_graph1$acti3





graph0 <- D_graph1 %>%
  e_charts(acti3) %>%
  e_bar(`coop indépendante`, stack = "grp1", name = "coopérative:indépendante", itemStyle = list(color = "#32CD32")) %>%
  e_bar(`tête de groupe coop`, stack = "grp1", name = "coopérative:groupe", itemStyle = list(color = "#228B22")) %>%
  e_bar(`non coop indépendante`, stack = "grp2", name = "non coopérative:indépendante", itemStyle = list(color = "#87CEEB")) %>%
  e_bar(`tête de groupe non coop`, stack = "grp2", name = "non coopérative:groupe", itemStyle = list(color = "#4682B4")) %>%
  e_bar(`tête de groupe étrangère`, name = "étrangère", itemStyle = list(color = "#1E3A5F")) %>% 
  e_bar(total, itemStyle = list(color = "#666666"),
        label = list(show = TRUE, position = "top", fontSize = 10, color = "black")) %>% 
  e_x_axis(name = "Activités") %>%  # Nom de l'axe X
  e_y_axis(name = "Nombre d'entreprise") %>%  # Nom de l'axe Y
  e_tooltip() %>% 
  e_toolbox(right = "5%",top = "5%") %>%  # Toolbox plus à gauche
  e_datazoom(type = "inside") %>% 
  e_theme("essos") %>% 
  e_legend(selected = list(
    "coopérative:indépendante" = FALSE,
    "coopérative:groupe" = FALSE,
    "non coopérative:indépendante" = FALSE,
    "non coopérative:groupe" = FALSE,
    "étrangère" = FALSE,
    "total" = TRUE  # Seul "total" reste actif
  )) %>% e_x_axis(axisLabel = list(rotate = 45)) # Rotation des étiquettes

graph0

saveWidget(graph0,"graphique0_interactif.html")


#### graph #####


D_graph <- d2 %>%  mutate(acti3 = substr(ape_diff,1,3)) %>% filter(acti3 %in% c(101,102,103,104,105,106,107,108,109,110,462,463)) %>% filter(activite != "agricole") %>% filter(activite != "AC" & stat_etat %in% c(1,6)) %>% mutate(acti3 = case_when(acti3 == 101 ~ "Viande",acti3 == 102 ~ "Poisson",acti3 == 103 ~ "Fruit et légume",acti3 == 104 ~ "Huiles et graisses",acti3 == 105 ~ "Lait",acti3 == 106 ~ "Travail du grain",acti3 == 107 ~ "Boulangerie",acti3 == 108 ~ "Autre produit alimentaire",acti3 == 109 ~ "Aliment pour animaux",acti3 == 110 ~ "Boisson",acti3 == 462 ~ "CGA agricoles bruts",acti3 == 463 ~ "CGA alimentaires")) %>% mutate(acti3 = str_replace_all(acti3, " ", "\n"))  %>%   group_by(acti3,classe) %>% summarise(across(where(is.numeric),\(x) round(sum(x,na.rm = TRUE)/1000000,2),.names = "{col}")) %>% 
  group_by(acti3) %>% mutate(across(where(is.numeric),\(x) x/sum(x,na.rm=TRUE),.names="{col} en %")) %>% as.data.frame()


D_graph1 <- D_graph %>% select(acti3,classe,`chiffre d'affaires`) %>%  pivot_wider(names_from = classe,values_from = `chiffre d'affaires`)
D_graph1[is.na(D_graph1)] <- 0

D_graph1 <- D_graph1 %>%
  mutate(total = `coop indépendante` + `non coop indépendante` + `tête de groupe coop` +
           `tête de groupe non coop` + `tête de groupe étrangère`) %>% arrange(desc(total))

D_graph1 <- as.data.frame(D_graph1)
rownames(D_graph1) <- D_graph1$acti3





graph <- D_graph1 %>%
  e_charts(acti3) %>%
  e_bar(`coop indépendante`, stack = "grp1", name = "coopérative:indépendante", itemStyle = list(color = "#32CD32")) %>%
  e_bar(`tête de groupe coop`, stack = "grp1", name = "coopérative:groupe", itemStyle = list(color = "#228B22")) %>%
  e_bar(`non coop indépendante`, stack = "grp2", name = "non coopérative:indépendante", itemStyle = list(color = "#87CEEB")) %>%
  e_bar(`tête de groupe non coop`, stack = "grp2", name = "non coopérative:groupe", itemStyle = list(color = "#4682B4")) %>%
  e_bar(`tête de groupe étrangère`, name = "étrangère", itemStyle = list(color = "#1E3A5F")) %>% 
  e_bar(total, itemStyle = list(color = "#666666"),
        label = list(show = TRUE, position = "top", fontSize = 10, color = "black")) %>% 
  e_x_axis(name = "Activités") %>%  # Nom de l'axe X
  e_y_axis(name = "Chiffre d'affaires\nen millard d'euros") %>%  # Nom de l'axe Y
  e_tooltip() %>% 
  e_toolbox(right = "5%",top = "5%") %>%  # Toolbox plus à gauche
  e_datazoom(type = "inside") %>% 
  e_theme("essos") %>% 
  e_legend(selected = list(
    "coopérative:indépendante" = FALSE,
    "coopérative:groupe" = FALSE,
    "non coopérative:indépendante" = FALSE,
    "non coopérative:groupe" = FALSE,
    "étrangère" = FALSE,
    "total" = TRUE  # Seul "total" reste actif
  )) %>% e_x_axis(axisLabel = list(rotate = 45)) # Rotation des étiquettes

graph

saveWidget(graph,"graphique_interactif.html")





##### graph1 ######




library(htmlwidgets)
library(tidyverse)
library(echarts4r)


D_graph <- d2 %>% 
  mutate(acti3 = substr(ape_diff,1,3)) %>% 
  filter(acti3 %in% c(101,102,103,104,105,106,107,108,109,110,462,463)) %>% filter(activite != "agricole") %>% filter(activite != "AC" & stat_etat %in% c(1,6)) %>% mutate(acti3 = case_when(acti3 == 101 ~ "Viande",acti3 == 102 ~ "Poisson",acti3 == 103 ~ "Fruit et légume",acti3 == 104 ~ "Huiles et graisses",acti3 == 105 ~ "Lait",acti3 == 106 ~ "Travail du grain",acti3 == 107 ~ "Boulangerie",acti3 == 108 ~ "Autre produit alimentaire",acti3 == 109 ~ "Aliment pour animaux",acti3 == 110 ~ "Boisson",acti3 == 462 ~ "CGA agricoles bruts",acti3 == 463 ~ "CGA alimentaires")) %>% mutate(acti3 = str_replace_all(acti3, " ", "\n"))  %>%   
  group_by(acti3,classe) %>% 
  summarise(across(where(is.numeric),\(x) round(mean(x,na.rm = TRUE),0),.names = "{col}")) %>% 
  group_by(acti3) %>% 
  mutate(across(where(is.numeric),\(x) x/sum(x,na.rm=TRUE),.names="{col} en %")) %>% 
  as.data.frame()

D_graph1 <- D_graph %>% 
  select(acti3,classe,`chiffre d'affaires`) %>%  
  pivot_wider(names_from = classe, values_from = `chiffre d'affaires`)
D_graph1[is.na(D_graph1)] <- 0

graph1 <- D_graph1 %>%
  e_charts(acti3) %>%
  e_bar(`coop indépendante`, name = "coopérative:indépendante", itemStyle = list(color = "#32CD32")) %>%
  e_bar(`tête de groupe coop`, name = "coopérative:groupe", itemStyle = list(color = "#228B22")) %>%
  e_bar(`non coop indépendante`, name = "non coopérative:indépendante", itemStyle = list(color = "#87CEEB")) %>%
  e_bar(`tête de groupe non coop`, name = "non coopérative:groupe", itemStyle = list(color = "#4682B4")) %>%
  e_bar(`tête de groupe étrangère`, name = "étrangère", itemStyle = list(color = "#1E3A5F")) %>% 
  e_x_axis(name = "Activités") %>%
  e_y_axis(name = "Chiffre d'affaires\nen milliers d'euros") %>%
  e_tooltip() %>% 
  e_toolbox(right = "5%",top = "5%") %>% 
  e_datazoom(type = "inside") %>% 
  e_theme("essos")  %>% e_x_axis(axisLabel = list(rotate = 45)) 

graph1

saveWidget(graph1,"graphique1_interactif.html")




#### graph2 #####

D_graph <- d2 %>%  mutate(acti3 = substr(ape_diff,1,3)) %>% filter(acti3 %in% c(101,102,103,104,105,106,107,108,109,110,462,463)) %>% filter(activite != "agricole") %>% filter(activite != "AC" & stat_etat %in% c(1,6)) %>%  mutate(acti3 = case_when(acti3 == 101 ~ "Viande",acti3 == 102 ~ "Poisson",acti3 == 103 ~ "Fruit et légume",acti3 == 104 ~ "Huiles et graisses",acti3 == 105 ~ "Lait",acti3 == 106 ~ "Travail du grain",acti3 == 107 ~ "Boulangerie",acti3 == 108 ~ "Autre produit alimentaire",acti3 == 109 ~ "Aliment pour animaux",acti3 == 110 ~ "Boisson",acti3 == 462 ~ "CGA agricoles bruts",acti3 == 463 ~ "CGA alimentaires")) %>% mutate(acti3 = str_replace_all(acti3, " ", "\n"))  %>%   group_by(acti3,classe) %>% summarise(across(where(is.numeric),\(x) round(sum(x,na.rm = TRUE)/1000000,2),.names = "{col}")) %>% 
  group_by(acti3) %>% mutate(across(where(is.numeric),\(x) x/sum(x,na.rm=TRUE),.names="{col} en %")) %>% as.data.frame()

D_graph1 <- D_graph %>% select(acti3,classe,VAHT) %>%  pivot_wider(names_from = classe,values_from = VAHT)
D_graph1[is.na(D_graph1)] <- 0

D_graph1 <- D_graph1 %>%
  mutate(total = `coop indépendante` + `non coop indépendante` + `tête de groupe coop` +
           `tête de groupe non coop` + `tête de groupe étrangère`) %>% arrange(desc(total))

D_graph1 <- as.data.frame(D_graph1)
rownames(D_graph1) <- D_graph1$acti3





graph2 <- D_graph1 %>%
  e_charts(acti3) %>%
  e_bar(`coop indépendante`, stack = "grp1", name = "coopérative:indépendante", itemStyle = list(color = "#32CD32")) %>%
  e_bar(`tête de groupe coop`, stack = "grp1", name = "coopérative:groupe", itemStyle = list(color = "#228B22")) %>%
  e_bar(`non coop indépendante`, stack = "grp2", name = "non coopérative:indépendante", itemStyle = list(color = "#87CEEB")) %>%
  e_bar(`tête de groupe non coop`, stack = "grp2", name = "non coopérative:groupe", itemStyle = list(color = "#4682B4")) %>%
  e_bar(`tête de groupe étrangère`, name = "étrangère", itemStyle = list(color = "#1E3A5F")) %>% 
  e_bar(total, itemStyle = list(color = "#666666"),
        label = list(show = TRUE, position = "top", fontSize = 10, color = "black")) %>% 
  e_x_axis(name = "Activités") %>%  # Nom de l'axe X
  e_y_axis(name = "VAHT\nen millard d'euros") %>%  # Nom de l'axe Y
  e_tooltip() %>% 
  e_toolbox(right = "5%",top = "5%") %>%  # Toolbox plus à gauche
  e_datazoom(type = "inside") %>% 
  e_theme("essos") %>% 
  e_legend(selected = list(
    "coopérative:indépendante" = FALSE,
    "coopérative:groupe" = FALSE,
    "non coopérative:indépendante" = FALSE,
    "non coopérative:groupe" = FALSE,
    "étrangère" = FALSE,
    "total" = TRUE  # Seul "total" reste actif
  )) %>% e_x_axis(axisLabel = list(rotate = 45)) # Rotation des étiquettes

graph2
saveWidget(graph2,"graphique2_interactif.html")


#### graph3 #####





D_graph <- d2 %>%  mutate(acti3 = substr(ape_diff,1,3)) %>% filter(acti3 %in% c(101,102,103,104,105,106,107,108,109,110,462,463)) %>%  filter(activite != "agricole") %>% filter(activite != "AC" & stat_etat %in% c(1,6)) %>%  mutate(acti3 = case_when(acti3 == 101 ~ "Viande",acti3 == 102 ~ "Poisson",acti3 == 103 ~ "Fruit et légume",acti3 == 104 ~ "Huiles et graisses",acti3 == 105 ~ "Lait",acti3 == 106 ~ "Travail du grain",acti3 == 107 ~ "Boulangerie",acti3 == 108 ~ "Autre produit alimentaire",acti3 == 109 ~ "Aliment pour animaux",acti3 == 110 ~ "Boisson",acti3 == 462 ~ "CGA agricoles bruts",acti3 == 463 ~ "CGA alimentaires")) %>% mutate(acti3 = str_replace_all(acti3, " ", "\n"))  %>%   group_by(acti3,classe) %>% summarise(across(where(is.numeric),\(x) round(sum(x,na.rm = TRUE),0),.names = "{col}")) %>% 
  group_by(acti3) %>% mutate(across(where(is.numeric),\(x) x/sum(x,na.rm=TRUE),.names="{col} en %")) %>% as.data.frame()


D_graph1 <- D_graph %>% select(acti3,classe,résultat) %>%  pivot_wider(names_from = classe,values_from = résultat)
D_graph1[is.na(D_graph1)] <- 0

D_graph1 <- D_graph1 %>%
  mutate(total = `coop indépendante` + `non coop indépendante` + `tête de groupe coop` +
           `tête de groupe non coop` + `tête de groupe étrangère`) %>% arrange(desc(total))

D_graph1 <- as.data.frame(D_graph1)
rownames(D_graph1) <- D_graph1$acti3





graph3 <- D_graph1 %>%
  e_charts(acti3) %>%
  e_bar(`coop indépendante`, stack = "grp1", name = "coopérative:indépendante", itemStyle = list(color = "#32CD32")) %>%
  e_bar(`tête de groupe coop`, stack = "grp1", name = "coopérative:groupe", itemStyle = list(color = "#228B22")) %>%
  e_bar(`non coop indépendante`, stack = "grp2", name = "non coopérative:indépendante", itemStyle = list(color = "#87CEEB")) %>%
  e_bar(`tête de groupe non coop`, stack = "grp2", name = "non coopérative:groupe", itemStyle = list(color = "#4682B4")) %>%
  e_bar(`tête de groupe étrangère`, name = "étrangère", itemStyle = list(color = "#1E3A5F")) %>% 
  e_bar(total, itemStyle = list(color = "#666666"),
        label = list(show = TRUE, position = "top", fontSize = 10, color = "black")) %>% 
  e_x_axis(name = "Activités") %>%  # Nom de l'axe X
  e_y_axis(name = "Résutat d'exploitation\nen milliers d'euros") %>%  # Nom de l'axe Y
  e_tooltip() %>% 
  e_toolbox(right = "5%",top = "5%") %>%  # Toolbox plus à gauche
  e_datazoom(type = "inside") %>% 
  e_theme("essos") %>% 
  e_legend(selected = list(
    "coopérative:indépendante" = FALSE,
    "coopérative:groupe" = FALSE,
    "non coopérative:indépendante" = FALSE,
    "non coopérative:groupe" = FALSE,
    "étrangère" = FALSE,
    "total" = TRUE  # Seul "total" reste actif
  )) %>% e_x_axis(axisLabel = list(rotate = 45)) # Rotation des étiquettes


graph3
saveWidget(graph3,"graphique3_interactif.html")


#### graph4 #####


D_graph <- d2 %>%  mutate(acti3 = substr(ape_diff,1,3)) %>% filter(acti3 %in% c(101,102,103,104,105,106,107,108,109,110,462,463)) %>% filter(activite != "agricole") %>% filter(activite != "AC" & stat_etat %in% c(1,6)) %>% mutate(acti3 = case_when(acti3 == 101 ~ "Viande",acti3 == 102 ~ "Poisson",acti3 == 103 ~ "Fruit et légume",acti3 == 104 ~ "Huiles et graisses",acti3 == 105 ~ "Lait",acti3 == 106 ~ "Travail du grain",acti3 == 107 ~ "Boulangerie",acti3 == 108 ~ "Autre produit alimentaire",acti3 == 109 ~ "Aliment pour animaux",acti3 == 110 ~ "Boisson",acti3 == 462 ~ "CGA agricoles bruts",acti3 == 463 ~ "CGA alimentaires")) %>% mutate(acti3 = str_replace_all(acti3, " ", "\n"))  %>%   group_by(acti3,classe) %>% summarise(across(where(is.numeric),\(x) round(sum(x,na.rm = TRUE)/1000000,2),.names = "{col}")) %>% 
  group_by(acti3) %>% mutate(across(where(is.numeric),\(x) x/sum(x,na.rm=TRUE),.names="{col} en %")) %>% as.data.frame()


D_graph1 <- D_graph %>% select(acti3,classe,`dettes financières`) %>%  pivot_wider(names_from = classe,values_from = `dettes financières`)
D_graph1[is.na(D_graph1)] <- 0

D_graph1 <- D_graph1 %>%
  mutate(total = `coop indépendante` + `non coop indépendante` + `tête de groupe coop` +
           `tête de groupe non coop` + `tête de groupe étrangère`) %>% arrange(desc(total))

D_graph1 <- as.data.frame(D_graph1)
rownames(D_graph1) <- D_graph1$acti3





graph4 <- D_graph1 %>%
  e_charts(acti3) %>%
  e_bar(`coop indépendante`, stack = "grp1", name = "coopérative:indépendante", itemStyle = list(color = "#32CD32")) %>%
  e_bar(`tête de groupe coop`, stack = "grp1", name = "coopérative:groupe", itemStyle = list(color = "#228B22")) %>%
  e_bar(`non coop indépendante`, stack = "grp2", name = "non coopérative:indépendante", itemStyle = list(color = "#87CEEB")) %>%
  e_bar(`tête de groupe non coop`, stack = "grp2", name = "non coopérative:groupe", itemStyle = list(color = "#4682B4")) %>%
  e_bar(`tête de groupe étrangère`, name = "étrangère", itemStyle = list(color = "#1E3A5F")) %>% 
  e_bar(total, itemStyle = list(color = "#666666"),
        label = list(show = TRUE, position = "top", fontSize = 10, color = "black")) %>% 
  e_x_axis(name = "Activités") %>%  # Nom de l'axe X
  e_y_axis(name = "Dettes financières\nen millard d'euros") %>%  # Nom de l'axe Y
  e_tooltip() %>% 
  e_toolbox(right = "5%",top = "5%") %>%  # Toolbox plus à gauche
  e_datazoom(type = "inside") %>% 
  e_theme("essos") %>% 
  e_legend(selected = list(
    "coopérative:indépendante" = FALSE,
    "coopérative:groupe" = FALSE,
    "non coopérative:indépendante" = FALSE,
    "non coopérative:groupe" = FALSE,
    "étrangère" = FALSE,
    "total" = TRUE  # Seul "total" reste actif
  )) %>% e_x_axis(axisLabel = list(rotate = 45)) # Rotation des étiquettes

graph4
saveWidget(graph4,"graphique4_interactif.html")



#### graph5 ####


library(htmlwidgets)
library(tidyverse)
library(echarts4r)

D_graph <- d2 %>%  mutate(acti3 = substr(ape_diff,1,3)) %>% filter(acti3 %in% c(101,102,103,104,105,106,107,108,109,110)) %>% filter(activite != "agricole") %>% filter(activite != "AC" & stat_etat %in% c(1,6)) %>% mutate(acti3 = case_when(acti3 == 101 ~ "Viande",acti3 == 102 ~ "Poisson",acti3 == 103 ~ "Fruit et légume",acti3 == 104 ~ "Huiles et graisses",acti3 == 105 ~ "Lait",acti3 == 106 ~ "Travail du grain",acti3 == 107 ~ "Boulangerie",acti3 == 108 ~ "Autre produit alimentaire",acti3 == 109 ~ "Aliment pour animaux",acti3 == 110 ~ "Boisson")) %>% mutate(acti3 = str_replace_all(acti3, " ", "\n"))  %>%   group_by(acti3,classe) %>% summarise(ebe_VACF =  round(ifelse(sum(ebe,na.rm = T) < 0 & sum(VACF,na.rm = T) < 0,-(sum(ebe,na.rm = TRUE)/ sum(VACF,na.rm = TRUE)),sum(ebe,na.rm = TRUE)/ sum(VACF,na.rm = TRUE) ),2)) %>% as.data.frame()


D_graph1 <- D_graph %>% select(acti3,classe,ebe_VACF) %>%  pivot_wider(names_from = classe,values_from = ebe_VACF)
D_graph1[is.na(D_graph1)] <- 0


D_graph1 <- as.data.frame(D_graph1)
rownames(D_graph1) <- D_graph1$acti3





graph5 <- D_graph1 %>%
  e_charts(acti3) %>%
  e_bar(`coop indépendante`, name = "coopérative:indépendante", itemStyle = list(color = "#32CD32")) %>%
  e_bar(`tête de groupe coop`, name = "coopérative:groupe", itemStyle = list(color = "#228B22")) %>%
  e_bar(`non coop indépendante`, name = "non coopérative:indépendante", itemStyle = list(color = "#87CEEB")) %>%
  e_bar(`tête de groupe non coop`, name = "non coopérative:groupe", itemStyle = list(color = "#4682B4")) %>%
  e_bar(`tête de groupe étrangère`, name = "étrangère", itemStyle = list(color = "#1E3A5F")) %>% 
  
  e_x_axis(name = "Activités") %>%  # Nom de l'axe X
  e_y_axis(name = "Taux de marge") %>%  # Nom de l'axe Y
  e_tooltip() %>% 
  e_toolbox(right = "5%",top = "5%") %>%  # Toolbox plus à gauche
  e_datazoom(type = "inside") %>% 
  e_theme("essos") %>% 
  e_legend(selected = list(
    "coopérative:indépendante" = TRUE,
    "coopérative:groupe" = TRUE,
    "non coopérative:indépendante" = TRUE,
    "non coopérative:groupe" = TRUE,
    "étrangère" = TRUE
    
  )) %>% e_x_axis(axisLabel = list(rotate = 45)) # Rotation des étiquettes

graph5
saveWidget(graph5,"graphique5_interactif.html")



