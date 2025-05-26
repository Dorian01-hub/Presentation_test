if (!require(tidyverse)) install.packages("tidyverse")
if (!require(arrow)) install.packages("arrow")

library(arrow)
library(tidyverse)


d <- readRDS("setup/fusion/ul_merge.rds") 
d2 <- readRDS("setup/fusion/ep_merge.rds") %>% filter(iaa == TRUE)

fare = list()
fare_ul = list()
fare_ep = list()
ul2 = list()
tête_groupe_iaa = list()
tête_groupe_cga = list()
d_iaa_ul = list()
d_cga_ul = list()


for (i in paste("_",c("2018","2019","2020","2021","2022"),sep = "")) {
  
  fare[[i]] = open_dataset(paste("/var/data/nfs/CERISE/03-Espace-de-Diffusion/150_Sources_externes/15070_INSEE/FARE/1_TABLES/fare",substr(i,2,5),"meth",substr(i,2,5),".parquet",sep = "")) |>
    select(siren, siren_ent,id_tg, redi_r310, redi_e200,stat_cj,ape_diff,diff_ul,diff_ep) |>
    mutate(coop = case_when(stat_cj %in% c("6317","5432", "5532", "5632", "6532","6318", "5459", "5559", "5659") ~ TRUE, TRUE ~ FALSE),
           iaa = (substr(ape_diff,1,2) %in% c("10","11") | ( substr(ape_diff,1,3) %in% c("462","463") & ape_diff != "4635Z") | substr(ape_diff,1,1) == "0" ),
           activite = case_when(
             ape_diff %in% c("1013B", "1071B", "1071C", "1071D") ~ "AC",
             substr(ape_diff,1,2) %in% c("10","11") & !(ape_diff %in% c("1013B", "1071B", "1071C", "1071D")) ~ "IAA",
             substr(ape_diff,1,3) %in% c("462","463") & ape_diff != "4635Z" ~ "CGA", ## on enlève le tabac
             substr(ape_diff,1,1) == "0" ~ "agricole",
             TRUE ~ NA_character_
           ))
  
  
  # je remet le bon id_tg pour créer base en unité légale
  fare_ul[[i]] = fare[[i]] |> filter(diff_ul == 1) |> mutate(sirent_2 = ifelse(siren_ent != "",siren_ent,siren)) |> select(-id_tg)
  fare_ep[[i]] = fare[[i]] |> filter(diff_ep == 1) |> select(siren,id_tg)
  
  
  
  
  ul2[[i]] = left_join(fare_ul[[i]],fare_ep[[i]],by = c("sirent_2" = "siren"))
  
  
  ### ici id_tg est le bon et coop et activite se rapporte à l'unité légale
  ### je selectionne toutes les tête de groupe des groupes coopératifs
  
  
  tête_groupe_iaa[[i]] <- d2 %>% filter(iaa == TRUE & classe == "tête de groupe coop" & activite == "IAA" & année == substr(i,2,5)) %>%
    count(id_tg) %>% filter(id_tg != "") %>% select(id_tg)
  
  tête_groupe_cga[[i]] <- d2 %>% filter(iaa == TRUE & classe == "tête de groupe coop" & activite == "CGA" & année == substr(i,2,5)) %>% 
    count(id_tg) %>% filter(id_tg != "") %>% select(id_tg)
  
  
  
  d_iaa_ul[[i]] <- ul2[[i]] %>% filter(id_tg %in% tête_groupe_iaa[[i]]$id_tg) 
  d_cga_ul[[i]] <- ul2[[i]] %>% filter(id_tg %in% tête_groupe_cga[[i]]$id_tg) 
  
  
  
  
  
}

rm(list = c("tête_groupe_cga","tête_groupe_iaa","ul2","fare_ul","fare_ep","fare","i","d","d2"))




