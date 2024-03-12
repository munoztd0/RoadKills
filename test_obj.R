
data_accidents_clean <-
    data_accidents_obj_impl |>
  #create new column that gathers cycle, Vélo électrique rapide and Vélo électrique lent into velo
      mutate(type_usager = 
               case_when(
                 genre_d_objet_genre_d_usager == "Vélo électrique rapide" ~ "Vélo",
                 genre_d_objet_genre_d_usager == "Vélo électrique lent" ~ "Vélo",
                 genre_d_objet_genre_d_usager == "Cycle" ~ "Vélo",
                 TRUE ~ genre_d_objet_genre_d_usager
               )
      ) |>
    #now gather Piéton and Engin assimilé à un véhicule  to pieton
      mutate(type_usager = 
               case_when(
                 genre_d_objet_genre_d_usager == "Aucun (piéton)" ~ "Pieton + EAV",
                 genre_d_objet_genre_d_usager == "Engin assimilé à un véhicule (EAV)" ~ "Pieton + EAV",
                 TRUE ~ type_usager
               )
      ) |>
    #now gather Motocycle and Cyclomoteur  to pieton
      mutate(type_usager = 
               case_when(
                 genre_d_objet_genre_d_usager == "Motocycle" ~ "Motocycle",
                 genre_d_objet_genre_d_usager == "Cyclomoteur" ~ "Motocycle",
                 TRUE ~ type_usager
               )
      ) |>
    #now gather Voiture de tourisme and Véhicule de transport de personnes  to voiture
      mutate(type_usager = 
               case_when(
                 genre_d_objet_genre_d_usager == "Voiture de tourisme" ~ "Voiture",
                 genre_d_objet_genre_d_usager == "Véhicule de transport de personnes" ~ "Voiture",
                 genre_d_objet_genre_d_usager == "Véhicule de transport de choses" ~ "Voiture",
                 TRUE ~ type_usager
               )
      ) |>
      #now transform autre (y compris inconnu) to NA
      mutate(type_usager = 
               case_when(
                 genre_d_objet_genre_d_usager == "Autre (y compris inconnu)" ~ NA_character_,
                 TRUE ~ type_usager
               )
      ) 

#now we can plot

data_accidents_clean |>
  tidyr::drop_na(type_usager) |>
  filter(annee == 2022) |>
  filter(gravite_de_l_accident != "Accident avec blessés léger") |>
  group_by(type_usager, responsabilite_presumee_de_l_objet) |>
  tally(value) |>
  ggplot(aes(x = reorder(type_usager, n), y = n)) +
  geom_bar(alpha = 0.6, position = "dodge", stat = "identity", aes(fill = responsabilite_presumee_de_l_objet)) +
  coord_flip() +
  labs(x = "Type d'usager", y = "Nombre d'accidents", title = "Accidents par type d'usager en Suisse") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# yaml of quarto with folding code chunck echo set to false
---
title: "Accidents par type d'usager en Suisse"
author: "Romain Fournier"
date: `r Sys.Date()`
format: quarto
output:
 html_document:
   df_print: paged
   toc: true
   toc_float: true
   toc_depth: 3
   code_folding: hide
   code_download: true
   code_chunck:
     echo: false

   knitr::opts_chunk$set(echo = FALSE, message = FALSE, warning = FALSE)