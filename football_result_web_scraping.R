rm(list = ls())

library(rvest)
library(dplyr)
library(stringr)
library(tibble)
library(httr)

# URL de la page
url <- "https://fbref.com/en/comps/Big5/Big-5-European-Leagues-Stats"

# Récupérer la page avec un user-agent
page <- read_html(url)

# utiliser l'extension google chrome (SelectorGadget) pour selectioner les éléments dont nous avons besoin
big_5_table <- page %>% html_nodes('#big5_table') %>% html_table()
big_5_table <- big_5_table[[1]] %>% 
  as_tibble() 

clean_table <- big_5_table %>% 
  mutate(
    # 1️⃣ Garder uniquement les majuscules dans Country
    Country = str_sub(Country, -3, -1),
    
    # 2️⃣ Extraire les 3 derniers caractères de 'Last 5'
    `Last 3` = str_sub(`Last 5`, -5, -1),
    
    # 3️⃣ Extraire la partie numérique du 'Top Team Scorer'
    `Top Team Scorer Goals` = str_extract(`Top Team Scorer`, "\\d+"),
    
    # 4️⃣ Créer la colonne 'Performance2' → calcul basée sur W=20, D=10, L=0
    `Actual Performance` = str_replace_all(`Last 5`, c("W" = "20+", "D" = "10+", "L" = "0+")) %>% str_remove("\\+$") %>%  # enlever le dernier '+'
      sapply(function(x) sum(as.numeric(unlist(strsplit(x, "\\+")))))
    
  )

# Most Goal in the Premier League
clean_table %>% 
  filter(Country == 'ENG') %>% 
  filter(`Top Team Scorer Goals` == max(`Top Team Scorer Goals`)) %>% 
  select(`Top Team Scorer`) %>% 
  pull()


page %>% html_nodes('#div_league_summary') %>% html_text()
