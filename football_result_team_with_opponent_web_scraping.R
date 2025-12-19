rm(list = ls())

# Load the packages
library(rvest)
library(dplyr)
library(stringr)
library(tidyr)
library(purrr)
library(jsonlite)

# Define the base URL, leagues, and seasons
base_url <- 'https://understat.com/team'
soccer_data <- read.csv("understat_per_game.csv", na.strings = "NA")
teams <- unique(soccer_data$teams)

# Function to scrape data for a single league and season
all_data <- list()
t = 2


for(t in 1:length(teams)){
  
  team <- teams[t]
  team_data <- soccer_data %>% filter(teams == team)
  seasons <- unique(team_data$season)
  team_opponents <- list()
  s = 2
  
  for(s in 1:length(seasons)){
    
    season = seasons[s]
    url <- paste0(base_url, '/', team, '/', season)
    url <- stringr::str_replace_all(url, ' ', '%20')
    webpage <- read_html(url)
    
    # Extract the JSON data from the script tags
    scripts <- webpage %>% html_nodes('script')
    string_with_json_obj <- ''
    
    for (el in scripts) {
      if (grepl('datesData', html_text(el))) {
        string_with_json_obj <- html_text(el)
        break
      }
    }
    
    # Strip unnecessary symbols and get only JSON data
    ind_start <- regexpr("\\('", string_with_json_obj) + 2
    ind_end <- regexpr("'\\)", string_with_json_obj) -1
    json_data <- substr(string_with_json_obj, ind_start, ind_end)
    json_data <- stringi::stri_unescape_unicode(json_data)
    
    # Convert JSON data into R list
    # data <- fromJSON(json_data)
    data <- fromJSON(json_data)
    data <- data %>% 
      select(side, h, a, datetime) %>% 
      unnest_wider(h, names_sep = '_')%>% 
      unnest_wider(a, names_sep = '_') %>% 
      select(-ends_with(c('_id', '_short_title'))) %>% 
      mutate(team = teams[t],
             opponent = str_replace_all(paste0(h_title, a_title), teams[t], '')) %>% 
      select(team, opponent, side, datetime) %>% 
      mutate(season = season) %>% 
      rename(date = datetime) %>% 
      relocate(season,  .before = team)
    
    team_opponents[[s]] <- data
    names(team_opponents)[s] <- season
  }
  
  all_data[[t]] <- data.table::rbindlist(team_opponents)
  names(all_data)[t] <- team
}

# flatten the list to get a datframe
team_w_opponents <- data.table::rbindlist(all_data)


# View the combined dataframe
team_w_opponents %>% head(20)

# Export the data to a CSV file
write.csv(team_w_opponents, 'understat_team_w_opponents_data.csv', row.names = FALSE)

