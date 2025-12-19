rm(list = ls())

# Load the packages
library(rvest)
library(dplyr)
library(stringr)
library(purrr)
library(jsonlite)

# Define the base URL, leagues, and seasons
base_url <- 'https://understat.com/league'
leagues <- c('La_liga', 'EPL', 'Bundesliga', 'Serie_A', 'Ligue_1', 'RFPL')
# seasons <- c('2016', '2017', '2018', '2019', '2020', '2021', '2022', '2023', '2024')
seasons <- c('2024', '2025')

#
league = leagues[runif(1, min = 1, max = length(leagues))] 
# season = seasons[runif(1, min = 1, max = length(seasons))]



# Function to scrape data for a single league and season
scrape_league_data <- function(league, season) {
  
  url <- paste0(base_url, '/', league, '/', season)
  webpage <- read_html(url)
  
  # Extract the JSON data from the script tags
  scripts <- webpage %>% html_nodes('script')
  string_with_json_obj <- ''
  
  for (el in scripts) {
    if (grepl('teamsData', html_text(el))) {
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
  
  # Get teams and their relevant ids
  teams <- sapply(data, function(x) x$title)
  
  # Get column names and sample values
  columns <- names(data[[1]]$history)
  
  # Getting data for all teams
  dataframes <- lapply(data, function(team_data) {
    df <- team_data[['history']]
    
    df$ppda <- df$ppda %>% 
      mutate(ppda = ifelse(def!= 0, att/def, 0)) %>% 
      select(ppda)
    
    df$ppda_allowed <- df$ppda_allowed %>% 
      mutate(ppda = ifelse(def!= 0, att/def, 0)) %>% 
      select(ppda)
    
    df <- df %>% 
      tidyr::unnest(ppda, ppda_allowed) %>% 
      mutate(league = league, 
               season = season)
    
    df$team <- team_data$title
    
    colnames(df) <- c(columns, c('league', 'season', 'teams'))
    
    return(df)})
  
  dataframes <- data.table::rbindlist(dataframes)
  
}

# Scrape data for all leagues and seasons and combine into a single dataframe
all_data <- map_df(leagues, function(league) {
  map_df(seasons, function(season) {
    scrape_league_data(league, season)
  })
})

# View the combined dataframe
print(all_data)

# Export the data to a CSV file
write.csv(all_data, 'understat_per_game.csv', row.names = FALSE)
