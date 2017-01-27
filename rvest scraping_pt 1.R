#setup
require(curl)
require(jsonlite)
library(dplyr)
library(tidyr)
library(lubridate)
library(rvest)
library(httr)

#custom function to extract JSON data from youtube API
get_JSON <- function(x){fromJSON(x)}

#custom function to find the mode (the variable that occurs the most)
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

#remove the white space in front of game titles
#create custom function to remove white space
trim <- function (x) gsub("^\\s+|\\s+$", "", x)

#create blank lists to store data generated from for-loop
all_game_titles<- c()
all_game_dates<- c()
all_game_scores<- c()
all_game_user_ratings<- c()

#decide from what years to get the games from
years<- as.character(c(2007:2016))

#the basic metacritic page to get information from, only needs the year as an input
the_page<- "http://www.metacritic.com/browse/games/score/metascore/year/pc/filtered?year_selected=%s&sort=desc"

#custom function to create scraping url by inputting the year
create_scrape_url <- function(x) {paste0(sprintf(the_page, x))}

#create list of urls to run through JSON
metcrit_url_list<- sapply(years, create_scrape_url)

for(metcrit in metcrit_url_list){
  #set the add_header to "user-agent"
  page_data <- read_html(GET(metcrit, add_headers('user-agent' = 'r')))
  
  #get the game titles, then convert titles to html_text
  game_titles<- page_data %>% 
    html_nodes("#main .product_title") %>%
    html_text() %>%
    as.character()
  
  #store the number of games scraped
  number_of_games<- (1:length(game_titles))
  
  #use for-loop to clean up the list of titles
  for(number in number_of_games){
    number2<- number
    game_titles[number2]<- gsub(pattern = "\n", replacement = "", x = game_titles[number2]) %>%
      trim()  
  }
  
  #get the game release dates, then convert dates to html_text, then year-month-date format
  game_dates<- page_data %>% 
    html_nodes(".product_date") %>%
    html_text() %>%
    mdy()
  
  #make sure all rows are accounted for
  if(length(game_dates) != length(number_of_games)){
    print(game_titles[1])
  }

  #get the game score, then convert score to html_text, then numeric data
  game_scores<- page_data %>% 
    html_nodes(".small") %>%
    html_text() %>%
    as.numeric()
  
  #make sure all rows are accounted for
  if(length(game_scores) != length(number_of_games)){
    print(game_titles[1])
  }
  
  #get the game user rating, then convert rating to html_text, then numeric data
  game_user_ratings<- page_data %>% 
    html_nodes("#main .textscore") %>%
    html_text() %>%
    as.numeric()
  
  #make sure all rows are accounted for
  if(length(game_user_ratings) != length(number_of_games)){
    print(game_titles[1])
  }
  
  #add game_titles and game_dates to the bigger list
  all_game_titles<- append(all_game_titles, game_titles)
  all_game_dates <- append(all_game_dates, game_dates)
  all_game_scores <- append(all_game_scores, game_scores)
  all_game_user_ratings<- append(all_game_user_ratings, game_user_ratings)
}

#create dataframe to contain game titles and their release dates
#later add all_number_of_critics and all_number_of_users
game_title_df <-data.frame(all_game_titles, all_game_dates, all_game_scores, all_game_user_ratings)

#remove the white space in front of game titles
#create custom function to remove white space
trim <- function (x) gsub("^\\s+|\\s+$", "", x)
#update game_title_df with new game titles
game_title_df$all_game_titles <- trim.leading(game_title_df$all_game_titles)

View(game_title_df)