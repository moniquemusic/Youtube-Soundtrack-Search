library(rvest)
library(httr)

#remove the white space in front of game titles
#create custom function to remove white space
trim <- function (x) gsub("^\\s+|\\s+$", "", x)

#create blank lists to store data generated from for-loop
all_game_titles<- c()
all_game_dates<- c()
all_game_scores<- c()
all_game_user_ratings<- c()

years<- as.character(c(2007:2016))

the_page<- "http://www.metacritic.com/browse/games/score/metascore/year/pc/filtered?year_selected=%s&sort=desc"

create_scrape_url <- function(x) {paste0(sprintf(the_page, x))}

metcrit_url_list<- sapply(years, create_scrape_url)

spec<- metcrit_page_list[6]

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

the_specific_page<- "http://www.metacritic.com/game/pc/%s"

create_specific_url <- function(x) {paste0(sprintf(the_specific_page, x))}

game_titles_for_specific<- gsub(":", "",  all_game_titles)
game_titles_for_specific<- gsub("'", "", game_titles_for_specific)
game_titles_for_specific<- gsub("& ", "", game_titles_for_specific)
game_titles_for_specific<- gsub(".", " ", game_titles_for_specific, fixed = TRUE)
game_titles_for_specific<- gsub(",", " ", game_titles_for_specific)




game_titles_for_specific<- tolower(gsub(" ", "-", game_titles_for_specific))


specific_metcrit_url_list<- sapply(game_titles_for_specific, create_specific_url)

specific_test<- specific_metcrit_url_list[1:20]

all_game_specific_critic<- c()

for(specific in specific_metcrit_url_list){
  #set the add_header to "user-agent"
  sp_page_data <- read_html(GET(specific, add_headers('user-agent' = 'r')))
  #if(grepl(pattern = "404 Page Not Found", x = sp_page_data) == TRUE){
    #print(specific)
  #}
  
  
  #get the game titles, then convert titles to html_text
  num_of_critics<- sp_page_data %>% 
    html_nodes(".highlight_metascore .count a") %>%
    html_text() 

  num_of_critics <- gsub(pattern = "\n", replacement = "", x = num_of_critics)
  num_of_critics <- gsub(pattern = "Critics", replacement = "", x = num_of_critics) %>%
    trim() %>%
    as.numeric()
  
  all_game_specific_critic <- append(all_game_specific_critic, num_of_critics)
}
  
game_scores<- page_data %>% 
  html_nodes(".hover_content") %>%
  html_text()



  
  
  
  
#QUESTION: how can this result in anything?
#looking for the first date among the rest of the dates return dates that aren't the same
grep(all_game_dates[1], all_game_dates[-1], fixed = TRUE)

#why can't I chain gsub function?
game_titles_for_specific<- tolower(gsub(" ", "-", game_titles))
#why can't I change that^^^ to this:
game_titles_for_specific<- game_titles %>% 
  gsub(" ", "-") %>%
  tolower()
#Warning message:
#In gsub(., " ", "-") :
  #argument 'pattern' has length > 1 and only the first element will be used

