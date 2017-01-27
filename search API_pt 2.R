#SEARCH_API
#used youtube search API to get search results when each game title is searched along with the term "soundtrack"


#assign the API url to yt_search
yt_search <- "https://www.googleapis.com/youtube/v3/search?part=snippet&maxResults=50&order=relevance&publishedBefore=%s%%3A00%%3A00Z&q=%s+soundtrack+full+-cover+-radio&type=video&videoDuration=long&key=%s"

#custom function pastes elements(game titles) into the proper places within the search API url and creates a useable url
create_search_url <- function(x) {paste0(sprintf(yt_search, "2017-06-24T00", x, "AIzaSyCPOy2cpD0SOXEfGDoc2v0riFqtSQ172ow"))}

#ceate a list of game titles from 2016 to be the elements inserted into the search API
games <- game_title_df$all_game_titles
#replace spaces with "+" so it works within the url
games <- gsub(pattern=" ", replacement= "+", x=games)

#create a list of urls to run through the get_JSON function
search_url_list <- sapply(games, create_search_url)

#run the urls through the get_JSON data to get the JSON data from the youtube search API
search_JSON <- sapply(search_url_list, get_JSON)

#not all urls run will return the right amount of data
#find which search_JSON elements have the wrong number of components
#search_JSON has ~100 entries due to there being 100 games tested per year for 10 years
rows<-c(1:nrow(game_title_df))

#store the entry length each game return via search API
row_length_list<- c()

for(row in rows){
  
  row_length_list<- append(row_length_list, lengths(search_JSON[row]))
}

#find the entry length most games return
optimal_entry_length <-Mode(row_length_list)

bad_line <-c()

#remove games that don't return the optimal entry length
for(row in rows){
  
  row_length <- lengths(search_JSON[row])
  other_row_length<- length(search_JSON[row])
  
  if(row_length != optimal_entry_length){
    bad_line<- append(bad_line, row)
  }
  if(other_row_length != 1){
    bad_line<- append(bad_line, row)
  }
}

row2<- rows[-bad_line]
bad_line_2<- c()

for(row in row2){
  temp <- as.data.frame(search_JSON[row])
  
  if(nrow(temp) != 50){
    bad_line<- append(bad_line, row)
  }
}


#remove inconsistent lines in search_JSON, turn search_JSON to dataframe with bad lines removed
search_JSON_df <- data.frame(search_JSON[-bad_line])

#.items.snippet column contains the titles of the returned youtube videos
#create dataframe with just the ".items.snippet" column
new_df <- subset.data.frame(select(search_JSON_df, ends_with("snippet")))

#nested within the .items.snippet column is the "title" column
#create function to specify the "title" from ".item.snippet"
get_title <- function(x){subset.data.frame(select(x, starts_with("title")))}
#store "title" information into "titles"
titles <- mapply(get_title, new_df, SIMPLIFY = FALSE)

#turn "titles" into a clean dataframe
#turn titles into dataframe
titles_df <- data.frame(titles)

#column names for titles_df is currently title, title1, title2, title3, etc...
#rename column names for titles_df with the name of the game

#create list of game titles from new_df
game_titles_list <- colnames(new_df)
#remove .items.snippet.title from the end of each element of titles_df
game_titles_list <- gsub(".items.snippet", "", x= game_titles_list)
#replace "." with " " from each row
game_titles_list <- gsub("\\.", " ", x= game_titles_list)
game_titles_list <- gsub("  ", " ", x= game_titles_list)
#apply game titles as title_df's new column names
colnames(titles_df) <- game_titles_list

View(titles_df)