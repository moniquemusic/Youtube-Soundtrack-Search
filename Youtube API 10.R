#setup
require(curl)
require(jsonlite)
library(dplyr)
library(tidyr)
library(lubridate)
library(rvest)
library(httr)
library(stringdist)

#custom function to extract JSON data from youtube API
get_JSON <- function(x){fromJSON(x)}

#custom function to find the mode (the variable that occurs the most)
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

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

#for-loop that collects data about the top 100 games from each year 
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
game_title_df$all_game_titles <- trim(game_title_df$all_game_titles)

View(game_title_df)





#SEARCH_API
#used youtube search API to get search results when each game title is searched along with the segment "soundtrack"


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


#SEGMENT SEARCH
#search through each game's video titles to find a segment that is the most common amongst the 50 titles
#this segment will most likely be the game title, or a commonly used abbreviation (ie: Battlefield 1 is BF4)

#create another dataframe so the original video titles are unchanged
titles2_df<- as.data.frame(titles_df)

#create a list with the number of games that are being investigated
gg_games<-c(1:ncol(titles2_df))

#remove special characters from video titles
#remove "Soundtrack" from video titles
for(gg_g in gg_games){
  titles2_df[,gg_g] <-gsub(pattern = "\\[", replacement = "", x = titles2_df[,gg_g])
  titles2_df[,gg_g] <-gsub(pattern = "\\]", replacement = "", x = titles2_df[,gg_g])
  titles2_df[,gg_g] <-gsub(pattern = "\\(", replacement = "", x = titles2_df[,gg_g])
  titles2_df[,gg_g] <-gsub(pattern = "\\)", replacement = "", x = titles2_df[,gg_g])
  titles2_df[,gg_g] <-gsub(pattern = "\\:", replacement = "", x = titles2_df[,gg_g])
  titles2_df[,gg_g] <-gsub(pattern = "\\|", replacement = "", x = titles2_df[,gg_g])
  titles2_df[,gg_g] <-gsub(pattern = "\\/", replacement = "", x = titles2_df[,gg_g])
  titles2_df[,gg_g] <-gsub(pattern = "  ", replacement = " ", x = titles2_df[,gg_g])
  titles2_df[,gg_g] <-gsub(pattern = "//?", replacement = "", x = titles2_df[,gg_g])
  titles2_df[,gg_g] <-gsub(pattern = "-", replacement = "", x = titles2_df[,gg_g])
  titles2_df[,gg_g] <-gsub(pattern = "\\{", replacement = "", x = titles2_df[,gg_g])
  titles2_df[,gg_g] <-gsub(pattern = "\\}", replacement = "", x = titles2_df[,gg_g])
  titles2_df[,gg_g] <-gsub(pattern = "'", replacement = "", x = titles2_df[,gg_g])
  titles2_df[,gg_g] <-gsub(pattern = "|", replacement = "", x = titles2_df[,gg_g])
  titles2_df[,gg_g] <-gsub(pattern = "Soundtrack", replacement = "", x = titles2_df[,gg_g])
  titles2_df[,gg_g] <-gsub(pattern = "\\", replacement = "", x = titles2_df[,gg_g], fixed = TRUE)
  titles2_df[,gg_g] <-gsub(pattern = "  ", replacement = " ", x = titles2_df[,gg_g], fixed = TRUE)
}

#there are 50 video titles per game
title_number <- c(1:50)
segment_lengthh <- c(8:12)
segment_startt<- c(1:20)


#create an empty list to store all the best segments into
#best_segments<-c()
best_segments_3 <- c()

for(gg_g in gg_games){
  gg2<- gg_g
  
  #create empty lists to store segments and segment frequency for each game
  segment_collection <- c()
  num_of_exact_matches<- c()
  
  #chop all video titles from a game into segments of different lengths and start points
  #store all created segments into segment_collection
  for(title in title_number){
    title2<- title
    
    min_segment_length<- c(8)
    
    #if the game title is shorter than 8, shrink the segment_length
    if(nchar(colnames(titles2_df[gg2])) < 8){
      segment_lengthh <- c(3:7)
      min_segment_length<-c(3)
    }
    
    for(lengthh in segment_lengthh){
      lengthh2<- lengthh
      
      for(startt in segment_startt){
        segment<-substr(titles2_df[,gg2][title2], start=startt, stop=(startt + lengthh2))
        if(nchar(segment) >= min_segment_length){
          segment_collection <- append(segment_collection, segment)
        }
      }
    }
  }
  
  
  #store the segments minus the repeats into another list
  #this is the list from where each segment will be pulled out from to be tested
  unique_segment_collection<- unique(segment_collection)
  
  #outline
  #the overall goal is now to put unique segments in order of how many times they appear, most to least
  #then store the 5 most frequent segments into a list
  
  #1) unique_segment_collection <-- the segment collection filtered for unique values
  
  #2) num_of_exact_matches <-- each segment from unique_segment_collection is tested for how many exact matches it has within the original segment_collection, and this number is stored
  
  #3) sorted_match_spots <-- num_of_exact_matches are sorted for unique values, then sorted from greatest to least
  #this is the key that allows the unique segments to be grouped by frequency
  
  #4) same_match_level_segments <-- isolate segments by frequency
  #group all the segments that appeared 30 times, all the segments that appeared 20 times, etc
  #from each frequency group, remove the segments that are found in other segments
  #ie: remove "ioshock" because it is found in "Bioshock"
  
  #5) sorted_segments <-- segments are stored in order of greatest frequency to least frequency
  #segments that occure over 200 times or more are not included to reduce running time
  
  #6)best_segments_2 <-- the first 5 segments from sorted_segments that don't occure more than 200 times in all the other game video titles are stored
  
  
  
  
  
  #create a list of how many unique segments exist
  num_of_segments<- c(1:length(unique_segment_collection))
  
  #for-loop to test each segment against the segment_collection
  #the more often the segment from unique_segment_collection appears in the original segment_collection, the more common the segment is
  for(num in num_of_segments){
    
    #use stringdist to get a number on how close the segments match
    match_distance <-stringdist(unique_segment_collection[num], segment_collection)
    
    #collect how many times a segment had an exact match (match_distance = 1)
    num_of_exact_matches<- append(num_of_exact_matches, table(match_distance == 1)["TRUE"])
  }
  
  #sort the number of exact matches between segments in decending order
  sorted_match_spots_2<- unique(sort(num_of_exact_matches, decreasing = TRUE))
  
  #create empty list to store the reuslts of the for-loop into
  sorted_segments<-c()
  
  #for-loop groups segments by frequency and eliminates segments that are smaller sections of other segments
  for(sorted in sorted_match_spots_2){
    #find indicies for the level of frequency
    testtest<- grep(sorted, num_of_exact_matches)
  
    #convert indicies to the actual segmetns
    same_match_level_segments <- unique_segment_collection[testtest]
  
    #only look at frequency levels that have 200 or less segments in them
    if(length(same_match_level_segments) <= 200){
      for(same in same_match_level_segments){
        #only keep segments that only appear once
        if(table(grepl(same, same_match_level_segments, fixed= TRUE))["TRUE"] == 1){
          sorted_segments <- append(sorted_segments, same)
        }
      }
    }
  }  
  
  #create increment meter to limit best segments to 5 per game 
  n<- c(1)
  c<- c(1)
  
  #create a temporary dataframe that contains all the game and their titles minus the game that is currently being inspected
  temp_titles_df<- titles2_df[-gg2]
  
  #create a list of game titles from the temporary dataframe
  all_titles3<- unlist(temp_titles_df[,1:ncol(temp_titles_df)])
 

  #keep n at 5 or under
  #each time a suitable segment term is found, n increases by one
  while(n<= 5){
    
    segment<- sorted_segments[c]
    
    #if searching for the segment within the list off all titles from other games results TRUE
    if(is.na(table(grepl(segment, all_titles3, fixed = TRUE))["TRUE"]) == FALSE){
      
      #store segments that result in TRUE 200 or less times
      if(table(grepl(segment, all_titles3, fixed = TRUE))["TRUE"] <= 200){
        best_segments_3<- append(best_segments_3, segment)
        n <- n + 1
      }
      #if the segment is not found in other games titles they should still be added to the list
    }else{
      best_segments_3<- append(best_segments_3, segment)
      n <- n + 1
    }
    
    c <- c + 1
    if(c == (length(sorted_segments) + 1)){
      print("stop")
      print(n)
      
      if(n == 1){
        best_segments_3<- c(best_segments_3, NA, NA, NA, NA, NA)
      }
      
      if(n == 2){
        best_segments_3<- c(best_segments_3, NA, NA, NA, NA)
      }
      if(n == 3){
        best_segments_3<- c(best_segments_3, NA, NA, NA)
      }
      if(n == 4){
        best_segments_3<- c(best_segments_3, NA, NA)
      }
      if(n == 5){
        best_segments_3<- c(best_segments_3, NA)
      }
      
      n<-6
    }
  }
  print(colnames(titles2_df[gg2]))
}






for(gg_g in gg_games){
  gg2<- gg_g
  
  num2<- gg2 *5
  num1<- num2-4
  
  print(colnames(titles2_df[gg2]))
  print(best_segments_3[num1:num2])
  
}










#STAT_API
#run video titles through a youtube stats API to get view count, like count, dislike count, comment count of each video

#API url to look up stats for videos
yt_stat_search <- "https://www.googleapis.com/youtube/v3/videos?part=statistics&id=%s&key=%s"

#custom function to create url for the stats API, requires etag of video (not the title like the previous API)
create_stats_url <-function(x) {paste0(sprintf(yt_stat_search, x, "AIzaSyCPOy2cpD0SOXEfGDoc2v0riFqtSQ172ow"))}

#stats API require etag of video
#find the corresponding etag for the relevent videos from the original search_JSON dataframe


#grab the section of search_JSON_df that contains the etag
new_2_df <- subset.data.frame(select(search_JSON_df, ends_with("items.id")))

#create function to further reduce dataframe and apply to above dataframe
get_videoID <- function(x){subset.data.frame(select(x, ends_with("videoID")))}

#store all etags into new dataframe
videoID<- mapply(get_videoID, new_2_df)
videoID_df<- data.frame(videoID)

#create dummy list from key_terms_list
best_segments_2_list <- as.list(best_segments_3)

#reset which_key_terms
which_key_term<-c(1:5)

#create an empty list to store dataframes created from running the stats API
list_of_data_frames<- as.list(c())

non_list_of_data_frame<- as.list(c())

#run for-loop that gathers stats for each relevant video
for(gg_g in gg_games){
  gg2<- gg_g
  
  #create an empty list to store video titles that contain one of the terms
  relevent_titles<-c()
  
  #focus which terms should be used as a filter for this particular game
  used_segments <- best_segments_3[which_key_term]
  
  segment_valid_collection<- c() 
  used_segment_additions<-c()
  
  for(used in used_segments){
    #look for used_key_term in colname of title2_df
    #grepl will result in TRUE if segment is found
    segment_valid_collection <- append(segment_valid_collection, grepl(used, colnames(titles2_df[gg2]), ignore.case = TRUE))
  }
  
  #if searching for TRUE in segment_valid_collection results in a NA, then that means none of the segments were found within the game title
  if(is.na(table(grepl("TRUE", segment_valid_collection))["TRUE"]) == TRUE){
    
    #created segments fromg game title of varying lenght and starting location
    for(lengthh in segment_lengthh){
      lengthh2<- lengthh
      
      for(startt in segment_startt){
        segment<-substr(colnames(titles2_df[gg2]), start=startt, stop=(startt + lengthh2))
        used_segment_additions <- append(used_segment_additions, segment)
      }  
    }
    
    #remove whitespace at the begining of each segment
    
    used_segment_additions<- trim(used_segment_additions)
    
    #filter for unique used_segment_additions
    unique_additions<- unique(used_segment_additions)
    
    used_segment_additions_2<-c()
    
    for(unique in unique_additions){
      unique2<- unique
      
      #if when searching for the segment in all the other game titles results in TRUE
      if(is.na(table(grepl(unique2, all_titles3, fixed = TRUE))["TRUE"]) == FALSE){
    
        #store segments that result in TRUE 200 or more times
        if(table(grepl(unique2, all_titles3, fixed = TRUE))["TRUE"] <= 200){
          used_segment_additions_2 <- append(used_segment_additions_2, unique2)
          
        }
        #segments that don't appear in other games's titles should still be used
      }else{
        used_segment_additions_2 <- append(used_segment_additions_2, unique2)
      }
    }
    used_segments <- used_segment_additions_2 
  }
  
   
  
  #reduce the list of video titles to titles that contain the key_term
  if(length(which(is.na(used_segments))) < 5){
    
    #narrow the list of video titles in titles_df to the ones that contain the used_segments (from the key_terms_list)
    for (used in used_segments){
      relevent_title_spots<- grep(used, titles2_df[,gg2], ignore.case = TRUE)
      relevent_titles<- append(relevent_titles, relevent_title_spots)
    }
  }
    
  if(length(relevent_titles) > 0){
    #filter the list of relevent titles indices for unique occurances
    key_title_spots <- unique(relevent_titles)
    #translate the indicies to actual video titles that will be used
    optimal_titles <- titles_df[,gg_g][key_title_spots]
      
    #turn the list of used video titles into a dataframe
    holder_data_frame<- data.frame(optimal_titles, stringsAsFactors = FALSE)
    holder_data_frame <- holder_data_frame %>% mutate(videoID=videoID_df[,gg_g][key_title_spots])
      
    #below is code to check if the titles and etag match up, work-in-progress
    #chichi<-c(1:nrow(holder_data_frame))
      
    #gg2<- gg_g
      
    #for (chi in chichi) {
    #if(holder_data_frame$optimal_titles[chi] == titles_df[,gg2][chi]){
    #gg3<-gg2
    #chi2<- chi
    #if(holder_data_frame$videoID[chi2] != videoID_df[,gg3][chi2]){
    #print("ERROR, MISMATCH")
    #print(chi2)
    #print(holder_data_frame$optimal_titles[chi2])
    #break
    #}
    #}
    #}
      
    #create list of urls to use from the etag column of holder_data_frame
    stats_url_list <- sapply(holder_data_frame[2], create_stats_url)
      
    #use urls that were created and JSON function to scrape Youtube Stats API for data
    stats_JSON <- sapply(stats_url_list, get_JSON)
      
    stats_JSON_df <- data.frame(stats_JSON)
      
    #manipulate stats_JSON_df 
      
      
    #transpose the stats dataframe to put video titles as rows
    stats_JSON_df<-as.data.frame(t(stats_JSON_df))
      
    #carve out section stats_JSON_df with subset that contains the stats
    stats <- subset.data.frame(select(stats_JSON_df, ends_with("items")))
      
    bahaha<-c(1:nrow(stats))
    stats_collection <- data.frame()
    
    #even after carving out "items" column from stats JSON, the results are still clumped together
    #use for-loop to separate stats JSON results into their separate components (viewCount, likeCount, etc) and store it in a matrix
    for (bah in bahaha) {
      vid_stat<-as.character(stats$items[bah])
        
      vid_stat_sep<- unlist(strsplit(vid_stat, split="\\,"))
        
      vid_stat_matrix <- matrix(data = vid_stat_sep[4:8], nrow = 5, ncol=1, byrow=TRUE)
        
      vid_df<- as.data.frame(vid_stat_matrix)
        
      stats_collection<- append(stats_collection, vid_df)
    }
      
    #store the matrix generated above into a dataframe
    stats_collection_df<- as.data.frame(stats_collection)
    colnames(stats_collection_df)<- holder_data_frame[1:nrow(holder_data_frame), 2]
      
    #when forming the matrix, sometimes the stats_JSON doesn't have anything for a certain stat for a video, and this messes up the data.
    #example: if a video returns nothing for "favoriteCount", commentCount" will be moved up a row in the matrix where "favoriteCount" should be
    #and the last spot on the matrix where "CommentCount" was supposed to be has an NA
    #we need the fourth row to contain NA, and the fifth row to contain "CommentCount"
      
    #rearrange stats_collection so that stats are in their appropriate rows
      
    #find the columns of stats_collection_df that contain NA
    #sum up the number of NAs per column
    NA_per_column<- colSums(is.na(stats_collection_df))
      
    #find all the columns that don't have any NAs
    non_empty_spots<-grep(pattern = 0, NA_per_column)
      
    #find columns with NAs by subtracting all the columns that don't have NAs
    lacking_columns<- c(1:ncol(stats_collection_df))[-non_empty_spots]  
    
    #create a list (go_test_row) of the objects in the lacking column
    #replace the NAs in go_test_row with term "placeholder" to prevent errors with using function: startsWith
    #replace every row in the lacking column with NA
    #re-fill the column by matching what the object in go_test_row list is with the appropriate row
      
    #only deal with lacking_columns that have something in them, otherwise the entire column should be NAs
    if(sum(lacking_columns) > 0){
      for (lacking in lacking_columns) {
          
        go_test_row<- as.character(stats_collection_df[,lacking])
        #replace the NAs in go_test_row with term "placeholder" to prevent errors with using function: startsWith
        go_test_row[grep(TRUE, is.na(go_test_row))] <- "placeholder"
        #ceate a blank slate, replace every row in the lacking column with NA, original values are stored in go_test_row
        stats_collection_df[,lacking]<- NA
          
        for(go_test in go_test_row){
            #if the object in go_test_row starts with "statistics", it should be placed in the first row of the column
            if(startsWith(go_test, " statistics") == TRUE){
              stats_collection_df[1,lacking]<- as.character(go_test)
            }
            #if the object in go_test_row starts with "likeCount", it should be placed in the second row of the column, etc, etc
            if(startsWith(go_test, " likeCount") == TRUE){
              stats_collection_df[2,lacking]<- as.character(go_test)
            }
            if(startsWith(go_test, " dislikeCount") == TRUE){
              stats_collection_df[3,lacking]<- as.character(go_test)
            }
            if(startsWith(go_test, " favoriteCount") == TRUE){
              stats_collection_df[4,lacking]<- as.character(go_test)
            }
            if(startsWith(go_test, " commentCount") == TRUE){
              stats_collection_df[5,lacking]<- as.character(go_test)
            }
          }
        }
      }
      
      #collect all the stats from test_data_fram_df into their appropriate lists (viewCount, likeCount, etc) to mass remove extra symbols
      number_of_videos <- c(1:ncol(stats_collection_df))
      
      viewCount_list<- c()
      likeCount_list<- c()
      dislikeCount_list<-c()
      favoriteCount_list<- c()
      commentCount_list<-c()
      
      for(num in number_of_videos){
        viewCount_list<- append(viewCount_list, as.character(stats_collection_df[1, num]))
        likeCount_list<- append(likeCount_list, as.character(stats_collection_df[2, num]))
        dislikeCount_list<- append(dislikeCount_list, as.character(stats_collection_df[3, num]))
        favoriteCount_list<- append(favoriteCount_list, as.character(stats_collection_df[4, num]))
        commentCount_list<- append(commentCount_list, as.character(stats_collection_df[5, num]))
      }
      
      #clean up the data
      #remove " statistics = list(viewCount = \"" and replace with ""
      viewCount_list<- gsub(pattern =" statistics = list(viewCount = \"", "", viewCount_list, fixed = TRUE)
      viewCount_list<- gsub(pattern ="\"", "", viewCount_list, fixed = TRUE)
      
      likeCount_list<- gsub(pattern =" likeCount = \"", "", likeCount_list, fixed = TRUE)
      likeCount_list<- gsub(pattern ="\"", "", likeCount_list, fixed = TRUE)
      
      dislikeCount_list<- gsub(pattern =" dislikeCount = \"", "", dislikeCount_list, fixed = TRUE)
      dislikeCount_list<- gsub(pattern ="\"", "", dislikeCount_list, fixed = TRUE)
      
      favoriteCount_list<- gsub(pattern =" favoriteCount = \"", "", favoriteCount_list, fixed = TRUE)
      favoriteCount_list<- gsub(pattern ="\"", "", favoriteCount_list, fixed = TRUE)
      
      commentCount_list<- gsub(pattern =" commentCount = \"", "", commentCount_list, fixed = TRUE)
      commentCount_list<- gsub(pattern ="))", "", commentCount_list, fixed = TRUE)
      commentCount_list<- gsub(pattern ="\"", "", commentCount_list, fixed = TRUE)
      
      
      
      #add stat data from the lists to the holder_data_frame
      holder_data_frame<- holder_data_frame %>% mutate(viewCount = viewCount_list)
      holder_data_frame<- holder_data_frame %>% mutate(likeCount = likeCount_list)
      holder_data_frame<- holder_data_frame %>% mutate(dislikeCount = dislikeCount_list)
      holder_data_frame<- holder_data_frame %>% mutate(favoriteCount = favoriteCount_list)
      holder_data_frame<- holder_data_frame %>% mutate(commentCount = commentCount_list)
      
      #create a title for the holder_data_frame that reflects what game it has data of
      stats_df_lable <- paste0(gsub(pattern=" ", replacement= "_", x=colnames(titles_df[
        gg_g])), "_stats_df")
      
      #use this line to keep track of which game is being done as the for-loop is run
      print(stats_df_lable)
      
      #rename the column names the represent what the data in the column is
      colnames(holder_data_frame)<- c(colnames(titles_df[gg_g]), "etag", "viewCount", "likeCount", "dislikeCount", "favoriteCount", "commentCount")
      
      #fill the empty list with the dataframe to be used later for manipulation and analysis
      list_of_data_frames[[gg_g]]<- data.frame(holder_data_frame)
      
      #name each object in the list_of_data_frames so we know which dataframe is which game
      names(list_of_data_frames[gg_g]) <- stats_df_lable 
      
      #increase which_key_term by 5 so the next 5 terms in the key_terms_list can be used with the next game
      which_key_term<- which_key_term +5
      
    }else{
    
    non_list_of_data_frame<- append(non_list_of_data_frame, colnames(titles2_df[gg2]))
        
    #increase which_key_term by 5 so the next 5 terms in the key_terms_list can be used with the next game
    which_key_term<- which_key_term + 5
    
    #a game that the youtube search API can't return related videos is a game that unfortunately has to be thrown out of the study
    #add the game's number to bad_line so we can update the original game_title_df and remove the game from it
    bad_line<- c(bad_line, gg2)
  }
}

#there are empty spots in list_of_data_frames due to some games not returning anything cohesive from the search API
#get ride of these spots by using the non_list_of_data_frames that was created in the previous for-loop
#create list to store the indicies of the empty spots
bad_line_3<-c()

#for-loop finds empty spots by finding the indicies of the game name from non_list_of_data_frame from the column names of titles2_df
for(non in non_list_of_data_frame){
  bad_line_3<- append(bad_line_3, grep(non, colnames(titles2_df)))
}

bad_line_4 <- c(bad_line, bad_line_3)

bad_line_4 <- unique(bad_line_4)
       
#remove the games that had incomplete results from the search API                                                                                                                                                                                                                                                                                                                                                                                                                             #create a dataframe with the final list of games to be analyzed 
final_game_title_df <- data.frame(game_title_df[-bad_line_4,])

#dataframe row indexes are removed when row is removed
#if two games were removed, this results in the 98th game still being labeled the 100th index
#reassign the dataframes index appropriately 
rownames(final_game_title_df) <- 1:nrow(final_game_title_df)

game_title_df[4,]
colnames(titles2_df[4])
final_game_title_df[4,]

#add the average of the stats found through the youtube stats API to the original games_titles_df dataframe

#make sure the gg_games is updated to represent any changes made in the list of games
gg_games_2<- c(1: nrow(final_game_title_df))

#create a dataframe about the stats overall
#create an empty dataframe to store averaged stats into
game_stats_df<- data.frame()

#run for loop the get the total views, average views, like, dislikes, and comment count of all the videos for each game
for(gg_g in gg_games_2){
  game_data_frame <- data.frame(list_of_data_frames[gg_g])
  
  video_num<- nrow(game_data_frame)
  total_views<- sum(as.numeric(game_data_frame$viewCount), na.rm = TRUE) 
  total_likes<- sum(as.numeric(game_data_frame$likeCount), na.rm = TRUE)
  total_dislikes<- sum(as.numeric(game_data_frame$dislikeCount), na.rm = TRUE)
  total_favorites<- sum(as.numeric(game_data_frame$favoriteCount), na.rm = TRUE)
  total_comments<- sum(as.numeric(game_data_frame$commentCount), na.rm = TRUE)
  
  #game_data_line<- list(video_num, total_views, total_likes, total_dislikes, total_favorites, total_comments)
  
  game_stats_df[gg_g, 1]<- video_num
  game_stats_df[gg_g, 2]<- total_views
  game_stats_df[gg_g, 3]<- total_likes
  game_stats_df[gg_g, 4]<- total_dislikes
  game_stats_df[gg_g, 5]<- total_favorites
  game_stats_df[gg_g, 6]<- total_comments
  
  print(colnames(titles_df[gg_g]))
}

#label game_stats_df's columns appropriately
colnames(game_stats_df)<- c("video_num", "total_views", "total_likes", "total_dislikes", "total_favorites", "total_comments")

#merge dataframes so that game titles, game release dates, and average stats are all under one dataframe
final_game_title_df<- cbind(final_game_title_df, game_stats_df) 

View(final_game_title_df)



