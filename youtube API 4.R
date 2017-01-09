#RUN FROM LINE 3 to LINE 74

#setup
require(curl)
require(jsonlite)
library(dplyr)
library(tidyr)
library(lubridate)

get_JSON <- function(x){fromJSON(x)}

Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}


#create games list
#read the original metacritic page
thepage <- readLines("http://www.metacritic.com/browse/games/score/metascore/year/pc/filtered?year_selected=2016")

#create funtions to read the specific line for game title and game release date


find_title <- function(x){thepage[26*(x-1)+573]}
find_date <- function(x){thepage[26*(x-1)+583]}

#focus on the top 100 games
top_100_games <- c(1:100)

#scrape for top 100 game titles and their corresponding release dates
game_title <- sapply(top_100_games, find_title)


#the correct html line on the metacritic webpage fluxuates between 573 and 571
#used if function to adjust as necessary, rerun lines 19 through 26
if(game_title[1] == "            </div>"){
  
  find_title <- function(x){thepage[26*(x-1)+571]}
  find_date <- function(x){thepage[26*(x-1)+581]}
  
  top_100_games <- c(1:100)
  
  game_title <- sapply(top_100_games, find_title)
  game_date <- sapply(top_100_games, find_date)
}

if(game_title[1] == "                                    </a>"){
  
  find_title <- function(x){thepage[26*(x-1)+572]}
  find_date <- function(x){thepage[26*(x-1)+582]}
  
  top_100_games <- c(1:100)
  
  game_title <- sapply(top_100_games, find_title)
  game_date <- sapply(top_100_games, find_date)
}

if(game_title[1] == "            <div class=\"product_item row_num\">"){
  
  find_title <- function(x){thepage[26*(x-1)+557]}
  find_date <- function(x){thepage[26*(x-1)+567]}
  
  top_100_games <- c(1:100)
  
  game_title <- sapply(top_100_games, find_title)
  game_date <- sapply(top_100_games, find_date)
}


if(game_title[1] == "                <a href=\"/game/pc/the-witcher-3-wild-hunt---blood-and-wine\">"){
  
  find_title <- function(x){thepage[26*(x-1)+574]}
  find_date <- function(x){thepage[26*(x-1)+584]}
  
  top_100_games <- c(1:100)
  
  game_title <- sapply(top_100_games, find_title)
  game_date <- sapply(top_100_games, find_date)
}

if(game_title[1] == "                                    "){
  
  find_title <- function(x){thepage[26*(x-1)+555]}
  find_date <- function(x){thepage[26*(x-1)+565]}
  
  top_100_games <- c(1:100)
  
  game_title <- sapply(top_100_games, find_title)
  game_date <- sapply(top_100_games, find_date)
}

if(game_title[1] == "            <div class=\"product_item product_title\">"){
  
  find_title <- function(x){thepage[26*(x-1)+559]}
  find_date <- function(x){thepage[26*(x-1)+569]}
  
  top_100_games <- c(1:100)
  
  game_title <- sapply(top_100_games, find_title)
  game_date <- sapply(top_100_games, find_date)
}

if(game_title[1] == "                    <div class=\"product_row game\">"){
  
  find_title <- function(x){thepage[26*(x-1)+560]}
  find_date <- function(x){thepage[26*(x-1)+570]}
  
  top_100_games <- c(1:100)
  
  game_title <- sapply(top_100_games, find_title)
  game_date <- sapply(top_100_games, find_date)
}

#When one of the if statements above does not turn out the game title like expected
#which_line <- c(540:640)
#for(wwhich in which_line){
  #print(wwhich)
  #print({thepage[26*(1-1)+wwhich]})
  #}



#create data frame to contain game titles
game_title_df <-data.frame(game_title, game_date)
View(game_title_df)

#remove the white space in front of game titles
trim.leading <- function (x)  sub("^\\s+", "", x)

game_title_df$game_title <- trim.leading(game_title_df$game_title)

#change date format from Month, Day, Year to Year-Month-Day

game_title_df$game_date <- mdy(game_title_df$game_date)
View(game_title_df)



#SEARCH_API



#basic search url API
yt_search <- "https://www.googleapis.com/youtube/v3/search?part=snippet&maxResults=50&order=relevance&publishedBefore=%s%%3A00%%3A00Z&q=%s+soundtrack+full+-cover+-radio&type=video&videoDuration=long&key=%s"

#function to paste elements into the search API url
create_search_url <- function(x) {paste0(sprintf(yt_search, "2017-06-24T00", x, "AIzaSyCPOy2cpD0SOXEfGDoc2v0riFqtSQ172ow"))}

#create_search_url <- function(x, y) {paste0(sprintf(yt_search, y, x, "AIzaSyCPOy2cpD0SOXEfGDoc2v0riFqtSQ172ow"))}

#list of game titles from 2016
games <- game_title_df$game_title
#replace spaces with "+" so it works within the url
games <- gsub(pattern=" ", replacement= "+", x=games)

#create list of urls to use
search_url_list <- sapply(games, create_search_url)

#use urls that were created and JSON function to scrape Youtube API for data
search_JSON <- sapply(search_url_list, get_JSON)

#find which search_JSON elements have the wrong number of components
rows<-c(1:100)

bad_line <-c()

for(row in rows){
  
  row_length<- lengths(search_JSON[row])
  
  if(row_length != 6){
    bad_line<- append(bad_line, row)
  }
}


#inconsistent lines in search_JSON, turn search_JSON to dataframe with bad lines removed
search_JSON_df <- data.frame(search_JSON[-bad_line])

#create dataframe with just the .items.snippet column
new_df <- subset.data.frame(select(search_JSON_df, ends_with("snippet")))

#create function to specify the title column of each game's .item.snippet
get_title <- function(x){subset.data.frame(select(x, starts_with("title")))}
titles <- mapply(get_title, new_df, SIMPLIFY = FALSE)

#turn titles list into dataframe and clean up
#turn titles into dataframe
titles_df <- data.frame(titles)

#column names for titles_df is currently title, title1, title2, title3, etc...
#rename column names for titles_df

#create list of game titles from new_df
game_titles_list <- colnames(new_df)
#remove .items.snippet.title from the end of each element
game_titles_list <- gsub(".items.snippet", "", x= game_titles_list)
#replace "." with " " from each row
game_titles_list <- gsub("\\.", " ", x= game_titles_list)
game_titles_list <- gsub("  ", " ", x= game_titles_list)



#apply game titles as title_df's new column names
colnames(titles_df) <- game_titles_list

View(titles_df)

#TERM_SEARCH

#reducing video list to relevent videos only

#create dummy dataframe to search for relevent videos
gg_games<- c(1:length(colnames(titles_df)))
titles2_df<- as.data.frame(titles_df)

#removing special characters from titles
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
}



for(gg_g in gg_games){
  for(title in title_number){
    if(grepl("\\(", titles2_df[,gg_g][title]) == TRUE){
      print(titles2_df[,gg_g][title])
      print(grep("\\(", titles2_df[,gg_g][title]))
    }
  }
}





#create for loop to get the top 5 terms that are repeated in the set of titles per game
title_number<- c(1:50)
term_lengthh <- c(8:12)
term_startt <- c(1:20)
matching_term <-c()
term_startt2 <- c(1:20)
most_matches<-c()
key_terms<-c()

for(gg_g in gg_games){
  matching_term<-c()
  gg2<- gg_g
  for(title in title_number){
    
    if(nchar(colnames(titles2_df[gg2])) < 7){
      print("game title is shorter than 8 char")
      print(colnames(titles2_df[gg2]))
      term_lengthh <- c(3:6)
    }else{term_lengthh<- c(8:12)}
    
    for(lengthh in term_lengthh){
      for(startt in term_startt){
        
        test1 <-substr(titles2_df[,gg2][title], start=startt, stop=(startt + lengthh))
        
        title_number_2 <- title_number[-title]
        lengthh2 <- lengthh
        
        for(title2 in title_number_2){
          for(startt2 in term_startt2){
            test2 <-substr(titles2_df[,gg2][title2], start=startt2, stop=(startt2 + lengthh2))
            if(test1 == test2){
              matching_term <- append(matching_term, test2)
            }
          }
        }
      }
    }
  }
  unique_terms <- unique(matching_term)
  #print(unique_terms)
  most_matches<-c()
  for(u in unique_terms){
    if(grepl(u, titles2_df[gg2]) == TRUE)
      most_matches<- append(most_matches, length(grep(u, matching_term)))
  }
  
  
  #best_terms <- unique_terms[grep(max(nchar(unique_terms)), nchar(unique_terms), ignore.case= TRUE)]
  
  best_term_lable <- paste0(gsub(pattern=" ", replacement= "_", x=colnames(titles2_df[
    gg_g])), "_best_terms")
  print(best_term_lable)
  #top 5 times the term matched the most
  temp_most_matches<- unique(most_matches)
  best_terms<- sort((temp_most_matches),decreasing=TRUE)[1:5]
  
  #print(best_terms)
  
  find_best_terms<- function(x){unique_terms[grep(x, most_matches)]}
  
  #find_best_terms<- function(x){unique_terms[grep(x, most_matches)]}
  
  bessssst<- sapply(best_terms, find_best_terms)
  
  #best_term_within_bessssst<- c()
  
  #for(bes in bees){
    #bes1<- bes
    #if(length(unlist(bes1)) > 1){
      #bes_unravel<- unlist(bes1)
      #print(bes_unravel)
      #print(length(bes_unravel))
      #for (bes_u in bes_unravel){
        #if(grepl(bes_u, colnames(titles2_df[gg2])) == TRUE){
          #best_term_within_bessssst<- append(best_term_within_bessssst, bes_u)
        #}
      #}
    #}
 #}
  
  if(length(bessssst) == 0){
    fillin<- c(NA, NA, NA, NA, NA)
    bessssst<- append(bessssst, fillin)
    bessssst<- unlist(bessssst)
  }
  if(length(bessssst) == 1){
    fillin<- c(NA, NA, NA, NA)
    bessssst<- append(bessssst, fillin)
    bessssst<- unlist(bessssst)
  }
  if(length(bessssst) == 2){
    fillin<- c(NA, NA, NA)
    bessssst<- append(bessssst, fillin)
    bessssst<- unlist(bessssst)
    
  }
  if(length(bessssst) == 3){
    fillin<- c(NA, NA)
    bessssst<- append(bessssst, fillin)
    bessssst<- unlist(bessssst)
    
  }
  if(length(bessssst) == 4){
    fillin<- c(NA)
    bessssst<- append(bessssst, fillin)
    bessssst<- unlist(bessssst)
    
  }
  
  key_terms<- append(key_terms, bessssst)
  #print(key_terms)
  #print(bessssst)
  #print(assign(best_term_lable, value = best_terms))
}


key_terms_list <- as.list(key_terms)
which_key_term <- c(1:5)

all_games2 <- unlist(colnames(titles2_df))
all_titles2<- unlist(titles2_df[,1:length(colnames(titles_df))])

suspect_term<- c()
term_occurance<- c()
example_title<- c()

for(gg_g in gg_games){
  
  #create temp titles_df that doesn't contain the column that the key_term belongs to
  temp_titles_df<- titles2_df[-gg_g]
  
  #create a list of all video titles from all games from the temp titles_df
  all_titles3<- unlist(temp_titles_df[,1:ncol(temp_titles_df)])
  
  gg2<- gg_g
  
  print(colnames(titles2_df[gg2]))
  for(which in which_key_term){
    which2<- which
    print(key_terms_list[which2])
    if(is.na(key_terms_list[which2]) == FALSE){
      if(nchar(key_terms_list[which2]) > 2 ){
        if(table(grepl(key_terms_list[which2], all_games2))["FALSE"] == 98){
          if(length(grep(key_terms_list[which2], all_titles3)) > 0){
            suspect_term<- unlist(append(suspect_term, key_terms_list[which2]))
            term_occurance<- unlist(append(term_occurance, length(grep(key_terms_list[which2], all_titles3))))
            example_title<- unlist(append(example_title, all_titles3[(grep(key_terms_list[which2], all_titles3))[1]]))
          }
        }
      }
    }
  }
  which_key_term <- which_key_term + 5
}

suspect_terms_df<- data.frame(suspect_term, term_occurance, example_title)
colnames(suspect_terms_df) <- c("term", "number_of_occurances", "title_example") 
View(suspect_terms_df)

problem_terms_df<- subset(suspect_terms_df, number_of_occurances > 50, select=term)
problem_terms<- as.vector(problem_terms_df[,1])

redo_game_spots<-c()

for(problem_t in problem_terms){
  #find where in key_terms_list the problem_term comes from
  term<- problem_t
  problem_spot<- grep(term, key_terms_list)
  
  #correlate key_term_list location to find which games the problem_term comes from
  for(problem_s in problem_spot){
    spot <- problem_s
    if((spot %% 5) != 0){
      issue_game_spot<- spot %/% 5 + 1
    }else{
      issue_game_spot<- spot %/% 5
    }
    titles2_df[, issue_game_spot]<- gsub(term, "", titles2_df[,issue_game_spot])
    
    redo_game_spots <- append(redo_game_spots, issue_game_spot) 
  }
}

redo_game<-unique(redo_game_spots)

title_number<- c(1:50)
term_lengthh <- c(8:15)
term_startt <- c(1:20)
matching_term <-c()
term_startt2 <- c(1:20)
most_matches<-c()

for(redo in redo_game){
  matching_term<-c()
  redo2<- redo
  for(title in title_number){
    
    if(nchar(colnames(titles2_df[redo2])) < 7){
      #print("game title is shorter than 8 char")
      #print(colnames(titles2_df[redo2]))
      term_lengthh <- c(3:6)
    }else{term_lengthh<- c(8:15)}
    #print(lengthh)
    for(lengthh in term_lengthh){
      for(startt in term_startt){
        
        test1 <-substr(titles2_df[,redo2][title], start=startt, stop=(startt + lengthh))
        
        title_number_2 <- title_number[-title]
        lengthh2 <- lengthh
        
        for(title2 in title_number_2){
          for(startt2 in term_startt2){
            test2 <-substr(titles2_df[,redo2][title2], start=startt2, stop=(startt2 + lengthh2))
            if(test1 == test2){
              matching_term <- append(matching_term, test2)
            }
          }
        }
      }
    }
  }
  unique_terms <- unique(matching_term)
  print(unique_terms)
  most_matches<-c()
  for(u in unique_terms){
    if(grepl(u, colnames(titles2_df[redo2])) == TRUE)
      most_matches<- append(most_matches, length(grep(u, matching_term)))
  }
  most_matches<- append(most_matches, length(grep(u, matching_term)))
  
  #best_terms <- unique_terms[grep(max(nchar(unique_terms)), nchar(unique_terms), ignore.case= TRUE)]
  
  best_term_lable <- paste0(gsub(pattern=" ", replacement= "_", x=colnames(titles2_df[
    gg_g])), "_best_terms")
  print(best_term_lable)
  #top 5 times the term matched the most
  best_terms<- sort((most_matches),decreasing=TRUE)[1:5]
  
  #print(best_terms)
  
  find_best_terms<- function(x){unique_terms[grep(x, most_matches)]}
  
  bessssst<- sapply(best_terms, find_best_terms)
  if(length(bessssst) == 0){
    fillin<- c(NA, NA, NA, NA, NA)
    bessssst<- append(bessssst, fillin)
    bessssst<- unlist(bessssst)
  }
  if(length(bessssst) == 1){
    fillin<- c(NA, NA, NA, NA)
    bessssst<- append(bessssst, fillin)
    bessssst<- unlist(bessssst)
  }
  if(length(bessssst) == 2){
    fillin<- c(NA, NA, NA)
    bessssst<- append(bessssst, fillin)
    bessssst<- unlist(bessssst)
    
  }
  if(length(bessssst) == 3){
    fillin<- c(NA, NA)
    bessssst<- append(bessssst, fillin)
    bessssst<- unlist(bessssst)
    
  }
  if(length(bessssst) == 4){
    fillin<- c(NA)
    bessssst<- append(bessssst, fillin)
    bessssst<- unlist(bessssst)
    
  }
  
  #replace the old key_term_list with the new key_terms of the issue games in the correct spot
  correction_spot<- redo2 *5 - 4
  
  all_correction_spots<- c(correction_spot: (correction_spot + 4))
  
  n <- c(1)
  for(all in all_correction_spots){
    key_terms_list[all]<- bessssst[n]
    n<- n + 1
  }
}

for(redo in redo_game){
  needed_key<- redo * 5 - 4
  needed_key<- needed_key + c(0, 1, 2, 3, 4)
  print(colnames(titles2_df[redo]))
  print(key_terms_list[needed_key])
}  








#STAT_API

#API url to look up stats for video (likes, views, etc)
yt_stat_search <- "https://www.googleapis.com/youtube/v3/videos?part=statistics&id=%s&key=%s"

#function to create url for the stats API, requires etag of video (not the title like the previous API)
create_stats_url <-function(x) {paste0(sprintf(yt_stat_search, x, "AIzaSyCPOy2cpD0SOXEfGDoc2v0riFqtSQ172ow"))}


#find the corresponding etag for the relevent videos


#carve out section search_JSON_df with subset
new_2_df <- subset.data.frame(select(search_JSON_df, ends_with("items.id")))

#create function to further reduce dataframe and apply to above dataframe
get_videoID <- function(x){subset.data.frame(select(x, ends_with("videoID")))}
videoID<- mapply(get_videoID, new_2_df)

videoID_df<- data.frame(videoID)
key_terms_list_2 <- as.list(key_terms_list)
which_key_term<-c(1:5)

(for(gg_g in gg_games){
  #create a list of key terms to narrow titles by, 5 terms per game
  
  relevent_titles<-c()
  
  #print(which_key_term)
  
  #focus which terms schould be used for this particular game
  used_key_terms <- key_terms_list_2[which_key_term]
  temp_used_key_terms<- unlist(used_key_terms)
  
  dont_use<- which(is.na(temp_used_key_terms))
  
  if(length(dont_use) > 5){
      temp_used_key_terms<- temp_used_key_terms[-dont_use]
      used_key_terms<- temp_used_key_terms[1:length(temp_used_key_terms)]
  }else{
    dont_use<- which(is.na(used_key_terms))
    if(length(dont_use) > 0){
      used_key_terms<- used_key_terms[-dont_use]
    }
  }

  number_of_terms<- c(1:5)
    for(number in number_of_terms){
      if(length(unlist(used_key_terms[number])) > 1){
        used_key_terms[number] <- unlist(used_key_terms[number])[1]
      }
     }
  
  #print(used_key_terms)
  
  if(length(which(is.na(used_key_terms))) < 5){
    #which_term<- c(1:5)
    #narrow the list of video titles in titles_df to the ones that contain the used_key_terms (fromt the key_terms_list)
    gg2<- gg_g
    for (used in used_key_terms){
      #print(used)
      relevent_title_spots<- grep(used, titles2_df[,gg2], ignore.case = TRUE)
      #print(relevent_title_spots)
      relevent_titles<- append(relevent_titles, relevent_title_spots)
      #print(relevent_titles)
    }
  }  
    
    #filter the grep(indicies of the elements) list for unique occurances
    key_title_spots <- unique(relevent_titles)
    #translate the indicies to actual video titles that will be used
    optimal_titles <- titles_df[,gg_g][key_title_spots]
    
    #turn the list of used video titles into a dataframe
    holder_data_frame<- data.frame(optimal_titles, stringsAsFactors = FALSE)
    holder_data_frame <- holder_data_frame %>% mutate(videoID=videoID_df[,gg_g][key_title_spots])
    
    
    #make sure the titles and etag match up
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
    
    
    #transpose stats data frame to put video titles as rows
    stats_JSON_df<-as.data.frame(t(stats_JSON_df))
    
    #carve out section stats_JSON_df with subset
    stats <- subset.data.frame(select(stats_JSON_df, ends_with("items")))
    
    bahaha<-c(1:nrow(stats))
    #stat_matrix<- matrix()
    stats_collection <- data.frame()
    
    for (bah in bahaha) {
      vid_stat<-as.character(stats$items[bah])
      
      vid_stat_sep<- unlist(strsplit(vid_stat, split="\\,"))
      
      vid_stat_matrix <- matrix(data = vid_stat_sep[4:8], nrow = 5, ncol=1, byrow=TRUE)
      
      vid_df<- as.data.frame(vid_stat_matrix)
      
      stats_collection<- append(stats_collection, vid_df)
    }
    
    stats_collection_df<- as.data.frame(stats_collection)
    colnames(stats_collection_df)<- holder_data_frame[1:nrow(holder_data_frame), 2]
    
    #find the columns of stats_collection_df that has NA
    
    #sum up the number of NAs per column, if greater than 0, column has rows with NA
    column_has_NA<- colSums(is.na(stats_collection_df))
    #finding all the columns without any NAs (previous line adds up to 0) with grep
    non_empty_spots<-grep(pattern = 0, column_has_NA)
    #collect all the columns with NAs by subtracting all the columns free of NAs
    lacking_columns<- c(1:ncol(stats_collection_df))[-non_empty_spots]  
    
    #create a list (go_test_row) of the objects in the lacking column
    #replace the NAs in go_test_row with term "placeholder" to prevent errors with using startsWith
    #replace every row in the lacking column with NA
    #re-fill the column based on what is in the go_test_row list
    
    if(sum(lacking_columns) > 0){
      for (lacking in lacking_columns) {
        go_test_row<- as.character(stats_collection_df[,lacking])
        go_test_row[grep(TRUE, is.na(go_test_row))] <- "placeholder"
        stats_collection_df[,lacking]<- NA
        
        for(go_test in go_test_row){
          if(startsWith(go_test, " statistics") == TRUE){
            stats_collection_df[1,lacking]<- as.character(go_test)
          }
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
    
    #collect all the stats from test_data_fram_df into their appropriate lists (viewCount, likeCount, etc)
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
    viewCount_list<- gsub(pattern =" statistics = list(viewCount = \"", "", viewCount_list, fixed = TRUE)
    viewCount_list<- gsub(pattern ="\"", "", viewCount_list, fixed = TRUE)
    
    likeCount_list<- gsub(pattern =" likeCount = \"", "", likeCount_list, fixed = TRUE)
    likeCount_list<- gsub(pattern ="\"", "", likeCount_list, fixed = TRUE)
    
    dislikeCount_list<- gsub(pattern =" dislikeCount = \"", "", dislikeCount_list, fixed = TRUE)
    dislikeCount_list<- gsub(pattern ="\"", "", dislikeCount_list, fixed = TRUE)
    
    favoriteCount_list<- gsub(pattern =" favoriteCount = \"", "", favoriteCount_list, fixed = TRUE)
    favoriteCount_list<- gsub(pattern ="\"", "", favoriteCount_list, fixed = TRUE)
    
    commentCount_list<- gsub(pattern =" commentCount = ", "", commentCount_list, fixed = TRUE)
    commentCount_list<- gsub(pattern ="))", "", commentCount_list, fixed = TRUE)
    
    
    #add stat data from the lists to the holder_data_frame
    holder_data_frame<- holder_data_frame %>% mutate(viewCount = viewCount_list)
    holder_data_frame<- holder_data_frame %>% mutate(likeCount = likeCount_list)
    holder_data_frame<- holder_data_frame %>% mutate(dislikeCount = dislikeCount_list)
    holder_data_frame<- holder_data_frame %>% mutate(favoriteCount = favoriteCount_list)
    holder_data_frame<- holder_data_frame %>% mutate(commentCount = commentCount_list)
    
    stats_df_lable <- paste0(gsub(pattern=" ", replacement= "_", x=colnames(titles_df[
      gg_g])), "_stats_df")
    
    print(stats_df_lable)
    
    assign(stats_df_lable, value = holder_data_frame)
    
    which_key_term<- which_key_term +5
  }else{
    print("This game has all NA terms")
    print(colnames(titles_df[gg_g]))
    which_key_term<- which_key_term +5
  }
}










