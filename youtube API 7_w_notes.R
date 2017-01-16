
#setup
require(curl)
require(jsonlite)
library(dplyr)
library(tidyr)
library(lubridate)

#custom function to extract JSON data from youtube API
get_JSON <- function(x){fromJSON(x)}

#custom function to find the mode (the variable that occurs the most)
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}


#create the list of games to inspect
#use the readLines function to gather from the metacritic page the top 100 games of 2016
thepage <- readLines("http://www.metacritic.com/browse/games/score/metascore/year/pc/filtered?year_selected=2016")

#create funtions to read the specific line for each game's title and release date
find_title <- function(x){thepage[26*(x-1)+573]}
find_date <- function(x){thepage[26*(x-1)+583]}

#focus on the top 100 games
top_100_games <- c(1:100)

#scrape for top 100 game titles and their corresponding release dates
game_title <- sapply(top_100_games, find_title)


#the correct html line on the metacritic webpage fluxuates between 573 and 571
#test the function by running games_title[1], and if the result is incomprehensive, remove # from lines 115 to 120 and run
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

#when none of if statements above produceds the game title like expected
#run the lines below, then pick out the line from the webpage that contains the game title
#change the last number in find_title and find_date to reflect the line the title and date is store in
#hint: the date line will change as much as the title line, so really all you need to do is find how much the title number will change, then replicated the difference in the find_date

#which_line <- c(540:640)
#for(wwhich in which_line){
  #print(wwhich)
  #print({thepage[26*(1-1)+wwhich]})
  #}



#create dataframe to contain game titles and their release dates
game_title_df <-data.frame(game_title, game_date)

#remove the white space in front of game titles
#create custom function to remove white space
trim.leading <- function (x)  sub("^\\s+", "", x)
#update game_title_df with new game titles
game_title_df$game_title <- trim.leading(game_title_df$game_title)

#clean up column containing the release date in game_title_df
#change date format from Month, Day, Year to Year-Month-Day
game_title_df$game_date <- mdy(game_title_df$game_date)

View(game_title_df)


#SEARCH_API
#used youtube search API to get search results when each game title is searched along with the term "soundtrack"


#assign the API url to yt_search
yt_search <- "https://www.googleapis.com/youtube/v3/search?part=snippet&maxResults=50&order=relevance&publishedBefore=%s%%3A00%%3A00Z&q=%s+soundtrack+full+-cover+-radio&type=video&videoDuration=long&key=%s"

#custom function pastes elements(game titles) into the proper places within the search API url and creates a useable url
create_search_url <- function(x) {paste0(sprintf(yt_search, "2017-06-24T00", x, "AIzaSyCPOy2cpD0SOXEfGDoc2v0riFqtSQ172ow"))}

#ceate a list of game titles from 2016 to be the elements inserted into the search API
games <- game_title_df$game_title
#replace spaces with "+" so it works within the url
games <- gsub(pattern=" ", replacement= "+", x=games)

#create a list of urls to run through the get_JSON function
search_url_list <- sapply(games, create_search_url)

#run the urls through the get_JSON data to get the JSON data from the youtube search API
search_JSON <- sapply(search_url_list, get_JSON)

#not all urls run will return the right amount of data
#find which search_JSON elements have the wrong number of components
#search_JSON has 100 entries due to there being 100 games tested 
rows<-c(1:100)

bad_line <-c()

for(row in rows){
  
  row_length<- lengths(search_JSON[row])
  
  #almost all of the urls returned an entry length of 6, remove games that don't
  if(row_length != 6){
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


#TERM_SEARCH


#each game returned 50 video results with the youtube search API
#however, not all videos are relevant to the game
#by focusing on segments of video title that start in various places along the video title and are of various lengths, a common phrase amongst the video titles will be found and used as a filter for relevant video titles
#for example, "Witcher 3 Soundtrack" was used to create a list of videos
#the goal is to have the term "Witcher 3" to be the most often occuring phrase found from this list
#then "Witcher 3" will be used to filter out any video titles that do not contain the phrase "Witcher 3"

#create dummy dataframe to search for relevent terms so that the original database will be unchanged while annoying terms can be taken out of the video titles
#create an object that stores the number of games that are being analyzed
gg_games<- c(1:length(colnames(titles_df)))
#create dummy dataframe
titles2_df<- as.data.frame(titles_df)

#each youtube uploader has their personal way of formatting their youtube titles
#remove special characters from titles
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


#FIND COMMON TERMS


#using a for-loop, get the top 5 terms that are repeated in the list of video titles per game
#create objects based on certain constants for the for-loop to loop through
#number of video titles per game
title_number<- c(1:50)
#range of how short or long the term can be
term_lengthh <- c(8:12)
#range of where the term can start
term_startt <- c(1:20)
#for each term created, a comparison term will be created to compare the first term to
#range of where the comparison term can start
term_startt2 <- c(1:20)
#create a blank master list where the top 5 terms for each game will be stored into
key_terms<-c()

for(gg_g in gg_games){
  #each game will deal different terms, so create a blank list for matching term to be stored into
  matching_term<-c()
  gg2<- gg_g
  
  #each video titles is compared to every other title in the list within the game
  for(title in title_number){
    
    #some games have a very short name, so the ideal term will also be shorter
    #shorten the term_lengthh for games with a short name
    if(nchar(colnames(titles2_df[gg2])) < 7){
      print("game title is shorter than 8 char")
      print(colnames(titles2_df[gg2]))
      term_lengthh <- c(3:6)
    }else{term_lengthh<- c(8:12)}
    
    for(lengthh in term_lengthh){
      
      #create the first term, and call it test1
      for(startt in term_startt){
        
        test1 <-substr(titles2_df[,gg2][title], start=startt, stop=(startt + lengthh))
        
        #create a list of title numbers that excludes the video title the first term was created from
        title_number_2 <- title_number[-title]
        
        #create object to assure the length of comparison term is the same as the first term
        lengthh2 <- lengthh
      
        for(title2 in title_number_2){
          for(startt2 in term_startt2){
            #store the comparison term under object "test2"
            test2 <-substr(titles2_df[,gg2][title2], start=startt2, stop=(startt2 + lengthh2))
            
            #if the two terms are the same, store the term into matching_term
            if(test1 == test2){
              matching_term <- append(matching_term, test2)
            }
          }
        }
      }
    }
  }
  #filter out the terms that match for only unique terms
  unique_terms <- unique(matching_term)
  #create an empty list where the number of times a term appeared can be stored
  most_matches<-c()
  for(u in unique_terms){
    
    #there was an idea to require the term to be found in the game title, but it eliminated too many terms
    #if(grepl(u, colnames(titles2_df[gg2])) == TRUE)
    
    #store the number of times a terms was found
    most_matches<- append(most_matches, length(grep(u, matching_term)))
  }
  
  #create a label to be printed out so progress can be tracked while the for-loop is running
  best_term_lable <- paste0(gsub(pattern=" ", replacement= "_", x=colnames(titles2_df[
    gg_g])), "_best_terms")
  print(best_term_lable)
  
  #many terms have the same number of matches as other terms
  #filter for unique number of matches
  temp_most_matches<- unique(most_matches)
  #filter for the top 5 number of times a term
  best_terms<- sort((temp_most_matches),decreasing=TRUE)[1:5]
  
  #best_terms is just a list of numbers
  #create custom function to convert the number of matches back to the term the number represents
  find_best_terms<- function(x){unique_terms[grep(x, most_matches)]}
  
  #store the terms that have found as many matches as the top 5 match terms
  bessssst<- sapply(best_terms, find_best_terms)
  
  #sometimes there were less than 5 terms that were found to be common amongst the 50 video titles
  #each game must have 5 terms stored into key_terms
  #fill each games bessssst list with NAs until it reaches 5 elements long
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
  
  #store the 5 best terms for each game into key_terms
  key_terms<- append(key_terms, bessssst)
}

key_terms_list <- as.list(key_terms)


#FIND AND ELEMINATE COMMON TERMS THAT ARE TOO COMMON


#sometimes the terms end up being phrases that don't relate to the game at all and are just comment catchall phrases that gaming videos often use
#examples: "Let's Play", "Walkthrough", "Best of"

#create for-loop to inspect the terms and rule out any common phrases

#the for-loop with go through each term in key_term
#every 5 terms belongs came from one game's list of videos
#which_key_term will help isolate the part of the key_term relevant to that game and to that iteration of the for-loop
which_key_term <- c(1:5)


#each term will be tested, to see if the term appears frequently in other games's video title list
#create a list of all video titles
all_games2 <- unlist(colnames(titles2_df))
#all_titles2<- unlist(titles2_df[,1:length(colnames(titles_df))])

#create empty lists to store entries from for-loop
suspect_term<- c()
term_occurance<- c()
example_title<- c()

for(gg_g in gg_games){
  
  #don't test the key_term against the video titles from the game the key_term came from
  #create temporary dataframe from titles2_df without the key_term's game's column
  temp_titles_df<- titles2_df[-gg_g]
  
  #create a list of all video titles minus the key_term's video titles
  all_titles3<- unlist(temp_titles_df[,1:ncol(temp_titles_df)])
  
  gg2<- gg_g
  
  #run each of the 5 key_terms through the for loop 
  for(which in which_key_term){
    which2<- which
    
    #filter out terms that are NA
    if(is.na(key_terms_list[which2]) == FALSE){
      
      #sometimes terms are 0 characters long and appear as ""
      #filter out terms that are 2 or less characters
      if(nchar(key_terms_list[which2]) > 2 ){
        
        #most terms do not appear in other game's video titles, and the grepl returns 98 FALSEs
        #if term is found in other video titles, grepl will return a TRUE and less FALSE
        if(table(grepl(key_terms_list[which2], all_games2))["FALSE"] < 98){
          
          if(length(grep(key_terms_list[which2], all_titles3)) > 0){
            
            #store terms that appear in other video titles
            suspect_term<- unlist(append(suspect_term, key_terms_list[which2]))
            
            #store how many time the term appears
            term_occurance<- unlist(append(term_occurance, length(grep(key_terms_list[which2], all_titles3))))
            
            #store the video title the term first appears in as an example
            example_title<- unlist(append(example_title, all_titles3[(grep(key_terms_list[which2], all_titles3))[1]]))
          }
        }
      }
    }
  }
  #increase the which_key_term by 5 to look at the next set of key_terms that relate to the next game
  which_key_term <- which_key_term + 5
}

#collect the repetative terms into a database and name it
suspect_terms_df<- data.frame(suspect_term, term_occurance, example_title)
colnames(suspect_terms_df) <- c("term", "number_of_occurances", "title_example") 
View(suspect_terms_df)

#focus on terms that occur 50 or more times in places they shouldn't
problem_terms_df<- subset(suspect_terms_df, number_of_occurances > 50, select=term)

#store the +50 terms in a vector
problem_terms<- as.vector(problem_terms_df[,1])

#create an empty list to store games with bad terms
redo_game_spots<-c()

for(problem_t in problem_terms){
  term<- problem_t
  
  #find where in key_terms_list the problem_term comes from
  problem_spot<- grep(term, key_terms_list)
  
  #calculate which game the problem_term comes from
  for(problem_s in problem_spot){
    spot <- problem_s
    if((spot %% 5) != 0){
      issue_game_spot<- spot %/% 5 + 1
    }else{
      issue_game_spot<- spot %/% 5
    }
    
    #remove the problem_term from the video titles
    titles2_df[, issue_game_spot]<- gsub(term, "", titles2_df[,issue_game_spot])
    
    #store which games need to have their terms generated again in a list
    redo_game_spots <- append(redo_game_spots, issue_game_spot) 
  }
}


#RE-GENERATE COMMON TERMS TO REPLACE THE TOO-COMMON TERMS


redo_game<-unique(redo_game_spots)

title_number<- c(1:50)
term_lengthh <- c(8:15)
term_startt <- c(1:20)
matching_term <-c()
term_startt2 <- c(1:20)
most_matches<-c()

#re-generate terms for the games that had bad terms
for(redo in redo_game){
  matching_term<-c()
  redo2<- redo
  for(title in title_number){
    
    if(nchar(colnames(titles2_df[redo2])) < 7){
      term_lengthh <- c(3:6)
      
    }else{term_lengthh<- c(8:15)}
    
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

  most_matches<-c()
  for(u in unique_terms){
    if(grepl(u, colnames(titles2_df[redo2])) == TRUE)
      most_matches<- append(most_matches, length(grep(u, matching_term)))
  }
  most_matches<- append(most_matches, length(grep(u, matching_term)))
  
  best_term_lable <- paste0(gsub(pattern=" ", replacement= "_", x=colnames(titles2_df[
    gg_g])), "_best_terms")

    best_terms<- sort((most_matches),decreasing=TRUE)[1:5]
  
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
  
  #identify the spot within key_term_list where corrections need to be made
  correction_spot<- redo2 *5 - 4
  
  #create a list of 5 numbers to cycle through the 5 bad terms and replace it with 5 good terms
  all_correction_spots<- c(correction_spot: (correction_spot + 4))
  
  n <- c(1)
  
  #for-loop replaces bad terms within key_terms_list with better term
  for(all in all_correction_spots){
    key_terms_list[all]<- bessssst[n]
    n<- n + 1
  }
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
key_terms_list_2 <- as.list(key_terms_list)

#reset which_key_terms
which_key_term<-c(1:5)

#create an empty list to store dataframes created from running the stats API
list_of_data_frames<- as.list(c())

#run for-loop that gathers stats for each relevant video
for(gg_g in gg_games){
  
  #create an empty list to store video titles that contain one of the terms
  relevent_titles<-c()
  
  #focus which terms should be used as a filter for this particular game
  used_key_terms <- key_terms_list_2[which_key_term]
  
  #sometimes one key_term is actually several terms
  #unlist to view all terms
  temp_used_key_terms<- unlist(used_key_terms)
  
  #filter out terms that are NA
  dont_use<- which(is.na(temp_used_key_terms))
  
  #clean up list of terms
  
  #if there are more than 5 NA terms, remove all NAs
  if(length(dont_use) > 5){
      temp_used_key_terms<- temp_used_key_terms[-dont_use]
      used_key_terms<- temp_used_key_terms[1:length(temp_used_key_terms)]
  }else{
    #if there are less than 5 NAs, then just used the original list of terms, minus the NAs
    dont_use<- which(is.na(used_key_terms))
    if(length(dont_use) > 0){
      used_key_terms<- used_key_terms[-dont_use]
      }
    }
  
  #if there are multiple terms stacked into one key_term_list spot, grab only the first one in the stack to use
  number_of_terms<- c(1:5)
    for(number in number_of_terms){
      if(length(unlist(used_key_terms[number])) > 1){
        used_key_terms[number] <- unlist(used_key_terms[number])[1]
      }
     }
  
  #reduce the list of video titles to titles that contain the key_term
  if(length(which(is.na(used_key_terms))) < 5){
    
    #narrow the list of video titles in titles_df to the ones that contain the used_key_terms (from the key_terms_list)
    gg2<- gg_g
    for (used in used_key_terms){
      relevent_title_spots<- grep(used, titles2_df[,gg2], ignore.case = TRUE)
      relevent_titles<- append(relevent_titles, relevent_title_spots)
    }
  
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
    
  }else {
    #notify operator that there is a game with video titles that failed to produce any kind of common term between the 50 of them
    print("This game has all NA terms")
    print(colnames(titles_df[gg_g]))
    
    #increase which_key_term by 5 so the next 5 terms in the key_terms_list can be used with the next game
    which_key_term<- which_key_term + 5
    
    #a game that the youtube search API can't return related videos is a game that unfortunately has to be thrown out of the study
    #add the game's number to bad_line so we can update the original game_title_df and remove the game from it
    bad_line<- bad_line + gg2
  }
}

#create a dataframe with the final list of games to be analyzed 
final_game_title_df <- data.frame(game_title_df[-bad_line,])

#add the average of the stats found through the youtube stats API to the original games_titles_df dataframe

#dataframe row indexes are removed when row is removed
#if two games were removed, this results in the 98th game still being labeled the 100th index
#reassign the dataframes index appropriately 
rownames(final_game_title_df) <- 1:nrow(final_game_title_df)

#make sure the gg_games is updated to represent any changes made in the list of games
gg_games<- c(1: nrow(final_game_title_df))

#create a dataframe about the stats overall
#create an empty dataframe to store averaged stats into
game_stats_df<- data.frame()

#run for loop the get the total views, average views, like, dislikes, and comment count of all the videos for each game
for(gg_g in gg_games){
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

View(game_stats_df)

#merge dataframes so that game titles, game release dates, and average stats are all under one dataframe
final_game_title_df<- cbind(final_game_title_df, game_stats_df) 




