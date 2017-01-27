library(stringdist)

#using a for-loop, get the top 5 terms that are repeated in the list of video titles per game
#create objects based on certain constants for the for-loop to loop through
#number of video titles per game
title_number<- c(1:50)
#range of how short or long the term can be
term_lengthh <- c(8:12)
#range of where the term can start
term_startt <- c(1:20)
#create a blank master list where the top 5 terms for each game will be stored into
#key_terms<-c()
#key_terms_2<-c()
#key_terms_3<- c()
num_terms_per_game<- c(1:5)



for(gg_g in gg_games){
  #each game will deal different terms, so create a blank list for matching term to be stored into
  matching_term<-c()
  gg2<- gg_g
  
  list_of_test2<- c()
  best_term_from_test <- c()
  num_best_appeared<- c()
  
  #each video titles is compared to every other title in the list within the game
  for(title in title_number){
    
    #some games have a very short name, so the ideal term will also be shorter
    #shorten the term_lengthh for games with a short name
    if(nchar(colnames(titles2_df[gg2])) < 7){
      #print("game title is shorter than 8 char")
      #print(colnames(titles2_df[gg2]))
      term_lengthh <- c(3:6)
    }else{term_lengthh<- c(8:12)}
    
    for(lengthh in term_lengthh){
      
      #create the first term, and call it test1
      for(startt in term_startt){
        
        test1 <-as.character(substr(titles2_df[,gg2][title], start=startt, stop=(startt + lengthh)))
        
        #create a list of title numbers that excludes the video title the first term was created from
        title_number_2 <- title_number[-title]
        
        for(title2 in title_number_2){
          for(startt2 in term_startt2){
            #store the comparison term under object "test2"
            test2 <-substr(titles2_df[,gg2][title2], start=startt2, stop=(startt2 + lengthh2))
            
            list_of_test2<- append(list_of_test2, test2)
            
            
          }
        }
        
        match_distance<- stringdist(c(test1),c(list_of_test2))
        
        if(is.na(table(match_distance)["1"]) == FALSE){
          exact_matches <- grep("1", match_distance)
          
          best_term_from_test <- append(best_term_from_test, list_of_test2[exact_matches])
          
          num_best_appeared <- append(num_best_appeared, length(exact_matches))
        }
        if(is.na(table(match_distance)["2"]) == FALSE){
          exact_matches <- grep("2", match_distance)
          
          best_term_from_test <- append(best_term_from_test, list_of_test2[exact_matches])
          
          num_best_appeared <- append(num_best_appeared, length(exact_matches))
        }
      }
    }
  }
  
  num_best_spot<- sort(num_best_appeared, decreasing = TRUE)[1:5]
  
  top_terms_spots <- c()
  
  for(num in num_best_spot){
    best_best_term_from_test<- grep(num, num_best_appeared)
    
    top_terms_spots<- append(top_terms_spots, best_best_term_from_test)
  
  }
  
  top_terms <- append(top_terms, best_terms_from_test[top_terms_spots])

}
