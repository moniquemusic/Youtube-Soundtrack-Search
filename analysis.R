# this page is dedicated to doing analysis of the youtube game soundtrack dataset datasets
#goal: see if game companies should invest more in their soundtracks

#function to show multiple plots on one page
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}



#load dataframe
game_soundtrack_df<- read.csv("game_soundtrack_df.csv")

#remove games without videos, because analysis cannot be done with them

game_soundtrack_df <- subset.data.frame(game_soundtrack_df, subset = game_soundtrack_df$video_num > 0)

#is soundtrack popularity related to game popularity?

#see if the difference between critics and users scores has decreased or increased over the years
critic_vs_user_opinion<-  game_soundtrack_df %>% 
  filter(is.na(game_user_score) == FALSE) %>%
  mutate(year = year(game_release_date), diff = (game_critic_score)/10 - game_user_score) %>% 
  group_by(year) %>% 
  summarize(avg_diff = mean(diff))

ggplot(critic_vs_user_opinion, aes(x= year, y= avg_diff)) + 
  geom_point()
#opinions between critics and users are getting further and further apart as time passes

#lets test to see what's happening to the actual scores
critic_and_user_opinion<- 
  game_soundtrack_df %>%
  filter(is.na(game_user_score) == FALSE) %>%
  mutate(year = year(game_release_date), critic_scores = (game_critic_score)/10, user_scores = game_user_score) %>% 
  group_by(year) %>% 
  summarize(avg_critic = mean(critic_scores), avg_user = mean(user_scores))

ggplot(critic_and_user_opinion, aes(x= year, y= score))+
  geom_point(aes(y= avg_critic, colour = "critic")) +
  geom_point(aes(y= avg_user, colour = "user"))
#average critics are more and more favorable overtime, while users have become more and more critical


#lets divide games into 4 bins based on global sales
#games that sold better probably had more invested into its marketing
critic_vs_user_opinion_2<-  
  game_soundtrack_df %>%
  filter(is.na(game_user_score) == FALSE) %>%
  filter(is.na(global_sales) == FALSE) %>%
  mutate(year = year(game_release_date), diff = (game_critic_score)/10 - game_user_score, sales_per_mil = cut(global_sales, c(-Inf, 0, 0.25, 0.5, 0.75, 1, Inf) , labels = c(0, 0.25, 0.5, 0.75, 1, "+1"))) %>% 
  group_by(year, sales_per_mil) %>%
  summarize(avg_diff= mean(diff))

ggplot(critic_vs_user_opinion_2, aes(x= year, y= avg_diff)) +
  geom_line(aes(colour= sales_per_mil))
#seems in overall the differences in opinion between critics and users stay within a range no matter how many games were sold
#there is an exception: in 2013, there was a big difference in opinion for games that sold half a million

critic_and_user_opinion_2<-
  game_soundtrack_df %>%
  filter(is.na(game_user_score) == FALSE) %>%
  filter(is.na(global_sales) == FALSE) %>%
  mutate(year = year(game_release_date), critic_scores = (game_critic_score)/10, user_scores = game_user_score, sales_per_mil = cut(global_sales, c(-Inf, 0, 0.25, 0.5, 0.75, 1, Inf), labels = c(0, 0.25, 0.5, 0.75, 1, "+1"))) %>% 
  group_by(year, sales_per_mil) %>%
  summarize(avg_critic = mean(critic_scores), avg_user = mean(user_scores))

ggplot(critic_and_user_opinion_2, aes(x= year, y=score)) +
  geom_line(aes(y= avg_critic, colour = "critic", linetype= sales_per_mil, alpha= sales_per_mil)) +
  geom_line(aes(y= avg_user, colour = "user", linetype= sales_per_mil, alpha= sales_per_mil)) + 
  scale_linetype_manual(values=c("dotted", "dotdash", "twodash", "dashed", "longdash", "solid"))
#from this graph we can see that the large difference in opinion between critics and user in 2013 came from users giving very negative ratings games that sold 0.5 million




#for now, we are going to assume popularity can be marked by the total views
#later we'll use google trends score

game_soundtrack_df <- game_soundtrack_df %>% mutate(video_average_views = total_views/video_num)

#find which user scores have the most views per video

game_soundtrack_df %>% 
  mutate(user_ratings = round(game_user_score)) %>%
  group_by(user_ratings) %>% 
  summarize(avg_views_rating = mean(video_average_views))
#games around the 7 user score have the most views per video




#game_soundtrack_df %>% mutate(game_user_score, soundtrack_pop_intensity = (total_views/video_num)) %>% group_by(user_ratings =round(game_user_score)) %>% summarize(avg_pop_intensity = mean(soundtrack_pop_intensity))

final_game_title_2_df <- game_soundtrack_df %>% mutate(all_game_titles, soundtrack_pop_intensity = (total_views/video_num))



#the more views a game collectively has, it would make sense the there would be more comments 
cor(game_soundtrack_df$total_views, game_soundtrack_df$total_comments)

#figure out what drives the comments....the more popular they are? or the more conflicted the opinions are?
#test correlation between popularity and average number of comments per video
cor(game_soundtrack_df$video_average_views, game_soundtrack_df$total_comments/game_soundtrack_df$video_num)
#0.5687507

#compare total likes vs comments, normalized with average views
cor(game_soundtrack_df$total_likes/game_soundtrack_df$video_average_views, game_soundtrack_df$total_comments/game_soundtrack_df$video_average_views)
#0.5311705

#compare total likes vs comments, normalized with average views
cor(game_soundtrack_df$total_dislikes/game_soundtrack_df$video_average_views, game_soundtrack_df$total_comments/game_soundtrack_df$video_average_views)
#0.2963238

#compare the difference between total likes and total dislikes vs comments, normalized with average views
game_soundtrack_df <- game_soundtrack_df %>% mutate(opinon_diff = total_likes- total_dislikes)

cor(game_soundtrack_df$opinon_diff/game_soundtrack_df$video_average_views, game_soundtrack_df$total_comments/game_soundtrack_df$video_average_views)
#0.5299687




#find if better games have better soundtracks
library(ggplot2)

#plot soundtrack popularity vs critics game score
critic_score_vs_soundtrack_pop<- game_soundtrack_df %>% 
  mutate(critic_score = game_critic_score, soundtrack_popularity= video_average_views)%>%
  group_by(critic_score) %>% 
  summarise(avg_sound_popularity = mean(soundtrack_popularity))

ggplot(critic_score_vs_soundtrack_pop, aes(x = critic_score, y= avg_sound_popularity)) + 
  geom_point() + 
  geom_smooth(method = lm, se= TRUE)

#plot doesn't really return anything, let's compare user scores vs soundtrack popularity next
user_score_vs_soundtrack_pop<- 
  game_soundtrack_df %>% 
  mutate(user_score = game_user_score, soundtrack_popularity= video_average_views) %>% 
  group_by(user_score) %>% 
  summarise(avg_sound_popularity = mean(soundtrack_popularity))

ggplot(user_score_vs_soundtrack_pop, aes(x = user_score, y= avg_sound_popularity)) + 
  geom_point() + 
  geom_smooth(method = lm, se= TRUE)
#results in more of an upward trend

#see if there is a correlation between critics score and soundtrack popularity
cor(game_soundtrack_df$game_critic_score, game_soundtrack_df$video_average_views)
#-0.0144181

df<- 
  game_soundtrack_df %>%
  filter(is.na(game_user_score) == FALSE) %>%
  filter(is.na(video_average_views) == FALSE)

cor(df$game_user_score, df$video_average_views)
#0.01288589



#average views favors games that have only a few soundtrack videos uploaded and a lot of views
#some games might be really popular but have so many videos uploaded it dilutes the results
#use video_total_views as a measure for popularity instead of average views
critic_score_vs_total_views<- 
  game_soundtrack_df %>%
  group_by(game_critic_score) %>%
  summarize(avg_view = mean(video_total_views))

ggplot(critic_score_vs_total_views, aes(game_critic_score, avg_view)) +
  geom_point() +
  geom_smooth(method = lm, se= TRUE)

cor(game_soundtrack_df$game_critic_score, game_soundtrack_df$video_total_views)
#0.06307652

#repeat process with user scores instead
user_score_vs_total_views<-
  game_soundtrack_df %>%
  filter(is.na(game_user_score) == FALSE) %>%
  group_by(game_user_score) %>%
  summarize(avg_view = mean(video_total_views))

ggplot(user_score_vs_total_views, aes(game_user_score, avg_view)) +
  geom_point() +
  geom_smooth(method = lm, se= TRUE)

df<-
  game_soundtrack_df %>%
  filter(is.na(game_user_score) == FALSE)

cor(df$game_user_score, df$video_total_views)
#0.01122621




#lets separate the data into bins based on sales
#a big budget game is going to be received differently from an indi game

critic_score_vs_total_views_2 <-
  game_soundtrack_df %>%
  mutate(sales_per_mil = cut(global_sales, c(-Inf, 0, 0.25, 0.5, 0.75, 1, Inf), labels = c(0, 0.25, 0.5, 0.75, 1, "+1"))) %>%
  group_by(sales_per_mil, game_critic_score) %>%
  summarize(avg_view= mean(video_total_views))

P1<- ggplot(critic_score_vs_total_views_2, aes(x= game_critic_score, y=avg_view)) +
  geom_point(aes(y= avg_view, colour= sales_per_mil, alpha= sales_per_mil)) +
  ggtitle("Critic scores vs all game soundtracks")
  
  
best_selling_critic_vs_total_views <-
  game_soundtrack_df %>%
  filter(global_sales >= 1) %>%
  group_by(game_critic_score) %>%
  summarize(avg_view= mean(log(video_total_views)))

P2<- ggplot(best_selling_critic_vs_total_views, aes(x= game_critic_score, y=avg_view)) +
  geom_point() +
  geom_smooth(method = lm, se= TRUE)+
  ggtitle("Games +1 million sold")

second_best_selling_critic_vs_total_views <-
  game_soundtrack_df %>%
  filter(global_sales < 1) %>%
  filter(global_sales >= 0.75) %>%
  group_by(game_critic_score) %>%
  summarize(avg_view= mean(video_total_views))

P3<- ggplot(second_best_selling_critic_vs_total_views, aes(x= game_critic_score, y=avg_view)) +
  geom_point() +
  geom_smooth(method = lm, se= TRUE)+
  ggtitle("Games three quarters of a million sold")


third_best_selling_critic_vs_total_views <-
  game_soundtrack_df %>%
  filter(global_sales < 0.75) %>%
  filter(global_sales >= 0.5) %>%
  group_by(game_critic_score) %>%
  summarize(avg_view= mean(log(video_total_views)))

P4<- ggplot(third_best_selling_critic_vs_total_views, aes(x= game_critic_score, y=avg_view)) +
  geom_point() +
  geom_smooth(method = lm, se= TRUE)+
  ggtitle("Games half a million sold")


fourth_best_selling_critic_vs_total_views <-
  game_soundtrack_df %>%
  filter(global_sales < 0.5) %>%
  filter(global_sales >= 0.25) %>%
  group_by(game_critic_score) %>%
  summarize(avg_view= mean(log(video_total_views)))

P5<- ggplot(fourth_best_selling_critic_vs_total_views, aes(x= game_critic_score, y=avg_view)) +
  geom_point() +
  geom_smooth(method = lm, se= TRUE)+
  ggtitle("Games quarter million sold")


fifth_best_selling_critic_vs_total_views <-
  game_soundtrack_df %>%
  filter(global_sales < 0.25) %>%
  filter(global_sales >= 0) %>%
  group_by(game_critic_score) %>%
  summarize(avg_view= mean(log(video_total_views)))

P6<- ggplot(fifth_best_selling_critic_vs_total_views, aes(x= game_critic_score, y=avg_view)) +
  geom_point() +
  geom_smooth(method = lm, se= TRUE) +
  ggtitle("Games less than a quarter million sold")

multiplot(P1, P2, P3, P4, P5, P6, cols= 3)




#lets test user scores vs soundtrack popularity
user_score_vs_total_views_2 <-
  game_soundtrack_df %>%
  mutate(sales_per_mil = cut(global_sales, c(-Inf, 0, 0.25, 0.5, 0.75, 1, Inf), labels = c(0, 0.25, 0.5, 0.75, 1, "+1"))) %>%
  group_by(sales_per_mil, game_user_score) %>%
  summarize(avg_view= mean(video_total_views))

P1<- ggplot(user_score_vs_total_views_2, aes(x= game_user_score, y=avg_view)) +
  geom_point(aes(y= avg_view, colour= sales_per_mil, alpha= sales_per_mil)) +
  ggtitle("User scores vs all game soundtracks")

best_selling_user_vs_total_views <-
  game_soundtrack_df %>%
  filter(global_sales >= 1) %>%
  group_by(game_user_score) %>%
  summarize(avg_view= mean(log(video_total_views)))

P2<- ggplot(best_selling_user_vs_total_views, aes(x= game_user_score, y=avg_view)) +
  geom_point() +
  geom_smooth(method = lm, se= TRUE)+
  ggtitle("Games +1 million sold")

second_best_selling_user_vs_total_views <-
  game_soundtrack_df %>%
  filter(global_sales < 1) %>%
  filter(global_sales >= 0.75) %>%
  group_by(game_user_score) %>%
  summarize(avg_view= mean(video_total_views))

P3<- ggplot(second_best_selling_user_vs_total_views, aes(x= game_user_score, y=avg_view)) +
  geom_point() +
  geom_smooth(method = lm, se= TRUE)+
  ggtitle("Games three quarters of a million sold")


third_best_selling_user_vs_total_views <-
  game_soundtrack_df %>%
  filter(global_sales < 0.75) %>%
  filter(global_sales >= 0.5) %>%
  group_by(game_user_score) %>%
  summarize(avg_view= mean(log(video_total_views)))

P4<- ggplot(third_best_selling_user_vs_total_views, aes(x= game_user_score, y=avg_view)) +
  geom_point() +
  geom_smooth(method = lm, se= TRUE)+
  ggtitle("Games half a million sold")


fourth_best_selling_user_vs_total_views <-
  game_soundtrack_df %>%
  filter(global_sales < 0.5) %>%
  filter(global_sales >= 0.25) %>%
  group_by(game_user_score) %>%
  summarize(avg_view= mean(log(video_total_views)))

P5<- ggplot(fourth_best_selling_user_vs_total_views, aes(x= game_user_score, y=avg_view)) +
  geom_point() +
  geom_smooth(method = lm, se= TRUE)+
  ggtitle("Games quarter million sold")


fifth_best_selling_user_vs_total_views <-
  game_soundtrack_df %>%
  filter(global_sales < 0.25) %>%
  filter(global_sales >= 0) %>%
  group_by(game_user_score) %>%
  summarize(avg_view= mean(log(video_total_views)))

P6<- ggplot(fifth_best_selling_user_vs_total_views, aes(x= game_user_score, y=avg_view)) +
  geom_point() +
  geom_smooth(method = lm, se= TRUE) +
  ggtitle("Games less than a quarter million sold")

multiplot(P1, P2, P3, P4, P5, P6, cols= 3)




game_type_frequency<-
  game_soundtrack_df %>%
  filter(is.na(game_type) == FALSE) %>%
  group_by(game_type)

plot1<- ggplot(game_type_frequency, aes(x= game_type)) + 
  geom_bar(fill= "steelblue")

game_type_frequency_top<- 
  game_soundtrack_df %>%
  filter(is.na(game_type) == FALSE) %>%
  group_by(game_type) %>%
  count() %>%
  filter(n > 20)

plot2<- ggplot(game_type_frequency_top, aes(x= game_type, y= n)) + 
  geom_bar(stat= "identity", fill= "steelblue") +
  ggtitle("High Game Genre Frequency in top 100")

game_type_frequency_bottom<- 
  game_soundtrack_df %>%
  filter(is.na(game_type) == FALSE) %>%
  group_by(game_type) %>%
  count() %>%
  filter(n < 20) %>%
  filter(n > 1)

plot3<- ggplot(game_type_frequency_bottom, aes(x= game_type, y= n)) + 
  geom_bar(stat= "identity", fill= "steelblue") +
  ggtitle("Low Game Genre Frequency in top 100")

#lets compare genres to soundtrack popularity
#some genres have many more games in it, so to normalize it divide the total soundtrack views with the number of games in the genre

genre_vs_soundtrack<-
  game_soundtrack_df %>%
  filter(is.na(game_type) == FALSE) %>%
  mutate(sound_popularity= mean(video_total_views)) %>%
  group_by(game_type) %>%
  count() %>%
  filter(n > 20)
  

plot4<- ggplot(genre_vs_soundtrack, aes(x= game_type, y= sound_popularity)) + 
  geom_bar(stat= "identity", fill= "steelblue") +
  ggtitle("Soundtrack Popularity per Game Genre")



#are there certain game types that have more popular soundtracks?
game_user_score_vs_soundtrack_pop_vs_type<- game_soundtrack_df %>% mutate(user_score = game_user_score, soundtrack_popularity= video_average_views, game_genre = game_type)%>% group_by(user_score) #%>% summarise(avg_sound_popularity = mean(soundtrack_popularity))

ggplot(game_user_score_vs_soundtrack_pop_vs_type, aes(x = user_score, y= soundtrack_popularity)) + geom_point(aes(x= user_score, y= soundtrack_popularity, colour =factor(game_type))) + geom_smooth(method = lm, se= TRUE)


game_type_vs_sound<- game_soundtrack_df %>% mutate(user_score = game_user_score, soundtrack_popularity= video_average_views, num_in_genre = table(game_soundtrack_df$game_type)[game_soundtrack_df$game_type]) %>% group_by(game_type) #%>% summarise(num_game_in_genre = table(game_type))


ggplot(game_type_vs_sound, aes(x = user_score, y= soundtrack_popularity)) + geom_point(aes(x= user_score, y= soundtrack_popularity, colour =factor(game_type), size= numeric(num_in_genre))) + geom_smooth(method = lm, se= TRUE)

game_type_vs_sound <- game_type_vs_sound %>% ungroup() %>% 
  mutate(game_type = as.character(game_type)) 

other_types <- game_type_vs_sound %>% 
  group_by(game_type) %>% summarize(n = n()) %>% 
  filter(n < 30) %>% 
  select(game_type) 
game_type_vs_sound2 <- game_type_vs_sound %>% 
  ungroup() %>% 
  mutate(game_type = ifelse(game_type %in% other_types$game_type, "other", game_type)) table(game_type_vs_sound2$game_type) ggplot(game_type_vs_sound2, aes(x = user_score, y= soundtrack_popularity)) + geom_point(aes(colour =factor(game_type), size = as.numeric(num_in_genre)))



game_type_vs_sound <- game_type_vs_sound %>% ungroup() %>% mutate(game_type = as.character(game_type)) other_types <- game_type_vs_sound %>% group_by(game_type) %>% summarize(n = n()) %>% filter(n < 30) %>% select(game_type) game_type_vs_sound2 <- game_type_vs_sound %>% ungroup() %>% mutate(game_type = ifelse(game_type %in% other_types$game_type, "other", game_type)) table(game_type_vs_sound2$game_type) ggplot(game_type_vs_sound2, aes(x = user_score, y= soundtrack_popularity)) + geom_point(aes(colour =factor(game_type), size = as.numeric(num_in_genre)))





#does a higher game_critic_score result in higher sales?
fit1<- lm(global_sale_list ~ game_critic_score, regression_df)
summary(fit1)
#yes: 0.039014

#does a higher higher game_user_critic_score result in higher sales?
fit2<- lm(global_sale_list ~ game_user_score, regression_df)
summary(fit2)
#no: -0.08329










