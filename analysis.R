# this page is dedicated to doing analysis of the youtube game soundtrack dataset datasets
#goal: see if game companies should invest more in their soundtracks

#load dataframe
final_game_title_df<- read.csv("final_game_title_df.csv")

#remove favorites column because it is empty
final_game_title_df <- final_game_title_df[-9]

#remove games without videos, because analysis cannot be done with them

final_game_title_df <- subset.data.frame(final_game_title_df, subset = final_game_title_df$video_num > 0)

#is soundtrack popularity related to game popularity

#see if the difference between critics and users scores has decreased or increased over the years
final_game_title_df %>% mutate(year = year(all_game_dates), diff = (all_game_scores)/10 - all_game_user_ratings) %>% group_by(year) %>% summarize(avg_diff = mean(diff))

#for now, we are going to assume popularity can be marked by the number of average views per video
#later we'll use google trends score

final_game_title_df <- final_game_title_df %>% mutate(average_views = total_views/video_num)

#find which user scores have the most views per video
#games around the 7 user score have the most views per video

final_game_title_df %>% mutate(all_game_user_ratings, average_views) %>% group_by(user_ratings = round(all_game_user_ratings)) %>% summarize(avg_views_rating = mean(average_views))


#final_game_title_df %>% mutate(all_game_user_ratings, soundtrack_pop_intensity = (total_views/video_num)) %>% group_by(user_ratings =round(all_game_user_ratings)) %>% summarize(avg_pop_intensity = mean(soundtrack_pop_intensity))

final_game_title_2_df <- final_game_title_df %>% mutate(all_game_titles, soundtrack_pop_intensity = (total_views/video_num))



#the more views a game collectively has, it would make sense the there would be more comments 
cor(final_game_title_df$total_views, final_game_title_df$total_comments)

#figure out what drives the comments....the more popular they are? or the more conflicted the opinions are?
#test correlation between popularity and average number of comments per video
cor(final_game_title_df$average_views, final_game_title_df$total_comments/final_game_title_df$video_num)
#0.5687507

#compare total likes vs comments, normalized with average views
cor(final_game_title_df$total_likes/final_game_title_df$average_views, final_game_title_df$total_comments/final_game_title_df$average_views)
#0.5311705

#compare total likes vs comments, normalized with average views
cor(final_game_title_df$total_dislikes/final_game_title_df$average_views, final_game_title_df$total_comments/final_game_title_df$average_views)
#0.2963238

#compare the difference between total likes and total dislikes vs comments, normalized with average views
final_game_title_df <- final_game_title_df %>% mutate(opinon_diff = total_likes- total_dislikes)

cor(final_game_title_df$opinon_diff/final_game_title_df$average_views, final_game_title_df$total_comments/final_game_title_df$average_views)
#0.5299687




#find if better games have better soundtracks
library(ggplot2)

#plot soundtrack popularity vs critics game score
game_critic_score_vs_soundtrack_pop<- final_game_title_df %>% mutate(critic_score = game_critic_score, soundtrack_popularity= video_average_views)%>% group_by(critic_score) %>% summarise(avg_sound_popularity = mean(soundtrack_popularity))

ggplot(game_critic_score_vs_soundtrack_pop, aes(x = critic_score, y= avg_sound_popularity)) + geom_point() + geom_smooth(method = lm, se= TRUE)

#plot doesn't really return anything, let's compare user scores vs soundtrack popularity next
game_user_score_vs_soundtrack_pop<- final_game_title_df %>% mutate(user_score = game_user_score, soundtrack_popularity= video_average_views)%>% group_by(user_score) %>% summarise(avg_sound_popularity = mean(soundtrack_popularity))

ggplot(game_user_score_vs_soundtrack_pop, aes(x = user_score, y= avg_sound_popularity)) + geom_point() + geom_smooth(method = lm, se= TRUE)
#results in more of an upward trend

#see if there is a correlation between critics score and soundtrack popularity
cor(final_game_title_df$game_critic_score, final_game_title_df$video_average_views)
#NA
cor(final_game_title_df$game_user_score, final_game_title_df$video_average_views)
#NA


#are there certain game types that have more popular soundtracks?
game_user_score_vs_soundtrack_pop_vs_type<- final_game_title_df %>% mutate(user_score = game_user_score, soundtrack_popularity= video_average_views, game_genre = game_type)%>% group_by(user_score) #%>% summarise(avg_sound_popularity = mean(soundtrack_popularity))

ggplot(game_user_score_vs_soundtrack_pop_vs_type, aes(x = user_score, y= soundtrack_popularity)) + geom_point(aes(x= user_score, y= soundtrack_popularity, colour =factor(game_type))) + geom_smooth(method = lm, se= TRUE)


game_type_vs_sound<- final_game_title_df %>% mutate(user_score = game_user_score, soundtrack_popularity= video_average_views, num_in_genre = table(final_game_title_df$game_type)[final_game_title_df$game_type]) %>% group_by(game_type) #%>% summarise(num_game_in_genre = table(game_type))


ggplot(game_type_vs_sound, aes(x = user_score, y= soundtrack_popularity)) + geom_point(aes(x= user_score, y= soundtrack_popularity, colour =factor(game_type), size= factor(num_in_genre))) + geom_smooth(method = lm, se= TRUE)


table(final_game_title_df$game_type)[final_game_title_df$game_type]

final_game_title_df %>% ggplot(aes(total_views, total_likes)) + geom_point()