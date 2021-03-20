library(tidyverse)
library(devtools)
library(ggrepel)
library(ggimage)
library(nflfastR)
library(readr)
library(XML)
library(ggplot2)
library(Metrics)
library(sjmisc)

generate_data_for_modeling <- function(season){
  Full_Regular_Season <- readRDS(
                url(
                  paste0("https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_",season,".rds")
                   )
                ) %>%
              filter(rush == 1 | pass == 1, week <= 17, !is.na(epa), !is.na(posteam), posteam != "")
  
  #Explore the season's distribution of points (Overall, Home, and Away)
  Full_Regular_Season_Scores <- Full_Regular_Season %>% 
    select(game_id, home_score, away_score, total) 
  
  Full_Regular_Season_Scores <- unique(Full_Regular_Season_Scores)
  
  #View a histogram for each score
  ggplot(Full_Regular_Season_Scores, aes(home_score)) + geom_bar() + ggtitle(paste0(season, " Home Team Score Distribution"))
  ggplot(Full_Regular_Season_Scores, aes(away_score)) + geom_bar() + ggtitle(paste0(season, " Away Team Score Distribution"))
  ggplot(Full_Regular_Season_Scores, aes(total)) + geom_bar() + ggtitle(paste0(season, " Total Score Distribution"))
  
  #View Statistical Summary for each score 
  summary(Full_Regular_Season_Scores$home_score)
  summary(Full_Regular_Season_Scores$away_score)
  summary(Full_Regular_Season_Scores$total)
  
  #Build Explanatoru Variables for Model predicting Home and Away Score
  full_offense <- Full_Regular_Season %>%
    group_by(game_id, posteam, season, week) %>% 
    summarize(plays = n(),
              off_epa_play = mean(epa),
              off_total_epa = sum(epa),
              off_success_rate = mean(success),
              explosive_play_rate = sum(epa>0.75) / plays,
              bad_play_rate = sum(epa < -0.6)/ plays,
              avg_wpa = mean(wpa, na.rm=T),
              series_success = mean(series_success),
              cpoe = mean(cpoe, na.rm=T),
              avg_yardline = mean(100 - (yardline_100)),
              home_score = home_score,
              away_score = away_score,
              total = total,
              home_team = home_team,
              away_team = away_team)
  
  #Clean up
  full_offense$is_home_team <- ifelse(full_offense$home_team == full_offense$posteam, TRUE, FALSE)
  full_offense <- unique(full_offense)
  
  
  full_defense <- Full_Regular_Season %>%
    group_by(game_id, defteam, season, week) %>% 
    summarize(plays = n(),
              def_good_play_rate = (sum(epa < -0.6)/plays),
              def_bad_play_rate = (sum(epa > 0.20)/plays),
              home_score = home_score,
              away_score = away_score,
              total = total,
              home_team = home_team,
              away_team = away_team)
  #Clean up
  full_defense$is_home_team <- ifelse(full_defense$home_team == full_defense$defteam, TRUE, FALSE)
  full_defense <- unique(full_defense)
  
  #Separate home and away stats                 
  home_full_offense <- full_offense[full_offense$is_home_team == TRUE,]
  home_full_defense <- full_defense[full_defense$is_home_team == TRUE,]
  away_full_offense <- full_offense[full_offense$is_home_team != TRUE,]
  away_full_defense <- full_defense[full_defense$is_home_team != TRUE,]
  
  
  #Now we have all the data for home and away teams that we need, consolodate into a single DF
  #For home and away  by adding whatever defensive variables are calculated
  home_results <- home_full_offense
  home_results$def_good_play_rate <- home_full_defense$def_good_play_rate
  home_results$def_bad_play_rate <- home_full_defense$def_bad_play_rate
  
  away_results <- away_full_offense
  away_results$def_good_play_rate <- away_full_defense$def_good_play_rate
  away_results$def_bad_play_rate <- away_full_defense$def_bad_play_rate
  
  return(list(home_results, away_results))
}

#Set the season here
season <- "2018"
All_season <- generate_data_for_modeling(season)

#Do not attempt to set variables 
if(length(All_season) == 2) {
  home_results <- All_season[[1]]
  away_results <- All_season[[2]]
}

#Finally, generate a linear model predicting home score and away score
# home_fit <- lm(home_score ~ 
#                  off_epa_play + off_total_epa + 
#                  explosive_play_rate + bad_play_rate, 
#                data = home_results)
# 
# home_preds <- predict(home_fit, home_results) %>%
#               as_tibble() %>%
#               rename(home_prediction = value) %>%
#               round(1) %>%
#               bind_cols(home_results) %>%
#               select(game_id, season, week, home_team, home_prediction, home_score, off_epa_play) %>%
#               mutate(prediction_minus_actual = home_prediction - home_score)
# 
# away_fit <- lm(away_score ~ 
#                  off_epa_play + off_total_epa + 
#                  explosive_play_rate + bad_play_rate, 
#                data = away_results)
# 
# away_preds <- predict(away_fit, away_results) %>%
#               as_tibble() %>%
#               rename(away_prediction = value) %>%
#               round(1) %>%
#               bind_cols(away_results) %>%
#               select(game_id, season, week, away_team, away_prediction, away_score, off_epa_play) %>%
#               mutate(prediction_minus_actual = away_prediction - away_score)

#Training and Testing datasets from the 2018 Season 
set.seed(1)
row.number <- sample(1:nrow(home_results), 0.8*nrow(home_results))
home_train <- home_results[row.number,]
home_test <- home_results[-row.number,]

home_model <- lm(home_score ~ 
                   off_epa_play + off_total_epa + 
                   explosive_play_rate + bad_play_rate, 
                 data = home_train)

home_predictions <- predict(home_model, newdata = home_test)
rmse <- rmse(home_test$home_score, home_predictions)
R_squared <- cor(home_test$home_score, home_predictions)^2





















