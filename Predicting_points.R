library(tidyverse)
library(devtools)
library(ggrepel)
library(ggimage)
library(nflfastR)
library(readr)
library(XML)
library(ggplot2)
library(Metrics)

## Current State Description (4/17/2021):
# Use code from Predicting_Score and place into function to work for any season
# Train and test the models for the desired season, observing RMSE and R^2



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
                            off_interceptions = sum(interception),
                            off_fumbles = sum(fumble),
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
                            def_interceptions = sum(interception),
                            def_fumbles = sum(fumble),
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
  
  #Clean up NAs
  home_full_defense$def_interceptions[is.na(home_full_defense$def_interceptions)] <- 0
  home_full_defense$def_fumbles[is.na(home_full_defense$def_fumbles)] <- 0
  
  home_results$def_interceptions <- home_full_defense$def_interceptions
  home_results$def_fumbles <- home_full_defense$def_fumbles
  
  away_results <- away_full_offense
  away_results$def_good_play_rate <- away_full_defense$def_good_play_rate
  away_results$def_bad_play_rate <- away_full_defense$def_bad_play_rate
  
  #Clean up NAs
  away_full_defense$def_interceptions[is.na(away_full_defense$def_interceptions)] <- 0
  away_full_defense$def_fumbles[is.na(away_full_defense$def_fumbles)] <- 0
  
  away_results$def_interceptions <- away_full_defense$def_interceptions
  away_results$def_fumbles <- away_full_defense$def_fumbles
  
  return(list(home_results, away_results))
}

#Set the season here
season <- "2020"
All_season <- generate_data_for_modeling(season)

#Do not attempt to set variables for inaccurately generated datasets
if(length(All_season) == 2) {
  home_results <- All_season[[1]]
  away_results <- All_season[[2]]
}

#Finally, generate a linear model predicting home score and away score
home_fit <- lm(home_score ~
                 off_epa_play + off_total_epa +
                 explosive_play_rate + bad_play_rate + 
                 def_interceptions + def_fumbles +
                 def_good_play_rate - def_bad_play_rate -
                 off_interceptions - off_fumbles,
               data = home_results)

home_preds <- predict(home_fit, home_results) %>%
              as_tibble() %>%
              rename(home_prediction = value) %>%
              round(1) %>%
              bind_cols(home_results) %>%
              select(game_id, season, week, home_team, home_prediction, home_score, off_epa_play) %>%
              mutate(prediction_minus_actual = home_prediction - home_score)

away_fit <- lm(away_score ~
                 off_epa_play + off_total_epa +
                 explosive_play_rate + bad_play_rate + 
                 def_interceptions + def_fumbles +
                 def_good_play_rate - def_bad_play_rate -
                 off_interceptions - off_fumbles,
               data = away_results)

away_preds <- predict(away_fit, away_results) %>%
              as_tibble() %>%
              rename(away_prediction = value) %>%
              round(1) %>%
              bind_cols(away_results) %>%
              select(game_id, season, week, away_team, away_prediction, away_score, off_epa_play) %>%
              mutate(prediction_minus_actual = away_prediction - away_score)

# Visualize Prediction Accuracy 
# ggplot(data = home_preds, aes(game_id, prediction_minus_actual)) +
#   geom_col() +
#     geom_hline(yintercept = mean(abs(home_preds$prediction_minus_actual)), color="blue")

mean(abs(home_preds$prediction_minus_actual))
mean(abs(away_preds$prediction_minus_actual))

#Training and Testing home datasets from the selected season 
set.seed(1)
row.number <- sample(1:nrow(home_results), 0.8*nrow(home_results))
home_train <- home_results[row.number,] #Train with 80% of the data
home_test <- home_results[-row.number,] #Test with the remaining 20%

home_model <- lm(home_score ~ 
                   off_epa_play + off_total_epa +
                   explosive_play_rate + bad_play_rate + 
                   def_interceptions + def_fumbles +
                   def_good_play_rate - def_bad_play_rate -
                   off_interceptions - off_fumbles, 
                 data = home_train)

home_predictions <- predict(home_model, newdata = home_test)
home_rmse <- rmse(home_test$home_score, home_predictions)
home_R_squared <- cor(home_test$home_score, home_predictions)^2

#Training and Testing away datasets from the selected season 
row.number <- sample(1:nrow(away_results), 0.8*nrow(away_results))
away_train <- away_results[row.number,] #Train with 80% of the data
away_test <- away_results[-row.number,] #Test with the remaining 20%

away_model <- lm(away_score ~ 
                   off_epa_play + off_total_epa +
                   explosive_play_rate + bad_play_rate + 
                   def_interceptions + def_fumbles +
                   def_good_play_rate - def_bad_play_rate -
                   off_interceptions - off_fumbles, 
                 data = away_train)

away_predictions <- predict(away_model, newdata = away_test)
away_rmse <- rmse(away_test$away_score, away_predictions)
away_R_squared <- cor(away_test$away_score, away_predictions)^2




















