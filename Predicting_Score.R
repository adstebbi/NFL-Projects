library(tidyverse)
library(devtools)
library(ggrepel)
library(ggimage)
library(nflfastR)
library(readr)
library(XML)
library(ggplot2)

## Current State Description (4/17/2021):
# 
# -Reads only 2019 plays, filtering to clean up
# -Graph & investigate the distribution of home and away scores for that season 
# -Split stats into offense and defense dataframes, using summary variables
# that will be used as x values for the score prediction model
# -After cleaning and creating singular models for offense and defense, separate 
# results into combinations of offense and defense, home and away
# -Create home & away models, home & away predictions
# -View models to determine if proper conditions are met to model a normal distribution

All_2019 <- readRDS(url("https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_2019.rds")) %>%
            filter(rush == 1 | pass == 1, week <= 17, !is.na(epa), !is.na(posteam), posteam != "")

#Explore the season's distribution of points (Overall, Home, and Away)
All_2019_Scores <- All_2019 %>% 
              select(game_id, home_score, away_score, total) 

All_2019_Scores <- unique(All_2019_Scores)

#View a histogram for each score
ggplot(All_2019_Scores, aes(home_score)) + geom_bar() + ggtitle("2019 Home Team Score Distribution")
ggplot(All_2019_Scores, aes(away_score)) + geom_bar() + ggtitle("2019 Away Team Score Distribution")
ggplot(All_2019_Scores, aes(total)) + geom_bar() + ggtitle("2019 Total Score Distribution")

#View Statistical Summary for each score 
summary(All_2019_Scores$home_score)
summary(All_2019_Scores$away_score)
summary(All_2019_Scores$total)

#Build Explanatory Variables for Model predicting Home and Away Score
offense_2019 <- All_2019 %>%
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
offense_2019$is_home_team <- ifelse(offense_2019$home_team == offense_2019$posteam, TRUE, FALSE)
offense_2019 <- unique(offense_2019)


defense_2019 <- All_2019 %>%
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
defense_2019$is_home_team <- ifelse(defense_2019$home_team == defense_2019$defteam, TRUE, FALSE)
defense_2019 <- unique(defense_2019)
          
#Separate home and away stats                 
home_offense_2019 <- offense_2019[offense_2019$is_home_team == TRUE,]
home_defense_2019 <- defense_2019[defense_2019$is_home_team == TRUE,]
away_offense_2019 <- offense_2019[offense_2019$is_home_team != TRUE,]
away_defense_2019 <- defense_2019[defense_2019$is_home_team != TRUE,]


#Now we have all the data for home and away teams that we need, consolodate into a single DF
#For home and away  by adding whatever defensive variables are calculated
home_results <- home_offense_2019
home_results$def_good_play_rate <- home_defense_2019$def_good_play_rate
home_results$def_bad_play_rate <- home_defense_2019$def_bad_play_rate

away_results <- away_offense_2019
away_results$def_good_play_rate <- away_defense_2019$def_good_play_rate
away_results$def_bad_play_rate <- away_defense_2019$def_bad_play_rate

#Finally, generate a linear model predicting home score and away score
home_fit <- lm(home_score ~ 
                off_epa_play + off_total_epa + 
                explosive_play_rate + bad_play_rate, 
                data = home_results)

home_preds <- predict(home_fit, home_results) %>%
              as_tibble() %>%
              rename(home_prediction = value) %>%
              round(1) %>%
              bind_cols(home_results) %>%
              select(game_id, season, week, home_team, home_prediction, home_score, off_epa_play) %>%
              mutate(prediction_minus_actual = home_prediction - home_score)

away_fit <- lm(away_score ~ 
                 off_epa_play + off_total_epa + explosive_play_rate + 
                 bad_play_rate, data = away_results)

away_preds <- predict(away_fit, away_results) %>%
              as_tibble() %>%
              rename(away_prediction = value) %>%
              round(1) %>%
              bind_cols(away_results) %>%
              select(game_id, season, week, away_team, away_prediction, away_score, off_epa_play) %>%
              mutate(prediction_minus_actual = away_prediction - away_score)

#Check assumptions to make sure a linear model is appropriate
#Residual v Fitted (No distinguishable pattern), QQ Plot follows the dashed line 
summary(home_fit)
summary(away_fit)

plot(home_fit)
plot(away_fit)


