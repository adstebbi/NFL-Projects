library(tidyverse)
library(devtools)
library(lubridate)
library(stringr)
library(rlang)
library(ggrepel)
library(ggimage)
library(nflfastR)
library(readr)
library(XML)

#Global Variables
NUM_TEAMS = 32

#Avoid scientific notation for numbers
options(scipen = 9999)

#Obtain 2019 season (regular + post)
data2K20 <- readRDS(url('https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_2020.rds'))

#Only look at non special-teams plays, with a valid EPA
no_sp_full_epa <- data2K20 %>%
  filter(!is.na(epa), special == 0)

#Figure out win probability calculation for win probability being between .05 < WP < 0.95 to eliminate garbage time points
#Filter for downs 1,2,3,4

new_dataset <- no_sp_full_epa %>%
  filter(wp > 0.05 & wp < 0.95, 
         down == 1 |
           down == 2 |
           down == 3 |
           down == 4)

#Season Week Home_Team Away_Team Home_season_EPA_avg Home_win Winning_team Losing_team Away_season_EPA_avg Home_avg_mrg_victory Away_avg_mrg_victory Home_season_winning_Percentage Away_season_winning_percentage

data2K10 <- readRDS(url('https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_2010.rds'))

#Apply modern names to dated teams
data2K10 <- data2K10 %>%
  mutate(
    team = case_when(
      team == 'OAK' ~ 'LV',
      team == 'SD' ~ 'LAC',
      team == 'STL' ~ 'LA',
      TRUE ~ team
    )
  )

log_reg_model_data10 <- data2K10 %>% 
                          group_by(game_id) %>%
                            select(home_team, away_team, posteam, 
                                   defteam, away_score, home_score, 
                                   result, total, epa) %>% 
                              mutate(home_win = ifelse(result < 0, 
                                                       FALSE, 
                                                       TRUE), 
                                     winning_team = ifelse(home_win == TRUE, 
                                                           home_team, 
                                                           away_team))

#Get offensive EPA per team and defensive EPA per team
offensive_EPA_per_team <- log_reg_model_data10 %>%
                            group_by(posteam) %>% 
                              summarize(epa = mean(epa, na.rm = TRUE)) 
defensive_EPA_per_team <- log_reg_model_data10 %>%
                            group_by(defteam) %>% 
                              summarize(epa = mean(epa, na.rm = TRUE)) 

#Make adjustments due to weird issue with extra rows
def_rows <- nrow(defensive_EPA_per_team) - 1
defensive_EPA_per_team <- defensive_EPA_per_team[1:def_rows,]

off_rows <- nrow(offensive_EPA_per_team) - 1
offensive_EPA_per_team <- offensive_EPA_per_team[2:off_rows,]


#To calculate AMOV, group by each winning team from each game in the season(use subset to get one row for each game), calculate their total MOV, 
#how many times they appeared in the dataset, and then divide to get the average

avg_margin_of_victory <- log_reg_model_data10 %>%
                          subset(!duplicated(game_id)) %>%
                             group_by(winning_team) %>%
                               summarise(total_MOV = sum(abs(result)),
                                         freq = table(winning_team),
                                         avg_MOV = round((total_MOV / freq),2))
 


#Now we have offensive epa per team, defensive EPA per team, average margin of victory per team, 
#just need home WP and away WP

one_row_per_game_home <- log_reg_model_data10 %>%
                          subset(!duplicated(game_id)) %>%
                            group_by(home_team) %>%
                              summarise(home_wins = sum(home_win == TRUE), 
                                        total_home_games = n()) 
one_row_per_game_away <- log_reg_model_data10 %>%
                          subset(!duplicated(game_id)) %>%
                            group_by(away_team) %>%
                              summarise(away_wins = sum(home_win == FALSE), 
                                        total_away_games = n()) 

#Combine home and away win count DFs, mutate to get percentages
team_home_and_away_WP <- full_join(one_row_per_game_home, 
                                   one_row_per_game_away, 
                                   by = c("home_team" = "away_team"))        
team_home_and_away_WP$home_WP <- round((team_home_and_away_WP$home_wins / team_home_and_away_WP$total_home_games),2)
team_home_and_away_WP$away_WP <- round((team_home_and_away_WP$away_wins / team_home_and_away_WP$total_away_games),2)

                                
                              
# 
#                         
# home_winning_percentage = sum(winning_team == home_team) / 
#   (names(table(winning_team)) == home_team)
# 
#  
#                         
# home_and_away_WP <- function(subsetted_data_frame, fill_data_frame){
#   for(i in 1:nrow(subsetted_data_frame)){
#     #Home Team calculations first
#       current_team <- subsetted_data_frame[i, "home_team"]
#       print(current_team)
#       team_frequency <- sum(subsetted_data_frame$home_team == current_team)
#       print(team_frequency)
#       home_wins <- sum(subsetted_data_frame$winning_team == current_team)
#       print(home_wins)
#       fill_data_frame$winning_team <- current_team
#       fill_data_frame$home_wp <- home_wins / team_frequency
#     
#     #Away Team calculations next
#       current_team <- subsetted_data_frame[i, "away_team"]
#       team_frequency <- sum(subsetted_data_frame$away_team == current_team)
#       away_wins <- sum(subsetted_data_frame$winning_team == current_team)
#       fill_data_frame$winning_team <- current_team
#       fill_data_frame$away_wp <- away_wins / team_frequency
#   }
}

home_WP <- data.frame()

home_and_away_WP(one_row_per_game, home_WP)




# Actually build Logistic Regression Model after Dataframe is proper
# glm(Home_win ~ )
# 
# 
# glm.fit <- glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume, data = Smarket, family = binomial)
# summary(glm.fit)
# 
# glm.probs <- predict(glm.fit,type = "response")
# glm.probs[1:5]
# 
# glm.pred <- ifelse(glm.probs > 0.5, "Up", "Down")
# table(glm.pred,Direction)
# 
# # Make training and test set
# train = Year<2005
# glm.fit <- glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume,
#                data = Smarket,
#                family = binomial,
#                subset = train)
# 
# glm.probs <- predict(glm.fit,
#                      newdata = Smarket[!train,],
#                      type = "response")
# 
# glm.pred <- ifelse(glm.probs > 0.5, "Up", "Down")
# 
# Direction.2005 = Smarket$Direction[!train]
# table(glm.pred, Direction.2005)
# 
# mean(glm.pred == Direction.2005)


# set.seed(1234)
# splitIndex <- createDataPartition(titanicDF[,outcomeName], p = .75, list = FALSE, times = 1)
# trainDF <- titanicDF[ splitIndex,]
# testDF  <- titanicDF[-splitIndex,]













