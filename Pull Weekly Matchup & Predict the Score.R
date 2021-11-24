library(tidyverse)
library(devtools)
library(ggrepel)
library(ggimage)
library(nflfastR)
library(readr)
library(xml2)
library(ggplot2)
library(Metrics)
library(textreadr)
library(rvest)
library(gt)
library(sjmisc)
library(R.utils)
library(janitor)
library(XML)

## Current State Description (10/16/2021):
# Games for given week are properly scraped from team rankings website and placed into "weekly_matchup" DF
# Data for entire given season is generated, cleaned, and organized into required home & away DFs with proper stats
# Models are generated for home and away teams in upcoming matchups, and their scores are predicted using the available data
# up to this point. gt() is used to create a table to display this information in a consumable fashion
NFL_Teams <- c("Miami", "Baltimore", "Pittsburgh", "Minnesota", "NY Jets", "Dallas", "Atlanta", 
               "Green Bay", "Las Vegas", "Jacksonville", "LA Chargers", "Arizona", "Seattle", 
               "New Orleans", "Tennessee", "Washington", "Buffalo", "Cincinnati", "Cleveland", 
               "Detroit", "New England", "NY Giants", "Tampa Bay", "Chicago", "Denver", 
               "Indianapolis", "Kansas City", "LA Rams", "San Francisco", "Carolina", "Houston", 
               "Philadelphia")

Mascots <- c('Cardinals', 'Falcons', 'Ravens','Bills','Panthers','Bears','Bengals',
             'Browns','Cowboys','Broncos','Lions','Packers','Texans','Colts',
              'Jaguars','Chiefs','Raiders','Chargers','Rams','Dolphins','Vikings',
             'Patriots','Saints','Giants','Jets','Eagles','Steelers','49ers','Seahawks',
             'Buccaneers','Titans','Washington')


official_team_names <- c('Arizona Cardinals', 'Atlanta Falcons', "Baltimore Ravens", 'Buffalo Bills', 'Carolina Panthers',
                         'Chicago Bears', 'Cincinnati Bengals', 'Cleveland Browns', 'Denver Broncos', 'Dallas Cowboys',
                         'Detroit Lions', 'Green Bay Packers', 'Houston Texans', "Indianapolis Colts", "Jacksonville Jaguars", 
                         'Kansas City Chiefs', 'Las Vegas Raiders', 'Los Angeles Chargers', 'Los Angeles Rams', 'Miami Dolphins',  
                         'Minnesota Vikings','New England Patriots', 'New Orleans Saints', 'New York Giants', 'Philadelphia Eagles',
                         'New York Jets', 'Pittsburgh Steelers', 'San Francisco 49ers', 'Seattle Seahawks', 'Tampa Bay Buccaneers', 
                          'Tennessee Titans', 'Washington Football Team')

scrape_weekly_games <- function(season, week_num){
  
  url <- paste0("https://www.pro-football-reference.com/years/", 2021, "/games.htm") #Pull full season's games from ProFootballReference
  
  schedule <- read_html(url) %>%  
                html_nodes("table#games") %>% 
                html_table() %>% 
                .[[1]] %>%
                as.data.frame(.) #Scrape the table in order to get all future matchups
  
  class(schedule) #Make sure the data frame is populated and the correct structure
  rownames(schedule) <- NULL #Remove the duplicate row names by setting all to NULL
  
  schedule <- unique(schedule) #Remove unnecessary header lines
  week_num_matchups <- schedule[schedule$Week == week_num,] #Filter to the desired week's matchups
  
  #Change from names to abbreviations for ease of use
  
    for (i in 1:length(week_num_matchups)){
      week_num_matchups$home_team_abbv[i] <- case_when(
                                                       week_num_matchups$`Loser/tie`[i] == 'Arizona Cardinals'         ~ "ARI",
                                                        week_num_matchups$`Loser/tie`[i] == 'Atlanta Falcons'          ~ "ATL",
                                                        week_num_matchups$`Loser/tie`[i] == "Baltimore Ravens"         ~ "BAL",
                                                        week_num_matchups$`Loser/tie`[i] == 'Buffalo Bills'            ~ "BUF",
                                                        week_num_matchups$`Loser/tie`[i] == 'Carolina Panthers'        ~ "CAR",
                                                        week_num_matchups$`Loser/tie`[i] == 'Chicago Bears'            ~ "CHI",
                                                        week_num_matchups$`Loser/tie`[i] == 'Cincinnati Bengals'       ~ "CIN",
                                                        week_num_matchups$`Loser/tie`[i] == 'Cleveland Browns'         ~ "CLE",
                                                        week_num_matchups$`Loser/tie`[i] == 'Denver Broncos'           ~ "DEN",
                                                        week_num_matchups$`Loser/tie`[i] == 'Dallas Cowboys'           ~ "DAL",
                                                        week_num_matchups$`Loser/tie`[i] == 'Detroit Lions'            ~ "DET",
                                                        week_num_matchups$`Loser/tie`[i] == 'Green Bay Packers'        ~ "GB",
                                                        week_num_matchups$`Loser/tie`[i] == 'Houston Texans'           ~ "HOU",
                                                        week_num_matchups$`Loser/tie`[i] == "Indianapolis Colts"       ~ "IND",
                                                        week_num_matchups$`Loser/tie`[i] == "Jacksonville Jaguars"     ~ "JAX",
                                                        week_num_matchups$`Loser/tie`[i] == 'Kansas City Chiefs'       ~ "KC",
                                                        week_num_matchups$`Loser/tie`[i] == 'Las Vegas Raiders'        ~ "LV",
                                                        week_num_matchups$`Loser/tie`[i] == 'Los Angeles Chargers'     ~ "LAC",
                                                        week_num_matchups$`Loser/tie`[i] == 'Los Angeles Rams'         ~ "LA",
                                                        week_num_matchups$`Loser/tie`[i] == 'Miami Dolphins'           ~ "MIA",
                                                        week_num_matchups$`Loser/tie`[i] == 'Minnesota Vikings'        ~ "MIN",
                                                        week_num_matchups$`Loser/tie`[i] == 'New England Patriots'     ~ "NE",
                                                        week_num_matchups$`Loser/tie`[i] == 'New Orleans Saints'       ~ "NO",
                                                        week_num_matchups$`Loser/tie`[i] == 'New York Giants'          ~ "NYG",
                                                        week_num_matchups$`Loser/tie`[i] == 'Philadelphia Eagles'      ~ "PHI",
                                                        week_num_matchups$`Loser/tie`[i] == 'New York Jets'            ~ "NYJ",
                                                        week_num_matchups$`Loser/tie`[i] == 'Pittsburgh Steelers'      ~ "PIT",
                                                        week_num_matchups$`Loser/tie`[i] == 'San Francisco 49ers'      ~ "SF",
                                                        week_num_matchups$`Loser/tie`[i] == 'Seattle Seahawks'         ~ "SEA",
                                                        week_num_matchups$`Loser/tie`[i] == 'Tampa Bay Buccaneers'     ~ "TB",
                                                        week_num_matchups$`Loser/tie`[i] == 'Tennessee Titans'         ~ "TEN",
                                                        week_num_matchups$`Loser/tie`[i] == 'Washington Football Team' ~ "WAS"
        
                              )
      
      week_num_matchups$away_team_abbv[i] <- case_when(
                                                       week_num_matchups$`Winner/tie`[i] == 'Arizona Cardinals'        ~ "ARI",
                                                       week_num_matchups$`Winner/tie`[i] == 'Atlanta Falcons'          ~ "ATL",
                                                       week_num_matchups$`Winner/tie`[i] == "Baltimore Ravens"         ~ "BAL",
                                                       week_num_matchups$`Winner/tie`[i] == 'Buffalo Bills'            ~ "BUF",
                                                       week_num_matchups$`Winner/tie`[i] == 'Carolina Panthers'        ~ "CAR",
                                                       week_num_matchups$`Winner/tie`[i] == 'Chicago Bears'            ~ "CHI",
                                                       week_num_matchups$`Winner/tie`[i] == 'Cincinnati Bengals'       ~ "CIN",
                                                       week_num_matchups$`Winner/tie`[i] == 'Cleveland Browns'         ~ "CLE",
                                                       week_num_matchups$`Winner/tie`[i] == 'Denver Broncos'           ~ "DEN",
                                                       week_num_matchups$`Winner/tie`[i] == 'Dallas Cowboys'           ~ "DAL",
                                                       week_num_matchups$`Winner/tie`[i] == 'Detroit Lions'            ~ "DET",
                                                       week_num_matchups$`Winner/tie`[i] == 'Green Bay Packers'        ~ "GB",
                                                       week_num_matchups$`Winner/tie`[i] == 'Houston Texans'           ~ "HOU",
                                                       week_num_matchups$`Winner/tie`[i] == "Indianapolis Colts"       ~ "IND",
                                                       week_num_matchups$`Winner/tie`[i] == "Jacksonville Jaguars"     ~ "JAX",
                                                       week_num_matchups$`Winner/tie`[i] == 'Kansas City Chiefs'       ~ "KC",
                                                       week_num_matchups$`Winner/tie`[i] == 'Las Vegas Raiders'        ~ "LV",
                                                       week_num_matchups$`Winner/tie`[i] == 'Los Angeles Chargers'     ~ "LAC",
                                                       week_num_matchups$`Winner/tie`[i] == 'Los Angeles Rams'         ~ "LA",
                                                       week_num_matchups$`Winner/tie`[i] == 'Miami Dolphins'           ~ "MIA",
                                                       week_num_matchups$`Winner/tie`[i] == 'Minnesota Vikings'        ~ "MIN",
                                                       week_num_matchups$`Winner/tie`[i] == 'New England Patriots'     ~ "NE",
                                                       week_num_matchups$`Winner/tie`[i] == 'New Orleans Saints'       ~ "NO",
                                                       week_num_matchups$`Winner/tie`[i] == 'New York Giants'          ~ "NYG",
                                                       week_num_matchups$`Winner/tie`[i] == 'Philadelphia Eagles'      ~ "PHI",
                                                       week_num_matchups$`Winner/tie`[i] == 'New York Jets'            ~ "NYJ",
                                                       week_num_matchups$`Winner/tie`[i] == 'Pittsburgh Steelers'      ~ "PIT",
                                                       week_num_matchups$`Winner/tie`[i] == 'San Francisco 49ers'      ~ "SF",
                                                       week_num_matchups$`Winner/tie`[i] == 'Seattle Seahawks'         ~ "SEA",
                                                       week_num_matchups$`Winner/tie`[i] == 'Tampa Bay Buccaneers'     ~ "TB",
                                                       week_num_matchups$`Winner/tie`[i] == 'Tennessee Titans'         ~ "TEN",
                                                       week_num_matchups$`Winner/tie`[i] == 'Washington Football Team' ~ "WAS"
                                                       
              )
    }

  return(week_num_matchups)
}

generate_data_for_modeling <- function(season, week_num){
  
  Full_Regular_Season <- readRDS(
    url(
      paste0("https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_",season,".rds")
    )
  ) %>%
    filter(rush == 1 | pass == 1, week <= week_num, !is.na(epa), !is.na(posteam), posteam != "")
  
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
  
  colnames(Full_Regular_Season)
  
  Full_Regular_Season$def_final_score <- ifelse(Full_Regular_Season$home_team == Full_Regular_Season$defteam, 
                                                Full_Regular_Season$away_score, 
                                                Full_Regular_Season$home_score) #new
  
  #Build Explanatory Variables for Model predicting Home and Away Score
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
                            total_line = total_line,
                            spread_line = spread_line,
                            total = total,
                            result = result,
                            home_team = home_team,
                            away_team = away_team)
                
  #Clean up
  full_offense$is_home_team <- ifelse(full_offense$home_team == full_offense$posteam, TRUE, FALSE)
  full_offense <- unique(full_offense)
  
  
  full_defense <- Full_Regular_Season %>%
                  group_by(defteam, season, week) %>% 
                  summarize(plays = n(),
                            def_good_play_rate = (sum(epa < -0.6)/plays),
                            def_bad_play_rate = (sum(epa > 0.20)/plays),
                            home_score = home_score,
                            away_score = away_score,
                            def_interceptions = sum(interception),
                            def_fumbles = sum(fumble),
                            total_line = total_line,
                            spread_line = spread_line,
                            total = total,
                            result = result,
                            opponent_ppg = mean(def_final_score), #new
                            def_epa_pp = (sum(epa)/plays),
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
  home_results$opponent_ppg <- home_full_defense$opponent_ppg #new
  home_results$def_epa_pp <- home_full_defense$def_epa_pp
  
  #Clean up NAs
  home_full_defense$def_interceptions[is.na(home_full_defense$def_interceptions)] <- 0
  home_full_defense$def_fumbles[is.na(home_full_defense$def_fumbles)] <- 0
  
  home_results$def_interceptions <- home_full_defense$def_interceptions
  home_results$def_fumbles <- home_full_defense$def_fumbles
  
  away_results <- away_full_offense
  away_results$def_good_play_rate <- away_full_defense$def_good_play_rate
  away_results$def_bad_play_rate <- away_full_defense$def_bad_play_rate
  away_results$opponent_ppg <- away_full_defense$opponent_ppg #new
  away_results$def_epa_pp <- away_full_defense$def_epa_pp
  
  #Clean up NAs
  away_full_defense$def_interceptions[is.na(away_full_defense$def_interceptions)] <- 0
  away_full_defense$def_fumbles[is.na(away_full_defense$def_fumbles)] <- 0
  
  away_results$def_interceptions <- away_full_defense$def_interceptions
  away_results$def_fumbles <- away_full_defense$def_fumbles
  
  return(list(home_results, away_results))
}

## End of function

#Set the season, week number to predict here
season <- "2021"
week_number <- 12
All_season <- generate_data_for_modeling(season, week_number)

#Do not attempt to set variables for inaccurately generated datasets
if(length(All_season) == 2) {
  home_results <- All_season[[1]]
  away_results <- All_season[[2]]
} else {
  print("CALCULATION ERROR")
}

full_stats <- full_join(away_results, home_results) 

teams <- scrape_weekly_games(season, week_number)

away_teams <- teams$away_team_abbv
home_teams <- teams$home_team_abbv

#Get this week's games and teams
weekly_matchup <- tibble(Home_Team = home_teams,
                             Away_Team = away_teams) %>% unique()

#Create DF to use to calculate averages for each team that will generate 
#the numbers used in the testing data
all_results <- full_join(home_results, away_results)

final_summary <- all_results %>%
                  group_by(posteam) %>%
                    #na.omit %>%
                      summarise(Team = posteam,
                                off_epa_play = mean(off_epa_play),
                                season = 2021,
                                week = week_number,
                                plays = sum(plays),
                                off_total_epa = mean(off_total_epa),
                                off_success_rate = mean(off_success_rate),
                                explosive_play_rate = mean(explosive_play_rate),
                                bad_play_rate = mean(bad_play_rate), 
                                avg_wpa = mean(avg_wpa),
                                series_success = mean(series_success),
                                cpoe = mean(cpoe),
                                opponent_ppg = mean(opponent_ppg), #new
                                def_epa_pp = mean(def_epa_pp),
                                avg_yardline = mean(avg_yardline),
                                def_interceptions = sum(def_interceptions),
                                def_fumbles = sum(def_fumbles),
                                def_good_play_rate = mean(def_good_play_rate),
                                def_bad_play_rate = mean(bad_play_rate),
                                off_interceptions = sum(off_interceptions),
                                off_fumbles = sum(off_fumbles))


#Replace NA with zeros
final_summary[is.na(final_summary)] <- 0

#Build training dataset with summary data
#Training and testing the home model using the data from the selected season
 home_train <- home_results[,] #Train with 100% of the 2021 data thus far
 away_train <- away_results[,] #Train with 100% of the 2021 data thus far
 
 home_test <- data_frame(game_id = NA,
                         posteam = NA,
                         season = NA,
                         week = NA,
                         plays = NA,
                         off_epa_play = NA,
                         off_total_epa = NA,
                         off_success_rate = NA,
                         explosive_play_rate = NA, 
                         bad_play_rate = NA,
                         opponent_ppg = NA, #new
                         def_epa_pp = NA,
                         avg_wpa = NA,
                         series_success = NA,
                         cpoe = NA,
                         avg_yardline = NA,
                         off_interceptions = NA,
                         off_fumbles = NA,
                         home_score = NA,
                         away_score = NA,
                         total_line = NA,
                         spread_line = NA,
                         total = NA,
                         result = NA,
                         home_team = NA,
                         away_team = NA,
                         is_home_team = NA,
                         def_good_play_rate = NA,
                         def_bad_play_rate = NA,
                         def_interceptions = NA,
                         def_fumbles = NA,
                         team = NA)
 
 away_test <- home_test
 
 final_summary <- unique(final_summary) #remove duplicates
 
 #Build home test DF
 for(i in 1:nrow(weekly_matchup)){
     if(week_number < 10){
       week_num <- paste0("0", week_number)
     }else{
       week_num <- week_number
     }
     created_game_id <- paste0("2021_", week_num, "_", weekly_matchup$Away_Team[i], "_", weekly_matchup$Home_Team[i])
     team_index <- which(final_summary$Team == weekly_matchup$Home_Team[i])
     
     home_test <- home_test %>% 
                        add_row(posteam = final_summary$Team[team_index],
                                game_id = created_game_id,
                                plays = final_summary$plays[team_index],
                                off_success_rate = final_summary$off_success_rate[team_index],
                                season = 2021,
                                week = week_number,
                                avg_wpa = final_summary$avg_wpa[team_index],
                                cpoe = final_summary$cpoe[team_index],
                                off_epa_play = final_summary$off_epa_play[team_index],
                                off_total_epa = final_summary$off_total_epa[team_index],
                                explosive_play_rate = final_summary$explosive_play_rate[team_index],
                                bad_play_rate = final_summary$bad_play_rate[team_index],
                                opponent_ppg = final_summary$opponent_ppg[team_index], #new
                                def_epa_pp = final_summary$def_epa_pp[team_index],
                                def_interceptions = final_summary$def_interceptions[team_index],
                                def_fumbles = final_summary$def_fumbles[team_index],
                                def_good_play_rate = final_summary$def_good_play_rate[team_index],
                                def_bad_play_rate = final_summary$def_bad_play_rate[team_index],
                                off_interceptions = final_summary$off_interceptions[team_index],
                                off_fumbles = final_summary$off_fumbles[team_index])
 }
 
 home_test <- home_test[-1, colnames(home_test) != "team"]
 
 # Build away test DF
 for(i in 1:nrow(weekly_matchup)){
   if(week_number < 10){
     week_num <- paste0("0", week_number)
   }else{
     week_num <- week_number
   }
   
   created_game_id <- paste0("2021_", week_num, "_", weekly_matchup$Away_Team[i], "_", weekly_matchup$Home_Team[i])
   team_index <- which(final_summary$Team == weekly_matchup$Away_Team[i])
   
   away_test <- away_test %>% 
                 add_row(posteam = final_summary$Team[team_index],
                         game_id = created_game_id,
                         plays = final_summary$plays[team_index],
                         off_success_rate = final_summary$off_success_rate[team_index],
                         season = 2021,
                         week = week_number,
                         avg_wpa = final_summary$avg_wpa[team_index],
                         cpoe = final_summary$cpoe[team_index],
                         off_epa_play = final_summary$off_epa_play[team_index],
                         off_total_epa = final_summary$off_total_epa[team_index],
                         explosive_play_rate = final_summary$explosive_play_rate[team_index],
                         bad_play_rate = final_summary$bad_play_rate[team_index],
                         opponent_ppg = final_summary$opponent_ppg[team_index], #new
                         def_epa_pp = final_summary$def_epa_pp[team_index],
                         def_interceptions = final_summary$def_interceptions[team_index],
                         def_fumbles = final_summary$def_fumbles[team_index],
                         def_good_play_rate = final_summary$def_good_play_rate[team_index],
                         def_bad_play_rate = final_summary$def_bad_play_rate[team_index],
                         off_interceptions = final_summary$off_interceptions[team_index],
                         off_fumbles = final_summary$off_fumbles[team_index])
 }
 
#remove empty rows of NAs and "team" column
away_test <- away_test[-1, colnames(away_test) != "team"]

#Training and testing the home model using the data from the selected season
home_model <- lm(home_score ~
                   off_epa_play + off_total_epa + off_success_rate +
                   explosive_play_rate + bad_play_rate + avg_wpa +
                   def_interceptions + def_fumbles + cpoe - def_epa_pp +
                   def_good_play_rate - def_bad_play_rate -
                   off_interceptions - off_fumbles - opponent_ppg , #new
                 data = home_train)

home_predictions <- predict(home_model, newdata = home_test)

#Training and testing the away model using the data from the selected season
away_model <- lm(away_score ~
                   off_epa_play + off_total_epa + off_success_rate +
                   explosive_play_rate + bad_play_rate + avg_wpa +
                   def_interceptions + def_fumbles + cpoe - def_epa_pp +
                   def_good_play_rate - def_bad_play_rate -
                   off_interceptions - off_fumbles - opponent_ppg, #new
                 data = away_train)

away_predictions <- predict(away_model, newdata = away_test)

# #Build full models from the entire dataset, then make predictions using the newly created models
home_fit <- lm(home_score ~
                 off_epa_play + off_total_epa + off_success_rate +
                 explosive_play_rate + bad_play_rate + avg_wpa +
                 def_interceptions + def_fumbles + cpoe - def_epa_pp +
                 def_good_play_rate - def_bad_play_rate -
                 off_interceptions - off_fumbles - opponent_ppg, #new
               data = home_results)

home_preds <- predict(home_fit, home_test) %>%
              as_tibble() %>%
              rename(home_prediction = value) %>%
              #mutate(home_prediction = home_prediction - 5) %>%
              round(1) %>%
              cbind(home_test) %>%
              select(posteam, game_id, plays, off_success_rate, season,
                     week, avg_wpa, cpoe, off_epa_play, off_total_epa,
                     explosive_play_rate, bad_play_rate, def_interceptions,
                     def_fumbles, def_good_play_rate, def_bad_play_rate, opponent_ppg, #new
                     def_epa_pp, off_interceptions, off_fumbles, home_prediction)

home_preds <- home_preds %>%
               rename(home_team = posteam)%>%
                left_join(teams_colors_logos, by = c('home_team' = 'team_abbr'))

away_fit <- lm(away_score ~
                 off_epa_play + off_total_epa + off_success_rate +
                 explosive_play_rate + bad_play_rate + avg_wpa +
                 def_interceptions + def_fumbles + cpoe - def_epa_pp +
                 def_good_play_rate - def_bad_play_rate -
                 off_interceptions - off_fumbles - opponent_ppg, #new
               data = away_results)

away_preds <- predict(away_fit, away_test) %>%
              as_tibble() %>%
              rename(away_prediction = value) %>%
              #mutate(away_prediction = away_prediction - 5) %>%
              round(1) %>%
              bind_cols(away_test) %>%
              select(posteam, game_id, plays, off_success_rate, season,
                     week, avg_wpa, cpoe, off_epa_play, off_total_epa,
                     explosive_play_rate, bad_play_rate, def_interceptions,
                     def_fumbles, def_good_play_rate, def_bad_play_rate, opponent_ppg, #new
                     def_epa_pp, off_interceptions, off_fumbles, away_prediction) 

away_preds <- away_preds %>%
                rename(away_team = posteam) %>%
                    left_join(teams_colors_logos, by = c('away_team' = 'team_abbr'))

############### VISUALIZE ######################################

all_predictions <- full_join(home_preds, away_preds, by="game_id")

all_predictions %>%
  select(team_logo_espn.x, home_team, home_prediction, away_prediction, away_team,  team_logo_espn.y) %>%
  gt() %>% # Make a unique table
  tab_options(table.border.top.color = "black",
              row.striping.include_table_body = TRUE) %>% # Set top border color, along with if rows should be stripped or not
  tab_header(title = md(paste0("Week ", week_num, " Predicted Scores"))) %>% # Set title
  tab_source_note(source_note = "SOURCE: nflfastR, Pro Football Reference")  %>% # Set footer
  cols_label(team_logo_espn.x = " ",
              home_team = "HOME TEAM",
              home_prediction = "PREDICTED HOME SCORE",
              away_prediction = "PREDICTED AWAY SCORE",
              away_team = "AWAY TEAM",
              team_logo_espn.y = " ") %>% # Set column names 
  tab_style(style = list(
              cell_fill(color = "mediumseagreen"),
              cell_text(weight = "bold")),
            locations = cells_body(
              columns = home_prediction,
              rows = home_prediction > away_prediction)) %>% # For cells in home_prediction column, highlight in X11 color if the condition is met
  tab_style(style = list(
              cell_fill(color = "mediumseagreen"),
              cell_text(weight = "bold")),
            locations = cells_body(
              columns = away_prediction,
              rows = away_prediction > home_prediction)) %>% # For cells in away_prediction column, highlight in X11 color if the condition is met
  tab_style(style = list(
              cell_fill(color = "lightcoral")),
            locations = cells_body(
              columns = home_prediction,
              rows = home_prediction < away_prediction)) %>% # For cells in home_prediction column, highlight in X11 color if the condition is met
  tab_style(style = list(
              cell_fill(color = "lightcoral")),
            locations = cells_body(
              columns = away_prediction,
              rows = away_prediction < home_prediction)) %>% # For cells in away_prediction column, highlight in X11 color if the condition is met
  text_transform(locations = cells_body(columns = team_logo_espn.x),
                  fn = function(x) {
                    web_image(all_predictions$team_logo_espn.x)
                  }) %>% #Insert logo image into team_logo_espn.x column
  text_transform(locations = cells_body(columns = team_logo_espn.y),
                  fn = function(x) {
                    web_image(all_predictions$team_logo_espn.y)
                  }) #Insert logo image into team_logo_espn.y column

#create a DF to filter the data down to the games that have been completed this week so far 
season <- "2021"
games_for_correct_week <- readRDS(url(paste0("https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_",season,".rds"))) %>%
                          filter(week == week_number) %>%
                          group_by(game_id) %>%
                          summarise(home_team = home_team,
                                    away_team = away_team,
                                    home_score = home_score,
                                    away_score = away_score,
                                    total_line = total_line,
                                    spread_line = spread_line,
                                    total = total,
                                    result = result
                          ) %>%
                          unique()

#find the row of the data for the completed games




####FIX BELOW TO GET PREVIOUS WEEK RECORDS!!!!!

!!!
  !!!
  
  
  !
  
  !!!!!!!^^^^

#Use models to see how accurate point prediction would have been for each game
compare_df <- full_join(all_predictions, games_for_correct_week, by = "game_id") 
compare_df <- compare_df[!is.na(compare_df$home_team.x),]
#initialize columns
compare_df$home_score <- NA
compare_df$away_score <- NA
compare_df$total_line <- NA
compare_df$spread_line <- NA
compare_df$total <- NA
compare_df$result <- NA

for(i in 1:length(completed_games)){
  correct_row <- completed_games[i] #find out the index of the completed game, then populate compare_df with the correct information

  compare_df$total_line[correct_row] <- games_for_correct_week$total_line[i]
  compare_df$spread_line[correct_row] <- games_for_correct_week$spread_line[i]
  compare_df$total[correct_row] <- games_for_correct_week$total[i]
  compare_df$result[correct_row] <- games_for_correct_week$result[i]
  compare_df$home_score[correct_row] <- games_for_correct_week$home_score[i]
  compare_df$away_score[correct_row] <- games_for_correct_week$away_score[i]
}

#Do some transformations to easily compare the predictions to reality

compare_df$predicted_winner <- ifelse(compare_df$home_prediction > compare_df$away_prediction,
                                      compare_df$home_team,
                                      compare_df$away_team)

compare_df$winner <- ifelse(compare_df$result > 0,
                            compare_df$home_team,
                            compare_df$away_team)

compare_df$correct_prediction <- ifelse(compare_df$predicted_winner == compare_df$winner,
                                        TRUE,
                                        FALSE)

compare_df$predicted_total <- compare_df$home_prediction + compare_df$away_prediction

compare_df$predicted_over <- ifelse(compare_df$predicted_total > compare_df$total_line,
                                    TRUE,
                                    FALSE)

compare_df$predicted_under <- ifelse(compare_df$predicted_total < compare_df$total_line,
                                    TRUE,
                                    FALSE)

compare_df$over <- compare_df$total > compare_df$total_line

compare_df$under <- compare_df$total < compare_df$total_line

compare_df$correct_over_under <- ifelse(compare_df$over == compare_df$predicted_over,
                                        TRUE,
                                        ifelse(compare_df$under == compare_df$predicted_under,
                                               TRUE,
                                               FALSE))

compare_df$predicted_spread <- compare_df$home_prediction - compare_df$away_prediction

table(compare_df$correct_prediction)
table(compare_df$correct_over_under)


write.csv(compare_df, paste(season, " ", week_num, "_Prediction_vs_Reality.csv", sep=""), row.names = TRUE)

#Next to do - calculate if correct decision was made for spread

# 
# graph_data <- full_defense %>%
#   group_by(defteam) %>%
#   summarise(defteam = defteam,
#             def_epa_pp = mean(def_epa_pp)) %>%
#   unique() %>% 
#   arrange(.$def_epa_pp)
# 
# 
# 
# 
# ggplot(data = graph_data, mapping = aes(x = reorder(defteam, def_epa_pp, sum), y = def_epa_pp)) +
#   geom_col()


