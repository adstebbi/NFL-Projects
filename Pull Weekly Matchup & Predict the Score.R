library(tidyverse)
library(devtools)
library(ggrepel)
library(ggimage)
library(nflfastR)
library(readr)
library(XML)
library(ggplot2)
library(Metrics)
library(textreadr)
library(rvest)
library(gt)

## Current State Description (10/8/2021):
# Games for given week are properly scraped from team rankings website and placed into "weekly_matchup" DF
# Data for entire given season is generated, cleaned, and organized into required home & away DFs with proper stats
# Models are generated for home and away teams in upcoming matchups, and their scores are predicted using the available data
# up to this point. gt() is used to create a table to display this information in a consumable fashion

scrape_weekly_games <- function(week_num){
  
  week_num <- 2
  correct_url <- paste0("https://www.teamrankings.com/nfl-odds-week-", week_num)
  
  #Pull in weekly matchup from Team Rankings website
  simple <- read_html(correct_url)
  
  #HTML container that holds team names is <a> </a>
  simple_a <- simple %>%
    html_nodes("a") %>%
    html_text()
  
  NFL_Teams <- c("Miami", "Baltimore", "Pittsburgh", "Minnesota", "NY Jets", "Dallas", "Atlanta", 
                 "Green Bay", "Las Vegas", "Jacksonville", "LA Chargers", "Arizona", "Seattle", 
                 "New Orleans", "Tennessee", "Washington", "Buffalo", "Cincinnati", "Cleveland", 
                 "Detroit", "New England", "NY Giants", "Tampa Bay", "Chicago", "Denver", 
                 "Indianapolis", "Kansas City", "LA Rams", "San Francisco", "Carolina", "Houston", 
                 "Philadelphia")
  
  
  #Get list of matchups by matching the characters that align between NFL_Teams and simple_a
  all_teams_in_order <- c()
  j <- 1
  length_simple_a <- length(simple_a)
  
  for(i in 1:length_simple_a) {
    if(grepl("@", simple_a[[i]], fixed = TRUE)){ #pull out the strings that contain the matchups 
      
      teams <- str_split(simple_a[[i]], " @ ")[[1]] #Find all matchups from the html
      
      if(teams[1] %in% NFL_Teams){
        all_teams_in_order[[j]] <- teams[[1]] #Separate the teams within the strings, and keep them in order
        j <- j + 1
        all_teams_in_order[[j]] <- teams[[2]]
        j <- j + 1
      }
    }
    else if(grepl("vs.", simple_a[[i]], fixed = TRUE)){
      teams <- str_split(simple_a[[i]], " vs. ")[[1]]
      
      if(teams[1] %in% NFL_Teams){
        all_teams_in_order[[j]] <- teams[[1]] #Separate the teams within the strings, and keep them in order
        j <- j + 1
        all_teams_in_order[[j]] <- teams[[2]]
        j <- j + 1
      }
    }
  }
  
  ##CURRENT ISSUE, not capturing NY Jets vs. Atlanta because it uses vs. instead of @ symbol due to a neutral field.
  ################################ FIX ME ########################################################
  
  all_teams_in_order <- unique(all_teams_in_order) #Remove duplicates
  all_teams_in_order <- all_teams_in_order[all_teams_in_order %in% NFL_Teams] #Make sure only NFL teams were collected
  
  all_teams_abbrv <- c()
  
  #Change from names to abbreviations for ease of use
  
    for (i in 1:length(all_teams_in_order)){
      all_teams_abbrv[[i]] <- case_when(
                                all_teams_in_order[[i]] == "Arizona" ~ "ARI",
                                all_teams_in_order[[i]] == "Atlanta" ~ "ATL",
                                all_teams_in_order[[i]] == "Baltimore" ~ "BAL",
                                all_teams_in_order[[i]] == "Buffalo" ~ "BUF",
                                all_teams_in_order[[i]] == "Carolina" ~ "CAR",
                                all_teams_in_order[[i]] == "Chicago" ~ "CHI",
                                all_teams_in_order[[i]] == "Cincinnati" ~ "CIN",
                                all_teams_in_order[[i]] == "Cleveland" ~ "CLE",
                                all_teams_in_order[[i]] == "Dallas" ~ "DAL",
                                all_teams_in_order[[i]] == "Denver" ~ "DEN",
                                all_teams_in_order[[i]] == "Detroit" ~ "DET",
                                all_teams_in_order[[i]] == "Green Bay" ~ "GB",
                                all_teams_in_order[[i]] == "Houston" ~ "HOU",
                                all_teams_in_order[[i]] == "Indianapolis" ~ "IND",
                                all_teams_in_order[[i]] == "Jacksonville" ~ "JAX",
                                all_teams_in_order[[i]] == "Kansas City" ~ "KC",
                                all_teams_in_order[[i]] == "Miami" ~ "MIA",
                                all_teams_in_order[[i]] == "Minnesota" ~ "MIN",
                                all_teams_in_order[[i]] == "New England" ~ "NE",
                                all_teams_in_order[[i]] == "New Orleans" ~ "NO",
                                all_teams_in_order[[i]] == "NY Giants" ~ "NYG",
                                all_teams_in_order[[i]] == "NY Jets" ~ "NYJ",
                                all_teams_in_order[[i]] == "Las Vegas" ~ "LV",
                                all_teams_in_order[[i]] == "Philadelphia" ~ "PHI",
                                all_teams_in_order[[i]] == "Pittsburgh" ~ "PIT",
                                all_teams_in_order[[i]] == "LA Chargers" ~ "LAC",
                                all_teams_in_order[[i]] == "San Francisco" ~ "SF",
                                all_teams_in_order[[i]] == "Seattle" ~ "SEA",
                                all_teams_in_order[[i]] == "LA Rams" ~ "LA",
                                all_teams_in_order[[i]] == "Tampa Bay" ~ "TB",
                                all_teams_in_order[[i]] == "Tennessee" ~ "TEN",
                                all_teams_in_order[[i]] == "Washington" ~ "WAS"
                          ) 
      }

  return(all_teams_abbrv)
}

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
                  group_by(game_id, defteam, season, week) %>% 
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

## End of function

#Set the season here
season <- "2021"
All_season <- generate_data_for_modeling(season)

#Do not attempt to set variables for inaccurately generated datasets
if(length(All_season) == 2) {
  home_results <- All_season[[1]]
  away_results <- All_season[[2]]
} else {
  print("CALCULATION ERROR")
}

full_stats <- full_join(away_results, home_results) 

#SET DESIRED WEEK NUMBER HERE
week_number <- 5
teams <- scrape_weekly_games(week_number)

odds <- seq(1, length(teams), 2)
evens <- seq(2, length(teams), 2)
away_teams <- teams[odds]
home_teams <- teams[evens]

#Get this week's games and teams
weekly_matchup <- tibble(Home_Team = home_teams,
                             Away_Team = away_teams)

#Create DF to use to calculate averages for each team that will generate 
#the numbers used in the testing data
all_results <- full_join(home_results, away_results)

final_summary <- all_results %>%
                  group_by(posteam) %>%
                    na.omit %>%
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
                                avg_yardline = mean(avg_yardline),
                                def_interceptions = sum(def_interceptions),
                                def_fumbles = sum(def_fumbles),
                                def_good_play_rate = mean(def_good_play_rate),
                                def_bad_play_rate = mean(bad_play_rate),
                                off_interceptions = sum(off_interceptions),
                                off_fumbles = sum(off_fumbles))

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
     created_game_id <- paste0("2021_01_", weekly_matchup$Away_Team[i], "_", weekly_matchup$Home_Team[i])
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
                                def_interceptions = final_summary$def_interceptions[team_index],
                                def_fumbles = final_summary$def_fumbles[team_index],
                                def_good_play_rate = final_summary$def_good_play_rate[team_index],
                                def_bad_play_rate = final_summary$def_bad_play_rate[team_index],
                                off_interceptions = final_summary$off_interceptions[team_index],
                                off_fumbles = final_summary$off_fumbles[team_index])
 }
 
 home_test <- home_test[-1, -30]

 # c("team", "off_epa_play", "off_total_epa", "explosive_play_rate",
 #   "bad_play_rate", "def_interceptions", "def_fumbles", "def_good_play_rate",
 #   "def_bad_play_rate", "off_interceptions", "off_fumbles", "home_score")
 
 # Build away test DF
 for(i in 1:nrow(weekly_matchup)){
   created_game_id <- paste0("2021_01_", weekly_matchup$Away_Team[i], "_", weekly_matchup$Home_Team[i])
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
                         def_interceptions = final_summary$def_interceptions[team_index],
                         def_fumbles = final_summary$def_fumbles[team_index],
                         def_good_play_rate = final_summary$def_good_play_rate[team_index],
                         def_bad_play_rate = final_summary$def_bad_play_rate[team_index],
                         off_interceptions = final_summary$off_interceptions[team_index],
                         off_fumbles = final_summary$off_fumbles[team_index])
 }
 
 away_test <- away_test[-1, -30] #remove empty rows of NAs and "team" column

#Training and testing the home model using the data from the selected season
home_model <- lm(home_score ~
                   off_epa_play + off_total_epa + off_success_rate +
                   explosive_play_rate + bad_play_rate + avg_wpa +
                   def_interceptions + def_fumbles + cpoe +
                   def_good_play_rate - def_bad_play_rate -
                   off_interceptions - off_fumbles,
                 data = home_train)

home_predictions <- predict(home_model, newdata = home_test)
#home_rmse <- rmse(home_test$home_score, home_predictions)
#home_R_squared <- cor(home_test$home_score, home_predictions)^2

#Training and testing the away model using the data from the selected season
away_model <- lm(away_score ~
                   off_epa_play + off_total_epa + off_success_rate +
                   explosive_play_rate + bad_play_rate + avg_wpa +
                   def_interceptions + def_fumbles + cpoe +
                   def_good_play_rate - def_bad_play_rate -
                   off_interceptions - off_fumbles,
                 data = away_train)

away_predictions <- predict(away_model, newdata = away_test)
# away_rmse <- rmse(away_test$away_score, away_predictions)
# away_R_squared <- cor(away_test$away_score, away_predictions)^2
# 
# #Build full models from the entire dataset, then make predictions using the newly created models
home_fit <- lm(home_score ~
                 off_epa_play + off_total_epa + off_success_rate +
                 explosive_play_rate + bad_play_rate + avg_wpa +
                 def_interceptions + def_fumbles + cpoe +
                 def_good_play_rate - def_bad_play_rate -
                 off_interceptions - off_fumbles,
               data = home_results)

home_preds <- predict(home_fit, home_test) %>%
              as_tibble() %>%
              rename(home_prediction = value) %>%
              round(1) %>%
              cbind(home_test) %>%
              select(posteam, game_id, plays, off_success_rate, season,
                     week, avg_wpa, cpoe, off_epa_play, off_total_epa,
                     explosive_play_rate, bad_play_rate, def_interceptions,
                     def_fumbles, def_good_play_rate, def_bad_play_rate,
                     off_interceptions, off_fumbles, home_prediction) 

home_preds <- home_preds %>%
               rename(home_team = posteam)%>%
                left_join(teams_colors_logos, by = c('home_team' = 'team_abbr'))

away_fit <- lm(away_score ~
                 off_epa_play + off_total_epa + off_success_rate +
                 explosive_play_rate + bad_play_rate + avg_wpa +
                 def_interceptions + def_fumbles + cpoe +
                 def_good_play_rate - def_bad_play_rate -
                 off_interceptions - off_fumbles,
               data = away_results)

away_preds <- predict(away_fit, away_test) %>%
              as_tibble() %>%
              rename(away_prediction = value) %>%
              round(1) %>%
              bind_cols(away_test) %>%
              select(posteam, game_id, plays, off_success_rate, season,
                     week, avg_wpa, cpoe, off_epa_play, off_total_epa,
                     explosive_play_rate, bad_play_rate, def_interceptions,
                     def_fumbles, def_good_play_rate, def_bad_play_rate,
                     off_interceptions, off_fumbles, away_prediction) 

away_preds <- away_preds %>%
                rename(away_team = posteam) %>%
                    left_join(teams_colors_logos, by = c('away_team' = 'team_abbr'))

############### VISUALIZE ######################################

all_predictions <- merge(home_preds,away_preds,by="game_id")


all_predictions %>%
  select(team_logo_espn.x, home_team, home_prediction, away_prediction, away_team,  team_logo_espn.y) %>%
  gt() %>% # Make a unique table
  tab_options(table.border.top.color = "white",
              row.striping.include_table_body = TRUE) %>% # Set top border color, along with if rows should be stripped or not
  tab_header(title = md("Week 5 Predicted Scores (:")) %>% # Set title
  tab_source_note(source_note = "SOURCE: nflfastR")  %>% # Set footer
  cols_label(team_logo_espn.x = "HOME LOGO",
              home_team = "HOME TEAM",
              home_prediction = "PREDICTED HOME SCORE",
              away_prediction = "PREDICTED AWAY SCORE",
              away_team = "AWAY TEAM",
              team_logo_espn.y = "AWAY LOGO") %>% # Set column names 
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

# #Average number of points the prediction is off by
# mean(abs(home_preds$prediction_minus_actual))
# mean(abs(away_preds$prediction_minus_actual))
#
# #Use models to see how accurate point prediction would have been for each game
# compare_df <- data.frame(game_id = home_preds$game_id,
#                          home_team = home_preds$home_team,
#                          home_prediction = home_preds$home_prediction,
#                          away_team = away_preds$away_team,
#                          away_prediction = away_preds$away_prediction,
#                          vegas_total = home_preds$total_line,
#                          vegas_spread = home_preds$spread_line,
#                          result = home_preds$result,
#                          game_total = home_preds$total)
#
# compare_df$predicted_total <- compare_df$home_prediction + compare_df$away_prediction
# compare_df$predicted_spread <- compare_df$home_prediction - compare_df$away_prediction
#
# #Create column because graphing with game_id is too many unique values for an x-axis to generate
# compare_df$numeric_game_ID <- seq.int(nrow(compare_df))
#
# #Columns created in order to calculate if the home team covered the spread
# compare_df$positive_spread <- ifelse(compare_df$vegas_spread > 0, TRUE, FALSE)
# compare_df$home_result_greater_than_spread <- ifelse(compare_df$result > compare_df$vegas_spread, TRUE, FALSE)
# compare_df$correct_spread <- ifelse(compare_df$predicted_spread > compare_df$vegas_spread, TRUE, FALSE)
#
# #Plot predicted spread v actual spread v vegas
# ggplot(data = compare_df, mapping = aes(x = numeric_game_ID)) +
#   geom_line(aes(y=result, color="Result"), linetype="solid", size=0.5) +
#   geom_line(aes(y=vegas_spread, color="Vegas"), linetype="solid", size=0.5) +
#   geom_line(aes(y=predicted_spread, color="Predicted"), linetype="solid", size=0.5) +
#   scale_color_manual(values = c(
#     'Result' = 'darkblue',
#     'Predicted' = 'darkorange',
#     'Vegas' = 'darkred'))+
#   labs(x = "Game Number", y = "Spread", color = "Spreads") +
#   ggtitle("Real Spread vs Vegas Spread vs Predicted Spread")
#
# #Calculate how many spreads were correctly predicted using this model
# compare_df %>% summarize(n = n(), true = sum(compare_df$correct_spread == TRUE), accuracy = true / n)
#
# #Calculate how many overs were correctly picked
# compare_df$real_over_hit <- ifelse(compare_df$game_total > compare_df$vegas_total, TRUE, FALSE)
# compare_df$predict_over_hit <- ifelse(compare_df$predicted_total > compare_df$vegas_total, TRUE, FALSE)
# compare_df$correct_over_under <- ifelse(compare_df$real_over_hit == compare_df$predict_over_hit, TRUE, FALSE)
#
# #Plot predicted total v actual total v vegas
# ggplot(data = compare_df, mapping = aes(x = numeric_game_ID)) +
#   geom_line(aes(y=game_total, color="Result"), linetype="solid", size=0.5) +
#   geom_line(aes(y=vegas_total, color="Vegas"), linetype="solid", size=0.5) +
#   geom_line(aes(y=predicted_total, color="Predicted"), linetype="solid", size=0.5) +
#   scale_color_manual(values = c(
#     'Result' = 'darkblue',
#     'Predicted' = 'darkorange',
#     'Vegas' = 'darkred'))+
#   labs(x = "Game Number", y = "Total", color = "Total") +
#   ggtitle("Real Total vs Vegas Total vs Predicted Total")
#
# #Calculate how many over/unders were correctly predicted using this model
# compare_df %>% summarize(n = n(), true = sum(compare_df$correct_over_under == TRUE), accuracy = true / n)
#
#
