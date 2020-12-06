library(tidyverse)
library(devtools)
library(ggrepel)
library(ggimage)
library(nflfastR)
library(readr)
library(XML)

#Avoid scientific notation for numbers
options(scipen = 9999)

#Obtain 2019 season (regular + post)
data2K20 <- readRDS(url('https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_2020.rds'))

Generate_Weekly_EPA_Model_2020 <- function(week_num){
  
  #Only look at non special-teams plays, with a valid EPA
  no_sp_full_epa <- data2K20 %>%
                      filter(!is.na(epa), special == 0)
  
  chart_1_data <- no_sp_full_epa %>%
                    filter(wp > 0.05 & wp < 0.95, 
                           down == 1 |
                             down == 2 |
                             down == 3 |
                             down == 4)
  #Get offensive EPA per team and defensive EPA per team
  offensive_EPA_per_team <- chart_1_data %>%
                            group_by(posteam, season) %>% 
                            summarize(epa = mean(epa)) 
  defensive_EPA_per_team <- chart_1_data %>%
                            group_by(defteam, season) %>% 
                            summarize(epa = mean(epa)) 
  
  #Cleaning so joining is easier
  offensive_EPA_per_team$off_epa <- offensive_EPA_per_team$epa
  offensive_EPA_per_team$team <- offensive_EPA_per_team$posteam
  
  offensive_EPA_per_team <- offensive_EPA_per_team %>% 
                            select(team, off_epa)
  
  defensive_EPA_per_team$def_epa <- defensive_EPA_per_team$epa
  defensive_EPA_per_team$team <- defensive_EPA_per_team$defteam
  
  defensive_EPA_per_team <- defensive_EPA_per_team %>% 
                            select(team, def_epa)
  
  
  final_chart_1_data <- full_join(offensive_EPA_per_team, defensive_EPA_per_team, by = "team") %>% 
                          select(team, off_epa, def_epa)
  
  #Merge team data with image data
  final_chart_1_data <- final_chart_1_data %>%
                        left_join(teams_colors_logos, by = c('team' = 'team_abbr'))
  
  defensive_EPA_per_team <- defensive_EPA_per_team %>%
                            left_join(teams_colors_logos, by = c('team' = 'team_abbr'))
  
  offensive_EPA_per_team <- offensive_EPA_per_team %>%
                            left_join(teams_colors_logos, by = c('team' = 'team_abbr'))
  
  #Copy over final_chart_1_data from previous project
  week_testrun <- final_chart_1_data
  
  #Create clean, create a total_epa variable and create a rank
  week_testrun$total_epa <- week_testrun$off_epa - week_testrun$def_epa
  week_testrun <- week_testrun %>% select(team, total_epa) %>% arrange(-total_epa)
  week_testrun$rank[order(-week_testrun$total_epa)] <- 1:nrow(week_testrun)
  
  #Set for week 12
  week_hometeams <- c("NO", "DET", "CLE", "CIN", "JAX", "LV", "IND", "LA", "NYG", "PHI", "NE", "DEN", "WAS", "BUF", "DAL")
  week_awayteams <- c("ATL", "CHI", "TEN", "MIA", "MIN", "NYJ", "HOU", "ARI", "SEA", "GB", "LAC", "KC", "PIT", "SF", "BAL")
  
  week_2020 <- data.frame(week = week_num, season = 2020, Home_Team = week_hometeams, Away_Team = week_awayteams)
  
  #Add EPA Ranks for each match up
  ht <- as.character(week_2020[1,"Home_Team"])
  week_testrun[week_testrun$team == ht, "rank"]
  
  #Add in EPA ranks for home and away teams for the week matchup
  for (i in 1:nrow(week_2020)) {
    ht <- as.character(week_2020[i,"Home_Team"])
    at <- as.character(week_2020[i,"Away_Team"])
    
    week_2020$Home_EPA_rank[i] <- week_testrun[week_testrun$team == ht, "rank"]
    week_2020$Away_EPA_rank[i] <- week_testrun[week_testrun$team == at, "rank"]
  }
  
  week_2020$Home_EPA_rank <- unlist(week_2020$Home_EPA_rank)
  week_2020$Away_EPA_rank <- unlist(week_2020$Away_EPA_rank)
  
  week_2020$Proj_Winner <- ifelse(week_2020$Home_EPA_rank < week_2020$Away_EPA_rank, 
                                    as.character(week_2020$Home_Team), 
                                    as.character(week_2020$Away_Team))
  week_2020$rank_diff <- abs(week_2020$Home_EPA_rank - week_2020$Away_EPA_rank)
  
  write.csv(week_2020, paste(week_num, "_EPA_Model_Bets.csv", sep=""), row.names = TRUE)
}


Generate_Weekly_EPA_Model_2020(13)






