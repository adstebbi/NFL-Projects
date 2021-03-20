library(tidyverse)
library(devtools)
library(ggrepel)
library(ggimage)
library(nflfastR)
library(readr)
library(XML)

#HELPER FUNCTION
scrape_weekly_games <- function(){
  #Pull in weekly matchup from Vegas Insider website
  simple <- read_html('http://www.vegasinsider.com/nfl/odds/las-vegas/')
  
  #HTML container that holds team names is <a> </a>
  simple_a <- simple %>%
    html_nodes("a") %>%
    html_text()
  
  NFL_Teams <- c("Miami", "Baltimore", "Pittsburgh", "Minnesota", "N.Y. Jets", "Dallas", "Atlanta", 
                 "Green Bay", "Las Vegas", "Jacksonville", "L.A. Chargers", "Arizona", "Seattle", 
                 "New Orleans", "Tennessee", "Washington", "Buffalo", "Cincinnati", "Cleveland", 
                 "Detroit", "New England", "N.Y. Giants", "Tampa Bay", "Chicago", "Denver", 
                 "Indianapolis", "Kansas City", "L.A. Rams", "San Francisco", "Carolina", "Houston", 
                 "Philadelphia")
  
  
  #Get list of matchups by matching the characters that align between NFL_Teams and simple_a
  all_teams_in_order <- c()
  j <- 1
  for(i in 1:length(simple_a)){
    if(simple_a[[i]] %in% NFL_Teams){
      all_teams_in_order[[j]] <- simple_a[[i]]
      j <- j + 1
    }
  }
  
  all_teams_abbrv <- c()
  
  #Change from names to abbreviations for ease of use
  for (i in 1:length(all_teams_in_order)){
    if (all_teams_in_order[[i]] == "Arizona") {
      all_teams_abbrv[[i]] <- "ARI"
    } 
    if (all_teams_in_order[[i]] == "Atlanta") {
      all_teams_abbrv[[i]] <- "ATL"
    } 
    if (all_teams_in_order[[i]] == "Baltimore") {
      all_teams_abbrv[[i]] <- "BAL"
    } 
    if (all_teams_in_order[[i]] == "Buffalo") {
      all_teams_abbrv[[i]] <- "BUF"
    }
    if (all_teams_in_order[[i]] == "Carolina") {
      all_teams_abbrv[[i]] <- "CAR"
    }
    if (all_teams_in_order[[i]] == "Chicago") {
      all_teams_abbrv[[i]] <- "CHI"
    }
    if (all_teams_in_order[[i]] == "Cincinnati") {
      all_teams_abbrv[[i]] <- "CIN"
    }
    if (all_teams_in_order[[i]] == "Cleveland") {
      all_teams_abbrv[[i]] <- "CLE"
    }
    if (all_teams_in_order[[i]] == "Dallas") {
      all_teams_abbrv[[i]] <- "DAL"
    }
    if (all_teams_in_order[[i]] == "Denver") {
      all_teams_abbrv[[i]] <- "DEN"
    }
    if (all_teams_in_order[[i]] == "Detroit") {
      all_teams_abbrv[[i]] <- "DET"
    }
    if (all_teams_in_order[[i]] == "Green Bay") {
      all_teams_abbrv[[i]] <- "GB"
    }
    if (all_teams_in_order[[i]] == "Houston") {
      all_teams_abbrv[[i]] <- "HOU"
    }
    if (all_teams_in_order[[i]] == "Indianapolis") {
      all_teams_abbrv[[i]] <- "IND"
    }
    if (all_teams_in_order[[i]] == "Jacksonville") {
      all_teams_abbrv[[i]] <- "JAX"
    }
    if (all_teams_in_order[[i]] == "Kansas City") {
      all_teams_abbrv[[i]] <- "KC"
    }
    if (all_teams_in_order[[i]] == "Miami") {
      all_teams_abbrv[[i]] <- "MIA"
    }
    if (all_teams_in_order[[i]] == "Minnesota") {
      all_teams_abbrv[[i]] <- "MIN"
    }
    if (all_teams_in_order[[i]] == "New England") {
      all_teams_abbrv[[i]] <- "NE"
    }
    if (all_teams_in_order[[i]] == "New Orleans") {
      all_teams_abbrv[[i]] <- "NO"
    }
    if (all_teams_in_order[[i]] == "N.Y. Giants") {
      all_teams_abbrv[[i]] <- "NYG"
    }
    if (all_teams_in_order[[i]] == "N.Y. Jets") {
      all_teams_abbrv[[i]] <- "NYJ"
    }
    if (all_teams_in_order[[i]] == "Las Vegas") {
      all_teams_abbrv[[i]] <- "LV"
    }
    if (all_teams_in_order[[i]] == "Philadelphia") {
      all_teams_abbrv[[i]] <- "PHI"
    }
    if (all_teams_in_order[[i]] == "Pittsburgh") {
      all_teams_abbrv[[i]] <- "PIT"
    }
    if (all_teams_in_order[[i]] == "L.A. Chargers") {
      all_teams_abbrv[[i]] <- "LAC"
    }
    if (all_teams_in_order[[i]] == "San Francisco") {
      all_teams_abbrv[[i]] <- "SF"
    }
    if (all_teams_in_order[[i]] == "Seattle") {
      all_teams_abbrv[[i]] <- "SEA"
    }
    if (all_teams_in_order[[i]] == "L.A. Rams") {
      all_teams_abbrv[[i]] <- "LA"
    }
    if (all_teams_in_order[[i]] == "Tampa Bay") {
      all_teams_abbrv[[i]] <- "TB"
    }
    if (all_teams_in_order[[i]] == "Tennessee") {
      all_teams_abbrv[[i]] <- "TEN"
    }
    if (all_teams_in_order[[i]] == "Washington") {
      all_teams_abbrv[[i]] <- "WAS"
    }
  }
  print(all_teams_abbrv)
  
  return(all_teams_abbrv)
}

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
  
  #Obtain this weeks matchups Home & Away
  teams <- scrape_weekly_games()
  print(teams)
  odd_teams <- seq(1,length(teams),by=2)
  even_teams <- seq(2,length(teams), by=2)
  week_hometeams <- teams[even_teams]
  week_awayteams <- teams[odd_teams]
  
  week_2020 <- data.frame(week = week_num, 
                          season = 2020, 
                          Home_Team = week_hometeams, 
                          Away_Team = week_awayteams)
  
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

#Actually Run the Program, just enter week number and the file will be generated for you
Generate_Weekly_EPA_Model_2020(21)






