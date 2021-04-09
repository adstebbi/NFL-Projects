library("rvest")
library("tidyverse")
library("rlist")

scrape_weekly_games <- function(){
  #Pull in weekly matchup from Team Rankings website, set the week number HERE
  simple <- read_html('https://www.teamrankings.com/nfl-odds-week-10')
  
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
  
  return(all_teams_abbrv)
}

teams <- scrape_weekly_games()

odd_teams <- seq(1,32,by=2)
even_teams <- seq(2,32, by=2)
Home_Teams <- teams[even_teams]
Away_Teams <- teams[odd_teams]










