library(tidyverse)
#NOTE: if you have problems with ggmap, try to install both ggplot and ggmap from github
#devtools::install_github("hadley/ggplot2")
library(ggmap)
library(ggplot2)
library(dplyr)
library(rjson)
library(jsonlite)
library(RCurl)
library(nflscrapR)

reg_2019 <- scrape_game_ids(2019, type = "reg")

API_key <- "31e124657432b638fe91137822e9b373" #the-odds-api key, 500 requests per month
url <- "https://api.the-odds-api.com/v3/odds/?apiKey=31e124657432b638fe91137822e9b373&sport=americanfootball_nfl&region=us&mkt=spreads"
#url <- paste(url, API_key)

full_url <- URLencode(url)
nfl_odds_df <- as.data.frame(fromJSON(getURL(full_url)))

Conf_Champ_Week <- data.frame(game_id = numeric(),
                              home_team = character(),
                              away_team = character(),
                              spread = list())

colnames(nfl_odds_df)

nfl_odds_df$data.teams


Conf_Champ_Week$Teams <- nfl_odds_df$data.teams
#Conf_Champ_Week$Spread <-


class(nfl_odds_df$data.teams[[1]])

for(i in 1:nrow(nfl_odds_df)){
  teams <- nfl_odds_df$data.teams[i]
  teams[[i]]
  Conf_Champ_Week[i, 1] <- teams[i]
  #Conf_Champ_Week$Spread <-
}

#Get day, month, and year columns for the date
for (i in 1:nrow(NFL_data)){
  str <- NFL_data$game_id[i]
  NFL_data$year[i] <- substr(str, 3, 4)
  NFL_data$month[i] <- substr(str, 5, 6)
  NFL_data$day[i] <- substr(str, 7, 8)
}



twoK9 <- c("DET", "STL", "KC", "SEA", "NYJ", "CIN", "OAK", "JAX", "GB", "SF")
twoK10 <- c("STL", "DET", "TB", "WAS", "KC", "SEA", "CLE", "OAK", "BUF", "JAX")
twoK11 <- c("CAR", "DEN", "BUF", "CIN", "ARI", "ATL", "SF", "TEN", "DAL", "JAX")
twoK12 <- c("IND", "WAS", "CLE", "MIN", "JAX", "DAL", "TB", "MIA", "CAR", "BUF")
twoK13 <- c("KC", "JAX", "MIA", "PHI", "DET", "CLE", "ARI", "STL", "NYJ", "TEN")
twoK14 <- c("HOU", "STL", "JAX", "BUF", "OAK", "ATL", "TB", "CLE", "MIN", "DET")
twoK15 <- c("TB", "TEN", "JAX", "OAK", "WAS", "NYJ", "CHI", "ATL", "NYG", "STL")
twoK16 <- c("STL", "PHI", "SD", "DAL", "JAX", "BAL", "SF", "TEN", "CHI", "NYG")
twoK17 <- c("CLE", "CHI", "SF", "JAX", "TEN", "NYJ", "SD", "CAR", "CIN", "KC")
twoK18 <- c("CLE", "NYG", "NYJ", "CLE", "DEN", "IND", "BUF", "CHI", "SF", "ARI")
twoK19 <- c("ARI", "SF", "NYJ", "OAK", "TB", "NYG", "JAX", "DET", "BUF", "PIT")

twok9through19 <- c(twoK9, twoK10, twoK11, twoK12, twoK13, twoK14, twoK15, twoK16, twoK17, twoK18, twoK19)
twok9through19 <- sort(twok9through19)

table(twok9through19)

df <- data.frame(first = as.character(),
                 second = as.character(),
                 third = as.character(),
                 fourth = as.character(),
                 fifth = as.character(),
                 sixth = as.character(),
                 seventh = as.character(),
                 eigth = as.character(),
                 nineth = as.character(),
                 tenth = as.character(), stringsAsFactors = FALSE)
?rbind
df <- rbind(df, twoK9)
df <- rbind(df, twoK10)



?append()

