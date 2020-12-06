library(tidyverse)
library(devtools)
library(ggrepel)
library(ggimage)
library(nflfastR)
library(readr)
library(XML)

#Avoid scientific notation for numbers
options(scipen = 9999)

#Copy over final_chart_1_data from previous project
week11_testrun <- final_chart_1_data

#Create clean, create a total_epa variable and create a rank
week11_testrun$total_epa <- week11_testrun$off_epa - week11_testrun$def_epa
week11_testrun <- week10_testrun %>% select(team, total_epa) %>% arrange(-total_epa)
week11_testrun$rank[order(-week11_testrun$total_epa)] <- 1:nrow(week11_testrun)

week11_hometeams <- c("SEA", "CLE", "NO", "WAS", "CAR", "JAX", "BAL", "HOU", "DEN", "LAC", "IND", "MIN", "LV", "TB")
week11_awayteams <- c("ARI", "PHI", "ATL", "CIN", "DET", "PIT", "TEN", "NE", "MIA", "NYJ", "GB", "DAL", "KC", "LA")

week11_2020 <- data.frame(week = 10, season = 2020, Home_Team = week11_hometeams, Away_Team = week11_awayteams)

#Add in EPA ranks for home and away teams for the week11 matchup
for (i in 1:nrow(week11_2020)) {
  ht <- as.character(week11_2020[i,"Home_Team"])
  at <- as.character(week11_2020[i,"Away_Team"])
  
  week11_2020$Home_EPA_rank[i] <- week11_testrun[week11_testrun$team == ht, "rank"]
  week11_2020$Away_EPA_rank[i] <- week11_testrun[week11_testrun$team == at, "rank"]
}
week11_2020$Away_EPA_rank
week11_2020$Home_EPA_rank <- unlist(week11_2020$Home_EPA_rank)
week11_2020$Away_EPA_rank <- unlist(week11_2020$Away_EPA_rank)

week11_2020$Proj_Winner <- ifelse(week11_2020$Home_EPA_rank < week11_2020$Away_EPA_rank, 
                                  as.character(week11_2020$Home_Team), 
                                  as.character(week11_2020$Away_Team))

write.csv(week11_2020,"EPA_Model_Bets_11.csv", row.names = TRUE)

