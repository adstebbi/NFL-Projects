library(tidyverse)
library(devtools)
library(ggrepel)
library(ggimage)
library(nflfastR)
library(readr)
library(XML)

historical_betting <- read_csv("~/Desktop/nfl_betting_df.csv")

lines_2017 <- historical_betting %>% filter(schedule_season == 2017)

#Avoid scientific notation for numbers
options(scipen = 9999)

#Obtain 2017 season (regular + post)
data2K17 <- readRDS(url('https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_2017.rds'))


#TBD