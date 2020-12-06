library(tidyverse)
library(devtools)
library(ggrepel)
library(ggimage)
library(nflfastR)

#Pull data from github repository, run once to install and comment out
#devtools::install_github("mrcaseb/nflfastR")

#Avoid scientific notation for numbers
options(scipen = 9999)

#Obtain 2019 season (regular + post)
data2K19 <- readRDS(url('https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_2019.rds'))

#Intro to nflfastR, Fourth down plays that aren't special teams plays
data2K19 %>% 
  filter(down == 4 & special == 0) %>%
  select(down, ydstogo, desc) %>% 
  head()

#Get lions dataset
DET2K19 <- data2K19 %>% 
            filter(home_team == "DET" | away_team == "DET")

#Check that there's 16 games, pulled the entire regular season 
DET2K19 %>%
  filter(desc == "GAME") %>%
    count()

DETOff <- DET2K19 %>% filter(posteam == "DET")
DETDef <- DET2K19 %>% filter(posteam != "DET")

#Observe the number of 4th downs the lions went for 
DETOff %>% 
  filter(down == 4 & special == 0) %>%
  select(play_id, down, ydstogo, desc) 

#EPA, Success Rate, YPC for Detroit Running Backs
DETOff %>%
        filter(rush == 1) %>%
          group_by(rusher) %>%
            summarize(
              mean_epa = mean(epa), success_rate = mean(success), ypc=mean(yards_gained), plays=n()
               ) %>%
                arrange(-mean_epa) %>%
                  filter(plays > 20)

#Same as above but excluding 2-pt conversions and penalties
RBS <- DETOff %>%
        filter(down <=4, play_type == 'run') %>%
          group_by(rusher) %>%
            summarize(mean_epa = mean(epa), success_rate = mean(success), ypc=mean(yards_gained), plays=n()) %>%
              arrange(-mean_epa) %>%
                filter(plays > 20 &  !is.na(rusher))


ggplot(data = RBS, mapping = aes(x = rusher, y=mean_epa)) + geom_col()

