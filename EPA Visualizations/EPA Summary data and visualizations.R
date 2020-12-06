library(tidyverse)
library(devtools)
library(ggrepel)
library(ggimage)
library(nflfastR)

#Pull nflfastR data for 2020
#Pull data from github repository, run once to install and comment out
#devtools::install_github("mrcaseb/nflfastR")


#Avoid scientific notation for numbers
options(scipen = 9999)

#Obtain 2019 season (regular + post)
data2K20 <- readRDS(url('https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_2020.rds'))

#Only look at non special-teams plays, with a valid EPA
no_sp_full_epa <- data2K20 %>%
              filter(!is.na(epa), special == 0)

#Fix old team names if data is before 2020
# final_chart_1_data <- final_chart_1_data %>%
#   mutate(
#     team = case_when(
#       team == 'OAK' ~ 'LV',
#       team == 'SD' ~ 'LAC',
#       team == 'STL' ~ 'LA',
#       TRUE ~ team
#     )
#   )

#Figure out win probability calculation for win probability being between .05 < WP < 0.95 to eliminate garbage time points
#Filter for downs 1,2,3,4

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

#Table of above data
#Top 5 offenses
offensive_EPA_per_team <-  offensive_EPA_per_team%>%
  arrange(-off_epa) 

#Top 5 defenses
defensive_EPA_per_team <- defensive_EPA_per_team %>%
  arrange(def_epa) 


final_chart_1_data <- full_join(offensive_EPA_per_team, defensive_EPA_per_team, by = "team") %>% 
                        select(team, off_epa, def_epa)

#Merge team data with image data
final_chart_1_data <- final_chart_1_data %>%
                         left_join(teams_colors_logos, by = c('team' = 'team_abbr'))

defensive_EPA_per_team <- defensive_EPA_per_team %>%
                            left_join(teams_colors_logos, by = c('team' = 'team_abbr'))

offensive_EPA_per_team <- offensive_EPA_per_team %>%
                            left_join(teams_colors_logos, by = c('team' = 'team_abbr'))

#Graph the above data
ggplot(final_chart_1_data, mapping = aes(x=def_epa, y=off_epa)) +
  #horizontal line with mean offensive EPA
  geom_hline(yintercept = mean(final_chart_1_data$off_epa), color = "red", linetype = "dashed", alpha=0.5) +
  #vertical line with mean defensive EPA
  geom_vline(xintercept =  mean(final_chart_1_data$def_epa), color = "red", linetype = "dashed", alpha=0.5) +
  #add points for the QBs with the logos
  geom_image(aes(image = team_logo_espn), asp = 16 / 9) + #size goes in here
  # #add names using ggrepel, which tries to make them not overlap
  # geom_text_repel(aes(label=name)) +
  #
  labs(x = "Defensive EPA per Team",
       y = "Offensive EPA per Team",
       title = "Overall Team Efficiency",
       caption = "Data: @nflfastR") +
  theme_bw() +
  #center title
  theme(
    aspect.ratio = 9 / 16,
    plot.title = element_text(size = 14, hjust = 0.5, face = "bold"),
    axis.text.x = element_text(angle = 90),
    legend.position = "none"
  ) +
  scale_x_reverse()

#Graph Offensive and Defensive EPA charts

#Defense
ggplot(defensive_EPA_per_team, mapping = aes(x=reorder(team, def_epa, sum), y=def_epa)) +
  geom_col(fill = defensive_EPA_per_team$team_color, colour=defensive_EPA_per_team$team_color2) +
  #vertical line with mean defensive EPA
  geom_hline(yintercept = mean(defensive_EPA_per_team$def_epa), color = "red", linetype = "dashed", alpha=0.5) +
  #add points for the QBs with the logos
  geom_image(aes(image = team_logo_espn), asp = 16 / 9) + #size goes in here
  # #add names using ggrepel, which tries to make them not overlap
  # geom_text_repel(aes(label=name)) +
  #
  labs(x = "Team",
       y = "Defensive EPA per Team",
       title = "Overall Team Defensive Efficiency",
       caption = "Data: @nflfastR") +
  theme_bw() +
  #center title
  theme(
    aspect.ratio = 9 / 16,
    plot.title = element_text(size = 14, hjust = 0.5, face = "bold"),
    axis.text.x = element_text(angle = 90),
    legend.position = "none"
  ) +
  scale_y_reverse()

#Offense
ggplot(offensive_EPA_per_team, mapping = aes(x=reorder(team, -off_epa, sum), y=off_epa)) +
  geom_col(fill = offensive_EPA_per_team$team_color, colour=offensive_EPA_per_team$team_color2) +
  #vertical line with mean defensive EPA
  geom_hline(yintercept = mean(offensive_EPA_per_team$off_epa), color = "red", linetype = "dashed", alpha=0.5) +
  #add points for the QBs with the logos
  geom_image(aes(image = team_logo_espn), asp = 16 / 9) + #size goes in here
  # #add names using ggrepel, which tries to make them not overlap
  # geom_text_repel(aes(label=name)) +
  #
  labs(x = "Team",
       y = "Offensive EPA per Team",
       title = "Overall Team Offensive Efficiency",
       caption = "Data: @nflfastR") +
  theme_bw() +
  #center title
  theme(
    aspect.ratio = 9 / 16,
    plot.title = element_text(size = 14, hjust = 0.5, face = "bold"),
    axis.text.x = element_text(angle = 90),
    legend.position = "none"
  ) 





