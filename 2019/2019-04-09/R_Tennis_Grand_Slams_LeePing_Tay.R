################################################################################################
# R TidyTuesday Data Exploratory Analysis
# Last Revision: 04/09/2019
#
# Author:
# Lee Ping Tay - joylp.tay@gmail.com
#
# Description:
#
# Introduction:
# The datasets can be obtained from github
# (https://github.com/rfordatascience/tidytuesday/tree/master/data/2019/2019-04-09)
# 
# The dataset contains information about tennis player name, gender, age, tournament name and
# year, outcome.
#
# This script is used to explore data on tennis grand slam players and create data 
# visualization.
#
# Contents: 
# Libraries and Environment
# Data Import and Preprocessing
# Data Wrangling / Data Visualization
#
################################################################################################
# Libraries and Environment
################################################################################################

#setwd("Documents/Projects/Project_R")

library(dplyr)
library(tidyr)
#library(reshape2)
library(readr)
library(lubridate)
library(ggplot2)

# install.packages("gganimate")
# devtools::install_github("thomasp85/gganimate")
library(gganimate)

# install.packages("gifski")
library(gifski)

################################################################################################
# Data Import and Preprocessing
################################################################################################

# read csv files
player_dob <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-04-09/player_dob.csv")

grand_slams <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-04-09/grand_slams.csv")

grand_slam_timeline <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-04-09/grand_slam_timeline.csv")


################################################################################################
#
# From Tidyverse github. For Reference only. Not using it. 

# To get the tournament performance at age rather than simply across time, we need to join the 
# Date of Birth dataset with the grandslam dataset.
age_slams_comb <- left_join(grand_slams, player_dob, by = c("name")) %>% 
  mutate(age = tournament_date - date_of_birth) %>% # needs to be datetime
  group_by(name, age, gender) %>% 
  summarize(counts = n()) %>% 
  group_by(name) %>% 
  mutate(total_wins = cumsum(counts)) %>% 
  arrange(desc(total_wins))
  ungroup() %>%
  mutate(age = age / 365)

dim(age_slams_comb)  # 410  5

# test plot
age_slams_comb %>% 
  ggplot(aes(x = age, y = total_wins, group = name)) +
  geom_point() +
  geom_step() +
  facet_wrap(~gender)

################################################################################################


################################################################################################
#  Data Wrangling / Data Visualization
################################################################################################

dim(player_dob)  # 105   5
dim(grand_slams)  # 416   6
dim(grand_slam_timeline)  #12605     5

summary(player_dob)
summary(grand_slams)
summary(grand_slam_timeline)


# Calculate win count for grand slam female players
grand_slams_female <- grand_slams %>%
  filter(gender == "Female") %>%
  select(name, grand_slam, rolling_win_count) %>%
  group_by(name, grand_slam) %>% 
  summarize(counts = n()) %>%
  mutate(sum_count = sum(counts)) %>%
  ungroup()

# Name of top 10 male players
top10_female <- grand_slams_female  %>%
  filter(!(duplicated(name))) %>%
  top_n(n=10, wt = sum_count) %>%
  arrange(-sum_count) %>% 
  select(name)

# Merge dataframes for top 10 female players
top10_grand_slams_female <- left_join(top10_female, grand_slams_female , by = c("name"))


# Calculate win count for grand slam male players
grand_slams_male <- grand_slams %>%
  filter(gender == "Male") %>%
  select(name, grand_slam, rolling_win_count) %>%
  group_by(name, grand_slam) %>% 
  summarize(counts = n()) %>%
  mutate(sum_count = sum(counts)) %>%
  ungroup()

# Name of top 10 male players
top10_male <- grand_slams_male  %>%
  filter(!(duplicated(name))) %>%
  top_n(n=10, wt = sum_count) %>%
  arrange(-sum_count) %>%
  select(name)

# Merge dataframes for top 10 male players
top10_grand_slams_male <- left_join(top10_male, grand_slams_male , by = c("name"))


# Horizontal Bar Graph for Top 10 Female PLayers
p1 <- ggplot(data = top10_grand_slams_female, aes(x = reorder(name, sum_count), y = counts, 
                                                  fill = grand_slam, label=counts)) + 
  geom_bar(stat = "identity") + coord_flip() +
  theme(panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  scale_fill_brewer(palette="Spectral", guide=guide_legend(reverse=T)) +
  geom_text(size = 3, position = position_stack(vjust = 0.5)) +
  labs(title = "Top 10 Tennis Grand Slam Female Winners (1968 to 2019)", 
       x = "Player Name",
       y ="Win Count",
  caption = "\nSource: Wikipedia | Graphic: Lee Ping Tay / @runjollyrun")
p1

ggsave(filename = "female_winners.png", p1, width = 7, height = 4, dpi = 300, 
       units = "in", device='png')


# Horizontal Bar Graph Top 10 Male PLayers
p2 <- ggplot(data = top10_grand_slams_male, aes(x = reorder(name, sum_count), 
                                          y = counts, fill = grand_slam, label=counts)) + 
  geom_bar(stat = "identity") + coord_flip() +
  theme(panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  scale_fill_brewer(palette="Spectral", guide=guide_legend(reverse=T)) +
  geom_text(size = 3, position = position_stack(vjust = 0.5)) +
  labs(title = "Top 10 Tennis Grand Slam Male Winners (1968 to 2019)", 
       x = "Player Name",
       y ="Win Count",
  caption = "\nSource: Wikipedia | Graphic: Lee Ping Tay / @runjollyrun")
p2

ggsave(filename = "male_winners.png", p2, width = 7, height = 4, dpi = 300, 
       units = "in", device='png')
