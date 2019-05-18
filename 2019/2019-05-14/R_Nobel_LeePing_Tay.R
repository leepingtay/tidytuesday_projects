################################################################################################
# R TidyTuesday Data Exploratory Analysis
# Last Revision: 05/18/2019
#
# Author:
# Lee Ping Tay
#
# Description:
# Nobel Laureate Publications
#
# Introduction:
# The datasets can be obtained from github
# (https://github.com/rfordatascience/tidytuesday/tree/master/data/2019/2019-05-14)
# 
# The dataset contains information such as Nobel prize winners' name, country, prize 
# name, and category between 1901 and 2016.
#
#
# This script is used to explore Nobel prize winner dataset which comes from Kaggle
# and create data visualization.
#
# Contents: 
# Libraries and Environment
# Data Import and Preprocessing
# Data Wrangling / Data Visualization
#
################################################################################################
# Libraries and Environment
################################################################################################

#setwd("/Users/leepingtay/Documents/Projects/Project_R")

library(tidyverse)
library(circlize)


################################################################################################
# Data Import and Preprocessing
################################################################################################

# read csv files
nobel_winners <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-05-14/nobel_winners.csv")
nobel_winner_all_pubs <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-05-14/nobel_winner_all_pubs.csv")

################################################################################################
#  Data Wrangling / Data Visualization
################################################################################################

dim(nobel_winners)   # 969  18

# Data cleaning on country
nobel_country <- nobel_winners %>%
  rename(country = death_country) %>% 
  mutate(country = ifelse(is.na(country), birth_country, country)) %>% 
  mutate(country = recode(country, 
                          `Canada` = 'CA',
                          `West Germany (Germany)` = 'Germany',
                          `Netherlands` = 'NL',
                          `Switzerland`= "CHE",
                          `United States of America` = 'USA', 
                          `United Kingdom`= 'UK'))

# top 10 nobel prize winners' countries
top10_nobel_country <- nobel_country %>%
  select(country, category) %>%
  filter(!is.na(country)) %>% 
  group_by(country, category) %>% 
  tally() %>% 
  mutate(sum_count = sum(n)) %>% 
  arrange(-sum_count) %>% 
  distinct(country, sum_count) %>% 
  head(10) %>% 
  select(country)

# data for the plot
data_nobel <- top10_nobel_country %>% 
  left_join(nobel_country, by="country") %>% 
  select(country, category) %>% 
  group_by(country, category)


# Percentage of top 10 Nobel winners by countries
nrow(data_nobel)/nrow(nobel_country)*100   # 74.4%


## circular network plot
grid.col = c(Japan = "#1B9E77", UK = "#00008B", Germany = "#FFCE00",
             USA = "#E7298A", France = "#66A61E", Sweden = "#9370DB",
             CHE = "#FF3030", Italy = "#98F5FF", CA= "#104E8B", NL = "#EE7600")

chordDiagram(data_nobel, 
             directional = 1, 
             diffHeight  = -0.04,
             grid.col=grid.col)

title(main = "Top 10 Countries with the most Nobel Prize Winners")



