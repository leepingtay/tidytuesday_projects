################################################################################################
# R TidyTuesday Data Exploratory Analysis
# Last Revision: 05/07/2019
#
# Author:
# Lee Ping Tay - joylp.tay@gmail.com
#
# Description:
#
# Introduction:
# The datasets can be obtained from github
# (https://github.com/rfordatascience/tidytuesday/tree/master/data/2019/2019-05-07)
# 
# The dataset contains information about global student to teacher ratios.
#
# This script is used to explore data on global student to teacher ratios in tertiary education
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

#setwd("Documents/Projects/Project_R")

library(tidyverse)

################################################################################################
# Data Import and Preprocessing
################################################################################################

# read csv files
student_ratio <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-05-07/student_teacher_ratio.csv")


################################################################################################
#  Data Wrangling / Data Visualization
################################################################################################

dim(student_ratio)  # 5189    8

# Get world map
map_world <- map_data('world') %>% 
             filter(region != "Antarctica")

dim(map_world) # 94680     6

# Check for mismatches in join
country_list <- student_ratio %>%
                distinct(country, .keep_all = FALSE) %>% 
                arrange(country)

mismatch <- anti_join(country_list, map.world, by = c('country' = 'region'))

map_country_list <- map_world %>%
  group_by(region) %>%
  summarise() %>%
  print(n = Inf)


# Correct country name
data_stud_ratio <- student_ratio %>%  
                   mutate(country = recode(country, 
                          `Bolivia (Plurinational State of)` = 'Bolivia',
                          `Brunei Darussalam` = 'Brunei',
                          `China, Hong Kong Special Administrative Region` = 'China',
                          `China, Macao Special Administrative Region` = 'China',
                          `Congo` = 'Democratic Republic of the Congo',
                          `Czechia` = 'Czech Republic',
                          `Democratic People's Republic of Korea` = 'North Korea',
                          `Republic of Korea` = 'South Korea',
                          `Iran (Islamic Republic of)` = 'Iran',
                          `Lao People's Democratic Republic` = 'Laos',
                          `Micronesia (Federated States of)` = 'Micronesia',
                          `Republic of Moldova` = 'Moldova',
                          `Russian Federation` = 'Russia',
                          `Sint Maarten (Dutch part)` = 'Sint Maarten',
                          `Syrian Arab Republic` = 'Syria',
                          `The former Yugoslav Republic of Macedonia` = 'Macedonia',
                          `United Kingdom of Great Britain and Northern Ireland` = 'UK',
                          `United Republic of Tanzania` = 'Tanzania',
                          `United States of America` = 'USA',
                          `Venezuela (Bolivarian Republic of)` = 'Venezuela',
                          `Viet Nam` = 'Vietnam'))


# Calculate mean of ratio by country
ratio_mean_grped <-data_stud_ratio %>%
                   filter(indicator == "Tertiary Education") %>% 
                   group_by(country) %>%
                   mutate(mean = mean(student_ratio, na.rm = TRUE))


# Join world map and grouped mean ratio
map_tertiary_join <- left_join(map_world, ratio_mean_grped, by = c('region' = 'country'))

dim(map_tertiary_join)  #336044     14


# Plot Student to Teacher Ratio in Tertiary Education by Country in World Map 
p1 <-
ggplot(map_tertiary_join, aes( x = long, y = lat, group = group )) +
  geom_polygon(aes(fill = student_ratio)) +
  scale_fill_gradientn(colours = c('#F9E53F','#8FD744','#35B779','#21808C','#404688','#461863'),
                       values = scales::rescale(c(10,20,30,40,50,60)),
                       labels = c("1 to 10","10.1 to 20","20.1 to 30","30.1 to 40","40.1 to 50", "50.1 to 65"),
                       breaks = c(10,20,30,40,50,60)) +
  theme(text = element_text(family = 'Comic Sans MS'),
        plot.title = element_text(size = 16),
        plot.subtitle = element_text(size = 14),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        panel.grid = element_blank(),
        panel.background = element_rect(fill = 'white'),
        plot.background = element_rect(fill = 'white'),
        legend.position = c(.12,.15),
        legend.background = element_blank(),
        legend.key = element_blank() ) +
  guides(fill = guide_legend(reverse = F)) +
  labs(title = "Student to Teacher Ratio in Tertiary Education by Country",
       subtitle = "2012 to 2017",
       x = NULL,
       y =NULL,
       caption = "\nSource: UNESCO Institute of Statistics | Graphic: @runjollyrun")
p1
ggsave(filename = "tertiary_StudtoTeacherRatio.png", p1, width = 7, height = 4, dpi = 300, 
       units = "in", device='png')

