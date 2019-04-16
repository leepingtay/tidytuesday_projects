################################################################################################
# R TidyTuesday Data Exploratory Analysis
# Last Revision: 04/16/2019
#
# Author:
# Lee Ping Tay - joylp.tay@gmail.com
#
# Description:
#
# Introduction:
# The datasets can be obtained from github
# (https://github.com/rfordatascience/tidytuesday/tree/master/data/2019/2019-04-16)
# 
# The dataset contains information about women in research with papers published between
# 2011 to 2015. Data include country name, field of study, and percentage of total 
# by field of study.
#
# This script is used to explore data on women in research and create data 
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

library(tidyverse)
library(viridis)

################################################################################################
# Data Import and Preprocessing
################################################################################################

# read csv files
women_research <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-04-16/women_research.csv")

################################################################################################
#  Data Wrangling / Data Visualization
################################################################################################

dim(women_research)  #60 3

# Rename some countries
women_research1 <- women_research %>% 
                   mutate(country = case_when(country=="United Kingdom"~"UK",
                                              country=="United States"~"US",
                                              TRUE~country))

# Create Bubble Chart
p1 <- ggplot(women_research1, aes(x = country, y = percent_women)) +
      geom_point(aes(size = percent_women, colour = field), alpha=.8) + 
      scale_size(range = c(1,6)) +
      scale_alpha(guide = 'none') +
      theme(panel.background = element_blank(), axis.line = element_line(colour = "black"),
            text = element_text(size=9),
            legend.position="bottom", legend.box = "horizontal") +
      scale_color_viridis(discrete = TRUE, option = "D")+
      scale_fill_viridis(discrete = TRUE) +
      guides(colour = guide_legend(override.aes = list(size=3)), size=FALSE) +
      labs(title="Women in Research with Papers Published 2011-15",
           x = "Country",
           y = "Percentage of total by field",
           caption = "\nSource: The Economist | Graphic: Joy Tay / @runjollyrun")
p1

ggsave(filename = "women_research.png", p1, width = 7, height = 4, dpi = 300, 
       units = "in", device='png')

