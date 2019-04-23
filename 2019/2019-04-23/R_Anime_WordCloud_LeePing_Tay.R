################################################################################################
# R TidyTuesday Data Exploratory Analysis
# Last Revision: 04/23/2019
#
# Author:
# Lee Ping Tay - joylp.tay@gmail.com
#
# Description:
#
# Introduction:
# The datasets can be obtained from github
# (https://github.com/rfordatascience/tidytuesday/tree/master/data/2019/2019-04-23)
# 
# The dataset contains information about animeID, name, genre, episodes, score, rank,
# popularity, favorites, and synopsis.
#
# This script is used to explore Anime dataset which comes from Kaggle and create data 
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

library(tidyverse)
library(tidytext)
library(ggwordcloud)

################################################################################################
# Data Import and Preprocessing
################################################################################################

# read csv files
tidy_anime <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-04-23/tidy_anime.csv")

################################################################################################
#  Data Wrangling / Data Visualization
################################################################################################

dim(tidy_anime)  # 77911    28

# Extract synopsis
df_synopsis <- tidy_anime %>%
               select(synopsis)
  
dim(df_synopsis)  # 77911     1

# Extract words from df_synopsis
common_words <- df_synopsis %>%
                unnest_tokens(word, synopsis)  
  
# Remove common stopwords
data(stop_words)

common_words <- common_words %>%
                anti_join(stop_words) 


# Filter some extra stopwords
common_words <- common_words %>%
                filter(!(word %in% c("source", "mal", "written", "rewrite", "named", "series",
                                     "begins", "called")))

nrow(common_words)   # 3326867


# Generate top common words 
top_common_words <- common_words %>%
  count(word, sort = TRUE) %>%
  filter(n > 5000) %>%
  mutate(word = reorder(word, n))

dim(top_common_words)  # 32  2

dev.off()

# Word Cloud on top common words
p1 <- ggplot(top_common_words,
             aes(label = word,
                 size = n,
                 color = factor(sample.int(10, nrow(top_common_words), replace = TRUE)),
                 angle = 0)) +
             geom_text_wordcloud_area() +
             scale_size_area(max_size = 24) +
             theme_minimal() +
             labs(caption = "\nSource: Kaggle | Graphic: Lee Ping Tay / @runjollyrun")
p1

ggsave(filename = "wordcloud_synopsis.png", p1, width = 7, height = 5, dpi = 300, 
       units = "in", device='png')

dev.off()
