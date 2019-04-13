################################################################################################
# R TidyTuesday Data Exploratory Analysis
# Last Revision: 04/02/2019
#
# Author:
# Lee Ping Tay - joylp.tay@gmail.com
#
# Description:
#
# Introduction:
# The dataset can be obtained from the Seattle Department of Transportation.
# (https://github.com/rfordatascience/tidytuesday/tree/master/data/2019/2019-04-02)
# 
# The dataset contains information about date and time, seven location names,
# direction, and bike count.
#
# This script is used to explore data on bike traffic count and create data visualization.
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
library(reshape2)
library(readr)
library(lubridate)
library(ggplot2)

# install.packages("gganimate")
# devtools::install_github("thomasp85/gganimate")
library(gganimate)

# install.packages("gifski")
library(gifski)

install.packages("mapdata")
library(mapdata)
library(ggmap)
library(maps)

################################################################################################
# Data Import and Preprocessing
################################################################################################

# read csv file
bike_traffic <- as.data.frame(read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-04-02/bike_traffic.csv"))

dim(bike_traffic)  # 515688      5


################################################################################################
#  Data Wrangling / Data Visualization
################################################################################################

head(bike_traffic)

bike_traffic %>%
  arrange(desc(bike_count)) %>%
  head(10)

summary(bike_traffic) 


# unique values of each column
unique(bike_traffic$crossing)
unique(bike_traffic$direction)


# check for missing data
colSums(is.na(bike_traffic))


# Filter missing data and create year, month, month name, and day
df_bike_traffic <- bike_traffic %>% 
                   select(date, crossing, direction, bike_count) %>%
                   na.omit() %>%
                   mutate(date_time= date,
                          date = as.Date(date, format = "%m/%d/%Y"),
                          year = format(date, "%Y"),
                          month = format(date, "%m"),
                          month2 = factor(format(date, "%b"), levels = month.abb),
                          day = weekdays(date),
                          hour_of_day =  as.numeric(format(as.POSIXct(date_time, "%m/%d/%Y %I:%M:%S %p", tz="UTC"),"%H")),
                          week = case_when(day=="Monday" ~ "Weekday",
                                           day=="Tuesday" ~ "Weekday",
                                           day=="Wednesday"~ "Weekday",
                                           day=="Thursday"~ "Weekday",
                                           day=="Friday" ~ "Weekday",
                                           day=="Saturday" ~ "Weekend",                      
                                           day=="Sunday" ~ "Weekend"),
                          julian_day = as.numeric(format(date, "%j")) )
                        
dim(df_bike_traffic)  # 509085      12


# Filter data from 2014 t0 2018 (remove outliers)
df_bike_traffic1 <- df_bike_traffic %>%
                    filter(bike_count < 4000, year > 2013 & year < 2019)  %>%
                    mutate(year= round(as.numeric(year)))
            
dim(df_bike_traffic1)  # 498694     12


# How has bike traffic count changed over time?

# Convert yearly sum of bike traffic count data to wide format
sum_bike_yr <- df_bike_traffic1 %>%
               select(crossing, year, bike_count) %>%
               filter(!crossing =="Sealth Trail") %>%
               group_by(crossing, year) %>%
               summarise(total_bike_count = sum(bike_count, na.rm=TRUE)) %>%
               #spread(year, total_bike_count) %>%
               ungroup()

dim(sum_bike_yr) # 6 6


# Bar Graph - Yearly Bike Traffic Count by Locations in Seattle
ggplot(sum_bike_yr, aes(year, total_bike_count/1000)) +   
  geom_bar(aes(fill = crossing), position = "dodge", stat = "identity") +
  theme(panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  theme(axis.text.x = element_text(face = "bold", size = 11),
        axis.text.y = element_text(face = "bold", size = 11)) +
  labs(title = "Yearly Bike Traffic Count by Locations in Seattle", 
       x = "Year",
       y ="Bikes Traffic Count in thousand")


# Filter data on Monthly Bike Traffic Count by Locations in Seattle
bike_traffic_mth <- bike_traffic %>%
                    mutate(date=mdy_hms(date),
                    month = month(date),
                    year = year(date)) %>%
                    filter(! year %in% c(2013,2019) & ! crossing =="Sealth Trail") %>%
                    group_by(crossing, year, month) %>%
                    summarize(total_bike_count = sum(bike_count, na.rm=TRUE)) %>%
                    ungroup()

dim(bike_traffic_mth)  # 353  4


# Line Graph - Monthly Bike Traffic Count by Locations in Seattle
bike_traffic_mth %>%
  ggplot(aes(x = month, y = total_bike_count/1000, color=as.factor(year))) +
  geom_line(size=1) +
  facet_wrap(~ crossing, labeller=labeller(crossing=label_wrap_gen(30))) +
  scale_x_continuous(name = "Month", breaks = seq(1, 12, by = 1), expand=c(0,0)) +
  scale_y_continuous(name = "Bikes Traffic Count in thousands") +
  theme_bw(base_size = 15) +
  labs(title = "Monthly Bike Traffic Count by Locations in Seattle")


# calculate the sum of bike traffic count for each month
bike_traffic_month <- df_bike_traffic1 %>%
                      group_by(month, year) %>%
                      summarise(max_bike_count = sum(bike_count))


# Bar Graph on Total Monthly Bike Traffic Count in Seattle
bike_traffic_month %>%
  ggplot(aes(x = month, y = max_bike_count/1000)) +
  geom_bar(stat = "identity", fill = "mediumspringgreen") +
  facet_wrap(~ year, scales="free_x", labeller=labeller(crossing=label_wrap_gen(15))) +
  theme_bw(base_size = 13) +
  labs(title = "Total Monthly Bike Traffic Count in Seattle",
       subtitle = "Data plotted by year",
       y = "Monthly Bike Traffic Count in thousands",
       x = "Month")


# Bar Graph - Daily Bike Traffic Count by Year
df_bike_traffic1 %>%
  ggplot(aes(x = julian_day, y = bike_count)) +
  geom_point(color = "springgreen") +
  facet_wrap( ~ year, ncol = 3) +
  labs(title = "Daily Bike Count, Seattle",
       subtitle = "Data plotted by year",
       y = "Daily Bike Traffic Count",
       x = "Day of Year") + theme_bw(base_size = 15) +
  scale_x_continuous(breaks = seq(0, 366, by = 100))


# Number of bike count at each hour of the day
# Static plot
p <- ggplot(
     df_bike_traffic1, 
     aes(x = hour_of_day, y=bike_count, colour = crossing)) +
     geom_point(show.legend = FALSE, alpha = 0.7) +
     scale_color_viridis_d() +
     scale_size(range = c(2, 12)) +
     labs(title = "Bike Traffic Count at Each Hour of the Day",
          x = "Hour of day", y = "Bike Traffic Count")
p

unique(df_bike_traffic$crossing)
"Broadway Cycle Track North Of E Union St"
"Burke Gilman Trail"                      
"Elliot Bay Trail"                         
"39th Ave NE Greenway at NE 62nd St"      
"MTS Trail"                               
"Sealth Trail"                            
"NW 58th St Greenway at 22nd Ave"    


# store longtitude and lagtitude data of the 4 existing stations into a data frame
lng <- tmp$lng
lat <- tmp$lat
df_lng_lat <- as.data.frame(cbind(lng,lat))
df_lng_lat

#plot the existing stations on the map
boston_map <- get_map(location = c(lon = -71.093198, lat = 42.3581), zoom = 12, maptype = "roadmap", color="color", source = "google")

#boston_map <- get_map(location = "boston", zoom = 100, maptype = "hybrid", color="color", source = "google")
ggmap(boston_map, extent = "device") + geom_point(data=df_lng_lat, aes(x = lng, y = lat), size = 2, color = "red")


