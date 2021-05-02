## Install libraries ##

# install.packages('rjson')
# install.packages('dplyr')
# install.packages("ggplot2")


## Load libraries ##
library(ggplot2)
library(rjson)
library(dplyr)


# Set the working directory the same as the data files (CHANGE THIS TO YOUR DIRECTORY)
setwd('/Users/Kaveh/Documents/School Files/STAT 403/Project/Project Delivarables/Data/')


## DATA CLEANING AND PROCESSING ##
##################################


# Load json file
# note: the json file contains a dictionary of all trending video categories
#       and their corresponding ID. We need the ID and name in order to parse
#       the main data file.
json_cat <- fromJSON(file = "CA_category_id.json")

# Read in from json, and add values of interest to a dataframe
cat_df = data.frame()
for(i in 1:length(json_cat$items)) {
  
  name = json_cat$items[[i]]$snippet$title
  id = json_cat$items[[i]]$id
  cat_df = rbind(cat_df, c(name,id))
  
}

names(cat_df)[1] <- "categoryName"
names(cat_df)[2] <- "categoryId"

# load the csv file containing the trending YouTube videos and corresponding attributes
video_df = read.csv('CA_youtube_trending_data.csv')

# inner join categories DF with videos DF
video_df = merge(cat_df, video_df, by = 'categoryId')

# Make sure the attributes are the correct data type
video_df$categoryName   = factor(video_df$categoryName)
video_df$view_count     = as.integer(video_df$view_count)
video_df$likes          = as.integer(video_df$likes)

# Get the like to view ratio of every video
video_df$like_view_ratio = video_df$likes / video_df$view_count

# Set the correct date format
video_df$trending_date  = as.Date(video_df$trending_date, '%Y-%m-%d')

# Extract the trending year of the video
video_df$trending_year  = factor(format(as.Date(video_df$trending_date, '%Y'),"%Y"))

# Get the trending month of the video
video_df$trending_month = factor(months(video_df$trending_date))
video_df$trending_month = factor(paste0(video_df$trending_month, '-', video_df$trending_year))

# Select attributes of interest
video_df = video_df %>% select(categoryName, trending_month, trending_year, like_view_ratio)

# Subset to only include the last 3 months of 2020 and the first 3 months of 2021
video_df = subset(video_df, trending_month == 'December-2020' | trending_month == 'November-2020' | trending_month == 'October-2020' |
                    trending_month == 'January-2021' | trending_month == 'February-2021' | trending_month == 'March-2021')

# Write to a new csv file
write.csv(video_df, './processed_video_data.csv')
