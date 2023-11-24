 #twitter data analysis
install.packages("rtweet")
install.packages("tidytext")
install.packages("twitteR")
 #data manipulation
install.packages("dplyr")
install.packages("ggplot2")
install.packages("plotly")
install.packages("readr")
 #statistical analysis
install.packages("stats")
install.packages("t_test")
install.packages("t.test()")
 #R markdown
install.packages("rmarkdown")
install.packages("knitr")

 #Loading the Dataset and Initial Inspection:

library(dplyr)
library(readr)
library(stringr)

tweets_df <- read_csv("D:/University of Plymouth/MATH513-Big Data and Social Network Visualization/Practical (presentation) submiaaion/Assesment/combined_chennai.csv")

 # Initial inspection
head(tweets_df)
summary(tweets_df)
str(tweets_df)

 #Handling Missing Values
  # Check for missing values
colSums(is.na(tweets_df))
#is.na(tweets_df)
#sum(is.na(tweets_df))

 #Removing rows with missing values in key columns
tweets_df <- tweets_df %>% drop_na(content, date) ##no need drop ,there is no NA in key columns

 # Converting date column to Date type
#tweets_df$date <- as.Date(tweets_df$date, format = "%Y-%m-%d")

 #text data processing

