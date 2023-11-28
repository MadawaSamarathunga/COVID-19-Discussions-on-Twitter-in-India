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

library(stringr)

clean_text <- function(text) {
  text %>%
  #str_replace_all("(@\\w+|#\\w+)", "") %>%  # Remove hashtags and mentions
  str_replace_all("[^[:alnum:][:space:]#,']", "")%>%# Remove special characters
  str_replace_all("https?://\\S+\\s?", "")   # Remove URLs
    
}

# Clean 'content' column
tweets_df$content <- sapply(tweets_df$content, clean_text)


                  ## Exploratory Data Analysis (EDA)

 #Descriptive Statistics:
 # Summary statistics for key numerical columns
summary(tweets_df$likeCount)
summary(tweets_df$retweetCount)
summary(tweets_df$replyCount)

# Boxplot to visualize the distribution
library(ggplot2)
ggplot(tweets_df, aes(y = likeCount)) + geom_boxplot() + theme_minimal() + ggtitle("Distribution of Like Counts")

 #Visualization:
# Time series plot for tweet frequency over time
tweets_df$date <- as.Date(tweets_df$date)  
tweets_by_date <- tweets_df %>% group_by(date) %>% summarize(count = n())

ggplot(tweets_by_date, aes(x = date, y = count)) +
  geom_line() +
  labs(title = "Tweet Frequency Over Time", x = "Date", y = "Number of Tweets") +
  theme_minimal()
 
# Histogram for like counts
ggplot(tweets_df, aes(y = likeCount)) +
  geom_histogram(bins = 30, fill = "blue", color = "black") +
  labs(title = "Histogram of Like Counts", x = "Like Count", y = "Frequency") +
  theme_minimal()

 #Trend and Pattern Identification:
library(tidyr)
library(tidytext)

# Extracting hashtags from tweets
tweets_df$hashtags <- str_extract_all(tweets_df$content, "#\\S+")

# Unnesting the hashtags for analysis
hashtag_counts <- tweets_df %>% 
  unnest(hashtags) %>% 
  count(hashtags, sort = TRUE)

# Top 10 hashtags bar chart
ggplot(head(hashtag_counts, 10), aes(x = reorder(hashtags, n), y = n, fill = hashtags)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Top 10 Hashtags in Tweets", x = "Hashtags", y = "Count") +
  theme_minimal()



















