 #twitter data analysis
install.packages("rtweet")
install.packages("tidytext")
install.packages("twitteR")
 #data manipulation
install.packages("dplyr")
install.packages("ggplot2")
install.packages("plotly")
install.packages("readr")
install.packages("textcat")
install.packages("ggmap")
install.packages("rnaturalearth")
install.packages('sf')
install.packages('hrbrthemes')
 #statistical analysis
install.packages("stats")
install.packages("t_test")
install.packages("t.test()")
installed.packages("lubridate")
 #R markdown
install.packages("rmarkdown")
install.packages("knitr")

 #Loading the Dataset and Initial Inspection:

library(ggplot2)
library(dplyr)
library(readr)
library(stringr)
library(textcat)
library(plotly)
library(ggmap)
library(tidyverse)
library(rnaturalearth)
library(sf)
library(tidyr)
library(tidytext)
library(hrbrthemes)
library(lubridate)
library(RColorBrewer)

tweets_df <- read_csv("D:/University of Plymouth/MATH513-Big Data and Social Network Visualization/Practical (presentation) submiaaion/Assesment/combined_chennai.csv")

 # Initial inspection
head(tweets_df)
summary(tweets_df)
str(tweets_df)
date_range <- range(tweets_df$date)


 #Handling Missing Values
  # Check for missing values
colSums(is.na(tweets_df))
#is.na(tweets_df)
#sum(is.na(tweets_df))

 #Removing rows with missing values in key columns
#tweets_df <- tweets_df %>% drop_na(content, date) ##no need drop ,there is no NA in key columns

 # Converting date column to Date type
#tweets_df$date <- as.Date(tweets_df$date, format = "%Y-%m-%d")

 #text data processing



clean_text <- function(text) {
  text %>%
  str_replace_all("(@\\w+|#\\w+)", "") %>%  # Remove hashtags and mentions
  str_replace_all("https://\\S+\\s?", "")%>%# Remove URLs
  str_replace_all("[^A-Za-z0-9 ,.!?'#@]", "")#remove non-English letters
  #str_replace_all("[^[:alnum:][:space:]#,']", "")# Remove special characters
  
}
# Clean content column
tweets_df$content <- sapply(tweets_df$content, clean_text)

#clean the hashtags column
tweets_df <- tweets_df %>%
  mutate(hashtags = str_replace_all(hashtags, "[\\[\\]',]", ""))
#clean coordinate column
tweets_df <- tweets_df %>%
  mutate(coordinates = str_replace_all(coordinates, "[\\[\\]']", ""))

#Extracting mentions
tweets_df <- tweets_df %>%
  mutate(mentions = str_extract_all(renderedContent, "@\\w+"))

tweets_df <- tweets_df %>%
  mutate(mentions = sapply(mentions, paste, collapse = ", "))






                  ## Exploratory Data Analysis (EDA)

 #Descriptive Statistics:
 # Summary statistics for key numerical columns
summary(tweets_df$likeCount)
summary(tweets_df$retweetCount)
summary(tweets_df$replyCount)

# Boxplot to visualize the distribution

ggplot(tweets_df, aes(y = likeCount)) + geom_boxplot() + theme_minimal() + ggtitle("Distribution of Like Counts")

 #Visualization:
# Time series plot for tweet frequency over time

# Assuming tweets_df is already loaded and it's in the appropriate format
tweets_df$date <- as.Date(tweets_df$date)  
tweets_by_date <- tweets_df %>%
  group_by(date) %>%
  summarize(count = n(), .groups = 'drop')

# Plot with a thicker line and improved aesthetics
ggplot(tweets_by_date, aes(x = date, y = count)) +
  geom_line(size = 1.5, color = "steelblue") +  # Increased line thickness and set color
  labs(title = "Tweet Frequency Over Time", x = "Date", y = "Number of Tweets") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),  # Center and bold the plot title
        axis.title = element_text(face = "bold"),  # Bold the axis titles
        axis.text = element_text(color = "gray20"),  # Color the axis text for better readability
        #panel.grid.minor = element_blank(),  # Remove minor grid lines
        panel.grid.major.x = element_blank(),  # Remove vertical grid lines
        panel.grid.major.y = element_line(color = "gray80", linetype = "dotted"))  # Style horizontal grid lines

 
# Histogram for like counts
#ggplot(tweets_df, aes(y = likeCount)) +
 #geom_histogram(bins = 30, fill = "blue", color = "black") +
  #labs(title = "Histogram of Like Counts", x = "Like Count", y = "Frequency") +
  #theme_minimal()

 #Trend and Pattern Identification:


hashtag_counts <- tweets_df %>%
  separate_rows(hashtags, sep = "\\s+") %>%  # Adjust the separator if needed
  filter(hashtags != "") %>%  # Remove empty strings if any
  count(hashtags, sort = TRUE)  # Count and sort the hashtags


# Creating a bar chart for the top 10 hashtags
top_hashtags <- head(hashtag_counts, 10)  # Get the top 10 hashtags
ggplot(top_hashtags, aes(x = reorder(hashtags, n), y = n, fill = hashtags)) +
  geom_bar(stat = "identity", show.legend = FALSE) +  # Remove legend for clarity
  coord_flip() +  # Flip coordinates for horizontal bars
  scale_fill_brewer(palette = "Paired") +  # Use a color palette from RColorBrewer
  labs(title = "Top 10 Hashtags in Tweets", x = "Hashtags", y = "Count") +
  theme_minimal() +
  theme(text = element_text(size = 12),  # Adjust text size for better readability
        plot.title = element_text(hjust = 0.5, face = "bold"),  # Center and bold the plot title
        axis.title.x = element_text(face = "bold"),  # Bold the X axis title
        axis.title.y = element_text(face = "bold"),  # Bold the Y axis title
        #panel.grid.major = element_blank(),  # Remove major grid lines
        #panel.grid.minor = element_blank(),  # Remove minor grid lines
        panel.border = element_blank(),  # Remove panel border
        axis.ticks = element_blank())  # Remove axis ticks

  #Creating a pie chart for the top 10 mentioned persons

mentions <- tweets_df %>%
  separate_rows(mentions, sep = ",\\s*") %>%  # Split mentions into separate rows
  filter(mentions != "" & !is.na(mentions)) %>%  # Filter out empty or NA strings
  count(mentions, sort = TRUE)  # Count mentions

# Create a 3D pie chart 
plot_ly(head(mentions,10), labels = ~mentions, values = ~n, type = 'pie', textinfo = 'label+percent') %>%
  layout(title = '3D Pie Chart of Mentions in Tweets', 
         scene = list(xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                      yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                      zaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)),
         showlegend = TRUE)

# heatmap for locations
tweets_df <- tweets_df %>%
  mutate(
    longitude = as.numeric(str_extract(coordinates, "(?<=longitude: )[-0-9.]+")),
    latitude = as.numeric(str_extract(coordinates, "(?<=latitude: )[-0-9.]+"))
  )

south_asia_countries <- c("India", "Pakistan", "Bangladesh", "Sri Lanka", "Nepal", "Bhutan", "Maldives")

south_asia_map <- ne_countries(country = south_asia_countries, returnclass = "sf")

ggplot() +
  geom_sf(data = south_asia_map, fill = "white", color = "black") +
  geom_point(data = tweets_df, aes(x = longitude, y = latitude), alpha = 0.5, color = "blue") +
  theme_minimal() +
  ggtitle("Geographical Distribution of Tweets in South Asia")


ggplot() +
  geom_sf(data = south_asia_map, fill = "white", color = "black") +
  geom_density2d_filled(data = tweets_df, aes(x = longitude, y = latitude), alpha = 0.7) +
  theme_ipsum() +
  ggtitle("Heatmap of Tweet Locations")









                    ##Sentiment Analysis:

 #Sentiment Analysis:


# Tokenizing words
tweets_tokens <- tweets_df %>%
  unnest_tokens(word, content)%>%
  
  

# Performing sentiment analysis
bing_lexicon <- get_sentiments("bing")

sentiment_analysis <- tweets_tokens %>%
  inner_join(bing_lexicon, by = "word") %>%
  count(id, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment_score = positive - negative)

 #Aggregating Sentiment Analysis:

# Aggregating sentiment scores for each tweet
tweets_sentiment <- sentiment_analysis %>%
  group_by(id) %>%
  summarize(total_sentiment = sum(sentiment_score))

# Joining back with the original dataframe
tweets_df <- tweets_df %>%
  left_join(tweets_sentiment, by = "id")

# Histogram of sentiment scores
ggplot(tweets_df, aes(x = total_sentiment)) +
  geom_histogram(bins = 30, fill = "blue", color = "red") +
  labs(title = "Histogram of Sentiment Scores", x = "Sentiment Score", y = "Frequency") +
  theme_minimal()


 #Top 10 words within this Covid periods

word_counts <- tweets_df %>%
  unnest_tokens(word, content) %>%  # Tokenizing the text into words
  anti_join(stop_words, by = "word") %>%
  count(word, sort = TRUE)          # Counting and sorting the words

top_words <- head(word_counts, 10)

ggplot(top_words, aes(x = reorder(word, n), y = n, fill = word)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Top 10 Words in the Dataset (Excluding Stopwords)", x = "Words", y = "Frequency") +
  theme_minimal()







                ##Statistical Testing:

 #Preparing the data
# Define the event date
event_date <- as.Date("2021-03-01")  # Replace with the actual event date

# Classify tweets as before and after the event
tweets_df <- tweets_df %>%
  #mutate(period = ifelse(date < event_date, "Before", "After"))
mutate(period = ifelse(date < as.POSIXct(event_date), "Before", "After"))


# Perform t-test
t_test_result <- t.test(total_sentiment ~ period, data = tweets_df)

# View the results
print(t_test_result)

# Histogram to check the distribution


ggplot(tweets_df, aes(x = total_sentiment, fill = period)) +
  geom_histogram(alpha = 0.5, position = "identity", bins = 30) +
  facet_wrap(~period) +
  labs(title = "Distribution of Sentiment Scores Before and After the Event",
       x = "Sentiment Score", y = "Frequency") +
  theme_minimal()









