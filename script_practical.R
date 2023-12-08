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
install.packages("lubridate")
install.packages("rnaturalearthdata")
install.packages("jsonlite")
install.packages("syuzhet")
 #R markdown
install.packages("rmarkdown")
install.packages("knitr")
install.packages("webshot")
install.packages("webshot2")

 #Loading the Dataset and Initial Inspection:

library(ggplot2)
library(readr)
library(dplyr)
library(stringr)
library(textcat)
library(tidyverse)
library(plotly)
library(ggmap)
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)
library(tidyr)
library(tidytext)
library(hrbrthemes)
library(lubridate)
library(RColorBrewer)
library(lubridate)
library(knitr)
library(webshot)
library(webshot2)

tweets_df <- read_csv("D:/University of Plymouth/MATH513-Big Data and Social Network Visualization/Practical (presentation) submiaaion/Assesment/combined_chennai.csv")

 # Initial inspection
head(tweets_df)
summary(tweets_df)
str(tweets_df)
date_range <- range(tweets_df$date)



# Check for missing values
colSums(is.na(tweets_df))


 #text data processing

clean_text <- function(text) {
  text %>%
  str_replace_all("(@\\w+|#\\w+)", "") %>%  # Remove hashtags and mentions
  str_replace_all("https://\\S+\\s?", "")%>%# Remove URLs
  str_replace_all("[^A-Za-z0-9 ,.!?'#@]", "")#remove non-English letters
  
  
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
  theme(plot.title = element_text(hjust = 0, face = "bold"),  # Center and bold the plot title
        axis.title = element_text(face = "bold"),  # Bold the axis titles
        axis.text = element_text(color = "gray20"),  # Color the axis text for better readability
        panel.grid.major.x = element_blank(),  # Remove vertical grid lines
        panel.grid.major.y = element_line(color = "gray80", linetype = "dotted"))  # Style horizontal grid lines

 
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
        plot.title = element_text(hjust = 0.07, face = "bold"),  # Center and bold the plot title
        axis.title.x = element_text(face = "bold"),  # Bold the X axis title
        axis.title.y = element_text(face = "bold"),  # Bold the Y axis title
        panel.border = element_blank(),  # Remove panel border
        axis.ticks = element_blank())  # Remove axis ticks

  #Creating a pie chart for the top 10 mentioned persons

mentions <- tweets_df %>%
  separate_rows(mentions, sep = ",\\s*") %>%  # Split mentions into separate rows
  filter(mentions != "" & !is.na(mentions)) %>%  # Filter out empty or NA strings
  count(mentions, sort = TRUE)  # Count mentions

# Create a 3D pie chart 
plot_ly(head(mentions,10), labels = ~mentions, values = ~n, type = 'pie', textinfo = 'label+percent') %>%
  layout(title = '', 
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
  ggtitle("")


ggplot() +
  geom_sf(data = south_asia_map, fill = "white", color = "black") +
  geom_density2d_filled(data = tweets_df, aes(x = longitude, y = latitude), alpha = 0.8) +
  theme_minimal() +
  ggtitle("")



# Function to extract city from the 'place' column
extract_city <- function(place) {
  if (!is.na(place) && place != "") {
    # Use a regular expression to extract the city name
    matches <- str_match(place, "'name': '([^']+)'")
    if (length(matches) > 1) {
      return(matches[1, 2])
    }
  }
  return(NA)
}

# Apply the function to create a new 'city' column
tweets_df$city <- sapply(tweets_df$place, extract_city)

# Group by city and count tweets
city_counts <- tweets_df %>%
  group_by(city) %>%
  summarise(tweet_count = n()) %>%
  arrange(desc(tweet_count))

# Get the top 10 cities
top_cities <- head(city_counts, 10)

# Create a bar plot
ggplot(top_cities, aes(x = reorder(city, tweet_count), y = tweet_count, fill = city)) +
  geom_bar(stat = "identity") +
  coord_flip() + # Flipping the coordinates for better readability
  labs(title = "Top 10 locations by Tweet Volume", x = "City", y = "Number of Tweets") +
  theme_minimal() +
  scale_fill_viridis_d() # Using a viridis color scale for the fill

############################

#Top 10 locations by Tweet Volume on VaccinationDrive


# Function to extract city from the 'place' column
extract_city <- function(place) {
  if (!is.na(place) && place != "") {
    # Use a regular expression to extract the city name
    matches <- str_match(place, "'name': '([^']+)'")
    if (length(matches) > 1) {
      return(matches[1, 2])
    }
  }
  return(NA)
}

# Apply the function to create a new 'city' column
tweets_df$city <- sapply(tweets_df$place, extract_city)

# Filter tweets that include the hashtag "VaccinationDrive"
tweets_df_vaccinationdrive <- tweets_df[grepl("VaccinationDrive", tweets_df$hashtags, ignore.case = TRUE), ]

# Group by city and count tweets
city_counts_vaccinationdrive <- tweets_df_vaccinationdrive %>%
  group_by(city) %>%
  summarise(tweet_count = n()) %>%
  arrange(desc(tweet_count))

# Get the top 10 cities
top_cities_vaccinationdrive <- head(city_counts_vaccinationdrive, 10)

# Create a bar plot
ggplot(top_cities_vaccinationdrive, aes(x = reorder(city, tweet_count), y = tweet_count, fill = city)) +
  geom_bar(stat = "identity") +
  coord_flip() + # Flipping the coordinates for better readability
  labs(title = "Top 10 locations by Tweet Volume on VaccinationDrive", x = "City", y = "Number of Tweets") +
  theme_minimal() +
  scale_fill_viridis_d() # Using a viridis color scale for the fill







                    ##Sentiment Analysis:

 #Sentiment Analysis:

################################


# Function to calculate total sentiment
calculate_total_sentiment <- function(tweet_text) {
  # Tokenize the text
  words <- tibble(text = tweet_text) %>%
    unnest_tokens(word, text)
  
  # Get sentiment lexicon
  bing_sentiment <- get_sentiments("bing")
  
  # Calculate sentiment score
  sentiment_score <- words %>%
    inner_join(bing_sentiment, by = "word") %>%
    summarise(total_sentiment = sum(case_when(
      sentiment == "positive" ~ 1,
      sentiment == "negative" ~ -1,
      TRUE ~ 0
    ))) %>%
    pull(total_sentiment)
  
  # Return 0 if no words matched the sentiment dictionary
  if (is.na(sentiment_score)) {
    return(0)
  } else {
    return(sentiment_score)
  }
}

# Applying the sentiment analysis function to the tweets
tweets_df$total_sentiment <- sapply(tweets_df$content, calculate_total_sentiment)



######################################################################



# Tokenizing the text and removing stopwords
tidy_tweets <- tweets_df %>%
  unnest_tokens(word, content) %>%
  anti_join(stop_words)

# Get sentiment lexicon
bing_sentiment <- get_sentiments("bing")

# Joining with sentiment lexicon
sentiment_words <- tidy_tweets %>%
  inner_join(bing_sentiment, by = "word")

top_sentiment_words <- sentiment_words %>%
  count(word, sentiment, sort = TRUE) %>%
  group_by(sentiment) %>%
  top_n(10, n)  # Adjust the number 10 to get more or fewer top words



ggplot(top_sentiment_words, aes(x = reorder(word, n), y = n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = 'free') +
  coord_flip() +
  labs(title = "Top Words in Positive and Negative Sentiments",
       x = "Word",
       y = "Frequency") +
  theme_minimal()





                   ##Statistical Testing:


################## t-test

tweets_df$date <- as.Date(tweets_df$date)

# Perform sentiment analysis
tweets_df$total_sentiment <- get_sentiment(tweets_df$content, method="syuzhet")

# Define the event date (change this to your specific event date)
event_date <- as.Date("2021-03-01")  # Replace YYYY-MM-DD with the actual date

# Classify tweets as before and after the event
tweets_df <- tweets_df %>%
  mutate(period = ifelse(date < event_date, "Before", "After"))

# Perform t-test
t_test_result <- t.test(total_sentiment ~ period, data = tweets_df)

# Print the t-test results
print(t_test_result)

# Plotting the histogram
ggplot(tweets_df, aes(x = total_sentiment, fill = period)) +
  geom_histogram(alpha = 0.5, position = "identity", bins = 30) +
  facet_wrap(~period) +
  labs(title = "Distribution of Sentiment Scores during Covaxin event",
       x = "Sentiment Score", y = "Frequency") +
  theme_minimal()





                




