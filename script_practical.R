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
library(textcat)
library(plotly)

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

hashtag_counts <- tweets_df %>%
  separate_rows(hashtags, sep = "\\s+") %>%  # Adjust the separator if needed
  filter(hashtags != "") %>%  # Remove empty strings if any
  count(hashtags, sort = TRUE)  # Count and sort the hashtags


# Creating a bar chart for the top 10 hashtags
ggplot(head(hashtag_counts, 10), aes(x = reorder(hashtags, n), y = n, fill = hashtags)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Top 10 Hashtags in Tweets", x = "Hashtags", y = "Count") +
  theme_minimal()

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







                    ##Sentiment Analysis:

 #Sentiment Analysis:
library(tidytext)
library(dplyr)
library(stringr)

# Tokenizing words
tweets_tokens <- tweets_df %>%
  unnest_tokens(word, content)

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









