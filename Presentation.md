---
title: "MATH513 Big Data and Social Network  Visualization"
author: '10883408'
date: "2023-12-05"
output: beamer_presentation
---



```{r setup, include=FALSE}

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
knitr::opts_chunk$set(echo = FALSE, warning = FALSE)
####################################################################################
tweets_df <- read_csv("D:/University of Plymouth/MATH513-Big Data and Social Network Visualization/Practical (presentation) submiaaion/Assesment/combined_chennai.csv")

######################################################################################3
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

#################################################################################


                  ## Exploratory Data Analysis (EDA)

 #Descriptive Statistics:
 # Summary statistics for key numerical columns
summary(tweets_df$likeCount)
summary(tweets_df$retweetCount)
summary(tweets_df$replyCount)

# Boxplot to visualize the distribution



 #Visualization:
# Time series plot for tweet frequency over time

# Assuming tweets_df is already loaded and it's in the appropriate format
tweets_df$date <- as.Date(tweets_df$date)  
tweets_by_date <- tweets_df %>%
  group_by(date) %>%
  summarize(count = n(), .groups = 'drop')

###############################################################################

 
 #Trend and Pattern Identification:



hashtag_counts <- tweets_df %>%
  separate_rows(hashtags, sep = "\\s+") %>%  
  filter(hashtags != "") %>%  
  count(hashtags, sort = TRUE)  

top_hashtags <- head(hashtag_counts, 10)  
###############################################################################

  #Creating a pie chart for the top 10 mentioned persons

mentions <- tweets_df %>%
  separate_rows(mentions, sep = ",\\s*") %>%  
  filter(mentions != "" & !is.na(mentions)) %>%  
  count(mentions, sort = TRUE)  

###############################################################################

# heatmap for locations
tweets_df <- tweets_df %>%
  mutate(
    longitude = as.numeric(str_extract(coordinates, "(?<=longitude: )[-0-9.]+")),
    latitude = as.numeric(str_extract(coordinates, "(?<=latitude: )[-0-9.]+"))
  )

south_asia_countries <- c("India", "Pakistan", "Bangladesh", "Sri Lanka", "Nepal", "Bhutan", "Maldives")

south_asia_map <- ne_countries(country = south_asia_countries, returnclass = "sf")

######################################################################################



# Function to extract city from the 'place' column
extract_city <- function(place) {
  if (!is.na(place) && place != "") {
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


##################################################################################



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


tweets_df$city <- sapply(tweets_df$place, extract_city)


tweets_df_vaccinationdrive <- tweets_df[grepl("VaccinationDrive", tweets_df$hashtags, ignore.case = TRUE), ]


city_counts_vaccinationdrive <- tweets_df_vaccinationdrive %>%
  group_by(city) %>%
  summarise(tweet_count = n()) %>%
  arrange(desc(tweet_count))

# Get the top 10 cities
top_cities_vaccinationdrive <- head(city_counts_vaccinationdrive, 10)

###############################################################################


                    ##Sentiment Analysis:



# Function to calculate total sentiment
calculate_total_sentiment <- function(tweet_text) {
 
  words <- tibble(text = tweet_text) %>%
    unnest_tokens(word, text)
  
 
  bing_sentiment <- get_sentiments("bing")
  
  
  sentiment_score <- words %>%
    inner_join(bing_sentiment, by = "word") %>%
    summarise(total_sentiment = sum(case_when(
      sentiment == "positive" ~ 1,
      sentiment == "negative" ~ -1,
      TRUE ~ 0
    ))) %>%
    pull(total_sentiment)
  
  
  if (is.na(sentiment_score)) {
    return(0)
  } else {
    return(sentiment_score)
  }
}


tweets_df$total_sentiment <- sapply(tweets_df$content, calculate_total_sentiment)




tidy_tweets <- tweets_df %>%
  unnest_tokens(word, content) %>%
  anti_join(stop_words)


bing_sentiment <- get_sentiments("bing")


sentiment_words <- tidy_tweets %>%
  inner_join(bing_sentiment, by = "word")

top_sentiment_words <- sentiment_words %>%
  count(word, sentiment, sort = TRUE) %>%
  group_by(sentiment) %>%
  top_n(10, n)  



##############################################################################
```

## Introduction:COVID-19 Discussions on Twitter in India (February-March 2021): A Detailed Analysis 


### Objective
* To get useful information by using a complete method that includes data visualization techniques and sentiment analysis.

### Approach
 * Focus on using a variety of data visualization methods to show patterns, trends, and important measures in the Twitter dataset.
 * Apply sentiment analysis to identify the emotional tones of the insights   .
 
### Analytical focus
* Analysis of the frequency and intensity of conversations.
* Identification of prominent themes within the dataset.
* Evaluation of changes in sentiment during the provided timeframe.

### Outcome 
* Anticipated Outcome: Actionable insights gathered through using various data analysis methods.
* Sentiment Analysis: A qualitative analysis that uncovers the main emotions present within a given timeframe.
* T-Test Consideration: Evaluation of suitability with a logical argument based on dataset features.


## Tweet frequency over time

* The graph spans from February 13, 2021, to March 16, 2021.

* Huge increase in the volume of tweets on March 1, 2021.

``` {r frequency-plot }
# Plot with a thicker line and improved aesthetics/ Time series plot for tweet frequency over time
ggplot(tweets_by_date, aes(x = date, y = count)) +
  geom_line(size = 1.5, color = "steelblue") +  # Increased line thickness and set color
  labs(title = "Tweet Frequency Over Time", x = "Date", y = "Number of Tweets") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0, face = "bold"),  # Center and bold the plot title
        axis.title = element_text(face = "bold"),  # Bold the axis titles
        axis.text = element_text(color = "gray20"),  # Color the axis text for better readability
        panel.grid.major.x = element_blank(),  # Remove vertical grid lines
        panel.grid.major.y = element_line(color = "gray80", linetype = "dotted"))  # Style horizontal grid lines
```

## Why maintain , English only tweets

 * Language Proficiency
 
 * Resource Constraints
 
 * Consistency and Comparability
 
 * Availability of Tools and Libraries
  
 * Research Focus

## Top hastags in Tweets

``` {r hashtags-plot}

# Creating a bar chart for the top 10 hashtags

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
      
```


## Top Mentioned people within the period 

Mostly tagged people and Locations

* Narendra Modi

* MoH India

* Prime Minister office India
``` {r mentions-plot, warning=FALSE }
# Create a 3D pie chart/pie chart for the top 10 mentioned persons
plot_ly(head(mentions,10), labels = ~mentions, values = ~n, type = 'pie', textinfo = 'label+percent') %>%
  layout(title = '', 
         scene = list(xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                      yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                      zaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)),
         showlegend = TRUE)
         

```


## Geolocation Distribution 

``` {r geolocation-plot}

ggplot() +
  geom_sf(data = south_asia_map, fill = "white", color = "black") +
  geom_point(data = tweets_df, aes(x = longitude, y = latitude), alpha = 0.5, color = "blue") +
  theme_minimal() +
  ggtitle("")
```

## Heatmap

``` {r geolocation_heatmap-plot}
ggplot() +
  geom_sf(data = south_asia_map, fill = "white", color = "black") +
  geom_density2d_filled(data = tweets_df, aes(x = longitude, y = latitude), alpha = 0.8) +
  theme_minimal() +
  ggtitle("")

```

## Locations by tweet volume 

```{r locations_by_volume-plot}

# Create a bar plot/ Get the top 10 cities
ggplot(top_cities, aes(x = reorder(city, tweet_count), y = tweet_count, fill = city)) +
  geom_bar(stat = "identity") +
  coord_flip() + # Flipping the coordinates for better readability
  labs(title = "Top 10 locations by Tweet Volume", x = "City", y = "Number of Tweets") +
  theme_minimal() +
  scale_fill_viridis_d() # Using a viridis color scale for the fill
```

## Locations by tweet volume and Vaccination Drive

``` {r locations_by_vaccinationDrive-plot}
# Create a bar plot/vaccination drive
ggplot(top_cities_vaccinationdrive, aes(x = reorder(city, tweet_count), y = tweet_count, fill = city)) +
  geom_bar(stat = "identity") +
  coord_flip() + # Flipping the coordinates for better readability
  labs(title = "Top 10 locations by Tweet Volume on VaccinationDrive", x = "City", y = "Number of Tweets") +
  theme_minimal() +
  scale_fill_viridis_d() # Using a viridis color scale for the fill

```

## Sentiment Analysis

``` {r sentiment-plot}
ggplot(top_sentiment_words, aes(x = reorder(word, n), y = n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = 'free') +
  coord_flip() +
  labs(title = "Top Words in Positive and Negative Sentiments",
       x = "Word",
       y = "Frequency") +
  theme_minimal()
```

## The T-Test Applicability

* Assumption of Normality- The t-test assumes normally distributed group data. This is especially important for small samples.

* Sample Size-  T-tests work with small and large samples, but smaller samples are less reliable.

* Outliers- When the sample size is small, outliers can have a big effect on the results of a t-test. 

## Overall Conclusion
 
* Diverse Conversations - reflecting public opinion during the period.
* Trends and Patterns - helps with a visual understanding of ongoing discussions.
* Emotional Tone - showed a mix of positive and negative sentiments.
* Analytical Focus - conversation frequency, intensity, and thematic analysis helped clarify how people communicated.
* Geographical Distribution - highlighted South Asian discussion patterns.
* Qualitative Perspective - highlighted user emotions and attitudes over the specified timeframe.
* Stakeholder Insights - gives stakeholders a complete picture of Indian Twitter users' COVID-19 discussions.

## Suggestions

* Comparative Analysis - Compare the analyzed time frame to other relevant periods or countries to find unique patterns or trends in India.

* Public Health Impact - Work with public health experts to link online discussions to indicators.

* Multimodal Analysis - Add tweet images and videos to the analysis to better understand how multimedia affects discussions.
