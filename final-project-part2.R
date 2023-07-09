############################
######Sumaiya Mahmud########
######Part 2################
############################

install.packages("sentimentr")
install.packages("wordcloud")
install.packages("wordcloud2")
install.packages("RColorBrewer")
install.packages("stopwords")


library(readr)
library(sentimentr)
library(dplyr)
library(lubridate)
library(tidytext)
library(ggplot2)
library(wordcloud)
library(wordcloud2)
library(RColorBrewer)
library(stopwords)
library(tidytext)

# Read in the CSV files and combine them into a single data frame
team2_tweets_df <- bind_rows(
  read_csv("tweets0601.csv"),
  read_csv("tweets0602.csv"),
  read_csv("tweets0603.csv"),
  read_csv("tweets0604.csv"),
  read_csv("tweets0605.csv"),
  read_csv("tweets0606.csv"),
  read_csv("tweets0607.csv")
)


#remove url
cleaned_tweets_df$text <- gsub("http\\S+\\s?", "", cleaned_tweets_df$text)

#mutate created at variable, convert to POSIXct
cleaned_tweets_df <- cleaned_tweets_df %>% mutate(created_at = as.POSIXct(created_at, format = "%a %b %d %H:%M:%S +0000 %Y"))

#add new date and time related columns to cleaned_tweets_df
cleaned_tweets_df <- cleaned_tweets_df %>% mutate(day = day(created_at),
                                                  hour = hour(created_at),
                                                  minute = minute(created_at),
                                                  wday = wday(created_at))

# Convert weekday numbers to weekday names
cleaned_tweets_df$wday <- wday(cleaned_tweets_df$wday, label = TRUE, abbr = FALSE)

#filter english tweets (contains 732407 rows)
english_tweets_df <- cleaned_tweets_df[cleaned_tweets_df$lang=="en",]


# Get the stop words from the tidytext package
stop_words <- tidytext::stop_words$word

# Combine the three profanity lists and remove duplicates
profanity_words <- unique(c(lexicon::profanity_alvarez, 
                            lexicon::profanity_banned,
                            lexicon::profanity_arr_bad))

# Get all non-stop words that are not in the profanity list
clean_tweet_words_df <- english_tweets_df %>%
  unnest_tokens(word, text) %>%
  anti_join(data.frame(word = stop_words)) %>%
  anti_join(data.frame(word = profanity_words))


# 9.a -- Get the top 20 English words and their frequency
top_words <- clean_tweet_words_df %>%
  filter(!word %in% stop_words) %>%
  count(word, sort = TRUE) %>%
  slice(1:20)

# Create the plot
ggplot(top_words, aes(x = reorder(word, n), y = n)) +
  geom_col(fill = "purple", color = "black") +
  scale_y_continuous(expand = c(0, 0)) +
  labs(title = "Top 20 English Words in Tweets",
       x = "Words",
       y = "Frequency") +
  coord_flip()

#9.b -- Save the plots in png and pdf

#Sentiment analysis using AFINN top 15 positive and negative words
afinn_word_counts <- clean_tweet_words_df %>%
  inner_join(get_sentiments("afinn")) %>%
  mutate(sentiment = if_else(value >0, 'positive','negative'))%>%
  count(word, value, sentiment, sort = TRUE) %>%
  ungroup()

afinn_word_counts %>%
  group_by(sentiment) %>%
  top_n(15) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(title = "AFINN Sentiment analysis",
       y = "Contribution to sentiment",
       x = NULL) +
  coord_flip()


#Bing top 15 positive and negative words
bing_word_counts <- clean_tweet_words_df %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

bing_word_counts %>%
  group_by(sentiment) %>%
  top_n(15) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(title = "BING Sentiment Analysis",
       y = "Contribution to sentiment",
       x = NULL) +
  coord_flip()


#NRC Top 10 words for each emotion
nrc_word_counts <- clean_tweet_words_df %>%
  inner_join(get_sentiments("nrc")) %>%
  count(word, sentiment, sort = TRUE)
ungroup()

nrc_word_counts %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(title = "NRC Sentiment Analysis",
       y = "Contribution to sentiment",
       x = NULL) +
  coord_flip()

#Part 2
# Remove most common words
clean_tweet_words_df <- clean_tweet_words_df[!clean_tweet_words_df$word %in% c("rt", "t.co"),]


#count the number of occurrences of each word in the word column
word_counts <- clean_tweet_words_df %>%
  count(word)



# 2a -- Choose a color palette
palette <- brewer.pal(7, "Blues")

# Get the top 100 words and their frequency counts
top_words <- clean_tweet_words_df %>%
  count(word, sort = TRUE) %>%
  top_n(100)

# Create a vector of colors for the top 100 words
word_colors <- rep(palette, length.out = 100)

# Create a word cloud with the chosen palette and display the top 100 words
wordcloud(top_words$word, top_words$n, max.words = 100, colors = word_colors)

#2b -- # Choose a color palette
palette <- brewer.pal(11, "RdYlBu")

# Get the top 75 words and their frequency counts
top_words2 <- clean_tweet_words_df %>%
  count(word, sort = TRUE) %>%
  top_n(75)

# Create a vector of colors for the top 75 words
word_colors <- rep(palette, length.out = 75)

# Create a word cloud with the chosen palette and display the top 75 words

wordcloud(words = top_words2$word, freq = top_words$n,
          colors = word_colors, scale = c(5, .2))

#2c -- Choose a color palette
palette <- brewer.pal(n = 9, name = "Set1")

# Get the top 50 words and their frequency counts
top_words <- clean_tweet_words_df %>%
  count(word, sort = TRUE) %>%
  top_n(50)

# Create a vector of colors for the top 50 words
word_colors <- rep(palette, length.out = 50)

# Create a word cloud with the chosen palette and display the top 50 words
wordcloud(words = top_words$word, freq = top_words$n,
          colors = word_colors, scale = c(5, .2))


#2d -- # Create a word cloud with PuBu palette, fonts, and display the top 100 words
clean_tweet_words_df %>%  count(word) %>%  with(wordcloud(word, n, max.words = 100, colors = brewer.pal(9, "PuBu"), 
scale = c(5, 0.2), rot.per = 0.35, random.order = FALSE, family = c("serif")))


#3a -- create a new data frame that contains the frequencies of words
words_with_freq_df <- clean_tweet_words_df %>% count(word, sort=TRUE)
wordcloud2(data=words_with_freq_df, color="random-dark")

# Create word cloud with random-light color palette and black background
wordcloud2(data = words_with_freq_df, color = "random-light", backgroundColor = "black")


# 3b -- Generate wordcloud with random-dark colour
wordcloud2(data = words_with_freq_df, 
           color = "random-dark", backgroundColor = "white")

# Change the shape to star
wordcloud2(words_with_freq_df, size = 0.4, shape = 'star')


# 3c -- Generate wordcloud with random-dark colour
wordcloud2(data = words_with_freq_df, 
           color = "random-dark", backgroundColor = "white")

# Change the shape to star
wordcloud2(words_with_freq_df[1:200,], size = 0.4, shape = "pentagon", fontFamily="gothic")


