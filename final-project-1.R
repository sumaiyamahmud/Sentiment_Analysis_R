############################
######Sumaiya Mahmud########
#########Part 1#############

install.packages("sentimentr")
install.packages("wordcloud")
install.packages("wordcloud2")
install.packages("RColorBrewer")
install.packages("cld2")

library(readr)
library(sentimentr)
library(dplyr)
library(lubridate)
library(tidytext)
library(ggplot2)
library(wordcloud)
library(wordcloud2)
library(RColorBrewer)
library(cld2)


# 2 -- Read in the CSV files and combine them into a single data frame
team2_tweets_df <- bind_rows(
  read_csv("tweets0601.csv"),
  read_csv("tweets0602.csv"),
  read_csv("tweets0603.csv"),
  read_csv("tweets0604.csv"),
  read_csv("tweets0605.csv"),
  read_csv("tweets0606.csv"),
  read_csv("tweets0607.csv")
)


# 3 -- remove urls
cleaned_tweets_df <- team2_tweets_df %>%
  mutate(text = gsub("(f|ht)tp(s?)://(.*)[.][a-z]+", "", text, ignore.case=TRUE)) %>%
  mutate(text = trimws(text))


# 4 -- mutate created at variable, convert to POSIXct
cleaned_tweets_df <- cleaned_tweets_df %>% mutate(created_at = as.POSIXct(created_at, format = "%a %b %d %H:%M:%S +0000 %Y"))

# 5 -- add new date and time related columns to cleaned_tweets_df
cleaned_tweets_df <- cleaned_tweets_df %>% mutate(day = day(created_at),
                                                 hour = hour(created_at),
                                                 minute = minute(created_at),
                                                 wday = wday(created_at))

# 6 -- Convert weekday numbers to weekday names
cleaned_tweets_df$wday <- wday(cleaned_tweets_df$wday, label = TRUE, abbr = FALSE)

# 7 -- filter english tweets (contains 732407 rows)
english_tweets_df <- cleaned_tweets_df[cleaned_tweets_df$lang == "en",]


# 8 -- Get the stop words from the tidytext package
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
clean_tweet_words_df %>% 
  count(word, sort = TRUE) %>% 
  top_n(20) %>% 
  mutate(word = reorder(word,n)) %>% 
  ggplot(aes(x=word, y=n))+
  geom_col()+
  coord_flip()+
  labs(y="Number of occurences",
       x="words",
       title="Top 20 English words")


#9.b -- Save the plots in png and pdf

# 10 -- Sentiment analysis using AFINN top 15 positive and negative words
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


# 11 -- Bing top 15 positive and negative words
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


# 12 -- NRC Top 10 words for each emotion
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




















