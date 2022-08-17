#Sentiment Analysis using Twitter Data
packages <- c("readxl","tidytext","plyr","dplyr","tidyr","ggplot2","scales",
              "purrr","textdata","wordcloud","reshape2","stringr","igraph",
              "ggraph","widyr","grid","arules","tm","topicmodels", 'rtweet','lubridate', 'tweet')
for(i in packages){
  if(!require(i,character.only = T, quietly = T)){
    install.packages(i)
  }
  library(i, character.only = T, quietly = T)
}

Sys.setlocale(category = "LC_ALL", locale = "persian")

t <- create_token(
  app = "..........",
  '........................',
  '.................................',
  access_token = '.................................',
  access_secret = '.................................',
  set_renv = TRUE
)


# Get tweets
tweets <- search_tweets("Tehran", n = 72000, include_rts = F, retryonratelimit = T, lang = 'en', token = get_token())


# Count the nubmer of tweets by source
tweets %>% count(source, sort = TRUE)

# Clean the tweets
cleaned_tweets <- tweets %>%
  select(created_at, source, text, favorite_count, retweet_count) %>%
  filter(source %in% c("Twitter for iPhone", "Twitter for Android")) %>%
  extract(source, "source", "(\\w+)$")

# Inspect the first six rows
head(cleaned_tweets)

# Plot the percentage of tweets by hour of the day for each device
cleaned_tweets %>%
  count(source, hour = hour(with_tz(created_at, "EST"))) %>%
  mutate(percent = n / sum(n)) %>%
  ggplot(aes(hour, percent, color = source)) +
  geom_line() +
  scale_y_continuous(labels = scales::label_percent()) +
  labs(x = "Hour of day (EST)",
       y = "% of tweets",
       color = "")

# Load stringr
library(stringr)

# Plot the number of tweets with and without quotes by device
cleaned_tweets %>%
  count(source,
        quoted = ifelse(str_detect(text, '^"'), "Quoted", "Not quoted")) %>%
  ggplot(aes(source, n, fill = quoted)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "", y = "Number of tweets", fill = "") +
  ggtitle('Whether tweets start with a quotation mark (")')

# Count the number of tweets with and without picture/links by device
tweet_picture_counts <- cleaned_tweets %>%
  filter(!str_detect(text, '^"')) %>%
  count(source,
        picture = ifelse(str_detect(text, "t.co"),
                         "Picture/link", "No picture/link"))

# Make a bar plot
ggplot(tweet_picture_counts, aes(source, n, fill = picture)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "", y = "Number of tweets", fill = "")

# Load the tidytext package
library(tidytext)

# Create a regex pattern
reg <- "([^A-Za-z\\d#@']|'(?![A-Za-z\\d#@]))"

# Unnest the text strings into a data frame of words
tweet_words <- cleaned_tweets %>%
  filter(!str_detect(text, '^"')) %>%
  mutate(text = str_replace_all(text, "https://t.co/[A-Za-z\\d]+|&amp;", "")) %>%
  unnest_tokens(word, text, token = "regex", pattern = reg) %>%
  filter(!word %in% stop_words$word,
         str_detect(word, "[a-z]"))

# Inspect the first six rows of tweet_words
head(tweet_words)

# Plot the most common words from @realDonaldTrump tweets
tweet_words %>%
  count(word, sort = TRUE) %>%
  head(20) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_bar(stat = "identity") +
  ylab("Occurrences") +
  coord_flip()

# Create the log odds ratio of each word
android_iphone_ratios <- tweet_words %>%
  count(word, source) %>%
  group_by(word)  %>%
  filter(sum(n) >= 5) %>%
  spread(source, n, fill = 0) %>%
  ungroup()  %>%
  mutate_if(is.numeric, ~((. + 1) / sum(. + 1))) %>%
  mutate(logratio = log2(Android / iPhone)) %>%
  arrange(desc(logratio))

# Inspect the first six rows
head(android_iphone_ratios)

# Plot the log odds ratio for each word by device
android_iphone_ratios %>%
  group_by(logratio > 0) %>%
  top_n(15, abs(logratio)) %>%
  ungroup() %>%
  mutate(word = reorder(word, logratio)) %>%
  ggplot(aes(word, logratio, fill = logratio < 0)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  ylab("Android / iPhone log ratio") +
  scale_fill_manual(name = "", labels = c("Android", "iPhone"),
                    values = c("red", "lightblue"))

# Create a sentiment data frame from the NRC lexicon
nrc <- get_sentiments('nrc')

# Join the NRC lexicon to log odds ratio data frame
android_iphone_sentiment <- android_iphone_ratios %>%
  inner_join(nrc, by = "word") %>%
  filter(!sentiment %in% c("positive", "negative")) %>%
  mutate(sentiment = reorder(sentiment, -logratio),
         word = reorder(word, -logratio)) %>%
  group_by(sentiment) %>%
  top_n(10, abs(logratio)) %>%
  ungroup()

# Inspect the first six rows
head(android_iphone_sentiment)

# Plot the log odds ratio of words by device in groups sentiments
ggplot(android_iphone_sentiment, aes(word, logratio, fill = logratio < 0)) +
  facet_wrap(~ sentiment, scales = "free", nrow = 2) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "", y = "Android / iPhone log ratio") +
  scale_fill_manual(name = "", labels = c("Android", "iPhone"),
                    values = c("red", "lightblue"))


# Count Sentiments
sentiment <- tweet_words %>%
  inner_join(nrc, by = "word")
sentiments_count <- sentiment %>% count(sentiment, sort = TRUE)

# Plot Sentiments with colors for each bar
ggplot(sentiments_count, aes(sentiment, n, fill = sentiment)) +
  geom_bar(stat = "identity") +
  labs(x = "Sentiments", y = "Number of tweets", fill = "n")


save_as_csv(tweets, 'Saudi.csv')







Kha <- get_timeline("IranIntl_En", 3000, include_rts = F, retryonratelimit = F,lang= 'en', token = t)
tweetss <- search_fullarchive("",fromDate ='201101012315',
                              toDate = '202201012315' ,
                              5000 ,token = t,env_name = 'dd', )
amber <- search_tweets("Elden Ring", 18000, token = t, lang= 'en', retryonratelimit = T)
save_as_csv(amber,'amber.csv')
# change locale to persian Iran

data(stop_words)
custom_stop_words <- bind_rows(
  tibble(word = c("t.co","csun","blm","rt","https",
                  "Johnny","dep"),lexicon = "custom"), stop_words)

remove_reg <- "&amp;|&lt;|&gt;"

#create a list containing all six data frames
dfList2<-list(chael)

tweets <- Kha
# Get the text column
text <- tweets$text
# Set the text to lowercase
text <- tolower(text)
# Remove mentions, urls, emojis, numbers, punctuations, etc.
text <- gsub("@\\w+", "", text)
text <- gsub("https?://.+", "", text)
text <- gsub("\\d+\\w*\\d*", "", text)
text <- gsub("#\\w+", "", text)
text <- gsub("[^\x01-\x7F]", "", text)
text <- gsub("[[:punct:]]", " ", text)
# Remove spaces and newlines
text <- gsub("\n", " ", text)
text <- gsub("^\\s+", "", text)
text <- gsub("\\s+$", "", text)
text <- gsub("[ |\t]+", " ", text)
# Put the data to a new column
tweets["fix_text"] <- text
head(tweets$fix_text, 10)

tweets$stripped_text <- gsub("http.*","",  tweets$text)
tweets$stripped_text <- gsub("https.*","", tweets$stripped_text)

# remove punctuation, convert to lowercase, add id for each tweet!
tweets_clean <- tweets %>%
  dplyr::select(stripped_text) %>%
  unnest_tokens(word, stripped_text)
data("stop_words")

cleaned_tweet_words <- tweets_clean %>%
  anti_join(stop_words)

cleaned_tweet_words %>%
  count(word, sort = TRUE) %>%
  top_n(15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
      labs(y = "Count",
      x = "Unique words",
      title = "Count of unique words found in tweets",
      subtitle = "Stop words removed from the list")

library(devtools)
library(widyr)

# remove punctuation, convert to lowercase, add id for each tweet!
climate_tweets_paired_words <- tweets %>%
  dplyr::select(stripped_text) %>%
  unnest_tokens(paired_words, stripped_text, token = "ngrams", n = 2)

climate_tweets_paired_words %>%
  count(paired_words, sort = TRUE)

library(tidyr)
climate_tweets_separated_words <- climate_tweets_paired_words %>%
  separate(paired_words, c("word1", "word2"), sep = " ")

climate_tweets_filtered <- climate_tweets_separated_words %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

# new bigram counts:
climate_words_counts <- climate_tweets_filtered %>%
  count(word1, word2, sort = TRUE)

library(igraph)
library(ggraph)

# plot climate change word network
# (plotting graph edges is currently broken)
climate_words_counts %>%
        filter(n >= 10) %>%
        graph_from_data_frame() %>%
        ggraph(layout = "fr") +
        # geom_edge_link(aes(edge_alpha = n, edge_width = n))
        # geom_edge_link(aes(edge_alpha = n, edge_width = n)) +
        geom_node_point(color = "darkslategray4", size = 3) +
        geom_node_text(aes(label = name), vjust = 1.8, size = 3) +
        labs(title = "Word Network: Tweets using the hashtag - WW3",
             subtitle = "Text mining twitter data ",
             x = "", y = "")

# plot climate change word network
climate_words_counts %>%
        filter(n >= 10) %>%
        graph_from_data_frame() %>%
        ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = n, edge_width = n)) +
    geom_node_point(color = "darkslategray4", size = 3) +
    geom_node_text(aes(label = name), vjust = 1.8, size = 3) +
    labs(title = "Word Network",
         #subtitle = "Text mining twitter data ",
         x = "", y = "")
###############################################################################
# Create a regex pattern
reg <- "([^A-Za-z\\d#@']|'(?![A-Za-z\\d#@]))"

# Unnest the text strings into a data frame of words
cleaned_tweets <- amber %>%
  select(created_at, source, text, favorite_count, retweet_count)
tweet_words <- cleaned_tweets %>%
  filter(!str_detect(text, '^"')) %>%
  mutate(text = str_replace_all(text, "https://t.co/[A-Za-z\\d]+|&amp;", "")) %>%
  unnest_tokens(word, text, token = "regex", pattern = reg) %>%
  filter(!word %in% stop_words$word,
         str_detect(word, "[a-z]"))

################################################
bing_word_counts <- tweet_words %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

bing_word_counts %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(title = "Sentiment during the 2013 flood event.",
       y = "Contribution to sentiment",
       x = NULL) +
  coord_flip()

nrc_word_counts <- tweet_words %>%
  inner_join(get_sentiments("nrc")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

nrc_word_counts %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(title = "Sentiment during the 2013 flood event.",
       y = "Contribution to sentiment",
       x = NULL) +
  coord_flip()

