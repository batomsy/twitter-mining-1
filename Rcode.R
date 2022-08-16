library (rtweet)
library(tidyverse)
library(RColorBrewer)
library(TSstudio)
library(wordcloud)
library(tidytext)
library(syuzhet)
library(ngram)
library(igraph)
library(ggraph)

twitter_token <- create_token
(
app = 
consumer_key = 
consumer_secret = 
access_token = 
access_secret = 

set_renv = TRUE)


#get tweets without retweets
P_Obi <- get_timeline("@PeterObi", n=3000,include_rts=FALSE)

users <- users_data(P_Obi)
P_obi_full <- cbind(P_Obi,users)

# Remove replies
P_Obi_organic <- subset(P_obi_full, is.na(P_obi_full$in_reply_to_status_id))

P_Obi_organic <- P_Obi_organic %>% arrange(-favorite_count)
P_Obi_organic[1,5]
P_Obi_organic <- P_Obi_organic %>% arrange(-retweet_count)
P_Obi_organic[1,5]

Favourites <- head(P_Obi_organic, n=5)

#Aggregates to show mean, median and max for likes and retweets
Aggregates_FAVS <- P_Obi_organic %>%
  group_by(screen_name) %>%
  summarise(mean(favorite_count), median(favorite_count), max(favorite_count))

Aggregates_RTS <- P_Obi_organic %>%
  group_by(screen_name) %>%
  summarise(mean(favorite_count), median(favorite_count), max(favorite_count))

#Tokenise the tweets to get single words

obi.words <- P_Obi_organic %>% 
  select(text) %>% 
  unnest_tokens(word, text)

#Create list of customised words as new_items

new_items <- c("https", "t.co", "amp", "rt", "po")

#Combine stopwords with customised list
stop_words_new <- stop_words %>%
  pull(word) %>%
  append(new_items)

#Filter stopwords from your tokens, count and order
obi.count <- obi.words %>% 
  filter(!word %in% stop_words_new) %>%
  count(word, sort = TRUE) %>% 
  head(15) %>% 
  mutate(word = reorder(word, n))

#Create a plot
ggplot(obi.count, aes(x = word, y = n)) + 
  geom_col() +
  coord_flip() +
  theme_minimal()


#Create tokens as a bigram instead of single words
obi.ngrams <- P_Obi_organic %>%
  select(text) %>%
  unnest_tokens(output = bigram, input = text, token = "ngrams", n = 2, collapse = NULL)

#Remove stopwords and sort bigram
obi.ngrams.count <- obi.ngrams %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% stop_words_new & !word2 %in% stop_words_new) %>% 
  count(word1, word2, sort = TRUE)

#ggplot of top 10 bigrams
obi.ngrams.count %>%
  mutate(bigram = paste(word1, word2)) %>%
  mutate(bigram = reorder(bigram, n)) %>%
  head(10) %>%
  ggplot(aes(x = bigram, y = n)) + 
  geom_col() +
  coord_flip()

#Create a plot of the bigram treemap
obi.ngrams.count %>%
  filter(n > 2) %>% 
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                 arrow = arrow(type = "closed", length = unit(.1, "inches")), 
                 end_cap = circle(.07, "inches")) +
  geom_node_point(color = "skyblue", size = 3) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1, size = 3) +
  theme_void() +
  ggtitle("Bigram of Peter Obi's tweets")

?ggraph
  

timeplot <- P_Obi_organic %>%
  mutate(text = tolower(text)) %>%
  filter(str_detect(text, "nigeria"))

ts_plot(timeplot)+
  #labs(title = "'fake news' tweets per day (from @realDonaldTrump)", x = "Time/day", y = "Number of tweets") +
  theme_minimal()

