library (rtweet)
library(tidyverse)
library(RColorBrewer)

twitter_token <- create_token
(
app = 
consumer_key = 
consumer_secret = 
access_token = 
access_secret = 

set_renv = TRUE)

P_Obi <- get_timeline("@PeterObi", n=3000)

#Removing retweets
P_Obi_organic <- P_Obi[P_Obi$retweeted==FALSE,]

#Removing replies
P_Obi_organic <- subset(P_Obi_organic, is.na(P_Obi_organic$in_reply_to_status_id))

#arranging tweets in descending order of likes and retweets

P_Obi_organic <- P_Obi_organic %>% arrange(-retweet_count)
P_Obi_organic[1,5]

P_Obi_organic <- P_Obi_organic %>% arrange(-favorite_count)
P_Obi_organic[1,5]

P_Obi.df <- data.frame(P_Obi_organic)
write.csv(P_Obi.df, "C:/Users/dell/Desktop/P_Obi.csv")
?write_csv
getwd()

# Exporting file as csv. First coerce the data.frame to all-character
P_Obi.df = data.frame(lapply(P_Obi.df, as.character), stringsAsFactors=FALSE)

# write file
write.csv(P_Obi.df,"tx.csv")

# Keeping only the retweets
P_Obi_retweets <- P_Obi[P_Obi$retweeted==TRUE,]
# Keeping only the replies
P_Obi_replies <- subset(P_Obi, !is.na(P_Obi$in_reply_to_status_id))

###########################################################################


####most frequent words


P_Obi_organic$text <-  gsub("https\\S*", "", P_Obi_organic$text)
P_Obi_organic$text <-  gsub("@\\S*", "", P_Obi_organic$text) 
P_Obi_organic$text  <-  gsub("amp", "", P_Obi_organic$text) 
P_Obi_organic$text  <-  gsub("[\r\n]", "", P_Obi_organic$text)
P_Obi_organic$text  <-  gsub("[[:punct:]]", "", P_Obi_organic$text)

library(tidytext)

tweets <- P_Obi_organic %>%
  select(text) %>%
  unnest_tokens(word, text)
tweets <- tweets %>%
  anti_join(stop_words)

mystopwords <- c("po","rt","nigerian")
mystopwords <-data.frame(mystopwords)
colnames(mystopwords)<-c("word")
tweets <- tweets %>%
  anti_join(mystopwords)

tweets %>% # gives you a bar chart of the most frequent words found in the tweets
  count(word, sort = TRUE) %>%
  top_n(15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(y = "Count",
       x = "Unique words",
       title = "Most frequent words found in Peter Obi's tweets",
       subtitle = "Stop words removed from the list")

library(syuzhet)
# Converting tweets to ASCII to trackle strange characters
tweets <- iconv(tweets, from="UTF-8", to="ASCII", sub="")
# removing retweets, in case needed 
tweets <-gsub("(RT|via)((?:\\b\\w*@\\w+)+)","",tweets)
# removing mentions, in case needed
tweets <-gsub("@\\w+","",tweets)
ew_sentiment<-get_nrc_sentiment((tweets))
sentimentscores<-data.frame(colSums(ew_sentiment[,]))
names(sentimentscores) <- "Score"
sentimentscores <- cbind("sentiment"=rownames(sentimentscores),sentimentscores)
rownames(sentimentscores) <- NULL
ggplot(data=sentimentscores,aes(x=sentiment,y=Score))+
  geom_bar(aes(fill=sentiment),stat = "identity")+
  theme(legend.position="none")+
  xlab("Sentiments")+ylab("Scores")+
  ggtitle("Peter Obi's tweet sentiments based on scores")+
  theme_minimal()

library(wordcloud)

P_Obi_organic$hashtags <- as.character(P_Obi_organic$entities)
P_Obi_organic$entities <- gsub("c\\(", "", P_Obi_organic$entities)
set.seed(1234)
wordcloud(P_Obi_organic$entities, min.freq=5, scale=c(3.5, .5), random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

