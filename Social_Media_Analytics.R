library(rtweet)
library(ggplot2)
library(dplyr)
library(tidytext)
library(wordcloud)

#Replace Asterics With your acccount details

twitter_token <- create_token(
  app = 'HashtagAnalytics',
  consumer_key = '***',
  consumer_secret = '***',
  access_token = '***',
  access_secret = '***')

rstats_tweets <- search_tweets(q = "#chanel",
                               n = 500)
rstats_user <- search_users(q = "#chanel",
                               n = 500)
rstats_tweets$quoted_location
rstats_user$location

################# LOCATION ##############
length(unique(rstats_user$location))

rstats_user %>%
  ggplot(aes(location)) +
  geom_bar() + coord_flip() +
  labs(x = "Count",
       y = "Location",
       title = "Twitter users - unique locations ")

rstats_user %>%
  count(location, sort = TRUE) %>%
  mutate(location = reorder(location, n)) %>%
  top_n(20) %>%
  ggplot(aes(x = location, y = n)) +
  geom_col() +
  coord_flip() +
  labs(x = "Count",
       y = "Location",
       title = "Where Twitter users are from - unique locations ")

rstats_user %>%
  count(location, sort = TRUE) %>%
  mutate(location = reorder(location,n)) %>%
  na.omit() %>%
  top_n(20) %>%
  ggplot(aes(x = location,y = n)) +
  geom_col() +
  coord_flip() +
  labs(x = "Location",
       y = "Count",
       title = "Twitter users - unique locations ")

############# company tweets

Gates_tweets <- get_timeline("@chanel", n= 2400)
head(Gates_tweets)

################## Timeline ##########

colnames(Gates_tweets)[colnames(Gates_tweets)=="screen_name"] <- "Twitter_Account"
ts_plot(dplyr::group_by(Gates_tweets, Twitter_Account), "year") +
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.title = ggplot2::element_text(face = "bold")) +
  ggplot2::labs(
    x = NULL, y = NULL,
    title = "Frequency of Tweets from Bill Gates",
    subtitle = "Tweet counts aggregated by year",
    caption = "\nSource: Data collected from Twitter's REST API via rtweet"
  )

########### Types of tweets ############

# Remove retweets
Gates_tweets_organic <- Gates_tweets[Gates_tweets$is_retweet==FALSE, ] # Remove replies
Gates_tweets_organic <- subset(Gates_tweets_organic, is.na(Gates_tweets_organic$reply_to_status_id)) 
Gates_tweets_organic <- Gates_tweets_organic %>% arrange(-favorite_count)
Gates_tweets_organic[1,5]
Gates_tweets_organic <- Gates_tweets_organic %>% arrange(-retweet_count)
Gates_tweets_organic[1,5]   

# Keeping only the retweets
Gates_retweets <- Gates_tweets[Gates_tweets$is_retweet==TRUE,]

# Keeping only the replies
Gates_replies <- subset(Gates_tweets, !is.na(Gates_tweets$reply_to_status_id))
nrow(Gates_retweets)
nrow(Gates_tweets_organic)
nrow(Gates_replies)

# Creating a data frame
data <- data.frame(
  category=c("Organic", "Retweets", "Replies"),
  count=c(2893, 23, 28)
)
# Adding columns 
data$fraction = data$count / sum(data$count)
data$percentage = data$count / sum(data$count) * 100
data$ymax = cumsum(data$fraction)
data$ymin = c(0, head(data$ymax, n=-1))

# Specify what the legend should say
Type_of_Tweet <- paste(data$category, data$percentage, "%")
ggplot(data, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=Type_of_Tweet)) +
  geom_rect() +
  coord_polar(theta="y") + 
  xlim(c(2, 4)) +
  theme_void() +
  theme(legend.position = "right")


################## Source of the tweets #############

Gates_app <- Gates_tweets %>% 
  select(source) %>% 
  group_by(source) %>%
  summarize(count=n())

Gates_app <- subset(Gates_app, count > 11)
data <- data.frame(
  category=Gates_app$source,
  count=Gates_app$count
)

data$fraction = data$count / sum(data$count)
data$percentage = data$count / sum(data$count) * 100
data$ymax = cumsum(data$fraction)
data$ymin = c(0, head(data$ymax, n=-1))

Source <- paste(data$category, data$percentage, "%")

ggplot(data, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=Source)) +
  geom_rect() +
  coord_polar(theta="y") + # Try to remove that to understand how the chart is built initially
  xlim(c(2, 4)) +
  theme_void() +
  theme(legend.position = "right")


################# Frequently used hashtags ###########

Gates_tweets_organic$hashtags <- as.character(Gates_tweets_organic$hashtags)
Gates_tweets_organic$hashtags <- gsub("c\\(", "", 
      Gates_tweets_organic$hashtags)
set.seed(1)
wordcloud(Gates_tweets_organic$hashtags, min.freq=5, scale=c(3.5, .5), random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

###################### Frequently used words  in the tweets ############

Gates_tweets_organic$text <-  gsub("https\\S*", "", Gates_tweets_organic$text)
Gates_tweets_organic$text <-  gsub("@\\S*", "", Gates_tweets_organic$text) 
Gates_tweets_organic$text  <-  gsub("amp", "", Gates_tweets_organic$text) 
Gates_tweets_organic$text  <-  gsub("[\r\n]", "", Gates_tweets_organic$text)
Gates_tweets_organic$text  <-  gsub("[[:punct:]]", "", Gates_tweets_organic$text)

tweets <- Gates_tweets_organic %>%
  select(text) %>%
  unnest_tokens(word, text)
tweets <- tweets %>%
  anti_join(stop_words)

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
       title = "Most frequent words found in the tweets of Bill Gates",
       subtitle = "Stop words removed from the list")
#################################################################################

################ SENTIMENT ANALYSIS OF THE TWEETS  

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
  ggtitle("Total sentiment based on scores")+
  theme_minimal()
