#Importing all libraries
library(rtweet)
library(ggplot2)
library(sentimentr)
library(syuzhet)
library(wordcloud)
library(wordcloud2)
library(tm)
library(dplyr)

#Collection
trump_tweets <- search_tweets(q = "trump", n = 10000,
                               lang = "en",
                               include_rts = FALSE)
#lower case
trump_tweets$clean_text<-tolower(trump_tweets$text)

#preprocessing
trump_tweets$clean_text<-gsub("@\\w+", "",trump_tweets$clean_text)
trump_tweets$clean_text<-gsub("[[:punct:]]", "",trump_tweets$clean_text)
trump_tweets$clean_text<-gsub("http\\w+", "",trump_tweets$clean_text)
trump_tweets$clean_text<-gsub("[ |\t]{2,}", "",trump_tweets$clean_text)
trump_tweets$clean_text<-gsub("^ ", "",trump_tweets$clean_text)
trump_tweets$clean_text<-gsub(" $", "",trump_tweets$clean_text)
trump_tweets$clean_text<-gsub('[^\x20-\x7E]', '',trump_tweets$clean_text)
trump_tweets$clean_text<-gsub("trump*", "",trump_tweets$clean_text)
trump_tweets$clean_text<-gsub("amp+", "",trump_tweets$clean_text)

#Corpus creation
corp_trump<-Corpus(VectorSource(trump_tweets$clean_text))
corp_trump <- tm_map(corp_trump, function(x) removeWords(x, stopwords()))

#the stemming of document is not done as it messes the words in the word cloud
#corp_trump <- tm_map(corp_trump, stemDocument, language = "english")

#wordcloud
wordcloud(corp_trump, min.freq = 20, colors = brewer.pal(8, "Dark2"), random.color = TRUE, max.words = 300)

#wordcloud2
dtm <- TermDocumentMatrix(corp_trump) 
matrix <- as.matrix(dtm) 
words <- sort(rowSums(matrix),decreasing=TRUE) 
df_trump <- data.frame(word = names(words),freq=words)
wordcloud2(data=df_trump, size=1.6, color='random-dark')

#getting nrc sentiment scores
sentiment_trump<-get_nrc_sentiment(trump_tweets$clean_text)
Sentimentscores_trump<-data.frame(colSums(sentiment_trump[,]))
names(Sentimentscores_trump)<-"Score"
Sentimentscores_trump<-cbind("sentiment"=rownames(Sentimentscores_trump),Sentimentscores_trump)
rownames(Sentimentscores_trump)<-NULL

#Bar plot of the count of different sentiments
ggplot(data=Sentimentscores_trump,aes(x=sentiment,y=Score))+geom_bar(stat = "identity")+
  theme(legend.position="none")+
  xlab("Sentiments")+ylab("scores")+ggtitle("Sentiments of people for trump")

#library(waffle)
#library(ggthemes)


#vals <- Sentimentscores_trump$Score
#val_names <- sprintf("%s (%s)", Sentimentscores_trump$sentiment, scales::percent(round(vals/sum(vals), 2)))
#names(vals) <- val_names

#waffle(vals) +
#  scale_fill_tableau()

#Calculating percentage of the sentiments
Sentimentscores_trump <- Sentimentscores_trump %>%
  mutate(percent=Score/sum(Score)*100.0) %>%
  arrange(desc(Score))

#Pie chart with user-defined colors
ggplot(Sentimentscores_trump, aes("", percent, fill = sentiment)) +
  geom_bar(width = 1, size = 1, color = "white", stat = "identity") +
  coord_polar("y") +
  geom_text(aes(label = paste0(round(percent), "%")), 
            position = position_stack(vjust = 0.5)) +
  labs(title = "Pie Chart for displaying percentages of sentiment") +
  guides(fill = guide_legend(reverse = TRUE)) +
  scale_fill_manual(values = c("#cc3300", "#ff471a", "#ffff00", "#00b300","#d147a3", "#bcbcbc", "#66ff33", "#00ccff","#ff99ff", "#8080ff")) +
  theme_classic() +
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0.5))

#Calculating sentence level sentiments
trump_sent_values<-sentiment_by(trump_tweets$clean_text)

#plotting line graph for the sentiments calculated
ggplot(data = trump_sent_values, aes(x = element_id, y = ave_sentiment))+geom_line()+geom_smooth(se=TRUE)+labs(title = "Line graph and average showing the sentiments for all the tweets.")

#plotting a small part of the line graph to make observations more clear
ggplot(data = trump_sent_values[7395:7900], aes(x = element_id, y = ave_sentiment))+geom_line()+geom_smooth()+labs(title = "Line graph and average showing the sentiments for a small part of the tweets.")

#Plotting histogram
ggplot(data = trump_sent_values, aes(x = ave_sentiment))+geom_histogram( bins = 40)+labs(title = "Histogram for the calculated sentiments of the tweets")

#Plotting frequency plot
ggplot(data = trump_sent_values, aes(x = ave_sentiment))+geom_freqpoly( bins = 40)+labs(title = "Frequency polygon for the calculated sentiments of the tweets")

#Combining both the histogram and frequency plot
ggplot(data = trump_sent_values, aes(x = ave_sentiment))+geom_histogram( bins = 40)+geom_freqpoly( bins = 40)+labs(title = "Histogram and frequency polygon for the calculated sentiments of the tweets")
