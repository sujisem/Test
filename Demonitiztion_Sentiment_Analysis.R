library(twitteR)
library(ROAuth)
library(devtools)
library(dplyr)
library(streamR)
library(stringr)
library(plyr)
consumer_key <- "ouSniK4pA583Inz4etZhl94m4"
consumer_secret <- "IRUAJpd1BxTlYemUywfYCI3kKouyeiIHxvIy5B6wmndMplvEXw"
access_token <- "1004417510-3regAcMwQMvJV4fRzZvQRbkRqFJtCExDmjTVRMl"
access_secret <- "Kkr0VvkoiZdIfeYE04Qi4vRfO5NKmuHuXx8etCoP26wCz"

TwitterAuth<-setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)


#SearchTwitter - Searches for the provided string on Twitter
search.twitter<-searchTwitter("#Demonitization",lang="en",n=1000)
tweets1.df<-twListToDF(search.twitter)
tweets.df<-tweets1.df
View(tweets.df)
tweets.txt<- lapply(search.twitter, function(t)t$getText())
class(tweets.txt)

head(tweets.txt)
tweets.df<-data.frame(tweets.df$text,tweets.df$created)
tweets.df <-data.frame(tweets.df$text)
View(tweets.df)
colnames(tweets.df)[1]<-"text"

tweets.df <- sapply(tweets.df$text, function(x) iconv(x, to='UTF-8', sub='byte'))
#post1.df <- sapply(post.df$comments.message, function(x) iconv(x, to='UTF-8', sub='byte'))

tweets.df<-gsub("@\\w+", "", tweets.df)
tweets.df<-gsub("#\\w+", '', tweets.df)
tweets.df<-gsub("RT\\w+", "", tweets.df)
tweets.df<-gsub("http.*", "", tweets.df)
tweets.df<-gsub("RT", "", tweets.df)
pos<-readLines("positive-words.txt")
neg<-readLines("negative-words.txt")
score.sentiment<-function(sentences, pos.words, neg.words, .progress='none')
{
  # Parameters
  # sentences: vector of text to score
  # pos.words: vector of words of postive sentiment
  # neg.words: vector of words of negative sentiment
  # .progress: passed to laply() to control of progress bar
  
  # create simple array of scores with laply
  scores<-laply(sentences,
                function(sentence, pos.words, neg.words)
                {
                  # remove punctuation
                  sentence<-gsub("[[:punct:]]", "", sentence)
                  # remove control characters
                  sentence<-gsub("[[:cntrl:]]", "", sentence)
                  # remove digits?
                  sentence<-gsub('\\d+', '', sentence)
                  
                  #convert to lower
                  sentence<-tolower(sentence)
                  
                  
                  # split sentence into words with str_split (stringr package)
                  word.list<- str_split(sentence, "\\s+")
                  words<- unlist(word.list)
                  
                  # compare words to the dictionaries of positive & negative terms
                  pos.matches<-match(words, pos)
                  neg.matches<- match(words, neg)
                  
                  # get the position of the matched term or NA
                  # we just want a TRUE/FALSE
                  pos.matches<- !is.na(pos.matches)
                  neg.matches<- !is.na(neg.matches)
                  
                  # final score
                  score<- sum(pos.matches) - sum(neg.matches)
                  return(score)
                }, pos.words, neg.words, .progress=.progress )
  # data frame with scores for each sentence
  scores.df<- data.frame(text=sentences, score=scores)
  return(scores.df)
}

scores_twitter<-score.sentiment(tweets.df, pos, neg, .progress='text')
cbind(tweets1.df,scores_twitter)
sent.trend<-cbind(tweets1.df[,c("text","created")],scores_twitter)
View(sent.trend)
plot(sent.trend$created,sent.trend$score,type ="l" )
hist(sent_trend$score)

install.packages("wordcloud",dependencies = TRUE)
install.packages("stringr",dependencies = TRUE)
install.packages("tm")
write.csv(tweets.df, file = 'C:/users/user/documents/tweets.txt')
setwd("c:/users/user/documents")
file <- 'tweets.txt'
text <- file(file,open="r")
text.decomposition = readLines(text)
text.decomposition[2]
library(tm)
corpus <- Corpus(VectorSource(text.decomposition))
inspect(corpus[2])
corpus1 <- tm_map(corpus, PlainTextDocument)
corpus2 <- tm_map(corpus1, tolower)
corpus3 <- tm_map(corpus2, removeNumbers)
corpus4 <- tm_map(corpus3, removePunctuation)
stopwords("english")
corpus5 <- tm_map(corpus4,removeWords, c(stopwords("english")))
corpus7 <- tm_map(corpus6, removeWords, c("Demonitization","Modi","modi","twitter","india","dailyo","modis","bhakt","mannkibaat","https","women","got","new","still"))
corpus6 <- tm_map(corpus5, stripWhitespace)
corpus <- tm_map(corpus, stemDocument)
writeLines(as.character(corpus[[2]]))
data_dtm<- DocumentTermMatrix(corpus7)
inspect(data_dtm)
frequent<- findFreqTerms(data_dtm,lowfreq = 100,highfreq = Inf)
frequent
wordcloud::wordcloud(corpus6,random.order = F,max.words = 50,scale=c(3, 0.5), colors = rainbow(50))

data_dtm1 <- DocumentTermMatrix(corpus7)
inspect(data_dtm1)
frequent <- findFreqTerms(data_dtm1,lowfreq = 100,highfreq = Inf)
frequent
wordcloud::wordcloud(corpus, max.words = 100, random.order = FALSE,scale = c(5,0.1),colors = brewer.pal(8,"Dark2"))
x <- as.character(corpus7)
str(corpus)
wordcloud::wordcloud(corpus7[1:40],max.words =100,scale=c(2,0.8),colors = brewer.pal(8,"Dark2"))
wordcloud::wordcloud(words = corpus7, scale = c(5,0.1), max.words =100,random.order = FALSE,rot.per = 0.35,use.r.layout = FALSE,colors = brewer.pal(8,"Dark2"))
