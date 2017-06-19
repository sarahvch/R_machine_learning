#Natural Language Processing
#install.packages('tm')
#install.packages('twitteR')
#install.packages('wordcloud')
#install.packages('RColorBrewer')
#install.packages('e1071')
#install.packages('class')
library(tm)
library(twitteR)
library(wordcloud)
library(RColorBrewer)
library(e1071)
library(class)
#twitter site with keys

#set varibles
ckey <- '#secret'
skey <- '#secret'
token <- '#secret'
sectoken <-'#secret'

#connect to twitter, set up authorization
#need to pass in consumer key, consumer secret key, access token, secret token
setup_twitter_oauth(ckey, skey, token, sectoken)

#search for soccer tweets, n = number of tweets, lang is language
#returing tweets
soccer.tweets <-searchTwitter('aerial dance', n=1000, lang = 'en')

#grabbing text data from tweets
soccer.text <-sapply(soccer.tweets, function(x) x$getText())

#clean text data, removes emoticons, and create corpus
#icon encoding function, removes characters not in utfa
soccer.text <-iconv(soccer.text, 'UTF-8', 'ASCII')

#create soccor corpus
soccor.corpus <- Corpus(VectorSource(soccer.text))

#document term matrix, pass in corpus, ctrl list of actions to do on corpus
#stop words, words you want removed, if we have soccer, will return a lot of words
#can pass stop words in as a function and put in 'english' for common english words
#remove numbers tolower
#uses tm library
term.doc.matrix <- TermDocumentMatrix(soccor.corpus, control = list(removePunctuation = TRUE, stopwords = c('aerial dance', 'https', 'aerial', stopwords('english')), removeNumbers=TRUE, tolower=TRUE))

#change term.doc.matrix to a matrix
term.doc.matrix <-as.matrix(term.doc.matrix)

#get word count
word.freq <-sort(rowSums(term.doc.matrix), decreasing = TRUE)

#creating a data frame with all the words and counts
dm <- data.frame(word=names(word.freq), freq = word.freq)

#create word cloud
#brewer color palette Dark2
wordcloud(dm$word, dm$freq, random.order = FALSE, color= brewer.pal(8, 'Dark2'))

