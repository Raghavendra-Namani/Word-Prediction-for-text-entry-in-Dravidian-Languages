rm(list=ls(all=T))
libs <- c("knitr","stringi","tm","RWeka")
lapply(libs,require,character.only=TRUE )
library(knitr)
opts_chunk$set(cache=TRUE,warning=FALSE)

freqs1<-scan(file=file.choose(), what="char", sep="\n", encoding="UTF-8")

#Media files general statistics:
#Lines
freqs1Lines <- length(freqs1)


#Characters
freqs1Nchar   <- sum(nchar(freqs1,type="chars"))

#words statistics :
sum(stri_count_words(freqs1))

#Other statistics
#Summary statistics :
stri_stats_general(freqs1)

#Due to the dataset size and the processing time required,and my pc limtations i will only sample the freqs2 documents assuming that the sample size allowed by my pc is representative of the freqs2 raw data.
#The exloratory analysis woud be similar for the other medias.
set.seed(100)
sample.size=300
mediaSample  <- freqs1[sample(c(1:length(freqs1)),sample.size,replace=FALSE)]
# preview the sample before cleaning
head(freqs1)
tail(freqs1)
#text cleaningPrior to corpus Tokenization and for an adequate text mining data need to be cleaned in order to extract significant features.
#The cleaning function cleans as follow:
#remove no alphanumeric
#remove stop word
#remove URLs
#remove punctuation
#remove no english words
#remove numbers
#strip white spaces
#Not all of them are necessary i will get back to this in next weeks.

doc.vec <- VectorSource(freqs1)
mycorpus <- Corpus(doc.vec)
summary(mycorpus)
mycorpus <- tm_map(mycorpus, removePunctuation)
mycorpus <- tm_map(mycorpus, removeNumbers)
#mycorpus <- tm_map(mycorpus, content_transformer(tolower))
mycorpus <- tm_map(mycorpus, stripWhitespace)
#mycorpus  <- Corpus(VectorSource(mycorpus )) 
#Document Tokenization#
#mycorpus <- tm_map(mycorpus, PlainTextDocument)
#preview the sample after cleaning
head(mycorpus)
tail(mycorpus)

#Tokenization : ngram analysis
#Tokenize the corpus into n-grams:
#We need to convert our cleaned mediaSample (cmediaSample) to corpus for
#further exploration like ngrams(1,2,3) frequency distribution using packages tm and RWeka
#  library
#mycorpus <-  tm_map(mycorpus,stemDocument)

#3 ngrams tokenization functions for unigrams,bigrams and trigrams
#Set the default number of threds to use, it is needed for ngram function.
options(mc.cores=1)
uniTokenizer <- function(t) NGramTokenizer(t,Weka_control(min=1,max=1))
biTokenizer  <- function(t) NGramTokenizer(t,Weka_control(min=2,max=2))
triTokenizer <- function(t) NGramTokenizer(t,Weka_control(min=3,max=3))
quadTokenizer <- function(t) NGramTokenizer(t,Weka_control(min=4,max=4))

#Document TERM Matrix
#Create Document Term Matrix and calculate frequencies using package tm and plot the 20 most frequent ngrams.
#Top 20 most frequent unigrams
dtm <- DocumentTermMatrix(mycorpus, control=list(tokenize=uniTokenizer))
unifreq <- colSums(as.matrix(dtm))
barplot(tail(sort(unifreq),20),las=2,main="Top 20 unigrams",cex.main=1,cex.axis =0.75,horiz=TRUE)

#Top 20 most frequent bigrams
dtm <- DocumentTermMatrix(mycorpus,control=list(tokenize=biTokenizer))
bifreq <- colSums(as.matrix(dtm))
barplot(tail(sort(bifreq),20),las=2,main="Top 20 bigrams",cex.main=1,cex.axis
        =0.75,horiz=TRUE)

#Top 20 most frequent trigrams
dtm <- DocumentTermMatrix(mycorpus,control=list(tokenize=triTokenizer))
trifreq <- colSums(as.matrix(dtm))
barplot(tail(sort(trifreq),20),las=2,main="Top 20 trigrams",cex.main=1,cex.axis
        =0.75,horiz=TRUE)

#Top 20 most frequent quadrigrams
dtm <- DocumentTermMatrix(mycorpus,control=list(tokenize=triTokenizer))
quadfreq <- colSums(as.matrix(dtm))
barplot(tail(sort(quadfreq),20),las=2,main="Top 20 quadrigrams",cex.main=1,cex.axis=0.75,horiz=TRUE)

#Word Cloud 
library(wordcloud)
freq.onegram <- sort(rowSums(as.matrix(unifreq)), decreasing = FALSE)
wordcloud(names(freq.onegram), freq.onegram, max.words = 300, colors = brewer.pal(6, "Dark2"))

