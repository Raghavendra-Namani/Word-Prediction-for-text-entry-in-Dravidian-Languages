rm(list=ls(all=T))
libs <- c("knitr","stringi","tm","RWeka")
lapply(libs,require,character.only=TRUE )
library(knitr)
opts_chunk$set(cache=TRUE,warning=FALSE)


freqs<-scan(file=file.choose(), what="char", sep="\n", encoding="UTF-8")


#Media files general statistics:
#Lines
freqsLines <- length(freqs)

#Characters
freqsNchar   <- sum(nchar(freqs,type="chars"))

#words statistics :
sum(stri_count_words(freqs))

#Other statistics
#Summary statistics :
stri_stats_general(freqs)

# preview the sample before cleaning
head(freqs)
tail(freqs)
#text cleaning Prior to corpus Tokenization and for an adequate text mining data need to be cleaned in order to extract significant features.


doc.vec <- VectorSource(freqs)
mycorpus <- Corpus(doc.vec)
summary(mycorpus)
mycorpus <- tm_map(mycorpus, removePunctuation)
mycorpus <- tm_map(mycorpus, removeNumbers)
mycorpus <- tm_map(mycorpus, stripWhitespace)

#Document Tokenization##
#mycorpus <- tm_map(mycorpus, PlainTextDocument)
#preview the sample after cleaning
head(mycorpus)
tail(mycorpus)

#Tokenization : ngram analysis
#Tokenize the corpus into n-grams:
#We need to convert our cleaned mediaSample (cmediaSample) to corpus for
#further exploration like ngrams(1,2,3) frequency distribution using packages tm and RWeka

# use library snowballC
mycorpus <-  tm_map(mycorpus,stemDocument)
corpus.tdm <- TermDocumentMatrix(mycorpus, control = list(wordLengths=c(1,Inf)))
corpus.matrix <- rowSums(as.matrix(corpus.tdm))
corpus.matrix <- apply(t(corpus.matrix), 2, sort)
corpus.df <- as.data.frame(corpus.matrix)
barplot(tail(corpus.df,20),las=2,main="Top 20 descendants",cex.main=1,cex.axis
        =0.75,horiz=TRUE).
