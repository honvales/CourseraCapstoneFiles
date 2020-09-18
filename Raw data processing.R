library(tm)
library(RWeka)
library(rJava)
library(slam)
library(ngram)
library(ggplot2)
library(gridExtra)

setwd('~/Desktop/Data Science Stuff/Coursera Data Science/Data Science Specialization/Final Project')

# Use readLines to read files
tweets <- file('en_US.twitter.txt','r')
tweetlines <- readLines(tweets)
close(tweets)

blogs <- file('en_US.blogs.txt','r')
bloglines <- readLines(blogs)
close(blogs)

news <- file('en_US.news.txt','r')
newslines <- readLines(news)
close(news)

# Remove non-ASCII characters (any weird stuff) using iconv
cleantweets <- iconv(tweetlines,'latin1','ASCII',sub='')
cleanblogs <- iconv(bloglines,'latin1','ASCII',sub='')
cleannews <- iconv(newslines,'latin1','ASCII',sub='')

remove('tweetlines','bloglines','newslines')

# Sample 10% of the text (Saves execution time)
set.seed(1989)
tweet_sample <- sample(cleantweets,0.2*round(length(cleantweets)))
blog_sample <- sample(cleanblogs,0.2*round(length(cleanblogs)))
news_sample <- sample(cleannews,0.2*round(length(cleannews)))

remove('cleantweets','cleanblogs','cleannews')

# Create corpus with vectorization using VCorpus
corps <- VCorpus(VectorSource(c(tweet_sample,blog_sample,news_sample)))
remove('tweet_sample','blog_sample','news_sample')

corps <- tm_map(corps,removePunctuation)
corps <- tm_map(corps,removeNumbers)
corps <- tm_map(corps,tolower)
corps <- tm_map(corps, stripWhitespace)
corps1 <- tm_map(corps,PlainTextDocument)
remove('corps')

# Word frequency test
rawdtm <- DocumentTermMatrix(corps1)

freqblogs <- colSums(as.matrix(rawdtm[1,]))
blogswords <- data.frame(word=names(freqblogs), freq=freqblogs) 
freqnews <- colSums(as.matrix(rawdtm[2,]))
newswords <- data.frame(word=names(freqnews), freq=freqnews) 
freqtweets <- colSums(as.matrix(rawdtm[3,]))
tweetwords <- data.frame(word=names(freqtweets), freq=freqtweets) 

remove('freqblogs','freqnews','freqtweets','blogswords','newswords','tweetwords','rawdtm')

remove('rawdtm')

# Tokenize data
onetok <- function(x) NGramTokenizer(x,Weka_control(min = 1, max = 1))
twotok <- function(x) NGramTokenizer(x,Weka_control(min = 2, max = 2))
thrtok <- function(x) NGramTokenizer(x,Weka_control(min = 3, max = 3))
foutok <- function(x) NGramTokenizer(x,Weka_control(min = 4, max = 4))

onegram <- TermDocumentMatrix(corps1,control=list(tokenize=onetok))
twogram <- TermDocumentMatrix(corps1,control=list(tokenize=twotok))
thrgram <- TermDocumentMatrix(corps1,control=list(tokenize=thrtok))
fougram <- TermDocumentMatrix(corps1,control=list(tokenize=foutok))

remove('corps1')

# Process data for usage in application
compone <- rollup(onegram, 2, na.rm=TRUE, FUN = sum)
comptwo <- rollup(twogram, 2, na.rm=TRUE, FUN = sum)
compthr <- rollup(thrgram, 2, na.rm=TRUE, FUN = sum)
compfou <- rollup(fougram, 2, na.rm=TRUE, FUN = sum)

remove('onegram','twogram','thrgram','fougram')

# Prep data for storage and plotting
onegramnum <- sort(rowSums(as.matrix(compone)), decreasing=TRUE)
twogramnum <- sort(rowSums(as.matrix(comptwo)), decreasing=TRUE)
thrgramnum <- sort(rowSums(as.matrix(compthr)), decreasing=TRUE)
fougramnum <- sort(rowSums(as.matrix(compfou)), decreasing=TRUE)

remove('compone','comptwo','compthr','compfou')

onegrams <- data.frame(gram=names(onegramnum), freq=onegramnum)
twograms <- data.frame(gram=names(twogramnum), freq=twogramnum)
thrgrams <- data.frame(gram=names(thrgramnum), freq=thrgramnum)
fougrams <- data.frame(gram=names(fougramnum), freq=fougramnum)

remove('onegramnum','twogramnum','thrgramnum','fougramnum')

subonegrams <- subset(onegrams,freq>4)
subtwograms <- subset(twograms,freq>4)
subthrgrams <- subset(thrgrams,freq>4)
subfougrams <- subset(fougrams,freq>4)

# Save data in .rds files for usage in the app
saveRDS(subonegrams,'onegrams.rds')
saveRDS(subtwograms,'twograms.rds')
saveRDS(subthrgrams,'thrgrams.rds')
saveRDS(subfougrams,'fougrams.rds')

# Plot top 25 n-grams for n=1,2,3
p <- ggplot(onegrams[1:25,], aes(x = reorder(gram, -freq), y = freq)) +
  geom_bar(stat = "identity") + labs(x='Word', y='Frequency',title='Blogs') + 
  theme(axis.text.x=element_text(angle=45, hjust=1))
p1 <- ggplot(twograms[1:25,], aes(x = reorder(gram, -freq), y = freq)) +
  geom_bar(stat = "identity") + labs(x='Word', y='Frequency',title='News') + 
  theme(axis.text.x=element_text(angle=45, hjust=1))
p2 <- ggplot(thrgrams[1:25,], aes(x = reorder(gram, -freq), y = freq)) +
  geom_bar(stat = "identity") + labs(x='Word', y='Frequency',title='Tweets') + 
  theme(axis.text.x=element_text(angle=45, hjust=1))

grid.arrange(p,p1,p2,ncol=1)
