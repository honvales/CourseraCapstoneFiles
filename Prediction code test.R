library(tm)
library(ngram)
library(RWeka)
library(rJava)
library(slam)
library(ggplot2)
library(gridExtra)

setwd('~/Desktop/Data Science Stuff/Coursera Data Science/Data Science Specialization/Final Project')

# This function turns the text input into a corpus that will be used for prediction
phrase <- c('Running around, I like my big... underwear')
clean_input <- function(phr){
  corps <- VCorpus(VectorSource(phr))
  corps <- tm_map(corps,removePunctuation)
  corps <- tm_map(corps,removeNumbers)
  corps <- tm_map(corps,tolower)
  corps <- tm_map(corps, stripWhitespace)
  corps1 <- tm_map(corps,PlainTextDocument)
  cleanp <- corps1$content[[1]]$content
  tokens <- strsplit(cleanp,' ')[[1]]
  return(tokens)
}

tokens <- clean_input(phrase)
second <- tokens[length(tokens) - 1]
first <- tokens[length(tokens)]

# Read
onegrams <- readRDS('onegrams.rds')[1:50,'gram']
twograms <- readRDS('twograms.rds')[,'gram']
thrgrams <- readRDS('thrgrams.rds')[,'gram']

# Prediction functions
trigram_finder <- function(word1,word2,thrgram_data){
  last2 <- paste('^','(',paste(word2,word1,sep = ' '),')',sep='')
  match <- grep(last2,thrgram_data,value=TRUE)[1]
  vals <- strsplit(match,' ')[[1]][3]
  if (is.na(vals)){
    vals1 <- twogram_finder(word1,twograms)
    return(vals1)
  }
  return(vals)
}

twogram_finder <- function(word1,twogram_data){
  last1 <- paste('^',word1,sep='')
  match <- grep(last1,twogram_data,value=TRUE)[1]
  vals <- strsplit(match,' ')[[1]][2]
  if (is.na(vals)){
    vals1 <- onegram_finder(onegrams)
    return(vals1)
  }
  return(vals)
}

onegram_finder <- function(onegram_data){
  return(sample(onegrams[,gram],size=1))
}

trigram_finder(first,second,thrgrams)

