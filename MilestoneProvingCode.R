# As I cannot submit the milestone yet, I will make some proves to the 
# dataset in order to have a good report for when it will be available:


# Libraries I need to use:

#suppressPackageStartupMessages(library(stringi, NLP, openNLP, tm, rJava, RWeka, RWekajars, SnowballC, RColorBrewer, qdap, ggplot2))

library(stringi) # stats files
library(NLP); library(openNLP)
library(tm) # Text mining
library(rJava)
library(RWeka) # tokenizer - create unigrams, bigrams, trigrams
library(RWekajars)
library(SnowballC) # Stemming
library(RColorBrewer) # Color palettes
library(qdap)
library(ggplot2) #visualization


# Let's start charging data as I did on GettingAndCleaningData.R:

url <- "https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip"
fileName <- "Coursera-SwiftKey.zip"
path <- file.path(getwd(),fileName)

if (!file.exists(path)){
        download.file(url,path)
}

folderData <- "final"
pathFolder <- file.path(getwd(),folderData)

if(!file.exists(pathFolder)){
        unzip(path)
}

englishFolder <- file.path(pathFolder, "en_US")
blogsPath <- file.path(englishFolder,"en_US.blogs.txt")
newsPath <- file.path(englishFolder,"en_US.news.txt")
twitterPath <-file.path(englishFolder,"en_US.twitter.txt")

# I will only work on english data.

twitterReaded <- readLines(twitterPath)
blogsReaded <- readLines(blogsPath)
newsReaded <- readLines(newsPath)

# Size of the files:

file.size(blogsPath)/(1024^2)
file.size(newsPath)/(1024^2)
file.size(twitterPath)/(1024^2)

# Number of lines:

length(twitterReaded)
length(blogsReaded)
length(newsReaded)

# Number of words:

sum(stri_count_words(twitterReaded)) 
sum(stri_count_words(blogsReaded)) 
sum(stri_count_words(newsReaded))

# In enunciate it says this model should run in mobile devices, so we have to select 
# a smaller subset:

set.seed(1994)
totalSize <- 15000
sampleTwitter <- sample(twitterReaded, size = totalSize/3, replace = TRUE)
sampleBlogs <- sample(blogsReaded, size = totalSize/3, replace = TRUE)
sampleNews <- sample(newsReaded, size = totalSize/3, replace = TRUE)

smallSubset <- c(sampleTwitter, sampleBlogs, sampleNews)
length(smallSubset)

# And write this new subset on a txt file, just for having it like the others


smallSubsetPath <- file.path(englishFolder,"subsetNotClean.txt")
writeLines(smallSubset, smallSubsetPath)


# Once I have the subset I have to clean it, in order to train the word prediction algorithm with it. 
# For that, I will remove punctuations, emoticons, numbers, URLs, multiple whitespaces, and converting 
# all letters to lowercase:

# For doing it I will use the text mining package (tm):

rvThings <- function(x,pattern){gsub(pattern,"",x)}
rvEmoticons <- function (x) {x <- iconv(x, "UTF-8", "ASCII", sub="")}
#rvURL <- function(x) {gsub("http[[:alnum:]]*","",x)}
#smallSubset <- rvEmoticons(smallSubset)
#smallSubset <- rvURL(smallSubset)
textTM <- Corpus(VectorSource(smallSubset))
textTM <- tm_map(textTM, removePunctuation)                        # remove punctuation
textTM <- tm_map(textTM,rvEmoticons)                               # remove URL
textTM <- tm_map(textTM, removeNumbers)                            # remove numbers
textTM <- tm_map(textTM, rvThings,"(f|ht)tp(s?)://(.*)[.][a-z]+")  # remove URL
textTM <- tm_map(textTM, stripWhitespace)                          # remove multiple whitespace
textTM <- tm_map(textTM, content_transformer(tolower))             # convert to lowercase
textTM <- tm_map(textTM, changetospace, "/|@|\\|")                 # change this separations to space

# proving without stopwords
#textTM2 <- textTM
# textTM2 <- tm_map(textTM2, removeWords, stopwords("en"))             # remove stopwords (of, a, at on...)

# Finally, I will remove profane words. Just tiping in Google "Profane word list" I found one, and I 
# will use this one:

urlProfane<- "http://www.cs.cmu.edu/~biglou/resources/bad-words.txt"
pathProfane <- file.path(englishFolder,"profaneWords.txt")
if(!file.exists(pathProfane)) {download.file(urlProfane,pathProfane)}

profaneReaded <- readLines(pathProfane)
textTM <- tm_map(textTM,removeWords,profaneReaded)
# textTM2 <-tm_map(textTM2,removeWords,profaneReaded)

textTM <- tm_map(textTM,PlainTextDocument)
# textTM2 <- tm_map(textTM2,PlainTextDocument)


# finalSubsetPath <- file.path(englishFolder,"SubsetClean.RData")
# saveRDS(textTM, file = finalSubsetPath)



# Now I can try to find bigrams and trigrams (and even quadgrams) using RWeka library

bigrams <- NGramTokenizer(textTM, Weka_control(min=2,max=2))
trigrams <- NGramTokenizer(textTM, Weka_control(min=3,max=3))
quadgrams <- NGramTokenizer(textTM, Weka_control(min=4,max=4))
pentagrams <- NGramTokenizer(textTM, Weka_control(min=5,max=5))
hexagrams <- NGramTokenizer(textTM, Weka_control(min=6,max=6))

bigrams <- data.frame(table(bigrams))
trigrams <- data.frame(table(trigrams))
quadgrams <- data.frame(table(quadgrams))
pentagrams <- data.frame(table(pentagrams))
hexagrams <- data.frame(table(hexagrams))

bigrams <- bigrams[order(bigrams$Freq,decreasing = TRUE),]
trigrams <- trigrams[order(trigrams$Freq,decreasing = TRUE),]
quadgrams <- quadgrams[order(quadgrams$Freq,decreasing = TRUE),]
pentagrams <- pentagrams[order(pentagrams$Freq,decreasing = TRUE),]
hexagrams <- hexagrams[order(hexagrams$Freq,decreasing = TRUE),]

names(bigrams) <- c("words","freq")
names(trigrams) <- c("words","freq")
names(quadgrams) <- c("words","freq")
names(pentagrams) <- c("words","freq")
names(hexagrams) <- c("words","freq")

bigrams$words <- as.character(bigrams$words)
trigrams$words <- as.character(trigrams$words)
quadgrams$words <- as.character(quadgrams$words)
pentagrams$words <- as.character(pentagrams$words)
hexagrams$words <- as.character(hexagrams$words)

wordsBigrams <- strsplit(bigrams$words,split=" ")
bigrams <- transform(bigrams,
                      word_1 = sapply(wordsBigrams,"[[",1),
                      word_2 = sapply(wordsBigrams,"[[",2))
wordsTrigrams <- strsplit(trigrams$words,split=" ")
trigrams <- transform(trigrams,
                     word_1 = sapply(wordsTrigrams,"[[",1),
                     word_2 = sapply(wordsTrigrams,"[[",2),
                     word_3 = sapply(wordsTrigrams,"[[",3))
wordsQuadgrams <- strsplit(quadgrams$words,split=" ")
quadgrams <- transform(quadgrams,
                     word_1 = sapply(wordsQuadgrams,"[[",1),
                     word_2 = sapply(wordsQuadgrams,"[[",2),
                     word_3 = sapply(wordsQuadgrams,"[[",3),
                     word_4 = sapply(wordsQuadgrams,"[[",4))
wordsPentagrams <- strsplit(pentagrams$words,split=" ")
pentagrams <- transform(pentagrams,
                     word_1 = sapply(wordsPentagrams,"[[",1),
                     word_2 = sapply(wordsPentagrams,"[[",2),
                     word_3 = sapply(wordsPentagrams,"[[",3),
                     word_4 = sapply(wordsPentagrams,"[[",4),
                     word_5 = sapply(wordsPentagrams,"[[",5))
wordsHexagrams <- strsplit(hexagrams$words,split=" ")
hexagrams <- transform(hexagrams,
                     word_1 = sapply(wordsHexagrams,"[[",1),
                     word_2 = sapply(wordsHexagrams,"[[",2),
                     word_3 = sapply(wordsHexagrams,"[[",3),
                     word_4 = sapply(wordsHexagrams,"[[",4),
                     word_5 = sapply(wordsHexagrams,"[[",5),
                     word_6 = sapply(wordsHexagrams,"[[",6))

bigrams2 <- data.frame(word_1 = bigrams$word_1,
                       word_2 = bigrams$word_2,
                       freq = bigrams$freq, stringsAsFactors=FALSE)
trigrams2 <- data.frame(word_1 = trigrams$word_1,
                         word_2 = trigrams$word_2, 
                         word_3 = trigrams$word_3,
                         freq = trigrams$freq, stringsAsFactors=FALSE)
quadgrams2 <- data.frame(word_1 = quadgrams$word_1,
                         word_2 = quadgrams$word_2, 
                         word_3 = quadgrams$word_3, 
                         word_4 = quadgrams$word_4,
                         freq = quadgrams$freq, stringsAsFactors=FALSE)
pentagrams2 <- data.frame(word_1 = pentagrams$word_1,
                         word_2 = pentagrams$word_2, 
                         word_3 = pentagrams$word_3, 
                         word_4 = pentagrams$word_4,
                         word_5 = pentagrams$word_5,
                         freq = pentagrams$freq, stringsAsFactors=FALSE)
hexagrams2 <- data.frame(word_1 = hexagrams$word_1,
                         word_2 = hexagrams$word_2, 
                         word_3 = hexagrams$word_3, 
                         word_4 = hexagrams$word_4,
                         word_5 = hexagrams$word_5,
                         word_6 = hexagrams$word_6,
                         freq = hexagrams$freq, stringsAsFactors=FALSE)


#names(bigrams) <- c("word_1","word_2", "freq")
#names(trigrams) <- c("word_1","word_2","word_3", "freq")
#names(quadgrams) <- c("word_1","word_2","word_3","word_4", "freq")
#names(pentagrams) <- c("word_1","word_2","word_3","word_4","word_5", "freq")
#names(hexagrams) <- c("word_1","word_2","word_3","word_4","word_5","word_6", "freq")

pathApp <- file.path(getwd(),"App")
if(!dir.exists(pathApp)){dir.create(pathApp)}

bigramsPath <- file.path(pathApp,"bigrams.RData")
saveRDS(bigrams2, file = bigramsPath)
trigramsPath <- file.path(pathApp,"trigrams.RData")
saveRDS(trigrams2, file = trigramsPath)
quadgramsPath <- file.path(pathApp,"quadgrams.RData")
saveRDS(quadgrams2, file = quadgramsPath)
pentagramsPath <- file.path(pathApp,"pentagrams.RData")
saveRDS(pentagrams2, file = pentagramsPath)
hexagramsPath <- file.path(pathApp,"hexagrams.RData")
saveRDS(hexagrams2, file = hexagramsPath)

# prove with no stopwords

#bigrams2 <- NGramTokenizer(textTM2, Weka_control(min=2,max=2))
#trigrams2 <- NGramTokenizer(textTM2, Weka_control(min=3,max=3))
#quadgrams2 <- NGramTokenizer(textTM2, Weka_control(min=4,max=4))

#bigrams2 <- data.frame(table(bigrams2))
#trigrams2 <- data.frame(table(trigrams2))
#quadgrams2 <- data.frame(table(quadgrams2))

#bigrams2 <- bigrams2[order(bigrams2$Freq,decreasing = TRUE),]
#trigrams2 <- trigrams2[order(trigrams2$Freq,decreasing = TRUE),]
#quadgrams2 <- quadgrams2[order(quadgrams2$Freq,decreasing = TRUE),]

#names(bigrams2) <- c("word_2", "freq")
#names(trigrams2) <- c("word_3", "freq")
#names(quadgrams2) <- c("word_4", "freq")


# And this is exploratory analysis done.