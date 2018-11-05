# Once I have created RData for bigrams, trigrams, etc. I can 
# make my prediction model.

# It is going to be very simple. Just look in my grams if words written 
# are in a construction saved.

suppressPackageStartupMessages(library(dplyr))

# First I must charge all grams I have created previously

pathApp <- file.path(getwd(),"App")
bigramsPath <- file.path(pathApp,"bigrams.RData")
trigramsPath <- file.path(pathApp,"trigrams.RData")
quadgramsPath <- file.path(pathApp,"quadgrams.RData")
#pentagramsPath <- file.path(pathApp,"pentagrams.RData")
#hexagramsPath <- file.path(pathApp,"hexagrams.RData")

bigrams <- readRDS(bigramsPath)
trigrams <- readRDS(trigramsPath)
quadgrams <- readRDS(quadgramsPath)
#pentagrams <- readRDS(pentagramsPath)
#hexagrams <- readRDS(hexagramsPath)


# Here I will define predictors for every one of my grams created


bigramPredictor <- function(input){
        
        words <- input[length(input)]
        out <- filter(.data = bigrams, word_1==words[1])
        if(!is.na(out$word_2[1])){out2 <- as.character(out$word_2)[1:3]} # Taking max 3 most probable predictions 
        if(is.na(out$word_2[1])){out2 <- c(NA,NA,NA)} # if there are no predictions, just NA NA NA
        out <- out2[complete.cases(out2)]
        if(length(out)==0){out <- c("NO","PREDICTION","AVAILABLE")}
        if(length(out)==1){out <- c(out,".","!")}
        if(length(out)==2){out <- c(out,".")}
        out
}

trigramPredictor <- function (input){
        words<- input[c(length(input)-1,length(input))]
        out <- filter(.data = trigrams, word_1==words[1],word_2==words[2])
        if(!is.na(out$word_3[1])){out2 <- as.character(out$word_3)[1:3]} # Taking max 3 most probable predictions 
        if(is.na(out$word_3[1])){out2 <- c(NA,NA,NA)} # if there are no predictions, just NA NA NA
        out <- out2[complete.cases(out2)]
        if(length(out)==0){out <- c("NO","PREDICTION","AVAILABLE")}
        if(length(out)==1){out <- c(out,".","!")}
        if(length(out)==2){out <- c(out,".")}
        out
}

quadgramPredictor <- function (input){
        words<- input[c((length(input)-2):length(input))]
        out <- filter(.data = quadgrams, word_1==words[1],word_2==words[2],word_3==words[3])
        if(!is.na(out$word_4[1])){out2 <- as.character(out$word_4)[1:3]} # Taking max 3 most probable predictions 
        if(is.na(out$word_4[1])){out2 <- c(NA,NA,NA)} # if there are no predictions, just NA NA NA
        out <- out2[complete.cases(out2)]
        if(length(out)==0){out <- c("NO","PREDICTION","AVAILABLE")}
        if(length(out)==1){out <- c(out,".","!")}
        if(length(out)==2){out <- c(out,".")}
        out
}

#pentagramPredictor <- function (input){
#        words<- input[c((length(input)-3):length(input))]
#         out <- filter(.data = pentagrams, word_1==words[1],word_2==words[2],word_3==words[3],word_4==words[4])
#         if(!is.na(out$word_5[1])){out2 <- as.character(out$word_5)[1:3]} # Taking max 3 most probable predictions 
#         if(is.na(out$word_5[1])){out2 <- c(NA,NA,NA)} # if there are no predictions, just NA NA NA
#         out <- out2[complete.cases(out2)]
#         if(length(out)==0){out <- c("NO","PREDICTION","AVAILABLE")}
#         if(length(out)==1){out <- c(out,".","!")}
#         if(length(out)==2){out <- c(out,".")}
#         out
# }
# hexagramPredictor <- function (input){
#         words<- input[c((length(input)-4):length(input))]
#         out <- filter(.data = hexagrams, word_1==words[1],word_2==words[2],word_3==words[3],word_4==words[4],word_5==words[5])
#         if(!is.na(out$word_6[1])){out2 <- as.character(out$word_6)[1:3]} # Taking max 3 most probable predictions
#         if(is.na(out$word_6[1])){out2 <- c(NA,NA,NA)} # if there are no predictions, just NA NA NA
#         out <- out2[complete.cases(out2)]
#         if(length(out)==0){out <- c("NO","PREDICTION","AVAILABLE")}
#         if(length(out)==1){out <- c(out,".","!")}
#         if(length(out)==2){out <- c(out,".")}
#         out
# }

# Here just apply predictors:

output <- "empty"

input <- "The guy in front of me just bought a pound of bacon, a bouquet, and a case of"
input <- strsplit(x = input, split = " ")[[1]]

# if (length(input)>4){output <-hexagramPredictor(input)}
# if (length(input)==4 || output == c("NO","PREDICTION","AVAILABLE")){output<-pentagramPredictor(input)}
if (length(input)>3 || output == c("NO","PREDICTION","AVAILABLE")){output<-quadgramPredictor(input)}
if (length(input)==2 || output == c("NO","PREDICTION","AVAILABLE")){output<-trigramPredictor(input)}
if (length(input)==1 || output == c("NO","PREDICTION","AVAILABLE")){output<-bigramPredictor(input)}
output