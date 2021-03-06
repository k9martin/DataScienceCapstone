
# Libraries

library(shiny)
library(stringr)
library(tm)
library(stylo)
library(dplyr)

# Functions:

# Loading bigram, trigram and quadgram
# pathApp <- file.path(getwd(),"App")
# bigramsPath <- file.path(pathApp,"bigrams.RData")
# trigramsPath <- file.path(pathApp,"trigrams.RData")
# quadgramsPath <- file.path(pathApp,"quadgrams.RData")
bigramsPath <- file.path(getwd(),"bigrams.RData")
trigramsPath <- file.path(getwd(),"trigrams.RData")
quadgramsPath <- file.path(getwd(),"quadgrams.RData")
bigrams <- readRDS(bigramsPath)
trigrams <- readRDS(trigramsPath)
quadgrams <- readRDS(quadgramsPath)

# # Cleaning input
# 
# CleaningInput <- function(input){
#         output <- tolower(input)
#         output <- removePunctuation(output)
#         output <- removeNumbers(output)
#         output <- stripWhitespace(output)
#         output <- txt.to.words.ext(output, language = "English.all", preserve.case = TRUE)
#         return(output)
# }
# 
# 
# # Predictor functions:
# bigramPredictor <- function(input){
#         
#         words <- input[length(input)]
#         out <- filter(.data = bigrams, word_1==words[1])
#         if(!is.na(out$word_2[1])){out2 <- as.character(out$word_2)[1:3]} # Taking max 3 most probable predictions 
#         if(is.na(out$word_2[1])){out2 <- c(NA,NA,NA)} # if there are no predictions, just NA NA NA
#         out <- out2[complete.cases(out2)]
#         if(length(out)==0){out <- c("NO","PREDICTION","AVAILABLE")}
#         if(length(out)==1){out <- c(out,".","!")}
#         if(length(out)==2){out <- c(out,".")}
#         out
# }
# 
# trigramPredictor <- function (input){
#         words<- input[c(length(input)-1,length(input))]
#         out <- filter(.data = trigrams, word_1==words[1],word_2==words[2])
#         if(!is.na(out$word_3[1])){out2 <- as.character(out$word_3)[1:3]} # Taking max 3 most probable predictions 
#         if(is.na(out$word_3[1])){out2 <- c(NA,NA,NA)} # if there are no predictions, just NA NA NA
#         out <- out2[complete.cases(out2)]
#         if(length(out)==0){out <- c("NO","PREDICTION","AVAILABLE")}
#         if(length(out)==1){out <- c(out,".","!")}
#         if(length(out)==2){out <- c(out,".")}
#         out
# }
# 
# quadgramPredictor <- function (input){
#         words<- input[c((length(input)-2):length(input))]
#         out <- filter(.data = quadgrams, word_1==words[1],word_2==words[2],word_3==words[3])
#         if(!is.na(out$word_4[1])){out2 <- as.character(out$word_4)[1:3]} # Taking max 3 most probable predictions 
#         if(is.na(out$word_4[1])){out2 <- c(NA,NA,NA)} # if there are no predictions, just NA NA NA
#         out <- out2[complete.cases(out2)]
#         if(length(out)==0){out <- c("NO","PREDICTION","AVAILABLE")}
#         if(length(out)==1){out <- c(out,".","!")}
#         if(length(out)==2){out <- c(out,".")}
#         out
# }
# 
# Predicted <- function(input){
#         inputCleaned <- CleaningInput(input)
#         if (length(inputCleaned)>3 || output == c("NO","PREDICTION","AVAILABLE")){output<-quadgramPredictor(input)}
#         if (length(inputCleaned)==2 || output == c("NO","PREDICTION","AVAILABLE")){output<-trigramPredictor(input)}
#         if (length(inputCleaned)==1 || output == c("NO","PREDICTION","AVAILABLE")){output<-bigramPredictor(input)}
#         output <- paste(output,collapse=", ")
#         output
#         
# }

functionsPath <- file.path(getwd(),"functions.R")
source(functionsPath)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

        wordPredicted <- reactive(
                wordPredicted <- Predicted(input$input,bigrams,trigrams,quadgrams)
        )
        output$output <- renderPrint({Predicted(input$input,bigrams,trigrams,quadgrams)})
        output$input <- renderPrint(input$input)
        
        })
