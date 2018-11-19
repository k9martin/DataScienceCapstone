
library(shiny)
library(stringr)
library(tm)
library(stylo)
library(dplyr)

bigramsPath <- file.path(getwd(),"bigrams.RData")
trigramsPath <- file.path(getwd(),"trigrams.RData")
quadgramsPath <- file.path(getwd(),"quadgrams.RData")
bigrams <- readRDS(bigramsPath)
trigrams <- readRDS(trigramsPath)
quadgrams <- readRDS(quadgramsPath)


# Define UI for application that draws a histogram
shinyUI(bootstrapPage(
        mainPanel(
                headerPanel("Text Predictor for Coursera Data Science Capstone"),
                h4("This app will predict the next word for the word/phrase given"),
                
                textInput("input", label = h3("Text input"), value = ""), 
                submitButton('Submit'),
                br(''),
                
                h4('Word/phrase you have entered is:'),
                verbatimTextOutput("input"),
                br(''),
                
                h4('The three most probable predicted words are:'),
                #tags$style(type='text/css', '#prediction {background-color: rgba(255,255,0,0.40); color: black;}'),
                verbatimTextOutput("output")
        )
))
