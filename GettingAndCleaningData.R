##      Week 1 of the Capstone: Getting and Cleaning Data

# This is the first scrip for the Capstone project of the data science specialization

# First we have to download data and unzip it

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


# Here data is downloaded and unzipped. As we are going to read only english data for now, 
# let's create path only for them

englishFolder <- file.path(pathFolder, "en_US")
blogsPath <- file.path(englishFolder,"en_US.blogs.txt")
newsPath <- file.path(englishFolder,"en_US.news.txt")
twitterPath <-file.path(englishFolder,"en_US.twitter.txt")


#       Code for Quiz1

# 1.- The en_US.blogs.txt file is how many megabytes?

# 150

# 200  (Correct)

# 100

# 250

file.size(blogsPath)/1024/1024

# 2.- The en_US.twitter.txt has how many lines of text?

# Around 5 hundred thousand

# Around 2 hundred thousand
 
# Over 2 million        (Correct)

# Around 1 million

twitterReaded <- readLines(twitterPath)

length(twitterReaded)

# 3.- What is the length of the longest line seen in any of the three en_US data sets?

# Over 40 thousand in the blogs data set        (Correct)

# Over 11 thousand in the news data set

# Over 40 thousand in the news data set

# Over 11 thousand in the blogs data set

blogsReaded <- readLines(blogsPath)
newsReaded <- readLines(newsPath)

max(nchar(blogsReaded))
max(nchar(newsReaded))
max(nchar(twitterReaded))

# 4.- In the en_US twitter data set, if you divide the number of lines where the word "love" 
# (all lowercase) occurs by the number of lines the word "hate" (all lowercase) occurs, 
# about what do you get?

# 0.25

# 0.5

# 2

# 4             (Correct)

sum(grep("love",twitterReaded))/sum(grep("hate",twitterReaded))

# 5.- The one tweet in the en_US twitter data set that matches the word "biostats" says what?

# They haven't studied for their biostats exam          (Correct)

# They need biostats help on their project

# It's a tweet about Jeff Leek from one of his students in class

# They just enrolled in a biostat program

twitterReaded[grep("biostats",twitterReaded)]

# 6.- How many tweets have the exact characters "A computer once beat me at chess, but it was 
# no match for me at kickboxing". (I.e. the line matches those characters exactly.)

# 0

# 2

# 1

# 3             (Correct)

length(grep("A computer once beat me at chess, but it was no match for me at kickboxing", twitterReaded))