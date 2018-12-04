#Sentiment Analysis using Bing Liu Lexicon

# Loading Libraries
library(tidyverse)
library(tidytext)
library(glue)
library(stringr)

## Getting a list of the comments files in the directory
files <- list.files("Youtube Comments Extraction/Comment Files/List of Files")

# Sticking together the file path to the comments file name
# Load "glue" package
fileName <- glue("Youtube Comments Extraction/Comment Files/List of Files/", files[251], sep = "")

# Getting rid of trailing spaces
fileName <- trimws(fileName)

# Reading comments file
fileComments <- read.csv(fileName)

# Deleting id coloumn and changing comment field from factor to character
fileComments <- fileComments[,-1]
fileComments <- as.character(fileComments)

# Tokenizing
tokens <- data_frame(text = fileComments) %>% unnest_tokens(word, text)

# Getting sentiment from comments file: 
tokens %>%
        inner_join(get_sentiments("bing")) %>% # pull out only sentiment words
        count(sentiment) %>% # count the # of positive & negative words
        spread(sentiment, n, fill = 0) %>% # transposing tibble
        mutate(sentiment = positive - negative) #  adding coloumn of positive words - # of negative words



# Writing function that takes comments file and returns number of positive, negative, 
# and the number of positive - negatieve words

GetSentiments <- function(file){
        # getting the comments file
        fileName <- glue("Youtube Comments Extraction/Comment Files/List of Files/", file, sep = "")
        
        # getting rid of trailing spaces in file name if they exist
        fileName <- trimws(fileName)
        
        # Reading comments file
        fileComments <- read.csv(fileName)
        
        # Deleting id coloumn and changing comment field from factor to character
        fileComments <- fileComments[,-1]
        fileComments <- as.character(fileComments)
        
        # Tokenizing
        tokens <- data_frame(text = fileComments) %>% unnest_tokens(word, text)
        
        # Getting sentiment from comments file: 
        SentimentScore <- tokens %>%
                inner_join(get_sentiments("bing")) %>% # pull out only sentiment words
                count(sentiment) %>% # count the # of positive & negative words
                spread(sentiment, n, fill = 0) %>% # transposing tibble
                mutate(sentiment = positive - negative) %>% #  adding column of 
                                                            # positive words - # of negative words
                mutate(id = file)
 
        return(SentimentScore)
}

# Creating data drame with Bing Liu sentiment scores

SentimentScoreTable <- data_frame()

# Loop to get the sentiment scores from each comments file for
for(i in files){
        SentimentScoreTable <- rbind(SentimentScoreTable, GetSentiments(i))
}

# Outputting Sentiment Scores Table to csv file
write.csv(SentimentScoreTable, "SentimentScoreTable.csv",row.names = FALSE)




# Getting AFINN sentiment from comments file: 
tokens %>%
        inner_join(get_sentiments("afinn")) %>% # pull out only sentiment words
        count(score) %>%
        summarize(score = sum(score * n))

# Writing function that takes comments file and returns number of positive, negative, 
# and the number of positive - negatieve words

GetSentimentsAFINN <- function(file){
        # getting the comments file
        fileName <- glue("Youtube Comments Extraction/Comment Files/List of Files/", file, sep = "")
        
        # getting rid of trailing spaces in file name if they exist
        fileName <- trimws(fileName)
        
        # Reading comments file
        fileComments <- read.csv(fileName)
        
        # Deleting id coloumn and changing comment field from factor to character
        fileComments <- fileComments[,-1]
        fileComments <- as.character(fileComments)
        
        # Tokenizing
        tokens <- data_frame(text = fileComments) %>% unnest_tokens(word, text)
        
        # Getting sentiment from comments file: 
        SentimentScoreAFINN <- tokens %>%
                inner_join(get_sentiments("afinn")) %>% # pull out only sentiment words
                count(score) %>%
                summarize(score = sum(score * n)) %>%
                mutate(id = file)
        
        return(SentimentScoreAFINN)
}

# Creating data drame with Bing Liu sentiment scores

SentimentScoreTableAFINN <- data_frame()

# Loop to get the sentiment scores from each comments file for
for(i in files){
        SentimentScoreTableAFINN <- rbind(SentimentScoreTableAFINN, GetSentimentsAFINN(i))
}

# Outputting Sentiment Scores Table to csv file
write.csv(SentimentScoreTableAFINN, "SentimentScoreTableAFINN.csv",row.names = FALSE)       
        
        



