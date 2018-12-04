# Import data set
data <- read.csv("Dataset/Dataset_Master.csv", stringsAsFactors = FALSE)


# Data set inspection
summary(data)
str(data)


#Convert variables to their appropriate format
data$ReleaseYear <- as.factor(data$ReleaseYear)
data$OpeningTheaters <- as.integer(gsub(',', '', data$OpeningTheaters))
data$OpeningWeekendGross <- as.integer(gsub('\\$|,', '', data$OpeningWeekendGross))
data$TotalGross <- as.integer(gsub('\\$|,', '', data$TotalGross))
data$ViewCount <- as.integer(data$ViewCount)
data$genre <- as.factor(data$genre)
data$mpaaRating <- as.factor(data$mpaaRating)


#Remove non-impatctful variables Trailer_id, Title, ReleaseYear, and Studio 
data <- data[-(1:4)]


#Descriptive statistics table of features
stargazer(data, type = "html", digits = 0, median = TRUE, out = "descriptive2.doc")
      

#Boxplots of numeric features
boxplot(data$OpeningTheaters, main="Number of Opening Weekend Theaters", 
                ylab="# of Theaters")

boxplot(data$ViewCount, main="Number of Views Per Movie Trailer", 
        ylab="# of Views")

boxplot(data$LikeCount, main="Number of Likes Per Movie Trailer", 
        ylab="# of Likes")

boxplot(data$DislikeCount, main="Number of Dis-Likes Per Movie Trailer", 
        ylab="# of Dis-Likes")

boxplot(data$LikeDislikeRatio, main="Likes to Dis-likes Ratio Per Movie Trailer", 
        ylab="Like to Dis-like Ratio")

boxplot(data$CommentCount, main="Number of Comments Per Movie Trailer", 
        ylab="# of Comments")

boxplot(data$positiveWordsBL, main="Bing-Liu Lexicon Positive Words in Comments Section", 
        ylab="# of Positive Words")

boxplot(data$negativeWordsBL, main="Bing-Liu Negative Words in Comments Section", 
        ylab="# of Negative Words")

boxplot(data$netSentimentBL, main="Net Positive Words Per Movie Trailer", 
        ylab="# of Net Positive Words")

boxplot(data$scoreAbsoluteAFINN, main="AFINN Sentiment Analysis Score", 
        ylab="AFINN Score")

boxplot(data$OpeningWeekendGross, main="Opening Weekend Box Office Revenue", 
        ylab="$ Revenue")

boxplot(data$TotalGross, main="Total Box Office Revenue", 
        ylab="$ Revenue")

par(mfrow = c(3,3))


#Histogram of numeric features
hist(data$OpeningTheaters, main="Number of Opening Weekend Theaters", 
        ylab="# of Theaters")

hist(data$ViewCount, main="Number of Views Per Movie Trailer", 
        ylab="# of Views")

hist(data$LikeCount, main="Number of Likes Per Movie Trailer", 
        ylab="# of Likes")

hist(data$DislikeCount, main="Number of Dis-Likes Per Movie Trailer", 
        ylab="# of Dis-Likes")

hist(data$LikeDislikeRatio, main="Likes to Dis-likes Ratio Per Movie Trailer", 
        ylab="Like to Dis-like Ratio")

hist(data$CommentCount, main="Number of Comments Per Movie Trailer", 
        ylab="# of Comments")

hist(data$positiveWordsBL, main="Bing-Liu Lexicon Positive Words in Comments Section", 
        ylab="# of Positive Words")

hist(data$negativeWordsBL, main="Bing-Liu Lexicon Negative Words in Comments Section", 
        ylab="# of Negative Words")

hist(data$netSentimentBL, main="Net Positive Words Per Movie Trailer", 
        ylab="# of Net Positive Words")

hist(data$scoreAbsoluteAFINN, main="AFINN Lexicon Sentiment Analysis Score", 
        ylab="AFINN Score")

hist(data$OpeningWeekendGross, main="Opening Weekend Box Office Revenue", 
        ylab="$ Revenue")

hist(data$TotalGross, main="Total Box Office Revenue", 
        ylab="$ Revenue")



#Histograms with Log Transformation:
hist(data$OpeningTheaters, main="Number of Opening Weekend Theaters", 
     ylab="# of Theaters")

hist(log(data$ViewCount), main="Number of Views Per Movie Trailer", 
     ylab="# of Views")

hist(log(data$LikeCount), main="Number of Likes Per Movie Trailer", 
     ylab="# of Likes")

hist(log(data$DislikeCount), main="Number of Dis-Likes Per Movie Trailer", 
     ylab="# of Dis-Likes")

hist(log(data$LikeDislikeRatio), main="Likes to Dis-likes Ratio Per Movie Trailer", 
     ylab="Like to Dis-like Ratio")

hist(log(data$CommentCount), main="Number of Comments Per Movie Trailer", 
     ylab="# of Comments")

hist(log(data$positiveWordsBL), main="Bing-Liu Lexicon Positive Words in Comments Section", 
     ylab="# of Positive Words")

hist(log(data$negativeWordsBL), main="Bing-Liu Lexicon Negative Words in Comments Section", 
     ylab="# of Negative Words")

hist(log(data$netSentimentBL), main="Net Positive Words Per Movie Trailer", 
     ylab="# of Net Positive Words")

hist(log(data$scoreAbsoluteAFINN), main="AFINN Lexicon Sentiment Analysis Score", 
     ylab="AFINN Score")

hist(log(data$OpeningWeekendGross), main="Opening Weekend Box Office Revenue", 
     ylab="$ Revenue")

hist(log(data$TotalGross), main="Total Box Office Revenue", 
     ylab="$ Revenue")


#Correlation Matrix
#install.packages("PerformanceAnalytics")
chart.Correlation(data[c(-13,-14)])

chart.Correlation(log(data[c(-1,-13,-14)]))



