data <- read.csv("Dataset/Dataset_messy02.csv", stringsAsFactors = FALSE)

str(data)

#Changing ReleaseYear, OpeningTheaters, and OpeningWeekendClass to correct class
data$ReleaseYear <- as.factor(data$ReleaseYear)
data$OpeningTheaters <- as.integer(gsub(',', '', data$OpeningTheaters))
data$OpeningWeekendGross <- as.integer(gsub('\\$|,', '', data$OpeningWeekendGross))
data$TotalGross <- as.integer(gsub('\\$|,', '', data$TotalGross))
data$ViewCount <- as.integer(data$ViewCount)

#Summary
summary(data)
str(data)

#Removing variables Trailer_id, Title, ReleaseYear, and Studio which is not needed for model
data <- data[-(1:4)]

#Correlation Matrix
chart.Correlation(data)

#Removing independent variables with < 0.40 correleation with independent variable TotalGross, 
#Removing negativeWordsBL which has correlation of 0.95 with positiveWordsBL reduce multicollinearity
data <- select(data,OpeningTheaters,ViewCount,LikeCount,CommentCount,positiveWordsBL,
               netSentimentBL,scoreAbsoluteAFINN,OpeningWeekendGross,TotalGross)

#Creating training and test set
install.packages("caTools")
set.seed(123)
split <- sample.split(data$TotalGross, SplitRatio = 0.7)
training_set <- subset(data, split == TRUE)
test_set <- subset(data, split == FALSE)

# Fitting multiple linear regression model to the training set
model <- lm(TotalGross~ OpeningTheaters + ViewCount + LikeCount + CommentCount +
            positiveWordsBL + netSentimentBL + scoreAbsoluteAFINN + OpeningWeekendGross,
            data = training_set)

#Summary of model
summary(model)

#Predicting model using training data and calculating RMSE
model_predict <- predict(model, test_set)
rmse(model_predict, test_set$TotalGross) #28402416


#Tuning Model for low AIC (Akaike Information Criterion) using Step function
model_best <- step(model)
summary(model_best)


#Predicting model using training data and calculating RMSE
model_best_predict <- predict(model_best, test_set)
rmse(model_best_predict, test_set$TotalGross) #28612892


par(mfrow = c(2, 2))
plot(model_best)


#Log transforming model to try to achieve a normal distribution
log_model <- lm(log(TotalGross) ~ OpeningTheaters + log(ViewCount) + log(LikeCount) + log(CommentCount) +
                log(positiveWordsBL) + netSentimentBL + scoreAbsoluteAFINN + log(OpeningWeekendGross),
                data = training_set)

summary(log_model)


#Predicting model using training data and calculating RMSE
log_model_predict <- predict(log_model, test_set)
rmse(exp(log_model_predict), (test_set$TotalGross)) 


#Tuning Model for low AIC (Akaike Information Criterion) using Step function
log_model_best <- step(log_model)
summary(log_model_best)


#Predicting model using training data and calculating RMSE
log_model_best_predict <- predict(log_model_best, test_set)
rmse(exp(log_model_best_predict), (test_set$TotalGross)) 


par(mfrow = c(2, 2))
plot(log_model_best)




