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
#install.packages("caTools")
set.seed(123)
split <- sample.split(data$TotalGross, SplitRatio = 0.70)
training_set <- subset(data, split == TRUE)
test_set <- subset(data, split == FALSE)

# Fitting the support vector regression model to the training set
model_svr <- svm(TotalGross~., data = training_set, kernel = "linear")

#predicting training set
predict_svr_train <- predict(model_svr, training_set) 

#Calculating RMSE to see how well model fit
rmse(training_set$TotalGross, predict_svr_train) 

#Predicting test set
predict_svr_test <- predict(model_svr, test_set) 
    
#Calculating RMSE to see how well model fit
rmse(test_set$TotalGross, predict_svr_test) 

#Tune support vector regression model to find best paramaters by performing grid search
tuneResult <- tune(svm, TotalGross ~ .,  data = training_set,
                    ranges = list(epsilon = seq(0,1,0.1), cost = 2^(seq(0.5,8,.5)), kernel = "linear"))

#Predicting tuneResult
#Find out the best model
BestModel=tuneResult$best.model

#Predict tuneResult using best model
predict_tuneResult=predict(BestModel,test_set)

#Calculate RMSE of the best tune model 
rmse(test_set$TotalGross,predict_tuneResult)













