#Creating training and test set
#install.packages("caTools")
set.seed(111)
split <- sample.split(data$TotalGross, SplitRatio = 0.70)
training_set <- subset(data, split == TRUE)
test_set <- subset(data, split == FALSE)


# Fitting the support vector regression model to the training set -- linear
model_01_svr <- svm((TotalGross) ~ ., data = training_set, kernel = "linear", 
                    cross = 10, epsilon = 0.1, cost = 100)
summary(model_01_svr)
sqrt(model_01_svr$tot.MSE)
#Predicting test set
predict_model_01_svr <- predict(model_01_svr, training_set) 
error <- (training_set - predict_model_01_svr)
rmse(error)
sqrt(mean(model_01_svr$residuals^2))
#Calculating RMSE to see how well model fit
rmse(predict_model_01_svr, training_set$TotalGross) 
mae(predict_model_01_svr, test_set$TotalGross)


# Fitting the support vector regression model to the training set -- radial
model_02_svr <- svm((TotalGross) ~ ., data = training_set, kernel = "radial", epsilon = 0.1, cost = 1)
summary(model_02_svr)

#Predicting test set
predict_model_02_svr <- predict(model_02_svr, test_set) 

#Calculating RMSE to see how well model fit
rmse(predict_model_02_svr, test_set$TotalGross) 
mae(predict_model_02_svr, test_set$TotalGross)


# Fitting the support vector regression model to the training set -- polynomial
model_03_svr <- svm((TotalGross) ~ ., data = training_set, kernel = "polynomial", 
                    epsilon = 0.1, cost = 1000)
summary(model_03_svr)

#Predicting test set
predict_model_03_svr <- predict(model_03_svr, test_set) 

#Calculating RMSE to see how well model fit
rmse(predict_model_03_svr, test_set$TotalGross) 
mae(predict_model_03_svr, test_set$TotalGross)


# Fitting the support vector regression model to the training set -- sigmoid
model_04_svr <- svm((TotalGross) ~ ., data = training_set, kernel = "sigmoid", 
                    epsilon = 0.1, cost = 0.05)
summary(model_04_svr)

#Predicting test set
predict_model_04_svr <- predict(model_04_svr, test_set) 

#Calculating RMSE to see how well model fit
rmse(predict_model_04_svr, test_set$TotalGross) 
mae(predict_model_04_svr, test_set$TotalGross)



# Fitting feature extracted support vector regression model to the training set -- linear
model_05_svr <- svm((TotalGross) ~ OpeningTheaters + (ViewCount) + (LikeCount) +
                    (positiveWordsBL) + netSentimentBL + scoreAbsoluteAFINN + (budget) + 
                    (OpeningWeekendGross) + genre + mpaaRating,
                    data = training_set, kernel = "linear", cross = 10, epsilon = 0.1, cost = 250)
summary(model_05_svr)
model_05_svr$residuals

#Predicting test set
predict_model_05_svr <- predict(model_05_svr, test_set) 

#Calculating RMSE to see how well model fit
rmse(predict_model_05_svr, test_set$TotalGross) 
mae(predict_model_05_svr, test_set$TotalGross)


# Fitting feature extracted support vector regression model to the training set -- radial
model_06_svr <- svm((TotalGross) ~ OpeningTheaters + (ViewCount) + (LikeCount) +
                        (positiveWordsBL) + netSentimentBL + scoreAbsoluteAFINN + (budget) + 
                        (OpeningWeekendGross) + genre + mpaaRating,
                    data = training_set, kernel = "radial", epsilon = 0.1, cost = 1000)
summary(model_06_svr)

#Predicting test set
predict_model_06_svr <- predict(model_06_svr, test_set) 

#Calculating RMSE to see how well model fit
rmse(predict_model_06_svr, test_set$TotalGross) 
mae(predict_model_06_svr, test_set$TotalGross)



#Tune support vector regression model to find best paramaters by performing grid search
model_svr_01 <- tune(svm, TotalGross ~ .,  data = training_set,
                    ranges = list(epsilon = seq(0,1,0.2), cost = 200:250, kernel = "linear"))


summary(model_svr_01)

#Predicting tuneResult
#Find out the best model
model_svr_01_best <- model_svr_01$best.model
summary(model_svr_01_best)

#Predict tuneResult using best model
predict_model_svr_01_best <- predict(model_svr_01_best,test_set)

#Calculate RMSE of the best tune model 
rmse(error)

error <- (predict_model_svr_01_best - test_set$TotalGross)











