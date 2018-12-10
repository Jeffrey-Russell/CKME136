#Creating training and test set
#install.packages("caTools")
set.seed(111)
split <- sample.split(data$TotalGross, SplitRatio = 0.70)
training_set<- subset(data, split == TRUE)
test_set<- subset(data, split == FALSE)


# Train the model
model_01 <- lm(log(TotalGross) ~ OpeningTheaters + log(ViewCount) + log(LikeCount) +
                      log(positiveWordsBL) + netSentimentBL + scoreAbsoluteAFINN + log(budget) + 
                      log(OpeningWeekendGross) + genre + mpaaRating, data = training_set)
summary(model_01)
plot(model_01_xcv)

model_02 <- lm(log(TotalGross) ~ OpeningTheaters + log(ViewCount) + log(LikeCount) +
                       log(positiveWordsBL) + netSentimentBL + scoreAbsoluteAFINN + log(budget) + 
                       genre + mpaaRating, data = train_set)
summary(model_02)

#Predict model using test_set and calculating RMSE on model_02
model_predict_02 <- predict(model_02, test_set)
rmse(exp(model_predict_02), test_set$TotalGross)
mae(exp(model_predict_02), test_set$TotalGross)

#=Apply stepwise variable selection on model_02
model_03 <- step(model_02, direction = "both")

summary(model_03)

#Predict stepwise model_03 using test_set and calculating RMSE
model_predict_03 <- predict(model_03, test_set)
rmse(exp(model_predict_03), test_set$TotalGross)
mae(exp(model_predict_03), test_set$TotalGross)


#Predict model_02 using backward step reduction -- model_04
model_04 <- step(model_02, direction = "backward")
summary(model_04)

#Predict stepwise model_02 using fo test_set and calculating RMSE
model_predict_04 <- predict(model_04, test_set)
rmse(exp(model_predict_04), test_set$TotalGross)
mae(exp(model_predict_04), test_set$TotalGross)



#Predict model_02 using forward step reduction -- model_05
model_05 <- step(model_02, direction = "forward")
summary(model_05)

#Predict stepwise model_02 using fo test_set and calculating RMSE
model_predict_05 <- predict(model_05, test_set)
rmse(exp(model_predict_05), test_set$TotalGross)
mae(exp(model_predict_05), test_set$TotalGross)


# Remove genre variable 
model_06 <- lm(log(TotalGross) ~ OpeningTheaters + log(ViewCount) + log(LikeCount) +
                   log(positiveWordsBL) + netSentimentBL + scoreAbsoluteAFINN + log(budget) 
                    + mpaaRating, data = training_set)
summary(model_06)

#Predict model_06 using test_set and calculating RMSE on model_06
model_predict_06 <- predict(model_06, test_set)
rmse(exp(model_predict_06), test_set$TotalGross)
mae(exp(model_predict_06), test_set$TotalGross)



# Remove mpaaRating variable 
model_07 <- lm(log(TotalGross) ~ OpeningTheaters + log(ViewCount) + log(LikeCount) +
                   log(positiveWordsBL) + netSentimentBL + scoreAbsoluteAFINN + log(budget), 
                    data = training_set)
summary(model_07)

#Predict model_07 using test_set and calculating RMSE on model_06
model_predict_07 <- predict(model_07, test_set)
rmse(exp(model_predict_07), test_set$TotalGross)
mae(exp(model_predict_07), test_set$TotalGross)





stargazer(model_01, model_02, type = "html", digits = 5, out = "descriptive2.doc")




