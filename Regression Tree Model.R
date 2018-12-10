
#Creating training and test set
install.packages("caTools")
set.seed(111)
split <- sample.split(data$TotalGross, SplitRatio = 0.70)
training_set_tree <- subset(data, split == TRUE)
test_set_tree <- subset(data, split == FALSE)

#using Rpart package to fit regression tree model

#Model 1 - All variables
model_tree_01 <- rpart(TotalGross ~ ., 
                            method = "anova", data = training_set)

predict_model_tree_01 <- predict(model_tree_01, test_set)
rmse(test_set$TotalGross, predict_model_tree_01)
mae(test_set$TotalGross, predict_model_tree_01)



#Model 2 - Featuers extracted from VIF test
model_tree_02 <- rpart(TotalGross ~ OpeningTheaters + (ViewCount) + (LikeCount) +
                           (positiveWordsBL) + netSentimentBL + scoreAbsoluteAFINN + (budget) + 
                           (OpeningWeekendGross) + genre + mpaaRating, 
                       method = "anova", data = training_set)

predict_model_tree_02 <- predict(model_tree_02, test_set)
rmse(test_set$TotalGross, predict_model_tree_02)
mae(test_set$TotalGross, predict_model_tree_02)


#Model 03 - All features from VIF test except opening weekend gross
model_tree_03 <- rpart(TotalGross ~ OpeningTheaters + (ViewCount) + (LikeCount) +
                           (positiveWordsBL) + netSentimentBL + scoreAbsoluteAFINN + (budget) + 
                           genre + mpaaRating, 
                       method = "anova", data = training_set)

predict_model_tree_03 <- predict(model_tree_03, test_set)
rmse(test_set$TotalGross, predict_model_tree_03)
mae(test_set$TotalGross, predict_model_tree_03)


stargazer(model_tree_01$cptable, type = "html", digits = 5, out = "descriptive2.doc")


# Random Forest
model_random_tree_01 <- randomForest(TotalGross ~ ., 
                       data = training_set)

predict_model_random_tree_01 <- predict(model_random_tree_01, test_set)
rmse(test_set$TotalGross, predict_model_random_tree_01)
mae(test_set$TotalGross, predict_model_random_tree_01)
summary(model_random_tree_01)
stargazer(model_random_tree_01$cptable, type = "html", digits = 5, out = "descriptive2.doc")



# Tuning Random Forest
ctrl <- trainControl(method = "repeatedcv",
                     number = 10, repeats = 10)

grid_rf <- expand.grid(.mtry = c(2, 4, 8, 16))

tuned_tree <- train(TotalGross ~ ., data = training_set, method = "rf",
              trControl = ctrl,
              tuneGrid = grid_rf)

summary(model_tree_01)


















rpart.plot::rpart.plot(model_tree_01)
plotcp(model_tree_01)
model_tree_01$cptable
fancyRpartPlot(model_tree,cex=3)

predict_model_tunedtree_01 <- predict(tuned_tree, test_set)
rmse(test_set$TotalGross, predict_model_tunedtree_01)
summary(predict_model_tree_01)
summary(test_set$TotalGross)











summary(p.rpart)
summary(test_set$TotalGross)
cor(p.rpart, test_set$TotalGross)

MAE <- function(actual, predicted) {
    mean(abs(actual - predicted))
}
MAE(p.rpart, test_set$TotalGross)
mean(test_set_tree$TotalGross)
model_tree
