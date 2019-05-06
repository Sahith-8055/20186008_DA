census <- read.csv("census.csv")

library(caTools)
set.seed(2000)
split <- sample.split(census$over50k, SplitRatio = 0.6)
Train <- subset(census, split==TRUE)
Test <- subset(census, split==FALSE)

model1 <- glm(over50k ~ ., data = Train, family = "binomial")
summary(model1)

predictModel1 <- predict(model1, newdata = Test, type = "response")
tbl <- table(Test$over50k, predictModel1 > 0.5)
sum(diag(tbl)) / nrow(Test)
# accuracy of the model on the testing set is 0.8552107.

table(Test$over50k)
9713/(3078+9713)
# baseline accuracy for the testing set is 0.7593621.
# For finding out baseline accuracy on the testing set, we need to
# first determine the most frequent outcome in the training set.
# Then, the most frequently occuring outcome from the training set
# is seen in the testing set.

library(ROCR)
predictROCR <- prediction(predictModel1, Test$over50k)
auc <- as.numeric(performance(predictROCR, "auc")@y.values)
auc
# the value of auc is 0.9061598.

library(rpart)
library(rpart.plot)
classTree <- rpart(over50k ~ ., data = Train, method = "class")
prp(classTree)

# What is the accuracy of the model on the testing set? Use a threshold of 0.5.
predictModel2 <- predict(classTree, newdata = Test, type = "class")
tbl2 <- table(Test$over50k, predictModel2)
sum(diag(tbl2)) / nrow(Test)
# Accuracy of the model on test set is 0.8473927.

# Plot the ROC curve for the CART model you have estimated.
# Observe that compared to the logistic regression ROC curve, the CART ROC curve is less smooth than the logistic regression ROC curve.
# Which of the following explanations for this behavior is most correct?
treePredicts <- predict(classTree, newdata = Test)
treePredicts
treePrediction <- prediction(treePredicts[,2], Test$over50k)
treePerformance <- performance(treePrediction, "tpr", "fpr")
plot(treePerformance)
logisticPerformance <- performance(predictROCR, "tpr", "fpr")
plot(logisticPerformance)
# The probabilities from the CART model take only a handful of values 
# (five, one for each end bucket/leaf of the tree); the changes in the 
# ROC curve correspond to setting the threshold to one of those values.

auc <- as.numeric(performance(treePrediction, "auc")@y.values)
auc
# 0.8470256 is the auc value for the CART model.

set.seed(1)
trainSmall = Train[sample(nrow(Train), 2000), ]
library(randomForest)
set.seed(1)
ForestModel1 <- randomForest(over50k ~ ., data = trainSmall)
predictForest1 <- predict(ForestModel1, newdata = Test)
tbl3 <- table(Test$over50k, predictForest1)
sum(diag(tbl3)) / nrow(Test)
# accuracy of the random forest model is 0.8515362.


# Which of the following variables is the most important in terms of the number of splits?
vu <- varUsed(ForestModel1, count = TRUE)
vusorted <- sort(vu, decreasing = FALSE, index.return = TRUE)
dotchart(vusorted$x, names(ForestModel1$forest$xlevels[vusorted$ix]))
# age.

# Which one of the following variables is the most important in terms of mean reduction in impurity?
varImpPlot(ForestModel1)
# occupation gives a large reduction in impurity.

library(caret)
library(e1071)
set.seed(2)
numFolds <- trainControl(method = "cv", number = 10)
cpGrid <- expand.grid(.cp = seq(0.002, 0.1, 0.002))
cpGrid
tr <- train(over50k ~ ., method = "rpart", trControl = numFolds, tuneGrid = cpGrid, data = Train)
tr
# the value of cp is 0.002.

newTreeModel <- rpart(over50k ~ ., data = Train, cp=0.002, method = "class")
newPredictionModel <- predict(newTreeModel, newdata = Test, type = "class")
tbl4 <- table(Test$over50k, newPredictionModel)
sum(diag(tbl4)) / nrow(Test)
# the accuracy of the new model is 0.8612306.

# Plot the CART tree for this model. How many splits are there?
prp(newTreeModel)
# there are 18 splits in this new model.