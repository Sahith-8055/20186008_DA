stevens <- read.csv("stevens.csv")
str(stevens)
# 1 for reverse and 0 for affirm.

library(caTools)
set.seed(3000)
spl <- sample.split(stevens$Reverse, SplitRatio = 0.7)
train <- subset(stevens, spl==TRUE)
test <- subset(stevens, spl==FALSE)

install.packages("rpart")
library(rpart)
install.packages("rpart.plot")
library(rpart.plot)

stevensTree <- rpart(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data = train, method = "class", minbucket=25)
prp(stevensTree)

PredictCART <- predict(stevensTree, newdata = test, type = "class")
table(test$Reverse, PredictCART)
(41 + 71)/(41+36+22+71)
# accuracy of the model is 0.6588235.

library(ROCR)
PredictROC <- predict(stevensTree, newdata = test)
PredictROC
pred <- prediction(PredictROC[,2], test$Reverse)
pref <- performance(pred, "tpr", "fpr")
plot(pref)

install.packages("randomForest")
library(randomForest)
stevensForest <- randomForest(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data = train, nodesize=25, ntree=200)
train$Reverse <- as.factor(train$Reverse)
test$Reverse <- as.factor(test$Reverse)
predictForest <- predict(stevensForest, newdata = test)
table(test$Reverse, predictForest)
(42+75)/(42+35+18+75)
# accuracy is 0.6882353.

install.packages("caret")
library(caret)
install.packages("e1071")
library(e1071)

numFolds <- trainControl(method = "cv", number = 10)
cpGrid <- expand.grid(.cp=seq(0.01, 0.5, 0.01))
train(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data=train, method="rpart", trControl=numFolds, tuneGrid=cpGrid)
stevensTreeCV <- rpart(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data=train, method="class", cp=0.19)
PredictCV <- predict(stevensTreeCV, newdata = test, type = "class")
table(test$Reverse, PredictCV)
(59+64)/(59+64+18+29)
# accuracy of this model is 0.7235294.

