data(state)
statedata <- data.frame(state.x77)
str(statedata)

model1 <- lm(Life.Exp ~ ., data = statedata)
summary(model1)
# The value of adjusted R^2 is 0.6922.

predictedModel1 <- predict(model1)
SSE <- sum((predictedModel1 - statedata$Life.Exp)^2)
SSE
SSE1 <- sum(model1$residuals^2)
SSE1
# The value of SSE is 23.29714.

model2 <- lm(Life.Exp ~ Population + Murder + Frost + HS.Grad, data = statedata)
summary(model2)
# The value of adjusted R^2 is 0.7126.
SSE2 <- sum(model2$residuals^2)
SSE2
# The value of SSE is 23.30804.


library(rpart)
library(rpart.plot)
CARTModel1 <- rpart(Life.Exp ~ ., data = statedata)
prp(CARTModel1)

TreePredict <- predict(CARTModel1)
SSE3 <- sum((TreePredict - statedata$Life.Exp)^2)
SSE3
# The value of SSE is 28.99848.

CARTModel2 <- rpart(Life.Exp ~ ., data = statedata, minbucket = 5)
prp(CARTModel2)
predictedModel2 <- predict(CARTModel2)

CARTModel3 <- rpart(Life.Exp ~ ., data = statedata, minbucket = 6)
prp(CARTModel3)


SSE4 <- sum((predictedModel2 - statedata$Life.Exp)^2)
SSE4
# The value of SSE is 23.64283.

CARTModel4 <- rpart(Life.Exp ~ Area, data = statedata, minbucket = 1)
prp(CARTModel4)
predictedModel3 <- predict(CARTModel4)

SSE5 <- sum((predictedModel3 - statedata$Life.Exp)^2)
SSE5
# The value of SSE is 9.312442

library(caret)
library(e1071)
set.seed(111)

numFolds <- trainControl(method = "cv", number = 10)
cpGrid <- expand.grid(.cp = seq(0.01, 0.50, 0.01))
cpGrid
tr <- train(Life.Exp ~ ., data = statedata, method = "rpart", trControl = numFolds, tuneGrid = cpGrid)
tr

finalTreeModel <- rpart(Life.Exp ~ ., data = statedata, cp=0.12)
prp(finalTreeModel)
finalTreePrediction <- predict(finalTreeModel)

SSE6 <- sum((finalTreePrediction - statedata$Life.Exp)^2)
SSE6
# The value of SSE is 32.86549

set.seed(111)
numFolds2 <- trainControl(method = "cv", number = 10)
cpGrid2 <- expand.grid(.cp = seq(0.01, 0.5, 0.01))
cpGrid2

tr2 <- train(Life.Exp ~ Area, data = statedata, method = "rpart", trControl = numFolds2, tuneGrid = cpGrid2)
tr2

areaTreeModel <- rpart(Life.Exp ~ Area, data = statedata, cp=0.01)
prp(areaTreeModel)
# There are 4 splits in this tree.

areaPredictedModel <- predict(areaTreeModel)
SSE7 <- sum((areaPredictedModel - statedata$Life.Exp)^2)
SSE7
# The value of SSE is 44.26817.
