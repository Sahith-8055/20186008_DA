letters <- read.csv("letters_ABPR.csv")
str(letters)

letters$isB <- as.factor(letters$letter=="B")

library(caTools)
set.seed(1000)
split <- sample.split(letters$isB, SplitRatio = 0.5)
Train1 <- subset(letters, split==TRUE)
Test1 <- subset(letters, split==FALSE)

table(Test1$isB)
1175 / (1175+383)
# accuracy of the baseline model is 0.754172.

library(rpart)
library(rpart.plot)
CARTb <- rpart(isB ~ . -letter, data = Train1, method = "class")
predict1 <- predict(CARTb, newdata = Test1, type = "class")
table(Test1$isB, predict1)
(340+1118)/(340+1118+57+43)
# accuracy of the CART model is 0.9358151.

library(randomForest)
set.seed(1000)
ForestModel <- randomForest(isB ~ . -letter, data = Train1)
predictForest1 <- predict(ForestModel, newdata = Test1)
table(Test1$isB, predictForest1)
(1163+374)/(1163+374+12+9)
# accuracy of this random forest model is 0.9865212.

letters$letter <- as.factor(letters$letter)
set.seed(2000)
split2 <- sample.split(letters$letter, SplitRatio = 0.5)
Train2 <- subset(letters, split2==TRUE)
Test2 <- subset(letters, split==FALSE)
max(table(Test2$letter)) / length(Test2$letter)
# accuracy of the baseline model is 0.254172.

library(rpart)
library(rpart.plot)
classTree <- rpart(letter ~ . -isB, data=Train2, method="class")
predictTree <- predict(classTree, newdata = Test2, type = "class")
tbl <- table(Test2$letter, predictTree)
sum(diag(tbl)) / nrow(Test2)
# Test set accuracy of the CART model is 0.8851091.

set.seed(1000)
ForestModel2 <- randomForest(letter ~ . -isB, data = Train2)
predictForest2 <- predict(ForestModel2, newdata = Test2)
tbl2 <- table(Test2$letter, predictForest2)
sum(diag(tbl2)) / nrow(Test2)
# accuracy of the randome forest model is 0.9910141.