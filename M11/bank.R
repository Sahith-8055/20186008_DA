bank <- read.csv("bank-full.csv")
str(bank)

library(caTools)
set.seed(1000)

spl <- sample.split(bank$y, SplitRatio = 0.6)
Train <- subset(bank, spl == TRUE)
Test <- subset(bank, spl == FALSE)

model1 <- glm(y ~ age + balance + campaign + duration, data = Train, family = "binomial")

summary(model1)

model2 <- glm(y ~ age + balance + duration, data = Train, family = "binomial")

summary(model2)

predictModel1 <- predict(model1, type = "response")
table(Train$y, predictModel1 >= 0.5)

predictTestModel1 <- predict(model1, newdata = Test, type = "response")
library(ROCR)
predROCR <- prediction(predictTestModel1, Test$y)
auc <- as.numeric(performance(predROCR, "auc")@y.values)
auc


library(rpart)
library(rpart.plot)

CARTModel1 <- rpart(y ~ age + balance + duration, data = Train)
prp(CARTModel1)


predictTestModel2 <- predict(model2, newdata = Test, type = "response")
predROCR2 <- prediction(predictTestModel2, Test$y)
auc2 <- as.numeric(performance(predROCR2, "auc")@y.values)
auc2
