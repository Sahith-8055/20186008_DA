framingham <- read.csv("framingham.csv")
str(framingham)

library(caTools)
set.seed(1000)
split <- sample.split(framingham$TenYearCHD, SplitRatio = 0.65)
split

train <- subset(framingham, split==TRUE)
test <- subset(framingham, split==FALSE)

framinghamLog <- glm(TenYearCHD ~ ., data = train, family = binomial)
summary(framinghamLog)

predictTest1 <- predict(framinghamLog, type = "response", newdata = test)
table(test$TenYearCHD, predictTest1 > 0.5)

(1069+11)/(1069+6+187+11)

(1069+6)/(1069+6+187+11)
#baseline accuracy

library(ROCR)
ROCRPred1 <- prediction(predictTest1, test$TenYearCHD)
as.numeric(performance(ROCRPred1, "auc")@y.values)

11 / (187+11)
1069 / (1069+6)
