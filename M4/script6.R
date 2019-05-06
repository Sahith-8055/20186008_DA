quality <- read.csv("quality.csv")
str(quality)

table(quality$PoorCare)

98/131
# Baseline accuracy is around 75% since the number of patients receiving good care are more than the number of patients receiving poor care.

install.packages("caTools")
library(caTools)
set.seed(88)
split <- sample.split(quality$PoorCare, SplitRatio = 0.75)
# the function sample split uses to split the data into training and testing sets.
# we need to set the seed so that it initializes the random number generator.
# the first argument in the outcome variable and the second variable is the percentage of data that we want in the training set.
# In this case, we put 75% of data in the training set, which will be used to build the model.
# sample split randomly splits the data but it also makes sure that the outcome varible is well balanced in each piece.

split
# after the output is displayed, if we see true, then we need to put that observation into training set.
# if we see false, we need to put that observation into the testing set.

qualityTrain <- subset(quality, split==TRUE)
qualityTest <- subset(quality, split==FALSE)

nrow(qualityTrain)
nrow(qualityTest)

QualityLog <- glm(PoorCare ~ OfficeVisits + Narcotics, data = qualityTrain, family = binomial)
summary(QualityLog)

predictTrain <- predict(QualityLog, type = "response")
summary(predictTrain)

tapply(predictTrain, qualityTrain$PoorCare, mean)

table(qualityTrain$PoorCare, predictTrain > 0.5)

10 / 25
70 / 74

table(qualityTrain$PoorCare, predictTrain > 0.7)
8/25
73/74

table(qualityTrain$PoorCare, predictTrain > 0.2)
16/25
54/74

20/25
15/25

install.packages("ROCR")
library(ROCR)
ROCRPred <- prediction(predictTrain, qualityTrain$PoorCare)
ROCRPerf <- performance(ROCRPred, "tpr", "fpr")
plot(ROCRPerf, colorize=TRUE, print.cutoffs.at=seq(0,1,0.1),text.adj=c(-0.2,1.7))

predictTest <- predict(QualityLog, type = "response", newdata = qualityTest)
ROCPredTest <- prediction(predictTest, qualityTest$PoorCare)
auc <- as.numeric(performance(ROCPredTest, "auc")@y.values)
auc
