FluTrain <- read.csv("FluTrain.csv")
str(FluTrain)

hist(FluTrain$ILI, main = "Plot of ILI", xlab = "ILI")

# A "skewed right" distribution is one in which the tail is on the right side.
# A "skewed left" distribution is one in which the tail is on the left side.
# Most of the ILI values are small, with a relatively small number of much larger values 
# (in statistics, this sort of data is called "skew right")

plot(log(FluTrain$ILI), FluTrain$Queries, main = "Plot between ILI and Queries", xlab = "ILI", ylab = "Queries")
# There is a positive, linear relationship between log(ILI) and Queries.

FluModel <- lm(log(ILI) ~ Queries, data = FluTrain)
summary(FluModel)
# log(ILI) = intercept + coefficient x Queries, where the coefficient is positive
# R^2 = 0.709

cor.test(log(FluTrain$ILI), FluTrain$Queries)
# correlation coefficient = 0.8420333
cor2 <- cor(log(FluTrain$ILI), FluTrain$Queries)
cor2^2
# 0.7090201

FluTest <- read.csv("FluTest.csv")

which(FluTest$Week=="2012-03-11 - 2012-03-17")
PredTest1[11]
# 2.187378

(FluTest$ILI[11] - PredTest1[11])/FluTest$ILI[11]
# 0.04623827

SSE8 <- sum((PredTest1 - FluTest$ILI)^2)
RMSE8 <- sqrt(SSE8/nrow(FluTest))
RMSE8
# 0.7490645

install.packages("zoo")
library(zoo)
ILILag2 <- lag(zoo(FluTrain$ILI), -2, na.pad = TRUE)
FluTrain$ILILag2 <- coredata(ILILag2)
# the value of -2 passed to lag means to return 2 observations before the current one
# The parameter na.pad=TRUE means to add missing values for the first two weeks of our dataset, where we can't compute the data from 2 weeks earlier
summary(FluTrain)
# 2

plot(log(FluTrain$ILILag2), log(FluTrain$ILI), main = "Plot of ILI vs ILILag2")
# There is a strong positive relationship between log(ILILag2) and log(ILI)

FluTrend2 <- lm(log(ILI) ~ Queries + log(ILILag2), data = FluTrain)
summary(FluTrend2)
summary(FluModel)
# sum(is.na(ILILag2))
# Intercept
# Queries
# log(ILILag2)
# R^2 = 0.9063
# FluTrend2 is a stronger model than FluTrend1 on the training set.
# because R^2 value for the FluTrend2 model is more than FluModel.

ILILag2 <- lag(zoo(FluTest$ILI), -2, na.pad = TRUE)
FluTest$ILILag2 <- coredata(ILILag2)
sum(is.na(FluTest$ILILag2))
# 2
# The ILI value of the second-to-last observation in the FluTrain data frame
# The ILI value of the last observation in the FluTrain data frame

FluTest$ILILag2[1] <- FluTrain$ILI[416]
FluTest$ILILag2[2] <- FluTrain$ILI[417]
# FluTest[c(1,2),"ILILag2"] <- FluTrain[c(416,417),"ILI"]
FluTest$ILILag2[1]
# 1.852736
FluTest$ILILag2[2]
# 2.12413

PredTest2 <- exp(predict(FluTrend2, newdata = FluTest))
SSE9 <- sum((PredTest2 - FluTest$ILI)^2)
RMSE9 <- sqrt(SSE9/nrow(FluTest))
RMSE9
# 0.2942029

# FluTrend2
# because the less RMSE, the better the model makes predictions.