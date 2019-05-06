pisaTrain <- read.csv("pisa2009train.csv")
pisaTest <- read.csv("pisa2009test.csv")

str(pisaTrain)
# 3663

tapply(pisaTrain$readingScore,pisaTrain$male,mean)
#483.5325

sapply(pisaTrain, function(x) sum(is.na(x)))
# raceeth
# preschool
# motherHS
# motherBachelors
# fatherHS
# fatherBachelors
# fatherWork
# fatherBornUS
# studentsInEnglish
# expectBachelors
# motherWork
# selfBornUS
# motherBornUS
# englishAtHome
# computerForSchoolwork
# read30MinsADay
# minutesPerWeekEnglish
# schoolHasLibrary
# schoolSize

pisaTrain = na.omit(pisaTrain)
pisaTest = na.omit(pisaTest)
str(pisaTrain)
# 2414

str(pisaTrain)
# raceeth

str(pisaTrain)
# grade

str(pisaTrain)
#raceethAmerican Indian/Alaska Native
#raceethAsian
#raceethBlack
#raceethHispanic
#raceethMore than one race
#raceethNative Hawaiian/Other Pacific Islander
#raceethWhite

#raceethAmerican Indian/Alaska Native
#raceethBlack
#raceethHispanic
#raceethMore than one race
#raceethNative Hawaiian/Other Pacific Islander

# raceethAmerican Indian/Alaska Native
# raceethAsian
# raceethBlack
# raceethHispanic
# raceethMore than one race
# raceethNative Hawaiian/Other Pacific Islander

pisaTrain$raceeth = relevel(pisaTrain$raceeth, "White")
pisaTest$raceeth = relevel(pisaTest$raceeth, "White")

ImScore <- lm(readingScore ~ ., data = pisaTrain)
summary(ImScore)
# 0.3251

SSE5 <- sum(ImScore$residuals^2)
RMSE5 <- sqrt(SSE5/nrow(pisaTrain))
RMSE5
# 73.36555
# RMSE6 <- sqrt(mean(ImScore$residuals^2))
# RMSE6

redTest <- predict(ImScore, newdata = pisaTest)
summary(redTest)
# 284.5

SSE7 <- sum((redTest - pisaTest$readingScore)^2)
SSE7
# 5762082

RMSE7 <- sqrt(SSE7/nrow(pisaTest))
RMSE7
# 76.29079

base2 <- mean(pisaTrain$readingScore)
base2
# 517.9629

SST2 <- sum((base2 - pisaTest$readingScore)^2)
SST2
# 7802354

R3 <- 1 - SSE7/SST2
R3
# 0.2614944