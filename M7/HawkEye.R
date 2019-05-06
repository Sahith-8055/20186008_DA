claims <- read.csv("ClaimsData.csv")
str(claims)
table(claims$bucket2009)/nrow(claims)

library(caTools)
set.seed(88)
spl <- sample.split(claims$bucket2009, SplitRatio = 0.6)
train <- subset(claims, spl==TRUE) 
test <- subset(claims, spl==FALSE)

tbl <- table(test$bucket2009, test$bucket2008)
sum(diag(tbl))
sum(tbl)
sum(diag(tbl)) / nrow(test)
# accuracy of the model is 0.6838135.

PenaltyMatrix <- matrix(c(0,1,2,3,4,2,0,1,2,3,4,2,0,1,2,6,4,2,0,1,8,6,4,2,0), byrow = TRUE, nrow = 5)
PenaltyMatrix

as.matrix(table(test$bucket2009, test$bucket2008)) * PenaltyMatrix
sum(as.matrix(table(test$bucket2009, test$bucket2008)) * PenaltyMatrix) / nrow(test)
# penalty error is 0.7386055.

library(rpart)
library(rpart.plot)

claimsTree <- rpart(bucket2009 ~ age + alzheimers + arthritis + cancer + copd + depression + diabetes + heart.failure + ihd + kidney + osteoporosis + stroke + bucket2008 + reimbursement2008, data = train, method = "class", cp=0.00005, parms = list(loss=PenaltyMatrix))
prp(claimsTree)

PredictTest <- predict(claimsTree, newdata=test, type="class")
tbl2 <- table(test$bucket2009, PredictTest)
sum(diag(tbl2)) / nrow(test)
# accuracy of this model is 0.6472746.
as.matrix(table(test$bucket2009, PredictTest)) * PenaltyMatrix
sum(as.matrix(table(test$bucket2009, PredictTest)) * PenaltyMatrix) / nrow(test)
# penalty error is 0.6418161.

