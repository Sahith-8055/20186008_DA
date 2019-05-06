emails <- read.csv("emails.csv", stringsAsFactors = FALSE)

str(emails)
nrow(emails)
# There are 5728 emails.

table(emails$spam)
emails$text[1]
# The word "subject" appears.

max(nchar(emails$text))
# 43952 characters

which.min(nchar(emails$text))
# 1992th row.

library(tm)
library(SnowballC)

corpus <- Corpus(VectorSource(emails$text))
corpus <- tm_map(corpus, tolower)
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeWords, stopwords("english"))
corpus <- tm_map(corpus, stemDocument)
dtm <- DocumentTermMatrix(corpus)
dtm
# There are 28687 terms in the dtm

spdtm <- removeSparseTerms(dtm, 0.95)
spdtm
# There are 330 terms in spdtm

emailsSparse <- as.data.frame(as.matrix(spdtm))
colnames(emailsSparse) <- make.names(colnames(emailsSparse))
which.max(colSums(emailsSparse))
# enron is the stem word that had come maximum number of times

emailsSparse$spam <- emails$spam
a <- subset(emailsSparse, spam == 0)
table(colSums(a) >= 5000)
# There are 6 stem words that appear more than 5000 times in the ham mails.

b <- subset(emailsSparse, spam == 1)
sort(colSums(b))
# There are 3 stem words that appear more than 1000 times in the spam mails.


emailsSparse$spam <- as.factor(emailsSparse$spam)
set.seed(123)
library(caTools)
split <- sample.split(emailsSparse$spam, SplitRatio = 0.7)
Train <- subset(emailsSparse, split==TRUE)
Test <- subset(emailsSparse, split==FALSE)

spamLog <- glm(spam ~ ., data = Train, family = "binomial")
library(rpart)
library(rpart.plot)
spamCART <- rpart(spam ~ ., data = Train, method = "class")
library(randomForest)
set.seed(123)
spamRF <- randomForest(spam ~ ., data = Train)

predTrainLog <- predict(spamLog, type = "response")
predTrainCART <- predict(spamCART)[,2]
predictTrainRF <- predict(spamRF, type = "prob")[,2]

table(predTrainLog < 0.00001)
# 3046
table(predTrainLog > 0.99999)
# 954
table(predTrainLog >= 0.00001 & predTrainLog <= 0.99999)
# 10

summary(spamLog)
# 0 variables are having p <= 0.05.
prp(spamCART)
# 2 words

tbl1 <- table(Train$spam, predTrainLog >= 0.5)
tbl1
sum(diag(tbl1)) / nrow(Train)
# accuracy of the model is 0.9990025

library(ROCR)
predictROCR <- prediction(predTrainLog, Train$spam)
auc <- as.numeric(performance(predictROCR, "auc")@y.values)
auc
# The value of auc is 0.9999959

tbl2 <- table(Train$spam, predTrainCART >= 0.5)
sum(diag(tbl2)) / nrow(Train)
# The training set accuracy is 0.942394.

predictROCR2 <- prediction(predTrainCART, Train$spam)
auc2 <- as.numeric(performance(predictROCR2, "auc")@y.values)
auc2
# AUC is 0.9696044

tbl3 <- table(Train$spam, predictTrainRF >= 0.5)
tbl3
sum(diag(tbl3)) / nrow(Train)
# Accuracy is 0.9810474

predictionROCR3 <- prediction(predictTrainRF, Train$spam)
auc3 <- as.numeric(performance(predictionROCR3, "auc")@y.values)
auc3
# 0.9978952

predTestLog <- predict(spamLog, newdata = Test, type = "response")
tbl4 <- table(Test$spam, predTestLog >= 0.5)
sum(diag(tbl4)) / nrow(Test)
# Accuracy is 0.9505239.
predictTestROCR <- prediction(predTestLog, Test$spam)
auc4 <- as.numeric(performance(predictTestROCR, "auc")@y.values)
auc4
# AUC is 0.9627517.

predTestCART <- predict(spamCART, newdata = Test)[,2]
tbl5 <- table(Test$spam, predTestCART >= 0.5)
sum(diag(tbl5)) / nrow(Test)
# Accuracy is 0.9394645
predictTestROCR2 <- prediction(predTestCART, Test$spam)
auc5 <- as.numeric(performance(predictTestROCR2, "auc")@y.values)
auc5
# AUC is 0.963176.

predTestRF <- predict(spamRF, newdata = Test, type = "prob")[,2]
tbl6 <- table(Test$spam, predTestRF >= 0.5)
tbl6
sum(diag(tbl6)) / nrow(Test)
# Accuracy of the model is 0.9749709.
predictTestROCR3 <- prediction(predTestRF, Test$spam)
auc6 <- as.numeric(performance(predictTestROCR3, "auc")@y.values)
auc6
# AUC is 0.997768

wordCount <- rowSums(as.matrix(dtm))
wordCount
hist(wordCount)
hist(log(wordCount))

emailsSparse$logWordCount <- log(wordCount)
boxplot(emailsSparse$logWordCount, emailsSparse$spam == 1)

Train2 <- subset(emailsSparse, split==TRUE)
Test2 <- subset(emailsSparse, split==FALSE)

spam2CART <- rpart(spam ~ ., data = Train2, method = "class")
set.seed(123)
spam2RF <- randomForest(spam ~ ., data = Train2)
prp(spam2CART)

predictTest2CART <- predict(spam2CART, newdata = Test2)
tbl7 <- table(Test2$spam, predictTest2CART[,2] >= 0.5)
tbl7
sum(diag(tbl7)) / nrow(Test2)
# Accuracy of the model is 0.9301513
newPredictROCR <- prediction(predictTest2CART[,2], Test2$spam)
auc7 <- as.numeric(performance(newPredictROCR, "auc")@y.values)
auc7
# AUC value is 0.9582438

set.seed(123)
predictTest2Forest <- predict(spam2RF, newdata = Test2, type = "prob")
tbl8 <- table(Test2$spam, predictTest2Forest[,2] >= 0.5)
tbl8
sum(diag(tbl8)) / nrow(Test2)
# Accuracy of the model is 0.9790454.
newPredictROCR2 <- prediction(predictTest2Forest[,2], Test2$spam)
auc8 <- as.numeric(performance(newPredictROCR2, "auc")@y.values)
auc8
# The value of AUC is 0.998084.
