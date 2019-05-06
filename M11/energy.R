energy <- read.csv("energy_readings.csv", stringsAsFactors = FALSE)
nrow(energy)

library(tm)
library(SnowballC)

corpus <- Corpus(VectorSource(energy$email))
corpus <- tm_map(corpus, tolower)
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeWords, stopwords("english"))
corpus <- tm_map(corpus, stemDocument)

dtm <- DocumentTermMatrix(corpus)
dtm

spdtm <- removeSparseTerms(dtm, 0.97)
spdtm

library(rpart)
library(rpart.plot)
library(caTools)

newFrame <- as.data.frame(as.matrix(spdtm))
colnames(newFrame) <- make.names(colnames(newFrame))
str(newFrame)

newFrame$responsive <- energy$responsive

set.seed(1500)
split <- sample.split(newFrame$responsive, SplitRatio = 0.75)
Train <- subset(newFrame, split == TRUE)
Test <- subset(newFrame, split == FALSE)

CARTModel1 <- rpart(responsive ~ ., data = Train, method = "class")
predictCART1 <- predict(CARTModel1, newdata = Test)
predictCART1[1:10,]

tbl1 <- table(Test$responsive, predictCART1[,2] > 0.5)
tbl1
tbl1[4] / nrow(Test)

tbl2 <- table(Test$responsive, predictCART1[,2] > 0.7)
tbl2
tbl2[4] / nrow(Test)

tbl3 <- table(Test$responsive, predictCART1[,2] > 0.9)
tbl3
tbl3[2] / nrow(Test)

tbl4 <- table(Test$responsive, predictCART1[,2] > 0.6)
tbl4
sum(diag(tbl4)) / nrow(Test)

tbl5 <- table(Test$responsive, predictCART1[,2] > 0.8)
tbl5

library(ROCR)
predROCR <- prediction(predictCART1[,2], Test$responsive)
perfROCR <- performance(predROCR, "tpr", "fpr")
plot(perfROCR, colorize = TRUE)
auc <- as.numeric(performance(predROCR, "auc")@y.values)
auc
abline(h = auc)
