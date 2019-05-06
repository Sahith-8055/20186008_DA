emails <- read.csv("energy_bids.csv", stringsAsFactors = FALSE)
str(emails)
emails$email[1]
strwrap(emails$email[1])
emails$responsive[1]
strwrap(emails$email[2])
emails$responsive[2]
table(emails$responsive)
library(tm)
library(SnowballC)

corpus <- Corpus(VectorSource(emails$email))
strwrap(corpus[[1]])
corpus <- tm_map(corpus, tolower)
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeWords, stopwords("english"))
corpus <- tm_map(corpus, stemDocument)
strwrap(corpus[[1]])

dtm <- DocumentTermMatrix(corpus)
dtm
dtm <- removeSparseTerms(dtm, 0.97)
dtm
labeledTerms <- as.data.frame(as.matrix(dtm))
labeledTerms$Responsive <- emails$responsive
str(labeledTerms)

library(caTools)
set.seed(144)
split <- sample.split(labeledTerms$Responsive, SplitRatio = 0.7)
Train <- subset(labeledTerms, split==TRUE)
Test <- subset(labeledTerms, split==FALSE)
library(rpart)
library(rpart.plot)

emailCART <- rpart(Responsive ~ ., data = Train, method = "class")
prp(emailCART)

pred <- predict(emailCART, newdata = Test)
pred[1:10,]
# Since, we are only interested in the responsiveness, we opt for only second
# column.
tbl1 <- table(Test$Responsive, pred[,2] >= 0.5)
# Accuracy of the model is
sum(diag(tbl1)) / nrow(Test)
# 0.8560311

table(Test$Responsive)
# The accuracy of the baseline model (For non-responsiveness) is
215 / (215+42)
# 0.8365759

library(ROCR)
predROCR <- prediction(pred[,2], Test$Responsive)
perfROCR <- performance(predROCR, "tpr", "fpr")
plot(perfROCR, colorize=TRUE)
abline(v = 0.2)
abline(h = 0.75)

auc <- as.numeric(performance(predROCR, "auc")@y.values)
auc
# The value of auc is 0.7936323.