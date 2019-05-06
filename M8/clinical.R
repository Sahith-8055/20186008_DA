clinical <- read.csv("clinical_trial.csv", stringsAsFactors = FALSE)
str(clinical)
summary(clinical)

max(nchar(clinical$abstract))
# The longest abstract consists of 3708 characters.

sum(clinical$abstract=="")
table(nchar(clinical$abstract) == 0)
sum(nchar(clinical$abstract) == 0)
# The number of search results that provided no abstract are 112

clinical$title[which.min(nchar(clinical$title))]
# A decade of letrozole: FACE.

library(tm)
library(SnowballC)

corpusTitle <- Corpus(VectorSource(clinical$title))
corpusAbstract <- Corpus(VectorSource(clinical$abstract))

corpusTitle <- tm_map(corpusTitle, tolower)
corpusAbstract <- tm_map(corpusAbstract, tolower)

corpusTitle <- tm_map(corpusTitle, removePunctuation)
corpusAbstract <- tm_map(corpusAbstract, removePunctuation)

corpusTitle <- tm_map(corpusTitle, removeWords, stopwords("english"))
corpusAbstract <- tm_map(corpusAbstract, removeWords, stopwords("english"))

corpusTitle <- tm_map(corpusTitle, stemDocument)
corpusAbstract <- tm_map(corpusAbstract, stemDocument)

dtmTitle <- DocumentTermMatrix(corpusTitle)
dtmAbstract <- DocumentTermMatrix(corpusAbstract)

sparseTitle <- removeSparseTerms(dtmTitle, 0.95)
sparseAbstract <- removeSparseTerms(dtmAbstract, 0.95)

dtmTitle <- as.data.frame(as.matrix(sparseTitle))
dtmAbstract <- as.data.frame(as.matrix(sparseAbstract))

str(dtmTitle)
str(dtmAbstract)
dim(dtmTitle)
dim(dtmAbstract)
ncol(dtmTitle)
ncol(dtmAbstract)
# There are 31 columns in dtmTitle and 335 columns in dtmAbstract.

which.max(colSums(dtmAbstract))
# patient is the word that is occuring across all the abstracts.

# Use of paste0() ?
colnames(dtmTitle) <- paste0("T", colnames(dtmTitle))
colnames(dtmAbstract) <- paste0("A", colnames(dtmAbstract))
# Adding the letter T in front of all the title variable names and adding the letter A in front of all the abstract variable names.

dtm <- cbind(dtmTitle, dtmAbstract)
dtm$trial <- clinical$trial
ncol(dtm)
# There are 367 columns in the new data frame.

library(caTools)
set.seed(144)
split <- sample.split(dtm$trial, SplitRatio = 0.7)
Train <- subset(dtm, split==TRUE)
Test <- subset(dtm, split==FALSE)

table(Train$trial)
# baseline model predicts the most frequent outcome in the training set for all observations
table(Train$trial)[1] / sum(table(Train$trial))
# The accuracy of the baseline model is 0.5606759.

library(rpart)
library(rpart.plot)
trialCART <- rpart(trial ~ ., data = Train, method = "class")
prp(trialCART)

predTrain = predict(trialCART)[,2]
summary(predTrain)
# The maximum predicted probability is 0.87189.

tbl1 <- table(Train$trial, predTrain >= 0.5)
tbl1
sum(diag(tbl1)) / nrow(Train)
# The training set accuracy of the model is 0.8233487
tbl1[2,2] / sum(tbl1[2,1], tbl1[2,2])
# The sensitivity of the training set is 0.770979
tbl1[1] / sum(tbl1[1] + tbl1[3])
# The specificity of the training set is 0.8643836

predictCART <- predict(trialCART, newdata = Test)
tbl2 <- table(Test$trial, predictCART[,2] >= 0.5)
tbl2
sum(diag(tbl2)) / nrow(Test)
# The accuracy of the testing set model is 0.7580645

library(ROCR)
predictROCR <- prediction(predictCART[,2], Test$trial)
auc <- as.numeric(performance(predictROCR, "auc")@y.values)
auc
# The value of auc is 0.8371063.