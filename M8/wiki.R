wiki <- read.csv("wiki.csv", stringsAsFactors = FALSE)
str(wiki)
wiki$Vandal
wiki$Vandal <- as.factor(wiki$Vandal)
table(wiki$Vandal)
# There are 1815 cases of Vandalism that are detected.

library(tm)
library(SnowballC)

corpusAdded <- Corpus(VectorSource(wiki$Added))
corpusAdded <- tm_map(corpusAdded, removeWords, stopwords("english"))
corpusAdded <- tm_map(corpusAdded, stemDocument)
dtmAdded <- DocumentTermMatrix(corpusAdded)
length(stopwords("english"))
dtmAdded

sparseAdded <- removeSparseTerms(dtmAdded, 0.997)
sparseAdded

wordsAdded <- as.data.frame(as.matrix(sparseAdded))
colnames(wordsAdded) <- paste("A", colnames(wordsAdded))

corpusRemoved <- Corpus(VectorSource(wiki$Removed))
corpusRemoved <- tm_map(corpusRemoved, removeWords, stopwords("english"))
corpusRemoved <- tm_map(corpusRemoved, stemDocument)
dtmRemoved <- DocumentTermMatrix(corpusRemoved)

sparseRemoved <- removeSparseTerms(dtmRemoved, 0.997)

wordsRemoved <- as.data.frame(as.matrix(sparseRemoved))
colnames(wordsRemoved) <- paste("R", colnames(wordsRemoved))
str(wordsRemoved)
# There are 162 words in the wordsRemoved data frame

wikiWords <- cbind(wordsAdded, wordsRemoved)
wikiWords$Vandal <- wiki$Vandal
library(caTools)
set.seed(123)
split <- sample.split(wikiWords$Vandal, SplitRatio = 0.7)
Train <- subset(wikiWords, split==TRUE)
Test <- subset(wikiWords, split==FALSE)
table(Test$Vandal)
618 / (618+545)
# The accuracy of the baseline model which predicts "Not Vandalism" is
# 0.5313844

library(rpart)
library(rpart.plot)
CARTModel <- rpart(Vandal ~ ., data = Train, method = "class")
predictCART <- predict(CARTModel, newdata = Test, type = "class")
tbl1 <- table(Test$Vandal, predictCART)
# The accuracy of the model is
sum(diag(tbl1)) / nrow(Test)
# 0.544282

prp(CARTModel)
# There are 3 stems.

wikiWords2 <- wikiWords
wikiWords2$HTTP <- ifelse(grepl("http", wiki$Added, fixed = TRUE),1,0)
table(wikiWords2$HTTP)
# There are 217 revisions who added a link.

Train2 <- subset(wikiWords2, split==TRUE)
Test2 <- subset(wikiWords2, split==FALSE)
CARTModel2 <- rpart(Vandal ~ ., data = Train2, method = "class")
predictCART2 <- predict(CARTModel2, newdata = Test2, type = "class")
tbl2 <- table(Test2$Vandal, predictCART2)
tbl2
# The accuracy of the current model is
sum(diag(tbl2)) / nrow(Test2)
# 0.5752365

wikiWords2$NumWordsAdded <- rowSums(as.matrix(dtmAdded))
wikiWords2$NumWordsRemoved <- rowSums(as.matrix(dtmRemoved))
# The average number of words that are added is
mean(wikiWords2$NumWordsAdded)
# 4.050052

Train3 <- subset(wikiWords2, split==TRUE)
Test3 <- subset(wikiWords2, split==FALSE)
CARTModel3 <- rpart(Vandal ~ ., data = Train3, method = "class")
predictCART3 <- predict(CARTModel3, newdata = Test3, type = "class")
tbl3 <- table(Test3$Vandal, predictCART3)
# The accuracy of the new model is
sum(diag(tbl3)) / nrow(Test3)
# 0.6552021


wikiWords3 <- wikiWords2
wikiWords3$Minor <- wiki$Minor
wikiWords3$Loggedin <- wiki$Loggedin
Train4 <- subset(wikiWords3, split==TRUE)
Test4 <- subset(wikiWords3, split==FALSE)
CARTModel4 <- rpart(Vandal ~ ., data = Train4, method = "class")
predictCART4 <- predict(CARTModel4, newdata = Test4, type = "class")
tbl4 <- table(Test4$Vandal, predictCART4)
# The accuracy of the model is
sum(diag(tbl4)) / nrow(Test4)
# 0.7188306

prp(CARTModel4)
# There are 3 splits in this model.