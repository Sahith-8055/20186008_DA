library(ggplot2)
library(ggmap)

statesMap <- map_data("state")
str(statesMap)

table(statesMap$group)
length(table(statesMap$group))
# There are 63 different groups.

ggplot(statesMap, aes(x = long, y = lat, group = group)) + geom_polygon(fill = "white", color = "black")
# color attribute defines the outline for the states

polling <- read.csv("PollingImputed.csv")
str(polling)

Train <- subset(polling, Year == 2004 | Year == 2008)
str(Train)

Test <- subset(polling, Year == 2012)
str(Test)

mod2 <- glm(Republican ~ SurveyUSA + DiffCount, data = Train, family = "binomial")
TestPrediction <- predict(mod2, newdata = Test, type = "response")

TestPredictionBinary <- as.numeric(TestPrediction > 0.5)
predictionDataFrame <- data.frame(TestPrediction, TestPredictionBinary, Test$State)

table(TestPredictionBinary)
mean(TestPredictionBinary)
# The number of predictions for republican win is 22
# The mean predicted probability of the model is 0.4888889

predictionDataFrame$region <- tolower(predictionDataFrame$Test.State)

predictionMap <- merge(statesMap, predictionDataFrame, by = "region")
predictionMap <- predictionMap[order(predictionMap$order),]

nrow(predictionMap)
nrow(statesMap)

ggplot(predictionMap, aes(x = long, y = lat, group = group, fill = TestPredictionBinary)) + geom_polygon(color = "black")

ggplot(predictionMap, aes(x = long, y = lat, group = group, fill = TestPredictionBinary)) + geom_polygon(color = "black") + scale_fill_gradient(low = "blue", high = "red", guide = "legend", breaks = c(0,1), labels = c("Democrat", "Republican"), name = "Prediction 2012")

ggplot(predictionMap, aes(x = long, y = lat, group = group, fill = TestPrediction)) + geom_polygon(color = "black") + scale_fill_gradient(low = "blue", high = "red", guide = "legend", breaks = c(0,1), labels = c("Democrat", "Republican"), name = "Prediction 2012")

table(TestPredictionBinary)
predictionDataFrame

ggplot(predictionMap, aes(x = long, y = lat, group = group, fill = TestPrediction)) + geom_polygon(color = "black", linetype = 3) + scale_fill_gradient(low = "blue", high = "red", guide = "legend", breaks = c(0,1), labels = c("Democrat", "Republican"), name = "Prediction 2012")
# linetype is used to alter the type of the line.

ggplot(predictionMap, aes(x = long, y = lat, group = group, fill = TestPrediction)) + geom_polygon(color = "black", size = 3) + scale_fill_gradient(low = "blue", high = "red", guide = "legend", breaks = c(0,1), labels = c("Democrat", "Republican"), name = "Prediction 2012")
# size is used to either increase / decrease the linewidth. 

ggplot(predictionMap, aes(x = long, y = lat, group = group, fill = TestPrediction)) + geom_polygon(color = "black", alpha = 0.3) + scale_fill_gradient(low = "blue", high = "red", guide = "legend", breaks = c(0,1), labels = c("Democrat", "Republican"), name = "Prediction 2012")
# alpha is used to increase (or) decrease the transparency of the ggplot.


