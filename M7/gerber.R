gerber <- read.csv("gerber.csv")
str(gerber)

# What proportion of people in this dataset voted in this election?
table(gerber$voting) / nrow(gerber)
# mean(gerber$voting)
# 0.3158996 (Around 32% of total people in this data set voted in this election)

# Which of the four "treatment groups" had the largest percentage of people who actually voted (voting = 1)?
table(gerber$civicduty[gerber$voting==1]) / length(gerber$civicduty[gerber$voting==1])
table(gerber$hawthorne[gerber$voting==1]) / length(gerber$hawthorne[gerber$voting==1])
table(gerber$self[gerber$voting==1]) / length(gerber$self[gerber$voting==1])
table(gerber$neighbors[gerber$voting==1]) / length(gerber$neighbors[gerber$voting==1])
# neighbour's group

model1 <- glm(voting ~ civicduty + hawthorne + neighbors + self, data = gerber, family = "binomial")
summary(model1)
pred <- predict(model1, type = "response")
table(gerber$voting, pred > 0.3)
(134513 + 51966) / (134513 + 100875 + 56730 + 51966)
# accuracy of the model when the threshold is > 0.3 is 0.5419578.

table(gerber$voting, pred > 0.5)
(235388) / (235388 + 108696)
# accuracy of the model when the threshold is > 0.5 is 0.6841004.

table(gerber$voting)
# The model is able to predict the baseline values only.
library(ROCR)
predictROC1 <- prediction(pred, gerber$voting)
auc <- as.numeric(performance(predictROC1, "auc")@y.values)
auc
# auc = 0.5308461
# Even though all of our variables are significant, our model does not improve over the baseline model of just predicting that someone will not vote, and the AUC is low.
# So while the treatment groups do make a difference, this is a weak predictive model.

library(rpart)
library(rpart.plot)
CARTmodel <- rpart(voting ~ civicduty + hawthorne + self + neighbors, data = gerber)
prp(CARTmodel)
# If you plot the tree, with prp(CARTmodel), you should just see one leaf! There are no splits in the tree
# because none of the variables make a big enough effect to be split on.

CARTmodel2 <- rpart(voting ~ civicduty + hawthorne + self + neighbors, data = gerber, cp=0.0)
prp(CARTmodel2)

# fraction of people who belong to the category of civic people and
# who voted is 0.31.

CARTmodel3 <- rpart(voting ~ civicduty + hawthorne + self + neighbors + sex, data = gerber, cp=0.0)
prp(CARTmodel3)
# among all the control groups, men are going to vote more than the woman.
# Among the people in the civic duty group, male people are going to vote more.

CARTmodel4 <- rpart(voting ~ control, data = gerber, cp=0.0)
prp(CARTmodel4, digits = 6)
abs(0.296638 - 0.34)
# absolute value of the difference in the predicted probability of voting 
# between being in the control group versus being in a different group is 0.043362.

CARTmodel5 <- rpart(voting ~ control + sex, data = gerber, cp=0.0)
prp(CARTmodel5, digits = 6)
# determine who is affected more by NOT being in the control group ?
# For women, not being in the control group increases the fraction voting by 0.04372.
# For men, not being in the control group increases the fraction voting by 0.04302.
# So men and women are affected about the same.


# Create a model using "sex" and "control". Interpret the coefficient for "sex"?
model2 <- glm(voting ~ sex + control, data = gerber, family = "binomial")
summary(model2)
# look at the summary of the model, you can see that the coefficient for the "sex" variable is -0.055791.
# This means that women are less likely to vote, since women have a larger value in the sex variable
# and a negative coefficient means that larger values are predictive of 0.

Possibilities <- data.frame(sex=c(0,0,1,1), control=c(0,1,0,1))
pred2 <- predict(model2, newdata = Possibilities, type = "response")
pred2
abs(0.290456 - 0.2908065)
# 0.0003505


model3 <- glm(voting ~ sex + control + sex:control, data = gerber, family = "binomial")
summary(model3)

predict(model3, newdata = Possibilities, type = "response")
abs(0.2904558 - 0.290456)
# 0.

# Should we always include all possible interaction terms of the independent variables when building a logistic regression model?
# We should not use all possible interaction terms in a logistic regression model due to overfitting.