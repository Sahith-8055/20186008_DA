rm(list = ls(all = TRUE))
parole <- read.csv("parole.csv")
str(parole)
nrow(parole)
# 675 observations

table(parole$violator)
# 78 members violated the terms of the parole
install.packages("knitr")
library(knitr)
install.packages("kableExtra")
library(kableExtra)
z <- table(parole$violator)
x <- kable(z, "latex", row.names = F, align = c(rep("1", 2), rep("r"), 2))
column_spec(x, 1:2, width = "4cm", bold = TRUE, italic = TRUE)

table(parole$multiple.offenses)
table(parole$crime)
table(parole$state)

parole$multiple.offenses <- as.factor(parole$multiple.offenses)
summary(parole$multiple.offenses)

parole$crime <- as.factor(parole$crime)
summary(parole$crime)

parole$state <- as.factor(parole$state)
summary(parole$state)

set.seed(144)
library(caTools)
split <- sample.split(parole$violator, SplitRatio = 0.7)
train <- subset(parole, split==TRUE)
test <- subset(parole, split==FALSE)
model1 <- glm(violator ~ ., data = train, family = "binomial")
summary(model1)

exp(1.6119919)
# Odds = 5.012786

male <- 1
age <- 50
race <- 1
time.served <- 3
max.sentence <- 12
crime2 <- 1
logit <- -4.2411574 + 0.3869904*male - 0.0001756*age + 0.8867192*race -0.1238867*time.served + 0.0802954*max.sentence + 0.6837143*crime2
logit
odds <- exp(logit)
odds
# 0.1825687

p <- 1/(1+(1/odds))
p
# 0.1543832

predictParole <- predict(model1, newdata = test, type = "response")
summary(predictParole)
# 0.907279

table(test$violator, predictParole > 0.5)
# sensitivity = 12/23 = 0.5217391 (TP / TP + FN)
# specificity = 167/(167+12) = 0.9329609 (TN / TN + FP)
# overall accuracy of the model is (167+12)/(167+12+11+12) = 0.8861386 (TN + TP) / N
table(test$violator)
179/(179+23)
# accuracy of the baseline model where every parolee is a non-violator is 0.8861386
# The board assigns more cost to a false negative than a false positive, and should therefore use a logistic regression cutoff less than 0.5.
# The model is likely of value to the board, and using a different logistic regression cutoff is likely to improve the model's value.

library(ROCR)
pred <- prediction(predictParole, test$violator)
auc <- as.numeric(performance(pred, "auc")@y.values)
auc
# the value of auc is 0.8945834.
