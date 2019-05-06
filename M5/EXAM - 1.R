machine <- read.csv("machine.csv")
str(machine)

# 1) Compute the model R2 (the "Multiple R-squared" value)? Consider the entire dataset as training dataset
model1 <- lm(PRP ~ X + MYCT + MMIN + MMAX + CACH + CHMIN + CHMAX, data = machine)
summary(model1)
# Multiple R-squared:   0.87

# 2) The significant variables in this model are:
# MYCT, MMIN, MMAX, CACH, CHMAX.

# 3) Compute the correlations between all the variables in the training set. Which of the following independent variables is MMAX highly correlated with (absolute correlation greater than 0.7)?
cor(machine[c("MYCT", "MMIN", "MMAX", "CACH", "CHMAX", "PRP", "X")])
# MMAX is highly correlated with:
# MMIN

# 4) Which of the independent variable is highly correlated with PRP?
# MMAX, MMIN, CACH.

# 5) Given that the correlations are so high, let us focus on the MMAX variable and build a model with only variables which have correlation is between -0.3 to 0.3 with MMAX. Compute the coefficient of MMAX in this reduced model.
model2 <- lm(PRP ~ MMAX + MYCT + X, data = machine)
summary(model2)
# coefficient of MMAX in the reduced variable is 1.201e-02.

# 6) Compute the above reduced model R2
# Multiple R-squared:  0.7503.

#7) Compute the R2 value of the model produced by the step function.
model3 <- step(model2)
# Multiple R-squared:  0.8697

# 8) Split your data set into train and test sets(75: 25). Compute the testing set R2 using the model produced from the step function on trained data set.
library(caTools)
set.seed(6)
sample4 <- sample.split(machine, SplitRatio = 0.75)
machineTrain <- subset(machine, sample4 == TRUE)
machineTest <- subset(machine, sample4 == FALSE)
model4 <- lm(PRP ~ X + MYCT + MMIN + MMAX + CACH + CHMIN + CHMAX, data = machineTrain)
model5 <- step(model4)
predictModel <- predict(model5, newdata = machineTest)
SSE <- sum((predictModel - machineTest$PRP)^2)
base <- mean(machineTrain$PRP)
SST <- sum((base - machineTest$PRP)^2)
R2 <- 1 - SSE/SST
R2
# The value of R2 is 0.8458878.