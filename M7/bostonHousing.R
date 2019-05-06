boston <- read.csv("boston.csv")
str(boston)

plot(boston$LON, boston$LAT)
points(boston$LON[boston$CHAS==1], boston$LAT[boston$CHAS==1], col="blue", pch=19)
points(boston$LON[boston$TRACT==3531], boston$LAT[boston$TRACT==3531], col="red", pch=19)
summary(boston$NOX)
points(boston$LON[boston$NOX >= 0.55], boston$LAT[boston$NOX >= 0.55], col="green", pch=19)

plot(boston$LON, boston$LAT)
summary(boston$MEDV)
points(boston$LON[boston$MEDV >= 21.2], boston$LAT[boston$MEDV >= 21.2], col="red", pch=19)

plot(boston$LAT, boston$MEDV)
plot(boston$LON, boston$MEDV)
latlonlm <- lm(MEDV ~ LAT + LON, data = boston)
summary(latlonlm)

plot(boston$LON, boston$LAT)
points(boston$LON[boston$MEDV >= 21.2], boston$LAT[boston$MEDV >= 21.2], col="red", pch=19)
latlonlm$fitted.values
points(boston$LON[latlonlm$fitted.values >= 21.2], boston$LAT[latlonlm$fitted.values >= 21.2], col="blue", pch="$")

library(rpart)
library(rpart.plot)
latlontree <- rpart(MEDV ~ LAT + LON, data = boston)
prp(latlontree)
plot(boston$LON, boston$LAT)
points(boston$LON[boston$MEDV >= 21.2], boston$LAT[boston$MEDV >= 21.2], col="red", pch=19)
fittedvalues <- predict(latlontree)
points(boston$LON[fittedvalues >= 21.2], boston$LAT[fittedvalues >= 21.2], col="blue", pch="$")
latlontree1 <- rpart(MEDV ~ LAT + LON, data = boston, minbucket=50)
plot(latlontree1)
text(latlontree1)
plot(boston$LON, boston$LAT)
abline(v=-71.07)
abline(h=42.21)
abline(h=42.17)
points(boston$LON[boston$MEDV >= 21.2], boston$LAT[boston$MEDV >= 21.2], col="red", pch=19)

library(caTools)
set.seed(123)
split <- sample.split(boston$MEDV, SplitRatio = 0.7)
Train <- subset(boston, split==TRUE)
Test <- subset(boston, split==FALSE)
linreg <- lm(MEDV ~ LON + LAT + CRIM + ZN + INDUS + CHAS + NOX + RM + AGE + DIS + RAD + TAX + PTRATIO, data = Train)
summary(linreg)
linreg.pred <- predict(linreg, newdata = Test)
linreg.sse <- sum((linreg.pred - Test$MEDV)^2)

tree <- rpart(MEDV ~ LON + LAT + CRIM + ZN + INDUS + CHAS + NOX + RM + AGE + DIS + RAD + TAX + PTRATIO, data = Train)
prp(tree)
tree.pred <- predict(tree, newdata = Test)
tree.sse <- sum((tree.pred - Test$MEDV)^2)
tree.sse

library(caret)
library(e1071)

tr.control <- trainControl(method = "cv", number = 10)
cp.grid <- expand.grid(.cp = (0:10) * 0.001)
cp.grid
remove(train)
remove(test)
tr <- train(MEDV ~ LON + LAT + CRIM + ZN + INDUS + CHAS + NOX + RM + AGE + DIS + RAD + TAX + PTRATIO, data = Train, method="rpart", trControl = tr.control, tuneGrid = cp.grid)
tr
best.tree <- tr$finalModel
prp(best.tree)
best.tree.pred <- predict(best.tree, newdata = Test)
best.tree.sse <- sum((best.tree.pred - Test$MEDV)^2)
best.tree.sse
