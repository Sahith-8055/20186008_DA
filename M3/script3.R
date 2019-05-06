NBA <- read.csv("NBA_train.csv")
str(NBA)
table(NBA$W, NBA$Playoffs)
NBA$PTSdiff <- NBA$PTS - NBA$oppPTS
plot(NBA$PTSdiff, NBA$W)
WinsReg2 <- lm(W ~ PTSdiff, data = NBA)
summary(WinsReg2)
PointsReg <- lm(PTS ~ X2PA + X3PA + FTA + AST + ORB + DRB + TOV + STL + BLK, data = NBA)
summary(PointsReg)

PointsReg2 <- lm(PTS ~ X2PA + X3PA + FTA + AST + ORB + DRB + STL + BLK, data = NBA)
summary(PointsReg2)

PointsReg3 <- lm(PTS ~ X2PA + X3PA + FTA + AST + ORB + STL + BLK, data = NBA)
summary(PointsReg3)

PointsReg4 <- lm(PTS ~ X2PA + X3PA + FTA + AST + ORB + STL, data = NBA)
summary(PointsReg4)

SSE4 <- sum(PointsReg4$residuals^2)
SSE4

RMSE4 <- sqrt(SSE4/nrow(NBA))
RMSE4

NBA_test = read.csv("NBA_test.csv")
PointsPrediction <- predict(PointsReg4, newdata = NBA_test)
SSE_test = sum((PointsPrediction - NBA_test$PTS)^2)
SST_test = sum((mean(NBA$PTS) - NBA_test$PTS)^2)
R3 <- 1 - SSE_test/SST_test
R3

RMSE_test <- sqrt(SSE_test/nrow(NBA_test))
RMSE_test
