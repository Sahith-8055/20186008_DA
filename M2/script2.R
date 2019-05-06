wine = read.csv("wine.csv")
str(wine)
summary(wine)
model1 <- lm(Price ~ AGST, data = wine)
summary(model1)
model1$residuals
SSE <- sum(model1$residuals^2)
SSE
model2 <- lm(Price ~ AGST + HarvestRain, data = wine)
summary(model2)
SSE <- sum(model2$residuals^2)
SSE
model3 <- lm(Price ~ AGST + HarvestRain + WinterRain + Age + FrancePop, data = wine)
summary(model3)
SSE <- sum(model3$residuals^2)
SSE
model4 <- lm(Price ~ HarvestRain + WinterRain, data = wine)
summary(model4)
model5 <- lm(Price ~ AGST + HarvestRain + WinterRain + Age, data = wine)
summary(model5)
cor(wine$WinterRain, wine$Price)
cor(wine$Age, wine$FrancePop)
cor(wine)
model6 <- lm(Price ~ AGST + HarvestRain + WinterRain, data = wine)
summary(model6)

wineTest <- read.csv("wine_test.csv")
str(wineTest)
predictTest <- predict(model5, newdata = wineTest)
predictTest

SSE <- sum((wineTest$Price - predictTest)^2)
SST <- sum((wineTest$Price - mean(wine$Price))^2)
1 - SSE/SST


baseball <- read.csv("baseball.csv")
str(baseball)
moneyball <- subset(baseball, Year < 2002)
str(moneyball)
moneyball$RD <- moneyball$RS - moneyball$RA
str(moneyball)
plot(moneyball$RD, moneyball$W, main = "Plot")

baseball2001 <- subset(baseball,baseball$Year < 2002 & baseball$Year >1995)
baseball2001$RD <- baseball2001$RS - baseball2001$RA
str(baseball2001)
WinsReg <- lm(W ~ RD, data = baseball2001)
summary(WinsReg)

OppRunsReg <- lm(RA ~ OOBP + OSLG, data = moneyball)
summary(OppRunsReg)

