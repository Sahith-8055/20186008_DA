climate <- read.csv("climate_change.csv")
ClimateTrainingSet <- subset(climate, Year <= 2006)
ClimateTestingSet <- subset(climate, Year > 2006 & Year <= 2008)

ClimateModel <- lm(Temp ~ MEI + CO2 + CH4 + N2O + CFC.11 + CFC.12 + TSI + Aerosols, data = ClimateTrainingSet)
summary(ClimateModel)

cor(ClimateTrainingSet)

ClimateModel <- lm(Temp ~ MEI + TSI + Aerosols + N2O, data = ClimateTrainingSet)
summary(ClimateModel)

ClimateModel <- lm(Temp ~ MEI + CO2 + CH4 + N2O + CFC.11 + CFC.12 + TSI + Aerosols, data = ClimateTrainingSet)
newModel <- step(ClimateModel)
summary(newModel)

PredictionModel <- predict(newModel, newdata = ClimateTestingSet)
SSE <- sum((PredictionModel - ClimateTestingSet$Temp)^2)
base1 <- mean(ClimateTrainingSet$Temp, na.rm = TRUE)
SST <- sum((ClimateTestingSet$Temp - base1)^2)
R <- 1 - (SSE/SST)
R