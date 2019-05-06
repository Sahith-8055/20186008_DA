elantra <- read.csv("elantra.csv")
str(elantra)

elantraTrain <- subset(elantra, Year <= 2012)
elantraTest <- susbet(elantra, Year > 2012)

trainmodel <- lm(ElantraSales ~ Unemployment + CPI_all + CPI_energy + Queries, data = elantraTrain)
summary(trainmodel)

trainmodel <- lm(ElantraSales ~ Unemployment + CPI_all + CPI_energy + Queries + Month, data = elantraTrain)
summary(trainmodel)

