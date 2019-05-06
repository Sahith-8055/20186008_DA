climate <- read.csv("climate_change.csv")
str(climate)
nrow(climate)

table(climate$Year)

library(ggplot2)

ggplot(climate, aes(x = N2O, y = Temp)) + geom_line() + xlab("Concentration of N2O") + ylab("Temperature")

model1 <- lm(Temp ~ Aerosols, data = climate)

ggplot(climate, aes(x = Aerosols, y = Temp)) + geom_line() + stat_smooth(method = "lm", color = "red")
