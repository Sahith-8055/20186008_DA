who <- read.csv("WHO.csv")
str(who)

plot(who$GNI, who$FertilityRate)

install.packages("ggplot2")
library(ggplot2)
scatterplot <- ggplot(who, aes(x = GNI, y = FertilityRate))
fertilityGNIplot <- scatterplot + geom_point(color="darkred", size=3, shape=8) + ggtitle("FertilityRate vs Gross National Income")
pdf("MyPlot.pdf")
print(fertilityGNIplot)
dev.off()

ggplot(who, aes(x = GNI, y = FertilityRate, color=Region)) + geom_point()
ggplot(who, aes(x = GNI, y = FertilityRate, color=LifeExpectancy)) + geom_point()
ggplot(who, aes(x = FertilityRate, y = Under15)) + geom_point()
ggplot(who, aes(x = log(FertilityRate), y = Under15)) + geom_point()

model <- lm(Under15 ~ log(FertilityRate), data = who)
summary(model)

ggplot(who, aes(x = log(FertilityRate), y = Under15)) + geom_point() + stat_smooth(method = "lm")
ggplot(who, aes(x = log(FertilityRate), y = Under15)) + geom_point() + stat_smooth(method = "lm", level = 0.99)
ggplot(who, aes(x = log(FertilityRate), y = Under15)) + geom_point() + stat_smooth(method = "lm", se=FALSE)
ggplot(who, aes(x = log(FertilityRate), y = Under15)) + geom_point() + stat_smooth(method = "lm", se=FALSE, color="orange")
