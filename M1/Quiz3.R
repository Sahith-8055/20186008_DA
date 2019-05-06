hist(mvt$Date, breaks = 100)
boxplot(mvt$Date ~ mvt$Arrest, main="Box plot of date vs arrest")
table(mvt$Year, mvt$Arrest)