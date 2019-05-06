mvt = read.csv("mvtWeek1.csv")
str(mvt)

max(mvt$ID)
min(mvt$Beat)
table(mvt$Arrest)

DateConvert = as.Date(strptime(mvt$Date, "%m/%d/%y %H:%M"))
mvt$Month = months(DateConvert)
mvt$Weekday = weekdays(DateConvert)
mvt$Date = DateConvert

hist(mvt$Date, breaks=100)

boxplot(mvt$Date~mvt$Arrest)

mvt$Year = format(DateConvert,"%Y")
table(mvt$Arrest,mvt$Year)

sort(table(mvt$LocationDescription))

locdes = mvt$LocationDescription
Top5 <- subset(mvt,locdes=="GAS STATION" | locdes=="ALLEY" | locdes=="STREET" | locdes=="PARKING LOT/GARAGE(NON.RESID.)" | locdes=="DRIVEWAY - RESIDENTIAL")
Top5$LocationDescription = factor(Top5$LocationDescription)

gasSta <- subset(Top5, Top5$LocationDescription=="GAS STATION")
which.max(table(gasSta$Weekday))

resDri <- subset(Top5, Top5$LocationDescription=="DRIVEWAY - RESIDENTIAL")
which.min(table(resDri$Weekday))