mvt$Date[1]
DateConvert = as.Date(strptime(mvt$Date, "%m/%d/%y %H:%M"))
summary(DateConvert)
mvt$Month = months(DateConvert)
mvt$Weekday = weekdays(DateConvert)
mvt$Date = DateConvert
table(months(DateConvert))
which.min(table(months(DateConvert)))
names(which.min(table(months(DateConvert))))
names(which.max(table(weekdays(DateConvert))))
names(which.max(table(mvt$Arrest, months(DateConvert))[2,]))