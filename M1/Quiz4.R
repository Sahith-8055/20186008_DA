sort(table(mvt$LocationDescription))
TopLocations <- c("STREET", "PARKING LOT/GARAGE(NON.RESID.)", "ALLEY", "GAS STATION", "DRIVEWAY - RESIDENTIAL")
Top5 <- subset(mvt, LocationDescription %in% TopLocations)
Top5$LocationDescription <- factor(Top5$LocationDescription)
table(Top5$LocationDescription)
table(Top5$Weekday, Top5$LocationDescription == "GAS STATION")
table(Top5$Weekday, Top5$LocationDescription == "DRIVEWAY - RESIDENTIAL")