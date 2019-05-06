str(mvt)
summary(mvt)
mvt$ID[which.max(mvt$ID)]
mvt$Beat[which.min(mvt$Beat)]
summary(mvt$Arrest)
sum(mvt$LocationDescription == "ALLEY")