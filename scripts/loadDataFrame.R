
# INIT DATA

# load trainings data: data
dataLocation = "..\\inputs\\worktimes.txt"

rawData = read.csv2(dataLocation, header=FALSE, encoding="ANSI", sep="\t", 
                    strip.white=TRUE, na.strings=c("?"))

rm(list=c("dataLocation"))

rawData = as.data.frame(rawData)
names(rawData) = c("date", "weekday", "nominal", "timespan", "fragPause", "fragActual", 
                   "baustein", "vorgang", "description")

rawData$nominal = as.numeric(rawData$nominal)
rawData$fragPause = as.numeric(rawData$fragPause)
rawData$fragActual = as.numeric(rawData$fragActual)
rawData$isKum = DATE_KUM_ROW == rawData$date
