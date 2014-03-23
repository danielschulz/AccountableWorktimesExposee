
# SETUP WORKSPACE

set.seed(4711)
PERSISTENT_CONSTANTS = c("PERSISTENT_CONSTANTS", "VORGANG_PATTERN", "DATE_PATTERN", "YEAR_OF_INTEREST",
                         "DATES_TO_IGNORE", "DATE_KUM_ROW", "DATE_FORMAT", "data",
                         "datesMap", "weekdaysMap", "kumMap", "bausteinMap", "vorgangMap")

DATE_FORMAT = "%d.%m.%y"

VORGANG_PATTERN = "^(\\d+(\\.\\d+)+)\\.*"
DATE_PATTERN = "^(\\d+\\.\\d+\\.\\d+)$"
DATE_KUM_ROW = "kum:"
DATES_TO_IGNORE = c(DATE_KUM_ROW, ">>>>")

# install.packages("stringr")
library("stringr")


# clean
rm(list = ls()[!(ls() %in% PERSISTENT_CONSTANTS)])


# constants
PERSISTENT_CONSTANTS = c("PERSISTENT_CONSTANTS")



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
