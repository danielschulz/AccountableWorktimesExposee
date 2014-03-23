
# SETUP WORKSPACE

set.seed(4711)
PERSISTENT_CONSTANTS = c("PERSISTENT_CONSTANTS", "VORGANG_PATTERN", "DATE_PATTERN", 
                         "DATES_TO_IGNORE", "DATE_KUM_ROW", "DATE_FORMAT",
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
dataLocation = "..\\inputs\\worktimes-example.txt"

rawData = read.csv2(dataLocation, header=FALSE, encoding="ANSI", sep="\t", 
                    strip.white=TRUE, na.strings=c("?"))

rm(list=c("dataLocation"))

rawData = as.data.frame(rawData)
names(rawData) = c("date", "weekday", "nominal", "timespan", "fragPause", "fragActual", 
                   "baustein", "vorgang", "description")

rawData$nominal = as.numeric(rawData$nominal)
rawData$fragPause = as.numeric(rawData$fragPause)
rawData$fragActual = as.numeric(rawData$fragActual)


curDate = NA
curNominal = NA
curWeekday = NA


# MAPS: dates, weekdays, kum, baustein, vorgang
datesMap = list()
weekdaysMap = list()
kumMap = list()
bausteinMap = list()
vorgangMap = list()

for (i in 1:length(rawData$vorgang)) {
  
  if (TRUE == regexpr(DATE_PATTERN, rawData[i,]$date)) {
    curDate = as.Date(rawData[i,]$date, format=DATE_FORMAT)
    curNominal = rawData$nominal
    curWeekday = rawData$weekday
  
  } else if (rawData[i,]$date == DATE_KUM_ROW) {
    kumMap[[paste0(curDate, "")]] = rawData[i,]$fragActual
  }
}

rawUniqueDates = unique(rawData$date)
uniqueDates = rawUniqueDates[!rawUniqueDates %in% DATES_TO_IGNORE]

regExpVorgang = regexpr(VORGANG_PATTERN, rawData$vorgang)
regExpDate = regexpr(DATE_PATTERN, rawData$date)
