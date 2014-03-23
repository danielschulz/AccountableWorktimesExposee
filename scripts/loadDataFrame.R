
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


curDate = NA
curNominal = NA
curWeekday = NA


# targets: dates, weekdays, kum, baustein, vorgang

for (i in 1:length(rawData$vorgang)) {
  
  if (TRUE == regexpr(DATE_PATTERN, rawData[i,]$date)) {
    curDate = as.Date(rawData[i,]$date, format=DATE_FORMAT)
    curNominal = rawData[i,]$nominal
    curWeekday = rawData[i,]$weekday
      
  } else {
    rawData[i,]$date = format(curDate, DATE_FORMAT)
    rawData[i,]$nominal = curNominal
    rawData[i,]$weekday = curWeekday
  }
}

rawData = subset(rawData, !rawData$isKum)


# kum

kum = data.frame(
  weekday = weekdays(as.Date(subset(rawData, rawData$isKum)$date, format=DATE_FORMAT)),
  fragPause = subset(rawData, rawData$isKum)$fragPause,
  fragActual = subset(rawData, rawData$isKum)$fragActual)


# accounts
accounts = data.frame(
  vorgang = unique(rawData$vorgang),
  actual = NA)

for (i in 1:length(accounts$vorgang)) {
  accounts[i,]$actual = sum(na.omit(subset(rawData, accounts[i,]$vorgang == rawData$vorgang)$fragActual))
}


accounts$rank = dim(accounts)[1] - rank(accounts$actual, ties.method="max") + 1



rawUniqueDates = unique(rawData$date)
uniqueDates = rawUniqueDates[!rawUniqueDates %in% DATES_TO_IGNORE]

regExpVorgang = regexpr(VORGANG_PATTERN, rawData$vorgang)
regExpDate = regexpr(DATE_PATTERN, rawData$date)
