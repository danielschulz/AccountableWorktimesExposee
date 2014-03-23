
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


rawData$weekday = weekdays(as.Date(rawData$date, DATE_FORMAT))
rawData$day   =   format(as.Date(rawData$date, DATE_FORMAT), "%d")
rawData$month =   format(as.Date(rawData$date, DATE_FORMAT), "%m")
rawData$year  =   format(as.Date(rawData$date, DATE_FORMAT), "%Y")

rawData$isWorkDay = 0 < rawData$nominal


# data

data = subset(rawData, !rawData$isKum)
rest = subset(rawData,  rawData$isKum)

doi = subset(data, data$year == YEAR_OF_INTEREST)
roi = subset(rest, rest$year == YEAR_OF_INTEREST)



# kum

kum = data.frame(
  weekday = weekdays(as.Date(rest$date, format=DATE_FORMAT)),
  isWorkDay = rest$isWorkDay,
  nominal = rest$nominal,
  fragPause =  rest$fragPause,
  fragActual = rest$fragActual)

koi = data.frame(
  weekday = weekdays(as.Date(roi$date, format=DATE_FORMAT)),
  isWorkDay = roi$isWorkDay,
  nominal = roi$nominal,
  fragPause =  roi$fragPause,
  fragActual = roi$fragActual)


rm(list=c("rawData"))


# accounts
accounts = data.frame(
  vorgang = unique(data$vorgang),
  actual = NA)

for (i in 1:length(accounts$vorgang)) {
  accounts[i,]$actual = sum(na.omit(subset(
    data, accounts[i,]$vorgang == data$vorgang)$fragActual))
}


accounts$rank = dim(accounts)[1] - rank(accounts$actual, ties.method="max") + 1



rawUniqueDates = unique(data$date)
uniqueDates = rawUniqueDates[!rawUniqueDates %in% DATES_TO_IGNORE]

regExpVorgang = regexpr(VORGANG_PATTERN, data$vorgang)
regExpDate =    regexpr(DATE_PATTERN,    data$date)
