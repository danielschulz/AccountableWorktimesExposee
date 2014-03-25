
curDate = NA
curNominal = NA
curWeekday = NA

rawData$vorgangNo = NA
rawData$vorgangNoFst = NA

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
  
  if (TRUE == regexpr(VORGANG_PATTERN, rawData[i,]$vorgang)) {
    rawData[i,]$vorgangNo = gsub(VORGANG_PATTERN, "\\1", rawData[i,]$vorgang)
    rawData[i,]$vorgangNoFst = gsub("(\\d+).*", "\\1", rawData[i,]$vorgang)
  }
}


rawData$weekday = weekdays(as.Date(rawData$date, DATE_FORMAT))
rawData$day   =   format(as.Date(rawData$date, DATE_FORMAT), "%d")
rawData$month =   format(as.Date(rawData$date, DATE_FORMAT), "%m")
rawData$year  =   format(as.Date(rawData$date, DATE_FORMAT), "%Y")

rawData$learning = "Ausbildung - intern" == rawData$baustein | 
  "Ausbildung - extern" == rawData$baustein
rawData$recreation = "Flex-Time" == rawData$baustein | 
  "Unbezahlter Urlaub" == rawData$baustein
rawData$holiday = "Urlaub" == rawData$baustein


rawData$investmentFor = NA
rawData$isIntern = NA

rawData$intermediateProxy = NA
rawData$benefitingCustomer = NA

rawData$isIntern = "43013" == rawData$vorgangNoFst | "47632" == rawData$vorgangNoFst | "52681" == rawData$vorgangNoFst | 
  "51450" == rawData$vorgangNoFst | "58120" == rawData$vorgangNoFst

rawData$benefitingCustomer[which(
                "46495" == rawData$vorgangNoFst | "49810" == rawData$vorgangNoFst | "52087" == rawData$vorgangNoFst)] = "HRS"

rawData$benefitingCustomer[which(
                "52604" == rawData$vorgangNoFst | "48504" == rawData$vorgangNoFst | "50760" == rawData$vorgangNoFst | 
                "53878" == rawData$vorgangNoFst | "55877" == rawData$vorgangNoFst | "54795" == rawData$vorgangNoFst |
                "57294" == rawData$vorgangNoFst | "58028" == rawData$vorgangNoFst | "57727" == rawData$vorgangNoFst)] = "Audi"

rawData$benefitingCustomer[which(
                      "54011" == rawData$vorgangNoFst)] = "Daimler"

rawData$intermediateProxy[which(
                      "52604" == rawData$vorgangNoFst | "48504" == rawData$vorgangNoFst | "50760" == rawData$vorgangNoFst | 
                      "53878" == rawData$vorgangNoFst | "55877" == rawData$vorgangNoFst | "54795" == rawData$vorgangNoFst |
                      "57294" == rawData$vorgangNoFst | "58028" == rawData$vorgangNoFst | "57727" == rawData$vorgangNoFst |
                      "54011" == rawData$vorgangNoFst)] = "GB A"


rawData$investmentFor[which("47632.4.2" == rawData$vorgangNo)] = "Other"
rawData$investmentFor[which(TRUE == regexpr("(Vorbereitung )?HRS.Termin", rawData$description) | "Einarbeitung eRFP-Projekt" == rawData$description)] = "HRS"
rawData$investmentFor[which("52681.4.3" == rawData$vorgangNo | "52604.1" == rawData$vorgangNo | "52681.4.3" == rawData$vorgangNo)] = "Audi"


# View(subset(data, data$vorgangNoFst == "51450"))

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
