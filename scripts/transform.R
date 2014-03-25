
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


isInternVorgangNoFst = function (vorgangNoFst) {
  "43013" == vorgangNoFst | "47632" == vorgangNoFst | "52681" == vorgangNoFst | "51450" == vorgangNoFst | "58120" == vorgangNoFst
}

isAudiVorgangNoFst = function (vorgangNoFst) {
  "48504" == vorgangNoFst | "50760" == vorgangNoFst | "53878" == vorgangNoFst | "55877" == vorgangNoFst | "54795" == vorgangNoFst | 
    "57294" == vorgangNoFst | "58028" == vorgangNoFst | "57727" == vorgangNoFst | "52604" == vorgangNoFst
}

isDaimlerVorgangNoFst = function (vorgangNoFst) {
  "54011" == vorgangNoFst
}

isHrsVorgangNoFst = function (vorgangNoFst) {
  "46495" == vorgangNoFst | "49810" == vorgangNoFst | "52087" == vorgangNoFst
}



rawData$isIntern = isInternVorgangNoFst(rawData$vorgangNoFst)

rawData$benefitingCustomer[which(isHrsVorgangNoFst(rawData$vorgangNoFst))] = "HRS"
rawData$benefitingCustomer[which(isAudiVorgangNoFst(rawData$vorgangNoFst))] = "Audi"
rawData$benefitingCustomer[which(isDaimlerVorgangNoFst(rawData$vorgangNoFst))] = "Daimler"
rawData$intermediateProxy[which(isAudiVorgangNoFst(rawData$vorgangNoFst) | isDaimlerVorgangNoFst(rawData$vorgangNoFst))] = "GB A"


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
  vorgangNoFst = unique(doi$vorgangNoFst),
  actual = NA,
  customer = NA)

for (i in 1:length(accounts$vorgangNoFst)) {
  accounts[i,]$actual = sum(na.omit(subset(
    doi, accounts[i,]$vorgangNoFst == doi$vorgangNoFst)$fragActual))
}

accounts$customer[which(isAudiVorgangNoFst(accounts$vorgangNoFst))] =     "Audi"
accounts$customer[which(isDaimlerVorgangNoFst(accounts$vorgangNoFst))] =  "Daimler"
accounts$customer[which(isHrsVorgangNoFst(accounts$vorgangNoFst))] =      "HRS"
accounts$customer[which(isInternVorgangNoFst(accounts$vorgangNoFst))] =   "GB L"


accounts$rank = dim(accounts)[1] - rank(accounts$actual, ties.method="max") + 1
