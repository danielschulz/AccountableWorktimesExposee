
# kum
boxplot(kum$fragActual ~ kum$weekday)
quantile(kum$fragActual, probs=seq(0, 1, 0.1))
quantile(subset(kum, kum$isWorkDay)$fragActual, probs=seq(0, 1, 0.1))
sum(kum$fragActual)
sd(kum$fragActual)


# koi
boxplot(koi$fragActual ~ koi$weekday)
quantile(koi$fragActual, probs=seq(0, 1, 0.1))
quantile(subset(koi, koi$isWorkDay)$fragActual, probs=seq(0, 1, 0.1))
sum(koi$fragActual)
sd(koi$fragActual)


# koi vs. kum
paste0(paste0("koi vs. kum sd: sd(koi) / sd(kum) = ", round(100 * (sd(koi$fragActual) / sd(kum$fragActual)), 1)), " %")
paste0(paste0(paste0("weekend work YOI: ", sum(na.omit(subset(koi, !koi$isWorkDay)$fragActual))), " h von "), 
       sum(na.omit(koi$nominal)))
paste0(paste0(paste0("weekend work ALL: ", sum(na.omit(subset(kum, !kum$isWorkDay)$fragActual))), " h von "), 
       sum(na.omit(kum$nominal)))

h_yoi_1 = sum(na.omit(subset(koi, !koi$isWorkDay)$fragActual))
h_yoi_2 = sum(na.omit(koi$nominal))
h_yoi   = h_yoi_1 / h_yoi_2

h_all_1 = sum(na.omit(subset(kum, !kum$isWorkDay)$fragActual))
h_all_2 = sum(na.omit(kum$nominal))
h_all   = h_all_1 / h_all_2

paste0("YOI: ", round(h_yoi, 5))
paste0("ALL: ", round(h_all, 5))
paste0("YOI vs. ALL: ", round(h_yoi / h_all, 3))


# accounts
quantile(accounts$actual, probs = seq(0, 1, 0.1))
hist(accounts$actual)


# holidays
for (y in unique(data$year)) {
  printHolidayDays(dim(subset(data, data$holiday & data$year == y))[1], y)
}

# investments
print(paste0(paste0(sum(subset(data, !is.na(data$investmentFor) & data$year == YEAR_OF_INTEREST)$fragActual), " h of investments in "), YEAR_OF_INTEREST))


# unique vorgaenge
# unique(subset(data, is.na(data$benefitingCustomer))$vorgang)
# unique(subset(data, (data$benefitingCustomer == "HRS" | data$benefitingCustomer == "Audi" | data$benefitingCustomer == "Daimler"))$vorgang)


# unique(subset(doi, "Audi" == doi$benefitingCustomer)$vorgang)
# unique(subset(doi, "Daimler" == doi$benefitingCustomer)$vorgang)

# unique(subset(data, "Audi" == data$benefitingCustomer)$vorgang)
# unique(subset(data, "Daimler" == data$benefitingCustomer)$vorgang)

print("Work done for Audi [h]")
sum(subset(data, "Audi" == data$benefitingCustomer)$fragActual)
print("Work done for Daimler [h]")
sum(subset(data, "Daimler" == data$benefitingCustomer)$fragActual)

print("Work done for Audi and Daimler [h]")
(sum(subset(doi, "Audi" == doi$benefitingCustomer)$fragActual) + 
  sum(subset(doi, "Daimler" == doi$benefitingCustomer)$fragActual)) / 1600

print("All work done [h]")
sum(subset(doi, !is.na(doi$fragActual))$fragActual)

print("Other work done [h]")
sum(subset(doi, is.na(doi$benefitingCustomer) & !is.na(doi$fragActual))$fragActual)

print("Customer work done [h]")
sum(subset(doi, !is.na(doi$fragActual) & (!is.na(doi$benefitingCustomer) | !is.na(doi$investmentFor) | "Reisezeit" == doi$baustein))$fragActual)

print("Sum internal work done with holidays [h]")
sum(subset(doi, !is.na(doi$fragActual) & is.na(doi$benefitingCustomer) & is.na(doi$investmentFor) & 
             "Reisezeit" != doi$baustein)$fragActual)

print("Sum internal work done without holidays [h]")
sum(subset(doi, !is.na(doi$fragActual) & is.na(doi$benefitingCustomer) & is.na(doi$investmentFor) & 
             "Reisezeit" != doi$baustein & "Urlaub" != doi$baustein)$fragActual)


# unique(subset(data, is.na(data$benefitingCustomer))$vorgang)

# View(subset(doi, !is.na(doi$fragActual) & 0 < doi$fragActual & is.na(doi$benefitingCustomer) & "Urlaub" != doi$baustein))

# print("Other work overview")
# View(subset(doi, (is.na(doi$benefitingCustomer) & is.na(doi$investmentFor) & "Urlaub" != doi$baustein) & 0 < doi$nominal))

print("Sum of investments")
sum(subset(doi, !is.na(doi$fragActual) & 0 < doi$fragActual & !is.na(doi$investmentFor))$fragActual)

print("Sum internal Reisezeiten")
sum(subset(doi, (is.na(doi$benefitingCustomer) & is.na(doi$investmentFor) & "Urlaub" != doi$baustein) & 
             "Reisezeit" == doi$baustein)$fragActual)

print("Sum all Reisezeiten")
sum(subset(doi, "Reisezeit" == doi$baustein)$fragActual)
