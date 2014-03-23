
# kum
boxplot(kum$fragActual ~ kum$weekday)
quantile(kum$fragActual, probs=seq(0, 1, 0.1))
quantile(subset(kum, kum$isWorkDay)$fragActual, probs=seq(0, 1, 0.1))
# sum(kum$fragActual)
# sd(kum$fragActual)


# koi
boxplot(koi$fragActual ~ koi$weekday)
quantile(koi$fragActual, probs=seq(0, 1, 0.1))
quantile(subset(koi, koi$isWorkDay)$fragActual, probs=seq(0, 1, 0.1))
# sum(koi$fragActual)
# sd(koi$fragActual)


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
