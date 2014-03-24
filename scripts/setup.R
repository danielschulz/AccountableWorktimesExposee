
# SETUP WORKSPACE

set.seed(4711)
PERSISTENT_CONSTANTS = c("PERSISTENT_CONSTANTS", "VORGANG_PATTERN", "DATE_PATTERN", "YEAR_OF_INTEREST",
                         "DATES_TO_IGNORE", "DATE_KUM_ROW", "DATE_FORMAT", "data",
                         "datesMap", "weekdaysMap", "kumMap", "bausteinMap", "vorgangMap")

DATE_FORMAT = "%d.%m.%y"

VORGANG_PATTERN = "(\\d+(?:\\.\\d+)*)\\s+.*"
DATE_PATTERN = "^(\\d+\\.\\d+\\.\\d+)$"
DATE_KUM_ROW = "kum:"
DATES_TO_IGNORE = c(DATE_KUM_ROW, ">>>>")

# install.packages("stringr")
library("stringr")


# clean
rm(list = ls()[!(ls() %in% PERSISTENT_CONSTANTS)])


# constants
PERSISTENT_CONSTANTS = c("PERSISTENT_CONSTANTS")



# FUNCTIONS
printHolidayDays = function(days, year) {
  paste0(paste0(days, " days holiday in "), year)
}
