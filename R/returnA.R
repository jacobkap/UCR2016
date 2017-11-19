setwd("C:/Users/user/Dropbox/R_project/UCR2016/data/returnA")
devtools::install_github("jacobkap/asciiReader")
library(readr)
library(gdata)
library(asciiSetupReader)
library(haven)
library(feather)
# Return A
UCR_returnA_2016 <- returnA_function("RETA2016.TXT")
UCR_returnA_yearly_2016 <- yearly_sums(UCR_returnA_2016)
save(UCR_returnA_2016, file = "UCR_returnA_2016.rda")
save(UCR_returnA_yearly_2016, file = "UCR_returnA_yearly_2016.rda")
write_csv(UCR_returnA_2016, path = "UCR_returnA_2016.csv")
write_csv(UCR_returnA_yearly_2016, path = "UCR_returnA_yearly_2016.csv")
write_sav(UCR_returnA_2016, path = "UCR_returnA_2016.sav")
write_sav(UCR_returnA_yearly_2016, path = "UCR_returnA_yearly_2016.sav")
write_feather(UCR_returnA_2016, path = "UCR_returnA_2016.feather")
write_feather(UCR_returnA_yearly_2016, path = "UCR_returnA_yearly_2016.feather")


# Changes names so it will fit in STATA. 32 character name limit
names(UCR_returnA_2016) <- gsub("TOTAL_CLEARED", "TOT_CLR", names(UCR_returnA_2016))
names(UCR_returnA_2016) <- gsub("CLEARED", "CLR", names(UCR_returnA_2016))
names(UCR_returnA_2016) <- gsub("FORCIBLE_ENTRY", "FORC_ENT", names(UCR_returnA_2016))
names(UCR_returnA_2016) <- gsub("BURGLARY", "BURG", names(UCR_returnA_2016))
names(UCR_returnA_2016) <- gsub("ROBBERY", "ROB", names(UCR_returnA_2016))
names(UCR_returnA_2016) <- gsub("UNFOUNDED", "UNF", names(UCR_returnA_2016))
names(UCR_returnA_2016) <- gsub("MOTOR_VEHICLE", "VEH", names(UCR_returnA_2016))
names(UCR_returnA_2016) <- gsub("VEHICLE", "VEH", names(UCR_returnA_2016))
names(UCR_returnA_2016) <- gsub("ATTEMPTED", "ATT", names(UCR_returnA_2016))
names(UCR_returnA_2016) <- gsub("UNDER_", "", names(UCR_returnA_2016))
names(UCR_returnA_2016) <- gsub("GRAND_TOTAL_OF_ALL_FIELDS", "ALL_CRIMES", names(UCR_returnA_2016))
names(UCR_returnA_2016) <- gsub("OTHER", "OTH", names(UCR_returnA_2016))
#
names(UCR_returnA_yearly_2016) <- gsub("TOTAL_CLEARED", "TOT_CLR", names(UCR_returnA_yearly_2016))
names(UCR_returnA_yearly_2016) <- gsub("CLEARED", "CLR", names(UCR_returnA_yearly_2016))
names(UCR_returnA_yearly_2016) <- gsub("FORCIBLE_ENTRY", "FORC_ENT", names(UCR_returnA_yearly_2016))
names(UCR_returnA_yearly_2016) <- gsub("BURGLARY", "BURG", names(UCR_returnA_yearly_2016))
names(UCR_returnA_yearly_2016) <- gsub("ROBBERY", "ROB", names(UCR_returnA_yearly_2016))
names(UCR_returnA_yearly_2016) <- gsub("UNFOUNDED", "UNF", names(UCR_returnA_yearly_2016))
names(UCR_returnA_yearly_2016) <- gsub("MOTOR_VEHICLE", "VEH", names(UCR_returnA_yearly_2016))
names(UCR_returnA_yearly_2016) <- gsub("VEHICLE", "VEH", names(UCR_returnA_yearly_2016))
names(UCR_returnA_yearly_2016) <- gsub("ATTEMPTED", "ATT", names(UCR_returnA_yearly_2016))
names(UCR_returnA_yearly_2016) <- gsub("UNDER_", "", names(UCR_returnA_yearly_2016))
names(UCR_returnA_yearly_2016) <- gsub("GRAND_TOTAL_OF_ALL_FIELDS", "ALL_CRIMES", names(UCR_returnA_yearly_2016))
names(UCR_returnA_yearly_2016) <- gsub("OTHER", "OTH", names(UCR_returnA_yearly_2016))


write_dta(UCR_returnA_2016, path = "UCR_returnA_2016.dta")
write_dta(UCR_returnA_yearly_2016, path = "UCR_returnA_yearly_2016.dta")


returnA_function <- function(text_name) {
  source('C:/Users/user/Dropbox/R_project/UCR2016/helper.R')
  returnA <- spss_ascii_reader(text_name,
                               "ReturnA.sps",
                               value_label_fix = FALSE)


  month_columns <- grep("JAN|FEB|MAR|APR|MAY|JUN|JUL|AUG|SEP|OCT|NOV|DEC", names(returnA), value = TRUE)
  PT_columns <- grep("_PT|CARD|MONTH", names(returnA), value = TRUE)
  month_columns <- c(month_columns, names(returnA)[c(7, 10, 14:22)])
  month_columns <- month_columns[!month_columns %in% PT_columns]
  month_columns <- unique(month_columns)
  returnA[, month_columns] <- sapply(returnA[, month_columns], fix_negatives)
  return(returnA)
}

# COnverts the monthly info to yearly info. Keeps all the monthly card columns
yearly_sums <- function(dataset) {
  for (type in c("UNFOUNDED", "ACTUAL", "TOTAL_CLEARED", "CLEARED_AGE18")) {
    for (column in crimes) {
      dataset[, paste0(type, "_", column)] <- rowSums(dataset[,
                            grep(paste0(type, "_", column), names(dataset))], na.rm = TRUE)
    }
  }
  for (column in police) {
    dataset[, column] <- rowSums(dataset[, grep(column, names(dataset))], na.rm = TRUE)
  }
  month_columns <- grep("JAN|FEB|MAR|APR|MAY|JUN|JUL|AUG|SEP|OCT|NOV|DEC", names(dataset), value = TRUE)
  month_columns <- month_columns[grep("CARD|UPDATE|INCLUDED", month_columns, invert = TRUE)]
  dataset <- dataset[, names(dataset)[!names(dataset) %in% month_columns]]
  return(dataset)
}


# TO make .sps
all_months1()
all_months2()
all_months1 <- function() {
  col_months(col_start = 33, char_start = 306) # January
  col_months(col_start = 151, char_start = 896) # February
  col_months(col_start = 269, char_start = 1486) # March
  col_months(col_start = 387, char_start = 2076) # April
  col_months(col_start = 505, char_start = 2666) # May
  col_months(col_start = 623, char_start = 3256) # June
}
all_months2 <- function(){
  col_months(col_start = 741, char_start = 3846) # July
  col_months(col_start = 859, char_start = 4436) # August
  col_months(col_start = 977, char_start = 5026) # September
  col_months(col_start = 1095, char_start = 5616) # October
  col_months(col_start =  1213, char_start = 6206) # November
  col_months(col_start = 1331, char_start = 6796) # December
}
# Need to break this up in pieces cause RStudio's console
# doesn't store enough rows to copy everything at once
crime_labels1()
crime_labels2()
crime_labels1 <- function() {
  make_crime_labels(month = "JAN", col_start = 33)
  make_crime_labels(month = "FEB", col_start = 151)
  make_crime_labels(month = "MAR", col_start = 269)
  make_crime_labels(month = "APR", col_start = 387)
  make_crime_labels(month = "MAY", col_start = 505)
  make_crime_labels(month = "JUN", col_start = 623)
}
crime_labels2 <- function() {
  make_crime_labels(month = "JUL", col_start = 741)
  make_crime_labels(month = "AUG", col_start = 859)
  make_crime_labels(month = "SEP", col_start = 977)
  make_crime_labels(month = "OCT", col_start = 1095)
  make_crime_labels(month = "NOV", col_start =  1213)
  make_crime_labels(month = "DEC", col_start = 1331)
}

 month_start <- function(col_start, char_start) {
   message(paste0("V", col_start, "         ",  char_start, "-", char_start+1))
   message(paste0("V", col_start+1, "         ",  char_start+2, "-", char_start+7))
   message(paste0("V", (col_start+2):(col_start+10), "         ",
                  seq(char_start+8, char_start+16, 1), "\n"))
 }
 month_end <- function(col_start, char_start) {
   message(paste0("V", col_start, "         ",  char_start, "-", char_start+2))
   message(paste0("V", col_start+1, "         ",  char_start+3, "-", char_start+5))
   message(paste0("V", col_start+2, "         ",  char_start+6, "-", char_start+12))
 }

col_months <- function(col_start, char_start) {
   month_start(col_start, char_start)


   # Unfounded
   message(paste0("V", (col_start+11):(col_start+36), "         ",
                  seq(char_start+17, char_start+142, 5), "-", seq(char_start+21,
                                                                  char_start+146, 5), "\n"))
   # Act
   message(paste0("V", (col_start+37):(col_start+62), "         ",
                  seq(char_start+157, char_start+282, 5), "-", seq(char_start+161,
                                                                  char_start+286, 5), "\n"))
   # Cleared
   message(paste0("V", (col_start+63):(col_start+88), "         ",
                  seq(char_start+297, char_start+422, 5), "-", seq(char_start+301,
                                                                   char_start+426, 5), "\n"))
   # Cleared 18
   message(paste0("V", (col_start+89):(col_start+114), "         ",
                  seq(char_start+437, char_start+562, 5), "-", seq(char_start+441,
                                                                   char_start+566, 5), "\n"))
   month_end(col_start+115, char_start+577)
 }


crimes <- c("MURDER", "MANSLAUGHTER", "RAPE_TOTAL", "RAPE_BY_FORCE",
            "ATTEMPTED_RAPE", "ROBBERY_TOTAL", "ROBBERY_WITH_GUN",
            "ROBBERY_WITH_KNIFE", "ROBBERY_OTHER_WEAPON", "STRONG_ARM_ROBBERY",
            "ASSAULT_TOTAL", "ASSAULT_WITH_GUN", "ASSAULT_WITH_KNIFE",
            "ASSAULT_OTHER_WEAPON", "ASSAULT_HAND_FEET", "SIMPLE_ASSAULT",
            "BURGLARY_TOTAL", "BURGLARY_FORCIBLE_ENTRY", "BURGLARY_NO_FORCIBLE_ENTRY",
            "ATTEMPTED_BURGLARY", "LARCENY_TOTAL", "MOTOR_VEHICLE_THEFT_TOTAL",
            "AUTO_THEFT", "TRUCK_BUS_THEFT", "OTHER_VEHICLE_THEFT",
            "GRAND_TOTAL_OF_ALL_FIELDS")
pre_offense_month_labels <- c("MONTH_INCLUDED", "DATE_OF_LAST_UPDATE", "CARD_0_TYPE",
                  "CARD_1_TYPE", "CARD_2_TYPE", "CARD_3_TYPE", "CARD_4_TYPE",
                  "CARD_0_PT", "CARD_1_PT", "CARD_2_PT", "CARD_3_PT")
police <- c("OFFICERS_KILLED_FELONIOUSLY", "OFFICERS_KILLED_ACCIDENTALLY",
            "OFFICERS_ASSAULTED")

make_crime_labels <- function(month, col_start) {
  pre_labels <- paste0(month, "_", pre_offense_month_labels, "'\n")
  unfounded <- paste0(month, "_UNFOUNDED_", crimes, "'\n")
  actual <- paste0(month, "_ACTUAL_", crimes, "'\n")
  cleared <- paste0(month, "_TOTAL_CLEARED_", crimes, "'\n")
  cleared18 <- paste0(month, "_CLEARED_UNDER_AGE18_", crimes, "'\n")
  police <- paste0(month, "_", police, "'\n")
  total <- c(pre_labels, unfounded, actual, cleared, cleared18, police)

  columns <- paste0("V", col_start:(col_start+117))
  message(paste0(columns, "         '", total))
}
