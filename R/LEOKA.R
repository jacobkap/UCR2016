# Police Employee - LEOKA
intro_columns(21)
leoka_months1()
leoka_months2()
leoka_months3()

leoka_months1 <- function(){
  monthly_columns(70, 274) # January
  monthly_columns(253, 892) # February
  monthly_columns(436, 1510) # March
  monthly_columns(619, 2128) # April
  monthly_columns(802, 2746) # May
}
leoka_months2 <- function() {
  monthly_columns(985, 3364) # June
  monthly_columns(1168, 3982) # July
  monthly_columns(1351, 4600) # August
  monthly_columns(1534, 5218) # September
}

leoka_months3 <- function() {
  monthly_columns(1717, 5836) # October
  monthly_columns(1900, 6454) # November
  monthly_columns(2083, 7072) # December
  # Don't need the blank column for december
}

# Patrol type - only runs once
intro_columns <- function(col_start) {
  writeLines(paste0("V", col_start:(col_start+29), "         ", seq(104, 249, 5), "-",
                    seq(108, 253, 5)))
  writeLines(paste0("V", (col_start+30):(col_start+31), "         ", 254:255))
  writeLines(paste0("V", (col_start+32):(col_start+43), "         ", 256:267))
  writeLines(paste0("V", col_start+44, "         ", 268, "-", 269))
  writeLines(paste0("V", (col_start+45):(col_start+48), "         ", 270:273))
}

monthly_columns <- function(col_start, char_start) {
  writeLines(paste0("V", (col_start):(col_start+1), "         ", char_start:(char_start+1)))
  writeLines(paste0("V", (col_start+2):(col_start+3), "         ", seq(char_start+2, char_start+6, 3), "-",
                    seq(char_start+4, char_start+7, 3)))
  writeLines(paste0("V", (col_start+4):(col_start+13), "         ", seq(char_start+8, char_start+55, 5), "-", seq(char_start+12, char_start+57, 5)))
  # All the crimes
  writeLines(paste0("V", (col_start+14):(col_start+156), "         ", seq(char_start+58, char_start+484, 3), "-", seq(char_start+60, char_start+486, 3)))
  # Total calls and Time of assaults
  writeLines(paste0("V", (col_start+157):(col_start+181), "         ", seq(char_start+487, char_start+611, 5), "-", seq(char_start+491, char_start+615, 5)))
  writeLines(paste0("V", (col_start+182), "         ", char_start+612 , "-", char_start+617))
}


### COLUMN NAMES
months <- c("JAN", "FEB", "MAR", "APR", "MAY", "JUN", "JUL", "AUG", "SEP", "OCT", "NOV", "DEC")
intro <- c("ASSAULT_INJURY", "ASSAULT_NO_INJURY", "OFFICERS_KILLED_FELONIOUS_ACTS", "OFFICERS_KILLED_ACCIDENT")
crimes <- c("ASSAULT_WITH_INJURY", "ASSAULTS_WITHOUT_INJURY", "DISTURBANCE_CALLS",
            "BURGLARIES", "ROBBERIES", "ATTEMPTING_OTHER_ARRESTS", "CIVIL_DISORDER",
            "CUSTODY_OF_PRISONERS", "SUSPICIOUS_PERSONS", "AMBUSH", "MENTALLY_DERANGED",
            "TRAFFIC_PURSUIT_STOPS", "ALL_OTHER", "TOTAL_CALLS")
crime_info <- c("WEAPON_FIREARMS", "WEAPON_KNIFE", "WEAPON_OTHER", "WEAPON_HAND",
                "TOTAL_ASSAULTS")
assignment <- c("TWO_MAN_VEHICLE", "ONE_MAN_ALONE", "ONE_MAN_ASSISTED", "DETECTIVE_ALONE",
                "DETECTIVE_ASSISTED", "OTHER_ALONE", "OTHER_ASSISTED")
time <- c("0001_TO_0200", "0201_TO_0400", "0401_TO_0600", "0601_TO_0800", "0801_TO_1000",
          "1001_TO_1200", "1201_TO_1400", "1401_TO_1600", "1601_TO_1800", "1801_2000",
          "2001_TO_2200", "2201_TO_2400")

month_names = c()
for (month in months) {
  month_names <- c(month_names, paste0(month, "_", intro))
  for (crime in crimes) {

    month_names <- c(month_names, paste0(month, "_", crime, "_", crime_info))
    if (!crime %in% c("ASSAULT_WITH_INJURY", "ASSAULTS_WITHOUT_INJURY"))
    month_names <- c(month_names, paste0(month, "_", crime, "_ASSIGN_", assignment))
  }
  month_names <- c(month_names, paste0(month, "_TIME_", time))
  month_names <- c(month_names, paste0(month, "_BLANK"))
}
