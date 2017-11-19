devtools::install_github("jacobkap/asciiReader")
library(readr)
library(gdata)
library(asciiSetupReader)
library(dplyr)
library(haven)
library(feather)

setwd("C:/Users/user/Dropbox/R_project/UCR2016/data/ASR")
UCR_ASR_2016 <- get_ASR("ASR2016.TXT", 2016)
save(UCR_ASR_2016, file = "UCR_ASR_2016.rda")
write_csv(UCR_ASR_2016, path = "UCR_ASR_2016.csv")
write_dta(UCR_ASR_2016, path = "UCR_ASR_2016.dta")
write_sav(UCR_ASR_2016, path = "UCR_ASR_2016.sav")
write_feather(UCR_ASR_2016, path = "UCR_ASR_2016.feather")

get_ASR <- function(dataset, year) {
  create_text_files(dataset, year)
  offense_details <- clean_details(year); gc()
  offense_details <- make_crime_groups(offense_details)
  ASR <- merge_files(offense_details, year); gc()

  ASR$CONTENTS <- "Detail record"
  ASR$CONTENTS[is.na(ASR$MONTH)] <- "Agency header only"
  ASR$CONTENTS[!is.na(ASR$MONTH) & is.na(ASR$MALE0_9)] <- "Agency+month header only"

  # Changes CONTENTS column to same location as ICPSR dataset
  ASR <- ASR[, c(1, ncol(ASR) , 2:(ncol(ASR)-1))]

  to_fix <- c("AGHEADER", "COUNTY", "POP", "MONTH", "MOHEADER",
              "DTLASTUP", "DTPRUP1", "DTPRUP2", "JDHANDDP", "JDREFJC",
              "JDREFWA", "JDREFOPA", "JDREFCC", "OCCUR", "MSA", "YEAR", "SEQNO")
  ASR[, to_fix] <- sapply(ASR[, to_fix], factor_to_numeric)

  ASR$BREAK[is.na(ASR$BREAK)] <- "Totals and breakdowns for both offenses of 18 and 19"
  ASR$AREO[is.na(ASR$AREO)] <- "Age, race, ethnic origin reported"

  ASR <- ASR[order(ASR$ORI, ASR$OFFENSE, ASR$MONTH),]
  return(ASR)
}

factor_to_numeric <- function(data) {
  return(as.numeric(as.character(data)))
}

create_text_files <- function(dataset, year) {
  temp <- read_lines(dataset)
  agency_header <- temp[substr(temp, 14, 15) == "00"]
  temp <- temp[!temp %in% agency_header]
  monthly_header <- temp[substr(temp, 18, 20) == "000"]
  temp <- temp[!temp %in% monthly_header]
  offense_detail <- data.frame(temp)
  rm(temp);gc()
  agency_header <- data.frame(agency_header)
  monthly_header <- data.frame(monthly_header)

  setwd("C:/Users/user/Dropbox/R_project/UCR2016/data/ASR")
  write.fwf(agency_header, file = paste0("ASR_agency_header_", year, ".txt"))
  write.fwf(monthly_header, file = paste0("ASR_monthly_header_", year, ".txt"))
  write.fwf(offense_detail, file = paste0("ASR_detail_header_", year, ".txt"))
}

merge_files <- function(offense_detail, year) {
  agency_header <- spss_ascii_reader(paste0("ASR_agency_header_", year, ".txt"),
                                     "ASR_agency_header.sps",
                                     skip = 1,
                                     real_names = FALSE)

  monthly_header <- spss_ascii_reader(paste0("ASR_monthly_header_", year, ".txt"),
                                      "ASR_monthly_header.sps",
                                      skip = 1,
                                      real_names = FALSE)
  ASR <- left_join(agency_header, monthly_header)
  ASR <- left_join(ASR, offense_detail)
  ASR$BLANK <- NULL
  return(ASR)

}

clean_details <- function(year) {
  source('C:/Users/user/Dropbox/R_project/UCR2016/R/helper.R')
  offense_detail <- spss_ascii_reader(paste0("ASR_detail_header_", year, ".txt"),
                                      "ASR_detail_header.sps",
                                      skip = 1,
                                      real_names = FALSE)
  code_columns <- grep("ARREST_CODE", names(offense_detail), value = TRUE)
  count_columns <- grep("ARREST_COUNT", names(offense_detail), value = TRUE)
  offense_detail[, count_columns] <- sapply(offense_detail[, count_columns], fix_negatives); gc()

  # Creates new, empty columns for group totals
  for (i in 1:nrow(arrest_groups)) {
    offense_detail[, paste0("temp", arrest_groups$code[i])] <- 0
  }
  for (i in 1:56) {
    message(i)
    for (group in arrest_groups$code) {
      true_vals <- grep(group, offense_detail[, paste0("ARREST_CODE_", i)])
      if (length(true_vals) > 0) {
        offense_detail[true_vals, paste0("temp", group)] <-  offense_detail[true_vals, paste0("temp", group)] +
          offense_detail[true_vals,
                         paste0("ARREST_COUNT_", i)]
      }
    }
  }
  # Deltes all the old columns
  offense_detail <- offense_detail[, grep("ARREST_CO",
                                          names(offense_detail),
                                          invert = TRUE)]
  # Fixes names of new columns
  for (i in 1:nrow(arrest_groups)) {
    names(offense_detail) <- gsub(paste0("temp", arrest_groups$code[i]), arrest_groups$name[i],
                                  names(offense_detail))
  }
  offense_detail$temp <- NULL
  return(offense_detail)
}

# Makes grouped categories, e.g. all male offenders, all minors
make_crime_groups <- function(dataset) {
  dataset$TOTAL <- rowSums(dataset[, grep("MALE[0-9]|FEMALE[0-9]", names(dataset))]) # All
  dataset$MALE_TOTAL <- rowSums(dataset[, grep("MALE[0-9]", names(dataset))]) # Male
  dataset$FEMALE_TOTAL <- rowSums(dataset[, grep("FEMALE[0-9]", names(dataset))]) # Female
  dataset$ADULT_MALE <- rowSums(dataset[, grep("MALE(18|19|2|3|4|5|6)", names(dataset))]) # Adult Male
  dataset$ADULT_FEMALE <- rowSums(dataset[, grep("FEMALE(18|19|2|3|4|5|6)", names(dataset))]) # Adult Female
  dataset$JUVENILE_MALE <- rowSums(dataset[, grep("MEMALE(10|13|15|16|17)", names(dataset))]) # Juvenile Male
  dataset$JUVENILE_FEMALE <- rowSums(dataset[, grep("FEMALE(10|13|15|16|17)", names(dataset))]) # Juvenile Female
  dataset$ADULT_TOTAL <- rowSums(dataset[, grep("(M.*|F.*)(18|19|2|3|4|5|6)", names(dataset))]) # All adult
  dataset$JUVENILE_TOTAL <- rowSums(dataset[, grep("(M.*|F.*)(10|13|15|16|17)", names(dataset))]) # All juvenile

  return(dataset)
}
