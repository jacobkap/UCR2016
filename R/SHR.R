devtools::install_github("jacobkap/asciiReader")
library(readr)
library(asciiSetupReader)
library(dplyr)
library(haven)
library(feather)
library(gdata)

setwd("C:/Users/user/Dropbox/R_project/UCR2016/data/SHR")

UCR_SHR_2015 <- get_SHR(text_name = "SHR2015-COMB.txt", 2015)
UCR_SHR_2016 <- get_SHR(text_name = "SHR2016.TXT", 2016)
get_SHR <- function(text_name, year) {
  text_name <- read_lines(text_name)
  text_name <- data.frame(text_name, stringsAsFactors = FALSE)
  write.fwf(text_name, file = paste0("UCR_SHR_",year, ".txt"), colnames = FALSE, width = 268)
  dataset <- spss_ascii_reader(paste0("UCR_SHR_",year, ".txt"),
                                               "SHR_setup.sps")
  columns <- c("POPULATION", "COUNTY", "MSA", "INCIDENT_NUMBER",
               "VICTIM_COUNT", "OFFENDER_COUNT")
  dataset[, columns] <- sapply(dataset[, columns], as.numeric)
  return(dataset)
}

old <- spss_ascii_reader("36790-0001-Data.txt",
                         "36790-0001-Setup.sps")
all.equal(UCR_SHR_2015, old)

# To make the setup file.
victims(33, 99)
offenders(73, 149)
victim_names(33)
offender_names(73)
victims <- function(col_start, char_start) {
  col_seq <- seq(col_start, col_start + 40, 4)
  char_seq <- seq(char_start, char_start+49, 5)
  for (i in 1:10) {
    col_start = col_seq[i]
    char_start = char_seq[i]
    writeLines(paste0("V", col_start, " ", char_start, "-", char_start+1))
    writeLines(paste0("V", (col_start+1):(col_start+3), " ",
                   seq(char_start+2, char_start+4, 1)))
  }
}


offenders <- function(col_start, char_start) {
  col_seq <- seq(col_start, col_start + 79, 8)
  char_seq <- seq(char_start, char_start+119, 12)
  for (i in 1:10) {
    col_start = col_seq[i]
    char_start = char_seq[i]
    writeLines(paste0("V", col_start, " ", char_start, "-", char_start+1))
    writeLines(paste0("V", (col_start+1):(col_start+3), " ",
                   seq(char_start+2, char_start+4, 1)))
    writeLines(paste0("V", (col_start+4):(col_start+6), " ",
                   seq(char_start+5, char_start+10, 2), "-",
                   seq(char_start+6, char_start+11, 2)))
    writeLines(paste0("V", col_start+7, " ", char_start+11))
  }
}


victim_names <- function(col_start) {
  types <- rep(c("AGE", "SEX", "RACE", "ETHNICITY"), 10)
  number <- sort(rep(2:11, 4))
  col_nums <- seq(col_start, col_start+39, 1)
  for (i in 1:length(col_nums)) {
    writeLines(paste0("V", col_nums[i], "         ", "'VICTIM_", number[i], "_", types[i], "'"))
  }
}

offender_names <- function(col_start) {
  types <- rep(c("AGE", "SEX", "RACE", "ETHNICITY", "WEAPON", "RELATIONSHIP_TO_FIRST_VICTIM",
                 "CIRCUMSTANCE", "SUBCIRCUMSTANCE"), 10)
  number <- sort(rep(2:11, 8))
  col_nums <- seq(col_start, col_start+79, 1)
  for (i in 1:length(col_nums)) {
    writeLines(paste0("V", col_nums[i], "         ", "'OFFENDER_", number[i], "_", types[i], "'"))
  }
}

