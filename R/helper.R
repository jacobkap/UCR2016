# Helper functions


fix_negatives <- function(column) {
  # These are all the values for negatives numbers in the UCR
  negatives <- data.frame(value = c("}", "J", "K", "L", "M", "N",
                                    "O", "P", "Q", "R", "1}", "1J",
                                    "1K", "1L", "1M", "1N"),
                          negative = 0:-15)

  # Gets all unique values
  current_values <- unique(column)
  # Removes those that are negative
  current_values <- unique(current_values[grep(paste0(negatives$value,
                                                      collapse = "|"),
                                               current_values, invert = TRUE)])

  current_values <- data.frame(value = current_values, negative = current_values)

  # Change all negative values to what it actually is in the data set
  negatives$value <- paste0("0000", negatives$value)
  negatives$value[nchar(negatives$value) == 6] <- substr(negatives$value[nchar(negatives$value) == 6], 2,6)

  negatives <- rbind(negatives, current_values)
  negatives <- negatives[!is.na(negatives$value),]
  negatives <- negatives[negatives$value != 0,]
  # Factorize
  column <- factor(column, levels = negatives$value, labels = negatives$negative)
  # Make the column numeric instead of factor
  column <- as.numeric(as.character(column))
}

# Arrest groups for ASR
arrest_groups <- data.frame(code = c("000", "001", "002", "003", "004", "005", "006",
                                     "007", "008", "009", "010", "011", "012",
                                     "013", "014", "015", "016", "017", "018",
                                     "019", "020", "021", "022", "023", "024",
                                     "025", "026", "027", "028", "029", "030",
                                     "031", "032", "033", "034", "035", "036",
                                     "037", "038", "039", "040", "041", "042",
                                     "043", "044", "045", "046", "047", "048",
                                     "049", "050", "051", "052", "053", "054",
                                     "055", "056"),
                            name = c("temp", "MALE0_9", "MALE10_12", "MALE13_14",
                                     "MALE15", "MALE16", "MALE17", "MALE18",
                                     "MALE19", "MALE20", "MALE21", "MALE22",
                                     "MALE23", "MALE24", "MALE25_29",
                                     "MALE30_34", "MALE35_39", "MALE40_44",
                                     "MALE45_49", "MALE50_54", "MALE55_59",
                                     "MALE60_64", "MALE65", "FEMALE0_9",
                                     "FEMALE10_12", "FEMALE13_14", "FEMALE15",
                                     "FEMALE16", "FEMALE17", "FEMALE18", "FEMALE19",
                                     "FEMALE20", "FEMALE21", "FEMALE22",
                                     "FEMALE23", "FEMALE24", "FEMALE25_29",
                                     "FEMALE30_34", "FEMALE35_39", "FEMALE40_44",
                                     "FEMALE45_49", "FEMALE50_54", "FEMALE55_59",
                                     "FEMALE60_64", "FEMALE65",
                                     "ADULT_WHITE", "ADULT_BLACK", "ADULT_INDIAN",
                                     "ADULT_ASIAN", "JUVENILE_WHITE", "JUVENILE_BLACK",
                                     "JUVENILE_INDIAN", "JUVENILE_ASIAN",
                                     "ADULT_HISPANIC", 'ADULT_NONHISPANIC',
                                     "JUVENILE_HISPANIC", "JUVENILE_NONHISPANIC"),
                            stringsAsfactors = FALSE)