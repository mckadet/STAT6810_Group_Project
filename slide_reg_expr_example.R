data$Brand[i] %>%
  gregexpr(paste0("([[:space:]]?[[:alpha:]]+[[:punct:]]?",
                  "[[:alpha:]]+[[:punct:]]?[[:alpha:]]+){1,3}?®"), .) %>%
  regmatches(data$OCR[i], .) -> data$Brand[i]

data$Save[i] %>%
  gregexpr("[[:digit:]]?[[:digit:]][[:space:]]?\\%", .) %>%
  regmatches(data$OCR[i], .) -> data$Save[i]