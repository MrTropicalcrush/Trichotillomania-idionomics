# Load the problematic data
data_80883 <- residuals_results[["80883"]]
data_81079 <- residuals_results[["81079"]]


# Check dimensions and NAs
check_data <- function(df, id) {
  list(
    ID = id,
    Rows = nrow(df),
    Cols = ncol(df),
    NA_Count = colSums(is.na(df)),
    Target_Length = length(df$MinutesPulling),
    Predictor_Lengths = sapply(df[, -c(1,2)], length)  # Exclude ID/time
  )
}

print(check_data(data_80883, "80883"))
print(check_data(data_81079, "81079"))

library(dplyr)

data_81079 %>%
  summarise(across(
    everything(),
    ~ if(is.numeric(.)) sd(., na.rm = TRUE) == 0 
    else n_distinct(.) == 1
  )) %>%
  select(where(isTRUE))

print(boruta_results[[4]])

# Sequential version of TS-Boruta analysis
Boruta_81079 <- Boruta(
  MinutesPulling ~ AffAnx + AffBored + CogWorrieAway + CogFix + 
    SensHand + SenseHair + AttThts + ValueImp + BehvStim,
  data = data_81079,
  maxRuns = 500,
  doTrace = 3 )

