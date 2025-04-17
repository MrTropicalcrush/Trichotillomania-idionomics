# Handling missing data using longitidunal imputation
# Load required packages
library(idionomics)
library(dplyr)

# Remove unneeded columns
trich_data <- trich_data[,-c(2,3,4)]

impute_these <- c("AffAnx","AffBored","CogWorrieAway","CogFix","SensHand","SenseHair","AttThts","ValueImp","BehvStim","MinutesPulling")

trich_data <- trich_data %>%
                    mutate(across(all_of(impute_these), ~ as.numeric(as.character(.))))

# Impute missing data
imputated_data <- imputatron_2000(data= trich_data, id_col = "ID", time_col = "Time", cols_of_interest = impute_these)

final_data <- imputated_data$imputed_df

#### BORUTA ####
BORUTA_trich_datalist <- split(final_data, final_data$ID)

library(parallel)
library(Boruta)

# Set up parallel processing
num_cores <- detectCores() - 4
cl <- makeCluster(num_cores)
clusterEvalQ(cl, library(Boruta))  # Load Boruta on each core

# Run Boruta in parallel for each individual
boruta_results <- parLapply(cl, BORUTA_trich_datalist, function(BORUTA_trich_datalist) {
  Boruta(
    MinutesPulling ~ AffAnx + AffBored + CogWorrieAway + CogFix + SensHand + SenseHair + AttThts + ValueImp + BehvStim,
    data = BORUTA_trich_datalist,
    maxRuns = 500,
    doTrace = 3  
  )
})

# Stop the cluster
stopCluster(cl)

