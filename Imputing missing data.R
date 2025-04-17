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

#################
#### i-BORUTA ####
################
BORUTA_trich_datalist <- split(final_data, final_data$ID)

library(Boruta)

# Sequential version of Boruta analysis
boruta_results <- lapply(BORUTA_trich_datalist, function(individual_data) {
  Boruta(
    MinutesPulling ~ AffAnx + AffBored + CogWorrieAway + CogFix + 
      SensHand + SenseHair + AttThts + ValueImp + BehvStim,
    data = individual_data,
    maxRuns = 500,
    doTrace = 3 
  )
})

print(boruta_results)

###################
#### TS-Boruta ####
###################
library(forecast)

# Function to calculate ARIMA residuals for one dataset
calculate_residuals <- function(single_dataset) {
  
  # Initialize an empty list to store residuals for all individuals
  residuals_list <- list()
  
  # Loop through each individual (based on 'ID')
  for (id in unique(single_dataset$ID)) {
    
    # Filter data for the current individual
    individual_data <- subset(single_dataset, ID == id)
    
    # Create dataframe to store residuals
    individual_residuals <- data.frame(
      ID = rep(id, nrow(individual_data)),
      time = individual_data$Time
    )
    
    # Calculate residuals for each variable (excluding ID and time)
    vars_to_model <- setdiff(colnames(individual_data), c("ID", "Time"))
    
    for (var in vars_to_model) {
      # Fit ARIMA model
      arima_model <- auto.arima(individual_data[[var]])
      
      # Store residuals
      individual_residuals[[var]] <- residuals(arima_model)
    }
    
    # Add to results list
    residuals_list[[as.character(id)]] <- individual_residuals
  }
  
  return(residuals_list)
}

residuals_results <- calculate_residuals(final_data)

# Run residuals in Boruta
library(Boruta)

# Sequential version of TS-Boruta analysis
TS_boruta_results <- lapply(residuals_results, function(individual_data) {
  Boruta(
    MinutesPulling ~ AffAnx + AffBored + CogWorrieAway + CogFix + 
      SensHand + SenseHair + AttThts + ValueImp + BehvStim,
    data = individual_data,
    maxRuns = 500,
    doTrace = 3 
  )
})

print(TS_boruta_results)

# 1. Find which individuals are missing
all_ids <- names(residuals_results)
processed_ids <- names(TS_boruta_results)
missing_ids <- setdiff(all_ids, processed_ids)


##################
#### i-ARIMAX ####
##################
run_iarimax_on_single_dataset <- function(single_dataset, IV, impute_these) {
  # Load necessary libraries
  library(dplyr)
  library(idionomics)
  library(MTS)
  
  # Within-person standardization
  zData <- i_standarbot_300(single_dataset, impute_these, "ID", explanation = TRUE)
  
  # Create a list to store i-ARIMAX results for each independent variable
  iarimax_results <- list()
  
  # Loop through each independent variable (IV)
  for (j in seq_along(IV)) {
    # Generate a model name
    model_name <- paste0("IV_", j)
    
    # Run IARIMAXoid_Pro for each IV
    iarimax_results[[model_name]] <- IARIMAXoid_Pro(
      zData,
      x_series = IV[[j]],  # Current IV
      y_series = "MinutesPulling",  # Dependent variable
      id_var = "ID",
      hlm_compare = FALSE,
      timevar = "Time",
      metaanalysis = TRUE
    )
  }
  
  # Return the list of all results
  return(iarimax_results)
}

IV <- c("AffAnx","AffBored","CogWorrieAway","CogFix","SensHand","SenseHair",
        "AttThts","ValueImp","BehvStim")

# Example usage with a single dataset:
iarimax_results <- run_iarimax_on_single_dataset(final_data, IV, impute_these)

print(iarimax_results[[1]])


