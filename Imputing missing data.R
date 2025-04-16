# Handling missing data using longitidunal imputation

library(idionomics)
library(dplyr)

impute_these <- c("AffAnx","AffBored","CogWorrieAway","CogFix","SensHand","SenseHair","AttThts","ValueImp","BehvStim","MinutesPulling")

trich_data <- trich_data %>%
                    mutate(across(all_of(impute_these), ~ as.numeric(as.character(.))))

imputated_data <- imputatron_2000(data= trich_data, id_col = "ID", time_col = "Time", cols_of_interest = impute_these)

final_data <- imputated_data$imputed_df


