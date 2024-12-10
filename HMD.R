library(dplyr)

#READ ALL CVS FILES AND MAKE A LIST OF DATAFRAMES 

# Specify the directory where the CSV files are located
directory_path <- "C:/Users/Alex/Documents/STA9750-2024-FALL/HDMA"

# Get a list of all CSV files in the directory
csv_files <- list.files(path = directory_path, pattern = "*.csv", full.names = TRUE)

# Read all CSV files into a list of data frames
data_list <- lapply(csv_files, read.csv)

# View column names for each data frame in the list
lapply(data_list, colnames)



#IDENTIFY COLUMN NAMES TO EXTRACT FROM EACH DATAFRAME AND CONVERT INTO ONE DATA FRAME 

# Specify the column names you want to extract
columns_to_extract <- c("activity_year" ,"county_code" ,"derived_msa.md" ,"derived_race","action_taken","ffiec_msa_md_median_family_income","lei" ,"census_tract" ,"derived_sex","loan_type" ,"loan_amount","interest_rate","loan_term" ,"applicant_credit_score_type","tract_to_msa_income_percentage" )  # Replace with actual column names

# Extract specified columns from each data frame in the list
extracted_data_list <- lapply(data_list, function(df) df[, columns_to_extract])

# View the first data frame with the extracted columns
head(extracted_data_list[[1]])
str(extracted_data_list)

# Convert the 'census_tract' column to 'character' in each data frame
extracted_data_list <- lapply(extracted_data_list, function(df) {
  df |>
    mutate(census_tract = as.character(census_tract))  # Convert to character
})

# Combine the list of data frames into one data frame
combined_data <- bind_rows(extracted_data_list)

# View the combined data
head(combined_data)

# Check the structure of the combined data
str(combined_data)



#SAVING COMBINED DATA AS RDS TO LOAD TO GIT

# Save the data frame as an RDS file
saveRDS(combined_data, "combined_data.rds")


























