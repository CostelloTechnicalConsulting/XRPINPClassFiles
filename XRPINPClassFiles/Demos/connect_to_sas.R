# Load required libraries
library(haven)

# Define the path to the SAS dataset
sas_dataset_path <- "data/NYSDOH_BRFSS_SurveyData_2020.sas7bdat"

# Read the SAS dataset
NYSDOH_BRFSS_df <- read_sas(sas_dataset_path)

# Display the first few rows
head(NYSDOH_BRFSS_df)

# Display all column names
colnames(NYSDOH_BRFSS_df)

# Display column metadata (data types)
str(NYSDOH_BRFSS_df)

# Display number of rows and columns
cat("Dimensions:", nrow(NYSDOH_BRFSS_df), "rows x", ncol(NYSDOH_BRFSS_df), "columns\n")

# Display variable labels (if available from SAS import)
if (!is.null(attr(NYSDOH_BRFSS_df, "variable.labels"))) {
  cat("\nVariable Labels:\n")
  print(attr(NYSDOH_BRFSS_df, "variable.labels"))
}

# Display column classes
cat("\nColumn Classes:\n")
print(sapply(NYSDOH_BRFSS_df, class))

# Display summary statistics
summary(NYSDOH_BRFSS_df)

# Create a histogram of the AGE column
hist(NYSDOH_BRFSS_df$AGE, breaks = 30, 
     main = "Distribution of AGE", 
     xlab = "Age", 
     ylab = "Frequency")
