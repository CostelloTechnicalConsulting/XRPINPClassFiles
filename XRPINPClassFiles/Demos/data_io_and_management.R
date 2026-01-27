# Data IO and Management with R Standard Datasets
# Uses mtcars and iris datasets with file I/O demonstrations

library(tidyr)
library(dplyr)
library(tibble)
library(stringr)

# ============================================================================
# Setup: Load and create demo data files (CSV, TXT)
# ============================================================================
cat("\n=== Setting Up Demo Data Files ===\n")

# Create data directory
if (!dir.exists("data")) {
  dir.create("data", recursive = TRUE)
}

# Use mtcars as primary dataset (similar to penguins)
mtcars_demo <- mtcars %>%
  rownames_to_column("car_model") %>%
  mutate(
    # Add some intentional missing values for demo purposes
    hp = ifelse(row_number() %in% c(3, 7, 15), NA, hp)
  )

# Define file paths
csv_path <- "data/mtcars_demo.csv"
txt_path <- "data/mtcars_demo.txt"

# Save files
write.csv(mtcars_demo, csv_path, row.names = FALSE)
write.table(mtcars_demo, txt_path, sep = "\t", row.names = FALSE)

cat("Files created:\n")
cat("  -", csv_path, "\n")
cat("  -", txt_path, "\n")

# ============================================================================
# Reading in data from CSV and TXT files
# ============================================================================
cat("\n=== Reading Data from Files ===\n")

mtcars_from_csv <- read.csv(csv_path)
mtcars_from_txt <- read.table(txt_path, sep = "\t", header = TRUE)

cat("Dimensions - CSV:", nrow(mtcars_from_csv), "rows,", ncol(mtcars_from_csv), "columns\n")
cat("Dimensions - TXT:", nrow(mtcars_from_txt), "rows,", ncol(mtcars_from_txt), "columns\n")

head(mtcars_from_csv)

# ============================================================================
# Data management: cleaning variables
# - Standardize column names
# - Normalize string values
# - Handle outliers with simple caps
# ============================================================================
cat("\n=== Data Cleaning ===\n")

cleaned <- mtcars_from_csv %>%
  # Standardize column names (lowercase, remove whitespace)
  rename_all(~tolower(str_trim(.))) %>%
  # Clip horsepower outliers (handling missing values with na.rm)
  mutate(hp = pmax(pmin(hp, 335, na.rm = TRUE), 50, na.rm = TRUE))

head(cleaned)

# ============================================================================
# Checking on missing values
# ============================================================================
cat("\n=== Missing Value Summary ===\n")

missing_summary <- cleaned %>%
  summarise(across(everything(), list(
    count = ~sum(is.na(.)),
    proportion = ~mean(is.na(.))
  ))) %>%
  pivot_longer(everything()) %>%
  separate(name, into = c("variable", "metric"), sep = "_(?=[^_]*$)") %>%
  pivot_wider(names_from = metric, values_from = value)

print(missing_summary)

# ============================================================================
# Creating variables for analysis from raw variables
# Examples: categorical bins from continuous, composite score, factors
# ============================================================================
cat("\n=== Creating Analysis Variables ===\n")

aug <- cleaned %>%
  mutate(
    # Categorical from continuous (mpg categories)
    mpg_category = cut(mpg,
                       breaks = c(0, 15, 25, Inf),
                       labels = c("low", "medium", "high"),
                       ordered = TRUE),
    # Composite score from multiple raw variables (efficiency index)
    efficiency_score = (0.4 * (mpg / max(mpg, na.rm = TRUE)) +
                       0.3 * (cyl / max(cyl, na.rm = TRUE)) +
                       0.3 * (1 / (wt / min(wt, na.rm = TRUE)))),
    # Factor/categorical variable
    cyl_f = factor(cyl, levels = c(4, 6, 8), ordered = FALSE)
  )

select(aug, mpg, mpg_category, efficiency_score, cyl_f) %>% head()

# ============================================================================
# Loops and array operations
# Example: Create rolling z-scores for MPG
# ============================================================================
cat("\n=== Computing Rolling Z-Scores ===\n")

window <- 5
z_scores <- numeric(nrow(aug))
mpg_vals <- aug$mpg

for (i in seq_len(length(mpg_vals))) {
  start <- max(1, i - window + 1)
  window_slice <- mpg_vals[start:i]
  z_scores[i] <- (mpg_vals[i] - mean(window_slice, na.rm = TRUE)) /
                 sd(window_slice, na.rm = TRUE)
}

aug$mpg_z_roll <- z_scores
select(aug, mpg, mpg_z_roll) %>% head(10)

# ============================================================================
# Visualize the rolling z-scores
# ============================================================================
cat("\n=== Creating Rolling Z-Score Visualization ===\n")

png("rolling_zscore_plot.png", width = 800, height = 600)

plot(aug$mpg_z_roll,
     type = "l",
     main = "Rolling Z-Scores of MPG (Window=5)",
     xlab = "Car Index",
     ylab = "Z-Score",
     col = "steelblue",
     lwd = 2)
grid()

dev.off()
cat("Plot saved as 'rolling_zscore_plot.png'\n")

# Display plot
plot(aug$mpg_z_roll,
     type = "l",
     main = "Rolling Z-Scores of MPG (Window=5)",
     xlab = "Car Index",
     ylab = "Z-Score",
     col = "steelblue",
     lwd = 2)
grid()

# ============================================================================
# Sanity checks with assertions and cross-tabulation
# ============================================================================
cat("\n=== Sanity Checks ===\n")

# Check that variables were created correctly
stopifnot(!any(is.na(aug$mpg_category)))
stopifnot(all(aug$efficiency_score >= 0, na.rm = TRUE))
stopifnot(all(aug$mpg_z_roll >= min(aug$mpg_z_roll, na.rm = TRUE), na.rm = TRUE))

cat("All sanity checks passed!\n")

# Cross-tabulation: cylinders vs mpg_category (with row proportions)
cat("\nCross-tabulation: Cylinders vs MPG Category\n")
ct <- table(aug$cyl_f, aug$mpg_category)
ct_prop <- prop.table(ct, margin = 1)
print(round(ct_prop, 3))

# ============================================================================
# Creating helper function for data transformation
# ============================================================================
cat("\n=== Defining Helper Functions ===\n")

# Helper function: add efficiency ratio
add_efficiency_ratio <- function(data) {
  data %>%
    mutate(efficiency_ratio = mpg / hp)
}

# Helper function: categorize weight classes
categorize_weight <- function(data) {
  data %>%
    mutate(weight_class = cut(wt,
                             breaks = c(0, 2.5, 3.5, Inf),
                             labels = c("light", "medium", "heavy"),
                             ordered = TRUE))
}

# Apply helpers
authored <- aug %>%
  add_efficiency_ratio() %>%
  categorize_weight()

head(authored)

# ============================================================================
# Creating labels for variables and levels (factors with labels)
# ============================================================================
cat("\n=== Variable Labeling ===\n")

# Define label mappings
cyl_labels <- c("4" = "4-Cylinder",
                "6" = "6-Cylinder",
                "8" = "8-Cylinder")

labeled <- authored %>%
  mutate(
    # Add descriptive labels via mapping
    cyl_label = cyl_labels[as.character(cyl)],
    # Create ordered factor for weight with explicit levels
    weight_f = factor(weight_class,
                     levels = c("light", "medium", "heavy"),
                     labels = c("Light (≤2.5)", "Medium (2.5-3.5)", "Heavy (>3.5)"),
                     ordered = FALSE)
  )

select(labeled, cyl, cyl_label, weight_class, weight_f) %>% head()

# ============================================================================
# Save final processed dataset
# ============================================================================
cat("\n=== Saving Final Dataset ===\n")

final_path <- "data/mtcars_processed.csv"
write.csv(labeled, final_path, row.names = FALSE)
cat("Final processed dataset saved to:", final_path, "\n")

cat("\n=== Data Management Demo Complete ===\n")

