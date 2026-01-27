# Statistical Tests and Modeling Syntax
# Uses R standard datasets (mtcars, iris, titanic) with base R and additional packages

# Install required packages (uncomment if needed)
# install.packages(c("broom", "sandwich", "lmtest", "geepack"))

library(tidyverse)
library(broom)
library(sandwich)
library(lmtest)
library(geepack)

# ============================================================================
# Load and prepare data using mtcars dataset
# ============================================================================
cat("\n=== Loading mtcars dataset ===\n")
data(mtcars)
mtcars <- mtcars %>%
  mutate(
    mpg_per_hp = mpg / hp,  # Create efficiency metric
    am_label = ifelse(am == 1, "Manual", "Automatic"),
    am_manual = ifelse(am == 1, 1, 0),
    cyl_label = factor(cyl)
  )

head(mtcars)

# Show unique values for categorical variables
cat("\nUnique transmission types:", unique(mtcars$am_label), "\n")
cat("Unique cylinder counts:", unique(mtcars$cyl), "\n")
cat("Unique gears:", unique(mtcars$gear), "\n")

# ============================================================================
# Obtaining basic statistics on continuous data
# ============================================================================
cat("\n=== Basic Statistics on Continuous Data ===\n")
summary(mtcars$mpg_per_hp)
quantile(mtcars$mpg_per_hp, probs = c(0.25, 0.5, 0.75))

# ============================================================================
# T-tests (two-sample)
# Compare mpg between automatic and manual transmissions
# ============================================================================
cat("\n=== T-test: MPG by Transmission Type ===\n")
manual <- mtcars$mpg[mtcars$am == 1]
automatic <- mtcars$mpg[mtcars$am == 0]

t_test_result <- t.test(manual, automatic, var.equal = FALSE)
print(t_test_result)

# Compare MPG by number of carburetors
cat("\n=== T-test: MPG by Carburetors (2 vs 4) ===\n")
carb_2 <- mtcars$mpg[mtcars$carb == 2]
carb_4 <- mtcars$mpg[mtcars$carb == 4]

t_test_carb <- t.test(carb_2, carb_4, var.equal = FALSE)
print(t_test_carb)

# ============================================================================
# Frequencies and percentages on categorical data
# ============================================================================
cat("\n=== Frequencies and Percentages: Cylinders ===\n")
freq_cyl <- table(mtcars$cyl)
freq_cyl_df <- data.frame(
  cyl = names(freq_cyl),
  count = as.numeric(freq_cyl),
  percent = round(as.numeric(freq_cyl) / sum(freq_cyl) * 100, 2)
)
print(freq_cyl_df)

# ============================================================================
# Chi-square tests
# Transmission type by cylinder count
# ============================================================================
cat("\n=== Chi-square Test: Transmission by Cylinders ===\n")
contingency_table <- table(mtcars$am_label, mtcars$cyl)
print(contingency_table)

chi_sq_result <- chisq.test(contingency_table)
print(chi_sq_result)

# ============================================================================
# Creating indicator variables for modeling
# ============================================================================
cat("\n=== Creating Indicator Variables ===\n")
# Create dummy variables for factors
indicators <- mtcars %>%
  mutate(
    cyl_6 = ifelse(cyl == 6, 1, 0),
    cyl_8 = ifelse(cyl == 8, 1, 0),
    am_manual = ifelse(am == 1, 1, 0)
  ) %>%
  select(mpg, hp, wt, cyl_6, cyl_8, am_manual)

head(indicators)

# ============================================================================
# Logistic regression (without interaction)
# Predict high MPG (>20) from horsepower, weight, and transmission
# ============================================================================
cat("\n=== Logistic Regression (without interaction) ===\n")
logit_df <- mtcars %>%
  mutate(high_mpg = ifelse(mpg >= 20, 1, 0),
    am_manual = ifelse(am == 1, 1, 0))

model1 <- glm(high_mpg ~ hp + wt + am_manual, 
              data = logit_df, 
              family = binomial(link = "logit"))
summary(model1)


# ============================================================================
# Logistic regression (with interaction)
# Add interaction between weight and transmission
# ============================================================================
cat("\n=== Logistic Regression (with interaction) ===\n")
model2 <- glm(high_mpg ~ hp * am_manual + wt, 
              data = logit_df, 
              family = binomial(link = "logit"))
summary(model2)

# ============================================================================
# Poisson regression with GEE (generalized estimating equations)
# Simulate clustered count data from mtcars
# ============================================================================
cat("\n=== Poisson Regression with GEE (Clustered Data) ===\n")
poisson_df <- mtcars %>%
  mutate(
    cluster_id = rep(1:8, length.out = n()),
    count_outcome = pmax(1, round(mpg / 5))
  ) %>%
  select(count_outcome, hp, wt, am_manual, cluster_id)

# Fit GEE model with exchangeable correlation
gee_model <- geeglm(count_outcome ~ hp + wt,
                    id = cluster_id,
                    data = poisson_df,
                    family = poisson(link = "log"),
                    corstr = "exchangeable")
summary(gee_model)

# ============================================================================
# Use Titanic dataset for weighted logistic regression
# ============================================================================
cat("\n=== Loading Titanic dataset ===\n")
data(Titanic)

# Convert to data frame and clean
titanic_df <- as.data.frame(Titanic) %>%
  uncount(Freq) %>%
  mutate(
    survived_binary = ifelse(Survived == "Yes", 1, 0),
    age_numeric = ifelse(Age == "Child", 5, 40),
    sex_male = ifelse(Sex == "Male", 1, 0),
    class_2 = ifelse(Class == "2nd", 1, 0),
    class_3 = ifelse(Class == "3rd", 1, 0),
    crew = ifelse(Class == "Crew", 1, 0)
  ) %>%
  filter(!is.na(survived_binary))

# Create weights based on gender (to mimic frequency/survey weights)
titanic_df <- titanic_df %>%
  mutate(weight = ifelse(Sex == "Male", 1.2, 0.8))

head(titanic_df)

# ============================================================================
# Logistic regression with weighted data
# ============================================================================
cat("\n=== Weighted Logistic Regression: Titanic Survival ===\n")
weighted_model <- glm(survived_binary ~ age_numeric + sex_male + class_2 + class_3,
                      data = titanic_df,
                      family = binomial(link = "logit"),
                      weights = weight)
summary(weighted_model)

# ============================================================================
# Extract coefficients and confidence intervals for visualization
# ============================================================================
cat("\n=== Extracting Model Results for Visualization ===\n")
model_results <- tidy(weighted_model, conf.int = TRUE) %>%
  filter(term != "(Intercept)")

print(model_results)

# ============================================================================
# Forest plot of coefficients with confidence intervals
# ============================================================================
cat("\n=== Creating Forest Plot ===\n")
png("forest_plot.png", width = 800, height = 600)

y_pos <- seq_len(nrow(model_results))
plot(model_results$estimate, y_pos,
     main = "Weighted Logistic Regression: Coefficient Estimates with 95% CI",
     xlab = "Coefficient Value",
     ylab = "",
     xlim = c(min(model_results$conf.low) - 0.1, max(model_results$conf.high) + 0.1),
     yaxt = "n",
     pch = 16,
     cex = 1.5,
     col = "steelblue")

# Add confidence interval lines
segments(model_results$conf.low, y_pos, model_results$conf.high, y_pos,
         lwd = 2, col = "steelblue")

# Add reference line at 0
abline(v = 0, col = "red", lty = 2, lwd = 2, alpha = 0.5)

# Add axis labels
axis(2, at = y_pos, labels = model_results$term, las = 1)
grid(NA, NULL, col = "gray", lty = 3)

dev.off()
cat("Forest plot saved as 'forest_plot.png'\n")

# Display the plot
plot(model_results$estimate, y_pos,
     main = "Weighted Logistic Regression: Coefficient Estimates with 95% CI",
     xlab = "Coefficient Value",
     ylab = "",
     xlim = c(min(model_results$conf.low) - 0.1, max(model_results$conf.high) + 0.1),
     yaxt = "n",
     pch = 16,
     cex = 1.5,
     col = "steelblue")

segments(model_results$conf.low, y_pos, model_results$conf.high, y_pos,
         lwd = 2, col = "steelblue")
abline(v = 0, col = "red", lty = 2, lwd = 2)
axis(2, at = y_pos, labels = model_results$term, las = 1)
grid(NA, NULL, col = "gray", lty = 3)

cat("\n=== Analysis Complete ===\n")
