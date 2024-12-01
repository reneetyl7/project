# Install necessary packages if not already installed
packages <- c("sampleSelection", "car", "ggplot2", "broom", "gridExtra")

# Check if packages are installed, and install if not
new_packages <- packages[!(packages %in% installed.packages()[,"Package"])]
if (length(new_packages)) {
  install.packages(new_packages)
}

# Load necessary libraries
library(sampleSelection) # For handling sample selection models
library(car) # For variance inflation factor (VIF) analysis (check)
library(ggplot2) # For creating graphics and visualizations
library(broom) # For tidying model outputs
library(gridExtra) # For arranging multiple plots in a grid layout

# View the structure of the data
str(n257)

# View the first few rows of the data
head(n257)

# Import the CSV file
df <- n257

# Remove the last row if necessary
df <- head(df, -1)

# Check for missing values
if (any(is.na(df))) {
  cat("Warning: There are missing values in the dataset.\n")
}

# 1. OLS Model
ols_model_adjusted_Y1 <- lm(Y1 ~ X1 + X2 + X3 + X4 + X5 + Pros + Cons + X10 + F1 + F2 + F3 + F43 + F5, data = df)

# Summary of the OLS model
cat("### OLS Model Summary_Y1 ###\n")
print(summary(ols_model_adjusted_Y1))

## Visualisation ##
# 1. Fitted vs Actual Values: visualize how well the model predictions match the actual data.
# Create a data frame for predictions
df$predicted_Y1 <- predict(ols_model_adjusted_Y1)

# Fitted vs. Actual for Y1
ggplot(df, aes(x = Y1, y = predicted_Y1)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", color = "blue") +
  labs(title = "Fitted vs. Actual for Y1",
       x = "Actual Y1",
       y = "Fitted Y1") +
  theme_minimal()

# 2. Residuals vs Fitted Values: Useful for diagnosing potential issues 
# with the model, such as non-linearity or heteroscedasticity.
# Calculate residuals
df$residuals_Y1 <- residuals(ols_model_adjusted_Y1)
df$residuals_Y2 <- residuals(ols_model_adjusted_Y2)

# Residuals vs. Fitted for Y1
ggplot(df, aes(x = predicted_Y1, y = residuals_Y1)) +
  geom_point(alpha = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Residuals vs. Fitted for Y1",
       x = "Fitted Y1",
       y = "Residuals") +
  theme_minimal()

# Residuals vs. Fitted for Y2
ggplot(df, aes(x = predicted_Y2, y = residuals_Y2)) +
  geom_point(alpha = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Residuals vs. Fitted for Y2",
       x = "Fitted Y2",
       y = "Residuals") +
  theme_minimal()

# 3. Diagnostic
# Function to create diagnostic plots
plot_ols_diagnostics <- function(model, model_name) {
  # Calculate residuals and fitted values
  residuals <- residuals(model)
  fitted <- fitted(model)
  
  # Create a data frame for plotting
  diagnostics_df <- data.frame(Fitted = fitted, Residuals = residuals)
  
  # 1. Residuals vs. Fitted
  p1 <- ggplot(diagnostics_df, aes(x = Fitted, y = Residuals)) +
    geom_point(alpha = 0.6) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
    labs(title = paste("Residuals vs. Fitted for", model_name),
         x = "Fitted Values",
         y = "Residuals") +
    theme_minimal()
  
  # 2. Q-Q Plot
  p2 <- ggplot(diagnostics_df, aes(sample = Residuals)) +
    stat_qq() +
    stat_qq_line(color = "red") +
    labs(title = paste("Q-Q Plot for", model_name),
         x = "Theoretical Quantiles",
         y = "Sample Quantiles") +
    theme_minimal()
  
  # 3. Scale-Location Plot
  p3 <- ggplot(diagnostics_df, aes(x = Fitted, y = sqrt(abs(Residuals)))) +
    geom_point(alpha = 0.6) +
    geom_smooth(se = FALSE, color = "blue") +
    labs(title = paste("Scale-Location Plot for", model_name),
         x = "Fitted Values",
         y = "Square Root of |Residuals|") +
    theme_minimal()
  
  # 4. Cook's Distance Plot
  cooks_d <- cooks.distance(model)
  diagnostics_df$Cooks_Distance <- cooks_d
  
  p4 <- ggplot(diagnostics_df, aes(x = seq_along(Cooks_Distance), y = Cooks_Distance)) +
    geom_point(alpha = 0.6) +
    geom_hline(yintercept = 4 / length(cooks_d), linetype = "dashed", color = "red") +
    labs(title = paste("Cook's Distance for", model_name),
         x = "Index",
         y = "Cook's Distance") +
    theme_minimal()
  
  # Combine all plots into one layout
  library(gridExtra) # For arranging multiple ggplots
  grid.arrange(p1, p2, p3, p4, nrow = 2)
}

# Plot diagnostics for both models
plot_ols_diagnostics(ols_model_adjusted_Y1, "OLS Model Y1")

# 4. Coefficient Plot: Visualize the estimated coefficients and 
# their confidence intervals for easy interpretation.
# Extract coefficients and their confidence intervals for Y1
coef_summary_Y1 <- summary(ols_model_adjusted_Y1)$coefficients
coef_df_Y1 <- data.frame(
  variable = rownames(coef_summary_Y1),
  estimate = coef_summary_Y1[, "Estimate"],
  lower = coef_summary_Y1[, "Estimate"] - 1.96 * coef_summary_Y1[, "Std. Error"],
  upper = coef_summary_Y1[, "Estimate"] + 1.96 * coef_summary_Y1[, "Std. Error"]
)

# Coefficient plot for Y1
ggplot(coef_df_Y1[-1, ], aes(x = reorder(variable, estimate), y = estimate)) +  # Exclude intercept
  geom_point() +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2) +
  coord_flip() +
  labs(title = "Coefficients for Y1 OLS Model",
       x = "Variables",
       y = "Estimate") +
  theme_minimal()

## Correlation ##
# Check correlation among predictors
cor_matrix <- cor(df[, c("X1", "X1d", "X2", "X2d", "X3", "X31", "X4", "X5", "Pros", "Cons", "X8", "X9", "X10", "F1", "F2", "F3", "F41", "F42", "F43", "F5")], use = "pairwise.complete.obs")
print("Correlation Matrix:")
print(cor_matrix)

# Check multicollinearity
alias(ols_model_adjusted_Y1)

# Check VIF (Y1)
vif_values_Y1_adjusted <- vif(ols_model_adjusted_Y1)
print("Adjusted VIF Values for Y1:")
print(vif_values_Y1_adjusted)

# Step 1: Convert Y1 and Y2 to factors
df$Y1_binary <- as.factor(df$Y1)  # Willing to use (Y1)

# Step 2: Fit the null logistic regression model (For R²)
null_logit_Y1 <- glm(Y1_binary ~ 1, family = binomial(link = "logit"), data = df)  # Null model for Y1

# Step 3: Logistic Regression Model
logistic_model_Y1 <- glm(Y1_binary ~ X1 + X2 + X3 + X4 + Pros + Cons + F1 + F2 + F3 + F41 + F5, 
                         family = binomial(link = "logit"), data = df)

# Check the summaries of the models
cat("\n### Logistic Regression Model Summary ###\n")
summary(logistic_model_Y1)
# McFadden's R²
mcfadden_r2_Y1 <- 1 - (logistic_model_Y1$deviance / null_logit_Y1$deviance)
cat("McFadden's R² for Y1 =", mcfadden_r2_Y1, "\n")

# Likelihood Ratio Test
lr_test_Y1 <- anova(null_logit_Y1, logistic_model_Y1, test = "LRT")
cat("Likelihood Ratio Test for Y1:\n")
print(lr_test_Y1)

# Add predicted probabilities to the data frame for Y1
df$predicted_prob_Y1 <- predict(logistic_model_Y1, type = "response")

# Predicted probabilities plot for Y1
ggplot(df, aes(x = predicted_prob_Y1)) +
  geom_histogram(bins = 30, fill = "blue", alpha = 0.7) +
  labs(title = "Predicted Probabilities for Y1",
       x = "Predicted Probability",
       y = "Frequency") +
  theme_minimal()
