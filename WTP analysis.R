# Name: Renee
# File: Willingness to Pay (WTP) Analysis

# Objectives:
# The study aimed to investigate respondents' willingness to pay (WTP) for a specific service or product, examining two payment scenarios:
# 1. Per-time payment (Y3)
# 2. Per-month payment (Y4)

# Methodology:
# 1. Ordinary Least Squares (OLS) Regression
# Developed predictive models for both payment scenarios
# Assessed the relationship between various predictors and WTP
# Conducted diagnostic checks for model reliability
# 2. Heckman Selection Model
# Addressed potential selection bias
# Analysed factors affecting both the selection and outcome of WTP

# Practical Implications:
# 1. Pricing Strategy
# The demand plot provides insights into potential pricing levels
# Demonstrates how different price points might impact consumer participation
# 2. Selection Bias Consideration:
# Heckman model revealed nuanced factors influencing payment selection
# Offers a more comprehensive understanding of WTP decision-making
# 3. Demand Characteristics
# Developed a demand plot showing the relationship between price levels and respondent quantities
# Illustrated variations in WTP across different price points
# Conclusion: 
# The study provides a multi-faceted analysis of willingness to pay (WTP), 
# offering insights into consumer behavior, pricing dynamics, and factors influencing payment decisions.


# Install necessary packages if not already installed
packages <- c("sampleSelection", "car", "ggplot2", "effects", "broom", "gridExtra", "dplyr", "pROC")

# Check if packages are installed, and install if not
new_packages <- packages[!(packages %in% installed.packages()[,"Package"])]
if (length(new_packages)) {
  install.packages(new_packages)
}

# Load necessary libraries
library(sampleSelection) # For handling sample selection models and Heckman corrections
library(car) # For variance inflation factor (VIF) analysis and regression diagnostics
library(ggplot2) # For creating sophisticated graphics and visualisations
library(broom) # For converting statistical objects into tidy data frames
library(gridExtra) # For arranging multiple plots in a grid layout
library(dplyr) # For data manipulation and transformation
library(effects) # For visualising model effects and interactions

# View the structure of the data
str(n193)

# View the first few rows of the data
head(n193)

# Import the CSV file
df <- n193

# Remove the last row if necessary
df <- head(df, -1)

# Check for missing values
if (any(is.na(df))) {
  cat("Warning: There are missing values in the dataset.\n")
}

class(df)
df <- as.data.frame(df)  # Convert to data frame if necessary
colnames(df)

# Convert Y3 to a factor
df$Y3 <- as.factor(df$Y3)

# Check Y3 & Y4
table(df$Y3)
table(df$Y4)

# Check the new distribution
table(df$Y3_combined)
sum(is.na(df)) # check NA value

# List of factor variables for fitting
factors <- c("Z1", "Z2", "Z3", "Z43", "Z44", "Z45", "Z5", "F1", "F2", "F3", "F41", "F5", "X8", "X9", "X10")

### Correlation ###
# Check correlation among predictors
cor_matrix <- cor(df[, c("Z1", "Z2", "Z3", "Z41", "Z42", "Z43", "Z44", "Z45", "Z5", "F1", "F2", "F3", "F41", "F42", "F43", "F5")], use = "pairwise.complete.obs")
print("Correlation Matrix:")
print(cor_matrix)

# Check multicollinearity
alias(ols_model_adjusted_Y3)
alias(ols_model_adjusted_Y4)

# Check VIF (Y3)
vif_values_Y3_adjusted <- vif(ols_model_adjusted_Y3)
print("Adjusted VIF Values for Y3:")
print(vif_values_Y3_adjusted)

# Check VIF (Y4)
vif_values_Y4_adjusted <- vif(ols_model_adjusted_Y4)
print("Adjusted VIF Values for Y4:")
print(vif_values_Y4_adjusted)

#### OLS ###
# 1. OLS Model
ols_model_adjusted_Y3 <- lm(Y3 ~ Z1 + Z3 + Z54 + Z55 + Z56 + F2 + F3 + F42 + F5 + X8 + X9, data = df)
ols_model_adjusted_Y4 <- lm(Y4 ~ Z1 + Z2 + Z3 + Z54 + Z55 + Z56 + F2 + F3 + F42 + F5 + X8 + X9, data = df)

ols_model_adjusted_X5 <- lm(Y1 ~ X5, data = df)
print(summary(ols_model_adjusted_X5))

# Summary of the OLS model
cat("### OLS Model Summary_Y3 ###\n")
print(summary(ols_model_adjusted_Y3))
cat("### OLS Model Summary_Y4 ###\n")
print(summary(ols_model_adjusted_Y4))

### Visualisation ###
# 1. Fitted vs Actual: stimulation
# Create a data frame for predictions
df$predicted_Y3 <- predict(ols_model_adjusted_Y3)
df$predicted_Y4 <- predict(ols_model_adjusted_Y4)

# Fitted vs. Actual for Y3
ggplot(df, aes(x = Y3, y = predicted_Y3)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", color = "blue") + # includes confidence interval
  labs(title = "Fitted vs. Actual for Y3",
       x = "Actual Y3",
       y = "Fitted Y3") +
  theme_minimal()

# Fitted vs. Actual for Y4
ggplot(df, aes(x = Y4, y = predicted_Y4)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", color = "blue") + # includes confidence interval
  labs(title = "Fitted vs. Actual for Y4",
       x = "Actual Y4",
       y = "Fitted Y4") +
  theme_minimal()


# 2. Residuals vs Fitted
# Calculate residuals for Y3 and Y4
df$residuals_Y3 <- residuals(ols_model_adjusted_Y3)
df$residuals_Y4 <- residuals(ols_model_adjusted_Y4)

# Residuals vs. Fitted for Y3
ggplot(df, aes(x = predicted_Y3, y = residuals_Y3)) +
  geom_point(alpha = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Residuals vs. Fitted for Y3",
       x = "Fitted Y3",
       y = "Residuals") +
  theme_minimal()

# Residuals vs. Fitted for Y4
ggplot(df, aes(x = predicted_Y4, y = residuals_Y4)) +
  geom_point(alpha = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Residuals vs. Fitted for Y4",
       x = "Fitted Y4",
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
plot_ols_diagnostics(ols_model_adjusted_Y3, "OLS Model Y3")
plot_ols_diagnostics(ols_model_adjusted_Y4, "OLS Model Y4")

# 4. Coefficient plot:V
# Function to create coefficient plot
plot_coefficients <- function(model, model_name) {
  # Get coefficients and confidence intervals
  tidy_model <- tidy(model, conf.int = TRUE)
  
  # Create the coefficient plot
  ggplot(tidy_model, aes(x = term, y = estimate, ymin = conf.low, ymax = conf.high)) +
    geom_point() +
    geom_errorbar(width = 0.2) +
    coord_flip() +
    labs(title = paste("Coefficient Plot for", model_name),
         x = "Predictors",
         y = "Coefficient Estimate") +
    theme_minimal()
}

# Create coefficient plots
plot_coefficients(ols_model_adjusted_Y3, "OLS Model Y3")
plot_coefficients(ols_model_adjusted_Y4, "OLS Model Y4")

### BLM test ###
# Step 1: Create binary versions of Y3 and Y4
# Define thresholds
threshold_Y3 <- 2  # You can adjust this based on your criteria
threshold_Y4 <- 3  # You can adjust this based on your criteria

# Create binary variables
df$Y3_binary <- ifelse(df$Y3 > threshold_Y3, 1, 0)  # Willing to pay (Y3)
df$Y4_binary <- ifelse(df$Y4 > threshold_Y4, 1, 0)  # Willing to pay (Y4)

# Convert to factors
df$Y3_binary <- as.factor(df$Y3_binary)
df$Y4_binary <- as.factor(df$Y4_binary)

# Step 2: Fit the null logistic regression model (For R^2)
null_logit_Y3 <- glm(Y3_binary ~ 1, family = binomial(link = "logit"), data = df)  # Null model
null_logit_Y4 <- glm(Y4_binary ~ 1, family = binomial(link = "logit"), data = df)  # Null model


# Step 3:Fit the logistic regression model
logistic_model_Y3 <- glm(Y3_binary ~ Z1 + Z2 + Z3 + Z43 + Z44 + Z45 + Z5 + F1 + F2 + F3 + F41 + F42 + F43 + F5, 
                         family = binomial(link = "logit"), data = df, na.action = na.omit)
logistic_model_Y4 <- glm(Y4_binary ~ Z1 + Z2 + Z3 + Z42 + Z43 + Z44 + Z45 + Z5 + F1 + F2 + F3 + F41 + F42 + F43 + F5, 
                         family = binomial(link = "logit"), data = df, na.action = na.omit)


# Check the summaries of the models
cat("\n### Logistic Regression Model Summary ###\n")
summary(logistic_model_Y3)
# McFadden's R²
mcfadden_r2_Y3 <- 1 - (logistic_model_Y3$deviance / null_logit_Y3$deviance)
cat("McFadden's R² for Y3 =", mcfadden_r2_Y3, "\n")
summary(logistic_model_Y4)
# McFadden's R²
mcfadden_r2_Y4 <- 1 - (logistic_model_Y4$deviance / null_logit_Y4$deviance)
cat("McFadden's R² for Y4 =", mcfadden_r2_Y4, "\n")

print(colSums(is.na(df)))  # Check for missing values in the dataset
table(df$selection)  # Check the distribution of the response variable

# Likelihood Ratio Test
lr_test_Y3 <- anova(null_logit_Y3, logistic_model_Y3, test = "LRT")
lr_test_Y4 <- anova(null_logit_Y4, logistic_model_Y4, test = "LRT")
cat("Likelihood Ratio Test for Y3:\n")
print(lr_test_Y3)
cat("Likelihood Ratio Test for Y4:\n")
print(lr_test_Y4)

### Check: logistic plot (different aspects check) ###
# Plot for logistic regression
## Coefficient
# Step 1: Extract coefficients and confidence intervals
coef_summary_Y3 <- tidy(logistic_model_Y3) %>% 
  mutate(term = reorder(term, estimate))

coef_summary_Y4 <- tidy(logistic_model_Y4) %>% 
  mutate(term = reorder(term, estimate))

# Step 2: Coefficient Plot for Y3
ggplot(coef_summary_Y3, aes(x = term, y = estimate)) +
  geom_point() +
  geom_errorbar(aes(ymin = estimate - std.error * 1.96, ymax = estimate + std.error * 1.96), width = 0.2) +
  coord_flip() +
  labs(title = "Coefficient Plot for Y3 (log)", x = "Variables", y = "Estimate") +
  theme_minimal()

# Step 3: Coefficient Plot for Y4
ggplot(coef_summary_Y4, aes(x = term, y = estimate)) +
  geom_point() +
  geom_errorbar(aes(ymin = estimate - std.error * 1.96, ymax = estimate + std.error * 1.96), width = 0.2) +
  coord_flip() +
  labs(title = "Coefficient Plot for Y4 (log)", x = "Variables", y = "Estimate") +
  theme_minimal()

## ROC
# Step 1: Generate predictions for Y3 and Y4
predictions_Y3 <- predict(logistic_model_Y3, type = "response")
predictions_Y4 <- predict(logistic_model_Y4, type = "response")

# Step 2: ROC Curve for Y3
roc_Y3 <- roc(df$Y3_binary, predictions_Y3)

# Calculate AUC and Confidence Interval for Y3
auc_Y3 <- auc(roc_Y3)
ci_Y3 <- ci.auc(roc_Y3)

# Calculate standard error and z-score for the AUC
se_auc_Y3 <- sqrt((auc_Y3 * (1 - auc_Y3)) / length(roc_Y3$case))
z_score_Y3 <- auc_Y3 / se_auc_Y3

# Calculate p-value for the AUC
pval_Y3 <- 2 * (1 - pnorm(abs(z_score_Y3)))

# Plot ROC Curve for Y3
plot(roc_Y3, main = "ROC Curve for Y3", col = "blue", lwd = 2)

# Add AUC and CI to the plot
text(0.5, 0.1, paste("AUC =", round(auc_Y3, 2)), col = "blue")
text(0.5, 0.05, paste("95% CI =", round(ci_Y3[1], 2), "-", round(ci_Y3[3], 2)), col = "blue")
text(0.5, 0.0, paste("p-value =", round(pval_Y3, 2)), col = "blue")

# Step 3: ROC Curve for Y4
roc_Y4 <- roc(df$Y4_binary, predictions_Y4)

# Calculate AUC and Confidence Interval for Y4
auc_Y4 <- auc(roc_Y4)
ci_Y4 <- ci.auc(roc_Y4)

# Calculate standard error and z-score for the AUC
se_auc_Y4 <- sqrt((auc_Y4 * (1 - auc_Y4)) / length(roc_Y4$case))
z_score_Y4 <- auc_Y4 / se_auc_Y4

# Calculate p-value for the AUC: Derived the two-tailed p-value from the z-score.
pval_Y4 <- 2 * (1 - pnorm(abs(z_score_Y4)))

# Plot ROC Curve for Y4
plot(roc_Y4, main = "ROC Curve for Y4 (+Z42, 1-2:0; 3-5:1)", col = "red", lwd = 2)

# Add AUC and CI to the plot
text(0.5, 0.1, paste("AUC =", round(auc_Y4, 2)), col = "red")
text(0.5, 0.05, paste("95% CI =", round(ci_Y4[1], 2), "-", round(ci_Y4[3], 2)), col = "red")
text(0.5, 0.0, paste("p-value =", round(pval_Y4, 2)), col = "red")

##
# Generate new data for prediction
new_data <- data.frame(
  Z1 = seq(min(df$Z1), max(df$Z1), length.out = 100),
  Z2 = mean(df$Z2),  # Set other variables to their mean or a specific value
  Z3 = mean(df$Z3),
  Z43 = mean(df$Z43),
  Z44 = mean(df$Z44),
  Z45 = mean(df$Z45),
  Z5 = mean(df$Z5),
  F1 = mean(df$F1),
  F2 = mean(df$F2),
  F3 = mean(df$F3),
  F41 = mean(df$F41),
  F42 = mean(df$F42),
  F43 = mean(df$F43),
  F5 = mean(df$F5)
)

# Predict probabilities for Y3
new_data$prob_Y3 <- predict(logistic_model_Y3, newdata = new_data, type = "response")

# Predicted Probability Plot for Y3
ggplot(new_data, aes(x = Z1, y = prob_Y3)) +
  geom_line() +
  labs(title = "Predicted Probability of Willingness to Pay (Y3) vs Z1", 
       x = "Z1", y = "Predicted Probability") +
  theme_minimal()

# Predict probabilities for Y4
new_data$prob_Y4 <- predict(logistic_model_Y4, newdata = new_data, type = "response")

# Predicted Probability Plot for Y4
ggplot(new_data, aes(x = Z1, y = prob_Y4)) +
  geom_line() +
  labs(title = "Predicted Probability of Willingness to Pay (Y4) vs Z1", 
       x = "Z1", y = "Predicted Probability") +
  theme_minimal()

### Economic: PQ demand plot ###
# Calculate quantities from original data
y3_counts <- as.data.frame(table(df$Y3))
y4_counts <- as.data.frame(table(df$Y4))

# Convert Freq to numeric and Var1 to numeric for proper ordering
y3_data <- data.frame(
  price = as.numeric(as.character(y3_counts$Var1)),
  quantity = y3_counts$Freq
)

y4_data <- data.frame(
  price = as.numeric(as.character(y4_counts$Var1)),
  quantity = y4_counts$Freq
)

# Create the plot
ggplot() +
  geom_line(data = y3_data, aes(x = quantity, y = price, color = "Y3"), size = 1) +
  geom_line(data = y4_data, aes(x = quantity, y = price, color = "Y4"), size = 1) +
  geom_point(data = y3_data, aes(x = quantity, y = price, color = "Y3"), size = 3) +
  geom_point(data = y4_data, aes(x = quantity, y = price, color = "Y4"), size = 3) +
  # Add labels for Y3
  geom_text(data = y3_data, 
            aes(x = quantity, y = price, label = quantity),
            vjust = -0.8, 
            color = "blue") +
  # Add labels for Y4
  geom_text(data = y4_data, 
            aes(x = quantity, y = price, label = quantity),
            vjust = -0.8, 
            color = "red") +
  scale_y_continuous(breaks = 1:5) +
  labs(title = "WTP Demand Plot",
       x = "Quantity (Number of Respondents)",
       y = "Price Level",
       color = "Variable") +
  theme_minimal() +
  scale_color_manual(values = c("Y3" = "blue", "Y4" = "red"))


### User attitude ###
# Step 4: Define a Selection Variable for Heckman Model
# Convert df$Y3 from factor to numeric safely
df$Y3 <- as.numeric(as.character(df$Y3))

# Check for NA values after conversion
if (any(is.na(df$Y3))) {
  cat("Warning: NA values found in df$Y3 after conversion.\n")
}
# Create a binary selection variable (adjust based on your criteria)
df$selection <- ifelse(df$Y3 < 4, 1, 0)  # Example condition for selection

# Remove rows with missing data (or use imputation)
df_clean <- na.omit(df)

# Check the structure of your dataframe to identify types
str(df_clean)

# Convert relevant columns to numeric if they are not
numeric_cols <- c("Z1", "Z3", "Z54", "Z55", "Z56", "X8", "X9")

# Convert columns to numeric
df_clean[numeric_cols] <- lapply(df_clean[numeric_cols], function(x) {
  if (is.factor(x)) {
    as.numeric(as.character(x))  # Convert factor to numeric
  } else {
    as.numeric(x)  # Convert to numeric
  }
})

# Check again to ensure all conversions were successful
str(df_clean[numeric_cols])

# Heckman model for Y3 using Two-Step method
heckman_model_Y3_2S <- heckit(selection ~  F2 + F3 + F42 + F5,  # Selection model
                              Y3 ~ Z1 + Z3 + Z54 + Z55 + Z56 + X8 + X9,     # Outcome model
                              data = df_clean)

# Summary of the Two-Step model for Y3
cat("\n### Heckman Model Summary (Two-Step) for Y3 ###\n")
summary(heckman_model_Y3_2S)

model_summary <- summary(heckman_model_Y3_2S)
print(model_summary)

# Extract coefficients from the outcome model directly from the summary
outcome_coef <- model_summary$coefficients[["Outcome"]]

# Prepare a model matrix for the predicted values
model_matrix <- model.matrix(~ Z1 + Z3 + Z54 + Z55 + Z56 + X8 + X9, data = df_clean)

# Check dimensions
cat("Dimensions of model matrix:", dim(model_matrix), "\n")
cat("Length of outcome coefficients:", length(outcome_coef), "\n")

# Calculate predicted values
predicted_values <- model_matrix %*% outcome_coef

# Calculate residuals
actual_values <- df_clean$Y3
residuals <- actual_values - predicted_values

# Check for NA values in residuals
if (any(is.na(residuals))) {
  stop("Residuals contain NA values. Check your predicted values or actual values.")
}

# Calculate log-likelihood
logLik <- -0.5 * sum((residuals^2) / var(residuals)) - length(residuals) * log(sqrt(2 * pi * var(residuals)))

# Number of parameters (including the intercept)
num_params <- length(outcome_coef)  # Coefficients from the outcome model

# Calculate AIC and BIC
aic_Y3 <- -2 * logLik + 2 * num_params
bic_Y3 <- -2 * logLik + log(length(actual_values)) * num_params

# Print AIC and BIC
cat("\n### Model Fit Statistics for Y3 ###\n")
cat("AIC:", aic_Y3, "\n")
cat("BIC:", bic_Y3, "\n")

# Repeat for another independent variables

# Create a binary selection variable for Y4
df$selection_Y4 <- ifelse(df$Y4 < 4, 1, 0)  # Example condition for selection

# Heckman model for Y4 using Two-Step method
heckman_model_Y4_2S <- heckit(selection ~ F2 + F3 + F42 + F5,  # Selection model
                              Y4 ~  Z1 + Z2 + Z3 + Z54 + Z55 + Z56 + X8 + X9 + X10,     # Outcome model
                              data = df)

# Summary of the Two-Step model for Y4
cat("\n### Heckman Model Summary (Two-Step) for Y4 ###\n")
summary(heckman_model_Y4_2S)
