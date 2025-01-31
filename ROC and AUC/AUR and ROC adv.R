# Load required packages
library(tidyverse)         # For data manipulation and visualization
theme_set(theme_bw())      # Beautifies plots with a clean theme

# Get the data
d <- carData::TitanicSurvival |> filter(!(is.na(age)))  # Filter out rows with missing age values

# Conduct logistic regression
m <- glm(survived ~ sex + age + passengerClass, d, family = binomial)

# Add predicted probabilities to your data
d$predicted_glm <- predict(m, type = "response")  # Predict probabilities of survival

# Look at the data
glimpse(d)  # Quick summary of the dataset

#install.packages("cutpointr")
# Load the cutpointr package
library(cutpointr)

# Determine the optimal cutpoint
result <- cutpointr(
  data  = d,               # The dataset
  x     = predicted_glm,   # Predicted probabilities from the logistic regression
  class = survived          # Binary outcome variable
)

# Transpose the result for better readability
result |> t()  # `t()` transposes the output

# Plot the ROC curve
plot_roc(result)

# Plot performance metrics
plot_metric(result)




#install.packages("patchwork")
# Load the patchwork package (used for combining plots)
library(patchwork)

# Use Youden's J statistic to maximize the metric
cutpoint(
  data    = d,
  x       = predicted_glm,         # Predicted probabilities
  class   = survived,              # Binary outcome variable
  method  = maximize_metric,       # Method to maximize a specific metric
  metric  = youden                 # Metric to optimize (Youden's J statistic)
) |> 
  plot_metric() +                    # Visualize metrics across cutpoints
  # Combines plots using patchwork
  
  # Use F1 score to maximize the metric
  cutpoint(
    data    = d,
    x       = predicted_glm,         # Predicted probabilities
    class   = survived,              # Binary outcome variable
    method  = maximize_metric,       # Method to maximize a specific metric
    metric  = F1_score               # Metric to optimize (F1 score)
  ) |> 
  plot_metric()

# Sensitive case: Minimize misclassification cost
sensitive_case <- cutpoint(
  data          = d,
  x             = predicted_glm,         # Predicted probabilities
  class         = survived,              # Binary outcome variable
  method        = minimize_metric,       # Method to minimize a specific metric
  metric        = misclassification_cost, # Metric to minimize
  cost_fp       = 1,                     # Cost of false positives
  cost_fn       = 10                     # Cost of false negatives
)

# View summary of the sensitive case results
sensitive_case |> summary()

# Plot the ROC curve for the sensitive case
sensitive_case |> plot_roc()






# Install and load patchwork (if not already installed)
# install.packages("patchwork")
library(patchwork)

# Use Youden's J statistic to maximize the metric
youden_result <- cutpoint(
  data    = d,
  x       = predicted_glm,         # Predicted probabilities
  class   = survived,              # Binary outcome variable
  method  = maximize_metric,       # Method to maximize a specific metric
  metric  = youden                 # Metric to optimize (Youden's J statistic)
)

# Plot for Youden's J statistic
youden_plot <- youden_result |> plot_metric()

# Use F1 score to maximize the metric
f1_result <- cutpoint(
  data    = d,
  x       = predicted_glm,         # Predicted probabilities
  class   = survived,              # Binary outcome variable
  method  = maximize_metric,       # Method to maximize a specific metric
  metric  = F1_score               # Metric to optimize (F1 score)
)

# Plot for F1 score
f1_plot <- f1_result |> plot_metric()

# Combine the two plots using patchwork
combined_plot <- youden_plot + f1_plot
print(combined_plot)

# Sensitive case: Minimize misclassification cost
sensitive_case <- cutpoint(
  data          = d,
  x             = predicted_glm,         # Predicted probabilities
  class         = survived,              # Binary outcome variable
  method        = minimize_metric,       # Method to minimize a specific metric
  metric        = misclassification_cost, # Metric to minimize
  cost_fp       = 1,                     # Cost of false positives
  cost_fn       = 10                     # Cost of false negatives
)

# View summary of the sensitive case results
sensitive_summary <- sensitive_case |> summary()
print(sensitive_summary)

# Plot the ROC curve for the sensitive case
roc_plot <- sensitive_case |> plot_roc()
print(roc_plot)

