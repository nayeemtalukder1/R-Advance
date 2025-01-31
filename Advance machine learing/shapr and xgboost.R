# Load necessary libraries
library(shapr)
library(xgboost)

# Simulate some data
set.seed(123)
data <- data.frame(
  x1 = rnorm(100),
  x2 = rnorm(100),
  y = rnorm(100)
)

# Train an XGBoost model
X_train <- as.matrix(data[, c("x1", "x2")])
y_train <- data$y

model <- xgboost(
  data = X_train,
  label = y_train,
  nrounds = 50,
  objective = "reg:squarederror",
  verbose = 0
)

# Prepare the `shapr` explainer
explainer <- shapr(X_train, model)

# Create a new dataset to explain predictions
X_test <- X_train[1:5, ]

# Compute SHAP values
shap_values <- explain(X_test, approach = "empirical", explainer = explainer)

# Plot SHAP values
plot(shap_values)



library(shapr)
library(xgboost)

# Simulate some data
set.seed(123)
data <- data.frame(
  x1 = rnorm(100),
  x2 = rnorm(100),
  y = rnorm(100)
)

# Train an XGBoost model
X_train <- as.matrix(data[, c("x1", "x2")])
y_train <- data$y

model <- xgboost(
  data = X_train,
  label = y_train,
  nrounds = 50,
  objective = "reg:squarederror",
  verbose = 0
)

# Prepare the `shapr` explainer
explainer <- shapr(X_train, model)

# Create a new dataset to explain predictions
X_test <- X_train[1:5, ]

# Compute the base prediction (mean of training predictions)
prediction_zero <- mean(predict(model, newdata = X_train))

# Compute SHAP values (Fixed version)
shap_values <- explain(X_test, approach = "empirical", explainer = explainer, prediction_zero = prediction_zero)

# Plot SHAP values
plot(shap_values)


