View(iris)
summary(iris)
set.seed(2)

install.packages("caTools")
library(caTools)

split <- sample.split(iris, SplitRatio = 0.7)
split

train <- subset(iris, split==TRUE)
test <- subset(iris, split==FALSE)

train
test

# Install the e1071 package if you haven't already
install.packages("e1071")

# Load the library
library(e1071)

# Train the SVM model
svm_model <- svm(Species ~ ., data = train, kernel = "linear")

# View the summary of the model
summary(svm_model)

# Make predictions
predictions <- predict(svm_model, newdata = test)

# View predictions
print(predictions)

# Create a confusion matrix
confusion_matrix <- table(Predicted = predictions, Actual = test$Species)

# Print the confusion matrix
print(confusion_matrix)

# Calculate accuracy
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
print(paste("Accuracy:", round(accuracy * 100, 2), "%"))

# Install ggplot2 if not installed
install.packages("ggplot2")
library(ggplot2)

# Subset the iris data to 2 features for visualization
svm_model_2d <- svm(Species ~ Sepal.Length + Sepal.Width, data = train, kernel = "linear")

# Generate a grid of values
x_range <- seq(min(iris$Sepal.Length), max(iris$Sepal.Length), length = 100)
y_range <- seq(min(iris$Sepal.Width), max(iris$Sepal.Width), length = 100)
grid <- expand.grid(Sepal.Length = x_range, Sepal.Width = y_range)

# Predict on the grid
grid$Species <- predict(svm_model_2d, newdata = grid)

# Plot the decision boundaries
ggplot() +
  geom_point(data = grid, aes(x = Sepal.Length, y = Sepal.Width, color = Species), alpha = 0.2) +
  geom_point(data = train, aes(x = Sepal.Length, y = Sepal.Width, color = Species)) +
  labs(title = "SVM Decision Boundaries", x = "Sepal Length", y = "Sepal Width") +
  theme_minimal()

