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

install.packages("randomForest")
library(randomForest)

# Train the model
rf_model <- randomForest(Species ~ ., data = train, ntree = 100, mtry = 2)

plot(rf_model)


# View model summary
print(rf_model)

# Predict on test data
predictions <- predict(rf_model, newdata = test)

# View predictions
print(predictions)

# Create a confusion matrix
confusion_matrix <- table(test$Species, predictions)

# Print the confusion matrix
print(confusion_matrix)

# Calculate accuracy
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
print(paste("Accuracy:", accuracy))
# Plot variable importance
varImpPlot(rf_model)

