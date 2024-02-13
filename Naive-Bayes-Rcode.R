# Load necessary libraries
library(e1071)

# Load your dataset (replace 'your_dataset.csv' with your actual dataset file)
data <- read.csv("C:/Users/dhira/OneDrive/Desktop/filtered_data.csv")

# Split the dataset into training and testing sets (e.g., 70% training and 30% testing)
set.seed(123)  # For reproducibility
sample_index <- sample(1:nrow(data), 0.7 * nrow(data))
train_data <- data[sample_index, ]
test_data <- data[-sample_index, ]

# Train a Naive Bayes classifier without Laplace smoothing
nb_classifier <- naiveBayes(Diabetes ~ ., data = train_data)

# Make predictions on the testing set
nb_predictions <- predict(nb_classifier, test_data)

# Create a confusion matrix
confusion_matrix <- table(Actual = test_data$Diabetes, Predicted = nb_predictions)
print("Confusion Matrix (Naive Bayes without Laplace Smoothing):")
print(confusion_matrix)

# Calculate accuracy
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
print(paste("Accuracy (Naive Bayes without Laplace Smoothing):", round(accuracy, 2)))

# Train a Naive Bayes classifier with Laplace (add-one) smoothing
nb_laplace_classifier <- naiveBayes(Diabetes ~ ., data = train_data, laplace = 1)

# Make predictions on the testing set with Laplace smoothing
nb_laplace_predictions <- predict(nb_laplace_classifier, test_data)

# Create a confusion matrix with Laplace smoothing
confusion_matrix_laplace <- table(Actual = test_data$Diabetes, Predicted = nb_laplace_predictions)
print("Confusion Matrix (Naive Bayes with Laplace Smoothing):")
print(confusion_matrix_laplace)

# Calculate accuracy with Laplace smoothing
accuracy_laplace <- sum(diag(confusion_matrix_laplace)) / sum(confusion_matrix_laplace)
round(accuracy_laplace, 2)
      
      