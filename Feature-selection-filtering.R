install.packages("nnet")
library(nnet)

my_data <- read.csv("C:/Users/12408/Desktop/INST737/balanced_data.csv")
categorical_columns <- c("Diabetes", "Sex", "Age", "COVIDPOS", "Smoker", "Physical_activity", "Food_shortage", "MEDCOST1", "General_health", "Income_category", "Stroke", "Walking_difficulty", "HeartDiseaseorAttack")

#specified columns to categorical
my_data[categorical_columns] <- lapply(my_data[categorical_columns], as.factor)

set.seed(123)  # For reproducibility
sample_indices <- sample(1:nrow(my_data), 0.8 * nrow(my_data))  # 70% train, 30% test
train_data <- my_data[sample_indices, ]
test_data <- my_data[-sample_indices, ]

model <- multinom(Diabetes ~ ., data = train_data)
summary(model)

#Wald tests for significance
wald_test <- summary(model)$coefficients / summary(model)$standard.errors
p_values <- 2 * (1 - pnorm(abs(wald_test)))
print(length(p_values))
sorted_p_values <- sort(p_values[1, ])

# Print the sorted p-values
print(sorted_p_values)








# Predicting the class labels for the test data
predicted_classes <- predict(model, newdata = test_data, "class")

# Calculate accuracy
correct_predictions <- sum(predicted_classes == test_data$Diabetes)
total_predictions <- length(predicted_classes)
accuracy <- correct_predictions / total_predictions
cat("Accuracy:",accuracy,"\n")

# Prediction and accuracy with feature selection
model_new <- multinom(Diabetes ~ BMI+ Age+General_health+ Income_category + Alcohol_consumed, data = train_data)
predicted_classes <- predict(model_new, newdata = test_data, "class")
correct_predictions <- sum(predicted_classes == test_data$Diabetes)
total_predictions <- length(predicted_classes)
accuracy <- correct_predictions / total_predictions
cat("Accuracy with feature selection:",accuracy,"\n")


