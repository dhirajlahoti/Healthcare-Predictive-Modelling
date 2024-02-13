library(caret)

my_data <- read.csv("C:/Users/12408/Desktop/INST737/balanced_data.csv")
categorical_columns <- c("Diabetes", "Sex", "Age", "COVIDPOS", "Smoker", "Physical_activity", "Food_shortage", "MEDCOST1", "General_health", "Income_category", "Stroke", "Walking_difficulty", "HeartDiseaseorAttack")

#specified columns to categorical
my_data[categorical_columns] <- lapply(my_data[categorical_columns], as.factor)
train_control <- trainControl(method='repeatedcv', number=10, repeats=3)
train_control_subsampl <- trainControl(method = 'boot', number = 50)
train_control_split <- trainControl(method = 'LGOCV', p = 0.8)

set.seed(123)
 
model_svm <- train(Diabetes ~ ., data=my_data, method ="svmRadial", control= train_control) 
model_random_forest <- train(Diabetes ~ ., data=my_data, method ="rf", control= train_control, verbose=FALSE )

model_svm_2 <- train(Diabetes ~ ., data=my_data, method ="svmRadial", control= train_control_subsampl) 
model_random_forest_2 <- train(Diabetes ~ ., data=my_data, method ="rf", control= train_control_subsampl, verbose=FALSE )

model_svm_3 <- train(Diabetes ~ ., data=my_data, method ="svmRadial", control= train_control_split) 
model_random_forest_3 <- train(Diabetes ~ ., data=my_data, method ="rf", control= train_control_split, verbose=FALSE )

##############################################
  
data_subset <- my_data[, c('BMI', 'Average_drink', 'Alcohol_consumed', 'HeartDiseaseorAttack', 'Stroke', 'Physical_activity', 'Diabetes')]
data_subset$Diabetes <- as.factor(data_subset$Diabetes)

minMax <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}

data_subset[c('BMI', 'Average_drink', 'Alcohol_consumed')] <- lapply(data_subset[c('BMI', 'Average_drink', 'Alcohol_consumed')], minMax)
hyperparameter_grid <- expand.grid(
  size = c(5, 3, 3),  # Number of neurons in the hidden layer
  decay = 0.01
)

model_nn <- train(Diabetes~ BMI +Average_drink +Alcohol_consumed + Physical_activity +Stroke + HeartDiseaseorAttack, data=data_subset, method ="nnet", control= train_control, tuneGrid = hyperparameter_grid, act.fct = "tanh")

model_nn_2 <- train(Diabetes~ BMI +Average_drink +Alcohol_consumed + Physical_activity +Stroke + HeartDiseaseorAttack, data=data_subset, method ="nnet", control= train_control_subsampl, tuneGrid = hyperparameter_grid, act.fct = "tanh")
model_nn_3 <- train(Diabetes~ BMI +Average_drink +Alcohol_consumed + Physical_activity +Stroke + HeartDiseaseorAttack, data=data_subset, method ="nnet", control= train_control_split, tuneGrid = hyperparameter_grid, act.fct = "tanh")


results <- resamples(list(RF=model_random_forest, SVM= model_svm, NeuralNet = model_nn))
summary(results)
bwplot(results)

results_2 <- resamples(list(RF=model_random_forest_2, SVM= model_svm_2, NeuralNet = model_nn_2))
summary(results_2)
bwplot(results_2)

results_3<- resamples(list(RF=model_random_forest_3, SVM= model_svm_3, NeuralNet = model_nn_3))
summary(results_3)
bwplot(results_3)
