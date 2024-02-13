install.packages("kernlab")
library(kernlab)
install.packages("caret")
library(caret)

install.packages("openxlsx")
library(openxlsx)

my_data <- read.csv("C:/Users/12408/Desktop/INST737/balanced_data.csv")
categorical_columns <- c("Diabetes", "Sex", "Age", "COVIDPOS", "Smoker", "Physical_activity", "Food_shortage", "MEDCOST1", "General_health", "Income_category", "Stroke", "Walking_difficulty", "HeartDiseaseorAttack")

#specified columns to categorical
my_data[categorical_columns] <- lapply(my_data[categorical_columns], as.factor)
table(my_data$Diabetes)

set.seed(123)
#balanced_data <-downSample(x = my_data[, -1], y = my_data$Diabetes)

#write.xlsx(balanced_data, file = "C:/Users/12408/Desktop/INST737/balanced_data.xlsx", sheetName = "BalancedData", row.names = FALSE)

#table(balanced_data$Diabetes)
#print(table(balanced_data$Class))

sample_indices <- sample(1:nrow(my_data), 0.7 * nrow(my_data))  # 70% train, 30% test
train_data <- my_data[sample_indices, ]
test_data <- my_data[-sample_indices, ]

diabetes_classifier<- ksvm(Diabetes~., data= train_data, kernel ="vanilladot")

diabetes_classifier

diabetes_prediction<- predict(diabetes_classifier, test_data)
diabetes_prediction

conf_matrix <- confusionMatrix(diabetes_prediction, test_data$Diabetes)
print(conf_matrix)

# rbf kernel

train_proportion <- 0.7  # 70% for training

# Generate random sample indices for training
sample_indices <- sample(1:nrow(my_data), size = round(train_proportion * nrow(my_data)))

# Create the training dataset using the sampled indices
train_data_rbf <- my_data[sample_indices, ]
diabetes_classifier_rbf<- ksvm(Diabetes~., data= train_data_rbf, kernel ="rbfdot")



diabetes_prediction_rbf<- predict(diabetes_classifier_rbf, test_data)
diabetes_prediction_rbf


conf_matrix_rbf <- confusionMatrix(diabetes_prediction_rbf, test_data$Diabetes)
print(conf_matrix_rbf)
