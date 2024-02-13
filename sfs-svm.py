
import pandas as pd
from sklearn.model_selection import train_test_split
from sklearn.svm import SVC
from mlxtend.feature_selection import SequentialFeatureSelector
from sklearn.metrics import confusion_matrix, accuracy_score, classification_report

# Read the data
my_data = pd.read_csv("C:/Users/12408/Desktop/INST737/balanced_data.csv")

# Specify categorical columns
categorical_columns = ["Diabetes", "Sex", "Age", "COVIDPOS", "Smoker", "Physical_activity", "Food_shortage", "MEDCOST1", "General_health", "Income_category", "Stroke", "Walking_difficulty", "HeartDiseaseorAttack"]

# Convert specified columns to categorical
my_data[categorical_columns] = my_data[categorical_columns].astype('category')

# Set seed for reproducibility
seed = 123

# Create train and test sets
train_data, test_data = train_test_split(my_data, test_size=0.3, random_state=seed)

# Specify the target variable
target_variable = "Diabetes"

# Define features and target variable
X_train = train_data.drop(target_variable, axis=1)
y_train = train_data[target_variable]
X_test = test_data.drop(target_variable, axis=1)
y_test = test_data[target_variable]

# Define SVM model
svm_model_initial = SVC(kernel='rbf', random_state=seed)

svm_model_initial.fit(X_train, y_train)

# Use the initial model to make predictions on the test set
svm_predictions_initial = svm_model_initial.predict(X_test)

# Evaluate the performance of the initial model
conf_matrix_initial = confusion_matrix(y_test, svm_predictions_initial)
report_initial = classification_report(y_test, svm_predictions_initial)

print("Confusion Matrix:")
print(conf_matrix_initial)
print("report:", report_initial)
# Perform Forward Feature Selection
feature_selector = SequentialFeatureSelector(svm_model_initial, forward=True, k_features='best', scoring='accuracy', cv=5)
feature_selector.fit(X_train, y_train)

# Get the selected feature indices
selected_feature_indices = feature_selector.k_feature_idx_

selected_feature_indices = list(selected_feature_indices)


# Print selected features
selected_features = X_train.columns[selected_feature_indices]
print("Selected Features:", selected_features)

# Train SVM model using selected features
svm_model_selected = SVC(kernel='rbf', random_state=seed)
svm_model_selected.fit(X_train[selected_features], y_train)

# Use the trained model to make predictions on the test set
svm_predictions = svm_model_selected.predict(X_test[selected_features])

# Evaluate the performance
conf_matrix_new = confusion_matrix(y_test, svm_predictions)
report_new = classification_report(y_test, svm_predictions)

print("Confusion Matrix:")
print(conf_matrix_new)
print("report:", report_new)
accuracy = accuracy_score(y_test, svm_predictions)
print(accuracy)
