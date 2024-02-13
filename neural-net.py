import pandas as pd
import numpy as np
from sklearn.model_selection import train_test_split
from sklearn.preprocessing import MinMaxScaler
from sklearn.neural_network import MLPClassifier
from sklearn.metrics import confusion_matrix, accuracy_score, classification_report


def train_neural_network(X_train, y_train, activation='relu', hidden_layers=(3,), max_iter=1000, random_state=None):
    model = MLPClassifier(hidden_layer_sizes=hidden_layers, activation=activation, max_iter=max_iter, random_state=random_state)
    model.fit(X_train, y_train)
    return model

def evaluate_model(model, X_test, y_test):
    # Make predictions on the test set
    prediction_values = model.predict_proba(X_test)

    # Assign 1 to the class with the highest probability and 0 to others
    binary_predictions = np.zeros_like(prediction_values)
    binary_predictions[np.arange(len(prediction_values)), prediction_values.argmax(1)] = 1

    # Convert to pandas DataFrame
    binary_predictions_df = pd.DataFrame(binary_predictions, columns=model.classes_)

    # Create a confusion matrix
    confusion_matrix_result = confusion_matrix(y_test, binary_predictions_df.idxmax(axis=1))

    # Calculate accuracy
    accuracy_result = accuracy_score(y_test, binary_predictions_df.idxmax(axis=1))

    # Classification Report
    report_result = classification_report(y_test, binary_predictions_df.idxmax(axis=1))

    return confusion_matrix_result, accuracy_result, report_result

# Load the data
my_data = pd.read_csv("C:/Users/12408/Desktop/INST737/balanced_data.csv")

# Select relevant columns
data_subset = my_data[['BMI', 'Average_drink', 'Alcohol_consumed', 'HeartDiseaseorAttack', 'Stroke', 'Physical_activity', 'Diabetes']]

# Convert 'Diabetes' to a factor
data_subset['Diabetes'] = data_subset['Diabetes'].astype('category')

# Min-max scaling
scaler = MinMaxScaler()
data_subset[['BMI', 'Average_drink', 'Alcohol_consumed']] = scaler.fit_transform(data_subset[['BMI', 'Average_drink', 'Alcohol_consumed']])

# Split the data into training and testing sets
train_data, test_data = train_test_split(data_subset, test_size=0.3, random_state=123)

# Specify the activation function and hidden layers
activation_function = 'tanh'
hidden_layers_config = (5,3,3)

# Train the neural network with specified parameters
neural_model = train_neural_network(
    train_data[['BMI', 'Average_drink', 'Alcohol_consumed', 'Physical_activity', 'Stroke', 'HeartDiseaseorAttack']],
    train_data['Diabetes'].cat.codes,
    activation=activation_function,
    hidden_layers=hidden_layers_config,
    random_state=123
)

# Evaluate the model
confusion_matrix_result, accuracy_result, report_result = evaluate_model(
    neural_model,
    test_data[['BMI', 'Average_drink', 'Alcohol_consumed', 'Physical_activity', 'Stroke', 'HeartDiseaseorAttack']],
    test_data['Diabetes']
)

# Print the results
print("Confusion Matrix:\n", confusion_matrix_result)
print("Accuracy:", accuracy_result)
print("Classification Report:\n", report_result)

# model = Sequential()
# model.add(Dense(units=hidden_layers_config[0], input_dim=train_data.shape[1]-1, activation=activation_function))
# model.add(Dense(units=len(neural_model.classes_), activation='softmax'))
# plot_model(model, to_file='neural_network.png', show_shapes=True, show_layer_names=True)
