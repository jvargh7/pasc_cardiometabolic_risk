# Neural network to predict variable of interest for multiple outcomes
import numpy as np
from keras.models import Sequential
from keras.layers import Dense

# Dummy data
X_train = np.random.random((1000, 10))  # Input features
y_train = np.random.randint(0, 3, (1000,))  # Target labels (3 classes)

# Convert target labels to one-hot encoding
y_train_one_hot = np.zeros((len(y_train), np.max(y_train) + 1))
y_train_one_hot[np.arange(len(y_train)), y_train] = 1

# Build the model
model = Sequential()
model.add(Dense(64, activation='relu', input_dim=10))
model.add(Dense(64, activation='relu'))
model.add(Dense(3, activation='softmax'))  # Output layer with 3 classes

# Compile the model
model.compile(optimizer='adam', loss='categorical_crossentropy', metrics=['accuracy'])

# Train the model
model.fit(X_train, y_train_one_hot, epochs=10, batch_size=32)

# Dummy test data
X_test = np.random.random((100, 10))

# Make predictions on test data
predictions = model.predict(X_test)
predicted_labels = np.argmax(predictions, axis=1)

print("Predicted labels:", predicted_labels)

