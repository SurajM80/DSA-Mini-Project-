# 1. Install and Load Libraries ---
# You may need to run these lines once to install the packages:
install.packages("rpart")
install.packages("rpart.plot")

library(rpart)
library(rpart.plot)

# 2. Build the Decision Tree ---
# We will use the built-in 'iris' dataset.
# Formula: "Species ~ ." means "Predict 'Species' using all other columns."
# Method: "class" tells rpart to build a classification tree.

fit <- rpart(Species ~ ., data = iris, method = "class")


# --- 3. Visualize the Tree ---
# This opens a new plot window with the flowchart
cat("Displaying decision tree plot...\n")
rpart.plot(fit)
# 

# --- 4. Make Predictions and Evaluate ---
# Use the 'fit' model to predict the class for the original 'iris' data
predictions <- predict(fit, newdata = iris, type = "class")

# Create a confusion matrix to see how well it did
cat("\n--- Confusion Matrix ---\n")
conf_matrix <- table(Actual = iris$Species, Predicted = predictions)
print(conf_matrix)

# Calculate the overall accuracy
accuracy <- mean(predictions == iris$Species)
cat("\n--- Evaluation ---\n")
cat("Overall Model Accuracy:", round(accuracy * 100, 2), "%\n")
