# 1. Load the library ---
library(class)

# 2. Prepare Data (Same as before) ---
data <- iris[1:100, ]
trainX <- as.matrix(data[, c("Sepal.Length", "Sepal.Width")])
trainY <- data$Species
testX <- as.matrix(rbind(
  c(5.0, 3.5),  # Should be setosa
  c(6.0, 2.5)   # Should be versicolor
))

# 3. Run the Library kNN Function ---
# Note: The function directly returns the predictions
predictions_lib <- knn(train = trainX, 
                       test = testX, 
                       cl = trainY, 
                       k = 3)

# 4. View the Results ---
print("Test Data:")
print(testX)
print("Library Model's Predictions:")
print(predictions_lib)
