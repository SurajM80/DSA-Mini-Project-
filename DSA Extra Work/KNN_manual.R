#Part 1:
# Calculates the straight-line distance between two points (a and b)
euclid <- function(a, b) {
  sqrt(sum((a - b)^2))
}

#Part2:
knn_manual <- function(trainX, trainY, testX, k = 5) {
  
  # 1. Go through each row in the test data
  apply(testX, 1, function(tx) {
    
    # tx is now ONE single row from testX that we want to predict
    
    # 2. Calculate distance from this one test row (tx)
    #    to EVERY row in the training data (trainX)
    d <- apply(trainX, 1, function(tr) euclid(tx, tr))
    
    # 3. Find the 'k' nearest neighbors' LABELS
    #    order(d) gets the *indices* (row numbers) of the closest neighbors
    #    trainY[...] retrieves the labels (e.g., "setosa") for those indices
    #    [1:k] keeps only the top 'k' closest labels
    cls <- trainY[order(d)][1:k]
    
    # 4. Perform the "majority vote"
    #    table(cls) counts the votes (e.g., setosa=3, versicolor=2)
    #    sort(...) orders them from most votes to least
    #    names(...)[1] gets the NAME of the class with the most votes
    names(sort(table(cls), decreasing = TRUE))[1]
    
  })
}

#Part3:How It Works in Practice
# 1. Prepare Data ---
# Use the first 100 rows (setosa & versicolor) for simplicity
data <- iris[1:100, ]

# Training features (the X values)
trainX <- as.matrix(data[, c("Sepal.Length", "Sepal.Width")])

# Training labels (the Y answers)
trainY <- data$Species

# Create two new "test" flowers we want to predict
testX <- as.matrix(rbind(
  c(5.0, 3.5),  # Should be setosa
  c(6.0, 2.5)   # Should be versicolor
))

# 2. Run the Manual kNN Function ---
# We will use k=3 (look at the 3 nearest neighbors)
predictions <- knn_manual(trainX, trainY, testX, k = 3)

# --- 3. View the Results ---
print("Test Data:")
print(testX)
print("Our Model's Predictions:")
print(predictions)

#output : The model correctly predicted the species for both new flowers based on their neighbors in the training data.