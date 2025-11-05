# --- 1. Helper Function (Euclidean Distance) ---
euclid <- function(a, b) {
  sqrt(sum((a - b)^2))
}

# --- 2. Prepare Data and Set Parameters ---
# Use a matrix for easier math
iris_matrix <- as.matrix(iris[, 1:4])
k <- 3
num_iterations <- 10

# --- 3. Initialization ---
# Randomly pick 'k' rows from the data to be the first centroids
set.seed(42)
centroids <- iris_matrix[sample(1:nrow(iris_matrix), k), ]
cat("Starting (Random) Centroids:\n")
print(centroids)

# We need a vector to store the cluster assignment for each row
assignments <- rep(0, nrow(iris_matrix))

# --- 4. Main Algorithm Loop ---
for (iter in 1:num_iterations) {
  
  # --- ASSIGNMENT STEP ---
  # Go through each data point
  for (i in 1:nrow(iris_matrix)) {
    
    # Calculate distance from this point to all 'k' centroids
    distances <- rep(0, k)
    for (j in 1:k) {
      distances[j] <- euclid(iris_matrix[i, ], centroids[j, ])
    }
    
    # Assign the point to the closest centroid (the one with the minimum distance)
    assignments[i] <- which.min(distances)
  }
  
  # --- UPDATE STEP ---
  # Calculate the new mean for each cluster
  for (j in 1:k) {
    # Get all points assigned to this cluster
    cluster_points <- iris_matrix[assignments == j, ]
    
    # Safety check: If a cluster is empty, don't update its centroid
    if (is.matrix(cluster_points) && nrow(cluster_points) > 0) {
      # Calculate the column means (average) of these points
      centroids[j, ] <- colMeans(cluster_points)
    }
  }
}

# --- 5. Final Results ---
cat("\nFinal Centroids (Manual):\n")
print(centroids)

cat("\nFinal Assignments (Manual):\n")
print(head(assignments, 10))

# Compare manual assignments to real species
cat("\nComparison Table (Manual vs. Real Species):\n")
table(Actual_Species = iris$Species, Cluster_Found = assignments)

