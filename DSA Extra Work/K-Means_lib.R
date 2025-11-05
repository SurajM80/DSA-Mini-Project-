# 1. Prepare Data ---
# We use the 'iris' dataset but remove the 'Species' column (column 5)
# k-Means is unsupervised, so it must find groups without the "answer".
iris_features <- iris[, 1:4]

# --- 2. Scale the Data (Critical Step) ---
# k-Means uses distance, so we scale features to have a mean of 0 
# and a standard deviation of 1. This prevents features with
# large ranges (like Petal.Length) from dominating the algorithm.
iris_scaled <- scale(iris_features)

# --- 3. Run the k-Means Algorithm ---
# We set 'centers = 3' because we expect 3 groups (like the 3 species).
# 'set.seed()' ensures your random starting points are the same as mine,
# so you get the same results. 'nstart' runs the algorithm 25 times
# with different random starting points and picks the best result.
set.seed(42) 
km_result <- kmeans(iris_scaled, centers = 3, nstart = 25)

# --- 4. Analyze the Results ---

# Print a summary of the model
print(km_result)

# 'km_result$cluster' contains the cluster number (1, 2, or 3) for each flower.
# Let's compare the clusters it found to the real species.
cat("\n--- Comparison Table (Found Clusters vs. Real Species) ---\n")
comparison_table <- table(Actual_Species = iris$Species, Cluster_Found = km_result$cluster)
print(comparison_table)

# --- 5. Visualize the Clusters ---
# We plot the original (unscaled) data, but use the 'km_result$cluster'
# to color each point. This lets us see the groups the algorithm found.
plot(iris_features[, c("Petal.Length", "Petal.Width")],
     col = km_result$cluster,
     main = "k-Means Clustering Results (k=3)",
     xlab = "Petal Length",
     ylab = "Petal Width",
     pch = 20, # Use solid circles
     cex = 2)  # Make circles larger

# Add the final cluster centers (centroids) to the plot
points(km_result$centers[, c("Petal.Length", "Petal.Width")],
       col = 1:3,
       pch = 4,   # A cross 'X' symbol
       cex = 4,   # Make 'X' large
       lwd = 4)  # Make 'X' thick

