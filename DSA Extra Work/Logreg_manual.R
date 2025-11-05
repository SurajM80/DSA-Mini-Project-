# 1. SETUP ---

# Load the built-in 'mtcars' dataset
data("mtcars")
# We want to predict 'am' (transmission: 0=auto, 1=manual)
# using 'wt' (weight) and 'hp' (horsepower)

# Create the feature matrix 'X'
X <- as.matrix(cbind(1, mtcars[, c("wt","hp")]))
# Create the target vector 'y'
y <- mtcars$am

# 2. HELPER FUNCTION ---

# The Sigmoid (or Logistic) function
# It squashes any number into a value between 0 and 1
sig <- function(z) {
  1 / (1 + exp(-z))
}

# 3. INITIALIZATION ---

# Initialize the weights (coefficients) to zero.
# We need one weight for each column in X (Intercept, wt, hp)
w <- matrix(0, ncol(X))

# Set the learning rate 'alpha'
# This controls how big of a step we take during training
alpha <- 0.01

# 4. TRAINING LOOP (GRADIENT DESCENT) ---

for (it in 1:5000) {
  
# Step 1: Make a prediction (Probability)
# Calculate the "weighted sum" (X %*% w)
# and run it through the sigmoid function.
p <- sig(X %*% w)
  
# Step 2: Calculate the error and the gradient
# 'grad' is the "direction of error" for each weight.
# It tells us *how* to change each weight to reduce the error.
grad <- t(X) %*% (p - y) / length(y)
  
# Step 3: Update the weights
# We "nudge" the weights in the *opposite* direction
# of the gradient, scaled by our learning rate.
w <- w - alpha * grad
}

# 5. EVALUATION ---

# Print the final learned weights
print("Final Learned Weights:")
print(w)

# 1. Get final probabilities with the trained weights
prob <- sig(X %*% w)

# 2. Convert probabilities into predictions (0 or 1)
# If probability > 50%, predict 1 (manual). Else, predict 0 (auto).
pred <- ifelse(prob > 0.5, 1, 0)

# 3. Calculate accuracy
# Compare our predictions ('pred') to the true answers ('y')
acc <- mean(pred == y)

print(paste("Final Model Accuracy:", round(acc * 100, 2), "%"))
