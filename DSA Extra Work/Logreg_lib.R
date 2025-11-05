# --- 1. Build the Model ---
# We use the 'mtcars' dataset directly
# Formula: 'am ~ wt + hp' means "predict 'am' using 'wt' and 'hp'"
# Family: 'binomial(link = "logit")' specifies logistic regression

glm_model <- glm(am ~ wt + hp, data = mtcars, family = binomial(link = "logit"))

# 2. Analyze the Results ---

# Use summary() to see the coefficients, p-values, etc.
summary(glm_model)

# 1. Get Predictions from glm_model ---
# 'type = "response"' tells glm() to output probabilities (from 0 to 1)
# This is the same as your manual 'p <- sig(X %*% w)'
probabilities_library <- predict(glm_model, type = "response")

# 2. Convert Probabilities to Predictions ---
predictions_library <- ifelse(probabilities_library > 0.5, 1, 0)

# 3. Calculate Accuracy ---
accuracy_library <- mean(predictions_library == mtcars$am)

cat("Manual Code Accuracy:", "87.5%", "\n") # From your previous code
cat("Library (glm) Accuracy:", round(accuracy_library * 100, 2), "%", "\n")