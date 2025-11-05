data("mtcars")
mtcars

set.seed(42)
idx <- sample(seq_len(nrow(iris)), 0.7*nrow(iris))
train <- iris[idx,]; test <- iris[-idx,]

#Linear Regression (Manual)
X <- as.matrix(cbind(1, mtcars[, c("wt","hp")]))
y <- mtcars$mpg
beta_hat <- solve(t(X) %*% X) %*% t(X) %*% y
y_hat <- X %*% beta_hat
rmse <- sqrt(mean((y - y_hat)^2))
r2 <- 1 - sum((y - y_hat)^2)/sum((y - mean(y))^2)

#------------------------------------------------------------------------------
# --- 1. Use the same data ---
temperature <- c(20, 22, 25, 28, 30, 32)
sales <- c(105, 112, 130, 155, 168, 180)
ice_cream_data <- data.frame(temperature, sales)

# --- 2. Calculate the means (the "bars" in the formula) ---
mean_temp <- mean(ice_cream_data$temperature)   # This is x-bar (x̄)
mean_sales <- mean(ice_cream_data$sales)     # This is y-bar (ȳ)

cat("Mean Temperature (x-bar):", mean_temp, "\n")
cat("Mean Sales (y-bar):", mean_sales, "\n\n")

# --- 3. Calculate the numerator for the slope (β1) ---
# This is: sum((x - mean_x) * (y - mean_y))
numerator <- sum((ice_cream_data$temperature - mean_temp) * (ice_cream_data$sales - mean_sales))

cat("Numerator (SSxy):", numerator, "\n")

# --- 4. Calculate the denominator for the slope (β1) ---
# This is: sum((x - mean_x)^2)
denominator <- sum((ice_cream_data$temperature - mean_temp)^2)

cat("Denominator (SSxx):", denominator, "\n\n")

# --- 5. Calculate the slope (β1) ---
beta_1_manual <- numerator / denominator

cat("MANUAL Slope (β1):", beta_1_manual, "\n")

# --- 6. Calculate the intercept (β0) ---
# This is: mean_y - (beta_1 * mean_x)
beta_0_manual <- mean_sales - (beta_1_manual * mean_temp)

cat("MANUAL Intercept (β0):", beta_0_manual, "\n")

