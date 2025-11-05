#1. Create Sample Data
temperature <- c(20, 22, 25, 28, 30, 32)
sales <- c(105, 112, 130, 155, 168, 180)

# Combine into a data frame
ice_cream_data <- data.frame(temperature, sales)

# Look at the data
print(ice_cream_data)

#2. Build the Model
#we use the lm() function
# Build the linear regression model
# Format is lm(dependent_variable ~ independent_variable, data = your_data_frame)
model <- lm(sales ~ temperature, data = ice_cream_data)

#3. Analyze the Results
# Get the summary of the model
summary(model)

#4. Using the Model for Prediction
# Create a new data frame with the temperatures you want to predict
new_temperatures <- data.frame(temperature = c(26, 35))

# Use the model to predict sales for these new temperatures
predicted_sales <- predict(model, newdata = new_temperatures)

print(predicted_sales)
