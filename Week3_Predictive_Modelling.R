# 1. Scenario Setup & Data Generation (Predicting Crop Yield)
set.seed(42)
agri_model_data <- data.frame(
  Rainfall_mm = rnorm(150, mean = 600, sd = 100),
  Fertilizer_kg = rnorm(150, mean = 120, sd = 30)
)
# Adding the target variable (Yield) based on rainfall and fertilizer
agri_model_data$Crop_Yield_Tons <- 10 + 0.05 * agri_model_data$Rainfall_mm + 0.2 * agri_model_data$Fertilizer_kg + rnorm(150, mean = 0, sd = 5)

# 2. Data Analysis (Take a Screenshot of this plot!)
plot(agri_model_data$Fertilizer_kg, agri_model_data$Crop_Yield_Tons, 
     main="Fertilizer Usage vs Crop Yield", xlab="Fertilizer (kg/Hectare)", ylab="Yield (Tons)", col="blue", pch=19)

# 3. Predictive Modelling (Linear Regression)
# Splitting data (80% train, 20% test)
split <- sample(1:nrow(agri_model_data), 0.8 * nrow(agri_model_data))
train_data <- agri_model_data[split, ]
test_data <- agri_model_data[-split, ]

# Training the model
yield_model <- lm(Crop_Yield_Tons ~ Rainfall_mm + Fertilizer_kg, data = train_data)

# View Model Summary (Take a Screenshot of the console output!)
summary(yield_model)

# 4. Predictions & Validation (Take a Screenshot of this plot!)
predictions <- predict(yield_model, test_data)
plot(test_data$Crop_Yield_Tons, predictions, 
     main="Actual vs Predicted Crop Yield", xlab="Actual Yield", ylab="Predicted Yield", col="darkgreen", pch=19)
abline(0, 1, col="red", lwd=2) # Line of perfect prediction
