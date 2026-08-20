# 1. Create Hypothetical Agriculture Dataset
set.seed(123)
agri_data <- data.frame(
  Region = sample(c("North", "South", "East", "West"), 100, replace = TRUE),
  Temperature_C = rnorm(100, mean = 25, sd = 5),
  Rainfall_mm = rnorm(100, mean = 800, sd = 150),
  Crop_Yield_Tons = rnorm(100, mean = 50, sd = 10)
)

# Introduce errors for cleaning simulation
agri_data$Rainfall_mm[c(15, 42)] <- NA       # Missing values
agri_data$Temperature_C[10] <- 140           # Impossible Outlier (140°C)

# 2. Data Cleaning Process
# Fix outlier using median imputation
agri_data$Temperature_C[agri_data$Temperature_C > 60] <- median(agri_data$Temperature_C)
# Handle missing rainfall data using mean
agri_data$Rainfall_mm[is.na(agri_data$Rainfall_mm)] <- mean(agri_data$Rainfall_mm, na.rm = TRUE)

# 3. Data Transformation (Aggregation)
regional_yield <- aggregate(Crop_Yield_Tons ~ Region, data = agri_data, FUN = mean)

# 4. Visualizations (Take Screenshots of these 2 plots!)
library(ggplot2)

# Plot 1: Average Yield by Region (Bar Chart)
ggplot(regional_yield, aes(x=Region, y=Crop_Yield_Tons, fill=Region)) + 
  geom_bar(stat="identity", color="black") + theme_minimal() + 
  labs(title="Average Crop Yield by Region", y="Yield (Tons)")

# Plot 2: Rainfall vs Crop Yield (Scatter Plot)
ggplot(agri_data, aes(x=Rainfall_mm, y=Crop_Yield_Tons, color=Region)) + 
  geom_point(size=2, alpha=0.8) + theme_minimal() + 
  labs(title="Impact of Rainfall on Crop Yield", x="Rainfall (mm)", y="Yield (Tons)")
