library(ggplot2)
library(randomForest)
library(xgboost)
library(e1071)
library(pracma)
library(ggcorrplot)

# Data Collection
all_cars <- readr::read_csv('all_cars.csv')

# Feature Engineering
# Convert categorical variables to factors exclude the car names
all_cars$fuel_type <- as.factor(all_cars$fuel_type)
all_cars$seller_type <- as.factor(all_cars$seller_type)
all_cars$transmission <- as.factor(all_cars$transmission)
all_cars$no_own <- as.factor(all_cars$no_own)

all_cars$car_age <- 2025 - all_cars$Year_mfd # car age instead of year manufactured
all_cars$Year_mfd <- NULL

# Feature Selection
# Correlation for Numerical
numeric_cols <- names(all_cars)[sapply(all_cars, is.numeric)]
cor_matrix <- cor(all_cars[, numeric_cols], use = "pairwise.complete.obs") # Handle NAs in correlation
ggcorrplot(cor_matrix, method = "square", digits = 2, lab = TRUE)

# Keeping the numerical features with corr value > 0.2
high_cor_features <- names(cor_matrix[abs(cor_matrix["Selling_price",]) > 0.2, "Selling_price"])
print(high_cor_features)

# Feature selection for categorucal values
categorical_cols <- names(all_cars)[sapply(all_cars, is.factor)]

anova_results <- list() # Store ANOVA results
for (col in categorical_cols) {
  model <- aov(Selling_price ~ all_cars[[col]], data = all_cars)
  anova_results[[col]] <- summary(model)
  print(paste("ANOVA for", col, ":"))
  print(anova_results[[col]])
}

# keep features with p-value < 0.05
significant_categorical <- names(anova_results)[sapply(anova_results, function(x) x[[1]][["Pr(>F)"]][1] < 0.05)]
print(significant_categorical)

# Combine the features
selected_features <- c(high_cor_features, significant_categorical) # Combine
selected_features <- unique(selected_features)  # Remove duplicates very necessary please write!

all_cars_selected <- all_cars[, c("Selling_price", selected_features)] # Include target variable
print(names(all_cars_selected))


# Now Splitting the data.
set.seed(300) # For Reproducibility
n <- nrow(all_cars)
train_idx <- sample(1:n , size = 0.7 * n) # While splitting we intend use 70% of our dataset to train hence the size
# split into train and test data
train_data <- all_cars[train_idx, ]
test_data <- all_cars[-train_idx, ]

# Model Building: for building this model we are going to be considering three model algorithms[Logistic Regression, Random Forest, Extreme Gradient Boosting].
# Linear Regression
linear_model <- lm(Selling_price ~ ., data = train_data)
summary(linear_model)

# Random Forest
rf_model <- randomForest(Selling_price ~ ., data = train_data, ntree = 100, importance=TRUE)
print(rf_model)

# Extreme Gradient Boosting
train_matrix <- model.matrix(Selling_price ~ .-1, data = train_data)
test_matrix <- model.matrix(Selling_price ~ .-1, data = test_data)

xgb_model <- xgboost(data = train_matrix, label = train_data$Selling_price, nrounds = 50, objective = "reg:squarederror")

# Nodel Evaluation
linear_predictions <- predict(linear_model, newdata = test_data)
rf_predictions <- predict(rf_model, newdata = test_data)
xgb_predictions <- predict(xgb_model, newdata = test_matrix)

# Evaluation metrics: Function calculate Root Mean Squared Value
calculate_rmse <- function(actual, predicted) {
  sqrt(mean((actual - predicted)^2, na.rm = TRUE))}

linear_rmse <- calculate_rmse(test_data$Selling_price, linear_predictions)
rf_rmse <- calculate_rmse(test_data$Selling_price, rf_predictions)
xgb_rmse <- calculate_rmse(test_data$Selling_price, xgb_predictions)

cat("Linear Regression RMSE:", linear_rmse, "\n")
cat("Random Forest RMSE:", rf_rmse, "\n")
cat("XGBoost RMSE:", xgb_rmse, "\n")

# Plotting predicted vs. actual values (example for Random Forest)
plot(test_data$Selling_price, rf_predictions, 
     xlab = "Actual Selling Price", ylab = "Predicted Selling Price",
     main = "Actual vs. Predicted (Random Forest)", col="blue", pch=16)
abline(0, 1, col = "red")

# Further Evaluation: Example - Feature Importance for Random Forest
importance(rf_model)
varImpPlot(rf_model)

# Compare model performance visually 
rmse_df <- data.frame(
  Model = c("Linear Regression", "Random Forest", "XGBoost"),
  RMSE = c(linear_rmse, rf_rmse, xgb_rmse)
)

ggplot(rmse_df, aes(x = Model, y = RMSE)) +
  geom_col(fill = "skyblue", color = "black") +
  labs(title = "RMSE Comparison", x = "Model", y = "RMSE") + theme_bw()


# Let's test with random rows
set.seed(200)
rdm_rows <- all_cars[sample(nrow(all_cars), size = 15), ]

rf_predictions <- predict(rf_model, newdata = rdm_rows)
linear_predictions <- predict(linear_model, newdata = random_rows)

# Create a data frame for easy comparison
predictions_df <- data.frame(
  Actual = rdm_rows$Selling_price,
  RandomForest = rf_predictions
)

# Print the predictions
print(predictions_df)

# This is temporary, only the random forest model is working update coming soon.