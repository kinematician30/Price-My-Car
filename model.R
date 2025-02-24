library(ggplot2)
library(randomForest)
library(xgboost)
library(e1071)
library(pracma)
library(ggcorrplot)

# Data Collection
all_cars <- readr::read_csv('all_cars.csv')

# Feature Engineering
# Convert categorical variables to factors
all_cars$fuel_type <- as.factor(all_cars$fuel_type)
all_cars$seller_type <- as.factor(all_cars$seller_type)
all_cars$transmission <- as.factor(all_cars$transmission)
all_cars$no_own <- as.factor(all_cars$no_own)

all_cars$car_age <- 2025 - all_cars$Year_mfd # car age instead of year manufactured

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
train_idx <- sample(1:n , 0.7 * n)
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

xgb_model <- xgboost(data = train_matrix, label = train_data$Selling_price, nrounds = 100, objective = "reg:squarederror")

# Nodel Evaluation
linear_predictions <- predict(linear_model, newdata = test_data)
rf_predictions <- predict(rf_model, newdata = test_data)
xgb_predictions <- predict(xgb_model, newdata = test_matrix)

# Evaluation metrics
calculate_rmse <- function(actual, predicted) {
  sqrt(mean((actual - predicted)^2, na.rm = TRUE)) #Added na.rm = TRUE
}