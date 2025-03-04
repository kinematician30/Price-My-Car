# install.packages("plumber") # if you have not installed plumber
library(plumber)
# Load the plumber library

# --- Load your trained models fro the saved RDS ---
rf_model <- readRDS("models/rf_models.rds")
# linear_model <- readRDS("models/linear_model.rds")
# xgb_model <- readRDS("models/xgb_model.rds")
# Commented not working well

# --- Create a Plumber API ---
#* @apiTitle Used Car Price Prediction API

#* @param Year_mfd Year of Manufacture
#* @param km_drv Kilometers driven
#* @param fuel_type Fuel type (Petrol, Diesel, etc.)
#* @param seller_type Seller type (Individual, Dealer)
#* @param transmission Transmission type (Manual, Automatic)
#* @param no_own Number of owners
#* @get /predict
#* @serializer json

function(Year_mfd, km_drv, fuel_type, seller_type, transmission, no_own) {
  
  # Calculate car age
  car_age <- 2025 - as.numeric(Year_mfd)
  
  # Create a data frame with the input data
  new_data <- data.frame(
    km_drv = as.numeric(km_drv),
    fuel_type = as.factor(fuel_type),
    seller_type = as.factor(seller_type),
    transmission = as.factor(transmission),
    no_own = as.factor(no_own),
    car_age = car_age
  )
  
  # Make predictions
  rf_pred <- predict(rf_model, newdata = new_data)
  linear_pred <- predict(linear_model, newdata = new_data)
  
  # Prepare data for xgboost
  xgb_data <- model.matrix(~.-1, new_data)
  xgb_pred <- predict(xgb_model, newdata = xgb_data)
  
  svr_pred <- predict(svr_model, newdata = new_data)
  
  # Return the predictions as a list
  list(
    RandomForest = rf_pred,
    LinearRegression = linear_pred,
    XGBoost = xgb_pred,
    SVR = svr_pred
  )
}

# --- Run the API ---
pr("deployment.R") %>%  
  pr_run(port = 8080)  # You can change the port number