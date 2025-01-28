car_1 <- readr::read_csv("used_cars_data//car1.csv")
car_2 <- readr::read_csv("used_cars_data//car2.csv")
car_3 <- readr::read_csv("used_cars_data//car3.csv")
car_4 <- readr::read_csv("used_cars_data//car4.csv")

# Reorder
car_1 <- car_1[, c("name", "year", "km_driven", "fuel", "seller_type", "transmission", "owner", "selling_price")]

#Rename the columns
colnames(car_1) <- c("Name", "Year_mfd", "km_drv", "fuel_type", "seller", "transmission", "no_own", "price")

# Reorder Car 2
# car_2 <- car_2[, c("Car_Name", "Year", "Kms_Driven", "Fuel_Type", "Seller_Type", "Transmission", "Owner", "Selling_Price")]
# 
# colnames(car_2) <- c("Name", "Year_mfd", "km_drv", "fuel_type", "seller", "transmission", "no_own", "price")
