

library(tidyverse)

df_survey <- read_csv('data/customer_survey.csv')
df_machine_status <- read_csv('data/machine_status_by_plant.csv')
df_machine_types <- read_csv('data/machine_type_details.csv')
df_phone_models <- read_csv('data/model_details.csv')
df_model_production <- read_csv('data/models_produced.csv')
df_machine_manufacturing_specs <- read_csv('data/new_machine_data.csv')
df_revenue_details <- read_csv('data/revenue_by_carrier_model.csv')
df_sales_units <- read_csv('data/sales_units.csv')
df_shipping_details <- read_csv('data/shipping_details.csv')

# Function to phone model names for df_revenue_details and df_sales_units
clean_model_names <- function(x) {
  if (x == '10') {
    'Model_10'
  }
  else if (x == '9'){
    'Model_9'
  }
  else if (x == '8'){
    'Model_8'
  }
  else if (x == '7'){
    'Model_7'
  }
  else if (x == '6'){
    'Model_6'
  }
  else if (x == '5'){
    'Model_5'
  }
  else if (x == '4'){
    'Model_4'
  }
  else if (x == '3'){
    'Model_3'
  }
  else if (x == '2'){
    'Model_2'
  }
  else if (x == '1'){
    'Model_1'
  }
  else{
    NA
  }
}

# Apply the function to clean phone model names
df_revenue_details$Model <- lapply(as.character(df_revenue_details$Model), clean_model_names)
df_revenue_details$Model <- (unlist(df_revenue_details$Model))  
df_sales_units$Model <- lapply(as.character(df_sales_units$Model), clean_model_names)
df_sales_units$Model <- (unlist(df_sales_units$Model))  

# Convert variables to factors
col_to_factor <- function(df, cols){
  df[,cols] <- lapply(df[,cols] , factor)
  return(df)
}

# Apply the function to clean phone model names
df_survey <- col_to_factor(df_survey, 1:25)
df_machine_status <- col_to_factor(df_machine_status, 1:2)
df_machine_types <- col_to_factor(df_machine_types, 1)
columns <- c(1,7,9:10, 12:18,21,22,25:28)
df_phone_models <- col_to_factor(df_phone_models, columns)
df_model_production <- col_to_factor(df_model_production, 1:3)
df_machine_manufacturing_specs <- col_to_factor(df_machine_manufacturing_specs, 1:3)
df_revenue_details <- col_to_factor(df_revenue_details, 1:2)
df_sales_units <- col_to_factor(df_sales_units, 1:3)
df_shipping_details <- col_to_factor(df_shipping_details, 1:2)

# Function to replace NA's with '0's
replace_nas <- function (x){
  if (is.na(x)){
    0}
  else{
    x
  }
}

# Apply the function to clean the na's
df_phone_models$GPS <- as.factor(unlist(lapply(df_phone_models$GPS, replace_nas)))
df_phone_models$Camera_Flashlight <- as.factor(unlist(lapply(df_phone_models$Camera_Flashlight, replace_nas)))
df_model_production$Model_1 <- unlist(lapply(df_model_production$Model_1, replace_nas))
df_model_production$Model_2 <- unlist(lapply(df_model_production$Model_2, replace_nas))
df_model_production$Model_3 <- unlist(lapply(df_model_production$Model_3, replace_nas))
df_model_production$Model_4 <- unlist(lapply(df_model_production$Model_4, replace_nas))
df_model_production$Model_5 <- unlist(lapply(df_model_production$Model_5, replace_nas))
df_model_production$Model_6 <- unlist(lapply(df_model_production$Model_6, replace_nas))
df_model_production$Model_7 <- unlist(lapply(df_model_production$Model_7, replace_nas))
df_model_production$Model_8 <- unlist(lapply(df_model_production$Model_8, replace_nas))
df_model_production$Model_9 <- unlist(lapply(df_model_production$Model_9, replace_nas))
df_model_production$Model_10 <- unlist(lapply(df_model_production$Model_10, replace_nas))
df_machine_manufacturing_specs$Required <- as.factor(unlist(lapply(df_machine_manufacturing_specs$Required, replace_nas)))
df_sales_units$Jan <- unlist(lapply(df_sales_units$Jan, replace_nas))
df_sales_units$Feb <- unlist(lapply(df_sales_units$Feb, replace_nas))
df_sales_units$Mar <- unlist(lapply(df_sales_units$Mar, replace_nas))
df_sales_units$Apr <- unlist(lapply(df_sales_units$Apr, replace_nas))
df_sales_units$May <- unlist(lapply(df_sales_units$May, replace_nas))
df_sales_units$Jun <- unlist(lapply(df_sales_units$Jun, replace_nas))
df_sales_units$Jul <- unlist(lapply(df_sales_units$Jul, replace_nas))
df_sales_units$Aug <- unlist(lapply(df_sales_units$Aug, replace_nas))
df_sales_units$Sep <- unlist(lapply(df_sales_units$Sep, replace_nas))
df_sales_units$Oct <- unlist(lapply(df_sales_units$Oct, replace_nas))
df_sales_units$Nov <- unlist(lapply(df_sales_units$Nov, replace_nas))
df_sales_units$Dec <- unlist(lapply(df_sales_units$Dec, replace_nas))

# Function to homogenize Y/N, 0/1 factors 
clean_binaries <- function(x) {
  if (grepl('0', x)) {
    'No'
  }
  else if (grepl('n', tolower(x))){
    'No'
  }
  else if (grepl('1', x)){
    'Yes'
  }
  else if (grepl('y', tolower(x))){
    'Yes'
  }
  else if (is.na(x)){
    'No'
  }
  else{
    x 
  }
}

# Apply the function to clean the binaries
df_phone_models$Touch_Screen <- as.factor(unlist(lapply(as.character(df_phone_models$Touch_Screen), clean_binaries)))
df_phone_models$GPS <- as.factor(unlist(lapply(as.character(df_phone_models$GPS), clean_binaries)))
df_phone_models$Radio <- as.factor(unlist(lapply(as.character(df_phone_models$Radio), clean_binaries)))
df_phone_models$Bluetooth <- as.factor(unlist(lapply(as.character(df_phone_models$Bluetooth), clean_binaries)))
df_phone_models$Wifi <- as.factor(unlist(lapply(as.character(df_phone_models$Wifi), clean_binaries)))
df_phone_models$Projector <- as.factor(unlist(lapply(as.character(df_phone_models$Projector), clean_binaries)))
df_phone_models$Hotspot <- as.factor(unlist(lapply(as.character(df_phone_models$Hotspot), clean_binaries)))
df_phone_models$Camera <- as.factor(unlist(lapply(as.character(df_phone_models$Camera), clean_binaries)))
df_phone_models$Camera_Flashlight <- as.factor(unlist(lapply(as.character(df_phone_models$Camera_Flashlight), clean_binaries)))
df_phone_models$Front_Facing_Camera <- as.factor(unlist(lapply(as.character(df_phone_models$Front_Facing_Camera), clean_binaries)))
df_phone_models$Wireless_Charging <- as.factor(unlist(lapply(as.character(df_phone_models$Wireless_Charging), clean_binaries)))
df_phone_models$Fingerprint_Sensor <- as.factor(unlist(lapply(as.character(df_phone_models$Fingerprint_Sensor), clean_binaries)))
df_phone_models$Silent_Ringer_Vibration <- as.factor(unlist(lapply(as.character(df_phone_models$Silent_Ringer_Vibration), clean_binaries)))
df_phone_models$Touch_Screen_Pressure_Sensor <- as.factor(unlist(lapply(as.character(df_phone_models$Touch_Screen_Pressure_Sensor), clean_binaries)))
df_phone_models$Tilt_Sensor <- as.factor(unlist(lapply(as.character(df_phone_models$Tilt_Sensor), clean_binaries)))
df_machine_manufacturing_specs$Required <- as.factor(unlist(lapply(as.character(df_machine_manufacturing_specs$Required), clean_binaries)))

df_phone_models <- df_phone_models %>%
  rename(Coverage = 'Regional_Global')

# Function to clean Coverage 
clean_coverage <- function(x) {
  if (grepl('r', tolower(x))){
    'Regional'
  }
  else if (grepl('g', tolower(x))){
    'Global'
  }
  else{
    NA 
  }
}

# Apply the function to clean the coverage values
df_phone_models$Coverage <- as.factor(unlist(lapply(as.character(df_phone_models$Coverage), clean_coverage)))

# Impute the missing machine data by using average values by location
df_machine_means <- df_machine_status %>%
  group_by(Plant) %>%
  summarize(Avg_Age = round(mean(Avg_Age, na.rm=TRUE)), Avg_Util_pct = round(mean(Utilization_pct, na.rm=TRUE),2))
df_machine_means

# Add the imputed values 
for (i in 1:nrow(df_machine_status)){
  if(df_machine_status[i,1] == 'Mexico' & is.na(df_machine_status[i,3])){
    df_machine_status[i,3] <- df_machine_means[2,2]
    df_machine_status[i,4] <- df_machine_means[2,3]
  }
  else if(df_machine_status[i,1] == 'Poland' & is.na(df_machine_status[i,3])){
    df_machine_status[i,3] <- df_machine_means[3,2]
    df_machine_status[i,4] <- df_machine_means[3,3]
  }
}

glimpse(df_survey)
glimpse(df_machine_status)
glimpse(df_machine_types)
glimpse(df_phone_models)
glimpse(df_model_production)
glimpse(df_machine_manufacturing_specs)
glimpse(df_revenue_details)
glimpse(df_sales_units)
glimpse(df_shipping_details)

summary(df_survey)
summary(df_machine_status)
summary(df_machine_types)
summary(df_phone_models)
summary(df_model_production)
summary(df_machine_manufacturing_specs)
summary(df_revenue_details)
summary(df_sales_units)
summary(df_shipping_details)
