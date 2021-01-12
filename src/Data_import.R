library(tidyverse)
library(lubridate)

df_survey <- read_csv('data/customer_survey.csv')
df_machine_status <- read_csv('data/machine_status_by_plant.csv')
df_machine_types <- read_csv('data/machine_type_details.csv')
df_phone_models <- read_csv('data/model_details.csv')
df_model_production <- read_csv('data/models_produced.csv')
df_machine_manufacturing_specs <- read_csv('data/new_machine_data.csv')
df_revenue_details <- read_csv('data/revenue_by_carrier_model.csv')
df_sales_units <- read_csv('data/sales_units.csv')
df_shipping_details <- read_csv('data/shipping_details.csv')
df_population <- read_csv('data/population.csv')

glimpse(df_revenue_details)

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

glimpse(df_sales_units)

# Apply the function to clean phone model names
df_revenue_details$Model_Type <- lapply(as.character(df_revenue_details$Model_Type), clean_model_names)
df_revenue_details$Model_Type <- (unlist(df_revenue_details$Model_Type))  
df_sales_units$Model_Type <- lapply(as.character(df_sales_units$Model_Type), clean_model_names)
df_sales_units$Model_Type <- (unlist(df_sales_units$Model_Type))  

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
summary(df_survey)

df_machine_status <- df_machine_status %>%
  rename(Machine_Type = 'Machiune_Type')
glimpse(df_machine_status)
summary(df_machine_status)

glimpse(df_machine_types)
summary(df_machine_types)

glimpse(df_phone_models)
summary(df_phone_models)

glimpse(df_model_production)
summary(df_model_production)

df_machine_manufacturing_specs <- df_machine_manufacturing_specs %>%
  rename(Units_per_hour = 'Units per Hour')
glimpse(df_machine_manufacturing_specs)
summary(df_machine_manufacturing_specs)

glimpse(df_revenue_details)
summary(df_revenue_details)

glimpse(df_sales_units)
summary(df_sales_units)

glimpse(df_shipping_details)
summary(df_shipping_details)

#  Calculate the average revenue per model
df_revenue_details %>%
  group_by(Carrier, Model_Type) %>%
  summarize(Avg_Revenue_per_unit = mean(Revenue_per_Unit))

# Calculate number of units produced by Model in each Plant
df_plant_production <- df_model_production %>% group_by(Plant) %>%
  summarize(Model_1 = sum(Model_1),
            Model_2 = sum(Model_2),
            Model_3 = sum(Model_3),
            Model_4 = sum(Model_4),
            Model_5 = sum(Model_5),
            Model_6 = sum(Model_6),
            Model_7 = sum(Model_7),
            Model_8 = sum(Model_8),
            Model_9 = sum(Model_9),
            Model_10 = sum(Model_10))

df_plant_production

df_plant_production_long <- df_plant_production %>%
  pivot_longer(-1, names_to = 'Model_Type', values_to = 'Units_Produced')

ggplot(df_plant_production_long, aes(x = Model_Type, y = Units_Produced, fill = Plant)) + 
  geom_col(position = 'dodge')

# Calculate number of units sold by Month and Model in each Country
df_sales <- df_sales_units %>% group_by(Country, Model_Type) %>%
  summarize('Jan 31,2016' = sum(Jan),
            'Feb 29, 2016' = sum(Feb),
            'Mar 31, 2016' = sum(Mar),
            'Apr 30, 2016' = sum(Apr),
            'May 31, 2016' = sum(May),
            'Jun 30, 2016' = sum(Jun),
            'Jul 31, 2016' = sum(Jul),
            'Aug 31, 2016' = sum(Aug),
            'Sep 30, 2016' = sum(Sep),
            'Oct 31, 2016' = sum(Oct),
            'Nov 30, 2016' = sum(Nov),
            'Dec 31, 2016' = sum(Dec))

df_sales_long <- df_sales %>%
  pivot_longer(3:14, names_to = 'Date', values_to = 'Units_Sold')
df_sales_long$Date <- mdy(df_sales_long$Date)

df_sales_by_model <- df_sales_long %>%
  group_by(Model_Type, Date) %>%
  summarize(Units_sold = sum(Units_Sold))

df_sales_by_country <- df_sales_long %>%
  group_by(Country, Date) %>%
  summarize(Units_sold = sum(Units_Sold))

(sales_plot <- ggplot(df_sales_by_country, aes(x = Date, y = Units_sold, color = Country)) +
  geom_line())

(sales_plot + 
  facet_wrap(vars(Country)))

glimpse(df_population)

df_total_by_country <- df_sales_long %>%
  group_by(Country) %>%
  summarize(Units_sold = sum(Units_Sold)) %>%
  left_join(df_population, by = 'Country') %>%
  mutate(Units_per_cap = (Units_sold/pop_2016))

df_total_by_country <- as.data.frame(df_total_by_country)
glimpse(df_total_by_country)
write.csv(df_total_by_country, 'df_total_by_country.csv')


# Calculate number of units sold by Month and Model by Carrier
df_sales_by_Carrier <- df_sales_units %>% group_by(Carrier, Model_Type) %>%
  summarize(Jan = sum(Jan),
            Feb = sum(Feb),
            Mar = sum(Mar),
            Apr = sum(Apr),
            May = sum(May),
            Jun = sum(Jun),
            Jul = sum(Jul),
            Aug = sum(Aug),
            Sep = sum(Sep),
            Oct = sum(Oct),
            Nov = sum(Nov),
            Dec = sum(Dec))

glimpse(df_sales_by_Carrier)

df_carrier_sales_long <- df_sales_by_Carrier %>%
  pivot_longer(3:14, names_to = 'Month', values_to = 'Units_Sold') %>%
  group_by(Carrier, Model_Type) #%>%
  summarize(Units_Sold =sum(Units_Sold))
glimpse(df_carrier_sales_long)

(carrier_sales_plot <- ggplot(df_carrier_sales_long, aes(x = Month, y = Units_Sold, fill = Model_Type)) +
    geom_col(position = 'dodge'))

(sales_plot + 
    facet_wrap(vars(Country)))

# Pivot the dataframe longer so we can total all the unit sales
glimpse(df_sales_units)
df_sales_long2 <- df_sales_units %>%
  pivot_longer(cols = 4:15, names_to = 'month', values_to = 'units')
glimpse(df_sales_long2)

write.csv(df_sales_long2, 'df_sales_long2.csv')


df_total_sales_by_carrier <- df_sales_long2 %>%
  group_by(Carrier, Model_Type) %>%
  summarize(Total_units_sold = sum(units))

df_total_sales_by_carrier2 <- df_total_sales_by_carrier %>%
  left_join(df_revenue_details, by = c('Carrier', 'Model_Type'))  %>%
  mutate(Total_revenue = Total_units_sold * Revenue_per_Unit)
glimpse(df_total_sales_by_carrier2)

write.csv(df_total_sales_by_carrier2, 'df_revenue.csv')

plot_survey <- function(column){
  ggplot(df_survey, aes(x = column)) +
          geom_bar()
}

glimpse(df_survey)
plot_survey(df_survey$Battery_Life)
plot_survey(df_survey$Operating_System)
plot_survey(df_survey$Speaker_Quality)
plot_survey(df_survey$Touch_Screen)
plot_survey(df_survey$Processor_Speed)
plot_survey(df_survey$GPS)
plot_survey(df_survey$Regional_Global_Service)
plot_survey(df_survey$Bluetooth)
plot_survey(df_survey$Wifi)
plot_survey(df_survey$Hotspot)
plot_survey(df_survey$Camera)
plot_survey(df_survey$Camera_Quality)
plot_survey(df_survey$Camera_Flashlight)
plot_survey(df_survey$Wireless_Charging)
plot_survey(df_survey$Cover_Materials)
plot_survey(df_survey$Display_Quality)
plot_survey(df_survey$Finger_Print_Sensor)
plot_survey(df_survey$Touch_Screen_Pressure_Sensor)
plot_survey(df_survey$Tilt_Sensor)
