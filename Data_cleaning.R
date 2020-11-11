

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

glimpse(df_survey)
glimpse(df_machine_status)
glimpse(df_machine_types)
glimpse(df_phone_models)
glimpse(df_model_production)
glimpse(df_machine_manufacturing_specs)
glimpse(df_revenue_details)
glimpse(df_sales_units)
glimpse(df_shipping_details)

#  Need a function to change 1,2,3 to Model_1, Model_2, etc.  for df_revenue_details and df_sales_units
#  Fix data types 
