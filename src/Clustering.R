library(tidyverse)
library(lubridate)
library(cluster)
library(factoextra)

df_survey <- read.csv('data/customer_survey.csv')
glimpse(df_survey)

# Convert variables to factors
col_to_factor <- function(df, cols){
  df[,cols] <- lapply(df[,cols] , factor)
  return(df)
}

df_survey <- col_to_factor(df_survey, 1:4)

cols <- c(2,5:25)
survey_by_country <- df_survey[,cols] %>%
  group_by(Country) %>%
  summarize(Weight = mean(Weight), Battery_Life = mean(Battery_Life), Operating_System = mean(Operating_System),
            Speaker_Quality = mean(Speaker_Quality), Touch_Screen = mean(Touch_Screen), Processor_Speed = mean(Processor_Speed),
            GPS = mean(GPS), Regional_Global_Service = mean(Regional_Global_Service), Bluetooth = mean(Bluetooth), Wifi = mean(Wifi), 
            Hotspot = mean(Hotspot), Camera = mean(Camera), Camera_Quality = mean(Camera_Quality), Camera_Flashlight = mean(Camera_Flashlight),
            Front_Facing_Camera = mean(Front_Facing_Camera), Wireless_Charging = mean(Wireless_Charging), Cover_Materials = mean(Cover_Materials), 
            Display_Quality = mean(Display_Quality), Finger_Print_Sensor = mean(Finger_Print_Sensor), Touch_Screen_Pressure_Sensor = mean(Touch_Screen_Pressure_Sensor),
            Tilt_Sensor = mean(Tilt_Sensor))
glimpse(survey_by_country)
class(survey_by_country)

survey <- survey_by_country %>% remove_rownames %>% column_to_rownames(var="Country")

survey_scaled <- scale(survey)

head(survey_scaled)

distance <- get_dist(survey_scaled)
fviz_dist(distance, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))

k2 <- kmeans(survey_scaled, centers = 2, nstart = 25)
str(k2)

k2

fviz_cluster(k2, data = survey_scaled)

k3 <- kmeans(survey_scaled, centers = 3, nstart = 25)
str(k3)

k3

fviz_cluster(k3, data = survey_scaled)



set.seed(14)

fviz_nbclust(survey_scaled, kmeans, method = "silhouette")

final <- kmeans(survey_scaled, 4, nstart = 25)
print(final)
fviz_cluster(final, data = survey_scaled)

Survey_Clustered <- survey %>%
  mutate(Cluster = final$cluster) 
write.csv(Survey_Clustered, 'Survey_Clustered.csv')


Survey_Clustered_grouped <- survey %>%
  mutate(Cluster = final$cluster) %>%
  group_by(Cluster) %>%
  summarise_all("mean")
write.csv(Survey_Clustered_grouped, 'Survey_Clustered_grouped.csv')

