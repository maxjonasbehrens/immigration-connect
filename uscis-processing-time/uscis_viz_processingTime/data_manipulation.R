
# Data Manipulation ------------------------------------------------------------
library(tidyverse)

library(readr)
X2019_01_28_processing_time <- read_csv("~/Documents/GitHub/immigration-connect/uscis-processing-time/2019-01-28-processing-time.csv")

library(readr)
uscis_processing_time_api_20190218 <- read_csv("~/Documents/GitHub/immigration-connect/uscis-processing-time/uscis-processing-time-api_20190218.csv")

test_data <- X2019_01_28_processing_time[,c("form","ZIP","time_min","time_max")]
test_data <- na.omit(test_data)

zip_coord <- read_delim("us_zipcode_coordinates.txt",delim = ",")

test_data <- merge(test_data, zip_coord, by = "ZIP")
test_data$LNG <- as.numeric(test_data$LNG)

# Simulate Historic Data --------------------------------------------------
data <- tibble()
months <- c(1:12)

for (i in months) {
  factor1 <- runif(1, min = 0.5, max = 1.2)
  factor2 <- runif(1, min = 0.8, max = 1.5)
  temp1 <- test_data %>% 
    mutate(year = 2017, month = i) %>% 
    mutate(time_min = round(time_min*factor1,1), time_max = round(time_max*factor1,1))
  temp2 <- test_data %>% 
    mutate(year = 2018, month = i) %>% 
    mutate(time_min = round(time_min*factor2,1), time_max = round(time_max*factor2,1))
  data <- rbind(data,temp1,temp2)
}

# Plot Data with Leaflet ----------------------------------------------------
library(leaflet)

m <- leaflet() %>% 
  addTiles() %>% 
  addCircleMarkers(lng = test_data$LNG, lat = test_data$LAT)
m
