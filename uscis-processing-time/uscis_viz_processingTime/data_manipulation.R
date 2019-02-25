
# Data Manipulation ------------------------------------------------------------
library(tidyverse)

library(readr)
X2019_01_28_processing_time <- read_csv("2019-01-28-processing-time.csv")

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

data <- data %>% 
  mutate(time_mean = (time_min+time_max)/2) %>% 
  filter(LNG < 0)

# Write Data to csv
write_csv(data, 'viz_data.csv')
