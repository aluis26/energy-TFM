library("readr")
library("dplyr")
library("miscTools")
library ("ggplot2")
library("lubridate")
library("tibble")
library("scales")
library("RColorBrewer")
library("reshape2")

#load the data:
library(readr)
train <- read_csv("data/train.csv")
building_metadata <- read_csv("data/building_metadata.csv")
data_wheater_imputated <- read_csv("data_wheater_imputated.csv")

######################################
# TRAIN DATASET:
set.seed(2)
selected_id <- sample(unique(train$building_id),350, replace=FALSE)
train <- train[train$building_id %in% selected_id,]

#Change the name of the meter to make it more readable and understandable:
train$meter <- plyr::mapvalues(train$meter, from = c(0, 1, 2, 3), to = c("electricity", "chilledwater", "steam", "hotwater"))

#Create a new variable with the log of the meter reading:
train["meter_log"] <- log(train$meter_reading+1) 
#Create variables of time
train  <- train %>% mutate(timestamp_date = ymd(gsub( " .*$", "", timestamp)),
                           timestamp_month = month(timestamp_date),
                           timestamp_day = wday(timestamp_date, label = T, abbr = T),
                           timestamp_day_number = day(timestamp_date),
                           time_ymd_hms = ymd_hms(timestamp),
                           time_hour = hour(time_ymd_hms))

######################################
# BUILDING DATASET:

#Create variables of time:
building_metadata$square_meters <- building_metadata$square_feet * 0.0929030 

#Create variables of age:
building_metadata$age <- 2016 - building_metadata$year_built
building_metadata$ageGroup <- findInterval(building_metadata$age, c(25, 60, 120 )) 
building_metadata$ageGroup <- replace(building_metadata$ageGroup, building_metadata$ageGroup == 0, "new" )
building_metadata$ageGroup <- replace(building_metadata$ageGroup, building_metadata$ageGroup == 1, "medium" )
building_metadata$ageGroup <- replace(building_metadata$ageGroup, building_metadata$ageGroup == 2, "old" )

######################################
#Merge train and building datasets
data_train <- merge(train, building_metadata, by='building_id')

######################################
#Merge all with wheater
final_train_data <- merge(data_train, data_wheater_imputated, by=c('site_id', 'timestamp'), all.x=TRUE)

final_train_data$X1 <-NULL


######################################
#ES NOMES TEMPORAL em carrego els nans, així puc seguir treballant. la taula amb el merge de totes les variables te mes NA a wheater per que hi ha site and timestamp que no tenim dades. s'hauria de fer un merge del site id i timestamp a la taula weather amb train, unique d'aquests i imputació del weather. per aixi tenir tots els posibles casos.

final_train_data <- final_train_data[!is.na(final_train_data$dew_temperature),]


write.csv(final_train_data, 'final_train_data.csv')

