######
final_train_data <- read_csv("final_train_data.csv")
str(final_train_data)

# change type to factor of categorical variable:
final_train_data$meter = factor(final_train_data$meter)
final_train_data$site_id = factor(final_train_data$site_id)
final_train_data$building_id = factor(final_train_data$building_id)
final_train_data$primary_use = factor(final_train_data$primary_use)
final_train_data$timestamp_day = factor(final_train_data$timestamp_day)
final_train_data$timestamp_month = factor(final_train_data$timestamp_month)
final_train_data$time_hour = factor(final_train_data$time_hour)

#split the dataset in train and test, as we have data from a complete year, and we want to predict the next one, we cannot split for dates. (even though this is a time series, if we make sure there is no leakeage of the target in the features we can do it.). We will do cross validation in the train, find the best models and tune them  and  finally check the performance of the model directy in the test uploading the result to the competition. http://www.sthda.com/english/articles/38-regression-model-validation/157-cross-validation-essentials-in-r/ https://rpubs.com/rdelgado/405322metode cv. explicar. els models lineals assumeixen una distribució normal en totes les variables (excepte les binaries i els factors), per aquest motiu haurem de treballar amb escala logaritmica en el meter reading per exemple. els meter_log que siguin -inf de moment els convertirem a 0.
library(tidyverse)
library(caret)
library(cvTools)
library("miscTools")



#######################Evaluation Metric
#Root Mean Squared Logarithmic Error: (buscar informació)

RMSLE = function(predicted, target){
 return (sqrt(mean((log(predicted + 1) - log(target + 1))^2)))
}

r2 = function (predicted, target){
  rSquared(target, resid= target - predicted)
}

#folds generation
set.seed(206)
folds <- cvFolds(NROW(final_train_data), K=5)

list_subsets_test = list(fold_1= folds$subsets[folds$which == 1], fold_2= folds$subsets[folds$which ==2], fold_3= folds$subsets[folds$which == 3], fold_4= folds$subsets[folds$which == 4], fold_5= folds$subsets[folds$which == 5])

#baseline model. Mean of results per type of meter and building id.
baseline_mean_model = function(test_ids){
  train_set = final_train_data[-test_ids,]
  test_set = final_train_data[test_ids,]
  means = final_train_data %>% group_by(meter, building_id) %>% summarise (meter_reading_mean = mean(meter_reading))
  predicted_reading = merge(test_set, means, by=c("meter", "building_id"))$meter_reading_mean

  return (list(RMSLE=RMSLE(predicted_reading,test_set$meter_reading),r2=r2(predicted_reading,test_set$meter_reading) ))
}

results_baseline = lapply(list_subsets_test, baseline_mean_model)

#baseline model. Median of results per type of meter and building id.
baseline_median_model = function(test_ids){
  train_set = final_train_data[-test_ids,]
  test_set = final_train_data[test_ids,]
  medians = final_train_data %>% group_by(meter, building_id) %>% summarise (meter_reading_median = median(meter_reading))
  predicted_reading = merge(test_set, medians, by=c("meter", "building_id"))$meter_reading_median
  
  return (list(RMSLE=RMSLE(predicted_reading,test_set$meter_reading),r2=r2(predicted_reading,test_set$meter_reading) ))
}

results_baseline_median = lapply(list_subsets_test, baseline_median_model)

#baseline model. Mean of results per type of meter and building id.
baseline_log_mean_model = function(test_ids){
  train_set = final_train_data[-test_ids,]
  test_set = final_train_data[test_ids,]
  log_means = final_train_data %>% group_by(meter, building_id) %>% summarise (meter_log_mean = mean(meter_log))
  predicted_log = merge(test_set, log_means, by=c("meter", "building_id"))$meter_log_mean
  predicted_reading = exp(predicted_log) - 1
  return (list(RMSLE=RMSLE(predicted_reading,test_set$meter_reading),r2=r2(predicted_reading,test_set$meter_reading) ))
}

results_baseline_log_mean = lapply(list_subsets_test, baseline_log_mean_model)


#Linear model:
linear_model = function(test_ids){
  train_set = final_train_data[-test_ids,]
  test_set = final_train_data[test_ids,]
  model <- lm(meter_log ~ meter + primary_use + air_temperature + cloud_coverage + dew_temperature + precip_depth_1_hr + sea_level_pressure + wind_direction + wind_speed + log1p(square_feet) + timestamp_day + time_hour + timestamp_month , data=train_set)
  print(summary(model))
  predicted_log <- predict(model, newdata = test_set)
  predicted_reading <- exp(predicted_log) - 1 
  
  return (list(RMSLE=RMSLE(predicted_reading,test_set$meter_reading),r2=r2(predicted_reading,test_set$meter_reading) ))
}
linear_model(test_ids)

results_linear_model = lapply(list_subsets_test, linear_model)

#this have proven to be better than the baseline but very poor too.





