library(tidyverse)
library(vroom)
library(rpart)
library(tidymodels)
library(modeltime)
library(forcats)
library(forecast)
library(timetk)
library(rpart)

#library(patchwork)
#library(DataExplorer)

train = vroom("train.csv") %>% filter(store == 3 & item ==20) %>% select(-c("store", "item"))
test = vroom("test.csv") %>% filter(store ==3 & item == 20)

cv_split = time_series_split(train, assess="3 months", cumulative = T)
# cv_split %>% 
#   tk_time_series_cv_plan() %>% 
#   plot_time_series_cv_plan(date, sales, .interactive = FALSE)

arima_recipe = recipe(sales ~ ., data = train) %>% 
  step_date(features = "doy") %>% 
  step_holiday()

arima_model = arima_reg(
  non_seasonal_ar = 5,
  non_seasonal_ma = 5,
  seasonal_ar = 2,
  seasonal_ma = 2,
  non_seasonal_differences = 2,
  seasonal_differences = 2
) %>% set_engine("auto_arima") 
# 
# es_model = exp_smoothing() %>% 
#   set_engine("ets") %>% 
#   fit(sales~date, data = training(cv_split))


arima_wf = workflow() %>% add_recipe(arima_recipe) %>%
  add_model(arima_model) %>% fit(data = training(cv_split))

cv_results = modeltime_calibrate(arima_wf, new_data = testing(cv_split))

plt1 = cv_results %>% modeltime_forecast(
  
  new_data = testing(cv_split),
  actual_data = train
  
  
) %>% plot_modeltime_forecast()

arima_fullfit = cv_results %>% 
  modeltime_refit(data = train)

arima_preds = arima_fullfit %>% 
  modeltime_forecast() %>% 
  rename(date = .index, sales = .value) %>% 
  select(date,sales) %>% 
  full_join(.,y = test, by = "date") %>% 
  select(id,sales)

plt2 = arima_fullfit %>% modeltime_forecast(actual_data = train) %>% 
  plot_modeltime_forecast(.interactive = T)


# Section 2 ---------------------------------------------------------------

library(tidyverse)
library(vroom)
library(rpart)
library(tidymodels)
library(modeltime)
library(forcats)
library(forecast)
library(timetk)
library(rpart)

#library(patchwork)
#library(DataExplorer)

train = vroom("train.csv") %>% filter(store == 4 & item ==20) %>% select(-c("store", "item"))
test = vroom("test.csv") %>% filter(store ==4 & item == 20)

cv_split = time_series_split(train, assess="3 months", cumulative = T)
# cv_split %>% 
#   tk_time_series_cv_plan() %>% 
#   plot_time_series_cv_plan(date, sales, .interactive = FALSE)

arima_recipe = recipe(sales ~ ., data = train) %>% 
  step_date(features = "doy") %>% 
  step_holiday()

arima_model = arima_reg(
  non_seasonal_ar = 5,
  non_seasonal_ma = 5,
  seasonal_ar = 2,
  seasonal_ma = 2,
  non_seasonal_differences = 2,
  seasonal_differences = 2
) %>% set_engine("auto_arima") 
# 
# es_model = exp_smoothing() %>% 
#   set_engine("ets") %>% 
#   fit(sales~date, data = training(cv_split))


arima_wf = workflow() %>% add_recipe(arima_recipe) %>%
  add_model(arima_model) %>% fit(data = training(cv_split))

cv_results = modeltime_calibrate(arima_wf, new_data = testing(cv_split))

plt1_420 = cv_results %>% modeltime_forecast(
  
  new_data = testing(cv_split),
  actual_data = train
  
  
) %>% plot_modeltime_forecast()

arima_fullfit = cv_results %>% 
  modeltime_refit(data = train)

arima_preds = arima_fullfit %>% 
  modeltime_forecast() %>% 
  rename(date = .index, sales = .value) %>% 
  select(date,sales) %>% 
  full_join(.,y = test, by = "date") %>% 
  select(id,sales)

plt2_420 = arima_fullfit %>% modeltime_forecast(actual_data = train) %>% 
  plot_modeltime_forecast(.interactive = T)





plotly::subplot(plt1, plt1_420, plt2,plt2_420,nrows = 2)


















