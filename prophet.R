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

prophet_model = prophet_reg() %>% set_engine(engine = "prophet") %>% fit(sales~date, data=training(cv_split))

cv_results = modeltime_calibrate(prophet_model, new_data = testing(cv_split))

plt1 = cv_results %>% modeltime_forecast(
  
  new_data = testing(cv_split),
  actual_data = train
  
  
) %>% plot_modeltime_forecast()

prophet_fullfit = cv_results %>% 
  modeltime_refit(data = train)

prophet_preds = prophet_fullfit %>% 
  modeltime_forecast(h = "3 months") %>% 
  rename(date = .index, sales = .value) %>% 
  select(date,sales) %>% 
  full_join(.,y = test, by = "date") %>% 
  select(id,sales)

plt2 = prophet_fullfit %>% modeltime_forecast(actual_data = train, h = "3 months") %>% 
  plot_modeltime_forecast(.interactive = T)


# 420 ---------------------------------------------------------------------


#library(DataExplorer)

train = vroom("train.csv") %>% filter(store == 4 & item ==20) %>% select(-c("store", "item"))
test = vroom("test.csv") %>% filter(store ==4 & item == 20)


cv_split = time_series_split(train, assess="3 months", cumulative = T)

prophet_model = prophet_reg() %>% set_engine(engine = "prophet") %>% fit(sales~date, data=training(cv_split))

cv_results = modeltime_calibrate(prophet_model, new_data = testing(cv_split))

plt1_420 = cv_results %>% modeltime_forecast(
  
  new_data = testing(cv_split),
  actual_data = train
  
  
) %>% plot_modeltime_forecast()

prophet_fullfit = cv_results %>% 
  modeltime_refit(data = train)

prophet_preds = prophet_fullfit %>% 
  modeltime_forecast(h = "3 months") %>% 
  rename(date = .index, sales = .value) %>% 
  select(date,sales) %>% 
  full_join(.,y = test, by = "date") %>% 
  select(id,sales)

plt2_420 = prophet_fullfit %>% modeltime_forecast(actual_data = train, h = "3 months") %>% 
  plot_modeltime_forecast(.interactive = T)

plotly::subplot(plt1, plt1_420, plt2,plt2_420,nrows = 2)

