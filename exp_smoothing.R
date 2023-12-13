library(tidyverse)
library(vroom)
library(timetk)
library(tidyverse)
library(rpart)
library(tidymodels)
library(modeltime)

train = vroom("train.csv") %>% filter(store == 4, item ==20)
test = vroom("test.csv")

cv_split = time_series_split(train, assess="3 months", cumulative = T)
cv_split %>% 
  tk_time_series_cv_plan() %>% 
  plot_time_series_cv_plan(date, sales, .interactive = FALSE)

es_model = exp_smoothing() %>% 
  set_engine("ets") %>% 
  fit(sales~date, data = training(cv_split))

cv_results = modeltime_calibrate(es_model, new_data = testing(cv_split))

plt1 = cv_results %>% modeltime_forecast(
  
  new_data = testing(cv_split),
  actual_data = train
  
  
) %>% plot_modeltime_forecast()

es_fullfit = cv_results %>% 
  modeltime_refit(data = train)

es_preds = es_fullfit %>% 
  modeltime_forecast(h = "3 months") %>% 
  rename(date = .index, sales = .value) %>% 
  select(date,sales) %>% 
  full_join(.,y = test, by = "date") %>% 
  select(id,sales)

plt2 = es_fullfit %>% modeltime_forecast(h = "3 months", actual_data = train) %>% 
  plot_modeltime_forecast(.interactive = F)


# pt2 ---------------------------------------------------------------------


train = vroom("train.csv") %>% filter(store == 1, item ==1)
test = vroom("test.csv")

cv_split = time_series_split(train, assess="3 months", cumulative = T)
cv_split %>% 
  tk_time_series_cv_plan() %>% 
  plot_time_series_cv_plan(date, sales, .interactive = FALSE)

es_model = exp_smoothing() %>% 
  set_engine("ets") %>% 
  fit(sales~date, data = training(cv_split))

cv_results = modeltime_calibrate(es_model, new_data = testing(cv_split))

plt3 = cv_results %>% modeltime_forecast(
  
  new_data = testing(cv_split),
  actual_data = train
  
  
) %>% plot_modeltime_forecast()

es_fullfit = cv_results %>% 
  modeltime_refit(data = train)

es_preds = es_fullfit %>% 
  modeltime_forecast(h = "3 months") %>% 
  rename(date = .index, sales = .value) %>% 
  select(date,sales) %>% 
  full_join(.,y = test, by = "date") %>% 
  select(id,sales)

plt4 = es_fullfit %>% modeltime_forecast(h = "3 months", actual_data = train) %>% 
  plot_modeltime_forecast(.interactive = F)

plotly::subplot(plt1,plt3,plt2,plt4, nrows = 2)












