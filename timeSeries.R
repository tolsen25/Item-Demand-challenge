library(tidyverse)
library(vroom)
library(timetk)
library(embed)
library(tidymodels)
library(discrim)

train = vroom("train.csv") %>% filter(store == 4, item == 20)
test = vroom("test.csv")

bayesRegModel = naive_Bayes(Laplace = tune(), smoothness= tune()) %>%
  set_mode("classification") %>%
  set_engine("naivebayes")


my_recipe <- recipe(sales ~ ., data=train) %>%
  step_date(date, features = "doy") %>%
  step_lag(sales, 365)


bayesReg_workflow = workflow()  %>%
  add_recipe(my_recipe) %>% add_model(bayesRegModel)

tuning_grid = grid_regular(Laplace(), smoothness(), levels = 5)

folds = vfold_cv(trainSet, v = 5, repeats = 1)

CV_results = bayesReg_workflow %>% tune_grid(resamples = folds, grid = tuning_grid,
                                             metrics = metric_set(smape)

bestTune = CV_results %>% select_best("accuracy")

final_wf = bayesReg_workflow %>% finalize_workflow(bestTune) %>% fit(trainSet)


ggg = predict(final_wf, new_data = testSet, type = "class")