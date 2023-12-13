library(tidyverse)
library(vroom)
library(timetk)

train = vroom("train.csv")
test = vroom("test.csv")

nStores = max(train$store)
nItems = max(train$item)

for (s in 1:nStores){
  
  
  
  
}

library(patchwork)
library(gridExtra)

train1_1 = train %>% filter(store == 1 & item == 1)
x1 = train1_1 %>% pull(sales) %>% forecast::ggAcf(.)
x1 + labs(
  
  title = "ggplot2 makes seaborn feel like trash"
  
)

train1_2 = train %>% filter(store == 1 & item == 2)
x2 = train1_2 %>% pull(sales) %>% forecast::ggAcf(.)

train2_1 = train %>% filter(store == 2 & item == 1)
x3 =train2_1 %>% pull(sales) %>% forecast::ggAcf(.)

train2_2 = train %>% filter(store == 2 & item == 2)
x4 = train2_2 %>% pull(sales) %>% forecast::ggAcf(.)


grid.arrange(x1,x2,x3,x4)


