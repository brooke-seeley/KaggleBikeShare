library(tidyverse)
library(tidymodels)
library(vroom)

## Read in Training Data

trainData <- vroom('train.csv') %>%
  mutate(weather = as.factor(weather), 
         holiday = as.factor(holiday),
         season = as.factor(season),
         workingday = as.factor(workingday)) %>%
  select(-casual, -registered)

## Create Regression Model

bike_model <- linear_reg() %>%
  set_engine("lm") %>%
  set_mode("regression") %>%
  fit(formula=count~., data=trainData)

## Read in Test Data

testData <- vroom('test.csv') %>%
  mutate(weather = as.factor(weather), 
         holiday = as.factor(holiday),
         season = as.factor(season),
         workingday = as.factor(workingday))

## Make Predictions

bike_predictions <- predict(bike_model,
                            new_data=testData)

## Format the Predictions for Submission to Kaggle

kaggle_submission <- bike_predictions %>%
  bind_cols(., testData) %>%
  select(datetime, .pred) %>% 
  rename(count=.pred) %>% 
  mutate(count=pmax(0, count)) %>%
  mutate(datetime=as.character(format(datetime)))

## Write Submission File

vroom_write(x=kaggle_submission, file="./LinearPreds.csv", delim=',')