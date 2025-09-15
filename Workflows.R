## Data Cleaning in R

library(tidyverse) #specifically the dplyr library
myCleanData <- myDataSet %>%
  filter(whereTrue) %>% # Selects rows according to conditions
  select(col1, col2, -col3) %>% # Keeps or throws out columns
  mutate(newVar=var1*var2) # Create a new variable

## Feature Engineering in R - Recipes

library(tidymodels)
myrecipe <- recipe(Response~..., data=trainData) %>% #Set model formula & dataset
  step_mutate(var1=factor(var1, levels=, labels=)) %>% #Make something a factor
  step_mutate(newVar=var1*var2) %>% #Create a new variable
  step_poly(var, degree=2) %>% #Create polynomial expansion of var
  step_date(timestamp, features="dow") %>% #gets day of week
  step_time(timestamp, features=c("hour", "minute")) %>% #create time variable
  step_dummy(all_nominal_predictors()) %>% #create dummy variables
  step_zv(all_predictors()) %>% #removes zero-variance predictors
  step_corr(all_predictors(), threshold=0.5) %>% #removes > than .5 corr
  step_rm(var) %>% #removes a variable
  step_select(var, -var2) #selects columns
prepped_recipe <- prep(my_recipe) #Sets up the preprocessing using myDataSet
bake(prepped_recipe, new_data=A_Data_Set)

## Linear Regression Workflow

#Define a recipe as before
bike_recipe <- recipe(...)

#Define a model
lin_model <- linear_reg() %>%
  set_engine("lm") %>%
  set_mode("regression")

#Combine into a Workflow and fit
bike_workflow <- workflow() %>%
  add_recipe(bike_recipe) %>%
  add_model(lin_model) %>%
  fit(data=bikeTrain)

#Run all the steps on test data
lin_preds <- predict(bike_workflow, new_data = bikeTest)