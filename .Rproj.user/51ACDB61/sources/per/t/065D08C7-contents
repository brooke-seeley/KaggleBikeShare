library(tidymodels)

## Create a recipe
my_recipe <- recipe(rFormula, data=myDataSet) %>%
  ... %>%
  step_dummy(all_nominal_predictors()) %>% # Make dummy variables
  step_normalize(all_numeric_predictors()) # Make mean 0, sd=1

## Penalized regression model
preg_model <- linear_reg(penalty=??, mixture=??) %>% # Set model and tuning
  set_engine("glmnet") # Function to fit in R
preg_wf <- workflow() %>%
  add_recipe(my_recipe) %>%
  add_model(preg_model) %>%
  fit(data=trainingData)
predict(preg_wf, new_data=testData)