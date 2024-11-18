library(tidyverse)
library(tidymodels)
library(ggplot2)
library(patchwork)
library(vroom)
library(glmnet)
library(poissonreg)

train <- vroom(file = "BikeShare/train.csv")
test <- vroom(file = "BikeShare/test.csv")

# Exploratory Data Analysis -----------------------------------------------


glimpse(train)
skimr::skim(train)
DataExplorer::plot_intro(train)
DataExplorer::plot_correlation(train)
DataExplorer::plot_bar(train)
DataExplorer::plot_histograms(train)
DataExplorer::plot_missing(train)
GGally::ggpairs(train)

plt_weather <- ggplot(data=train, mapping = aes(x=weather, y=count)) + 
  geom_bar(stat="identity", color="steelblue")

plt_temp <- ggplot(data=train, mapping = aes(x=temp, y=count)) +
  geom_point(color="gold") +
  geom_smooth(se=FALSE, color="brown")

plt_humid <- ggplot(data=train, mapping = aes(x=humidity, y=count)) + 
  geom_point(color="limegreen")

plt_casual <- ggplot(data=train, mapping=aes(x=casual, y=count)) +
  geom_point(color="purple")

image <- (plt_weather + plt_temp) / (plt_humid + plt_casual)

# save the plot panel image
ggsave("fourpanel.png", plot = image)


# Linear Regression -------------------------------------------------------

my_lm <- linear_reg() %>% # type of model
  set_engine("lm") %>% # set default R function
  set_mode("regression") %>% # require quantitative response
  fit(formula=log(count)~    # log transform count
        windspeed+humidity+temp+weather+workingday+holiday+season, 
      data=train) 

# Generate Predictions
bike_preds <- predict(my_lm,
                      new_data=test)
predictions <- exp(bike_preds) # untransform
predictions

# format to Kaggle
kaggle_submission <- predictions %>%
  bind_cols(., test) %>% # bind predictions with test data
  select(datetime, .pred) %>% # keep only datetime and prediction variables
  rename(count=.pred) %>% # rename .pred
  mutate(count=pmax(0,count)) %>% # take only positive inputs
  mutate(datetime=as.character(format(datetime))) # proper Kaggle format for dates

vroom_write(kaggle_submission,"./bikePredictions.csv", delim = ",")


# Poisson Regression ------------------------------------------------------
library(poissonreg)

my_pois_model <- poisson_reg(engine="glm") %>%
  set_mode("regression") %>%
  fit(formula=count~windspeed+humidity+temp+weather+workingday+holiday+season,
      data=train)

predictions <-  predict(my_pois_model,
                          new_data=test)
predictions

pois_kaggle_submission <- predictions %>%
  bind_cols(., test) %>% # bind predictions with test data
  select(datetime, .pred) %>% # keep only datetime and prediction variables
  rename(count=.pred) %>% # rename .pred
  mutate(count=pmax(0,count)) %>% # take only positive inputs
  mutate(datetime=as.character(format(datetime))) # proper Kaggle format for dates

vroom_write(pois_kaggle_submission,"./bikePredictionsPoisson.csv", delim = ",")


# Cleaning ----------------------------------------------------------------
trainClean <- train %>%
  select(., -casual, -registered) %>% # unhelpful variables
  mutate(count = log(count)) # loglinear


# Feature Engineering -----------------------------------------------------
"improve the results of the outcome"

bike_recipe <- recipe(count~., data = trainClean) %>%
  # recode weather 4 to 3, and factor
  step_mutate(weather = factor(weather, 
                               levels = c(1,2,3,4), 
                               labels = c('sunny','misty','stormy','stormy'))) %>%  
  # extract hour variable from timestamp, and factor
  step_time(datetime, features = "hour") %>%
  step_mutate(datetime_hour = as.factor(datetime_hour)) %>% # make hours a factor
  # factor season
  step_mutate(season = factor(season, 
                              levels = c(1,2,3,4), 
                              labels = c('spring','summer','fall','winter'))) %>%
  # interaction variable
  step_mutate(int_temp = temp*atemp) %>%
    # could also intract working day and hour
  # cleanup
  step_rm(datetime) %>% # remove variables
  step_dummy(all_nominal_predictors()) %>% # make dummy variables
  step_normalize(all_numeric_predictors()) # %>% # make mean=0 and sd=1
  #step_select(., -datetime, -atemp, -temp) # remove the original datetime column

                
"for troubleshooting use"
#prepped_recipe <- prep(bike_recipe) # Sets up the preprocessing for other datasets
#testBaked <- bake(prepped_recipe, new_data=test)
#training_wheels <- bake(prepped_recipe, new_data=train)


# Linear Regression Workflow ----------------------------------------------

bike_lm <- linear_reg() %>%
    set_engine("lm") %>%
    set_mode("regression") #%>%
    #fit(formula=count~., 
    #data=train) 
  
bike_workflow <- workflow() %>%
  add_recipe(bike_recipe) %>%
  add_model(bike_lm) %>%
  fit(data=trainClean)


## Run all the steps on test data
lm_preds <- exp(predict(bike_workflow, new_data = test)) # untransform the preds
"for troubleshooting use"
#lm_preds <- exp(predict(bike_lm, testClean))

# format to Kaggle
kaggle_submission <- lm_preds %>%
  bind_cols(., test) %>% # bind predictions with test data
  select(datetime, .pred) %>% # keep only datetime and prediction variables
  rename(count=.pred) %>% # rename .pred
  mutate(count=pmax(0,count)) %>% # take only positive inputs
  mutate(datetime=as.character(format(datetime))) # proper Kaggle format for dates

vroom_write(kaggle_submission,"./bikePredictions.csv", delim = ",")



# Penalized Regression Model with Tuning ----------------------------------------------

# preg_lm <- poisson_reg(penalty=0.01, mixture=0) %>% # set model and tuning
#   set_engine("glmnet")  # best function to fit in R

preg_model <- linear_reg(penalty=tune(), # mark for tuning later
                          mixture=tune()) %>%
  set_engine("glmnet")

preg_wf_fit <- workflow() %>%
  add_recipe(bike_recipe) %>%
  add_model(preg_model)  # dont fit yet!

# grid of tuning parameters to tune over
tuning_grid <- grid_regular(penalty(), # tuning parameter on coefficients
                            mixture(), # tuning parameter that determines regression model
                            levels = 7) # L^2 total tuning parameter combinations

# Split the data for Cross-Validation
folds <- vfold_cv(trainClean, v = 5, repeats = 1) # v = K-folds

cv_results <- preg_wf_fit %>%
  tune_grid(resamples = folds, # separated data
            grid = tuning_grid, # possible tuning parameters
            metrics = metric_set(rmse, mae, rsq)) # metrics to measure loss

# plot results
collect_metrics(cv_results) %>% # makes dataframe
  filter(.metric=="rmse") %>% # select the metric
  ggplot(data=., aes(x=penalty, y=mean, color=factor(mixture))) +
  geom_line()

# find best tuning parameters
bestTune <- cv_results %>%
  select_best(metric="rmse")


# Regression Trees --------------------------------------------------------
library(rpart)

tree_model <- decision_tree(tree_depth = tune(),
                            cost_complexity = tune(), # R will take care of this
                            min_n = tune()) %>% # type of model
  set_engine("rpart") %>% # type of function
  set_mode("regression")

tree_workflow <- workflow() %>%
  add_recipe(bike_recipe) %>%
  add_model(tree_model)  # dont fit yet!
  # %>% fit(trainClean)

# grid of tuning parameters to tune over
tuning_grid <- grid_regular(penalty(), # tuning parameter on coefficients
                            mixture(), # tuning parameter that determines regression model
                            levels = 5) # L^2 total tuning parameter combinations

# Split the data for Cross-Validation
folds <- vfold_cv(trainClean, v = 7, repeats = 1) # v = K-folds

cv_results <- tree_workflow %>%
  tune_grid(resamples = folds, # separated data group
            grid = tuning_grid, # possible tuning parameters
            metrics = metric_set(rmse, mae, rsq)) # metrics to measure loss

# plot results
collect_metrics(cv_results) %>% # makes dataframe
  filter(.metric=="rmse") %>% # select the metric
  ggplot(data=., aes(x=penalty, y=mean, color=factor(mixture))) +
  geom_line()

# find best tuning parameters
bestTune <- cv_results %>%
  select_best(metric="rmse")



# Random Forest Regression -------------------------------------------------
library(ranger)

forest_model <- rand_forest(mode = "regression", # type of fit
                            engine = "ranger", # type of function for this regression
                            mtry = tune(), # num of features at each split
                            min_n = tune(),
                            trees=500) # or model with 1000
 
  
forest_workflow <- workflow() %>%
  add_recipe(bike_recipe) %>%
  add_model(forest_model)  # dont fit yet!

# grid of tuning parameters to tune over
tuning_grid <- grid_regular(parameters(finalize(mtry(), trainClean), # finalize was needed to get a range
                            min_n()), # tuning parameters from model
                            levels = 8) # L^2 total tuning parameter combinations

"alternate:" # mtry needs boundaries
tuning_grid <- grid_regular(mtry(range=c(1,ncol(bake(prep(bike_recipe), trainClean)))),
                                       min_n(), # tuning parameters from model
                            levels = 8) # L^2 total tuning parameter combinations

# Split the data for Cross-Validation
folds <- vfold_cv(trainClean, v = 5, repeats = 1) # v = K-folds

cv_results <- forest_workflow %>%
  tune_grid(resamples = folds, # separated data
            grid = tuning_grid, # possible tuning parameters
            metrics = metric_set(rmse, mae, rsq)) # metrics to measure loss

# plot results
collect_metrics(cv_results) %>% # makes dataframe
  filter(.metric=="rmse") %>% # select the metric
  ggplot(data=., aes(x=penalty, y=mean, color=factor(mixture))) +
  geom_line()

# find best tuning parameters
bestTune <- cv_results %>%
  select_best(metric="rmse")


# Stacking Models ---------------------------------------------------------
library(stacks)
library(dbarts)

# Split data for Cross-validation
folds <- vfold_cv(trainClean, v = 7, repeats = 1)

# Create a control grid
untuned_model <- control_stack_grid() #If tuning over a grid
tuned_model <- control_stack_resamples() #If not tuning a model

## Penalized Regression Model
preg_model <- linear_reg(penalty = tune(),
                         mixture = tune()) %>% 
  set_engine("glmnet")

# Set Workflow
preg_wf <- workflow() %>%
  add_recipe(bike_recipe) %>%
  add_model(preg_model)

# Grid of tuning values
preg_tuning_grid <- grid_regular(penalty(),
                                 mixture(),
                                 levels = 7)
# Run the cross-validation
preg_model_cv <- preg_wf %>%
  tune_grid(resamples = folds,
            grid = preg_tuning_grid,
            metrics=metric_set(rmse, mae, rsq),
            control = untuned_model) # including the grid makes it callable later

## Bayesian additive regression tree model
bart_model <- bart(
  mode = "regression",
  engine = "dbarts",
  trees = 1000,
  prior_terminal_node_coef = .95,
  prior_terminal_node_expo = 2,
  prior_outcome_range = 2
)

bart_wf <- workflow() %>% 
  add_model(bart_model) %>%
  add_recipe(bike_recipe)


## linear model
# Create other resampling objects with different ML algorithms to include in a stacked model
lin_reg_spec <- linear_reg() %>%
  set_engine("lm")

lin_reg_wf <- workflow() %>%
  add_model(lin_reg_spec) %>% 
  add_recipe(bike_recipe)

lin_reg_model <- fit_resamples(
  lin_reg_wf,
  resamples = folds,
  metrics = metric_set(rmse, mae, rsq),
  control = tuned_model
)


## specify which models to include
my_stack <- stacks() %>%
  add_candidates(preg_model_cv) %>%
  add_candidates(lin_reg_model) %>%
  add_candidates(bart_wf)

# fit the stacked model
stack_mod <- my_stack %>%
  blend_predictions() %>% # LASSO penalized regression meta-learner
  fit_members()
# or create own metalearner with as_tibble(my_stack)

# use the stacked model to get predictions
stack_preds <- exp(predict(stack_mod, new_data = test)) #backtransform

  
# Prediction & Submission -------------------------------------------------

#bike_preds <- exp(predict(preg_wf_fit, new_data=test)) # back-transform

#final_wf <- forest_workflow %>% # select the right workflow
#  finalize_workflow(bestTune) %>% # add the parameters
#  fit(data=trainClean) # use the training set

#final_preds <- exp(predict(final_wf, new_data = test)) # backtransform

# format to Kaggle
kaggle_submission <- stack_preds %>% 
  bind_cols(., test) %>% # bind predictions with test data
  select(datetime, .pred) %>% # keep only datetime and prediction variables
  rename(count=.pred) %>% # rename .pred
  mutate(count=pmax(0,count)) %>% # take only positive inputs
  mutate(datetime=as.character(format(datetime))) # proper Kaggle format for dates

vroom_write(kaggle_submission,"./bikePredictions.csv", delim = ",")
# 10, 1 : 4.76
# 100, 0.1 : 1.42
# 1000, 0.01 : 1.42
# 1000, 1 : 1.42
# 100, 0 : 1.33
# 10, 0 : 1.00
# 0.1, 0 : 0.65
# 0.01, 0 : 0.64


