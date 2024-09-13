library(tidyverse)
library(tidymodels)
library(ggplot2)
library(patchwork)
library(vroom)

train <- vroom(file = "BikeShare/train.csv")
test <- vroom(file = "BikeShare/test.csv")

# Exploratory Data Analysis
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

