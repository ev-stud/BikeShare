library(tidyverse)
library(tidymodels)
library(ggplot2)
library(patchwork)

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

ggsave("fourpanel.png", plot = image)

"update the working directory for next steps"
cd ./BikeShare

# create Bike.R file in git repository

"check that the Bike.R file exists for use"
git status 

"add the local Bike.R file to online GitHub"
git add Bike.R

"save the filem, with an optional comment"
git commit -m "I added the Bike.R file"

"check that our local job is complete, and online repo is ready for next step"
git status

"complete the sync process"
git push
