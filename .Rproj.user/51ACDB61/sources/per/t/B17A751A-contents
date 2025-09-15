library(tidyverse)
library(tidymodels)
library(patchwork)
library(vroom)

bike <- vroom('~/STAT 348/Git/Competitions/KaggleBikeShare/train.csv') |>
  mutate(weather = as.factor(weather), 
         holiday = as.factor(holiday),
         season = as.factor(season),
         workingday = as.factor(workingday))

skimr::skim(bike)

GGally::ggpairs(bike)

plot1 <- ggplot(data=bike, aes(x=temp, y=count)) +
  geom_point() +
  geom_smooth(se=FALSE)
plot2 <- ggplot(data=bike, aes(x=weather)) +
  geom_bar()
plot3 <- ggplot(data=bike, aes(x=season)) +
  geom_bar()
plot4 <- ggplot(data=bike, aes(x=humidity)) +
  geom_boxplot()

(plot1 + plot2) / (plot3 + plot4)
