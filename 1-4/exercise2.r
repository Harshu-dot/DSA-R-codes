#1
data(airquality)
str(airquality)

#2
summary(is.na(airquality))

#3
data(mtcars)
summary(mtcars)

#4
data(mtcars)
mean_hp <- mean(mtcars$hp)
print(mean_hp)

#5
data(iris)
aggregate(Sepal.Width ~ Species, data = iris, FUN = mean)
library(dplyr)
iris %>%
  group_by(Species) %>%
  summarise(avg_sepal_width = mean(Sepal.Width, na.rm = TRUE))

