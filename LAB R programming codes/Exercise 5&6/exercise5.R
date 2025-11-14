# Load libraries
library(dplyr)
library(caret)

# Load datasets
data("iris")
data("mtcars")

#1 Categorize Sepal.Length into Short, Medium, Long
iris$SepalLength_Category <- cut(
  iris$Sepal.Length,
  breaks = c(-Inf, 5.5, 6.5, Inf),   
  labels = c("Short", "Medium", "Long")
)
table(iris$SepalLength_Category)

#2 Convert Species into Dummy Variables
iris_dummy <- cbind(iris, model.matrix(~ Species - 1, data = iris))
head(iris_dummy)

#3 Scale mpg column in mtcars (Min-Max Normalization)
mtcars$mpg_normalized <- (mtcars$mpg - min(mtcars$mpg)) / 
  (max(mtcars$mpg) - min(mtcars$mpg))
head(mtcars$mpg_normalized)

#4 Z-score for Sepal.Width in iris
iris$z_width <- scale(iris$Sepal.Width)
head(iris$z_width)

#5 Efficiency = mpg / hp in mtcars
mtcars$efficiency <- mtcars$mpg / mtcars$hp
head(mtcars$efficiency)
