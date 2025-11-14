
library(ggplot2)

# 1 Histogram of mpg from mtcars
ggplot(mtcars, aes(x = mpg)) +
  geom_histogram(bins = 15, fill = "lightblue", color = "black") +
  labs(
    title = "Histogram of MPG (Miles per Gallon)",
    x = "Miles per Gallon",
    y = "Frequency"
  )

# 2 Scatterplot of hp vs wt from mtcars
ggplot(mtcars, aes(x = wt, y = hp)) +
  geom_point(color = "darkred", size = 3) +
  labs(
    title = "Scatterplot of Horsepower vs Weight",
    x = "Weight (1000 lbs)",
    y = "Horsepower"
  )

# 3️ Boxplot of Ozone values from airquality
data("airquality")
ggplot(airquality, aes(y = Ozone)) +
  geom_boxplot(fill = "lightgreen", color = "black") +
  labs(
    title = "Boxplot of Ozone Levels",
    y = "Ozone (ppb)"
  )

# 4️ Correlation between Sepal.Length and Sepal.Width in iris
data("iris")
cor_value <- cor(iris$Sepal.Length, iris$Sepal.Width)
cat("Correlation between Sepal.Length and Sepal.Width:", cor_value, "\n")

# 5️ Pair plot of first four columns of mtcars
pairs(
  mtcars[, 1:4],
  main = "Pair Plot of First Four Columns in mtcars",
  col = "blue",
  pch = 19
)

