# Load necessary libraries
library(ggplot2)
library(dplyr)

# Load built-in datasets
data("iris")
data("mtcars")
data("airquality")

# 1. Simple Linear Regression: Predict Petal.Length from Sepal.Length (iris)
model_iris <- lm(Petal.Length ~ Sepal.Length, data = iris)
summary(model_iris)

# Plot regression line
ggplot(iris, aes(x = Sepal.Length, y = Petal.Length)) +
  geom_point(color = "blue") +
  geom_smooth(method = "lm", se = TRUE, color = "red") +
  labs(
    title = "Regression: Petal.Length vs Sepal.Length",
    x = "Sepal Length",
    y = "Petal Length"
  )

# 2. Multiple Linear Regression: Predict mpg using wt, hp, and drat (mtcars)

model_mtcars <- lm(mpg ~ wt + hp + drat, data = mtcars)
summary(model_mtcars)

# Extract R-squared and Adjusted R-squared
r_squared <- summary(model_mtcars)$r.squared
adj_r_squared <- summary(model_mtcars)$adj.r.squared

cat("R-squared:", r_squared, "\n")
cat("Adjusted R-squared:", adj_r_squared, "\n")

# 3. Plot residuals of the iris regression model (Q1)

plot(model_iris$residuals,
     main = "Residual Plot: Petal.Length ~ Sepal.Length",
     ylab = "Residuals",
     xlab = "Observation Index",
     pch = 19, col = "pink")

abline(h = 0, col = "red", lwd = 2)

airquality_clean <- na.omit(airquality[, c("Ozone", "Temp")])

model_air <- lm(Ozone ~ Temp, data = airquality_clean)
summary(model_air)

