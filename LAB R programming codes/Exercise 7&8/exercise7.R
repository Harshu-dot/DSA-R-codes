# Load necessary datasets
data("mtcars")
data("iris")
data("airquality")

# 1. One-sample t-test: Check if mean mpg in mtcars differs from 20
t_test_mtcars <- t.test(mtcars$mpg, mu = 20)
t_test_mtcars

# 2. ANOVA: Compare Petal.Width across species in iris
anova_iris <- aov(Petal.Width ~ Species, data = iris)
summary(anova_iris)

# 3. Correlation: mpg and hp in mtcars
cor_mtcars <- cor(mtcars$mpg, mtcars$hp)
cor_mtcars

# 4. Paired t-test: Sepal.Length and Sepal.Width in iris
paired_t_iris <- t.test(iris$Sepal.Length, iris$Sepal.Width, paired = TRUE)
paired_t_iris

# 5. Correlation test: Ozone and Temp in airquality
# (Remove rows with missing values)
airquality_clean <- na.omit(airquality[, c("Ozone", "Temp")])
cor_test_air <- cor.test(airquality_clean$Ozone, airquality_clean$Temp)
cor_test_air

