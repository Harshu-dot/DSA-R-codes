#1
data(mtcars)
head(mtcars,15)

#2
data(iris)

names(iris)

min_val <- min(iris$Sepal.Length, na.rm = TRUE)
max_val <- max(iris$Sepal.Length, na.rm = TRUE)

cat("Min Sepal.Length:", min_val, "\n")
cat("Max Sepal.Length:", max_val, "\n")

#3
mean_of_cars <- mean(mtcars$mpg)

print(mean_of_cars)

#4
data(airquality)

str(airquality)

#5
x <- 100
y <- 50
x>y
