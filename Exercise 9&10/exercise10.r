

library(dplyr)
library(ggplot2)
library(factoextra)
library(FactoMineR)

# 1. Let's sort flower numbers into 4 groups, then see which flower-type is in which group!
data("iris")
iris_numbers <- iris[, 1:4]
set.seed(123)  # This helps us get the same answer every time!
fl_grp <- kmeans(iris_numbers, centers = 4, nstart = 20)
print("Let’s see how our groups of flowers match with their types!")
print(table(fl_grp$cluster, iris$Species))

# 2. Let’s sort toy cars into 3 groups by how much petrol they eat, how strong they are, and how heavy they are.
data("mtcars")
car_numbers <- mtcars[, c("mpg", "hp", "wt")]
set.seed(123)
car_groups <- kmeans(car_numbers, centers = 3, nstart = 20)
mtcars$group <- car_groups$cluster
print("How many toy cars are in each group?")
print(table(mtcars$group))
# Make a picture to show the groups!
fviz_cluster(car_groups, data = car_numbers, 
             geom = "point", 
             ellipse.type = "norm", 
             palette = "jco", 
             ggtheme = theme_minimal())

# 3. Let’s play a guessing game: how many groups should the flowers have?
# R will help us by drawing a zigzag line so we can choose the best number!
fviz_nbclust(iris_numbers, kmeans, method = "wss") + 
  labs(title = "Where should we stop grouping?")

# 4. Let’s draw a magic picture to see our 3 flower groups in 2 colors!
set.seed(123)
magic_groups <- kmeans(iris_numbers, centers = 3, nstart = 20)
fviz_cluster(magic_groups, data = iris_numbers, 
             geom = "point", 
             ellipse.type = "norm", 
             palette = "jco", 
             ggtheme = theme_minimal())

# 5. Let’s pick only two flower numbers: how long and how wide the flower's leaves are, 
# and then make groups for these!
just_two <- iris[, c("Sepal.Length", "Sepal.Width")]
set.seed(123)
two_groups <- kmeans(just_two, centers = 3, nstart = 20)
fviz_cluster(two_groups, data = just_two, 
             geom = "point", 
             ellipse.type = "norm", 
             palette = "jco", 
             ggtheme = theme_minimal())

