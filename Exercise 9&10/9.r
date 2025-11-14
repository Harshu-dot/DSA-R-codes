library(caret)

data("mtcars")

# Build logistic regression model for 'am' using 'hp' and 'wt'
model <- glm(am ~ hp + wt, data = mtcars, family = binomial())

# View model summary
summary(model)

library(caret)
library(rpart)
library(rpart.plot)
library(pROC)

data("iris")

# 1. Decision tree to classify Species
tree_model <- rpart(Species ~ ., data = iris, method = "class")
rpart.plot(tree_model)

# 2. Confusion matrix and accuracy for decision tree
tree_pred <- predict(tree_model, iris, type = "class")
conf_matrix <- confusionMatrix(tree_pred, iris$Species)
print(conf_matrix)

library(dplyr)

# 3. Logistic regression model for Species (setosa vs non-setosa example)
iris_bin <- iris %>% mutate(Species = factor(ifelse(Species == "setosa", "setosa", "non_setosa")))
log_model <- glm(Species ~ Sepal.Length + Petal.Length, data = iris_bin, family = binomial())
log_prob <- predict(log_model, iris_bin, type = "response")
log_class <- ifelse(log_prob > 0.5, "setosa", "non_setosa")
confusionMatrix(factor(log_class, levels=levels(iris_bin$Species)), iris_bin$Species)

# Plot ROC curve for logistic regression
roc_obj <- roc(iris_bin$Species, log_prob, levels=c("non_setosa", "setosa"))
plot(roc_obj, col="blue", main="ROC Curve - Logistic Regression")

# 4. Compare decision tree and logistic regression accuracy
# Use accuracy values printed above
