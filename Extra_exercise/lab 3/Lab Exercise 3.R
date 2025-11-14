# PART A: Descriptive Statistics & Visuals (mtcars)
mtcars
#1
mean(mtcars$mpg); median(mtcars$mpg)
mode_mpg <- as.numeric(names(sort(table(mtcars$mpg), decreasing = TRUE))[1]); mode_mpg
var(mtcars$mpg); sd(mtcars$mpg); range(mtcars$mpg)

#2
table(mtcars$cyl)

#3
hist(mtcars$mpg, prob=TRUE, main="Histogram of mpg")
lines(density(mtcars$mpg), lwd=2)

#4
boxplot(mpg ~ cyl, data=mtcars, main="Boxplot: mpg by cyl")

#5
summary(mtcars)



# PART B: Probability & Distributions (iris)
data(iris)

#1
mu <- mean(iris$Sepal.Length); s <- sd(iris$Sepal.Length)
x <- seq(min(iris$Sepal.Length), max(iris$Sepal.Length), length=200)
plot(x, dnorm(x, mean=mu, sd=s), type="l", lwd=2, main="Normal curve (Sepal.Length)")

#2
shapiro.test(iris$Sepal.Length)

#3
set.seed(123); binom_samples <- rbinom(1000, size=10, prob=0.5)
hist(binom_samples, main="Binomial (n=10,p=0.5)")

#4
mean(binom_samples); var(binom_samples); cat("Theoretical mean:", 10*0.5, "variance:", 10*0.5*0.5, "\n")


# PART C: Estimation & Confidence Intervals (mtcars)

#1: 95% CI for mean mpg
t.test(mtcars$mpg, conf.level=0.95)

#2: bootstrap CI for hp
boot_mean <- function(d, i) mean(d[i])
set.seed(123); bt <- boot(mtcars$hp, boot_mean, R=1000)
boot.ci(bt, type="perc")

#3: CI mpg automatic vs manual (am)
t.test(mpg ~ am, data=mtcars, conf.level=0.95)

# PART D: Hypothesis Testing (iris, Titanic)

#1: One-sample t-test Sepal.Length vs 5.5
t.test(iris$Sepal.Length, mu = 5.5)

#2: Two-sample t-test mpg auto vs manual
t.test(mpg ~ am, data = mtcars)

#3: Chi-square Survived vs Sex (Titanic CSV if available else base Titanic)
if(file.exists("train_titanic.csv")){
  titanic <- read.csv("train_titanic.csv", stringsAsFactors = TRUE)
  if(all(c("Survived","Sex") %in% names(titanic))){
    print(chisq.test(table(titanic$Survived, titanic$Sex)))
  } else message("train_titanic.csv loaded but missing Survived or Sex columns")
} else {
  tt <- as.data.frame(Titanic) # Class, Sex, Age, Survived, Freq
  print(chisq.test(xtabs(Freq ~ Survived + Sex, data = tt)))
}

#4: One-way ANOVA Sepal.Length by Species
aov_res <- aov(Sepal.Length ~ Species, data = iris); summary(aov_res)

#5: Tukey HSD
TukeyHSD(aov_res)


# PART E: Correlation & Association

#1 Pearson cor mpg and hp
cor(mtcars$mpg, mtcars$hp)

#2 scatter + regression line
plot(mtcars$hp, mtcars$mpg, xlab="hp", ylab="mpg", main="mpg vs hp")
abline(lm(mpg ~ hp, data=mtcars), col="red", lwd=2)

#3 correlation matrix numeric mtcars
cor(mtcars)

#4 Spearman rank cor Sepal.Length vs Petal.Length
cor(iris$Sepal.Length, iris$Petal.Length, method = "spearman")

#5 association rules (Groceries built-in)
data("Groceries")
rules <- apriori(Groceries, parameter = list(supp = 0.005, conf = 0.3))
inspect(head(sort(rules, by = "lift"), 5))


# PART F: Mini Data Science Applications

#1 Titanic logistic regression (requires train_titanic.csv with Survived, Age, Sex, Pclass)
if(file.exists("train_titanic.csv")){
  tdf <- read.csv("train_titanic.csv", stringsAsFactors = FALSE)
  # ensure columns exist and clean
  if(all(c("Survived","Age","Sex","Pclass") %in% names(tdf))){
    tdf <- tdf %>% mutate(Survived = as.factor(Survived), Sex = as.factor(Sex), Pclass = as.factor(Pclass))
    glm_mod <- glm(Survived ~ Age + Sex + Pclass, data = tdf, family = binomial)
    print(summary(glm_mod))
    cat("Odds ratios:\n"); print(exp(coef(glm_mod)))
  } else message("train_titanic.csv loaded but missing required columns (Survived/Age/Sex/Pclass)")
} else message("train_titanic.csv not found; skip logistic regression")

#2 iris linear regression Petal.Length ~ Sepal.Length
lm_mod <- lm(Petal.Length ~ Sepal.Length, data = iris)
summary(lm_mod)
plot(iris$Sepal.Length, iris$Petal.Length, main="Petal.Length ~ Sepal.Length")
abline(lm_mod, col="red", lwd=2)
plot(lm_mod$residuals, main="Residuals of lm_mod")

#3 k-means clustering (mtcars) on mpg, hp, wt (k=3)
set.seed(123)
X <- scale(mtcars[, c("mpg","hp","wt")])
k3 <- kmeans(X, centers = 3, nstart = 25)
print(k3$centers)
fviz_cluster(k3, data = X, main = "K-means (k=3) on mtcars (mpg,hp,wt)")
