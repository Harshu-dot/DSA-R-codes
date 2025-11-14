fb <- read.csv("dataset_Facebook.csv")
fb <- read.csv2("dataset_Facebook.csv", stringsAsFactors = FALSE)

View(fb)

head(fb,10)

install.packages("readxl")

library(readxl)
lungcap_data <- read_excel("LungCap_Dataset.xls")
head(lungcap_data)


titanic_train <- read.csv("train.csv", header = TRUE)
str(titanic_train)
sapply(titanic_train, class)

titanic_train <- read.csv("train.csv", header = TRUE)
titanic_survivors <- subset(titanic_train, Survived == 1)
write.csv(titanic_survivors, "titanic_survivors.csv", row.names = FALSE)
head(titanic_survivors)


titanic <- read.csv("train.csv", header = TRUE)
titanic_subset <- titanic[, c("Name", "Sex", "Age", "Survived")]
View(titanic_subset)
old <- subset(titanic_subset, Age > 50)
View(old)
survivors <- table(titanic$Pclass, titanic$Survived)
survivors


summary(fb$Page.total.likes)
maxi <- max(fb$Page.total.likes, na.rm = TRUE)
maxi

# Titanic dataset
# 1a. Select specific columns
titanic_sel <- titanic[, c("Name", "Sex", "Age", "Survived")]

# 1b. Passengers older than 50
subset(titanic, Age > 50)

# 1c. Count survivors by passenger class
table(titanic$Pclass, titanic$Survived)

# Facebook dataset
# 2a. Post with maximum likes
fb[which.max(fb$like), ]

# 2b. Average shares per post
mean(fb$share, na.rm=TRUE)

# 2c. New column Engagement
fb$Engagement <- fb$like + fb$comment + fb$share

# Lung Capacity dataset
# 3a. Children below 12
subset(lungcap_data, Age < 12)

# 3b. Average LungCap by Gender
aggregate(lungcap ~ Gender, data=lungcap_data, FUN=mean)

# 3c. Child with max Lung Capacity
lungcap_data[which.max(lungcap_data$LungCap), ]

library(dplyr)

titanic <- rename_with(titanic, tolower)


titanic %>% arrange(desc(age))


titanic <- titanic %>%
  mutate(AgeGroup = case_when(
    age < 12 ~ "Child",
    age >= 12 & age <= 18 ~ "Teen",
    age >= 19 & age <= 59 ~ "Adult",
    age >= 60 ~ "Senior"
  ))


titanic %>%
  group_by(pclass, survived) %>%
  summarise(mean_fare = mean(fare, na.rm=TRUE))


fb %>%
  group_by(Type) %>%
  summarise(avg_likes = mean(like, na.rm=TRUE))

# 1. Columns with missing values
colSums(is.na(titanic))

# 2. Replace missing Age with median
titanic$age[is.na(titanic$age)] <- median(titanic$age, na.rm=TRUE)

# 3. Drop rows where Embarked is missing
titanic <- titanic[!is.na(titanic$embarked), ]

# 4. Fill missing LungCap with mean
lungcap_data$LungCap[is.na(lungcap_data$LungCap)] <- mean(lungcap_data$LungCap, na.rm=TRUE)


# Titanic
hist(titanic$age, main="Age Distribution", col="skyblue")
barplot(table(titanic$pclass), main="Passenger Count by Class", col="orange")
pie(table(titanic$survived), main="Survival Proportion", col=c("red", "green"))

# LungCap
boxplot(LungCap ~ Gender, data=lungcap
      , main="LungCap by Gender", col=c("pink","lightblue"))

# Facebook
plot(fb$like, fb$comment, main="Likes vs Comments", xlab="Likes", ylab="Comments")
hist(fb$share, main="Distribution of Shares", col="purple")




# Titanic - Fare
Q1 <- quantile(titanic$fare, 0.25, na.rm=TRUE)
Q3 <- quantile(titanic$fare, 0.75, na.rm=TRUE)
IQR_val <- Q3 - Q1
lower <- Q1 - 1.5*IQR_val
upper <- Q3 + 1.5*IQR_val
sum(titanic$fare < lower | titanic$fare > upper, na.rm=TRUE)

# Titanic - Age
Q1 <- quantile(titanic$age, 0.25, na.rm=TRUE)
Q3 <- quantile(titanic$age, 0.75, na.rm=TRUE)
IQR_val <- Q3 - Q1
lower <- Q1 - 1.5*IQR_val
upper <- Q3 + 1.5*IQR_val
sum(titanic$age < lower | titanic$age > upper, na.rm=TRUE)

# Capping outliers
titanic$age <- ifelse(titanic$age < lower, lower,
                      ifelse(titanic$age > upper, upper, titanic$age))

# LungCap dataset
Q1 <- quantile(lungcap_data$LungCap, 0.25, na.rm=TRUE)
Q3 <- quantile(lungcap_data$LungCap, 0.75, na.rm=TRUE)
IQR_val <- Q3 - Q1
lower <- Q1 - 1.5*IQR_val
upper <- Q3 + 1.5*IQR_val
outliers <- lungcap$LungCap < lower | lungcap_data$LungCap > upper
mean_before <- mean(lungcap$LungCap, na.rm=TRUE)
lungcap_clean <- lungcap[!outliers, ]
mean_after <- mean(lungcap_clean$LungCap, na.rm=TRUE)
mean_before; mean_after


