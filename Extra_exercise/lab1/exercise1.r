#Q 1.1

my_name <- "Isha Sahu"          # store charcter
my_age <- 21                    # store numeric
is_student <- TRUE              # logical

#Q 1.2

numbers <- 1:10
vec <- c(1,2,3,4,5,6,7,8,9,10)

#Q 1.3

seqof5 <- 1:10*5        
alter <- seq(5,50,5)             # (start,end,step)

#Q 1.4

fruits <- c("Apple","Banana","Lichi","Mango","Strawberry")
fruits[c(2,4)]                   # To fetch 2nd and 4th position

#Q 1.5

random <- sample(1:100,10)        #sample(range,count) for random no.generation
x=min(random)
y=max(random)
z=mean(random)

#Q 1.6

students <- data.frame(
  Name = c("Alka", "Bharat", "Charlie", "Dravid", "Elizabeth"),
  Age= c(20, 22, 20, 21, 19),
  Marks = c(85, 90, 78, 92, 88))

print(students)

#Q 1.7

desc <- students[order(students$Marks,decreasing =TRUE),]
print(desc)



#Q 2.1

10+5         
10-5
10*5
10/5
10%%3
10%/%3  

#Q 2.2
15>10
7==7

#Q2.3

a <- c(2,4,6,8)
b <- c(1,3,5,7)
a+b
a-b
a*b

#Q 2.4

a>5
b<=4

#Q 2.5

5 %in% a

#Q 2.6

x <- c(TRUE, FALSE, TRUE, FALSE)
y <- c(TRUE, TRUE, FALSE, FALSE)
x&y
x|y
!x

#Q 3.1

for (i in 1:10) {
  print(i)
}

#Q 3.2

sum <- 0
i <- 1

while (i <= 100) {
  sum <- sum + i
  i <- i + 1
}
print(sum)  

#Q 3.3

for (i in 1:50) {
  if (i %% 2 == 0) {
    print(i)
  }
}

#Q 3.4

for (i in 1:10) {
  result <- 7 * i
  cat("7 x", i, "=", result, "\n")
}

#Q 3.5

n <- 10     
factorial <- 1

for (i in 1:n) {
  factorial <- factorial * i
}

cat("Factorial of", n, "is", factorial)

#Q 4.1 Write an if statement to check if a number is positive or negative.

n <- -10

if (n > 0) {
  print(" +ve")
} else {
  print(" -ve")
}

#Q 4.2 Write an if-else statement to check if a given number is even or odd.

num <- 7

if (num %% 2 == 0) {
  print("Even")
} else {
  print("Odd ")
}

#Q 4.3 Write a program to check if a given year is a leap year.

year <- 2024

if (year %% 4 == 0){
  print("Leap year")
} else {
  print("Not a leap year")
}

#Q 4.4 Take a numeric input for marks and print:

marks <- 38

if (marks >= 40) {
  print("Pass")
} else {
  print("Fail")
}

#Q 4.5 5. Using nested if-else, assign grades:

marks <- 76

if (marks >= 90) {
  print("Grade: A")
} else if (marks >= 75) {
  print("Grade: B")
} else if (marks >= 60) {
  print("Grade: C")
} else {
  print("Fail")
}

#Data Analysis – Adult Census Dataset

# Q6.1 Load the Adult dataset into R.
cols <- c("age","workclass","fnlwgt","education","education_num","marital_status","occupation","relationship","race","sex","capital_gain","capital_loss","hours_per_week","native_country","income")

adult <- read.csv("lab1/adult-data.txt",
                  header=FALSE,col.names=cols,
                  strip.white=TRUE,na.strings="?",
                  stringsAsFactors=FALSE)

# 6.2 Display the first 10 rows
print(head(adult,10))
View(head(adult,10))

# 6.3 structure
str(adult)

#4 average
print(mean(adult$age,na.rm=TRUE))

#5 Count how many individuals earn >50K and how many earn <=50K.
print(table(adult$income))
?table

#6 Find the most common occupation
occ_counts <- sort(table(adult$occupation),
                   decreasing=TRUE)
occ_counts
m_c_occ <- names(occ_counts)[1]
print(m_c_occ)

#7 Calculate the average hours-per-week for people earning >50K vs <=50K.
print(tapply(adult$hours_per_week,adult$income,mean,na.rm=TRUE))

#8 Create a bar chart showing the distribution of education levels. 
edu_counts <- sort(table(adult$education),decreasing=TRUE)

#barplot(edu_counts)
barplot(edu_counts,
        col = rainbow(length(unique(edu_counts))),
        las=2,
        main = "Education Level Distribution",
        xlab = "Education",
        ylab = "Count")

?barplot

#9 Find which native country has the highest percentage of people earning >50K.
tbl <- (table(adult$native_country[adult$income == ">50K"]))
print(tbl)
high_per <- names(which.max(tbl))
print(high_per)

#Data Analysis-IPL Datasets
#7 Loading the dataset


ipl <- read.csv("batting_bowling_ipl_bat.csv",
                strip.white=TRUE,na.strings="?",
                stringsAsFactors=FALSE)

#displaying first 10 values

print(head(ipl,10))
View(head(ipl,10))

# 5players with max runs

print(head(ipl[order(ipl$Runs, decreasing = TRUE), c("Name", "Runs")], 5))


#highest batting average

ipl1 <- ipl[!is.na(ipl$Ave), ]
ipl1[max(ipl1$Ave),c("Name", "Ave")]

#bar plot by strike rate

ipl2 <- ipl1[order(ipl1$SR, decreasing = TRUE), ]

top10 <- head(ipl2, 10)

bp<-barplot(
  top10$SR,
  names.arg = top10$Name,
  las = 2,          
  col = "skyblue",
  main = "Top 10 Players by Strike Rate",
  xlab = "Player",
  ylab = "Strike Rate"
)

#corelation


correlation <- cor(ipl$HF, ipl$Runs, use = "complete.obs")
correlation




