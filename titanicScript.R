getwd()
setwd("Users/melis/Desktop/R/titanicR")
train <- read.csv("train.csv", header =T)
test <- read.csv("test.csv", header =T)

#difference between test and train 
#machine learning algorithm learns pattern from train data
#then we will test the algorithm to see if it works

#add a "Survived" column to the test set to allow for combining data sets
test.survived <- data.frame(Survived = rep("None", nrow(test)), test[,])

#combine data sets
data.combined <- rbind(train, test.survived)

#Changing data types

data.combined$Survived <- as.factor(data.combined$Survived)
data.combined$Pclass <- as.factor(data.combined$Pclass)
str(data.combined)
table(data.combined$Survived) 
table(data.combined$Pclass) 

library(ggplot2)

#data visualization to see which class has the high survival rate
train$Pclass <- as.factor(train$Pclass)
ggplot(train, aes(x= Pclass, fill = factor(Survived))) +
      geom_bar(width = 0.5) +
      xlab("Pclass") +
      ylab("TotalCount") +
      labs(fill = "Survived")

#Dealing with titles
install.packages("stringr")
library(stringr)




#Lets look at the name variable on the dataset
head(as.character(train$Name))
str(train$Name)

#how many unique names are there across both train and test datasets?
length(unique(as.character(data.combined$Name)))

#two duplicate names! lets look at closer
#names converted as character to check the duplication
dup_names <- as.character(data.combined[which(duplicated(as.character(data.combined$Name))), "Name"])


#to find other information for duplicate names
data.combined[which(data.combined$Name %in% dup_names), ]

#seperating the data as title
tail(data.combined$Name, 10)

misses <- data.combined[which(str_detect(data.combined$Name, "Miss")), ]
head(misses, 10)

mrses <- data.combined[which(str_detect(data.combined$Name, "Mrs")), ]
head(mrses, 10)

males <- data.combined[which(train$Sex == "male"),]
head(males, 5)

#making things easier with a function
?grep


extractTitle <- function(Name) {
  Name <- as.character(Name)
  
 if (length(grep("Miss.", Name)) > 0) {
   return ("Miss.")
 } else if (length(grep("Master.", Name)) > 0) {
   return ("Master.")
 } else if (length(grep("Mrs.", Name)) > 0) {
    return ("Mrs.")
  } else if (length(grep("Mr.", Name)) > 0) {
    return ("Mr.")
  } else { 
    return ("Other")
  }
}

titles <- NULL
for(i in 1:nrow(data.combined)) {
  titles <- c(titles, extractTitle(data.combined[i,"Name"]))
}
  data.combined$Title <- as.factor(titles)

#sapply function - is the same with for loop but more prettier
data.combined$Title2 <- sapply(data.combined$Name, extractTitle)

#since we have surviver labels on the train dataframe we only use those rows belong to train 
library(ggplot2)
ggplot(data.combined[1:891,], aes(x = Title, fill = Survived)) +
    geom_bar(width = 0.9) +
    facet_wrap(~Pclass) +
    ggtitle("Pclass") +
    xlab("Title") +
    ylab("Total Count") +
    labs(fill = "Survived")
?facet_wrap

#age and sex pretty important so we look at the distribution of them

table(data.combined$Sex)
ggplot(data.combined[1:891,], aes(x= Sex, fill = Survived)) +
      geom_bar(width = 0.5) +
      facet_wrap(~Pclass) +
      ggtitle("Pclass") +
      xlab("Sex") +
      ylab("Total Count") +
      labs(fill = "Survived")
summary(data.combined$Age)
#age distribution
library(ggplot2)
ggplot(data.combined[1:891,], aes(x = Age, fill = Survived)) +
  facet_wrap(~Sex + Pclass) +
  geom_histogram(binwidth = 10) +
  xlab("Age") +
  ylab("Total Count")

#hypothesis - "Master." is a goo proxy for male children
boys <- data.combined[which(data.combined$Title == "Master."),]
summary(boys$Age)

summary

#age for misses by Pclass
ggplot(misses[misses$Survived != "None",], aes(x = Age, fill = Survived)) +
  facet_wrap(~Pclass) +
  geom_histogram(binwidth = 5) +
  xlab("Age") +
  ylab("Total Count")

#female children have different survical rates
#we will create dataset to catch a pattern in female children who travel alone.

misses.alone <- misses[which(misses$SibSp == 0 & misses$Parch == 0),]
summary(misses.alone$Age)

#the reason to ook at age 14.5 is to make equal analysis for both female and male children. 
#Male children in boys data.frame shows that max age of boys is 14.5
length(which(misses.alone$Age <= 14.5))

#summary of SibSp variable
summary(data.combined$SibSp)
?unique

#with this method you can see how many level we have. its usage similar with factor.
length(unique(data.combined$SibSp))

data.combined$SibSp <- as.factor(data.combined$SibSp)
is.factor(data.combined$SibSp)
library(ggplot2)
ggplot(data.combined[1:891,], aes(x = as.numeric(SibSp), fill = Survived)) +
  facet_wrap(~Pclass + Title) +
  geom_histogram(binwidth = 1) +
  ggtitle("Pclass, Title")
  xlab("SibSp") +
  ylab("Total Count") +
  ylim(0,300) +
  labs(fill = "Survived")

#to see distribution parch variable
  
ggplot(data.combined[1:891,], aes(x = Parch, fill = Survived)) +
    facet_wrap(~Pclass + Title) +
    geom_histogram(binwidth = 1) +
    ggtitle("Pclass, Title")
  xlab("Parch") +
    ylab("Total Count") +
    ylim(0,300) +
    labs(fill = "Survived")
  
#to examine correlation between family size and survival rate
temp.sibSp <- c(train$SibSp, test$SibSp)
temp.Parch <- c(train$Parch, test$Parch)
  
data.combined$familySize <- (temp.Parch + temp.sibSp + 1)
  
ggplot(data.combined[1:891,], aes(x = as.integer(familySize), fill = Survived)) +
facet_wrap(~Pclass + Title) +
geom_histogram(binwidth = 1) +
ggtitle("Pclass, Title")
xlab("familySize") +
ylab("Total Count") +
ylim(0,300) +
labs(fill = "Survived")

str(data.combined$Parch)
str(data.combined$familySize)

#look at the other variables; ticket, cabin.
str(data.combined$Ticket)

#based on the huge number of levels make ticket variable string
data.combined$Ticket <- as.character(data.combined$Ticket)
str(data.combined$Ticket)
data.combined$Ticket[1:10]

#it seems ticket number has lots of variant. So, we can look at the first character of ticket to try to get any patter from it.

?substr
?ifelse
#we obtain a new vector which includes only first letter of ticket variable.
#we also took precaution to see empty characters too. However, there is no empty row in ticket variable.
firstchar_ticket <- ifelse(data.combined$Ticket == "", " ", substr(data.combined$Ticket, 1, 1))
unique(firstchar_ticket)

data.combined$firstchar_ticket <- as.factor(firstchar_ticket)
str(data.combined$firstchar_ticket)
library(ggplot2)
#lets have a look at the title via ggplot
ggplot(data.combined[1:891,], aes(x = firstchar_ticket, fill = Survived)) +
  geom_bar() +
  ggtitle("Survival rate of ticket type")
xlab("ticket") +
  ylab("Total Count") +
  ylim(0,300) +
  labs(fill = "Survived")

#drill down ticket variable a bit
ggplot(data.combined[1:891,], aes(x = firstchar_ticket, fill = Survived)) +
  facet_wrap(~Pclass) +
  geom_bar() +
  ggtitle("Pclass") +
  xlab("ticket") +
  ylab("Total Count") +
  ylim(0,300) +
  labs(fill = "Survived")
  
#combining ticket with titles
#there is no clear pattern in ticket names
ggplot(data.combined[1:891,], aes(x = firstchar_ticket, fill = Survived)) +
  facet_wrap(~Pclass + Title) +
  geom_bar() +
  ggtitle("Pclass & Title")
  xlab("ticket") +
  ylab("Total Count") +
  ylim(0,300) +
  labs(fill = "Survived")
  
#to catch pattern with fare variable
summary(data.combined$Fare)
length(unique(data.combined$Fare))

#can't make fare a factor, trat as numeric and visualize it via histogram
ggplot(data.combined, aes(x = Fare)) +
  geom_histogram(binwidth = 5) +
  ggtitle("Fare Distribution")
  xlab("Fare") +
  ylab("Total Count") +
  ylim(0,300)

#to catch pattern with fare variable to develop predictive model
ggplot(data.combined[1:891,], aes(x = Fare, fill = Survived)) +
  facet_wrap(~Pclass + Title) +
  geom_histogram(binwidth = 5) +
  ggtitle("Pclass & Title")
  xlab("Fare") +
  ylab("Total Count") +
  ylim(0, 50) +
  labs(fill = "Survived")
  
str(data.combined$Fare)

#Analysis of the cabin variable
str(data.combined$Cabin)
head(data.combined$Cabin, 5)
data.combined[!complete.cases(data.combined$Cabin),]

#convert cabin variable to string 
data.combined$Cabin <- as.character(data.combined$Cabin)
data.combined$Cabin[1:100]

#replace missing data in cabin variable - U for unknown
data.combined[which(data.combined$Cabin ==""), "Cabin"] <- "U"
data.combined$Cabin[1:100]

#take a look at just the first character as factor
firstchar_cabin <- substr(data.combined$Cabin, 1, 1)
data.combined$firstchar_cabin <- as.factor(firstchar_cabin)
str(data.combined$firstchar_cabin)

#visualize the first char cabin variable 
ggplot(data.combined[1:891,], aes(x = firstchar_cabin, fill = Survived)) +
  geom_bar() +
  xlab("Cabin") +
  ylab("Total Count") +
  ylim(0, 750) +
  labs(fill = "Survived")

#Drill in
ggplot(data.combined[1:891,], aes(x = firstchar_cabin, fill = Survived)) +
  geom_bar() +
  facet_wrap(~Pclass)
  ggtitle("Pclass")
  xlab("Cabin") +
  ylab("Total Count") +
  ylim(0, 750) +
  labs(fill = "Survived")
  

#add title too
ggplot(data.combined[1:891,], aes(x = firstchar_cabin, fill = Survived)) +
  geom_bar() +
  facet_wrap(~Pclass + Title) +
  ggtitle("Pclass & Title")
  xlab("Cabin") +
  ylab("Total Count") +
  ylim(0, 300) +
  labs(fill = "Survived")  
  
#multiple cabins
install.packages("stringr")
library(stringr)
data.combined$cabin_multipled <- as.factor(ifelse(str_detect(data.combined$Cabin, " "), "Y" ,"N"))

ggplot(data.combined[1:891,], aes(x = cabin_multipled, fill = Survived)) +
  geom_bar() +
  facet_wrap(~Pclass + Title) +
  ggtitle("Pclass + Title")
  xlab("Cabin") +
  ylab("Total Count") +
  ylim(0, 300) +
  labs(fill = "Survived")
  
#Analysis of the embarked variable
ggplot(data.combined[1:891,], aes(x = Embarked, fill = Survived)) +
  geom_bar() +
  facet_wrap(~Pclass + Title) +
  ggtitle("Pclass & Title")
  xlab("Embarked") +
  ylab("Total Count") +
  ylim(0, 750) +
  labs(fill = "Survived")