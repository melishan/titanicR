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

ggplot(data.combined[1:891,], aes(x = Title, fill = Survived)) +
    geom_bar(width = 0.9) +
    facet_wrap(~Pclass) +
    ggtitle("Pclass") +
    xlab("Title") +
    ylab("Total Count") +
    labs(fill = "Survived")
?facet_wrap