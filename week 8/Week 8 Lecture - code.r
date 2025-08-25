#LOAN data example

ID <-c(1:15)
age <- c('young','young','young','young','young','middle','middle','middle','middle','middle','old','old','old','old','old')
has_job <- c('false','false','true','true','false','false','false','true','false','false','false','false','true','true','false')
own_house <- c('false','false','false','true','false','false','false','true','true','true','true','true','false','false','false')
credit_rating <- c('fair','good','good','fair','fair','fair','good','good','excellent','excellent','excellent','good','good','excellent','fair')
class <- c('No','No','Yes','Yes','No','No','No','Yes','Yes','Yes','Yes','Yes','Yes','Yes','No')
loan_data <- data.frame(age, has_job, own_house, credit_rating, class)
loan_data

# entropy S1
probabilities <- prop.table(table(loan_data$own_house))
probabilities
-sum(probabilities*log2(probabilities))

loan_data$class<-as.factor(loan_data$class)

library(C50)
myTree = C5.0(x = loan_data[, -5], y = loan_data$class)
plot(myTree)

summary(myTree)

# Example of using CART algorithm
library(rpart)
library(rpart.plot)
tree <- rpart(class ~ . , method='class', data= loan_data,control=rpart.control(minsplit=2, minbucket=1, cp=0.001))

printcp(tree)

rpart.plot

# HITTERS dataset example

install.packages('ISLR')

library(ISLR)

Hitters

str(Hitters)

any(is.na(Hitters))

## Data cleaning
# remove NA values
Hitters <-na.omit(Hitters)
dim(Hitters)

any(is.na(Hitters))

install.packages('tree')

hist(Hitters$Salary)

#log transform Salary to make it a bit more normally distributed
hist(log(Hitters$Salary))

# Build a regression tree
library(tree)
# put salary on log scale and fit reg. tree
treefit <- tree(log(Salary) ~ Years + Hits, data=Hitters)
summary(treefit)

plot(treefit)
text(treefit,cex=0.75)

plot(Hitters$Years, Hitters$Hits,
     xlab="Years", ylab="Hits")
#text(Hitters$Years, Hitters$Hits, c("s", "c", "v")[Hitters$Salary])
partition.tree(treefit, add = TRUE, cex = 1.5)

prune.mytree = prune.tree(treefit, best=3)
plot(prune.mytree)
text(prune.mytree)
title("This is a pruned tree with 3 terminal nodes!")

library(caret)

split <- createDataPartition(y=Hitters$Salary, p=0.5, list=FALSE)

train <- Hitters[split,]
test <- Hitters[-split,]

#Create tree model
trees <- tree(log(Salary)~., train)
trees
plot(trees)
text(trees, pretty=0)

#Cross validate to see whether pruning the tree will improve performance
cv.trees <- cv.tree(trees)
plot(cv.trees)

prune.trees <- prune.tree(trees, best=7)
plot(prune.trees)
text(prune.trees, pretty=0)

yhat <- predict(prune.trees, test)
plot(yhat, log(test$Salary))
abline(0,1)

mean((yhat - log(test$Salary))^2)
