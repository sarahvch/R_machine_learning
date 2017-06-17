
library(randomForest)
library(Amelia)
library(dplyr)
library(ggplot2)
library(rmarkdown)
library(caTools)
library(stringr)
library(randomForest)
library(readr) # CSV file I/O, e.g. the read_csv function
library(caTools)
library(rpart.plot)
library(corrplot)
library(Hmisc)
library(rpart.plot)
library(class)
library(e1071)
library(neuralnet)

# Import Data and check structure

boobs <- read.csv("../input/data 2.csv")
str(boobs)
head(boobs)

#Checking out data
any(is.na(boobs))
missmap(boobs, main='Missing data', col=c("yellow","black"), legend = FALSE)

#removing X column
boobs$X <- NULL
boobs$ids <- boobs$id
boobs$id <- NULL
boobs$diagnosis <- as.factor(boobs$diagnosis)
str(boobs)

#checking out correlations between data
boob.cor <- test <- select(boobs,-ids)
boob.cor$diagnosis <- as.numeric(boobs$diagnosis)
B <- cor(boob.cor)
corrplot(B, method="circle")

#investigating coorelations futher
ggplot(boobs,aes(perimeter_mean, area_mean)) + geom_point(aes(color=factor(diagnosis)),alpha=0.5) + scale_fill_discrete(name="diagnosis", breaks=c("0", "1"), labels=c("M", "B")) + labs(title = "diagnosis based on perimeter and area mean")
ggplot(boobs,aes(symmetry_mean, smoothness_se)) + geom_point(aes(color=factor(diagnosis)),alpha=0.5) + scale_fill_discrete(name="diagnosis", breaks=c("0", "1"), labels=c("M", "B")) + labs(title = "diagnosis based on symetry and smoothness")

#train test split
split <- sample.split(boobs$diagnosis, Split = 0.7)
train <- subset(boobs, split == T)
test <- subset(boobs, split == F)

#train model
log.model <- glm(formula=diagnosis ~ . , family = binomial(link='logit'),data = train)
summary(log.model)
fitted.probabilities <- predict(log.model,newdata=test[2:32],type='response')

c <-table(test$diagnosis, fitted.probabilities > 0.5)
c
c.t <-sum(diag(c))/sum(c)
print(c.t)

#train random forest model
rf.model <- randomForest(diagnosis ~., data = train)

#prediction accuracy
predicted.values <- predict(rf.model, test[2:32])
d <- table(predicted.values, test$diagnosis)
print(d)
d.t <-sum(diag(d))/sum(d)
print(d.t)

```

#K Nearest Neighbor
normalize <- function(x){
  return((x-min(x))/(max(x)-min(x)))
}

boobs1<- as.data.frame(lapply(boobs[2:30],normalize))
boobs1$diagnosis <- boobs$diagnosis

#split data
split <- sample.split(boobs1$diagnosis, Split = 0.7)
train <- subset(boobs1, split == T)
test <- subset(boobs1, split == F)

#predict with k=1
predicted.boobs <- knn(train[1:29],test[1:29],train$diagnosis,k=1)

#missclassification rate
mean(test$diagnosis != predicted.boobs)

# create k values fuction
predicted.boobs <-NULL
error.rate <-NULL

for(i in 1:10){
  predicted.boobs <- predicted.species <- knn(train[1:29],test[1:29],train$diagnosis,k=i)
  error.rate[i] <- mean(test$diagnosis != predicted.boobs)
}


#set k values
k.values <- 1:10
error.df <- data.frame(error.rate,k.values)

#plot elbow
pl <- ggplot(error.df,aes(x=k.values,y=error.rate)) + geom_point()
pl + geom_line(lty="dotted",color='red')

#choose k
predicted.boobs <- knn(train[1:29],test[1:29],train$diagnosis,k=5)
mean(test$diagnosis != predicted.boobs)

#confusion matrix
e <-table(test$diagnosis, predicted.boobs)
print(e)
e.t <-sum(diag(e))/sum(e)
print(e.t)

#Support Vector Machine
model <- svm(diagnosis ~., data = train)
summary(model)

#predict
pred.values <- predict(model, test[1:29])
table(pred.values, test$diagnosis)

#tune model
tune.results <-tune(svm, train.x = train[1:29], train.y = train$diagnosis, kernal = 'radial', ranges=list(cost = c(0.1,1,10), gamma = c(0.5, 1, 2)))
summary(tune.results)

#tuned svm
tuned.svm <-svm(diagnosis~., data = train, kernal='radial', cost =10, gamma=0.5)
summary(tuned.svm)

#predict again
pred.values <- predict(tuned.svm, test[1:29])
f <- table(pred.values, test$diagnosis)
f.t <-sum(diag(f))/sum(f)
print(f.t)

#Neural Nets
normalize <- function(x){
  return((x-min(x))/(max(x)-min(x)))
}

boobs1<- as.data.frame(lapply(boobs[2:31],normalize))
boobs1$diagnosis <- boobs$diagnosis
boobs1$diagnosis <- as.numeric(boobs1$diagnosis)

binary <- function(dg){
  for(i in 1:length(dg)){
    if(dg[i] == 1){
      dg[i] <- 0
    }else{
      dg <- 1
    }
  }
  return(dg)
}

boobs1$diagnosis <-sapply(boobs1$diagnosis,binary)

#set up split
split <- sample.split(boobs1$diagnosis, Split = 0.7)
train <- subset(boobs1, split == T)
test <- subset(boobs1, split == F)

#train model
nn <- neuralnet(diagnosis ~ radius_mean + texture_mean + perimeter_mean + area_mean + 
                  smoothness_mean + compactness_mean + concavity_mean + concave.points_mean + 
                  symmetry_mean + fractal_dimension_mean + radius_se + texture_se + 
                  perimeter_se + area_se + smoothness_se + compactness_se + 
                  concavity_se + concave.points_se + symmetry_se + fractal_dimension_se + 
                  radius_worst + texture_worst + perimeter_worst + area_worst + 
                  smoothness_worst + compactness_worst + concavity_worst + 
                  concave.points_worst + symmetry_worst + fractal_dimension_worst, data=train, hidden = c(5,3), linear.output = FALSE)

#predict
predicted.nn.values <- compute(nn, test[,1:30])

#round predicted values
predictions <- sapply(predicted.nn.values$net.result,round)

#table
g <-table(predictions, test$diagnosis)
g.t <- sum(diag(g))/sum(g)
print(g.t)

#Accuracy
accur<- matrix(c(c.t,d.t,e.t,f.t,g.t),ncol=1,byrow=FALSE)
colnames(accur) <- c("Accuracy")
rownames(accur) <- c("LG","RF","KNN","SVM","NN")
accur <- as.table(accur)
accur
