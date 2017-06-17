#Predict if borrower will payback loan in full
library(caTools)
library(rmarkdown)
library(ggplot2)
library(e1071)

#save data as loans
loans <-read.csv('loan_data.csv')

#summary of loans
summary(loans)

#sturcture of loans
str(loans)

#It looks like we can change a few columns to factors
loans$credit.policy <- as.factor(loans$credit.policy)
loans$inq.last.6mths <-as.factor(loans$inq.last.6mths)
loans$delinq.2yrs <- as.factor(loans$delinq.2yrs)
loans$pub.rec <- as.factor(loans$pub.rec)
loans$not.fully.paid <- as.factor(loans$not.fully.paid)

#Check if the factor was applied
str(loans)

#Check out how data effects not fully paid
#Fico Score vs. Not Fully Paid
ggplot(loans, aes(fico)) + geom_histogram(aes(fill=not.fully.paid), color='black') + theme_bw()
#it looks like the higher fico scores correspond with lower not-paid-loans

#Type of Loan vs Not Fully paid
ggplot(loans, aes(factor(purpose))) + geom_bar(aes(fill=not.fully.paid), position='dodge') 
+ theme(axis.text.x = element_text(angle = 90, hjust = 1))

#It looks like debt consolidation is the least likely to be paid in full

#lets check out fico score vs inerest rate
ggplot(loans, aes(int.rate, fico)) + geom_point(aes(color=not.fully.paid), alpha=0.5) + theme_bw()

#split data into 0.7 train/0.3 test on not.fully.paid
set.seed(101)
loans$sample <-sample.split(loans$not.fully.paid, SplitRatio = 0.7)
train <- subset(loans, loans$sample == TRUE)
test <- subset(loans, loans$sampl == FALSE)

#use svm to train a model on training set
model <- svm(not.fully.paid ~., data = train)

#summary of model
summary(model)

#predict new values from the test set using model
predic.values <- predict(model, train)

#predict
table(predic.values, train[,14])

#finding cost and gamma fit first try
#tune.results <- tune(svm,not.fully.paid ~., data=train,kernel='radial',ranges=list(cost=c(0.1,1,10), gamma=c(0.5,1,2)))
#summary(tune.results)

#results cost = 1 and gamma = 0.5
#finding cost and gamma fit second try 
#using other numbers around these ranges=list(cost=c(0.5,0.9,1,2), gamma=c(0.3,0.4,0.5,0.6,0.7))
#tune.results <- tune(svm,not.fully.paid ~., data=train,kernel='radial',ranges=list(cost=c(0.5,0.9,1,2), gamma=c(0.3,0.4,0.5,0.6,0.7)))


#results cost = 0.9 and gamma = 0.4

#tuned model
tuned.svm <-svm(not.fully.paid ~., data = train, kernal='radial', cost =0.9, gamma=0.4)
predicted.values <- predict(tuned.svm, train)

#table predicted values
table(predicted.values, train[,14])


