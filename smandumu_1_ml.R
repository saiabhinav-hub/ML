library(gdata)                   # load gdata package 
library(dplyr)
library(caret)
bankdatat = read.csv("UniversalBank.csv")
bankdata<-bankdatat[,c(-1,-5)] # Excluding ID and ZIP Code
set.seed(123)
head(bankdata)
bankdata$Personal.Loan<-factor(bankdata$Personal.Loan,levels = c(0,1),labels =c("Deny","Accept"))
View(bankdata)
summary(bankdata)
# Splitting Data into training (60%) and validation (40%) 
trgindex <- createDataPartition(bankdata$Age,p=0.6,list = FALSE)
trgdata <- bankdata[trgindex,]
vlddata <- bankdata[-trgindex,]
tstindex <- createDataPartition(bankdata$Age,p=0.2,list = FALSE) #20% test data
tstdata <- bankdata[tstindex,]

summary(trgdata)
summary(vlddata)
summary(tstdata)
train.norm.df <- trgdata[,-8]
valid.norm.df <- vlddata[,-8]
test.norm.df <- tstdata[,-8]

View(train.norm.df)
View(valid.norm.df)
View(traval.norm.df)
set.seed(15)
# use preProcess() from the caret package to normalize Sales and Age.
norm.values <- preProcess(train.norm.df, method=c("center", "scale"))
train.norm.df <- predict(norm.values, train.norm.df) # Replace first two columns with normalized values
valid.norm.df <- predict(norm.values, valid.norm.df)
test.norm.df <- predict(norm.values, test.norm.df)
library(FNN)
nn <- knn(train = train.norm.df, test = test.norm.df, 
          cl = trgdata$Personal.Loan, k = 1, prob=TRUE) # We use k = 1

library("gmodels")
CrossTable(x=tstdata$Personal.Loan,y=nn, prop.chisq = FALSE)
confusionMatrix(nn,tstdata$Personal.Loan)$overall[1]

accuracy.df <- data.frame(k = seq(1, 56, 1), accuracy = rep(0, 56))
# compute knn for different k on validation.
for(i in 1:56) {
  knn.pred <- knn(train.norm.df, valid.norm.df, 
                  cl = trgdata$Personal.Loan, k = i)
  accuracy.df[i, 2] <- confusionMatrix(knn.pred, vlddata$Personal.Loan)$overall[1] 
}
accuracy.df 
accuracy.df[which.max(accuracy.df$accuracy),] 
## End of Q1-a ##
#Q3#
nn4 <- knn(train = train.norm.df, test = valid.norm.df, 
          cl = trgdata$Personal.Loan, k = 4, prob=TRUE) # We use k = 4

library("gmodels")
CrossTable(x=vlddata$Personal.Loan,y=nn4, prop.chisq = FALSE)
confusionMatrix(nn4,vlddata$Personal.Loan)$overall[1]
# Q1-b #
# Binding to consider the given customer scenario
E1 <- data.frame(Age =40, Experience = 10, Income = 84,Family = 2, CCAvg = 2,Education = 1,Mortgage = 0,"Personal Loan"= "Deny","Securities Account" =0, "CD Account" = 0, Online = 1,"Credit Card" = 1)
E2 <- data.frame(Age =40, Experience = 10, Income = 84,Family = 2, CCAvg = 2,Education = 2,Mortgage = 0,"Personal Loan"= "Accept","Securities Account" =0, "CD Account" = 0, Online = 1,"Credit Card" = 1)
E3 <- data.frame(Age =40, Experience = 10, Income = 84,Family = 2, CCAvg = 2,Education = 3,Mortgage = 0,"Personal Loan"= "Deny","Securities Account" =0, "CD Account" = 0, Online = 1,"Credit Card" = 1)
test.predict <- as.data.frame(rbind(E1,E2,E3))
test.predict
class(test.predict)
test.predict.norm <- test.predict[,-8]
norm.values3<- preProcess(test.predict.norm, method=c("center", "scale"))
test.predict.norm<-predict(norm.values3,test.predict.norm)
nn3 <- knn(train = train.norm.df, test = test.predict.norm, 
           cl = trgdata$Personal.Loan, k = 2, prob=TRUE) # We use k = 4  # Q4
nn3
confusionMatrix(nn3,test.predict$Personal.Loan)$overall[1]
# No loan Acceptane



# Question 5 

# Splitting Data into training (50%) and validation (30%) 

trgindex2 <- createDataPartition(bankdata$Age,p=0.5,list = FALSE)
trgdata2 <- bankdata[trgindex2,]
vlddata2 <- bankdata[-trgindex2,]
tstindex2 <- createDataPartition(vlddata2$Age,p=0.2,list = FALSE) #20% test data
tstdata2 <- vlddata2[tstindex2,]
vlddata2 <- vlddata2[-tstindex2,] #30 %


View(trgdata2)

summary(trgdata2)
summary(vlddata2)
summary(tstdata2)
train.norm.df2 <- trgdata2[,-8]
valid.norm.df2 <- vlddata2[,-8]
test.norm.df2 <- tstdata2[,-8]

View(train.norm.df2)
View(valid.norm.df2)
View(test.norm.df2)

set.seed(15)
# use preProcess() from the caret package to normalize Sales and Age.
norm.values2 <- preProcess(train.norm.df2, method=c("center", "scale"))
train.norm.df2 <- predict(norm.values2, train.norm.df2) # Replace first two columns with normalized values
valid.norm.df2 <- predict(norm.values2, valid.norm.df2)
test.norm.df2 <- predict(norm.values2, test.norm.df2)

View(test.norm.df2)




library(FNN)
nn2 <- knn(train = train.norm.df2, test = test.norm.df2, 
          cl = trgdata2$Personal.Loan, k = 4, prob=TRUE) # We use k = 1

library("gmodels")
CrossTable(x=tstdata2$Personal.Loan,y=nn2, prop.chisq = FALSE)
confusionMatrix(nn2,tstdata2$Personal.Loan)$overall[1]

#test and valid 

nn5 <- knn(train = train.norm.df2, test = valid.norm.df2, 
           cl = trgdata2$Personal.Loan, k = 4, prob=TRUE) # We use k = 1

library("gmodels")
CrossTable(x=vlddata2$Personal.Loan,y=nn2, prop.chisq = FALSE)
confusionMatrix(nn5,vlddata2$Personal.Loan)$overall[1]



accuracy.df2 <- data.frame(k = seq(1, 56, 1), accuracy = rep(0, 56))
# compute knn for different k on validation.
for(i in 1:56) {
  knn.pred <- knn(train.norm.df2, valid.norm.df2, 
                  cl = trgdata2$Personal.Loan, k = i)
  accuracy.df2[i, 2] <- confusionMatrix(knn.pred, vlddata2$Personal.Loan)$overall[1] 
}
accuracy.df2 
accuracy.df2[which.max(accuracy.df2$accuracy),]



