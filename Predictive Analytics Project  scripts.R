getwd()


MWUF  = read.csv("MWUF-2.csv")
MWUF



#2 There are no missing values in the data 
sum(is.na(MWUF))

#3 coding the binary variables as factors 

str(MWUF)

MWUF$reg1 = factor(MWUF$reg1)


MWUF$reg2= factor(MWUF$reg2)


MWUF$reg3 = factor(MWUF$reg3)



MWUF$reg4 = factor(MWUF$reg4)


MWUF$genf = factor(MWUF$genf)

MWUF$donr = factor(MWUF$donr)

MWUF$home = as.factor(MWUF$home)

MWUF$hinc = as.numeric(MWUF$hinc)

MWUF$wrat = as.numeric(MWUF$wrat)

MWUF$avhv = as.numeric(MWUF$avhv)

MWUF$incm = as.numeric(MWUF$incm)

MWUF$tgif = as.numeric(MWUF$tgif)

MWUF$lgif = as.numeric(MWUF$lgif)

MWUF$rgif = as.numeric(MWUF$rgif)

MWUF$tdon = as.numeric(MWUF$tdon)

MWUF$tlag = as.numeric(MWUF$tlag)


MWUF$chld= as.numeric(MWUF$chld)

str(MWUF)
#4

MWUF= MWUF[,-c(1,2,3,4,5,9,13,14,15,23)]
MWUF


# PARTITION DATA WITH sEED 123

set.seed(123)
train.index= sample(c(1:dim(MWUF)[1]), dim(MWUF)[1]*.7)
train = MWUF[train.index, ]
test= MWUF[-train.index, ]



#4.a

LogR1= glm(donr~., data=train, family = "binomial")
options(scipen=999)
summary(LogR1)


#PREDICTED PROBABILITIES 
LogR1.pred = predict(LogR1, test[, -13], type = "response")


#Confusion matrix

cv = table(test$donr, LogR1.pred>0.5)
cv

#Accuracy of LogR1, Precision and sensitivity and F1 score


sum(diag(cv))/sum(cv)

# Precision (Positive Predictive Value)=TP/(TP+FP) 
precisionR1 = cv[2, 2]/(cv[2, 2] + cv[2, 1]) 
#Calculating Hit Rate (TPR) = TP/(TP+FN)
sensitivityR1= cv[2, 2]/(cv[2, 2] + cv[1, 2])



f1scoreR1= 2*precisionR1*sensitivityR1/(precisionR1+sensitivityR1)
f1scoreR1



install.packages(c("rpart", "party", "partykit", "caret","nnet")  ,repos = "https://cran.rstudio.com")

# roc and auc stats 

library(rpart) 
library(party) 
library(partykit)
library(caret)
library(pROC)
library(rpart.plot)
library(car)


test_probR1 = predict(LogR1, newdata = test[, -13], type = "response")
test_rocR1 = roc(test$donr~ test_probR1, plot = TRUE, print.auc = TRUE)
par(pty="s")
test_rocR1 = roc(test$donr ~ test_probR1, plot = TRUE, print.auc = TRUE, legacy.axes=TRUE, percent=TRUE, xlab="False Positive Rate", ylab="True Positive Rate", lwd=4)

auc(test_rocR1)


#4.b do you see any multicollinearity # no


vif(LogR1)>4


#4.c 


DT = rpart(donr~.,method="class",data=train)
plot(as.party(DT))



rpart.rules(DT)

printcp(DT)

#PRUNED DT OR m

m= which.min(DT$cptable[, "xerror"])


DT$cptable[m, "CP"]

# A NEW TREE USING CP



x = prune(DT, cp = DT$cptable[which.min(DT$cptable[, "xerror"]), "CP"])


plot(as.party(x))


#PREDICTIONS


predDT = predict(x, test, decision.values=TRUE, type="class") 




cmDT= table(predDT, true=test$donr)
cmDT



#Accuracy of DT, Precision and sensitivity and F1 score


sum(diag(cmDT))/sum(cmDT)

# Precision (Positive Predictive Value)=TP/(TP+FP) 
precisionDT = cmDT[2, 2]/(cmDT[2, 2] + cmDT[2, 1]) 

precisionDT
#Calculating Hit Rate (TPR) = TP/(TP+FN)
sensitivityDT= cmDT[2, 2]/(cmDT[2, 2] + cmDT[1, 2])



f1scoreDT= 2*precisionDT*sensitivityDT/(precisionDT+sensitivityDT)
f1scoreDT


# ROC AND AUC STATS 

test_probDT = as.numeric(predict(x, test, decision.values=TRUE,type = "class"))
test_rocDT= roc(test$donr ~ test_probDT, plot = TRUE, print.auc = TRUE, legacy.axes=TRUE, percent=TRUE, xlab="False Positive Rate", ylab="True Positive Rate", lwd=4)


auc(test_rocDT)



#4. d a neural network model with 15 nodes

install.packages("nnet",repos = "https://cran.rstudio.com")

library(nnet)

ANN1= nnet(donr ~ ., data=train,  size=15, maxit=100, rang=0.1, decay=5e-4)

predANN1 = predict(ANN1,test,type='class')

# confusion matrix for ANN1



CMANN1=  table(predANN1,true=test$donr)
CMANN1

#Accuracy of ANN1, Precision and sensitivity and F1 score


sum(diag(CMANN1))/sum(CMANN1)

# Precision (Positive Predictive Value)=TP/(TP+FP) 
precisionANN1 = CMANN1[2, 2]/(CMANN1[2, 2] + CMANN1[2, 1]) 
precisionANN1
#Calculating Hit Rate (TPR) = TP/(TP+FN)
sensitivityANN1= CMANN1[2, 2]/(CMANN1[2, 2] + CMANN1[1, 2])



f1scoreANN1= 2*precisionANN1*sensitivityANN1/(precisionANN1+sensitivityANN1)
f1scoreANN1

install.packages("devtools")
library(devtools)
source_url('https://gist.githubusercontent.com/fawda123/7471137/raw/466c1474d0a505ff044412703516c34f1a4684a5/nnet_plot_update.r')
plot.nnet(ANN1)

test_probANN1 = (predict(ANN1,test))
test_rocANN1 = roc(test$donr~ test_probANN1, plot = TRUE, print.auc = TRUE, legacy.axes=TRUE, percent=TRUE, xlab="False Positive Rate", ylab="True Positive Rate", lwd=4)

auc(test_rocANN1)

# Neural network model with 50 hidden nodes 

ANN2= nnet(donr ~ ., data=train,  size=50, maxit=100, rang=0.1, decay=5e-4)

predANN2 = predict(ANN2,test,type='class')

# confusion matrix for ANN2



CMANN2=  table(predANN2,true=test$donr)
CMANN2

sum(diag(CMANN2))/sum(CMANN2)

# Precision (Positive Predictive Value)=TP/(TP+FP) 
precisionANN2 = CMANN2[2, 2]/(CMANN2[2, 2] + CMANN2[2, 1]) 
precisionANN2
#Calculating Hit Rate (TPR) = TP/(TP+FN)
sensitivityANN2= CMANN2[2, 2]/(CMANN2[2, 2] + CMANN2[1, 2])



f1scoreANN2= 2*precisionANN2*sensitivityANN2/(precisionANN2+sensitivityANN2)
f1scoreANN2



install.packages("devtools")
library(devtools)
source_url('https://gist.githubusercontent.com/fawda123/7471137/raw/466c1474d0a505ff044412703516c34f1a4684a5/nnet_plot_update.r')
plot.nnet(ANN2)

test_probANN2 = (predict(ANN2,test))
test_rocANN2= roc(test$donr ~ test_probANN2, plot = TRUE, print.auc = TRUE, legacy.axes=TRUE, percent=TRUE, xlab="False Positive Rate", ylab="True Positive Rate", lwd=4)


auc(test_rocANN2)


# using the logistic Reg in the new dataset


MWUFnew  = read.csv("MWUF_new.csv")
MWUFnew
str(MWUFnew)


MWUFnew$donr = factor(MWUFnew$donr)



MWUFnew$reg1 = factor(MWUFnew$reg1)


MWUFnew$reg2= factor(MWUFnew$reg2)


MWUFnew$reg3 = factor(MWUFnew$reg3)



MWUFnew$reg4 = factor(MWUFnew$reg4)


MWUFnew$genf = factor(MWUFnew$genf)

MWUFnew$donr = factor(MWUFnew$donr)

MWUFnew$home = as.factor(MWUFnew$home)

MWUFnew$hinc = as.numeric(MWUFnew$hinc)

MWUFnew$wrat = as.numeric(MWUFnew$wrat)

MWUFnew$avhv = as.numeric(MWUFnew$avhv)

MWUFnew$incm = as.numeric(MWUFnew$incm)

MWUFnew$tgif = as.numeric(MWUFnew$tgif)

MWUFnew$lgif = as.numeric(MWUFnew$lgif)

MWUFnew$rgif = as.numeric(MWUFnew$rgif)

MWUFnew$tdon = as.numeric(MWUFnew$tdon)

MWUFnew$tlag = as.numeric(MWUFnew$tlag)


MWUFnew$chld= as.numeric(MWUFnew$chld)

 ##dropping the variables we don't need '
MWUFnew= MWUFnew[,-c(1,2,3,4,5,9,13,14,15,23)]
MWUFnew

# using LogR1 to make predictions  MWUFnew

Prednew = predict(LogR1,MWUFnew[,-13],type= "response")
prednew.df = as.data.frame(Prednew)


prednew.df= ifelse(prednew.df>0.5, "donor", "non-donor")

prednew.df_count = table(prednew.df)

prednew.df_count
