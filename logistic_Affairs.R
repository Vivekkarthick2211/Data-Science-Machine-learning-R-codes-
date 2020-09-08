#read a dataset
attr<-read.csv("C:\\Users\\jeeva\\Downloads\\R assignment\\logistic\\affairs.csv")
attach(attr)
#output as categorical either 1 or 0
attr$affairs[attr$affairs>0]<-1
attr$affairs[attr$affairs==0]<-0
library(dummies)
#dummy variables for categorical data
dummiesgen = dummy(attr$gender , sep = "_")
dummieschild = dummy(attr$children , sep = "_")
attr$gender=dummiesgen #assign column values as dummy values
attr$children=dummieschild #assign column values as dummy values
summary(attr$affairs)

attr=na.omit(attr)#if null then omit these things
#in these dataset have no null values
# Logistic Regression 
#linear model
model1=lm(affairs~factor(gender)+age+yearsmarried+factor(children)+religiousness+education+occupation+rating)
summary(model1)
#logistic model
model2=glm(affairs~gender+age+yearsmarried+children+religiousness+education+occupation+rating,data=attr,family = "binomial")
summary(model2)
exp(coef(model2))
# Confusion Matrix Table
prob1=predict(model2,type=c("response"),attr)
prob1

confus1=table(prob1>0.6,attr$affairs)
confus1

# Model Accuracy

Accuracy1<-sum(diag(confus1))/sum(confus1)
Accuracy1  #0.2562396
#create new columns
pred_values1  = NULL
yes_NO1 = NULL
for (i in 1:601){
  pred_values1[i] = ifelse(prob1[i]>= 0.5,1,0)
  yes_NO1[i] = ifelse(prob1[i]>= 0.5,'Yes',"no")
  
}
attr[,"prob1"] = prob1
attr[,"pred_values1"] = pred_values1
attr[,"yes_NO1"] = yes_NO1

# ROC Curve 
library(ROCR)
rocrpredd<-prediction(prob1,attr$affairs)
rocrperff<-performance(rocrpredd,'tpr','fpr')
plot(rocrperff,colorize=T)

