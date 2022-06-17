getwd()
read.csv("customer_churn.csv")->customer_churn
#Questions on Simple logistic regression:
class(customer_churn$Churn)
as.factor(customer_churn$Churn)->customer_churn$Churn
glm(Churn ~ TechSupport, data= customer_churn, family="binomial") ->log_mod1
summary(log_mod1) 
predict(log_mod1,data.frame(TechSupport="Yes"),type="response")
predict(log_mod1,data.frame(TechSupport="No"),type="response")
predict(log_mod1,data.frame(TechSupport="No internet service"),type="response")

class(customer_churn$Dependents)
as.factor(customer_churn$Dependents)->customer_churn$Dependents
glm(Dependents~tenure, data= customer_churn, family="binomial") ->log_mod2
summary(log_mod2)
predict(log_mod2,data.frame(tenure=10),type="response")
predict(log_mod2,data.frame(tenure=50),type="response")
predict(log_mod2,data.frame(tenure=70),type="response")


#Questions on Multiple Logistic regression:
library(caTools)
class(customer_churn$gender)
as.factor(customer_churn$gender)->customer_churn$gender

sample.split(customer_churn$gender,SplitRatio = 0.65)-> split_tag
View(split_tag)
subset(customer_churn, split_tag==T)->train
subset(customer_churn, split_tag==F)->test
View(train)
View(test)

glm(gender~Dependents+InternetService+Contract, data= train, family="binomial") ->log_mod_multi
predict(log_mod_multi,newdata = test,type="response") -> result_log_multi
View(result_log_multi)
range(result_log_multi)
table(test$gender, result_log_multi>0.49)->tab
View(tab)
accuracy<-sum(diag(tab))/sum(tab)
accuracy


glm(gender~tenure+MonthlyCharges+PaymentMethod, data= train, family="binomial") ->log_mod_multi2
predict(log_mod_multi2,newdata = test,type="response") -> result_log_multi2
range(result_log_multi2)
table(test$gender, result_log_multi2>0.49)->tab
#d.	Calculating the accuracy from the confusion matrix
accuracy<-sum(diag(tab))/sum(tab)
accuracy

#ROCR 
library(caTools)
sample.split(customer_churn$Churn,SplitRatio = 0.80)-> split_tag
subset(customer_churn, split_tag==T)->train
subset(customer_churn, split_tag==F)->test
glm(Churn~tenure+MonthlyCharges+PaymentMethod, data= train, family="binomial") ->log_mod_roc
predict(log_mod_roc,newdata = test,type="response") -> result_log_roc

#d.	Building the 'Accuracy vs CutOff' plot
install.packages("ROCR")
library(ROCR)
prediction(result_log_roc,test$Churn) -> predict_log_roc
performance(predict_log_roc,"acc")->acc
plot(acc)

#e.	Plotting the 'ROC' curve
performance(predict_log_roc,"tpr","fpr")->roc_curve
plot(roc_curve)
plot(roc_curve,colorize=T)
plot(roc_curve,colorize=T,print.cutoffs.at=seq(0.1,by=0.1))
#f.	Finding the 'Area under the curve'
performance(predict_log_roc,"auc")->auc
auc



