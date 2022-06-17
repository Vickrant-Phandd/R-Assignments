getwd()
read.csv("customer_churn.csv")->customer_churn
library(caTools)
sample.split(customer_churn$tenure, SplitRatio = 0.7)->split_tag
#View(split_tag)
subset(customer_churn,split_tag==T)->train
subset(customer_churn,split_tag==F)->test
#View(train)
#View(test)
lm(tenure~Contract, data=train)->model1
predict(model1, newdata=test)->predicted_values
head(predicted_values)
cbind(Actual=test$tenure,Predicted=predicted_values)->final_data
as.data.frame(final_data) -> final_data
head(final_data)
View(final_data)
final_data$Actual - final_data$Predicted ->error
head(error)
#nrow(test)
#nrow(predicted_values)
cbind(final_data,error)->final_data
head(final_data)

View(final_data)

#Calculate rmse with formula
sqrt(mean((final_data$error)^2))->rmse
rmse  

#install.packages("caret")  for RMSE(Alternative)
library(caret)
RMSE(predicted_values, test$tenure)
###########################################################
#multiple linear regression
getwd()
setwd("E:/Intellipaat/Assignments")
read.csv('E:/Intellipaat/Assignments/customer_churn.csv')->customer_churn
library(caTools)
sample.split(customer_churn$MonthlyCharges,SplitRatio = 0.75)-> split_model
subset(customer_churn, split_model==T)->train1
subset(customer_churn, split_model==F)->test1
View(train1)
View(test1)
lm(MonthlyCharges~Dependents+MultipleLines+OnlineSecurity+OnlineBackup+DeviceProtection, data=train1)->model1
predict(model1, newdata=test1)->predicted_values
as.data.frame(predicted_values)->predicted_values
head(predicted_values)
View(predicted_values)
cbind(Actual=test1$MonthlyCharges,predicted=predicted_values)->final_data1
as.data.frame(final_data)->final_data1

#cbind(Actual=test$tenure,Predicted=predicted_values)->final_data

head(final_data1)
View(final_data1)

final_data1$Actual-final_data1$predicted->error
head(error)
#nrow(test)
#nrow(predicted_values)
cbind(final_data1,error)->final_data1
head(final_data1)

View(final_data1)

#Calculate rmse with formula
sqrt(mean((final_data1$error)^2))->rmse
rmse  

library(ggplot2)

ggplot(customer_churn,aes(x = tenure, y = Contract )) + geom_point(size = 1)+geom_smooth(method = "lm")

#plot of predicted Vs error

ggplot(customer_churn, aes(x = Predicted, y =error)) + geom_point()

qqnorm(final_data$error)

