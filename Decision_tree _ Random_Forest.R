#Questions on decision tree:
#1.	Building a decision tree model:
#a.	Start off by dividing the 'customer_churn' data into train & test sets in 70:30 ratio. 
#The split-criteria would be determined by the 'Dependents' column.	

getwd()
setwd("E:/Intellipaat/R/Assignments")
read.csv('E:/Intellipaat/R/Assignments/customer_churn.csv', ,stringsAsFactors = T)->customer_churn
str(customer_churn)
View(customer_churn)
set.seed(1)
library(caTools)
sample.split(customer_churn$Dependents,SplitRatio = 0.7)->dec_split_data
View(dec_split_data)
subset(customer_churn, dec_split_data==T)->dec_train
subset(customer_churn, dec_split_data==F)->dec_test
nrow(dec_train)
nrow(dec_test)
View(dec_train)
View(dec_test)

#b.Build a decision tree model on top of the 'train' set, where the dependent variable is 
#'Dependents' & the independent variable is 'Partner'. Store the result in 'mod_tree1'
#install.packages("rpart")
#install.packages("rpart.plot")
install.packages("tree")

library(rpart)
library(rpart.plot)
library(tree)

tree(Dependents~Partner, data=dec_train)->dec_treemodel
plot(dec_treemodel)
text(dec_treemodel)
#or
rpart(formula = Dependents~Partner, data=dec_train, method = "class")->dec_rpartmodel

#c.	Plot the tree and add text
rpart.plot(x=dec_rpartmodel, type=5 , extra=0, tweak=1.2)

#d.	Predict the values on the test set and store the result in 'result_tree1'
predict(object = dec_rpartmodel, newdata = dec_test, type="class")->predict_dec_tree

#e.	Build a confusion matrix for the actual values & the predicted values
table(predict_dec_tree, dec_test$Dependents)->confmat_dec_tree
confmat_dec_tree

#f.	Calculate the accuracy from the confusion matrix
#accuracy
sum(diag(confmat_dec_tree))/sum(confmat_dec_tree)->accu_dec_tree
accu_dec_tree

#2.Building 2nd decision tree model on same 'train' & 'test' sets:
#a.	In this case the dependent variable is 'Dependents' & the independent variables are 'Partner' & 'InternetService'. 
#Store the result in 'mod_tree2'
set.seed(2)
#library(caTools)
#sample.split(customer_churn$Dependents,SplitRatio = 0.7)->dec_split_data
#View(dec_split_data)
#subset(customer_churn, dec_split_data==T)->dec_train
#subset(customer_churn, dec_split_data==F)->dec_test
#nrow(dec_train)
#nrow(dec_test)
#View(dec_train)
#View(dec_test)
#install.packages("rpart")
#install.packages("rpart.plot")
#install.packages("tree")
#library(rpart)
#library(rpart.plot)
#library(tree)
#tree(Dependents~Partner, data=dec_train)->dec_treemodel
#plot(dec_treemodel)
#text(dec_treemodel)
rpart(formula=Dependents~Partner+InternetService,data=dec_train,method="class")->mod_tree2

#b.	Plot the tree & add text
rpart.plot(x=mod_tree2, type=5 , extra=0, tweak=1.2)

#c.	Predict the values on the test set & store the result in 'result_tree2'
predict(object = mod_tree2, newdata = dec_test, type="class")->result_tree2

#d.	Build a confusion matrix for the actual values & the predicted values
table(result_tree2, dec_test$Dependents)->confmat_dec_tree
confmat_dec_tree

#e.	Calculate the accuracy from the confusion matrix
sum(diag(confmat_dec_tree))/sum(confmat_dec_tree)->accu_dec_tree
accu_dec_tree

###random forest$$
#1.Building the first "Random Forest" model:
#a.	Start off by dividing the 'customer_churn' data into train & test sets in 65:35 ratio.
#The split-criteria would be determined by the 'gender' column
set.seed(3)
library(caTools)
sample.split(customer_churn$gender, SplitRatio = 0.65)->ran_split_data
View(ran_split_data)
subset(customer_churn, ran_split_data==T)->ran_train
subset(customer_churn, ran_split_data==F)->ran_test
nrow(ran_train)
nrow(ran_test)
View(ran_train)
View(ran_test)

#b.	Build a random forest model on top of the 'train' set,
#where the dependent variable is 'gender' & the independent variables are 'MonthlyCharges' & 'tenure'. 
#The number of decision trees in the random forest would be 35. Store the result in 'mod_forest1'
#install.packages("randomForest")
library(randomForest)
randomForest(gender~MonthlyCharges+tenure, data = ran_train, ntree = 35 )->mod_forest1

#c.	Find the importance of the independent variables and also plot it
importance(mod_forest1)
varImpPlot(mod_forest1)

#d.	Predict the values on top of the test set & store the result in 'result_forest1'
predict(mod_forest1, newdata = ran_test, type = "class" )->result_forest1

#e.	Build a confusion matrix for the actual values & the predicted values
table(result_forest1, ran_test$gender)->ran_confmat_tab
ran_confmat_tab

#f.	Find out the accuracy from the confusion matrix
sum(diag(ran_confmat_tab)/sum(ran_confmat_tab))->ran_acc_forest
ran_acc_forest


#2.Build a 2nd 'Random forest' model on the same train & test sets:
#a.The dependent & the independent variables would be same. 
#The number of decision trees would be 350. Store the result in 'mod_forest2'
randomForest(gender~MonthlyCharges+tenure, data = ran_train, ntree = 350 )->mod_forest2


#b.	Find the importance of the independent variables & also plot it
importance(mod_forest2)
varImpPlot(mod_forest2)

#c.	Predict the values on top of test set & store the result in 'result_forest2'
predict(mod_forest2, newdata = ran_test, type = "class" )->result_forest2

#d.	Build a confusion matrix for the actual values & predicted values
table(result_forest2, ran_test$gender)->ran_confmat_tab
ran_confmat_tab

#e.	Find out the accuracy from the confusion matrix
sum(diag(ran_confmat_tab)/sum(ran_confmat_tab))->ran_acc_forest
ran_acc_forest




