#1.Data science project#
getwd()
setwd("E:/Intellipaat/R/Assignments")
read.csv('E:/Intellipaat/R/Assignments/census_income.csv', ,stringsAsFactors = T)->censusdata
str(censusdata)
View(censusdata)

#1.Data Preprocessing
#a.Replace all missing value with NA
table(is.na(censusdata))
sum(is.na(censusdata))
colSums(is.na(censusdata))

#space and ? 
censusdata[censusdata==" ?"]=NA
View(censusdata)
table(is.na(censusdata))
sum(is.na(censusdata))
colSums(is.na(censusdata))

#b.Remove all the rows that contain NA values.
censusdata=na.omit(censusdata)
table(is.na(censusdata))
sum(is.na(censusdata))
colSums(is.na(censusdata))

#c. Remove all the white space from the colums
#install.packages("stringr")
library(stringr)
library(dplyr)

str(censusdata)
censusdata$workclass=as.character(censusdata$workclass) 
censusdata$education=as.character(censusdata$education) 
censusdata$marital.status=as.character(censusdata$marital.status) 
censusdata$occupation=as.character(censusdata$occupation) 
censusdata$relationship=as.character(censusdata$relationship) 
censusdata$race=as.character(censusdata$race) 
censusdata$sex=as.character(censusdata$sex) 
censusdata$native.country=as.character(censusdata$native.country) 
censusdata$X=as.character(censusdata$X) 


censusdata<-censusdata %>% mutate_if(is.character, str_trim)
#OR
censusdata=mutate_if(censusdata, is.character, str_trim)
str(censusdata)


#2.	Data Manipulation:
#Questions:
#a)	Extract the "education" column and store it in "census_ed" .
census_ed=censusdata$education
#OR
census_ed1=select(censusdata, education)
View(census_ed)
View(census_ed1)

#b)	Extract all the columns from "age" to "relationship" and store it in "census_seq".
census_seq1<-censusdata%>%select(age:relationship)
View(census_seq1)
#OR with Dplyr package
census_seq=select(censusdata, age:relationship)
View(census_seq)

#c)	Extract the column number "5", "8", "11" and store it in "census_col".
census_col1<-censusdata%>%select(5,8,11)
View(census_col1)
#OR with Dplyr package
census_col=select(censusdata, 5,8,11)
View(census_col)

#d)	Extract all the male employees who work in state-gov and store it in "male_gov".
names(censusdata)
table(censusdata$workclass)
male_gov <- filter(censusdata ,sex == "Male" & workclass == "State-gov")
View(male_gov)
#or
abc=censusdata%>%filter(sex == "Male" & workclass == "State-gov")
View(abc)

#e)	Extract all the 39 year olds who either have a bachelor's degree or who are native of United States and store the result in "census_us".
table(censusdata$native.country)
table(censusdata$education)
census_us=censusdata%>%filter(age=="39"&(education=="Bachelors"|native.country=="United-States"))
View(census_us)

#f)	Extract 200 random rows from the "census" data frame and store it in "census_200".
census_200<-sample_n(census_us,200)
View(census_200)

#g)	Get the count of different levels of the "workclass" column.
table(census_us$workclass)
#OR
countwrkcls<-count(census_us, workclass)
View(countwrkcls)


#h)	Calculate the mean of "capital.gain" column grouped according to "workclass".
table(census_us$capital.gain)
mean(census_us$capital.gain)
View(census_us[c("capital.gain", "workclass")])

tapply(censusdata$capital.gain, censusdata$workclass, mean)->c
View(c)
#or
summarise(group_by(censusdata,workclass), mean(capital.gain))->B
View(B)
#or
censusdata %>% group_by(workclass) %>% summarise(mean(capital.gain))->A
View(A)


#3.Data Visualization:
# a)	Build a bar-plot for the "relationship" column and fill the bars according to the "race"
#column.
library(ggplot2)
ggplot(censusdata, aes(x=relationship, fill=race))+
  geom_bar(position = "dodge")

#i.	Set x-axis label to 'Categories of Relationships'
#ii.	Set y-axis label to 'Count of Categories'
ggplot(censusdata, aes(x=relationship, fill=race))+
  geom_bar(position = "dodge")+
labs(x="Categories of Relationships", y="Count of Categories")

#iii.	Fill the bars according to "sex"
#iv.	Set the position of the bars to "dodge"
ggplot(censusdata, aes(x=relationship, fill=sex))+
  geom_bar(position = "dodge")+
  labs(x="Categories of Relationships", y="Count of Categories")

#v.	Set the title of plot to be 'Distribution of Relationships by Sex"
ggplot(censusdata, aes(x=relationship, fill=sex))+
  geom_bar(position = "dodge")+
  labs(x="Categories of Relationships", y="Count of Categories", title = "Distribution of Relationships by Sex")


#b)	Build a Histogram for the "age" column with number of bins equal to 50.
ggplot(censusdata, aes(x=age, fill=sex))+
  geom_histogram(bins = 90)
  
#i)	Fill the bars of the histogram according to yearly income column i.e., "X"
ggplot(censusdata, aes(x=age, fill=X))+
  geom_histogram(bins = 90)

#ii)	Set the title of the plot to "Distribution of Age". 
ggplot(censusdata, aes(x=age, fill=X))+
  geom_histogram(bins = 90)+
  labs(title = "Distribution of Age")

#iii)Set the legend title to "Yearly income".
ggplot(censusdata, aes(x=age, fill=X))+
  geom_histogram(bins = 90)+
  labs(title = "Distribution of Age", fill="Yearly income")


#iv) Set the theme of the plot to black and white.
ggplot(censusdata, aes(x=age, fill=X))+
  geom_histogram(bins = 90)+
  labs(title = "Distribution of Age", fill="Yearly income")+theme_dark()


#c)	Build a scatter-plot between "capital.gain" and "hours.per.week". Map "capital.gain" on the x- axis and "hours.per.week" on the y-axis.
ggplot(censusdata,aes(x=capital.gain, y=hours.per.week))+geom_point()

#i)	Set the transparency of the points to 40% and size as 2.
ggplot(censusdata,aes(x=capital.gain, y=hours.per.week))+geom_point(size=3, alpha=0.6)

#ii)	Set the color of the points according to the "X" (yearly income) column.
ggplot(censusdata,aes(x=capital.gain, y=hours.per.week, col=X))+geom_point(size=3, alpha=0.6)

#iii)Set the x-axis label to "Capital Gain", y-axis label to "Hours per Week", title
#to "Capital Gain vs Hours per Week by Income", and legend label to "Yearly Income".
ggplot(censusdata,aes(x=capital.gain, y=hours.per.week, col=X))+
  geom_point(size=3, alpha=0.6)+
  labs(x="Capital Gain",y="Hours per Week",title="Capital Gain vs Hours per Week by Income",col="Yearly Income")



#d)	Build a box-plot between "education" and "age" column.Map "education" on the x-axis and
#"age" on the y-axis.
ggplot(censusdata,aes(x=education, y=age))+geom_boxplot()

#i)	Fill the box-plots according to the "sex" column.
ggplot(censusdata,aes(x=education, y=age, fill=sex))+geom_boxplot()


#ii)	Set the title to "Box-Plot of age by Education and Sex".
ggplot(censusdata,aes(x=education, y=age, fill=sex))+
  geom_boxplot()+labs(title="Box-Plot of age by Education and Sex")

library(plotly)
plot_ly(censusdata, x=~capital.gain, y=~hours.per.week, color=~X, type="scatter")

#4.Linear Regression:
#a)Build a simple linear regression model as follows:
#i)	Divide the dataset into training and test sets in 70:30 ratio.
#ii)	Build a linear model on the test set where the dependent variable is
#"hours.per.week" and independent variable is "education.num".
#iii)	Predict the values on the train set and find the error in prediction.
#iv)Find the root-mean-square error (RMSE).
set.seed(1)
library(caTools)
sample.split(censusdata$hours.per.week, SplitRatio = 0.7)->split_data
View(split_data)
subset(censusdata, split_data==T)->census_train
subset(censusdata, split_data==F)->census_test
View(census_train)
View(census_test)
lm(hours.per.week~education.num, data = census_train )->model
summary(model)
predict(model, newdata=census_test)->Predicted_value
View(Predicted_value)
cbind(Actual=census_test$hours.per.week, Predicted=Predicted_value)->Result
class(Result)
Result=as.data.frame(Result)
View(Result)
error<-Result$Actual-Result$Predicted
View(error)
cbind(Result,error)->final_data
View(final_data)
sqrt(mean((error)^2))->RMSE
sqrt(mean((final_data1$error)^2))->rmse
View(RMSE)
#RMSE is 11.82266 which is good its near to zero


#5.	Logistic Regression:
#a)	Build a simple logistic regression model as follows:
#i)	Divide the dataset into training and test sets in 65:35 ratio.
set.seed(2)
#convert Dependent and Independent variable into Factor type
censusdata$X<-as.factor(censusdata$X)
censusdata$occupation<-as.factor(censusdata$occupation)
sample.split(censusdata$X, SplitRatio =0.65)->splitdata
View(splitdata)
subset(censusdata, splitdata==T)->train
subset(censusdata, splitdata==F)->test
View(train)
View(test)

#ii)	Build a logistic regression model where the dependent variable is "X"(yearly income) and independent variable is "occupation".
glm(X~occupation, data=train, family="binomial")->glm_model

#iii)	Predict the values on the test set.
predict(glm_model, newdata=test, type = "response")->predicted_value1
head(predicted_value1)
View(predicted_value1)
range(predicted_value1)

#iv)Plot accuracy vs cut-off and pick an ideal value for cut-off.
#install.packages("ROCR")
library(ROCR)
prediction(predicted_value1, test$X)->pred_log_roc
pred_log_roc
performance(pred_log_roc, "acc")->acc
plot(acc) ##check for which value accuracy get constant
ifelse(predicted_value1>0.47,">50k","<=50k")->lm.pred

#v)Build a confusion matrix and find the accuracy.
tab<-table(lm.pred, test$X)
tab

#TN TP - Correctly predicted #7214 & 678 Diagonal value 
#FP FN - Wrongly predicted #1950 & 715 Diagonal value

#Accuracy formula (TN+TP)/(TN+TP+FP+FN)
#(7214+678)/(7214+678+1950+715)
#diag is diagonal value in tab
accuracy<-sum(diag(tab))/sum(tab)
#accuracy we got 0.7475609
accuracy

#vi)Plot the ROC curve and find the auc(Area Under Curve).
roc_curve<-performance(pred_log_roc, "tpr", "fpr")
plot(roc_curve)
auc<-performance(pred_log_roc, "auc")
auc<-auc@y.values[[1]]
auc
plot(auc)

#b)Build a multiple logistic regression model as follows:
#i)	Divide the dataset into training and test sets in 80:20 ratio.
set.seed(3)
sample.split(censusdata$X, SplitRatio = 0.8)->split_Multi_data
View(split_Mult_data)
subset(censusdata, split_Mult_data==T)->multi_train
subset(censusdata, split_Mult_data==F)->multi_test
View(multi_train)
View(multi_test)

#ii)	Build a logistic regression model where the dependent variable is "X"(yearly income) and independent variables are "age", "workclass", and "education".
glm(X~age+workclass+education, data = multi_train, family="binomial")->multi_model
summary(multi_model)

#iii)	Predict the values on the test set.
predict(multi_model, newdata=multi_test, type="response")->multi_predict
summary(multi_predict)
head(multi_predict)
range(multi_predict)

#iv)	Plot accuracy vs cut-off and pick an ideal value for cut-off.
#install.packages("ROCR")
#library(ROCR)
prediction(multi_predict, multi_test$X)->pred_multilog_roc
pred_multilog_roc
performance(pred_multilog_roc, "acc")->acc
plot(acc) ##check for which value accuracy get constant
ifelse(multi_predict>0.5,">50k","<=50k")->multi_lm.pred
multi_lm.pred


#v)Build a confusion matrix and find the accuracy.
multi_tab<-table(multi_lm.pred, multi_test$X)
multi_tab

#TN TP - Correctly predicted #4214 & 511 Diagonal value 
#FP FN - Wrongly predicted #317 & 991 Diagonal value

#Accuracy formula (TN+TP)/(TN+TP+FP+FN)
#(4284 +445)/(4284 +445+247 +1057)
#diag is diagonal value in tab
Multi_accuracy<-sum(diag(multi_tab))/sum(multi_tab)
#accuracy we got 0.7838555
Multi_accuracy

#0.965 - 0.7512017
#0.5 - 0.7838555 giving more accuracy

#vi)Plot the ROC curve and calculate the auc(Area Under Curve).
multi_roc_curve<-performance(pred_multilog_roc, "tpr", "fpr")
plot(multi_roc_curve)
multi_auc<-performance(pred_multilog_roc, "auc")
multi_auc<-multi_auc@y.values[[1]]
multi_auc
plot(multi_auc)
#multi_auc - 0.779626

#6.Decision Tree:
  
#a)Build a decision tree model as follows:
#i)Divide the dataset into training and test sets in 70:30 ratio.
set.seed(4)
library(caTools)
sample.split(censusdata$X, SplitRatio = 0.7)->dec_split_data
View(dec_split_data)
subset(censusdata, dec_split_data==T)->dec_train
subset(censusdata, dec_split_data==F)->dec_test
nrow(dec_train)
nrow(dec_test)
View(dec_train)
View(dec_test)

#ii)	Build a decision tree model where the dependent variable is "X"(Yearly Income) and the rest of the variables as independent variables.
#install.packages("rpart")
#install.packages("rpart.plot")
#install.packages("tree")

library(rpart)
library(rpart.plot)
library(tree)

tree(X~., data=dec_train)->dec_treemodel
#or
rpart(formula = X~., data=dec_train, method = "class")->dec_rpartmodel

#iii)	Plot the decision tree.
#https://www.rdocumentation.org/packages/rpart.plot/versions/3.0.9/topics/rpart.plot
rpart.plot(x=dec_rpartmodel, type=5 , extra=0, tweak=1.2)

#iv)	Predict the values on the test set.
predict(object = dec_rpartmodel, newdata = dec_test, type="class")->predict_dec_tree

#v)	Build a confusion matrix and calculate the accuracy.
table(predict_dec_tree, dec_test$X)->confmat_dec_tree
confmat_dec_tree

#accuracy
sum(diag(confmat_dec_tree))/sum(confmat_dec_tree)->accu_dec_tree
accu_dec_tree

#7.Random Forest:
#a)	Build a random forest model as follows:
#i)	Divide the dataset into training and test sets in 80:20 ratio.
set.seed(5)
library(caTools)
sample.split(censusdata$X, SplitRatio = 0.8)->ran_split_data
View(ran_split_data)
subset(censusdata, ran_split_data==T)->ran_train
subset(censusdata, ran_split_data==F)->ran_test
nrow(ran_train)
nrow(ran_test)
View(ran_train)
View(ran_test)

#ii)	Build a random forest model where the dependent variable is "X"(Yearly Income) and the rest of the variables as independent variables and number of trees as 300.
install.packages("randomForest")
library(randomForest)
randomForest(X~., data = ran_train, ntree = 300 )->ran_model
importance(ran_model)
varImpPlot(ran_model)

#iii)	Predict values on the test set
predict(ran_model, newdata = ran_test, type = "class" )->ran_predict

#iv)	Build a confusion matrix and calculate the accuracy
table(ran_predict, ran_test$X)->ran_confmat_tab
ran_confmat_tab

#accuracy
sum(diag(ran_confmat_tab)/sum(ran_confmat_tab))->ran_acc_forest
ran_acc_forest
