rm(list = ls())

getwd()
setwd("E:/Intellipaat/Assignments")
read.csv('E:/Intellipaat/Assignments/customer_churn.csv')->customer_churn

View(customer_churn)
head(customer_churn)
head(customer_churn,5)
tail(customer_churn)
nrow(customer_churn)
ncol(customer_churn)
str(customer_churn)
table(customer_churn$SeniorCitizen)
colnames(customer_churn)
rownames(customer_churn)
sum(is.na(customer_churn))
colSums(is.na(customer_churn))

# Working with arithmetic operators:

#a.	Add 5 to the fifth record of 'MonthlyCharges' column
customer_churn$MonthlyCharges->a
View(a)
customer_churn$MonthlyCharges[5]+5->customer_churn$MonthlyCharges[5]
View(customer_churn$MonthlyCharges)
customer_churn$MonthlyCharges[5]

#b.	Subtract 9.65 from the sixth record of 'MonthlyCharges' column
customer_churn$MonthlyCharges[6]
customer_churn$MonthlyCharges[6]-9.65
customer_churn$MonthlyCharges[6]-9.65->customer_churn$MonthlyCharges[6]
customer_churn$MonthlyCharges[6]

#c.	Multiply the 1st record of 'MonthlyCharges' column with 3
customer_churn$MonthlyCharges[1]
customer_churn$MonthlyCharges[1]*3
customer_churn$MonthlyCharges[1]*3->customer_churn$MonthlyCharges[1]
customer_churn$MonthlyCharges[1]

#d.	Divide the 37th record of 'MonthlyCharges' column with 3
customer_churn$MonthlyCharges[37]
customer_churn$MonthlyCharges[37]/3
customer_churn$MonthlyCharges[37]/3->customer_churn$MonthlyCharges[37]
customer_churn$MonthlyCharges[37]


#2.	Working with the relational operators:

#a.	Check if the value of 'tenure' in the 1st row is greater than the value of 'tenure' in the 10th row
customer_churn$tenure
View(customer_churn$tenure)
customer_churn$tenure[1]>customer_churn$tenure[10]

#b.	Check if the value of 'tenure' in the 3rd row is equal to the value of 'tenure' in the 5th row
customer_churn$tenure[3]==customer_churn$tenure[5]


#3.	Working with logical operators:

#a.	Get the count of those customers who have subscribed to both "TechSupport" & "StreamingTV"
sum(customer_churn$TechSupport=="Yes" & customer_churn$StreamingTV=="Yes")


#b.	Extract those customers whose 'InternetService' is either 'DSL' or 'Fiber optic' & store the result in 'Internet_dsl_fiber'
sum(customer_churn$InternetService=="DSL" | customer_churn$InternetService=="Fiber optic")-> Internet_dsl_fiber
View(Internet_dsl_fiber)
