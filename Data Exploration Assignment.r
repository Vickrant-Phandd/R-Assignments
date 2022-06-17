getwd()
setwd("E:/Intellipaat/Assignments")
read.csv('E:/Intellipaat/Assignments/customer_churn.csv')->customer_churn


#1.Extract these individual columns with the '$' symbol:
#a.	Extract 'InternetService' column and store it in 'customer_internet_service'
customer_churn$InternetService->customer_internet_service
View(customer_internet_service)

#b.	Extract 'PaperlessBilling' column and store it in 'customer_Paperless_Billing'
customer_churn$PaperlessBilling->customer_Paperless_Billing
View(customer_Paperless_Billing)

#c.	Extract 'StreamingTV' column and store it in 'customer_Streaming_TV'
customer_churn$StreamingTV->customer_Streaming_TV
View(customer_Streaming_TV)

#2.	Extract the 3rd, 6th and 9th columns from the 'customer_churn' data.frame & store it in 'customer_random_columns'
customer_random_columns=customer_churn[,c(3,6,9)]
customer_churn[,c(3,6,9)]

library(dplyr)
View(customer_churn)
customer_random_columns=select(customer_churn, 3,6,9)
View(customer_random_columns)

#3.	Extract all the columns from column number-10 to column-number 20 and store the result in 'customer_10_20'
customer_10_20=customer_churn[10:20,]
customer_churn[10:20]


#4.	Extract only these row numbers: 65, 765, 3726 & 7000 and store the result in 'customer_random_rows'
customer_random_rows=customer_churn[c(65,765,3726,7000),]
customer_random_rows

#5.	Extract all the rows starting from row number-655 to row number-6550 & store the result in 'customer_continuous_rows'
customer_continuous_rows=customer_churn[c(655:6550),]
View(customer_continuous_rows)

#6.	Extract row numbers- 10, 100 & 1000 & column numbers- 5, 10, 15 & store the result in 'customer_random_data'
customer_random_data=customer_churn[c(10,100,100), c(5,10,15)]
View(customer_random_data)

#Questions on Flow Control Statements:
#1.Check if the value in the 6th cell of 'PaymentMethod' column is 'Electronic check'. If yes, print "Yes, the payment method is Electronic check"
customer_churn$PaymentMethod[6]

if(customer_churn$PaymentMethod[6]=="Electronic check"){
  print('Yes, the payment method is Electronic check')
}

#2.Check the value present in 12th cell of 'Contract' column. 
#If it's 'month-to-month', print 'The contract is on a month to month basis'
#If it's 'One year', print 'The contract is on a yearly basis'
#If it's 'Two year', print 'The contract is on a two-year basis'
customer_churn$Contract[12]
if(customer_churn$Contract[12]=="month-to-month"){
  print('The contract is on a month to month basis')
}else if(customer_churn$Contract[12]=="One year"){
  print("The contract is on a yearly basis")
}else{
  print("The contract is on a two-year basis")
}


#3.Use switch to check the gender in 6th cell of 'gender' column.
#If it's 'Male', give a discount of 20% in 'MonthlyCharges'
#If it's 'Female', give a discount of 50% in 'MonthlyCharges'
class(customer_churn$gender)

customer_churn$gender[6]

#code using if else function
if(customer_churn$gender[6]=="Male"){
  print(customer_churn$MonthlyCharges[6]*0.8)
}else if(customer_churn$gender[6]=="Female"){
  print(customer_churn$MonthlyCharges[6]*0.5)
}

#code using switch function
customer_churn$MonthlyCharges[6]
switch(customer_churn$gender[6],
      "Male"=customer_churn$MonthlyCharges[6]*0.8,
      "Female"=customer_churn$MonthlyCharges[6]*0.5)

#4.Use for loop to get the count of customers whose 'InternetService' is 'DSL'
#i is the travel variable which will go from 1 record to last record that is nrow
#if is the statement here
count=0

for(i in 1:nrow(customer_churn)){
  if(customer_churn$InternetService[i]=="DSL"){
    count=count+1
  }
}
count



#5.Use while to find the number of customers whose tenure is exactly '2' months

count_number=0
row=1
while(row<=nrow(customer_churn)){
  if(customer_churn$tenure[row]==2){
    count_number=count_number+1
  }
  row=row+1
}
count_number

#Questions on Inbuilt Functions:
  
#1.	Do these operations with the head() function:
#a.	Get the first 4 records from 'PhoneService' column
head(customer_churn$PhoneService, 4)

#b.	Get the first 8 records from 'Contract' column
head(customer_churn$Contract, 8)


#2.	Do these operations with the tail() function:
#a.	Get the last record of 'TotalCharges' column
tail(customer_churn$TotalCharges,1)

#b.	Get the last 5 records of 'tenure' column
tail(customer_churn$tenure,5)

#3.	Find the average, minimum, maximum & range from the 'tenure' column
summary(customer_churn$tenure)
mean(customer_churn$tenure)
min(customer_churn$tenure)
max(customer_churn$tenure)
range(customer_churn$tenure)
#4.	Get 10 random values from the 'TotalCharges' column using sample()
sample(customer_churn$TotalCharges,10)

#5.	Find the count of different levels in 'PaymentMethod' & 'Contract' columns using table() 
table(customer_churn$PaymentMethod)
table(customer_churn$Contract)

