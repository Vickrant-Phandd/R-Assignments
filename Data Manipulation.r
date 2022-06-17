
getwd()
setwd("E:/Intellipaat/Assignments")
read.csv('E:/Intellipaat/Assignments/customer_churn.csv')->customer_churn
View(customer_churn)
library(dplyr)
table(customer_churn)
#Questions on select() function:
  
# 1.	Extract these individual columns:
#a.	Extract the 5th column & store it in 'customer_5'
customer_5=select(customer_churn,5)
View(customer_5)

#b.	Extract the 15th column & store it in 'customer_15'
customer_15=select(customer_churn,15)
View(customer_15)

#2.	Extract the column numbers 3,6,9,12,15 & 18 and store the result in 'customer_3_multiple'
customer_3_multiple=select(customer_churn,3,6,9,12,15,18)
View(customer_3_multiple)


#3.	Extract all the columns from column number-10 to column number-20 and store the result in 'c_10_20'
c_10_20=select(customer_churn, 10:20)
View(c_10_20)

#4.	Extract all the columns which start with letter 'P' & store it in 'customer_P'
customer_P=select(customer_churn,starts_with("p"))
View(customer_P)

#5.	Extract all the columns which end with letter 's' & store it in 'customer_s'
customer_s=select(customer_churn,ends_with("s"))
View(customer_s)


#Questions on filter() function:
  
# 1.	Extract all the customers whose Internet Service is "DSL" & store the result in 'customer_dsl'
table(customer_churn$InternetService)
customer_dsl=filter(customer_churn,InternetService=="DSL")
View(customer_dsl)

#2.	Extract all the customers whose Contract type is 'Month-to-month' & store the result in 'customer_month'
table(customer_churn$Contract)
customer_month=filter(customer_churn,Contract=="Month-to-month")
View(customer_month)

#3.	Extract all the male senior citizens whose Payment Method is Electronic check & store the result in 'senior_male_electronic'
table(customer_churn$SeniorCitizen)
table(customer_churn$gender)
table(customer_churn$PaymentMethod)

senior_male_electronic = filter(customer_churn, gender=="Male" & SeniorCitizen=="1" & PaymentMethod=="Electronic check")
View(senior_male_electronic)


#4.	Extract all those customers whose tenure is greater than 70 months or their Total charges is more than 8000$ & store the result in 'customer_total_tenure'
names(customer_churn)
customer_total_tenure = filter(customer_churn, tenure>70 | TotalCharges>8000)
View(customer_total_tenure)

#5.	Extract all the customers whose Contract is of two years, payment method is Mailed check & the value of Churn is 'Yes' & store the result in 'two_mail_yes'
table(customer_churn$PaymentMethod)
table(customer_churn$Contract)
table(customer_churn$Churn)
two_mail_yes = filter(customer_churn, Contract=="Two year" & PaymentMethod=="Mailed check" , Churn=="Yes" )
View(two_mail_yes)

#Questions on sample_n(), sample_frac() & count():
# 1.	Extract 333 random records from the customer_churn dataframe & store the result in 'customer_333'
customer_333 = sample_n(customer_churn, 333)
View(customer_333)

#2.	Extract 1000 random records from the customer_churn dataframe & store the result in 'customer_1000'
customer_1000 = sample_n(customer_churn, 1000)
View(customer_1000)

#3.	Randomly extract 23% of the records from the customer_churn dataframe & store the result in 'customer_23_percent'
customer_23_percent = sample_frac(customer_churn, 0.23)
View(customer_23_percent)

#4.	Get the count of different levels from the 'PaymentMethod' column
customer_churn%>%count(PaymentMethod)
count(customer_churn,PaymentMethod)
table(customer_churn$PaymentMethod)

#5.	Get the count of different levels from the 'Churn' column
count(customer_churn,Churn)

#Questions on summarise() & group_by():
#1.	Get the median, variance & standard deviation for the 'tenure' column
summarise(customer_churn, mean(tenure), median(tenure), sd(tenure))

#2.	Get the median, variance & standard deviation for the 'MonthlyCharges' column
summarise(customer_churn, mean(MonthlyCharges), median(MonthlyCharges), sd(MonthlyCharges))

#3.	Get the standard deviation of 'tenure' & group it w.r.t 'PaymentMethod' column
summarise(group_by(customer_churn, PaymentMethod), sd(tenure))

#4.	Get the median of 'MonthlyCharges' & group it w.r.t 'Contract' column
summarise(group_by(customer_churn,Contract ), median(MonthlyCharges))

#5.	Get the variance of 'TotalCharges' & group it w.r.t 'InternetService' column
summarise(group_by(customer_churn,InternetService ), var(TotalCharges, na.rm=T))

