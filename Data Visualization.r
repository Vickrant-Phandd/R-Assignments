getwd()
read.csv("customer_churn.csv")->customer_churn.csv

#code for Mode use DescTools package
#install.packages("DescTools")
library(DescTools)
Mode(customer_churn$tenure)
Mode(customer_churn$TotalCharges, na.rm = T)
Mode(customer_churn$MonthlyCharges)

#install.packages("ggplot2")
library(ggplot2)
##############################geom_bar#############################################################
#Questions on geom_bar():
#1.	Build a bar-plot for the 'PhoneService' column
#a.	Assign the fill color to be 'pink'
#b.	Assign the boundary color to be 'peru'
ggplot(data = customer_churn, aes(x=PhoneService))+geom_bar(fill="Pink", col="peru")


#2.	Build a bar-plot for the 'InternetService' column
#a.	Assign 'InternetService' to the fill aesthetic
##if we give fill in aes then column name you have to use
ggplot(data = customer_churn, aes(x=InternetService, fill=InternetService))+geom_bar()


#b.	Assign 'Contract' to the fill aesthetic
ggplot(data = customer_churn, aes(x=InternetService, fill=Contract))+geom_bar()


#c.	Change the position of bars to 'identity'
ggplot(data = customer_churn, aes(x=InternetService, fill=Contract))+
  geom_bar(position = "identity")

#dodge#
ggplot(data = customer_churn, aes(x=InternetService, fill=Contract))+
  geom_bar(position = "dodge")

#stack is default#
ggplot(data = customer_churn, aes(x=InternetService, fill=Contract))+
  geom_bar(position = "stack")

#3.	Build a bar-plot for 'TechSupport' column
#a.	Assign 'Churn' to the fill aesthetic
ggplot(customer_churn, aes(x=TechSupport, fill=Churn))+
  geom_bar(position = "dodge")

###############################geom_histogram################################################
#Questions on geom_histogram():
#1.	Build a histogram for the 'tenure' column
ggplot(customer_churn, aes(x=tenure))+
  geom_bar(position = "dodge")

#a.	Assign the fill color to be 'mediumspringgreen'
ggplot(customer_churn, aes(x=tenure))+
  geom_bar(fill = "mediumspringgreen", position = "dodge")


#b.	Assign the boundary color to be 'azure'
ggplot(customer_churn, aes(x=tenure))+
  geom_bar(fill = "mediumspringgreen", col="azure", position = "dodge")

#c.	Change the number of bins to be 100
ggplot(customer_churn, aes(x=tenure))+
  geom_histogram(fill = "mediumspringgreen", col="azure",bins="5")

#2.	Build histogram for the 'MonthlyCharges' column
#a.	Assign 'PaymentMethod' to the fill aesthetic
ggplot(data = customer_churn, aes(x=MonthlyCharges, fill=PaymentMethod))+
  geom_histogram(position = "dodge")

#b.	Assign 'OnlineBackup' to the fill aesthetic
ggplot(customer_churn, aes(x=MonthlyCharges, fill=OnlineBackup))+
  geom_histogram(position = "dodge")

#3.	Build histogram for the 'TotalCharges' column
#a.	Assign 'gender' to the fill aesthetic
ggplot(customer_churn,aes(TotalCharges, fill=gender))+
  geom_histogram(position = "dodge")

#b.	Assign 'InternetService' to the fill aesthetic
ggplot(customer_churn,aes(TotalCharges, fill=InternetService))+
  geom_histogram(position = "dodge")

################################geom_point#############################################
#Questions on geom_point():
#1.	Build a scatter-plot between 'TotalCharges' & 'tenure'. Map 'TotalCharges' to the y-axis & 'tenure' to the 'x-axis'
#a.	Assign it the color 'wheat3'
ggplot(customer_churn,aes(y=TotalCharges, x=tenure))+
  geom_point(col="wheat3")

#b.	Use 'col' as an aesthetic and Map 'PaymentMethod' to col
ggplot(customer_churn,aes(y=TotalCharges, x=tenure, col=PaymentMethod))+
  geom_point()

#c.	Use 'col' as an aesthetic and Map 'gender' to col
ggplot(customer_churn,aes(y=TotalCharges, x=tenure, col=gender))+
  geom_point()

#d.	Map 'Dependents' to both 'col' & 'shape' aesthetics
ggplot(customer_churn,aes(y=TotalCharges, x=tenure, col=Dependents, shape=Dependents))+
  geom_point()

#2.	Build a scatter-plot between 'tenure' & 'MonthlyCharges'. Map 'tenure' to the y-axis & 'MonthlyCharges' to the 'x-axis'
#a.	Assign it the color 'yellowgreen'
ggplot(customer_churn,aes(y=tenure, x=MonthlyCharges))+
  geom_point(col="yellowgreen")

#b.	Use 'col' as an aesthetic and Map 'InternetService' to col
ggplot(customer_churn,aes(y=tenure, x=MonthlyCharges, col=InternetService))+
  geom_point()

#c.	Use 'col' as an aesthetic and Map 'Contract' to col
ggplot(customer_churn,aes(y=tenure, x=MonthlyCharges, col=Contract))+
  geom_point()

####################################plotly#####################################
install.packages("plotly")
library(plotly)
plot_ly(customer_churn, x=~MonthlyCharges, y=~tenure, color = ~Dependents, type="scatter")

##############################geom_boxplot##############################################################
#Questions on geom_boxplot:
#1.	Build a box-plot between 'tenure' & 'Partner'. Map 'tenure' to the y-axis & 'Partner' to the 'x-axis'
#a.	Assign it a fill color of 'violet'
ggplot(customer_churn,aes(y=tenure, x=Partner))+geom_boxplot(fill="violet")
  

#b.	Assign it a boundary color of 'snow3'
ggplot(customer_churn,aes(y=tenure, x=Partner))+geom_boxplot(fill="violet", col="snow3")

#2.	Build a box-plot between 'tenure' & 'MultipleLines'. Map 'tenure' to the y-axis & 'MultipleLines' to the 'x-axis'
#a.	Assign 'Partner' to the fill aesthetic
ggplot(customer_churn,aes(y=tenure, x=MultipleLines, fill=Partner))+geom_boxplot()

#b.	Assign 'PhoneService' to the fill aesthetic
ggplot(customer_churn,aes(y=tenure, x=MultipleLines, fill=PhoneService))+geom_boxplot()


#3.	Build a box-plot between 'tenure' & 'Contract'
#a.	Assign 'InternetService' to the fill aesthetic
ggplot(customer_churn,aes(y=tenure, x=MultipleLines, fill=InternetService))+geom_boxplot()


#b.	Assign 'PaymentMethod' to the fill aesthetic
ggplot(customer_churn,aes(y=tenure, x=MultipleLines, fill=PaymentMethod))+geom_boxplot()

################################facet_grid#########################################################
#Questions on facet_grid():
#1.	Build a box-plot between 'tenure' & 'MultipleLines'. Map 'tenure' on the y-axis & 'MultipleLines' on the x-axis. Map 'InternetService' to the fill aesthetic
#a.	Facet the plot w.r.t 'InternetService' column
ggplot(customer_churn,aes(y=tenure, x=MultipleLines, fill=InternetService))+
  geom_boxplot()+facet_grid(~InternetService)
 

#2.	Build a scatter-plot between 'TotalCharges' & 'tenure'. Map 'TotalCharges' on the y-axis & 'tenure' on the x-axis. Map 'Contract' onto col aesthetic
#a.	Facet the plot w.r.t 'Contract' column
ggplot(customer_churn,aes(y=TotalCharges, x=tenure, col=Contract))+
  geom_point()+facet_grid(~Contract)

#3.	Build a histogram for the 'MonthlyCharges' column. Map 'PaymentMethod' onto fill aesthetic.
#a.	Facet the plot w.r.t 'PaymentMethod' column
ggplot(customer_churn,aes(x=MonthlyCharges, fill=PaymentMethod))+
  geom_histogram()+facet_grid(~PaymentMethod)

#####################################theme() layer#############################################################
#Questions on theme() layer:
#1.	Build a bar-plot for the 'gender' column. Give it a fill color of 'blue4'
#a.	Give the panel a background color of 'chartreuse4'
ggplot(data = customer_churn, aes(x=gender))+
  geom_bar(fill="blue4")+theme(panel.background = element_rect(fill="chartreuse4") )

#b.	Give the plot a background color of 'cornsilk4'
ggplot(data = customer_churn, aes(x=gender))+
  geom_bar(fill="blue4")+theme(panel.background = element_rect(fill="cornsilk4") )


#2.	Build a scatter-plot between 'tenure' & 'MonthlyCharges'. Map 'tenure' on the y-axis & 'MonthlyCharges' on the x-axis. Assign a color of 'yellowgreen' to the points.
#a.	Add a title to the plot 'Tenure vs Monthly Charges'
ggplot(data = customer_churn,aes(y=tenure,x=MonthlyCharges))+
  geom_point(col="yellowgreen")+labs(title="Tenure vs Monthly Charges")
  

#b.	Give the panel a background color of 'coral2'
ggplot(data = customer_churn,aes(y=tenure,x=MonthlyCharges))+
  geom_point(col="yellowgreen")+labs(title="Tenure vs Monthly Charges")+
  theme(panel.background = element_rect(fill="coral2"))

#c.	Give the plot a background color of 'beige'
ggplot(data = customer_churn,aes(y=tenure,x=MonthlyCharges))+
  geom_point(col="yellowgreen")+labs(title="Tenure vs Monthly Charges")+
  theme(panel.background = element_rect(fill="coral2"))+
  theme(plot.background = element_rect(fill="beige"))

#d.	Center align the title & make the color of the title to be red
ggplot(data = customer_churn,aes(y=tenure,x=MonthlyCharges))+
  geom_point(col="yellowgreen")+labs(title="Tenure vs Monthly Charges")+
  theme(panel.background = element_rect(fill="coral2"))+
  theme(plot.background = element_rect(fill="beige"))+  
  theme(plot.title = element_text(hjust=0.5,colour="red"))

############################################################################################
#install.packages("plotly")
library(plotly)
plot_ly(data=customer_churn, x = ~MonthlyCharges, y = ~tenure, color = ~Dependents, type='scatter')
customer_churn$Dependents
