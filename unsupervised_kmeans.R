#Questions on k-means:
#1.	Building the k-means clustering algorithm:
read.csv("customer_churn.csv")->customer_churn

#a.	Start off by extracting the 'MonthlyCharges', 'tenure' & 'TotalCharges' columns from the 'customer_churn' data.frame. Store the result in 'customer_features'
customer_churn[,c("MonthlyCharges", "tenure", "TotalCharges")]->customer_features
View(customer_features)

#b.	Remove any 'NA' values from 'customer_features' if present
sum(is.na(customer_features))
colSums(is.na(customer_features))
na.omit(customer_features)->customer_features

#c.	Build the kmeans algorithm on top of 'customer_features'. Here, the number of clusters should be 3
kmeans(customer_features,3)->kmeans_cluster
View(kmeans_cluster$cluster)
kmeans_cluster$centers


#d.	Bind the clustering vector to 'customer_features'.
cbind(customer_features, cluster=kmeans_cluster$cluster)->features
View(features)

#e.	Extract observations belonging to individual clusters 
kmeans_cluster$size
library(dplyr)
filter(features, cluster==1)->cluster1
View(cluster1)
filter(features, cluster==2)->cluster2
View(cluster2)
filter(features, cluster==3)->cluster3
View(cluster3)
###############################################################################
#Apply Kmeans columns view
#MonthlyCharges column#
kmeans(customer_features$MonthlyCharges,3)-> k_month
cbind(Month=customer_features$MonthlyCharges, Clusters=k_month$cluster) ->month_group
head(month_group)
View(month_group)

as.data.frame(month_group)->month_group
month_group %>% filter(Clusters==1)-> month_group_1
month_group %>% filter(Clusters==2)-> month_group_2
month_group %>% filter(Clusters==3)-> month_group_3

head(month_group_1)
head(month_group_2)
head(month_group_3)

#tenure_group column#
kmeans(customer_features$tenure,3)->tenure_group
cbind(Tenure=customer_features$tenure, Clusters=tenure_group$cluster) ->tenure_group_data
head(tenure_group_data)

as.data.frame(tenure_group_data)->tenure_group_data
tenure_group_data %>% filter(Clusters==1)-> tenure_group_data1
tenure_group_data %>% filter(Clusters==2)-> tenure_group_data2
tenure_group_data %>% filter(Clusters==3)-> tenure_group_data3

head(tenure_group_data1)
head(tenure_group_data2)
head(tenure_group_data3)

#TotalCharges column#
na.omit(customer_features)->customer_features 
kmeans(customer_features$TotalCharges,3)->k_total

cbind(Total=customer_features$TotalCharges, Clusters=k_total$cluster) ->total_group
head(total_group)

as.data.frame(total_group)->total_group
total_group %>% filter(Clusters==1)-> total_group1
total_group %>% filter(Clusters==2)-> total_group2
total_group %>% filter(Clusters==3)-> total_group3

head(total_group1)
head(total_group2)
head(total_group3)


