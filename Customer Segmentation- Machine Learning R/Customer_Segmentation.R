library(tidyverse)
library(purrr)
library(ggthemes)
library(cluster)
theme_set(theme_minimal())
# Read in Data
mall_data <- read.csv('/Users/max/Documents/Data Science/Project_Notebooks/Customer Segmentation/Mall_Customers.csv')

# Data Structure and fist cleaning
colnames(mall_data) <- c("CustomerID", "Gender", "Age", "Annual_Income_dollar_k", "Spending_Score")
mall_data$Gender <-  factor(mall_data$Gender, levels= c("Male", "Female"))
str(mall_data)
head(mall_data)


# Short summaries
summary(mall_data)

# More females in the data

ggplot(mall_data, aes(x= Gender))+
  scale_y_continuous(limits= c(0,120), breaks = seq(from= 5, to= 115, by= 10))+
  scale_x_discrete(labels= c("Male", "Female"))+
  ylab("Amount")+
  theme_minimal()+
  geom_bar(fill= c("dodgerblue3", "tomato3"), width = 0.5)

# Age distribution
ggplot(mall_data, aes(x= Age))+
  geom_histogram(aes(y= ..density..), alpha= 0.5, position= "identity")+
  geom_density(alpha=.2)

ggplot(mall_data, aes(x= Age, fill= Gender, col= Gender)) +
  geom_histogram(aes(y=..density..), alpha=0.5, position="identity")+
  geom_density(alpha=.2) +
  scale_colour_manual(values= c("dodgerblue3", "tomato3"))+
  scale_fill_manual(values= c("dodgerblue3", "tomato3"))+
  facet_grid(mall_data$Gender)

# Annual Income

ggplot(mall_data, aes(x= Annual_Income_dollar_k))+
  labs(x= "Annual Income in US Dollar")+
  geom_histogram(aes(y= ..density..), alpha= 0.5, position= "identity")+
  geom_density(alpha=.2)+
  scale_x_continuous(breaks = seq(from= 0, to= 140, by= 10 ))

#By Gender
ggplot(mall_data, aes(x= Annual_Income_dollar_k, fill= Gender, col= Gender))+
  labs(x= "Annual Income in 1000 US Dollar")+
  geom_histogram(aes(y= ..density..), alpha= 0.5, position= "identity")+
  geom_density(alpha=.2)+
  scale_x_continuous(breaks = seq(from= 0, to= 140, by= 10 ))+
  scale_colour_manual(values= c("dodgerblue3", "tomato3"))+
  scale_fill_manual(values= c("dodgerblue3", "tomato3"))+
  facet_grid(mall_data$Gender)

# Spending Score
ggplot(mall_data, aes(x= Gender, y= Spending_Score))+
  ylab("Spending Score")+
  stat_boxplot(geom='errorbar')+
  scale_y_continuous(breaks= seq(from= 0, to= 100, by= 10))+
  geom_boxplot()

ggplot(mall_data, aes(x= Spending_Score))+
  xlab("Spending Score")+
  geom_histogram(aes(y= ..density..), alpha= 0.5, position= "identity")+
  geom_density(alpha=.2)+
  scale_x_continuous(breaks = seq(from= 0, to= 140, by= 10 ))


ggplot(mall_data, aes(x= Spending_Score, fill= Gender, colour= Gender))+
  xlab("Spending Score")+
  geom_histogram(aes(y= ..density..), alpha= 0.5, position= "identity")+
  geom_density(alpha=.2)+
  scale_x_continuous(breaks = seq(from= 0, to= 140, by= 10 ))+
  scale_colour_manual(values= c("dodgerblue3", "tomato3"))+
  scale_fill_manual(values= c("dodgerblue3", "tomato3"))+
  facet_grid(mall_data$Gender)

# K Means algortihm

#Determining optimal Cluster size
# ELbow Method
set.seed(123)

iss <- function(k){
  kmeans(mall_data[,3:5], k, iter.max= 100, nstart= 100, algorithm = "Lloyd")$tot.withinss
}

k.values <- 1:10

iss_values <- map_dbl(k.values, iss)

df_iss_values <- as.data.frame(iss_values)


ggplot(df_iss_values, aes(x= 1:10,y= iss_values))+
  xlab("Number of clusters")+
  ylab("intra-cluster sum of square")+
  scale_x_continuous(breaks= c(1:10), labels= c(1:10))+
  annotate(geom= "text", x= 4.5, y= 1.25e+05, label= "Optimal number \n of Cluster", size= 2.5)+
  geom_point()

############ 4 Clusters
k4<-kmeans(mall_data[,3:5],4,iter.max=300,nstart=50,algorithm="Lloyd")
k4

pc_clust=prcomp(mall_data[,3:5],scale=FALSE)
summary(pc_clust)
pc_clust$rotation[,1:2]             

set.seed(123)
ggplot(mall_data, aes(x= Annual_Income_dollar_k, y= Spending_Score, colour= factor(k4$cluster)))+
  geom_point(stat = "identity")+
  scale_color_discrete(name= " ", breaks= c("1","2","3","4"),
                       labels =c("Cluster 1","Cluster 2","Cluster 3","Cluster 4"))+
  labs(x= "Spending Score (1-100)", y= "Annual income in 1000 US $")+
  ggtitle("Segments of Mall Customers", subtitle= "K-means Clustering")
 
############ 5 Clusters
k5<-kmeans(mall_data[,3:5],5,iter.max=300,nstart=50,algorithm="Lloyd")
k5

pc_clust=prcomp(mall_data[,3:5],scale=FALSE)
summary(pc_clust)
pc_clust$rotation[,1:2]             

set.seed(123)
ggplot(mall_data, aes(x= Annual_Income_dollar_k, y= Spending_Score, colour= factor(k5$cluster)))+
  geom_point(stat = "identity")+
  scale_color_discrete(name= " ", breaks= c("1","2","3","4","5"),
                       labels =c("Cluster 1","Cluster 2","Cluster 3","Cluster 4", "Cluster 5"))+
  labs(x= "Spending Score (1-100)", y= "Annual income in 1000 US $")+
  ggtitle("Segments of Mall Customers", subtitle= "K-means Clustering")

############ 6 Clusters
k6<-kmeans(mall_data[,3:5],6,iter.max=300,nstart=50,algorithm="Lloyd")
k6

pc_clust=prcomp(mall_data[,3:5],scale=FALSE)
summary(pc_clust)
pc_clust$rotation[,1:2]             

set.seed(123)
ggplot(mall_data, aes(x= Annual_Income_dollar_k, y= Spending_Score, colour= factor(k6$cluster)))+
  geom_point(stat = "identity")+
  scale_color_discrete(name= " ", breaks= c("1","2","3","4","5","6"),
                       labels =c("Cluster 1","Cluster 2","Cluster 3","Cluster 4", "Cluster 5", "Cluster 6"))+
  labs(x= "Spending Score (1-100)", y= "Annual income in 1000 US $")+
  ggtitle("Segments of Mall Customers", subtitle= "K-means Clustering")




clusplot(mall_data,
         k5$cluster,
         lines=0,
         shade=TRUE,
         color= TRUE,
         labels=5,
         plotchar=TRUE,
         span=FALSE,
         main=paste("Segments of Mall Customers"),
         sub= paste("K-means Clustering"),
         xlab="annual incomes",
         ylab="spending score")
