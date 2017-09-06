setwd("~/Work In Progress/Analytics Edge/Unit 06")
airlines <- read.csv('AirlinesCluster.csv')
# Looking at the summary of airlines, which TWO variables have 
# (on average) the smallest values?
summary(airlines) #BonusTrans FlightTrans

# Which TWO variables have (on average) the largest values?
# Balance BonusMiles

# If we don't normalize the data, the clustering will be 
# dominated by the variables that are on a larger scale. 

library(caret)

preproc <- preProcess(airlines)
airlinesNorm <- predict(preproc, newdata = airlines)
summary(airlinesNorm)

# In the normalized data, which variable has the largest maximum value?
# FlightMiles

# In the normalized data, which variable has the smallest minimum value?
# DaysSinceEnroll


# Compute the distances between data points (using euclidean distance) and then 
# run the Hierarchical clustering algorithm (using method="ward.D") 
# on the normalized data.
airlinesDist <- dist(as.vector(airlinesNorm), method = 'euclidean')
airlineClusters <- hclust(airlinesDist, method = 'ward.D')

# Then, plot the dendrogram of the hierarchical clustering process. 
# Suppose the airline is looking for somewhere between 2 and 10 clusters. 
# According to the dendrogram, which of the following is NOT a good choice 
# for the number of clusters?
plot(airlineClusters) # between 2,3,6,7 - 6 is NOT a good choice

# Suppose that after looking at the dendrogram and discussing with the 
# marketing department, the airline decides to proceed with 5 clusters. 
# Divide the data points into 5 clusters by using the cutree function. 
# How many data points are in Cluster 1?
airlineClusterGroups <- cutree(airlineClusters, k = 5)
sum(airlineClusterGroups == 1) # 776

# compare the average values in each of the variables for the 5 clusters
# How would you describe the customers?
# Cluster 1: Infrequent but loyal customers
# Cluster 2: Customers who have accumulated a large amount of miles, and the ones with the largest number of flight transactions.
# Cluster 3: Customers who have accumulated a large amount of miles, mostly through non-flight transactions. 
# Cluster 4: Relatively new customers who seem to be accumulating miles, mostly through non-flight transactions.
# Cluster 5: Relatively new customers who don't use the airline very often.

for(name in names(airlines)){
  print(name)
  print(tapply(airlines[,name], airlineClusterGroups, mean))
}

# K-Means Clustering
set.seed(88)
airlinesKMeans <- kmeans(as.vector(airlines), centers = 5, iter.max = 1000)

# How many clusters have more than 1,000 observations?
table(airlinesKMeans$cluster)

# Now, compare the cluster centroids to each other 
table(airlineClusterGroups, airlinesKMeans$cluster)
























