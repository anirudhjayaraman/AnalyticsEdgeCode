setwd("~/Work In Progress/Analytics Edge/Unit 06")

dailykos <- read.csv('dailykos.csv')
# this dataset has 3430 rows (observations) and 1545 columns

dailyKosVector <- as.vector(dailykos)
distKos <- dist(dailyKosVector, method = 'euclidean')
dailyKosCluster <- hclust(distKos, method = 'ward.D')

# Plot the dendrogram of your hierarchical clustering model. 
# Just looking at the dendrogram, which of the following seem like good 
# choices for the number of clusters? (among options 2,3,5,6)
plot(dailyKosCluster) # 2 and 3

# Let's pick 7 clusters. This number is reasonable according to the dendrogram, 
# and also seems reasonable for the application. 
# Create 7 new datasets, each containing the observations from one of the clusters.
clusterGroups <- cutree(dailyKosCluster, k = 7)

for(i in 1:7){
  assign(paste0('dataset_',i), subset(dailykos, clusterGroups == i))
}

# Instead of looking at the average value in each variable individually, 
# we'll just look at the top 6 words in each cluster.
# What is the most frequent word in the first cluster, in terms of average value? 

head(sort(colSums(dataset_1), decreasing = T))

# Which words best describe cluster 2?
head(sort(colSums(dataset_2), decreasing = T))

# Which cluster could best be described as the cluster related to the Iraq war?
head(sort(colSums(dataset_3), decreasing = T))
head(sort(colSums(dataset_4), decreasing = T))
head(sort(colSums(dataset_5), decreasing = T)) # Iraq war
head(sort(colSums(dataset_6), decreasing = T))
head(sort(colSums(dataset_7), decreasing = T)) # Democratic Party

# In 2004, one of the candidates for the Democratic nomination for the President 
# of the United States was Howard Dean, John Kerry was the candidate who won 
# the democratic nomination, and John Edwards with the running mate of John Kerry 
# (the Vice President nominee). Given this information, which cluster best 
# corresponds to the democratic party?
head(sort(colSums(dataset_7[1:(ncol(dataset_7)-1)]), decreasing = T))

# Now, run k-means clustering, setting the seed to 1000 right before you run 
# the kmeans function. Again, pick the number of clusters equal to 7. 
# Subset your data into the 7 clusters (7 new datasets) by using the 
# "cluster" variable of your kmeans output.

set.seed(1000)
KMC_dailykos <- kmeans(dailyKosVector, centers = 7)
dailykos$clusterGroups <- NULL

for(i in 1:7){
  assign(paste0('KmeansDataset_',i),subset(dailykos,KMC_dailykos$cluster == i))
}

# Which k-means cluster best corresponds to the Iraq War?
# Which k-means cluster best corresponds to the democratic party? 
head(sort(colSums(KmeansDataset_1), decreasing = T))
head(sort(colSums(KmeansDataset_2), decreasing = T)) # Democratic Party
head(sort(colSums(KmeansDataset_3), decreasing = T)) # Iraq War
head(sort(colSums(KmeansDataset_4), decreasing = T))
head(sort(colSums(KmeansDataset_5), decreasing = T)) 
head(sort(colSums(KmeansDataset_6), decreasing = T))
head(sort(colSums(KmeansDataset_7), decreasing = T))


# Which Hierarchical Cluster best corresponds to K-Means Cluster 2?
head(sort(colSums(KmeansDataset_2), decreasing = T)) # Democratic Party
head(sort(colSums(dataset_7), decreasing = T)) # Democratic Party

# Which Hierarchical Cluster best corresponds to K-Means Cluster 3?
head(sort(colSums(KmeansDataset_3), decreasing = T)) # Iraq War
head(sort(colSums(dataset_5), decreasing = T)) # Iraq war


# Which Hierarchical Cluster best corresponds to K-Means Cluster 7?
head(sort(colSums(KmeansDataset_7), decreasing = T)) # Kerry Bush Poll Campaign Presided
head(sort(colSums(dataset_4), decreasing = T))
table(clusterGroups, KMC_dailykos$cluster) # no more than 123 (39.9%) 
# of the observations in K-Means Cluster 7 fall in any hierarchical cluster

# Which Hierarchical Cluster best corresponds to K-Means Cluster 6?
table(clusterGroups, KMC_dailykos$cluster) # cluster 2



