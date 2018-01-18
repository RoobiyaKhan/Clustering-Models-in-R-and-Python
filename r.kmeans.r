#K-Means Clustering
                               #K Means is based on distance,so we can use Scaled values as inputs to avoid 
                             #the final cluster output to be biased towards the larger valued attributes in the dataset
#Importing the dataset
dataset = read.csv('Mall_Customers.csv')
X = dataset[4:5] #array X

# Using the elbow method to find the optimal number of clusters
set.seed(6)
wcss <- vector()
for (i in 1:10) wcss[i] <- sum(kmeans(X, i)$withinss) #i clusters
plot(1:10, wcss, type = 'b', main = paste('Clusters of clients'), xlab = 'Number of clusters', ylab = 'WCSS')#b-both(points,lines)

# Fitting K-Means to the dataset
set.seed(29)
kmeans = kmeans(X, 5, iter.max = 300, nstart = 10) #centers = 5
y_kmeans = kmeans$cluster
#dataset$cluster_number <- y_kmeans 

# Visualising the clusters (scaled by k-means)
#install.packages('cluster')
library(cluster)
clusplot(X,
         kmeans$cluster,
         line = 0,
         shade = TRUE,
         color = TRUE,
         labels = 2,
         plotchar = FALSE,
         span = TRUE,
         main = paste('Clusters of Clients'),
         xlab = 'Annual Income',
         ylab = 'Spending Score')

#to visualise the clusters without scaling factor(actual clusters)
plot(x= X[,1], y= X[,2], col= y_kmeans, pch=19, 
     xlim=c(from=min(dataset[,1]), to=max(dataset[,1]+30)),
     xlab="Annual Income", ylab="Spending Score")
clusters=c("Careless", "Standard", "Sensible", "Target", "Careful")
legend('bottomright', legend=clusters, col=1:5, pch=19, horiz=F)