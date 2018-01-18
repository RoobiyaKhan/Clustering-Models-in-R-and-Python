# Hierarchical Clustering

# Importing the dataset
dataset = read.csv('Mall_Customers.csv')
X = dataset[4:5]

# Using the dendrogram to find the optimal number of clusters
dendrogram = hclust(dist(X, method = 'euclidean'), method = 'ward.D')
                  #euclidean dist matrix for data X
plot(dendrogram,
     main = paste('Dendrogram'),
     xlab = 'Customers',
     ylab = 'Euclidean distances')

# Fitting Hierarchical Clustering to the dataset
hc = hclust(dist(X, method = 'euclidean'), method = 'ward.D')
y_hc = cutree(hc, 5) #Vector of clusters which each customer belong to
y_hc

# Visualising the clusters  
library(cluster)
clusplot(X,
         y_hc,
         lines = 0,
         shade = TRUE,
         color = TRUE,
         labels= 2,
         plotchar = FALSE,
         span = TRUE,
         main = paste('Clusters of customers'),
         xlab = 'Annual Income',
         ylab = 'Spending Score')

# Visualising the clusters 
plot(x=X[,1], y= X[,2], col=y_hc, pch=19, 
     xlim=c(from=min(dataset[,1]), to=max(dataset[,1]+30)),
     xlab="Annual Income", ylab="Spending Score")
clusters=c("Sensible", "Careless", "Standard", "Target", "Careful")
legend('bottomright', inset=0.01, legend=clusters, col=1:5, pch=19, horiz=F)
