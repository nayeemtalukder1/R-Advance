# Load the iris dataset
data(iris)

# Remove the species column to focus on numeric data
iris_data <- iris[, -5]

# Scale the data
scaled_data <- scale(iris_data)

# Compute the distance matrix
distance_matrix <- dist(scaled_data)

# Perform hierarchical clustering with complete linkage
hc <- hclust(distance_matrix, method = "complete")

# View the clustering object
print(hc)

# Plot the dendrogram
plot(hc, main = "Hierarchical Clustering Dendrogram", xlab = "", sub = "", cex = 0.8)

# Cut the dendrogram into 3 clusters
clusters <- cutree(hc, k = 3)

# Add cluster assignments to the original data
iris$Cluster <- as.factor(clusters)

# View the data with cluster assignments
head(iris)

# Install and load ggplot2 if not already installed
install.packages("ggplot2")
library(ggplot2)

# Visualize clusters using Sepal.Length and Sepal.Width
ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width, color = Cluster)) +
  geom_point(size = 2) +
  labs(title = "Hierarchical Clustering - Iris Dataset", x = "Sepal Length", y = "Sepal Width") +
  theme_minimal()

# Install and load dendextend
install.packages("dendextend")
library(dendextend)

# Create a dendrogram object
dend <- as.dendrogram(hc)

# Customize the dendrogram
dend <- color_branches(dend, k = 3) # Color branches by cluster
plot(dend, main = "Enhanced Dendrogram", cex = 0.8)

# Install and load the cluster package for silhouette
install.packages("cluster")
library(cluster)

# Compute silhouette information
sil <- silhouette(clusters, dist = distance_matrix)

# Plot the silhouette
plot(sil, main = "Silhouette Plot")

