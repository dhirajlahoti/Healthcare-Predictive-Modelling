library(dplyr)
library(factoextra)

# Your existing code for reading data and scaling
D <- read.csv(file.choose(), header = TRUE)
mydata <- select(D, c(3, 15))
df <- scale(mydata)

# Function to generate clustering summary
generate_cluster_summary <- function(data, k, seed = 1234) {
  set.seed(seed)
  kmeans_result <- kmeans(data, centers = k, nstart = 100)
  
  # Extracting cluster centers, sizes, and within-cluster sum of squares
  centers <- kmeans_result$centers
  sizes <- table(kmeans_result$cluster)
  wss <- kmeans_result$tot.withinss
  
  # Create a data frame for summary
  summary_data <- data.frame(
    Cluster = 1:k,
    Center_1 = centers[, 1],
    Center_2 = centers[, 2],  # Add more columns if needed
    Size = sizes,
    WSS = wss
  )
  
  return(summary_data)
}

# Function to plot within-cluster sum of squares
wssplot <- function(data, nc = 15, seed = 1234) {
  wss <- (nrow(data) - 1) * sum(apply(data, 2, var))
  for (i in 2:nc) {
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers = i)$withinss)
  }
  plot(1:nc, wss, type = "b", xlab = "Number of Clusters", ylab = "Within groups sum of squares")
  wss
}

# Generate clustering summary for k = 2
summary_2 <- generate_cluster_summary(df, k = 2)
print("Summary for k = 2:")
print(summary_2)

# Plot within-cluster sum of squares for various cluster numbers
wss_values <- wssplot(df, nc = 15)

# Generate clustering summary for k = 3
summary_3 <- generate_cluster_summary(df, k = 3)
print("Summary for k = 3:")
print(summary_3)

# Your existing code for plotting k = 3
k3 <- kmeans(df, centers = 3, nstart = 100)
autoplot(k3, df, frame = TRUE)
print("Centers for k = 3:")
print(k3$centers)
print("Sizes for k = 3:")
print(k3$size)
######################################################################

library(cluster)

# Assuming 'df' is your data
d <- dist(df, method = "euclidean")
hc1 <- hclust(d, method = "complete")

# Cut the dendrogram to obtain clusters
clusters <- cutree(hc1, k = 5)
print("Cluster Assignments:")
print(clusters)

# Calculate and print the size of each cluster
cluster_size <- table(clusters)
print("Size of Each Cluster:")
print(cluster_size)

# Calculate within-cluster sum of squares for different numbers of clusters
calculate_wss <- function(k, linkage_matrix) {
  clusters <- cutree(linkage_matrix, k = k)
  wss <- sum((d^2) * as.integer(clusters) %*% t(as.integer(clusters)))
  return(wss)
}

# Calculate within-cluster sum of squares for different numbers of clusters
k_values <- 2:10
wss_values <- sapply(k_values, function(k) calculate_wss(k, hc1))

# Plot the elbow curve
plot(k_values, wss_values, type = "b", pch = 19, frame = FALSE, main = "Elbow Method",
     xlab = "Number of Clusters", ylab = "Within-Cluster Sum of Squares")

# Identify the "elbow" point
elbow_point <- k_values[which.min(wss_values)]
abline(v = elbow_point, col = "red", lty = 2)
cat("Elbow point:", elbow_point, "\n")

# Generate a hierarchical clustering summary
hierarchical_summary <- data.frame(
  Cluster = 1:max(clusters),
  Size = as.numeric(table(clusters)),
  Height = hc1$height[rev(order(cutree(hc1, k = max(clusters))))][1:max(clusters)]
)
print("Hierarchical Clustering Summary:")
print(hierarchical_summary)


###############################################################################


library(dbscan)

# Assuming 'df' is your data
# Perform density-based clustering using DBSCAN
dbscan_result <- dbscan(df, eps = 0.5, minPts = 5)

# Print the cluster assignments
print("Cluster Assignments:")
print(dbscan_result$cluster)

# Plot the clusters
plot(df, col = dbscan_result$cluster, main = "Density-Based Clustering",
     xlab = "Age", ylab = "Number of Drinks Consumed", pch = 19)
legend("topright", legend = unique(dbscan_result$cluster), col = 1:length(unique(dbscan_result$cluster)), pch = 19)

# Determine the ideal number of clusters (estimated from the plot)
# Note: DBSCAN doesn't have a direct concept of "number of clusters" as it can discover varying density clusters
# Instead, analyze the density plot to determine the appropriate parameters (eps and minPts)
# You might also consider visualizing the reachability plot or using the 'dbscan::reachability' function.

# Calculate and print cluster sizes
cluster_size <- table(dbscan_result$cluster)
print("Cluster Sizes:")
print(cluster_size)

# Summary statistics related to density-based clustering
cat("Number of clusters:", length(unique(dbscan_result$cluster)), "\n")
cat("Total number of noise points:", sum(dbscan_result$cluster == 0), "\n")
cat("Cluster sizes:")
print(cluster_size)
