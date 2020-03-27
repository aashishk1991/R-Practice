library(tidyverse)
library(cluster)
library(factoextra)
df<- USArrests

df <- na.omit(df) #To remove any missing value that might be present in the data

df <- scale(df)
head(df)

distance <- get_dist(df)
fviz_dist(distance, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))

k2 <- kmeans(df, centers = 2, nstart = 25)
str(k2)
k2

fviz_cluster(k2, data = df)
# Examin multiple clusters 
k3 <- kmeans(df, centers = 3, nstart = 25)
k4 <- kmeans(df, centers = 4, nstart = 25)
k5 <- kmeans(df, centers = 5, nstart = 25)

# plots to compare
p1 <- fviz_cluster(k2, geom = "point", data = df) + ggtitle("k = 2")
p2 <- fviz_cluster(k3, geom = "point",  data = df) + ggtitle("k = 3")
p3 <- fviz_cluster(k4, geom = "point",  data = df) + ggtitle("k = 4")
p4 <- fviz_cluster(k5, geom = "point",  data = df) + ggtitle("k = 5")

library(gridExtra)
grid.arrange(p1, p2, p3, p4, nrow = 2)

#Determining Optimal Clusters
#Elbow Method
library(dplyr)

# function to compute total within-cluster sum of square 
set.seed(123)
wss <- function(k) 
  {
  kmeans(df, k, nstart = 10 )$tot.withinss
  }

# Compute and plot wss for k = 1 to k = 15

k.values <- 1:15

# extract wss for 2-15 clusters
wss_values <- map_dbl(k.values, wss)

plot(k.values, wss_values,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")

set.seed(123)

fviz_nbclust(df, kmeans, method = "wss")

# Average Silhouette Method
# In short, the average silhouette approach measures the quality of a clustering. 
# That is, it determines how well each object lies within its cluster. 
# A high average silhouette width indicates a good clustering.

# function to compute average silhouette for k clusters
avg_sil <- function(k) {
  km.res <- kmeans(df, centers = k, nstart = 25)
  ss <- silhouette(km.res$cluster, dist(df))
  mean(ss[, 3])
}

# Compute and plot wss for k = 2 to k = 15
k.values <- 2:15

# extract avg silhouette for 2-15 clusters
avg_sil_values <- map_dbl(k.values, avg_sil)

plot(k.values, avg_sil_values,
     type = "b", pch = 19, frame = FALSE, 
     xlab = "Number of clusters K",
     ylab = "Average Silhouettes")

fviz_nbclust(df, kmeans, method = "silhouette")


# Compute k-means clustering with k = 4
set.seed(123)
final <- kmeans(df, 4, nstart = 25)
print(final)

fviz_cluster(final, data = df)


USArrests %>%
  mutate(Cluster = final$cluster) %>%
  group_by(Cluster) %>%
  summarise_all("mean")
