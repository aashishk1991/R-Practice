library(tidyverse)  # data manipulation
library(cluster)    # clustering algorithms
library(factoextra) # clustering visualization
library(dendextend) # for comparing two dendrograms
library(purrr)

df <- USArrests

df <- na.omit(df) #To remove any missing value that might be present in the data, type this:

df <- scale(df)
head(df)

# Agglomerative Hierarchical Clustering

# Dissimilarity matrix
d <- dist(df, method = "euclidean")

# Hierarchical clustering using Complete Linkage
hc1 <- hclust(d, method = "complete" )

# Plot the obtained dendrogram
plot(hc1, cex = 0.6, hang = -1)


# Alternatively, we can use the agnes function. 
# These functions behave very similarly; however, 
# with the agnes function you can also get the agglomerative coefficient, 
# which measures the amount of clustering structure found

# Compute with agnes
hc2 <- agnes(df, method = "complete")

# Agglomerative coefficient
hc2$ac
## [1] 0.8531583

# methods to assess
m <- c( "average", "single", "complete", "ward")
names(m) <- c( "average", "single", "complete", "ward")

# function to compute coefficient
ac <- function(x) {
  agnes(df, method = x)$ac
}

map_dbl(m, ac)

hc3 <- agnes(df, method = "ward")
pltree(hc3, cex = 0.6, hang = -1, main = "Dendrogram of agnes") 


#Divisive Hierarchical Clustering
# It's also known as DIANA (Divise Analysis) and it works in atop-down manner. 
# The algorithm is an inverse order of AGNES

# compute divisive hierarchical clustering
hc4 <- diana(df)

# Divise coefficient; amount of clustering structure found
hc4$dc
## [1] 0.8514345

# plot dendrogram
pltree(hc4, cex = 0.6, hang = -1, main = "Dendrogram of diana")


# Ward's method
hc5 <- hclust(d, method = "ward.D2" )

# Cut tree into 4 groups
sub_grp <- cutree(hc5, k = 4)

# Number of members in each cluster


USArrests %>% 
  mutate(cluster = sub_grp) %>% 
  head

plot(hc5, cex = 0.6)
rect.hclust(hc5, k = 4, border = 2:5)

fviz_cluster(list(data = df, cluster = sub_grp))

#To use cutree with agnes and diana you can perform the following:
  
# Cut agnes() tree into 4 groups
hc_a <- agnes(df, method = "ward")
cutree(as.hclust(hc_a), k = 4)

# Cut diana() tree into 4 groups
hc_d <- diana(df)
cutree(as.hclust(hc_d), k = 4)


# Determining Optimal Clusters
#Elbow Method
#To perform the elbow method we just need to change 
#the second argument in fviz_nbclust to FUN = hcut.

fviz_nbclust(df, FUN = hcut, method = "wss")

#Average Silhouette Method
#To perform the average silhouette method we follow a similar process.

fviz_nbclust(df, FUN = hcut, method = "silhouette")

#Gap Statistic Method
#And the process is quite similar to perform the gap statistic method.

gap_stat <- clusGap(df, FUN = hcut, nstart = 25, K.max = 10, B = 50)
fviz_gap_stat(gap_stat)