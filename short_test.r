#import data, subset
data <- read.csv("C:/Users/darre/Downloads/Short Test/Appendix.csv", header = T, sep = ";")
data_final <- data.frame(data[,2:8])

#data scaling
data_final_scale <- scale(data_final)
summary(data_final_scale)

#standardize data
library(dplyr)
data_std <- data_final %>% mutate_all(~(scale(.) %>% as.vector))
frame <- as.data.frame(data_std)
summary(data_std)

#set library
library(ggplot2)
library(cluster)
library(factoextra)
library(amap)
library(animation)

#kmeans
set.seed(123)
fviz_nbclust(data_std, kmeans, method = "silhouette")
#fviz_nbclust(data_std, kmeans, method = "wss")

k <- 3
cluster_std <- kmeans(data_std, k)
print(cluster_std)

member <- as.data.frame(cluster_std$cluster)
clus1 <- as.data.frame(cluster_std$cluster == 1)
clus2 <- as.data.frame(cluster_std$cluster == 2)
clus3 <- as.data.frame(cluster_std$cluster == 3)
