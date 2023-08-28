fac_1 <- c(0.35, 0.911, 0.915, 0.905,0.784,-0.066,-0.222)
fac_2 <- c(0.642, -0.138, -0.330, - 0.371, 0.413, -0.864, -0.709)
fac_3 <- c(0.592, -0.017, 0.024, -0.060, -0.041, 0.04, 0.584)

matrix_L <- matrix(cbind(fac_1,fac_2,fac_3), ncol = 3)

matrix_LL_T <- matrix_L %*% t(matrix_LL)

spec_var <- matrix(diag(c(0.115, 0.151, 0.052, 0.04, 0.213, 0.247, 0.107)), 7, 7)
spec_var

summation <- matrix_LL_T + spec_var
summation

data <- read.csv('C:/Users/darre/Downloads/Quiz Multivariate Analysis/data_quiz_1_3.csv', header = T, sep = ";")

#standardize data
library(dplyr)
data_std <- data %>% mutate_all(~(scale(.) %>% as.vector))
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
fviz_nbclust(data_std, kmeans, method = "wss")

gap_stat <- clusGap(data_std, kmeans, nstart = 25, K.max = 10, B = 50)
fviz_gap_stat(gap_stat)

k <- 2 #from silhoutte method
cluster_std <- kmeans(data_std, k)
print(cluster_std)

member <- as.data.frame(cluster_std$cluster)
clus1 <- as.data.frame(cluster_std$cluster == 1)
clus2 <- as.data.frame(cluster_std$cluster == 2)
member
