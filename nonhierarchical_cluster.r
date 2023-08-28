library(ggplot2)
x1 <- c(1,1,0,2,3)
x2 <- c(1,0,2,4,5)

x <- cbind(x1,x2)
awal = data.frame(x)

ggplot(awal, aes(x=x1, y=x2)) + geom_point()

set.seed(2345)

install.packages('animation')

library(animation)

kmeans.ani(awal,2)

cluster <- kmeans(awal,2)
print(cluster)

#===============================================#


data <- USArrests
data_scale <- scale(data)
summary(data)
summary(data_scale)

k <- 4

cluster <- kmeans(data_scale,k)
print(cluster)
cluster$cluster

kmeans.ani(data_scale[,1:2], k, pch = 1:k, col = 1:k)

install.packages('amap')
library(amap)
cluster_dist = Kmeans(data_scale, k, method = "manhattan")
cluster_dist$cluster

library(factoextra)
library(cluster)
set.seed(123)
#elbow
fviz_nbclust(data_scale, kmeans, method = "wss")
#silhouette
fviz_nbclust(data_scale, kmeans, method = "silhouette")
#gap statistic method
gap_stat <- clusGap(data_scale, kmeans, nstart = 25, K.max = 10, B = 50)
fviz_gap_stat(gap_stat)
