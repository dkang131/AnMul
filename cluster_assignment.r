library(ggplot2)
#number 1
x1 <- c(5,6,15,16,25,30)
x2 <- c(5,6,14,15,20,19)
x <- cbind(x1,x2)

awal = data.frame(x)
ggplot(awal, aes(x=x1, y = x2)) + geom_point()

dist_mat <- dist(awal, method = 'euclidean')
dist_mat

hclust_simp <- hclust(dist_mat, method = 'single')
plot(hclust_simp, cex = 0.6, hang = -1)
rect.hclust(hclust_simp, k = 3, border = 2:5)

#number2
hclust_simp_2 <- hclust(dist_mat, method = 'complete')
plot(hclust_simp_2, cex = 0.6, hang = -1)
rect.hclust(hclust_simp_2, k = 3, border = 2:5)

#number3
hclust_simp_3 <- hclust(dist_mat, method = 'average')
plot(hclust_simp_3, cex = 0.6, hang = -1)
rect.hclust(hclust_simp_3, k = 3, border = 2:5)
