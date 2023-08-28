library(ggplot2)
x1 <- c(0.4,0.22,0.35,0.26,0.08,0.45)
x2 <- c(0.53,0.38,0.32,0.19,0.41,0.30)
x <- cbind(x1,x2)

awal = data.frame(x)
ggplot(awal, aes(x=x1, y = x2)) + geom_point()

dist_mat <- dist(awal, method = 'euclidean')
dist_mat

hclust_simp <- hclust(dist_mat, method = 'single')
plot(hclust_simp, cex = 0.6, hang = -1)
rect.hclust(hclust_simp, k = 3, border = 2:5)

?dist()
