apple <- c(36,3,56,27,48,37)
samsung <- c(42,5,29,31,36,45)
nokia <- c(7,43,13,24,8,6)
huawei <- c(22,32,35,17,35,19)

apple_sum <- sum(apple)
samsung_sum <- sum(samsung)
nokia_sum <- sum(nokia)
huawei_sum <- sum(huawei)

coltot <- c(apple_sum, samsung_sum, nokia_sum, huawei_sum)
frame1 <- cbind(apple, samsung, nokia, huawei)
frame2 <- rbind(frame1, coltot)
rowtot <- rowSums(frame2)

data_final <- cbind(frame2, rowtot)
table <- data_final[-7,-5]
column.masses <- colSums(table)
row.masses <- rowSums(table)
E <- row.masses %o% column.masses
Z <- (p_mat - E) / sqrt(E)

#conrrespondence matrix
conff <- data_final/(data_final[7,5])
p_mat <- conff[-7,-5]

library('matlib')
#R MATRIX
dr_mat <- matrix(diag(conff[-7,5]), 6, 6)
inv_dr <- inv(dr_mat)
R_mat <- inv_dr %*% p_mat
  
#C MATRIX
dc_mat <- matrix(diag(conff[7,-5]), 4, 4)
inv_dc <- inv(dc_mat)
C_mat <- p_mat %*% inv_dc

#SVD
SVD <- svd(Z)
u_mat <- SVD$u
v_mat <- SVD$v
d_mat <- SVD$d

#chisq test
#H0 = there is no relationship between 2 variables
#H1 = there is relationship between 2 variables

for (i in 1:6){
  for (j in 1:4){
    chisq = (table[i,j] - (sum(table[i,]) * sum(table[,j]) / sum(table)))^2 /
      (sum(table[i,]) * sum(table[,j]) / sum(table))
  }
}
