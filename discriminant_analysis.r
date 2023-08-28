xbar_1 = matrix(c(-0.0065,-0.0390),ncol=1,nrow=2)
xbar_2 = matrix(c(-0.2483,0.0262),ncol=1,nrow=2)

spl_invv = matrix(c(131.158,-90.423,-90.423,108.147),ncol=2,nrow=2,byrow=T)

a = t((xbar_1 - xbar_2)) %*% spl_invv
ybar_1 = a %*% xbar_1
ybar_2 = a %*% xbar_2
x0 = matrix(c(-0.21,-0.044),ncol=1,nrow=2)
yhat = a %*% x0

#yhat < m then allocate x0 into phi2

p2 = 0.25
p1 = 1-p2

m = (ybar_1 + ybar_2)/2

yhat - m
yhat-m >= log(p2/p1)
