x1 = matrix(c(1,2,3,3,4,5,6,8,9,10),nrow=10,ncol=1)
x2 = matrix(c(2,2,2,1,2,1,1,1,1,1),nrow=10,ncol=1)

data_final = cbind(x1,x2)

xbar = matrix(c(mean(data_final[,1]),mean(data_final[,2])))

sd = cov(data_final)
library('matlib')
inv_sd = inv(sd)

#proporsi chisq
#data-xbar
diff = matrix(0, nrow=10, ncol=2)
for(i in 1:10){
  for(j in 1:2){
    diff[i,j] = t(data_final[i,j])-xbar[j]
  }
}

#dji
dj = matrix(0,nrow=10,ncol=1)
for(i in 1:10){
  for(j in 1){
    dj[i,j] = t(diff[i,])%*%inv_sd%*%diff[i,]
  }
}
#change sd into spl
chi_alpha <- qchisq(0.5,2,lower.tail = F)
decision_q = matrix(0,nrow=10,ncol=1)
for(i in 1:10){
  if(dj[i] > chi_alpha){
    decision_q[i] = print("outside 50%")
  } else{
    decision_q[i] = print('inside 50%')
  }
}
dj_decision = cbind(dj,decision_q)

#3 observation < chi_alpha (inside 50%)
p = 3/10

if(abs(p-0.5) > 0.05){
  print('reject H0')
}else{
  print('not reject H0')
}

#nilai q
percentile = matrix(0,ncol=1,nrow=10)
for(i in 1:10){
  percentile[i] = ((i-1/2)/10)
}

chisq_val = matrix(0,ncol=1,nrow=10)
for(i in 1:10){
  chisq_val[i] = qchisq(percentile[i],2)
}
sort <- sort(dj)
plot(sort(dj),chisq_val)


tr_1 = matrix(c(6,5,8,4,7,7,9,6,9,9),ncol = 5, nrow = 2, byrow = T)
tr_2 = matrix(c(3,1,2,3,6,3),ncol = 3, nrow = 2, byrow = T)
tr_3 = matrix(c(2,5,3,2,3,1,1,3),ncol = 4, nrow = 2, byrow = T)

W = matrix(c(18,-13,-13,18),ncol = 2, nrow = 2, byrow = T)
det(W)

total = matrix(c(54,35,35,102),ncol = 2, nrow = 2, byrow = T)
det(total)

wilk = det(W)/det(total)
wilk

sq_wilk = sqrt(wilk)
4 * ((1-sq_wilk)/sq_wilk)

data_3 = matrix(c(3.17,3.45,3.73,1.82,4.39,2.91,3.54,4.09,2.85,2.05,
             3.45,2.35,5.09,3.88,3.64,4.63,2.88,3.98,3.74,4.36),ncol = 2, nrow = 10,
             byrow = F)
mean = matrix(c(mean(data_3[,1]),mean(data_3[,2])))
cov = cov(data_3)
inv_data_3 = inv(cov)
mu0 = matrix(c(3,5),ncol = 1, nrow = 2, byrow = T)
t = 10%*%(t(mean-mu0))%*%inv_data_3%*%(mean-mu0)

ev = eigen(cov)
values <- ev$values
vectors <- ev$vectors

major1 = mean - sqrt(values[1])*sqrt((2*9*4.46)/(10*8))*vectors[,1]
major2 = mean + sqrt(values[1])*sqrt((2*9*4.46)/(10*8))*vectors[,1]

minor1 = mean - sqrt(values[2])*sqrt((2*9*4.46)/(10*8))*vectors[,2]
minor2 = mean + sqrt(values[2])*sqrt((2*9*4.46)/(10*8))*vectors[,2]

nutrient_a_1 = mean[1] - 2.262 * sqrt(cov[1,1]/10)
nutrient_a_2 = mean[1] + 2.262 * sqrt(cov[1,1]/10)

nutrient_b_1 = mean[2] - 2.262 * sqrt(cov[2,2]/10)
nutrient_b_2 = mean[2] + 2.262 * sqrt(cov[2,2]/10)
