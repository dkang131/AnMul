library("matlib")
bream <- as.data.frame(read.csv("C:/Users/darre/Desktop/SEM 6/Multivariate Analysis/archive/bream.csv",
                  header = TRUE))
bream_final = cbind(bream[2],bream[6],bream[7])
attach(bream_final)

weight_avg = mean(Weight)
height_avg = mean(Height)
width_avg = mean(Width)

m1 = matrix(data = c(weight_avg, height_avg,
                     width_avg),byrow = TRUE)

perch <- as.data.frame(read.csv("C:/Users/darre/Desktop/SEM 6/Multivariate Analysis/archive/perch.csv",
                               header = TRUE))
perch_final <- cbind(perch[2],perch[6],perch[7])
attach(perch_final)
weight_avg_p = mean(Weight_p)
height_avg_p = mean(Height_p)
width_avg_p = mean(Width_p)

m2 = matrix(data = c(weight_avg_p, height_avg_p,
                     width_avg_p),byrow = TRUE)
n1 = 20
n2 = 20
xbar_m = ((n1*m1)+(n2*m2))/(n1+n2)

s1 = cov(bream_final)
s2 = cov(perch_final)
W = ((n1-1)*s1)+((n2-1)*s2)
spl = W/(n1+n2-2)
inv_spl = inv(spl)
t_hoteling = ((n1*n2)/(n1+n2))%*%(t(m1-m2))%*%inv_spl%*%(m1-m2)
f_val = 2.866
c_sq = (((n1+n2-2)*3)/(n1+n2-3-1))*f_val

if (t_hoteling > c_sq){
  print("reject H0") 
} else{
  print("fail to reject H0")
}


B = (n1*((m1-xbar_m)%*%(t(m1-xbar_m))))+(n2*((m2-xbar_m)%*%(t(m2-xbar_m))))
total = B+W

df_B = 1
df_W = 20+20-2
df_total = 20+20-1
det_W = det(W)
det_total = det(total)
wilk = det_W/det_total

#p>=1 & g = 2
#p = 3 & g = 2

test_stat = ((20+20-3-1)/3)*((1-sqrt(wilk))/(sqrt(wilk)))
if (test_stat > f_val){
  print("reject H0")
} else{
  print("fail to reject H0")
}

#dif = m1-m2;dif

ss_mean = 40*(381.2875^2)
r1 = t(bream_final[1])
r1_sq = r1^2
r2 = t(perch_final[1])
r2_sq = r2^2
ss_obs = rowSums(r1_sq)+rowSums(r2_sq)
#typeof(ss_obs)

tr1 = m1[1]-xbar_m[1]
tr2 = m2[1]-xbar_m[1]
ss_tr = (20*(tr1^2))+(20*(tr2^2))

#===================================================

ss_mean1 = 40*(10.55236^2)
r_1 = t(bream_final[2])
r1_sq1 = r_1^2
r_2 = t(perch_final[2])
r2_sq1 = r_2^2
ss_obs1 = rowSums(r1_sq1)+rowSums(r2_sq1)

tr_1 = m1[2]-xbar_m[2]
tr_2 = m2[2]-xbar_m[2]
ss_tr1 = (20*(tr_1^2))+(20*(tr_2^2))

#===================================================

ss_mean2 = 40*(4.66903^2)
r_12 = t(bream_final[3])
r1_sq12 = r_12^2
r_22 = t(perch_final[3])
r2_sq12 = r_22^2
ss_obs12 = rowSums(r1_sq12)+rowSums(r2_sq12)

tr11 = m1[3]-xbar_m[3]
tr22 = m2[3]-xbar_m[3]
ss_tr2 = (20*(tr11^2))+(20*(tr22^2))

#===================================================
txbar = t(xbar_m)
xbar_mat = matrix(rep(txbar,20),ncol=3,nrow=20,byrow=TRUE)
inv_s1 = inv(s1)
bream_xbar = (bream_final-xbar_mat)

bream_test = matrix(0, nrow=20, ncol=3)
for(i in 1:20){
  for(j in 1:3){
    bream_test[i,j] = bream_final[i,j]-xbar_mat[i,j]
  }
  hasil_bream = bream_test
}
#hasil_bream
dj = matrix(0,nrow=20,ncol=1)
for(i in 1:20){
  for(j in 1){
    dj[i,j] = t(hasil_bream[i,])%*%inv_s1%*%hasil_bream[i,]
  }
  hasil_dj = dj
}
#hasil_dj

#======================================================


inv_s2 = inv(s2)
perch_test = matrix(0,nrow=20,ncol=3)
for(i in 1:20){
  for(j in 1:3){
    perch_test[i,j] = perch_final[i,j]-xbar_mat[i,j]
  }
  hasil_perch = perch_test
}
#hasil_perch
dj_1 = matrix(0,nrow=20,ncol=1)
for(i in 1:20){
  for(j in 1){
    dj_1[i,j] = t(hasil_perch[i,])%*%inv_s2%*%hasil_perch[i,]
  }
  hasil_dj_1 = dj_1
}
#hasil_dj_1

q = matrix(0,ncol=1,nrow=20)
for (i in 1:20){
  q[i] = (i-(1/2))/10
}

plot(dj,q)
plot(dj_1,q)
