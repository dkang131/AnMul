com_lab = matrix(c(6,6,18,8,11,34,28,43,33,20,27,23,64,44,30,75,26,54,30,14),
                 nrow=10,ncol=2,byrow=FALSE)

state_lab = matrix(c(25,28,36,35,15,44,42,34,29,39,15,13,22,29,31,64,30,56,20,21),
                   nrow=10,ncol=2,byrow=FALSE)

com_xbar = matrix(c(mean(com_lab[,1]),mean(com_lab[,2])),nrow=2)
state_xbar = matrix(c(mean(state_lab[,1]),mean(state_lab[,2])),nrow=2)

d_lab = matrix(c(-19,-22,-18,-27,-4,-10,-14,9,4,-19,12,10,42,15,-1,11,-4,-2,10,-7),
            byrow = FALSE, nrow = 10, ncol = 2)
dbar = matrix(c(mean(d_lab[,1]),mean(d_lab[,2])),nrow = 2)
sd = cov(d_lab)
library("matlib")
sd_inv = round(inv(sd),4)

diff = matrix(0, nrow=10, ncol=2)
for(i in 1:10){
  for(j in 1:2){
    diff[i,j] = t(d_lab[i,j])-dbar[j]
  }
}

dj = matrix(0,nrow=10,ncol=1)
for(i in 1:10){
  for(j in 1){
    dj[i,j] = diff[i,]%*%sd_inv%*%diff[i,]
  }
}

f_outlier = matrix(0,nrow=10,ncol=1)
for(i in 1:10){
  for(j in 1){
    n = 20
    p = 2
    f_outlier[i,j] = ((n-p-1)*n%*%dj[i,j])/(p*((n-1)^2)-(n*p%*%dj[i,j]))
  }
}



diff_com = matrix(0, nrow=10, ncol=2)
for(i in 1:10){
  for(j in 1:2){
    diff_com[i,j] = t(com_lab[i,j])-com_xbar[j]
  }
  s_com = matrix(0)
  s_com = (t(diff_com)%*%diff_com)/9
}

diff_state = matrix(0, nrow=10, ncol=2)
for(i in 1:10){
  for(j in 1:2){
    diff_state[i,j] = t(state_lab[i,j])-state_xbar[j]
  }
  s_state = matrix(0)
  s_state = (t(diff_state)%*%diff_state)/9
}

spl = ((9*s_state)+(9*s_com))/(10+10-2)

det_s_com = det(s_com)
det_s_state = det(s_state)
det_spl = det(spl)

ln_m = (1/2*((9*log(det_s_com))+(9*log(det_s_state))))-(1/2*(18*log(det_spl)))
c1 = 0.1204
c2 = 0.0144
c1_sq = c1^2
a1 = 1/2*(2-1)*2*(2+1)
a2 = a1 +(2/(abs(c2-c1_sq)))
b1 = (1-c1-(a1/a2))/a1
b2 = (1-c1-(2/a2))/a2

f = -(2*a2*b2*ln_m)/(a1*(1+2*b2*ln_m))

percentile = matrix(0,ncol=1,nrow=10)
for(i in 1:10){
  percentile[i] = ((i-1/2)/10)
}

chisq_val = matrix(0,ncol=1,nrow=10)
for(i in 1:10){
  chisq_val[i] = qchisq(percentile[i],2)
}

dj_com = matrix(0,nrow=10,ncol=1)
for(i in 1:10){
  for(j in 1){
    dj_com[i,j] = diff_com[i,]%*%spl%*%diff_com[i,]
  }
}

plot(chisq_val,dj_com)
