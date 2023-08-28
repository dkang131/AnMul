dom_rad = matrix(c(1.103,0.842,0.925,0.857,0.795,0.787,0.933,0.799,0.945,0.921,
                        0.792,0.815,0.755,0.88,0.9,0.764,0.733,0.932,0.856,0.89),nrow=20,ncol=1)
rad = matrix(c(1.052,0.859,0.873,0.744,0.809,0.779,0.88,0.851,0.876,0.906,
               0.825,0.751,0.724,0.866,0.838,0.757,0.748,0.898,0.786,0.95),ncol=1,nrow=20)

dom_rad_1year = matrix(c(1.027,0.857,0.875,0.873,0.811,0.64,0.947,0.886,0.991,0.977,
                         0.825,0.851,0.770,0.912,0.905,0.756,0.765,0.932,0.843,0.879),
                       ncol=1,nrow=20)

rad_1year = matrix(c(1.051,0.817,0.88,0.698,0.813,0.734,0.865,0.806,0.923,0.925,
                     0.826,0.765,0.73,0.875,0.826,0.727,0.764,0.914,0.782,0.906),
                   ncol=1,nrow=20)

year_0 = cbind(dom_rad,rad)
year_1 = cbind(dom_rad_1year,rad_1year)

diff_year = year_0-year_1
xbar_diff = matrix(c(mean(diff_year[,1]),mean(diff_year[,2])))
sd = cov(diff_year)
library('matlib')
inv_sd = inv(sd)

#find dj
#p-variate normality test
diff = matrix(0, nrow=20, ncol=2)
for(i in 1:20){
  for(j in 1:2){
    diff[i,j] = t(diff_year[i,j])-xbar_diff[j]
  }
}

dj = matrix(0,nrow=20,ncol=1)
for(i in 1:20){
  for(j in 1){
    dj[i,j] = diff[i,]%*%inv_sd%*%diff[i,]
  }
}

chi_alpha <- qchisq(0.5,2,lower.tail = F)
decision_q = matrix(0,nrow=20,ncol=1)
for(i in 1:20){
  if(dj[i] > chi_alpha){
    decision_q[i] = print("outside 50%")
  } else{
    decision_q[i] = print('inside 50%')
  }
}
dj_decision = cbind(dj,decision_q)

#11 observation < chi_alpha (inside 50%)
p = 11/30

if(abs(p-0.5) > 0.05){
  print('reject H0')
}else{
  print('not reject H0')
}

#nilai q
percentile = matrix(0,ncol=1,nrow=20)
for(i in 1:20){
  percentile[i] = ((i-1/2)/20)
}

chisq_val = matrix(0,ncol=1,nrow=20)
for(i in 1:20){
  chisq_val[i] = qchisq(percentile[i],2)
}

plot_data <- data.frame(sort(dj),chisq_val)
plot(plot_data)

#outlier test
f_outlier = matrix(0,nrow=20,ncol=1)
for(i in 1:20){
  for(j in 1){
    n = 20
    p = 2
    f_outlier[i,j] = ((n-p-1)*n%*%dj[i,j])/(p*((n-1)^2)-(n*p%*%dj[i,j]))
  }
}

decision <- matrix(0,nrow=20,ncol=1)
f_alpha = qf(0.05,2,17,lower.tail = F)
for(i in 1:20){
  if(f_outlier[i] > f_alpha){
    decision[i] = print("outlier")
  } else{
    decision[i] = print('not outlier')
  }
}

f_outlier_decision = cbind(f_outlier,decision)

#correlation test
correl = cor(diff_year)
det_r = det(correl)
#n = 20
#p = 2
x_hit = -(20-1-((2*2+5)/6))*log(det_r)
x_tabel = qchisq(0.05,1,lower.tail = F)
if(x_hit > x_tabel){
  print('reject H0')
}else{
  print('not reject H0')
}

#box m test
#k = 2
#p = 2
#v = 19

s1 = cov(year_0)
s2 = cov(year_1)
spl = ((19*s1)+(19*s2))/(19+19)

det_s1 = det(s1)
det_s2 = det(s2)
det_spl = det(spl)

ln_m = (1/2*((19*det_s1)+(19*det_s2)))+(1/2*((19+19)*det_spl))
c1 = ((2+1)*(2*(2^2)+3*2-1))/(6*2*19*(2+1))
c2 = ((2-1)*(2+2)*((2^2)+2+1))/(6*(2^2)*(19^2))
c1_sq = c1^2

c2>c1_sq
c2<c1_sq

a1 = 1/2*(2-1)*2*(2+1)
a2 = (a1+2)/(abs(c2-c1_sq))
b1 = (1-c1-(a1/a2))/a1
b2 = (1-c1+(2/a2))/a2

f = -(2*a2*b2*ln_m)/(a1*(1+2*b2*ln_m))
f_tabel = qf(0.05,a1,a2,lower.tail = F)

if(f > f_tabel){
  print("reject H0")
}else{
  print("not reject H0")
}

#mean vecctor dependent
t = 20*t(xbar_diff)%*%inv_sd%*%xbar_diff

ftab = ((20-1)*2)/(20-2)*qf(0.05,2,18,lower.tail = F)

if(t > ftab){
  print('reject H0')
}else{
  print('not reject H0')
}

#simultan test
theta1 = xbar_diff[1]-sqrt(ftab)*sqrt(sd[1,1]/20)
theta2 = xbar_diff[1]+sqrt(ftab)*sqrt(sd[1,1]/20)

theta_1 = xbar_diff[2]-sqrt(ftab)*sqrt(sd[2,2]/20)
theta_2 = xbar_diff[2]+sqrt(ftab)*sqrt(sd[2,2]/20)

data.frame(theta1,theta2,theta_1,theta_2)

#since 0 is insde interval -> not reject H0

#univariate konventional
#weight
left_side = xbar_diff[1]-(qt(0.025,9,lower.tail = F))*(sd[1,1]/sqrt(20))
right_side = xbar_diff[1]+(qt(0.025,9,lower.tail = F))*(sd[1,1]/sqrt(20))
left_side < 0 
0 < right_side
data.frame(left_side,right_side)
#height
left_height = xbar_diff[2]-(qt(0.025,9,lower.tail = F))*(sd[2,2]/sqrt(20))
right_height = xbar_diff[2]+(qt(0.025,9,lower.tail = F))*(sd[2,2]/sqrt(20))
left_height < 0
0 < right_height
data.frame(left_height,right_height)
#bonferroni
#weight
left_weight_bon = xbar_diff[1]-(qt(0.0125,38))*sqrt(((1/20)+(1/20))*sd[1,1])
right_weight_bon = xbar_diff[1]+(qt(0.0125,38))*sqrt(((1/20)+(1/20))*sd[1,1])
left_weight_bon < 0 
0 < right_weight_bon
data.frame(left_weight_bon,right_weight_bon)
#height
left_height_bon = xbar_diff[2]-(qt(0.0125,38))*sqrt(((1/20)+(1/20))*sd[2,2])
right_height_bon = xbar_diff[2]+(qt(0.0125,38))*sqrt(((1/20)+(1/20))*sd[2,2])
left_height_bon < 0
0 < right_height_bon
data.frame(left_height_bon,right_height_bon)
