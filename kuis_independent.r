weight = matrix(c(242,290,340,363,430,450,500,390,450,500,
                  55,60,90,120,150,140,170,145,200,273,
                 32,40,51.5,70,100,78,80,85,85,110),nrow=30,ncol=1)
height = matrix(c(11.52,12.48,12.3778,12.73,12.444,13.6024,14.1795,12.67,14.0049,14.2266,
                  6.8475,6.5772,7.4052,8.3922,8.8928,8.5376,9.396,9.7364,10.3458,11.088,
                  3.528,3.824,4.5924,4.588,5.2224,5.1992,5.6358,5.1376,5.082,5.6925),nrow=30,ncol=1)

data_final = cbind(weight,height)

xbar = matrix(c(mean(data_final[,1]),mean(data_final[,2])))

sd = cov(data_final)
library('matlib')
inv_sd = inv(sd)

#proporsi chisq
#data-xbar
diff = matrix(0, nrow=30, ncol=2)
for(i in 1:30){
  for(j in 1:2){
    diff[i,j] = t(data_final[i,j])-xbar[j]
  }
}

#dji
dj = matrix(0,nrow=30,ncol=1)
for(i in 1:30){
  for(j in 1){
    dj[i,j] = t(diff[i,])%*%inv_sd%*%diff[i,]
  }
}
#change sd into spl
chi_alpha <- qchisq(0.5,2,lower.tail = F)
decision_q = matrix(0,nrow=30,ncol=1)
for(i in 1:30){
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
percentile = matrix(0,ncol=1,nrow=30)
for(i in 1:30){
  percentile[i] = ((i-1/2)/30)
}

chisq_val = matrix(0,ncol=1,nrow=30)
for(i in 1:30){
  chisq_val[i] = qchisq(percentile[i],2)
}

plot(sort(dj),chisq_val)

#outlier
f_outlier = matrix(0,nrow=30,ncol=1)
for(i in 1:30){
  for(j in 1){
    n = 30
    p = 2
    f_outlier[i,j] = ((n-p-1)*n%*%dj[i,j])/(p*((n-1)^2)-(n*p%*%dj[i,j]))
  }
}

decision <- matrix(0,nrow=30,ncol=1)
f_alpha = qf(0.05,2,27,lower.tail = F)
for(i in 1:30){
  if(f_outlier[i] > f_alpha){
    decision[i] = print("outlier")
  } else{
    decision[i] = print('not outlier')
  }
}

f_outlier_decision = cbind(f_outlier,decision)

# #box m test
weight_box_m = matrix(c(242,290,340,363,430,450,500,390,450,500,
                  55,60,90,120,150,140,170,145,200,273,
                   32,40,51.5,70,100,78,80,85,85,110),nrow=10,ncol=3,byrow = F)
 
height_box_m = matrix(c(11.52,12.48,12.3778,12.73,12.444,13.6024,14.1795,12.67,14.0049,14.2266,
                   6.8475,6.5772,7.4052,8.3922,8.8928,8.5376,9.396,9.7364,10.3458,11.088,
                   3.528,3.824,4.5924,4.588,5.2224,5.1992,5.6358,5.1376,5.082,5.6925),
                   nrow=10,ncol=3,byrow=F)
# weight_xbar = matrix(c(mean(weight_box_m[,1]),mean(weight_box_m[,2]),mean(weight_box_m[,3])),nrow=3)
# height_xbar = matrix(c(mean(height_box_m[,1]),mean(height_box_m[,2]),mean(height_box_m[,3])),nrow=3)
# grand_mean = ((10*weight_xbar)+(10*height_xbar))/(10+10)
# 
# s_weight = cov(weight_box_m)
# s_height = cov(height_box_m)
# spl = (((10-1)*s_weight)+((10-1)*s_height))/(9+9)
# 
# det_s_weight = det(s_weight)
# det_s_height = det(s_height)
# det_spl = det(spl)
# 
# 
# #k = 3
# #p = 2
# #v = 9
# 
# ln_m = (1/2*((9*log(det_s_weight))+(9*log(det_s_height))))-(1/2*(18*log(det_spl)))
# 
c1 = ((3+1)*((2*(2^2))+(3*2)-1))/(6*3*9*(2+1))
c2 = ((2-1)*(2+2)*((3^2)+3+1))/(6*(3^2)*(9^2))
c1_sq = c1^2
# 
# if(c2 > c1_sq){
#   print("use F1")
# }else{
#   print("use F2")
# }
# 
a1 = 1/2*(3-1)*2*(2+1)
a2 = (a1+2)/(abs(c2-c1_sq))
b1 = (1-c1-(a1/a2))/a1
# 
# f1 = -2*b1*ln_m
# f_table = qf(0.05,a1,a2,lower.tail = F)
# 
# if(f1 > f_table){
#   print("reject H0")
# }else{
#   print("not reject H0")
# }
# 

#correl test bartlett
correl = cor(data_final)
det_r = det(correl)
#n = 30
#p = 2
x_hit = -(30-1-((2*2+5)/6))*log(det_r)
x_tabel = qchisq(0.05,1,lower.tail = F)
if(x_hit > x_tabel){
  print('reject H0')
}else{
  print('not reject H0')
}

#MANOVA
#weight_xbar_diff = weight_xbar-grand_mean
#height_xbar_diff = height_xbar-grand_mean

pop_1 = cbind(weight_box_m[,1],height_box_m[,1])
pop_2 = cbind(weight_box_m[,2],height_box_m[,2])
pop_3 = cbind(weight_box_m[,3],height_box_m[,3])

xbar_pop1 = matrix(c(mean(pop_1[,1]),mean(pop_1[,2])))
xbar_pop2 = matrix(c(mean(pop_2[,1]),mean(pop_2[,2])))
xbar_pop3 = matrix(c(mean(pop_3[,1]),mean(pop_3[,2])))


xbar_tot = ((10*xbar_pop1)+(10*xbar_pop2)+(10*xbar_pop3))/(10+10+10)

#MANOVA

pop1_xbar_diff = xbar_pop1-xbar_tot
pop2_xbar_diff = xbar_pop2-xbar_tot
pop3_xbar_diff = xbar_pop3-xbar_tot


B = (10*pop1_xbar_diff%*%(t(pop1_xbar_diff)))+
  (10*pop2_xbar_diff%*%(t(pop2_xbar_diff)))+
  (10*pop3_xbar_diff%*%(t(pop3_xbar_diff)))

s_pop1 = cov(pop_1)
s_pop2 = cov(pop_2)
s_pop3 = cov(pop_3)

W = ((10-1)*s_pop1)+((10-1)*s_pop2)+((10-1)*s_pop3)

total = B+W

det_W = det(W)
det_total = det(total)
wilk = det_W/det_total

df_B = 2
df_W = 10+10+10-3
df_total = 10+10+10-1

#p=2
#g=3

test_stat = ((10+10+10-3-1)/2)*((1-sqrt(wilk))/sqrt(wilk))
v1 = 2*(3-1)
v2 = 10+10+10-3-1

f_tab = qf(0.05,v1,v2,lower.tail = F)

if(test_stat > f_tab){
  print('reject H0')
}else{
  print('not reject H0')
}

#univariate test anova one way
#var1
ss_tr = (10*(xbar_pop1[1]-xbar_tot[1])^2)+(10*(xbar_pop2[1]-xbar_tot[1])^2)+
  (10*(xbar_pop3[1]-xbar_tot[1])^2)
ss_obs = (sum((pop_1[,1])^2))+(sum((pop_2[,1])^2))+(sum((pop_3[,1])^2))
ss_mean = 30*((xbar_tot[1])^2)

#ss_obs = ss_mean + ss_tr + ss_res
#ss_total = ss_obs - ss_mean

ss_total = ss_obs-ss_mean
ss_res = ss_total-ss_tr

f_var1 = (ss_tr/2)/(ss_res/(10+10+10-3))
qf(0.05,2,27,lower.tail = F)

#var2
ss_tr_2 = (10*(xbar_pop1[2]-xbar_tot[2])^2)+(10*(xbar_pop2[2]-xbar_tot[2])^2)+
  (10*(xbar_pop3[2]-xbar_tot[2])^2)
ss_obs_2 = (sum((pop_1[,2])^2))+(sum((pop_2[,2])^2))+(sum((pop_3[,2])^2))
ss_mean_2 = 30*((xbar_tot[2])^2)

#ss_obs = ss_mean + ss_tr + ss_res
#ss_total = ss_obs - ss_mean

ss_total_2 = ss_obs_2-ss_mean_2
ss_res_2 = ss_total_2-ss_tr_2

f_var2 = (ss_tr_2/2)/(ss_res_2/(10+10+10-3))
qf(0.05,2,27,lower.tail = F)

#LSD 
#LSD weight
abs_pop12 = abs(xbar_pop1-xbar_pop2)
abs_pop13 = abs(xbar_pop1-xbar_pop3)
abs_pop23 = abs(xbar_pop2-xbar_pop3)

#REVISED DJ
spool = ((9*s_pop1)+(9*s_pop2)+(9*s_pop3))/(10+10+10-2)
inv_spool = inv(spool)
dj_rev = matrix(0,nrow=30,ncol=1)
for(i in 1:30){
  for(j in 1){
    dj_rev[i,j] = t(diff[i,])%*%inv_spool%*%diff[i,]
  }
}

chi_alpha <- qchisq(0.5,2,lower.tail = F)
decision_q_rev = matrix(0,nrow=30,ncol=1)
for(i in 1:30){
  if(dj_rev[i] > chi_alpha){
    decision_q_rev[i] = print("outside 50%")
  } else{
    decision_q_rev[i] = print('inside 50%')
  }
}

dj_decision_rev = cbind(dj_rev,decision_q_rev)
data_frame = data.frame(sort(dj_rev),chisq_val)
plot(data_frame)

p_new = 0/30

if(abs(p_new-0.5) > 0.05){
  print('reject H0')
}else{
  print('not reject H0')
}


#BOX M TEST (REVISED)
det_s1 = det(s_pop1)
det_s2 = det(s_pop2)
det_s3 = det(s_pop3)
spl_new = ((9*s_pop1)+(9*s_pop2)+(9*s_pop3))/(27)
det_spl_new = det(spl_new)
ln_m_rev = (1/2*((9*log(det_s1))+(9*log(det_s2))+(9*log(det_s3))))+
  (1/2*(27)*log(det_spl_new))
f_rev = -2*b1*ln_m_rev

f_table_rev = qf(0.05,a1,a2,lower.tail = F)

if(f_rev > f_table_rev){
  print("reject H0")
}else{
  print("not reject H0")
}

