class_a = matrix(c(9,6,9,3,2,7),
                 nrow=3,ncol=2,byrow=FALSE)

class_c = matrix(c(3,1,2,8,9,7),
                   nrow=3,ncol=2,byrow=FALSE)

class_a_xbar = matrix(c(mean(class_a[,1]),mean(class_a[,2])),nrow=2)
class_c_xbar = matrix(c(mean(class_c[,1]),mean(class_c[,2])),nrow=2)

xbar = ((3*class_a_xbar)+(3*class_c_xbar))/6

#korelasi multivariate
cov_a = cov(class_a)
cov_c = cov(class_c)
spl = (((3-1)*cov_a)+((3-1)*cov_c))/(3+3-2)

r11 = spl[1,1]/(sqrt(spl[1,1])*sqrt(spl[1,1]))
r12 = spl[1,2]/(sqrt(spl[1,1])*sqrt(spl[2,2]))
r21 = spl[2,1]/(sqrt(spl[2,2])*sqrt(spl[1,1]))
r22 = spl[2,2]/(sqrt(spl[2,2])*sqrt(spl[2,2]))
r_mat = matrix(c(r11,r12,r21,r22),2,2,byrow = T)

det_r = det(r_mat)
x_hit = -(6-1-((2*2+5)/6))*log(det_r)


#outlier test
diff_a = matrix(0, nrow=3, ncol=2)
for(i in 1:3){
  for(j in 1:2){
    diff_a[i,j] = t(class_a[i,j])-class_a_xbar[j]
  }
}
diff_c = matrix(0, nrow=3, ncol=2)
for(i in 1:3){
  for(j in 1:2){
    diff_c[i,j] = t(class_c[i,j])-class_c_xbar[j]
  }
}

library("matlib")
inv_spl = inv(spl)

dj_a = matrix(0,nrow=3,ncol=1)
for(i in 1:3){
  for(j in 1){
    dj_a[i,j] = diff_a[i,]%*%inv_spl%*%diff_a[i,]
  }
}

dj_c = matrix(0,nrow=3,ncol=1)
for(i in 1:3){
  for(j in 1){
    dj_c[i,j] = diff_c[i,]%*%inv_spl%*%diff_c[i,]
  }
}

f_outlier_a = matrix(0,nrow=3,ncol=1)
for(i in 1:3){
  for(j in 1){
    n = 6
    p = 2
    f_outlier_a[i,j] = ((n-p-1)*n%*%dj_a[i,j])/(p*((n-1)^2)-(n*p%*%dj_a[i,j]))
  }
}

f_outlier_c = matrix(0,nrow=3,ncol=1)
for(i in 1:3){
  for(j in 1){
    n = 6
    p = 2
    f_outlier_c[i,j] = ((n-p-1)*n%*%dj_c[i,j])/(p*((n-1)^2)-(n*p%*%dj_c[i,j]))
  }
}

det_a = det(cov_a)
det_c = det(cov_c)
det_spl = det(spl)

ln_m = (1/2*((2*log(det_a))+(2*log(det_c))))-(1/2*(6*log(det_spl)))
c1 = ((2+1)*((2*(2^2))+(3*2)-1))/(6*2*6*(2+1))
c2 = ((2-1)*(2+2)*((2^2)+2+1))/(6*(6^2)*(2^2))
c1_sq = c1^2

a1 = 1/2*(2-1)*2*(2+1)
a2 = a1 +(2/(abs(c2-c1_sq)))
b1 = (1-c1-(a1/a2))/a1
b2 = (1-c1-(2/a2))/a2

f = -(2*a2*b2*ln_m)/(a1*(1+2*b2*ln_m))
f_table = 2.605


eig_a = matrix(c(1.3944,8.6056),2,1)
eig_c = matrix(c(0.5,1.5),2,1)

#8.6056 -> 0.4719;0.8817
#1.3944 -> -0.8817;0.4719
eig_major_a = 8.6056
eig_minor_a = 1.3944

eigv_a_major = matrix(c(0.4719,0.8817),2,1)
eigv_a_minor = matrix(c(-0.8817,0.4719),2,1)

eig_major_c = 1.5
eig_minor_c = 0.5

eigv_c_major = matrix(c(-0.7071,0.7071),2,1)
eigv_c_minor = matrix(c(-0.7071),2,1)

c = 2.4478

#major axis class a
major1_a = class_a_xbar-(c*sqrt(eig_major_a)*eigv_a_major)
major2_a = class_a_xbar+(c*sqrt(eig_major_a)*eigv_a_major)

#minor axis class a
minor1_a = class_a_xbar-(c*sqrt(eig_minor_a)*eigv_a_minor)
minor2_a = class_a_xbar+(c*sqrt(eig_minor_a)*eigv_a_minor)

#major axis class c
major1_c = class_c_xbar-(c*sqrt(eig_major_c)*eigv_c_major)
major2_c = class_c_xbar+(c*sqrt(eig_major_c)*eigv_c_major)

#minor axis class c
minor1_c = class_c_xbar-(c*sqrt(eig_minor_c)*eigv_c_minor)
minor2_c = class_c_xbar+(c*sqrt(eig_minor_c)*eigv_c_minor)

class_a_value = rbind(t(class_a_xbar),t(major1_a),t(major2_a),t(minor1_a),
                      t(minor2_a))
class_c_value = rbind(t(class_c_xbar),t(major1_c),t(major2_c),t(minor1_c),
                      t(minor2_c))
