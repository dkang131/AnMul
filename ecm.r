#ECM for "g" groups
#allocate xbar_0 to phi_k that has smallest

classify <- matrix(c(0,500,100,10,0,50,50,200,0),nrow = 3, ncol = 3, byrow = T)
prior <- matrix(c(0.05,0.6,0.35),ncol = 3, nrow = 1)
density <- matrix(c(0.01,0.85,2),ncol = 3,nrow = 1)

#k = 1
ecm_1 = prior[2]*density[2]*classify[1,2] + prior[3]*density[3]*classify[1,3]
#k = 2
ecm_2 = prior[1]*density[1]*classify[2,1] + prior[3]*density[3]*classify[2,3]
#k = 3
ecm_3 = prior[1]*density[1]*classify[3,1] + prior[2]*density[2]*classify[3,2]


#TPM for multiple group equals to ECM with equal misclassification cost
#allocate xbar_0 to phi_k that has largest pkfk(xbar_0) -> correctly classified
#allocate xbar_1 to phi_k that has largest posterior probability


#point b
#k = 1
ecm1 = prior[1] * density[1]
#k = 2
ecm2 = prior[2] * density[2]
#k = 3
ecm3 = prior[3] * density[3]

#point c
prob1 = ecm1 / (ecm1 + ecm2 + ecm3)
prob2 = ecm2 / (ecm1 + ecm2 + ecm3)
prob3 = ecm3 / (ecm1 + ecm2 + ecm3)
