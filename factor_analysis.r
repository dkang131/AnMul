#anmul
#factor analysis example 2

data = matrix(c(1,0.02,0.96,0.42,0.01,
                0.02,1,0.13,0.71,0.85,
                0.96,0.13,1,0.5,0.11,
                0.42,0.71,0.5,1,0.79,
                0.01,0.85,0.11,0.79,1),ncol = 5, nrow = 5, byrow =T)

ev = eigen(data)
values <- ev$values
vector <- ev$vectors

values_12 <- matrix(c(values[1:2]))
values_12_sqrt <- sqrt(values_12)
vector_12 <- cbind(vector[,1],vector[,2])

LL = matrix(0,ncol = 2, nrow = 5)
for(i in 1:5){
  for (j in 1:2){
    LL[i,j] = values_12_sqrt[j] %*% vector_12[i,j] 
  }
}


LL_sqr = LL^2
communal = as.matrix(rowSums(LL_sqr))
spc_var = 1-communal

spc_var_m <- function(){
  spc_var_m = matrix(0,5,5)
  diag(spc_var_m) = spc_var
  spc_var_m
}
spc_var_m()

LL_spcvar = LL %*% t(LL) + spc_var_m()
