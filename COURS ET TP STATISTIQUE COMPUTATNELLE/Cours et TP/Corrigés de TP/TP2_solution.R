##############################
##### STARTING EXERCICE 1 ####
##############################
sigma1 = 1
sigma2 = 10
n = 20
p = 0.9
sigma.smple = sample(c(sigma1,sigma2), size = n, replace = TRUE, prob = c(p,1-p))
x = rnorm(n, mean = 0, sd = sigma.smple)
n = 20
m = 1000
p.list = c(1,0.95,0.9,0.8,0.7,0.6,0.5)
k.list = c(1,3,5,7)
MSE = c()
SE = c()
for(p in p.list){
    tmp = replicate(m, expr = {
      # generate sd to use
      sigma = sample(c(1,10), size = n, replace = TRUE, prob = c(p,1-p))
      # draw samples
      x = sort(rnorm(n, mean = 0, sd = sigma))
      # compute estimators
      res = c(mean(x))
      for(k in k.list){
        res = c(res,mean(x[(k+1):(n-k)]))
      }
      res = c(res, median(x))
      return(res)
    })
    
    mse = apply(tmp, 1, function(x){ mean(x^2) })
    se = apply(tmp, 1, function(x){ sqrt( sum( (x - mean(x))^2 ) / m ) })
    
    MSE = cbind(MSE, mse)
    SE = cbind(SE, se)
}

cols = c("black","red","blue","palegreen2", "purple","orange")
plot(p.list, MSE[1,],  log = "y", ylim = range(MSE), col = cols[1], type = "l", xlab = "p", ylab = "MSE", main = "MSE of trimmed estimators vs level of contamination")
grid()
for(i in 1:nrow(MSE)){
  lines(p.list, MSE[i,], type = "l", col = cols[i], lwd = 2)
}
legend("topright", c("mean",paste("trimmed-",k.list,sep=""),"median"), col = cols, lwd = 2, bg = "white")
p.list = seq(0, 1, by = 0.1)
k.list = c(1,3,5,7)
sigma1 = 1
sigma2 = 10
MSE = c()
SE = c()
for(p in p.list){
    tmp = replicate(m, expr = {
      # generate sd to use
      sigma = sample(c(sigma1,sigma2), size = n, replace = TRUE, prob = c(p,1-p))
      # draw samples
      x = sort(rnorm(n, mean = 0, sd = sigma))
      # compute estimators
      res = c(mean(x))
      for(k in k.list){
        res = c(res,mean(x[(k+1):(n-k)]))
      }
      res = c(res, median(x))
      return(res)
    })
    
    mse = apply(tmp, 1, function(x){ mean(x^2) })
    se = apply(tmp, 1, function(x){ sqrt( sum( (x - mean(x))^2 ) / m ) })

    MSE = cbind(MSE, mse)
    SE = cbind(SE, se)
}
# plot
cols = c("black","red","blue","palegreen2", "purple","orange")
plot(p.list, MSE[1,],  log = "y", ylim = range(MSE), col = cols[1], type = "l", xlab = "p", ylab = "MSE")
title("MSE of trimmed estimators vs level of contamination")
grid()
for(i in 1:nrow(MSE)){
  lines(p.list, MSE[i,], type = "l", col = cols[i], lwd = 2)
}
legend("topright", c("mean",paste("trimmed-",k.list,sep=""),"median"), col = cols, lwd = 2, bg = "white")


##############################
#### STARTING  EXERCICE 2 ####
##############################
n = 20
m = 1000
p.list = c(1,0.95,0.9,0.8,0.7)
sigma1 = 1
sigma2.list = c(1,2,5,10)
alpha = 0.05
# initialize output matrix
CONF = matrix(0, nrow = length(sigma2.list), ncol = length(p.list))
rownames(CONF) = paste("sigma2-",sigma2.list,sep="")
colnames(CONF) = paste("p-",p.list,sep="")
# process each configuration
for(i in 1:length(p.list)){
  p = p.list[i]
  for(j in 1:length(sigma2.list)){
    sigma2 = sigma2.list[j]
    tmp = replicate(m, expr = {
      # generate sd to use
      sigma.smple = sample(c(sigma1,sigma2), size = n, replace = TRUE, prob = c(p,1-p))
      # draw samples
      x = rnorm(n, mean = 0, sd = sigma.smple)
      # compute upper limit of CI
      Ilow  = (n-1) * var(x) /qchisq(1-alpha/2, df = n-1)
      Ihigh  = (n-1) * var(x) /qchisq(alpha/2, df = n-1)  
      # return
      return(c(Ilow,Ihigh))
     })

    CONF[j,i] = mean(sigma1^2 > tmp[1,] & sigma1^2 < tmp[2,])
  }
}
CONF = round(100*CONF, digits = 1)
# plot
plot(p.list, CONF[1,], ylim = range(CONF), xlab = "p", ylab = "empirical confidence level", , type = "l", lwd = 2, col = "gray")

title("empirical confidence level vs p and sigma2")
grid()
for(i in 2:nrow(CONF)){
  lines(p.list, CONF[i,], col = i, lwd = 2)
}
abline(h = 95, lty = 2, lwd = 2)
legend("bottomright", paste("sigma2 =", sigma2.list), col = c("gray",seq(2,nrow(CONF))), lwd = 2, bg = "white")
##############################
#### STARTING  EXERCICE 3 ####
##############################
mu0 = 500
alpha = 0.05
m = 10000
n = 20
sigma = 100

p.val = numeric(m)
for(i in 1:m){
    x = rnorm(n, mu0, sigma)
    tt = t.test(x, alternative="greater", mu = mu0)  
    p.val[i] = tt$p.value
}

alpha.hat = mean(p.val < alpha)
cat("*** empirical Type-I error =", alpha.hat, "(expected =", alpha, ")***\n")
##############################
#### STARTING  EXERCICE 4 ####
##############################
mu0 = 500
alpha = 0.05
m = 1000
sigma = 100
# define  mu1 and n values to consider
mu1.list = seq(500, 700, by = 10)
n.list = c(20, 50, 100, 200)
#initialize output
P = matrix(0, nrow = length(n.list), ncol = length(mu1.list))
rownames(P) = n.list
colnames(P) = mu1.list
# process each configuration
for(i in seq(length(n.list))){
    n = n.list[i]
    for(j in seq(length(mu1.list))){
        mu1 = mu1.list[j]
        # initialize vector of p.values
        p.val = numeric(m)
        # simulater according to mu1 and compute p.value
        for(k in 1:m){
          x = rnorm(n, mu1, sigma)
          tt = t.test(x, alternative="greater", mu = mu0)  
          p.val[k] = tt$p.value
        }
        # compute power
        P[i,j] = mean(p.val < alpha)
  }
}

# plot
plot(mu1.list, P[1,], type = "l", lwd = 2, xlab = "mu1", ylab = "power", main  = "empirical power vs n and mu1")
grid()
for(i in 1:nrow(P)){
  lines(mu1.list, P[i,], type = "l", col = i, lwd = 2)
}
legend("bottomright", paste("n =", n.list), col = seq(length(n.list)), lwd = 2, bg = "white")


