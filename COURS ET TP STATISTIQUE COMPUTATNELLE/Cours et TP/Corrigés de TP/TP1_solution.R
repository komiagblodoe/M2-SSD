## ----fig=TRUE------------------------------------------------------------
#############################
#### STARTING EXERCICE 1 ####
#############################


set.seed(20)
# choose parameters
mu = 5
sigma = 3
N = c(100, 200, 1000, 10000)
# draw numbers
x = list()
for(i in 1:length(N)){
  x[[i]] = rnorm(N[i], mean = mu, sd = sigma) 
}

# plot histograms

par(mfrow = c(2,2))
for(i in 1:length(N)){
  hist(x[[i]], xlab = "x", main = paste("histogram computed from\n", N[i], " points drawn from N(", mu, ",", sigma, ")", sep = ""))
}

## ----fig=TRUE------------------------------------------------------------
# show theoretical density

x.range = range(unlist(x))
x.grid = seq(x.range[1], x.range[2], by = 0.01)
f = 1/(sigma*sqrt(2*pi)) * exp( -(x.grid-mu)^2/(2*sigma^2) ) 

# compare theoretical to empirical densities

cols = c("red","blue","orange","green4","gold")
plot(x.grid, f, type = "l", lty = 2, lwd = 2, ylim = c(0, 1.2*max(f)), xlab = "x", main = paste("N(", mu, ",", sigma, ") - theoretical vs empirical distributions", sep = ""))
for(i in 1:length(x)){
  lines(density(x[[i]]), col = cols[i], lwd = 2)
}
grid()
legend("topright",  paste("N =",N), col = cols, lwd = 2, bg = "white")


## ----echo=false,fig=TRUE-------------------------------------------------
# choose parameters

r = 0.2
N = c(100, 200, 1000, 1000)

# draw numbers

x = list()
for(i in 1:length(N)){
  x[[i]] = rexp(N[i], rate = r) 
}

## plot histograms
#par(mfrow = c(2,2))
#for(i in 1:length(N)){
#  hist(x[[i]], xlab = "x", main = paste("histogram computed from\n", N[i], " points drawn from exp(", r, ")", sep = ""))
#}
## plot histograms - more breaks
#par(mfrow = c(2,2))
#for(i in 1:length(N)){
#  hist(x[[i]], breaks = 25, xlab = "x", main = paste("histogram computed from\n", N[i], " points drawn from exp(", r, ")", sep = ""))
#}

#par(mfrow = c(1,1))
# show theoretical density

x.range = range(unlist(x))
x.grid = seq(x.range[1], x.range[2], by = 0.01)
f = r * exp(-r*x.grid)

# compare theoretical to empirical densities

cols = c("red","blue","orange","green4","gold")
plot(x.grid, f, type = "l", lty = 2, lwd = 2, ylim = c(0, 1.2*max(f)), xlab = "x", main = paste("exp(", r, ") - theoretical vs empirical distributions", sep = ""))
for(i in 1:length(x)){
    lines(density(x[[i]]), col = cols[i], lwd = 2)
}
grid()
legend("topright",  paste("N =",N), col = cols, lwd = 2, bg = "white")


## ----echo=false,fig=TRUE-------------------------------------------------
# choose parametes

alpha = 2
beta = 5
N = c(100, 200, 1000, 10000)

# draw numbers

x = list()
for(i in 1:length(N)){
  x[[i]] = rbeta(N[i], alpha, beta) 
}

## plot histograms
#par(mfrow = c(2,2))
#for(i in 1:length(N)){
#  hist(x[[i]], xlab = "x", main = paste("histogram computed from\n", N[i], " points drawn from beta(", alpha, ",", beta, ")", sep = ""))
#}
## plot histograms - more breaks
#for(i in 1:length(N)){
#  hist(x[[i]], breaks = 25, xlab = "x", main = paste("histogram computed from\n", N[i], " points drawn from beta(", alpha, ",", beta, ")", sep = ""))
#}

#par(mfrow = c(1,1))
# show theoretical density

x.range = range(unlist(x))
x.grid = seq(x.range[1], x.range[2], by = 0.01)
f = x.grid^(alpha-1)*(1-x.grid)^(beta-1)/beta(alpha,beta)

# compare theoretical to empirical densities

cols = c("red","blue","orange","green4")
plot(x.grid, f, type = "l", lty = 2, lwd = 2, ylim = c(0, 1.2*max(f)), xlab = "x", main = paste("beta(", alpha, ",", beta , ") - theoretical vs empirical distributions", sep = ""))
for(i in 1:length(x)){
  lines(density(x[[i]]), col = cols[i], lwd = 2)
}
grid()
legend("topright",  paste("N =",N), col = cols, lwd = 2, bg = "white")


## ----fig=TRUE------------------------------------------------------------

#############################
#### STARTING EXERCICE 2 ####
#############################

n = 10000
lambda = 0.5
x = runif(n, 0, 1)
y = -log(x)/lambda
plot(density(y), main = "exponential law - empirical densities")
y2 = rexp(n, lambda)
lines(density(y2), col = 2)
legend("topright", c("inversion method","R implementation"), col = c(1,2), lwd = 1)


## ----fig=TRUE------------------------------------------------------------

#############################
#### STARTING EXERCICE 3 ####
#############################

# draw samples

x = c()
cptr = 0
M = 1.5
n  = 1000
while(length(x) < n){
  a = runif(1, 0, 1)
  b = runif(1, 0, 1)
  if( b*M <= 6*a*(1-a) ){
      x = c(x,a)
  }
  cptr = cptr + 1
}
cat("number of draws required to get", n, "samples =", cptr, "\n")

# compare the empirical and theoretical distributions 

grid = seq(0, 1, by = 0.01)
fgrid = 6*grid*(1-grid)
plot(grid, fgrid, type = "l")
lines(density(x), col = 2)

## ----fig=TRUE------------------------------------------------------------
# same exemple for increasing values of M

M.grid = seq(M, 3*M, by = 0.1)
cptr.grid = c()
for(M2 in M.grid){
  x = c()
  cptr = 0
  while(length(x) < n){
    a = runif(1, 0, 1)
    b  = runif(1, 0, 1) 
    if( b*M2 <= 6*a*(1-a) ){
      x = c(x,a)
    }
    cptr = cptr + 1
  }  
  cptr.grid = c(cptr.grid, cptr)
}
plot(M.grid,  cptr.grid, xlab = "M", ylab = "number of draws required to get n samples", main = "simulation by reject : illustration of the reject rate", type = "b", pch = 19)
grid()



## ------------------------------------------------------------------------
#############################
#### STARTING EXERCICE 4 ####
#############################

n = 50
x = runif(n, 0, 1)
y = runif(n, 0, 1)
pi_hat = 4*mean(x^2+y^2 <= 1)
cat("estimation of pi from", n, "samples =", round(pi_hat, digits=4))

## ----fig=TRUE------------------------------------------------------------
N = seq(50, 1000, by=50)
pi_hat = numeric(length(N))
for(i in 1:length(N)){
  x = runif(N[i], 0, 1)
  y = runif(N[i], 0, 1)
  pi_hat[i] = 4*mean(x^2+y^2 <= 1)
}
plot(N, pi_hat, type = "b", pch = 19, xlab = "number of samples", ylab = "estimated value")
title("estimation of pi as a function de n")
grid()
abline(h = pi, lty = 2, col = 2)

## ----fig=TRUE------------------------------------------------------------

N = seq(50, 1000, by=50)
m  = 100
pi_hat = matrix(0, nrow = m, ncol = length(N))
for(i in 1:length(N)){
  pi_hat[,i] = replicate(m, expr = {
          x = runif(N[i], 0, 1)
          y = runif(N[i], 0, 1)
        4*mean(x^2+y^2 <= 1)
  })  
}
boxplot(pi_hat, names = N, las = 2, xlab = "number of samples", ylab = "estimated value")
title("estimation of pi - variabily")
grid()
abline(h = pi, lty = 2, col = "red")


## ------------------------------------------------------------------------
#############################
#### STARTING EXERCICE 5 ####
#############################


# 1) via simulation  de loi uniforme
m = 1000
x = runif(m)
Ihat.1 = mean( cos(x^3)*exp(-x) )
cat("estimation based on uniform samples =", Ihat.1, "\n")

## ------------------------------------------------------------------------
# 2) via loi exponentielle
y = rexp(m)
Ihat.2 = mean(cos(y^3)*(y<=1))
cat("estimation based on eponential samples =", Ihat.2, "\n")



## ------------------------------------------------------------------------
#############################
#### STARTING EXERCICE 6 ####
#############################

n = 1000
x = runif(n, 0, 1)
I.hat = mean( sin(sqrt(x)) )
cat("estimated value based on  1000 samples =", I.hat, "\n")

## ----fig=TRUE------------------------------------------------------------

N = seq(100, 2000, by = 100)
alpha = 0.05
I.hat = c()
I.conf = c()
for(n in N){
  x = runif(n, 0,1)
  gx = sin(sqrt(x))
  I = mean( gx )
  I1 = I - qnorm(1-0.5*alpha)*sqrt(var(gx)/n)
  I2 = I + qnorm(1-0.5*alpha)*sqrt(var(gx)/n)
  # store
  I.hat = c(I.hat,I)
  I.conf = cbind(I.conf, c(I1,I2))
}
plot(N, I.hat, ylim = range(I.conf), xlab = "number of samples", ylab = "estimated value", pch = 19)
title("Estimated value + 95% confidence interval as a function of n")
grid()
arrows(N, I.conf[1,], N, I.conf[2,], length = 0.1, angle = 90, code = 3)

