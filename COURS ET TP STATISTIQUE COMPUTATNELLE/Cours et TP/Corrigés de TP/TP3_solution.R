#############################
####### STARTING EXO 1 ######
#############################

install.packages("bootstrap")
library(bootstrap)

set.seed(27)

# on trace les variables

x = law$LSAT
y = law$GPA
n = length(x)

plot(x, y, pch = 19, xlab = "LSAT", ylab = "GPA", main = "LAW dataset - GPA vs LSAT")

# on calcule la corrélation sur l'échantillon

theta = cor(x,y)
cat("*** sample correlation =", theta, "***\n")

# proc\'edure bootstrap

B = 200
theta.b = numeric(B)
for(b in 1:B){
    ind = sample(c(1:n), n, replace = TRUE)
    theta.b[b] = cor(x[ind],y[ind])
}

# compute bias & SE

bias = mean(theta.b) - theta
se = sd(theta.b)
cat("** bias =", round(bias, digits=5), "standard-error =", round(se, digits = 3), "**\n*")

# show distribution

hist(theta.b, main = "bootstrap estimates of correlation")

# compute confidence intervals

alpha = 0.05
    
# percentile method

q1 = quantile(theta.b, alpha/2)
q2 = quantile(theta.b, 1-alpha/2)
Iperc = c(q1,q2)
cat("percentile confidence interval = [", Iperc[1], ";", Iperc[2], "]\n")

# basic method

I1 = 2*theta - q2
I2 = 2*theta - q1
Ibasic = c(I1,I2)
cat("basic confidence interval = [", Ibasic[1], ";", Ibasic[2], "]\n")

# plot

hist(theta.b, col = "lightblue", border = "white", xlim = range(c(theta.b,Iperc,Ibasic)))
abline(v = Ibasic,lty = 2, lwd = 2, col = "red")
abline(v = Iperc,lty = 2, lwd = 2, col = "blue")
legend("topleft", c("red","blue"), c("IC-basic","IC-perc"), col = c("red","blue"),lty =2, lwd = 2, bg = "white")
abline(v = theta, lwd = 2)
mtext("sample estimate",  side = 1, at = theta)
box()


#############################
####### STARTING EXO 2 ######
#############################


install.packages("boot")
library(boot)

# define a function to compute the statistic

my.cor = function(x,ind){
  return( cor(x[ind,1],x[ind,2]) )
}

# call the bootstrap procedure

X = cbind(x,y)
res = boot(data = X, statistic = my.cor, R = B)

# print  results

print(res)

# compute confidence intervals

res.ci= boot.ci(res, type = c("basic","perc","norm"))

# print results

print(res.ci)


#############################
####### STARTING EXO 3 ######
#############################


# show dataset and fit global linear model

fit.global = lm(dist~speed, data = cars)
plot(cars, pch = 19, main = "cars dataset - linear regression")
abline(fit.global, col = "red", lwd = 2)

# carry out bootstrap procedure #
#-------------------------------#

B = 500
n = nrow(cars)
par(mfrow = c(1,2))

# 1) from pairs

plot(cars, main = "cars dataset - pairs-bootstrap regression")
beta.pairs = c()

for(b in 1:B){
  # get model coefficients
  ind = sample(c(1:n), n, replace = TRUE)
  x = cars$speed[ind]
  y = cars$dist[ind]
  fit.b = lm(y~x)
  # store coefficients
  beta.pairs = rbind(beta.pairs, fit.b$coefficients)
  # show fit for illustration purposes
  abline(fit.b, col = "grey")
}
points(cars, pch = 19)
abline(fit.global, col = "red", lwd = 2)
box()

# 2) from residuals

x = cars$speed
resid = fit.global$residuals
fitted = fit.global$fitted
plot(cars, main = "cars dataset - resid-bootstrap regression")
beta.resid = c()

for(b in 1:B){
  # pick residuals
  ind = sample(c(1:n), n, replace = TRUE)
  y = fitted + resid[ind]
  # fit model
  fit.b = lm(y~x)
  # store coefficients
  beta.resid = rbind(beta.resid, fit.b$coefficients)
  # show fit for illustration purposes
  abline(fit.b, col = "grey")
}
points(cars, pch = 19)
abline(fit.global, col = "red", lwd = 2)
box()

# get confidence intervals #
#--------------------------#
# boostrap approaches

ci.pairs = apply(beta.pairs, 2, quantile, probs = c(0.025,0.975))
ci.resid = apply(beta.resid, 2, quantile, probs = c(0.025,0.975))

# get "standard" confidence intervals

ci.lm = confint(fit.global, level = .95)
par(mfrow = c(1,2))

# intercept

for(i in 1:2){
  plot(density(beta.pairs[,i]), main = paste("coefficient no", i))
  grid()
  lines(density(beta.resid[,i]), col  = 2)
  abline(v = fit.global$coefficients[i], col = "blue", lwd = 2)
  abline(v = ci.pairs[,i], col = 1, lty = 2)
  abline(v = ci.resid[,i], col = 2, lty = 2)
  abline(v = ci.lm[i,], col = "blue", lty = 2)
}
plot(density(resid), main = "distribution of residuals\n of the global model")
rug(resid)

# boostrap  procedure

B = 500
x.test = 21
preds = numeric(B)
n = nrow(cars)
plot(cars, main = "cars dataset - bootstrapped regression")

for(b in 1:B){
  ind = sample(c(1:n), n, replace = TRUE)
  x = cars$speed[ind]
  y = cars$dist[ind]
  fit.b = lm(y~x)
  abline(fit.b, col = "grey")
  # compute prediction
  preds[b] = fit.b$coefficients[1] + x.test*fit.b$coefficients[2]
}
points(cars, pch = 19)
abline(fit.global, col = "red", lwd = 2)
box()
abline(v = x.test, lty = 2, lwd = 2)

# compute global prediction

pred.global = fit.global$coefficients[1] + x.test*fit.global$coefficients[2]
hist(preds, prob = TRUE, main = "distribution of predictions", xlab = "predicted value", col = "lightblue", border = "white")
lines(density(preds), lwd = 2)
abline(v = pred.global, col = 2, lwd = 2)
mtext("global model",  side = 1, at = pred.global, col = 2)

# same procedure with standard CI

B = 2000
x.test = 21
n = nrow(cars)
plot(cars, main = "cars dataset - bootstrapped regression")

for(b in 1:B){
  ind = sample(c(1:n), n, replace = TRUE)
  x = cars$speed[ind]
  y = cars$dist[ind]
  fit.b = lm(y~x)
  abline(fit.b, col = "grey")
}
points(cars, pch = 19)
abline(fit.global, col = "red", lwd = 2)
box()

I = predict(fit.global, interval = "confidence", level = 0.95, newdata = data.frame(speed=0:30))
lines(0:30,I[,2], col = "blue", lwd = 2)
lines(0:30,I[,3], col = "blue", lwd = 2)
