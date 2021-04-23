########################
#### STARTING EXO 1 ####
########################
attach(chickwts)
boxplot(weight ~ feed, las = 2)
print(table(feed))
x = weight[feed == "soybean"]
y = weight[feed == "linseed"]
ttest.res = t.test(x,y)
t0 = ttest.res$statistic
p0 = ttest.res$p.value
cat("*** linseed vs soybean : p.val of standard t.test =", p0, "***\n")
cat("*** linseed vs soybean : test statistic =", t0, "***\n")
k = length(x)
N = length(x)+length(y)
comb = choose(N,k)
cat("*** total number of permutations =", comb, "***")
z = c(x,y)
set.seed(27)
B = 1000
t.perm = numeric(B)
t0 = t.test(x, y)$statistic
for(b in 1:B){
  z1 = sample(z, replace = F)
  x1 = z1[1:length(x)]
  y1 = z1[(length(x)+1):length(z1)]
  t.perm[b] = t.test(x1, y1)$statistic
}
t.perm = c(t.perm, t0)
hist(t.perm, breaks = 20, main = "distribution of test statistic obtained by permutation")
abline(v = t0, col = 2, lty = 2)
pval = mean(t.perm >= t0)
cat("*** linseed vs soybean : p.val obtained by permutation =", pval, "***\n")
B.list = seq(100,2000, by = 100)
pval.list = c()
t0 = t.test(x, y)$statistic
for(B in B.list){
  t.perm = numeric(B)
  for(b in 1:B){
    z1 = sample(z, replace = F)
    x1 = z1[1:length(x)]
    y1 = z1[(length(x)+1):length(z1)]
    t.perm[b] = t.test(x1, y1)$statistic
  }
  t.perm = c(t.perm, t0)
  pval.list = c(pval.list, mean(t.perm >= t0))
}

plot(B.list, pval.list, xlab = "B", ylab = "p.value", main = "permutation p.value vs B", type = "b", pch = 19)
grid()
x = weight[feed == "sunflower"]
y = weight[feed == "linseed"]
ttest.res = t.test(x,y)
t0 = ttest.res$statistic
p0 = ttest.res$p.value
cat("*** linseed vs sunflower : p.val of standard t.test =", p0, "***\n")
z = c(x,y)
set.seed(27)
B = 1000
t.perm = numeric(B)
t0 = t.test(x, y)$statistic
for(b in 1:B){
  z1 = sample(z, replace = F)
  x1 = z1[1:length(x)]
  y1 = z1[(length(x)+1):length(z1)]
  t.perm[b] = t.test(x1, y1)$statistic
}
t.perm = c(t.perm, t0)
hist(t.perm, breaks = 20, main = "distribution of test statistic obtained by permutation")
abline(v = t0, col = 2, lty = 2)
pval = mean(t.perm >= t0)
cat("*** linseed vs sunflower : permutation p.val =", pval, "***\n")
########################
#### STARTING EXO 2 ####
########################
rm(list = ls())
x = c(8,9,19,6,8,10)
chitest = chisq.test(x)
p0 = chitest$p.value
x0 = chitest$statistic
cat("*** pval obtained by standard test =", p0, "***\n")
B = 1000
x.perm = numeric(B)
for(b in 1:B){
  x1 = table( sample(c(1:6), sum(x), prob = rep(1/6,6), replace = T) )
  x.perm[b] = chisq.test(x1)$statistic
}
x.perm = c(x.perm, x0)
hist(x.perm, breaks = 20, main = "distribution of test statistic obtained by permutation")
abline(v = x0, col = 2, lty = 2)
pval = mean(x.perm >= x0)
cat("*** pval obtained by permutation procedure =", pval, "***\n")
########################
#### STARTING EXO 3 ####
########################
library(bootstrap)
x = law$LSAT
y = law$GPA
plot(x, y, pch = 19, xlab = "LSAT", ylab = "GPA", main = "law datasaet")
c0 = cor(x,y)
cat("*** correlation observed =", c0, "***\n")
set.seed(27)
B = 1000
c.perm = numeric(B)
for(b in 1:B){
  y1 = sample(y, replace = F)
  c.perm[b] = cor(x, y1)
}
c.perm = c(c.perm, c0)
hist(c.perm, breaks = 20, main = "distribution of correlation obtained by permutation")
abline(v = c0, col = 2, lty = 2)
pval = mean(c.perm >= c0)
cat("**** pval obtained by permutation =", pval, "***\n")
########################
#### STARTING EXO 4 ####
########################
mu0 = 0
sigma0 = 1
mu1 = 2
sigma1 = 1

B = 1000
n = 10
nrep = 100
set.seed(27)

PVAL = list()
PVAL.T = list()
sigma.list = c(1,2,5,10)
for(sigma1 in sigma.list){
  cat("*** processing sigma1 =", sigma1, "***\n")
  pval = numeric(nrep)
  pval.t = numeric(nrep)
  for(r in 1:nrep){
    x = rnorm(n, mu0, sigma0)
    y = rnorm(n, mu1, sigma1)
    z = c(x,y)
    t.perm = numeric(B)
    t0 = t.test(x, y)$statistic
    for(b in 1:B){
      ind = sample(length(z), n)  
      x1 = z[ind]
      y1 = z[-ind]
      t.perm[b] = t.test(x1,y1)$statistic
    }
    t.perm = c(t.perm,t0)
    pval[r] = mean(t.perm <= t0)  # NB : here, we know that x is smaller
    pval.t[r] = t.test(x, y, alternative = "less")$p.value # same --> use one-side
  }
  PVAL[[as.character(sigma1)]] = pval
  PVAL.T[[as.character(sigma1)]] = pval.t
}
# show pvalues
par(mfrow = c(1,2))
boxplot(PVAL, xlab = "sigma1", ylab = "p-value", main = "p-values obtained by permutation")
abline(h = 0.05, col = 2, lty = 2, lwd = 2)
boxplot(PVAL.T, xlab = "sigma1", ylab = "p-value", main = "p-values obtained by t-test")
abline(h = 0.05, col = 2, lty = 2, lwd = 2)
par(mfrow = c(1,1))
# show power
po.perm = sapply(PVAL, function(x){mean(x<0.05)})
po.t = sapply(PVAL.T, function(x){mean(x<0.05)})
plot(sigma.list, po.perm, ylim = c(0,1), type = "b", col = 1, pch = 19, xlab = "sigma", ylab = "power", main = "power obtained")
lines(sigma.list, po.t, type = "b", col = 2, pch = 19)
legend("topright", c("permutation","t-test"), col = c(1,2), pch = 19)
