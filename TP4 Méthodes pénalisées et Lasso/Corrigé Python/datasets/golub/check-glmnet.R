
library(glmnet)
X = read.table("X.txt")
y = read.table("y.txt")$V1

X = as.matrix(X)

# fit models
fit.lasso = glmnet(X, y, family = "binomial")
fit.ridge = glmnet(X, y, family = "binomial", alpha = 0)

# plot
pdf("reg-paths_R.pdf")
plot(fit.lasso)
plot(fit.ridge)
dev.off()



# build (fake) multinomial problems
set.seed(27)
ind.new = sample(length(y), round(length(y)/3))
y_mult = (y+1)/2
y_mult[ind.new] = 2

fit.mult = glmnet(X, y_mult, family = "multinomial")
## --> fit.mult$beta = list of length 3 (number of classes) 
preds.mult = predict(fit.mult, X)
## --> preds.mult = array of size 72 x 3 x 100 (--> n x K x n_lambda)


