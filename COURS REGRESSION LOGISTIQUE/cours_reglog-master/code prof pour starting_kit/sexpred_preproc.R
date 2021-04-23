# Mini questionnaire 1 
# https://forms.gle/Aig59t5msYPPa9Re9



d = readRDS("data_learn.rds")
head(d[,1:19])
table(d$sex, useNA="ifany")
table(d$histology, useNA="ifany")
table(d$sex,d$histology, useNA="ifany")
table(d$tissue_status, useNA="ifany")



# definme sex as 0 or 1 
s = as.numeric(d$sex == "M")
s

# get gene names
gs = colnames(d)[13:1012]
g = gs[1]
d[[g]]
g = "ATP10B"


layout(matrix(1:2, 1), respect=TRUE)
boxplot(d[[g]]~s)
plot(d[[g]], s, main=paste0("sex~",g), xlab=g, ylab="sex")
# P(Y=1|X) = logitinv(a + b.x)
m = glm(s~d[[g]], family = binomial(logit))
summary(m)$coefficients
pv = summary(m)$coefficients[2,4]
logitinv = function(x) 1/(1 + exp(-x))
x = min(d[[g]]):max(d[[g]])
lines(x, logitinv(m$coefficients[[1]] + m$coefficients[[2]]*x), col=2, lwd=2)
legend("bottomright", "logit(Y)=b.X", col=2, lty=1, cex=0.6)



pvs = sapply(gs, function(g) {
  print(g)
  m = glm(s~d[[g]], family = binomial(logit))
  b = m$coefficients[2]
  pv = summary(m)$coefficients[2,4]
  c(pv,b)
})

layout(matrix(1:2, 1), respect=TRUE)
g = "SNORA8"
plot(d[[g]], s, main=paste0("sex~",g), xlab=g, ylab="sex")
plot(c(d[[g]],0,max(d[,gs]),0,max(d[,gs])), c(s,0,0,1,1), main=paste0("sex~",g), xlab=g, ylab="sex")

res = sapply(gs, function(g) {
  print(g)
  m = glm(c(s,0,0,1,1)~c(d[[g]],0,max(d[,gs]),0,max(d[,gs])), 
    family = binomial(logit))
  b = m$coefficients[[2]]
  pv = summary(m)$coefficients[2,4]
  c(pval = pv,beta = b)
})

res = t(res)
res = as.data.frame(res)
head(res)

layout(matrix(1:2, 1), respect=TRUE)
plot(res$beta, -log10(res$pval), main="volcano plot")
text(res$beta, -log10(res$pval), rownames(res))
plot(-log10(res$pval), main="Manhattan plot")
text(res$beta, -log10(res$pval), rownames(res))
rownames(res)[which(res$pval == min(res$pval))]

plot(res$beta, -log10(res$pval), main="volcano plot")
text(res$beta, -log10(res$pval), rownames(res))



g = "USP9Y"

layout(matrix(1:2, 1), respect=TRUE)
boxplot(d[[g]]~s)
plot(d[[g]], s, main=paste0("sex~",g), xlab=g, ylab="sex")
# P(Y=1|X) = logitinv(a + b.x)
m = glm(s~d[[g]], family = binomial(logit))
summary(m)$coefficients
pv = summary(m)$coefficients[2,4]
logitinv = function(x) 1/(1 + exp(-x))
x = min(d[[g]]):max(d[[g]])
lines(x, logitinv(m$coefficients[[1]] + m$coefficients[[2]]*x), col=2, lwd=2)
legend("bottomright", "logit(Y)=b.X", col=2, lty=1, cex=0.6)





g = "LINC02428"

layout(matrix(1:2, 1), respect=TRUE)
boxplot(d[[g]]~s)
plot(d[[g]], s, main=paste0("sex~",g), xlab=g, ylab="sex")
# P(Y=1|X) = logitinv(a + b.x)
m = glm(s~d[[g]], family = binomial(logit))
summary(m)$coefficients
pv = summary(m)$coefficients[2,4]
logitinv = function(x) 1/(1 + exp(-x))
x = min(d[[g]]):max(d[[g]])
lines(x, logitinv(m$coefficients[[1]] + m$coefficients[[2]]*x), col=2, lwd=2)
legend("bottomright", "logit(Y)=b.X", col=2, lty=1, cex=0.6)














