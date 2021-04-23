
tab = read.csv("leukemia_small.csv")


tt = colnames(tab)
y = rep(1, length(tt))
y[grep("AML",tt)] = -1

X = t(tab)
write.table(X, file = "X.txt", row.names = F, col.names = F, quote = F)
write.table(y, file = "y.txt", row.names = F, col.names = F, quote = F)

