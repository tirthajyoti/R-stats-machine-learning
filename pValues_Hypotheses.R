#set.seed(1234)

#Ntimes<- 1000

sumP<- function (Ntimes, alpha=0.05){
    pValues <- rep(NA, Ntimes)
    for (i in 1:Ntimes){
        x<- runif(20)
        y<- runif(20)
        pValues[i]<- summary(lm(y~x))$coeff[2,4]
    }
    return (sum(pValues< alpha))
}

par(mfrow=c(1,2))

v<- seq(10,200,10)
s<- sapply(v,sumP,0.05)

plot(v,s,pch=19, xlab = "Number of Hypotheses", ylab = "Number of statistically-significant P-values",
     main = "Significant P-values with 95% confidence interval",
     col='red', cex=2, tck=1, lab=c(10,10,7))
abline(reg = lm(s~v), lwd=3)

smax <- max(s)

v<- seq(10,200,10)
s<- sapply(v,sumP,0.01)

plot(v,s,pch=19, xlab = "Number of Hypotheses", ylab = "Number of statistically-significant P-values",
     main = "Significant P-values with 99% confidence interval",
     col='red', cex=2, tck=1, lab=c(10,10,7), ylim = c(0,smax))
abline(reg = lm(s~v), lwd=3)