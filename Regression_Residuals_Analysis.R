Ndata<-100
x<- runif(Ndata,0,5)
y<- 0.5*x+(0.5*rnorm(Ndata,0.1,1))

print(cor(y,x)^2)

fit <- lm(y~x)
s<- summary(fit)
r<- fit$residuals
print(s$r.squared)

par(mfrow=c(2,2))
plot(x,y,pch=19,cex=1.5,tck=1, lab=c(10,10,5),
     xlim=c(0,5), cex.lab=1.5)
abline(fit,lwd=3, lty=2, col='blue')

plot(x,r,cex=1.5, tck=1, lab=c(10,10,5), 
     xlim=c(0,5), col='red',pch=19,
     ylab = "Residuals",cex.lab=1.5)
abline(0,0,lwd=3)

for (i in 1:length(r)){
    lines(c(x[i],x[i]), c(0,r[i]), lwd=3, col='red')
}

hist(r, breaks=Ndata/5, col='green', main = "Histogram of residuals",
     xlim = c(min(r),max(r)), cex.lab=1.5,
     xlab = "Residuals")
plot(density(r), lwd=3, main = "Density plot of residuals", 
     tck=1, col='blue',cex.lab=1.5)