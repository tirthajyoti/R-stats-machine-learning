prob.die <- c(0.05,0.05,0.05,0.05,0.4,0.4)

CLT.Die <- function (n){
    a=integer()
    for (i in 1:n){
        a[i]=sample(1:6,size=1, prob = prob.die, replace = T)
    }
    m <- mean(a)
    s <- sd(a)
    result <- (m-3.5)/(1.71/sqrt(n))
    return (result)
}

ntimes<-2000
nroll<- 100

clt<-c()

for (i in 1:ntimes){
    clt[i]<- CLT.Die(nroll)
}

par(mfrow=c(1,2))
hist(clt, col='green', border='black')
plot(density(clt), lwd=3)

s.clt <- sd(clt)
m.clt <- mean(clt)

print(paste ("SD of CLT: ", round(s.clt,3)))
print(paste ("Mean of CLT: ", round(m.clt,3)))