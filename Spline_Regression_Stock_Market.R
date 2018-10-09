# Time period and granularity of measurement
n<-52*5+1; time.p <- 52
# Base price (starting) and std.dev (measure of day to day variation)
base.price <- 10
std.dev <- 1.0
mean.noise=0
# Synthesize the data 
# With seasonality (sinusoidal) and long-term pattern (logarithimic increase)
x<- seq(0,time.p, length.out=n)
y.nonoise= base.price+sin(2*pi*x/12)+0.5*cos(2*pi*x/12+0.5)+log(x+0.1)
y<- y.nonoise+rnorm(n,mean=mean.noise, sd=std.dev)

plot(x,y,tck=1, lab=c(10,10,5), main = "Day to day price variation",
     xlab="Week on stock market",ylab="Day to day price",cex=1.5,pch=20)
lines(x,y)

# Play with knot length
# Higher the knot length, more flexible the fit is but with higher variance
knot.length<-100
knot.f <- seq(0,time.p,length=knot.length)
splineTerms <- sapply(knot.f, function (k){(x>k)*(x-k)^2})

# Binding the spline terms with regular quadratic regression terms
xMat <- cbind(1,x,x^2,splineTerms)
df.spline<- data.frame(y,xMat)

# Now fit linear regression model including the spline terms
fit.spline<- lm(y~.-1, data=df.spline)

# Prediction
yhat.spline<- predict(fit.spline)

rss.spline<- (sum((y-yhat.spline)^2))
rmse.spline<- sqrt(rss.spline/n)
print(paste("RSS (Spline): ", rss.spline))
print(paste("RMSE (Spline): ", rmse.spline))
subtitle.spline = paste('RMSE with a quadratic spline', toString(rmse.spline))

plot(x,y,pch=21, cex=1.5, col='blue',tck=1, lab=c(10,10,5),bg='lightblue',
     xlab='Week on the stock market',
     ylab='Stock price',
     main=paste('Stock price over 52-week period fitted with a quadratic spline',
                '\n\n',subtitle.spline))
lines(x,yhat.spline,col='red',lwd=3)

plot(x,y.nonoise,pch=20,main="How well the spline fit approximates the True function",
     ylab = "True function (no noise)",
     xlab='Data points')
lines(x,yhat.spline,col='red',lwd=2,lty=2)

# data frame with only y and x variables
df.simple<- data.frame(y,x)

# Now fit linear regression model including the spline terms
fit.poly1<- lm(y~., data=df.simple)

# Prediction
yhat.poly1<- predict(fit.poly1)

rss.poly1<- (sum((y-yhat.poly1)^2))
rmse.poly1<- sqrt(rss.poly1/n)
print(paste("RSS (linear): ", rss.poly1))
print(paste("RMSE (linear): ", rmse.poly1))
subtitle.poly1 = paste('RMSE with a linear model', toString(rmse.poly1))

plot(x,y,pch=21, cex=1.5, col='blue',tck=1, lab=c(10,10,5),bg='lightblue',
     xlab='Week on the stock market',
     ylab='Stock price',
     main=paste('Stock price over 52-week period fitted with a linear model',
                '\n\n',subtitle.poly1))
lines(x,yhat.poly1,col='red',lwd=3)