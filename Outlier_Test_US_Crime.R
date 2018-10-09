# Load the crime data from the given text file
crime<-read.csv("uscrime.txt",header = TRUE,sep = "")

par(mfrow=c(2,1))
# Plot distribution of the last column crime per 100,000 people
hist(crime$Crime,breaks=10,main="Histogram of the crimes per 100,000 people",
     density = 50,col='orange',border = 'black',xlim = c(0,2000),xlab="Crime/100,000 people")
# Also plot the kernel density estimation
plot(density(crime$Crime),lwd=3,col='blue',main="Kernel density estimate of the crimes per 100,000 people")

# Calculate and print the Tuckey five-number summary and Inter-quartile range of the crime data
fn<-fivenum(crime$Crime)
print(paste("Minimum:",fn[1]))
print(paste("25th percentile:",fn[2]))
print(paste("50th percentile (median):",fn[3]))
print(paste("75th percentile:",fn[4]))
print(paste("Maximum:",fn[5]))
print(paste("Inter-quartile range:",fn[4]-fn[2]))

# Calculate and print mean and standard deviation of the crime data
print(paste("Mean of the crime data:",mean(crime$Crime)))
print(paste("Standard deviation of the crime data:",sd(crime$Crime)))

# So, how far is the maximum data point from the mean?
distance.max <- (max(crime$Crime)-mean(crime$Crime))/sd(crime$Crime)
print(paste("Distance (in terms of std dev) of the max data point from the mean:",distance.max))

# Load the "outliers" package and run grubbs.test on the crime data
library("outliers")
m1<-grubbs.test(crime$Crime,type=10)
print(m1)

# Two-sided test
m2<-grubbs.test(crime$Crime,type=11)
print(m2)

# Grubb's test does not seem to indicate there is an outlier
# Let's build a linear regression model out of the whole dataset except that point

lm.model <-lm(Crime~.,data = crime[-26,])
summary(lm.model)

# Now we try to predict the crime rate for that row using this model...
predict.lm(lm.model,crime[26,])
