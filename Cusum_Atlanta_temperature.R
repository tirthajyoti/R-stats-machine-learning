# Read in the data and set proper column names
temp<-read.csv("temps.txt",header = TRUE,sep = "")
colnames(temp)<-c("day",seq(1996,2015))
temp$date<-as.Date(temp$day,"%d-%b")
temp$day<-NULL

# Use mean of each column as C in the cumsum approach


# Create a scaled version of the data with mean subtracted (but not standardized)
temp.scaled<-as.data.frame(scale(temp[,-21],scale = F))

for (c in colnames(temp.scaled)){
    newcol<-paste("cumsum.",c,sep="")
    temp.scaled[newcol]<-cumsum(temp.scaled[c])
}

temp.scaled<-cbind(day=temp$date,temp.scaled)

# Create a cumsum only df and attach the argmax values at the bottom
cols<-paste(rep('cumsum',20),seq(1996,2015),sep=".")
df.cumsum<-temp.scaled[cols]

# Use sapply with which.max to determine the day at which max is reached for the cumsum
max.cumsum<-sapply(df.cumsum,which.max)
print(mean(max.cumsum))

# Plot the cumsum values (with dates as x-axis) for all the years
par(mfrow=c(1,1))
for (c in colnames(df.cumsum)){
    plot(x=temp$date,y=df.cumsum[,c],ylab=c,xlab = "Date",
         main=paste("Cumsum for year",substr(c,8,11)))
} 
df.cumsum<-rbind(df.cumsum,max.cumsum)

for (i in 1:20){
    date<-paste("01-July-",as.character(1995+i),sep="")
    date<-as.Date(date,"%d-%b-%Y")
    summer.ends<-date+as.numeric(df.cumsum[124,i])
    print(summer.ends)
}

# Is there an increase of mean temperature over the years?
mean.temp<-colSums(temp[,-21])
mean.temp<-mean.temp/123
mean.temp<-as.vector(mean.temp)
mean.temp
lm.mean.temp<-lm(mean.temp~seq(1996:2015))
summary(lm.mean.temp)
plot(1996:2015,mean.temp,pch=19,cex=2,col='red',
     xlab="Years (1996-2015)",ylab="Mean summer temperature each year",
     main="Plot of mean summer temperature over the years (1996-2015)",
     cex.main=1.5,cex.axis=1.25,cex.lab=1.25)

abline(lm.mean.temp<-lm(mean.temp~seq(1996:2015)),lwd=3)

mean.cumsum<-cumsum(mean.temp-mean(mean.temp))
plot(1996:2015,mean.cumsum,pch=19,cex=2,col='red',
     xlab="Years (1996-2015)",ylab="Cumsum of mean temperature each year",
     main="Plot of cumsum of mean temperature over the years (1996-2015)",
     cex.main=1.5,cex.axis=1.25,cex.lab=1.25)
abline(a=0,b=0,lwd=2,col='blue')



