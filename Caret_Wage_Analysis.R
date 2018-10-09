library(ISLR); library(ggplot2); library(caret)
data(Wage)
Wage <- subset(Wage, select = -c(logwage))

# Test/Train split
inTrain<- createDataPartition(y=Wage$wage,p=0.7,list=F)
training <- Wage[inTrain,]
test<- Wage[-inTrain,]

# Plots
# qplot(age,wage,data=training, col=jobclass, cex=jobclass)
# qplot(education,wage,data=training, col=jobclass, cex=jobclass)
# Wage cut-points
cutwage <- cut(training$wage,breaks=4, 
               labels = c("Low.wage", "Medium.wage", "High.wage", "Super.High.wage"))
training$wageBracket <- cutwage
boxplot(training$age~ training$wageBracket, col=c(1,2,3,4))
t1<- table(cutwage, training$jobclass)
#plot(t1, col=training$jobclass)
# Density plot
print(qplot(wage, colour=education, data=training,geom="density"))

# Dummy Variables
dummies<- dummyVars(wage~jobclass+education, data=training)
dummycols<- predict(dummies, newdata=training)
training<- cbind(training, dummycols)

# Near-zero variables
nsv<- nearZeroVar(training,saveMetrics = T, names=T)
nsvNames<- rownames(subset(nsv, nsv$nzv==TRUE))
training[,nsvNames]<- list(NULL)

# B-Splines (polynomial knots)
library(splines)
bsBasis <- bs(training$age, df=3)
lm1<- lm(wage~bsBasis, data=training)
par(mfrow=c(1,2))
plot(training$age, training$wage, col=training$jobclass,pch=19, cex=0.5)
points(training$age, predict(lm1, newdata=training), col='blue', lwd=2)
plot(training$age, training$wage, col=training$education,pch=19, cex=0.5)
points(training$age, predict(lm1, newdata=training), col='blue', lwd=2)

# Multivariate regression fit with Caret function
fit <- train(wage~year+jobclass+education+age+race, data=training, method='lm')
finmod<- fit$finalModel

# Residulas plot
par(mfrow=c(1,3))
plot(finmod$fitted.values, finmod$residuals, col=training$race)
plot(finmod$fitted.values, finmod$residuals, col=training$education)
plot(finmod$fitted.values, finmod$residuals, col=training$jobclass)

par(mfrow=c(1,3))
plot(finmod$residuals,pch=19, cex.axis=1.5, cex.lab=1.5, ylab="Model Residuals")
hist(finmod$residuals, ylab="Count (residuals)", xlab="Model Residuals",
     breaks=as.integer(2*length(finmod$residuals)^(1/3)), cex.axis=1.5,cex.lab=1.5)
qqnorm(y=finmod$residuals, ylab = "Model Residuals", cex.axis=1.5,cex.lab=1.5)
