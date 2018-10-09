# Read in the text file
df<-read.csv("uscrime.txt",header = TRUE, sep = "")
summary(df)

par(pch="O",cex=2,col="Blue",mfrow=c(2,2))
for (i in 1:4){
  hist(df[,i],main=paste("Histogram: ",colnames(df)[i]),col="Blue",border = "Black")
}

for (i in 5:8){
  hist(df[,i],main=paste("Histogram: ",colnames(df)[i]),col="Blue",border = "Black")
}

for (i in 9:12){
  hist(df[,i],main=paste("Histogram: ",colnames(df)[i]),col="Blue",border = "Black")
}

for (i in 13:16){
  hist(df[,i],main=paste("Histogram: ",colnames(df)[i]),col="Blue",border = "Black")
}

# Change the plot grid and colors
par(pch="O",cex=2,col="orange",mfrow=c(2,2))

for (i in 1:4){
  plot(df[,i],df$Crime,xlab=colnames(df)[i],ylab="Crime rate",
       main=paste("Scatter plot and best fit line for:",colnames(df)[i]))
  string<-paste("Crime",colnames(df)[i],sep = " ~ ")
  formula<-as.formula(string)
  reg<-lm(formula,data=df)
  abline(reg,lwd=2,lty="dashed",col='black')
}

for (i in 5:8){
  plot(df[,i],df$Crime,xlab=colnames(df)[i],ylab="Crime rate",
       main=paste("Scatter plot and best fit line for:",colnames(df)[i]))
  string<-paste("Crime",colnames(df)[i],sep = " ~ ")
  formula<-as.formula(string)
  reg<-lm(formula,data=df)
  abline(reg,lwd=2,lty="dashed",col='black')
}

for (i in 9:12){
  plot(df[,i],df$Crime,xlab=colnames(df)[i],ylab="Crime rate",
       main=paste("Scatter plot and best fit line for:",colnames(df)[i]))
  string<-paste("Crime",colnames(df)[i],sep = " ~ ")
  formula<-as.formula(string)
  reg<-lm(formula,data=df)
  abline(reg,lwd=2,lty="dashed",col='black')
}

for (i in 13:15){
  plot(df[,i],df$Crime,xlab=colnames(df)[i],ylab="Crime rate",
       main=paste("Scatter plot and best fit line for:",colnames(df)[i]))
  string<-paste("Crime",colnames(df)[i],sep = " ~ ")
  formula<-as.formula(string)
  reg<-lm(formula,data=df)
  abline(reg,lwd=2,lty="dashed",col='black')
}

# Build a linear regression model with Crime as the response and all other features as predictors
linear.model <- lm(Crime~.,data=df)

# Summary of the linear model
summary(linear.model)

# AIC and BIC of the model
cat("AIC of the model: ",AIC(linear.model))
cat("\n")
cat("BIC of the model: ",BIC(linear.model))

# Residual and fitted vs true plots
plot(df$Crime,linear.model$fitted.values,main="True vs fitted response values",
     xlab="True crime rate",ylab="Fitted crime rate",pch=20,cex=2,col='blue')

hist(linear.model$residuals,col="orange",border="Black",
     main="Histogram of the residuals",xlab="Residuals")

# Construct a numeric vector with the predictor variables needed
pred.vector<-c(14.0, 0, 10.0, 12.0, 15.5, 0.640, 94.0, 150, 1.1, 0.120, 3.6, 3200, 20.1, 0.04, 39.0, 0)

# Create a new dataframe by binding the prediction vector to the training data frame
df1<-rbind(df,pred.vector)

predict(linear.model,df1[48,1:15])

# A second model with only the significant parameters
linear.model2 <- lm(Crime~M+Ed+Ineq+Prob+Po1+U2,data=df)
summary(linear.model2)

cat("\n")
cat("AIC of the model: ",AIC(linear.model2))
cat("\n")
cat("BIC of the model: ",BIC(linear.model2))

# Residual and fitted vs true plots
plot(df$Crime,linear.model2$fitted.values,main="True vs fitted response values",
     xlab="True crime rate",ylab="Fitted crime rate",pch=20,cex=2,col='blue')

hist(linear.model2$residuals,col="orange",border="Black",
     main="Histogram of the residuals",xlab="Residuals")

# A third model leaving out the police expense variable
linear.model3 <- lm(Crime~M+Ed+Ineq+Prob+U2,data=df)
summary(linear.model3)

cat("\n")
cat("AIC of the model: ",AIC(linear.model3))
cat("\n")
cat("BIC of the model: ",BIC(linear.model3))

# Residual and fitted vs true plots
plot(df$Crime,linear.model3$fitted.values,main="True vs fitted response values",
     xlab="True crime rate",ylab="Fitted crime rate",pch=20,cex=2,col='blue')

hist(linear.model3$residuals,col="orange",border="Black",
     main="Histogram of the residuals",xlab="Residuals")
