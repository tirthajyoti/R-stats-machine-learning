# Read the text file with na.string set to '?'
df <- read.csv("breast-cancer-wisconsin.data.txt",header=F,na.strings = '?')

# How many NA/missing values are present?
sum(is.na.data.frame(df))

library('mice')
md.pattern(df)

# Imputation
tempData <- mice(df,m=5,maxit=50,meth='pmm',seed=500)
summary(tempData)

densityplot(tempData)

x=as.vector(df$V7)
miss.mean = mean(x,na.rm = T)

# Imputation using mean
#==========================
# Function to replace a vector with its mean without missing values
NA2mean <- function(x) replace(x, is.na(x), mean(x, na.rm = TRUE))
# New dataframe by applying this function to the original dataframe
df.impute.mean<-as.data.frame(lapply(df, NA2mean))
# Check there is no missing value in the imputed dataframe
sum(is.na.data.frame(df.impute.mean))

# Imputation using regression
#==================================
imp1<-mice(df,method = 'norm.predict')
df.impute.regression <- complete(imp1)

# Imputation using regression with perturbation
#==================================================
imp2<-mice(df,method = 'norm.nob')
df.impute.regression.perturb <- complete(imp2)

# Density plots of imputed objects
#====================================
densityplot(imp1)
densityplot(imp2)

# Dataset by removing the missing values
df.removed<-na.omit(df)

# kNN classification using imputed datasets

library(caret)
ctrl <- trainControl(method="repeatedcv",number=10,repeats = 3)
knnFit.mean <- train(x=df.impute.mean[,1:10],y=as.factor(df.impute.mean[,11]), method = "knn", trControl = ctrl, 
                preProcess = c("center","scale"), tuneLength = 10)

plot(knnFit.mean,col = "dark red",lwd=5,lty=2,cex.lab=1.25,cex.main=1.5,
     main="Mean Imputed Dataset: Accuracy of kNN with repeated 10-fold CV",
     xlab="Number of neighbors",
     ylab="Accuracy of classification")

knnFit.reg <- train(x=df.impute.regression[,1:10],y=as.factor(df.impute.regression[,11]), method = "knn", trControl = ctrl, 
                     preProcess = c("center","scale"), tuneLength = 10)

plot(knnFit.reg,col = "dark red",lwd=5,lty=2,cex.lab=1.25,cex.main=1.5,
     main="Regression Imputed Dataset: Accuracy of kNN with repeated 10-fold CV",
     xlab="Number of neighbors",
     ylab="Accuracy of classification")

knnFit.perturb <- train(x=df.impute.regression.perturb[,1:10],y=as.factor(df.impute.regression.perturb[,11]), method = "knn", trControl = ctrl, 
                     preProcess = c("center","scale"), tuneLength = 10)

plot(x=knnFit.perturb,col = "dark red",lwd=5,lty=2,cex.lab=1.25,cex.main=1.5,
     main="Regression with Perturbation Imputed Dataset: Accuracy of kNN with repeated 10-fold CV",
     xlab="Number of neighbors",
     ylab="Accuracy of classification")

knnFit.removed <- train(x=df.removed[,1:10],y=as.factor(df.removed[,11]), method = "knn", trControl = ctrl, 
                        preProcess = c("center","scale"), tuneLength = 10)

plot(knnFit.removed,col = "dark red",lwd=5,lty=2,cex.lab=1.25,cex.main=1.5,
     main="Dataset with missing values removed: Accuracy of kNN with repeated 10-fold CV",
     xlab="Number of neighbors",
     ylab="Accuracy of classification")

# Errors of kNN classification with various imputed datasets
#================================================================
error1=100*(1-knnFit.mean$results$Accuracy)
error2=100*(1-knnFit.reg$results$Accuracy)
error3=100*(1-knnFit.perturb$results$Accuracy)
error4=100*(1-knnFit.removed$results$Accuracy)

# Plot the erros
plot(c(1:10),error1,type="l", lwd=2, col="blue", ylim=c(3, 4),
     xlab="Number of neighbors",
     ylab="Error of the classification")
lines(c(1:10),error2,lwd=2, col="red")
lines(c(1:10),error3,lwd=2, col="orange")
lines(c(1:10),error4,lwd=2, col="brown")
legend("topleft", legend=c("Mean impute","regression impute",
                           "Regression+Perturbation impute","Removed the missing values"), 
       lwd=c(2,2,2), col=c("blue","red","orange","brown"))



