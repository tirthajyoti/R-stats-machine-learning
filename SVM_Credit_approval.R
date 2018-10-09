# Reading the credit card data (with headers) with `read.csv` function
credit.data<-read.csv('credit_card_data-headers.txt',header = TRUE, sep = "")

summary(credit.data)

# Test/train split
# Create a temporary 'split' Boolean variable
library(caTools)
credit.data$spl<-sample.split(credit.data$R1,SplitRatio=0.6)

# Randomly split into test and train sets
credit.data.train<-subset(credit.data,credit.data$spl==TRUE)
credit.data.test<-subset(credit.data,credit.data$spl==FALSE)

# Split test set randomly into halves to create a VALIDATION set (for parameter tuning)
credit.data.test$spl2<-sample.split(credit.data.test$R1,SplitRatio=0.5)
credit.data.val<-subset(credit.data.test,credit.data.test$spl2==FALSE)
credit.data.test<-subset(credit.data.test,credit.data.test$spl2==TRUE)
    
# Drop the temporary 'split' variables
credit.data.train$spl<-NULL
credit.data.test$spl<-NULL
credit.data.test$spl2<-NULL
credit.data$spl<-NULL
credit.data.val$spl2<-NULL

# SUPPORT VECTOR MACHINE (SVM) CLASSIFICATION
# =============================================

library('kernlab')

# Calling `ksvm` from kernlab package with simple linear kernel, low penalty C=0.01, 10-fold cross validation
svm.model.linear.cv <- ksvm(x=as.matrix(credit.data.train[,1:10]),y=as.factor(credit.data.train[,11]),
                         scaled=T,type="C-svc",kernel="vanilladot",C=0.01,cross=10)

# Predictions on Training set
pred.svm.train <- predict(svm.model.linear.cv,credit.data.train[,1:10])

# Predictions on Test set with cross-validated model
pred.svm.test.cv <- predict(svm.model.linear.cv,credit.data.test[,1:10])

# Accuracy of Training set
accuracy.svm.train<-sum(pred.svm.train == credit.data.train[,11]) / nrow(credit.data.train)
cat("Accuracy (Training): ",accuracy.svm.train)

# Accuracy of Test set
accuracy.svm.test.cv<-sum(pred.svm.test.cv == credit.data.test[,11]) / nrow(credit.data.test)
cat("Accuracy (Test): ",accuracy.svm.test.cv)

#-------------------------------------------------------------------------
# Running a loop with various C-values 
# train with Training set and calculate accuracy with validation set
acc.svm.C<-c()
c.values<-10^(seq(-5,2,0.2))
for (c.val in c.values){
    svm.model <- ksvm(x=as.matrix(credit.data.train[,1:10]),
                      y=as.factor(credit.data.train[,11]),
                      scaled=TRUE,type="C-svc",kernel="vanilladot",C=c.val)
    pred.svm <- predict(svm.model,credit.data.val[,1:10])
    accuracy.svm<-sum(pred.svm == credit.data.val[,11]) / nrow(credit.data.val)
    acc.svm.C<-append(acc.svm.C,accuracy.svm)
}

# Plot the accuracy values for different C-values
plot(x=log10(c.values),y=acc.svm.C,type='b', 
     main="Accuracy for various C-values with linear kernel",
     xlab="C-values in Log10 scale",ylab="Accuracy",
     col = "dark red",lwd=5,lty=2,cex.lab=1.25,cex.main=1.5,
     ylim=c(0.0,1.0))
grid(col = "dark red", lty = "dotted",
     lwd = 2, equilogs = TRUE)

#-------------------------------------------------------------------------
# Choosing a different kernel function - radial basis function i.e. Gaussian
# train with Training set and accuracy with validation set
acc.svm.rbf<-c()
sigma.rbv<-10^(seq(-2,1,0.1))

for (s in sigma.rbv){
    svm.model <- ksvm(x=as.matrix(credit.data.train[,1:10]),
                      y=as.factor(credit.data.train[,11]),
                      scaled=TRUE,type="C-svc",kernel="rbfdot",C=1,kpar=list(sigma=s))
    pred.svm <- predict(svm.model,credit.data.val[,1:10])
    accuracy.svm<-sum(pred.svm == credit.data.val[,11]) / nrow(credit.data.val)
    acc.svm.rbf<-append(acc.svm.rbf,accuracy.svm)
}

# Plot the accuracy values for different sigma
plot(x=log10(sigma.rbv),y=acc.svm.rbf,type='b', 
     main="Accuracy for various  sigma values with RBF kernel",
     xlab="Sigma values in Log10 scale",ylab="Accuracy",
     col = "dark red",lwd=5,lty=2,cex.lab=1.25,cex.main=1.5,
     ylim=c(0.0,1.0))
grid(col = "dark red", lty = "dotted",
     lwd = 2, equilogs = TRUE)

#-------------------------------------------------------------------------
# Choosing a different kernel function - polynomial
# train with Training set and accuracy with validation set
acc.svm.degree<-c()
degree.values<-c(1,2,3,4)

for (d in degree.values){
  svm.model <- ksvm(x=as.matrix(credit.data.train[,1:10]),
                    y=as.factor(credit.data.train[,11]),
                    scaled=TRUE,type="C-svc",kernel="polydot",C=1,kpar=list(degree=d))
  pred.svm <- predict(svm.model,credit.data.val[,1:10])
  accuracy.svm<-sum(pred.svm == credit.data.val[,11]) / nrow(credit.data.val)
  acc.svm.degree<-append(acc.svm.degree,accuracy.svm)
}

# Plot the accuracy values for different sigma
plot(x=degree.values,y=acc.svm.degree,type='b', 
     main="Accuracy for various  degrees with polynomial kernel",
     xlab="Degrees in polynomial kernel",ylab="Accuracy",
     col = "dark red",lwd=5,lty=2,cex.lab=1.25,cex.main=1.5,
     ylim=c(0.0,1.0))
grid(col = "dark red", lty = "dotted",
     lwd = 2, equilogs = TRUE)

#-------------------------------------------------------------------------
# Tuning polynomial kernel
# train with Training set and accuracy with validation set
acc.svm.scale<-c()
scale.values<-seq(0.1,2,0.1)

for (s in scale.values){
  svm.model <- ksvm(x=as.matrix(credit.data.train[,1:10]),
                    y=as.factor(credit.data.train[,11]),
                    scaled=TRUE,type="C-svc",kernel="polydot",C=1,kpar=list(degree=1,scale=s))
  pred.svm <- predict(svm.model,credit.data.val[,1:10])
  accuracy.svm<-sum(pred.svm == credit.data.val[,11]) / nrow(credit.data.val)
  acc.svm.scale<-append(acc.svm.scale,accuracy.svm)
}

# Plot the accuracy values for different sigma
plot(x=scale.values,y=acc.svm.scale,type='b', 
     main="Accuracy for various scale values with polynomial kernel",
     xlab="Scale in polynomial kernel",ylab="Accuracy",
     col = "dark red",lwd=5,lty=2,cex.lab=1.25,cex.main=1.5,
     ylim=c(0.0,1.0))
grid(col = "dark red", lty = "dotted",
     lwd = 2, equilogs = TRUE)

#-------------------------------------------------------------------------
# Tuning polynomial kernel
# train with Training set and accuracy with validation set
acc.svm.offset<-c()
offset.values<-seq(0.1,2,0.1)

for (o in offset.values){
  svm.model <- ksvm(x=as.matrix(credit.data.train[,1:10]),
                    y=as.factor(credit.data.train[,11]),
                    scaled=TRUE,type="C-svc",kernel="polydot",C=1,kpar=list(degree=1,offset=o,scale=1.0))
  pred.svm <- predict(svm.model,credit.data.val[,1:10])
  accuracy.svm<-sum(pred.svm == credit.data.val[,11]) / nrow(credit.data.val)
  acc.svm.offset<-append(acc.svm.offset,accuracy.svm)
}

# Plot the accuracy values for different sigma
plot(x=offset.values,y=acc.svm.offset,type='b', 
     main="Accuracy for various offset values with polynomial kernel",
     xlab="Offset in polynomial kernel",ylab="Accuracy",
     col = "dark red",lwd=5,lty=2,cex.lab=1.25,cex.main=1.5,
     ylim=c(0.0,1.0))
grid(col = "dark red", lty = "dotted",
     lwd = 2, equilogs = TRUE)

#-------------------------------------------------------------------------
# After tuning parameters with the validation test, apply on the test set
svm.model.best<-ksvm(x=as.matrix(credit.data.train[,1:10]),
                     y=as.factor(credit.data.train[,11]),
                     scaled=TRUE,type="C-svc",kernel="rbfdot",C=1)

# Accuracy of Test set using the best model
pred.svm.best <- predict(svm.model.best,credit.data.test[,1:10])
accuracy.svm.best<-sum(pred.svm.best == credit.data.test[,11]) / nrow(credit.data.test)
cat("Accuracy (Test) on the best model: ",accuracy.svm.best)
