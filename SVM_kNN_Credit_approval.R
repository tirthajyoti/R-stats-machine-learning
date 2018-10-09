# Reading the credit card data (with headers) with `read.csv` function
credit.data<-read.csv('credit_card_data-headers.txt',header = TRUE, sep = "")


# SUPPORT VECTOR MACHINE (SVM) CLASSIFICATION
# =============================================

library('kernlab')

# Calling `ksvm` from kernlab package with simple linear kernel
svm.model.1 <- ksvm(x=as.matrix(credit.data[,1:10]),y=as.factor(credit.data[,11]),
                    scaled=T,type="C-svc",kernel="vanilladot",C=100)

# Calculate coefficients of the SVM model
a <- colSums(svm.model.1@xmatrix[[1]] * svm.model.1@coef[[1]])
cat("a: ",a)
# Extract model intercept
a0 <- svm.model.1@b
cat("Model intercept: ",a0)

# Predictions
pred.svm.1 <- predict(svm.model.1,credit.data[,1:10])
#pred.svm.1

# See what fraction of the model's predictions match the actual classification
accuracy.svm.1<-sum(pred.svm.1 == credit.data[,11]) / nrow(credit.data)
cat("Accuracy: ",accuracy.svm.1)

# Confusion matrix
library(caret)
cm.1<-confusionMatrix(as.factor(pred.svm.1),as.factor(credit.data[,11]))
cm.1$table

# Not SCALED
svm.model.2 <- ksvm(x=as.matrix(credit.data[,1:10]),y=as.factor(credit.data[,11]),
                    scaled=F,type="C-svc",kernel="vanilladot",C=100)

# Calculate coefficients of the SVM model
a <- colSums(svm.model.2@xmatrix[[1]] * svm.model.2@coef[[1]])
cat("a: ",a)
# Extract model intercept
a0 <- svm.model.2@b
cat("Model intercept: ",a0)

# Predictions
pred.svm.2 <- predict(svm.model.2,credit.data[,1:10])
#pred.svm.1

# See what fraction of the model's predictions match the actual classification
accuracy.svm.2<-sum(pred.svm.2 == credit.data[,11]) / nrow(credit.data)
cat("Accuracy with NOT scaled data: ",accuracy.svm.2)

#==========================================================

# Running a loop with various C-values
acc.svm.C<-c()
c.values<-10^(seq(-5,2,0.2))
for (c.val in c.values){
  svm.model <- ksvm(x=as.matrix(credit.data[,1:10]),y=as.factor(credit.data[,11]),scaled=TRUE,type="C-svc",kernel="vanilladot",C=c.val)
  pred.svm <- predict(svm.model,credit.data[,1:10])
  accuracy.svm<-sum(pred.svm == credit.data[,11]) / nrow(credit.data)
  acc.svm.C<-append(acc.svm.C,accuracy.svm)
}

#print(acc.svm.C)
# Plot the accuracy values for different C-values
plot(x=log10(c.values),y=acc.svm.C,type='b', main="Accuracy for various C-values with linear kernel",
     xlab="C-values in Log10 scale",ylab="Accuracy",col = "dark red",lwd=5,lty=2,cex.lab=1.25,cex.main=1.5,
     ylim=c(0.0,1.0))
grid(col = "dark red", lty = "dotted",
     lwd = 2, equilogs = TRUE)

acc.svm.kernel<-c()
svm.kernels<-c('rbfdot','polydot','vanilladot','tanhdot','laplacedot','besseldot','splinedot')

for (k in svm.kernels){
  svm.model <- ksvm(x=as.matrix(credit.data[,1:10]),y=as.factor(credit.data[,11]),
                    scaled=TRUE,type="C-svc",kernel=k,C=1)
  pred.svm <- predict(svm.model,credit.data[,1:10])
  accuracy.svm<-sum(pred.svm == credit.data[,11]) / nrow(credit.data)
  acc.svm.kernel<-append(acc.svm.kernel,accuracy.svm)
}

acc.svm.kernel.sorted<-sort(acc.svm.kernel)

barplot(acc.svm.kernel.sorted, names.arg=svm.kernels, main="Accuracy with various kernel functions with C=1.0",
     xlab="Kernel functions",ylab="Accuracy",col = "orange",cex.lab=1.25,cex.main=1.5,ylim=c(0.0,1.0),horiz = F)


# K-NEAREST NEIGHBOR (KNN) CLASSIFICATION
# =======================================
library(kknn)
# Empty data frame
df<-data.frame()
# Run a loop for each data point, do not include it in the knn
# Create a vector with the grount truth and the knn fitted value (fraction of 1)
for (i in c(1:nrow(credit.data))){
    m1<-kknn(R1~ .,credit.data[-i,],credit.data[i,],k=3,scale = TRUE)
    df1<-list(as.numeric(credit.data$R1[i]),m1$fitted.values)
    df<-rbind(df,df1)
}
# Give proper column names to the data frame
names(df)<-c("Ground.Truth","knn.fitted.value")
# Majority vote - predict class based on knn-predicted fraction i.e. if it is > 0.5
df$pred<-as.numeric(df$knn.fitted.value>0.5)
# If ground truth == prediction
df$correct<-as.numeric(df$Ground.Truth==df$pred)
# Confusion matrix for this exercise
confusionMatrix(as.factor(df$pred),as.factor(df$Ground.Truth))

# RUNNING LOOP FOR VARIOUS K VALUES
#=====================================
acc.knn.vector<-c()

for (k.val in seq(3,20,2)){
    df<-data.frame()
    # Run a loop for each data point, do not include it in the knn
    # Create a vector with the grount truth and the knn fitted value (fraction of 1)
    for (i in c(1:nrow(credit.data))){
        m1<-kknn(R1~ .,credit.data[-i,],credit.data[i,],k=k.val,scale=TRUE)
        df1<-list(as.numeric(credit.data$R1[i]),m1$fitted.values)
        df<-rbind(df,df1)
    }
    # Give proper column names to the data frame
    names(df)<-c("Ground.Truth","knn.fitted.value")
    # Majority vote - predict class based on knn-predicted fraction i.e. if it is > 0.5
    df$pred<-as.numeric(df$knn.fitted.value>0.5)
    # If ground truth == prediction
    df$correct<-as.numeric(df$Ground.Truth==df$pred)
    acc.knn<-sum(df$correct)/nrow(df)
    acc.knn.vector<-append(acc.knn.vector,acc.knn)
    cat("k value done: ",k.val)
}

