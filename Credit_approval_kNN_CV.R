# K-NEAREST NEIGHBOR (KNN) CLASSIFICATION
# =======================================
library(kknn)

# Cross-validation with train.kknn and cv.kknn functions
knn.loocv<-train.kknn(R1~.,data=credit.data,kmax=21,scale=TRUE,
                      kernel=c("rectangular", "triangular", "epanechnikov",
                               "gaussian", "rank", "optimal"))

knn.cv<-cv.kknn(R1~.,data=credit.data,kcv=10)

plot(knn.loocv$fitted.values)

library(caret)
ctrl <- trainControl(method="repeatedcv",number=10,repeats = 3)
knnFit <- train(x=credit.data[,1:10],y=as.factor(credit.data[,11]), method = "knn", trControl = ctrl, 
                preProcess = c("center","scale"), tuneLength = 20)

plot(knnFit,col = "dark red",lwd=5,lty=2,cex.lab=1.25,cex.main=1.5,
     main="Accuracy of knn with repeated 10-fold cross validation",
     xlab="Number of neighbors",
     ylab="Accuracy of classification")