library(caret); library(kernlab); data(spam)

# Test/Train split
p<- 0.75; # Split-ratio
inTrain<- createDataPartition(y=spam$type,p=p,list=F)
training <- spam[inTrain,]; test<- spam[-inTrain,]

# Centering and scaling pre-process
prepro <- preProcess(training[,-58],method = c("center", "scale"))
trainScale<- predict(prepro, training[,-58])
trainScale<- cbind(trainScale, type=training$type); #Cbind to include 'type' data to trainScale
testScale<- predict(prepro, test[,-58])

# Logistic Regression (GLM) model fit with stopwatch
start.time <- Sys.time()
fit.1 <- train(type~., data=trainScale, method = 'glm')
end.time <- Sys.time()
t.glm.normal <- end.time - start.time

testPred1<- cbind(testScale,Prediction=predict(fit.1,testScale))
m1<- confusionMatrix(test$type,testPred1$Prediction)

# Correlation check for PCA
M<- abs(cor(training[,-58]))
diag(M)<- 0
m<-which(M>0.8, arr.ind = T)

# PCA by Caret preProcess function
typeCol <- ((training$type=='spam')*1+1)
prC<- preProcess(log10(training[,-58]+1), method = 'pca', pcaComp = 2)
trPC <- predict(prC,log10(training[,-58]+1))
trPC<- cbind(trPC, type=training$type); #Cbind to include 'type' data to trPC
#plot(trPC[,1], trPC[,2], col=typeCol)

# Logistic Regression (GLM) model fit with PCA with stopwatch
start.time <- Sys.time()
fit.pca <- train(type~., data=trPC, method = 'glm')
end.time <- Sys.time()
t.glm.pca <- end.time - start.time

testPC <- predict(prC, log10(test[,-58]+1))
testPred<- cbind(testPC,Prediction=predict(fit.pca,testPC))
m2<- confusionMatrix(test$type,testPred$Prediction)

# Printing of important parameters from confusion matrix
print(paste("Accuracy of normal GLM:", round(m1$overall[1],4)))
print(paste("Sensitivity of normal GLM:", round(m1$byClass[1],4)))
print(paste("Specificity of normal GLM:", round(m1$byClass[2],4)))
print(paste("Time taken to fit normally scaled GLM:", round(t.glm.normal,2), "seconds"))

print(paste("Accuracy with PCA:", round(m2$overall[1],4)))
print(paste("Sensitivity with PCA:", round(m1$byClass[1],4)))
print(paste("Specificity with PCA:", round(m1$byClass[2],4)))
print(paste("Time taken to fit GLM with PCA:", round(t.glm.pca,2), "seconds"))
