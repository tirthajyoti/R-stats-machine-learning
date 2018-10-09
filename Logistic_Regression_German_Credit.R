# Read in the text file
df<-read.table("germancredit.txt",header = F, sep = " ")
head(df)

# Simple transformation of the response variable to 0/1
df$V21<-df$V21%%2

# Build training, validation, and test sets using caTools package
library(caTools)
set.seed(101) 
sample1 <- sample.split(df$V21, SplitRatio = .6)
train <- subset(df, sample1 == TRUE)
test  <- subset(df, sample1 == FALSE)
sample2 <- sample.split(test$V21, SplitRatio = .5)
valid <- subset(test, sample2 == TRUE)
test  <- subset(test, sample2 == FALSE)

# Show relative prevelance of classes in the training/validation/test sets
hist(train$V21,col='orange',main="Histogram of Response: Good or Bad credit?")
hist(valid$V21,col='orange',main="Histogram of Response: Good or Bad credit?")
hist(test$V21,col='orange',main="Histogram of Response: Good or Bad credit?")

# GLM model fit to training data set
glm.1 <-glm(V21~.,data=train,family=binomial(link="logit"))

# Model summary
summary(glm.1)

# Prediction on validation set
prob.val<-predict(glm.1,newdata = valid[,1:20],type='response')

hist(prob.val)

# Turning probabilities into concrete binary predictions
# Start with a 0.5 probability threshold
threshold<-0.5
pred.val<-as.numeric(prob.val>threshold)

# Validation set responses as factors
true.val<-as.factor(valid$V21)

# Confusion matrix (with Caret package)
library(caret)
cat("\n")
confusionMatrix(true.val,as.factor(pred.val))

# ROC and AUC
library(pROC)
plot(roc(true.val,prob.val), main="ROC curve of the Logistic Regression Classifier",cex=1.2,
     col='blue',lty=3,lwd=5)

auc(roc(true.val,prob.val))

# Determining which factors show significance from the GLM model
p.vals<-coef(summary(glm.1))[,4]
lowP<-p.vals<0.05
significant.factors<-subset(glm.1$coefficients,lowP==TRUE)

# 1-hot encoding
library(onehot)
onehot.encoder<-onehot(train)
train.1hot<-as.data.frame(predict(onehot.encoder,train))
# Replacing all "=" in column names with "" (produced from 1-hot encoding)
library(stringr)
colnames(train.1hot)<-str_replace_all(colnames(train.1hot),"=","")

valid.1hot<-as.data.frame(predict(onehot.encoder,valid))
# Replacing all "=" in column names with "" (produced from 1-hot encoding)
colnames(valid.1hot)<-str_replace_all(colnames(valid.1hot),"=","")

# Build a formula object from the significant factors of the original GLM model
fml<-as.formula(V21~V1A14+V2+V3A34+V4A41+V4A42+V4A43+V5+V6A65+V8+V10A103+V12A124+V14A143+V15A153)

# Build new GLM based on 1-hot-encoded data
glm.1hot<-glm(fml,data=train.1hot,family=binomial(link="logit"))

# Prediction on validation set
prob.val<-predict(glm.1hot,newdata = valid.1hot,type='response')

hist(prob.val)

# Turning probabilities into concrete binary predictions
# A 0.5 probability threshold
threshold<-0.5
pred.val<-as.numeric(prob.val>threshold)

# Validation set responses as factors
true.val<-as.factor(valid$V21)

# Confusion matrix (with Caret package)
cat("\n")
confusionMatrix(true.val,as.factor(pred.val))

#========================================
# Cross-validation with the GLM model
#=======================================
library(boot)
cv.err<-cv.glm(data=train.1hot,glm.1hot,K=10)
cat("10-fold cross-validation error estimate: ",cv.err$delta[2])

#========================================================
# Producing confusion matrices for different thresholds
#========================================================
for (p in 10:90){
  prob.val<-predict(glm.1,newdata = valid,type='response')
  pred.val<-as.numeric(prob.val>0.01*p)
  true.val<-as.factor(valid$V21)
  cm<-confusionMatrix(true.val,as.factor(pred.val))
  cat("\n")
  cat("Confusion matrix for",p)
  print(cm$table)
  }

cost <-c()
for (p in 5:95){
  prob.val<-predict(glm.1,newdata = valid,type='response')
  pred.val<-as.numeric(prob.val>0.01*p)
  true.val<-as.factor(valid$V21)
  cm<-confusionMatrix(true.val,as.factor(pred.val))
  cost<-c(cost,cm$table[2]+5*cm$table[3])
}
plot(0.01*c(5:95),cost,pch=19, cex=1.25, main="Plot of total cost to the bank as a function of prob threshold",
     xlab="Probability",ylab="Total cost",cex.main=1.5,cex.axis=1.5,cex.lab=1.5)
