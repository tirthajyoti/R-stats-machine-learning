# Load readxl library to read the excel input data
library (readxl)
wine.data <- read_xlsx("wine.xlsx",sheet="Sheet1")
wine.data<-as.data.frame(wine.data)

# Load caTools library to create test/train split
# Using a fixed seed for random generation and split ratio of 0.7
library(caTools)
set.seed(101)
sample <- sample.split(wine.data$Class, SplitRatio = 0.7)
train <- subset(wine.data, sample == T)
test <- subset(wine.data, sample == F)

# Scale (normalize) the input data to make neural network work properly
train1<- as.data.frame(scale(train[2:14]))
test1<-as.data.frame(scale(test[2:14]))
train[2:14]<-train1
test[2:14]<-test1

# Create formula to use in the neural network model from column names of the data set
v<- colnames(test)
m<-v[2]
for (i in 3:length(v)){
    m<-paste(m,v[i], sep="+")
}
m<- paste(v[1],'~',m)
f<- as.formula(m)

# Load softmax neural network library and fit model
# 3 hidden layers of 5 neurons each, feel free to change and experiment
# Mini-batch size of 10; activation function = RELU, learning rate = 0.1
# Algorithm = rmsprop, maximum iterations = 500
# Feel free to play with these parameters and train your own model
library(softmaxreg)
softmax.model<- softmaxReg(formula=f,data=train,hidden=c(5,5,5),
                           funName = "relu",type='class', 
                           batch=10,rang=1,rate=0.1,threshold = 0.01,maxit=500)

# Predit using the fitted model
# Show histogram of the predicted classes (as factors)
p<-predict(softmax.model,test[2:14])
hist(p,breaks=10,main = "Class of wine predicted by the neural network")

# Create classification error table for 3 classes and print overall accuracy
tab<- as.matrix(table(test$Class,p))
accu=(tab[1,1]+tab[2,2])/(tab[1,1]+tab[1,2]+tab[2,1]+tab[2,2])
print(paste("Accuracy:",round(accu,3)))
