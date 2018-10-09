data(iris)
summary(iris)

# Pairwise plots
pairs(iris[1:4], main = "Iris Data", pch = 21, bg = c("red", "green3", "blue")[unclass(iris$Species)])

# Boxplots
library(ggplot2)
p1<-ggplot(iris,aes(x=Species,y=Sepal.Length))+
        geom_boxplot(outlier.colour="red", outlier.shape=8,outlier.size=4)
plot(p1)

p2<-ggplot(iris,aes(x=Species,y=Sepal.Width))+
    geom_boxplot(outlier.colour="red", outlier.shape=8,outlier.size=4)
plot(p2)

p3<-ggplot(iris,aes(x=Species,y=Petal.Length))+
    geom_boxplot(outlier.colour="red", outlier.shape=8,outlier.size=4)
plot(p3)

p4<-ggplot(iris,aes(x=Species,y=Petal.Width))+
    geom_boxplot(outlier.colour="red", outlier.shape=8,outlier.size=4)
plot(p4)

hist(iris.features[,1],main="Histogram of Sepal Length", breaks=20,
     cex.main=1.5,cex.lab=1.25,
     xlab="Sepal Length",col="orange", border="black",
     xlim=c(4,8))    

hist(iris.features[,2],main="Histogram of Sepal Width", breaks=20,
     cex.main=1.5,cex.lab=1.25,
     xlab="Sepal Width",col="orange", border="black",xlim=c(2,4.5))

hist(iris.features[,3],main="Histogram of Petal Length", breaks=30,
     cex.main=1.5,cex.lab=1.25,
     xlab="Petal Length",col="orange", border="black",xlim=c(1,7))

hist(iris.features[,4],main="Histogram of Petal Width", breaks=20,
     cex.main=1.5,cex.lab=1.25,
     xlab="Petal Width",col="orange", border="black",xlim = c(0,2.6))

numtoclass <- function(num){
  if (num==1){
    return ("verginica")
  }
  if (num==2){
    return ("versicolor")
  }
  if (num==3){
    return ("sertosa")
  }
  
}

iris.features<-iris[,1:4]

km.1<-kmeans(iris.features,centers = 3)

x<-as.matrix(km.1$cluster)
x<-apply(x,FUN = numtoclass,MARGIN = 1)
iris.compare<-iris.features
iris.compare$predicted<-as.factor(x)

# Pairwise plots
pairs(iris.compare[1:4], main = "After clustering", pch = 21, 
      bg = c("red", "green3", "blue")[unclass(iris.compare$predicted)])

#---------------------------------------------
# How to get best combination of predictors
library(gtools)
comb.1<-combinations(4,1,colnames(iris[1:4]))
comb.2<-combinations(4,2,colnames(iris[1:4]))
comb.3<-combinations(4,3,colnames(iris[1:4]))
comb.4<-combinations(4,4,colnames(iris[1:4]))

distance.metric<-c()
for (i in 1:4){
    m1<-kmeans(iris.features[c(comb.1[i,])],centers = 3)
    distance.metric<-append(distance.metric,m1$tot.withinss)
}
for (i in 1:6){
    m1<-kmeans(iris.features[c(comb.2[i,])],centers = 3)
    distance.metric<-append(distance.metric,m1$tot.withinss)
}
for (i in 1:4){
    m1<-kmeans(iris.features[c(comb.3[i,])],centers = 3)
    distance.metric<-append(distance.metric,m1$tot.withinss)
}
m1<-kmeans(iris.features[1:4],centers = 3)
distance.metric<-append(distance.metric,m1$tot.withinss)

plot(distance.metric)

#---------------------
# Determining the optimum k by elbow plot

elbow.distance<-c()
k.values <-c(1:10)
for (k in k.values){
    m1<-kmeans(iris.features,centers = k)
    elbow.distance<-append(elbow.distance,m1$tot.withinss)
}

plot(x=k.values,y=elbow.distance,type='b', 
     main="Total distance metric for various k-values",
     xlab="Number of clusters (k)",ylab="Distance metric",
     col = "dark red",lwd=5,lty=2,cex.lab=1.25,cex.main=1.5)
grid(col = "dark red", lty = "dotted",
     lwd = 2, equilogs = TRUE)

m1<-kmeans(iris.features,centers = 4)
pairs(iris[1:4], main = "Iris Data", pch = 21, 
      bg = c("red", "green3", "blue","black")[unclass(m1$cluster)])
    
