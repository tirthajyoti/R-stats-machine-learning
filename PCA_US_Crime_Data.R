# Read in the text file
df<-read.csv("uscrime.txt",header = TRUE, sep = "")

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

cor_mat <- cor(df[,1:15])
library('corrplot')
corrplot(cor_mat)

# Build a linear regression model with Crime as the response and all other features as predictors
linear.model <- lm(Crime~.,data=df)

# Summary of the linear model
summary(linear.model)

# Preparing for PCA
df.pca<-df[,-c(16)]

# Perform PCA
pca.model.1<-prcomp(df.pca,scale. = T)
pca.model.2<-prcomp(df.pca,scale. = T,rank=5)

# PCA model summary and plot
print(pca.model.1)
plot(pca.model.1)

print(pca.model.2)
plot(pca.model.2)

# 'factoextra' package for visualization
library(factoextra)

fviz_eig(pca.model.1)

fviz_pca_ind(pca.model.1,
             col.ind = "cos2", # Color by the quality of representation
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)

fviz_pca_var(pca.model.1,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)

fviz_pca_biplot(pca.model.1, repel = TRUE,
                col.var = "#2E9FDF", # Variables color
                col.ind = "#696969"  # Individuals color
)

fviz_eig(pca.model.2)

fviz_pca_ind(pca.model.2,
             col.ind = "cos2", # Color by the quality of representation
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)

fviz_pca_var(pca.model.2,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)

fviz_pca_biplot(pca.model.2, repel = TRUE,
                col.var = "#2E9FDF", # Variables color
                col.ind = "#696969"  # Individuals color
)

# Access to the model components
eig.val <- get_eigenvalue(pca.model.1)
eig.val


# Results for Variables
res.var <- get_pca_var(pca.model.1)
res.var$coord          # Coordinates
res.var$contrib        # Contributions to the PCs
res.var$cos2           # Quality of representation 

# Results for individuals
res.ind <- get_pca_ind(pca.model.1)
res.ind$coord          # Coordinates
res.ind$contrib        # Contributions to the PCs
res.ind$cos2           # Quality of representation


pca.df<-as.data.frame(cbind(pca.model.1$x[,1:5],df$Crime))
colnames(pca.df)[6]="Crime"
head(pca.df)

# Build a linear model with PCA
lm.pca<-lm(Crime~.,data=pca.df)

# Quality plots
par(mfrow=c(2,2),pch=19)
plot(linear.model)
plot(lm.pca)

#===============================
# Predictions using simple lm
#===============================
# Construct a numeric vector with the predictor variables needed
pred.vector<-c(14.0, 0, 10.0, 12.0, 15.5, 0.640, 94.0, 150, 1.1, 0.120, 3.6, 3200, 20.1, 0.04, 39.0, 0)

# Create a new dataframe by binding the prediction vector to the training data frame
df1<-rbind(df,pred.vector)

predict(linear.model,df1[48,1:15])

#===============================
# Predictions using pca lm
#===============================
# Transform the new city vector using predict method of the PCA object
new.city.pc<-predict(pca.model.1,df1[48,1:15])

# Then predict the crime rate using this transformed (un-scaled) vector and linear model built with PCA
predict(lm.pca,as.data.frame(new.city.pc))

# PCA Linear Model coefficients
a<-lm.pca$coefficients
# Only predictors
a1<-as.matrix(a[2:6])

# PCA model rotation components
b<-as.matrix(pca.model.1$rotation)
# Only first 5 components
b1<- b[,1:5]

# Multiply a1 and b1 to get PCA linear model coefficients in terms of original variables
coeff.orig.var<-b1%*%a1
coeff.orig.var

# Un-scale by multiplying with scale and adding center
coeff.orig.var<-coeff.orig.var*pca.model.1$scale+pca.model.1$center