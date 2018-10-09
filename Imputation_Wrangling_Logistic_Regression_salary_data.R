adult <- read.csv('adult_sal.csv')

# Grouping employment status
print (table(adult$type_employer))
print ('After reducing employer types...')
replace_employer <- function (job){
  job <- as.character(job)
  if (job == 'State-gov' | job =='Local-gov'){
    return ('SL-gov')
  } else if (job == 'Self-emp-inc' | job =='Self-emp-not-inc'){
    return ('self-emp')
  } else if(job =='Never-worked' | job == 'Without-pay'){
    return ('Unemployed')
  } else {
    return (job)
  }
}

adult$type_employer <- sapply(adult$type_employer, replace_employer)
print (table(adult$type_employer))

# Grouping marital status
print (table(adult$marital))
print ('After reducing marital status...')

replace_marital <- function (marital){
  marital <- as.character(marital)
  if (marital == 'Married-AF-spouse' | marital == 'Married-civ-spouse' | marital == 'Married-spouse-absent'){
    return ('Married')
  } else if (marital == 'Divorced' | marital =='Widowed' | marital == 'Separated'){
    return ('Not-Married')
  } else{
    return (marital)
  }
}

adult$marital <- sapply(adult$marital, replace_marital)
print (table(adult$marital))

# Grouping country by continent
replace_country <- function (country){
  country <- as.character(country)
  if (country ){
    return ('Married')
  } else if (marital == 'Divorced' | marital =='Widowed' | marital == 'Separated'){
    return ('Not-Married')
  } else{
    return (marital)
  }
}

print (table(adult$country))
print ('After grouping countries...')

Asia <- c('China', 'Cambodia', 'Hong', 'India', 'Iran', 'Japan', 'Laos', 'Philippines', 'Taiwan', 'Thailand', 'Vietnam')
Western.Europe <- c('England', 'France', 'Germany', 'Ireland', 'Portugal', 'Scotland', 'Holand-Netherlands')
Eastern.Europe <- c('Greece', 'Hungary', 'Poland', 'Yugoslavia')
North.America <- c('Canada', 'United-States', 'Outlying-US(Guam-USVI-etc)')
Central.America <- c('Puerto-Rico', 'Mexico', 'Nicaragua', 'Cuba', 'Guatemala', 'Dominican-Republic')
Carribeans <- c('Trinadad&Tobago', 'Jamaica', 'Haiti')
South.America <- c('Columbia', 'Ecuador', 'El-Salvador', 'Honduras', 'Peru')

reduce_country <- function (country){
  if (country %in% Asia){
    return ('Asia')
  } else if (country %in% Western.Europe){
    return ("Western.Europe")
  } else if (country %in% Eastern.Europe){
    return ("Eastern.Europe")
  } else if (country %in% North.America){
    return ("North.America")
  } else if (country %in% Central.America){
    return ("Central.America")
  } else if (country %in% Carribeans){
    return ("Carribeans")
  } else if (country %in% South.America){
    return ("South.America")
  } else{
    return ("Other")
  }
}

adult$country <- sapply(adult$country, reduce_country)
print (table(adult$country))
#cat ('\n\n')
#print (str(adult))

adult[adult=='?'] <- NA
# Converting employer, country, marital status to factors
adult$type_employer <- sapply (adult$type_employer, factor)
adult$country <- sapply (adult$country, factor)
adult$marital <- sapply (adult$marital, factor)

## DROP NA DATA
adult <- na.omit(adult)

## LOGISTIC REGRESSION MODEL

# TRAIN TEST SPLIT
library(caTools)
set.seed(101)
sample <- sample.split(adult$income, SplitRatio = 0.7)
train <- subset(adult, sample == T)
test <- subset(adult, sample == F)

# MODEL BUILDING
model <- glm(income ~ . , family = binomial(link = 'logit'), train)

# PREDICTION OF INCOME ON TEST DATA
test$predicted.income <- predict(model, newdata = test, type = 'response')

# CONFUSION MATRIX
t=table(test$income, test$predicted.income>0.5)
print(t)
t = matrix(t)
accuracy = (t[1]+t[4])/sum(t)
cat("\n")
print ("ACCURACY")
print("============")
cat("Predicted salary accuracy:",accuracy)
cat('\n')
#print(accuracy)

