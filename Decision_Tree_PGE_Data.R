# Reading the downloaded CSV file
# You may have to run it couple of times depending on what PG&E is generating.
# Sometimes the seperator is ' ' and sometime it is ','
d1<- read.csv("pge1.csv", header=T, skip=5, sep=',')

# Subsetting to eliminate unwanted columns - type, notes (blank), end.time, and units (all kWh)
d1<- subset(d1, select=-c(TYPE, NOTES, END.TIME, UNITS))
# Cost column casted as numeric and converted to cents
d1$COST<- as.numeric(d1$COST)
d1$cost.per.unit<- round(d1$COST/d1$USAGE,2)
# Date column casted in date-time format and factorized 
d1$DATE<-as.Date(d1$DATE)
d1$DAY<- as.factor(weekdays(d1$DATE))

# Various time-of-the-day vectors
early.morning <- as.factor(c("00:00", "01:00", "02:00", "03:00", "04:00", "05:00", "06:00"))
morning <- as.factor(c("07:00", "08:00", "09:00", "10:00", "11:00"))
afternoon<- as.factor(c("12:00", "13:00", "14:00", "15:00", "16:00"))
evening <- as.factor(c("17:00", "18:00", "19:00", "20:00"))
night<- as.factor(c("21:00", "22:00", "23:00"))
d1$time.of.day<- c(0)
# Function to categorize time points into one of the time-of-the-day slot
t.day<- function (x){
  if (x %in% early.morning){
    return ("Early.Morning")
  } else if (x %in% morning){
   return ("Morning")
  } else if(x %in% afternoon){
    return ("Afternoon")
  } else if (x %in% evening){
    return ("Evening")
  } else if (x %in% night){
    return ("Night")
  }
}
# Sapply the function and factorize
d1$time.of.day <- sapply(d1$START.TIME, t.day)
d1$time.of.day<- as.factor(d1$time.of.day)

# Load 'rpart' and 'rpart.plot' libraries
library(rpart)
library(rpart.plot)

# Tree model(s), Build whatever relationship you want. I modeled USAGE with other variables
t.usage<- rpart(USAGE~time.of.day+DAY, data=d1, minsplit=5, method='anova', xval=100)
t.cost<- rpart(cost.per.unit~START.TIME+time.of.day+DAY+USAGE, data=d1, minsplit=5, method='anova', xval=100)

# Plotting TREE model, Note the options of shadowing and box.pallete to make the plot nicer looking
rpart.plot(t.usage,type=4, shadow.col = 'darkgray', box.palette = 'Oranges', cex=1.1)

# Boxplots of usage data in ggplot2 to illustrate the usage pattern by the time-of-the-day
library(ggplot2)
pl1<- ggplot(data=d1, aes(y=USAGE, x=time.of.day))+geom_boxplot(col='black', fill='green', alpha=0.5) + theme_grey(base_size = 24)
print(pl1)
