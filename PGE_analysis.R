# Data loading and manupulation

df <- read.csv("PGEdata1.csv", header = TRUE)

# Converting DATE to POSIXct format
df$DATE<-as.POSIXct(strptime(df$DATE, "%m/%d/%Y"))
# Adding a 'time' column to convert START.TIME to HH:MM format
df$time<-as.POSIXct(strptime(df$START.TIME, "%H:%M"))
# Converting 'time' column to starting hour data using 'format' function
df$time<-as.numeric(format(df$time, "%H"))

df$month <- as.numeric(format(df$DATE, "%m"))
library(ggplot2)
#pl <- ggplot(df,aes(month, USAGE))+geom_area(aes(color= time), size = 4, position = position_jitter(w=1,h=0))
#pl <- pl + scale_color_gradientn(colors=c('light blue', 'blue', 'light green', 'yellow', 'orange', 'red', 'black'))
#pl <- ggplot(df, aes(time, USAGE))+ geom_line(size=3, color='red')
pl <- ggplot(df, aes(factor(time), USAGE))+ geom_boxplot(color='red', fill = 'red')
pl<- pl+ coord_cartesian(ylim = c(0, 5))
print (pl)

# Linear Regression
df$ftime <-factor(df$time)
model <- lm(formula = USAGE ~ time, data = df)
print (summary(model))
