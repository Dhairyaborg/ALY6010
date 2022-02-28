install.packages("Hmisc")
#Calling libraries

library(FSA)
library(FSAdata)
library(magrittr)
library(dplyr)
library(tidyr)
library(plyr)
library(tidyverse)
library(ggplot2)
library(Hmisc)

#importing Dataset
mydata<- read.csv("googleplaystore.csv")
is.data.frame(mydata)
View(mydata)

#Summary
summary(mydata)
str(mydata)
temp<-headtail(mydata)
View(temp)

is.na.data.frame(mydata)

#checking for outliers
boxplot(mydata$Rating) 
title("Showing Outlier in Ratings")

newdata<- subset(mydata,mydata$Rating <6)
df<-na.omit(newdata)
as.data.frame(df)

#converting into appropiate data structure.
df$Price<-gsub("\\$","",df$Price)
df$Installs<-gsub("\\+","",df$Installs)
df$Installs<-gsub(",","",df$Installs)

DF<-transform(df, Price = as.numeric(Price))
DF<-transform(DF, Reviews = as.numeric(Reviews))
DF<-transform(DF, Installs = as.numeric(Installs))
sapply(DF,class)

#Summary after cleaning
summary(DF)

#Grouping
price<-aggregate(DF$Price,list(DF$Category),FUN=sum)
rating<-aggregate(DF$Rating,list(DF$Category),FUN=mean)
reviews<-aggregate(DF$Reviews,list(DF$Category),FUN=mean)

plot(price, col="blue", main="Category wise Price", xlab="Price", ylab="Category",las=2,cex.names="0.55",
     cex.axis="0.4", type="b")
hist(rating$x,rating$Group.1, main="Category wise Rating")
?line
