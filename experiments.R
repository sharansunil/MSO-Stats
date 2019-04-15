rm(list=ls())
library(ggplot2)
library(dplyr)
library(tidyverse)
library(readr)
library(readxl)
library(GGally)
library(magrittr)

#initial data load
df<-read_excel('data.xlsx')
df$month<-as.numeric(format(as.Date(df$order_date), "%y%m"))
df$day<-strftime(df$order_date,"%A")
df<-df %>% select(order_date,country,orders,utr,month,day)
is_wknd<-function(x){
  var=0
  if(x %in% c("Saturday","Sunday")){
    var=1
  }
  return(var)
}
df$is_wknd<-as.factor(sapply(df$day,is_wknd))
#initializee dummies
months<-unique(df$month)
rvec<-c()
for(d in df$month){
  for(i in seq(1,13)){
    if(d==months[i]){
      rvec<-c(rvec,i)
    }
  } 
}
df$index<-rvec 
df$country<-as.factor(df$country)


#country vs time regression
df.country.lm<-df %>% select(index,country,orders)
df.country.lm$index<-scale(df.country.lm$index)
country.lm<-lm(orders~country*index,data=df.country.lm)
summary(country.lm)
ggplot(data=df.country.lm,aes(x=index,y=orders)) + geom_smooth(method='lm')
#interaction plots
df.int<-df %>% group_by(country,is_wknd) %>% summarise(mean(orders))
colnames(df.int)<-c("country","is_wknd","mean_orders")
predicted_df <- data.frame(order_pred = predict(country.lm, df.country.lm), index=scale(df$index))

ggplot(df.country.lm,aes(x=index,y=orders,colour=country))+ geom_point() + geom_line(color='red',data = predicted_df, aes(x=index, y=order_pred)) 


#boxplot overall
ggplot(data=df,aes(x=month,y=orders,colour=country))+geom_boxplot()+facet_grid(~country)

aov(data=df.int,mean_orders~country*is_wknd)
qqplot(x=df$order_date,y=df$orders)
summary(aov(mean_orders~country+is_wknd+country:is_wknd,data=df.int))



