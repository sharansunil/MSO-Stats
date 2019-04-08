rm(list=ls())
library(ggplot2)
library(dplyr)
library(tidyverse)
library(readr)
library(readxl)
#dataframe prep
df<-read_excel('data.xlsx')
df$riders <- df$orders/df$utr
df$month<-format(as.Date(df$order_date), "%m")
df$day<-strftime(df$order_date,"%A")
df<-df %>% select(order_date,country,orders,utr,riders,month,day)
write.csv(df,'prepped_df.csv',row.names = FALSE)


df_weekend<-df %>% filter(day %in% c("Saturday","Sunday","Friday"))
df_weekday<- df %>% filter(day %in% c("Monday","Thursday","Tuesday","Wednesday"))
#viz

country_viz<-function(df,country_){
g<-ggplot(data=df,aes(x=order_date,y=riders,colour=country,group=month))+geom_boxplot()
g %+% subset(df,country %in% c(country_))
}

country_growth<-function(df_,country_){
  df_ = df_ %>% filter(country==country_)
  first_riders = df_[1,5]
  last_riders = df_[365,5]
  yoy_growth = last_riders/first_riders -1 
  return (yoy_growth)
}

growths<-rbind(unique(df$country),unlist(sapply(unique(df$country),function(x)(country_growth(df,x)))))
colnames(growths)<-growths[1,]
growths<-growths[2,]
growths<-sapply(growths,as.numeric)
