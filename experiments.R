rm(list=ls())
library(ggplot2)
library(dplyr)
library(tidyverse)
library(readr)
library(readxl)
library(GGally)
#dataframe prep
df<-read_excel('data.xlsx')
df$month<-format(as.Date(df$order_date), "%y-%m")
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
df
#viz
ggplot(data=df ,aes(x=order_date,y=orders,colour=is_wknd,group=month))+geom_boxplot()+geom_jitter()+
  facet_grid(~country)