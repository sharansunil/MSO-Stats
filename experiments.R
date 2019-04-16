rm(list=ls())
library(ggplot2)
library(dplyr)
library(tidyverse)
library(readr)
library(readxl)
library(GGally)
library(magrittr)

#DATA LOADING + CLEANING
df<-read_excel('data.xlsx')
head(df)
frame_prep<-function(df){
  

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
df$time<-rvec 
df$country<-as.factor(df$country)
return(df)
}
df<-frame_prep(df)


#DATASET EXPLORATION
qqplot(x=df$order_date,y=df$orders) #heavy tailed normal distribution

ggplot(data=df,aes(x=country,y=orders,colour=country))+geom_boxplot()#boxplots to show data spread

###########REGRESSION: COUNTRY X TIME###########

#DF PREP
df.country<-df %>% select(time,country,orders,order_date)
df.country$time<-scale(df.country$time)
df.country$orders<-scale(scale=FALSE,df.country$orders)
df.country$order_date<-scale(df.country$order_date)

#PAIRWISE COR, KRUSKAL WALLIS
cor(df.country$time,df.country$orders) #shows little pairwose correlation between month and orders
kruskal.test(orders~order_date,data=df.country) #shows little pairwise correl between date and orders
kruskal.test(orders~country,data=df.country) #shows little pairwise correl between country and orders
pairwise.wilcox.test(df.country$orders,df.country$country,p.adjust.method = "BH") #pairwise wilcox to see if dependencies within countries

#REGRESSION EVAL
country.lm<-lm(orders~country*scale(order_date),data=df.country)
summary(country.lm)
car::vif(country.lm) ###results show no multicollinearity

#REGRESSION VIZ
ggplot(data=df.country.lm,aes(x=index,y=orders)) + geom_smooth(method='lm')
predicted_df <- data.frame(order_pred = predict(country.lm, df.country.lm), index=scale(df$index))
ggplot(df.country.lm,aes(x=index,y=orders,colour=country))+ geom_point() + geom_line(alpha=0.4,size=2,color='red',data = predicted_df, aes(x=index, y=order_pred)) 


#############ANOVA: WEEKEND X COUNTRY EFFECTS###################

#interaction plots country vs weekend
df.int<-df %>% group_by(country,is_wknd) %>% summarise(mean(orders))
colnames(df.int)<-c("country","is_wknd","mean_orders")
df.int$centered_mean_orders<-scale(scale=FALSE,df.int$mean_orders)
ggplot(data=df.int,aes(x=is_wknd,y=centered_mean_orders,colour=country,group=country))+geom_point()+geom_line()

#ONE WAY ANOVA FOR EACH COUNTRY X WKND
summary(aov(orders~is_wknd,data=df %>% filter(country=="Malaysia")))
summary(aov(orders~is_wknd,data=df %>% filter(country=="Singapore")))
summary(aov(orders~is_wknd,data=df %>% filter(country=="Taiwan")))
summary(aov(orders~is_wknd,data=df %>% filter(country=="Philippines")))
summary(aov(orders~is_wknd,data=df %>% filter(country=="Hong Kong")))
summary(aov(orders~is_wknd,data=df %>% filter(country=="Pakistan")))
summary(aov(orders~is_wknd,data=df %>% filter(country=="Bangladesh")))
summary(aov(orders~is_wknd,data=df %>% filter(country=="Thailand")))
summary(aov(orders~is_wknd*country,data=df))

