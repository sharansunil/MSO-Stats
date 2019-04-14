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
df$month<-format(as.Date(df$order_date), "%y%m")
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

df_reg<-df %>% group_by(month,country) %>% summarise(mean(orders))

for(level in unique(df_reg$country)){
  if(level=="Singapore"){
    }
  else if(level=="Hong Kong"){
    df_reg[paste("dummy", "HK", sep = "_")] <- ifelse(df_reg$country == level, 1, 0)
  }
  else{
  df_reg[paste("dummy", level, sep = "_")] <- ifelse(df_reg$country == level, 1, 0)
  }
}

#prepare df
df_growths<-df_reg[FALSE,] %>% select(month,country,`mean(orders)`)
colnames(df_growths) = c("month","country","mean_order_growth")

#generate monthly growth rates
for(c in unique(df_reg$country)){
  d=df_reg %>%  filter(country==c)
  growths<-function(d){
    x=d$`mean(orders)`
    i=1
    revec=c()
    print(length(x))
    while(i<length(x)){
      revec[i]=(x[i+1]-x[i])*100/x[i]
      i=i+1
    }
    fvec=cbind(d$month,d$country,c(0,revec))
    colnames(fvec)=c("month","country","mean_order_growth")
    return(fvec)
  }
  dx_=tbl_df(growths(d))
  dx_$mean_order_growth %<>% as.double
  print(dx_)
  df_growths<-bind_rows(df_growths,dx_)
}

rm(list=c("dx_","d","c","level"))

df_reg<-left_join(df_reg,df_growths,by=c("month","country"))
df_reg<- df_reg %>%  select(month,country,mean_order_growth,dummy_Bangladesh,dummy_HK,dummy_Malaysia,dummy_Pakistan,dummy_Philippines,dummy_Taiwan,dummy_Thailand)
df_reg<-df_reg %>% filter(month!="1804") #removes first month as no growth
df_reg$month <- as.numeric(df_reg$month) #converts month to numeric for regression
df_reg$country <- as.factor(df_reg$country) #converts country to factor


#MoM order growth: month x country

encoded.growth.lm<-lm(mean_order_growth~month+dummy_Bangladesh+dummy_Thailand+dummy_Pakistan+dummy_HK+dummy_Malaysia+dummy_Philippines+dummy_Taiwan + dummy_Bangladesh*month+dummy_Thailand*month+dummy_Pakistan*month+dummy_HK*month+dummy_Malaysia*month+dummy_Philippines*month+dummy_Taiwan*month,data=df_reg)

growth.lm<-lm(mean_order_growth~month+country+country*month,data=df_reg)


summary(encoded.growth.lm)
summary(growth.lm)
AIC(encoded.growth.lm)
AIC(growth.lm)
plot(encoded.growth.lm)
plot(growth.lm)

#average daily orders: weekend x country x month
df_wk<- df
df_wk <- df_wk %>% group_by(country,month,is_wknd) %>% summarise(mean(orders))
colnames(df_wk) <- c("country","month","is_wknd","mean_orders")
df_wk$month <-as.numeric(df_wk$month)

wkend.lm<-lm(mean_orders~month+country+is_wknd+country*is_wknd+country*month+is_wknd*month+month*is_wknd*country,data=df_wk)

summary(wkend.lm)
AIC(wkend.lm)
plot(wkend.lm)


#interaction plots
df.int<-df %>% group_by(country,is_wknd) %>% summarise(mean(orders))
colnames(df.int)<-c("country","is_wknd","mean_orders")

country.weekend.int<-ggplot(df.int,aes(x=is_wknd,y=mean_orders,colour=country))+geom_line(aes(group=country))+geom_point()

