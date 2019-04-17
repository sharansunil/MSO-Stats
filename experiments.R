rm(list=ls())
library(ggplot2)
library(dplyr)
library(tidyverse)
library(readxl)
library(magrittr)
library(car)


##HELPER FUNCTIONS
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
df$order_date<-as.Date(df$order_date)

return(df)
}
center_this <- function(x){
  (x - mean(x, na.rm=TRUE))
}
generate_2019<-function(lm,transform){
  base_date<-as.Date(max(df$order_date))
  base_scale_date<-max(df_lm$order_date)
  predict_frame<-data.frame(base_date,base_scale_date,"")
  colnames(predict_frame)<-c("date","order_date","country")
  while(base_date<as.Date("2020-01-01")){
    tmp<-data.frame(rep(base_date,8),rep(base_scale_date,8),c("Singapore","Thailand","Taiwan","Malaysia","Pakistan","Hong Kong","Philippines","Bangladesh"))
    colnames(tmp)<-c("date","order_date","country")
    predict_frame<-rbind(predict_frame,tmp)
    base_scale_date=base_scale_date+(2/365)
    base_date=base_date+1
  }
  predict_frame<-as.tbl(predict_frame)
  predict_frame<-predict_frame[-1,]
  predict_frame$pred_orders<-predict(lm,predict_frame)
  if(transform=="square"){
    predict_frame$pred_orders<-predict_frame$pred_orders^2
  }
  else if (transform=="exp"){
    predict_frame$pred_orders<-exp(predict_frame$pred_orders)
  }
  predict_frame<- predict_frame %>% select(date,country,pred_orders)
  colnames(predict_frame)<-c("order_date","country","orders")
  prdf<-df %>% filter(order_date>="2019-01-01") %>% select(order_date,country,orders)
  prdf<-rbind(prdf,predict_frame)
  return(prdf)
}

#DATA LOADING + CLEANING    
df<-read_excel('data.xlsx')
df<-frame_prep(df)

#DATASET EXPLORATION
qqnorm(df$orders) #heavy tailed normal distribution
ggplot(data=df,aes(x=country,y=orders,colour=country))+geom_boxplot() #boxplots to show data spread

###########REGRESSION: COUNTRY X TIME###########

#DF PREP
df_lm<-df %>% select(order_date,country,orders) #select fields
df_lm<-df_lm %>% mutate(order_date=scale(order_date)) #scale order dates

#PAIRWISE CORRELATIONS
cor(df_lm$orders,df_lm$order_date) #shows little pairwise correl between day and orders
pairwise.t.test(df_lm$orders, df_lm$country,p.adjust.method = "BH", pool.sd = FALSE)#heavy tailed normal so t test

#REGRESSION WITH INTERACTION ON TRAINING SET
country.lm<-lm(sqrt(orders)~country*as.vector(order_date),data=df_lm %>% filter(df_lm$order_date>1))
summary(country.lm)
plot(country.lm) #plot 1 check for homoscedacity. plot 2 qqplot plot 3 smth. plot 4 als homoscedastic.
car::vif(country.lm) ###results show no multicollinearity
AIC(country.lm)


#REGRESSION WITHOUT INTERACTION ON TRAINING SET
basiclm<-lm(log(orders)~country+as.vector(order_date),data=df_lm)
summary(basiclm)
car::vif(basiclm) ###results show no multicollinearity
AIC(basiclm)
plot(basiclm)

#REGRESSION VIZ
df_test <-df_lm %>% sample_n(50)
df_test$orders<-log(df_test$orders)
df_test<-rbind(df_test,c(1.74,"Singapore",0))
df_test$order_date<-as.numeric(df_test$order_date)
df_test$orders<-as.numeric(df_test$orders)

ggplot(data=df_lm,aes(x=order_date,y=orders)) + geom_smooth(method='lm')
predicted_df <- as.tbl(data.frame(order_pred = predict(country.lm, df_test), order_date=df_test$order_date))

ggplot(df_test,aes(x=order_date,y=orders))+ geom_point(aes(colour=country,alpha=0.75)) + geom_line(linetype="dotted",colour="black",data = predicted_df, aes(x=order_date, y=order_pred))

##########USING REGRESSION TO PREDICT 2019#################
df_19<-generate_2019(country.lm,"square")
ggplot(df_19_summ,aes(x=month,y=orders,colour=country,group=country))+geom_point()+geom_line()

df_19$month<-strftime(df_19$order_date,"%m")
df_19_summ<-df_19 %>% group_by(country,month) %>% summarise(sum(orders)) 
df_19_summ <- df_19_summ %>%rename(orders=`sum(orders)`)  
write.csv(df_19_summ,"mso_2019_data.csv")
