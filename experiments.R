rm(list=ls())
library(ggplot2)
library(dplyr)
library(tidyverse)
library(readxl)
library(magrittr)
library(car)


##HELPER FUNCTIONS
is_wknd<-function(x){
  var=0
  if(x %in% c("Saturday","Sunday")){
    var=1
  }
  return(var)
}
frame_prep<-function(df){
  

df$month<-as.numeric(format(as.Date(df$order_date), "%y%m"))
df$day<-strftime(df$order_date,"%A")
df<-df %>% select(order_date,country,orders,utr,month,day)

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
generate_2019<-function(lm){
  base_date<-as.Date("2019-01-01")
  mean_date<-as.Date("2018-10-4")
  base_scale_date<-0.844529874409563
  sd_lm<-sd(df_lm$real_order_date)
  predict_frame<-data.frame(base_date,base_scale_date,"")
  colnames(predict_frame)<-c("date","order_date","country")
  while(base_date<as.Date("2020-01-01")){
    tmp<-data.frame(rep(base_date,8),rep(base_scale_date,8),c("Singapore","Thailand","Taiwan","Malaysia","Pakistan","Hong Kong","Philippines","Bangladesh"))
    colnames(tmp)<-c("date","order_date","country")
    predict_frame<-rbind(predict_frame,tmp)
    base_scale_date=(base_date-mean_date)/sd_lm
    base_date=base_date+1
  }
  predict_frame<-as.tbl(predict_frame)
  predict_frame<-predict_frame[-1,]
  predict_frame$day<-strftime(predict_frame$date,"%A")
  predict_frame$is_wknd<-sapply(predict_frame$day,is_wknd)
  predict_frame$is_wknd<-as.factor(predict_frame$is_wknd)
  preds<-predict(lm,predict_frame,interval="prediction")
  preds<-as_tibble(preds)
  if (deparse(substitute(lm))=="lm_log_int"){
    preds$fit<-exp(preds$fit)
    preds$lwr<-exp(preds$lwr)
    preds$upr<-exp(preds$upr)
  }
  else{}
  predict_frame<-as.tbl(cbind(predict_frame,preds))
  
  predict_frame<- predict_frame %>% select(date,country,is_wknd,fit,lwr,upr)
  
  return(predict_frame)
}

#DATA LOADING + CLEANING    
df<-read_excel('data.xlsx')
df<-frame_prep(df)
df<-df %>% select(order_date,country,orders,utr,day,is_wknd,time)
#DATASET EXPLORATION
qqnorm(df$orders) #qqplot heavy tailed normal distribution
ggplot(data=df,aes(x=country,y=orders,colour=country))+geom_boxplot() #boxplot country x orders
ggplot(data=df,aes(x=order_date,y=orders,colour=country))+geom_point()
ggplot(data=df,aes(x=is_wknd,y=orders))+geom_boxplot() #boxplot weekend x orders
###########REGRESSION: WKND X COUNTRY X TIME###########

#DF PREP
df_lm<-df %>% select(order_date,country,orders,is_wknd) #select fields
df_lm$real_order_date<-df_lm$order_date
df_lm<-df_lm %>% mutate(order_date=as.vector(scale(order_date)))#scale order dates
#PAIRWISE CORRELATIONS
cor(df_lm$orders,df_lm$order_date) #shows little pairwise correl between day and orders
pairwise.t.test(df_lm$orders, df_lm$country,p.adjust.method = "BH", pool.sd = TRUE)#heavy tailed normal so t test
pairwise.t.test(df_lm$orders,df_lm$is_wknd,p.adjust.method = "BH", pool.sd = FALSE)

#REGRESSIONS
lm_no_int<-lm(orders~country+order_date,data=df_lm)
lm_int<-lm(orders~country*order_date,data=df_lm)
lm_log_int<-lm(log(orders)~country*order_date,data=df_lm)
lm_three_way<-lm(orders~country*order_date*is_wknd,data=df_lm)
summary(lm_no_int)
summary(lm_int)
summary(lm_log_int)
summary(lm_three_way)
plot(lm_no_int)
plot(lm_int)
plot(lm_log_int)
plot(lm_three_way,1)
AIC(lm_no_int,lm_int,lm_log_int,lm_three_way)
car::vif(lm_no_int)
car::vif(lm_int)
car::vif(lm_log_int)
car::vif(lm_three_way)

#REGRESSION VIZ

df_test_sg_wknd <-df_lm %>% filter(country=="Singapore",is_wknd==1)
predicted_df <- as.tbl(data.frame(order_pred = predict(lm_three_way, df_test_sg_wknd,interval="prediction"), order_date=df_test_sg_wknd$order_date))
ggplot(predicted_df ,aes(x=order_date))+geom_line(aes(y=order_pred.lwr),linetype="dotted")+geom_line(aes(y=order_pred.fit),colour="#ff007f")+ geom_line(aes(y=order_pred.upr),linetype="dotted") + labs(x="scale_order_date",y="orders",title="Regression with 95% prediction interval for SG orders on weekends")+geom_point(data=df_test_sg_wknd,aes(x=order_date,y=orders))

##########USING REGRESSION TO PREDICT 2019#################
df_19<-generate_2019(lm_three_way) #generates 2019 predictions
df_19$month<-strftime(df_19$date,"%m") #gets month

msodata<-df_19 %>% group_by(month,country) %>% summarise(sum(fit),sum(lwr),sum(upr)) #output data for mso
colnames(msodata)<-c("month","country","fit","lwr","upr")
ggplot(msodata %>% filter(country=="Singapore"),aes(x=month,group=country))+geom_line(aes(y=fit,color="fit")) +geom_line(aes(y=lwr,color="lower"),linetype='longdash')+geom_line(aes(y=upr,colour="upper"),linetype="longdash")+labs(x="month",y="predicted orders",title="2019 Prediction of orders for Singapore at 95%PI",color="Prediction Line") #plot SG 2019predictions with PI
ggplot(msodata,aes(x=month,y=fit,colour=country))+geom_point()
write.csv(msodata,"mso_2019_data.csv")
       