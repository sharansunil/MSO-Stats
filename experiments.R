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
return(df)
}
center_this <- function(x){
  (x - mean(x, na.rm=TRUE))
}

#DATA LOADING + CLEANING
df<-read_excel('data.xlsx')
head(df)
df<-frame_prep(df)

#DATASET EXPLORATION
qqplot(x=df$order_date,y=df$orders) #heavy tailed normal distribution
ggplot(data=df,aes(x=country,y=orders,colour=country))+geom_boxplot()#boxplots to show data spread

###########REGRESSION: COUNTRY X TIME###########

#DF PREP
df_lm<-df %>% select(order_date,country,orders,order_date) #select fields
df_lm<-df_lm %>% mutate(orders =center_this(orders)) #center orders
df_lm$order_date<-scale(df_lm$order_date) #scale order dates

#TRAIN/TEST SPLIT
set.seed(101) # Set Seed so that same sample can be reproduced in future also
# Now Selecting 75% of data as sample from total 'n' rows of the data  
sample <- sample.int(n = nrow(df_lm), size = floor(.9*nrow(df_lm)), replace = F)
df_lm_ <- df_lm[sample, ]
df_test  <- df_lm[-sample, ]
df_lm<-df_lm_

#PAIRWISE CORRELATIONS
cor(df_lm$orders,df_lm$order_date) #shows little pairwise correl between day and orders
pairwise.t.test(df_lm$orders, df_lm$country,p.adjust.method = "BH", pool.sd = FALSE)#heavy tailed normal so t test
#day and country obviously unrelated

#REGRESSION WITH INTERACTION ON TRAINING SET
country.lm<-lm(orders~country*order_date,data=df_lm)
summary(country.lm)
plot(country.lm) #plot 1 check for homoscedacity. plot 2 qqplot plot 3 smth. plot 4 als homoscedastic.
car::vif(country.lm) ###results show no multicollinearity
AIC(country.lm)


#REGRESSION WITHOUT INTERACTION ON TRAINING SET
basiclm<-lm(orders~country+order_date,data=df_lm)
summary(basiclm)
car::vif(basiclm) ###results show no multicollinearity
AIC(basiclm)
plot(basiclm)

#PREDICTION OF MODEL ON TEST SET
set.seed(123)
predicted_orders_country<-predict(country.lm,df_test)
predicted_orders_basic<-predict(basiclm,df_test)
test_eval<-cbind(df_test,predicted_orders_country,predicted_orders_basic) %>% sample_n(100)
ggplot(test_eval,aes(x=order_date))+geom_line(aes(y=orders,colour="orders"))+geom_line(aes(y=predicted_orders_country,colour="predicted_orders_country"))+geom_line(aes(y=predicted_orders_basic,colour="predicted_orders_basic"))
test_eval$res<-test_eval$orders-test_eval$predicted_orders

#REGRESSION VIZ
ggplot(data=df_lm,aes(x=order_date,y=orders)) + geom_smooth(method='lm')
predicted_df <- data.frame(order_pred = predict(country.lm, df_lm), order_date=df_lm$order_date)
ggplot(df_lm,aes(x=order_date,y=orders,colour=country))+ geom_point() + geom_line(alpha=0.4,size=1,color='black',data = predicted_df, aes(x=order_date, y=order_pred)) 

