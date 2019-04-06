library(ggplot2)
library(dplyr)
library(tidyverse)
library(readr)
library(readxl)
#dataframe prep
df<-read_excel('data.xlsx')
df$riders <- df$orders/df$utr
df$month<-format(as.Date(df$order_date), "%m")
df<-df %>% select(order_date,country,orders,utr,riders,month)
write.csv(df,'prepped_df.csv',row.names = FALSE)
#viz

country_viz<-function(df,country_){
g<-ggplot(data=df,aes(x=order_date,y=riders,colour=country,group=month))+geom_boxplot()
g %+% subset(df,country %in% c(country_))
}


country_growth<-function(df,country_){
  df_ = df %>% filter(country==country_)
  first_riders = df_[1,5]
  last_riders = df_[365,5]
  yoy_growth = last_riders/first_riders -1 
  return (yoy_growth)
}

country_viz(df,"Pakistan")
country_viz(df,"Thailand")
