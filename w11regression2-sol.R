# WEEK 11: Linear Regression =====
rm(list=ls())

library(readxl)
library(ggplot2)
library(dummies)
  
# Triple Jump with dummy variables =====
triple_jump_gender <- as.data.frame(read_excel("w11 regression2.xlsx", sheet=3))
triple_jump_gender_dummy <- dummy.data.frame(triple_jump_gender) #dummies package allows you to control the choice for dummy variables

#plotting
ggplot(data=triple_jump_gender, aes(x=year, y=distance)) +
  geom_line()
ggplot(data=triple_jump_gender, aes(x=gender, y=distance)) +
  geom_jitter() 

# R automatically encodes categorical variables using 
# dummy coding but you are not in control
triple_jump_gender.lm = lm(distance ~ year+gender+(year*gender), data=triple_jump_gender)    # 'distance ~ .' means distance vs. everything else
# you can do the same as above but here you are choosing the dummy coding
# you can choose gendermen or genderwomen
triple_jump_gender_dummy.lm = lm(distance ~ year+genderwomen+(year*genderwomen), data=triple_jump_gender_dummy)    # 'distance ~ .' means distance vs. everything else

#the answers you get are the same
summary(triple_jump_gender.lm)
summary(triple_jump_gender_dummy.lm)
confint(triple_jump_gender_dummy.lm)    # confidence intervals for coefficients

# notice the overall fit of the model is significant (F-statistic), 
# BUT none of the coefficients are significant
# this often happens due to MULTICOLLINEARITY which arises when you use interaction
# terms. Let's check pairwise correlations between the variables 
cor(triple_jump_gender_dummy$year,triple_jump_gender_dummy$genderwomen*triple_jump_gender_dummy$year)
cor(triple_jump_gender_dummy$genderwomen,triple_jump_gender_dummy$genderwomen*triple_jump_gender_dummy$year)
#cor(Gender, Year*Gender) is extremely high. therefore estimates for beta are 
# not reliable (see really wide CI's for each beta). 
# SOLUTION: standardize variable YEAR and rerun regression!


# Sales per season  =====
sales1 = as.data.frame(read_excel("w11 regression2.xlsx", sheet=2))

# plotting
seasons = c("Spring", "Summer", "Autumn", "Winter")
sales1$season = factor(seasons, levels=seasons)
ggplot(data=sales1, aes(x=quarter, y=sales)) +
  geom_line() + 
  geom_point(aes(shape=season, colour=season))
sales1
# fit linear regression with dummy variables
sales1.lm = lm(sales ~ quarter+season+(quarter*season), data=sales1)    # 'distance ~ .' means distance vs. everything else
summary(sales1.lm)
confint(sales1.lm)    # confidence intervals for coefficients

# just looking at summer data
summer_coeffs = sales1.lm$coefficients[c(1,2,3,6)]    # coefficients when x1 = 1, and x2, x3 = 0
sum(summer_coeffs[c(1,3)])    # 10207.95
sum(summer_coeffs[c(2,4)])    # 32.975

# Triple Jump From Week 10 Confidence Intervals =====
triple_jump = as.data.frame(read_excel("w10 regression.xlsx", sheet=1))
colnames(triple_jump) = c("year", "winning_distance")
triple_jump.lm = lm(winning_distance ~ year, data=triple_jump)    # 'distance ~ .' means distance vs. everything else
summary(triple_jump.lm)
confint(triple_jump.lm)    # confidence intervals for coefficients (default alpha = 0.05)
confint(triple_jump.lm, level = 0.90)    # with alpha = 0.1


# US Economy =====
US_economy = as.data.frame(read_excel("w10 regression2.xlsx", sheet=4))

US_economy.lm = lm(`Unemployment (% of labor force)` ~ . - Year ,data=US_economy)    # '.' means everything else, '- Year' means without Year.

summary(US_economy.lm)
confint(US_economy.lm)
anova(US_economy.lm) #see ANOVA regression table interpretation.pdf file on eDim

# Sales =====
sales2 = read_excel("w10 regression2.xlsx", sheet=5)

sales2.lm.x1.x2 = lm(y ~ ., data=sales2)    # model with both x1 and x2
sales2.lm.x1 = lm(y ~ x1, data=sales2)    # just x1
sales2.lm.x2 = lm(y ~ x2, data=sales2)    # just x2

# Let's define a function that will calculate AIC from the results of an lm() object.
# we can also use built-in AIC function, but values are different. 
# We still look for the lowest value of AIC
calcAIC.lm = function(model){
  SSE = sum((model$residuals)^2)    # Sum of square error
  n = length(model$fitted.values)    # number of observations
  k = sum("(Intercept)" != names(model$coefficients))    # Total number of predictors (excluding intercept)
  return(n*log(SSE/n) + (2*k))
}

calcAIC.lm(sales2.lm.x1.x2)    # -17.82741
calcAIC.lm(sales2.lm.x1)    # -19.50538    (Lowest)
calcAIC.lm(sales2.lm.x2)    # -7.931908

AIC(sales2.lm.x1.x2)  #14.55136
AIC(sales2.lm.x1)    #12.87339 (Lowest)
AIC(sales2.lm.x2)   #24.44686 

summary(sales2.lm.x1.x2)$adj.r.squared    # 0.92849
summary(sales2.lm.x1)$adj.r.squared    # 0.93538 (Highest)
summary(sales2.lm.x2)$adj.r.squared    # 0.79441

# Regression on standardised data
sales2.scaled = as.data.frame(scale(sales2))    # see ?scale()
sales2.scaled.lm = lm(y ~ ., data=sales2.scaled)
summary(sales2.scaled.lm)
# effect of x1 on y is higher than the effect of x2 