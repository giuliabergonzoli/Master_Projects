rm(list = ls()) #Clean workspace

#libraries
library(foreign)
library(xts)
library(ggplot2)
library(pastecs)
require(psych)
library(MASS)
if(!require(stargazer)) install.packages('stargazer')
if(!require(BatchGetSymbols)) install.packages('BatchGetSymbols')
if(!require(car)) install.packages("car")
require(gplots)
library(reshape2)
library(Rcpp)
if(!require(plm)) install.packages("plm")
library("plm")
library(dyn)
library(dynlm)
library(zoo)
library(lmtest)
library(orcutt)
library(sandwich)
library(corrplot)

# Set directory
setwd("/Users/niccolofoglia/Desktop/Programmi/ECONOMETRICS/project")
# Check if the directory is correct
getwd()


#######################################
## Initializing and Visualizing Data ##
#######################################

# monthly returns and indexs
mydata <- read.csv('dataset.csv', header=TRUE, dec = ',', sep = ';')
mytime <- zoo(mydata)
head(mydata)
summary(mydata)

index <- mydata[, 3:6 ]  #c(3,6) We take the indexes
matplot(index, type = 'l',  main = "Climate Index")

# We calculate the returns by means of the log difference (on excel)
returns_comm <- mydata[, 23:25]
returns_green <- mydata[, 26:32]
returns_brown <- mydata[, 33:38]
# we calculate the average for both green and brown returns
avg_returns_green <- mydata[, 39]
avg_returns_brown <- mydata[, 40]

matplot(returns_comm, type = 'l',  main = "Returns Commodities")
matplot(returns_green, type = 'l',  main = "Returns Green Stocks")
matplot(returns_brown, type = 'l',  main = "Returns Brown Stocks")

# save our important data
wsj <- mytime[, 3]
chn <- mytime[, 5]
wsj_inn <- mytime[, 4]
chn_inn <- mytime[, 6]
gas <- returns_comm[,1]
gold <- returns_comm[,2]
oil <- returns_comm[,3]
green_idx <- returns_green[,7] # world ESG leaders returns

#####################
## CORRPLOT MATRIX ##
#####################

market <- data.frame(gas, gold, oil, green_idx, avg_returns_green, avg_returns_brown, wsj, chn, wsj_inn, chn_inn)
market = na.omit(market)
# correlation matrix
corrplot(cor(market))

################
## LAG MODELS ##
################

########################
### Returns Analysis ###
########################

# we use the single indexes

## wsj ##

# World ESG Index
green_idx <- returns_green[,7]
fit1 <- dynlm(green_idx ~ wsj + L(wsj,1) + L(wsj,2) + L(wsj,3) +L(wsj,4))
summary(fit1)
acf(fit1$residuals)

# Average Returns Green
avg_returns_green <- mydata[, 39]
fit2 <- dynlm(avg_returns_green ~ wsj + L(wsj,1) + L(wsj,2) + L(wsj,3) +L(wsj,4) )
summary(fit2)
acf(fit2$residuals)

# Average Returns Brown
avg_returns_brown <- mydata[, 40]
fit3 <- dynlm(avg_returns_brown ~ wsj + L(wsj,1) + L(wsj,2) + L(wsj,3) +L(wsj,4) )
summary(fit3)
acf(fit3$residuals)

## chn ##

# World ESG Index
green_idx <- returns_green[,7]
fit1 <- dynlm(green_idx ~ chn + L(chn,1) + L(chn,2) + L(chn,3) +L(chn,4))
summary(fit1)
acf(fit1$residuals)

# Average Returns Green
avg_returns_green <- mydata[, 39]
fit2 <- dynlm(avg_returns_green ~ chn + L(chn,1) + L(chn,2) + L(chn,3) +L(chn,4) )
summary(fit2)
acf(fit2$residuals)

# Average Returns Brown
avg_returns_brown <- mydata[, 40]
fit3 <- dynlm(avg_returns_brown ~ chn + L(chn,1) + L(chn,2) + L(chn,3) +L(chn,4) )
summary(fit3)
acf(fit3$residuals)

# we tried also with different time lags but these were the best we could get

##########################
## Commodities Analysis ##
##########################

## wsj ##

# gas
gas <- returns_comm[,1]
fit1 <- dynlm(gas ~ wsj + L(wsj,1) + L(wsj,2) + L(wsj,3) + L(wsj,3) + L(wsj,4) )
summary(fit1)
acf(fit1$residuals)

# gold
gold <- returns_comm[,2]
fit2 <- dynlm(gold ~ wsj + L(wsj,1) + L(wsj,2) + L(wsj,3) + L(wsj,3) + L(wsj,4))
summary(fit2)
acf(fit2$residuals)

#oil
oil <- returns_comm[,3]
fit3 <- dynlm(oil ~ wsj + L(wsj,1) + L(wsj,2) + L(wsj,3) + L(wsj,3) + L(wsj,4) )
summary(fit3)
acf(fit3$residuals)

## chn ##

# gas
gas <- returns_comm[,1]
fit1 <- dynlm(gas ~ chn + L(chn,1) + L(chn,2) + L(chn,3) + L(chn,3) + L(chn,5) + L(chn,6)
              + L(chn,7) + L(chn,8) + L(chn,9) + L(chn,10) + L(chn,11) + L(chn,12))
summary(fit1)
acf(fit1$residuals)

# gold
gold <- returns_comm[,2]
fit2 <- dynlm(gold ~ chn + L(chn,1) + L(chn,2) + L(chn,3) + L(chn,3) + L(chn,5) + L(chn,6)
              + L(chn,7) + L(chn,8) + L(chn,9) + L(chn,10) + L(chn,11) + L(chn,12))
summary(fit2)
acf(fit2$residuals)

#oil
oil <- returns_comm[,3]
fit3 <- dynlm(oil ~ chn + L(chn,1) + L(chn,2) + L(chn,3) + L(chn,3) + L(chn,5) + L(chn,6)
              + L(chn,7) + L(chn,8) + L(chn,9) + L(chn,10) + L(chn,11) + L(chn,12))
summary(fit3)
acf(fit3$residuals)

##############################
## Autoregression Analysis ##
##############################

wsj <- mytime[, 3]
fit1 <- dynlm(wsj ~ L(wsj,1) + L(wsj,2))
summary(fit1)
acf(wsj)

chn <- mytime[, 5]
fit2 <- dynlm(chn ~ L(chn,1) + L(chn,2))
summary(fit2)
acf(chn)

# these two following indexes are basically the same of the above but zero-centered

wsj_inn <- mytime[, 4]
fit3 <- dynlm(wsj ~ L(wsj_inn,1) + L(wsj_inn,2))
summary(fit3)
acf(wsj_inn)

chn_inn <- mytime[, 6]
fit4 <- dynlm(chn ~ L(chn_inn,1) + L(chn_inn,2))
summary(fit4)
acf(chn_inn)

#########
## PCA ##
#########

#pca on our four indexes
indexes_features = data.frame(mydata$wsj, mydata$wsj_AR1_Innovation, mydata$chneg, mydata$chneg_AR1_innovation)
cind = princomp(indexes_features)
summary(cind)
par(mar = c(2,2,2,1), mfrow=c(3,1))
for(i in 1:3) barplot(cind$loadings[,i], ylim = c(-1, 1), main=paste('Loadings PC ',i,sep=''))

# saving the first principal component
climate_change_index = cind$scores[,1]
CCI <- zoo(climate_change_index)

################################
## Auto regression Analysis 2 ##
################################

fit5 <- dynlm(CCI ~ L(CCI,1) + L(CCI,2) + L(CCI,3) + L(CCI,4))
summary(fit5)
acf(CCI)

########################
## Returns Analysis 2 ##
########################

# we use the PCA index

# World ESG Index
green_idx <- returns_green[,7]
fit6 <- dynlm(green_idx ~ CCI + L(CCI,1) + L(CCI,2) + L(CCI,3) + L(CCI,4))
summary(fit6)
acf(fit6$residuals)

# Average Returns Green
fit7 <- dynlm(avg_returns_green ~ CCI + L(CCI,1) + L(CCI,2) + L(CCI,3) + L(CCI,4) )
summary(fit7)
acf(fit7$residuals)

# Average Returns Brown
fit8 <- dynlm(avg_returns_brown ~ CCI + L(CCI,1) + L(CCI,2) + L(CCI,3) + L(CCI,4) )
summary(fit8)
acf(fit8$residuals)


#########################
## COMPARISON ANALYSIS ##
#########################

# portfolio long in green stocks and short in brown ones
diff_1 <- mydata[,41]
diff_2 <- mydata[,42]

# 1
fit1 <- dynlm(diff_1 ~ wsj + L(wsj,1) + L(wsj,2) + L(wsj,3))
summary(fit1)
acf(fit1$residuals)

# 2
fit2 <- dynlm(diff_2 ~ wsj + L(wsj,1) + L(wsj,2) + L(wsj,3) + L(wsj,3) + L(wsj,5) + L(wsj,6)
              + L(wsj,7) + L(wsj,8) + L(wsj,9) + L(wsj,10) + L(wsj,11) + L(wsj,12))
summary(fit2)
acf(fit2$residuals)


# 1
fit1 <- dynlm(diff_1 ~ chn + L(chn,1) + L(chn,2) + L(chn,3) + L(chn,3) + L(chn,5) + L(chn,6)
              + L(chn,7) + L(chn,8) + L(chn,9) + L(chn,10) + L(chn,11) + L(chn,12))
summary(fit1)
acf(fit1$residuals)

# 2
fit2 <- dynlm(diff_2 ~ chn + L(chn,1) + L(chn,2) + L(chn,3) + L(chn,3) + L(chn,5) + L(chn,6)
              + L(chn,7) + L(chn,8) + L(chn,9) + L(chn,10) + L(chn,11) + L(chn,12))
summary(fit2)
acf(fit2$residuals)


###########################################
## REGRESSION ON WSJ AND UNEXPECTED NEWS ##
###########################################

# taking the wsj index data from January 1984 to June 2008
dataXX <- read.csv('WSJ.csv', header=TRUE, dec = ',', sep = ';')
dataYY <- zoo(dataXX)

wsj_pre <- dataYY[,1]
fit <- dynlm(wsj_pre ~ L(wsj_pre,1) + L(wsj_pre,2)  + L(wsj_pre,3) + L(wsj_pre,4) )
summary(fit)
AIC(fit)
acf(wsj_pre)
acf(fit$residuals)

wsj <- index[,1]
ex_wsj <- index[1:4,1]
for (i in 5:108) {
  ex_wsj[i] = fit$coefficients[1] + fit$coefficients[2]*wsj[i-1] + fit$coefficients[3]*wsj[i-2] + fit$coefficients[4]*wsj[i-4]
}

# prediction error => constructing the unexpected news index
un_wsj = zoo(wsj - ex_wsj)


# 1
fit1 <- dynlm(diff_1 ~ un_wsj + L(un_wsj,1) + L(un_wsj,2) + L(un_wsj,3) + L(un_wsj,4))
summary(fit1)
acf(fit1$residuals)

# 2
fit2 <- dynlm(diff_2 ~ un_wsj + L(un_wsj,1) + L(un_wsj,2) + L(un_wsj,3) + L(un_wsj,4))
summary(fit2)
acf(fit2$residuals)

# Green Index
fit3 <- dynlm(green_idx ~  un_wsj + L(un_wsj,1) + L(un_wsj,2) + L(un_wsj,3) + L(un_wsj,4)  )
summary(fit3)
acf(fit3$residuals)

# Brown
fit4 <- dynlm(avg_returns_brown ~ un_wsj + L(un_wsj,1) + L(un_wsj,2) + L(un_wsj,3) + L(un_wsj,4) )
summary(fit4)
acf(fit4$residuals)

################################################################################ WE RESTART
# now we use the mccc index and daily returns

#######################################
## Initializing and Visualizing Data ##
#######################################

rm(list = ls()) #Clean workspace

mydata <- read.csv('esg_mccc.csv', header=TRUE, dec = ',', sep = ';')
mydata2 <- read.csv('brown.csv', header=TRUE, dec = ',', sep = ';')
summary(mydata)

mytime <- zoo(mydata[3:28])

# taking the mccc index
index <- mytime[,13]
summary(index)
plot(index)
acf(index)

# taking the avarage green and brown stocks returns
green <- mydata[,28]
brown <- mydata2[,25]

######################
## CORRPLOT  MATRIX ##
######################

market <- data.frame(green, brown, index)
market = na.omit(market)
# correlation matrix
corrplot(cor(market))

################
## LAG MODELS ##
################

# auto-regressive model of mccc
AR <- dynlm(index ~ L(index,1) + L(index,2) + L(index,3) + L(index,4) + L(index,5))
summary(AR)
acf(AR$residuals)

# linear regression of the mean of returns
green <- mydata[,28]
brown <- mydata2[,25]

fit <- dynlm(green ~ index + L(index,1) + L(index,2) + L(index,3) + L(index,4) + L(index,5))
summary(fit)
acf(fit$residuals)

fit1 <- dynlm(brown ~ index + L(index,1) + L(index,2) + L(index,3) + L(index,4) + L(index,5))
summary(fit1)
acf(fit1$residuals)

#comparison analysis: portfolio long in green stocks and short in brown ones
fit2 <- dynlm(green - brown ~ index + L(index,1) + L(index,2) + L(index,3) + L(index,4) + L(index,5))
summary(fit2)
acf(fit2$residuals)


###########################
## PCA ON MARKET RETURNS ##
###########################

mkt_returns_green <- mydata[,16:27]
mkt_returns_green = na.omit(mkt_returns_green)
cind_green = princomp(mkt_returns_green)
summary(cind_green) 

mkt_returns_brown <- mydata2[,14:24]
mkt_returns_brown = na.omit(mkt_returns_brown)
cind_brown = princomp(mkt_returns_brown)

# correlations
corrplot(cor(mkt_returns_green))
corrplot(cor(mkt_returns_brown))

# fist component --> market
# second component --> climate change
par(mar = c(2,2,2,1), mfrow=c(3,1))
for(i in 1:3) barplot(cind_green$loadings[,i], ylim = c(-1, 1), main=paste('Loadings PC ',i,sep=''))

for(i in 1:3) barplot(cind_brown$loadings[,i], ylim = c(-1, 1), main=paste('Loadings PC ',i,sep=''))

scnd_load_green <- cind_green$scores[,2]
scnd_load_brown <- cind_brown$scores[,2]


fit1 <-  dynlm(scnd_load_green ~ index + L(index,1) + L(index,2) + L(index,3) + L(index,4) + L(index,5))
summary(fit1)
acf(fit1$residuals)

fit2 <-  dynlm(scnd_load_green - scnd_load_brown ~ index + L(index,1) + L(index,2) + L(index,3) + L(index,4) + L(index,5))
summary(fit2)
acf(fit2$residuals)


############################
## FAMA&FRENCH CORRECTION ##
############################

fama <- mydata[,29]
summary(fama)

# green
new <- green - fama

fit3 <- dynlm(new ~ index + L(index,1) + L(index,2) + L(index,3) + L(index,4) + L(index,5))
summary(fit3)
acf(fit3$residuals)

# portfolio long in green stocks and short in brown ones
new2 <- green - brown - fama
  
fit4 <- dynlm(new2 ~ index + L(index,1) + L(index,2) + L(index,3) + L(index,4) + L(index,5))
summary(fit4)
acf(fit4$residuals)



