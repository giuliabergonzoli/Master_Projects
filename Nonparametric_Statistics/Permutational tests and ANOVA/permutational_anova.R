library(ISLR2)
library(car)
library(mgcv)
library(rgl)
library(splines)
library(pbapply)
library(fda)

data <- read.csv("data_regression.csv", header=TRUE, dec = ',', sep = ';')

data[which(data$Date>'2015-09-01' & data$Date<'2016-12-31'),24]=1516
data[which(data$Date>'2016-09-01' & data$Date<'2017-12-31'),24]=1617
data[which(data$Date>'2017-09-01' & data$Date<'2018-12-31'),24]=1718
data[which(data$Date>'2018-09-01' & data$Date<'2019-12-31'),24]=1819
data[which(data$Date>'2019-09-01' & data$Date<'2020-12-31'),24]=1920
data[which(data$Date>'2020-09-01' & data$Date<'2021-12-31'),24]=2021

names(data)[24]='Anno'


#clean data2021: quota 12 in NULL, omit NA and omit extrem termperatures
data[which(data$NomeStazione=="Schivenoglia v. Malpasso"),11] = 12
data = na.omit(data)
delete_rows = which(data$val_temp<0 | data$val_temp>40)
data = data[-delete_rows,]

data = data[order(data$Idstazione),] # sort by station

data = data[order(data$Date),] # sort by date

#remove negative values of PM10
data[which(data[,7]<0),7] =NA
#remove negative values of wind
data[which(data[,20]<0),20] =NA
#remove negative values of rain
data[which(data[,19]<0),19] =NA
data = na.omit(data)

data$Anno = as.factor(data$Anno)

g <- nlevels(data$Anno)
n <- dim(data)[1]

data[,7] = replace(data[,7], data[,7]==0, 1)
data[,7] = log(data[,7])

plot(data$Anno, data$Valore, xlab='year',col=rainbow(g),main='Original Data')

fit <- aov(data$Valore ~ data$Anno)
summary(fit)

shapiro.test(fit$residuals)
qqnorm(fit$residuals)
abline(a=0,b=1,col='red')

T0 <- summary(fit)[[1]][1,4]
T0

B = 1000
seed = 26111992

T_stat <- numeric(B) 
n <- dim(data)[1]

for(perm in 1:B){
  # Permutation:
  permutation <- sample(1:n)
  valore_perm <- data$Valore[permutation]
  fit_perm <- aov(valore_perm ~ data$Anno)
  
  # Test statistic:
  T_stat[perm] <- summary(fit_perm)[[1]][1,4]
}

hist(T_stat,xlim=range(c(T_stat,T0)),breaks=30)
abline(v=T0,col=3,lwd=2)

plot(ecdf(T_stat),xlim=c(-1,20))
abline(v=T0,col=3,lwd=4)

p_val <- sum(T_stat>=T0)/B
p_val

